--module ChangeDetection where
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Parenthesize unary negation" #-}
{-# HLINT ignore "Use fewer imports" #-}

import ForSyDe.Shallow (signal, fromSignal)
import ForSyDe.Shallow.Core.Vector
    ( atV, fromVector, mapV, vector, zipWithV )
import ForSyDe.Shallow
import System.IO
    ( hGetContents, openFile, IOMode(ReadMode), readFile )
import Data.Complex ()
import Data.Word ()
import Data.Ord (comparing)
--import Data.List (findIndices, sort)
import Data.List (maximumBy, minimumBy, findIndices, sort)
import Statistics.Quantile ( midspread, normalUnbiased )
import Data.Function (on)
import Control.Monad ()
import Data.Maybe ()
import Data.Vector.Unboxed qualified as U
import Data.Massiv.Array
    ( dropWindow,
      fromLists',
      toList,
      avgStencil,
      makeStencil,
      mapStencil,
      U,
      Stencil,
      Array,
      Border(Edge),
      Sz(Sz),
      Ix2((:.)),
      Comp(Par) )


-- First-order Autoregressive Model [AR(1)]
---------------------------------------------------------------------------------------------------------------

arSystem :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> ForSyDe.Shallow.Matrix Double
arSystem dimx dimy y_initial x_n = out
   where 
        out    = fromSignal y_cd !! 0
        y_cd   = mapSY (anomaly dimx dimy) (signal [sf]) -- p-quantile on ecdf
        sf     = mapSY (spatialFilter dimx dimy) (signal [y_n]) -- Spatial Filtering
        y_n    = zipWithSY addMatrix x' y_delayed'  -- AR(1) recursion

        nrho   = zipWithSY pearson (signal [x_n]) (signal [y_initial])  -- normalized sample cross-correlation
        rho    = fromSignal nrho !! 0
        r      = fromSignal y_initial !! 0
        --t      = fromSignal x_n !! 0

        x'     = mapSY (scale rho) x_n
        y_delayed' = mapSY (scale' rho) y_delayed
        y_delayed  = delaySY r y_n


-- p-MC
---------------------------------------------------------------------------------------------------------------

mcSystem :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> Double
mcSystem dimx dimy filtered ref = y_cd
   where 
        y_cd   = rho
        nrho   = zipWithSY pearson (signal [filtered]) (signal [ref])  -- normalized sample cross-correlation
        rho    = fromSignal nrho !! 0


-- Auxiliar: AR recursion
---------------------------------------------------------------------------------------------------------------

scale :: Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double
scale rho matrix = mapV (mapV f) matrix
  where f x = rho * x

scale' :: Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double
scale' rho matrix = mapV (mapV f) matrix
  where f x = sqrt (1 - rho**2) * x

addMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double
addMatrix a b = zipWithV (zipWithV (\x y -> if x > y then x+y  else 0)) a b

-- mulMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double
-- mulMatrix a b = zipWithV (zipWithV (\x y -> if x < 4*y then x  else 0)) a b

pearson :: Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> Double
pearson xs ys = (n * sumXY - sumX * sumY) /
                 sqrt ( (n * sumX2 - sumX*sumX) *
                        (n * sumY2 - sumY*sumY) )
  where 
        t = fromSignal xs !! 0
        r = fromSignal ys !! 0
        xn = flaTTen t
        yn = flaTTen r
        n = fromIntegral (length xn)
        sumX = Prelude.sum xn
        sumY = Prelude.sum yn
        sumX2 = Prelude.sum $ Prelude.zipWith (*) xn xn
        sumY2 = Prelude.sum $ Prelude.zipWith (*) yn yn
        sumXY = Prelude.sum $ Prelude.zipWith (*) xn yn


-- Auxiliar: Anomaly Detection
----------------------------------------------------------------------------------------------------------------
anomaly ::  Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> ForSyDe.Shallow.Matrix Double
anomaly dimx dimy ns = matrix dimx dimy $ Prelude.tail $ scanl (\acc n -> if n > hUP && n < hDOWN then 0  else n) 1 $ flaTTen dat
  where
        dat = fromSignal ns !! 0
        a = flaTTen dat

        -- 10 to 15
        hUP = midspread normalUnbiased 12 (U.fromList a)
        hDOWN = midspread normalUnbiased 16 (U.fromList a)


-- dotMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double
-- dotMatrix ns ms = zipWithMat (*) ns ms

flaTTen :: ForSyDe.Shallow.Matrix a -> [a]
flaTTen = concatMap fromVector . fromVector



-- Auxiliar: Data reading
----------------------------------------------------------------------------------------------------------------

readDat :: String -> (Int, Int, [Double])  -- file content -> (X dimension, Y dimension, list of pixel values)
readDat str = (dimX, dimY, image)
  where
        image = Prelude.map Prelude.read $ words str :: [Double]
        dimX = 500
        dimY = 500

partition :: Int -> Int -> [a] -> [[a]]
partition x y list
        | y - length image > 1 = error "image dimention Y mismatch"
        | otherwise = image
  where
        image = groupEvery x list

groupEvery :: Int -> [a] -> [[a]]
groupEvery n [] = []
groupEvery n l  | length l < n = error "input Data is ill-formed"
                | otherwise    = Prelude.take n l : groupEvery n (Prelude.drop n l)

asciiLevels :: [Char]
asciiLevels = ['0','1',':','-','=','+','/','t','z','U','w','*','0','#','%','@']

toAsciiArt :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Char
toAsciiArt = mapMat num2char
  where
        num2char n = asciiLevels !! level n
        level n = truncate $ nLevels * (n / 255)
        nLevels = fromIntegral $ length asciiLevels - 1


chunks :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double))
chunks dimx dimy img = mapSY (groupMat dimx dimy) img


-- Auxiliar: Stencil with Massiv
----------------------------------------------------------------------------------------------------------------
spatialFilter :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> ForSyDe.Shallow.Matrix Double
spatialFilter dimx dimy img = matrix dimx dimy $ toList $ dropWindow $ mapStencil Edge (avgStencil 9) barImg
  where
        y_n' = fromSignal img !! 0      -- img (signal of matrices) -> list of matrices -> take elem 0 -> y_n' (matrix)
        imG = fromMatrix y_n'           -- converts a matrix into a list. imG is a matrix in form of list
        barImg = fromLists' Par [imG] :: Array U Ix2 Double    -- Par: parallel computation using all available cores

-- basically it's just some sort of casting from a signal of matrices, then the 9x9(?) average filter is applyed


-- average3x3Filter :: Fractional a => Stencil Ix2 a a
-- average3x3Filter = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ get ->
--        (get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
--         get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
--         get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9

-- bubbleSort :: Ord a => [a] -> [a]
-- bubbleSort xs = foldr bubble [] xs
--   where
--         bubble x []     = [x]
--         bubble x (y:ys) | x < y     = x:y:ys
--                         | otherwise = y:bubble x ys


qsort :: Ord a => [a] -> [a]
qsort  [] = []
qsort l@(x:xs) = qsort small ++ mid ++ qsort large
  where
        small = [y | y<-xs, y<x]
        mid   = [y | y<-l, y==x]
        large = [y | y<-xs, y>x]


reverseOrder :: [a] -> [a]
reverseOrder [] = []
reverseOrder (x : xs) = reverseOrder xs ++ [x]



-- Call functions
----------------------------------------------------------------------------------------------------------------

procMatrix1 :: Int -> Int -> ForSyDe.Shallow.Vector (Signal (ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double))) -> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)
procMatrix1 dimx dimy dat = cm1
  where
        t = dat `atV` 0; st = fromSignal t !! 0
        
        ref1 = dat `atV` 1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        
        sv = signal [ssr1]
        
        m = (zipxSY . mapV (mapSY (zipWithMat (\ x y -> arSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        m1 = fromSignal m !! 0; cm1 = m1 `atV` 0


procMatrix3 :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)
procMatrix3 dimx dimy dat = cms !! head sorList
  where
        t = dat `atV` 0; st = fromSignal t !! 0

        ref1 = dat `atV` 1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        ref2 = dat `atV` 3; sr2 = fromSignal ref2 !! 0; ssr2 = vector [sr2]
        ref3 = dat `atV` 5; sr3 = fromSignal ref3 !! 0; ssr3 = vector [sr3]

        sv = signal [ssr1, ssr2, ssr3]

        m = (zipxSY . mapV (mapSY (zipWithMat (\ x y -> arSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        c = (zipxSY . mapV (mapSY (zipWithMat (\ x y -> mcSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) m

        p1 = fromSignal c !! 0; p1mat = fromMatrix p1 !! 0; p1List = p1mat `atV` 0
        p2 = fromSignal c !! 1; p2mat = fromMatrix p2 !! 0; p2List = p2mat `atV` 0
        p3 = fromSignal c !! 2; p3mat = fromMatrix p3 !! 0; p3List = p3mat `atV` 0
        pLists = [p1List] ++ [p2List] ++ [p3List]

        m1 = fromSignal m !! 0; cm1 = m1 `atV` 0
        m2 = fromSignal m !! 1; cm2 = m2 `atV` 0
        m3 = fromSignal m !! 2; cm3 = m3 `atV` 0

        cms = [cm1, cm2, cm3]

        ---- Sorting Lists ---
        revpLists = reverseOrder pLists

        pOrder = qsort pLists
        sorList = findIndices (\l -> l <= pOrder !! 0) revpLists


procMatrix6 :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)
procMatrix6 dimx dimy dat = cms !! head sorList
  where
        t = dat `atV` 0; st = fromSignal t !! 0

        ref1 = dat `atV`  1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        ref2 = dat `atV`  3; sr2 = fromSignal ref2 !! 0; ssr2 = vector [sr2]
        ref3 = dat `atV`  5; sr3 = fromSignal ref3 !! 0; ssr3 = vector [sr3]
        ref4 = dat `atV`  7; sr4 = fromSignal ref4 !! 0; ssr4 = vector [sr4]
        ref5 = dat `atV`  9; sr5 = fromSignal ref5 !! 0; ssr5 = vector [sr5]
        ref6 = dat `atV` 11; sr6 = fromSignal ref6 !! 0; ssr6 = vector [sr6]

        sv = signal [ssr1, ssr2, ssr3, ssr4, ssr5, ssr6]

        m = (zipxSY . mapV (mapSY (zipWithMat (\ x y -> arSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        c = (zipxSY . mapV (mapSY (zipWithMat (\ x y -> mcSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) m

        p1 = fromSignal c !! 0; p1mat = fromMatrix p1 !! 0; p1List = p1mat `atV` 0
        p2 = fromSignal c !! 1; p2mat = fromMatrix p2 !! 0; p2List = p2mat `atV` 0
        p3 = fromSignal c !! 2; p3mat = fromMatrix p3 !! 0; p3List = p3mat `atV` 0
        p4 = fromSignal c !! 3; p4mat = fromMatrix p4 !! 0; p4List = p4mat `atV` 0
        p5 = fromSignal c !! 4; p5mat = fromMatrix p5 !! 0; p5List = p5mat `atV` 0
        p6 = fromSignal c !! 5; p6mat = fromMatrix p6 !! 0; p6List = p6mat `atV` 0
        pLists = [p1List] ++ [p2List] ++ [p3List] ++ [p4List] ++ [p5List]++ [p6List]

        m1 = fromSignal m !! 0; cm1 = m1 `atV` 0
        m2 = fromSignal m !! 1; cm2 = m2 `atV` 0
        m3 = fromSignal m !! 2; cm3 = m3 `atV` 0
        m4 = fromSignal m !! 3; cm4 = m4 `atV` 0
        m5 = fromSignal m !! 4; cm5 = m5 `atV` 0
        m6 = fromSignal m !! 5; cm6 = m6 `atV` 0

        cms = [cm1, cm2, cm3, cm4, cm5, cm6]

        ---- Sorting Lists ---
        revpLists = reverseOrder pLists

        pOrder = qsort pLists
        sorList = findIndices (\l -> l <= pOrder !! 0) revpLists

procMatrix9 :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)
procMatrix9 dimx dimy dat = cms !! head sorList
  where
        t = dat `atV` 0; st = fromSignal t !! 0

        ref1 = dat `atV`  1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        ref2 = dat `atV`  3; sr2 = fromSignal ref2 !! 0; ssr2 = vector [sr2]
        ref3 = dat `atV`  5; sr3 = fromSignal ref3 !! 0; ssr3 = vector [sr3]
        ref4 = dat `atV`  7; sr4 = fromSignal ref4 !! 0; ssr4 = vector [sr4]
        ref5 = dat `atV`  9; sr5 = fromSignal ref5 !! 0; ssr5 = vector [sr5]
        ref6 = dat `atV` 11; sr6 = fromSignal ref6 !! 0; ssr6 = vector [sr6]
        ref7 = dat `atV` 13; sr7 = fromSignal ref7 !! 0; ssr7 = vector [sr7]
        ref8 = dat `atV` 15; sr8 = fromSignal ref8 !! 0; ssr8 = vector [sr8]
        ref9 = dat `atV` 17; sr9 = fromSignal ref9 !! 0; ssr9 = vector [sr9]

        -- parallism is here
        sv = signal [ssr1, ssr2, ssr3, ssr4, ssr5, ssr6, ssr7, ssr8, ssr9]

        m = (zipxSY . mapV (mapSY (zipWithMat (\ x y -> arSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        c = (zipxSY . mapV (mapSY (zipWithMat (\ x y -> mcSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) m

        -----------------------------------------------------------------------------------------------------------------------

        p1 = fromSignal c !! 0; p1mat = fromMatrix p1 !! 0; p1List = p1mat `atV` 0
        p2 = fromSignal c !! 1; p2mat = fromMatrix p2 !! 0; p2List = p2mat `atV` 0
        p3 = fromSignal c !! 2; p3mat = fromMatrix p3 !! 0; p3List = p3mat `atV` 0
        p4 = fromSignal c !! 3; p4mat = fromMatrix p4 !! 0; p4List = p4mat `atV` 0
        p5 = fromSignal c !! 4; p5mat = fromMatrix p5 !! 0; p5List = p5mat `atV` 0
        p6 = fromSignal c !! 5; p6mat = fromMatrix p6 !! 0; p6List = p6mat `atV` 0
        p7 = fromSignal c !! 6; p7mat = fromMatrix p7 !! 0; p7List = p7mat `atV` 0
        p8 = fromSignal c !! 7; p8mat = fromMatrix p8 !! 0; p8List = p8mat `atV` 0
        p9 = fromSignal c !! 8; p9mat = fromMatrix p9 !! 0; p9List = p9mat `atV` 0
        pLists = [p1List] ++ [p2List] ++ [p3List] ++ [p4List] ++ [p5List]++ [p6List]++ [p7List] ++ [p8List]++ [p9List]

        m1 = fromSignal m !! 0; cm1 = m1 `atV` 0
        m2 = fromSignal m !! 1; cm2 = m2 `atV` 0
        m3 = fromSignal m !! 2; cm3 = m3 `atV` 0
        m4 = fromSignal m !! 3; cm4 = m4 `atV` 0
        m5 = fromSignal m !! 4; cm5 = m5 `atV` 0
        m6 = fromSignal m !! 5; cm6 = m6 `atV` 0
        m7 = fromSignal m !! 6; cm7 = m7 `atV` 0
        m8 = fromSignal m !! 7; cm8 = m8 `atV` 0
        m9 = fromSignal m !! 8; cm9 = m9 `atV` 0

        cms = [cm1, cm2, cm3, cm4, cm5, cm6, cm7, cm8, cm9]

        ---- Sorting Lists ---
        revpLists = reverseOrder pLists

        pOrder = qsort pLists
        sorList = findIndices (\l -> l <= pOrder !! 0) revpLists

-- MAIN
----------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    test <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Itest0.dat" ReadMode; contentsTest <- hGetContents test

    ref1 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0A.dat" ReadMode; contentsRef1 <- hGetContents ref1
    ref2 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0B.dat" ReadMode; contentsRef2 <- hGetContents ref2
    ref3 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0C.dat" ReadMode; contentsRef3 <- hGetContents ref3
    ref4 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0D.dat" ReadMode; contentsRef4 <- hGetContents ref4
    ref5 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0E.dat" ReadMode; contentsRef5 <- hGetContents ref5
    ref6 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0F.dat" ReadMode; contentsRef6 <- hGetContents ref6
    ref7 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0G.dat" ReadMode; contentsRef7 <- hGetContents ref7
    ref8 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0H.dat" ReadMode; contentsRef8 <- hGetContents ref8
    ref9 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0I.dat" ReadMode; contentsRef9 <- hGetContents ref9
    ref10 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0J.dat" ReadMode; contentsRef10 <- hGetContents ref10
    ref11 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0K.dat" ReadMode; contentsRef11 <- hGetContents ref11
    ref12 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0L.dat" ReadMode; contentsRef12 <- hGetContents ref12
    ref13 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0M.dat" ReadMode; contentsRef13 <- hGetContents ref13
    ref14 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0N.dat" ReadMode; contentsRef14 <- hGetContents ref14
    ref15 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0O.dat" ReadMode; contentsRef15 <- hGetContents ref15
    ref16 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0P.dat" ReadMode; contentsRef16 <- hGetContents ref16
    ref17 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0Q.dat" ReadMode; contentsRef17 <- hGetContents ref17
    ref18 <- openFile "/home/gwebber/ChangeDetection/Demos/SampleData/Iref0R.dat" ReadMode; contentsRef18 <- hGetContents ref18


     ----- Dataset Arrangement---------------------------------------------------------------------------------------------

    let dimx = 500; dimy = 500
        (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest
        (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1
        (dimX2, dimY2, imageStreamRef2) = readDat contentsRef2; inrefMat2 = matrix dimX1 dimY1 imageStreamRef2
        (dimX3, dimY3, imageStreamRef3) = readDat contentsRef3; inrefMat3 = matrix dimX1 dimY1 imageStreamRef3
        (dimX4, dimY4, imageStreamRef4) = readDat contentsRef4; inrefMat4 = matrix dimX1 dimY1 imageStreamRef4
        (dimX5, dimY5, imageStreamRef5) = readDat contentsRef5; inrefMat5 = matrix dimX1 dimY1 imageStreamRef5
        (dimX6, dimY6, imageStreamRef6) = readDat contentsRef6; inrefMat6 = matrix dimX1 dimY1 imageStreamRef6
        (dimX7, dimY7, imageStreamRef7) = readDat contentsRef7; inrefMat7 = matrix dimX1 dimY1 imageStreamRef7
        (dimX8, dimY8, imageStreamRef8) = readDat contentsRef8; inrefMat8 = matrix dimX1 dimY1 imageStreamRef8
        (dimX9, dimY9, imageStreamRef9) = readDat contentsRef9; inrefMat9 = matrix dimX1 dimY1 imageStreamRef9
        (dimX10, dimY10, imageStreamRef10) = readDat contentsRef10; inrefMat10 = matrix dimX1 dimY1 imageStreamRef10
        (dimX11, dimY11, imageStreamRef11) = readDat contentsRef11; inrefMat11 = matrix dimX1 dimY1 imageStreamRef11
        (dimX12, dimY12, imageStreamRef12) = readDat contentsRef12; inrefMat12 = matrix dimX1 dimY1 imageStreamRef12
        (dimX13, dimY13, imageStreamRef13) = readDat contentsRef13; inrefMat13 = matrix dimX1 dimY1 imageStreamRef13
        (dimX14, dimY14, imageStreamRef14) = readDat contentsRef14; inrefMat14 = matrix dimX1 dimY1 imageStreamRef14
        (dimX15, dimY15, imageStreamRef15) = readDat contentsRef15; inrefMat15 = matrix dimX1 dimY1 imageStreamRef15
        (dimX16, dimY16, imageStreamRef16) = readDat contentsRef16; inrefMat16 = matrix dimX1 dimY1 imageStreamRef16
        (dimX17, dimY17, imageStreamRef17) = readDat contentsRef17; inrefMat17 = matrix dimX1 dimY1 imageStreamRef17
        (dimX18, dimY18, imageStreamRef18) = readDat contentsRef18; inrefMat18 = matrix dimX1 dimY1 imageStreamRef18

        st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st])
        sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])
        sr2 = signal [inrefMat2]; inref2 = mapSY (chunks dimx dimy)  (signal [sr2])
        sr3 = signal [inrefMat3]; inref3 = mapSY (chunks dimx dimy)  (signal [sr3])
        sr4 = signal [inrefMat4]; inref4 = mapSY (chunks dimx dimy)  (signal [sr4])
        sr5 = signal [inrefMat5]; inref5 = mapSY (chunks dimx dimy)  (signal [sr5])
        sr6 = signal [inrefMat6]; inref6 = mapSY (chunks dimx dimy)  (signal [sr6])
        sr7 = signal [inrefMat7]; inref7 = mapSY (chunks dimx dimy)  (signal [sr7])
        sr8 = signal [inrefMat8]; inref8 = mapSY (chunks dimx dimy)  (signal [sr8])
        sr9 = signal [inrefMat9]; inref9 = mapSY (chunks dimx dimy)  (signal [sr9])
        sr10 = signal [inrefMat10]; inref10 = mapSY (chunks dimx dimy)  (signal [sr10])
        sr11 = signal [inrefMat11]; inref11 = mapSY (chunks dimx dimy)  (signal [sr11])
        sr12 = signal [inrefMat12]; inref12 = mapSY (chunks dimx dimy)  (signal [sr12])
        sr13 = signal [inrefMat13]; inref13 = mapSY (chunks dimx dimy)  (signal [sr13])
        sr14 = signal [inrefMat14]; inref14 = mapSY (chunks dimx dimy)  (signal [sr14])
        sr15 = signal [inrefMat15]; inref15 = mapSY (chunks dimx dimy)  (signal [sr15])
        sr16 = signal [inrefMat16]; inref16 = mapSY (chunks dimx dimy)  (signal [sr16])
        sr17 = signal [inrefMat17]; inref17 = mapSY (chunks dimx dimy)  (signal [sr17])
        sr18 = signal [inrefMat18]; inref18 = mapSY (chunks dimx dimy)  (signal [sr18])

        u1 = vector [intest,inref1]
        u3 = vector [intest,inref1,intest, inref3, intest, inref7]
        u6 = vector [intest,inref1,intest, inref3, intest, inref5, intest, inref7, intest, inref9, intest, inref11]
        u9 = vector [intest,inref1,intest, inref3, intest, inref5, intest, inref7, intest, inref9, intest, inref11,
                    intest, inref13, intest, inref15, intest, inref17]


    ----- AR(n) + MC ------------------------------------------------------------------------------------------------
    let m = zipWithxSY (procMatrix1 dimx dimy) u1
    --let m = zipWithxSY (procMatrix3 dimx dimy) u3
    --let m = zipWithxSY (procMatrix6 dimx dimy) u6
    --let m = zipWithxSY (procMatrix9 dimx dimy) u9

    let v = lengthS m
    let m1 = fromSignal m !! 0; cm1 = m1 `atV` 0
    let w = sizeMat m1
    print w


    ----- Output File ------------------------------------------------------------------------------------------------
    writeFile "/home/gwebber/ChangeDetection/Demos/Out/CD0.txt" (show m)
    -- writeFile "/home/gwebber/ChangeDetection/Demos/Out/Test1.txt" (show intest)
    -- writeFile "/home/gwebber/ChangeDetection/Demos/Out/Iref2.txt" (show inref) 


    ---- GHC terminal ----------------------------------------------------------------------------------------------
    -- ghci
    -- :load Demo2.hs    
    -- main

    ---- GHC terminal with parallelism ----------------------------------------------------------------------------------------------
    -- ghc -O2 -threaded --make Demo2.hs 
    -- time ./Demo1 +RTS -s -N12
   