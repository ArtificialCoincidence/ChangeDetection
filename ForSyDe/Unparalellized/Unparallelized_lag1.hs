module Main (main) where

import Lib
import ForSyDe.Shallow

import ForSyDe.Shallow.Core.Vector
-- import ForSyDe.Shallow
import System.IO
import Data.Complex
import Data.Word
import Data.Ord (comparing)
--import Data.List (findIndices, sort)
import Data.List (maximumBy, minimumBy, findIndices, sort)
import Statistics.Quantile
import System.IO (readFile)
import Data.Function (on)
import Control.Monad
import Data.Maybe
-- import Data.Vector.Unboxed qualified as U
import qualified Data.Vector.Unboxed as U
import Data.Massiv.Array
import Data.Massiv.Array.IO

-- First-order Autoregressive Model [AR(1)]: (ELEMENTARY MODEL)
---------------------------------------------------------------------------------------------------------------


arSys :: Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
arSys y_initial x_n = out
    where
        out = addMatrixProc (x') (y_delayed')
        x' = scaleProc (rho) (x_n)
        y_delayed' = scale'Proc (rho) (y_delayed)
        y_delayed = delaySY r out -- Somehow the initial state has to come from something that is not a process, is this when we have more lags? can it be removed? What does it do now?
        r = fromSignal y_initial !! 0 --This has to be changed, cant have fromsignal here (need Marcello to explain how this delay works. I dont think we need this as long as we only use 1 lag, have to check again)
        rho = pearsonProc (x_n) (y_delayed)
        
         
-- Auxiliar: AR recursion
---------------------------------------------------------------------------------------------------------------

scale :: Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -- This returns something weird (it becomes like two matrices?)
scale rho matrix = mapV (mapV f) matrix
  where f x = rho * x

scale' :: Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
scale' rho matrix = mapV (mapV f) matrix
  where f x = sqrt(1 - rho**2) * x

-- changed roh to also be a signal, so scale (and scale') now need zip as a constructor
scaleProc :: Signal Double -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
scaleProc = zipWithSY scale 
scale'Proc :: Signal Double -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
scale'Proc = zipWithSY scale'



addMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
addMatrix a b = zipWithV (zipWithV (\x y -> if x > y then x+y  else 0)) a b

addMatrixProc :: Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
addMatrixProc = zipWithSY addMatrix


mulMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
mulMatrix a b = zipWithV (zipWithV (\x y -> if x < 4*y then x  else 0)) a b

-- let reS = zipWithMat (zipWithMat (\x y -> if x > 0.5*y then x  else 0)) m2 intestMat

pearson :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> Double
pearson xs ys = (n * sumXY - sumX * sumY) / 
                 sqrt ( (n * sumX2 - sumX*sumX) * 
                        (n * sumY2 - sumY*sumY) ) 
  where 
        xn = flaTTen xs
        yn = flaTTen ys
        n = fromIntegral (length xn)
        sumX = Prelude.sum xn
        sumY = Prelude.sum yn
        sumX2 = Prelude.sum $ Prelude.zipWith (*) xn xn
        sumY2 = Prelude.sum $ Prelude.zipWith (*) yn yn
        sumXY = Prelude.sum $ Prelude.zipWith (*) xn yn

pearsonProc :: Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal Double
pearsonProc = zipWithSY pearson

-- Auxiliar: Anomaly Detection
----------------------------------------------------------------------------------------------------------------

anomaly ::  Int -> Int -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double
anomaly dimx dimy ns = matrix dimx dimy $ Prelude.tail $ scanl (\acc n -> if n > hUP && n < hDOWN then 0  else n) 1 $ flaTTen ns
    where
        a = flaTTen ns
        hUP = midspread normalUnbiased 12 (U.fromList a) 
        hDOWN = midspread normalUnbiased 16 (U.fromList a)

anomalyProc' :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
anomalyProc' dimx dimy = mapSY (anomaly dimx dimy)

dotMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
dotMatrix ns ms = zipWithMat (*) ns ms 

flaTTen :: ForSyDe.Shallow.Matrix a -> [a]
flaTTen = concatMap fromVector . fromVector

-- Auxiliar: Data reading
----------------------------------------------------------------------------------------------------------------

readDat :: String            -- ^ file content
        -> (Int, Int, [Double]) -- ^ (X dimension, Y dimension, list of pixel values)
readDat str = (dimX, dimY, image)
  where
    image = Prelude.map Prelude.read $ words str :: [Double]
    dimX = 500
    dimY = 500

chunks :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)) --This is already a process
chunks dimx dimy img = mapSY (groupMat dimx dimy) img

-- Auxiliar: Stencil with Massiv
----------------------------------------------------------------------------------------------------------------

spatialFilter :: Int -> Int -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
spatialFilter dimx dimy img = matrix dimx dimy $ toList $ dropWindow $ mapStencil Edge (avgStencil 9) barImg 
    where
        
        imG = fromMatrix img
        
        barImg = fromLists' Par [imG] :: Array U Ix2 Double

spatialFilterProc' :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
spatialFilterProc' dimx dimy = mapSY (spatialFilter dimx dimy)

average3x3Filter :: Fractional a => Stencil Ix2 a a
average3x3Filter = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \ get ->
    (  get (-1 :. -1) + get (-1 :. 0) + get (-1 :. 1) +
        get ( 0 :. -1) + get ( 0 :. 0) + get ( 0 :. 1) +
        get ( 1 :. -1) + get ( 1 :. 0) + get ( 1 :. 1)   ) / 9
{-# INLINE average3x3Filter #-}


---Sorting Methods

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = foldr bubble [] xs
                where
                bubble x []     = [x]
                bubble x (y:ys) | x < y     = x:y:ys
                                | otherwise = y:bubble x ys


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

processChain :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
processChain dimx dimy x y = anomalyProc' dimx dimy (spatialFilterProc' dimx dimy (arSys x y))

unzipMatByMe :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)) -> ForSyDe.Shallow.Matrix (Signal (ForSyDe.Shallow.Matrix Double))
unzipMatByMe dimx dimy sigMat = matSig
    where
        listMatrix = mapSY (fromMatrix) sigMat -- Signal [Matrix Double]
        vecMat = mapSY (vector) listMatrix -- Signal (Vector (Matrix Double))
        vecSigMat = unzipxSY vecMat -- Vector (Signal (Matrix Double))
        matSig = matrix (dimx) (dimy) (fromVector vecSigMat) -- Matrix (Signal (Matrix Double))

zipMatByMe :: Int -> Int -> ForSyDe.Shallow.Matrix (Signal (ForSyDe.Shallow.Matrix Double)) -> Signal (ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double))
zipMatByMe dimx dimy matSig = sigMat
    where
        vecSigMat = vector (fromMatrix matSig)
        vecMat = zipxSY vecSigMat
        listMatrix = mapSY (fromVector) vecMat
        sigMat = mapSY (matrix dimx dimy) listMatrix    

        
-- MAIN
----------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do



    test <- openFile "/Users/ozone/Desktop/Programing/CG_proj/MC_AR_testing/Test1/SampleData/test0/Itest0.dat" ReadMode; contentsTest <- hGetContents test
    ref1 <- openFile "/Users/ozone/Desktop/Programing/CG_proj/MC_AR_testing/Test1/SampleData/test0/Iref0A.dat" ReadMode; contentsRef1 <- hGetContents ref1
    --ref2 <- openFile "/Users/ozone/Desktop/Programing/CG_proj/MC_AR_testing/Test1/SampleData/test0/Iref0B.dat" ReadMode; contentsRef2 <- hGetContents ref2
    --ref3 <- openFile "/Users/ozone/Desktop/Programing/CG_proj/MC_AR_testing/Test1/SampleData/test0/Iref0C.dat" ReadMode; contentsRef3 <- hGetContents ref3
    --ref4 <- openFile "/Users/ozone/Desktop/Programing/CG_proj/MC_AR_testing/Test1/SampleData/test0/Iref0D.dat" ReadMode; contentsRef4 <- hGetContents ref4
    --ref5 <- openFile "/Users/ozone/Desktop/Programing/CG_proj/MC_AR_testing/Test1/SampleData/test0/Iref0E.dat" ReadMode; contentsRef5 <- hGetContents ref5
    
   
     
     ----- Dataset Arrangement---------------------------------------------------------------------------------------------

    let dimx = 500; dimy = 500
        (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest 
        (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1
        --(dimX2, dimY2, imageStreamRef2) = readDat contentsRef2; inrefMat2 = matrix dimX1 dimY1 imageStreamRef2
        --(dimX3, dimY3, imageStreamRef3) = readDat contentsRef3; inrefMat3 = matrix dimX1 dimY1 imageStreamRef3
        --(dimX4, dimY4, imageStreamRef4) = readDat contentsRef4; inrefMat4 = matrix dimX1 dimY1 imageStreamRef4
        --(dimX5, dimY5, imageStreamRef5) = readDat contentsRef5; inrefMat5 = matrix dimX1 dimY1 imageStreamRef5

        --st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st]) 
        --sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])
        --sr2 = signal [inrefMat2]; inref2 = mapSY (chunks dimx dimy)  (signal [sr2])
        --sr3 = signal [inrefMat3]; inref3 = mapSY (chunks dimx dimy)  (signal [sr3]) 
        --sr4 = signal [inrefMat4]; inref4 = mapSY (chunks dimx dimy)  (signal [sr4])
        --sr5 = signal [inrefMat5]; inref5 = mapSY (chunks dimx dimy)  (signal [sr5])

        st = signal [intestMat];  intest = chunks dimx dimy $ st
        sr1 = signal [inrefMat1]; inref1 = chunks dimx dimy $ sr1
        --sr3 = signal [inrefMat3]; inref3 = chunks dimx dimy $ sr3
        --sr5 = signal [inrefMat5]; inref5 = chunks dimx dimy $ sr5

        --u1 = vector [intest,inref1]
        --u3 = vector [intest,inref1,intest, inref3, intest, inref5]

        dimx1 = dimX `div` dimx; dimy1 = dimY `div` dimy --This to know the size of the new 'container' matrices
        dimx2 = dimX1 `div` dimx; dimy2 = dimY1 `div` dimy

        intestSkel = unzipMatByMe dimx1 dimy1 $ intest
        inref1Skel = unzipMatByMe dimx2 dimy2 $ inref1

        mc = zipWithMat (processChain dimx dimy) inref1Skel intestSkel
        m = zipMatByMe dimx1 dimy1 mc
  
    ----- AR(n) + MC ------------------------------------------------------------------------------------------------
    --let m = zipWithxSY (procMatrix1 dimx dimy) u1
    --let m = zipWithxSY (procMatrix3 dimx dimy) u3
    --let m = zipWithxSY (procMatrix6 dimx dimy) u6
    --let m = zipWithxSY (procMatrix9 dimx dimy) u9
    --let v = lengthS m
    --let m1 = fromSignal m !! 0; cm1 = m1 `atV` 0
    --let w = sizeMat m1
    --print m
    let m2 = sizeMat (fromSignal m !! 0)
    print m2
    let m1 = fromSignal m !! 0


    ----- Output File ------------------------------------------------------------------------------------------------
    writeFile "/Users/ozone/Desktop/Programing/CG_proj/Classifier/Out/CD0.txt" (show m)
    -- writeFile "/home/marcello-costa/workspace/AR1MoC/Out/Test1.txt" (show intest)
    -- writeFile "/home/marcello-costa/workspace/AR1MoC/Out/Iref2.txt" (show inref) 
    

-- main :: IO ()
-- main = print $ aProcess (signal [1..10])
