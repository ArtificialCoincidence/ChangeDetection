--module ChangeDetection where

import ForSyDe.Shallow (signal, fromSignal)
import ForSyDe.Shallow.Core.Vector
import ForSyDe.Shallow
import System.IO
import Data.Complex
import Data.Word
import Data.Ord (comparing)
--import Data.List (findIndices, sort)
import Data.List (maximumBy, minimumBy, findIndices, sort)
import Statistics.Quantile
import Control.Parallel.Strategies
import System.IO (readFile)
import Data.Function (on)
import Control.Monad
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import Data.Massiv.Array

import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Types (ProcessID)



-- First-order Autoregressive Model [AR(1)]: (ELEMENTARY MODEL)
---------------------------------------------------------------------------------------------------------------

arSystem :: Int -> Int ->Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) ->  ForSyDe.Shallow.Matrix Double
arSystem dimx dimy y_initial x_n = out
   where out = fromSignal y_cd !! 0
    
         y_cd = mapSY (anomaly dimx dimy) (signal [sf]) -- p-quantile on ecdf
         sf = mapSY (spatialFilter dimx dimy) (signal [y_n]) -- Spatial Filtering

         y_n        = zipWithSY addMatrix x' y_delayed'  -- AR(1) recursion
         nrho = zipWithSY pearson (signal [x_n]) (signal [y_initial])  -- normalized sample cross-correlation
         rho = fromSignal nrho !! 0
         r = fromSignal y_initial !! 0
         t = fromSignal x_n !! 0
         x'         = mapSY (scale rho) (x_n)
         y_delayed' = mapSY (scale' rho) y_delayed
         y_delayed  = delaySY r y_n 


-- p-MC: MARKOV-CHAIN transition probabilities Pkj
---------------------------------------------------------------------------------------------------------------

mcSystem :: Int -> Int ->Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> Double
mcSystem dimx dimy filtered ref = y_cd
   where y_cd = rho
         nrho = zipWithSY pearson (signal [filtered]) (signal [ref])  -- normalized sample cross-correlation
         rho = fromSignal nrho !! 0

        
         
-- Auxiliar: AR recursion
---------------------------------------------------------------------------------------------------------------

scale :: Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double
scale rho matrix = mapV (mapV f) matrix
  where f x = rho * x

scale' :: Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
scale' rho matrix = mapV (mapV f) matrix
  where f x = sqrt(1 - rho**2) * x

addMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
addMatrix a b = zipWithV (zipWithV (\x y -> if x > y then x+y  else 0)) a b

mulMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
mulMatrix a b = zipWithV (zipWithV (\x y -> if x < 4*y then x  else 0)) a b

-- let reS = zipWithMat (zipWithMat (\x y -> if x > 0.5*y then x  else 0)) m2 intestMat

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

partition :: Int -> Int -> [a] -> [[a]]
partition x y list
  | y - length image > 1 = error "image dimention Y mismatch"
  | otherwise = image
  where
    image = groupEvery x list
    
groupEvery n [] = []
groupEvery n l | length l < n = error "input Data is ill-formed"
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
spatialFilter :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double)  -> ForSyDe.Shallow.Matrix Double  
spatialFilter dimx dimy img = matrix dimx dimy $ toList $ dropWindow $ mapStencil Edge (avgStencil 9) barImg
    where

        y_n' = fromSignal img  !! 0
        
        imG = fromMatrix y_n'
        
        barImg = fromLists' Par [imG] :: Array U Ix2 Double

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

procMatrix1 :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double) 
procMatrix1 dimx dimy dat = cm1
        where

        t = dat `atV` 0; st = fromSignal t !! 0
        
        ref1 = dat `atV` 1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        sv = signal [ssr1]

        m = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> arSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        m1 = fromSignal m !! 0; cm1 = m1 `atV` 0
        

procMatrix4 :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double) 
procMatrix4 dimx dimy dat = cms !! head sorList
        where

        t = dat `atV` 0; st = fromSignal t !! 0
        
        ref1 = dat `atV` 1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        ref2 = dat `atV` 3; sr2 = fromSignal ref2 !! 0; ssr2 = vector [sr2]
        ref3 = dat `atV` 5; sr3 = fromSignal ref3 !! 0; ssr3 = vector [sr3]
        ref4 = dat `atV` 7; sr4 = fromSignal ref4 !! 0; ssr4 = vector [sr4]

        sv = signal [ssr1, ssr2, ssr3, ssr4]  

        

        m = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> arSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        c = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> mcSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) m

        
        p1 = fromSignal c !! 0; p1mat = fromMatrix p1 !! 0; p1List = p1mat `atV` 0
        p2 = fromSignal c !! 1; p2mat = fromMatrix p2 !! 0; p2List = p2mat `atV` 0
        p3 = fromSignal c !! 2; p3mat = fromMatrix p3 !! 0; p3List = p3mat `atV` 0
        p4 = fromSignal c !! 3; p4mat = fromMatrix p4 !! 0; p4List = p4mat `atV` 0

        pLists = [p1List] ++ [p2List] ++ [p3List] ++ [p4List]                  
        
        m1 = fromSignal m !! 0; cm1 = m1 `atV` 0
        m2 = fromSignal m !! 1; cm2 = m2 `atV` 0
        m3 = fromSignal m !! 2; cm3 = m3 `atV` 0
        m4 = fromSignal m !! 3; cm4 = m4 `atV` 0

        cms = [cm1, cm2, cm3, cm4]
        
        ---- Sorting Lists ---
        revpLists = reverseOrder pLists

        pOrder = qsort pLists
        sorList = findIndices (\l -> l <= pOrder !! 0) revpLists

-- Wait for a list of processes to finish
waitForAllProcesses :: [ProcessID] -> IO ()
waitForAllProcesses [] = return ()
waitForAllProcesses (pid:rest) = do
    -- Wait for the process to change state (i.e., finish)
    _ <- getProcessStatus True False pid
    waitForAllProcesses rest


main :: IO ()
main = do


    -- Partition 1 (250 x 250 segment 1)



    -- test <- openFile "SampleData/test6/Itest6.dat" ReadMode; contentsTest <- hGetContents test

    -- ref1 <- openFile "SampleData/test6/Iref6A.dat" ReadMode; contentsRef1 <- hGetContents ref1
    
     
    --  ----- Dataset Arrangement---------------------------------------------------------------------------------------------

    -- let dimx =250; dimy = 250
    --     (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest 
    --     (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1

        
    --     st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st]) 
    --     sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])

  
        
    -- -- Test Sub-images
    -- let n = fromSignal m !! 0; m = fromSignal intest !! 0
    --     subImage1 = atMat 0 0 n; subImage2 = atMat 0 1 n; subImage3 = atMat 1 0 n; subImage4 = atMat 1 1 n
    --     st1 = signal [subImage1]; st2 = signal [subImage2]; st3 = signal [subImage3]; st4 = signal [subImage4]

    -- -- Ref Sub-Images
    -- let n = fromSignal m !! 0; m = fromSignal inref1 !! 0
    --     subRef1_1 = atMat 0 0 n; subRef1_2 = atMat 0 1 n; subRef1_3 = atMat 1 0 n; subRef1_4 = atMat 1 1 n
    --     sr1_1 = signal [subRef1_1]; sr1_2 = signal [subRef1_2]; sr1_3 = signal [subRef1_3]; sr1_4 = signal [subRef1_4]

    -- -- formating
    -- let test_1 = mapSY (chunks dimx dimy)  (signal [st1]) 
    --     test_2 = mapSY (chunks dimx dimy)  (signal [st2])
    --     test_3 = mapSY (chunks dimx dimy)  (signal [st3])
    --     test_4 = mapSY (chunks dimx dimy)  (signal [st4])
    --     ref1_1 = mapSY (chunks dimx dimy)  (signal [sr1_1])
    --     ref1_2 = mapSY (chunks dimx dimy)  (signal [sr1_2])
    --     ref1_3 = mapSY (chunks dimx dimy)  (signal [sr1_3])
    --     ref1_4 = mapSY (chunks dimx dimy)  (signal [sr1_4])
    
    -- let u1_1 = vector [test_1,ref1_1]
    -- let u1_2 = vector [test_2,ref1_2]
    -- let u1_3 = vector [test_3,ref1_3]
    -- let u1_4 = vector [test_4,ref1_4]

    pid1 <- forkProcess $ do
        print "Process 1: Reading Start"
        test <- openFile "SampleData/test6/Itest6.dat" ReadMode; contentsTest <- hGetContents test

        ref1 <- openFile "SampleData/test6/Iref6A.dat" ReadMode; contentsRef1 <- hGetContents ref1
    
        let dimx =250; dimy = 250
            (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest 
            (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1

            
            st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st]) 
            sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])

    -- Test Sub-images
        let n = fromSignal m !! 0; m = fromSignal intest !! 0
            subImage1 = atMat 0 0 n; subImage2 = atMat 0 1 n; subImage3 = atMat 1 0 n; subImage4 = atMat 1 1 n
            st1 = signal [subImage1]; st2 = signal [subImage2]; st3 = signal [subImage3]; st4 = signal [subImage4]

    -- Ref Sub-Images
        let n = fromSignal m !! 0; m = fromSignal inref1 !! 0
            subRef1_1 = atMat 0 0 n; subRef1_2 = atMat 0 1 n; subRef1_3 = atMat 1 0 n; subRef1_4 = atMat 1 1 n
            sr1_1 = signal [subRef1_1]; sr1_2 = signal [subRef1_2]; sr1_3 = signal [subRef1_3]; sr1_4 = signal [subRef1_4]

        let test_1 = mapSY (chunks dimx dimy)  (signal [st1])
            ref1_1 = mapSY (chunks dimx dimy)  (signal [sr1_1])

        let u1_1 = vector [test_1,ref1_1]

        print "Process 1: Start"
        let result = zipWithxSY (procMatrix1 dimx dimy) u1_1
        writeFile "IPC/proc1.txt" (show result)
        print "Process 1: Complete"




    pid2 <- forkProcess $ do

        print "Process 2: Reading Start"
        test <- openFile "SampleData/test6/Itest6.dat" ReadMode; contentsTest <- hGetContents test

        ref1 <- openFile "SampleData/test6/Iref6A.dat" ReadMode; contentsRef1 <- hGetContents ref1
    
        let dimx =250; dimy = 250
            (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest 
            (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1

            
            st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st]) 
            sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])

    -- Test Sub-images
        let n = fromSignal m !! 0; m = fromSignal intest !! 0
            subImage1 = atMat 0 0 n; subImage2 = atMat 0 1 n; subImage3 = atMat 1 0 n; subImage4 = atMat 1 1 n
            st1 = signal [subImage1]; st2 = signal [subImage2]; st3 = signal [subImage3]; st4 = signal [subImage4]

    -- Ref Sub-Images
        let n = fromSignal m !! 0; m = fromSignal inref1 !! 0
            subRef1_1 = atMat 0 0 n; subRef1_2 = atMat 0 1 n; subRef1_3 = atMat 1 0 n; subRef1_4 = atMat 1 1 n
            sr1_1 = signal [subRef1_1]; sr1_2 = signal [subRef1_2]; sr1_3 = signal [subRef1_3]; sr1_4 = signal [subRef1_4]

        let test_2 = mapSY (chunks dimx dimy)  (signal [st2])
            ref1_2 = mapSY (chunks dimx dimy)  (signal [sr1_2])

        let u1_2 = vector [test_2,ref1_2]

        print "Process 2: Start"
        let result = zipWithxSY (procMatrix1 dimx dimy) u1_2
        writeFile "IPC/proc2.txt" (show result)
        print "Process 2: Complete"



    pid3 <- forkProcess $ do

        print "Process 3: Reading Start"
        test <- openFile "SampleData/test6/Itest6.dat" ReadMode; contentsTest <- hGetContents test

        ref1 <- openFile "SampleData/test6/Iref6A.dat" ReadMode; contentsRef1 <- hGetContents ref1
    
        let dimx =250; dimy = 250
            (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest 
            (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1

            
            st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st]) 
            sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])

    -- Test Sub-images
        let n = fromSignal m !! 0; m = fromSignal intest !! 0
            subImage1 = atMat 0 0 n; subImage2 = atMat 0 1 n; subImage3 = atMat 1 0 n; subImage4 = atMat 1 1 n
            st1 = signal [subImage1]; st2 = signal [subImage2]; st3 = signal [subImage3]; st4 = signal [subImage4]

    -- Ref Sub-Images
        let n = fromSignal m !! 0; m = fromSignal inref1 !! 0
            subRef1_1 = atMat 0 0 n; subRef1_2 = atMat 0 1 n; subRef1_3 = atMat 1 0 n; subRef1_4 = atMat 1 1 n
            sr1_1 = signal [subRef1_1]; sr1_2 = signal [subRef1_2]; sr1_3 = signal [subRef1_3]; sr1_4 = signal [subRef1_4]

        let test_3 = mapSY (chunks dimx dimy)  (signal [st3])
            ref1_3 = mapSY (chunks dimx dimy)  (signal [sr1_3])

        let u1_3 = vector [test_3,ref1_3]

        print "Process 3: Start"
        let result = zipWithxSY (procMatrix1 dimx dimy) u1_3
        writeFile "IPC/proc3.txt" (show result)
        print "Process 3: Complete"



    pid4 <- forkProcess $ do


        print "Process 4: Reading Start"
        test <- openFile "SampleData/test6/Itest6.dat" ReadMode; contentsTest <- hGetContents test

        ref1 <- openFile "SampleData/test6/Iref6A.dat" ReadMode; contentsRef1 <- hGetContents ref1
    
        let dimx =250; dimy = 250
            (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest 
            (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1

            
            st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st]) 
            sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])

    -- Test Sub-images
        let n = fromSignal m !! 0; m = fromSignal intest !! 0
            subImage1 = atMat 0 0 n; subImage2 = atMat 0 1 n; subImage3 = atMat 1 0 n; subImage4 = atMat 1 1 n
            st1 = signal [subImage1]; st2 = signal [subImage2]; st3 = signal [subImage3]; st4 = signal [subImage4]

    -- Ref Sub-Images
        let n = fromSignal m !! 0; m = fromSignal inref1 !! 0
            subRef1_1 = atMat 0 0 n; subRef1_2 = atMat 0 1 n; subRef1_3 = atMat 1 0 n; subRef1_4 = atMat 1 1 n
            sr1_1 = signal [subRef1_1]; sr1_2 = signal [subRef1_2]; sr1_3 = signal [subRef1_3]; sr1_4 = signal [subRef1_4]
            
        let test_4 = mapSY (chunks dimx dimy)  (signal [st4])
            ref1_4 = mapSY (chunks dimx dimy)  (signal [sr1_4])

        let u1_4 = vector [test_4,ref1_4]


        print "Process 4: Start"
        let result = zipWithxSY (procMatrix1 dimx dimy) u1_4
        writeFile "IPC/proc4.txt" (show result)
        print "Process 4: Complete"

    waitForAllProcesses [pid1, pid2, pid3, pid4]


    print "res"

    --  ghc -O2 Demo_Parallel.hs -threaded
    -- time ./Demo1 +RTS -s -N12