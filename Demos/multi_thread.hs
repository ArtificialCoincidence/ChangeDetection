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
import Control.Concurrent
import Control.Parallel.Strategies
import System.IO (readFile)
import Data.Function (on)
import Control.Monad
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import Data.Massiv.Array
import GHC.Conc (labelThread)
import Data.Time


-- First-order Autoregressive Model [AR(1)]: (ELEMENTARY MODEL)
---------------------------------------------------------------------------------------------------------------

arSystem :: Int -> Int ->Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> ForSyDe.Shallow.Matrix Double
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
anomaly dimx dimy ns = out
    where
        out =matrix dimx dimy $ Prelude.tail $ scanl (\acc n -> if n > hUP && n < hDOWN then 0  else n) 1 $ flaTTen dat
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
        
        barImg = fromLists' Seq [imG] :: Array U Ix2 Double

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

procAR1 :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> Signal (ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double) )
procAR1 dimx dimy dat = out
        where

        t = dat `atV` 0; st = fromSignal t !! 0
        
        ref1 = dat `atV` 1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        sv = signal [ssr1]

        m = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> arSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        m1 = fromSignal m !! 0; cm1 = m1 `atV` 0

        out = signal [cm1]
        

probMatrix :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> Signal (ForSyDe.Shallow.Matrix Double)
probMatrix dimx dimy dat = signal [out1]
        where

        t = dat `atV` 0; st = fromSignal t !! 0
        ref1 = dat `atV` 1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        ref2 = dat `atV` 3; sr2 = fromSignal ref2 !! 0; ssr2 = vector [sr2]
        ref3 = dat `atV` 5; sr3 = fromSignal ref3 !! 0; ssr3 = vector [sr3]
        ref4 = dat `atV` 7; sr4 = fromSignal ref4 !! 0; ssr4 = vector [sr4]
        -- ref5 = dat `atV` 9; sr5 = fromSignal ref5 !! 0; ssr5 = vector [sr5]
        -- ref6 = dat `atV` 11; sr6 = fromSignal ref6 !! 0; ssr6 = vector [sr6]
        -- ref7 = dat `atV` 13; sr7 = fromSignal ref7 !! 0; ssr7 = vector [sr7]
        -- ref8 = dat `atV` 15; sr8 = fromSignal ref8 !! 0; ssr8 = vector [sr8]
        -- ref9 = dat `atV` 17; sr9 = fromSignal ref9 !! 0; ssr9 = vector [sr9]

    
        --sv = signal [ssr1, ssr2, ssr3, ssr4, ssr5, ssr6, ssr7, ssr8, ssr9]  
        sv = signal [ssr1, ssr2, ssr3, ssr4]
        c = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> mcSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv

        p1 = fromSignal c !! 0; p1mat = fromMatrix p1 !! 0; p1List = p1mat `atV` 0
        p2 = fromSignal c !! 1; p2mat = fromMatrix p2 !! 0; p2List = p2mat `atV` 0
        p3 = fromSignal c !! 2; p3mat = fromMatrix p3 !! 0; p3List = p3mat `atV` 0
        p4 = fromSignal c !! 3; p4mat = fromMatrix p4 !! 0; p4List = p4mat `atV` 0
        -- p5 = fromSignal c !! 4; p5mat = fromMatrix p5 !! 0; p5List = p5mat `atV` 0
        -- p6 = fromSignal c !! 5; p6mat = fromMatrix p6 !! 0; p6List = p6mat `atV` 0
        -- p7 = fromSignal c !! 6; p7mat = fromMatrix p7 !! 0; p7List = p7mat `atV` 0
        -- p8 = fromSignal c !! 7; p8mat = fromMatrix p8 !! 0; p8List = p8mat `atV` 0
        -- p9 = fromSignal c !! 8; p9mat = fromMatrix p9 !! 0; p9List = p9mat `atV` 0
       
        --pLists = [p1List] ++ [p2List] ++ [p3List] ++ [p4List] ++ [p6List] ++ [p6List] ++ [p7List] ++ [p8List] ++ [p9List]   
        pLists = [p1List] ++ [p2List] ++ [p3List] ++ [p4List] 
        
        cm1 = sr1 `atV` 0
        cm2 = sr2 `atV` 0
        cm3 = sr3 `atV` 0
        cm4 = sr4 `atV` 0
        -- cm5 = sr5 `atV` 0
        -- cm6 = sr6 `atV` 0
        -- cm7 = sr7 `atV` 0
        -- cm8 = sr8 `atV` 0
        -- cm9 = sr9 `atV` 0
       
        --cms = [cm1, cm2, cm3, cm4, cm5, cm6, cm7, cm8, cm9]
        cms = [cm1, cm2, cm3, cm4]
        
        ---- Sorting Lists ---
        revpLists = reverseOrder pLists
        pOrder = qsort pLists
        sorList = findIndices (\l -> l <= pOrder !! 0) pOrder

        out = cms !! head sorList
        out1 = out `atV` 0

        
-- MAIN
----------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do

    -- modify begin
    -- use forkio to create 4 threads, one for each zipwithxsy
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
    m3 <- newEmptyMVar
    m4 <- newEmptyMVar
    m5 <- newEmptyMVar
    m6 <- newEmptyMVar
    m7 <- newEmptyMVar
    m8 <- newEmptyMVar
    m9 <- newEmptyMVar

    ----- Dataset Arrangement---------------------------------------------------------------------------------------------
    let dimx =500; dimy = 500

    test <- openFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/SampleData/test6/Itest6.dat" ReadMode; contentsTest <- hGetContents test
    let
        (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest
        st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st])

    timeParallelStart <- getCurrentTime

    tid1 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 1"
        ref1 <- openFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/SampleData/test6/Iref6A.dat" ReadMode; contentsRef1 <- hGetContents ref1
        let
            (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1
            sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])
            u1 = vector [intest,inref1]
            m = zipWithxSY (procAR1 dimx dimy) u1
        writeFile "IPC2/proc1.txt" (show m)
        putMVar m1 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 1 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid2 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 2"
        ref2 <- openFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/SampleData/test6/Iref6B.dat" ReadMode; contentsRef2 <- hGetContents ref2
        let
            (dimX2, dimY2, imageStreamRef2) = readDat contentsRef2; inrefMat2 = matrix dimX2 dimY2 imageStreamRef2
            sr2 = signal [inrefMat2]; inref2 = mapSY (chunks dimx dimy)  (signal [sr2])
            u2 = vector [intest,inref2]
            m = zipWithxSY (procAR1 dimx dimy) u2
        writeFile "IPC2/proc2.txt" (show m)
        putMVar m2 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 2 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid3 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 3"
        ref3 <- openFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/SampleData/test6/Iref6C.dat" ReadMode; contentsRef3 <- hGetContents ref3
        let
            (dimX3, dimY3, imageStreamRef3) = readDat contentsRef3; inrefMat3 = matrix dimX3 dimY3 imageStreamRef3
            sr3 = signal [inrefMat3]; inref3 = mapSY (chunks dimx dimy)  (signal [sr3]) 
            u3 = vector [intest,inref3]
            m = zipWithxSY (procAR1 dimx dimy) u3
        writeFile "IPC2/proc3.txt" (show m)
        putMVar m3 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 3 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid4 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 4"
        ref4 <- openFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/SampleData/test6/Iref6D.dat" ReadMode; contentsRef4 <- hGetContents ref4
        let
            (dimX4, dimY4, imageStreamRef4) = readDat contentsRef4; inrefMat4 = matrix dimX4 dimY4 imageStreamRef4
            sr4 = signal [inrefMat4]; inref4 = mapSY (chunks dimx dimy)  (signal [sr4])
            u4 = vector [intest,inref4]
            m = zipWithxSY (procAR1 dimx dimy) u4
        writeFile "IPC2/proc4.txt" (show m)
        putMVar m4 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 4 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid5 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 5"
        ref5 <- openFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/SampleData/test6/Iref6E.dat" ReadMode; contentsRef5 <- hGetContents ref5
        let
            (dimX5, dimY5, imageStreamRef5) = readDat contentsRef5; inrefMat5 = matrix dimX5 dimY5 imageStreamRef5
            sr5 = signal [inrefMat5]; inref5 = mapSY (chunks dimx dimy)  (signal [sr5])
            u5 = vector [intest,inref5]
            m = zipWithxSY (procAR1 dimx dimy) u5
        writeFile "IPC2/proc5.txt" (show m)
        putMVar m5 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 5 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid6 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 6"
        ref6 <- openFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/SampleData/test6/Iref6F.dat" ReadMode; contentsRef6 <- hGetContents ref6
        let
            (dimX6, dimY6, imageStreamRef6) = readDat contentsRef6; inrefMat6 = matrix dimX6 dimY6 imageStreamRef6
            sr6 = signal [inrefMat6]; inref6 = mapSY (chunks dimx dimy)  (signal [sr6])
            u6 = vector [intest,inref6]
            m = zipWithxSY (procAR1 dimx dimy) u6
        writeFile "IPC2/proc6.txt" (show m)
        putMVar m6 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 6 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid7 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 7"
        ref7 <- openFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/SampleData/test6/Iref6G.dat" ReadMode; contentsRef7 <- hGetContents ref7
        let
            (dimX7, dimY7, imageStreamRef7) = readDat contentsRef7; inrefMat7 = matrix dimX7 dimY7 imageStreamRef7
            sr7 = signal [inrefMat7]; inref7 = mapSY (chunks dimx dimy)  (signal [sr7])
            u7 = vector [intest,inref7]
            m = zipWithxSY (procAR1 dimx dimy) u7
        writeFile "IPC2/proc7.txt" (show m)
        putMVar m7 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 7 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid8 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 8"
        ref8 <- openFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/SampleData/test6/Iref6H.dat" ReadMode; contentsRef8 <- hGetContents ref8
        let
            (dimX8, dimY8, imageStreamRef8) = readDat contentsRef8; inrefMat8 = matrix dimX8 dimY8 imageStreamRef8
            sr8 = signal [inrefMat8]; inref8 = mapSY (chunks dimx dimy)  (signal [sr8])
            u8 = vector [intest,inref8]
            m = zipWithxSY (procAR1 dimx dimy) u8
        writeFile "IPC2/proc8.txt" (show m)
        putMVar m8 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 8 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid9 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 9"
        ref9 <- openFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/SampleData/test6/Iref6I.dat" ReadMode; contentsRef9 <- hGetContents ref9
        let
            (dimX9, dimY9, imageStreamRef9) = readDat contentsRef9; inrefMat9 = matrix dimX9 dimY9 imageStreamRef9
            sr9 = signal [inrefMat9]; inref9 = mapSY (chunks dimx dimy)  (signal [sr9])
            u9 = vector [intest,inref9]
            m = zipWithxSY (procAR1 dimx dimy) u9
        writeFile "IPC2/proc9.txt" (show m)
        putMVar m9 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 9 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)


    m1 <- takeMVar m1
    m2 <- takeMVar m2
    m3 <- takeMVar m3
    m4 <- takeMVar m4
    m5 <- takeMVar m5
    m6 <- takeMVar m6
    m7 <- takeMVar m7
    m8 <- takeMVar m8
    m9 <- takeMVar m9

    threadDelay 10
    -- modify end

    -- MC Probabilities Matrix  (skeleton)
    let out = vector [intest,m1,intest, m2, intest,m3,intest, m4, intest, m5, intest, m6, intest, m7, intest, m8, intest, m9]
    let res = zipWithxSY (probMatrix dimx dimy) out



    ----- Output File ------------------------------------------------------------------------------------------------
    writeFile "/Users/dairuijia/Documents/EmbeddedSystemsProject/code/ChangeDetection/Demos/Out/CD0.txt" (show res)
    
    -- RUN CODE USING THE TERMINAL :
    -- sudo apt install threadscope
    --  ghc -O2 multi_thread.hs -threaded -rtsopts -eventlog
    -- ./multi_thread multi_thread.1.txt +RTS -N4 -ls; threadscope Dai.eventlog


    











