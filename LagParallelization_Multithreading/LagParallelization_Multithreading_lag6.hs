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
arSystem dimx dimy y_initial x_n = fromSignal out !! 0
   where 
    out 
   
     | rho > 0.5 =  zipWithSY addMatrix x' y_delayed'
     | rho > -0.5 =  zipWithSY addMatrix x' y_delayed'
     | otherwise         = x_n
         
    nrho = zipWithSY pearson (signal [x_n]) (signal [y_initial])
    rho = fromSignal nrho !! 0
    x'         = mapSY (scale rho) (x_n)
    y_delayed' = mapSY (scale' rho) y_delayed
    r = fromSignal y_initial !! 0
    y_delayed  = delaySY r out


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

subMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
subMatrix a b = zipWithV (zipWithV (\x y -> x-y)) a b


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
spatialFilter :: Int -> Int -> ForSyDe.Shallow.Matrix Double  -> ForSyDe.Shallow.Matrix Double  
spatialFilter dimx dimy img = matrix dimx dimy $ toList $ dropWindow $ mapStencil Edge (avgStencil 9) barImg
    where

        --y_n' = fromSignal img  !! 0
        
        imG = fromMatrix img
        
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
        

-- probMatrix :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> Signal (ForSyDe.Shallow.Matrix Double)
-- probMatrix dimx dimy dat = signal [out1]
--         where

--         t = dat `atV` 0; st = fromSignal t !! 0
--         ref1 = dat `atV` 1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
--         ref2 = dat `atV` 3; sr2 = fromSignal ref2 !! 0; ssr2 = vector [sr2]
--         ref3 = dat `atV` 5; sr3 = fromSignal ref3 !! 0; ssr3 = vector [sr3]
--         ref4 = dat `atV` 7; sr4 = fromSignal ref4 !! 0; ssr4 = vector [sr4]
--         ref5 = dat `atV` 9; sr5 = fromSignal ref5 !! 0; ssr5 = vector [sr5]
--         ref6 = dat `atV` 11; sr6 = fromSignal ref6 !! 0; ssr6 = vector [sr6]
--         -- ref7 = dat `atV` 13; sr7 = fromSignal ref7 !! 0; ssr7 = vector [sr7]
--         -- ref8 = dat `atV` 15; sr8 = fromSignal ref8 !! 0; ssr8 = vector [sr8]
--         -- ref9 = dat `atV` 17; sr9 = fromSignal ref9 !! 0; ssr9 = vector [sr9]

    
--         --sv = signal [ssr1, ssr2, ssr3, ssr4, ssr5, ssr6, ssr7, ssr8, ssr9]  
--         sv = signal [ssr1, ssr2, ssr3, ssr4, ssr5, ssr6]
--         c = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> mcSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv

--         p1 = fromSignal c !! 0; p1mat = fromMatrix p1 !! 0; p1List = p1mat `atV` 0
--         p2 = fromSignal c !! 1; p2mat = fromMatrix p2 !! 0; p2List = p2mat `atV` 0
--         p3 = fromSignal c !! 2; p3mat = fromMatrix p3 !! 0; p3List = p3mat `atV` 0
--         p4 = fromSignal c !! 3; p4mat = fromMatrix p4 !! 0; p4List = p4mat `atV` 0
--         p5 = fromSignal c !! 4; p5mat = fromMatrix p5 !! 0; p5List = p5mat `atV` 0
--         p6 = fromSignal c !! 5; p6mat = fromMatrix p6 !! 0; p6List = p6mat `atV` 0
--         -- p7 = fromSignal c !! 6; p7mat = fromMatrix p7 !! 0; p7List = p7mat `atV` 0
--         -- p8 = fromSignal c !! 7; p8mat = fromMatrix p8 !! 0; p8List = p8mat `atV` 0
--         -- p9 = fromSignal c !! 8; p9mat = fromMatrix p9 !! 0; p9List = p9mat `atV` 0
       
--         --pLists = [p1List] ++ [p2List] ++ [p3List] ++ [p4List] ++ [p5List] ++ [p6List] ++ [p7List] ++ [p8List] ++ [p9List]   
--         pLists = [p1List] ++ [p2List] ++ [p3List] ++ [p4List] ++ [p5List] ++ [p6List]
        
--         cm1 = sr1 `atV` 0
--         cm2 = sr2 `atV` 0
--         cm3 = sr3 `atV` 0
--         cm4 = sr4 `atV` 0
--         cm5 = sr5 `atV` 0
--         cm6 = sr6 `atV` 0
--         -- cm7 = sr7 `atV` 0
--         -- cm8 = sr8 `atV` 0
--         -- cm9 = sr9 `atV` 0
       
--         --cms = [cm1, cm2, cm3, cm4, cm5, cm6, cm7, cm8, cm9]
--         cms = [cm1, cm2, cm3, cm4, cm5, cm6]
        
--         ---- Sorting Lists ---
--         revpLists = reverseOrder pLists
--         pOrder = qsort pLists
--         sorList = findIndices (\l -> l <= pOrder !! 0) pOrder

--         out = cms !! head sorList
--         out1 = out `atV` 0

procMatrix :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> ForSyDe.Shallow.Matrix Double
procMatrix dimx dimy dat = fromMatrix res !! 0 
        where

        t = dat `atV` 0; st = fromSignal t !! 0

        ref1 = dat `atV` 1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        ref2 = dat `atV` 3; sr2 = fromSignal ref2 !! 0; ssr2 = vector [sr2]
        ref3 = dat `atV` 5; sr3 = fromSignal ref3 !! 0; ssr3 = vector [sr3]
        ref4 = dat `atV` 7; sr4 = fromSignal ref4 !! 0; ssr4 = vector [sr4]
        ref5 = dat `atV` 9; sr5 = fromSignal ref5 !! 0; ssr5 = vector [sr5]
        ref6 = dat `atV` 11; sr6 = fromSignal ref6 !! 0; ssr6 = vector [sr6]
        -- ref7 = dat `atV` 13; sr7 = fromSignal ref7 !! 0; ssr7 = vector [sr7]
        -- ref8 = dat `atV` 15; sr8 = fromSignal ref8 !! 0; ssr8 = vector [sr8]
        -- ref9 = dat `atV` 17; sr9 = fromSignal ref9 !! 0; ssr9 = vector [sr9]

        sv = signal [ssr1, ssr2, ssr3, ssr4, ssr5, ssr6]

        c = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> mcSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        

        -----------------------------------------------------------------------------------------------------------------------

        
        p1 = fromSignal c !! 0; p1mat = fromMatrix p1 !! 0; p1List = p1mat `atV` 0
        p2 = fromSignal c !! 1; p2mat = fromMatrix p2 !! 0; p2List = p2mat `atV` 0
        p3 = fromSignal c !! 2; p3mat = fromMatrix p3 !! 0; p3List = p3mat `atV` 0
        p4 = fromSignal c !! 3; p4mat = fromMatrix p4 !! 0; p4List = p4mat `atV` 0
        p5 = fromSignal c !! 4; p5mat = fromMatrix p5 !! 0; p5List = p5mat `atV` 0
        p6 = fromSignal c !! 5; p6mat = fromMatrix p6 !! 0; p6List = p6mat `atV` 0
        -- p7 = fromSignal c !! 6; p7mat = fromMatrix p7 !! 0; p7List = p7mat `atV` 0
        -- p8 = fromSignal c !! 7; p8mat = fromMatrix p8 !! 0; p8List = p8mat `atV` 0
        -- p9 = fromSignal c !! 8; p9mat = fromMatrix p9 !! 0; p9List = p9mat `atV` 0
        pLists = [p1List] ++ [p2List] ++ [p3List] ++ [p4List] ++ [p5List]++ [p6List] -- ++ [p7List] ++ [p8List]++ [p9List]         
        
        
        cms = [sr1, sr2, sr3, sr4, sr5, sr6]
        
        ---- Sorting Lists ---
        revpLists = reverseOrder pLists
        sorList = findIndices (\l -> l <= pLists !! 0) revpLists

        arCS = cms !! head sorList
        res = zipWithMat(\ x y -> subMatrix x y) st arCS
        
-- MAIN
----------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do

    -- choose read and write filepath for the target
    let target = "12"
    -- let readpath = "./../SampleData/test0"; writepath = "./Out/S1/Lag6/LagParallelization/CD0.txt" -- S1
    -- let readpath = "./../SampleData/test6"; writepath = "./Out/K1/Lag6/LagParallelization/CD0.txt" -- K1
    let readpath = "./../SampleData/test12"; writepath = "./Out/F1/Lag6/LagParallelization/CD0.txt" -- F1
    -- let readpath = "./../SampleData/test18"; writepath = "./Out/AF1/Lag6/LagParallelization/CD0.txt" -- AF1

    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
    m3 <- newEmptyMVar
    m4 <- newEmptyMVar
    m5 <- newEmptyMVar
    m6 <- newEmptyMVar

    ----- Dataset Arrangement---------------------------------------------------------------------------------------------
    let dimx =500; dimy = 500
    let readpath0 = readpath ++ "/Itest" ++ target ++ ".dat"

    test <- openFile readpath0 ReadMode; contentsTest <- hGetContents test
    let
        (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest
        st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st])

    timeParallelStart <- getCurrentTime

    tid1 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 1"
        let readpatha = readpath ++ "/Iref" ++ target ++ "A.dat"
        ref1 <- openFile readpatha ReadMode; contentsRef1 <- hGetContents ref1
        let
            (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1
            sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])
            u1 = vector [intest,inref1]
            m = zipWithxSY (procAR1 dimx dimy) u1
        putMVar m1 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 1 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid2 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 2"
        let readpathb = readpath ++ "/Iref" ++ target ++ "F.dat"
        ref2 <- openFile readpathb ReadMode; contentsRef2 <- hGetContents ref2
        let
            (dimX2, dimY2, imageStreamRef2) = readDat contentsRef2; inrefMat2 = matrix dimX2 dimY2 imageStreamRef2
            sr2 = signal [inrefMat2]; inref2 = mapSY (chunks dimx dimy)  (signal [sr2])
            u2 = vector [intest,inref2]
            m = zipWithxSY (procAR1 dimx dimy) u2
        putMVar m2 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 2 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid3 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 3"
        let readpathc = readpath ++ "/Iref" ++ target ++ "G.dat"
        ref3 <- openFile readpathc ReadMode; contentsRef3 <- hGetContents ref3
        let
            (dimX3, dimY3, imageStreamRef3) = readDat contentsRef3; inrefMat3 = matrix dimX3 dimY3 imageStreamRef3
            sr3 = signal [inrefMat3]; inref3 = mapSY (chunks dimx dimy)  (signal [sr3]) 
            u3 = vector [intest,inref3]
            m = zipWithxSY (procAR1 dimx dimy) u3
        putMVar m3 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 3 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid4 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 4"
        let readpathd = readpath ++ "/Iref" ++ target ++ "L.dat"
        ref4 <- openFile readpathd ReadMode; contentsRef4 <- hGetContents ref4
        let
            (dimX4, dimY4, imageStreamRef4) = readDat contentsRef4; inrefMat4 = matrix dimX4 dimY4 imageStreamRef4
            sr4 = signal [inrefMat4]; inref4 = mapSY (chunks dimx dimy)  (signal [sr4])
            u4 = vector [intest,inref4]
            m = zipWithxSY (procAR1 dimx dimy) u4
        putMVar m4 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 4 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid5 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 5"
        let readpathe = readpath ++ "/Iref" ++ target ++ "M.dat"
        ref5 <- openFile readpathe ReadMode; contentsRef5 <- hGetContents ref5
        let
            (dimX5, dimY5, imageStreamRef5) = readDat contentsRef5; inrefMat5 = matrix dimX5 dimY5 imageStreamRef5
            sr5 = signal [inrefMat5]; inref5 = mapSY (chunks dimx dimy)  (signal [sr5])
            u5 = vector [intest,inref5]
            m = zipWithxSY (procAR1 dimx dimy) u5
        putMVar m5 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 5 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    tid6 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 6"
        let readpathf = readpath ++ "/Iref" ++ target ++ "R.dat"
        ref6 <- openFile readpathf ReadMode; contentsRef6 <- hGetContents ref6
        let
            (dimX6, dimY6, imageStreamRef6) = readDat contentsRef6; inrefMat6 = matrix dimX6 dimY6 imageStreamRef6
            sr6 = signal [inrefMat6]; inref6 = mapSY (chunks dimx dimy)  (signal [sr6])
            u6 = vector [intest,inref6]
            m = zipWithxSY (procAR1 dimx dimy) u6
        putMVar m6 m
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 6 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)

    m1 <- takeMVar m1
    m2 <- takeMVar m2
    m3 <- takeMVar m3
    m4 <- takeMVar m4
    m5 <- takeMVar m5
    m6 <- takeMVar m6

    -- MC Probabilities Matrix  (skeleton)
    -- let out = vector [intest,m1,intest, m2, intest,m3,intest, m4, intest, m5, intest, m6]
    -- let res = zipWithxSY (probMatrix dimx dimy) out
    let t = fromSignal intest !! 0
    let out = vector [intest,m1,intest,m2, intest,m3, intest, m4, intest, m5, intest, m6]
    let res = zipWithxSY (procMatrix dimx dimy) out
        sf = mapSY (spatialFilter dimx dimy) res -- Spatial Filtering
        output = mapSY (anomaly dimx dimy) (signal [sf])     -- anomaly detection



    ----- Output File ------------------------------------------------------------------------------------------------
    writeFile writepath (show res)
    
    -- RUN CODE USING THE TERMINAL :
    --  ghc -O2 multi_thread_lag6.hs -threaded -rtsopts -eventlog
    -- ./multi_thread_lag6 +RTS -N6 -ls; threadscope multi_thread_lag6.eventlog