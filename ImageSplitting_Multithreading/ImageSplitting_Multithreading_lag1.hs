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
import Control.Concurrent
import GHC.Conc (labelThread)
import Data.Time
-- import System.Posix.Process (forkProcess, getProcessStatus)
-- import System.Posix.Types (ProcessID)



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
addMatrix a b = zipWithV (zipWithV (\x y -> x+y)) a b

subMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
subMatrix a b = zipWithV (zipWithV (\x y -> x-y)) a b

avgMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
avgMatrix a b = zipWithV (zipWithV (\x y -> (x+y)/2)) a b


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

        hUP = midspread normalUnbiased 3 (U.fromList a) 
        hDOWN = midspread normalUnbiased 4 (U.fromList a)


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

procAR1 :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)
procAR1 dimx dimy dat = res
        where

        t = dat `atV` 0; st = fromSignal t !! 0
        
        ref1 = dat `atV` 1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        sv = signal [ssr1]

        m = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> arSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        m1 = fromSignal m !! 0; cm1 = m1 `atV` 0

        res = zipWithMat(\ x y -> subMatrix x y) st cm1
        



-- Function to apply necessary operations for each index
extractSignal :: Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double))) -> ForSyDe.Shallow.Vector (ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double))
extractSignal idx datSignal = vector [fromSignal (datSignal `atV` idx) !! 0]

procMatrix :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double) 
procMatrix dimx dimy dat = res
        where

        t = dat `atV` 0; st = fromSignal t !! 0

        -- -- OLD CODE
        -- ref1 = dat `atV`  1; sr1 = fromSignal ref1 !! 0; ssr1 = vector [sr1]
        -- ref2 = dat `atV`  3; sr2 = fromSignal ref2 !! 0; ssr2 = vector [sr2]
        -- ref3 = dat `atV`  5; sr3 = fromSignal ref3 !! 0; ssr3 = vector [sr3]
        -- ref4 = dat `atV`  7; sr4 = fromSignal ref4 !! 0; ssr4 = vector [sr4]
        -- ref5 = dat `atV`  9; sr5 = fromSignal ref5 !! 0; ssr5 = vector [sr5]
        -- ref6 = dat `atV` 11; sr6 = fromSignal ref6 !! 0; ssr6 = vector [sr6]
        -- ref7 = dat `atV` 13; sr7 = fromSignal ref7 !! 0; ssr7 = vector [sr7]
        -- ref8 = dat `atV` 15; sr8 = fromSignal ref8 !! 0; ssr8 = vector [sr8]
        -- ref9 = dat `atV` 17; sr9 = fromSignal ref9 !! 0; ssr9 = vector [sr9]

        -- sv = signal [ssr1, ssr2, ssr3, ssr4, ssr5, ssr6, ssr7, ssr8, ssr9]

        -- -- NEW CODE:

        -- -- Create a list of indices to extract data
        indices = signal[1]

        -- Create the signal vector using mapSY
        sv = mapSY (`extractSignal` dat) indices

        -- -- END OF NEW CODE

        m = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> arSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        c = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> mcSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) m

        -----------------------------------------------------------------------------------------------------------------------

        
        p1 = fromSignal c !! 0; p1mat = fromMatrix p1 !! 0; p1List = p1mat `atV` 0
        -- p2 = fromSignal c !! 1; p2mat = fromMatrix p2 !! 0; p2List = p2mat `atV` 0
        -- p3 = fromSignal c !! 2; p3mat = fromMatrix p3 !! 0; p3List = p3mat `atV` 0
        -- p4 = fromSignal c !! 3; p4mat = fromMatrix p4 !! 0; p4List = p4mat `atV` 0
        -- p5 = fromSignal c !! 4; p5mat = fromMatrix p5 !! 0; p5List = p5mat `atV` 0
        -- p6 = fromSignal c !! 5; p6mat = fromMatrix p6 !! 0; p6List = p6mat `atV` 0
        -- p7 = fromSignal c !! 6; p7mat = fromMatrix p7 !! 0; p7List = p7mat `atV` 0
        -- p8 = fromSignal c !! 7; p8mat = fromMatrix p8 !! 0; p8List = p8mat `atV` 0
        -- p9 = fromSignal c !! 8; p9mat = fromMatrix p9 !! 0; p9List = p9mat `atV` 0
        pLists = [p1List] -- ++ [p2List] ++ [p3List] ++ [p4List] ++ [p5List]++ [p6List]          
        
        m1 = fromSignal m !! 0; cm1 = m1 `atV` 0
        -- m2 = fromSignal m !! 1; cm2 = m2 `atV` 0
        -- m3 = fromSignal m !! 2; cm3 = m3 `atV` 0
        -- m4 = fromSignal m !! 3; cm4 = m4 `atV` 0
        -- m5 = fromSignal m !! 4; cm5 = m5 `atV` 0
        -- m6 = fromSignal m !! 5; cm6 = m6 `atV` 0
        -- m7 = fromSignal m !! 6; cm7 = m7 `atV` 0
        -- m8 = fromSignal m !! 7; cm8 = m8 `atV` 0
        -- m9 = fromSignal m !! 8; cm9 = m9 `atV` 0

        cms = [cm1]
        
        ---- Sorting Lists ---
        revpLists = reverseOrder pLists
        pOrder = qsort pLists
        sorList = findIndices (\l -> l <= pLists !! 1) revpLists

        arCS0 = cms !! head sorList
        -- arCS1 = cms !! 1
        -- avg = zipWithMat(\ x y -> avgMatrix x y) arCS0 arCS1
        res = zipWithMat(\ x y -> subMatrix x y) st arCS0
        

main :: IO ()
main = do

    -- modify begin
    -- use forkio to create 4 threads, one for each zipwithxsy
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
    m3 <- newEmptyMVar
    m4 <- newEmptyMVar

    -- Partition 1 (250 x 250 segment 1)

    let target = "0"
        mission = "S1"
        
        --C E H J N P -- worst sub-lags
 
    let readpath = "./../SampleData/test" ++ target; 
    let writepath = "./Out/" ++ mission ++ "/Lag1/ImageSplitting" 

        readpath0 = readpath ++ "/Itest" ++ target ++ ".dat"
        readpathA = readpath ++ "/Iref" ++ target ++"A.dat"
        

        writepath1 = writepath ++ "/proc1.txt"
        writepath2 = writepath ++ "/proc2.txt"
        writepath3 = writepath ++ "/proc3.txt"
        writepath4 = writepath ++ "/proc4.txt"



    test <- openFile readpath0 ReadMode; contentsTest <- hGetContents test

    ref1 <- openFile readpathA ReadMode; contentsRef1 <- hGetContents ref1


     ----- Dataset Arrangement---------------------------------------------------------------------------------------------

    let dimx =250; dimy = 250
        (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest 
        (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1
       
        st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st]) 
        sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])
    
        
    let n_test = fromSignal m_test !! 0; m_test = fromSignal intest !! 0
    let n_1 = fromSignal m_1 !! 0; m_1 = fromSignal inref1 !! 0

 

    timeParallelStart <- getCurrentTime

    pid1 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 1"

        let subImage1 = atMat 0 0 n_test;st1 = signal [subImage1]
            subRef1_1 = atMat 0 0 n_1;sr1_1 = signal [subRef1_1]
      
            test_1 = mapSY (chunks dimx dimy)  (signal [st1])
            ref1_1 = mapSY (chunks dimx dimy)  (signal [sr1_1])
     
            u_1 = vector [test_1,ref1_1]

        let result = zipWithxSY (procAR1 dimx dimy) u_1
            out = fromSignal result !! 0; mout = fromMatrix out !! 0
            sf = mapSY (spatialFilter dimx dimy) (signal [mout]) -- Spatial Filtering
            output = mapSY (anomaly dimx dimy) (signal [sf])      -- anomaly detection

        writeFile writepath1 (show output)
        putMVar m1 output
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 1 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
      

    pid2 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 2"

        let subImage2 = atMat 0 1 n_test;st2 = signal [subImage2]
            subRef1_2 = atMat 0 1 n_1;sr1_2 = signal [subRef1_2]

            test_2 = mapSY (chunks dimx dimy)  (signal [st2])
            ref1_2 = mapSY (chunks dimx dimy)  (signal [sr1_2])

            u_2 = vector [test_2,ref1_2]

        let result = zipWithxSY (procAR1 dimx dimy) u_2
            out = fromSignal result !! 0; mout = fromMatrix out !! 0
            sf = mapSY (spatialFilter dimx dimy) (signal [mout]) -- Spatial Filtering
            output = mapSY (anomaly dimx dimy) (signal [sf])      -- anomaly detection
        writeFile writepath2 (show output)
        putMVar m2 result
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 2 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
       

    pid3 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 3"

        let subImage3 = atMat 1 0 n_test;st3 = signal [subImage3]
            subRef1_3 = atMat 1 0 n_1;sr1_3 = signal [subRef1_3]

            test_3 = mapSY (chunks dimx dimy)  (signal [st3])
            ref1_3 = mapSY (chunks dimx dimy)  (signal [sr1_3])            

            u_3 = vector [test_3,ref1_3]

        let result = zipWithxSY (procAR1 dimx dimy) u_3
            out = fromSignal result !! 0; mout = fromMatrix out !! 0
            sf = mapSY (spatialFilter dimx dimy) (signal [mout]) -- Spatial Filtering
            output = mapSY (anomaly dimx dimy) (signal [sf])      -- anomaly detection
        writeFile writepath3 (show output)
        putMVar m3 result
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 3 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
       

    pid4 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 4"

        let subImage4 = atMat 1 1 n_test;st4 = signal [subImage4]
            subRef1_4 = atMat 1 1 n_1;sr1_4 = signal [subRef1_4]
           
            test_4 = mapSY (chunks dimx dimy)  (signal [st4])
            ref1_4 = mapSY (chunks dimx dimy)  (signal [sr1_4])

            u_4 = vector [test_4,ref1_4]


        let result = zipWithxSY (procAR1 dimx dimy) u_4
            out = fromSignal result !! 0; mout = fromMatrix out !! 0
            sf = mapSY (spatialFilter dimx dimy) (signal [mout]) -- Spatial Filtering
            output = mapSY (anomaly dimx dimy) (signal [sf])      -- anomaly detection
        writeFile writepath4 (show output)
        putMVar m4 result
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 4 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
    


    m1 <- takeMVar m1
    m2 <- takeMVar m2
    m3 <- takeMVar m3
    m4 <- takeMVar m4

    threadDelay 10

    print "Done"

        -- RUN CODE USING THE TERMINAL :
    -- sudo apt install threadscope
    --  ghc -O2 MT6par.hs -threaded -rtsopts -eventlog
    -- ./MT6par MT6par.1.txt +RTS -N4 -ls; threadscope MT6par.eventlog

    -- ghci >> :set -package unix
    -- ghci >> :l MT6par.hs
    -- ghci >> main
