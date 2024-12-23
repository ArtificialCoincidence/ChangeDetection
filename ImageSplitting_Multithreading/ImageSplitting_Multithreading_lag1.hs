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

-- Function computing only the first-order autoregressive model for lag 1
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

-- Function computing first-order autoregreassive model and the Markov chain correlation for 1 lag
procMatrix :: Int -> Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double) 
procMatrix dimx dimy dat = res
        where

        t = dat `atV` 0; st = fromSignal t !! 0

        -- -- Create a list of indices to extract data
        indices = signal[1]

        -- Create the signal vector using mapSY
        sv = mapSY (`extractSignal` dat) indices

        -- Computing AR(1) and p-MC
        m = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> arSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) sv
        c = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> mcSystem dimx dimy (signal [y]) (signal [x]) ) st )) . unzipxSY) m

        p1 = fromSignal c !! 0; p1mat = fromMatrix p1 !! 0; p1List = p1mat `atV` 0

        pLists = [p1List]          
        
        m1 = fromSignal m !! 0; cm1 = m1 `atV` 0

        cms = [cm1]
        
        ---- Sorting Lists ---
        revpLists = reverseOrder pLists
        pOrder = qsort pLists
        sorList = findIndices (\l -> l <= pLists !! 1) revpLists

        arCS0 = cms !! head sorList

        res = zipWithMat(\ x y -> subMatrix x y) st arCS0
        

main :: IO ()
main = do

    -- Inter thread communication variables
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
    m3 <- newEmptyMVar
    m4 <- newEmptyMVar

    let target = "0"
        mission = "S1"
        
    --C E H J N P -- worst sub-lags
 
    -- Preparing the read and write file paths

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

    -- Creating 4 threads, one for each sub image

    -- First thread
    pid1 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 1"

        -- Image splitting
    
        let subImage1 = atMat 0 0 n_test;st1 = signal [subImage1]
            subRef1_1 = atMat 0 0 n_1;sr1_1 = signal [subRef1_1]
      
            test_1 = mapSY (chunks dimx dimy)  (signal [st1])
            ref1_1 = mapSY (chunks dimx dimy)  (signal [sr1_1])
     
            u_1 = vector [test_1,ref1_1]

        -- Computations, first-order AR model, Markov chain correlation, filtering and anomaly detection

        let result = zipWithxSY (procMatrix dimx dimy) u_1
            out = fromSignal result !! 0; mout = fromMatrix out !! 0
            sf = mapSY (spatialFilter dimx dimy) (signal [mout]) -- Spatial Filtering
            output = mapSY (anomaly dimx dimy) (signal [sf])      -- anomaly detection

        writeFile writepath1 (show output)
        putMVar m1 output
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 1 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
      
    -- Second thread
    pid2 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 2"

        -- Image Splitting

        let subImage2 = atMat 0 1 n_test;st2 = signal [subImage2]
            subRef1_2 = atMat 0 1 n_1;sr1_2 = signal [subRef1_2]

            test_2 = mapSY (chunks dimx dimy)  (signal [st2])
            ref1_2 = mapSY (chunks dimx dimy)  (signal [sr1_2])

            u_2 = vector [test_2,ref1_2]

        -- Computations, first-order AR model, Markov chain correlation, filtering and anomaly detection

        let result = zipWithxSY (procMatrix dimx dimy) u_2
            out = fromSignal result !! 0; mout = fromMatrix out !! 0
            sf = mapSY (spatialFilter dimx dimy) (signal [mout]) -- Spatial Filtering
            output = mapSY (anomaly dimx dimy) (signal [sf])      -- anomaly detection
        writeFile writepath2 (show output)
        putMVar m2 result
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 2 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
       
    -- Third thread
    pid3 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 3"

        -- Image Splitting

        let subImage3 = atMat 1 0 n_test;st3 = signal [subImage3]
            subRef1_3 = atMat 1 0 n_1;sr1_3 = signal [subRef1_3]

            test_3 = mapSY (chunks dimx dimy)  (signal [st3])
            ref1_3 = mapSY (chunks dimx dimy)  (signal [sr1_3])            

            u_3 = vector [test_3,ref1_3]

        -- Computations, first-order AR model, Markov chain correlation, filtering and anomaly detection

        let result = zipWithxSY (procAR1 dimx dimy) u_3
            out = fromSignal result !! 0; mout = fromMatrix out !! 0
            sf = mapSY (spatialFilter dimx dimy) (signal [mout]) -- Spatial Filtering
            output = mapSY (anomaly dimx dimy) (signal [sf])      -- anomaly detection
        writeFile writepath3 (show output)
        putMVar m3 result
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 3 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
       
    -- Forth thread
    pid4 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 4"

        -- Image Splitting

        let subImage4 = atMat 1 1 n_test;st4 = signal [subImage4]
            subRef1_4 = atMat 1 1 n_1;sr1_4 = signal [subRef1_4]
           
            test_4 = mapSY (chunks dimx dimy)  (signal [st4])
            ref1_4 = mapSY (chunks dimx dimy)  (signal [sr1_4])

            u_4 = vector [test_4,ref1_4]

        -- Computations, first-order AR model, Markov chain correlation, filtering and anomaly detection

        let result = zipWithxSY (procMatrix dimx dimy) u_4
            out = fromSignal result !! 0; mout = fromMatrix out !! 0
            sf = mapSY (spatialFilter dimx dimy) (signal [mout]) -- Spatial Filtering
            output = mapSY (anomaly dimx dimy) (signal [sf])      -- anomaly detection
        writeFile writepath4 (show output)
        putMVar m4 result
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 4 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
    

    -- Joinning the 4 threads
    m1 <- takeMVar m1
    m2 <- takeMVar m2
    m3 <- takeMVar m3
    m4 <- takeMVar m4

    -- Delay added, suggested for better use of the threads
    threadDelay 10

    print "Done"

    
    -- RUN CODE USING THE TERMINAL :
    
    -- First install threadscope: sudo apt install threadscope
    -- Then compile the haskell code: ghc -O2 ImageSplitting_Multithreading_lag1.hs -threaded -rtsopts -eventlog
    -- Run the executable: ./ImageSplitting_Multithreading_lag1 ImageSplitting_Multithreading_lag1.1.txt +RTS -N4 -ls; threadscope ImageSplitting_Multithreading_lag1.eventlog


