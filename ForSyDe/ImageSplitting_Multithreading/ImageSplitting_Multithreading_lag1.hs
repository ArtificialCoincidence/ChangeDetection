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
import qualified Data.Vector.Unboxed as U
import Data.Massiv.Array
import Control.Concurrent
import GHC.Conc (labelThread)
import Data.Time


-- First-order Autoregressive Model [AR(1)]: (ELEMENTARY MODEL)
---------------------------------------------------------------------------------------------------------------

arSys :: Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
arSys y_initial x_n = out
    where
    out 
     | fromSignal rho !! 0 < 0.5 = addMatrixProc (x') (y_delayed')
     | otherwise         = x_n
    
    x' = scaleProc (rho) (x_n)
    y_delayed' = scale'Proc (rho) (y_delayed)
    y_delayed = delaySY r out -- Somehow the initial state has to come from something that is not a process, is this when we have more lags? can it be removed? What does it do now?
    r = fromSignal y_initial !! 0 --This has to be changed, cant have fromsignal here (need Marcello to explain how this delay works. I dont think we need this as long as we only use 1 lag, have to check again)
    rho = pearsonProc (x_n) (y_delayed)
 
    -- p-MC: MARKOV-CHAIN transition probabilities Pkj
---------------------------------------------------------------------------------------------------------------

        
         
-- Auxiliar: AR recursion
---------------------------------------------------------------------------------------------------------------

scale :: Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -- This returns something weird (it becomes like two matrices?)
scale rho matrix = mapV (mapV f) matrix
  where f x = rho * x

scale' :: Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
scale' rho matrix = mapV (mapV f) matrix
  where f x = sqrt(1 - rho**2) * x

scaleProc :: Signal Double -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
scaleProc = zipWithSY scale 

scale'Proc :: Signal Double -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
scale'Proc = zipWithSY scale'

addMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
addMatrix a b = zipWithV (zipWithV (\x y -> if x > y then x+y  else 0)) a b

addMatrixProc :: Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
addMatrixProc = zipWithSY addMatrix

subMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
subMatrix a b = zipWithV (zipWithV (\x y -> x-y)) a b

avgMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
avgMatrix a b = zipWithV (zipWithV (\x y -> (x+y)/2)) a b


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


-- Function to apply necessary operations for each index
extractSignal :: Int -> ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double))) -> ForSyDe.Shallow.Vector (ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double))
extractSignal idx datSignal = vector [fromSignal (datSignal `atV` idx) !! 0]

-- Function computing first-order autoregreassive model and the Markov chain correlation for 3 lag

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

processChain :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double)
processChain dimx dimy x = anomalyProc' dimx dimy (spatialFilterProc' dimx dimy x)

markovProc :: ForSyDe.Shallow.Matrix (Signal (ForSyDe.Shallow.Matrix (Double))) -> ForSyDe.Shallow.Matrix (Signal (ForSyDe.Shallow.Matrix (Double))) -> Signal Double
markovProc m intest = c
    where
        cMat = zipWithMat (pearsonProc) m intest
        c = reduceMat (zipWithSY (+)) cMat

choose :: ForSyDe.Shallow.Vector (ForSyDe.Shallow.Matrix  Double) -> ForSyDe.Shallow.Matrix Double ->ForSyDe.Shallow.Vector Double -> ForSyDe.Shallow.Matrix Double
choose mVec st cVec = res
    where
        pLists = fromVector cVec
        cms = fromVector mVec
        revpLists = reverseOrder pLists
        sorList = findIndices (\l -> l <= pLists !! 0) revpLists

        arCS0 = cms !! head sorList
        res = subMatrix st arCS0


chooseProc :: Signal (ForSyDe.Shallow.Vector (ForSyDe.Shallow.Matrix Double)) -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Vector Double) -> Signal (ForSyDe.Shallow.Matrix Double)
chooseProc mVec st cVec = out
    where out = zipWith3SY choose mVec st cVec

getMat :: Int -> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double) -> ForSyDe.Shallow.Matrix Double
getMat indx input = fromMatrix input !! indx

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

    let readpath = "SampleData/test" ++ target; 
    let writepath = "Out/" ++ mission ++ "/Lag3/ImageSplitting" 

        readpath0 = readpath ++ "/Itest" ++ target ++ ".dat"
        readpathA = readpath ++ "/Iref" ++ target ++"A.dat"
        readpathF = readpath ++ "/Iref" ++ target ++"F.dat"
        readpathG = readpath ++ "/Iref" ++ target ++"G.dat"



        writepath1 = writepath ++ "/proc1.txt"
        writepath2 = writepath ++ "/proc2.txt"
        writepath3 = writepath ++ "/proc3.txt"
        writepath4 = writepath ++ "/proc4.txt"



    test <- openFile readpath0 ReadMode; contentsTest <- hGetContents test
    ref1 <- openFile readpathA ReadMode; contentsRef1 <- hGetContents ref1
    --ref3 <- openFile readpathF ReadMode; contentsRef3 <- hGetContents ref3
    --ref5 <- openFile readpathG ReadMode; contentsRef5 <- hGetContents ref5



     ----- Dataset Arrangement---------------------------------------------------------------------------------------------

    let dimx =250; dimy = 250
        (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest 
        (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1
        --(dimX3, dimY3, imageStreamRef3) = readDat contentsRef3; inrefMat3 = matrix dimX1 dimY1 imageStreamRef3
        --(dimX5, dimY5, imageStreamRef5) = readDat contentsRef5; inrefMat5 = matrix dimX1 dimY1 imageStreamRef5
       
       
        st = signal [intestMat];  intest = chunks dimx dimy st 
        sr1 = signal [inrefMat1]; inref1 = chunks dimx dimy sr1
        --sr3 = signal [inrefMat3]; inref3 = chunks dimx dimy sr3
        --sr5 = signal [inrefMat5]; inref5 = chunks dimx dimy sr5

    timeParallelStart <- getCurrentTime

    -- Creating 4 threads, one for each sub image

    -- First thread
    pid1 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 1"

        -- Image splitting

        let st1 = mapSY (getMat 0) intest --getMat extracts the submatrix m_i where i = [1,m*n] for matrix with size m*n
            sr1_1 = mapSY (getMat 0) inref1
            --sr2_1 = mapSY (getMat 0) inref3
            --sr3_1 = mapSY (getMat 0) inref5

            sv1 = vector [sr1_1] --, sr2_1, sr3_1] --Put each sub-image into a vector for paralelization

            

        -- Computations, first-order AR model, Markov chain correlation, filtering and anomaly detection

        let m = mapV (arSys  st1) sv1 --arSys and st1 is applied (as parallel processes) to each element in sv1
            c = mapV (pearsonProc st1) m
            msig = zipxSY m
            csig = zipxSY c
            res = chooseProc msig st1 csig
            output =  processChain dimx dimy res

        writeFile writepath1 (show output)
        putMVar m1 output
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 1 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
      
    -- Second thread
    pid2 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 2"

        -- Image Splitting

        let st2 = mapSY (getMat 1) intest --getMat extracts the submatrix m_i where i = [1,m*n] for matrix with size m*n
            sr1_2 = mapSY (getMat 1) inref1
            --sr2_2 = mapSY (getMat 1) inref3
            --sr3_2 = mapSY (getMat 1) inref5

            sv2 = vector [sr1_2] --, sr2_2, sr3_2]

        -- Computations, first-order AR model, Markov chain correlation, filtering and anomaly detection

        let m = mapV (arSys  st2) sv2 --arSys and st1 is applied (as parallel processes) to each element in sv1
            c = mapV (pearsonProc st2) m
            msig = zipxSY m
            csig = zipxSY c
            res = chooseProc msig st2 csig
            output =  processChain dimx dimy res

        writeFile writepath2 (show output)
        putMVar m2 output
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 2 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
       
    -- Third thread
    pid3 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 3"

        -- Image Splitting

        let st3 = mapSY (getMat 2) intest --getMat extracts the submatrix m_i where i = [1,m*n] for matrix with size m*n
            sr1_3 = mapSY (getMat 2) inref1
            --sr2_3 = mapSY (getMat 2) inref3
            --sr3_3 = mapSY (getMat 2) inref5

            sv3 = vector [sr1_3] --, sr2_3, sr3_3]

        -- Computations, first-order AR model, Markov chain correlation, filtering and anomaly detection

        let m = mapV (arSys  st3) sv3 --arSys and st1 is applied (as parallel processes) to each element in sv1
            c = mapV (pearsonProc st3) m
            msig = zipxSY m
            csig = zipxSY c
            res = chooseProc msig st3 csig
            output =  processChain dimx dimy res

        writeFile writepath3 (show output)
        putMVar m3 output
        
        timeParallelEnd <- getCurrentTime
        putStrLn $ "parallelism 3 done, execution time: " ++ show(diffUTCTime timeParallelEnd timeParallelStart)
       
    -- Fourth thread
    pid4 <- forkIO $ do
        myTid <- myThreadId
        labelThread myTid "parallelism 4"

        -- Image Splitting

        let st4 = mapSY (getMat 3) intest --getMat extracts the submatrix m_i where i = [1,m*n] for matrix with size m*n
            sr1_4 = mapSY (getMat 3) inref1
            --sr2_4 = mapSY (getMat 3) inref3
            --sr3_4 = mapSY (getMat 3) inref5

            sv4 = vector [sr1_4] --, sr2_4, sr3_4]

        -- Computations, first-order AR model, Markov chain correlation, filtering and anomaly detection

        let m = mapV (arSys  st4) sv4 --arSys and st1 is applied (as parallel processes) to each element in sv1
            c = mapV (pearsonProc st4) m
            msig = zipxSY m
            csig = zipxSY c
            res = chooseProc msig st4 csig
            output =  processChain dimx dimy res

        writeFile writepath4 (show output)
        putMVar m4 output
        
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
    -- Then compile the haskell code: ghc -O2 ImageSplitting_Multithreading_lag3.hs -threaded -rtsopts -eventlog
    -- Run the executable: ./ImageSplitting_Multithreading_lag3 ImageSplitting_Multithreading_lag3.1.txt +RTS -N4 -ls; threadscope ImageSplitting_Multithreading_lag3.eventlog