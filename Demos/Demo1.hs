--module ChangeDetection where

import ForSyDe.Shallow (signal, fromSignal)
import ForSyDe.Shallow.Core.Vector
import ForSyDe.Shallow
import System.IO
import Data.Complex
import Data.Word
import Data.Ord (comparing)
import Data.List (maximumBy, minimumBy, findIndices, sort)
import System.IO (readFile)
import Data.Function (on)
import Control.Monad
import Data.Maybe



-- change detection model
---------------------------------------------------------------------------------------------------------------

changeDetectionModel :: Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix Double) ->  ForSyDe.Shallow.Matrix Double
changeDetectionModel x_n y_p = out
   where out = fromSignal y_n !! 0
         y_n = zipWithSY combMatrix x_n y_p  
        
       
         
-- Auxiliar: recursion
---------------------------------------------------------------------------------------------------------------

combMatrix :: ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double -> ForSyDe.Shallow.Matrix Double  
combMatrix a b = zipWithV (zipWithV (\x y -> if x > y || x < y then x  else 0)) a b -- Combinational solution



-- Auxiliar: Data reading
----------------------------------------------------------------------------------------------------------------

readDat :: String            -- ^ file content
        -> (Int, Int, [Double]) -- ^ (X dimension, Y dimension, list of pixel values)
readDat str = (dimX, dimY, image)
  where
    image = Prelude.map Prelude.read $ words str :: [Double]
    dimX = 500; dimY = 500 -- (full dimension)

partition :: Int -> Int -> [a] -> [[a]]
partition x y list
  | y - length image > 1 = error "image dimention Y mismatch"
  | otherwise = image
  where
    image = groupEvery x list
    
groupEvery n [] = []
groupEvery n l | length l < n = error "input Data is ill-formed"
                | otherwise    = Prelude.take n l : groupEvery n (Prelude.drop n l)

chunks :: Int -> Int -> Signal (ForSyDe.Shallow.Matrix Double) -> Signal (ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)) -- sub-images (test)
chunks dimx dimy img = mapSY (groupMat dimx dimy) img



-- Call functions with Parallelism
----------------------------------------------------------------------------------------------------------------
changeDetection1Lag :: ForSyDe.Shallow.Vector (Signal(ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double)))-> ForSyDe.Shallow.Matrix (ForSyDe.Shallow.Matrix Double) 
changeDetection1Lag pair = recMat
        where

        test = pair `atV` 0; st = fromSignal test !! 0
        ref1 = pair `atV` 1; sr1 = fromSignal ref1 !! 0; vsr1 = vector [sr1]
        sv = signal [vsr1]
        -- G.5. Data-Parallel Skeletons (Embedded System Design Lecture Notes for IL2206 Prof. I.Sander)
        m = (zipxSY . mapV (mapSY (zipWithMat(\ x y -> changeDetectionModel (signal [x]) (signal [y]) ) st )) . unzipxSY) sv  -- simple CD model (changeDetectionModel)
        recV = fromSignal m !! 0; recMat = recV `atV` 0

    
       
-- MAIN
----------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do


    test <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Itest0.dat" ReadMode; contentsTest <- hGetContents test

    ref1 <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Iref0A.dat" ReadMode; contentsRef1 <- hGetContents ref1
    ref2 <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Iref0B.dat" ReadMode; contentsRef2 <- hGetContents ref2
    ref3 <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Iref0C.dat" ReadMode; contentsRef3 <- hGetContents ref3
    ref5 <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Iref0E.dat" ReadMode; contentsRef5 <- hGetContents ref5
    ref7 <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Iref0G.dat" ReadMode; contentsRef7 <- hGetContents ref7
    ref9 <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Iref0I.dat" ReadMode; contentsRef9 <- hGetContents ref9
    ref11 <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Iref0K.dat" ReadMode; contentsRef11 <- hGetContents ref11
    ref13 <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Iref0M.dat" ReadMode; contentsRef13 <- hGetContents ref13
    ref15 <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Iref0O.dat" ReadMode; contentsRef15 <- hGetContents ref15
    ref17 <- openFile "/home/marcello-costa/workspace/Demo1/SampleData/Iref0Q.dat" ReadMode; contentsRef17 <- hGetContents ref17
    
     
    ----- Dataset Arrangement (odd references) ------------------------------------------------------------------------------------
    let dimx =500; dimy =500
        (dimX, dimY, imageStreamTest) = readDat contentsTest; intestMat = matrix dimX dimY imageStreamTest 
        (dimX1, dimY1, imageStreamRef1) = readDat contentsRef1; inrefMat1 = matrix dimX1 dimY1 imageStreamRef1
        (dimX3, dimY3, imageStreamRef3) = readDat contentsRef3; inrefMat3 = matrix dimX1 dimY1 imageStreamRef3
        (dimX5, dimY5, imageStreamRef5) = readDat contentsRef5; inrefMat5 = matrix dimX1 dimY1 imageStreamRef5
        (dimX7, dimY7, imageStreamRef7) = readDat contentsRef7; inrefMat7 = matrix dimX1 dimY1 imageStreamRef7
        (dimX9, dimY9, imageStreamRef9) = readDat contentsRef9; inrefMat9 = matrix dimX1 dimY1 imageStreamRef9
        (dimX11, dimY11, imageStreamRef11) = readDat contentsRef11; inrefMat11 = matrix dimX1 dimY1 imageStreamRef11
        (dimX13, dimY13, imageStreamRef13) = readDat contentsRef13; inrefMat13 = matrix dimX1 dimY1 imageStreamRef13
        (dimX15, dimY15, imageStreamRef15) = readDat contentsRef15; inrefMat15 = matrix dimX1 dimY1 imageStreamRef15
        (dimX17, dimY17, imageStreamRef17) = readDat contentsRef17; inrefMat17 = matrix dimX1 dimY1 imageStreamRef17
        
        st = signal [intestMat];  intest = mapSY (chunks dimx dimy)  (signal [st]) -- (1) Partition step
        sr1 = signal [inrefMat1]; inref1 = mapSY (chunks dimx dimy)  (signal [sr1])
        sr3 = signal [inrefMat3]; inref3 = mapSY (chunks dimx dimy)  (signal [sr3]) 
        sr5 = signal [inrefMat5]; inref5 = mapSY (chunks dimx dimy)  (signal [sr5])
        sr7 = signal [inrefMat7]; inref7 = mapSY (chunks dimx dimy)  (signal [sr7])
        sr9 = signal [inrefMat9]; inref9 = mapSY (chunks dimx dimy)    (signal [sr9])
        sr11 = signal [inrefMat11]; inref11 = mapSY (chunks dimx dimy)  (signal [sr11])
        sr13 = signal [inrefMat13]; inref13 = mapSY (chunks dimx dimy)  (signal [sr13])
        sr15 = signal [inrefMat15]; inref15 = mapSY (chunks dimx dimy)  (signal [sr15])
        sr17 = signal [inrefMat17]; inref17 = mapSY (chunks dimx dimy)  (signal [sr17])
        
        v1lag = vector [intest,inref1]
    
  
    -----  ------------------------------------------------------------------------------------------------
    let m = zipWithxSY (changeDetection1Lag) v1lag -- (2) Comb
    let v = lengthS m

    print v


    ----- Output File ------------------------------------------------------------------------------------------------
    writeFile "/home/marcello-costa/workspace/Demo1/Out/CD0.txt" (show m)
    --writeFile "/home/marcello-costa/workspace/Demo1/Out/Test1.txt" (show intest)
    --writeFile "/home/marcello-costa/workspace/Demo1/Out/Iref2.txt" (show inref17) 


    ---- GHC terminal ----------------------------------------------------------------------------------------------
    -- ghci
    -- :load Demo1.hs    
    --- main

      ---- GHC terminal with parallelism ----------------------------------------------------------------------------------------------
    -- ghc -O2 -threaded --make Demo1.hs 
    -- time ./Demo1 +RTS -s -N12
   

