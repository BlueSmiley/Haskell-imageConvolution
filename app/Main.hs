{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import qualified Codec.Picture.Types as M
import Lib

main :: IO ()
main = do
  [path, newname] <- getArgs
  eimg <- readImage path
  case eimg of
    Left err -> putStrLn ("Could not read image: " ++ err)
    Right img -> 
        savePngImage newname $ImageRGB8 $ 
        mypseudoBlur $ convertRGB8 img


greyScaleRGB8 :: Image PixelRGB8 -> Image PixelRGB8
greyScaleRGB8 img = pixelMap grey img
    where grey (PixelRGB8 r g b) = 
            let average = 
                    (toInteger r + toInteger g + toInteger b) `div` 3 
            in
                PixelRGB8 
                (fromIntegral average) 
                (fromIntegral average) 
                (fromIntegral average)

mypseudoBlur = imageConvulution [
    [(1,1,1), (2,2,2), (4,4,4)],
    [(2,2,2), (4,4,4), (8,8,8)],
    [(4,4,4), (8,8,8), (16,16,16)]
    ] (1/49 :: Double)

sobelHor = imageConvulution [
    [(1,1,1), (2,2,2), (1,1,1)],
    [(0,0,0), (0,0,0), (0,0,0)],
    [(-1,-1,-1), (-2,-2,-2), (-1,-1,-1)]
    ] 1

sobelVer = imageConvulution [
    [(1,1,1), (0,0,0), (-1,-1,-1)],
    [(2,2,2), (0,0,0), (-2,-2,-2)],
    [(1,1,1), (0,0,0), (-1,-1,-1)]
    ] 1

emboss = imageConvulution [
    [(-2,-2,-2), (-1,-1,-1), (0,0,0)],
    [(-1,-1,-1), (1,1,1), (1,1,1)],
    [(0,0,0), (1,1,1), (2,2,2)]
    ] 1

blueFilter = imageConvulution [
    [(0,0,1)]
    ] 1
    
greenFilter = imageConvulution [
    [(0,1,0)]
    ] 1

redFilter = imageConvulution [
    [(1,0,0)]
    ] 1

identity = imageConvulution [
    [(1,1,1)]
    ] 1

unsharpMask = imageConvulution [
    [(1,1,1), (4,4,4), (6,6,6), (4,4,4), (1,1,1)],
    [(4,4,4), (16,16,16), (24,24,24), (16,16,16), (4,4,4)],
    [(6,6,6), (24,24,24), (-476,-476,-476), (24,24,24), (6,6,6)],
    [(4,4,4), (16,16,16), (24,24,24), (16,16,16), (4,4,4)],
    [(1,1,1), (4,4,4), (6,6,6), (4,4,4), (1,1,1)]
    ] (-1/256 :: Double)

gaussBlur3x3 = imageConvulution [
    [(1,1,1), (2,2,2), (1,1,1)],
    [(2,2,2), (4,4,4), (2,2,2)],
    [(1,1,1), (2,2,2), (1,1,1)]
    ] (1/16 :: Double)

gaussBlur5x5 = imageConvulution [
    [(1,1,1), (4,4,4), (6,6,6), (4,4,4), (1,1,1)],
    [(4,4,4), (16,16,16), (24,24,24), (16,16,16), (4,4,4)],
    [(6,6,6), (24,24,24), (36,36,36), (24,24,24), (6,6,6)],
    [(4,4,4), (16,16,16), (24,24,24), (16,16,16), (4,4,4)],
    [(1,1,1), (4,4,4), (6,6,6), (4,4,4), (1,1,1)]
    ] (1/256 :: Double)

boxBlur = imageConvulution [
    [(1,1,1), (1,1,1), (1,1,1)],
    [(1,1,1), (1,1,1), (1,1,1)],
    [(1,1,1), (1,1,1), (1,1,1)]
    ] (1/9 :: Double)

sharpen = imageConvulution [
    [(0,0,0), (-1,-1,-1), (0,0,0)],
    [(-1,-1,-1), (5,5,5), (-1,-1,-1)],
    [(0,0,0), (-1,-1,-1), (0,0,0)]
    ] 1

edgeDetection3 = imageConvulution [
    [(-1,-1,-1), (-1,-1,-1), (-1,-1,-1)],
    [(-1,-1,-1), (8,8,8), (-1,-1,-1)],
    [(-1,-1,-1), (-1,-1,-1), (-1,-1,-1)]
    ] 1

edgeDetection2 = imageConvulution [
    [(0,0,0), (1,1,1), (0,0,0)],
    [(1,1,1), (-4,-4,-4), (1,1,1)],
    [(0,0,0), (1,1,1), (0,0,0)]
    ] 1

edgeDetection1 = imageConvulution [
    [(1,1,1), (0,0,0), (-1,-1,-1)],
    [(0,0,0), (0,0,0), (0,0,0)],
    [(-1,-1,-1), (0,0,0), (1,1,1)]
    ] 1

imageConvulution ::(RealFrac p, Show p) =>
    [[(p, p, p)]] -> p -> Image PixelRGB8 -> Image PixelRGB8 
imageConvulution matrix multiplier img@Image {..} = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let for x y
            | x >= imageWidth  = for 0 (y + 1)
            | y >= imageHeight = M.unsafeFreezeImage mimg
            | otherwise = do
                writePixel mimg x y $ 
                    pixelConvulution matrix multiplier x y img 
                for (x + 1) y
    for 0 0

pixelConvulution :: (RealFrac a, Show a)=>
    [[(a, a, a)]] -> a -> Int -> Int -> Image PixelRGB8 -> PixelRGB8    
pixelConvulution matrix multiplier x y img@Image {..} = 
    let (r0,g0,b0) = 
            go (x-centerOffset) (y-centerOffset) (0,0,0)
        (rr,rg,rb) = 
            (
                max (min (r0 * multiplier) 255) 0,
                max (min (g0 * multiplier) 255) 0,
                max (min (b0 * multiplier) 255) 0
            )    
    in
        PixelRGB8 (fromIntegral.floor$rr) (fromIntegral.floor$rg) 
            (fromIntegral.floor$rb) 
    where 
        centerOffset = length matrix `div` 2
        go a b acc
            | a >= imageWidth || a > x + centerOffset = 
                go (x-centerOffset) (b+1) acc
            | b >= imageHeight || b > y + centerOffset = acc 
            | a < 0 || b < 0 = go (a+1) b acc
            | otherwise = 
                let PixelRGB8 r1 g1 b1 = pixelAt img a b
                    col = centerOffset - a + x
                    row = centerOffset - b + y
                    (rmult,gmult,bmult) = (matrix `get` row) `get` col
                    (rmod,gmod,bmod) = 
                        (
                            fromIntegral r1 * rmult,
                            fromIntegral g1 * gmult, 
                            fromIntegral b1 * bmult)
                    (r2,g2,b2) = acc
                    newacc = (r2 + rmod,g2 + gmod,b2 + bmod)
                in
                    go (a+1) b newacc

--wrapper around !! to give some useful debugging info
get xs n 
    | length xs-1 < n = error 
        ("list =" ++ show xs ++ " index ="  ++ show n)
    | n < 0 = error ("index =" ++ show n)
    | otherwise = xs !! n



