{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs           #-}
module Main where

import qualified Prelude

import Feldspar
import Feldspar.Vector
import Feldspar.IO
import Feldspar.OpenCV

newtype HyperPlane i a = HyperPlane (i -> a)

complexPlane :: HyperPlane (Data (Complex a)) (Data (Complex a))
complexPlane = HyperPlane id

instance Functor (HyperPlane i) where
  fmap f (HyperPlane g) = HyperPlane (f . g)

quantize :: Shape sh -> (Shape sh -> i) -> HyperPlane i a -> Pull sh a
quantize sh shf (HyperPlane f) = Pull (f . shf) sh

quantizeC :: (Fraction a, Prelude.RealFloat a)
          => Data Index -> Data Index -> Data (Complex a) -> Data (Complex a)
          -> HyperPlane (Data (Complex a)) e -> Pull DIM2 e
quantizeC width height ul lr (HyperPlane f) =
    Pull (f . convert) (Z :. width :. height)
  where convert (Z :. x :. y) =
          complex (i2f x / i2f width * (realPart lr - realPart ul) + realPart ul)
                  (i2f y / i2f height * (imagPart lr - imagPart ul) + imagPart ul)

maxIter = 1000

convert :: Data Index -> Data Double
convert i = i2f i / 128.0 - 1.0

mandelbrot c = whileLoop (c,0)
                (\(z,i) -> magnitude z < 2 && i < maxIter)
                (\(z,i) -> (z*z+c,i+1))

colourScheme :: (a,Data Index) -> Data Word8
colourScheme (_,i) = round (log (i2f i) / log (i2f maxIter) * 256.0 :: Data Double)

mandel w = fromPush $ toPush $
           quantizeC w w (complex (-2.0) (-1.5 :: Data Double))
                         (complex 1.0 1.5) $
           fmap colourScheme $
           fmap mandelbrot $
           complexPlane

prog = do addInclude "<feldspar_c99.h>"
          addInclude "<feldspar_array.h>"
          opencv@OpenCV{..} <- importOpenCV
          refI <- initRef 512
          twofiftysix <- getRef refI
          refW <- initRef 512
          w <- getRef refW
          foo <- createImage twofiftysix twofiftysix 8 1
          let arr = mandel w
          setArr foo arr 512
          newWindow "Display Image"
          imageShow "Display Image" foo
          waitKey 0
          cap <- captureCam
          while (return true) $ do
            i <- queryFrame cap
            (x,y) <- getDim i
            c <- getChannels i
            d <- getDepth i
            printf "Dims: %d %d, Channel: %d, Depth: %d\n" x y (i2n c :: Data WordN) (i2n d :: Data WordN)
            imageShow "DisplayImage" i
            waitKey 1
            return ()

run = compileAndRun    ["-I/usr/local/include/opencv"
                       ,"-I/usr/local/include/opencv2"
                       ,"-L/usr/local/lib/"
                       ]
                       prog
                       ["m","opencv_core","opencv_highgui"]

main = compileAndCheck ["-I/usr/local/include/opencv"
                       ,"-I/usr/local/include/opencv2"
                       ,"-L/usr/local/lib/"
                       ]
                       prog
                       ["m","opencv_core","opencv_highgui"]
