{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Prelude

import Feldspar
import Feldspar.Vector
import Feldspar.IO hiding (forM)
import Feldspar.OpenCV
import qualified Feldspar.Core.Frontend.MutableReference as R
import Feldspar.Core.Frontend.LoopM (forM)

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

smoothColor :: (Data (Complex Double), Data Index) -> Data Word32
smoothColor (zn,_) = round (i2f maxIter + 1 - log (log (magnitude zn)) / log 2)

--apa :: Data [Word32] -> Data [Word8]
--apa

split32to16 :: Data Word32 -> (Data Word16, Data Word16)
split32to16 w32 = (i2n (w32 .>>. 16), i2n w32)

split16to8 :: Data Word16 -> (Data Word8, Data Word8)
split16to8 w16 = (i2n (w16 .>>. 8), i2n w16)

bepa :: Pull (sh :. Data Length) (Data Word32) ->
        Push (sh :. Data Length) (Data Word8)
bepa (Pull ixf (sh :. l)) = Push k (sh :. l * 4)
  where k wf = forShape (sh :. l) $ \(shi :. i) -> do
                 r <- R.newRef (ixf (shi :. i))
                 e <- R.getRef r
                 wf (shi :. 4*i) (i2n e)
                 wf (shi :. 4*i + 1) (i2n (e .<<. 8))
                 wf (shi :. 4*i + 2) (i2n (e .<<. 16))
                 wf (shi :. 4*i + 3) (i2n (e .<<. 24))

mandel w = fromPush $ toPush $
           quantizeC w w (complex (-2.0) (-1.5 :: Data Double))
                         (complex 1.0 1.5) $
           fmap colourScheme $
           fmap mandelbrot $
           complexPlane

{- depth 16. cannot get it to work
mandel w = fromPush $
           fmap i2n $
           fmap (`div` 2) $
           bepa $
           fmap smoothColor $
           quantizeC w w (complex (-2.0) (-1.5 :: Data Double))
                         (complex 1.0 1.5) $
           fmap mandelbrot $
           complexPlane
-}

videoProg filter = do
  addInclude "<feldspar_c99.h>"
  addInclude "<feldspar_array.h>"
  opencv@OpenCV{..} <- importOpenCV
  newWindow "Display Image"
  cap <- captureCam
  while (return true) $ do
    i <- queryFrame cap
    (x,y) <- getDim i
    c <- getChannels i
    d <- getDepth i
    (arr,step) <- getImageData i
--    arr <- getArr i -- segfaults
    let apa = arrToPull (Z :. 480 :. 640 :. 3) arr
        bepa = fromPush $ toPush (filter apa)
--    printf "Foo: %d\n" (fromZero (sum (arrToPull (Z :. (x*y*3)) arr)))
--    printf "Step: %d\n" (i2n step :: Data WordN)
    setArr i bepa step
    imageShow "Display Image" i
    c <- waitKey 1
--    switch
--    releaseImage i
    return ()

trans :: Pull (Z :. Data Index :. Data Index :. Data Index) a ->
         Pull (Z :. Data Index :. Data Index :. Data Index) a
trans (Pull ixf sh@(Z :. y :. x :. c)) = Pull ixf' sh
  where ixf' (Z :. yi :. xi :. ci) = ixf (Z :. yi :. (x - 1) - xi :. ci)

swirlImage f = dmap' (swirl f)
swirlImage' f = dmapS' (toPush . swirl f)

liftSwirl :: (Pull sh a -> Push sh a) ->
             (Pull (sh :. Data Index) a -> Push (sh :. Data Index) a)
liftSwirl f (Pull ixf sh@(shp :. l)) = Push ixf' sh
  where ixf' wf = forM l $ \ i -> do
                    let Push ixf'' _ = f (Pull (\shl -> ixf (shl :. i)) shp)
                    ixf'' (\shl -> wf (shl :. i))
{-
liftSwirl' :: (Pull sh a -> Pull sh a) ->
              (Pull (sh :. Data Index) a -> Push (sh :. Data Index) a)
liftSwirl' f (Pull ixf sh@(shp :. l)) = Push ixf' sh
  where ixf' wf = forShape shp $ \shi ->
                    f (Pull (\
                    wf (\shl -> 
-}
swirl :: Data Float ->
         Pull (sh :. Data Index :. Data Index) a ->
         Pull (sh :. Data Index :. Data Index) a
swirl scale (Pull ixf sh@(_ :. y :. x)) = Pull ixf' sh
  where ixf' (shi :. yi :. xi) =
          let yr = i2f yi / i2f y - 0.5
              xr = i2f xi / i2f x - 0.5
              r  = sqrt (yr * yr + xr * xr)
              theta = atan2 xr yr
              theta1 = 50 * scale * r * (0.5 - r) + theta
              ys = round ((0.5 + (r < 0.5 ? r * cos theta1 $ yr)) * i2f y)
              xs = round ((0.5 + (r < 0.5 ? r * sin theta1 $ xr)) * i2f x)
          in ixf (shi :. ys :. xs)

swirly :: Data Float ->
          Pull (sh :. Data Index :. Data Index :. Data Index) a ->
          Push (sh :. Data Index :. Data Index :. Data Index) a
swirly scale (Pull ixf sh@(coord@(_ :. y :. x) :. c)) = Push ixf' sh
  where ixf' wf = forShape coord $ \ (shi :. yi :. xi) -> do
                    let yr = i2f yi / i2f y - 0.5
                        xr = i2f xi / i2f x - 0.5
                        r  = sqrt (yr * yr + xr * xr)
                        theta = atan2 xr yr
                        theta1 = 50 * scale * r * (0.5 - r) + theta
                        ys = round ((0.5 + (r < 0.5 ? r * cos theta1 $ yr)) * i2f y)
                        xs = round ((0.5 + (r < 0.5 ? r * sin theta1 $ xr)) * i2f x)
                    forM c $ \ci -> do
                      wf (shi :. ys :. xs :. ci) (ixf (shi :. yi :. xi :. ci))


gray :: Pull (sh :. Data Index :. Data Index :. Data Index) (Data Word8) ->
        Pull (sh :. Data Index :. Data Index :. Data Index) (Data Word8)
gray (Pull ixf sh) = Pull ixf' sh
  where ixf' (sh :. y :. x :. c) = i2n $
          (30 * i2n (ixf (sh :. y :. x :. 0)) +
           59 * i2n (ixf (sh :. y :. x :. 1)) +
           11 * i2n (ixf (sh :. y :. x :. 2)) :: Data WordN)
          `div` 100

replicatePush :: Data Index -> a -> Push DIM1 a
replicatePush n a = Push ixf (Z :. n)
  where ixf wf = forM n $ \i ->
                   wf (Z :. i) a

replicatePushS :: WordN -> a -> Push DIM1 a
replicatePushS n a = Push ixf (Z :. value n)
  where ixf wf = forM_ [0..n-1] $ \i ->
                   wf (Z :. value i) a

grey :: Pull DIM1 (Data Word8) -> Push DIM1 (Data Word8)
grey p = replicatePush 3 $ i2n $ (30 * i2n (p !! 0) +
                                   59 * i2n (p !! 1) +
                                   11 * i2n (p !! 2) :: Data WordN)
                                   `div` 100

mapPixel :: (Pull DIM1 a -> Push DIM1 a) ->
            (Pull (sh :. Data Index) a -> Push (sh :. Data Index) a)
mapPixel f (Pull ixf (sh :. l)) = Push ixf' (sh :. l)
  where ixf' wf = forShape sh $ \shi -> do
                    let Push foo _ = f (Pull (\ (Z :. i) -> ixf (shi :. i)) (Z :. l))
                    foo (\ (Z :. i) -> wf (shi :. i))

grayPush :: Pull (sh :. Data Index) (Data Word8) ->
            Push (sh :. Data Index) (Data Word8)
grayPush (Pull ixf sh@(shc :. _)) = Push ixf' sh
  where ixf' wf = forShape shc $ \shi -> do
                   let c = i2n $ (30 * i2n (ixf (shi :. 0)) +
                                  59 * i2n (ixf (shi :. 1)) +
                                  11 * i2n (ixf (shi :. 2)) :: Data WordN)
                                 `div` 100
                   wf (shi :. 0) c
                   wf (shi :. 1) c
                   wf (shi :. 2) c

-- invert :: Data a -> Data a

mandelProg = do
  addInclude "<feldspar_c99.h>"
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



runVideo = runVideoFilter id

runVideoFilter f = compileAndRun
  []
  (videoProg f)
  ["m","opencv_core","opencv_highgui"]

main = runVideo

runMandel = compileAndRun
  []
  mandelProg
  ["m","opencv_core","opencv_highgui"]

