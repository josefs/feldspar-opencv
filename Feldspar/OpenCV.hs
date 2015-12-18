{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes  #-}
module Feldspar.OpenCV
  ( OpenCV (..)
  , Window
  , IplImage
  , importOpenCV
  , getImageData
  )where

import Feldspar
import Feldspar.IO
import Language.C.Quote.C

type Window = String

newtype IplImage = IplImage { unImg :: Object }
newtype Point = Point { unPoint :: Object }
newtype Color = Color { unColor :: Object }
newtype Font  = Font  { unFont  :: Object }

newtype Capture = Capture { unCap :: Object }

data OpenCV = OpenCV
  { newImage    :: Program IplImage
  , createImage :: Data IntN -> Data IntN -> Data IntN -> Data IntN
                -> Program IplImage
  , loadImage   :: FilePath -> IplImage -> Program ()
  , newWindow   :: Window   -> Program ()
  , imageShow   :: Window   -> IplImage -> Program ()
  , waitKey     :: Int      -> Program (Data IntN)
  , getArr      :: IplImage -> Program (Data [Word8])
  , setArr      :: IplImage -> Data [Word8] -> Data IntN -> Program ()
  , getDim      :: IplImage -> Program (Data Index,Data Index)
  , getChannels :: IplImage -> Program (Data IntN)
  , getDepth    :: IplImage -> Program (Data IntN)
  , releaseImage:: IplImage -> Program ()

  , captureCam :: Program Capture
  , queryFrame :: Capture -> Program IplImage
  }

newImage_ :: Program IplImage
newImage_ = fmap IplImage $ newObject "IplImage"

loadImage_def = [cedecl|
void loadImageCV(const char* filename, typename IplImage** image) {
  *image = cvLoadImage(filename,1);
}
|]

loadImage_ :: String -> IplImage -> Program ()
loadImage_ f i = callProc "loadImageCV"
                 [ strArg f
                 , addr $ objArg $ unImg i
                 ]

imageShow_ :: Window -> IplImage -> Program ()
imageShow_ w a = callProc "cvShowImage"
                 [ strArg w
                 , objArg $ unImg a
                 ]

newWindow_ :: Window -> Program ()
newWindow_  w = callProc "cvNamedWindow"
                [ strArg w 
                , valArg (1 :: Data IntN)
                ]

waitKey_ :: Int -> Program (Data IntN)
waitKey_ w = callFun "cvWaitKey"
             [ valArg (value (fromIntegral w) :: Data IntN)]
{-
releaseImage_ :: IplImage -> Program ()
releaseImage_ i = callProc "cvReleaseImage"
                  [ objArg (unImg i) ]
-}
getArr_ :: IplImage -> Program (Data [Word8])
getArr_ i = callFun "getArrData" [ objArg (unImg i) ]

getArr_def =
  [cedecl|
  typename uchar* getArrData(typename IplImage *image) {
    typename uchar* data;
    int step;
    struct CvSize roi_size;
    cvGetRawData(image,&data,&step,&roi_size);
    return data;
  }
  |]

setArr_ :: IplImage -> Data [Word8] -> Data IntN -> Program ()
setArr_ i d s = do
  arr <- unsafeThawArr d
  callProc "cvSetData"
                  [objArg (unImg i)
                  ,arrArg arr
                  ,valArg s
                  ]

createImage_ :: Data IntN -> Data IntN -> Data IntN -> Data IntN
             -> Program IplImage
createImage_ width height depth channels = do
  size <- initUObject "cvSize" "CvSize" [valArg width, valArg height]
  fmap IplImage $ initObject "cvCreateImage" "IplImage"
                    [objArg size, valArg depth, valArg channels]

captureCam_ :: Program Capture
captureCam_ = fmap Capture $
              initObject "cvCaptureFromCAM" "CvCapture"
                         [valArg (0 :: Data IntN)]

queryFrame_ :: Capture -> Program IplImage
queryFrame_ cap = fmap IplImage $
                  initObject "cvQueryFrame" "IplImage"
                             [objArg (unCap cap)]

getDim_ :: IplImage -> Program (Data Index, Data Index)
getDim_ i = do dims <- newArr 2 :: Program (Arr Index Index)
               d <- callFun "cvGetDims"
                      [objArg (unImg i),arrArg dims] :: Program (Data IntN)
               x <- Feldspar.IO.getArr 1 dims
               y <- Feldspar.IO.getArr 0 dims
               return (x,y)

{- For this to be useful, we really need dynamic string arguments.

putText_ :: IplImage -> String -> Point -> Font -> Color -> Program ()
putText_ i s p f c
  = callProc "cvPutText"
      [ObjArg (unImg i)
      ,StrArg s
      ,ObjArg (unPoint p)
      ,ObjArg f
      ,ObjArg c
      ]
-}

getChannels_def = [cedecl|
                  int getChannels(typename IplImage *image) {
                    return image->nChannels;
                  }
                  |]

getChannels_ :: IplImage -> Program (Data IntN)
getChannels_ image = callFun "getChannels" [objArg (unImg image)]

getDepth_def = [cedecl|
               int getDepth(typename IplImage *image) {
                 return image->depth;
               }
               |]

getDepth_ :: IplImage -> Program (Data IntN)
getDepth_ image = callFun "getDepth" [objArg (unImg image)]

releaseImage_ :: IplImage -> Program ()
releaseImage_ image = callProc "cvReleaseImage" [addr $ objArg (unImg image)]

getImageData :: IplImage -> Program (Data [Word8], Data IntN)
getImageData image = do
  (y,x) <- getDim_ image
  d     <- getDepth_ image
  arr   <- newArr_
  step  <- newRef :: Program (Ref IntN)
  callProc "cvGetRawData"
    [objArg (unImg image)
    ,addr $ arrArg arr
    ,refArg step
    ,valArg (0 :: Data IntN) -- NULL
    ]
  farr <- freezeArr arr (x*y*i2n d)
  s <- getRef step
  return (farr, s)

-- getManifest :: IplImage -> Program (Manifest DIM2 RGB)

importOpenCV :: Program OpenCV
importOpenCV = do
  addInclude "<opencv2/core/core_c.h>"
  addInclude "<opencv2/highgui/highgui_c.h>"
  addDefinition loadImage_def
  addDefinition getArr_def
  addDefinition getChannels_def
  addDefinition getDepth_def
  return $ OpenCV
    newImage_
    createImage_
    loadImage_
    newWindow_
    imageShow_
    waitKey_
    getArr_
    setArr_
    getDim_
    getChannels_
    getDepth_
    releaseImage_

    captureCam_
    queryFrame_

opencvLibs = ["opencv_core","opencv_highgui"]
opencvIncludes = ["-I/usr/local/include/opencv","-I/usr/local/include/opencv2"]

-- Measuring time

data Time = Time
  { time  :: forall a . Program a -> Program (a,Data Double)
  , time_ :: forall a . Program a -> Program (Data Double)
  }

gettime_decl = [cedecl|
double feldspar_gettime() {
  struct timespec ts;

  clock_gettime(CLOCK_MONOTONIC, &ts);

  return ts.tv_sec + ts.tv_nsec * 1e-9;
}
|]

time_i :: Program a -> Program (a,Data Double)
time_i prog = do d1 <- callFun "feldspar_gettime" [] :: Program (Data Double)
                 a <- prog
                 d2 <- callFun "feldspar_gettime" [] :: Program (Data Double)
                 return (a,Feldspar.max 0 (d1-d1))

time__ :: Program a -> Program (Data Double)
time__ prog = do d1 <- callFun "feldspar_gettime" [] :: Program (Data Double)
                 prog
                 d2 <- callFun "feldspar_gettime" [] :: Program (Data Double)
                 return (Feldspar.max 0 (d1-d2))

data Platform = Posix

importTime :: Platform -> Program Time
importTime Posix = do
  addInclude "<time.h>"
  addDefinition gettime_decl
  return $ Time
    time_i
    time__
