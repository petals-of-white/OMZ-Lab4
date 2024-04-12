{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where
import           Data.Binary
import qualified Data.ByteString.Lazy as L
import           Data.DICOM           (readObjectFromFile)
import           Data.DICOM.Utilities as DicomMap
import           Data.List.Split      (splitOneOf)
import           Data.Vector.Storable (fromList)
import           System.Environment   (getArgs)
import           Text.Read
import           Vision.Histogram     (ToHistogram (..))
import           Vision.Image         (Manifest (Manifest),
                                       ThresholdType (BinaryThreshold), otsu)
import           Vision.Primitive     (DIM1, ix1, ix2)

instance (ToHistogram Word16) where
  type PixelValueSpace Word16 = DIM1
  pixToIndex word = ix1 $ fromIntegral word
  domainSize = const $ ix1 (fromIntegral (maxBound :: Word16) + 1)

main :: IO ()
main =
  do
    args <- getArgs

    case parseArgs args of
      Right ProgramArgs {argColor=color, argDicom=dicomPath} -> do
        dicomMap <- DicomMap.toMap . either error id <$> readObjectFromFile dicomPath
        let (pixByteString, row, col) = either (error . show) id $ do
              pixBytes <- pixelData dicomMap
              height <- rows dicomMap
              width <- columns dicomMap
              return (pixBytes, fromIntegral height, fromIntegral width)

            pixels =  byteSwap16 <$> decode (L.append (encode (row*col :: Int)) (L.fromStrict pixByteString))
            fridayImg = Manifest (ix2 row col) (fromList pixels)
            otsud = otsu (BinaryThreshold maxBound minBound) fridayImg :: Manifest Word16


        -- print otsud
        return ()

      Left err -> error err

    return ()

data ProgramArgs = ProgramArgs {argColor :: (Float, Float, Float, Float), argDicom :: FilePath}

parseArgs :: [String] -> Either String ProgramArgs
parseArgs [path, color] =
  case splitOneOf ",." color of
    [rs,gs,bs,as] -> do
      r <- readEither rs
      g <- readEither gs
      b <- readEither bs
      a <- readEither as
      return ProgramArgs{argColor=(r,g,b,a),argDicom=path}
    _ -> Left "Wrong number of channels for color"

parseArgs _ = Left "Wrong number of arguments. Expected format: dicompath r,g,b,a"
