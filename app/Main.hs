{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase   #-}
module Main where
import           Codec.BMP                  (parseBMP)
import           Data.Binary
import qualified Data.ByteString.Lazy       as L
import           Data.DICOM                 (readObjectFromFile)
import           Data.DICOM.Utilities       as DicomMap
import           Data.List.Split            (splitOneOf)
import           System.Environment         (getArgs)
import           Text.Read
import           Vision.Image               as I

import           Graphics.Gloss
import           Segmentation
import           Vision.Image.Storage.DevIL (BMP (BMP), saveBS)


main :: IO ()
main =
  do
    args <- getArgs

    case parseArgs args of
      Right ProgramArgs {argColor=(r,g,b,a), argDicom=dicomPath} -> do
        dicomObj <- either error id <$> readObjectFromFile dicomPath
        let fridayImg = either (error . show) id $ dicomToFriday16 dicomObj
            -- fridayOgEqualizied = I.map (GreyPixel . convertPropToBounds) (equalizeImage fridayImg) :: Grey
            fridayOgNormalized = I.map (GreyPixel . convertPropToBounds) (normalizePeaks 0 65535 fridayImg) :: Grey
            otsud = I.otsu (I.BinaryThreshold (I.RGBAPixel 0 0 0 a) (I.RGBAPixel r g b a)) fridayImg :: I.RGBA

            -- what is this... 😭
            origPic =
              fmap bitmapOfBMP $
              (mapLeft show . parseBMP) . L.fromStrict
              =<<
              mapLeft show (saveBS BMP fridayOgNormalized)

            otsuPic =
              fmap bitmapOfBMP $
              (mapLeft show . parseBMP) . L.fromStrict
              =<<
              mapLeft show (saveBS BMP otsud)

        case (origPic, otsuPic) of
          (Right origP, Right otsuP) ->
            play (InWindow "Lab4" (256,256) (10,10)) black 0 Original
              (\case Original -> origP; Otsu -> origP <> otsuP) keyEvents (const id)

          (Left err1, Left err2)     -> error (err1 ++ err2)
          (Left err1, Right _)       -> error err1
          (Right _, Left err2)       -> error err2

      Left err -> error err


data ProgramArgs = ProgramArgs {argColor :: (Word8, Word8, Word8, Word8), argDicom :: FilePath}

parseArgs :: [String] -> Either String ProgramArgs
parseArgs [path, colour] =
  case splitOneOf ",." colour of
    [rs,gs,bs,as] -> do
      r <- readEither rs
      g <- readEither gs
      b <- readEither bs
      a <- readEither as
      return ProgramArgs{argColor=(r,g,b,a),argDicom=path}
    _ -> Left "Wrong number of channels for color"

parseArgs _ = Left "Wrong number of arguments. Expected format: dicompath r,g,b,a"
