{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Segmentation where
import           Data.Binary                        (Word16, byteSwap16, decode,
                                                     encode)
import qualified Data.ByteString.Lazy               as L
import qualified Data.DICOM                         as Dicom
import qualified Data.Vector.Storable               as VS
import           Graphics.Gloss.Interface.Pure.Game as Game
import           Vision.Histogram
import           Vision.Image                       as I
import           Vision.Primitive.Shape


data Mode = Original | Otsu

keyEvents :: Event -> Mode -> Mode
keyEvents (EventKey (Char 's') Down _ _) = const Otsu
keyEvents (EventKey (Char 'o') Down _ _) = const Original
keyEvents _                              = id

instance (ToHistogram Word16) where
  type PixelValueSpace Word16 = DIM1
  pixToIndex word = ix1 $ fromIntegral word
  domainSize = const $ ix1 (fromIntegral (maxBound :: Word16) + 1)

dicomToFriday16 :: Dicom.Object -> Either Dicom.ElementError (Manifest Word16)
dicomToFriday16 dicomObj =
    let dicomMap = Dicom.toMap dicomObj
    in do
        (pixByteString, row, col) <-  do
            pixBytes <- Dicom.pixelData dicomMap
            height <- Dicom.rows dicomMap
            width <- Dicom.columns dicomMap
            return (pixBytes, fromIntegral height, fromIntegral width)

        let pixels =  byteSwap16 <$> decode (L.append (encode (row*col :: Int)) (L.fromStrict pixByteString))

        return $ Manifest (ix2 row col) (VS.fromList pixels)

normalizePeaks :: Word16 -> Word16 -> Manifest Word16 -> Manifest Word16
normalizePeaks minNew maxNew img =
  I.map
  (\p -> round $ fromIntegral (minNew + (p - minPeak)) / fromIntegral(maxPeak - minPeak) * fromIntegral(maxNew - minNew))
  img
  where
    maxPeak = VS.maximum (manifestVector img)
    minPeak = VS.minimum (manifestVector img)

normalizeBounds :: forall a b. (Bounded a, Real a, Fractional b) => a -> b
normalizeBounds v = realToFrac (v - (minBound :: a)) / realToFrac ((maxBound :: a) - (minBound :: a))


convertPropToBounds :: forall a b. (Bounded a, Bounded b, Real a, Real b, Convertible Double b) => a -> b
convertPropToBounds  = convert . ((realToFrac (maxBound :: b) :: Double) *)  . normalizeBounds
