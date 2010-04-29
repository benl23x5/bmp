
module Codec.BMP.Base
	( getLSBWord32
	, getLSBWord16)
where
import Data.Bits
import Data.Word
import Data.ByteString
	
	
-- | Loads and converts a 32bit LSB word to machine representation.
getLSBWord32 :: ByteString -> Int -> Word32
getLSBWord32 str ix
 	=	(fromIntegral $ str `index` (ix + 3)) `shiftL` 24
	.|.	(fromIntegral $ str `index` (ix + 2)) `shiftL` 16
	.|.	(fromIntegral $ str `index` (ix + 1)) `shiftL` 8
	.|.	(fromIntegral $ str `index` (ix + 0)) `shiftL` 0
	
getLSBWord16 :: ByteString -> Int -> Word16
getLSBWord16 str ix
 	= 	(fromIntegral $ str `index` (ix + 1)) `shiftL` 8
	.|.	(fromIntegral $ str `index` (ix + 0)) `shiftL` 0
	

