{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codec.BMP.Unpack
	(unpackBMPToRGBA32)
where	
import Codec.BMP.Base
import Codec.BMP.BitmapInfo
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe
import Data.Word
import Data.ByteString		as BS
import Data.ByteString.Unsafe	as BS
import Prelude			as P


-- | Unpack a BMP image to a string of RGBA component values.
--	The A (alpha) value is set to 255 for every pixel.
unpackBMPToRGBA32 :: BMP -> ByteString
unpackBMPToRGBA32 bmp 
 = case bmpBitmapInfo bmp of
	InfoV3 info
	 -> unpackRGB24ToRGBA32 
			(fromIntegral $ dib3Width info) 
			(fromIntegral $ dib3Height info)
			(bmpRawImageData bmp)


-- | Unpack raw, uncompressed 24 bit BMP image data to a string of RGBA component values.
--	The A (alpha) value is set to 255 for every pixel.
unpackRGB24ToRGBA32
	:: Int 			-- Width of image.
	-> Int			-- Height of image.
	-> ByteString 		-- Input string.
	-> ByteString
		
unpackRGB24ToRGBA32 width height str
 = let	bytesPerLine	= BS.length str `div` height
	padPerLine	= bytesPerLine - width * 3
	sizeDest	= width * height * 4
   in	if height * (width * 3 + padPerLine) /= BS.length str
	 then error "Codec.BMP.unpackRGB24ToRGBA32: given image dimensions don't match input data."
 	 else unsafePerformIO
       	 	$ allocaBytes sizeDest      $ \bufDest -> 
   	   	  BS.unsafeUseAsCString str $ \bufSrc  ->
            	   do	unpackRGB24ToRGBA32' width height padPerLine (castPtr bufSrc) (castPtr bufDest)
			packCStringLen (bufDest, sizeDest)
		
-- We're doing this via Ptrs because we don't want to take the
-- overhead of doing the bounds checks in ByteString.index.
unpackRGB24ToRGBA32' width height pad ptrSrc ptrDest 
 = go 0 0 0 0
 where	
	go posX posY oSrc oDest
	 -- skip over padding bytes at the end of each line.
	 | posX == width 
	 = go 0 (posY + 1) (oSrc + pad) oDest
	
	 -- we've finished the image.
	 | posY == height
	 = return ()
	
	 -- process a pixel.
	 | otherwise
	 = do	blue  :: Word8	<- peekByteOff ptrSrc (oSrc + 0)
		green :: Word8	<- peekByteOff ptrSrc (oSrc + 1)
		red   :: Word8	<- peekByteOff ptrSrc (oSrc + 2)

		pokeByteOff ptrDest (oDest + 0) red
		pokeByteOff ptrDest (oDest + 1) green
		pokeByteOff ptrDest (oDest + 2) blue
		pokeByteOff ptrDest (oDest + 3) (255 :: Word8)
		
		go (posX + 1) posY (oSrc + 3) (oDest + 4)


		
