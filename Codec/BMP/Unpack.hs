{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codec.BMP.Unpack
	(unpackBMPToRGBA32)
where	
import Codec.BMP.Base
import Codec.BMP.BitmapInfo
import Codec.BMP.BitmapInfoV3
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe
import Data.Word
import Data.ByteString		as BS
import Data.ByteString.Unsafe	as BS
import Prelude			as P


-- | Unpack a BMP image to a string of RGBA component values.
unpackBMPToRGBA32 :: BMP -> ByteString
unpackBMPToRGBA32 bmp 
 = let	info		= getBitmapInfoV3 $ bmpBitmapInfo bmp
	width		= fromIntegral $ dib3Width  info
	height		= fromIntegral $ dib3Height info
        flipX           = dib3HeightFlipped info
	bitCount	= dib3BitCount info
   in	case bitCount of
	 24	-> packRGB24ToRGBA32 width height flipX (bmpRawImageData bmp)
	 32	-> packRGB32ToRGBA32 width height flipX (bmpRawImageData bmp)
	 _	-> error "Codec.BMP: Unhandled bitcount."


-- | Unpack raw, uncompressed 24 bit BMP image data to a string of
--   RGBA component values.
--
--   The alpha component is set to 255 for every pixel.
packRGB24ToRGBA32
	:: Int 			-- ^ Width of image.
	-> Int			-- ^ Height of image.
        -> Bool                 -- ^ Image data is flipped along the X axis.
	-> ByteString 		-- ^ Input string.
	-> ByteString
		
packRGB24ToRGBA32 width height flipX str
 = let	-- Number of bytes per line in the source file, 
        -- including padding bytes.
        srcBytesPerLine	= BS.length str `div` height
	sizeDest	= width * height * 4

        -- We allow padding bytes on the end of the image data.
   in	if BS.length str < height * srcBytesPerLine
	 then error "Codec.BMP: Image data is truncated."
 	 else unsafePerformIO
       	 	$ allocaBytes sizeDest      $ \bufDest -> 
   	   	  BS.unsafeUseAsCString str $ \bufSrc  ->
            	   do	packRGB24ToRGBA32' 
                                width height flipX 
                                srcBytesPerLine
                                (castPtr bufSrc) (castPtr bufDest)
			packCStringLen (bufDest, sizeDest)

		
-- We're doing this via Ptrs because we don't want to take the
-- overhead of doing the bounds checks in ByteString.index.
packRGB24ToRGBA32' width height flipX srcBytesPerLine ptrSrc ptrDst 
 = go 0
 where	
        go posY
         -- we've finished the image.
         | posY == height
         = return ()

         -- Image source data is flipped along the X axis.
         | flipX
         = let  !oSrc   = srcBytesPerLine * (height - 1 - posY)
                !oDst   = width * 4 * posY
           in   go_line 0 posY oSrc oDst

         -- Image source data is in the natural order.
         | otherwise
         = let  !oSrc   = srcBytesPerLine * posY
                !oDst   = width * 4 * posY
           in   go_line 0 posY oSrc oDst

	go_line posX posY oSrc oDst
	 -- move to the next line.
	 | posX == width 
	 = go (posY + 1)
		
	 -- process a pixel.
	 | otherwise
	 = do	blue  :: Word8	<- peekByteOff ptrSrc (oSrc + 0)
		green :: Word8	<- peekByteOff ptrSrc (oSrc + 1)
		red   :: Word8	<- peekByteOff ptrSrc (oSrc + 2)

		pokeByteOff ptrDst (oDst + 0) red
		pokeByteOff ptrDst (oDst + 1) green
		pokeByteOff ptrDst (oDst + 2) blue
		pokeByteOff ptrDst (oDst + 3) (255 :: Word8)
		
		go_line (posX + 1) posY (oSrc + 3) (oDst + 4)



-- | Unpack raw, uncompressed 32 bit BMP image data to a string of
--   RGBA component values.
--   Note in the BMP file the components are arse-around ABGR instead of RGBA. 
--   The 'unpacking' here is really just flipping the components around.
packRGB32ToRGBA32
	:: Int 			-- ^ Width of image.
	-> Int			-- ^ Height of image.
        -> Bool                 -- ^ Image data is flipped along the X axis.
	-> ByteString 		-- ^ Input string.
	-> ByteString
		
packRGB32ToRGBA32 width height flipX str
  = let sizeDest = height * width * 4
    in  if  BS.length str < sizeDest
	 then error "Codec.BMP: Image data is truncated."
 	 else unsafePerformIO
       	 	$ allocaBytes sizeDest      $ \bufDest -> 
   	   	  BS.unsafeUseAsCString str $ \bufSrc  ->
            	   do	packRGB32ToRGBA32' width height
                                flipX
                                (castPtr bufSrc) (castPtr bufDest)
			packCStringLen (bufDest, sizeDest)
		
-- We're doing this via Ptrs because we don't want to take the
-- overhead of doing the bounds checks in ByteString.index.
packRGB32ToRGBA32' width height flipX ptrSrc ptrDst
 = go 0
 where	
	go posY
         -- we've finished the image.
         | posY == height
         = return ()

         -- Image source data is flipped along the X axis.
         | flipX
         = let  !oSrc   = width * 4 * (height - 1 - posY)
                !oDst   = width * 4 * posY
           in   go_line 0 posY oSrc oDst
	
         -- Image source data is in the natural order.
         | otherwise
         = let  !oSrc   = width * 4 * posY
                !oDst   = width * 4 * posY
           in   go_line 0 posY oSrc oDst

        go_line posX posY oSrc oDst
         -- move to the next line.
         | posX == width 
         = go (posY + 1)
	
	 -- process a pixel.
	 | otherwise
	 = do	blue   :: Word8	<- peekByteOff ptrSrc (oSrc + 0)
		green  :: Word8	<- peekByteOff ptrSrc (oSrc + 1)
		red    :: Word8	<- peekByteOff ptrSrc (oSrc + 2)
		alpha  :: Word8 <- peekByteOff ptrSrc (oSrc + 3)

		pokeByteOff ptrDst (oDst + 0) red
		pokeByteOff ptrDst (oDst + 1) green
		pokeByteOff ptrDst (oDst + 2) blue
		pokeByteOff ptrDst (oDst + 3) alpha
		
		go_line (posX + 1) posY (oSrc + 4) (oDst + 4)

