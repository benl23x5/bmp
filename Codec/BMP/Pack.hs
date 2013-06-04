{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.Pack
        ( packRGBA32ToBMP
        , packRGBA32ToBMP24
        , packRGBA32ToBMP32)
where
import Codec.BMP.Base
import Codec.BMP.BitmapInfo
import Codec.BMP.BitmapInfoV3
import Codec.BMP.FileHeader
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe
import Data.Word
import Data.Maybe
import Data.ByteString		as BS
import Data.ByteString.Unsafe	as BS
import Prelude			as P


-- | Pack a string of RGBA component values into a 32-bit BMP image.
--  
--   Alias for `packRGBA32ToBMP32`.
--
packRGBA32ToBMP
        :: Int          -- ^ Width of image  (must be positive).
        -> Int          -- ^ Height of image (must be positive).
        -> ByteString   -- ^ A string of RGBA component values.
                        --   Must have length (@width * height * 4@)
        -> BMP

packRGBA32ToBMP = packRGBA32ToBMP32
{-# INLINE packRGBA32ToBMP #-}


-- BMP 32 bit -----------------------------------------------------------------
-- | Pack a string of RGBA component values into a 32-bit BMP image.
--
--  * If the given dimensions don't match the input string then `error`.
--
--  * If the width or height fields are negative then `error`.
--
packRGBA32ToBMP32
        :: Int          -- ^ Width of image  (must be positive).
        -> Int          -- ^ Height of image (must be positive).
        -> ByteString   -- ^ A string of RGBA component values.
                        --   Must have length (@width * height * 4@)
        -> BMP

packRGBA32ToBMP32 width height str
 | width < 0    
 = error "Codec.BMP: Negative width field."

 | height < 0   
 = error "Codec.BMP: Negative height field."

 | height * width * 4 /= BS.length str
 = error "Codec.BMP: Image dimensions don't match input data."

 | otherwise
 = let  imageData       = packRGBA32ToBGRA32 width height str
   in   packDataToBMP 32 width height imageData


-- BMP 24 bit -----------------------------------------------------------------
-- | Pack a string of RGBA component values into a 24-bit BMP image,
--   discarding the alpha channel of the source data.
--
--  * If the given dimensions don't match the input string then `error`.
--
--  * If the width or height fields are negative then `error`.

packRGBA32ToBMP24
        :: Int          -- ^ Width of image  (must be positive).
        -> Int          -- ^ Height of image (must be positive).
        -> ByteString   -- ^ A string of RGBA component values.
                        --   Must have length (@width * height * 4@)
        -> BMP

packRGBA32ToBMP24 width height str
 | width < 0    
 = error "Codec.BMP: Negative width field."

 | height < 0   
 = error "Codec.BMP: Negative height field."

 | height * width * 4 /= BS.length str
 = error "Codec.BMP: Image dimensions don't match input data."

 | otherwise
 = let  imageData       = packRGBA32ToBGR24 width height str
   in   packDataToBMP 24 width height imageData


-- data -----------------------------------------------------------------------
-- | Wrap pre-packed image data into BMP image.
--
packDataToBMP
	:: Int          -- ^ Number of bits per pixel
        -> Int 		-- ^ Width of image  (must be positive).
	-> Int 		-- ^ Height of image (must be positive).
	-> ByteString	-- ^ A string of RGBA component values.
                        --   Must have length (@width * height * 4@)
	-> BMP
	
packDataToBMP bits width height imageData
 = let  fileHeader
		= FileHeader
		{ fileHeaderType	= bmpMagic

		, fileHeaderFileSize	
                        = fromIntegral
			$ sizeOfFileHeader + sizeOfBitmapInfoV3	
                                           + BS.length imageData

		, fileHeaderReserved1	= 0
		, fileHeaderReserved2	= 0
		, fileHeaderOffset	
                        = fromIntegral (sizeOfFileHeader + sizeOfBitmapInfoV3)}

	bitmapInfoV3
		= BitmapInfoV3
		{ dib3Size		= fromIntegral sizeOfBitmapInfoV3
		, dib3Width		= fromIntegral width
		, dib3Height		= fromIntegral height
                , dib3HeightFlipped     = False
		, dib3Planes		= 1
		, dib3BitCount		= fromIntegral bits
		, dib3Compression	= CompressionRGB
		, dib3ImageSize		= fromIntegral $ BS.length imageData

		-- The default resolution seems to be 72 pixels per inch.
		--	This equates to 2834 pixels per meter.
		--	Dunno WTF this should be in the header though...
		, dib3PelsPerMeterX	= 2834
		, dib3PelsPerMeterY	= 2834

		, dib3ColorsUsed	= 0
		, dib3ColorsImportant	= 0 }
		
        -- We might as well check to see if we've made a well-formed BMP file.
        -- It would be sad if we couldn't read a file we just wrote.
	errs	= catMaybes		
			[ checkFileHeader   fileHeader
			, checkBitmapInfoV3 bitmapInfoV3 
                                           (fromIntegral $ BS.length imageData)]
		
   in	case errs of
	 [] -> BMP 
		{ bmpFileHeader		= fileHeader
		, bmpBitmapInfo		= InfoV3 bitmapInfoV3
		, bmpRawImageData	= imageData }
	 
	 _  -> error $ "Codec.BMP: Constructed BMP file has errors, sorry." 
                     ++ show errs


-------------------------------------------------------------------------------
-- | Pack RGBA data into the format need by BMP image data.
packRGBA32ToBGR24 
	:: Int		       -- ^ Width of image.
	-> Int		       -- ^ Height of image.
	-> ByteString          -- ^ Source bytestring holding the image data. 
	-> ByteString          --   output bytestring.
	
packRGBA32ToBGR24 width height str
 | height * width * 4 /= BS.length str
 = error "Codec.BMP: Image dimensions don't match input data."

 | otherwise
 = let	padPerLine	
	 = case (width * 3) `mod` 4 of
		0	-> 0
		x	-> 4 - x
				
	sizeDest	= height * (width * 3 + padPerLine)
   in	unsafePerformIO
	 $ allocaBytes sizeDest 	$ \bufDest ->
	   BS.unsafeUseAsCString str	$ \bufSrc  ->
	    do	packRGBA32ToBGR24' width height padPerLine
                        (castPtr bufSrc) (castPtr bufDest)
		bs	<- packCStringLen (bufDest, sizeDest)
		return bs
	
			
packRGBA32ToBGR24' width height pad ptrSrc ptrDest
 = go 0 0 0 0
 where
	go posX posY oSrc oDest

	 -- add padding bytes at the end of each line.
	 | posX == width
	 = do	mapM_ (\n -> pokeByteOff ptrDest (oDest + n) (0 :: Word8)) 
			$ P.take pad [0 .. ]
		go 0 (posY + 1) oSrc (oDest + pad)
		
	 -- we've finished the image.
	 | posY == height
	 = return ()
	
	 -- process a pixel
	 | otherwise
	 = do   red	:: Word8  <- peekByteOff ptrSrc (oSrc + 0)
                green	:: Word8  <- peekByteOff ptrSrc (oSrc + 1)
		blue	:: Word8  <- peekByteOff ptrSrc (oSrc + 2)
	
		pokeByteOff ptrDest (oDest + 0) blue
		pokeByteOff ptrDest (oDest + 1) green
		pokeByteOff ptrDest (oDest + 2) red
		
		go (posX + 1) posY (oSrc + 4) (oDest + 3)


-------------------------------------------------------------------------------
-- | Pack RGBA data into the byte order needed by BMP image data.
packRGBA32ToBGRA32 
	:: Int		       -- ^ Width of image.
	-> Int		       -- ^ Height of image.
	-> ByteString          -- ^ Source bytestring holding the image data. 
	-> ByteString          

packRGBA32ToBGRA32 width height str
 | height * width * 4 /= BS.length str
 = error "Codec.BMP: Image dimensions don't match input data."

 | otherwise
 = let  sizeDest	= height * (width * 4)
   in	unsafePerformIO
	 $ allocaBytes sizeDest 	$ \bufDest ->
	   BS.unsafeUseAsCString str	$ \bufSrc  ->
	    do	packRGBA32ToBGRA32' width height
                        (castPtr bufSrc) (castPtr bufDest)
		bs	<- packCStringLen (bufDest, sizeDest)
		return	bs
	
packRGBA32ToBGRA32' width height ptrSrc ptrDest
 = go 0 0 0 0
 where
	go posX posY oSrc oDest

	 -- advance to the next line.
	 | posX == width
	 = do  go 0 (posY + 1) oSrc oDest
		
	 -- we've finished the image.
	 | posY == height
	 = return ()
	
	 -- process a pixel
	 | otherwise
	 = do   red	:: Word8  <- peekByteOff ptrSrc (oSrc + 0)
                green	:: Word8  <- peekByteOff ptrSrc (oSrc + 1)
		blue	:: Word8  <- peekByteOff ptrSrc (oSrc + 2)
		alpha	:: Word8  <- peekByteOff ptrSrc (oSrc + 3)
	
		pokeByteOff ptrDest (oDest + 0) blue
		pokeByteOff ptrDest (oDest + 1) green
		pokeByteOff ptrDest (oDest + 2) red
		pokeByteOff ptrDest (oDest + 3) alpha
		
		go (posX + 1) posY (oSrc + 4) (oDest + 4)

