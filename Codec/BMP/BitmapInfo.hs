{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.BitmapInfo
	( BitmapInfo	(..)
	, BitmapInfoV3	(..)
	, sizeOfBitmapInfoV3)

where
import Data.Binary
import Data.Binary.Get	

-- Image Headers ----------------------------------------------------------------------------------
-- | A wrapper for the bitmap info, 
--	in case we want to support other header types in the future.
data BitmapInfo
	= InfoV3 BitmapInfoV3
	deriving (Show)

-- | Device Independent Bitmap (DIB) header for Windows V3.
--	Numbers stated on each field are the offsets (in bytes) from the start of the header for that field.
data BitmapInfoV3
	= BitmapInfoV3			
	{ -- | Size of the image header, in bytes.
	  dib3Size		:: Word32

	  -- | Width of the image, in pixels.
	, dib3Width		:: Word32
	
	  -- | Height of the image, in pixels.
	, dib3Height		:: Word32
	
	  -- | Number of color planes.
	, dib3Planes		:: Word16

	  -- | Number of bits per pixel.
	, dib3BitCount		:: Word16

	  -- | Image compression mode. 0 = uncompressed.
	, dib3Compression	:: Word32

	  -- | Size of image.
	, dib3ImageSize		:: Word32

	  -- | Prefered resolution in pixels per meter, along the X axis.
	, dib3PelsPerMeterX	:: Word32

	  -- | Prefered resolution in pixels per meter, along the Y axis.
	, dib3PelsPerMeterY	:: Word32

	  -- | Number of color entries that are used.
	, dib3ColorsUsed	:: Word32

	  -- | Number of significant colors.
	, dib3ColorsImportant	:: Word32
	}
	deriving (Show)


-- | Size of `BitmapInfoV3` header (in bytes)
sizeOfBitmapInfoV3 :: Int
sizeOfBitmapInfoV3 = 40


instance Binary BitmapInfoV3 where
 get
  = do	size	<- getWord32le
	width	<- getWord32le
	height	<- getWord32le
	planes	<- getWord16le
	bitc	<- getWord16le
	comp	<- getWord32le
	imgsize	<- getWord32le
	pelsX	<- getWord32le
	pelsY	<- getWord32le
	cused	<- getWord32le
	cimp	<- getWord32le
	
	return	$ BitmapInfoV3
		{ dib3Size		= size
		, dib3Width		= width
		, dib3Height		= height
		, dib3Planes		= planes
		, dib3BitCount		= bitc
		, dib3Compression	= comp
		, dib3ImageSize		= imgsize
		, dib3PelsPerMeterX	= pelsX
		, dib3PelsPerMeterY	= pelsY
		, dib3ColorsUsed	= cused
		, dib3ColorsImportant	= cimp }

 put header
  = do	put	$ dib3Size header
	put	$ dib3Width header
	put	$ dib3Height header
	put 	$ dib3Planes header
	put	$ dib3BitCount header
	put	$ dib3Compression header
	put	$ dib3ImageSize header
	put	$ dib3PelsPerMeterX header
	put	$ dib3PelsPerMeterY header
	put	$ dib3ColorsUsed header
	put	$ dib3ColorsImportant header
	
	
	
	
	
	
	
	
