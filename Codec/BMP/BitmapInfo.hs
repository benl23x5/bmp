{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.BitmapInfo
	( BitmapInfo	(..)
	, BitmapInfoV3	(..)
	, sizeOfBitmapInfoV3
	, checkBitmapInfoV3)

where
import Codec.BMP.Error
import Data.Binary
import Data.Binary.Get	
import Data.Binary.Put

-- Image Headers ----------------------------------------------------------------------------------
-- | A wrapper for the bitmap info, 
--	in case we want to support other header types in the future.
data BitmapInfo
	= InfoV3 BitmapInfoV3
	deriving (Show)

instance Binary BitmapInfo where
 get
  = do	info	<- get
	return	$ InfoV3 info

 put (InfoV3 info)
  	= put info

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

	  -- | Size of raw image data.
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
  = do	putWord32le 	$ dib3Size header
	putWord32le	$ dib3Width header
	putWord32le	$ dib3Height header
	putWord16le	$ dib3Planes header
	putWord16le	$ dib3BitCount header
	putWord32le	$ dib3Compression header
	putWord32le	$ dib3ImageSize header
	putWord32le	$ dib3PelsPerMeterX header
	putWord32le	$ dib3PelsPerMeterY header
	putWord32le	$ dib3ColorsUsed header
	putWord32le	$ dib3ColorsImportant header
	
	
-- | Check headers for problems and unsupported features.	 
checkBitmapInfoV3 :: BitmapInfoV3 ->  Maybe Error
checkBitmapInfoV3 header
	
	| dib3Size header /= (fromIntegral sizeOfBitmapInfoV3)
	= Just	$ ErrorUnhandledBitmapHeaderSize 
		$ fromIntegral $ dib3Size header
	
	| dib3Planes header /= 1
	= Just	$ ErrorUnhandledPlanesCount 
		$ fromIntegral $ dib3Planes header
	
	| dib3BitCount header /= 24
	= Just 	$ ErrorUnhandledColorDepth  
		$ fromIntegral $ dib3BitCount header
	
	| dib3Compression header /= 0
	= Just	$ ErrorUnhandledCompressionMode 
		$ fromIntegral $ dib3Compression header

	| dib3ImageSize header == 0
	= Just $ ErrorZeroImageSize
	
	| dib3ImageSize header `mod` dib3Height header /= 0
	= Just $ ErrorLacksWholeNumberOfLines

	| otherwise
	= Nothing


	
	
	
	
	
