{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.BitmapInfoV5
	( BitmapInfoV5	(..)
	, CIEXYZ        (..)
	, sizeOfBitmapInfoV5
	, checkBitmapInfoV5)

where
import Codec.BMP.Error
import Data.Binary
import Data.Binary.Get	
import Data.Binary.Put

-- | Device Independent Bitmap (DIB) header for Windows V5 (98/2000 and newer)
data BitmapInfoV5
	= BitmapInfoV5
	{ -- | Size of the image header, in bytes.
	  dib5Size		:: Word32

	  -- | Width of the image, in pixels.
	, dib5Width		:: Word32
	
	  -- | Height of the image, in pixels.
	, dib5Height		:: Word32
	
	  -- | Number of color planes.
	, dib5Planes		:: Word16

	  -- | Number of bits per pixel.
	, dib5BitCount		:: Word16

	  -- | Image compression mode. 0 = uncompressed.
	, dib5Compression	:: Word32

	  -- | Size of raw image data.
	, dib5ImageSize		:: Word32

	  -- | Prefered resolution in pixels per meter, along the X axis.
	, dib5PelsPerMeterX	:: Word32

	  -- | Prefered resolution in pixels per meter, along the Y axis.
	, dib5PelsPerMeterY	:: Word32

	  -- | Number of color entries that are used.
	, dib5ColorsUsed	:: Word32

	  -- | Number of significant colors.
	, dib5ColorsImportant	:: Word32

	  -- | Color masks specify components of each pixel.
	  --   Only used with the bitfields compression mode.
	, dib5RedMask		:: Word32
	, dib5GreenMask		:: Word32
	, dib5BlueMask		:: Word32
	, dib5AlphaMask		:: Word32

	-- | The color space used by the image.
	, dib5ColorSpaceType	:: Word32

	-- | Specifies the XYZ coords of the three colors that correspond to the RGB endpoints
	--   for the logical color space associated with the bitmap. 
	--   Only used when ColorSpaceType specifies a calibrated image.
	, dib5Endpoints		:: (CIEXYZ, CIEXYZ, CIEXYZ)

	-- | Toned response curves for each component. 
	--   Only used when the ColorSpaceType specifies a calibrated image.
	, dib5GammaRed		:: Word32
	, dib5GammaGreen	:: Word32
	, dib5GammaBlue		:: Word32

	-- | Rendering intent for the bitmap.
	, dib5Intent		:: Word32

	-- | Offset (in bytes) from the beginning of the header to the start of the profile data.
	, dib5ProfileData	:: Word32

	-- | Size (in bytes) of embedded profile data.
	, dib5ProfileSize	:: Word32
	
	-- | Reserved, should be zero.
	, dib5Reserved		:: Word32
	}
	deriving (Show)


-- | Contains the XYZ coordinates of a specific color in a specified color space.
data CIEXYZ 
	= CIEXYZ Word32 Word32 Word32
	deriving Show

-- | Size of `BitmapInfoV5` header (in bytes)
sizeOfBitmapInfoV5 :: Int
sizeOfBitmapInfoV5 = 124


instance Binary BitmapInfoV5 where
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
	rmask	<- getWord32le
	gmask	<- getWord32le
	bmask	<- getWord32le
	amask	<- getWord32le
	cstype	<- getWord32le
	ends	<- get
	rgamma	<- getWord32le
	ggamma	<- getWord32le
	bgamma	<- getWord32le
	intent	<- getWord32le
	pdata	<- getWord32le
	psize	<- getWord32le
	res	<- getWord32le
	
	return	$ BitmapInfoV5
		{ dib5Size		= size
		, dib5Width		= width
		, dib5Height		= height
		, dib5Planes		= planes
		, dib5BitCount		= bitc
		, dib5Compression	= comp
		, dib5ImageSize		= imgsize
		, dib5PelsPerMeterX	= pelsX
		, dib5PelsPerMeterY	= pelsY
		, dib5ColorsUsed	= cused
		, dib5ColorsImportant	= cimp
		, dib5RedMask		= rmask
		, dib5GreenMask		= gmask
		, dib5BlueMask		= bmask
		, dib5AlphaMask		= amask
		, dib5ColorSpaceType	= cstype
		, dib5Endpoints		= ends
		, dib5GammaRed		= rgamma
		, dib5GammaGreen	= ggamma
		, dib5GammaBlue		= bgamma
		, dib5Intent		= intent
		, dib5ProfileData	= pdata
		, dib5ProfileSize	= psize
		, dib5Reserved		= res }
		

 put header
  = do	putWord32le 	$ dib5Size		header
	putWord32le	$ dib5Width		header
	putWord32le	$ dib5Height		header
	putWord16le	$ dib5Planes		header
	putWord16le	$ dib5BitCount		header
	putWord32le	$ dib5Compression	header
	putWord32le	$ dib5ImageSize		header
	putWord32le	$ dib5PelsPerMeterX	header
	putWord32le	$ dib5PelsPerMeterY	header
	putWord32le	$ dib5ColorsUsed	header
	putWord32le	$ dib5ColorsImportant	header
	putWord32le	$ dib5RedMask		header
	putWord32le	$ dib5GreenMask		header
	putWord32le	$ dib5BlueMask		header
	putWord32le	$ dib5AlphaMask		header
	putWord32le	$ dib5ColorSpaceType	header
	put		$ dib5Endpoints 	header
	putWord32le	$ dib5GammaRed		header
	putWord32le	$ dib5GammaGreen	header
	putWord32le	$ dib5GammaBlue		header
	putWord32le	$ dib5Intent		header
	putWord32le	$ dib5ProfileData	header
	putWord32le	$ dib5ProfileSize	header
	putWord32le	$ dib5Reserved		header


instance Binary CIEXYZ where
 get 
  = do	r	<- getWord32le
	g	<- getWord32le
	b	<- getWord32le
	return	$ CIEXYZ r g b
	
 put (CIEXYZ r g b)
  = do	putWord32le r
	putWord32le g
	putWord32le b

	
-- | Check headers for problems and unsupported features.	 
checkBitmapInfoV5 :: BitmapInfoV5 ->  Maybe Error
checkBitmapInfoV5 header
	
	| dib5Size header /= (fromIntegral sizeOfBitmapInfoV5)
	= Just	$ ErrorUnhandledBitmapHeaderSize 
		$ fromIntegral $ dib5Size header
	
	| dib5Planes header /= 1
	= Just	$ ErrorUnhandledPlanesCount 
		$ fromIntegral $ dib5Planes header
	
	| dib5BitCount header /= 24
	= Just 	$ ErrorUnhandledColorDepth  
		$ fromIntegral $ dib5BitCount header
	
	| dib5Compression header /= 0
	= Just	$ ErrorUnhandledCompressionMode 
		$ fromIntegral $ dib5Compression header

	| dib5ImageSize header == 0
	= Just $ ErrorZeroImageSize
	
	| dib5ImageSize header `mod` dib5Height header /= 0
	= Just $ ErrorLacksWholeNumberOfLines

	| otherwise
	= Nothing
