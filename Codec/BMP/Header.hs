
-- | Headers for the BMP file format.
module Codec.BMP.Header
	( FileHeader(..)
	, sizeOfFileHeader
	, unpackFileHeader

	, BitmapInfoV3(..)
	, sizeOfBitmapInfoV3
	, unpackBitmapInfoV3)
where
import Codec.BMP.Base
import Data.Word
import Data.ByteString


-- File Headers -----------------------------------------------------------------------------------
-- | BMP file header.
--	Numbers stated on each field are the offsets (in bytes) from the start of the header for that field.
data FileHeader
	= FileHeader			
	{ -- | (+0) Magic numbers 0x42 0x4d
	  fileHeaderType	:: Word16
	
	  -- | (+2) Size of the file, in bytes.
	, fileHeaderSize	:: Word32

	  -- | (+6) unused, must be zero.
	, fileHeaderReserved1	:: Word16

	  -- | (+8) unused, must be zero.
	, fileHeaderReserved2	:: Word16

	  -- | (+10) offset in bytes to the start of the pixel data.
	, fileHeaderOffset	:: Word32
	}
	deriving (Show)

-- | Size of a file header (in bytes).
sizeOfFileHeader :: Int
sizeOfFileHeader = 14

-- | Unpack a file header from a `ByteString`
unpackFileHeader :: ByteString -> FileHeader
unpackFileHeader str	
	= FileHeader
	{ fileHeaderType	= getLSBWord16 str 0
	, fileHeaderSize	= getLSBWord32 str 2
	, fileHeaderReserved1	= getLSBWord16 str 6
	, fileHeaderReserved2   = getLSBWord16 str 8
	, fileHeaderOffset	= getLSBWord32 str 10 }



-- Image Headers ----------------------------------------------------------------------------------
-- | Device Independent Bitmap (DIB) header for Windows V3.
--	Numbers stated on each field are the offsets (in bytes) from the start of the header for that field.
data BitmapInfoV3
	= ImageHeader				
	{ -- | (+0) Size of the image header, in bytes.
	  dib3Size		:: Word32

	  -- | (+4) Width of the image, in pixels.
	, dib3Width		:: Word32
	
	  -- | (+8) Height of the image, in pixels.
	, dib3Height		:: Word32
	
	  -- | (+12) Number of color planes.
	, dib3Planes		:: Word16

	  -- | (+14) Number of bits per pixel.
	, dib3BitCount		:: Word16

	  -- | (+16) Image compression mode. 0 = uncompressed.
	, dib3Compression	:: Word32

	  -- | (+20) Size of image. May be 0 for uncompressed image.
	, dib3ImageSize		:: Word32

	  -- | (+24) Perfered resolution in pixels per meter, along the X axis.
	, dib3PelsPerMeterX	:: Word32

	  -- | (+28) Perfered resolution in pixels per meter, along the Y axis.
	, dib3PelsPerMeterY	:: Word32

	  -- | (+32) Number of color entries that are used.
	, dib3ColorsUsed	:: Word32

	  -- | (+36) Number of significant colors.
	, dib3ColorsImportant	:: Word32
	}
	deriving (Show)


-- | Size of `BitmapInfoV3` header (in bytes)
sizeOfBitmapInfoV3 :: Int
sizeOfBitmapInfoV3 = 40

-- | Unpack a `BitmapInfoV3` header from a `ByteString`.
unpackBitmapInfoV3 :: ByteString -> BitmapInfoV3
unpackBitmapInfoV3 str
	= ImageHeader
	{ dib3Size		= getLSBWord32 str 0
	, dib3Width		= getLSBWord32 str 4
	, dib3Height		= getLSBWord32 str 8
	, dib3Planes		= getLSBWord16 str 12 
	, dib3BitCount		= getLSBWord16 str 14
	, dib3Compression	= getLSBWord32 str 16
	, dib3ImageSize		= getLSBWord32 str 20
	, dib3PelsPerMeterX	= getLSBWord32 str 24
	, dib3PelsPerMeterY	= getLSBWord32 str 28
	, dib3ColorsUsed	= getLSBWord32 str 32
	, dib3ColorsImportant	= getLSBWord32 str 36 }
	
	