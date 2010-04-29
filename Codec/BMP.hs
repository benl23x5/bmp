{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

-- | Reading and writing uncompressed 24 bit BMP files.
--	We only handle Windows V3 file headers, but this is the most common.
--
--  Typical usage is like:
--
--  > do handle     <- openFile fileName ReadMode
--  >    Right bmp  <- hGetBMP handle 
--  >    let rgba   =  unpackBMPToRGBA32 bmp
--  >    ... 
--  
--
module Codec.BMP
	( BMP		(..)
	, FileHeader	(..)
	, BitmapInfo    (..)
	, BitmapInfoV3	(..)
	, Error         (..)
	, hGetBMP
	, unpackBMPToRGBA32)
where
import Codec.BMP.Types
import Codec.BMP.Error
import Codec.BMP.Unpack
import Codec.BMP.FileHeader
import Codec.BMP.BitmapInfo
import Data.Binary
import System.IO
import Data.ByteString		as BS
import Data.ByteString.Lazy	as BSL

		
-- | Get a BMP image from a file handle.
--	The file is checked for problems and unsupported features when read.
--	If there is anything wrong this gives an `Error` instead.
hGetBMP :: Handle -> IO (Either Error BMP)
hGetBMP h
 = do	-- load the file header.
	buf	<- BSL.hGet h sizeOfFileHeader
	if (fromIntegral $ BSL.length buf) /= sizeOfFileHeader
	 then 	return $ Left ErrorReadOfFileHeaderFailed
	 else	hGetBMP2 h (decode buf)
	
	
hGetBMP2 h fileHeader
 | fileHeaderType fileHeader /= 0x4d42
 = return $ Left $ ErrorBadMagic (fileHeaderType fileHeader)
	
 | otherwise
 = do	-- load the image header.
	buf	<- BSL.hGet h sizeOfBitmapInfoV3
	if (fromIntegral $ BSL.length buf) /= sizeOfBitmapInfoV3
	 then 	return $ Left ErrorReadOfImageHeaderFailed
	 else 	hGetBMP3 h fileHeader (decode buf)
			
hGetBMP3 h fileHeader imageHeader
 | Just err	<- checkHeaders fileHeader imageHeader
 = return $ Left err

 | otherwise
 = do	-- load the image data.
	let len		= fromIntegral $ dib3ImageSize imageHeader
	imageData	<- BS.hGet h len
				
	if (fromIntegral $ BS.length imageData) /= len
	 then return $ Left ErrorReadOfImageDataFailed
	 else return 
		$ Right $ BMP 
		{ bmpFileHeader 	= fileHeader
		, bmpBitmapInfo		= InfoV3 imageHeader
		, bmpRawImageData	= imageData }


-- | Check headers for problems and unsupported features.	 
checkHeaders :: FileHeader -> BitmapInfoV3 ->  Maybe Error
checkHeaders fileHeader imageHeader
	| fileHeaderReserved1 fileHeader /= 0
	= Just 	$ ErrorReservedFieldNotZero

	| fileHeaderReserved2 fileHeader /= 0
	= Just 	$ ErrorReservedFieldNotZero

	| dib3Planes imageHeader /= 1
	= Just	$ ErrorUnhandledPlanesCount 
		$ fromIntegral $ dib3Planes imageHeader
	
	| dib3BitCount imageHeader /= 24
	= Just 	$ ErrorUnhandledColorDepth  
		$ fromIntegral $ dib3BitCount imageHeader
	
	| dib3Compression imageHeader /= 0
	= Just	$ ErrorUnhandledCompressionMode 
		$ fromIntegral $ dib3Compression imageHeader

	| dib3ImageSize imageHeader == 0
	= Just $ ErrorZeroImageSize
	
	| dib3ImageSize imageHeader `mod` dib3Height imageHeader /= 0
	= Just $ ErrorLacksWholeNumberOfLines


	| otherwise
	= Nothing

