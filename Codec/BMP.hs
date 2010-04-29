{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

-- | Reading and writing uncompressed 24 bit BMP files.
--	We only handle Windows V3 file headers, but this is the most common.
module Codec.BMP
	( BMP(..)
	, hGetBMP)
where
import Codec.BMP.Header
import Codec.BMP.Error
import System.IO
import Data.ByteString		as BS


-- | A BMP image.
--	The image data may contain padding bytes for each line, to bring them
--	up to a multiple of 4 bytes.
data BMP
	= BMP
	{ bmpFileHeader		:: FileHeader
	, bmpBitmapInfo		:: BitmapInfoV3
	, bmpRawImageData	:: ByteString}
	deriving Show
		

-- | Get a BMP image from a file handle.
hGetBMP :: Handle -> IO (Either Error BMP)
hGetBMP h
 = do	-- load the file header.
	buf	<- BS.hGet h sizeOfFileHeader
	if BS.length buf /= sizeOfFileHeader
	 then 	return $ Left ErrorReadOfFileHeaderFailed
	 else do
		let fileHeader	= unpackFileHeader buf
		print fileHeader
		hGetBMP2 h fileHeader
	
	
hGetBMP2 h fileHeader
 | fileHeaderType fileHeader /= 0x4d42
 = return $ Left $ ErrorBadMagic (fileHeaderType fileHeader)
	
 | otherwise
 = do	-- load the image header.
	buf	<- BS.hGet h sizeOfBitmapInfoV3
	if BS.length buf /= sizeOfBitmapInfoV3
	 then 	return $ Left ErrorReadOfImageHeaderFailed
	 else do
		let imageHeader	= unpackBitmapInfoV3 buf
		print imageHeader
		hGetBMP3 h fileHeader imageHeader
			
hGetBMP3 h fileHeader imageHeader
 | Just err	<- checkHeaders fileHeader imageHeader
 = return $ Left err

 | otherwise
 = do	-- load the image data.
	let len		= fromIntegral $ dib3ImageSize imageHeader
	imageData	<- BS.hGet h len
				
	if BS.length imageData /= len
	 then return $ Left ErrorReadOfImageDataFailed
	 else return 
		$ Right $ BMP 
		{ bmpFileHeader 	= fileHeader
		, bmpBitmapInfo		= imageHeader
		, bmpRawImageData	= imageData }


-- | Check headers for problems and unsupported features.	 
checkHeaders :: FileHeader -> BitmapInfoV3 ->  Maybe Error
checkHeaders _fileHeader imageHeader
	| dib3Planes imageHeader /= 1
	= Just	$ ErrorUnhandledPlanesCount 
		$ fromIntegral $ dib3Planes imageHeader
	
	| dib3BitCount imageHeader /= 24
	= Just 	$ ErrorUnhandledColorDepth  
		$ fromIntegral $ dib3BitCount imageHeader
	
	| dib3Compression imageHeader /= 0
	= Just	$ ErrorUnhandledCompressionMode 
		$ fromIntegral $ dib3Compression imageHeader
	
	| dib3ImageSize imageHeader `mod` dib3Height imageHeader /= 0
	= Just $ ErrorLacksWholeNumberOfLines

	| otherwise
	= Nothing

