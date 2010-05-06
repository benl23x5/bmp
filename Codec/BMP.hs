{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

-- | Reading and writing uncompressed 24 bit BMP files.
--
-- To write a file do something like:
--
--  > do let rgba   = Data.ByteString.pack [some list of Word8s]
--  >    let bmp    = packRGBA32ToBMP width height rgba
--  >    writeBMP fileName bmp
--
-- To read a file do something like:
--
--  > do Right bmp  <- readBMP fileName
--  >    let rgba   =  unpackBMPToRGBA32 bmp
--  >    let (width, height) = bmpDimensions bmp
--  >    ... 
--      
-- Note: We don't currently support the plain OS/2 BitmapCoreHeader
--       and BitmapCoreHeader2 image headers. I'm not sure how common
--       these are in the wild. 
--
--
module Codec.BMP
	( BMP		(..)
	, FileHeader	(..)
	, BitmapInfo    (..)
	, BitmapInfoV3	(..)
	, BitmapInfoV4  (..)
	, BitmapInfoV5  (..)
	, CIEXYZ        (..)
	, Error         (..)
	, readBMP
	, writeBMP
	, hGetBMP
	, hPutBMP
	, packRGBA32ToBMP 
	, unpackBMPToRGBA32
	, bmpDimensions)
where
import Codec.BMP.Base
import Codec.BMP.Error
import Codec.BMP.Unpack
import Codec.BMP.Pack
import Codec.BMP.FileHeader
import Codec.BMP.BitmapInfo
import Codec.BMP.BitmapInfoV3
import Codec.BMP.BitmapInfoV4
import Codec.BMP.BitmapInfoV5
import System.IO
import Data.ByteString		as BS
import Data.ByteString.Lazy	as BSL
import Data.Binary
import Data.Binary.Get

-- Reading ----------------------------------------------------------------------------------------
-- | Wrapper for `hGetBMP`
readBMP :: FilePath -> IO (Either Error BMP)
readBMP fileName
 = do	h	<- openBinaryFile fileName ReadMode
	hGetBMP h
	
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
 -- Check the magic before doing anything else.
 --	If the specified file is not a BMP file then we'd prefer to get 
 --	this error than a `ReadOfImageHeaderFailed`.
 | fileHeaderType fileHeader /= bmpMagic
 = return $ Left $ ErrorBadMagic (fileHeaderType fileHeader)
	
 | otherwise
 = do	-- Next comes the image header. 
	-- The first word tells us which header format it is.
	bufSize		<- BSL.hGet h 4
	let sizeHeader	= runGet getWord32le bufSize
	
	-- Load the rest of the image header
	let sizeRest	= fromIntegral sizeHeader - 4
	bufRest	<- BSL.hGet h sizeRest
	if (fromIntegral $ BSL.length bufRest) /= sizeRest
	 then 	return $ Left ErrorReadOfImageHeaderFailed
	 else do
		let bufHeader 	= BSL.append bufSize bufRest
		hGetBMP3 h fileHeader sizeHeader bufHeader
		
			
hGetBMP3 h fileHeader sizeHeader bufHeader
	| sizeHeader == 40 
	= do	let info	= decode bufHeader
		case checkBitmapInfoV3 info of
		 Just err	-> return $ Left err
		 Nothing	-> hGetBMP4 h fileHeader (InfoV3 info) 
					(fromIntegral $ dib3ImageSize info)

	| sizeHeader == 108
	= do	let info	= decode bufHeader
		case checkBitmapInfoV4 info of
		 Just err	-> return $ Left err
		 Nothing	-> hGetBMP4 h fileHeader (InfoV4 info) 
					(fromIntegral 
						$ dib3ImageSize 
						$ dib4InfoV3 info)
		
	| sizeHeader == 124
	= do	let info	= decode bufHeader
		case checkBitmapInfoV5 info of
		 Just err	-> return $ Left err
		 Nothing	-> hGetBMP4 h fileHeader (InfoV5 info) 
					(fromIntegral
					 	$ dib3ImageSize 
						$ dib4InfoV3
						$ dib5InfoV4 info)
		
	| otherwise
 	= return $ Left $ ErrorUnhandledBitmapHeaderSize (fromIntegral sizeHeader)


hGetBMP4 h fileHeader imageHeader sizeImage
 = do	-- load the image data.
	imageData	<- BS.hGet h sizeImage
				
	if (fromIntegral $ BS.length imageData) /= sizeImage
	 then return $ Left ErrorReadOfImageDataFailed
	 else return 
		$ Right $ BMP 
		{ bmpFileHeader 	= fileHeader
		, bmpBitmapInfo		= imageHeader
		, bmpRawImageData	= imageData }


-- Writing ----------------------------------------------------------------------------------------
-- | Wrapper for `hPutBMP`
writeBMP :: FilePath -> BMP -> IO ()
writeBMP fileName bmp
 = do	h	<- openBinaryFile fileName WriteMode
	hPutBMP h bmp
	hFlush h


-- | Put a BMP image to a file handle, in Windows V3 format.
--	The size of the provided image data is checked against the given dimensions.
--	If these don't match then `error`.
hPutBMP :: Handle -> BMP -> IO ()
hPutBMP h bmp
 = do	BSL.hPut h (encode $ bmpFileHeader bmp)
	BSL.hPut h (encode $ bmpBitmapInfo bmp)
	BS.hPut h $ bmpRawImageData bmp


-- | Get the width and height of an image.
--	It's better to use this function than to access the headers directly.
bmpDimensions :: BMP -> (Int, Int)
bmpDimensions bmp	
	= dimsBitmapInfo $ bmpBitmapInfo bmp
