{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

-- | Reading and writing uncompressed BMP files.
--
--   Reading works for both uncompressed 24bit RGB and 32bit RGBA
--   WindowsV3, WindowsV4 and WindowsV5 formats.
-- 
--   Writing is limited to the uncompressed 24bit RGB WindowsV3 format.
--
--   We don't support the plain OS/2 BitmapCoreHeader
--       and BitmapCoreHeader2 image headers, but I haven't yet seen one of
--       these in the wild.
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
-- Release Notes:
--
--  >  * bmp 1.2.3
--  >    Add pure parseBMP / renderBMP API.
--  >
--  >  * bmp 1.2.2
--  >    Allow the physical image buffer to be larger than the image
--  >     size stated in the header, to accept output of foolish Win7 codec.
--  >
--  >  * bmp 1.2.1
--  >    Fix slow ByteString conversion via lists.  
--  >
--  >  * bmp 1.2.0
--  >    Accept files with zero padding on the end of the file.
--  >    Accept RGBA files with V3 headers.
--  >
--  >  * bmp 1.1.2   
--  >    Accept files with the image size field set to zero.
--
module Codec.BMP
	( -- * Data Structures
          BMP		  (..)
	, FileHeader  	  (..)
	, BitmapInfo      (..)
	, BitmapInfoV3	  (..)
	, BitmapInfoV4    (..)
	, BitmapInfoV5    (..)
	, Compression     (..)
	, CIEXYZ          (..)
	, Error           (..)

          -- * Reading
	, readBMP,  hGetBMP, parseBMP

          -- * Writing
	, writeBMP, hPutBMP, renderBMP

          -- * Pack and Unpack
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

-- Reading --------------------------------------------------------------------
-- | Read a BMP from a file.
--      The file is checked for problems and unsupported features when read.
--      If there is anything wrong this gives an `Error` instead.
readBMP :: FilePath -> IO (Either Error BMP)
readBMP fileName
 = do	h	<- openBinaryFile fileName ReadMode
	hGetBMP h
	
-- | Get a BMP image from a file handle.
hGetBMP :: Handle -> IO (Either Error BMP)
hGetBMP h
 = do	-- lazily load the whole file
	buf	<- BSL.hGetContents h
        return $ parseBMP buf


-- | Parse a BMP image from a lazy `ByteString`
parseBMP :: BSL.ByteString -> Either Error BMP
parseBMP buf
 = let  -- split off the file header
	(bufFileHeader, bufRest) 
	       = BSL.splitAt (fromIntegral sizeOfFileHeader) buf

   in   if (fromIntegral $ BSL.length bufFileHeader) /= sizeOfFileHeader
	 then	Left ErrorFileHeaderTruncated
	 else	parseBMP2 bufRest (decode bufFileHeader)
	
parseBMP2 buf fileHeader
 -- Check the magic before doing anything else.
 | fileHeaderType fileHeader /= bmpMagic
 = Left $ ErrorBadMagic (fileHeaderType fileHeader)
	
 | otherwise
 = let	-- Next comes the image header. 
	-- The first word tells us how long it is.
	sizeHeader	= runGet getWord32le buf
	
	-- split off the image header
	(bufImageHeader, bufRest)
		= BSL.splitAt (fromIntegral sizeHeader) buf
        
        -- How much non-header data is present in the file.
        -- For uncompressed data without a colour table, the remaining data
        -- should be the image, but there may also be padding bytes on the
        -- end.
        physicalBufferSize
                = (fromIntegral $ BSL.length bufRest) :: Word32

   in if (fromIntegral $ BSL.length bufImageHeader) /= sizeHeader
	 then 	Left ErrorImageHeaderTruncated
	 else 	parseBMP3 fileHeader bufImageHeader bufRest physicalBufferSize


parseBMP3 fileHeader bufImageHeader bufRest physicalBufferSize
	| BSL.length bufImageHeader == 40 
	= let info	= decode bufImageHeader
	  in  case checkBitmapInfoV3 info physicalBufferSize of
		 Just err	-> Left err
		 Nothing
                  |  Just imageSize      <- imageSizeFromBitmapInfoV3 info
                  -> parseBMP4 fileHeader (InfoV3 info) bufRest imageSize

                  |  otherwise
                  -> Left $ ErrorInternalErrorPleaseReport

	| BSL.length bufImageHeader == 108
	= let info	= decode bufImageHeader
	  in  case checkBitmapInfoV4 info physicalBufferSize of
		 Just err	-> Left err
		 Nothing	
                  | Just imageSize      <- imageSizeFromBitmapInfoV4 info
                  -> parseBMP4 fileHeader (InfoV4 info) bufRest imageSize

                  | otherwise
                  -> Left $ ErrorInternalErrorPleaseReport
		
	| BSL.length bufImageHeader == 124
	= let info	= decode bufImageHeader
	  in  case checkBitmapInfoV5 info physicalBufferSize of
		 Just err	-> Left err
		 Nothing	
                  | Just imageSize      <- imageSizeFromBitmapInfoV5 info
                  -> parseBMP4 fileHeader (InfoV5 info) bufRest imageSize

                  | otherwise
                  -> Left $ ErrorInternalErrorPleaseReport
		
	| otherwise
 	= Left $ ErrorUnhandledBitmapHeaderSize 
               $ fromIntegral $ BSL.length bufImageHeader


parseBMP4 fileHeader imageHeader bufImage (sizeImage :: Int)
 = let  bufLen  = fromIntegral $ BSL.length bufImage
   in   if bufLen < sizeImage
	 then Left $ ErrorImageDataTruncated sizeImage bufLen
	 else Right $ BMP 
		{ bmpFileHeader 	= fileHeader
		, bmpBitmapInfo		= imageHeader
		, bmpRawImageData	= BS.concat $ BSL.toChunks bufImage }


-- Writing --------------------------------------------------------------------
-- | Wrapper for `hPutBMP`
writeBMP :: FilePath -> BMP -> IO ()
writeBMP fileName bmp
 = do	h	<- openBinaryFile fileName WriteMode
	hPutBMP h bmp
	hFlush h
	hClose h


-- | Put a BMP image to a file handle.
hPutBMP :: Handle -> BMP -> IO ()
hPutBMP h bmp
        = BSL.hPut h (renderBMP bmp)


-- | Render a BMP image to a lazy `ByteString`.
renderBMP :: BMP -> BSL.ByteString
renderBMP bmp
        = BSL.append    (encode $ bmpFileHeader bmp)
        $ BSL.append    (encode $ bmpBitmapInfo bmp)
        $ BSL.fromStrict (bmpRawImageData bmp) 


-- | Get the width and height of an image.
--	It's better to use this function than to access the headers directly.
bmpDimensions :: BMP -> (Int, Int)
bmpDimensions bmp	
 = let	info	= getBitmapInfoV3 $ bmpBitmapInfo bmp
   in	( fromIntegral $ dib3Width info
	, fromIntegral $ dib3Height info)


