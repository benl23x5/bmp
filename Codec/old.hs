

-----
convertImage
	:: (?transColor	 	:: (Word8, Word8, Word8))	-- ^ Transparency color.
	-> (?ptrDest		:: Ptr Word8)			-- ^ Pointer to destination image.
	-> (?ptrSrc		:: Ptr Word8)			-- ^ Pointer to source image.
	-> (?width		:: Int)				-- ^ Width of image in pixels.
	-> (?height		:: Int)				-- ^ Height of image in pixels.
	-> (?slackPerLine	:: Int)				-- ^ Number of bytes of slack space per line.

	-> Int 							-- ^ X position in the image.
	-> Int							-- ^ Y position in the image.

	-> Int 							-- ^ Offset into source image, in bytes.
	-> Int							-- ^ Offset into destination image, in bytes.
	-> IO ()

-- ^ Performs conversion between 24bit RGB and 32bit RGBA. 

convertImage
	posX  posY
	oSrc  oDest		

 | posX	== ?width
 = convertImage 0 (posY + 1) (oSrc + ?slackPerLine) oDest
 
 | posY	== ?height
 = return ()
 
 | otherwise
 = do
 	red	<- peekByteOff ?ptrSrc (oSrc + 2)
	green	<- peekByteOff ?ptrSrc (oSrc + 1)
	blue	<- peekByteOff ?ptrSrc (oSrc + 0)
	
	pokeByteOff ?ptrDest (oDest + 3) red
	pokeByteOff ?ptrDest (oDest + 2) green
	pokeByteOff ?ptrDest (oDest + 1) blue
	
	let (transRed, transGreen, transBlue)
		= ?transColor
	
 	(if  red   == transRed
  	  && green == transGreen
	  && blue  == transBlue
	 	then pokeByteOff ?ptrDest (oDest + 0) (0 	:: Word8)
		else pokeByteOff ?ptrDest (oDest + 0) (255	:: Word8))
		
	convertImage
		(posX + 1) posY 
		(oSrc + 3) (oDest + 4)




-----
-- |	Loads a BMP file in uncompressed 24bit RGB format.
--	The resulting data is in uncompressed 32bit RGBA format.

loadBMP24 
	:: (Word8, Word8, Word8) 			-- ^ The RGB color to treat as transparent.
							--	If a pixel in the BMP file has this color then the corresponding
							--	pixel in the loaded image with have an alpha value of 0. All
							--	other pixels in the loaded image will have an alpha of 1.

	-> FilePath 					-- ^ Path to the image file.
	-> IO (FileHeader, ImageHeader, Ptr Word8)
	
loadBMP24
	transColor
	filePath
 = do
	h		<- openBinaryFile filePath ReadMode

	---------------
	-- read the file header
	--
	fileHeaderBuf		<- mallocBytes fileHeaderSize
	fileHeaderRead		<- hGetBuf h fileHeaderBuf fileHeaderSize

	when (fileHeaderRead /= fileHeaderSize)
	 (error "loadBMP24: read of file header failed")
	
	fileHeader		<- peekFileHeader fileHeaderBuf

	when debug
	 $ do	putStr	$  "* fileHeader\n"
	 		++ show fileHeader
			++ "\n"
	
	when (fileType fileHeader /= 0x4d42)
	 $ do	error $ "loadBMP24: Bad magic in " ++ show filePath ++ " this doesn't look like a BMP file."
	 
	 

	
	
	---------------
	-- read the image header
	--
	imageHeaderBuf		<- mallocBytes imageHeaderSize
	imageHeaderRead		<- hGetBuf h imageHeaderBuf imageHeaderSize
	
	when (imageHeaderRead /= imageHeaderSize)
	 (error "loadBMP24: read of image header failed")
	 
	imageHeader		<- peekImageHeader imageHeaderBuf

	when debug
	 $ do	putStr	$ "* imageHeader\n"
	 		++ show imageHeader
			++ "\n"
	
	when (imagePlanes imageHeader /= 1)
	 (error $ "loadBMP24: Unhandled planes count, " ++ (show $ imagePlanes imageHeader))
	 
	when (imageBitCount imageHeader /= 24)
	 (error $ "loadBMP24: Unhandled color depth, "	++ (show $ imageBitCount imageHeader))
	 
	when (imageCompression imageHeader /= 0)
	 (error $ "loadBMP24: Unhandled compression, "	++ (show $ imageCompression imageHeader))

	-- sanity check
	when (	imageSizeImage imageHeader `mod` imageHeight imageHeader /= 0)
	 (error $ "loadBMP24: Image doesn't have a whole number of lines!\n")
	 
	let bytesPerLine	= fromIntegral $ imageSizeImage imageHeader `div` imageHeight imageHeader
	let slackPerLine	= fromIntegral $ bytesPerLine  - (imageWidth imageHeader * 3)

	when debug
	 $ do	putStr	$  "  bytesPerLine = " ++ show bytesPerLine ++ "\n"
			++ "  slackPerLine = " ++ show slackPerLine ++ "\n"
	
	-- load in the image data
	when debug	
	 $ do	putStr	$ "* reading image\n"

	imageSrc		<- mallocBytes (fromIntegral $ imageSizeImage imageHeader)
	imageRead		<- hGetBuf h imageSrc (fromIntegral $ imageSizeImage imageHeader)

	-- convert to flat RGBA format
	let destSize		= imageHeight imageHeader
				* imageWidth  imageHeader
				* 4
	
	imageData		<- mallocBytes (fromIntegral destSize)
	 	
	when debug
	 $ do	putStr	$ "* converting image\n"

	-----
	let ?transColor		= transColor
	let ?ptrDest		= imageData
	let ?ptrSrc		= imageSrc
	let ?width		= fromIntegral $ imageWidth 	imageHeader
	let ?height		= fromIntegral $ imageHeight	imageHeader
	let ?slackPerLine	= slackPerLine
	convertImage 0 0 0 0
	-----

	return	(fileHeader, imageHeader, imageData)