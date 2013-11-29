bmp
===

Reading and writing uncompressed BMP files.

* Supports uncompressed 24bit RGB and 32bit RGBA WindowsV3, WindowsV4 and WindowsV5 formats.
 
* We don't support the plain OS/2 BitmapCoreHeader and BitmapCoreHeader2 image headers, but I haven't yet seen one of these in the wild.
 
To write a file do something like:

    do let rgba   = Data.ByteString.pack [some list of Word8s]
       let bmp    = packRGBA32ToBMP width height rgba
       writeBMP fileName bmp

To read a file do something like:

    do Right bmp  <- readBMP fileName
       let rgba   =  unpackBMPToRGBA32 bmp
       let (width, height) = bmpDimensions bmp
       ...
  
