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
  


PULL REQUESTS
-------------

As `bmp` is now fairly stable, I typically make releases only when there is a new GHC version. If you have added a new feature or fixed a bug, and want your pull request merged sooner than that, then send email to benl AT ouroborus.net. I don't pay attention to github notifications, but am happy to receive emails from people. If you have changed any internal functionality then please test that all the example images under `data/` still load with the `test/Dump.hs` harness. 

If you just want to bump a dependency version then get a Hackage trustee to edit the package information directly on Hackage. You don't need to raise a pull request here.
