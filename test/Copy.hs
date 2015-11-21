
import Codec.BMP
import System.Environment
import System.IO


main
 = do   [srcFile, dstFile] <- getArgs

        -- read it in
        handle          <- openFile srcFile ReadMode
        mBMP            <- hGetBMP handle
        case mBMP of
         Left err       -> print err
         Right bmp
          -> do let rgba        = unpackBMPToRGBA32 bmp
                let (width, height)     = bmpDimensions bmp

                -- write it back
                let bmp'        = packRGBA32ToBMP width height rgba
                handle'         <- openFile dstFile WriteMode
                hPutBMP handle' bmp'

