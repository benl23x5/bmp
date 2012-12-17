
import Codec.BMP
import System.Environment
import System.IO

main
 = do	[fileName]	<- getArgs
	handle		<- openFile fileName ReadMode

	mBMP		<- hGetBMP handle
	case mBMP of
	 Left err	-> print err
	 Right bmp	
          -> do print $ bmpFileHeader bmp
                print $ bmpBitmapInfo bmp
	
