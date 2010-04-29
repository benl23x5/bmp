
import Codec.BMP
import System.Environment
import System.IO

main
 = do	[fileName]	<- getArgs
	h		<- openFile fileName ReadMode
	bmp		<- hGetBMP h
	print bmp
	
	