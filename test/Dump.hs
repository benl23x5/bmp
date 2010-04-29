
import Codec.BMP
import System.Environment
import System.IO
import Data.ByteString	as BS

main
 = do	[fileName]	<- getArgs
	handle		<- openFile fileName ReadMode
	Right bmp	<- hGetBMP handle
	
	let str	= BS.unpack $ unpackBMPToRGBA32	bmp
	print str
	
	