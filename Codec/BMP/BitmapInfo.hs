{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.BitmapInfo
	(BitmapInfo	(..))
where
import Codec.BMP.BitmapInfoV3
import Codec.BMP.BitmapInfoV5
import Data.Binary
import Data.Binary.Get

-- Image Headers ----------------------------------------------------------------------------------
-- | A wrapper for the bitmap info, 
--	in case we want to support other header types in the future.
data BitmapInfo
	= InfoV3 BitmapInfoV3
	| InfoV5 BitmapInfoV5
	deriving (Show)

instance Binary BitmapInfo where
 get
  = do	size	<- lookAhead getWord32le 
	case size of
	 40 -> do
		info 	<- get
		return	$ InfoV3 info
		
	 124 -> do
		info	<- get
		return	$ InfoV5 info
		
	 _   -> error "Codec.BMP.BitmapInfo.get: unhandled header size"
	
 put xx
  = case xx of
	InfoV3 info	-> put info
	InfoV5 info	-> put info
	


	
	
	
	
	
