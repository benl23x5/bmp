
module Codec.BMP.Error
	(Error(..))
where
import Data.Word

-- | Things that can go wrong when loading a BMP file.
data Error
	= ErrorBadMagic 		Word16
	| ErrorReadOfFileHeaderFailed
	| ErrorReadOfImageHeaderFailed
	| ErrorReadOfImageDataFailed
	| ErrorUnhandledPlanesCount	Int
	| ErrorUnhandledColorDepth	Int
	| ErrorUnhandledCompressionMode	Int
	| ErrorLacksWholeNumberOfLines
	deriving (Eq, Show)

