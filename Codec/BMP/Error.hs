{-# OPTIONS_HADDOCK hide #-}
module Codec.BMP.Error
	(Error(..))
where
import Data.Word

-- | Things that can go wrong when loading a BMP file.
data Error
	= ErrorReadOfFileHeaderFailed
	| ErrorReadOfImageHeaderFailed
	| ErrorReadOfImageDataFailed
	| ErrorBadMagic 		Word16
	| ErrorReservedFieldNotZero
	| ErrorDodgyOffsetInFileHeader
	| ErrorUnhandledPlanesCount	Int
	| ErrorUnhandledColorDepth	Int
	| ErrorUnhandledCompressionMode	Int
	| ErrorZeroImageSize
	| ErrorLacksWholeNumberOfLines
	deriving (Eq, Show)

