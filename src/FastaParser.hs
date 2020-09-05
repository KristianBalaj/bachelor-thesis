module FastaParser (parseFasta, FastaSeq(..)) where 

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Lazy as A
import Data.Word8

data FastaSeq = FastaSeq {
    fastaHeader :: BS.ByteString,
    fastaSeq :: BS.ByteString
} deriving (Show)

fastaSeqStart :: (Word8 -> Bool)
fastaSeqStart c = c == _greater

recordStart :: Parser BS.ByteString
recordStart = A.takeWhile1 fastaSeqStart

word :: Parser BS.ByteString
word = A.takeWhile1 $ not . isSpace

header :: Parser BS.ByteString
header = do
    _ <- recordStart
    _ <- A.takeWhile isSpace
    hdr <- word
    _ <- A.takeWhile isSpace
    return hdr

fastaSequence :: Parser [BS.ByteString]
fastaSequence = A.takeWhile (\c -> not (isSpace c) && not (fastaSeqStart c)) `A.sepBy1` satisfy isSpace

singleFastaSeq :: Parser FastaSeq
singleFastaSeq = do
    hdr <- header
    fSeq <- BS.concat <$> fastaSequence
    return (FastaSeq hdr fSeq)

fasta :: Parser [FastaSeq]
fasta = do
    res <- A.many' singleFastaSeq
    A.endOfInput 
    return res

parseFasta :: BL.ByteString -> Maybe [FastaSeq]
parseFasta contents = 
    case A.parse fasta contents of
        Fail {} -> Nothing
        Done _ fastas -> Just fastas