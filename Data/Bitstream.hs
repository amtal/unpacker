{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |Wrapper that treats a stream of bytes as a stream of bits.
module Data.Bitstream( Bitstream
                     , runBitstream
                     -- *Data unpackers
                     , bite, chomp
                     , le
                     , streamErr
                     ) where            
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word
import Data.Bits

import MonadLib
import MonadLib.Monads

import Data.Maybe

-- State for maintaining position within a string being parsed.
data Place = Place { atByte :: Int
                   , atBit  :: Int
                   } deriving (Eq,Show)

-- Immutable data used by the parser.
data Conf = Conf { bitData :: B.ByteString
                 } deriving (Eq,Show)

newtype Bitstream a = MkStream {
    unStream :: ReaderT Conf (StateT Place (Exception String)) a
} deriving (Monad, Functor) -- what is this sorcery

runBitstream :: ByteString -> Bitstream a -> Either String a
runBitstream bs = fmap fst
                . runException
                . runStateT (Place 0 0)
                . runReaderT (Conf bs)
                . unStream


-- Internal, unwrapped helper function.
getStreamData = do
    bs <- asks bitData
    (Place byte bit) <- get
    return (bs,byte,bit)

-- TODO:
-- fancy-ass debug data of
--      bit+byte location
--      parse history to date (positions+sizes+results) (optional?)
-- a way to externally raise an exception
--      parsing might look like it succeeded, but data turns out bogus
--      in this case, want the above debug data + external exception data
-- generalize bite and chomp to an "le" (and "be"?) that return Int(eger?)

-- hmm... this needs its own range errors, and possibly its own logging
-- TODO: lift errors to le and cstr, as well as MkStream
-- have bite and chomp be internal, abstract things
le :: Int -> Bitstream Int
le n | n<=8 = fmap fromIntegral $ bite n
     | otherwise = fmap fromIntegral $ chomp n

-- | Reads a null terminated string from the stream.
-- The null is consumed in the stream, but does not appear in the result.
cstr :: Bitstream ByteString
cstr = MkStream $ liftM (B.pack . takeWhile (/=0)) (unStream bs) where
    bs :: Bitstream [Word8]
    bs = sequence . repeat $ bite 0

-- | Hmm...
streamErr :: String -> Bitstream a
streamErr msg = MkStream $ do
    (Place byte bit) <- get
    raise $ show byte++"."++show bit++": "++msg

    
-- |Reads up to a byte of data from the bitstream, moving position forward.
--
-- Bits are read from most significant, to least significant.
-- Raises exceptions when out of range, or when trying to read more than 8 bits. 
bite :: Int -> Bitstream Word8
bite 0 = MkStream $ return 0
bite n = MkStream $ do
    when (n>8) $ raise "bite can only chew up to 8 bits at a time!"
    (bs,byte,bit) <- getStreamData
    let outOfString = raise "out of bounds" -- TODO detailed stat print function
        parsedOkay result = do
            set $ Place (byte + ((bit+n) `div` 8)) ((bit+n) `mod` 8)
            return result
    maybe outOfString parsedOkay (parse byte bit bs)
        where
    -- Word* is unsigned, so sign-extension doesn't happen when shifting :)
    parse :: Int -> Int -> ByteString -> Maybe Word8
    parse byte bit bs
        | byte*8+bit+n > 8*B.length bs = Nothing -- out of bounds
        | bit==0 = Just $ (bs `B.index` byte) `shiftR` (8-n) 
        | otherwise = Just $ (msbs .|. lsbs) `shiftR` (8-n) where
            -- virtual byte spanning two physical bytes, at the current position            
            msbs = (bs `B.index` byte) `shiftL` bit -- right side is 0-filled
            lsbs = (bs `B.index` (byte+1)) `shiftR` (8-bit)
-- I can see this being done more efficiently in C via loading 16 bits, then
-- shifting. No clue how that would work in Haskell, though.

-- |Reads up to 32 bits with /little endian/ byte ordering.
--
-- Bytes are read in increasing order.
-- Bits are read from most significant, to least significant.
-- Raises exceptions when out of range, or trying to read more than 32 bits.
chomp :: Int -> Bitstream Word32
chomp 0 = MkStream $ return 0
chomp n = MkStream $ do
    when (n>32) $ raise "chomp can only chew up to 32 bits at a time!"
    -- check if the parse will succeed
    -- this way, error message can be tailored to chomp rather than bite
    (bs,byte,bit) <- getStreamData
    when (byte*8+bit+n > 8*B.length bs) $ raise "out of string error"
    -- parse byte by byte
    head <- unStream $ bite (n `mod` 8)
    bytes <- replicateM (n `div` 8) (unStream $ bite 8)
    -- combine bytes into word32
    let comps = map fromIntegral (head:bytes)
        comps :: [Word32]
    return . fromIntegral . fst . foldl mash (0,0) $ comps
        where
    mash (  _,0) val = (val,8)
    mash (acc,n) val = (acc .|. (val `shiftL` n), n+8)






