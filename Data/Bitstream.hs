-- |Wrapper that treats a stream of bytes as a stream of bits.
-- Simple, and reasonably efficient.
module Data.Bitstream( Bitstream(..)
                , stream
                -- *Data unpackers
                , bite, chomp
                ) where
import qualified Data.ByteString as B
import Data.Word
import Data.Bits

-- |Data type for sequentially reading bits from a ByteString.
data Bitstream = Bitstream { bsByte :: Int
                           , bsBit  :: Int
                           , bsData :: B.ByteString
                           } deriving (Eq,Show)

-- |Wraps a 'ByteString' in a bit reader, position set to the beginning.
stream :: B.ByteString -> Bitstream
stream bs = Bitstream 0 0 bs

-- |Reads up to a byte of data from the bitstream, moving position forward.
--
-- Returns 'Nothing' if out of range. Number of bits to be read /must/ be 8 or less.
bite :: Int -> Bitstream -> Maybe (Word8, Bitstream)
bite n (Bitstream byte bit bs)
    | n>8 = error "Bitstream.bite can only chew up to 8 bits at a time!"
    | byte*8+bit+n > 8*B.length bs = Nothing -- out of bounds
    | bit==0 = let
        value = n `bitsOf` (bs `B.index` byte)
        bitsOf b = (.&.) ((-1) `shiftR` (8-b))
               in
        Just (value, Bitstream (byte+((bit+n)`div`8)) ((bit+n)`mod`8) bs)
    | otherwise = Just (value, Bitstream byte' bit' bs) where
            value = lsbs .|. msbs
            lsbs = ((bs `B.index` byte) `shiftR` bit) .&. ((-1) `shiftR` bit)
            msbs = if B.length bs /= byte+1
                then ((bs `B.index` (byte+1)) `shiftL` (8-bit)) .&. ((-1) `shiftL` (8-bit))
                    else 0
            byte' = byte + ((bit+n) `div` 8)
            bit'  = (bit+n) `mod` 8
-- I can see this being done more efficiently in C via loading 16 bits, then
-- shifting. No clue how that would work in Haskell, though.

-- |Reads numbers with /little endian/ byte ordering.
--
-- Returns 'Nothing' if out of range. Number of bits to be read /must/ be 32 or less.
chomp :: Int -> Bitstream -> Maybe (Word32, Bitstream)
chomp n stream@(Bitstream byte bit bs)
    | n>32 = error "Bitstream.chomp can only chew up to 32 bits at a time!"
    | byte*8+bit+n > 8*B.length bs = Nothing -- out of bounds
    | n`mod`8/=0 = let
        Just (seed,stream') = bite (n`mod`8) stream
        seed :: Word8
               in
        loop (n-n`mod`8) stream' (fromIntegral seed)
    | otherwise = loop n stream 0 where
        loop :: Int -> Bitstream -> Word32 -> Maybe (Word32, Bitstream)
        loop 0 btstr acc = Just (acc, btstr)
        loop n btstr acc = loop (n-8) btstr' (acc`shiftL`8 .|. byte') where
            Just (byte,btstr') = bite 8 btstr
            byte' = fromIntegral byte
