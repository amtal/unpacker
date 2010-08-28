{- |Binary packed data unpacker for:

-bit-packed data
-that is unaligned to byte boundaries
-where future structure depend on past values
-the format definition may not be total, and errors must be detected
-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Unpacker
    ({- Size
    , Range
    , (%)
    , (<<)
    -}) where
import Item
import Data.Bitstream
import Control.Monad

class Range r a where inRange :: r -> a -> Bool
instance Range (Int,Int) Int where inRange (a,b) n = a<=n && n<=b
instance Range [Int] Int where inRange = flip elem
--instance (Bounded a, Enum a) => Range a Int where
--    inRange a n = (fromEnum minBound)<=n && n<=(fromEnum maxBound)

(<<) :: (Range r a, Monad m, Functor m) => m a -> r -> m a
a << r = guard $ fmap (inRange r) a $ error "TODO raise this properly"

header :: WriterT [Attribute] (Bitstream ())
header = do
    packet <- le 8 << [0x9c,0x9d]
    action <- le 8 << (0,0x17)

    --(bs,_,_) <- getStreamData     
    length <- le 8-- << [S.length bs]
    
    category <- le 8 << [0,1,5,6,7,10,16]
    id <- le 32
    when (packet==0x9d && action==fromEnum Amazon) $ do --AutoUnequip
        _ownerAction <- le 8
        _ownerId <- le 32
        return 0
    when (action==fromEnum Amazon) $ do --InSocket
        _unk0 <- le 8
        _socketedInto <- le 32
        return 0
    return 0
{-

flags = do        
    flags [ Equipped, Unk1, Unk2, InSocket,
          , Identified, Unk3, SwitchedIn, SwitchedOut     -- 1 byte
          , Broken, Unk3, Potion, Socketed
          , Unk4, InStore, NotInSocket, Unk5             -- 2 bytes
          , Ear, StarterItem, Unk6, Unk7,
          , Unk8, SimpleItem, Eth, Any -- 3
          , Personalized, Gamble, Runeword, Unk7
      ] where flags = foldl1 (>>) . map (%1)
    le 4       -- 4
    Attr Version%2 << [0x02,0x65]
    le 2
    destination<- Destination%3 << (0,6) -- urg, didn't plan for this
    if (destination==Ground)
        then GndX%16 >> GndY%16
        else do
            EquippedAt%le 4 << (0,12)
            x<-le 4 << (0,9)
            y<-le 3 << (0,10)-- what was the range on this again? not 8...
            write $ Position x y
            Container%4 << (0,10) -- more complex than that, if in shop
    flagged ear do
        earClass <- le 3 << minBound::Class --(0,6)
        level <- le 7 << (0,99)
        name <- cstr 15
        write $ Ear (toEnum name) level earClass
        Done
    code<- Code % cstr << [3]
    guard code=="gld" do
        bigpile <- Bigpile%1
        if bigpile then Gold%32 else Gold%12
        done
    Unk10%8
    SocketsUsed%3
    flagged SimpleItem do
        set Quality None
    flagged Gamble do
        set Quality Rare
    ILvl%7
    quality<- Quality%4
    HasGraphic%1
    flagged HasGraphic Graphic%3
    HasColor%1
    flagged HasColor Color%11
    flagged Identified do
        case quality of
            Inferior-> Prefix%3
            Superior-> Prefix%3
            Magic   -> Prefix%11 >> Suffix%11
            Rare    -> Prefix%8 >> Suffix%8
            Crafted -> Prefix%8 >> Suffix%8
            Set     -> Prefix%12
            Unique  -> Prefix%12
    flagged Runeword do
        Prefix%12
        Suffix%4
      -- How to handle calculation??
    flagged Personalized $ PersonalizedTo%"15"
    -- how to handle armor defense lookup?
    Unk12%1
    flagged Throwable do -- undefined flag?
        Unk%9
        Unk%17
    -- durability stuff todo
    flagged Socketed $ Sockets%4
    flagged Identified $ Done
    flagged Stackable do -- undefined flag
        usable <- peek Usable
        guard usable $ UnkUse%5
        StackSize%9
    guard (quality==Set) SetMods%5
    -- this is where the list of stats kicks in:
    Repeat
        [ StatId%9
        , If (\m-> m!StatId==0x1ff) [Done]
            [
            ]
        ]

--}

