{- |Binary packed data unpacker for:

-bit-packed data

-that is unaligned to byte boundaries

-where future structure depend on past values

-the format definition may not be total, and errors must be detected
-}
module Unpacker
    ( Size
    , Range
    , (%)
    , (<<)
    ) where
import Data.Bitstream
import qualified Data.Map as M

class Size s where
    unpack :: s -> Bitstream -> Maybe (Dynamic, Bitstream)
instance Size Int where
    unpack s bst = do
        (val,bst') <- s `chomp` bst
        return (toDyn val, bst')
{--
-- rework to use enum TODO
class Ord a => Range n a where
    within :: n -> a -> Bool
instance Range a (List a) where within = elem
instance Range a (a,a) where within n (lo,hi) = lo<=n `and` n<=hi
--}

-- |Unpack a value of a certain size.
(%) :: Size s => k -- ^key to store the resulting value in
              -> s -- ^size of value to unpack
              -> Unpacker k
(%) k s = do
    (bst,vMap) <- get
    let (val,bst') = case unpack s bst of
                    Nothing -> error "something useful plz"
                    Just (a,b) -> (a,b)
    set (bst',M.insert vMap k val)
    return val

-- |Check that a value is within acceptable range.
(<<) :: Range a => Unpacker k -- ^value to check
                -> a          -- ^range it should be in
                -> Unpacker k -- ^continue if value okay

data Unpacker k a = Unpack { unStream :: Bitstream
                           , unMap :: M.Map k Dynamic
                           , unVal :: a
                           }
instance Monad (Unpacker k) where
    (>>=) a f = f (unVal a)
    return =
-- Run the unpacker until out of data, or a range check error occurs.
runUnpacker :: Unpacker a -> Bitstream -> (Map Key Dynamic, Bitstream)

{--
data Key = Packet|Action|Length|Category|ID|OwnerAction|OwnerID|InSocket|Unk0
         |SocketedInto|Equipped|Unk1|Unk2|InSocket|Identified|Unk3 -- ...

structure :: Unpacker Key
structur = do
    packet<-Packet%8 << [0x9c,0x9d]
    action<-Action%8 << (0,0x17)
    Length%8
    Category%8 << [0,1,5,6,7,10,16]
    ID%32
    guard (packet==0x9d && action==AutoUnequip) do
        OwnerAction%8
        OwnerID%32
    guard (action==InSocket) do
        Unk0%8
        SocketedInto%32
    flags [ Equipped, Unk1, Unk2, InSocket,
          , Identified, Unk3, SwitchedIn, SwitchedOut     -- 1 byte
          , Broken, Unk3, Potion, Socketed
          , Unk4, InStore, NotInSocket, Unk5             -- 2 bytes
          , Ear, StarterItem, Unk6, Unk7,
          , Unk8, SimpleItem, Eth, Any -- 3
          , Personalized, Gamble, Runeword, Unk7
      ] where flags = foldl1 (>>) . map (%1)
    Unk8%4       -- 4
    Version%2 << [0x02,0x65]
    Unk9%2
    destination<-Destination%3 << (0,6)
    if (destination==Ground)
        then GndX%16 >> GndY%16
        else do
            EquippedAt%4 << (0,12)
            GridX%4 << (0,9)
            GridY%3 -- what was the range on this again? not 8...
            Container%4 << (0,10) -- more complex than that, if in shop
    flagged ear do
        EarClass%3 << (0,6)
        EarLevel%7 << (0,99)
        EarName%"15"
        Done
    code<-Code%"3"
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
    quality<-Quality%4
    HasGraphic%1
    flagged HasGraphic Graphic%3
    HasColor%1
    flagged HasColor Color%11
    flagged Identified do
        case quality of
            Inferior->Prefix%3
            Superior->Prefix%3
            Magic   ->Prefix%11 >> Suffix%11
            Rare    ->Prefix%8 >> Suffix%8
            Crafted ->Prefix%8 >> Suffix%8
            Set     ->Prefix%12
            Unique  ->Prefix%12
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
        , If (\m->m!StatId==0x1ff) [Done]
            [
            ]
        ]

--}

