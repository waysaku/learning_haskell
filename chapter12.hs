-- data CoolBool = CoolBool { getCoolBool :: Bool }
newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"







data Suit = Club | Diamond | Heart | Spade deriving (Read, Show, Enum, Eq, Ord)
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Read, Show, Enum , Eq, Ord)

data Card = Card { value :: CardValue, suit :: Suit} deriving (Read, Show, Eq)

instance Ord Card where
  compare c1 c2 | (value c1 == (value c2)) = compare (suit c1) (suit c2)
                | otherwise = compare (value c1) (value c2)

instance Enum Card where 
  toEnum n = Card (toEnum (n `div` 4)) (toEnum (n `mod` 4))
  fromEnum c = 4 * (fromEnum (value c)) + (fromEnum (suit c))

type Deck = [Card]

deck :: Deck
deck = [Card val su | val <- [Two .. Ace], su <- [Club .. Spade]]


newtype Pair b a = Pair {getPair :: (a, b)}
instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)































