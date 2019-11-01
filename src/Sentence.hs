module Sentence where

-- Each thematic role has its own vector representation.
class ThRole a where
  thSymbol ::  a -> [Int]

-- TODO: Generify ThRoles.
-- TODO: Introduce dependent types between head and arguments.
data Sentence = Vt SemanticVal ThAgent ThAcc deriving (Show)

newtype ThAgent = ThAgent Np deriving (Show)
instance ThRole ThAgent where
  thSymbol _ = [1, 0, 0]

newtype ThAcc = ThAcc Np deriving (Show)
instance ThRole ThAcc where
  thSymbol _ = [0, 1, 0]

-- Each Subtype has its own vector representation.
class TypeEncodable a where
  typeSymbol :: a -> [Int]

data Np = Noun SemanticVal
  | Comp [Int] Sentence
  deriving (Show)

-- Note that in case of Comp, we assign an identifier vector at construction time.
-- This may change in future.

instance TypeEncodable Np where
  typeSymbol (Noun _  ) = [1, 0]
  typeSymbol (Comp _ _) = [0, 1]

newtype SemanticVal = SemanticVal [Int] deriving (Show)

-- Example sentence from Idea 1.

sI = SemanticVal [1, 0, 0, 0]
sKnow = SemanticVal [0, 1, 0, 0]
sEat = SemanticVal [0, 0, 1, 0]
sApple = SemanticVal [0, 0, 0, 1]

cInnerThat = Comp [1, 0] $ Vt sEat (ThAgent $ Noun sI) (ThAcc $ Noun sApple)
sExample = Vt sKnow (ThAgent $ Noun sI) (ThAcc cInnerThat)

encodeSingleSentence :: Sentence -> [Int]
encodeSingleSentence (Vt (SemanticVal s) tAgent@(ThAgent taNp) tAcc@(ThAcc tAccNp))
  = s ++ thSymbol tAgent ++ encodeNp taNp ++ thSymbol tAcc ++ encodeNp tAccNp

encodeNp :: Np -> [Int]
encodeNp n@(Noun (SemanticVal sVal)) = typeSymbol n ++ sVal
encodeNp c@(Comp id sentence       ) = typeSymbol c ++ id

encodeAll :: [([Int], Sentence)] -> [Int]
encodeAll []             = []
encodeAll ((id, s) : xs) = id ++ encodeSingleSentence s ++ encodeAll xs

-- |`allSentences` returns a list of (id, sentence) pair containing itself and all inner sentences
allSentences :: Sentence -> [Int] -> [([Int], Sentence)]
allSentences sOuter@(Vt _ tAgent tAcc) sId = [(sId, sOuter)] ++ sAgent ++ sAcc
 where
  sAgent = case tAgent of
    ThAgent (Noun _        ) -> []
    ThAgent (Comp id sInner) -> allSentences sInner id
  sAcc = case tAcc of
    ThAcc (Noun _        ) -> []
    ThAcc (Comp id sInner) -> allSentences sInner id

encode :: Sentence -> [Int]
encode s = encodeAll ss where ss = allSentences s [0, 0]

exampleEncoded = encode sExample
{-
[0,0, -- Id for the outermost sentence
  0,1,0,0, -- know
  1,0,0,   -- Agent
  1,0,     -- Noun
  1,0,0,0, -- I
  0,1,0,   -- Acc
  0,1,     -- Comp
  1,0,     -- <1,0>
  
  1,0,     -- <1,0> - Id for the inner phrase
  0,0,1,0, -- eat
  1,0,0,   -- Agent
  1,0,     -- Noun
  1,0,0,0, -- I
  0,1,0,   -- Acc
  1,0,     -- Noun
  0,0,0,1] -- Apple
-}
