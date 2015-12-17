module Cz.MartinDobias.ChristmasKoma.PadCracker (
  Passphrase(..),
  Cracker(..),
  crack,
  decode,
  zipAll,
  candecode,
  valid,
  findCandidates
) where

  import Data.Char(ord, chr)
  import Data.Bits(xor)

  data Passphrase = Passphrase [String] deriving (Show)
  data Cracker = Cracker [String] Passphrase deriving (Show)

  zipAll [] = []
  zipAll xs = map head (filterEmpty xs) : zipAll (filterEmpty (map tail (filterEmpty xs)))
    where filterEmpty = filter (not . null)

  legalOutput = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', ',', '.', '!', '?', '\'']
  possibleCandidates = [(chr 0) .. (chr 255)]

  candecode c = all (\x -> chr(ord c `xor` ord x) `elem` legalOutput)

  valid xs ps = [p | p <- ps, candecode p xs]

  findCandidates [] _ = []
  findCandidates (x : xs) (p : ps) = (if null p then valid x possibleCandidates else valid x p) : findCandidates xs ps

  crack :: Cracker -> Passphrase
  crack (Cracker ms (Passphrase ps)) = Passphrase $ findCandidates (zipAll ms) (infinitePassphrase ps)

  infinitePassphrase p = p ++ infinitePassphrase p

  decodeOrHide [] = []
  decodeOrHide ((_, []) : m) = '_' : decodeOrHide m
  decodeOrHide ((c, [p]) : m) = chr (ord c `xor` ord p) : decodeOrHide m
  decodeOrHide ((_, p : ps) : m) = '_' : decodeOrHide m

  decode :: Passphrase -> String -> String
  decode (Passphrase cs) m = decodeOrHide zipped
    where zipped = zip m (infinitePassphrase cs)
