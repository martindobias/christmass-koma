module Cz.MartinDobias.ChristmasKoma.PadCracker (
  Message,
  PossibleKeystrokes,
  Passphrase(..),
  crack,
  decode,
  zipAll
) where

  import Data.Char(ord, chr)
  import Data.Bits(xor)

  type Message = String
  type PossibleKeystrokes = [Char]
  type AvailableChars = [Char]
  type Passphrase = [PossibleKeystrokes]

  zipAll :: [Message] -> [AvailableChars]
  zipAll [] = []
  zipAll xs = map head (filterEmpty xs) : zipAll (filterEmpty (map tail (filterEmpty xs)))
    where filterEmpty = filter (not . null)

  legalOutput = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ,.:!-\'"
  legalOutputInitial = ['A'..'Z'] ++ ['0'..'9']
  possibleCandidates = [(chr 0) .. (chr 255)]

  candecode :: [Char] -> Char -> AvailableChars -> Bool
  candecode l c = all (\x -> chr(ord c `xor` ord x) `elem` l)

  valid :: [Char] -> AvailableChars -> PossibleKeystrokes -> PossibleKeystrokes
  valid l xs ps = [p | p <- ps, candecode l p xs]

  findCandidates :: [AvailableChars] -> [PossibleKeystrokes] -> [PossibleKeystrokes]
  findCandidates [] _ = []
  findCandidates (x : xs) (p : ps) = (if null p then valid legalOutput x possibleCandidates else valid legalOutput x p) : findCandidates xs ps
  findCandidatesInitial :: [AvailableChars] -> [PossibleKeystrokes] -> [PossibleKeystrokes]
  findCandidatesInitial [] _ = []
  findCandidatesInitial (x : xs) (p : ps) = (if null p then valid legalOutputInitial x possibleCandidates else valid legalOutputInitial x p) : findCandidates xs ps

  crack :: [Message] -> Passphrase -> Passphrase
  crack ms ps = findCandidatesInitial (zipAll ms) (infinitePassphrase ps)

  infinitePassphrase p = p ++ infinitePassphrase p

  decodeOrHide [] = []
  decodeOrHide ((_, []) : m) = '_' : decodeOrHide m
  decodeOrHide ((c, [p]) : m) = chr (ord c `xor` ord p) : decodeOrHide m
  decodeOrHide ((_, p : ps) : m) = '_' : decodeOrHide m

  decode :: Passphrase -> Message -> String
  decode cs m = decodeOrHide zipped
    where zipped = zip m (infinitePassphrase cs)
