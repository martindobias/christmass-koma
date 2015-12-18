module Main where
  import Cz.MartinDobias.ChristmasKoma.PadCracker
  import System.Console.ArgParser
  import Data.Char(digitToInt, chr, ord)
  import Numeric(showHex)
  import Text.Printf(printf)

  data Input = Input String String Int Int
    deriving (Show)

  inputParser :: ParserSpec Input
  inputParser = Input
    `parsedBy` reqPos "messages" `Descr` "Path to file with HEX encoded messages"
    `andBy` optPos "" "passphrase" `Descr` "Passphrase HEX encoded with wildcards (_)"
    `andBy` optPos 5 "keyLength" `Descr` "Resulting passphrase length to print"
    `andBy` optPos 0 "keyStart" `Descr` "Resulting passphrase start position to print"

  inputInterface :: IO (CmdLnInterface Input)
  inputInterface =
      (`setAppDescr` "Vit Koma Christmas Cracker")
      . (`setAppEpilog` "Merry Christmas")
      <$> mkApp inputParser

  parsePhrase :: String -> Passphrase
  parsePhrase [] = [[]]
  parsePhrase [c] = case c of
    '_' -> [[]]
    _ -> error "Invalid input"
  parsePhrase (h : l : cs) = case h of
    '_' -> [] : parsePhrase (l : cs)
    _ -> [chr (digitToInt h * 16 + digitToInt l)] : parsePhrase cs

  parseMessage :: String -> Message
  parseMessage [] = []
  parseMessage [c] = if c == '\r' || c == '\n' then [] else error "Invalid input"
  parseMessage (h : l : cs) = chr (digitToInt h * 16 + digitToInt l) : parseMessage cs

  app :: Input -> IO()
  app (Input mf pf l off) = do
    putStrLn ("Processing " ++ mf ++ ", with initial passphrase: " ++ pf)
    m <- readFile mf
    let ms = map parseMessage $ lines m
        p = parsePhrase pf
        r = crack ms p
    --putStrLn "Original messages"
    --mapM_ print ms
    --putStrLn ""
    putStrLn "Decoded messages"
    mapM_ (print . decode r) ms
    putStrLn ""
    printf "Resulting passphrase candidate %v - %v\n" off (off + l)
    let rr = take l (drop off (map (map (showHex . ord)) r))
    mapM_ (print . foldr (\f x -> ' ' : f x) "") rr

  main :: IO()
  main = do
    input <- inputInterface
    runApp input app
