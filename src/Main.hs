module Main where
  import Cz.MartinDobias.ChristmasKoma.PadCracker
  import System.Console.ArgParser
  import Data.Char(digitToInt, chr)

  data Input = Input String String
    deriving (Show)

  inputParser :: ParserSpec Input
  inputParser = Input
    `parsedBy` reqPos "messages" `Descr` "Path to file with HEX encoded messages"
    `andBy` optPos "" "passphrase" `Descr` "Passphrase with wildcards (*)"

  inputInterface :: IO (CmdLnInterface Input)
  inputInterface =
      (`setAppDescr` "Vit Koma Christmas Cracker")
      . (`setAppEpilog` "Merry Christmas")
      <$> mkApp inputParser

  parsePhrase :: String -> Passphrase
  parsePhrase [] = [[]]
  parsePhrase cs = map (\c -> [c | c /= '_']) cs

  parseMessage :: String -> Message
  parseMessage [] = []
  parseMessage [c] = if c == '\r' || c == '\n' then [] else error "Invalid input"
  parseMessage (h : l : cs) = chr (digitToInt h * 16 + digitToInt l) : parseMessage cs

  app :: Input -> IO()
  app (Input mf pf) = do
    putStrLn ("Processing " ++ mf ++ ", with initial passphrase: " ++ pf)
    m <- readFile mf
    let ms = map parseMessage $ lines m
        p = parsePhrase pf
        r = crack ms p
    putStrLn "Original messages"
    mapM_ print ms
    putStrLn ""
    putStrLn "Decoded messages"
    mapM_ (print . decode r) ms
    putStrLn ""
    putStrLn "Resulting passphrase candidate"
    mapM_ print r

  main :: IO()
  main = do
    input <- inputInterface
    runApp input app
