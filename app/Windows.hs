{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Windows where

import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.List (intersperse, findIndex)
import Data.Char (isSpace, ord, intToDigit)
import Streamly.Data.Parser (Parser)
import Streamly.Unicode.String (str)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Internal.System.Command as Cmd

type WinId = String

data Line =
    Line
        { wid :: WinId
        , workspace :: Int
        , appName :: String
        , title :: String
        }
    deriving (Show)

decToHex :: Int -> String
decToHex 0 = "0x00000000"
decToHex n = "0x" ++ replicate padding '0' ++ val

     where

      hexChars 0 = ""
      hexChars x = intToDigit (x `mod` 16) : hexChars (x `div` 16)

      val = reverse (hexChars n)
      padding = 8 - length val

getCurrentDesktop :: IO Int
getCurrentDesktop =
    Cmd.toLines Fold.toList [str|wmctrl -d|]
        & Stream.mapM (Stream.parse parser . Stream.fromList)
        & fmap (either (error . show) id)
        & Stream.fold (Fold.find (\(i, a) -> a == "*"))
        & fmap (fst . fromJust)

    where

    parser =
        (,)
            <$> Parser.wordBy isSpace (fmap read Fold.toList)
            <*> Parser.wordBy isSpace Fold.toList

parseLine :: Monad m => Parser.Parser Char m Line
parseLine =
    Line
        <$> Parser.wordBy isSpace Fold.toList
        <*> Parser.wordBy isSpace (fmap read Fold.toList)
        <*> Parser.wordBy isSpace (fmap (tail . dropWhile (/= '.')) Fold.toList)
        <*> (Parser.wordBy isSpace Fold.drain *> Parser.fromFold Fold.toList)

appColumLen :: [Line] -> Int
appColumLen = maximum . map ((+4) . length . appName)

titleColumLen :: [Line] -> Int
titleColumLen = maximum . map ((+4) . length . title)

translateChar :: Char -> Char
translateChar '\8212' = '-'
translateChar '\183' = '-'
translateChar x = x

formatLine :: Int -> Int -> Line -> String
formatLine appColLen titleColLen Line {..} =
    let paddingApp = appColLen - length appName
        paddingTitle = titleColLen - length title
     in map translateChar
            $ appName ++ replicate paddingApp ' '
                      ++ title ++ replicate paddingTitle ' '

currWindow :: [Line] -> IO Int
currWindow ls = do
    currWid <-
        Cmd.toLines Fold.toList [str|xdotool getwindowfocus|]
            & Stream.fold (decToHex . read . fromJust <$> Fold.head)
    pure $ fromJust $ findIndex ((== currWid) . wid) ls

winList :: IO [Line]
winList = do
    elines <-
        Cmd.toLines Fold.toList [str|wmctrl -lxu|]
            & Stream.mapM (Stream.parse parseLine . Stream.fromList)
            & Stream.toList
    let lines = map (either (error . show) id) elines
    currDesk <- getCurrentDesktop
    pure $ filter ((== currDesk) . workspace) lines

activateWindow :: Line -> IO ()
activateWindow Line {..} = Cmd.toNull [str|wmctrl -ia #{wid}|]
