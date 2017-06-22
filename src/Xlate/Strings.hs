module Xlate.Strings where

import           Control.Applicative
import           Control.Exception.Base (try, evaluate, SomeException, ioError)
import qualified Data.Attoparsec.Text as Atto
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           System.IO
import           System.IO.Error (userError)
import           Test.Hspec

newtype Whitespace
  = Whitespace Text deriving (Eq)

instance Show Whitespace where
  show (Whitespace _) = "Whitespace"

newtype Key
  = Key Text deriving (Show, Eq, Ord)

data HString
  = HString Key Whitespace (Maybe Text)
  deriving (Show, Eq)

hValue (HString k w v) = v

data Hon
  = End
  | Skip
  | Comment Text
  | HonString HString
  deriving (Show, Eq)

end = do
  Atto.string "// End - do not delete this line"
  Atto.endOfLine
  return End

comment = do
  Atto.string "//"
  Atto.skipSpace
  comment <- Atto.takeTill Atto.isEndOfLine
  Atto.endOfLine
  return (Comment comment)


string = do
  key <- Key <$> Atto.takeTill Atto.isHorizontalSpace
  wsp <- Whitespace <$> Atto.takeWhile Atto.isHorizontalSpace
  value <- Atto.takeTill Atto.isEndOfLine
  Atto.endOfLine
  return (HonString (HString key wsp (if Text.null value then Nothing else Just value)))

skip = do
  Atto.skipSpace
  Atto.endOfLine
  return Skip

emptyLine = do
  Atto.endOfLine
  return Skip

honStrings = end <|> comment <|> skip <|> emptyLine <|> string

go = do
  text <- Text.readFile "/Users/vladki/hongg/resources/hon_4.1.2_strings/entities_ru.str"
  right (return $ Atto.parseOnly (Atto.many1 honStrings) text)


stringMap :: [Hon] -> (Vector Hon, Map Key Text)
stringMap xs = (V.fromList xs,
                Map.fromList [(k, v) | HonString (HString k _ (Just v)) <- xs])

readFile :: FilePath -> IO (Vector Hon, Map Key Text)
readFile f = do
  text <- Text.readFile f
  hons <- right (return $ Atto.parseOnly (Atto.many1 honStrings) text)
  return (stringMap hons)

rewriteFile :: FilePath -> Vector Hon -> (Key -> Maybe Text) -> IO ()
rewriteFile f hons lookup = withFile f WriteMode (flip V.imapM_ hons . go . Text.hPutStrLn) where
  go put _ hon =
    case hon of
      End -> do
        put "// End - do not delete this line\r"
        put "\r"
        put "\r"
      Skip ->
        put ""
      Comment x ->
        put ("// " <> x <> "\r")
      HonString (HString k@(Key tk) (Whitespace w) v) ->
        put (tk <> w <> maybe Text.empty id (lookup k <|> v) <> "\r")


spec = hspec $
  describe "standalone parsers" $ do
    it "parses comments" $
      Atto.parseOnly comment "// Game Mechanics\n"
      `shouldBe`
      Right (Comment "Game Mechanics")
    it "parses strings" $
      Atto.parseOnly string "Popup_gainint    +{value} Int!\n"
      `shouldBe`
      Right (HonString (HString (Key "Popup_gainint") (Whitespace "    ") (Just "+{value} Int!")))



right = match (\(Right x) -> x)

match :: Show a => (a -> b) -> IO a -> IO b
match f xIO = do
    x <- xIO
    ret <- try (evaluate (f x))
    case ret of
        Right v -> return v
        Left (ex :: SomeException) ->
          ioError (userError ("match failed, got: " ++ show x))
