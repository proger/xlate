{-# LANGUAGE RecordWildCards #-}

module Xlate.Excel where

import qualified Data.Text as Text
import           Data.Text (Text)
import           Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import           Control.Lens

excelRows :: FilePath -> Text -> IO [(Int, [(Int, Maybe CellValue)])]
excelRows filename sheet = do
  bs <- L.readFile filename
  let Just worksheet = toXlsx bs ^? ixSheet sheet
  let Just rows = toRows <$> (worksheet ^? wsCells)
  return (rowToValues <$> rows)

rowToValues :: (Int, [(Int, Cell)]) -> (Int, [(Int, Maybe CellValue)])
rowToValues (row, xs) = (row, [(col, cell ^. cellValue) | (col, cell) <- xs])

readFileSheet :: FilePath -> Text -> IO [Either [(Int, Maybe CellValue)] Translation]
readFileSheet fn sheet = ((rowToTranslation <$>) . drop 1) <$> excelRows fn sheet

data Translation
  = Translation { tkey :: Text, ru :: Maybe Text, uk :: Maybe Text }
    deriving Show

catRuns :: [RichTextRun] -> Text
catRuns = Text.concat . map (view richTextRunText)

mText x = if x == "#Н/Д" || Text.null x then Nothing else Just x

missingOrEmpty :: Maybe CellValue -> Maybe Text
missingOrEmpty (Just (CellText x))   = mText x
missingOrEmpty (Just (CellRich x))   = mText (catRuns x)
-- XXX: treating all doubles as ints
missingOrEmpty (Just (CellDouble x)) = Just (Text.pack (show (fromIntegral (truncate x) :: Int)))
missingOrEmpty (Just (CellBool x))   = Just (Text.toLower (Text.pack (show x)))
missingOrEmpty Nothing               = Nothing

normKey = head . Text.words

rowToTranslation :: (Int, [(Int, Maybe CellValue)]) -> Either [(Int, Maybe CellValue)] Translation
rowToTranslation (_, (1, Just (CellText key)):
                     (2, _):
                     (3, tru):
                     (4, tuk):
                     _) = Right (Translation{..})
  where ru = missingOrEmpty tru
        uk = missingOrEmpty tuk
        tkey = normKey key
rowToTranslation (_, x) = Left x


main :: IO _
main = do
  xl <- readFileSheet "/Users/vladki/hongg/resources/hon.xlsx" "interface"
  return $ [t | Right t <- xl]
