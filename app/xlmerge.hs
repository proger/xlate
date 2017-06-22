{-# LANGUAGE TupleSections #-}

import           Control.Applicative
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Xlate.Excel as Excel
import qualified Xlate.Strings as Strings

data Origin = ExcelInput
            | OldStrings
            | EnglishStrings
            deriving Show

type Xlang = (Excel.Translation -> Maybe Text)

merge :: Maybe Text -> Maybe Text -> Text -> (Origin, Text)
merge xl sfile sref
  = maybe (EnglishStrings, sref) id (fmap (ExcelInput,) xl <|> fmap (OldStrings,) sfile)

ekey = Strings.Key . Excel.tkey

gatherTranslations :: [Either _ Excel.Translation] -> Xlang -> Map Strings.Key Text
gatherTranslations input get
  = List.foldl' f Map.empty [i | Right i <- input]
  where f map xl = maybe map (flip (Map.insert (ekey xl)) map) (get xl)

translate i g o r = translate' (gatherTranslations i g) o r

translate' :: Map Strings.Key Text
           -> Map Strings.Key Text
           -> Map Strings.Key Text
           -> Map Strings.Key (Origin, Text)
translate' input old reference
  = Map.mapWithKey f reference
  where f k = merge (Map.lookup k input) (Map.lookup k old)


origin :: Map Strings.Key (Origin, Text) -> Strings.Key -> Maybe Text
origin map = maybe (Just "English") (Just . T.pack . show) . fmap fst . flip Map.lookup map

main = do
  xl <- Excel.readFileSheet "/Users/vladki/hongg/resources/hon.xlsx" "interface"

  (intRu, intRuMap) <- Strings.readFile
                       "/Users/vladki/hongg/resources/hon_4.1.2_strings/interface_ru.str"
  (intUk, intUkMap) <- Strings.readFile
                       "/Users/vladki/hongg/resources/hon_4.1.2_strings/interface_ua.str"
  (intEn, intEnMap) <- Strings.readFile
                       "/Users/vladki/hongg/resources/hon_4.1.2_strings/interface_en.str"

  let newRu = translate xl Excel.ru intRuMap intEnMap
  let newUk = translate xl Excel.uk intUkMap intEnMap

  Strings.rewriteFile "/Users/vladki/hongg/resources/new_interface_ru.str" intEn (fmap snd . flip Map.lookup newRu)

  Strings.rewriteFile "/Users/vladki/hongg/resources/new_interface_ua.str" intEn (fmap snd . flip Map.lookup newUk)

  Strings.rewriteFile "/Users/vladki/hongg/resources/origin_interface_ru.str" intEn (origin newRu)

  Strings.rewriteFile "/Users/vladki/hongg/resources/origin_interface_ua.str" intEn (origin newUk)

  return ()
