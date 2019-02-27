module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (intercalate, maximum)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String as S
import Data.String.CodeUnits as CU
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff_, makeAff, message, nonCanceler)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (catchException)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Obj
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Process (exit)
import Simple.JSON (class ReadForeign, E, read, readImpl, readJSON)

explodeIfLeft :: forall a. ReadForeign a => E a -> Aff a
explodeIfLeft x = liftEffect $ case x of
  Left e -> do 
    logShow e
    exit (-1)
  Right a -> 
    pure a

type NpmResult = { report :: Object NpmEntry }
type NpmEntry = { license :: ArrayOrString }

newtype ArrayOrString = StringArray (Array String)
derive instance ntAoS :: Newtype ArrayOrString _
instance rfAoS :: ReadForeign ArrayOrString where
  readImpl s = StringArray <$> (readImpl s <|> (pure <$> readImpl s))
derive instance genericArrayOrString :: Generic ArrayOrString _

type BowerEntry = { licenses :: ArrayOrString }

npmToUnified :: String -> NpmEntry -> UnifiedResult
npmToUnified name e = 
  { depType: "npm", depName: name, depLicenses: intercalate ", " (unwrap e.license) }

bowerToUnified :: String -> BowerEntry -> UnifiedResult
bowerToUnified name e =  
  { depType: "bower", depName: unundefined name, depLicenses: intercalate ", " (unwrap e.licenses) }
  where
    unundefined = S.replace (S.Pattern "@undefined") (S.Replacement "")

renderUnifiedResult :: UnifiedResult -> String
renderUnifiedResult { depType, depName, depLicenses } = 
  "|" <> depType <> "|" <> depName <> "|" <> depLicenses <> "|"

foreign import npmCrawler :: Effect Unit
foreign import bowerCrawlerImpl 
  :: Fn3 (Error   -> Either Error Foreign) 
         (Foreign -> Either Error Foreign) 
         (Either Error Foreign -> Effect Unit) 
         (Effect Unit)

bowerCrawler :: (Either Error Foreign -> Effect Unit) -> Effect Unit
bowerCrawler = runFn3 bowerCrawlerImpl Left Right

fileName :: String
fileName = "./tmpLicenses.json"

type UnifiedResult = 
  { depLicenses :: String
  , depType :: String
  , depName :: String
  }

type MaxLengths =
  { maxDepType :: Int
  , maxDepName :: Int
  , maxDepLicense :: Int
  }

depTypeHeader :: String
depTypeHeader = "Dependency type"

depNameHeader :: String
depNameHeader = "Dependency name"

depLicensesHeader :: String
depLicensesHeader = "Licenses"

printHeader :: forall m. MonadEffect m => MaxLengths -> m Unit 
printHeader { maxDepType , maxDepName , maxDepLicense } = do
  log $ "# Licenses used by dependencies\n\n"
     <> "|" <> spacePad maxDepType    depTypeHeader  
     <> "|" <> spacePad maxDepName    depNameHeader
     <> "|" <> spacePad maxDepLicense depLicensesHeader 
     <> "|" <> "\n"
     <> "|" <> rightPad '-' maxDepType "" 
     <> "|" <> rightPad '-' maxDepName "" 
     <> "|" <> rightPad '-' maxDepLicense "" 
     <> "|"

calcMaxLengths :: Array UnifiedResult -> MaxLengths
calcMaxLengths results = { maxDepType, maxDepName, maxDepLicense }
  where
    maxDepType    = max (S.length depTypeHeader) (fromMaybe 0 (maximum $ (\x -> S.length x.depType) <$> results))
    maxDepName    = max (S.length depNameHeader) (fromMaybe 0 (maximum $ (\x -> S.length x.depName) <$> results))
    maxDepLicense = max (S.length depLicensesHeader) (fromMaybe 0 (maximum $ (\x -> S.length x.depLicenses) <$> results))

padResults :: MaxLengths -> Array UnifiedResult -> Array UnifiedResult
padResults { maxDepType, maxDepName, maxDepLicense } = map f
  where
    f { depType, depName, depLicenses } = 
      { depType: spacePad maxDepType depType
      , depName: spacePad maxDepName depName
      , depLicenses: spacePad maxDepLicense depLicenses
      }

main :: Effect Unit
main = launchAff_ do 
  res <- attempt run
  either (log <<< message) pure res

rightPad :: Char -> Int -> String -> String
rightPad c n unpadded = 
  let whiteSpaces = n - S.length unpadded in
    unpadded <> CU.fromCharArray (replicate whiteSpaces c)

spacePad :: Int -> String -> String
spacePad = rightPad ' '

run :: Aff Unit
run = do
  liftEffect npmCrawler
  file <- liftEffect $ Buffer.toString UTF8 =<< FS.readFile fileName
  content :: NpmResult <- explodeIfLeft <<< readJSON $ file
  liftEffect $ catchException (const $ pure unit) (FS.unlink fileName)
  let npmResults = Obj.values $ Obj.mapWithKey npmToUnified content.report
  rawBowerResults :: Foreign <- makeAff $ \cb -> bowerCrawler cb $> nonCanceler
  bowerObject <- explodeIfLeft $ read rawBowerResults
  let bowerResults = Obj.values $ Obj.mapWithKey bowerToUnified bowerObject
  let combined = npmResults <> bowerResults
  let sorted = A.sortWith _.depLicenses combined
  let maxLengths = calcMaxLengths sorted
  let padded = padResults maxLengths sorted
  let rendered = renderUnifiedResult <$> padded
  printHeader maxLengths
  log $ intercalate "\n" rendered