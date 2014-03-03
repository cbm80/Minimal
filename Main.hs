{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-name-shadowing #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment
import System.Exit
import System.IO

import Filesystem.Path.CurrentOS (FilePath, decodeString)

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import qualified Data.Text as T
import Data.Text (Text(..))

import Control.Monad.IO.Class

import Data.Conduit
import Text.XML.Stream.Parse
import Data.XML.Types


----------------------------------------------------------------------------------------------------


data NZSeg = NZSeg {
   _nzs_body    :: ByteString
  ,_nzs_number  :: Int
  ,_nzs_bytes   :: Int
  } deriving (Show, Eq)

nzsDefault = NZSeg B.empty 0 0


data NZBFile = NZBFile {
   _nzb_subject     :: ByteString
  ,_nzb_totalsize   :: Int
  ,_nzb_date        :: Int
  ,_nzb_segments    :: [NZSeg]
  } deriving (Show, Eq)

nzbDefault = NZBFile B.empty 0 0 []


----------------------------------------------------------------------------------------------------


main :: IO ()
main = do
    argv <- getArgs
    name <- getProgName
    if not (null argv)
      then runResourceT $ cond (head argv) >> return ()
      else hPutStr stderr $ "usage: " ++ name ++ " nzb-filename\n"
    exitWith ExitSuccess


----------------------------------------------------------------------------------------------------


nm :: Text -> Name
nm t = Name t (Just "http://www.newzbin.com/DTD/2003/nzb") Nothing

mkn :: Text -> Name -> Name
mkn mytag = \ns -> Name mytag (maybe Nothing id (Just $ nameNamespace ns)) Nothing

(===) :: Name -> Text -> Bool
n === t = (== n) (mkn t n)

(/==) :: Name -> Text -> Bool
n /== t = (/= n) (mkn t n)


----------------------------------------------------------------------------------------------------


parseGroups :: (MonadThrow m) => ConduitM Event o m (Maybe [Text])
parseGroups = 
    (tagNoAttr (nm "groups") $ many $ tagNoAttr (nm "group") $ content) 


parseSegments :: (MonadThrow m) => ConduitM Event o m (Maybe [NZSeg])
parseSegments = 
    (tagNoAttr (nm "segments") $ many $ tagPredicate (=== "segment") ignoreAttrs $ const $ (content >> return nzsDefault)) 


parseChildren :: (MonadThrow m) => ConduitM Event o m (Maybe [NZSeg])
parseChildren = parseGroups >> parseSegments


parseNZBFile :: (MonadIO m, MonadThrow m) => ConduitM Event o m (Maybe [NZBFile])
parseNZBFile =
    tagPredicate (=== "file") ignoreAttrs $ const $ do
      contentMaybe >> parseChildren
      liftIO $ putStrLn "nzb: parsed one element."
      return []


parseNZB :: (MonadIO m, MonadThrow m) => ConduitM Event o m (Maybe [NZBFile])
parseNZB =
    parseNZBFile `orE`
    (fmap . fmap) concat (tagPredicate (const True) ignoreAttrs $ const $ many (skipJunk >> parseNZB))


skipJunk :: (MonadThrow m) => ConduitM Event o m (Maybe a)
skipJunk = do
    junk <- contentMaybe
    case junk of
      Just _ -> skipJunk
      Nothing -> return Nothing


cond :: (MonadResource m) => String -> m [NZBFile]
cond filename = 
    parseFile def (decodeString filename) $$ force "nzbfile required" parseNZB


