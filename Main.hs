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
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text(..), unpack)
import Data.List (foldl')

import Control.Monad.State.Strict

import Data.Conduit
import Text.XML.Stream.Parse
import Data.XML.Types

import Lens.Family2
import Lens.Family.TH
import Lens.Family2.State.Lazy


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

$(mkLenses ''NZSeg)
$(mkLenses ''NZBFile)


data MyState = MyState {
    _stbar :: Int
  , _stbaz :: String
  } deriving (Show)

$(mkLenses ''MyState)


----------------------------------------------------------------------------------------------------


main :: IO ()
main = do
    argv <- getArgs
    name <- getProgName
    if not (null argv)
      then cond (head argv) >> return ()
      else hPutStr stderr $ "usage: " ++ name ++ " nzb-filename\n"
    exitWith ExitSuccess


----------------------------------------------------------------------------------------------------


nm :: Text -> Name
nm t = Name t (Just "http://www.newzbin.com/DTD/2003/nzb") Nothing

nn :: Text -> Name
nn t = Name t Nothing Nothing

mkn :: Text -> Name -> Name
mkn mytag = \ns -> Name mytag (maybe Nothing id (Just $ nameNamespace ns)) Nothing

(===) :: Name -> Text -> Bool
n === t = (== n) (mkn t n)

(/==) :: Name -> Text -> Bool
n /== t = (/= n) (mkn t n)


----------------------------------------------------------------------------------------------------


parseGroups :: (MonadThrow m) => ConduitM Event o m (Maybe [Text])
parseGroups = 
    (tagNoAttr (nm "groups") $ many $ tagNoAttr (nm "group") $ content) `orE`
    (tagNoAttr (nn "groups") $ many $ tagNoAttr (nn "group") $ content)



parseSegments :: (MonadThrow m) => ConduitM Event o m (Maybe [NZSeg])
parseSegments = 
    (tagNoAttr (nm "segments") $ many $ parseNZSeg) `orE`
    (tagNoAttr (nn "segments") $ many $ parseNZSeg)




parseNZSeg :: (MonadThrow m) => ConduitM Event o m (Maybe NZSeg)
parseNZSeg =
    tagPredicate (=== "segment") attrs $ \(abytes, anumber) -> do
      segbody <- content
      let rbytes = (read $ unpack abytes)
          rnumber = (read $ unpack anumber)
          rbody = encodeUtf8 segbody
      return $ NZSeg rbody rnumber rbytes
  where
    attrs = do
      attrBytes <- requireAttr "bytes"
      attrNumber <- requireAttr "number"
      ignoreAttrs
      return (attrBytes, attrNumber)



parseChildren :: (MonadThrow m) => ConduitM Event o m (Maybe [NZSeg])
parseChildren = parseGroups >> parseSegments



parseNZBFile :: (MonadIO m, MonadState MyState m, MonadThrow m) => ConduitM Event o m (Maybe [NZBFile])
parseNZBFile =
    tagPredicate (=== "file") attrs $ \(asubject, adate) -> do
      stbar += 1
      st <- get
      junk <- contentMaybe
      let rsubject = encodeUtf8 asubject
          rdate = (read $ unpack adate)
      msegs <- parseChildren
      let segs = maybe [nzsDefault] id msegs
          nzb_total = foldl' (\acc elm -> acc + (_nzs_bytes elm)) 0 segs
          nzb = NZBFile rsubject nzb_total rdate segs
      liftIO $ putStrLn $ "nzb: " ++ (show $ nzb^.nzb_subject)
      liftIO $ putStrLn $ "Files read: " ++ show (_stbar st)
      -- return $ [NZBFile rsubject nzb_total rdate segs]
      return $ []
  where
    attrs = do
      attrDate <- requireAttr "date"
      attrSubj <- requireAttr "subject"
      ignoreAttrs
      return (attrSubj, attrDate)



parseNZB :: (MonadIO m, MonadState MyState m, MonadThrow m) => ConduitM Event o m (Maybe [NZBFile])
parseNZB =
    parseNZBFile `orE`
    (fmap . fmap) concat (tagPredicate (const True) ignoreAttrs (const $ many (skipJunk >> parseNZB)))



skipJunk :: (MonadThrow m) => ConduitM Event o m (Maybe a)
skipJunk = do
    junk <- contentMaybe
    case junk of
      Just _ -> skipJunk
      Nothing -> return Nothing



kond :: (MonadState MyState m, MonadResource m) => String -> m [NZBFile]
kond filename = 
    parseFile def (decodeString filename) $$ force "nzbfile required" parseNZB



cond :: String -> IO [NZBFile]
cond filename =
    flip evalStateT (MyState 0 "Blablabla") $ runResourceT $ kond filename

