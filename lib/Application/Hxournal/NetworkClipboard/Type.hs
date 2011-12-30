{-# LANGUAGE DeriveDataTypeable, 
             TemplateHaskell, 
             TypeFamilies, 
             TypeSynonymInstances, 
             OverloadedStrings,   
             RecordWildCards, 
             FlexibleInstances #-}

module Application.Hxournal.NetworkClipboard.Type where

import Control.Applicative 
import Control.Monad.Reader
import Control.Monad.State
import Data.Typeable
import Data.Data
import Data.SafeCopy
import qualified Data.Map as M

import Data.Acid 
import Data.UUID
import Data.Aeson
import Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Xournal.Simple
import qualified Data.Vector as V
import Data.Strict.Tuple
import Data.List.Split
import Data.Foldable 
import Data.Aeson.Types hiding (Pair)

import Prelude hiding (fst,snd,foldr)

data HxournalclipInfo = HxournalclipInfo { 
  hxournalclip_uuid :: UUID, 
  hxournalclip_strokes :: [Stroke]
} deriving (Show,Typeable) -- ,Data)


instance FromJSON UUID where
  parseJSON x = do 
    r <- return . fromString . C.unpack . E.encodeUtf8 =<< parseJSON x
    case r of 
      Nothing -> fail ("UUID parsing failed " ++ show x )
      Just uuid -> return uuid 

instance ToJSON UUID where
  toJSON = toJSON . E.decodeUtf8 . C.pack . toString 

instance FromJSON [Pair Double Double] where
  parseJSON (Array vs) = foldrM worker [] . splitEvery 2 . V.toList $ vs   
    where worker :: [Value] -> [Pair Double Double] -> Parser [Pair Double Double]
          worker [] acc = return acc
          worker [x] acc = return acc
          worker [x,y] acc =(:acc) <$> ((:!:) <$> parseJSON x <*> parseJSON y)
          worker _ acc = return acc 
  parseJSON _ = fail "Error in parsing [Pair Double Double]"

instance ToJSON [Pair Double Double] where   
  toJSON = Array . V.fromList . foldr (\x acc -> toJSON (fst x) : toJSON (snd x) : acc) [] 


instance FromJSON Stroke where
  parseJSON (Object v) = Stroke <$> v .: "tool" 
                                <*> v .: "color" 
                                <*> v .: "width" 
                                <*> v .: "data"

instance ToJSON Stroke where
  toJSON Stroke{..} = object [ "tool"  .= toJSON stroke_tool
                             , "color" .= toJSON stroke_color
                             , "width" .= toJSON stroke_width
                             , "data"  .= toJSON stroke_data ] 

                          
    
instance FromJSON HxournalclipInfo where
  parseJSON (Object v) = HxournalclipInfo <$>  v .: "uuid" <*> v .: "strokes"

instance ToJSON HxournalclipInfo where
  toJSON (HxournalclipInfo uuid strokes) = object [ "uuid" .= uuid 
                                                  , "strokes" .= strokes ]   
    

instance SafeCopy UUID where 
  putCopy uuid = contain $ safePut (toByteString uuid) 
  getCopy = contain 
            $ maybe (fail "cannot parse UUID") return . fromByteString 
              =<< safeGet

instance SafeCopy (Pair Double Double) where
  putCopy (x :!: y) = contain $ safePut (x,y)
  getCopy = contain $ do  
              (x,y) <- safeGet 
              return (x :!: y)


$(deriveSafeCopy 0 'base ''Stroke)
$(deriveSafeCopy 0 'base ''HxournalclipInfo)


type HxournalclipInfoRepository = M.Map UUID HxournalclipInfo 

addHxournalclip :: HxournalclipInfo -> Update HxournalclipInfoRepository HxournalclipInfo 
addHxournalclip minfo = do 
  m <- get 
  let (r,m') = M.insertLookupWithKey (\_k _o n -> n) (hxournalclip_uuid minfo) minfo m
  put m'
  return minfo
 
queryHxournalclip :: UUID -> Query HxournalclipInfoRepository (Maybe HxournalclipInfo) 
queryHxournalclip uuid = do 
  m <- ask 
  return (M.lookup uuid m)

queryAll :: Query HxournalclipInfoRepository [HxournalclipInfo]
queryAll = do m <- ask   
              return (M.elems m)


updateHxournalclip :: HxournalclipInfo -> Update HxournalclipInfoRepository (Maybe HxournalclipInfo)
updateHxournalclip minfo = do 
  m <- get 
  let (r,m') = M.updateLookupWithKey (\_ _ -> Just minfo) (hxournalclip_uuid minfo) m
  put m'
  maybe (return Nothing) (const (return (Just minfo))) r 

deleteHxournalclip :: UUID -> Update HxournalclipInfoRepository (Maybe HxournalclipInfo)
deleteHxournalclip uuid = do 
  m <- get
  let r = M.lookup uuid m  
  case r of 
    Just _ -> do  
      let m' = M.delete uuid m  
      put m' 
      return r
    Nothing -> return Nothing


$(makeAcidic ''HxournalclipInfoRepository [ 'addHxournalclip, 'queryHxournalclip, 'queryAll, 'updateHxournalclip, 'deleteHxournalclip] )
