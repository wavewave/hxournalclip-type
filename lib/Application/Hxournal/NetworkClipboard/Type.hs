{-# LANGUAGE DeriveDataTypeable, 
             TemplateHaskell, 
             TypeFamilies, 
             TypeSynonymInstances, 
             OverloadedStrings  #-}

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

data HxournalclipInfo = HxournalclipInfo { 
  hxournalclip_uuid :: UUID, 
  hxournalclip_name :: String
} deriving (Show,Typeable,Data)


instance FromJSON UUID where
  parseJSON x = do r <- return . fromString . C.unpack . E.encodeUtf8 =<< parseJSON x
                   case r of 
                     Nothing -> fail ("UUID parsing failed " ++ show x )
                     Just uuid -> return uuid 

instance ToJSON UUID where
  toJSON = toJSON . E.decodeUtf8 . C.pack . toString 

instance FromJSON HxournalclipInfo where
  parseJSON (Object v) = HxournalclipInfo <$>  v .: "uuid" <*> v .: "name"

instance ToJSON HxournalclipInfo where
  toJSON (HxournalclipInfo uuid name) = object [ "uuid" .= uuid , "name" .= name ] 


instance SafeCopy UUID where 
  putCopy uuid = contain $ safePut (toByteString uuid) 
  getCopy = contain 
            $ maybe (fail "cannot parse UUID") return . fromByteString 
              =<< safeGet

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
