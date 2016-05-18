{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Csv.Parser as CSV
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V 
import qualified Data.HashMap.Strict as HM
import qualified Control.Arrow as A
import qualified Data.List as L

main = return ()

emails = do
  s <- B.readFile "/home/danielbarter/hillary/output/Emails.csv"
  let x = AB.parseOnly (CSV.csvWithHeader CSV.defaultDecodeOptions) s
  case x of Right d -> return d
            _       -> undefined

persons = do
  s <- B.readFile "/home/danielbarter/hillary/output/Persons.csv"
  let x = AB.parseOnly (CSV.csvWithHeader CSV.defaultDecodeOptions) s
  case x of Right d -> return d
            _       -> undefined

getHeader x         = fst <$> x
getRecords x        = snd <$> x
getRecord x n       = (\r -> r V.!? n) <$> getRecords x
getRecordUnsafe x n = (\r -> r V.! n) <$> getRecords x

getKeys x n = ((fmap . fmap) HM.keys) $ getRecord x n
getKeysUnsafe x n = (fmap HM.keys) $ getRecordUnsafe x n 

-- email headers
metadataSubject   record = HM.lookup "MetadataSubject"   record
rawText           record = HM.lookup "RawText"           record
extractedBodyText record = HM.lookup "ExtractedBodyText" record
metadataTo        record = HM.lookup "MetadataTo"        record
metadataFrom      record = HM.lookup "MetadataFrom"      record
senderPersonId    record = HM.lookup "SenderPersonId"    record

-- person headers
nameID           record = HM.lookup "Id"                record
name             record = HM.lookup "Name"              record

-- h should be one of the header extractors above
extract x h = (fmap . fmap) (killJust . h) (getRecords x)
  where killJust (Just z) = z


frequency :: Ord a => V.Vector a -> [(Int,a)] 
frequency = (map (L.length A.&&& L.head) . L.group . L.sort) . V.toList

frequencySenderPersonIDpairs = fmap frequency $ extract emails senderPersonId

senderFrequencyPairs = 
  do map <- personIDnameMap
     p <- frequencySenderPersonIDpairs
     let l = [(map HM.! i,f) | (f,i) <- p]
     return $ L.sortOn snd l

personIDnameMap = do ids <- V.toList <$> (extract persons nameID)
                     names <- V.toList <$> (extract persons name)
                     let map = HM.fromList $ zip ids names
                     return $ HM.insert "" "Not Named" map

namePersonIDmaps = do ids <- V.toList <$> (extract persons nameID)
                      names <- V.toList <$> (extract persons name)
                      let map = HM.fromList $ zip names ids
                      return map

findPersonID name = do map <- namePersonIDmaps
                       return $ map HM.! name


getEmailsFrom name = 
  do i <- findPersonID name
     records <- getRecords emails
     let records' = V.filter (\r -> ((r HM.! "SenderPersonId") == i)) records
     return records'
