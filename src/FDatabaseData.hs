{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module FDatabaseData where

import qualified Control.Monad         as CM
import           Data.Maybe            as Maybe
import           Data.Either           as Either
import           Data.Foldable         as Fold
import qualified Data.List             as List
import qualified Data.Map              as Map
import           Prelude

import qualified FDatabaseUtils        as Utils



--------------------------------------------------------------------
-- Constants & Data

const_DB_Settings_Database_Link :: String
const_DB_Settings_Database_Link = "db/settings/database"

const_DB_Settings_Database_AdminPassword :: String
const_DB_Settings_Database_AdminPassword = "adminPassword"


data FDBNode = FDBNode
  {
    fieldNodeName :: String,
    fieldNodeMap :: Maybe (Map.Map String (Either FDBNode String))
  }
  deriving( Show)


--------------------------------------------------------------------
--- Get admin password

getAdminPassword :: IO (Maybe String)
getAdminPassword = do
  readValueFromDBLink const_DB_Settings_Database_Link const_DB_Settings_Database_AdminPassword


--------------------------------------------------------------------
-- Create Node function & Query function

createNode :: String -> IO (Maybe FDBNode)
createNode fileOrFolder = do

  let fileOrFolder' = fileOrFolder
  isDirectory <- Utils.isDirectory fileOrFolder'

  if not isDirectory
    then return $ Nothing
    else do
          subItems <- Utils.getFilesAndDirectoriesWithRelativePath fileOrFolder'
          files <- Utils.getFiles fileOrFolder'
          subFolders <- Utils.getDirectories fileOrFolder'

          if null subItems
            then  return $ Nothing
            else do          

              filesContent <- CM.forM files (\fileName -> Utils.readFileAsTrimmedString fileName ) 

              let filesTrimmed = Utils.removePrefixForListWithSlash fileOrFolder files
              let fileNodes = zip filesTrimmed [ Right fileContent | fileContent <- filesContent ]
                      
              subFolderNodes <- CM.forM subFolders createNode 
              let subFolderNodes' = map (\sfNode -> Left sfNode) (catMaybes subFolderNodes)
              
              let folderNodes = zip  subFolders subFolderNodes'
              let nodeMap = Just $ Map.fromList ( folderNodes ++ fileNodes )

              return $ Just $ FDBNode fileOrFolder' nodeMap


queryMaybeNodeTo2DMap :: Maybe FDBNode -> [(String,String)]  -> IO (Maybe (Map.Map String (Map.Map String String)))
queryMaybeNodeTo2DMap inputNode matchKeysValuesPair= do
                                        case inputNode of
                                                Nothing         ->  return Nothing
                                                Just inputNode' -> do
                                                                     queryNodeTo2dMap inputNode' matchKeysValuesPair

queryNodeTo2dMap :: FDBNode -> [(String,String)]   -> IO (Maybe (Map.Map String (Map.Map String String)))
queryNodeTo2dMap inputNode matchKeysValuesPair = do
                               
                               twoDMap <- get2DMapFromNode inputNode
                               tupleListMap <- getTupleMapFrom2DMap twoDMap

                               case tupleListMap of 
                                                  Nothing            -> return Nothing
                                                  Just tupleListMap' -> do
                                                                            let matches = [  (headKey,inputMap)   | (headKey, inputMap )     <-  tupleListMap' ,  check2dMapForGivenKeysAndValues (headKey, inputMap ) matchKeysValuesPair ]            
                                                                            return $ Just $ Utils.listToMap matches

readValueFromDBLink :: String -> String -> IO (Maybe String)
readValueFromDBLink parentDBLink childKey = do
                  readValueFromDBLink' (Utils.combineDirectoriesForPath parentDBLink childKey)

readValueFromDBLink' :: String -> IO (Maybe String)
readValueFromDBLink' dbLink = do
                  Utils.readFileAsMaybeTrimmedString dbLink


--------------------------------------------------------------------
-- Create Object List of given Key/Value equal mapping to query with



queryDBForObjecttList :: String ->  [(String,String)]  -> ( String -> (Map.Map String String) ->  a) -> IO (Maybe [a])
queryDBForObjecttList dbLink matchKeysValuesPair functionToObjectA = do
                                        
                                        node <- createNode dbLink
                                        twoDMap <- queryMaybeNodeTo2DMap node matchKeysValuesPair
                                        
                                        case twoDMap of
                                                      Nothing       -> return Nothing
                                                      Just twoDMap' -> do
                                                                        let keys = Utils.getMapKeys twoDMap'
                                                                        let objects = catMaybes [ createMaybeObjectByFunction key functionToObjectA  (Utils.getMaybeMapValue twoDMap' key ) |  key  <- keys ]
                                                                        return $ Just objects
                                    

queryDBForObject :: String -> ( String -> (Map.Map String String) ->  a) -> IO (Maybe a)
queryDBForObject dbLink functionToObjectA = do                  
                                            let objectId = shortenNodeName dbLink
                                            node <- createNode dbLink
                                            case node of
                                              Nothing -> return Nothing 
                                              Just node' -> do
                                                              mapFromNode <- getMapFromNode node'
                                                              return $ createMaybeObjectByFunction objectId functionToObjectA mapFromNode

createMaybeObjectByFunction :: String -> (String -> (Map.Map String String) ->  a) -> Maybe ( Map.Map String String ) ->  Maybe a
createMaybeObjectByFunction inputObjectId  functionToObjectA inputMap = do
                                            case inputMap of
                                                            Nothing        -> Nothing
                                                            Just inputMap' -> Just $ functionToObjectA inputObjectId inputMap'
                                                                  
--------------------------------------------------------------------
-- Helper fucntions to get a printable string representation of the DB Nodes

getFDBNodeToString ::  IO (Maybe FDBNode) -> IO String
getFDBNodeToString node  = do
                          node' <- node
                          case node' of 
                            Nothing -> return ""
                            Just node'' -> getFDBNodeToString' 0 node''

getFDBNodeToString' :: Int ->  FDBNode -> IO String
getFDBNodeToString' offset node  = do
        let nodeMap = fieldNodeMap node
        case nodeMap of
          Nothing -> return $ ""
          Just map' -> do

            let newStringPaddingOffset = offset + 2

            let nodeStringValues = filterStringEitherNodeStringToStringTupleList $  Utils.mapToList map'
            let nodeStringContent = [ Utils.padRightEmpty newStringPaddingOffset ++  key ++ " = " ++ value | ( key,  value ) <- nodeStringValues]
            let subContent = List.intercalate "\n" nodeStringContent

            let headLeftValues = lefts $ Utils.getMapValues map'
            subHeadValues <- CM.forM headLeftValues (getFDBNodeToString' (newStringPaddingOffset))
            let additionalNewLine  = if  ( not $ null subHeadValues) && (not $ null nodeStringValues) then "\n" else ""
            let subHeadContent = Utils.listToTrimmedNewLinedString  subHeadValues 

            let nodeName = fieldNodeName node
            let outputString = (Utils.padRightEmpty offset) ++"[" ++ (nodeName) ++ "]\n"++  subContent ++ additionalNewLine ++  subHeadContent
             
            return outputString


--------------------------------------------------------------------
-- Auxiliary functions

get2DMapFromNode :: FDBNode -> IO (Maybe (Map.Map String (Map.Map String String)))
get2DMapFromNode inputNode = do
              let nodeMap = fieldNodeMap inputNode
              case nodeMap of
                Nothing -> return $ Nothing
                Just map' -> do
                              let headElems = Utils.getMapKeys map'
                              let subNodes = lefts $ Utils.getMapValues map' 

                              subNodeMaps <-  CM.forM subNodes getMapFromNode
                              let outputValue = Map.fromList $ zip headElems (catMaybes subNodeMaps)
                              return $ Just outputValue

check2dMapForGivenKeysAndValues :: (String, Map.Map String String) -> [(String, String)] -> Bool
check2dMapForGivenKeysAndValues ( _, inputMap) matchKeysValuesPair = and [  (Utils.getMapStringValue inputMap key ) == value  |  (key,value)   <- matchKeysValuesPair ] 



getTupleMapFrom2DMap ::   (Maybe (Map.Map String (Map.Map String String))) -> IO (Maybe [(String, Map.Map String String )])
getTupleMapFrom2DMap twoDMap = do
                                case twoDMap of 
                                                Nothing       -> return Nothing
                                                Just twoDMap' -> return $ Just $ Utils.mapToList twoDMap'

                            
getMapFromNode :: FDBNode -> IO (Maybe (Map.Map String String))
getMapFromNode inputNode = do
              let nodeMap = fieldNodeMap inputNode
              case nodeMap of
                        Nothing     -> return Nothing
                        Just map'   -> do
                                        let map'' = (Utils.mapToList map')
                                        let map''' =  Map.fromList $ filterStringEitherNodeStringToStringTupleList map''
                                        return $ Just map'''
                  

filterStringEitherNodeStringToStringTupleList :: [(String, Either FDBNode String)] -> [(String, String)]
filterStringEitherNodeStringToStringTupleList inputTupleList =   [   (key,value)   |  (key, Right value) <- filtered ]
                            where
                              filtered = (filter isStringInStringEitherNodeStringTuple inputTupleList)

isStringInStringEitherNodeStringTuple :: (String, Either FDBNode String) -> Bool
isStringInStringEitherNodeStringTuple (_, Left _)= False
isStringInStringEitherNodeStringTuple (_, Right _) = True


shortenNodeName :: String -> String
shortenNodeName inputString = Utils.removeTextFromStringUntilLastMatchingCharacter inputString '/'

removeLastPartOfDBLink :: String -> String
removeLastPartOfDBLink dbLink = take ( (length dbLink) -   ( length $ shortenNodeName dbLink ) -1)  dbLink 

