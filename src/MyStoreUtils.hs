{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}


module MyStoreUtils where


import qualified Control.Concurrent    as Concurrent
import qualified Control.Monad         as CM
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Foldable         as Fold
import           Data.Maybe      
import qualified Data.List             as List
import qualified Data.Map              as Map
import           Data.Time.Clock
import           Data.Time.Format
import           Prelude
import qualified System.Directory      as SysDir
import qualified System.FilePath.Posix as SysFile
import           Text.Read             as Text

import qualified MyStoreConstants      as Constants



const_FileSeperator :: String
const_FileSeperator = "/" 


--------------------------------------------------------------------
-- Print IO

voidIO :: IO ()
voidIO = return ()

printVoid ::  IO ()
printVoid = printSameLine ""

printEmptyLine :: IO ()
printEmptyLine = printLine ""

printLine :: String -> IO ()
printLine displayString = putStrLn displayString

printSameLine :: String -> IO ()
printSameLine displayString = putStr displayString

printIO :: Show a => IO a -> IO ()
printIO inputData = do
                      inputData' <- inputData
                      printLine $ readShow inputData'

printMaybe :: Show a => Maybe a -> IO ()
printMaybe inputData = case inputData of
                                        Nothing         ->  printEmptyLine
                                        Just inputData' ->  printLine $ show inputData'

printMaybeIO :: Show a => IO (Maybe a) -> IO ()
printMaybeIO inputData = do
                      inputData' <- inputData
                      printMaybe inputData'                      

printList :: Show a => [a] -> IO ()
printList inputData =  printLine $  listToString inputData

printIOList :: Show a => IO [a] -> IO ()
printIOList inputData = do
                        inputData' <- inputData
                        printList inputData'

printMaybeList :: Show a => Maybe [a] -> IO ()
printMaybeList inputData = case inputData of
                                            Nothing         ->  printEmptyLine
                                            Just inputData' ->  printList inputData'

printMaybeIOList :: Show a => IO (Maybe [a]) -> IO ()
printMaybeIOList inputData = do
                              inputData' <- inputData
                              printMaybeList inputData'

printLineAndPromptEnterKey :: String -> IO String
printLineAndPromptEnterKey displayString = do 
                                          printLine $ "\n" ++ displayString ++ "\n\n" ++ Constants.string_Prompt_PressAnyKey
                                          readLine                  


--------------------------------------------------------------------
-- Prompt IO

readLine :: IO String
readLine = getLine
  
promptLine :: String ->  IO String
promptLine displayString = do 
                         printLine displayString
                         inputString <- readLine
                         if isEmptyString inputString
                          then promptLine displayString 
                          else return inputString 


promptChar :: String -> String -> IO (Char) 
promptChar displayString errorString = do 
                         inputString <- promptLine displayString
                         let inputCharacter = stringToMaybeFirstChar inputString
                         if Data.Maybe.isNothing inputCharacter
                           then do
                                  printLine errorString
                                  promptChar displayString errorString
                           else
                                  return $ fromMaybe '?' inputCharacter 

stringToMaybeFirstChar :: String -> Maybe Char
stringToMaybeFirstChar inputString 
                                    | not $ null inputString = Just $ head inputString
                                    | otherwise = Nothing


promptInt :: String -> IO Int
promptInt displayString = do
                          printLine displayString
                          inputLine <- readLine 

                          let maybeInt = Text.readMaybe inputLine :: Maybe Int
                          case maybeInt of
                                          Nothing         -> clearScreenAndPromptInt displayString
                                          Just maybeInt'  -> return maybeInt'

promptDouble :: String -> IO Double
promptDouble displayString = do
                              printLine displayString
                              inputLine <- readLine 
                              let maybeDouble = Text.readMaybe inputLine :: Maybe Double

                              case maybeDouble of
                                                Nothing -> clearScreenAndPromptDouble displayString
                                                Just maybeDouble' -> return maybeDouble'   

promptEnterKey ::  IO ()
promptEnterKey = do 
                    printLine $ "\n"++ Constants.string_Prompt_PressAnyKey
                    _ <- readLine
                    return ()

promptSelectedValueFromOptionMenu :: String -> String ->  [String] -> [a] ->  IO (Maybe a)
promptSelectedValueFromOptionMenu  inputMenuHeaderText inputPromptText  inputTextLines inputData = do
  
  let inputText = (listToTrimmedNewLinedString $  prefixIterationIntegerForLines inputTextLines) 
  selectedOption <- promptInt ( inputMenuHeaderText ++ "\n\n" ++ inputPromptText  ++ "\n\n" ++ inputText ++"\n\nOption: " )

  if (selectedOption > 0 ) && ( selectedOption -1 < length inputData )
    then  return $ Just $ getElementOfList inputData (selectedOption - 1)
    else return Nothing

promptSelectedValueFromOptionTableMenu :: String -> String ->  [String] -> [a] ->  IO (Maybe a)
promptSelectedValueFromOptionTableMenu inputMenuHeaderText inputPromptText  inputTextLines inputData = do
  
  let inputText = (listToTrimmedNewLinedString  inputTextLines) 
  selectedOption <- promptInt ( inputMenuHeaderText ++ "\n\n" ++ inputPromptText  ++ "\n\n" ++ inputText ++"\n\nOption: " )

  if (selectedOption > 0 ) && ( selectedOption -1 < length inputData )
    then return $ Just $ getElementOfList inputData (selectedOption - 1)
    else return Nothing

-- promptSelectedValueFromOptionMenuAsTable :: String -> String ->  [String] -> [a] ->  IO (Maybe a)
-- promptSelectedValueFromOptionMenuAsTable  inputMenuHeaderText inputPromptText  inputTextLines inputData = do
  
--   let inputText = (listToTrimmedNewLinedString $  prefixIterationIntegerForLines inputTextLines) 
--   selectedOption <- promptInt ( inputMenuHeaderText ++ "\n\n" ++ inputPromptText  ++ "\n\n" ++ inputText ++"\n\nOption: " )

--   if (selectedOption > 0 ) && ( selectedOption -1 < length inputData )
--     then  return $ Just $ getElementOfList inputData (selectedOption - 1)
--     else return Nothing                           

clearScreen :: IO ()
clearScreen = printLine Constants.string_Other_ClearScreenString

clearScreenAndPrintLine :: String -> IO ()
clearScreenAndPrintLine displayString = do
                                        clearScreen
                                        printLine displayString

clearScreenAndPromptOptionMenu ::  String ->  String ->  [String] -> [a] ->  IO (Maybe a)
clearScreenAndPromptOptionMenu inputMenuHeaderText  inputPromptText  inputTextLines inputData = 
                        do
                        clearScreen
                        promptSelectedValueFromOptionMenu inputMenuHeaderText inputPromptText inputTextLines inputData  

clearScreenAndPromptOptionTableMenu ::  String ->  String ->  [String] -> [a] ->  IO (Maybe a)
clearScreenAndPromptOptionTableMenu inputMenuHeaderText  inputPromptText  inputTextLines inputData = 
                        do
                        clearScreen
                        promptSelectedValueFromOptionTableMenu inputMenuHeaderText inputPromptText inputTextLines inputData                          

clearScreenAndPromptLine :: String -> IO String
clearScreenAndPromptLine displayString = do
                                          clearScreenAndPrintLine displayString
                                          inputString <- readLine
                                          if isEmptyString inputString
                                            then clearScreenAndPromptLine displayString 
                                            else return inputString


clearScreenAndPromptChar :: String -> String -> IO (Char) 
clearScreenAndPromptChar displayString errorString = do 
                                                      clearScreen
                                                      promptChar displayString errorString                                                         


clearScreenAndPromptInt :: String -> IO Int
clearScreenAndPromptInt displayString = do
                                        clearScreenAndPrintLine displayString
                                        inputLine <- readLine
                                        let maybeInt = Text.readMaybe inputLine :: Maybe Int
                                        case maybeInt of
                                                        Nothing         -> clearScreenAndPromptInt displayString
                                                        Just maybeInt'  -> return maybeInt'

clearScreenAndPromptDouble :: String -> IO Double
clearScreenAndPromptDouble displayString = do
                                          clearScreenAndPrintLine displayString
                                          inputLine <- readLine
                                          let maybeDouble = Text.readMaybe inputLine :: Maybe Double
                                          case maybeDouble of
                                                              Nothing           -> clearScreenAndPromptDouble displayString
                                                              Just maybeDouble' -> return maybeDouble'

clearScreenPrintLineAndPromptEnter :: String -> IO ()
clearScreenPrintLineAndPromptEnter displayString = do 
                                                    clearScreen
                                                    _ <- printLineAndPromptEnterKey displayString  
                                                    return () 


--------------------------------------------------------------------
-- Time / Sleep IO
              
getCurrentTimeAsInteger :: IO Int
getCurrentTimeAsInteger  = do
    currentTime <- getCurrentTimeAsString
    return $ read currentTime

getCurrentTimeAsString :: IO String
getCurrentTimeAsString  = do
    currentTime <- getCurrentTime
    let seconds = formatTime defaultTimeLocale "%s" currentTime
    return seconds

getCurrentDateAndTimeAsString :: IO String
getCurrentDateAndTimeAsString = do
    currentTime <- getCurrentTime
    return $ formatTime defaultTimeLocale "%d.%m.%Y - %H:%M:%S" currentTime

sleep :: Int -> IO ()
sleep timeToSleep = Concurrent.threadDelay timeToSleep

--------------------------------------------------------------------
-- File IO

isFile :: String ->  IO Bool
isFile file = SysDir.doesFileExist file

isDirectory :: String -> IO Bool
isDirectory folder = SysDir.doesDirectoryExist folder

createDirectory :: String -> IO ()
createDirectory directoryPath = SysDir.createDirectoryIfMissing True directoryPath

getFiles :: String  -> IO [String]
getFiles inputFolder  = do
  filesAndDirectories <- getFilesAndDirectories inputFolder
  onlyFiles <- getFilesOrDirectories isFile inputFolder filesAndDirectories []
  return onlyFiles

getDirectories :: String  -> IO [String]
getDirectories inputFolder  = do
  filesAndDirectories <- getFilesAndDirectories inputFolder
  onlyDirectories <- getFilesOrDirectories isDirectory inputFolder filesAndDirectories []
  return onlyDirectories

getFilesAndDirectories :: String -> IO [String]
getFilesAndDirectories inputFolder = do
          items <- SysDir.listDirectory inputFolder
          return $ filter ( not . List.isPrefixOf "." ) items 

getFilesAndDirectoriesWithRelativePath:: String -> IO [String]
getFilesAndDirectoriesWithRelativePath inputFolder = do

          items <- getFilesAndDirectories inputFolder
          let normalizedItems = [combineDirectoriesForPath inputFolder item | item <- items]
          return normalizedItems

getFilesOrDirectories :: (String -> IO Bool) -> String -> [String]  -> [String] -> IO [String]
getFilesOrDirectories booleanCheckFunction relativePath (itemToCheck:remainingItemsToCheck) listOfValidItems = do

      let relativeItemToCheck = combineDirectoriesForPath relativePath itemToCheck
  
      isValidItem <- booleanCheckFunction relativeItemToCheck
      let updatedListOfValidItems = if isValidItem
                                    then listOfValidItems ++ [relativeItemToCheck]
                                    else listOfValidItems
  
      getFilesOrDirectories booleanCheckFunction relativePath remainingItemsToCheck updatedListOfValidItems


getFilesOrDirectories _ _ _ listOfValidItems =  return $ listOfValidItems

readFileAsString  :: String -> IO String
readFileAsString inputFile =  do
  content <- B.readFile inputFile
  return $ read $ show content

readFileAsTrimmedString  :: String -> IO String
readFileAsTrimmedString inputFile =  do
  content <- readFileAsString inputFile
  return $ read $ trimNewLines $ show content 

readFileAsMaybeTrimmedString :: String -> IO (Maybe String)
readFileAsMaybeTrimmedString inputFile = do
  fileExists <- isFile inputFile
  if fileExists
    then do
      fileContent <- readFileAsTrimmedString inputFile
      return $ Just fileContent
    else return $ Nothing

readFileToMaybeStringList :: String -> IO (Maybe [String])
readFileToMaybeStringList fileName = do
    fileExists <- SysDir.doesFileExist fileName 
    if ( not fileExists )
      then do
        return Nothing
      else do
        words' <- B.readFile fileName

        return $ Just $ lines $ show $ words'

readFileToStringList :: String -> IO [String]
readFileToStringList fileName = do
    maybeReadFileContent <- readFileToMaybeStringList fileName
    case maybeReadFileContent of 
                              Just fileContent  -> return fileContent
                              Nothing           -> return []

createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  SysDir.createDirectoryIfMissing True $ SysFile.takeDirectory path
  writeFile' path content

writeFile' :: String -> String -> IO ()
writeFile' filePath content = do
  B.writeFile filePath $ B.pack content

writeFilesToFolderByTupleList :: String -> [(String, String)] -> IO ()
writeFilesToFolderByTupleList folder fileContentTupleList = do
  _ <- CM.forM fileContentTupleList (\(file,content) -> createAndWriteFile (combineDirectoriesForPath folder file) content) 
  return ()

combineDirectoriesForPath :: String -> String -> String
combineDirectoriesForPath directory subDirectory = directory ++ const_FileSeperator  ++ subDirectory

--------------------------------------------------------------------
-- Parsing

parseInt :: String -> Int
parseInt inputInt = read inputInt

parseDouble :: String -> Double
parseDouble inputDouble = read inputDouble

parseBool :: String -> Bool
parseBool inputBool = read inputBool

-- String / List manipulation --

readShow :: Show a =>  a -> String
readShow input = read $ show input

isEmptyString :: String -> Bool
isEmptyString inputString = length inputString == 0

isNonEmptyString :: String -> Bool
isNonEmptyString inputString = not $ isEmptyString inputString

padRight ::  String -> Int -> String
padRight inputString times
    | length inputString < times  = inputString ++ replicate (times - length inputString) ' '
    | otherwise     = inputString

padLeft ::  String -> Int -> String
padLeft inputString times
    | length inputString < times  =  ( replicate (times - length inputString) ' ' ) ++ inputString
    | otherwise     = inputString    

padRightEmpty :: Int -> String
padRightEmpty times = padRight "" times

postfixNewLineIfNonEmpty :: String -> String
postfixNewLineIfNonEmpty inputString
                                    | isEmptyString inputString = inputString
                                    | otherwise = inputString ++ "\n" 


isEmptyList :: [a] -> Bool
isEmptyList inputList = null inputList 

isNonEmptyList :: [a] -> Bool
isNonEmptyList inputList = not $ isEmptyList inputList

isEmptyMaybeList :: Maybe [a] -> Bool
isEmptyMaybeList inputMaybeList = do
                  case inputMaybeList of
                    Nothing -> False
                    Just inputList -> isEmptyList inputList

isNonEmptyMaybeList :: Maybe [a] -> Bool
isNonEmptyMaybeList inputMaybeList = not $ isEmptyMaybeList inputMaybeList

maybeIOListToIOList :: IO (Maybe [a]) -> IO [a]
maybeIOListToIOList ioMaybeList =do
  maybeList <-ioMaybeList
  return $ maybeListToList  maybeList

maybeListToList :: Maybe [a] -> [a]
maybeListToList inputMaybeList = case inputMaybeList of
    Nothing -> []
    Just inputList -> inputList


getListLength :: [a] -> Int
getListLength inputList = length inputList

getFirstElementOfList :: [a] -> a
getFirstElementOfList inputList = getElementOfList inputList 0 

getElementOfList :: [a] -> Int -> a
getElementOfList inputList index = inputList !!  index

getMaybeFirstElementOfMaybeList :: Maybe [a] -> Maybe a
getMaybeFirstElementOfMaybeList inputMaybeList = do
                                    case inputMaybeList of
                                      Nothing -> Nothing
                                      Just inputList -> do
                                        if isEmptyList inputList then Nothing else Just $ getFirstElementOfList inputList

getListOrPrintTryingWithEmptyList :: Maybe [a] -> String -> String -> IO [a]
getListOrPrintTryingWithEmptyList inputMaybeList errorWhenNothing errorWhenEmpty = 

  case inputMaybeList of
  
    Nothing         -> do
                        clearScreenPrintLineAndPromptEnter errorWhenNothing
                        return []
    Just inputList  -> do
                        if isEmptyList inputList
                          then do
                            clearScreenPrintLineAndPromptEnter errorWhenEmpty
                            return []
                          else return inputList

listToString :: Show a => [a] -> String
listToString inputData = ( listToTrimmedNewLinedString $ map (\inputDataElement ->  (show $ inputDataElement) ) inputData )

listToSpacedString :: [String] -> String
listToSpacedString inputList = List.intercalate " " inputList

objectListToStringList :: Show a => [a] -> [String]
objectListToStringList inputData =  map (\inputDataElement ->  (show $ inputDataElement) ) inputData 

removeEmptyStringsFromList :: [String] -> [String]
removeEmptyStringsFromList inputList = [ inputString  |  inputString <- inputList ,  not $ isEmptyString inputString]


removePrefix :: String -> String -> String
removePrefix prefixString inputString  = maybe "" id (List.stripPrefix prefixString inputString)
                                        
removePrefixWithSlash :: String -> String -> String
removePrefixWithSlash prefixString inputString  = maybe "" id (List.stripPrefix (prefixString++ const_FileSeperator ) inputString)

removePrefixForList ::  String -> [String] -> [String]
removePrefixForList prefixString inputList  = map (\x -> removePrefix prefixString x) inputList

removePrefixForListWithSlash ::  String -> [String] -> [String]
removePrefixForListWithSlash prefixString inputList  = map (\x -> removePrefixWithSlash prefixString x) inputList
 
removePrefixForTupleListWithSlash ::  [(String,[String])] -> [[String]]
removePrefixForTupleListWithSlash inputTupleList =  [  removePrefixForListWithSlash prefix stringList   | (prefix, stringList) <- inputTupleList ]
  
removeTextFromStringUntilLastMatchingCharacter :: String -> Char -> String
removeTextFromStringUntilLastMatchingCharacter inputString matchChar = reverse $ removeTextFromStringUntilLastMatchingCharacter' "" (reverse inputString) matchChar

removeTextFromStringUntilLastMatchingCharacter' :: String -> String -> Char -> String
removeTextFromStringUntilLastMatchingCharacter' output [] _ = output
removeTextFromStringUntilLastMatchingCharacter' output s m =  if  head s /= m then removeTextFromStringUntilLastMatchingCharacter' (output++ [(head s)]) (tail s) m else output

trimNewLines:: String -> String 
trimNewLines =  dropWhile (=='\n') .  reverse . dropWhile (=='\n') . reverse

listToNewlinedString :: [String] -> String
listToNewlinedString inputList = if isNonEmptyList inputList then unlines inputList else ""

listToTrimmedNewLinedString :: [String] -> String
listToTrimmedNewLinedString inputList = trimNewLines $ listToNewlinedString inputList

listToTrimmedNewLinedStringIO :: IO [String] -> IO String
listToTrimmedNewLinedStringIO inputList = do
                inputList' <- inputList
                return $ trimNewLines $ listToNewlinedString inputList'

listToTrimmedNewLinedStringMaybeIO :: IO (Maybe [String]) -> IO String
listToTrimmedNewLinedStringMaybeIO inputList = do
                                      inputList' <- inputList
                                      case inputList' of
                                        Nothing -> return ""
                                        Just inputList'' -> return $ listToTrimmedNewLinedString inputList''

prefixIterationIntegerForLines :: [String] -> [String]
prefixIterationIntegerForLines inputList = prefixIterationIntegerForLines' inputList 1

prefixIterationIntegerForLines' :: [String] -> Int -> [String]
prefixIterationIntegerForLines' [] _ = []
prefixIterationIntegerForLines' [line] counter = [ (show counter) ++ ") " ++ line]
prefixIterationIntegerForLines' (line:lines') counter =  [ (show counter) ++ ") " ++ line] ++  (prefixIterationIntegerForLines' lines' (counter+1))

--------------------------------------------------------------------
-- BoolTupleList

createFalseEmptyTupleList :: ([a],Bool)
createFalseEmptyTupleList = ([],False)

createListBoolTupleFromList :: [a] -> ([a],Bool)
createListBoolTupleFromList inputList = (inputList, isNonEmptyList inputList)

createJustListBoolTupleFromMaybeList :: [Maybe a] -> ([a],Bool)
createJustListBoolTupleFromMaybeList maybeList = ( justList ,allJust )
                      where
                        justList = catMaybes maybeList
                        allJust = length maybeList == length justList


isListBoolTupleTrue :: ([a],Bool) -> Bool
isListBoolTupleTrue listBoolTuple  = snd listBoolTuple

getListFromListBoolTuple :: ([a],Bool) -> [a]
getListFromListBoolTuple listBoolTuple = fst listBoolTuple

getFirstElementOfListBoolTuple :: ([a],Bool) -> a
getFirstElementOfListBoolTuple (list,_) =  getFirstElementOfList list


--------------------------------------------------------------------
-- Maps

createEmptyMap :: Map.Map k a
createEmptyMap = Map.empty

insertMap :: (Ord k) => k -> a -> Map.Map k a -> Map.Map k a
insertMap key value inputMap = Map.insert key value inputMap

getMapKeys :: Map.Map k a -> [k]
getMapKeys inputMap = Map.keys inputMap

getMapValues :: Map.Map k a -> [a]
getMapValues inputMap = Map.elems inputMap


getMaybeMapValue :: (Ord k) => Map.Map k a -> k -> Maybe a
getMaybeMapValue inputMap key = Map.lookup key inputMap

getValueFromMapByIndex :: (Ord k) => Map.Map k a -> k -> a
getValueFromMapByIndex inputMap index = inputMap Map.! index


getMapStringValue :: (Ord k) => Map.Map k String -> k -> String
getMapStringValue inputMap key = outputValue
                  where
                    maybeValue = getMaybeMapValue inputMap key
                    outputValue = case maybeValue of
                      Nothing -> ""
                      Just maybeValue' -> maybeValue' 

mapToList :: Map.Map k a -> [(k,a)]
mapToList inputMap = Map.toList inputMap

listToMap ::  (Ord k) =>  [(k,a)] -> Map.Map k a
listToMap inputTupleList = Map.fromList inputTupleList

deriveMapFromMapByKeys :: (Ord k) =>  Map.Map k a -> [k] -> Map.Map k (Maybe a)
deriveMapFromMapByKeys inputMap keys = do
                            let derivedMap = [   (key, getMaybeMapValue inputMap key)  | key <- keys ]
                            Map.fromList derivedMap


--------------------------------------------------------------------
-- Table

createTable :: [[String]] -> String
createTable input2DList = listToTrimmedNewLinedString  [  createTableRow inputList |  inputList  <-  input2DList ]

createTableWithHeader :: [String] -> [[String]] -> String
createTableWithHeader headerList input2DList = table
                            where
                                table =  headerRow ++ "\n" ++ dividerRow ++ "\n" ++ tableRows
                                headerRow = createTableRow headerList
                                dividerRow = replicate dividerRowLength '-'
                                dividerRowLength = (1 + length headerList) * 10
                                tableRows = createTable input2DList

createTableRow :: [String] -> String
createTableRow inputList = List.intercalate " | "  [   padRight inputString 10    | inputString  <- inputList ]

-- To do:  some of these functions can use a refactor to use a function instead

createTableWithHeaderWithDefiniedColumnWidth :: [Int] -> [String] -> [[String]] -> String
createTableWithHeaderWithDefiniedColumnWidth columnWidthLengths headerList input2DList = table
                            where
                                table =  headerRow ++ "\n" ++ dividerRow ++ "\n" ++ tableRows
                                headerRow = createTableRowWithDefinedColumnWidth columnWidthLengths headerList
                                dividerRow = replicate dividerRowLength '-'
                                dividerRowLength = sum columnWidthLengths
                                tableRows = createTableWithDefinedColumnWidth columnWidthLengths input2DList

createTableWithHeaderWithDefiniedColumnWidthForOptionMenuAsString ::   [Int] -> [String] -> [[String]] -> String
createTableWithHeaderWithDefiniedColumnWidthForOptionMenuAsString columnWidthLengths headerList input2DList = 
                                                     listToTrimmedNewLinedString $ createTableWithHeaderWithDefiniedColumnWidthForOptionMenu columnWidthLengths headerList input2DList 


createTableWithHeaderWithDefiniedColumnWidthForOptionMenu :: [Int] -> [String] -> [[String]] -> [String]
createTableWithHeaderWithDefiniedColumnWidthForOptionMenu columnWidthLengths headerList input2DList = table
                            where
                                table =  headerRow ++  dividerRow  ++ tableRows
                                headerRow = [createTableRowWithDefinedColumnWidth columnWidthLengths headerList]
                                dividerRow = [replicate dividerRowLength '-']
                                dividerRowLength = sum columnWidthLengths + (3* length columnWidthLengths)
                                tableRows = createTableWithDefinedColumnWidthAsStringList columnWidthLengths input2DList

createTableWithHeaderWithDefiniedColumnWidthAsStringList :: [Int] -> [String] -> [[String]] -> [String]
createTableWithHeaderWithDefiniedColumnWidthAsStringList columnWidthLengths headerList input2DList = table
                            where
                                table =  headerRow ++  dividerRow  ++ tableRows
                                headerRow = [createTableRowWithDefinedColumnWidth columnWidthLengths headerList]
                                dividerRow = [replicate dividerRowLength '-']
                                dividerRowLength = sum columnWidthLengths
                                tableRows = createTableWithDefinedColumnWidthAsStringList columnWidthLengths input2DList

createTableWithDefinedColumnWidth :: [Int] ->  [[String]] -> String
createTableWithDefinedColumnWidth columnWidthLengths input2DList = listToTrimmedNewLinedString $ createTableWithDefinedColumnWidthAsStringList columnWidthLengths input2DList

createTableWithDefinedColumnWidthAsStringList :: [Int] ->  [[String]] -> [String]
createTableWithDefinedColumnWidthAsStringList columnWidthLengths input2DList =   [  createTableRowWithDefinedColumnWidth columnWidthLengths inputList |  inputList  <-  input2DList ]

createTableRowWithDefinedColumnWidth :: [Int] -> [String] -> String
createTableRowWithDefinedColumnWidth  columnWidthLengths inputList = List.intercalate " | "  [   padRight (takeNCharsOrLengthFromString columnWidth inputString) (columnWidth)    | (columnWidth, inputString)  <- zip columnWidthLengths inputList ]

createTableRowWithDefinedColumnWidthWithPrefix :: [Int] -> [String] -> Int ->  String
createTableRowWithDefinedColumnWidthWithPrefix  columnWidthLengths inputList prefix  = List.intercalate " | "  [    (if ( prefix) > 0 then show prefix else "")  ++ padRight (takeNCharsOrLengthFromString columnWidth inputString) (columnWidth)    | (columnWidth, inputString)  <- zip columnWidthLengths inputList ]



takeNCharsOrLengthFromString :: Int -> String -> String
takeNCharsOrLengthFromString takeNChars inputString = takenString
                  where 
                    takenString = if takeNChars < lengthInputString then take takeNChars inputString else take lengthInputString inputString            
                    lengthInputString = length inputString


prefixIndexTo2DStringList :: [[String]] -> [[String]]
prefixIndexTo2DStringList inputList = prefixIndexTo2DStringList' 1 inputList

prefixIndexTo2DStringList' :: Int ->  [[String]] -> [[String]]
prefixIndexTo2DStringList' _ [] = [[]]
prefixIndexTo2DStringList' index [listItem] =    [       [  padRight ((show index) ++ ") " ) 5  ++ head listItem] ++ tail listItem]
prefixIndexTo2DStringList' index (listItem:listItems) =   [       [  padRight ((show index) ++ ") " ) 5 ++ head listItem] ++ tail listItem] ++ prefixIndexTo2DStringList' (index+1) listItems

prefixIndexToStringList :: [String] -> [String]
prefixIndexToStringList inputList = prefixIndexToStringList' 1 inputList

prefixIndexToStringList' :: Int ->  [String] -> [String]
prefixIndexToStringList' _ [] = []
prefixIndexToStringList' index [stringItem] = [(show index) ++ ") " ++ stringItem]
prefixIndexToStringList' index (stringItem:stringItems) =  [ (show index) ++ ") " ++ stringItem] ++ prefixIndexToStringList' (index+1) stringItems
