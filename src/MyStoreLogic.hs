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

module MyStoreLogic where

import           Data.List      hiding (product)
import qualified Data.Map       as Map
import           Data.Maybe
import           Prelude        hiding (product)

import qualified FDatabaseData  as FD

import qualified MyStoreData      as MSD
import qualified MyStoreConstants as Constants
import qualified MyStoreUtils     as Utils

--------------------------------------------------------------------
-- Constants


const_DB_Accounts_Link :: String
const_DB_Accounts_Link = "db/accounts"

const_DB_Account_Name :: String
const_DB_Account_Name = "name"

const_DB_Account_Password :: String
const_DB_Account_Password = "password"



const_DB_Products_Link :: String
const_DB_Products_Link = "db/products"

const_DB_Product_ProductId :: String
const_DB_Product_ProductId = "productId"

const_DB_Product_Name :: String
const_DB_Product_Name = "name"

const_DB_Product_Price :: String
const_DB_Product_Price = "price"

const_DB_Product_Deprecated :: String
const_DB_Product_Deprecated = "deprecated"



const_DB_Orders_Link :: String
const_DB_Orders_Link = "db/orders"

const_DB_Order_OrderId :: String
const_DB_Order_OrderId = "orderId"

const_DB_Order_AccountId :: String
const_DB_Order_AccountId = "accountId"

const_DB_Order_Date :: String
const_DB_Order_Date = "orderDate"

const_DB_Order_OrderSum :: String
const_DB_Order_OrderSum = "orderSum"

const_DB_Order_OrderOverview :: String
const_DB_Order_OrderOverview = "orderOverview"


const_DB_Field_Not_Deprecated :: Bool
const_DB_Field_Not_Deprecated = False

const_DB_Field_Deprecated :: Bool
const_DB_Field_Deprecated = True


--------------------------------------------------------------------
-- Product

getProductsFromDB ::  IO [MSD.StoreProduct]
getProductsFromDB = Utils.maybeIOListToIOList $ FD.queryDBForObjecttList const_DB_Products_Link [] createProductFromMap

getNonDeprecatedProductsFromDB ::  IO [MSD.StoreProduct]
getNonDeprecatedProductsFromDB =  Utils.maybeIOListToIOList $  FD.queryDBForObjecttList const_DB_Products_Link [ (const_DB_Product_Deprecated, show const_DB_Field_Not_Deprecated)] createProductFromMap
                                    
getProductFromDB :: String  -> IO (Maybe MSD.StoreProduct)
getProductFromDB productLink =  FD.queryDBForObject productLink createProductFromMap

createProductFromMap :: String -> (Map.Map String String) -> MSD.StoreProduct
createProductFromMap productLink productMap  = product'
                where 
                    product' =  MSD.StoreProduct productId productName productPrice productDeprecated
                    productId = FD.shortenNodeName productLink
                    productName = Utils.getMapStringValue productMap const_DB_Product_Name
                    productPrice = Utils.parseDouble $ Utils.getMapStringValue productMap const_DB_Product_Price
                    productDeprecated = Utils.parseBool $ Utils.getMapStringValue productMap const_DB_Product_Deprecated

createAndSaveProduct :: String -> Double -> Bool -> IO (Maybe MSD.StoreProduct)
createAndSaveProduct productName productPrice productDeprecated = do
                    productId <- Utils.getCurrentTimeAsString
                    let productObject = MSD.StoreProduct productId productName productPrice productDeprecated
                    writeProductToDB productObject

createProductFromPrompt :: IO (Maybe MSD.StoreProduct)
createProductFromPrompt = do
  
  let promptPleaseEnterString = Constants.string_Product_Prompt_PleaseEnter
  productName <- Utils.clearScreenAndPromptLine $ promptPleaseEnterString ++ const_DB_Product_Name
  productPrice <- Utils.clearScreenAndPromptDouble $ promptPleaseEnterString ++ const_DB_Product_Price
  
  product <- createAndSaveProduct productName productPrice const_DB_Field_Not_Deprecated
  
  case product of
    Nothing -> return Nothing
    Just product' -> do
      product'' <- writeProductToDB product'
      case product'' of 
        Nothing -> do
          Utils.clearScreenPrintLineAndPromptEnter Constants.string_Product_Prompt_Error
          return Nothing
        Just product''' -> do
          Utils.clearScreenPrintLineAndPromptEnter $ Constants.string_Product_Prompt_Success ++ "\n" ++ (show product''')
          return $ Just product'''

editProduct :: MSD.StoreProduct -> IO (Maybe MSD.StoreProduct)
editProduct product = do

  let prefixPrompt = Constants.string_Product_Edit_Prompt_Prefix ++ "\n\n "++ show product ++ "\n\n" 
  let promptPleaseEnterString = Constants.string_Product_Edit_Prompt
  productName <- Utils.clearScreenAndPromptLine $ prefixPrompt ++ promptPleaseEnterString ++ const_DB_Product_Name ++ ": "
  productPrice <- Utils.clearScreenAndPromptDouble $ prefixPrompt ++ "(" ++ Constants.string_Product_Edit_New_Name ++ productName ++ ")\n\n" ++ promptPleaseEnterString ++ const_DB_Product_Price ++": "
  
  let editedProduct = product { MSD.fieldProductName = productName,  MSD.fieldProductPrice = productPrice }
  editedProduct' <- writeProductToDB editedProduct
  
  case editedProduct' of 
    Nothing -> do
      Utils.clearScreenPrintLineAndPromptEnter Constants.string_Product_Edit_Error_ProductNotSaved
      return Nothing
    Just editedProduct'' -> do
      Utils.clearScreenPrintLineAndPromptEnter $ Constants.string_Product_Edit_Success ++ "\n\n" ++ (show editedProduct'')
      return $ Just editedProduct''

writeProductToDB :: MSD.StoreProduct -> IO (Maybe MSD.StoreProduct)
writeProductToDB product = do
                            let productId = MSD.fieldProductId product
                            let productName = MSD.fieldProductName product
                            let productPrice = MSD.fieldProductPrice product
                            let productDeprecated =  show $ MSD.fieldProductDeprecated product

                            let productLink = Utils.combineDirectoriesForPath const_DB_Products_Link productId
                            let writeContents = [ ( const_DB_Product_Name, productName) , ( const_DB_Product_Price, show productPrice), (const_DB_Product_Deprecated , productDeprecated)]
                            Utils.writeFilesToFolderByTupleList productLink writeContents
                            
                            getProductFromDB productLink

getProductOverviewForAdmin :: [MSD.StoreProduct] -> IO String
getProductOverviewForAdmin products = do
  let productOverviewForAdmin = ["[Name] [Price] [Deprecated] [ID]\n"] ++ [ MSD.fieldProductName product ++ " | " ++ (show $ MSD.fieldProductPrice product) ++ " | $ "++  (show $ MSD.fieldProductDeprecated product) ++ " | " ++ MSD.fieldProductId product    |  product <- products]
  return $ Utils.listToTrimmedNewLinedString  productOverviewForAdmin

getProductSelectionOverviewForAdmin :: [MSD.StoreProduct] -> IO [String]
getProductSelectionOverviewForAdmin products = do
  let productOverviewForAdmin = [ MSD.fieldProductName product ++ " - " ++ (show $ MSD.fieldProductPrice product) ++ " -  #"++  MSD.fieldProductId product ++ "    "++(if MSD.fieldProductDeprecated product then "( Deprecated )" else "" ) ++ ""    |  product <- products]
  return  productOverviewForAdmin


--------------------------------------------------------------------
-- Account

getAccountsFromDBNode ::  IO (Maybe [MSD.StoreAccount])
getAccountsFromDBNode = FD.queryDBForObjecttList const_DB_Accounts_Link [] createAccountFromMap

getAccount :: String  -> IO (Maybe MSD.StoreAccount)
getAccount accountLink =  FD.queryDBForObject accountLink createAccountFromMap

createAccountFromMap :: String -> (Map.Map String String) -> MSD.StoreAccount
createAccountFromMap accountLink accountMap  = account 
              where 
                  account =  MSD.StoreAccount accountId accountName accountPassword
                  accountId = FD.shortenNodeName accountLink
                  accountName = Utils.getMapStringValue accountMap const_DB_Account_Name
                  accountPassword =  Utils.getMapStringValue accountMap const_DB_Account_Password 

createAndSaveAccount :: String -> String -> IO (Maybe MSD.StoreAccount)
createAndSaveAccount accountName accountPassword = do
                    accountId <- Utils.getCurrentTimeAsString
                    let accountObject = MSD.StoreAccount accountId accountName accountPassword
                    account <- writeAccountToDB accountObject
                    return account

writeAccountToDB :: MSD.StoreAccount -> IO (Maybe MSD.StoreAccount)
writeAccountToDB account = do
                            let accountId = MSD.fieldAccountId account
                            let accountName = MSD.fieldAccountName account
                            let accountPassword = MSD.fieldAccountPassword account
                            
                            let accountLink = Utils.combineDirectoriesForPath const_DB_Accounts_Link accountId
                            let writeContents = [ ( const_DB_Account_Name, accountName) , (const_DB_Account_Password, accountPassword) ]
                            Utils.writeFilesToFolderByTupleList accountLink writeContents
                            getAccount accountLink

--------------------------------------------------------------------
-- Order


getOrdersFromDBForSession :: MSD.StoreSession ->  IO (Maybe [MSD.StoreOrder])
getOrdersFromDBForSession storeSession = do
                                    let accountId = MSD.getAccountIdFromSession storeSession
                                    FD.queryDBForObjecttList const_DB_Orders_Link [(const_DB_Order_AccountId, accountId)] createOrderFromMap

getOrdersFromDBByOrderId ::  String -> IO (Maybe MSD.StoreOrder)
getOrdersFromDBByOrderId orderId = do
                                orders  <- FD.queryDBForObjecttList const_DB_Orders_Link [(const_DB_Order_OrderId, orderId)] createOrderFromMap
                                print orders
                                return $ Utils.getMaybeFirstElementOfMaybeList orders                                

getOrdersFromDB ::  IO (Maybe [MSD.StoreOrder])
getOrdersFromDB = FD.queryDBForObjecttList const_DB_Orders_Link [] createOrderFromMap

getOrder :: String  -> IO (Maybe MSD.StoreOrder)
getOrder orderLink =  FD.queryDBForObject orderLink createOrderFromMap


createOrderFromMap :: String -> (Map.Map String String) -> MSD.StoreOrder
createOrderFromMap orderLink orderMap  = order 
              where 
                  order =  MSD.StoreOrder orderId orderAccountId orderDate orderSum orderOverview
                  orderId = FD.shortenNodeName orderLink
                  orderAccountId = Utils.getMapStringValue orderMap const_DB_Order_AccountId
                  orderDate = Utils.getMapStringValue orderMap const_DB_Order_Date
                  orderSum = Utils.getMapStringValue orderMap const_DB_Order_OrderSum
                  orderOverview =  Utils.getMapStringValue orderMap const_DB_Order_OrderOverview 


writeOrderToDB :: MSD.StoreSession -> IO (Maybe MSD.StoreOrder)
writeOrderToDB storeSession = do
                            let shoppingCartData= MSD.getShoppingCartData storeSession
                            let checkoutOverviewData = MSD.evaluateShoppingCartDataToCheckoutOverview ( shoppingCartData )

                            orderId <- Utils.getCurrentTimeAsString
                            let accountId = MSD.getAccountIdFromSession storeSession
                            orderDate <- Utils.getCurrentDateAndTimeAsString
                            let orderSum = show $ MSD.getTotalSumForProducts $ MSD.fieldCheckoutShoppingCartItems checkoutOverviewData
                            let checkoutOverviewString = getShoppingCartOverviewAsString storeSession False

                            let writeContents = [ (const_DB_Order_AccountId, accountId) , (const_DB_Order_Date, orderDate), (const_DB_Order_OrderSum,orderSum), (const_DB_Order_OrderOverview, checkoutOverviewString) ]
                        
                            let orderLink = Utils.combineDirectoriesForPath const_DB_Orders_Link orderId
                            Utils.writeFilesToFolderByTupleList orderLink writeContents
                            getOrder orderLink

--------------------------------------------------------------------
-- Functions

--------------------------------------------------------------------
-- Functions for Account

createSession :: MSD.StoreAccount -> MSD.StoreShoppingCartData -> MSD.StoreSession
createSession storeAccount shoppingCartData =  MSD.StoreSession (storeAccount) (shoppingCartData)

getAccountForUsername :: String -> IO (Maybe MSD.StoreAccount)
getAccountForUsername inputName = do
                accountObjects <-  FD.queryDBForObjecttList const_DB_Accounts_Link [(const_DB_Account_Name,inputName)] createAccountFromMap
                return $ Utils.getMaybeFirstElementOfMaybeList accountObjects

getAccountForUsernameAndPassword :: String -> String -> IO (Maybe MSD.StoreAccount)
getAccountForUsernameAndPassword inputName inputPassword = do
                accountObjects <- FD.queryDBForObjecttList  const_DB_Accounts_Link [(const_DB_Account_Name, inputName),(const_DB_Account_Password, inputPassword)] createAccountFromMap
                return $ Utils.getMaybeFirstElementOfMaybeList accountObjects

checkAdminCredentials :: IO Bool
checkAdminCredentials = do
  inputAdminPassword <- Utils.clearScreenAndPromptLine Constants.string_Admin_Credentials_Prompt
  adminPassword <- FD.getAdminPassword
  case adminPassword of
    Nothing -> do
      _ <- Utils.printLineAndPromptEnterKey Constants.string_Admin_Credentials_Error_PasswordNotFound
      return False
    Just adminPassword' -> do
      if (adminPassword' /= inputAdminPassword )
        then do
          _ <- Utils.printLineAndPromptEnterKey Constants.string_Admin_Credentials_Error_PasswordInputMismatch
          return False
        else return True

--------------------------------------------------------------------
-- Functions for Order

performCheckout :: MSD.StoreSession -> IO (Maybe MSD.StoreOrder)
performCheckout storeSession = do
  writeOrderToDB storeSession 


--------------------------------------------------------------------
-- Misc. functions for Product 


findProductsByName :: [MSD.StoreProduct] ->  String -> [MSD.StoreProduct]
findProductsByName storeProductList matchName =  mapMaybe (tryMatchProductByName matchName) storeProductList

tryMatchProductByName ::   String -> MSD.StoreProduct -> Maybe MSD.StoreProduct
tryMatchProductByName  matchName  product'
                                            | matchName `isInfixOf` MSD.fieldProductName product' = Just product'
                                            | otherwise = Nothing

excludeSpecifiedProductFromShoppingCart :: [MSD.StoreShoppingCartItemData] -> MSD.StoreProduct ->   [MSD.StoreShoppingCartItemData]
excludeSpecifiedProductFromShoppingCart cartEntries productToExclude =   filter (\x -> productToExclude /= (MSD.fieldCartProduct x) ) cartEntries


findAmountOfProductsInShoppingCartForProduct :: [MSD.StoreShoppingCartItemData] -> MSD.StoreProduct ->   Int
findAmountOfProductsInShoppingCartForProduct cartEntries compareProduct  =  returnValue
                                                        where
                                                          tryMatch = [ amount | MSD.StoreShoppingCartItemData product' amount <- cartEntries , compareProduct == product']
                                                          returnValue = if not $ null tryMatch then head tryMatch else 0

getSelectProductList :: [MSD.StoreProduct] -> Int -> [(Int,String,MSD.StoreProduct)]
getSelectProductList [] _ = []
getSelectProductList [product'] index = [(index,show index ++ ") " ++ show product', product')]
getSelectProductList (product' : products) index = [(index, show index ++ ") "++ show product',product')] ++ getSelectProductList products (index+1)

tryMatchProduct :: Int -> (Int,String,MSD.StoreProduct) -> Maybe MSD.StoreProduct
tryMatchProduct productIndex (index, _ ,product') = if productIndex == index then Just product' else Nothing

getCheckoutOverviewString :: MSD.StoreSession -> String 
getCheckoutOverviewString storeSession = show $ MSD.evaluateShoppingCartDataToCheckoutOverview ( MSD.fieldSessionShoppingCartData storeSession)

createProductOverviewAs2DStringList :: [MSD.StoreProduct] -> [[String]]
createProductOverviewAs2DStringList productList = [ [MSD.fieldProductName product , show $ MSD.fieldProductPrice product ,  MSD.fieldProductId product,  if MSD.fieldProductDeprecated product then "Deprecated" else "" ]  |  product  <- productList ]

getShoppingCartOverviewAsString :: MSD.StoreSession -> Bool -> String
getShoppingCartOverviewAsString storeSession asSelectionMenu = do
         
                  let shoppingCartDataItems = MSD.getShoppingCartDataItems storeSession 

                  let cartDataAs2DList = [  [ show $ MSD.fieldCartProductAmount shoppingCartData, MSD.fieldProductName  $ MSD.fieldCartProduct shoppingCartData, show $ MSD.fieldProductPrice $ MSD.fieldCartProduct shoppingCartData ,  show $ MSD.getTotalPriceForProduct shoppingCartData ]  |  shoppingCartData  <- shoppingCartDataItems]
                  let grandTotalString = "Grand total: $ " ++ show ( MSD.getTotalSumForProductsInSession storeSession)

                  if asSelectionMenu
                    then do
                        Utils.createTableWithHeaderWithDefiniedColumnWidthForOptionMenuAsString [10,16,10,10] ["Amount", "Name" ,"Price", "Sum"] ( Utils.prefixIndexTo2DStringList  cartDataAs2DList) ++ "\n\n" ++ grandTotalString
                    else do
                        Utils.createTableWithHeaderWithDefiniedColumnWidth [7,16,10,10] ["Amount", "Name" ,"Price", "Sum"] cartDataAs2DList ++ "\n\n" ++ grandTotalString

