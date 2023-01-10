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


module MyStoreData where


import           Data.Foldable         as Fold
import           Prelude
import qualified MyStoreUtils          as Utils


--------------------------------------------------------------------
-- StoreSession

data StoreSession = StoreSession
  { 
    fieldSessionAccountData :: StoreAccount, 
    fieldSessionShoppingCartData :: StoreShoppingCartData 
  }
  deriving (Eq, Show)

--------------------------------------------------------------------
-- StoreAccount

data StoreAccount = StoreAccount 
  {
     fieldAccountId :: String,
     fieldAccountName :: String ,
     fieldAccountPassword :: String
  }
  deriving(Eq)

instance Show StoreAccount where
    show StoreAccount{fieldAccountId=accountId,fieldAccountName=accountName,fieldAccountPassword=accountPassword} = 
                       Utils.listToSpacedString ["Account ID:", accountId, "| Name:", accountName, "| Password:", accountPassword]


data StoreProductList = StoreProductList [StoreProduct]

instance Show StoreProductList where
    show (StoreProductList productList ) =  if hasProducts then "Products:\n\n" ++  productListToString else "No products" where
         productListToString = concat [ show product' ++ "\n" | product' <- productList ]
         hasProducts  = (length productList) > 0



--------------------------------------------------------------------
-- StoreProduct         

data StoreProduct = StoreProduct 
  {
     fieldProductId :: String ,
     fieldProductName :: String,
     fieldProductPrice :: Double,
     fieldProductDeprecated :: Bool
  }

instance Show StoreProduct where
    show product' =     id  ( fieldProductName product')  ++ " - $" ++  id (show $ fieldProductPrice product') 
    
instance Eq StoreProduct where
    (==) leftProduct rightProduct = ( fieldProductId leftProduct ) == ( fieldProductId rightProduct )

instance Ord StoreProduct where
    compare leftProduct rightProduct  = compare ( fieldProductName leftProduct) ( fieldProductName rightProduct)
    (>) leftProduct rightProduct = ( fieldProductName leftProduct)  > ( fieldProductName rightProduct)

--------------------------------------------------------------------
-- StoreOrder


data StoreOrder = StoreOrder
  {
    fieldOrderId :: String,
    fieldOrderAccountId :: String,
    fieldOrderDate :: String,
    fieldOrderSum :: String,
    fieldOrderOverview :: String
  }

instance Show StoreOrder where
  show StoreOrder{fieldOrderId=orderId,fieldOrderDate=orderDate,fieldOrderAccountId=accountId,fieldOrderOverview=overview} =
    "Order #" ++ orderId ++ " ( " ++orderDate ++  " )\n\n" ++ "Account: " ++ accountId ++ "\n\n" ++ overview
 


--------------------------------------------------------------------
-- ShoppingCartData 


data StoreShoppingCartData = StoreShoppingCartData
  {
    fieldShoppingCartItems :: [StoreShoppingCartItemData]
  }
  deriving (Eq)
    

instance Show StoreShoppingCartData where
    show shoppingCartData =  if hasShoppingCartItems then "Products:\n" ++ shoppingCartItemsToString else "Shopping cart is empty"
        where
         shoppingCartItemsToString = concat [ ( show shoppingCartItem ) ++ "\n" | shoppingCartItem <- shoppingCartItems' ]
         shoppingCartItems' = fieldShoppingCartItems shoppingCartData
         hasShoppingCartItems = length shoppingCartItems' > 0
       
--------------------------------------------------------------------
-- StoreShoppingCartItemData 

data StoreShoppingCartItemData = StoreShoppingCartItemData
  {
    fieldCartProduct :: StoreProduct,
    fieldCartProductAmount :: Int
  }
  deriving (Eq)

instance Show StoreShoppingCartItemData where
    show shoppingCartItem = show (fieldCartProductAmount shoppingCartItem) ++ " x " ++ show (fieldCartProduct shoppingCartItem)



--------------------------------------------------------------------
-- StoreCheckoutOverviewData 


data StoreCheckoutOverviewData = StoreCheckoutOverviewData
  {
    fieldCheckoutShoppingCartItems :: [StoreCheckoutShoppingCartItemData]
  }
  deriving (Eq)

instance Show StoreCheckoutOverviewData where
    show StoreCheckoutOverviewData{fieldCheckoutShoppingCartItems=shoppingCartItems'} = 
      if not $ null shoppingCartItems'
      then "Products in cart:\n\n" ++ toText ++ "\nGrand total: $ " ++ totalSum
      else "No items in shopping cart"
      where
        toText =  unlines [ show checkoutShoppingCartItemData | checkoutShoppingCartItemData <- shoppingCartItems']
        totalSum = (show $ getTotalSumForProducts shoppingCartItems')

--------------------------------------------------------------------
-- StoreCheckoutShoppingCartItemData 


data StoreCheckoutShoppingCartItemData = StoreCheckoutShoppingCartItemData
  {
    fieldStoreShoppingCartItemData :: StoreShoppingCartItemData,
    fieldTotalPriceSum :: Double
  }
  deriving (Eq)
instance Show StoreCheckoutShoppingCartItemData where
    show StoreCheckoutShoppingCartItemData{fieldStoreShoppingCartItemData=productData,fieldTotalPriceSum=totalPriceSum'}  =  toText
      where
        product' = fieldCartProduct productData
        productName' = fieldProductName product'
        proudctPrice' = show $ fieldProductPrice product'
        productAmount = show $ fieldCartProductAmount productData
        totalPriceSum'' = show $ totalPriceSum'
        toText  = productName'  ++ ":\n-- "
                  ++ productAmount ++ " x $" ++ proudctPrice' ++ " = $ " ++ totalPriceSum''



--------------------------------------------------------------------
-- getter helper functions

getAccountFromSession :: StoreSession -> StoreAccount
getAccountFromSession session = fieldSessionAccountData session

getAccountUserNameFromSession :: StoreSession -> String
getAccountUserNameFromSession session = fieldAccountName $ getAccountFromSession session

getAccountIdFromSession :: StoreSession -> String
getAccountIdFromSession session = fieldAccountId $ getAccountFromSession session


getProductId :: StoreProduct -> String
getProductId product' = fieldProductId product'


getShoppingCartData :: StoreSession -> StoreShoppingCartData
getShoppingCartData storeSession = fieldSessionShoppingCartData storeSession

getShoppingCartDataItems :: StoreSession -> [StoreShoppingCartItemData]
getShoppingCartDataItems storeSession = fieldShoppingCartItems $ fieldSessionShoppingCartData storeSession

getTotalPriceForProduct :: StoreShoppingCartItemData -> Double
getTotalPriceForProduct StoreShoppingCartItemData{fieldCartProduct=product',fieldCartProductAmount=amount} = 
  (fromIntegral amount) * ( fieldProductPrice product')

getTotalSumForProducts :: [StoreCheckoutShoppingCartItemData] -> Double
getTotalSumForProducts shoppingCartItems =  foldr (+) 0 [ fieldTotalPriceSum checkoutShoppingCartItemData | checkoutShoppingCartItemData <- shoppingCartItems]

getTotalSumForProductsInSession :: StoreSession -> Double
getTotalSumForProductsInSession storeSession = getTotalSumForProducts $ fieldCheckoutShoppingCartItems $  evaluateShoppingCartDataToCheckoutOverview $ fieldSessionShoppingCartData storeSession

evaluateShoppingCartDataToCheckoutOverview :: StoreShoppingCartData -> StoreCheckoutOverviewData
evaluateShoppingCartDataToCheckoutOverview shoppingCartData = StoreCheckoutOverviewData [ evaluateShoppingCartDataItemToEvaluatedDataItem shoppingCartItem  | shoppingCartItem <- fieldShoppingCartItems shoppingCartData ]

evaluateShoppingCartDataItemToEvaluatedDataItem :: StoreShoppingCartItemData -> StoreCheckoutShoppingCartItemData
evaluateShoppingCartDataItemToEvaluatedDataItem shoppingCartItemData = StoreCheckoutShoppingCartItemData shoppingCartItemData (totalPriceSum $ shoppingCartItemData)
    where
      totalPriceSum StoreShoppingCartItemData{fieldCartProduct=product',fieldCartProductAmount=amount} = 
                                                                               (fromIntegral amount) * (fieldProductPrice product') 