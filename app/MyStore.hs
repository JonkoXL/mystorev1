{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8 as BS8
import           Data.Char
import           Data.Either           as Either
import           Data.Foldable         as Fold
import           Data.Maybe
import           Data.List
import           Data.String
import qualified Data.String           as FS
import           Data.Typeable
import           Data.Void
import           Prelude
import           System.Directory
import           System.Environment (getArgs)
import           System.Exit

import           qualified FDatabaseData          as FD
import           qualified FDatabaseUtils         as FU

import           qualified MyStoreConstants       as Constants
import           qualified MyStoreData            as MSD
import           qualified MyStoreLogic           as MSL
import           qualified MyStoreUtils           as Utils

--------------------------------------------------------------------
-- Main, Login menu. Main Menu & About

main :: IO ()
main = do
  arguments <- getArgs

  printLoginMenu

  return ()

printLoginMenu :: IO ()
printLoginMenu = do
  
  userName <- Utils.clearScreenAndPromptLine $ Constants.string_Application_Title ++ "\n\n"  ++ Constants.string_Prompt_Username
  case userName of
            "new" -> do
                      createNewAccountMenu
                      printLoginMenu
            _     -> do
                      password <- Utils.clearScreenAndPromptLine Constants.string_Prompt_Password
                      queriedAccount <- MSL.getAccountForUsernameAndPassword userName password
                      case queriedAccount of
                        Just queriedAccount'  -> do
                                                  let session = MSL.createSession queriedAccount' (MSD.StoreShoppingCartData [])
                                                  printMainMenu session
                                                  return ()

                        Nothing               -> do
                                                  Utils.clearScreenPrintLineAndPromptEnter Constants.string_Error_Invalid_Username_Password
                                                  printLoginMenu
                                                  return ()


createNewAccountMenu :: IO ()
createNewAccountMenu = do

  accountName <- Utils.clearScreenAndPromptLine $ Constants.string_New_Account_Title ++ "\n\n" ++ Constants.string_New_Account_Username
  existingAccount <-MSL.getAccountForUsername accountName 
  case existingAccount of
    Just existingAccount' -> do
       Utils.printLineAndPromptEnterKey $ Constants.string_New_Account_Error_UsernameAlreadyExists
       createNewAccountMenu
    Nothing -> do
      accountPassword <- Utils.promptLine Constants.string_New_Account_Password
      accountObject <- MSL.createAndSaveAccount accountName accountPassword
      
      case accountObject of
        Nothing             -> die $ Constants.string_New_Account_Error
        Just accountObject' -> do
                                MSL.writeAccountToDB accountObject'
                                Utils.clearScreenPrintLineAndPromptEnter $ "\n" ++ Constants.string_New_Account_WelcomeText_One ++ accountName ++ "!\n\n" ++ Constants.string_New_Account_WelcomeText_Two


printMainMenu :: MSD.StoreSession ->  IO MSD.StoreSession
printMainMenu storeSession = do

  let storeUserName = MSD.getAccountUserNameFromSession storeSession

  let mainMenuTitle = Constants.string_Application_Title ++ "\n\n" ++ Constants.string_Text_Welcome ++  (id storeUserName) ++ Constants.string_Text_Exclamation_Mark 
  let mainMenuPromptText = Constants.string_Main_Menu_Select_Prompt
  let mainMenuOptionsText = [Constants.string_Main_Menu_Option_ShowContentShoppingCart, Constants.string_Main_Menu_Option_ShowAvailableProducts, Constants.string_Main_Menu_Option_SearchProductByName ,Constants.string_Main_Menu_Option_Checkout, Constants.string_Main_Menu_Option_ViewOrders,Constants.string_Main_Menu_Option_ChangeAccountPassword,Constants.string_Main_Menu_Option_AdminMenu, Constants.string_Main_Menu_Option_About, Constants.string_Main_Menu_Option_Exit] 
  let mainMenuOptions = ["show shopping cart","show products","search product","checkout","view orders","change password","admin menu","about", "exit"]
  
  menuOption <- Utils.clearScreenAndPromptOptionMenu mainMenuTitle mainMenuPromptText mainMenuOptionsText mainMenuOptions
  
  updatedStoreSession <- case menuOption of
                            Just "show shopping cart" ->  showShoppingCartOverview storeSession
                            Just "show products"      ->  showProductOverview storeSession
                            Just "search product"     ->  searchProductsMenu storeSession
                            Just "checkout"           ->  checkoutMenu storeSession
                            Just "view orders"        ->  viewOrderMenuForSession storeSession
                            Just "change password"    ->  changePasswordMenu storeSession
                            Just "admin menu"         ->  checkCredentialsAndShowAdminMenu storeSession
                            Just "about"              ->  aboutMenu storeSession
                            Just "exit"               ->  die $ Constants.string_Other_ClearScreenString ++ Constants.string_Application_Exit
                            _                         ->  return storeSession

  printMainMenu updatedStoreSession

  return updatedStoreSession


aboutMenu :: MSD.StoreSession -> IO MSD.StoreSession
aboutMenu storeSession = do
  Utils.clearScreenPrintLineAndPromptEnter Constants.string_Application_About
  return storeSession

--------------------------------------------------------------------
-- Shopping cart overview


showShoppingCartOverview :: MSD.StoreSession -> IO MSD.StoreSession
showShoppingCartOverview storeSession = do

                  let shoppingCartOverview = MSL.getShoppingCartOverviewAsString storeSession False

                  Utils.clearScreenAndPrintLine $ Constants.string_ShoppingCartOverview_Title ++ " \n"
                  Utils.printLineAndPromptEnterKey $ shoppingCartOverview
                  return storeSession

showShoppingCartSelectionOverview :: MSD.StoreSession -> IO MSD.StoreSession
showShoppingCartSelectionOverview storeSession = do

                  let shoppingCartOverview = MSL.getShoppingCartOverviewAsString  storeSession True

                  Utils.clearScreenAndPrintLine $ Constants.string_ShoppingCartOverview_Title ++ " \n"
                  Utils.printLineAndPromptEnterKey $ shoppingCartOverview
                  return storeSession



--------------------------------------------------------------------
-- Show product overview


showProductOverview :: MSD.StoreSession ->  IO MSD.StoreSession
showProductOverview storeSession = do

  Utils.clearScreen
  productList <- MSL.getNonDeprecatedProductsFromDB  
  Utils.printLine $ "\n" ++ Constants.string_Text_ProductList_DisplayText ++ "\n"

  let productOverview2DList  =  Utils.prefixIndexTo2DStringList  [ [MSD.fieldProductName product, show $ MSD.fieldProductPrice product]     | product <- productList]  
  let productOverviewTable = Utils.createTableWithHeaderWithDefiniedColumnWidthForOptionMenu [24,10] ["Name","Price"] productOverview2DList

  selectedProduct <- Utils.clearScreenAndPromptOptionTableMenu Constants.string_BrowseProducts_Title Constants.string_BrowseProducts_Prompt productOverviewTable productList
  case selectedProduct of
                      Nothing               -> return storeSession                         
                      Just selectedProduct' -> do
                                                let shoppingCartData = MSD.getShoppingCartDataItems  storeSession
                                                updatedCartProducts <- addOrChangeProductIntoCart shoppingCartData selectedProduct'
                                                let updatedCartData = MSD.StoreShoppingCartData updatedCartProducts
                                                let updatedSession = storeSession { MSD.fieldSessionShoppingCartData = updatedCartData}
                                                showShoppingCartOverview updatedSession
                                                showProductOverview updatedSession



--------------------------------------------------------------------
-- Add product to cart menu

searchProductsMenu :: MSD.StoreSession -> IO MSD.StoreSession
searchProductsMenu storeSession = do

    Utils.clearScreen
    productList <- MSL.getNonDeprecatedProductsFromDB  
    let shoppingCartData = MSD.fieldSessionShoppingCartData storeSession

    Utils.printLine $ "\n" ++ Constants.string_Text_ProductList_DisplayText ++"\n"
    print productList

    updatedShoppingCartData <- findProductByNameMenu shoppingCartData

    let account = MSD.getAccountFromSession storeSession
    let storeUserName = MSD.fieldAccountName account
    let updatedSession = MSL.createSession account updatedShoppingCartData

    showShoppingCartOverview updatedSession
    return updatedSession


findProductByNameMenu :: MSD.StoreShoppingCartData -> IO MSD.StoreShoppingCartData
findProductByNameMenu cartData@(MSD.StoreShoppingCartData cartEntries) = do

   productList <- MSL.getNonDeprecatedProductsFromDB  
   queryName <- Utils.clearScreenAndPromptLine Constants.string_Prompt_Product_Name

   let matchedProducts = MSL.findProductsByName productList queryName

   if null matchedProducts
    then do
            Utils.printLine $ Constants.string_Text_No_Products_Found_For_Term ++ queryName 
            return $ cartData
    else do
            addToCartProducts <- addProductToCartMenu cartEntries matchedProducts
            return $ MSD.StoreShoppingCartData addToCartProducts


addProductToCartMenu :: [MSD.StoreShoppingCartItemData]  -> [MSD.StoreProduct] ->  IO [MSD.StoreShoppingCartItemData]
addProductToCartMenu cartEntries matchedProducts  = do
      
  --  let selectProductListStrings = Utils.objectListToStringList matchedProducts

    let productOverview2DList  =  Utils.prefixIndexTo2DStringList  [ [MSD.fieldProductName product, show $ MSD.fieldProductPrice product]     | product <- matchedProducts]  
    let productOverviewTable = Utils.createTableWithHeaderWithDefiniedColumnWidthForOptionMenu [24,10] ["Name","Price"] productOverview2DList
  
    selectedProduct <- Utils.clearScreenAndPromptOptionTableMenu Constants.string_AddProductToCart_Title Constants.string_BrowseProducts_Prompt productOverviewTable matchedProducts

   -- selectedProduct <- Utils.clearScreenAndPromptOptionMenu Constants.string_AddProductToCart_Title Constants.string_Prompt_Select_Product  selectProductListStrings matchedProducts 
 
    case selectedProduct of
        Nothing               -> return cartEntries
        Just selectedProduct' -> addOrChangeProductIntoCart cartEntries selectedProduct'
                          
    
addOrChangeProductIntoCart :: [MSD.StoreShoppingCartItemData] -> MSD.StoreProduct -> IO [MSD.StoreShoppingCartItemData]
addOrChangeProductIntoCart cartEntries selectedProduct'  = do

  let productName = MSD.fieldProductName selectedProduct'
  let amountOfProductInCart = MSL.findAmountOfProductsInShoppingCartForProduct cartEntries selectedProduct'
  let hasProductAlreaydInCart = amountOfProductInCart > 0
  let warningHasProductsInCartText = if hasProductAlreaydInCart then Constants.string_Warning_ProductAlreadyInCart_One ++ show amountOfProductInCart ++ " of product '" ++ productName ++ "' " ++ Constants.string_Warning_ProductAlreadyInCart_Two else "Product: " ++ productName ++ "\n"
  let promptTextAmountOfProduct =  warningHasProductsInCartText ++ "\n" ++ Constants.string_Prompt_How_Many_Products_To_Order
  promptProductAmount <- Utils.clearScreenAndPromptInt promptTextAmountOfProduct
  
  let newCartEntry = [MSD.StoreShoppingCartItemData selectedProduct' promptProductAmount]
  
  let normalizedCartData = if hasProductAlreaydInCart then MSL.excludeSpecifiedProductFromShoppingCart cartEntries selectedProduct' else cartEntries
  
  if promptProductAmount <= 0
    then return normalizedCartData

    else do
            let updatedCartData = newCartEntry ++ normalizedCartData
            return updatedCartData

--------------------------------------------------------------------
-- Checkout


checkoutMenu :: MSD.StoreSession ->  IO MSD.StoreSession
checkoutMenu storeSession = do

  let hasProductsInCart = length (MSD.fieldShoppingCartItems  $ MSD.fieldSessionShoppingCartData storeSession) > 0
  if not hasProductsInCart
    then do
      Utils.clearScreenPrintLineAndPromptEnter Constants.string_Checkout_Error_NoProductsInCart
      return storeSession
    else do
      let checkoutOverview = MSL.getShoppingCartOverviewAsString storeSession False

      let checkoutTitle = Constants.string_Checkout_Title ++ "\n\n" ++ checkoutOverview
      let checkoutPromptText = "\n" ++ Constants.string_Checkout_Prompt 
      let checkoutOptionsText = [Constants.string_Checkout_Option_Checkout,Constants.string_Checkout_Option_ChangeQuantity] 
      let checkoutOptions = ["checkout","edit"]

      checkoutOption <- Utils.clearScreenAndPromptOptionMenu checkoutTitle checkoutPromptText checkoutOptionsText checkoutOptions
      
      case checkoutOption of
        Just "checkout" -> do
                            order  <- MSL.performCheckout storeSession
                            case order of 
                                          Nothing       -> do
                                                              Utils.printLineAndPromptEnterKey Constants.string_Checkout_Error_OrderNotSaved
                                                              return storeSession
                                          Just order'   -> do 
                                                              let updatedStoreSession = storeSession { MSD.fieldSessionShoppingCartData =  MSD.StoreShoppingCartData []  }
                                                              Utils.clearScreenPrintLineAndPromptEnter $ Constants.string_Checkout_Success ++ "\n\n" ++ show order'
                                                              return updatedStoreSession
                          

        Just "edit"   -> do        
                            let shoppingCartEditMenuTitle = Constants.string_Checkout_Edit_Cart_Title 
                            let shoppingCartEditMenuPrompt = "---------\n" ++ Constants.string_Checkout_Edit_Cart_Prompt

                            let shoppingCartDataItems = MSD.getShoppingCartDataItems storeSession 

                            let cartDataAs2DList = Utils.prefixIndexTo2DStringList [  [ show $ MSD.fieldCartProductAmount shoppingCartData, MSD.fieldProductName  $ MSD.fieldCartProduct shoppingCartData, show $ MSD.fieldProductPrice $ MSD.fieldCartProduct shoppingCartData ,  show $ MSD.getTotalPriceForProduct shoppingCartData ]  |  shoppingCartData  <- shoppingCartDataItems]
                            let cartDataTable = Utils.createTableWithHeaderWithDefiniedColumnWidthForOptionMenu [10,16,10,10] ["Amount", "Name" ,"Price", "Sum"] cartDataAs2DList
                            
                            shoppingCartDataObject <- Utils.clearScreenAndPromptOptionTableMenu shoppingCartEditMenuTitle shoppingCartEditMenuPrompt cartDataTable shoppingCartDataItems
                          
                            case shoppingCartDataObject of

                              Nothing                      ->   do
                                                                Utils.printLineAndPromptEnterKey Constants.string_Checkout_Error_InvalidOption
                                                                return storeSession

                              Just shoppingCartDataObject' ->  do
                                                                updatedShoppingCartItems <- addOrChangeProductIntoCart shoppingCartDataItems (MSD.fieldCartProduct shoppingCartDataObject')
                                                                let updatedStoreSession =  storeSession { MSD.fieldSessionShoppingCartData =  MSD.StoreShoppingCartData updatedShoppingCartItems  }

                                                                showShoppingCartOverview updatedStoreSession 
                                                                checkoutMenu updatedStoreSession

        _ -> return storeSession


--------------------------------------------------------------------
-- Order overview for Account

viewOrderMenuForSession :: MSD.StoreSession -> IO MSD.StoreSession
viewOrderMenuForSession storeSession = do
  orders <- MSL.getOrdersFromDBForSession storeSession
  viewOrderMenu orders
  return storeSession



viewOrderMenu :: Maybe [MSD.StoreOrder] ->  IO ()
viewOrderMenu orders = do
        orders' <- Utils.getListOrPrintTryingWithEmptyList orders Constants.string_OrderOverview_Menu_Error_CouldNotRetrieve Constants.string_OrderOverview_Menu_Error_NoOrdersFound
        if Utils.isEmptyList orders'
          then return ()
          else do
            
            let dateDescendingOrders = reverse orders'

            let ordersAs2DStringList = Utils.prefixIndexTo2DStringList [  [ MSD.fieldOrderDate order, MSD.fieldOrderId order, MSD.fieldOrderSum order ]  |  order  <- dateDescendingOrders]
            let orderTable = Utils.createTableWithHeaderWithDefiniedColumnWidthForOptionMenu [16,10,20] ["Date", "Order ID" ,"Sum"] ordersAs2DStringList

            selectedOrder <- Utils.clearScreenAndPromptOptionTableMenu Constants.string_OrderOverview_Menu_Title Constants.string_OrderOverview_Menu_Prompt orderTable dateDescendingOrders
            case selectedOrder of
                                Nothing             -> return ()                           
                                Just selectedOrder' -> do
                                                        Utils.clearScreenPrintLineAndPromptEnter $ show selectedOrder'
                                                        viewOrderMenu orders


--------------------------------------------------------------------
-- Change password


changePasswordMenu :: MSD.StoreSession ->  IO MSD.StoreSession
changePasswordMenu storeSession = do

  newPassword <- Utils.clearScreenAndPromptLine Constants.string_Prompt_New_Password

  let account = MSD.getAccountFromSession storeSession
  let newAccountData =  account { MSD.fieldAccountPassword = newPassword }
  newAccountData' <- MSL.writeAccountToDB newAccountData
  
  case newAccountData' of

      Just newAccountData'' ->  do
                                  return $ storeSession { MSD.fieldSessionAccountData =  newAccountData'' }
      Nothing               ->  do
                                  Utils.printLineAndPromptEnterKey Constants.string_Account_ChangePassword_Error_AccountNotSaved
                                  restoredAccount' <- MSL.writeAccountToDB account
                                  case restoredAccount' of
                                          Nothing                 -> do
                                                                      die Constants.string_Account_ChangePassword_Error_OldAccountCouldNotBeRestotred
                                                                      return storeSession
                                          Just restoredAccount''  -> do
                                                                      Utils.printLineAndPromptEnterKey Constants.string_Account_ChangePassword_Success
                                                                      return $ storeSession { MSD.fieldSessionAccountData =  restoredAccount'' }


--------------------------------------------------------------------
-- Admin Menu
  
checkCredentialsAndShowAdminMenu :: MSD.StoreSession -> IO MSD.StoreSession
checkCredentialsAndShowAdminMenu storeSession = do   
  validAdminCredentials <- MSL.checkAdminCredentials
  if validAdminCredentials
    then do
      showAdminMenu
      return storeSession
    else return storeSession

showAdminMenu :: IO ()
showAdminMenu = do

  let adminMenuOptionsText = [Constants.string_Admin_Menu_Option_ViewAccounts, Constants.string_Admin_Menu_Option_ViewOrders, Constants.string_Admin_Menu_Option_ManageProducts]
  let adminMenuOptions = ["view accounts", "view orders","manage products"]

  selectedAdminOption <- Utils.clearScreenAndPromptOptionMenu Constants.string_Admin_Menu_Title Constants.string_Admin_Menu_Prompt  adminMenuOptionsText adminMenuOptions

  case selectedAdminOption of
                              Just "view accounts" -> do
                                                    viewAccounts
                                                    showAdminMenu
                              Just "view orders" -> do
                                                    viewOrderMenuForAdmin
                                                    showAdminMenu
                              Just "manage products" ->  do
                                                    productManagerMenu
                                                    showAdminMenu
                              _ -> return ()

--------------------------------------------------------------------
-- Admin Account Menu

viewAccounts :: IO ()
viewAccounts = do
  
  accounts <- MSL.getAccountsFromDBNode
  accounts' <- Utils.getListOrPrintTryingWithEmptyList accounts Constants.string_Admin_ViewAccounts_Error_CouldNotRetrieve Constants.string_Admin_ViewAccounts_Error_NoAccountsFound
  
  if Utils.isEmptyList accounts'
    then return ()
    else do

      let accountsAs2DStringList = Utils.prefixIndexTo2DStringList [  [ MSD.fieldAccountId account, MSD.fieldAccountName account, MSD.fieldAccountPassword account ]  |  account  <- accounts']
      let accountTable = Utils.createTableWithHeaderWithDefiniedColumnWidthForOptionMenu [16,10,20] ["Account ID", "Name" ,"Password"] accountsAs2DStringList
      
      selectedAccount <- Utils.clearScreenAndPromptOptionTableMenu  Constants.string_Admin_ViewAccounts_Menu_Title  Constants.string_Admin_ViewAccounts_Menu_Prompt accountTable accounts'
      
      case selectedAccount of
        Just selectedAccount' -> do 
                                  Utils.clearScreenPrintLineAndPromptEnter $ show selectedAccount'
                                  viewAccounts
                                  return ()

        Nothing               ->  return ()


--------------------------------------------------------------------
-- Admin Order Menun

viewOrderMenuForAdmin :: IO ()
viewOrderMenuForAdmin = do
    orders <- MSL.getOrdersFromDB
    viewOrderMenu orders
    return ()

--------------------------------------------------------------------
-- Admin Product Manager Menu


productManagerMenu :: IO ()
productManagerMenu = do

  let productManagerMenuOptionsText = [Constants.string_Admin_ProductManager_Menu_Option_ViewProducts, Constants.string_Admin_ProductManager_Menu_Option_AddProduct, Constants.string_Admin_ProductManager_Menu_Option_EditProduct, Constants.string_Admin_ProductManager_Menu_Option_ToggleDeprecation]
  let productManagerMenuOptions = ["view products" , "add product", "edit product","toggle deprecation"]
  
  productManagerOption <- Utils.clearScreenAndPromptOptionMenu Constants.string_Admin_ProductManager_Menu_Title  Constants.string_Admin_ProductManager_Menu_Prompt productManagerMenuOptionsText productManagerMenuOptions  
  
  case productManagerOption of
    Just "view products"      -> do
                                  viewProductsMenuForAdmin
                                  productManagerMenu
    Just "add product"        -> do
                                  MSL.createProductFromPrompt
                                  productManagerMenu
    Just "edit product"       -> do
                                  editSelectProductMenu
                                  productManagerMenu
    Just "toggle deprecation" -> do
                                  toggleDeprecationMenu
                                  productManagerMenu   
    _                         -> do
                                  return ()      





viewProductsMenuForAdmin :: IO ()
viewProductsMenuForAdmin = do

  products <- MSL.getProductsFromDB
  let productTable = Utils.createTableWithHeaderWithDefiniedColumnWidthForOptionMenuAsString [12,10,12,10] ["Name", "Price" ,"ID", "Status"] (MSL.createProductOverviewAs2DStringList products)
 
  Utils.clearScreenPrintLineAndPromptEnter productTable
  return ()




editSelectProductMenu :: IO ()
editSelectProductMenu = do  

  selectedProduct <- promptSelectProductForAdmin
  
  case selectedProduct of

    Just selectedProduct' -> do
                              editedProduct  <- MSL.editProduct selectedProduct'
                              editSelectProductMenu  
                              return ()  
    Nothing               -> return ()


toggleDeprecationMenu :: IO ()
toggleDeprecationMenu = do

  selectedProduct <- promptSelectProductForAdmin
  case selectedProduct of

    Just selectedProduct' -> do
                                let newDeprecatedValue = not $ MSD.fieldProductDeprecated selectedProduct'
                                let deprecatedToggledProduct = selectedProduct' { MSD.fieldProductDeprecated = newDeprecatedValue }
                                MSL.writeProductToDB deprecatedToggledProduct
                                toggleDeprecationMenu
                                return ()

    Nothing               ->    return ()


  

promptSelectProductForAdmin :: IO (Maybe (MSD.StoreProduct))
promptSelectProductForAdmin = do

  products <- MSL.getProductsFromDB
  let productsOverview =  Utils.prefixIndexTo2DStringList  $ (MSL.createProductOverviewAs2DStringList products)
  let productTable =  Utils.createTableWithHeaderWithDefiniedColumnWidthForOptionMenu [20,10,12,10] ["Name", "Price" ,"ID", "Status"] productsOverview
  Utils.clearScreenAndPromptOptionTableMenu Constants.string_Admin_SelectProduct_Menu_Title Constants.string_Admin_SelectProduct_Menu_Prompt productTable products
