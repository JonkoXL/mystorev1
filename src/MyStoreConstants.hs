{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module MyStoreConstants where

string_Main_Menu_Select_Prompt :: String
string_Main_Menu_Select_Prompt =  "Please select the relevant menu option or type '9' to exit the program." 

string_Main_Menu_Option_ShowContentShoppingCart :: String
string_Main_Menu_Option_ShowContentShoppingCart = "Show contents of your shopping cart"

string_Main_Menu_Option_ShowAvailableProducts :: String
string_Main_Menu_Option_ShowAvailableProducts = "Browse the list of available products"

string_Main_Menu_Option_SearchProductByName :: String
string_Main_Menu_Option_SearchProductByName = "Search for a specific product by name"

string_Main_Menu_Option_Checkout :: String
string_Main_Menu_Option_Checkout = "Checkout"

string_Main_Menu_Option_ViewOrders :: String
string_Main_Menu_Option_ViewOrders = "View orders"

string_Main_Menu_Option_ChangeAccountPassword :: String
string_Main_Menu_Option_ChangeAccountPassword = "Change account password"

string_Main_Menu_Option_AdminMenu :: String
string_Main_Menu_Option_AdminMenu = "Admin menu"

string_Main_Menu_Option_About :: String
string_Main_Menu_Option_About = "About"

string_Main_Menu_Option_Exit :: String
string_Main_Menu_Option_Exit = "Exit"



string_New_Account_Title :: String
string_New_Account_Title = "[New Account]"

string_New_Account_Username:: String
string_New_Account_Username = "Enter a username"

string_New_Account_Password:: String
string_New_Account_Password = "Enter a password"

string_New_Account_Error :: String
string_New_Account_Error =  "Could not create a new account, please contact Support!"

string_New_Account_Error_UsernameAlreadyExists :: String
string_New_Account_Error_UsernameAlreadyExists = "An account with the given name already exists, please choose another name"

string_New_Account_WelcomeText_One :: String
string_New_Account_WelcomeText_One = "Welcome "

string_New_Account_WelcomeText_Two :: String
string_New_Account_WelcomeText_Two = "Press any key and then log in with the following information your provided!"

string_ShoppingCartOverview_Title :: String
string_ShoppingCartOverview_Title = "[Shopping cart overview]"


string_BrowseProducts_Title :: String
string_BrowseProducts_Title = "[Browse products]"

string_BrowseProducts_Prompt :: String
string_BrowseProducts_Prompt = "Please select the relevant product that you would like to order or type '0' to exit"

string_Checkout_Title :: String
string_Checkout_Title = "[Checkout]"

string_Checkout_Prompt :: String
string_Checkout_Prompt = "Please select the relevant option or type '0' to return to the main menu."

string_Checkout_Option_Checkout :: String
string_Checkout_Option_Checkout = "Confirm checkout"

string_Checkout_Option_ChangeQuantity :: String
string_Checkout_Option_ChangeQuantity = "Edit amount of / remove specific product in shopping cart"

string_Checkout_Option_Exit :: String
string_Checkout_Option_Exit = "Return to main menu"

string_Checkout_Error_NoProductsInCart :: String
string_Checkout_Error_NoProductsInCart = "No products in cart yet, please add a product to the cart before checking out."

string_Checkout_Error_OrderNotSaved :: String
string_Checkout_Error_OrderNotSaved = "Checkout failed, order not submitted.\nPlease contact support."

string_Checkout_Error_InvalidOption :: String
string_Checkout_Error_InvalidOption = "Invalid option."

string_Checkout_Success :: String
string_Checkout_Success = "Checkout successful, thank you for doing business!"

string_Checkout_Edit_Cart_Success :: String
string_Checkout_Edit_Cart_Success = "Succesfully changed your shopping cart"

string_Checkout_Edit_Cart_Title :: String
string_Checkout_Edit_Cart_Title = "[Checkout - Edit shopping cart]"

string_Checkout_Edit_Cart_Prompt :: String
string_Checkout_Edit_Cart_Prompt = "Which product would you like to edit the amount to order for?"


string_AddProductToCart_Title :: String
string_AddProductToCart_Title = "[Select a product]"

string_AddProductToCart_NoProductsFound :: String
string_AddProductToCart_NoProductsFound = "No products matched your selection."


string_Account_ChangePassword_Error_AccountNotSaved :: String
string_Account_ChangePassword_Error_AccountNotSaved = "Something went wrong, trying to restore the old account..."

string_Account_ChangePassword_Error_OldAccountCouldNotBeRestotred:: String
string_Account_ChangePassword_Error_OldAccountCouldNotBeRestotred = "Could not restore the old account, please contact Support with your account information."

string_Account_ChangePassword_Success :: String
string_Account_ChangePassword_Success = "Password succesfully changed..."


string_Admin_Menu_Title :: String
string_Admin_Menu_Title = "[Admin panel]"

string_Admin_Menu_Prompt :: String
string_Admin_Menu_Prompt = "Please select the relevant option, or type '0' to exit."

string_Admin_Menu_Option_ViewAccounts :: String
string_Admin_Menu_Option_ViewAccounts = "View accounts"

string_Admin_Menu_Option_ViewOrders :: String
string_Admin_Menu_Option_ViewOrders = "View orders"

string_Admin_Menu_Option_ManageProducts :: String
string_Admin_Menu_Option_ManageProducts = "Manage products"


string_Admin_ViewAccounts_Menu_Title :: String
string_Admin_ViewAccounts_Menu_Title = "[Select an account]"

string_Admin_ViewAccounts_Menu_Prompt :: String
string_Admin_ViewAccounts_Menu_Prompt = "Please select the relevant account or type '0' to exit" 

string_Admin_ViewAccounts_Error_CouldNotRetrieve :: String
string_Admin_ViewAccounts_Error_CouldNotRetrieve = "An error occured retrieving the accounts.\nPlease contact support"

string_Admin_ViewAccounts_Error_NoAccountsFound:: String
string_Admin_ViewAccounts_Error_NoAccountsFound = "No accounts were found."


string_Admin_ProductManager_Menu_Title :: String
string_Admin_ProductManager_Menu_Title = "[Product manager]"

string_Admin_ProductManager_Menu_Prompt :: String
string_Admin_ProductManager_Menu_Prompt = "Please select the relevant option or type '0' to exit" 

string_Admin_ProductManager_Menu_Option_ViewProducts :: String
string_Admin_ProductManager_Menu_Option_ViewProducts = "View products"

string_Admin_ProductManager_Menu_Option_AddProduct :: String
string_Admin_ProductManager_Menu_Option_AddProduct = "Add product"

string_Admin_ProductManager_Menu_Option_EditProduct :: String
string_Admin_ProductManager_Menu_Option_EditProduct = "Edit product"

string_Admin_ProductManager_Menu_Option_ToggleDeprecation :: String
string_Admin_ProductManager_Menu_Option_ToggleDeprecation = "Toggle deprecation of product"


string_Admin_SelectProduct_Menu_Title :: String
string_Admin_SelectProduct_Menu_Title = "[Product manager - Select a product]"

string_Admin_SelectProduct_Menu_Prompt :: String
string_Admin_SelectProduct_Menu_Prompt = "Please select the relevant product or type '0' to exit" 

string_OrderOverview_Menu_Error_CouldNotRetrieve :: String
string_OrderOverview_Menu_Error_CouldNotRetrieve = "An error occured retrieving the orders.\nPlease contact support"

string_OrderOverview_Menu_Error_NoOrdersFound :: String
string_OrderOverview_Menu_Error_NoOrdersFound = "No orders were found."

string_OrderOverview_Menu_Title :: String
string_OrderOverview_Menu_Title = "[Select an order]"

string_OrderOverview_Menu_Prompt :: String
string_OrderOverview_Menu_Prompt = "Please select which order you want to view or type '0' to exit." 

string_Product_Prompt_PleaseEnter :: String
string_Product_Prompt_PleaseEnter = "Please enter the "

string_Product_Prompt_Error :: String
string_Product_Prompt_Error = "Failed to create a product with the provided data, please try again or contact support"

string_Product_Prompt_Success :: String
string_Product_Prompt_Success = "Product added:"

string_Product_Edit_Prompt_Prefix :: String
string_Product_Edit_Prompt_Prefix = "Exiting Product:"

string_Product_Edit_Prompt :: String
string_Product_Edit_Prompt = "Please enter a new "

string_Product_Edit_New_Name :: String
string_Product_Edit_New_Name = "new name: "

string_Product_Edit_Error_ProductNotSaved :: String
string_Product_Edit_Error_ProductNotSaved =  "Failed to updated the product with the provided data, please try again or contact support"

string_Product_Edit_Success :: String
string_Product_Edit_Success = "Product updated:"

string_Admin_Credentials_Prompt :: String
string_Admin_Credentials_Prompt = "Please enter the administrator password:"

string_Admin_Credentials_Error_PasswordNotFound :: String
string_Admin_Credentials_Error_PasswordNotFound = "There was an error determining the administrator password.\nPlease contact support."

string_Admin_Credentials_Error_PasswordInputMismatch :: String
string_Admin_Credentials_Error_PasswordInputMismatch = "The entered password does not match the administrator password."



--------------------------------------------------------------------
-- Misc

string_Application_Exit :: String
string_Application_Exit = "Thank you, goodbye!"

string_Application_Menu :: String
string_Application_Menu = "Welcome to this application"

string_Application_Title :: String
string_Application_Title = "[MyStore v1.0]"

string_Application_About :: String
string_Application_About = "Thank you for using the 'MyStore' application.\n\nThis application uses a custom made proprietary file database system called 'FDatabase'.\n\nMyStore & FDatabase are Haskell products made by JonkoXL (c) 2023.\n\nAll rights reserved.\n\nFor education purposes only, please do not redistribute this project."

string_Error_NoCharacterEntered :: String
string_Error_NoCharacterEntered = "Please enter a valid entry"

string_Error_Generic_Error :: String
string_Error_Generic_Error = "A generic error occured!"

string_Error_Invalid_Username_Password :: String
string_Error_Invalid_Username_Password = "Invalid username / password combination, try again..."

string_Text_Done :: String
string_Text_Done = "Done"

string_Text_Exclamation_Mark :: String
string_Text_Exclamation_Mark = "!"

string_Text_No_Products_Found_For_Term :: String
string_Text_No_Products_Found_For_Term = "No products were found for term: "

string_Text_ProductList_DisplayText :: String
string_Text_ProductList_DisplayText = "This is the list of products that can be ordered.."

string_Text_Your_Username :: String
string_Text_Your_Username = "Your current username is " 

string_Text_Welcome :: String
string_Text_Welcome = "Welcome "

string_Other_ClearScreenString :: String
string_Other_ClearScreenString = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

string_Prompt_How_Many_Products_To_Order :: String
string_Prompt_How_Many_Products_To_Order = "How many would you like to order in total?"

string_Prompt_Option :: String
string_Prompt_Option = "Option: "

string_Prompt_New_Password :: String
string_Prompt_New_Password = "Please enter your new password:"

string_Prompt_Password :: String
string_Prompt_Password = "Please enter your password:"

string_Prompt_Product_Name :: String
string_Prompt_Product_Name = "Enter the name you would like to search the product for:"

string_Prompt_PressAnyKey :: String
string_Prompt_PressAnyKey = "(Press any key to continue...)"

string_Prompt_Select_Product :: String
string_Prompt_Select_Product = "Please enter the corresponding number option of the product, or '0' to leave this menu"

string_Prompt_Username :: String
string_Prompt_Username = "Please log in with your username, or type 'new' to create a new account"

string_Warning_ProductAlreadyInCart_One :: String
string_Warning_ProductAlreadyInCart_One = "Note: You have "

string_Warning_ProductAlreadyInCart_Two :: String
string_Warning_ProductAlreadyInCart_Two = "in your cart."


