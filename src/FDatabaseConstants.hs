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


module FDatabaseConstants where

import           Prelude


string_Application_Exit :: String
string_Application_Exit = "Thank you, goodbye!"

string_Application_Menu :: String
string_Application_Menu = "Welcome to this application"

string_Application_Title :: String
string_Application_Title = "[MyStore v1.0]"

string_Error_NoCharacterEntered :: String
string_Error_NoCharacterEntered = "Please enter a valid entry"

string_Error_Generic_Error :: String
string_Error_Generic_Error = "A generic error occured!"

string_Error_File_Does_Not_Exist :: String
string_Error_File_Does_Not_Exist = "Error: File does not exist!"


string_Menu_Main_Menu :: String
string_Menu_Main_Menu = "Please make your choice from the following options\n"
                        ++ "1) ? \n"
                        ++ "2) Do Test\n"
                        ++ "9) Exit\n\n"

string_Other_ClearScreenString :: String
string_Other_ClearScreenString = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

string_Prompt_How_Many_Products_To_Order :: String
string_Prompt_How_Many_Products_To_Order = "How many would you like to order in total?"

string_Prompt_Option :: String
string_Prompt_Option = "Option: "

string_Prompt_New_Password:: String
string_Prompt_New_Password = "Please enter your new password:"

string_Prompt_Product_Name :: String
string_Prompt_Product_Name = "Enter the name you would like to search the product for:"

string_Prompt_PressAnyKey :: String
string_Prompt_PressAnyKey = "Press any key to continue..."

string_Prompt_Select_Product :: String
string_Prompt_Select_Product = "Please enter the 'option code' of the product, or '0' to leave this menu\n\n0) 'Return to main menu'"

string_Prompt_Username :: String
string_Prompt_Username = "Please log in with a username, or type 'new' to create a new account"



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

