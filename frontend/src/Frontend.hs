{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Frontend where

import Control.Lens
import Control.Monad
import qualified Data.Text as T

import Reflex.Dom

import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Common.Route
import Common.Smt
import Common.Grammar

import qualified GHC.Generics as G
import qualified Data.Aeson as A
import qualified Data.Text.Encoding as E

-- "MAIN function"
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { -- HTML <head> tag contents
    _frontend_head = do
      -- title of the tab in the browser
      el "title" $ text "Slicing Example"

      -- add stylesheet static/main.css to website
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank

      -- add d3js to website
      elAttr "script" ("src" =: "https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.17/d3.min.js") blank

      -- add static/d3prep.js to website
      elAttr "script" ("src" =: static @"d3prep.js" <> "type" =: "text/javascript") blank

  , -- HTML <body> tag contents
    _frontend_body = do
      -- header/title of the website
      elAttr "div" ("id" =: "header") $ text "Slicing Example"

      -- the code area on the left side
      programText <- textAreaElement $
                        def & initialAttributes .~ ("id" =: "codeMirror")

      -- the empty diagram placeholder on the right side
      elAttr "diagram" ("id" =: "diagram") blank

      -- set code area to initial text 
      executeJS "document.getElementById(\"codeMirror\").value=JSON.stringify(sampleTreeData)"

      -- listens for changes in programText and invokes showProgram (updating the tree)
      void $ dyn (showProgram <$> (value $ programText))
  }

-- helper function to execute javascript clientside
executeJS :: (Prerender js t m, Monad m) => T.Text -> m ()
executeJS t = prerender_ blank $ liftJSM $ void $ eval t

-- function that takes the input from the text area, transforms it and updates the tree
-- TODO: use `t` to parse and construct the tree to be shown
-- use the parse and convertToTree methods for this
showProgram :: (Prerender js t m, Monad m) => T.Text -> m ()
showProgram t = do


    -- filter for valid programs
    when (T.length t > 0) $


        -- this tries to parse and convert the program text or shows the resulting error.
        -- NOTE: `convertToTree` MUST produce a valid json otherwise there will be a parsing error
        -- breaking the application
        {-executeJS $ "try { showTree(" <> (convertToTree . parse $ t) <> "); }\
                    \catch (e) {console.log(\"error\",e);}"-}
        executeJS $ "try { showTree(" <> (convertToTree . parse $ t) <> "); }\
                    \catch (e) {console.log(\"error\",e);}"
  where
      parse t' = t'
        -- E.decodeUtf8  $ A.encode $  Node "asf" Nothing Nothing
      convertToTree t' = t'

data GraphNode  = Node { name :: T.Text
                      , parent :: Maybe T.Text
                      , children :: Maybe [GraphNode] }
  deriving (Show,Eq,G.Generic)

instance A.FromJSON GraphNode
instance A.ToJSON GraphNode


convertToJSON :: ArithExpr -> GraphNode
convertToJSON  AHole = Node "AHole" Nothing Nothing







        -- E.decodeUtf8  $ encode $  Node "asf" Nothing Nothing
      -- E.decodeUtf8 $ encode $ convertToJSON AHole
        -- exprToString "null" (Add AHole AHole)
        -- "[{\"name\":\"Top Level\",\"parent\":\"null\"}]"
        --"hallo" -- strToProp smtProg





exprToString :: String -> ArithExpr -> T.Text
exprToString parent AHole = 
  "[{\"name\":\"Top Level: " <> T.pack "AHole" <> " \",\"parent\":\"null\"}]"
exprToString parent (Num i) = 
  "[{\"name\":\"Top Level: " <> T.pack (show i)  <> " \",\"parent\":\"null\"}]"
exprToString parent (Var v) = 
  "[{\"name\":\"Top Level: " <> T.pack v  <> " \",\"parent\":\"null\"}]"
exprToString parent (Add a1 a2) = 
  "[{\"name\":\"Top Level: ADD \",\"parent\":\"null\",\"children\":[{\"name\":\"Level 2: B\",\"parent\":\"Top Level\"}]}]"



  --"[{\"name\":\"Top Level: " <> T.pack $ show AHole <> " \",\"parent\":\"null\"}]"
  -- "[{\"name\":\"Top " <> "schabernack" <> "Leel\",\"parent\":\"null\"}]"
  --"[{\"name\":\"" <> T.pack (show AHole) <> "\",\"parent\":\"null\"}]"
  --"[{\"name\":\"Top Level\",\"parent\":\"null\"}]"
  --"[{\"name\":\"" <> (T.pack $ show AHole) <> "\",\"parent\":\"null\"}]"
  --T.pack $ "[{\"name\":\"" ++ show AHole ++ "\",\"parent\":\"" ++ parent ++ "\"}]"
{-exprToString parent n@(Num i) = T.pack $
  "[{\"name\":\""<> show n <> "\",\"parent\":\"" <> parent <> "\"}]"
exprToString parent v@(Var var) = T.pack $
  "[{\"name\":\""<> show v <> "\",\"parent\":\"" <> parent <> "\"}]"
exprToString parent a@(Add a1 a2) = T.pack $
  "[{\"name\":\""<> show a <> "\",\"parent\":\"" <> parent <> "\"}]"
-}



