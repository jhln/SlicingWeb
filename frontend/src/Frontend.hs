{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}

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
    when ((T.length t) > 0) $ 
        -- this tries to parse and convert the program text to a or shows the resulting error.
        -- NOTE: `convertToTree` MUST produce a valid json otherwise there will be a parsing error
        -- breaking the application
        executeJS $ "try { showTree(" <> (convertToTree . parse $ t) <> "); }\
                    \catch (e) {console.log(\"error\",e);}"
    return()
  where
      parse t' = t'
      convertToTree t' = t'