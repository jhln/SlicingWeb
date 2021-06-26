{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

--import Reflex.Dom.Core (run,Run(..))
import Reflex.Dom

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      {-el "h1" $ text "Hi Obelisk!"
      el "p" $ text $ T.pack commonStuff
      
      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s-}
      something
      return ()
  }
  
something :: (DomBuilder t m,PostBuild t m) => m ()
something = el "div" $ do
    
   elAttr "p" ("id" =: "myText") $ text $ T.pack "this is my text"
   t <- inputElement $ def
       & initialAttributes .~
          ("id" =: "myInput")
   el "p" $ dynText $ (T.pack . fromProg . myParser) 
            <$> (_inputElement_value $ t)


newtype Program = Program { fromProg :: String }

myParser :: T.Text -> Program
myParser a 
  | T.length a == 0 = Program ""
  | otherwise = Program $ "program {" ++ (T.unpack a) ++ "}"