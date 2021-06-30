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
import qualified Data.Text.Lazy as TL

import Reflex.Dom

import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Common.Route

import Smt
import qualified Grammar as G
import Parser
import Eval
import qualified TestBackward as B

import qualified GHC.Generics as G
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Types as A
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
      -- executeJS "document.getElementById(\"codeMirror\").value=JSON.stringify(sampleTreeData)"
      executeJS "document.getElementById(\"codeMirror\").value=startProgramCode"

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
        executeJS $ "try { showTree(" <> (convertToTree . exec . parse $ t) <> "); }\
                    \catch (e) {console.log(\"error\",e);}"

parse :: T.Text -> G.Command
parse t = extractCommands
        $ T.unpack t
exec :: G.Command -> (G.CommandTrace, G.State)
exec = evalC orgInState
-- L [Node "TopLevel" "null" []]
-- [{"name":"Top Level","parent":"null"}]
--TL.toStrict $ A.encodeToLazyText $ Node "TopLevel" (Just "null")
--T "asf" "Nothing" 
convertToTree :: (G.CommandTrace, G.State) -> T.Text
convertToTree (t,s) =
  TL.toStrict
  $ A.encodeToLazyText
  $ fromCTraceToGraphNode t
  where
    -- (ct, st) =  evalC orgInState $ B.prog
    -- G.Seq B.rAsn $ G.Seq G.Skip G.Skip-- G.Assign "r" (G.Var "a")  -- G.If (G.Bool True) G.Skip G.Skip

progStr :: T.Text
progStr = T.pack "r := a; while ( b <= r ) do { q := (q + 1); r := (r - b)}; if ( ! (r = 0) ) then {res := 0} else {res := 1}"


makeState   :: [(G.Var, Maybe G.ArithVal)]
            -> G.State
makeState ((v,maybeVal):rest) =
    G.Store (makeState rest) v maybeVal
makeState [] = G.Empty

-- [x -> 1, y -> 0, z -> 2]
beginState :: G.State
beginState = makeState
    [ ("z", Just 2)
    , ("y", Just 0)
    , ("x", Just 1)
    , ("a", Just 6)
    , ("r", Just 4)]

orgInState :: G.State
orgInState = makeState 
    [ ("q", Just 0)
    , ("r", Just 0)
    , ("res", Just 0)
    , ("a", Just 4)
    , ("b", Just 2)]



-- hard coded data example
--[ { "name":"Top Level"
--  , "parent":"null"
--  , "children":
--    [ { "name":"Level 2: A"
--      , "parent":"Top Level"
--      , "children":
--        [ { "name":"Son of A"
--          , "parent":"Level 2: A"}
--        , { "name":"Daughter of A"
--          , "parent":"Level 2: A"}]}
--    , { "name":"Level 2: B"
--      , "parent":"Top Level"}]}]
ex1 = [ Node "Top Level" "null"
          [ Node "Level 2:A" "aha"
            [ Node "Son of A" "Level 2: A"
              []
            , Node "Daughter of A" "Level 2: A"
              []]
          , Node "Level 2:B" "Top Level"
            []]]


fromCTraceToGraphNode :: G.CommandTrace -> [GraphNode]
fromCTraceToGraphNode  G.TSkip
  = [Node "Skip" "" []]
fromCTraceToGraphNode (G.TAssign v a)
  = [Node ("Assign " <> T.pack v <> " to ")  "" $ fromATraceToGraphNode a]
fromCTraceToGraphNode (G.TSeq c1 c2)
  = let t1 = fromCTraceToGraphNode c1
        t2 = fromCTraceToGraphNode c2
     in [Node "Sequent" "" (inject t1 t2)]
fromCTraceToGraphNode (G.TWhileTrue b c1 c2)
  = let tb = fromBTraceToGraphNode b
        t1 = fromCTraceToGraphNode c1
        t2 = fromCTraceToGraphNode c2
     in [Node "WhileTrue" "" $ inject (inject tb t1) t2]
fromCTraceToGraphNode (G.TWhileFalse b)
  = [Node "WhileFalse" "" []]
fromCTraceToGraphNode (G.TIfTrue b c)
  = let tb = fromBTraceToGraphNode b
        tc = fromCTraceToGraphNode c
     in [Node "IfTrue" "" $ inject tb tc]
fromCTraceToGraphNode (G.TIfFalse b c)
  = let tb = fromBTraceToGraphNode b
        tc = fromCTraceToGraphNode c
     in [Node "IfFalse" "" $ inject tb tc]


inject :: [GraphNode] -> [GraphNode] -> [GraphNode]
inject [] g2 = g2
inject [Node n p c] g2 = [Node n p $ inject c g2]


fromATraceToGraphNode :: G.ArithTrace -> [GraphNode]
fromATraceToGraphNode (G.TNum i)
  = [Node ("Number " <> T.pack (show i)) "" []]
fromATraceToGraphNode (G.TRead v maybeA)
  = [Node ("VarRead " <> T.pack v <> T.pack (show maybeA)) "" []]
fromATraceToGraphNode (G.TAdd a1 a2)
  = let t1 = fromATraceToGraphNode a1
        t2 = fromATraceToGraphNode a2
     in [Node "Add " "" $ inject t1 t2]
fromATraceToGraphNode (G.TSub a1 a2)
  = let t1 = fromATraceToGraphNode a1
        t2 = fromATraceToGraphNode a2
     in [Node "Sub " "" $ inject t1 t2]


fromBTraceToGraphNode :: G.BoolTrace -> [GraphNode]
fromBTraceToGraphNode G.TTruth = [Node "Truth" "" []]
fromBTraceToGraphNode G.TFalsum = [Node "Falsum" "" []]
fromBTraceToGraphNode (G.TEq a1 a2)
  = let t1 = fromATraceToGraphNode a1
        t2 = fromATraceToGraphNode a2
     in [Node "Equality" "" $ inject t1 t2]
fromBTraceToGraphNode (G.TLEq a1 a2)
  = let t1 = fromATraceToGraphNode a1
        t2 = fromATraceToGraphNode a2
     in [Node "LessEqual" "" $ inject t1 t2]
fromBTraceToGraphNode (G.TNot bt)
  = let t = fromBTraceToGraphNode bt
     in [Node "Not" "" $ t]
fromBTraceToGraphNode (G.TAnd b1 b2)
  = let t1 = fromBTraceToGraphNode b1
        t2 = fromBTraceToGraphNode b2
     in [Node "And" "" $ inject t1 t2]

data GraphNode  = Node { name :: T.Text
                      , parent :: T.Text
                      , children :: [GraphNode]
                      }
  deriving (Show, Eq, G.Generic, A.FromJSON, A.ToJSON)


-- convertToJSON :: ArithExpr -> GraphNode
-- convertToJSON  AHole = Node "AHole" Nothing Nothing







        -- E.decodeUtf8  $ encode $  Node "asf" Nothing Nothing
      -- E.decodeUtf8 $ encode $ convertToJSON AHole
        -- exprToString "null" (Add AHole AHole)
        -- "[{\"name\":\"Top Level\",\"parent\":\"null\"}]"
        --"hallo" -- strToProp smtProg





exprToString :: String -> G.ArithExpr -> T.Text
exprToString parent G.AHole =
  "[{\"name\":\"Top Level: " <> T.pack "AHole" <> " \",\"parent\":\"null\"}]"
exprToString parent (G.Num i) =
  "[{\"name\":\"Top Level: " <> T.pack (show i)  <> " \",\"parent\":\"null\"}]"
exprToString parent (G.Var v) =
  "[{\"name\":\"Top Level: " <> T.pack v  <> " \",\"parent\":\"null\"}]"
exprToString parent (G.Add a1 a2) =
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



