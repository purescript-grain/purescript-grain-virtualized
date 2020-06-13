module Main where

import Prelude

import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Grain (class Grain, VNode, fromConstructor, grain, mountUI, useLocalState)
import Grain.Markup as H
import Grain.Virtualized (virtualList)
import Web.DOM.Element (toNode)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

newtype State = State Int

derive instance newtypeState :: Newtype State _

instance grainState :: Grain State where
  initialState _ = pure $ State 999
  typeRefOf _ = fromConstructor State

main :: Effect Unit
main = do
  maybeEl <- window >>= document <#> toParentNode >>= querySelector (QuerySelector "#app")
  case maybeEl of
    Nothing -> pure unit
    Just el ->
      void $ mountUI view $ toNode el

view :: VNode
view = H.component do
  Tuple (State state) updateState <- useLocalState (grain :: _ State)
  let addMany = updateState $ over State (_ + 1000)
  pure $ H.div # H.kids
    [ H.h1 # H.kids [ H.text "Virtual list demo" ]
    , H.button
        # H.onClick (const addMany)
        # H.kids [ H.text "Add 1000" ]
    , H.div # H.css css # H.kids
        [ virtualList
            { height: 500.0
            , rows: 0 .. state
            , rowView: item
            , calcRowHeight: const 50.0
            }
        ]
    ]
  where
    css = ".&{width: 800px;border: 1px solid #EEE;}"

item :: { style :: String, row :: Int } -> VNode
item { style, row } =
  H.div
    # H.key (show row)
    # H.css css
    # H.style style
    # H.kids [ H.text $ "Item " <> show row ]
  where
    css =
      """
      .& {
        display: flex;
        justify-content: flex-start;
        align-items: center;
        padding: 0 16px;
        border-bottom: 1px solid #EEE;
        box-sizing: border-box;
      }
      .&:last-of-type {
        border-bottom: none;
      }
      """
