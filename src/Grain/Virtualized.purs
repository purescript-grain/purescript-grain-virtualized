module Grain.Virtualized
  ( Config
  , virtualList
  ) where

import Prelude

import Data.Array (concat, snoc)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Grain (class Grain, VNode, fromConstructor, grain, useLocalState)
import Grain.Markup as H
import Web.DOM.Element as E
import Web.Event.Event (target)

-- | The type of virtual list config.
-- |
-- | - `height`: The px height of container
-- | - `rows`: The data for row view
-- | - `rowView`: The renderer of a row
-- | - `calcRowHeight`: The calculator of a row view's px height
type Config a =
  { height :: Number
  , rows :: Array a
  , rowView :: { style :: String, row :: a } -> VNode
  , calcRowHeight :: a -> Number
  }

type Params =
  { topPadding :: Number
  , bottomPadding :: Number
  , renderingHeight :: Number
  , renderingTargets :: Array VNode
  }

newtype ScrollTop = ScrollTop Number

instance grainScrollTop :: Grain ScrollTop where
  initialState _ = pure $ ScrollTop 0.0
  typeRefOf _ = fromConstructor ScrollTop

-- | Render a virtual list.
virtualList :: forall a. Config a -> VNode
virtualList config = H.component do
  Tuple (ScrollTop scrollTop) updateScrollTop <- useLocalState (grain :: _ ScrollTop)

  let style = "overflow:auto;height:" <> show config.height <> "px;"

      children = calculateChildren config scrollTop

      onScroll evt =
        case join $ E.fromEventTarget <$> target evt of
          Nothing -> pure unit
          Just element -> do
            scrollTop' <- E.scrollTop element
            updateScrollTop $ const $ ScrollTop scrollTop'

  pure $ H.div
    # H.style style
    # H.onScroll onScroll
    # H.kids children

calculateChildren :: forall a. Config a -> Number -> Array VNode
calculateChildren config scrollTop =
  let params = calculateParams config scrollTop
      topPadding = H.key "_virtualized_top_padding" $ padding params.topPadding
      bottomPadding = H.key "_virtualized_bottom_padding" $ padding params.bottomPadding
   in concat [ [ topPadding ], params.renderingTargets, [ bottomPadding ] ]

calculateParams
  :: forall a
   . Config a
  -> Number
  -> Params
calculateParams config scrollTop =
  foldl
    (calculateParamsByRow config scrollTop)
    { topPadding: 0.0
    , bottomPadding: 0.0
    , renderingHeight: 0.0
    , renderingTargets: []
    }
    config.rows

calculateParamsByRow
  :: forall a
   . Config a
  -> Number
  -> Params
  -> a
  -> Params
calculateParamsByRow config scrollTop params row =
  if params.topPadding + rowHeight < scrollTop then
    params { topPadding = params.topPadding + rowHeight }
  else if params.renderingHeight < config.height + rowHeight then
    params
      { renderingHeight = params.renderingHeight + rowHeight
      , renderingTargets =
          snoc params.renderingTargets
            $ calculateRowView config rowHeight row
      }
  else
    params { bottomPadding = params.bottomPadding + rowHeight }
  where
    rowHeight = config.calcRowHeight row

calculateRowView :: forall a. Config a -> Number -> a -> VNode
calculateRowView { rowView } rowHeight row =
  let style = "height: " <> show rowHeight <> "px;"
   in rowView { style, row }

padding :: Number -> VNode
padding height =
  H.div # H.style ("height: " <> show height <> "px;")
