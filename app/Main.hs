----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import           Miso.Html
import           Miso.Html.Property
import           Miso.Media hiding (Stream)
import           Miso.Navigator
import qualified Miso.CSS as CSS
----------------------------------------------------------------------------
import           Data.Function
import qualified Data.Map.Strict as M
----------------------------------------------------------------------------
-- | Component model state
data Action
  = OpenCamera
  | OpenedStream Stream
  | ErrorCamera JSVal
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = startApp (M.insert "click" BUBBLE mediaEvents) app
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
type Model = ()
----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = component () update_ $ \() ->
  vfrag
  [ h2_ [ CSS.style_ [ CSS.fontFamily "monospace" ] ] [ "🍜 📷 miso-camera" ]
  , button_
    [ id_ "button"
    , autoplay_ True
    , onClick OpenCamera 
    , CSS.style_
      [ CSS.fontSize (CSS.px 20)
      , CSS.fontFamily "monospace"
      , CSS.margin "10px"
      ]
    ]
    [ "Open Camera"
    ]
  , div_
    [ CSS.style_
      [ CSS.width (CSS.px 500)
      , CSS.height (CSS.px 400)
      , CSS.border "2px solid black"
      ]
    ]
    [ video_
      [ id_ "video"
      , muted_ True
      , CSS.style_
        [ CSS.width (CSS.px 500)
        , CSS.height (CSS.px 400)
        , "object-fit" =: "cover"
        ]
      ]
      []
    ]
  ]
   where
     update_ :: Action -> Effect parent Model Action
     update_ = \case
       OpenCamera ->
         getUserMedia userMedia OpenedStream ErrorCamera
       OpenedStream stream ->
         io_ $ do
           vid <- Media <$> getElementById "video"
           vid & srcObject stream
           vid & play
       ErrorCamera errorValue ->
         io_ $ do
           consoleLog "Error opening camera"
           consoleLog' errorValue
----------------------------------------------------------------------------
