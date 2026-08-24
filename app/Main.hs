----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Monad      (forM_, when)
----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
import qualified Miso.Html.Element  as H
import           Miso.Html.Event    (onClick)
import qualified Miso.Html.Property as P
import           Miso.Media         (Media (..), play, srcObject, videoHeight,
                                     videoWidth)
import           Miso.Navigator     (Stream, getUserMedia, userMedia)
import qualified Miso.CSS           as CSS
----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
data Model = Model
  { _streaming :: Bool
  , _mirrored  :: Bool
  , _filterIx  :: Int
  , _shots     :: [MisoString] -- ^ PNG data URLs, newest first
  , _oops      :: Maybe MisoString
  } deriving (Eq, Show)
----------------------------------------------------------------------------
streaming :: Lens Model Bool
streaming = lens _streaming $ \m x -> m { _streaming = x }

mirrored :: Lens Model Bool
mirrored = lens _mirrored $ \m x -> m { _mirrored = x }

filterIx :: Lens Model Int
filterIx = lens _filterIx $ \m x -> m { _filterIx = x }

shots :: Lens Model [MisoString]
shots = lens _shots $ \m x -> m { _shots = x }

oops :: Lens Model (Maybe MisoString)
oops = lens _oops $ \m x -> m { _oops = x }
----------------------------------------------------------------------------
data Action
  = OpenCamera
  | OpenedStream Stream
  | ErrorCamera JSVal
  | StopCamera
  | ToggleMirror
  | SetFilter Int
  | Snapshot
  | GotShot MisoString
  | ClearShots
----------------------------------------------------------------------------
-- | CSS filters selectable for the live view; also baked into snapshots.
filters :: [(MisoString, MisoString)]
filters =
  [ ("none",    "none")
  , ("mono",    "grayscale(1)")
  , ("sepia",   "sepia(0.85)")
  , ("pop",     "contrast(1.5) saturate(1.6)")
  , ("invert",  "invert(1)")
  , ("hue",     "hue-rotate(140deg)")
  ]
----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
----------------------------------------------------------------------------
app :: App Model Action
app = component (Model False False 0 [] Nothing) updateModel viewModel
----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  OpenCamera ->
    getUserMedia userMedia OpenedStream ErrorCamera
  OpenedStream stream -> do
    streaming .= True
    oops .= Nothing
    io_ $ do
      video <- Media <$> getElementById "video"
      srcObject stream video
      play video
  ErrorCamera err -> do
    streaming .= False
    oops ?= "Could not open the camera — check permissions and try again."
    io_ (consoleLog' err)
  StopCamera -> do
    streaming .= False
    io_ stopTracks
  ToggleMirror ->
    mirrored %= not
  SetFilter i ->
    filterIx .= i
  Snapshot -> do
    css <- activeFilter <$> get
    flip_ <- use mirrored
    io (GotShot <$> takeSnapshot css flip_)
  GotShot url ->
    shots %= \xs -> take 12 (url : xs)
  ClearShots ->
    shots .= []
----------------------------------------------------------------------------
activeFilter :: Model -> MisoString
activeFilter m =
  case drop (m ^. filterIx) filters of
    (_, css) : _ -> css
    []           -> "none"
----------------------------------------------------------------------------
-- | Stop all tracks of the current stream and detach it.
stopTracks :: IO ()
stopTracks = do
  video <- getElementById "video"
  stream <- video ! "srcObject"
  bad <- isNull stream
  when (not bad) $ do
    tracks <- fromJSValUnchecked =<< (stream # "getTracks" $ ())
    forM_ (tracks :: [JSVal]) $ \t -> t # "stop" $ ()
  setField video "srcObject" jsNull
----------------------------------------------------------------------------
-- | Draw the current video frame (with filter and mirroring applied) onto an
-- offscreen canvas and return it as a PNG data URL.
takeSnapshot :: MisoString -> Bool -> IO MisoString
takeSnapshot css flip_ = do
  videoRef <- getElementById "video"
  let video = Media videoRef
  w <- videoWidth video
  h <- videoHeight video
  doc <- jsg "document"
  cnv <- doc # "createElement" $ [ "canvas" :: MisoString ]
  setField cnv "width" w
  setField cnv "height" h
  ctx <- cnv # "getContext" $ [ "2d" :: MisoString ]
  setField ctx "filter" css
  when flip_ $ do
    _ <- ctx # "translate" $ (fromIntegral w :: Double, 0 :: Double)
    _ <- ctx # "scale" $ (-1 :: Double, 1 :: Double)
    pure ()
  _ <- ctx # "drawImage" $ (videoRef, 0 :: Double, 0 :: Double)
  fromJSValUnchecked =<< (cnv # "toDataURL" $ [ "image/png" :: MisoString ])
----------------------------------------------------------------------------
viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ m =
  H.div_
  [ P.class_ "app" ]
  [ H.header_
    [ P.class_ "hero" ]
    [ H.h1_ [] [ "🍜 📷 ", H.a_ [ P.href_ repoUrl ] [ "miso-camera" ] ]
    , H.p_ [ P.class_ "tagline" ]
      [ "getUserMedia from Haskell: live camera preview, CSS filters, and "
      , "snapshots rendered to PNG — all client-side, compiled to WebAssembly."
      ]
    , H.a_ [ P.class_ "gh", P.href_ repoUrl ] [ "View source on GitHub" ]
    ]
  , H.main_
    [ P.class_ "panel" ]
    ( [ H.div_
        [ P.class_ "viewport" ]
        [ H.video_
          [ P.id_ "video"
          , P.autoplay_ True
          , P.muted_ True
          , textProp "playsinline" "true"
          , P.class_ (if m ^. mirrored then "video flip" else "video")
          , CSS.style_ [ "filter" =: activeFilter m ]
          ]
          []
        , if m ^. streaming
            then H.div_ [ P.class_ "hidden" ] []
            else H.div_
              [ P.class_ "overlay" ]
              [ H.button_ [ P.class_ "btn big", onClick OpenCamera ]
                [ "🎥 Open camera" ]
              ]
        ]
      ]
      ++ [ H.p_ [ P.class_ "error" ] [ text e ] | Just e <- [ m ^. oops ] ]
      ++ [ H.div_
           [ P.class_ "filters" ]
           [ H.button_
             [ P.class_ (if i == m ^. filterIx then "chip active" else "chip")
             , onClick (SetFilter i)
             ]
             [ text name ]
           | (i, (name, _)) <- zip [ 0 .. ] filters
           ]
         | m ^. streaming
         ]
      ++ [ H.div_
           [ P.class_ "controls" ]
           [ H.button_ [ P.class_ "btn primary", onClick Snapshot ] [ "📸 Snapshot" ]
           , H.button_ [ P.class_ "btn", onClick ToggleMirror ]
             [ text (if m ^. mirrored then "Mirror: on" else "Mirror: off") ]
           , H.button_ [ P.class_ "btn", onClick StopCamera ] [ "Stop" ]
           ]
         | m ^. streaming
         ]
      ++ [ H.div_
           []
           [ H.div_
             [ P.class_ "toolbar" ]
             [ H.h2_ [] [ "Snapshots" ]
             , H.button_ [ P.class_ "btn", onClick ClearShots ] [ "Clear" ]
             ]
           , H.div_
             [ P.class_ "gallery" ]
             [ H.a_
               [ P.href_ url, P.download_ "miso-camera.png", P.class_ "shot" ]
               [ H.img_ [ P.src_ url, P.alt_ "snapshot" ] ]
             | url <- m ^. shots
             ]
           ]
         | m ^. shots /= []
         ]
    )
  , H.footer_
    [ P.class_ "foot" ]
    [ H.p_ []
      [ "Built with "
      , H.a_ [ P.href_ "https://github.com/dmjio/miso" ] [ "miso" ]
      , ", a Haskell web framework — compiled to WebAssembly. "
      , "Nothing leaves your browser."
      ]
    ]
  ]
  where
    repoUrl = "https://github.com/haskell-miso/miso-camera"
----------------------------------------------------------------------------
