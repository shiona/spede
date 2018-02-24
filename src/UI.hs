{-# LANGUAGE OverloadedStrings #-}

module UI where

import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)

import Spede

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, hBox, vBox, setAvailableSize
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str, raw
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>), (<=>)
  )

import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color.Extra (brighten)

import Lens.Micro ((&), (.~), (%~), (^.))

--data GameEvent = LightUp | LightDown
type GameEvent = ()
type Name = ()

app :: App Game GameEvent Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan ()
    threadDelay 10000
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

handleEvent :: Game -> BrickEvent Name GameEvent -> EventM Name (Next Game)
handleEvent g (AppEvent ())                          = continue $ if (g ^. state) == Running then step g else g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))         = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') []))  = continue $ start g
handleEvent g (VtyEvent (V.EvKey V.KEnter []))       = continue $ start g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') []))  = continue $ press g B
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') []))  = continue $ press g Y
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') []))  = continue $ press g G
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') []))  = continue $ press g R
handleEvent g _                                      = continue g


drawUI :: Game -> [Widget Name]
drawUI g = [ C.center $ drawGame g ]

drawGame :: Game -> Widget Name
drawGame g = withBorderStyle BS.unicodeBold
  $ hLimit 20
  $ B.borderWithLabel (str " Spede ")
  -- $ C.hCenter
  $ padAll 1
  $ (C.hCenter $ drawScore g) <=> (padTop (Pad 2) $ C.hCenter $ hBox $ map (drawButton g) [B, Y, G, R])

drawScore :: Game -> Widget Name
drawScore g = str ("Score " ++ show (g ^. score)) <=> padTop (Pad 1) (str (show (g ^. state)))

drawButton :: Game -> Button -> Widget Name
drawButton g b = topLight <=> button <=> botLight --withAttr attr $ str "abcdefghijk"
  where
    isLit = g ^. lit == Just b
    topLight = if isLit
                 then raw $ V.string hAttr "\\  /"
                 else str "    "
    botLight = if isLit
                 then raw $ V.string hAttr "/  \\"
                 else str "    "
    button = str " " <+> raw (V.string attr "ab") <+> str " "
    attr = color `on` color
    hAttr = V.defAttr `V.withForeColor` color
    style = if isLit then brighten else id
    color = style $ case b of
                      R -> V.red
                      G -> V.green
                      Y -> V.yellow
                      B -> V.blue
    {-
    topLight = if isLit
      then withAttr hAttr $ str "\\  /"
      else str "    "
    botLight = if isLit
      then withAttr hAttr $ str "/  \\"
      else str "    "
    button = str " " <+> withAttr attr (str "  ") <+> str " "
    s = if isLit
         then str "  "
         --else V.string (V.red `on` V.magenta) "x"
         else str "  "
    --attr :: V.AttrName
    attr = if isLit
             then case b of
                R -> brAttr
                G -> bgAttr
                Y -> byAttr
                B -> bbAttr
             else case b of
                R -> rAttr
                G -> gAttr
                Y -> yAttr
                B -> bAttr
    hAttr = case b of
                R -> hrAttr
                G -> hgAttr
                Y -> hyAttr
                B -> hbAttr

rAttr, gAttr, yAttr, bAttr :: AttrName
brAttr, bgAttr, byAttr, bbAttr :: AttrName
--boldAttr   = "boldAttr"
rAttr   = "rAttr"
gAttr   = "gAttr"
yAttr   = "yAttr"
bAttr   = "bAttr"
brAttr   = "brAttr"
bgAttr   = "bgAttr"
byAttr   = "byAttr"
bbAttr   = "bbAttr"
hrAttr   = "hrAttr"
hgAttr   = "hgAttr"
hyAttr   = "hyAttr"
hbAttr   = "hbAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (rAttr,       V.red `on` V.red)
  , (gAttr,       V.green `on` V.green)
  , (yAttr,       V.yellow `on` V.yellow)
  , (bAttr,       V.blue `on` V.blue)
  , (brAttr,       V.brightRed `on` V.brightRed)
  , (bgAttr,       V.brightGreen `on` V.brightGreen)
  , (byAttr,       V.brightYellow `on` V.brightYellow)
  , (bbAttr,       V.brightBlue `on` V.brightBlue)
  , (hrAttr,       V.defAttr `V.withForeColor` V.brightRed)
  , (hgAttr,       V.defAttr `V.withForeColor` V.brightGreen)
  , (hyAttr,       V.defAttr `V.withForeColor` V.brightYellow)
  , (hbAttr,       V.defAttr `V.withForeColor` V.brightBlue)
  ]
-}
theMap :: AttrMap
theMap = attrMap V.defAttr [ ]
