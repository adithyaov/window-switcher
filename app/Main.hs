{-# LANGUAGE CPP #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (modify, get)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Maybe (fromMaybe, fromJust)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)
import Windows

drawUI :: Int -> Int -> L.List () Line -> [Widget ()]
drawUI appColLen titleColLen l = [ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              L.renderList (listDrawElement appColLen titleColLen) True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press n/p to move."
                              , C.hCenter $ str "Press Enter to select."
                              , C.hCenter $ str "Press C-g to exit."
                              ]

appEvent :: T.BrickEvent () e -> T.EventM () (L.List () Line) ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar 'n') [] -> modify L.listMoveDown

        V.EvKey (V.KChar 'p') [] -> modify L.listMoveUp

        V.EvKey (V.KEnter) [] -> do
            list <- get
            let sel = snd $ fromJust $ L.listSelectedElement list
            liftIO $ activateWindow sel
            M.halt

        ev -> L.handleListEvent ev
appEvent _ = return ()

listDrawElement :: Int -> Int -> Bool -> Line -> Widget ()
listDrawElement appColLen titleColLen sel a =
    let selStr s = if sel
                   then str $ formatLine appColLen titleColLen s
                   else str $ formatLine appColLen titleColLen s
    in C.hCenter $ selStr a

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.black)
    , (L.listSelectedAttr,    V.black `on` V.white)
    ]

theApp :: Int -> Int -> M.App (L.List () Line) e ()
theApp appColLen titleColLen =
    M.App { M.appDraw = drawUI appColLen titleColLen
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
    ws0 <- winList
    ix <- currWindow ws0
    let ws = take ix ws0 ++ drop (ix + 1) ws0
    void
        $ M.defaultMain
              (theApp (appColumLen ws) (titleColumLen ws))
              (L.listMoveTo ix (L.list () (Vec.fromList ws) 4))
