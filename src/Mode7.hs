
module Mode7 (main) where -- WIP mode-7 simulation using SDL

import SDL (Window,Surface, Point(..), V4(..),V2(..),)
import SDL.Font (Font,Color)
import qualified Data.Text as Text (pack)
import qualified SDL
import qualified SDL.Font as Font

import Screen7 (Screen7)
import qualified Bbc (Cycles,State,init,step,getScreen7)
import qualified Screen7 (lines)

main :: IO ()
main = do
  SDL.initializeAll
  Font.initialize
  let cw = 40 -- width of the mode-7 screen in chars
  let ch = 25 -- height of the mode-7 screen in chars
  -- how are these numbers related
  let (pw,ph) = (19,36)
  let w = pw*cw
  let h = ph*ch
  font :: Font <- Font.load "assets/Acorn Full Nostalgia.ttf" ph
  win :: Window <- SDL.createWindow (Text.pack "BBC Micro, Mode 7") $ winConfig (w,h)
  screen :: Surface <- SDL.getWindowSurface win
  let
    renderLine :: Int -> String -> IO ()
    renderLine i s = do
      let start = P (V2 0 (fromIntegral (i*ph)))
      let t = Text.pack [ if c == '\0' then ' ' else c | c <- s ]
      image <- Font.shaded font white black t
      screen :: Surface <- SDL.getWindowSurface win
      _ <- SDL.surfaceBlit image Nothing screen (Just start)
      pure ()

    updateScreen7 :: Screen7 -> IO ()
    updateScreen7 screen7 = do
      let lines = Screen7.lines screen7
      SDL.surfaceFillRect screen Nothing black
      sequence_ [ renderLine i s | (i,s) <- zip [0..24] lines ]
      SDL.updateWindowSurface win

    loop :: Int -> Bbc.State -> IO ()
    loop n bbc = do
      let target :: Bbc.Cycles = fromIntegral (100000 * n)
      putStrLn $ "loop: " <> show target
      screen7 <- Bbc.getScreen7 bbc
      updateScreen7 screen7
      bbc <- Bbc.step target bbc
      SDL.delay 50
      loop (n+1) bbc

  bbc0 <- Bbc.init
  loop 0 bbc0


black,white :: Color
black = V4 0 0 0 255
white = V4 255 255 255 255

winConfig :: (Int,Int) -> SDL.WindowConfig
winConfig (x,y) =
  SDL.defaultWindow { SDL.windowInitialSize = z } where
  z = SDL.V2 (fromIntegral x) (fromIntegral y)
