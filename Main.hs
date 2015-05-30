{-# LANGUAGE ScopedTypeVariables #-}
import System.Process
import Control.Monad (void)

import Vision.Image
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Vision.Primitive

import System.Random

main :: IO ()
main = do
  n <- getStdRandom (randomR (6,30))
  let -- n = 7
      meth = TruncateInteger -- Bilinear
      [input, output] = ["/tmp/.i3lock_pre.png", "/tmp/.i3lock_post.png"]

  h1 <- spawnCommand "scrot /tmp/.i3lock_pre.png"
  h2 <- spawnCommand "if [ -e /tmp/.i3lock_post.png ] ; then rm /tmp/.i3lock_post.png; fi"

  waitForProcess h2
  waitForProcess h1

  io <- load Autodetect input
  case io of
   Left err -> print $ "Unable to load the image: " ++ show err
   Right (rgb :: RGB) -> do
     let s@(Z :. sh :. sw) = shape rgb
         resized = resize meth (ix2 (sh `div` n) (sw `div` n)) rgb :: RGB
         reresized = resize meth s resized :: RGB
     save_err <- save Autodetect output reresized
     case save_err of
      Nothing  -> void $ putStrLn "Success." >> spawnCommand "i3lock -i /tmp/.i3lock_post.png"
      Just err -> print $ "Unable to save the image: " ++ show err
