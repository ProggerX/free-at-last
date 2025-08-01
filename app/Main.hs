{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brillo
import Data.ByteString as BS
import Data.FileEmbed
import Web.Browser

pinosHeight, pinosWidth :: Float
pinosHeight = 844
pinosWidth = 720

bmpe :: ByteString
bmpe = $(embedFileRelative "pinos.bmp")

sc :: Float
sc = 0.5

bouncing :: Float -> Float -> Float -> Float -> Float -> (Float, Float)
bouncing screenWidth screenHeight x0 y0 t =
  (calcPos x0 maxX t, calcPos y0 maxY t)
 where
  maxX = screenWidth - pinosWidth
  maxY = screenHeight - pinosHeight

  calcPos :: Float -> Float -> Float -> Float
  calcPos initial maxDim steps =
    let total = initial + steps
        modPos = fromIntegral @Integer $ floor total `mod` floor (2 * maxDim)
     in if modPos > maxDim
          then 2 * maxDim - modPos
          else modPos

animation :: Picture -> Float -> Picture
animation pinos t =
  scale 0.2 0.2 $
    pictures
      [ translate (x + pinosWidth / 2 - 5000 * sc) (y + pinosHeight / 2 - 5000 * sc) pinos
      , color white $ rectangleWire (10000 * sc) (10000 * sc)
      ]
 where
  (x, y) = bouncing (10000 * sc) (10000 * sc) 0 0 (t * 2000 * sc)

main :: IO ()
main = do
  BS.writeFile "tmp.bmp" bmpe
  pic <- loadBMP "tmp.bmp"
  _ <- openBrowser "https://bald.su"
  animate FullScreen black $ animation pic
