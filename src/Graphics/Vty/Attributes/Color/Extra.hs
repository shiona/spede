module Graphics.Vty.Attributes.Color.Extra (brighten) where

import Graphics.Vty.Attributes (Color(..))

brighten :: Color -> Color
brighten (ISOColor n)
 | n < 8 = ISOColor (n+8)
brighten x = x
