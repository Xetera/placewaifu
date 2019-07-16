module Waifu.Server.Views.Head where

import Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

siteHead :: Html
siteHead =
  H.head $ do
    title "Placewaifu"
    meta ! A.name "An anime girl placeholder service"
