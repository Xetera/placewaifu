module Waifu.Server.Views.Home where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Waifu.Server.Views.Head

home :: H.Html
home =
  H.docTypeHtml $ do
    siteHead
    H.body $ do
      H.img ! A.src "/assets/images/aoba.png"
      H.h1 "Placewaifu"
      H.p "Coming soon..."
