module HTML where


import Control.Monad.Free

data Element = Element {
    name :: String
  , attribs :: [Attribute]
  , content :: Maybe [Content]
  }

data Content = TextContent String
             | ElementContent Element

data Attribute = Attribute {
    key :: String
  , value :: String
  }



{-

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
    renderAttribute (Attribute x) = x.key <> "=\"" <> x.value <> "\""

    renderContent :: Maybe [Content] -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
        ">" <> joinWith "" (map renderContentItem content) <>
        "</" <> e.name <> ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'
-}
