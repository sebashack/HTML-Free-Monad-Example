{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE FlexibleInstances    #-}

module HTML ( Element
            , Attribute
            , Content
            , ContentF
            , AttributeKey
            , toValue

            , a
            , p
            , img

            , href
            , _class
            , src
            , width
            , height

            , attribute
            , text
            , elem

            , render   ) where


import Prelude hiding (elem)
import Control.Monad.Free
import Control.Lens hiding (element)
import Data.List (intercalate)


data Attribute = Attribute {
    _key :: String
  , _value :: String
  } deriving Show
makeLenses ''Attribute

data ContentF a = TextContent String a
                | ElementContent Element a
                deriving Show

instance Functor ContentF where
  fmap f (TextContent str x) = TextContent str (f x)
  fmap f (ElementContent elt x) = ElementContent elt (f x)

type Content a = Free ContentF a

data Element = Element {
    _name :: String
  , _attribs :: [Attribute]
  , _content :: Maybe [Content ()]
  } deriving Show
makeLenses ''Element


-- << Valid Attribute Keys
newtype AttributeKey a = AttributeKey String

class IsValue a where
  toValue :: a -> String

instance IsValue String where
  toValue = id

instance IsValue Int where
  toValue = show


href :: AttributeKey String
href = AttributeKey "href"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey Int
width = AttributeKey "width"

height :: AttributeKey Int
height = AttributeKey "height"



-- << Constructors
element :: String -> [Attribute] -> Maybe [Content ()] -> Element
element name attribs content = Element name attribs content

a :: [Attribute] -> [Content ()] -> Element
a attribs content = element "a" attribs (Just content)

p :: [Attribute] -> [Content ()] -> Element
p attribs content = element "p" attribs (Just content)

img :: [Attribute] -> Element
img attribs = element "img" attribs Nothing

text :: String -> Content ()
text str = liftF $ TextContent str ()

elem :: Element -> Content ()
elem elt = liftF $ ElementContent elt ()

attribute ::IsValue a =>  AttributeKey a -> a -> Attribute
attribute (AttributeKey key) value = Attribute key (toValue value)



-- << Interpreter
render :: Element -> String
render elt = undefined

 {-

  "<" ++
  (view name elt)  ++ " " ++
  (intercalate " " (renderAttribute <$> view attribs elt)) ++
  (renderContent $ view content elt)
  where
    renderAttribute :: Attribute -> String
    renderAttribute attr =
      (view key attr) ++ "=\"" ++
      (view value attr) ++ "\""
    renderContent :: Maybe [Content] -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
      ">" ++ (intercalate "" (renderContentItem <$> content)) ++
      "</" ++ (view name elt) ++ ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'
-}
