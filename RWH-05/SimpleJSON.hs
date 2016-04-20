-- file: ch05/SimpleJSON.hs
module SimpleJSON
    (
      JValue(..)
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where

data JValue = JString String
              | JNumber Double
              | JBool Bool
              | JNull
              | JObject [(String, JValue)]
              | JArray [JValue] 
                deriving (Eq, Ord, Show)

-- Accessor functions follow, as they call it!
-- back conversion from JValue to String
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt (JNumber x) = Just (truncate x)
getInt _           = Nothing

getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull
