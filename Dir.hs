module Dir(Dir(Norte, Este, Sur, Oeste), opuesto, siguiente) where

data Dir = Norte | Este | Sur | Oeste deriving (Show)

opuesto :: Dir -> Dir
opuesto Norte = Sur 
opuesto Sur = Norte 
opuesto Este = Oeste 
opuesto Oeste = Este 

siguiente :: Dir -> Dir
siguiente Norte = Este 
siguiente Sur = Oeste 
siguiente Este = Sur 
siguiente Oeste = Norte 
