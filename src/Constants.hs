module Constants where

type TagType = Int

tagNumber :: TagType
tagNumber = 1

tagChar :: TagType
tagChar = 2

tagClosure :: TagType
tagClosure = 3

tagTrue :: TagType
tagTrue = 4

tagFalse :: TagType
tagFalse = 5 

tagNil :: TagType
tagNil = 6

tagCons :: TagType
tagCons = 7
