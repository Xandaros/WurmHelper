module WurmParser
    ( parseEvent
    , EventMessage(ActionStart, ActionRepeat, ActionEnd, OtherMessage)
    ) where
--import Text.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec

data EventMessage = ActionStart
                  | ActionRepeat
                  | ActionEnd
                  | OtherMessage
                  deriving (Show)

parseEvent :: Parser EventMessage
parseEvent = try (parseTime >> (try actionStart <|> try actionRepeat <|> try actionEnd)) <|> return OtherMessage

parseTime :: Parser String
parseTime = do
    char '['
    digit >> digit
    char ':'
    digit >> digit
    char ':'
    digit >> digit
    char ']'
    space
    return "" -- TODO

actionStart :: Parser EventMessage
actionStart = string "You start" >> notFollowedBy (string " leading") >> return ActionStart

actionRepeat :: Parser EventMessage
actionRepeat = do
    string "After "
    action <- manyTill anyChar $ try $ string "you will start "
    return ActionRepeat

actionEnd :: Parser EventMessage
actionEnd = (parseEndMessages <|> noRepairMessage <|> stopMessage) >> return ActionEnd

stopMessage :: Parser ()
stopMessage = void $ try $ string "You stop" >> notFollowedBy (string " leading")

noRepairMessage :: Parser ()
noRepairMessage = void $ manyTill anyChar $ try $ string "doesn't need repairing"

endMessages :: [String]
endMessages = [ "You create a"
              , "You repair the"
              , "You almost made it"
              , "You attach"
              , "You push"
              , "You pull"
              , "You chip away"
              , "You cut down"
              , "There is no space"
              , "You dig a hole"
              , "You finish"
              , "You improve"
              , "You damage"
              , "You must use"
              , "You notice some"
              , "You will want"
              , "You managed to get a yield"
              , "You sow"
              , "You find"
              , "This area looks picked clean."
              , "You mine some"
              , "You must not move"
              , "The field is tended."
              , "The field is now"
              , "The field looks better"
              , "doesn't need repairing"
              , "could be improved"
              , "has some excess cloth"
              , "has some stains"
              , "has an open seam"
              , "has a seam"
              , "is in too poor shape"
              , "before you try to finish"
              , "You fail to relax."
              , "You failed to find anything to do with that."
              , "You add"
              ]

parseEndMessages :: Parser ()
parseEndMessages = void $ manyTill anyChar $ try $ foldl (<|>) mzero $ map (void . try . string) endMessages
