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
actionStart = string "You start" >> return ActionStart

actionRepeat :: Parser EventMessage
actionRepeat = do
    string "After "
    action <- many $ noneOf " "
    string " you will start "
    string action
    string " again."
    return ActionRepeat

actionEnd :: Parser EventMessage
actionEnd = (try createEnd <|> try repairEnd) >> return ActionEnd

createEnd :: Parser ()
createEnd = void $ string "You create a"

repairEnd :: Parser ()
repairEnd = void $ string "You repair the"
