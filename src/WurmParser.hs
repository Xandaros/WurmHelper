module WurmParser
    ( parseEvent
    , EventMessage(ActionStart, ActionEnd, OtherMessage)
    ) where
--import Text.Parsec
import Text.ParserCombinators.Parsec

data EventMessage = ActionStart
                  | ActionEnd
                  | OtherMessage
                  deriving (Show)

parseEvent :: Parser EventMessage
parseEvent = try (parseTime >> (try actionStart <|> try actionEnd)) <|> return OtherMessage

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
actionStart = do
    string "After "
    action <- many $ noneOf " "
    string " you will start "
    string action
    string " again."
    return ActionStart

actionEnd :: Parser EventMessage
actionEnd = try createEnd

createEnd :: Parser EventMessage
createEnd = do
    string "You create a "
    many $ noneOf "."
    char '.'
    return ActionEnd
