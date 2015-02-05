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
    action <- manyTill anyChar $ string "you will start "
    return ActionRepeat

actionEnd :: Parser EventMessage
actionEnd = parseEndMessages >> return ActionEnd

endMessages :: [String]
endMessages = [ "You create a"
              , "You repair the"
              , "You almost made it"
              , "You attach"
              , "You push"
              , "You pull"
              , "You chip away"
              , "You cut down"
              ]

parseEndMessages :: Parser ()
parseEndMessages = foldl (<|>) mzero $ map (void . try . string) endMessages
