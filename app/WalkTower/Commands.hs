{-# LANGUAGE OverloadedStrings #-}

module WalkTower.Commands
  ( Command(..)
  , parseCommand
  ) where

import Data.Map qualified as M

data Command
    = CommandAttack [String]
    | CommandFlee
    | CommandDefend
    | CommandUseItem [String]
    | CommandSteal [String]
    | CommandQuit
    | CommandHelp [String]
    deriving (Show, Eq)


validCommands :: [String]
validCommands =
    [ "attack"
    , "flee"
    , "defend"
    , "use"
    , "steal"
    , "quit"
    , "exit"
    , "help"
    ]

-- | A map of all command prefixes to the full command name (ie: "at" -> "attack")
commandMap :: M.Map String String
commandMap = M.fromList [(take i cmd, cmd) | cmd <- validCommands, i <- [1 .. length cmd]]


parseCommand :: String -> Maybe Command
parseCommand msg = do
    let tokens = words msg
    case tokens of
        (cmd:xs) ->
            case M.lookup cmd commandMap of
                Just "attack" -> Just (CommandAttack xs)
                Just "flee"   -> Just CommandFlee
                Just "defend" -> Just CommandDefend
                Just "use"    -> Just (CommandUseItem xs)
                Just "steal"  -> Just (CommandSteal xs)
                Just "exit"   -> Just CommandQuit
                Just "quit"   -> Just CommandQuit
                Just "help"   -> Just (CommandHelp xs)
                _             -> Nothing
        _ -> Nothing
