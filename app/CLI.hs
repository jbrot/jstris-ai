module CLI where

import Moo.GeneticAlgorithm.Continuous (Population)
import Options.Applicative

import AI

data Command = Run (Maybe AIState) String | Simulate (Maybe AIState) Bool | Train (Either Bool (Population Double))

aisOpt :: Parser (Maybe AIState)
aisOpt = optional . fmap (listToAI) . option auto $ opts
    where opts = short 'a'
              <> long "ai"
              <> help "Specify how to weight the AI's neural network. This should be parseable as [Double]. Defaults to random weights."
              <> metavar "WEIGHTS"

urlOpt :: Parser String
urlOpt = fmap ("https://jstris.jezevec10.com/" <>) . strOption $ opts
    where opts = short 'g'
              <> long "game"
              <> help "The specific game on jstris to join, as a subdomain of https://jstris.jezevec10.com/. For instance, to play Cheese Race you would specify '?play=3&mode=1'"
              <> value ""
              <> metavar "PATH"

verboseFlag :: Parser Bool
verboseFlag = flag False True opts
    where opts = short 'v'
              <> long "verbose"
              <> help "Whether to print out the game state after each tick."

popCliOpt :: Parser (Population Double)
popCliOpt = option auto $ opts
    where opts = short 'p'
              <> long "population"
              <> help "Specify the population to start the training with."
              <> metavar "POPULATION"

popStdFlag :: Parser Bool
popStdFlag = flag False True opts
    where opts = short 'i'
              <> long "stdin"
              <> help "Read the starting population from stdin."


parserInfo :: ParserInfo Command
parserInfo = info (helper <*> (parser <|> runP)) (progDesc "jstris-ai manages an AI that can play jstris, an online, multiplayer version of Tetris found at https://jstris.jezevec10.com/.")
    where parser = hsubparser . mconcat . fmap command' $
            [ ("run", "Run the AI online.", runP)
            , ("simulate", "Run the AI locally.", simP)
            , ("train", "Train a new AI.", trainP)
            ]
          runP   = Run <$> aisOpt <*> urlOpt
          simP   = Simulate <$> aisOpt <*> verboseFlag
          trainP = Train <$> (fmap Right popCliOpt <|> fmap Left popStdFlag)
          command' (n,d,p) = command n . info p . progDesc $ d


processCLI :: IO Command
processCLI = execParser parserInfo
