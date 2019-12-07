module CLI where

import AI
import qualified Data.ByteString as B
import Grenade (Gradients)
import Grenade.Exts.Adam
import Options.Applicative


-- FilePath: Read AIState from file
-- True: Read AIState from stdin
-- False: Random AIState
type AISpec = Either FilePath Bool

parseAISpec :: AISpec -> IO AIState
parseAISpec (Left f) = (either fail pure . parseAI) =<< B.readFile f
parseAISpec (Right True) = (either fail pure . parseAI ) =<< B.getContents
parseAISpec (Right False) = defaultState

data Command = Run AISpec String | Simulate AISpec Bool | Train (Adam (Gradients NL)) AISpec Bool (Maybe FilePath)

aiFileOpt :: Parser FilePath 
aiFileOpt = strOption $ opts
    where opts = short 'f'
              <> long "file"
              <> help "Load the AI from the specified file."
              <> metavar "FILE"

aiStdFlag :: Parser Bool
aiStdFlag = flag False True opts
    where opts = short 'i'
              <> long "stdin"
              <> help "Read the AI from stdin."

aiSpecOpt :: Parser AISpec
aiSpecOpt = fmap Left aiFileOpt <|> fmap Right aiStdFlag


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

alphaOpt :: Parser Double
alphaOpt = option auto $ opts
    where opts = short 'a'
              <> long "alpha"
              <> help "Adam's learning rate"
              <> value 0.001
              <> metavar "LR"

beta1Opt :: Parser Double
beta1Opt = option auto $ opts
    where opts = short 'b'
              <> long "beta1"
              <> help "Adam's running average coefficient for the gradient"
              <> value 0.9
              <> metavar "C"
beta2Opt :: Parser Double
beta2Opt = option auto $ opts
    where opts = short 'B'
              <> long "beta2"
              <> help "Adam's running average coefficient for the gradient squared"
              <> value 0.999
              <> metavar "C"

epsOpt :: Parser Double
epsOpt = option auto $ opts
    where opts = short 'e'
              <> long "epsilon"
              <> help "Term added to increase numerical stability"
              <> value 1e-8
              <> metavar "C"

adamP :: Parser (Adam (Gradients NL))
adamP = Adam <$> fmap rtf alphaOpt <*> fmap rtf beta1Opt <*> fmap rtf beta2Opt <*> fmap rtf epsOpt <*> pure (rtf 0) <*> pure (rtf 0) <*> pure 0
    where rtf = realToFrac

aiOutFileOpt :: Parser (Maybe FilePath)
aiOutFileOpt = optional . strOption $ opts
    where opts = short 'o'
              <> long "out"
              <> help "Which file to save the trained AI to"
              <> metavar "FILE"

parserInfo :: ParserInfo Command
parserInfo = info (helper <*> (parser <|> runP)) (progDesc "jstris-ai manages an AI that can play jstris, an online, multiplayer version of Tetris found at https://jstris.jezevec10.com/.")
    where parser = hsubparser . mconcat . fmap command' $
            [ ("run", "Run the AI online.", runP)
            , ("simulate", "Run the AI locally.", simP)
            , ("train", "Train a new AI.", trainP)
            ]
          runP   = Run <$> aiSpecOpt <*> urlOpt
          simP   = Simulate <$> aiSpecOpt <*> verboseFlag
          trainP = Train <$> adamP <*> aiSpecOpt <*> verboseFlag <*> aiOutFileOpt
          command' (n,d,p) = command n . info p . progDesc $ d


processCLI :: IO Command
processCLI = execParser parserInfo
