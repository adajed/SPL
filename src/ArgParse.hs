module ArgParse where

data Args = Args { showTree :: Bool
                 , saveIR :: Bool
                 , filepaths :: [String]
                 }

startArgs :: Args
startArgs = Args { showTree = False
                 , saveIR = False
                 , filepaths = [] }

parseArgs :: [String] -> Args
parseArgs args = parseArgs' args startArgs
    where parseArgs' :: [String] -> Args -> Args
          parseArgs' [] args = args
          parseArgs' (x:xs) args = case x of
                                     "--show-tree" -> parseArgs' xs (args { showTree = True })
                                     "--write-ir" -> parseArgs' xs (args { saveIR = True })
                                     _ -> parseArgs' xs (args { filepaths = x:(filepaths args) })
