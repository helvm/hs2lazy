module GoldenSpec (test_golden) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS
import HS2Lazy
import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

inputFiles :: [FilePath]
inputFiles = unsafePerformIO (findByExtension [".hs"] ("examples" </> "apps"))

preludeIO :: IO String
preludeIO = LBS.toString <$> LBS.readFile ("examples" </> "libs" </> "hs2lazy-prelude.hs")

generateSKIFromBS :: LBS.ByteString -> IO LBS.ByteString
generateSKIFromBS source = do
  prelude <- preludeIO
  let src = LBS.toString source
  let ski = generateSKI $ prelude ++ src
  pure $ LBS.fromString $ ski

generateExprFromBS :: LBS.ByteString -> IO LBS.ByteString
generateExprFromBS source = do
  prelude <- preludeIO
  let src = LBS.toString source
  let expr = generateExpr $ prelude ++ src
  pure $ LBS.fromString $ expr

test_golden :: TestTree
test_golden =
  testGroup
    "Golden tests"
    [ testGroup
        "SKI output"
        [ goldenVsString
            (takeBaseName inFile)
            (".golden" </> "lazy" </> takeBaseName inFile <.> "lazy")
            (generateSKIFromBS =<< LBS.readFile inFile)
        | inFile <- inputFiles
        ]
    , testGroup
        "Expr output"
        [ goldenVsString
            (takeBaseName inFile)
            (".golden" </> "expr" </> takeBaseName inFile <.> "expr")
            (generateExprFromBS =<< LBS.readFile inFile)
        | inFile <- inputFiles
        ]
    ]
