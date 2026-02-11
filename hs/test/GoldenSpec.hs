module GoldenSpec (test_golden) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text.Lazy.Encoding as TL
import HS2Lazy
import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

inputFiles :: [FilePath]
inputFiles = unsafePerformIO (findByExtension [".hs"] ("examples" </> "apps"))

preludeIO :: IO String
preludeIO = BSL.toString <$> BSL.readFile ("examples" </> "libs" </> "hs2lazy-prelude.hs")

generateSKIFromBS :: BSL.ByteString -> IO BSL.ByteString
generateSKIFromBS source = do
  prelude <- preludeIO
  let src = BSL.toString source
  let ski = generateSKI $ prelude ++ src
  pure $ BSL.fromString $ ski

generateExprFromBS :: BSL.ByteString -> IO BSL.ByteString
generateExprFromBS source = do
  prelude <- preludeIO
  let src = BSL.toString source
  let expr = generateExpr $ prelude ++ src
  pure $ TL.encodeUtf8 $ expr

test_golden :: TestTree
test_golden =
  testGroup
    "Golden tests"
    [ let inFileContentIO = BSL.readFile inFile
       in testGroup
            (takeBaseName inFile)
            [ goldenVsString
                "SKI output"
                (".golden" </> "lazy" </> takeBaseName inFile <.> "lazy")
                (inFileContentIO >>= generateSKIFromBS),
              goldenVsString
                "Expr output"
                (".golden" </> "expr" </> takeBaseName inFile <.> "expr")
                (inFileContentIO >>= generateExprFromBS)
            ]
    | inFile <- inputFiles
    ]
