module GoldenSpec (test_golden) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text.Lazy.Encoding as TL
import HS2Lazy
import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Pretty.Simple

inputFiles :: [FilePath]
inputFiles = unsafePerformIO (findByExtension [".hs"] ("examples" </> "apps"))

preludeIO :: IO String
preludeIO = BSL.toString <$> BSL.readFile ("examples" </> "libs" </> "hs2lazy-prelude.hs")

test_golden :: TestTree
test_golden =
  testGroup "Golden tests" $
    map createTests inputFiles
  where
    createTests inFile = unsafePerformIO $ do
      prelude <- preludeIO
      source <- BSL.readFile inFile
      let (_, _, p', e, _) = compile $ prelude ++ (BSL.toString source)
      let baseName = takeBaseName inFile
      pure $
        testGroup
          baseName
          [ goldenVsString
              "SKI output"
              (".golden" </> "lazy" </> baseName <.> "lazy")
              (pure $ BSL.fromString $ renderSKI e),
            goldenVsString
              "Expr output"
              (".golden" </> "expr" </> baseName <.> "expr")
              (pure $ TL.encodeUtf8 $ pShowNoColor p')
          ]
