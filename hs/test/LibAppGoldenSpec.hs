module LibAppGoldenSpec (test_golden) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text.Lazy.Encoding as TL
import HS2Lazy
import HS2Lazy.Compiler.Lambda
import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Pretty.Simple

inputFiles :: [FilePath]
inputFiles = unsafePerformIO (findByExtension [".hs"] ("examples" </> "apps"))

test_golden :: TestTree
test_golden =
  testGroup "Golden tests" $
    map createTests inputFiles
  where
    createTests inFile = unsafePerformIO $ do
      prelude <- BSL.readFile ("examples" </> "libs" </> "hs2lazy-prelude.hs")
      source <- BSL.readFile inFile
      let result = compile $ (BSL.toString prelude) ++ (BSL.toString source)
      let ((expandedSki, compiledSki), (optimizedExpr, expandedExpr, compiledExpr)) = result
      let lambda = compileToLambda optimizedExpr
      let baseName = takeBaseName inFile
      pure $
        testGroup
          baseName
          [ goldenVsString
              "SKI output"
              (".golden" </> "apps" </> "lazy" </> "expanded" </> baseName <.> "lazy")
              (pure $ BSL.fromString $ renderSKI expandedSki),
            goldenVsString
              "SKI output"
              (".golden" </> "apps" </> "lazy" </> "compiled" </> baseName <.> "lazy")
              (pure $ BSL.fromString $ renderSKI compiledSki),
            goldenVsString
              "Lambda output"
              (".golden" </> "apps" </> "lambda" </> baseName <.> "lambda")
              (pure $ TL.encodeUtf8 $ pShowNoColor lambda),
            goldenVsString
              "Expr output"
              (".golden" </> "apps" </> "expr" </> "optimized" </> baseName <.> "expr")
              (pure $ TL.encodeUtf8 $ pShowNoColor optimizedExpr),
            goldenVsString
              "Expr output"
              (".golden" </> "apps" </> "expr" </> "expanded" </> baseName <.> "expr")
              (pure $ TL.encodeUtf8 $ pShowNoColor expandedExpr),
            goldenVsString
              "Expr output"
              (".golden" </> "apps" </> "expr" </> "compiled" </> baseName <.> "expr")
              (pure $ TL.encodeUtf8 $ pShowNoColor compiledExpr)
          ]
