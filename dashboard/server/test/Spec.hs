import Test.Hspec

import qualified Test.OverTheFinishLine.Dashboard.GitHub
import qualified Test.OverTheFinishLine.Dashboard.Lists
import qualified Test.OverTheFinishLine.Dashboard.Model

main :: IO ()
main = hspec $ do
  Test.OverTheFinishLine.Dashboard.GitHub.spec
  Test.OverTheFinishLine.Dashboard.Lists.spec
  Test.OverTheFinishLine.Dashboard.Model.spec
