module ParsecSpec
  ( spec
  )
where

import           Test.Hspec

import           Parsec

spec :: Spec
spec = parallel $ do
  describe "Test args parser" $ do
    it "should parse empty args" $ do
      parseArgs "" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "bad-cmd" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "--bad-arg" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)

    it "should parse create args" $ do
      parseArgs "create" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "create desc"
        `shouldBe` ArgTree "create" 0 "desc" [] (ArgOpts False)
      parseArgs "create +tag desc"
        `shouldBe` ArgTree "create" 0 "desc" ["tag"] (ArgOpts False)
      parseArgs "create desc +tag"
        `shouldBe` ArgTree "create" 0 "desc" ["tag"] (ArgOpts False)
      parseArgs "add desc +tag"
        `shouldBe` ArgTree "create" 0 "desc" ["tag"] (ArgOpts False)
      parseArgs "add desc +tag --json"
        `shouldBe` ArgTree "create" 0 "desc" ["tag"] (ArgOpts True)
      parseArgs "add desc --bad-arg"
        `shouldBe` ArgTree "create" 0 "desc" [] (ArgOpts False)

    it "should parse update args" $ do
      parseArgs "update" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "update bad-id" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "update 1" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "update 2 desc"
        `shouldBe` ArgTree "update" 2 "desc" [] (ArgOpts False)
      parseArgs "update 3 4 +tag desc"
        `shouldBe` ArgTree "update" 3 "4 desc" ["tag"] (ArgOpts False)
      parseArgs "edit 100 +tag desc -tag"
        `shouldBe` ArgTree "update" 100 "desc" [] (ArgOpts False)
      parseArgs "edit 100 +tag desc -tag --json"
        `shouldBe` ArgTree "update" 100 "desc" [] (ArgOpts True)

    it "should parse replace args" $ do
      parseArgs "replace" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "replace bad-id" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "replace 1" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "replace 2 desc"
        `shouldBe` ArgTree "replace" 2 "desc" [] (ArgOpts False)
      parseArgs "replace 3 4 +tag desc"
        `shouldBe` ArgTree "replace" 3 "4 desc" ["tag"] (ArgOpts False)
      parseArgs "set 100 +tag desc"
        `shouldBe` ArgTree "replace" 100 "desc" ["tag"] (ArgOpts False)
      parseArgs "set 100 --json +tag desc"
        `shouldBe` ArgTree "replace" 100 "desc" ["tag"] (ArgOpts True)

    it "should parse start args" $ do
      parseArgs "start" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "start bad-id" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "start 1" `shouldBe` ArgTree "start" 1 "" [] (ArgOpts False)
      parseArgs "start --json 1" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "start 1 --json"
        `shouldBe` ArgTree "start" 1 "" [] (ArgOpts True)

    it "should parse stop args" $ do
      parseArgs "stop" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "stop bad-id" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "stop 1" `shouldBe` ArgTree "stop" 1 "" [] (ArgOpts False)
      parseArgs "stop 1 --json" `shouldBe` ArgTree "stop" 1 "" [] (ArgOpts True)

    it "should parse toggle args" $ do
      parseArgs "toggle" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "toggle bad-id" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "toggle 1" `shouldBe` ArgTree "toggle" 1 "" [] (ArgOpts False)
      parseArgs "toggle 1 --json"
        `shouldBe` ArgTree "toggle" 1 "" [] (ArgOpts True)

    it "should parse done args" $ do
      parseArgs "done" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "done bad-id" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "done 1" `shouldBe` ArgTree "done" 1 "" [] (ArgOpts False)
      parseArgs "done 1 --json" `shouldBe` ArgTree "done" 1 "" [] (ArgOpts True)

    it "should parse delete args" $ do
      parseArgs "delete" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "delete bad-id" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "delete 1" `shouldBe` ArgTree "delete" 1 "" [] (ArgOpts False)
      parseArgs "delete 1 --json"
        `shouldBe` ArgTree "delete" 1 "" [] (ArgOpts True)

    it "should parse remove args" $ do
      parseArgs "remove" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "remove bad-id" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "remove 1" `shouldBe` ArgTree "remove" 1 "" [] (ArgOpts False)
      parseArgs "remove 1 --json"
        `shouldBe` ArgTree "remove" 1 "" [] (ArgOpts True)

    it "should parse context args" $ do
      parseArgs "context" `shouldBe` ArgTree "context" 0 "" [] (ArgOpts False)
      parseArgs "context bad-tag"
        `shouldBe` ArgTree "context" 0 "" [] (ArgOpts False)
      parseArgs "context +tag"
        `shouldBe` ArgTree "context" 0 "" ["tag"] (ArgOpts False)
      parseArgs "ctx +tag +tag2"
        `shouldBe` ArgTree "context" 0 "" ["tag", "tag2"] (ArgOpts False)
      parseArgs "ctx +tag --json +tag2"
        `shouldBe` ArgTree "context" 0 "" ["tag", "tag2"] (ArgOpts True)

    it "should parse list args" $ do
      parseArgs "list" `shouldBe` ArgTree "list" 0 "" [] (ArgOpts False)
      parseArgs "list bad-arg" `shouldBe` ArgTree "list" 0 "" [] (ArgOpts False)
      parseArgs "list --bad-opt"
        `shouldBe` ArgTree "list" 0 "" [] (ArgOpts False)
      parseArgs "list --json" `shouldBe` ArgTree "list" 0 "" [] (ArgOpts True)

    it "should parse show args" $ do
      parseArgs "show" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "show bad-id" `shouldBe` ArgTree "" 0 "" [] (ArgOpts False)
      parseArgs "show 1" `shouldBe` ArgTree "show" 1 "" [] (ArgOpts False)
      parseArgs "show 1 --json" `shouldBe` ArgTree "show" 1 "" [] (ArgOpts True)

    it "should parse worktime args" $ do
      parseArgs "worktime" `shouldBe` ArgTree "worktime" 0 "" [] (ArgOpts False)
      parseArgs "worktime bad-tag"
        `shouldBe` ArgTree "worktime" 0 "" [] (ArgOpts False)
      parseArgs "worktime +tag +tag2"
        `shouldBe` ArgTree "worktime" 0 "" ["tag", "tag2"] (ArgOpts False)
      parseArgs "worktime +tag --json +tag2"
        `shouldBe` ArgTree "worktime" 0 "" ["tag", "tag2"] (ArgOpts True)
