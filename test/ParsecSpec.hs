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
      parseArgs "" `shouldBe` emptyArgTree
      parseArgs "bad-cmd" `shouldBe` emptyArgTree
      parseArgs "--bad-arg" `shouldBe` emptyArgTree

    it "should parse create args" $ do
      parseArgs "create" `shouldBe` emptyArgTree
      parseArgs "create desc"
        `shouldBe` ArgTree Cmd "create" 0 "desc" [] (ArgOpts False)
      parseArgs "create +tag desc"
        `shouldBe` ArgTree Cmd "create" 0 "desc" ["tag"] (ArgOpts False)
      parseArgs "create desc +tag"
        `shouldBe` ArgTree Cmd "create" 0 "desc" ["tag"] (ArgOpts False)
      parseArgs "add desc +tag"
        `shouldBe` ArgTree Cmd "create" 0 "desc" ["tag"] (ArgOpts False)
      parseArgs "add desc +tag --json"
        `shouldBe` ArgTree Cmd "create" 0 "desc" ["tag"] (ArgOpts True)
      parseArgs "add desc --bad-arg"
        `shouldBe` ArgTree Cmd "create" 0 "desc" [] (ArgOpts False)

    it "should parse update args" $ do
      parseArgs "update" `shouldBe` emptyArgTree
      parseArgs "update bad-id" `shouldBe` emptyArgTree
      parseArgs "update 1" `shouldBe` emptyArgTree
      parseArgs "update 2 desc"
        `shouldBe` ArgTree Cmd "update" 2 "desc" [] (ArgOpts False)
      parseArgs "update 3 4 +tag desc"
        `shouldBe` ArgTree Cmd "update" 3 "4 desc" ["tag"] (ArgOpts False)
      parseArgs "edit 100 +tag desc -tag"
        `shouldBe` ArgTree Cmd "update" 100 "desc" [] (ArgOpts False)
      parseArgs "edit 100 +tag desc -tag --json"
        `shouldBe` ArgTree Cmd "update" 100 "desc" [] (ArgOpts True)

    it "should parse replace args" $ do
      parseArgs "replace" `shouldBe` emptyArgTree
      parseArgs "replace bad-id" `shouldBe` emptyArgTree
      parseArgs "replace 1" `shouldBe` emptyArgTree
      parseArgs "replace 2 desc"
        `shouldBe` ArgTree Cmd "replace" 2 "desc" [] (ArgOpts False)
      parseArgs "replace 3 4 +tag desc"
        `shouldBe` ArgTree Cmd "replace" 3 "4 desc" ["tag"] (ArgOpts False)
      parseArgs "set 100 +tag desc"
        `shouldBe` ArgTree Cmd "replace" 100 "desc" ["tag"] (ArgOpts False)
      parseArgs "set 100 --json +tag desc"
        `shouldBe` ArgTree Cmd "replace" 100 "desc" ["tag"] (ArgOpts True)

    it "should parse start args" $ do
      parseArgs "start" `shouldBe` emptyArgTree
      parseArgs "start bad-id" `shouldBe` emptyArgTree
      parseArgs "start 1" `shouldBe` ArgTree Cmd "start" 1 "" [] (ArgOpts False)
      parseArgs "start --json 1" `shouldBe` emptyArgTree
      parseArgs "start 1 --json"
        `shouldBe` ArgTree Cmd "start" 1 "" [] (ArgOpts True)

    it "should parse stop args" $ do
      parseArgs "stop" `shouldBe` emptyArgTree
      parseArgs "stop bad-id" `shouldBe` emptyArgTree
      parseArgs "stop 1" `shouldBe` ArgTree Cmd "stop" 1 "" [] (ArgOpts False)
      parseArgs "stop 1 --json"
        `shouldBe` ArgTree Cmd "stop" 1 "" [] (ArgOpts True)

    it "should parse toggle args" $ do
      parseArgs "toggle" `shouldBe` emptyArgTree
      parseArgs "toggle bad-id" `shouldBe` emptyArgTree
      parseArgs "toggle 1"
        `shouldBe` ArgTree Cmd "toggle" 1 "" [] (ArgOpts False)
      parseArgs "toggle 1 --json"
        `shouldBe` ArgTree Cmd "toggle" 1 "" [] (ArgOpts True)

    it "should parse done args" $ do
      parseArgs "done" `shouldBe` emptyArgTree
      parseArgs "done bad-id" `shouldBe` emptyArgTree
      parseArgs "done 1" `shouldBe` ArgTree Cmd "done" 1 "" [] (ArgOpts False)
      parseArgs "done 1 --json"
        `shouldBe` ArgTree Cmd "done" 1 "" [] (ArgOpts True)

    it "should parse delete args" $ do
      parseArgs "delete" `shouldBe` emptyArgTree
      parseArgs "delete bad-id" `shouldBe` emptyArgTree
      parseArgs "delete 1"
        `shouldBe` ArgTree Cmd "delete" 1 "" [] (ArgOpts False)
      parseArgs "delete 1 --json"
        `shouldBe` ArgTree Cmd "delete" 1 "" [] (ArgOpts True)

    it "should parse remove args" $ do
      parseArgs "remove" `shouldBe` emptyArgTree
      parseArgs "remove bad-id" `shouldBe` emptyArgTree
      parseArgs "remove 1"
        `shouldBe` ArgTree Cmd "remove" 1 "" [] (ArgOpts False)
      parseArgs "remove 1 --json"
        `shouldBe` ArgTree Cmd "remove" 1 "" [] (ArgOpts True)

    it "should parse context args" $ do
      parseArgs "context"
        `shouldBe` ArgTree Cmd "context" 0 "" [] (ArgOpts False)
      parseArgs "context bad-tag"
        `shouldBe` ArgTree Cmd "context" 0 "" [] (ArgOpts False)
      parseArgs "context +tag"
        `shouldBe` ArgTree Cmd "context" 0 "" ["tag"] (ArgOpts False)
      parseArgs "ctx +tag +tag2"
        `shouldBe` ArgTree Cmd "context" 0 "" ["tag", "tag2"] (ArgOpts False)
      parseArgs "ctx +tag --json +tag2"
        `shouldBe` ArgTree Cmd "context" 0 "" ["tag", "tag2"] (ArgOpts True)

    it "should parse list args" $ do
      parseArgs "list" `shouldBe` ArgTree Qry "list" 0 "" [] (ArgOpts False)
      parseArgs "list bad-arg"
        `shouldBe` ArgTree Qry "list" 0 "" [] (ArgOpts False)
      parseArgs "list --bad-opt"
        `shouldBe` ArgTree Qry "list" 0 "" [] (ArgOpts False)
      parseArgs "list --json"
        `shouldBe` ArgTree Qry "list" 0 "" [] (ArgOpts True)

    it "should parse show args" $ do
      parseArgs "show" `shouldBe` emptyArgTree
      parseArgs "show bad-id" `shouldBe` emptyArgTree
      parseArgs "show 1" `shouldBe` ArgTree Qry "show" 1 "" [] (ArgOpts False)
      parseArgs "show 1 --json"
        `shouldBe` ArgTree Qry "show" 1 "" [] (ArgOpts True)

    it "should parse worktime args" $ do
      parseArgs "worktime"
        `shouldBe` ArgTree Qry "worktime" 0 "" [] (ArgOpts False)
      parseArgs "worktime bad-tag"
        `shouldBe` ArgTree Qry "worktime" 0 "" [] (ArgOpts False)
      parseArgs "worktime +tag +tag2"
        `shouldBe` ArgTree Qry "worktime" 0 "" ["tag", "tag2"] (ArgOpts False)
      parseArgs "worktime +tag --json +tag2"
        `shouldBe` ArgTree Qry "worktime" 0 "" ["tag", "tag2"] (ArgOpts True)

    it "should parse help args" $ do
      parseArgs "help" `shouldBe` ArgTree Qry "help" 0 "" [] (ArgOpts False)
      parseArgs "--help" `shouldBe` ArgTree Qry "help" 0 "" [] (ArgOpts False)
      parseArgs "-h" `shouldBe` ArgTree Qry "help" 0 "" [] (ArgOpts False)
