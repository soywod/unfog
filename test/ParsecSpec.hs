module ParsecSpec
  ( spec
  )
where

import           Test.Hspec

import           Parsec

spec :: Spec
spec = parallel $ do
  describe "Test args parser" $ do
    it "invalid cmd" $ do
      parseArgs "" `shouldBe` emptyArgTree
      parseArgs "bad-cmd" `shouldBe` emptyArgTree
      parseArgs "--bad-arg" `shouldBe` emptyArgTree

    it "invalid create" $ do
      parseArgs "create" `shouldBe` emptyArgTree

    it "create desc with - hyphen" $ do
      parseArgs "create desc with - hyphen" `shouldBe` emptyArgTree
        { _cmd  = "create"
        , _desc = "desc with - hyphen"
        }

    it "create +tag desc +tag2 desc2" $ do
      parseArgs "create +tag desc +tag2 desc2" `shouldBe` emptyArgTree
        { _cmd  = "create"
        , _desc = "desc desc2"
        , _tags = ["tag", "tag2"]
        }

    it "add +tag desc --json +tag2 desc2 --bad-arg" $ do
      parseArgs "add +tag desc --json +tag2 desc2 --bad-arg"
        `shouldBe` emptyArgTree { _cmd  = "create"
                                , _desc = "desc desc2 --bad-arg"
                                , _tags = ["tag", "tag2"]
                                , _opts = ArgOpts True
                                }

    it "invalid update" $ do
      parseArgs "update" `shouldBe` emptyArgTree
      parseArgs "update bad-id" `shouldBe` emptyArgTree
      parseArgs "update 1" `shouldBe` emptyArgTree

    it "update 1 desc" $ do
      parseArgs "update 1 desc"
        `shouldBe` emptyArgTree { _cmd = "update", _id = 1, _desc = "desc" }

    it "update 2 +tag desc +tag2 desc2 --json" $ do
      parseArgs "update 2 +tag desc +tag2 desc2 --json" `shouldBe` emptyArgTree
        { _cmd  = "update"
        , _id   = 2
        , _desc = "desc desc2"
        , _tags = ["tag", "tag2"]
        , _opts = ArgOpts True
        }

    it "edit 2 +tag desc +tag2 desc2 -tag" $ do
      parseArgs "edit 2 +tag desc +tag2 desc2 -tag" `shouldBe` emptyArgTree
        { _cmd  = "update"
        , _id   = 2
        , _desc = "desc desc2"
        , _tags = ["tag2"]
        }

    it "invalid replace" $ do
      parseArgs "replace" `shouldBe` emptyArgTree
      parseArgs "replace bad-id" `shouldBe` emptyArgTree
      parseArgs "replace 1" `shouldBe` emptyArgTree

    it "replace 1 desc" $ do
      parseArgs "replace 1 desc"
        `shouldBe` emptyArgTree { _cmd = "replace", _id = 1, _desc = "desc" }

    it "replace 2 +tag desc +tag2 desc2 --json" $ do
      parseArgs "replace 2 +tag desc +tag2 desc2 --json" `shouldBe` emptyArgTree
        { _cmd  = "replace"
        , _id   = 2
        , _desc = "desc desc2"
        , _tags = ["tag", "tag2"]
        , _opts = ArgOpts True
        }

    it "set 2 +tag desc +tag2 desc2 -tag" $ do
      parseArgs "set 2 +tag desc +tag2 desc2 -tag" `shouldBe` emptyArgTree
        { _cmd  = "replace"
        , _id   = 2
        , _desc = "desc desc2 -tag"
        , _tags = ["tag", "tag2"]
        }

    it "invalid start" $ do
      parseArgs "start" `shouldBe` emptyArgTree
      parseArgs "start bad-id" `shouldBe` emptyArgTree

    it "start 1" $ do
      parseArgs "start 1" `shouldBe` emptyArgTree { _cmd = "start", _id = 1 }
      parseArgs "start 1 --json" `shouldBe` emptyArgTree { _cmd  = "start"
                                                         , _id   = 1
                                                         , _opts = ArgOpts True
                                                         }

    it "invalid stop" $ do
      parseArgs "stop" `shouldBe` emptyArgTree
      parseArgs "stop bad-id" `shouldBe` emptyArgTree

    it "stop 1" $ do
      parseArgs "stop 1" `shouldBe` emptyArgTree { _cmd = "stop", _id = 1 }
      parseArgs "stop 1 --json" `shouldBe` emptyArgTree { _cmd  = "stop"
                                                        , _id   = 1
                                                        , _opts = ArgOpts True
                                                        }

    it "invalid toggle" $ do
      parseArgs "toggle" `shouldBe` emptyArgTree
      parseArgs "toggle bad-id" `shouldBe` emptyArgTree

    it "toggle 1" $ do
      parseArgs "toggle 1" `shouldBe` emptyArgTree { _cmd = "toggle", _id = 1 }
      parseArgs "toggle 1 --json" `shouldBe` emptyArgTree { _cmd  = "toggle"
                                                          , _id   = 1
                                                          , _opts = ArgOpts True
                                                          }

    it "invalid done" $ do
      parseArgs "done" `shouldBe` emptyArgTree
      parseArgs "done bad-id" `shouldBe` emptyArgTree

    it "done 1" $ do
      parseArgs "done 1" `shouldBe` emptyArgTree { _cmd = "done", _id = 1 }
      parseArgs "done 1 --json" `shouldBe` emptyArgTree { _cmd  = "done"
                                                        , _id   = 1
                                                        , _opts = ArgOpts True
                                                        }

    it "invalid delete" $ do
      parseArgs "delete" `shouldBe` emptyArgTree
      parseArgs "delete bad-id" `shouldBe` emptyArgTree

    it "delete 1" $ do
      parseArgs "delete 1" `shouldBe` emptyArgTree { _cmd = "delete", _id = 1 }
      parseArgs "delete 1 --json" `shouldBe` emptyArgTree { _cmd  = "delete"
                                                          , _id   = 1
                                                          , _opts = ArgOpts True
                                                          }

    it "invalid remove" $ do
      parseArgs "remove" `shouldBe` emptyArgTree
      parseArgs "remove bad-id" `shouldBe` emptyArgTree

    it "remove 1" $ do
      parseArgs "remove 1" `shouldBe` emptyArgTree { _cmd = "remove", _id = 1 }
      parseArgs "remove 1 --json" `shouldBe` emptyArgTree { _cmd  = "remove"
                                                          , _id   = 1
                                                          , _opts = ArgOpts True
                                                          }

    it "clear context" $ do
      parseArgs "context" `shouldBe` emptyArgTree { _cmd = "context" }
      parseArgs "context bad-tag" `shouldBe` emptyArgTree { _cmd = "context" }

    it "context +tag" $ do
      parseArgs "context +tag +tag2"
        `shouldBe` emptyArgTree { _cmd = "context", _tags = ["tag", "tag2"] }

    it "ctx +tag +tag2 --json" $ do
      parseArgs "ctx +tag +tag2"
        `shouldBe` emptyArgTree { _cmd = "context", _tags = ["tag", "tag2"] }

    it "list" $ do
      parseArgs "list" `shouldBe` emptyArgTree { _type = Qry, _cmd = "list" }
      parseArgs "list --json" `shouldBe` emptyArgTree { _type = Qry
                                                      , _cmd  = "list"
                                                      , _opts = ArgOpts True
                                                      }

    it "invalid show" $ do
      parseArgs "show" `shouldBe` emptyArgTree
      parseArgs "show bad-id" `shouldBe` emptyArgTree

    it "show 1" $ do
      parseArgs "show 1"
        `shouldBe` emptyArgTree { _type = Qry, _cmd = "show", _id = 1 }
      parseArgs "show 1 --json" `shouldBe` emptyArgTree { _type = Qry
                                                        , _cmd  = "show"
                                                        , _id   = 1
                                                        , _opts = ArgOpts True
                                                        }

    it "worktime" $ do
      parseArgs "worktime"
        `shouldBe` emptyArgTree { _type = Qry, _cmd = "worktime" }

    it "worktime +tag +tag2" $ do
      parseArgs "worktime +tag +tag2" `shouldBe` emptyArgTree
        { _type = Qry
        , _cmd  = "worktime"
        , _tags = ["tag", "tag2"]
        }

    it "worktime +tag +tag2 --json" $ do
      parseArgs "worktime +tag +tag2 --json" `shouldBe` emptyArgTree
        { _type = Qry
        , _cmd  = "worktime"
        , _tags = ["tag", "tag2"]
        , _opts = ArgOpts True
        }

    it "wtime <01234" $ do
      parseArgs "worktime [10" `shouldBe` emptyArgTree
        { _type    = Qry
        , _cmd     = "worktime"
        , _minDate = Just $ ArgDate 10 0 0 0 0
        }

    it "wtime <:20" $ do
      parseArgs "worktime [:20" `shouldBe` emptyArgTree
        { _type    = Qry
        , _cmd     = "worktime"
        , _minDate = Just $ ArgDate 0 0 0 20 0
        }

    it "wtime <10:20" $ do
      parseArgs "worktime [10:20" `shouldBe` emptyArgTree
        { _type    = Qry
        , _cmd     = "worktime"
        , _minDate = Just $ ArgDate 10 0 0 20 0
        }

    it "help" $ do
      parseArgs "help" `shouldBe` emptyArgTree { _type = Qry, _cmd = "help" }
      parseArgs "h" `shouldBe` emptyArgTree { _type = Qry, _cmd = "help" }
      parseArgs "--help" `shouldBe` emptyArgTree { _type = Qry, _cmd = "help" }
      parseArgs "-h" `shouldBe` emptyArgTree { _type = Qry, _cmd = "help" }
