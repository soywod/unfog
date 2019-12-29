module ParsecSpec
  ( spec
  )
where

import           Test.Hspec

import           Parsec

spec :: Spec
spec = parallel $ do
  it "cmd expr" $ do
    parseArgs "" `shouldBe` emptyArgTree
    parseArgs "bad-cmd" `shouldBe` emptyArgTree

  describe "create expr" $ do
    it "invalid args" $ do
      parseArgs "create" `shouldBe` emptyArgTree
      parseArgs "create --json" `shouldBe` emptyArgTree

    it "with desc" $ do
      parseArgs "create desc1 desc2    desc3" `shouldBe` emptyArgTree
        { _cmd  = "create"
        , _desc = "desc1 desc2 desc3"
        }
      parseArgs "add + :desc [desc desc{}" `shouldBe` emptyArgTree
        { _cmd  = "create"
        , _desc = "+ :desc [desc desc{}"
        }

    it "with tags" $ do
      parseArgs "create +tag1 +tag2"
        `shouldBe` emptyArgTree { _cmd = "create", _tags = ["tag1", "tag2"] }
      parseArgs "add +tag1   -tag1  +tag2" `shouldBe` emptyArgTree
        { _cmd  = "create"
        , _desc = "-tag1"
        , _tags = ["tag1", "tag2"]
        }

    it "with due" $ do
      parseArgs "create  :10:20   " `shouldBe` emptyArgTree
        { _cmd = "create"
        , _due = Just $ ArgDate 10 0 0 20 0
        }
      parseArgs "add    ::20" `shouldBe` emptyArgTree
        { _cmd = "create"
        , _due = Just $ ArgDate 0 0 0 20 0
        }

    it "with different order" $ do
      let expectedTree = emptyArgTree { _cmd  = "create"
                                      , _desc = "desc"
                                      , _tags = ["tag"]
                                      , _due  = Just $ ArgDate 0 0 0 20 0
                                      , _opts = ArgOpts { _json = True }
                                      }
      parseArgs "create   desc +tag ::20 --json" `shouldBe` expectedTree
      parseArgs "create +tag   desc --json ::20" `shouldBe` expectedTree
      parseArgs "add ::20 --json   desc +tag" `shouldBe` expectedTree

  describe "update expr" $ do
    it "invalid args" $ do
      parseArgs "update" `shouldBe` emptyArgTree
      parseArgs "update bad-id" `shouldBe` emptyArgTree
      parseArgs "update 1" `shouldBe` emptyArgTree

    it "with desc" $ do
      parseArgs "update 1 desc"
        `shouldBe` emptyArgTree { _cmd = "update", _id = 1, _desc = "desc" }

    it "with tags" $ do
      parseArgs "update  1   +tag"
        `shouldBe` emptyArgTree { _cmd = "update", _id = 1, _tags = ["tag"] }
      parseArgs "edit 2   +tag -tag   +tag2"
        `shouldBe` emptyArgTree { _cmd = "update", _id = 2, _tags = ["tag2"] }

    it "with due" $ do
      parseArgs "edit   2 :10:20 " `shouldBe` emptyArgTree
        { _cmd = "update"
        , _id  = 2
        , _due = Just $ ArgDate 10 0 0 20 0
        }

    it "with opt" $ do
      parseArgs "update 1 desc +tag --json" `shouldBe` emptyArgTree
        { _cmd  = "update"
        , _desc = "desc"
        , _tags = ["tag"]
        , _id   = 1
        , _opts = ArgOpts True
        }

  describe "replace expr" $ do
    it "invalid args" $ do
      parseArgs "replace" `shouldBe` emptyArgTree
      parseArgs "replace bad-id" `shouldBe` emptyArgTree
      parseArgs "replace 1" `shouldBe` emptyArgTree
      parseArgs "replace 1 --json" `shouldBe` emptyArgTree

    it "with desc" $ do
      parseArgs "replace 1 desc"
        `shouldBe` emptyArgTree { _cmd = "replace", _id = 1, _desc = "desc" }

    it "with tags" $ do
      parseArgs "replace  1   +tag"
        `shouldBe` emptyArgTree { _cmd = "replace", _id = 1, _tags = ["tag"] }
      parseArgs "set 2   +tag -tag   +tag2" `shouldBe` emptyArgTree
        { _cmd  = "replace"
        , _id   = 2
        , _desc = "-tag"
        , _tags = ["tag", "tag2"]
        }

    it "with due" $ do
      parseArgs "set   2 :10:20 " `shouldBe` emptyArgTree
        { _cmd = "replace"
        , _id  = 2
        , _due = Just $ ArgDate 10 0 0 20 0
        }

    it "with opt" $ do
      parseArgs "replace 1 desc +tag --json" `shouldBe` emptyArgTree
        { _cmd  = "replace"
        , _desc = "desc"
        , _tags = ["tag"]
        , _id   = 1
        , _opts = ArgOpts True
        }

  describe "start expr" $ do
    it "invalid args" $ do
      parseArgs "start" `shouldBe` emptyArgTree
      parseArgs "start bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "start 1" `shouldBe` emptyArgTree { _cmd = "start", _id = 1 }
      parseArgs "start 1 --json" `shouldBe` emptyArgTree { _cmd  = "start"
                                                         , _id   = 1
                                                         , _opts = ArgOpts True
                                                         }

  describe "stop expr" $ do
    it "invalid args" $ do
      parseArgs "stop" `shouldBe` emptyArgTree
      parseArgs "stop bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "stop 1" `shouldBe` emptyArgTree { _cmd = "stop", _id = 1 }
      parseArgs "stop 1 --json" `shouldBe` emptyArgTree { _cmd  = "stop"
                                                        , _id   = 1
                                                        , _opts = ArgOpts True
                                                        }

  describe "toggle expr" $ do
    it "invalid args" $ do
      parseArgs "toggle" `shouldBe` emptyArgTree
      parseArgs "toggle bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "toggle 1" `shouldBe` emptyArgTree { _cmd = "toggle", _id = 1 }
      parseArgs "toggle 1 --json" `shouldBe` emptyArgTree { _cmd  = "toggle"
                                                          , _id   = 1
                                                          , _opts = ArgOpts True
                                                          }

  describe "done expr" $ do
    it "invalid args" $ do
      parseArgs "done" `shouldBe` emptyArgTree
      parseArgs "done bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "done 1" `shouldBe` emptyArgTree { _cmd = "done", _id = 1 }
      parseArgs "done 1 --json" `shouldBe` emptyArgTree { _cmd  = "done"
                                                        , _id   = 1
                                                        , _opts = ArgOpts True
                                                        }

  describe "delete expr" $ do
    it "invalid args" $ do
      parseArgs "delete" `shouldBe` emptyArgTree
      parseArgs "delete bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "delete 1" `shouldBe` emptyArgTree { _cmd = "delete", _id = 1 }
      parseArgs "delete 1 --json" `shouldBe` emptyArgTree { _cmd  = "delete"
                                                          , _id   = 1
                                                          , _opts = ArgOpts True
                                                          }

  describe "remove expr" $ do
    it "invalid args" $ do
      parseArgs "remove" `shouldBe` emptyArgTree
      parseArgs "remove bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "remove 1" `shouldBe` emptyArgTree { _cmd = "remove", _id = 1 }
      parseArgs "remove 1 --json" `shouldBe` emptyArgTree { _cmd  = "remove"
                                                          , _id   = 1
                                                          , _opts = ArgOpts True
                                                          }

  describe "context expr" $ do
    it "clear context" $ do
      parseArgs "context" `shouldBe` emptyArgTree { _cmd = "context" }

    it "with tags" $ do
      parseArgs "context +tag1 +tag2 tag3" `shouldBe` emptyArgTree
        { _cmd  = "context"
        , _tags = ["tag1", "tag2", "tag3"]
        }

    it "with opt" $ do
      parseArgs "ctx +tag1 tag2 --json" `shouldBe` emptyArgTree
        { _cmd  = "context"
        , _tags = ["tag1", "tag2"]
        , _opts = ArgOpts True
        }

  describe "list expr" $ do
    it "with opt" $ do
      parseArgs "list  " `shouldBe` emptyArgTree { _type = Qry, _cmd = "list" }
      parseArgs "list   --json" `shouldBe` emptyArgTree { _type = Qry
                                                        , _cmd  = "list"
                                                        , _opts = ArgOpts True
                                                        }

  describe "show expr" $ do
    it "invalid args" $ do
      parseArgs "show" `shouldBe` emptyArgTree
      parseArgs "show bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "show 1"
        `shouldBe` emptyArgTree { _type = Qry, _cmd = "show", _id = 1 }
      parseArgs "show 1   --json" `shouldBe` emptyArgTree { _type = Qry
                                                          , _cmd  = "show"
                                                          , _id   = 1
                                                          , _opts = ArgOpts True
                                                          }

  describe "worktime expr" $ do
    it "global" $ do
      parseArgs "worktime"
        `shouldBe` emptyArgTree { _type = Qry, _cmd = "worktime" }

    it "with tags" $ do
      parseArgs "worktime tag +tag2" `shouldBe` emptyArgTree
        { _type = Qry
        , _cmd  = "worktime"
        , _tags = ["tag", "tag2"]
        }

    it "with opts" $ do
      parseArgs "worktime +tag tag2 --json" `shouldBe` emptyArgTree
        { _type = Qry
        , _cmd  = "worktime"
        , _tags = ["tag", "tag2"]
        , _opts = ArgOpts True
        }

    it "with min date" $ do
      parseArgs "wtime [10 tag" `shouldBe` emptyArgTree
        { _type    = Qry
        , _cmd     = "worktime"
        , _tags    = ["tag"]
        , _minDate = Just $ ArgDate 10 0 0 0 0
        }

    it "with min time" $ do
      parseArgs "wtime tag [:20" `shouldBe` emptyArgTree
        { _type    = Qry
        , _cmd     = "worktime"
        , _tags    = ["tag"]
        , _minDate = Just $ ArgDate 0 0 0 20 0
        }

    it "with min datetime" $ do
      parseArgs "wtime [10:20 tag" `shouldBe` emptyArgTree
        { _type    = Qry
        , _cmd     = "worktime"
        , _tags    = ["tag"]
        , _minDate = Just $ ArgDate 10 0 0 20 0
        }

    it "with max date" $ do
      parseArgs "wtime tag ]10" `shouldBe` emptyArgTree
        { _type    = Qry
        , _cmd     = "worktime"
        , _tags    = ["tag"]
        , _maxDate = Just $ ArgDate 10 0 0 0 0
        }

    it "with max time" $ do
      parseArgs "wtime ]:20 tag" `shouldBe` emptyArgTree
        { _type    = Qry
        , _cmd     = "worktime"
        , _tags    = ["tag"]
        , _maxDate = Just $ ArgDate 0 0 0 20 0
        }

    it "with max datetime" $ do
      parseArgs "wtime tag ]10:20" `shouldBe` emptyArgTree
        { _type    = Qry
        , _cmd     = "worktime"
        , _tags    = ["tag"]
        , _maxDate = Just $ ArgDate 10 0 0 20 0
        }

    it "with min and max datetimes" $ do
      parseArgs "wtime [10:20 ]14 tag" `shouldBe` emptyArgTree
        { _type    = Qry
        , _cmd     = "worktime"
        , _tags    = ["tag"]
        , _minDate = Just $ ArgDate 10 0 0 20 0
        , _maxDate = Just $ ArgDate 14 0 0 0 0
        }

  describe "help expr" $ do
    it "with aliases" $ do
      parseArgs "help" `shouldBe` emptyArgTree { _type = Qry, _cmd = "help" }
      parseArgs "h" `shouldBe` emptyArgTree { _type = Qry, _cmd = "help" }
      parseArgs "--help" `shouldBe` emptyArgTree { _type = Qry, _cmd = "help" }
      parseArgs "-h" `shouldBe` emptyArgTree { _type = Qry, _cmd = "help" }
