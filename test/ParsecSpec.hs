module ParsecSpec
  ( spec,
  )
where

import Parsec
import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "cmd expr" $ do
    parseArgs "" `shouldBe` emptyArgTree
    parseArgs "bad-cmd" `shouldBe` emptyArgTree

  describe "add expr" $ do
    it "invalid args" $ do
      parseArgs "add" `shouldBe` emptyArgTree
      parseArgs "add --json" `shouldBe` emptyArgTree

    it "with desc" $ do
      parseArgs "add desc1 desc2    desc3"
        `shouldBe` emptyArgTree {_cmd = "add", _type = Cmd, _desc = "desc1 desc2 desc3"}
      parseArgs "add + :desc [desc desc{}"
        `shouldBe` emptyArgTree
          { _cmd = "add",
            _type = Cmd,
            _desc = "+ :desc [desc desc{}"
          }

    it "with tags" $ do
      parseArgs "add +tag1 +tag2"
        `shouldBe` emptyArgTree {_cmd = "add", _type = Cmd, _tags = ["tag1", "tag2"]}
      parseArgs "add +tag1   -tag1  +tag2"
        `shouldBe` emptyArgTree
          { _cmd = "add",
            _type = Cmd,
            _desc = "-tag1",
            _tags = ["tag1", "tag2"]
          }

    it "with due" $ do
      parseArgs "add  :10:20   "
        `shouldBe` emptyArgTree
          { _cmd = "add",
            _type = Cmd,
            _due = Just $ ArgDate Rel 10 0 0 20 0
          }
      parseArgs "add    ::20"
        `shouldBe` emptyArgTree
          { _cmd = "add",
            _type = Cmd,
            _due = Just $ ArgDate Rel 0 0 0 20 0
          }

    it "with different order" $ do
      let expectedTree =
            emptyArgTree
              { _cmd = "add",
                _type = Cmd,
                _desc = "desc",
                _tags = ["tag"],
                _due = Just $ ArgDate Rel 0 0 0 20 0,
                _opts =
                  ArgOpts
                    { _json = True,
                      _more = False,
                      _onlyIds = False,
                      _onlyTags = False
                    }
              }
      parseArgs "a   desc +tag ::20 --json" `shouldBe` expectedTree
      parseArgs "a +tag   desc --json ::20" `shouldBe` expectedTree
      parseArgs "a ::20 --json   desc +tag" `shouldBe` expectedTree

  describe "edit expr" $ do
    it "invalid args" $ do
      parseArgs "edit" `shouldBe` emptyArgTree
      parseArgs "edit bad-id" `shouldBe` emptyArgTree
      parseArgs "e 1" `shouldBe` emptyArgTree

    it "with desc" $ do
      parseArgs "edit 1 desc"
        `shouldBe` emptyArgTree {_cmd = "edit", _type = Cmd, _ids = [1], _desc = "desc"}

    it "with tags" $ do
      parseArgs "edit  1   +tag"
        `shouldBe` emptyArgTree {_cmd = "edit", _type = Cmd, _ids = [1], _tags = ["tag"]}
      parseArgs "edit 2   +tag -tag   +tag2"
        `shouldBe` emptyArgTree
          { _cmd = "edit",
            _type = Cmd,
            _ids = [2],
            _tags = ["tag2"]
          }

    it "with due" $ do
      parseArgs "edit   2 :10:20 "
        `shouldBe` emptyArgTree
          { _cmd = "edit",
            _type = Cmd,
            _ids = [2],
            _due = Just $ ArgDate Rel 10 0 0 20 0
          }

    it "with opt" $ do
      parseArgs "e 1 desc +tag --json"
        `shouldBe` emptyArgTree
          { _cmd = "edit",
            _type = Cmd,
            _desc = "desc",
            _tags = ["tag"],
            _ids = [1],
            _opts = ArgOpts True False False False
          }

  describe "set expr" $ do
    it "invalid args" $ do
      parseArgs "set" `shouldBe` emptyArgTree
      parseArgs "set bad-id" `shouldBe` emptyArgTree
      parseArgs "s 1" `shouldBe` emptyArgTree
      parseArgs "s 1 --json" `shouldBe` emptyArgTree

    it "with desc" $ do
      parseArgs "set 1 desc"
        `shouldBe` emptyArgTree {_cmd = "set", _type = Cmd, _ids = [1], _desc = "desc"}

    it "with tags" $ do
      parseArgs "set  1   +tag"
        `shouldBe` emptyArgTree {_cmd = "set", _type = Cmd, _ids = [1], _tags = ["tag"]}
      parseArgs "s 2   +tag -tag   +tag2"
        `shouldBe` emptyArgTree
          { _cmd = "set",
            _type = Cmd,
            _ids = [2],
            _desc = "-tag",
            _tags = ["tag", "tag2"]
          }

    it "with due" $ do
      parseArgs "set   2 :10:20 "
        `shouldBe` emptyArgTree
          { _cmd = "set",
            _type = Cmd,
            _ids = [2],
            _due = Just $ ArgDate Rel 10 0 0 20 0
          }

    it "with opt" $ do
      parseArgs "set 1 desc +tag --json"
        `shouldBe` emptyArgTree
          { _cmd = "set",
            _type = Cmd,
            _desc = "desc",
            _tags = ["tag"],
            _ids = [1],
            _opts = ArgOpts True False False False
          }

  describe "start expr" $ do
    it "invalid args" $ do
      parseArgs "start" `shouldBe` emptyArgTree
      parseArgs "start bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "start 1" `shouldBe` emptyArgTree {_cmd = "start", _type = Cmd, _ids = [1]}
      parseArgs "start 1 2"
        `shouldBe` emptyArgTree {_cmd = "start", _type = Cmd, _ids = [1, 2]}
      parseArgs "+ 1 --json"
        `shouldBe` emptyArgTree
          { _cmd = "start",
            _type = Cmd,
            _ids = [1],
            _opts = ArgOpts True False False False
          }

  describe "stop expr" $ do
    it "invalid args" $ do
      parseArgs "stop" `shouldBe` emptyArgTree
      parseArgs "stop bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "stop 1" `shouldBe` emptyArgTree {_cmd = "stop", _type = Cmd, _ids = [1]}
      parseArgs "- 1 --json"
        `shouldBe` emptyArgTree
          { _cmd = "stop",
            _type = Cmd,
            _ids = [1],
            _opts = ArgOpts True False False False
          }

  describe "toggle expr" $ do
    it "invalid args" $ do
      parseArgs "toggle" `shouldBe` emptyArgTree
      parseArgs "toggle bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "toggle 1"
        `shouldBe` emptyArgTree {_cmd = "toggle", _type = Cmd, _ids = [1]}
      parseArgs "t 1 --json"
        `shouldBe` emptyArgTree
          { _cmd = "toggle",
            _type = Cmd,
            _ids = [1],
            _opts = ArgOpts True False False False
          }

  describe "done expr" $ do
    it "invalid args" $ do
      parseArgs "done" `shouldBe` emptyArgTree
      parseArgs "done bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "done 1" `shouldBe` emptyArgTree {_cmd = "done", _type = Cmd, _ids = [1]}
      parseArgs "d 1 --json"
        `shouldBe` emptyArgTree
          { _cmd = "done",
            _type = Cmd,
            _ids = [1],
            _opts = ArgOpts True False False False
          }

  describe "undone expr" $ do
    it "invalid args" $ do
      parseArgs "undone" `shouldBe` emptyArgTree
      parseArgs "undone bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "undone 1"
        `shouldBe` emptyArgTree {_cmd = "undone", _type = Cmd, _ids = [1]}
      parseArgs "u 1 --json"
        `shouldBe` emptyArgTree
          { _cmd = "undone",
            _type = Cmd,
            _ids = [1],
            _opts = ArgOpts True False False False
          }

  describe "delete expr" $ do
    it "invalid args" $ do
      parseArgs "delete" `shouldBe` emptyArgTree
      parseArgs "delete bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "delete 1"
        `shouldBe` emptyArgTree {_cmd = "delete", _type = Cmd, _ids = [1]}
      parseArgs "D 1 --json"
        `shouldBe` emptyArgTree
          { _cmd = "delete",
            _type = Cmd,
            _ids = [1],
            _opts = ArgOpts True False False False
          }

  describe "remove expr" $ do
    it "invalid args" $ do
      parseArgs "remove" `shouldBe` emptyArgTree
      parseArgs "remove bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "remove 1"
        `shouldBe` emptyArgTree {_cmd = "remove", _type = Cmd, _ids = [1]}
      parseArgs "r 1 --json"
        `shouldBe` emptyArgTree
          { _cmd = "remove",
            _type = Cmd,
            _ids = [1],
            _opts = ArgOpts True False False False
          }

  describe "context expr" $ do
    it "clear context" $ do
      parseArgs "context" `shouldBe` emptyArgTree {_cmd = "context", _type = Cmd}

    it "with tags" $ do
      parseArgs "context +tag1 +tag2 tag3"
        `shouldBe` emptyArgTree
          { _cmd = "context",
            _type = Cmd,
            _tags = ["tag1", "tag2", "tag3"]
          }

    it "with opt" $ do
      parseArgs "c +tag1 tag2 --json"
        `shouldBe` emptyArgTree
          { _cmd = "context",
            _type = Cmd,
            _tags = ["tag1", "tag2"],
            _opts = ArgOpts True False False False
          }

  describe "list expr" $ do
    it "with opt" $ do
      parseArgs "list  " `shouldBe` emptyArgTree {_type = Qry, _cmd = "list"}
      parseArgs "l   --json"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "list",
            _opts = ArgOpts True False False False
          }

  describe "info expr" $ do
    it "invalid args" $ do
      parseArgs "info" `shouldBe` emptyArgTree
      parseArgs "info bad-id" `shouldBe` emptyArgTree

    it "with id" $ do
      parseArgs "info 1"
        `shouldBe` emptyArgTree {_type = Qry, _cmd = "info", _ids = [1]}
      parseArgs "i 1   --json"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "info",
            _ids = [1],
            _opts = ArgOpts True False False False
          }

  describe "worktime expr" $ do
    it "global" $ do
      parseArgs "worktime"
        `shouldBe` emptyArgTree {_type = Qry, _cmd = "worktime"}

    it "with tags" $ do
      parseArgs "worktime tag +tag2"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "worktime",
            _tags = ["tag", "tag2"]
          }

    it "with opts" $ do
      parseArgs "worktime +tag tag2 --json"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "worktime",
            _tags = ["tag", "tag2"],
            _opts = ArgOpts True False False False
          }

    it "with min date" $ do
      parseArgs "w [10 tag"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "worktime",
            _tags = ["tag"],
            _minDate = Just $ ArgDate Rel 10 0 0 0 0
          }

    it "with min time" $ do
      parseArgs "w tag [:20"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "worktime",
            _tags = ["tag"],
            _minDate = Just $ ArgDate Rel 0 0 0 20 0
          }

    it "with min datetime" $ do
      parseArgs "w [10:20 tag"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "worktime",
            _tags = ["tag"],
            _minDate = Just $ ArgDate Rel 10 0 0 20 0
          }

    it "with max date" $ do
      parseArgs "w tag ]10"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "worktime",
            _tags = ["tag"],
            _maxDate = Just $ ArgDate Rel 10 0 0 0 0
          }

    it "with max time" $ do
      parseArgs "w ]:20 tag"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "worktime",
            _tags = ["tag"],
            _maxDate = Just $ ArgDate Rel 0 0 0 20 0
          }

    it "with max datetime" $ do
      parseArgs "w tag ]10:20"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "worktime",
            _tags = ["tag"],
            _maxDate = Just $ ArgDate Rel 10 0 0 20 0
          }

    it "with min and max datetimes" $ do
      parseArgs "w [10:20 ]14 tag"
        `shouldBe` emptyArgTree
          { _type = Qry,
            _cmd = "worktime",
            _tags = ["tag"],
            _minDate = Just $ ArgDate Rel 10 0 0 20 0,
            _maxDate = Just $ ArgDate Rel 14 0 0 0 0
          }

  describe "help expr" $ do
    it "with aliases" $ do
      parseArgs "help" `shouldBe` emptyArgTree {_type = Qry, _cmd = "help"}
      parseArgs "h" `shouldBe` emptyArgTree {_type = Qry, _cmd = "help"}
      parseArgs "--help" `shouldBe` emptyArgTree {_type = Qry, _cmd = "help"}
      parseArgs "-h" `shouldBe` emptyArgTree {_type = Qry, _cmd = "help"}
