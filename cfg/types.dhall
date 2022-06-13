let CodeNames = List { mapKey : Text, mapValue : Natural }

let GestureNames = List { mapKey : Text, mapValue : Text }

let KCfg =
      { codeNames : CodeNames
      , gestureNames : GestureNames
      , fallthrough : Bool
      , cmdAllow : Bool
      , logLevel : Text
      , keyInputCfg : Text
      , keyOutputCfg : Text
      , keymapFile : Text
      , keyRepeat : Optional Text
      , preKioCmd : Optional Text
      , postKioCmd : Optional Text
      }

in  { KCfg, CodeNames, GestureNames }
