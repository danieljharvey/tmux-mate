{ sessionTitle : Text
, sessionWindows :
    List
      { windowTitle : Text
      , windowPanes : List { paneCommand : Text }
      , windowArrangement : < Horizontal | Vertical | Tiled >
      }
}
