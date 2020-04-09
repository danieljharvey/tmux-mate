-- here we are taking our first Dhall file and adding another window to it
let sample1 = ./Sample1.dhall

in  { sessionTitle = sample1.sessionTitle
    , sessionWindows =
          sample1.sessionWindows
        # [ { windowTitle = "second-window"
            , windowArrangement = "vertical"
            , windowPanes =
              [ { paneCommand = "yes 'Pane 3'" }
              , { paneCommand = "yes 'Pane 4'" }
              ]
            }
          ]
    }
