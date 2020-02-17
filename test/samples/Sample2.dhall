-- here we are taking our first Dhall file and adding another item to it
let sample1 = ./Sample1.dhall

in  { title = sample1.title
    , panes = sample1.panes # [
      { paneCommand = "yes 'Pane 3'"
      , paneTitle = "Three"
      }
    ]
  }