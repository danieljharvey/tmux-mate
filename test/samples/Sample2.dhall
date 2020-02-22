-- here we are taking our first Dhall file and adding another item to it
let sample1 = ./Sample1.dhall

in  { sessionTitle = sample1.sessionTitle
    , sessionPanes = sample1.sessionPanes # [ { paneCommand = "yes 'Pane 3'" } ]
    }
