{ sessionTitle = "tmux-mate"
, sessionWindows =
  [ { windowTitle = "development"
    , windowPanes =
      [ { paneCommand = "vim ." }
      , { paneCommand = "stack test --file-watch" }
      , { paneCommand = "ghcid -c 'stack repl'" }
      , { paneCommand = "ghcid -c 'stack repl test/Spec.hs'" }
      , { paneCommand = "watch --color -n 10 git -c color.status=always status" }
      , { paneCommand = "$SHELL" }
      ]
    , windowArrangement = "main-vertical"
    }
  ]
}
