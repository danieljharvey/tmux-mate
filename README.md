# tmux-mate

Manage your tmux sessions with the delicious power of Dhall.

### Motivation

Working on modern microservice architectures usually means spinning up various combinations of 5 or more different services. Remembering what they are is a totally `1x` use of your time, let's automate it!

### Getting started

```bash
# clone this repo
git clone https://github.com/danieljharvey/tmux-mate`

# enter the blessed folder
cd tmux-mate

# install tmux-mate using Haskell Stack (install instructions here: https://docs.haskellstack.org/en/stable/install_and_upgrade/)
# this will put tmux-mate-exe in your path
stack install

# curse this terrible env var based API for passing config files and run tmux-mate
export TMUX_MATE_PATH='./samples/Sample1.dhall && tmux-mate-exe
```

You should now see a `tmux` window running two infinite loops (that will soon wear your battery down, apologies). What if it turns out we need more things in our development environment?

```bash
# Run tmux-mate with the second sample script
export TMUX_MATE_PATH='./samples/Sample2.dhall && tmux-mate-exe
```

You will now see your same session with an extra window added. `tmux-mate` has diffed the two sessions and added/removed the changes. This might seem like a useless optimization when running a trivial process like `yes`, but when running multiple build environments this saves loads of time.

### Configuration

This project uses [Dhall](https://dhall-lang.org/) files for configuration. There are some examples in the `/samples/` folders that demonstrate how to put one together. This is the schema:

```
{ sessionTitle : Text
, sessionWindows : List
  { windowTitle : Text
  , windowPanes : List { paneCommand : Text }
  }
}
```

A few rules

- All of the `sessionTitle` and `windowTitle` entries must be non-empty - they are used to manage the sessions internally.
- The session must contain at least one window, and each window must contain at least one pane.

### Requirements

You will need a recent version of `tmux` installed. I tested on version 3, but I'm pretty sure the commands I am using are pretty basic so should work backwards too.

### Prior art

Very much inspired by [Tmuxinator](https://github.com/tmuxinator/tmuxinator), a
great project that doesn't _quite_ do what I needed.
