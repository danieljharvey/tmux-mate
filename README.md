# tmux-mate

Manage your tmux sessions with the delicious power of Dhall.

### Motivation

Working on modern microservice architectures usually means spinning up various combinations of 5 or more different services. Remembering what they are is a totally `1x` use of your time, let's automate it!

### Installation

#### MacOS

```bash
brew update && brew install danieljharvey/tools/tmux-mate
```

#### Ubuntu 

Binaries available on the [releases](https://github.com/danieljharvey/tmux-mate/releases) page.

### Getting started

```bash
# create a default tmux-mate.dhall
tmux-mate init
# Start running everything
tmux-mate start
```

### Tutorial

Let's grab a couple of sample config files...

```bash
curl https://raw.githubusercontent.com/danieljharvey/tmux-mate/master/samples/Sample1.dhall > Sample1.dhall
curl https://raw.githubusercontent.com/danieljharvey/tmux-mate/master/samples/Sample2.dhall > Sample2.dhall
```

Let's open the first config file in `tmux-mate`.

```
# Run tmux-mate with the first sample script
tmux-mate ./Sample1.dhall
```

You should now see a `tmux` window running two infinite loops (that will soon wear your battery down, apologies). What if it turns out we need more things in our development environment?

```bash
# Run tmux-mate with the second sample script
tmux-mate ./Sample2.dhall
```

You will now see your same session with an extra window added. `tmux-mate` has diffed the two sessions and added/removed the changes. This might seem like a useless optimization when running a trivial process like `yes`, but when running multiple build environments this saves loads of time.

### Configuration

This project uses [Dhall](https://dhall-lang.org/) files for configuration. There are some examples in the `/samples/` folders that demonstrate how to put one together. This is the schema:

```
{ sessionTitle : Text
, sessionWindows :
    List
      { windowTitle : Text
      , windowPanes : List { paneCommand : Text }
      , windowArrangement : Text
      }
}
```

A few rules

- All of the `sessionTitle` and `windowTitle` entries must be non-empty - they are used to manage the sessions internally.
- The session must contain at least one window, and each window must contain at least one pane.
- `windowArrangement` is one of `tmux`'s options `tiled`, `even-horizontal`,
  `even-vertical`, `main-horizontal` and `main-vertical`. Info on what those
mean in the [man page](http://man7.org/linux/man-pages/man1/tmux.1.html) -
search for `select-layout` for info.

### Options

Sometimes if what you expect to happen is not happening, pop in the `-v`
(or `--verbose`) flag to see what `tmux-mate` is thinking.

Alternatively, to see what it's thinking without actually running the commands,
then instead use `-d` (or `--dry-run`).

### Requirements

You will need a recent version of `tmux` installed. I tested on version 3, but I'm pretty sure the commands I am using are pretty basic so should work backwards too.

### Development

Run `stack install` to install `tmux-mate` and then run `tmux-mate
development.dhall` to launch an environment with everything you need.

If `ghcid` is missing, add it with `stack install ghcid`.

### Prior art

Very much inspired by [Tmuxinator](https://github.com/tmuxinator/tmuxinator), a
great project that doesn't _quite_ do what I needed.
