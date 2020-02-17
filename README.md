# tmux-mate

Manage your tmux sessions with the delicious power of Dhall.

### Trying it

To use:

`git clone https://github.com/danieljharvey/tmux-mate`

`cd tmux-mate`

`stack install`

`export TMUX_MATE_PATH='./test/samples/Sample1.dhall && tmux-mate`

You should now see some garbage and your session.

### Making your own dhall files

Look in `test/samples` for ideas.

### Requirements

You will need a recent version of `tmux` installed. I tested on version 3, but I'm pretty sure the commands I am using are pretty basic so should work backwards too.
