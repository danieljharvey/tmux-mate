let
  pkgs =
    let
      # 2020-02-17T09:51:51+00:00",
      rev = "fc592a52cacfbf5f22e6479a22263983f5346ea6";
      sha256 = "0ma4jxxjdp1pnc1nz0b1h13kh49z4na4rjixg5sbdi8iz5fmq4iy";
    in
    import (builtins.fetchTarball {
      name = "nixpkgs-${rev}";
      url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256;
    }) {};
in
{ ghc ? null }: # passed in by `stack build --nix`
if ghc == null then throw "build with stack!" else
pkgs.haskell.lib.buildStackProject {
  name = "tmux-mate";
  inherit ghc;
  buildInputs = [ pkgs.zlib ];
}
