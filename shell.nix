{ pkgs ? import (
    builtins.fetchTarball {
      name = "nixos-20.03";
      url = "https://github.com/NixOS/nixpkgs/archive/5272327b81ed355bbed5659b8d303cf2979b6953.tar.gz";
      sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
    }
  )
    {}
}:
let
  easyPS = import (
    builtins.fetchTarball {
      name = "easy-purescript";
      url = "https://github.com/justinwoo/easy-purescript-nix/archive/1ec689df0adf8e8ada7fcfcb513876307ea34226.tar.gz";
      sha256 = "12hk2zbjkrq2i5fs6xb3x254lnhm9fzkcxph0a7ngxyzfykvf4hi";
    }
  ) {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.git
    pkgs.yarn
    pkgs.nodePackages.bower
    pkgs.nodePackages.pulp
    pkgs.nodejs
    easyPS.spago
    easyPS.purs
    easyPS.purty
  ];
}
