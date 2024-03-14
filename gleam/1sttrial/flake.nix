{
  description = "1st trial gleam";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
   flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.bash-prompt-prefix = "\\e[33m[dev]\\e[0m ";
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = import ./shell.nix { inherit pkgs; };
      }
    );
}
