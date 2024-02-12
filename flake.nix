{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.11";
    utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, easy-purescript-nix }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [ ];
        };
        easy-ps = import easy-purescript-nix { inherit pkgs; };
      in
      {
        devShell =
          with pkgs;
          mkShell {
            buildInputs = [
              nodejs_20

              easy-ps.purty

              dhall
            ];
          };
      });
}
