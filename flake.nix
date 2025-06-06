{
  description = "MUSIKELL: Music engine in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install

            darwin.apple_sdk.frameworks.AudioToolbox
            darwin.apple_sdk.frameworks.CoreAudio
            darwin.apple_sdk.frameworks.CoreFoundation
          ];

          shellHook = ''
            export NIX_LDFLAGS="-framework AudioToolbox -framework CoreAudio -framework CoreFoundation $NIX_LDFLAGS"
          '';
        };
      });
}
