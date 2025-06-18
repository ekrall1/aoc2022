{
  description = "Flake to build AOC2022.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };

        dotnet = with pkgs; [
          dotnet-sdk
        ];

        vscode = (
          pkgs.vscode-with-extensions.override {
            vscodeExtensions = with pkgs.vscode-extensions; [
              bbenoist.nix
              jnoortheen.nix-ide
              ionide.ionide-fsharp
              ms-dotnettools.vscode-dotnet-runtime
              ms-dotnettools.csharp
              ms-dotnettools.csdevkit
              golang.go
            ];
          }
        );

        azure-cli = pkgs.azure-cli.withExtensions [
          pkgs.azure-cli-extensions.azure-devops
        ];

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            dotnet
            vscode
            azure-cli
          ];
          shellHook = ''
            echo "Welcome to the F# shell."

            export DOTNET_ROOT=${pkgs.dotnet-sdk}
            export PATH="$HOME/.dotnet/tools:$PATH"
            export IONIDE_CACHE_DIR=$HOME/.cache/ionide
            mkdir -p $IONIDE_CACHE_DIR


            if [ ! -f .config/dotnet-tools.json ]; then
              echo "Initializing dotnet tool manifest..."
              mkdir -p .config
              dotnet new tool-manifest
            fi

            if ! grep -q fantomas .config/dotnet-tools.json; then
              echo "Installing fantomas..."
              dotnet tool install fantomas
            fi

            dotnet add ./src/Aoc2022/Aoc2022.fsproj package Argu || true

            dotnet tool restore || echo "Could not restore dotnet tools"
          '';
        };
      }
    );
}
