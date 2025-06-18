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

        dotnet = pkgs.dotnet-sdk;

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

        arguPkg = pkgs.fetchurl {
          url = "https://www.nuget.org/api/v2/package/Argu/6.2.5";
          sha256 = "sha256-5HcZcvco4e8+hgLhzlxk7ZmFVLtZL9LVr7LbmXsLmNU=";
        };

        localNugetRepo = pkgs.stdenv.mkDerivation {
          pname = "local-nuget-repo";
          version = "1.0";
          unpackPhase = "true";
          buildPhase = ''
            mkdir -p $out
            cp ${arguPkg} $out/MathNet.Numerics.6.2.5.nupkg
          '';
          installPhase = "true";
        };

      in
      {

        packages.default = pkgs.stdenv.mkDerivation {
          name = "aoc2022-build";
          src = ./.;

          buildInputs = [ dotnet ];

          buildPhase = ''
                # Write NuGet.Config to add local package source
                cat > NuGet.Config <<EOF
            <?xml version="1.0" encoding="utf-8"?>
            <configuration>
              <packageSources>
                <add key="local" value="file://${localNugetRepo}" />
                <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
              </packageSources>
            </configuration>
            EOF

            export DOTNET_NUGET_CONFIG_FILE=$PWD/NuGet.Config

            dotnet restore ./Aoc2022Proj.sln --configfile NuGet.Config
            dotnet build ./Aoc2022Proj.sln --configuration Release
          '';

          installPhase = "mkdir -p $out"; # no actual outputs, just needs to succeed

          checkPhase = ''
            dotnet test ./src/Aoc2022Tests/Aoc2022Tests.fsproj --configuration Release --no-build
          '';
          doCheck = true;
        };

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

            dotnet tool restore || echo "Could not restore dotnet tools"
          '';
        };
      }
    );
}
