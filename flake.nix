{
  inputs = {
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };
  outputs = inputs: inputs.ghc-wasm-meta.inputs.flake-utils.lib.eachDefaultSystem (system:
    let pkgs = inputs.ghc-wasm-meta.inputs.nixpkgs.legacyPackages.${system};
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          inputs.ghc-wasm-meta.packages.${system}.all_9_12
          pkgs.gnumake
          pkgs.http-server
        ];
      };
    });
  }

