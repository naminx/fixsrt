{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [inputs.haskell-flake.flakeModule];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (self: super: {
              all-cabal-hashes = inputs.all-cabal-hashes;
              my-neovim = self.callPackage ./vim.nix {};
            })
          ];
          config = {
            # for script-monad & wedriver-w3c
            allowBroken = true;
            # for google-chrome
            allowUnfree = true;
          };
        };
        haskellProjects.default = {
          # packages.example.root = ./.;  # This value is detected based on .cabal files
          # overrides = self: super:
          #   with pkgs.haskell.lib; {
          #     script-monad = dontCheck super.script-monad;
          #     webdriver-w3c = dontCheck super.webdriver-w3c;
          #   };
          devShell = {
            enable = true; # Enabled by default
            tools = hp: {
              inherit
                (pkgs)
                dos2unix
                lambdabot
                nodejs
                nomacs
                my-neovim
                stack
                ;
              inherit
                (hp)
                fourmolu
                ;
            };
            #  hlsCheck.enable = true;
          };
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.example;
      };
    };
}
