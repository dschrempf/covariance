{
  description = "Covariance estimation development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
    }:
    let
      theseHpkgNames = [ "covariance" ];
      thisGhcVersion = "ghc98";
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides =
            selfh: superh:
            supern.haskell.packageOverrides selfh superh
            // {
              covariance = selfh.callCabal2nix "covariance" ./. { };
            };
        };
      };
      perSystem =
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ hOverlay ];
          };
          hpkgs = pkgs.haskell.packages.${thisGhcVersion};
          hlib = pkgs.haskell.lib;
          theseHpkgs = nixpkgs.lib.genAttrs theseHpkgNames (n: hpkgs.${n});
          theseHpkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) theseHpkgs;
        in
        {
          packages = theseHpkgs // {
            default = theseHpkgs.covariance;
          };

          devShells.default = hpkgs.shellFor {
            packages = _: (builtins.attrValues theseHpkgsDev);
            nativeBuildInputs = [
              # Haskell toolchain.
              hpkgs.cabal-fmt
              hpkgs.cabal-install
              hpkgs.haskell-language-server
            ];
            buildInputs = [ ];
            doBenchmark = true;
            # withHoogle = true;
          };
        };
    in
    { overlays.default = hOverlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
