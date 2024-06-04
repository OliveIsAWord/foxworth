{

description = "A System Fω evaluator";

inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";

outputs = { self, nixpkgs }: let
  supportedSystems = nixpkgs.lib.systems.flakeExposed;
  allSystems = output: nixpkgs.lib.genAttrs supportedSystems
    (system: output nixpkgs.legacyPackages.${system});
in {
  packages = allSystems (pkgs: {
    default = pkgs.haskellPackages.developPackage { root = ./.; };
  });

  devShells = allSystems (pkgs: {
    default = pkgs.haskellPackages.shellFor {
      packages = _: pkgs.lib.attrValues self.packages.${pkgs.system};
      nativeBuildInputs = with pkgs.haskellPackages; [
        ghc
        ghcid
        hlint
        cabal-install
        ormolu
        fourmolu
      ] ++ (with pkgs; [
        python311Packages.mdformat-gfm
      ]);
    };
  });
};

}
