{
  description = "fida-contracts: Fida insurance smart contracts in Scala 3 with Scalus";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];

      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs:
        let
          # CI builds with JDK 21 (Temurin)
          jdk = pkgs.jdk21;
        in
        {
          default = pkgs.mkShell {
            packages = [
              jdk
              (pkgs.sbt.override { jre = jdk; })
              (pkgs.coursier.override { jre = jdk; })
              (pkgs.metals.override { jre = jdk; })
            ];

            # picked up by sbt, Metals and the VS Code Metals extension
            JAVA_HOME = jdk.home;
          };
        });
    };
}
