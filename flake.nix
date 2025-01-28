{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      packages.${system}.default = pkgs.stdenv.mkDerivation {
        name = "scheme";
        src = ./.;

        nativeBuildInputs = [ pkgs.zig ];

        buildPhase = ''
          ZIG_GLOBAL_CACHE_DIR=$PWD/zig-cache zig build --release=fast
        '';

        installPhase = ''
          mkdir -p $out/bin
          cp zig-out/bin/scheme $out/bin/
        '';
      };

      apps.${system}.default = {
        type = "app";
        program = "${self.packages.${system}.default}/bin/scheme";
      };
    };
}
