{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs }@inputs:
    let package = "thumper";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        
        spiceSource = pkgs.fetchzip {
          url = "https://github.com/max-legrand/spice/archive/refs/heads/main.zip";
          sha256 = "sha256-gW2grebfEVthIh0SYltfJ+ah9A7tgb9pgIkbhy0DK0g=";
        };
        
        # Add development tools to the scope
        devPackagesQuery = {
          ocaml-base-compiler = "5.2.0";
          ocaml-lsp-server = "1.19.0";  # Add explicit version
          ocamlformat = "*";
        };
        
        scope = on.buildOpamProject { } package ./. devPackagesQuery;
        
        overlay = final: prev: {
          ocurl = prev.ocurl.overrideAttrs (old: {
            buildInputs = (old.buildInputs or [ ]) ++ [ 
              pkgs.curl
              pkgs.curl.dev
            ];
          });
        } // (pkgs.lib.optionalAttrs (pkgs.stdenv.isDarwin) {
          caqti = prev.caqti.overrideAttrs (old: {
            preBuild = ''
              mkdir -p $TMP/bin
              echo '#!/bin/sh' > $TMP/bin/codesign
              chmod +x $TMP/bin/codesign
              export PATH="$TMP/bin:$PATH"
            '';
          });
        });
        scope' = scope.overrideScope overlay;
        
        finalPackage = scope'.${package}.overrideAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
            pkgs.git
          ];
          buildPhase = ''
            mkdir -p lib
            rm -rf lib/spice
            mkdir -p lib/spice
            cp -rL ${spiceSource}/lib/* lib/spice/
            cp -rL ${spiceSource}/dune-project lib/spice/
            chmod -R u+w lib/spice
            
            echo "=== New contents of lib/spice ==="
            ls -la lib/spice/
            
            dune build --release @install
          '';
          installPhase = ''
            mkdir -p $out/lib/ocaml/5.2.0/site-lib
            dune install --prefix $out --libdir $out/lib/ocaml/5.2.0/site-lib --release
          '';
        });
      in {
        legacyPackages = scope';
        packages = {
          default = finalPackage;
          ${package} = finalPackage;
        };
        apps.default = {
          type = "app";
          program = "${finalPackage}/bin/${package}";
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.default ];
          buildInputs = [ scope'.ocaml-lsp-server scope'.ocamlformat ];
        };
      });
}
