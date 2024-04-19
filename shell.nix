let
  sources = import ./nix/sources.nix;
  emacsOverlay = import sources.emacs-overlay;
  pkgs = import sources.nixpkgs { overlays = [ emacsOverlay ]; };
  emacsPackages = pkgs.emacsPackagesFor pkgs.emacsPgtk;
  emacsWithPackages = emacsPackages.emacsWithPackages;
  epkgs = emacsWithPackages (epkgs: [ epkgs.vterm ]);
in with pkgs;
mkShell {
  buildInputs = [
    pkgs.emacsPgtkGcc
    pkgs.libtool
    pkgs.ripgrep
    pkgs.fd
    epkgs
    pkgs.nixfmt
    (agda.withPackages (ps: [ ps.standard-library ps.cubical ]))
  ];
}
