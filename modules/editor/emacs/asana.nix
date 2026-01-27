{
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "asana"
  ];
  homeManager =
    { pkgs, ... }:
    let
      emacs-asana = pkgs.emacsPackages.melpaBuild {
        pname = "asana";
        version = "0.0";
        src = pkgs.fetchFromGitHub {
          owner = "lmartel";
          repo = "emacs-asana";
          rev = "fccaab30e92b9f1b74cd5c17dda3bc5e704480fb";
          hash = "sha256-ZyvG9Vc5Wu2qi8KM4UgCqHs/edickPlhT1+O6KNI/1o=";
        };
        packageRequires = with pkgs.emacsPackages; [
          helm
          exec-path-from-shell
        ];
      };
    in
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-asana";
        config = # elisp
          ''
            (setq asana-token (auth-source-pick-first-password :host "asana"))
          '';
        elispPackages = [
          emacs-asana
        ];
      };
    };
}
