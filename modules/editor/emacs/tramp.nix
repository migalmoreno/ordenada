{ mkFeature, ordenada-lib, ... }:

mkFeature {
  name = [
    "emacs"
    "tramp"
  ];
  homeManager =
    { pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-tramp";
        config = ''
          (with-eval-after-load 'tramp
            (setopt tramp-verbose 1)
            (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
          (with-eval-after-load 'vc
            (setopt vc-ignore-dir-regexp
                    (format "%s\\|%s"
                            vc-ignore-dir-regexp tramp-file-name-regexp)))
        '';
      };
    };
}
