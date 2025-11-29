{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = "python";
  options =
    { pkgs, ... }:
    {
      package = lib.mkPackageOption pkgs "python3" { };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      home.packages = with pkgs; [
        config.ordenada.features.python.package
        black
      ];
      home.sessionVariables = {
        "IPYTHONDIR" = "${config.ordenada.features.xdg.baseDirs.configHome}/ipython";
        "PYTHONSTARTUP" =
          pkgs.writeText "pythonrc" # python
            ''
              import os
              import atexit
              import readline
              from pathlib import Path

              if readline.get_current_history_length() == 0:
                  state_home = os.environ.get("XDG_STATE_HOME")
                  if state_home is None:
                      state_home = Path.home() / ".local" / "state"
                  else:
                      state_home = Path(state_home)

                  history_path = state_home / "python_history"
                  if history_path.is_dir():
                    raise OSError(f"'{history_path}' cannot be a directory")

                  history = str(history_path)

                  try:
                      readline.read_history_file(history)
                  except OSError: # Non existent
                      pass

                  def write_history():
                      try:
                          readline.write_history_file(history)
                      except OSError:
                          pass

                  atexit.register(write_history)
            '';
      };
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-python";
        config =
          with config.ordenada.features; # elisp
          ''
            (defgroup ordenada-python nil
              "General Python programming utilities."
              :group 'ordenada)

            (add-to-list 'major-mode-remap-alist
                         '(python-mode . python-ts-mode))

            (with-eval-after-load 'eglot
              (add-to-list
               'eglot-server-programs
               '(python-ts-mode
                 . ("${pkgs.pyright}/bin/pyright-langserver" "--stdio"))))

            (define-minor-mode ordenada-python-mode
              "Set up convenient tweaks for Python development."
              :group 'ordenada-python
              (when ordenada-python-mode
                (eglot-ensure)))

            (add-hook 'python-ts-mode-hook #'ordenada-python-mode)

            ${lib.optionalString emacs.org.enable ''
                (with-eval-after-load 'org
                  (add-to-list 'org-structure-template-alist
                               '("py" . "src python")))
              (with-eval-after-load 'ob-core
                (require 'ob-python))
              (with-eval-after-load 'ob-python
                (setq org-babel-python-command "${lib.getExe python.package}"))
            ''}
          '';
        elispPackages = with pkgs.emacsPackages; [
          (treesit-grammars.with-grammars (
            grammars: with grammars; [
              tree-sitter-python
            ]
          ))
        ];
      };
    };
}
