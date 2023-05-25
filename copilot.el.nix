{ trivialBuild, fetchFromGitHub, s, dash, editorconfig }:
trivialBuild rec {
  pname = "copilot";
  version = "15a698ebc1d6ffa10da7d6d7e9f972786d0ce526";
  src = fetchFromGitHub {
    owner = "zerolfx";
    repo = "copilot.el";
    rev = "15a698ebc1d6ffa10da7d6d7e9f972786d0ce526";
    hash = "sha256-/+XyEI8iTwYf0R/kLC1tXCq9PP5fs6Xob55lrwLWhUg=";
  };
  # elisp dependencies
  propagatedUserEnvPkgs = [ s dash editorconfig ];

  buildInputs = propagatedUserEnvPkgs;

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp $out/share/emacs/native-lisp
    cp -r dist *.el $out/share/emacs/site-lisp
    cp -r dist *.elc $out/share/emacs/native-lisp
  '';
}
