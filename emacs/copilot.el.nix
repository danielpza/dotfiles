{ trivialBuild, fetchFromGitHub, s, dash, editorconfig }:
trivialBuild rec {
  pname = "copilot";
  version = "30a054f8569550853a9b6f947a2fe1ded7e7cc6b";
  src = fetchFromGitHub {
    owner = "zerolfx";
    repo = "copilot.el";
    rev = "30a054f8569550853a9b6f947a2fe1ded7e7cc6b";
    hash = "sha256-uNtoduh/29dpBcE556G3qHdycdj0OGMridCTy3k5vX8=";
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
