{ trivialBuild, fetchFromGitHub, s, dash, editorconfig }:
trivialBuild rec {
  pname = "copilot";
  version = "efd6c1079a0abe989c8645cf1fb1c2e6c5e97c70";
  src = fetchFromGitHub {
    owner = "zerolfx";
    repo = "copilot.el";
    rev = "efd6c1079a0abe989c8645cf1fb1c2e6c5e97c70";
    hash = "sha256-VxgeXEFdhawRb978eSbj4aYMluBBTJQ333CvEk51YYk=";
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
