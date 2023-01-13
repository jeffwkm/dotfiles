{ lib, stdenv, buildPythonPackage, fetchFromGitHub, numpy, pyyaml, setuptools }:

buildPythonPackage rec {
  pname = "amdgpu-fan";
  version = "0.0.6";

  src = fetchFromGitHub {
    owner = "chestm007";
    repo = pname;
    # rev = version;
    rev = "b0cb2d0da866c58329ce2edf146be1c41d3f6d8d";
    sha256 = "7ZhwOQVlkY7yA6HKgr82cetrGU5wqRPECA+37di2FF0=";
  };

  doCheck = false;

  propagatedBuildInputs = [ numpy pyyaml ];
  buildInputs = [ setuptools ];

  postPatch = ''
    substituteInPlace setup.py --replace 'PROJECTVERSION' '${version}'
  '';

  meta = with lib; {
    description = "Python controller for amdgpu fan";
    homepage = "https://github.com/chestm007/amdgpu-fan";
    maintainers = with maintainers; [ grburst ];
    license = licenses.gpl2;
    platforms = platforms.linux;
  };
}
