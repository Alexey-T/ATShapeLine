{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit shapeline_pkg;

interface

uses
  shapeline, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('shapeline', @shapeline.Register);
end;

initialization
  RegisterPackage('shapeline_pkg', @Register);
end.
