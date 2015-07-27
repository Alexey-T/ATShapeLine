{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit shapeline_pkg;

interface

uses
  shapeline, shapeline_reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('shapeline_reg', @shapeline_reg.Register);
end;

initialization
  RegisterPackage('shapeline_pkg', @Register);
end.
