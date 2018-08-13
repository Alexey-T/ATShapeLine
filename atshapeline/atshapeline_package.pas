{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit atshapeline_package;

interface

uses
  atshapeline, atshapeline_register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('atshapeline_register', @atshapeline_register.Register);
end;

initialization
  RegisterPackage('atshapeline_package', @Register);
end.
