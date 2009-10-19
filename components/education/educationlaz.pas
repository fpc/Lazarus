{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit EducationLaz; 

interface

uses
    EduEnvOptsFrame, EduOptions, EduPkgSystem, EduCompPalette, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('EduEnvOptsFrame', @EduEnvOptsFrame.Register); 
  RegisterUnit('EduPkgSystem', @EduPkgSystem.Register); 
  RegisterUnit('EduCompPalette', @EduCompPalette.Register); 
end; 

initialization
  RegisterPackage('EducationLaz', @Register); 
end.
