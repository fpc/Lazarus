{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit educationlaz; 

interface

uses
    EduEnvOptsFrame, EduOptions, EduPkgSystem, EduCompPalette, EduMenu, 
  EduNewProgram, EduPropsEvents, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('EduEnvOptsFrame', @EduEnvOptsFrame.Register); 
  RegisterUnit('EduPkgSystem', @EduPkgSystem.Register); 
  RegisterUnit('EduCompPalette', @EduCompPalette.Register); 
  RegisterUnit('EduMenu', @EduMenu.Register); 
  RegisterUnit('EduNewProgram', @EduNewProgram.Register); 
  RegisterUnit('EduPropsEvents', @EduPropsEvents.Register); 
end; 

initialization
  RegisterPackage('EducationLaz', @Register); 
end.
