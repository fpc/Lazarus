{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit EducationLaz; 

interface

uses
    EduEnvOptsFrame, EduOptions, EduPkgSystem, EduCompPalette, EduMenu, 
  EduNewProgram, EduPropsEvents, EduOIPages, EduOptionsDlg, EduSpeedButtons, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('EduEnvOptsFrame', @EduEnvOptsFrame.Register); 
  RegisterUnit('EduPkgSystem', @EduPkgSystem.Register); 
  RegisterUnit('EduCompPalette', @EduCompPalette.Register); 
  RegisterUnit('EduMenu', @EduMenu.Register); 
  RegisterUnit('EduNewProgram', @EduNewProgram.Register); 
  RegisterUnit('EduPropsEvents', @EduPropsEvents.Register); 
  RegisterUnit('EduOIPages', @EduOIPages.Register); 
  RegisterUnit('EduOptionsDlg', @EduOptionsDlg.Register); 
  RegisterUnit('EduSpeedButtons', @EduSpeedButtons.Register); 
end; 

initialization
  RegisterPackage('EducationLaz', @Register); 
end.
