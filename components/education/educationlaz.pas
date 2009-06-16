{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit EducationLaz; 

interface

uses
  EduOptionsDlg, EduOptions, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('EduOptionsDlg', @EduOptionsDlg.Register); 
end; 

initialization
  RegisterPackage('EducationLaz', @Register); 
end.
