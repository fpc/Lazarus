{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit anchordockpkg; 

interface

uses
    AnchorDocking, AnchorDockStorage, AnchorDockStr, AnchorDockOptionsDlg, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('AnchorDocking', @Register); 
end.
