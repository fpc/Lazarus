{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit TrayIconLaz; 

interface

uses
  TrayIcon, wstrayicon, WSCommonTrayIcon, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('TrayIcon', @TrayIcon.Register); 
end; 

initialization
  RegisterPackage('TrayIconLaz', @Register); 
end.
