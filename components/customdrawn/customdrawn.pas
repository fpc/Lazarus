{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit customdrawn; 

interface

uses
  customdrawnextras, customdrawn_wince, customdrawn_win2000, 
  customdrawn_winxp, customdrawn_android, customdrawn_extra1, customdrawn_kde, 
  customdrawn_gnome, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('customdrawnextras', @customdrawnextras.Register); 
end; 

initialization
  RegisterPackage('customdrawn', @Register); 
end.
