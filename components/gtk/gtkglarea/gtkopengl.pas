{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit GTKOpenGL; 

interface

uses
  gtkglareacontrol, nvgl, nvglx, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('gtkglareacontrol', @gtkglareacontrol.Register); 
end; 

initialization
  RegisterPackage('GTKOpenGL', @Register); 
end.
