{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit GTKOpenGL; 

interface

uses
  GTKGLAreaControl, nvGL, nvGLX, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('GTKGLAreaControl', @GTKGLAreaControl.Register); 
end; 

initialization
  RegisterPackage('GTKOpenGL', @Register); 
end.
