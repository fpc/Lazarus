{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install
  the package GTKOpenGL 1.0.
}

unit GTKOpenGL; 

interface

uses
  GTKGLArea, GTKGLArea_Int, nvGL, nvGLX, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('GTKGLArea', @GTKGLArea.Register); 
end; 

initialization
  RegisterPackage('GTKOpenGL', @Register)
end.
