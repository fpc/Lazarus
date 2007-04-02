{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit weblaz; 

interface

uses
  WebLazIDEIntf, fpWeb, HTTPDefs, fphttp, fpcgi, lazweb, fpTemplate, HTMLDefs, 
    htmlelements, htmlwriter, webutil, fphtml, fpdatasetform, websession, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('WebLazIDEIntf', @WebLazIDEIntf.Register); 
end; 

initialization
  RegisterPackage('weblaz', @Register); 
end.
