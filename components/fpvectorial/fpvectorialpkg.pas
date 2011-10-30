{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fpvectorialpkg; 

interface

uses
  svgvectorialwriter, fpvtocanvas, fpvectorial, fpvectbuildunit, 
  dxfvectorialreader, cdrvectorialreader, avisozlib, avisocncgcodewriter, 
  avisocncgcodereader, svgvectorialreader, epsvectorialreader, fpvutils, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('fpvectorialpkg', @Register); 
end.
