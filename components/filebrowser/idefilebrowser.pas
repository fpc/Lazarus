{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit idefilebrowser; 

interface

uses
  frmFileBrowser, regidefilebrowser, frmconfigfilebrowser, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('regidefilebrowser', @regidefilebrowser.Register); 
end; 

initialization
  RegisterPackage('idefilebrowser', @Register); 
end.
