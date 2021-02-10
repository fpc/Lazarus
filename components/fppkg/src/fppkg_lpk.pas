unit fppkg_lpk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLPKStatus = (lpDesigntime, lpRuntime, lpBoth);

function LPKStatus(AFileName: string): TLPKStatus;
function LPKFindPackage(PackageName: string): string;

implementation

uses
  XMLConf, fprepos, pkgmessages, pkgglobals, pkgrepos, pkghandler, fileutil;

function LPKStatus(AFileName: string): TLPKStatus;
var
  xml: TXMLConfig;
  pkgtype: string;
begin
  xml := TXMLConfig.Create(nil);

  try
    xml.Filename := AFileName;
    pkgtype := xml.GetValue('Package/Type/Value', '');
    //default
    Result := lpRunTime;

    if CompareText(pkgtype, 'designtime') = 0 then
      Result := lpDesignTime
    else if CompareText(pkgtype, 'runanddesigntime') = 0 then
      Result := lpBoth;

  finally
    FreeAndNil(xml);
  end;
end;

function LPKFindPackage(PackageName: string): string;
  //this really needs to come through the manifest,
  //what if a package has multiple .lpk files?
  //do we install them all?
  //do we need to add a target to each LPK file?
  //or do we split the package in multiple ones
var
//  BuildDir: string;
//  P: TFPPackage;
  files: TStrings;
begin
  if PackageName = '' then
    pkgglobals.Error(SErrNoPackageSpecified);

{  P := AvailableRepository.PackageByName(PackageName);
  BuildDir := PackageBuildPath(P);
  files := FindAllFiles(BuildDir, '*.lpk');
 }
  if files.Count > 0 then
    Result := files[0]
  else
    Result := '';

  FreeAndNil(files);
end;

end.

