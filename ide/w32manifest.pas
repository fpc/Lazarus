{
 /***************************************************************************
                        w32manifest.pas  -  Lazarus IDE unit
                        ---------------------------------------
              TProjectXPManifest is responsible for the inclusion of the 
                   manifest in windows executables.


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  The manifest file is needed for windows XP themes.
  The file is created in the directory, where the project exe is created.
}
unit W32Manifest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Laz2_XMLCfg, LCLProc, Controls, Forms,
  CodeToolManager, LazConf, LResources,
  ProjectResourcesIntf, resource;

const
  DefaultXPManifestTextName = 'CompanyName.ProductName.AppName';
  DefaultXPManifestTextDesc = 'Your application description.';
type
  TXPManifestExecutionLevel = (
    xmelAsInvoker,
    xmelHighestAvailable,
    xmelRequireAdministrator
  );

  TXPManifestDpiAware = (
    xmdaFalse,
    xmdaTrue,
    xmdaPerMonitor,
    xmdaTruePM,
    xmdaPerMonitorV2
  );

type
  { TProjectXPManifest }

  TProjectXPManifest = class(TAbstractProjectResource)
  private
    FExecutionLevel: TXPManifestExecutionLevel;
    FDpiAware: TXPManifestDpiAware;
    FUIAccess: Boolean;
    FUseManifest: boolean;
    FLongPathAware: Boolean;
    FTextName: string;
    FTextDesc: string;
    procedure SetDpiAware(AValue: TXPManifestDpiAware);
    procedure SetExecutionLevel(AValue: TXPManifestExecutionLevel);
    procedure SetUIAccess(AValue: Boolean);
    procedure SetUseManifest(const AValue: boolean);
    procedure SetLongPathAware(AValue: Boolean);
    procedure SetTextName(const AValue: string);
    procedure SetTextDesc(const AValue: string);
  public
    constructor Create; override;
    function UpdateResources(AResources: TAbstractProjectResources; const {%H-}MainFilename: string): Boolean; override;
    procedure WriteToProjectFile(AConfig: {TXMLConfig}TObject; const Path: String); override;
    procedure ReadFromProjectFile(AConfig: {TXMLConfig}TObject; const Path: String); override;

    property UseManifest: boolean read FUseManifest write SetUseManifest;
    property DpiAware: TXPManifestDpiAware read FDpiAware write SetDpiAware;
    property ExecutionLevel: TXPManifestExecutionLevel read FExecutionLevel write SetExecutionLevel;
    property UIAccess: Boolean read FUIAccess write SetUIAccess;
    property LongPathAware: Boolean read FLongPathAware write SetLongPathAware;
    property TextName: string read FTextName write SetTextName;
    property TextDesc: string read FTextDesc write SetTextDesc;
  end;

const
  ExecutionLevelToStr: array[TXPManifestExecutionLevel] of String = (
    'asInvoker',
    'highestAvailable',
    'requireAdministrator'
  );

  ManifestDpiAwareValues: array[TXPManifestDpiAware] of string = (
    'False',
    'True',
    'Per-monitor',
    'True/PM',
    'True/PM_V2'
  );

  ManifestDpiAwarenessValues: array[TXPManifestDpiAware] of string = (
    '',
    '',
    '',
    '',
    '<dpiAwareness>PerMonitorV2, PerMonitor</dpiAwareness>'
  );

implementation

const
  sManifestFileData: String =
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'#$D#$A+
    '<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">'#$D#$A+
    ' <assemblyIdentity version="1.0.0.0" processorArchitecture="*" name="%s" type="win32"/>'#$D#$A+
    ' <description>%s</description>'#$D#$A+
    ' <dependency>'#$D#$A+
    '  <dependentAssembly>'#$D#$A+
    '   <assemblyIdentity type="win32" name="Microsoft.Windows.Common-Controls" version="6.0.0.0" processorArchitecture="*" publicKeyToken="6595b64144ccf1df" language="*"/>'#$D#$A+
    '  </dependentAssembly>'#$D#$A+
    ' </dependency>'#$D#$A+
    ' <trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">'#$D#$A+
    '  <security>'#$D#$A+
    '   <requestedPrivileges>'#$D#$A+
    '    <requestedExecutionLevel level="%s" uiAccess="%s"/>'#$D#$A+
    '   </requestedPrivileges>'#$D#$A+
    '  </security>'#$D#$A+
    ' </trustInfo>'#$D#$A+
    ' <compatibility xmlns="urn:schemas-microsoft-com:compatibility.v1">'#$D#$A+
    '  <application>'#$D#$A+
    '   <!-- Windows Vista -->'#$D#$A+
    '   <supportedOS Id="{e2011457-1546-43c5-a5fe-008deee3d3f0}" />'#$D#$A+
    '   <!-- Windows 7 -->'#$D#$A+
    '   <supportedOS Id="{35138b9a-5d96-4fbd-8e2d-a2440225f93a}" />'#$D#$A+
    '   <!-- Windows 8 -->'#$D#$A+
    '   <supportedOS Id="{4a2f28e3-53b9-4441-ba9c-d69d4a4a6e38}" />'#$D#$A+
    '   <!-- Windows 8.1 -->'#$D#$A+
    '   <supportedOS Id="{1f676c76-80e1-4239-95bb-83d0f6d0da78}" />'#$D#$A+
    '   <!-- Windows 10 -->'#$D#$A+
    '   <supportedOS Id="{8e0f7a12-bfb3-4fe8-b9a5-48fd50a15a9a}" />'#$D#$A+
    '   </application>'#$D#$A+
    '  </compatibility>'#$D#$A+
    ' <asmv3:application xmlns:asmv3="urn:schemas-microsoft-com:asm.v3">'#$D#$A+
    '  <asmv3:windowsSettings xmlns="http://schemas.microsoft.com/SMI/2005/WindowsSettings">'#$D#$A+
    '   <dpiAware>%s</dpiAware>'#$D#$A+
    '  </asmv3:windowsSettings>'#$D#$A+
    '  <asmv3:windowsSettings xmlns="http://schemas.microsoft.com/SMI/2016/WindowsSettings">'#$D#$A+
    '   %s'#$D#$A+
    '   <longPathAware>%s</longPathAware>'#$D#$A+
    '  </asmv3:windowsSettings>'#$D#$A+
    ' </asmv3:application>'#$D#$A+
    '</assembly>';

function StrToXPManifestDpiAware(const s: string): TXPManifestDpiAware;
begin
  for Result:=Low(TXPManifestDpiAware) to High(TXPManifestDpiAware) do
    if CompareText(s,ManifestDpiAwareValues[Result])=0 then exit;
  Result:=xmdaFalse;
end;

function StrToXPManifestExecutionLevel(const s: string): TXPManifestExecutionLevel;
begin
  for Result:=Low(TXPManifestExecutionLevel) to High(TXPManifestExecutionLevel) do
    if CompareText(s,ExecutionLevelToStr[Result])=0 then exit;
  Result:=xmelAsInvoker;
end;

procedure TProjectXPManifest.SetUseManifest(const AValue: boolean);
begin
  if FUseManifest = AValue then exit;
  FUseManifest := AValue;
  Modified := True;
end;

procedure TProjectXPManifest.SetLongPathAware(AValue: Boolean);
begin
  if FLongPathAware = AValue then exit;
  FLongPathAware := AValue;
  Modified := True;
end;

procedure TProjectXPManifest.SetDpiAware(AValue: TXPManifestDpiAware);
begin
  if FDpiAware = AValue then Exit;
  FDpiAware := AValue;
  Modified := True;
end;

procedure TProjectXPManifest.SetExecutionLevel(AValue: TXPManifestExecutionLevel);
begin
  if FExecutionLevel = AValue then Exit;
  FExecutionLevel := AValue;
  Modified := True;
end;

procedure TProjectXPManifest.SetTextDesc(const AValue: string);
begin
  if FTextDesc = AValue then Exit;
  FTextDesc := AValue;
  Modified := True;
end;

procedure TProjectXPManifest.SetTextName(const AValue: string);
begin
  if FTextName = AValue then Exit;
  FTextName := AValue;
  Modified := True;
end;

procedure TProjectXPManifest.SetUIAccess(AValue: Boolean);
begin
  if FUIAccess = AValue then Exit;
  FUIAccess := AValue;
  Modified := True;
end;

constructor TProjectXPManifest.Create;
begin
  inherited Create;
  FIsDefaultOption := True;
  UseManifest := False;
  DpiAware := xmdaFalse;
  ExecutionLevel := xmelAsInvoker;
  UIAccess := False;
  LongPathAware := False;
  TextName := DefaultXPManifestTextName;
  TextDesc := DefaultXPManifestTextDesc;
end;

function TProjectXPManifest.UpdateResources(AResources: TAbstractProjectResources;
  const MainFilename: string): Boolean;
var
  Res: TGenericResource;
  RName, RType: TResourceDesc;
  ManifestFileData: String;
begin
  Result := True;
  if UseManifest then
  begin
    RType := TResourceDesc.Create(RT_MANIFEST);
    RName := TResourceDesc.Create(1);
    Res := TGenericResource.Create(RType, RName);
    RType.Free; //no longer needed
    RName.Free;
    ManifestFileData := Format(sManifestFileData, [
      TextName,
      TextDesc,
      ExecutionLevelToStr[ExecutionLevel],
      BoolToStr(UIAccess, 'true', 'false'),
      ManifestDpiAwareValues[DpiAware],
      ManifestDpiAwarenessValues[DpiAware],
      BoolToStr(LongPathAware, 'true', 'false')
      ]);
    Res.RawData.Write(ManifestFileData[1], Length(ManifestFileData));
    AResources.AddSystemResource(Res);
  end;
end;

procedure TProjectXPManifest.WriteToProjectFile(AConfig: TObject;
  const Path: String);
begin
  TXMLConfig(AConfig).SetDeleteValue(Path+'General/UseXPManifest/Value', UseManifest, False);
  TXMLConfig(AConfig).SetDeleteValue(Path+'General/XPManifest/DpiAware/Value', ManifestDpiAwareValues[DpiAware], ManifestDpiAwareValues[xmdaFalse]);
  TXMLConfig(AConfig).SetDeleteValue(Path+'General/XPManifest/ExecutionLevel/Value', ExecutionLevelToStr[ExecutionLevel], ExecutionLevelToStr[xmelAsInvoker]);
  TXMLConfig(AConfig).SetDeleteValue(Path+'General/XPManifest/UIAccess/Value', UIAccess, False);
  TXMLConfig(AConfig).SetDeleteValue(Path+'General/XPManifest/LongPathAware/Value', LongPathAware, False);
  TXMLConfig(AConfig).SetDeleteValue(Path+'General/XPManifest/TextName/Value', TextName, DefaultXPManifestTextName);
  TXMLConfig(AConfig).SetDeleteValue(Path+'General/XPManifest/TextDesc/Value', TextDesc, DefaultXPManifestTextDesc);
end;

procedure TProjectXPManifest.ReadFromProjectFile(AConfig: TObject;
  const Path: String);
var
  Cfg: TXMLConfig;
begin
  Cfg := TXMLConfig(AConfig);
  UseManifest := Cfg.GetValue(Path+'General/UseXPManifest/Value', False);

  //support prev values "True/False"
  if Cfg.GetValue(Path+'Version/Value',0)<=9 then
  begin
    if Cfg.GetValue(Path+'General/XPManifest/DpiAware/Value', False) then
      DpiAware := xmdaTrue
    else
      DpiAware := xmdaFalse;
  end else
    DpiAware := StrToXPManifestDpiAware(Cfg.GetValue(Path+'General/XPManifest/DpiAware/Value', ''));

  if Cfg.GetValue(Path+'Version/Value',0)<=9 then
    ExecutionLevel := TXPManifestExecutionLevel(Cfg.GetValue(Path+'General/XPManifest/ExecutionLevel/Value', 0))
  else
    ExecutionLevel := StrToXPManifestExecutionLevel(Cfg.GetValue(Path+'General/XPManifest/ExecutionLevel/Value', ''));

  UIAccess := Cfg.GetValue(Path+'General/XPManifest/UIAccess/Value', False);
  LongPathAware := Cfg.GetValue(Path+'General/XPManifest/LongPathAware/Value', False);
  TextName := Cfg.GetValue(Path+'General/XPManifest/TextName/Value', TextName);
  TextDesc := Cfg.GetValue(Path+'General/XPManifest/TextDesc/Value', TextDesc);
end;

initialization
  RegisterProjectResource(TProjectXPManifest);

end.

