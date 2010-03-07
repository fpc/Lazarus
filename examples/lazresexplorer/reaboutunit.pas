unit reAboutUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, versionresource;

type

  { TreAboutForm }

  TreAboutForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    lblLicense: TLabel;
    Label2: TLabel;
    lblBuildDate: TLabel;
    lblFPCVersion: TLabel;
    lblLCLVersion: TLabel;
    lblTargCPU: TLabel;
    lblTargetOS: TLabel;
    lblWidgetName: TLabel;
    ListBox1: TListBox;
    lblAppName: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure ParceVersionInfo(V:TVersionResource);
  public
    { public declarations }
  end; 

var
  reAboutForm: TreAboutForm;

const
  appBuildDate = {$I %DATE%};
  TargetOS = {$I %FPCTARGETOS%};
  fpcVersion = {$I %FPCVERSION%};
  TargetCPU = {$I %FPCTARGETCPU%};

implementation
uses reConstsUnit, resource, resreader, LCLVersion,
{$IFDEF WINDOWS}
  winpeimagereader
{$ELSE}
  elfreader
{$ENDIF}
;

{$R *.lfm}

{ TreAboutForm }

procedure TreAboutForm.FormCreate(Sender: TObject);
var
  Res:TResources;
  i:integer;
  Reader:TAbstractResourceReader;
  V:TVersionResource;
begin
  Caption:=sAbout;
  lblTargCPU.Caption:=sLCLVersion + lcl_version;
  lblBuildDate.Caption:=sBuildDate + appBuildDate;
  lblLCLVersion.Caption:=sLCLVersion + LCLVersion;
  lblFPCVersion.Caption:=sFpcVersion + fpcVersion;
  lblTargCPU.Caption:=sTargetCPU + TargetCPU;
  lblTargetOS.Caption:=sTargetOS + TargetOS;
  lblAppName.Caption:=sResourceExplorer;
  lblWidgetName.Caption:=LCLVersionStr;
  lblLicense.Caption:=sLicense;

  ListBox1.Items.Clear;
  {$IFDEF WINDOWS}
  Reader:=TWinPEImageResourceReader.Create;
  {$ELSE}
  Reader:=TElfResourceReader.Create;
  {$ENDIF}
  Res:=TResources.Create;
  Res.LoadFromFile(ParamStr(0), Reader);
  for i:=0 to Res.Count-1 do
  begin
    if Res[i] is TVersionResource then
      V:=Res[i] as TVersionResource;
  end;
  if Assigned(V) then
    ParceVersionInfo(V);
  Res.Free;
  Reader.Free;
end;

procedure TreAboutForm.ParceVersionInfo(V: TVersionResource);
var
  i,j:integer;
begin
  for i:=0 to V.StringFileInfo.Count-1 do
  begin
    for j:=0 to V.StringFileInfo[i].Count-1 do
      ListBox1.Items.Add(SysToUTF8(V.StringFileInfo[i].Keys[j])+' = ' + SysToUTF8(V.StringFileInfo[i].ValuesByIndex[j]));
  end;
end;

end.

