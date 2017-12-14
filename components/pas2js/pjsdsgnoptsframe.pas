{ Installs pas2js options frame in the Lazarus IDE.

  Copyright (C) 2017 Mattias Gaertner mattias@freepascal.org
}
unit PJSDsgnOptsFrame;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs, IDEOptionsIntf,
  MacroIntf, BaseIDEIntf, IDEUtils, LazFileCache, LazConfigStorage,
  LazFileUtils;

const
  PJSDsgnOptsFile = 'pas2jsdsgnoptions.xml';
  PJSDefaultCompiler = '$MakeExe(IDE,pas2js)';

type

  { TPas2jsOptions }

  TPas2jsOptions = class
  private
    FChangeStamp: int64;
    FSavedStamp: int64;
    FCompilerFilename: string;
    function GetModified: boolean;
    procedure SetModified(AValue: boolean);
    procedure SetCompilerFilename(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncreaseChangeStamp; inline;
    procedure Load;
    procedure Save;
    procedure LoadFromConfig(Cfg: TConfigStorage);
    procedure SaveToConfig(Cfg: TConfigStorage);
  public
    property CompilerFilename: string read FCompilerFilename write SetCompilerFilename;
    property ChangeStamp: int64 read FChangeStamp;
    property Modified: boolean read GetModified write SetModified;
  end;

  { TPas2jsOptionsFrame }

  TPas2jsOptionsFrame = class(TAbstractIDEOptionsEditor)
    Pas2jsPathBrowseButton: TButton;
    Pas2jsPathComboBox: TComboBox;
    Pas2jsPathLabel: TLabel;
    procedure Pas2jsPathBrowseButtonClick(Sender: TObject);
  private
    function CheckCompiler(Buttons: TMsgDlgButtons): boolean;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

var
  PJSOptions: TPas2jsOptions = nil;

function GetStandardPas2jsExe: string;

implementation

function GetStandardPas2jsExe: string;
begin
  Result:='$MakeExe(IDE,pas2js)';
  if not IDEMacros.SubstituteMacros(Result) then
    Result:='pas2js';
end;

{$R *.lfm}

{ TPas2jsOptions }

procedure TPas2jsOptions.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    FSavedStamp:=FChangeStamp;
end;

function TPas2jsOptions.GetModified: boolean;
begin
  Result:=FSavedStamp<>FChangeStamp;
end;

procedure TPas2jsOptions.SetCompilerFilename(AValue: string);
begin
  if FCompilerFilename=AValue then Exit;
  FCompilerFilename:=AValue;
  IncreaseChangeStamp;
end;

constructor TPas2jsOptions.Create;
begin
  FChangeStamp:=LUInvalidChangeStamp64;
  FCompilerFilename:=PJSDefaultCompiler;
end;

destructor TPas2jsOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TPas2jsOptions.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp64(FChangeStamp);
end;

procedure TPas2jsOptions.Load;
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(PJSDsgnOptsFile,true);
  try
    LoadFromConfig(Cfg);
  finally
    Cfg.Free;
  end;
end;

procedure TPas2jsOptions.Save;
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(PJSDsgnOptsFile,false);
  try
    SaveToConfig(Cfg);
  finally
    Cfg.Free;
  end;
end;

procedure TPas2jsOptions.LoadFromConfig(Cfg: TConfigStorage);
begin
  CompilerFilename:=Cfg.GetValue('compiler/value',PJSDefaultCompiler);
  Modified:=false;
end;

procedure TPas2jsOptions.SaveToConfig(Cfg: TConfigStorage);
begin
  Cfg.SetDeleteValue('compiler/value',CompilerFilename,PJSDefaultCompiler);
end;

{ TPas2jsOptionsFrame }

procedure TPas2jsOptionsFrame.Pas2jsPathBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: String;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:='Select pas2js executable';
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(Pas2jsPathComboBox,AFilename,cstFilename,30);
      CheckCompiler([mbOk]);
    end;
  finally
    OpenDialog.Free;
  end;
end;

function TPas2jsOptionsFrame.CheckCompiler(Buttons: TMsgDlgButtons): boolean;
var
  NewExe: TCaption;
begin
  NewExe:=Pas2jsPathComboBox.Text;
  if NewExe=PJSOptions.CompilerFilename then exit(true);
  Result:=false;
  PJSOptions.CompilerFilename:=NewExe;
  //NewExe:=PJSOptions.GetParsedCompilerExe;
end;

function TPas2jsOptionsFrame.GetTitle: String;
begin
  Result:='Pas2JS';
end;

procedure TPas2jsOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  ExeName: String;
begin
  ExeName:=GetStandardPas2jsExe;
  Pas2jsPathLabel.Caption:='Path of '+ExeName;
  Pas2jsPathLabel.Hint:='You can use IDE macros like $MakeExe(). Without a full path, '+ExeName+' is searched in PATH.';
  Pas2jsPathBrowseButton.Caption:='...';
  Pas2jsPathBrowseButton.Hint:='Browse';
end;

procedure TPas2jsOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  SetComboBoxText(Pas2jsPathComboBox,PJSOptions.CompilerFilename,cstFilename,30);
end;

procedure TPas2jsOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  PJSOptions.CompilerFilename:=Pas2jsPathComboBox.Text;
end;

class function TPas2jsOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

finalization
  FreeAndNil(PJSOptions);
end.

