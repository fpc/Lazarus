{ IDE options frame for pas2js options

  Author: Mattias Gaertner
}
unit PJSDsgnOptsFrame;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, Dialogs, Spin,
  // LazUtils
  LazFileCache, LazFileUtils, FileUtil,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEUtils, IDEDialogs,
  // Pas2Js
  PJSDsgnOptions, strpas2jsdesign;

Type
  { TPas2jsOptionsFrame }

  TPas2jsOptionsFrame = class(TAbstractIDEOptionsEditor)
    BBrowserBrowseButton: TButton;
    BrowserComboBox: TComboBox;
    BrowserLabel: TLabel;
    HTTPServerBrowseButton: TButton;
    HTTPServerCmdLabel: TLabel;
    HTTPServerComboBox: TComboBox;
    NodeJSBrowseButton: TButton;
    NodeJSComboBox: TComboBox;
    NodeJSLabel: TLabel;
    Pas2jsPathBrowseButton: TButton;
    Pas2jsPathComboBox: TComboBox;
    Pas2jsPathLabel: TLabel;
    ServerPortLabel: TLabel;
    ServerPortSpinEdit: TSpinEdit;
    procedure BBrowserBrowseButtonClick(Sender: TObject);
    procedure HTTPServerBrowseButtonClick(Sender: TObject);
    procedure NodeJSBrowseButtonClick(Sender: TObject);
    procedure Pas2jsPathBrowseButtonClick(Sender: TObject);
  private
    function CheckCompiler({%H-}Buttons: TMsgDlgButtons): boolean;
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

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
    OpenDialog.Title:=pjsdSelectPas2jsExecutable;
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(Pas2jsPathComboBox,AFilename,cstFilename,30);
      CheckCompiler([mbOk]);
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TPas2jsOptionsFrame.HTTPServerBrowseButtonClick(Sender: TObject);

var
  OpenDialog: TOpenDialog;
  AFilename: String;

begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=SafeFormat(pjsdSelectXExecutable,[PJSDefaultWebServer]);
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(HTTPServerComboBox,AFilename,cstFilename,30);
      PJSOptions.WebServerFileName:=AFileName;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TPas2jsOptionsFrame.NodeJSBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: String;

begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=pjsdSelectNodeJSExecutable;
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(NodeJSComboBox,AFilename,cstFilename,30);
      PJSOptions.NodeJSFileName:=AFileName;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TPas2jsOptionsFrame.BBrowserBrowseButtonClick(Sender: TObject);

var
  OpenDialog: TOpenDialog;
  AFilename: String;

begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=pjsdSelectBrowserExecutable;
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(BrowserComboBox,AFilename,cstFilename,30);
      PJSOptions.BrowserFileName:=AFileName;
    end;
  finally
    OpenDialog.Free;
  end;
end;

function TPas2jsOptionsFrame.CheckCompiler(Buttons: TMsgDlgButtons): boolean;

var
  NewExe, ErrMsg: string;

begin
  NewExe:=Pas2jsPathComboBox.Text;
  if NewExe=PJSOptions.CompilerFilename then exit(true);
  Result:=false;
  PJSOptions.CompilerFilename:=NewExe;
  NewExe:=PJSOptions.GetParsedCompilerFilename;
  if (NewExe='') or not FileExistsUTF8(NewExe) then
    ErrMsg:='Unable to find pas2js at "'+PJSOptions.CompilerFilename+'"'
  else if not FileIsExecutable(NewExe) then
    ErrMsg:='pas2js is not executable at "'+PJSOptions.CompilerFilename+'"'
  else
    ErrMsg:='';
  if ErrMsg<>'' then
  begin
    IDEMessageDialog('Error',ErrMsg,mtError,[mbOk]);
    exit(false);
  end;
  if Pos('pas2js',lowercase(ExtractFileNameOnly(NewExe)))<1 then
  begin
    IDEMessageDialog('Warning','The pas2js executable filename "'+NewExe+'" does not look like pas2js',mtWarning,[mbOk]);
    exit(true);
  end;
  // todo: run and check if this pas2js returns macros
end;

function TPas2jsOptionsFrame.GetTitle: String;
begin
  Result:='Pas2JS';
end;

procedure TPas2jsOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  DefPas2jsExe: String;
begin
  DefPas2jsExe:=GetStandardPas2jsExe;

  Pas2jsPathLabel.Caption:=SafeFormat(pjsdPathOfXMacroPas2js, ['pas2js'+GetExeExt]);
  Pas2jsPathLabel.Hint:=Format(
    pjsdYouCanUseIDEMacrosLikeMakeExeWithoutAFullPathIsSea, [DefPas2jsExe]);
  Pas2jsPathBrowseButton.Hint:=pjsdBrowse;

  HTTPServerCmdLabel.Caption:=SafeFormat(pjsdPathOfXMacroPas2JSWebServer, [
    PJSDefaultWebServerName+GetExeExt]);
  HTTPServerCmdLabel.Hint:=SafeFormat(
    pjsdYouCanUseIDEMacrosLikeMakeExeWithoutAFullPathIsSea, [PJSDefaultWebServerName]);
  HTTPServerBrowseButton.Hint:=pjsdBrowse;

  ServerPortLabel.Caption:=pjsdPortNumberToStartAllocatingFrom;
  ServerPortLabel.Hint:=pjsdServerInstancesWillBeStartedWithAPortStartingFromT;

  BrowserLabel.Caption:=pjsdBrowserToOpenHTMLPage;
  BrowserLabel.Hint:=pjsdUseThisBrowserWhenOpeningTheURLOrHTMLFileOfAWebBro;

  NodeJSLabel.Caption:=pjsdPathOfNodeJsExecutable;
end;

procedure TPas2jsOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  SetComboBoxText(Pas2jsPathComboBox,PJSOptions.CompilerFilename,cstFilename,30);
  SetComboBoxText(HTTPServerComboBox,PJSOptions.WebServerFileName,cstFilename,30);
  ServerPortSpinEdit.Value:=PJSOptions.StartAtPort;
  SetComboBoxText(BrowserComboBox,PJSOptions.BrowserFileName,cstFilename,30);
  SetComboBoxText(NodeJSComboBox,PJSOptions.NodejsFileName,cstFilename,30);
end;

procedure TPas2jsOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  PJSOptions.CompilerFilename:=Pas2jsPathComboBox.Text;
  PJSOptions.WebServerFileName:=HTTPServerComboBox.Text;
  PJSOptions.StartAtPort:=ServerPortSpinEdit.Value;
  PJSOptions.BrowserFileName:=BrowserComboBox.Text;
  PJSOptions.NodeJSFileName:=NodeJSComboBox.Text;
  If PJSOptions.Modified then
    PJSOptions.Save;
end;

class function TPas2jsOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

