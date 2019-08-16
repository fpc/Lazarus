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
  LazFileCache, LazFileUtils,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEUtils,
  // Pas2Js
  PJSDsgnOptions, strpas2jsdesign;

Type
  { TPas2jsOptionsFrame }

  TPas2jsOptionsFrame = class(TAbstractIDEOptionsEditor)
    BBrowserBrowseButton: TButton;
    NodeJSBrowseButton: TButton;
    BrowserComboBox: TComboBox;
    NodeJSComboBox: TComboBox;
    NodeJSLabel: TLabel;
    HTTPServerBrowseButton: TButton;
    HTTPServerComboBox: TComboBox;
    HTTPServerCmdLabel: TLabel;
    BrowserLabel: TLabel;
    ServerPortLabel: TLabel;
    Pas2jsPathBrowseButton: TButton;
    Pas2jsPathComboBox: TComboBox;
    Pas2jsPathLabel: TLabel;
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
    OpenDialog.Title:=pjsdSelectSimpleserverExecutable;
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(HTTPServerComboBox,AFilename,cstFilename,30);
      PJSOptions.HTTPServerFileName:=AFileName;
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
  NewExe: string;

begin
  NewExe:=Pas2jsPathComboBox.Text;
  if NewExe=PJSOptions.CompilerFilename then exit(true);
  Result:=false;
  PJSOptions.CompilerFilename:=NewExe;
  // ToDo: check file
  //NewExe:=PJSOptions.GetParsedCompilerExe;
end;

function TPas2jsOptionsFrame.GetTitle: String;
begin
  Result:='Pas2JS';
end;

procedure TPas2jsOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);

var
  ExeName: String;
  ServerName : String;
  //BrowserName : String;

begin
  ExeName:=GetStandardPas2jsExe;
  ServerName:=GetStandardHTTPServer;
  //BrowserName:=GetStandardBrowser;
  Pas2jsPathLabel.Caption:=Format(pjsdPathOf, [ExeName]);
  Pas2jsPathLabel.Hint:=Format(
    pjsdYouCanUseIDEMacrosLikeMakeExeWithoutAFullPathIsSea, [ExeName]);
  Pas2jsPathBrowseButton.Hint:=pjsdBrowse;
  HTTPServerCmdLabel.Caption:=Format(pjsdPathOf, [ServerName]);
  HTTPServerCmdLabel.Hint:=Format(
    pjsdYouCanUseIDEMacrosLikeMakeExeWithoutAFullPathIsSea, [ServerName]);
  HTTPServerBrowseButton.Hint:=pjsdBrowse;
  ServerPortLabel.Caption:=Format(pjsdPortNumbersToStartAllocatingFrom, [
    ServerName]);
  ServerPortLabel.Hint:=pjsdServerInstancesWillBeStartedWithAPortStartingFromT;
  BrowserLabel.Caption:=pjsdBrowserToUseWhenOpeningHTMLPage;
  BrowserLabel.Hint:=pjsdUseThisBrowserWhenOpeningTheURLOrHTMLFileOfAWebBro;
  NodeJSLabel.Caption:=pjsdPathOfNodeJsExecutable;
end;

procedure TPas2jsOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  SetComboBoxText(Pas2jsPathComboBox,PJSOptions.CompilerFilename,cstFilename,30);
  SetComboBoxText(HTTPServerComboBox,PJSOptions.HTTPServerFileName,cstFilename,30);
  SetComboBoxText(BrowserComboBox,PJSOptions.BrowserFileName,cstFilename,30);
  SetComboBoxText(NodeJSComboBox,PJSOptions.NodejsFileName,cstFilename,30);
  ServerPortSpinEdit.Value:=PJSOptions.StartAtPort;
end;

procedure TPas2jsOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  PJSOptions.CompilerFilename:=Pas2jsPathComboBox.Text;
  PJSOptions.HTTPServerFileName:=HTTPServerComboBox.Text;
  PJSOptions.BrowserFileName:=BrowserComboBox.Text;
  PJSOptions.NodeJSFileName:=NodeJSComboBox.Text;
  PJSOptions.StartAtPort:=ServerPortSpinEdit.Value;
  If PJSOptions.Modified then
    PJSOptions.Save;
end;

class function TPas2jsOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

