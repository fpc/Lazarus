{ IDE options frame for pas2js options

  Author: Mattias Gaertner
}
unit PJSDsgnOptsFrame;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs, Spin, IDEOptionsIntf,
  IDEUtils, LazFileCache, PJSDsgnOptions,
  LazFileUtils;

Type
  { TPas2jsOptionsFrame }

  TPas2jsOptionsFrame = class(TAbstractIDEOptionsEditor)
    BBrowserBrowseButton: TButton;
    NodeJSBrowseButton: TButton;
    BrowserComboBox: TComboBox;
    NodeJSComboBox: TComboBox;
    BrowserLabel1: TLabel;
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

procedure TPas2jsOptionsFrame.HTTPServerBrowseButtonClick(Sender: TObject);

var
  OpenDialog: TOpenDialog;
  AFilename: String;

begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:='Select simpleserver executable';
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
    OpenDialog.Title:='Select browser executable';
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
    OpenDialog.Title:='Select browser executable';
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
  Pas2jsPathLabel.Caption:='Path of '+ExeName;
  Pas2jsPathLabel.Hint:='You can use IDE macros like $MakeExe(). Without a full path, '+ExeName+' is searched in PATH.';
  Pas2jsPathBrowseButton.Caption:='...';
  Pas2jsPathBrowseButton.Hint:='Browse';
  HTTPServerCmdLabel.Caption:='Path of '+ServerName;
  HTTPServerCmdLabel.Hint:='You can use IDE macros like $MakeExe(). Without a full path, '+ServerName+' is searched in PATH.';
  HTTPServerBrowseButton.Caption:='...';
  HTTPServerBrowseButton.Hint:='Browse';
  ServerPortLabel.Caption:='Port numbers to start allocating from '+ServerName;
  ServerPortLabel.Hint:='Server instances will be started with a port starting from this number, increasing per new project';
  BrowserLabel.Caption:='Browser to use when opening HTML page';
  BrowserLabel.Hint:='Use this browser when opening the URL or HTML file of a web browser project';
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

