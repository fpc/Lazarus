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
  LazFileCache, LazFileUtils, LazStringUtils, FileUtil,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEUtils, IDEDialogs,
  // Pas2Js
  PJSDsgnOptions, strpas2jsdesign, PJSController, SimpleWebSrvOptionsFrame;

Type
  { TPas2jsOptionsFrame }

  TPas2jsOptionsFrame = class(TAbstractIDEOptionsEditor)
    lblElectronExe: TLabel;
    SimpleWebServerLinkLabel: TLabel;
    VSCodeTemplateDirBrowseButton: TButton;
    ElectronExeBrowseButton: TButton;
    VSCodeTemplateDirComboBox: TComboBox;
    lblVSCodeTemplateDir: TLabel;
    NodeJSBrowseButton: TButton;
    AtomTemplateDirBrowseButton: TButton;
    NodeJSComboBox: TComboBox;
    AtomTemplateDirComboBox: TComboBox;
    NodeJSLabel: TLabel;
    lblAtomTemplateDir: TLabel;
    Pas2jsPathBrowseButton: TButton;
    Pas2jsPathComboBox: TComboBox;
    Pas2jsPathLabel: TLabel;
    ServerPortLabel: TLabel;
    ServerPortSpinEdit: TSpinEdit;
    ElectronExeComboBox: TComboBox;
    procedure AtomTemplateDirBrowseButtonClick(Sender: TObject);
    procedure ElectronExeBrowseButtonClick(Sender: TObject);
    procedure NodeJSBrowseButtonClick(Sender: TObject);
    procedure Pas2jsPathBrowseButtonClick(Sender: TObject);
    procedure SimpleWebServerLinkLabelClick(Sender: TObject);
    procedure VSCodeTemplateDirBrowseButtonClick(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
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

procedure TPas2jsOptionsFrame.SimpleWebServerLinkLabelClick(Sender: TObject);
begin
  FDialog.OpenEditor(GroupEnvironment,SimpleWebServerOptionID);
end;

procedure TPas2jsOptionsFrame.VSCodeTemplateDirBrowseButtonClick(Sender: TObject
  );
var
  ADirname: String;

begin
  aDirName:=VSCodeTemplateDirComboBox.Text;
  if SelectDirectory(pjsdSelectVSCodeTemplateDir,aDirName,aDirName) then
  begin
    ADirName:=CleanAndExpandFilename(ADirName);
    SetComboBoxText(VSCodeTemplateDirComboBox,ADirName,cstFilename,30);
    PJSOptions.VSCodeTemplateDir:=ADirName;
  end;
end;

procedure TPas2jsOptionsFrame.AtomTemplateDirBrowseButtonClick(Sender: TObject);

var
  ADirname: String;

begin
  aDirName:=AtomTemplateDirComboBox.Text;
  if SelectDirectory(pjsdSelectAtomTemplateDir,aDirName,aDirName) then
  begin
    ADirName:=CleanAndExpandFilename(ADirName);
    SetComboBoxText(AtomTemplateDirComboBox,ADirName,cstFilename,30);
    PJSOptions.AtomTemplateDir:=ADirName;
  end;
end;

procedure TPas2jsOptionsFrame.ElectronExeBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TIDEOpenDialog;
  AFilename: String;
begin
  OpenDialog:=IDEOpenDialogClass.Create(nil);
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:='Select Electron executable';
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(ElectronExeComboBox,AFilename,cstFilename,30);
      PJSOptions.ElectronFileName:=AFileName;
    end;
  finally
    StoreIDEFileDialog(OpenDialog);
    OpenDialog.Free;
  end;
end;

procedure TPas2jsOptionsFrame.NodeJSBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TIDEOpenDialog;
  AFilename: String;
begin
  OpenDialog:=IDEOpenDialogClass.Create(nil);
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=pjsdSelectNodeJSExecutable;
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(NodeJSComboBox,AFilename,cstFilename,30);
      PJSOptions.NodeJSFileName:=AFileName;
    end;
  finally
    StoreIDEFileDialog(OpenDialog);
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
  if PosI('pas2js',ExtractFileNameOnly(NewExe))<1 then
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
  FDialog := ADialog;

  DefPas2jsExe:=GetStandardPas2jsExe;
  Pas2jsPathLabel.Caption:=SafeFormat(pjsdPathOfXMacroPas2js, ['pas2js'+GetExeExt]);
  Pas2jsPathLabel.Hint:=Format(
    pjsdYouCanUseIDEMacrosLikeMakeExeWithoutAFullPathIsSea, [DefPas2jsExe]);
  Pas2jsPathBrowseButton.Hint:=pjsdBrowse;

  SimpleWebServerLinkLabel.Caption:=pjsdWebServerAndBrowserOptions;
  ServerPortLabel.Caption:=pjsdPortNumberToStartAllocatingFrom;
  ServerPortLabel.Hint:=pjsdServerInstancesWillBeStartedWithAPortStartingFromT;

  NodeJSLabel.Caption:=pjsdPathOfNodeJsExecutable;
  lblElectronExe.Caption:=pjsdPathOfElectronExecutableMacroPas2JSElectron;

  lblAtomTemplateDir.Caption := pjsdAtomPackageTemplateDirectory;
  lblVSCodeTemplateDir.Caption := pjsdVisualStudioCodeExtensionTemplateDirectory;
end;

procedure TPas2jsOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  SetComboBoxText(Pas2jsPathComboBox,PJSOptions.CompilerFilename,cstFilename,30);
  ServerPortSpinEdit.Value:=PJSOptions.StartAtPort;
  SetComboBoxText(NodeJSComboBox,PJSOptions.NodejsFileName,cstFilename,30);
  SetComboBoxText(ElectronExeComboBox,PJSOptions.ElectronFileName,cstFilename,30);
  SetComboBoxText(AtomTemplateDirComboBox,PJSOptions.AtomTemplateDir,cstFilename,30);
  SetComboBoxText(VSCodeTemplateDirComboBox,PJSOptions.VSCodeTemplateDir,cstFilename,30);
end;

procedure TPas2jsOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  PJSOptions.CompilerFilename:=Pas2jsPathComboBox.Text;
  PJSOptions.StartAtPort:=ServerPortSpinEdit.Value;
  PJSOptions.NodeJSFileName:=NodeJSComboBox.Text;
  PJSOptions.ElectronFileName:=ElectronExeComboBox.Text;
  PJSOptions.AtomTemplateDir:=AtomTemplateDirComboBox.Text;
  PJSOptions.VSCodeTemplateDir:=VSCodeTemplateDirComboBox.Text;
  TPJSController.Instance.StoreMacros;
  If PJSOptions.Modified then
    PJSOptions.Save;
end;

class function TPas2jsOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

