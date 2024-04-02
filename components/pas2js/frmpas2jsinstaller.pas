unit FrmPas2jsInstaller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  StrPas2JSDesign, PJSDsgnOptions, IDEUtils, LazFileUtils;

type

  { TPas2jsInstallerDialog }

  TPas2jsInstallerDialog = class(TForm)
    ApplyButton: TButton;
    BtnPanel: TPanel;
    FPCExeLabel: TLabel;
    FPCSrcDirBrowseButton: TButton;
    CloseButton: TButton;
    DetailsGroupBox: TGroupBox;
    DetailsMemo: TMemo;
    FPCExeBrowseButton: TButton;
    FPCExeComboBox: TComboBox;
    FPCSrcDirComboBox: TComboBox;
    FPCGroupBox: TGroupBox;
    FPCSrcDirLabel: TLabel;
    FPCSrcDirVersionLabel: TLabel;
    Pas2jsExeBrowseButton: TButton;
    Pas2jsExeComboBox: TComboBox;
    Pas2jsExeGroupBox: TGroupBox;
    Pas2jsSrcDirBrowseBtn: TButton;
    Pas2jsSrcDirComboBox: TComboBox;
    Pas2jsSrcDirGroupBox: TGroupBox;
    Pas2jsSrcVersionLabel: TLabel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Pas2jsExeBrowseButtonClick(Sender: TObject);
  private
    FLastCheckedPas2jsExe: String;
    FLastCheckedPas2jsSrcDir: String;
    FLastCheckedPas2js: boolean;
    FOldPas2jsExe: string;
    FOldPas2jsSrcDir: string;
    FOldFPCExe: string;
    FOldFPCSrcDir: string;
    procedure UpdateButtons;
    function NeedsApply: boolean;
    function CheckPas2js: boolean;
  public
    procedure Init;
  end;

var
  Pas2jsInstallerDialog: TPas2jsInstallerDialog;

function ShowPas2jsInstallerDialog: boolean; // returns true if pas2js looks ok and user did not cancel

implementation

function ShowPas2jsInstallerDialog: boolean;
begin
  Pas2jsInstallerDialog:=TPas2jsInstallerDialog.Create(nil);
  try
    Pas2jsInstallerDialog.Init;
    Result:=Pas2jsInstallerDialog.ShowModal=mrOk;
  finally
    Pas2jsInstallerDialog.Free;
  end;
end;

{$R *.lfm}

{ TPas2jsInstallerDialog }

procedure TPas2jsInstallerDialog.FormCreate(Sender: TObject);
begin
  Caption:='Pas2js Installer';

  Pas2jsExeGroupBox.Caption:='Pas2js executable';
  Pas2jsExeBrowseButton.Hint:='Browse';

  Pas2jsSrcDirComboBox.Caption:='Pas2js source directory';
  Pas2jsSrcDirBrowseBtn.Hint:='Browse';

  FPCGroupBox.Caption:='Free Pascal Compiler used for compiling tools and pas2js itself';
  FPCExeLabel.Caption:='FPC executable:';
  FPCExeBrowseButton.Hint:='Browse';
  FPCSrcDirLabel.Caption:='FPC source directory:';
  FPCSrcDirBrowseButton.Hint:='Browse';

  DetailsGroupBox.Caption:='Details';
  DetailsMemo.Clear;

  ApplyButton.Caption:='Apply';
  CloseButton.Caption:='Close';
end;

procedure TPas2jsInstallerDialog.CloseButtonClick(Sender: TObject);
begin
  // restore options
  PJSOptions.CompilerFilename:=FOldPas2jsExe;
  PJSOptions.Pas2jsSrcDir:=FOldPas2jsSrcDir;
  PJSOptions.FPCExe:=FOldFPCExe;
  PJSOptions.FPCSrcDir:=FOldFPCSrcDir;

  if NeedsApply then
    ModalResult:=mrCancel
  else if CheckPas2js then
    ModalResult:=mrOk
  else
    ModalResult:=mrCancel;
end;

procedure TPas2jsInstallerDialog.Pas2jsExeBrowseButtonClick(Sender: TObject);
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
      SetComboBoxText(Pas2jsExeComboBox,AFilename,cstFilename,30);
      // ToDo CheckCompiler([mbOk]);
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TPas2jsInstallerDialog.UpdateButtons;
begin
  if NeedsApply then
  begin
    ApplyButton.Enabled:=true;
    CloseButton.Caption:='Cancel';
  end else begin
    ApplyButton.Enabled:=false;
    CloseButton.Caption:='Close';
  end;
end;

function TPas2jsInstallerDialog.NeedsApply: boolean;
var
  CurPas2jsExe, CurPas2jsSrcDir, CurFPCExe, CurFPCSrcDir: TCaption;
begin
  CurPas2jsExe:=Pas2jsExeComboBox.Text;
  CurPas2jsSrcDir:=Pas2jsSrcDirComboBox.Text;
  CurFPCExe:=FPCExeComboBox.Text;
  CurFPCSrcDir:=FPCSrcDirComboBox.Text;
  Result:=(CurPas2jsExe<>FOldPas2jsExe)
      or (CurPas2jsSrcDir<>FOldPas2jsSrcDir)
      or (CurFPCExe<>FOldFPCExe)
      or (CurFPCSrcDir<>FOldFPCSrcDir);
end;

function TPas2jsInstallerDialog.CheckPas2js: boolean;
var
  NewPas2jsExe, NewPas2jsSrcDir: String;
begin
  NewPas2jsExe:=PJSOptions.GetParsedCompilerFilename;
  NewPas2jsSrcDir:=PJSOptions.GetParsedPas2jsSrcDir;
  if (NewPas2jsExe<>FLastCheckedPas2jsExe)
      or (NewPas2jsSrcDir<>FLastCheckedPas2jsSrcDir) then
  begin
    FLastCheckedPas2js:=false;
    FLastCheckedPas2jsExe:=NewPas2jsExe;
    FLastCheckedPas2jsSrcDir:=NewPas2jsSrcDir;
    if (NewPas2jsExe='') or not FileExistsUTF8(NewPas2jsExe) then
    else if not FileIsExecutable(NewPas2jsExe) then
    else
      FLastCheckedPas2js:=true;
  end;
  Result:=FLastCheckedPas2js;
end;

procedure TPas2jsInstallerDialog.Init;
begin
  FOldPas2jsExe:=PJSOptions.CompilerFilename;
  FOldPas2jsSrcDir:=PJSOptions.Pas2jsSrcDir;
  FOldFPCExe:=PJSOptions.FPCExe;
  FOldFPCSrcDir:=PJSOptions.FPCSrcDir;

  SetComboBoxText(Pas2jsExeComboBox,PJSOptions.CompilerFilename,cstFilename,30);
  SetComboBoxText(Pas2jsSrcDirComboBox,PJSOptions.Pas2jsSrcDir,cstFilename,30);
  SetComboBoxText(FPCExeComboBox,PJSOptions.FPCExe,cstFilename,30);
  SetComboBoxText(FPCSrcDirComboBox,PJSOptions.FPCSrcDir,cstFilename,30);

  UpdateButtons;
end;

end.

