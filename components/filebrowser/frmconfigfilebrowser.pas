unit frmConfigFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  StdCtrls, Dialogs, EditBtn,
  IDEOptionsIntf,
  lazIDEIntf, IDEOptEditorIntf,
  FileBrowserTypes, CtrlFileBrowser;

type

  { TFileBrowserOptionsFrame }

  TFileBrowserOptionsFrame = class(TAbstractIDEOptionsEditor)
    CBSyncCurrentEditor: TCheckBox;
    DEStartDir: TDirectoryEdit;
    DERootDir: TDirectoryEdit;
    GBStartDir: TGroupBox;
    GBStartDir1: TGroupBox;
    GBFileTree: TGroupBox;
    RBLastDir: TRadioButton;
    RBRootFileSystemRoot: TRadioButton;
    RBRootUserDir: TRadioButton;
    RBThisDir: TRadioButton;
    RBRootThisDir: TRadioButton;
    RBUseProjectDir: TRadioButton;
    RBRootUseProjectDir: TRadioButton;
  private
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;

  end;

implementation

{$R *.lfm}

{ TFileBrowserOptionsFrame }

function TFileBrowserOptionsFrame.GetTitle: String;
begin
  Result:='File browser options';
end;

procedure TFileBrowserOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  //
end;

procedure TFileBrowserOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  C : TFileBrowserController;
  RB: TRadioButton;
begin
  C:=LazarusIDE.OwningComponent.FindComponent('IDEFileBrowserController') as TFileBrowserController;
  if not Assigned(C) then
    exit;
  case C.StartDir of
    sdProjectDir: RB := RBUseProjectDir;
    sdLastOpened: RB := RBLastDir;
    sdCustomDir: RB  := RBThisDir;
  end;
  RB.Checked := True;
  if C.StartDir=sdCustomDir then
    DEStartDir.Directory:=C.CustomStartDir;
  case C.RootDir of
    rdProjectDir: RB := RBUseProjectDir;
    rdRootDir: RB := RBRootFileSystemRoot;
    rdUserDir : RB  := RBRootUserDir;
    rdCustomDir: RB  := RBRootThisDir;
  end;
  RB.Checked := True;
  if C.RootDir=rdCustomDir then
    DERootDir.Directory:=C.CustomRootDir;
  CBSyncCurrentEditor.Checked:=C.SyncCurrentEditor;
end;

procedure TFileBrowserOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  C : TFileBrowserController;
  SD : TStartDir;
  RD : TRootDir;
  lRootDir: String;
begin
  C:=LazarusIDE.OwningComponent.FindComponent('IDEFileBrowserController') as TFileBrowserController;
  if not Assigned(C) then
    exit;
  lRootDir:=C.GetResolvedRootDir;
  if RBUseProjectDir.Checked then
    SD:=sdProjectDir
  else if RBLastDir.Checked then
    SD:=sdLastOpened
  else
    SD:=sdCustomDir;
  C.StartDir:=SD;
  if SD=sdCustomDir then
    C.CustomStartDir:=DEStartDir.Directory
  else
    C.CustomStartDir:='';

  if RBRootUseProjectDir.Checked then
    RD:=rdProjectDir
  else if RBRootFileSystemRoot.Checked then
    RD:=rdRootDir
  else if RBRootUserDir.Checked then
    RD:=rdUserDir
  else
    RD:=rdCustomDir;
  C.RootDir:=rD;
  if rD=rdCustomDir then
    C.CustomRootDir:=DERootDir.Directory
  else
    C.CustomRootDir:='';
  C.SyncCurrentEditor:=CBSyncCurrentEditor.Checked;
  C.WriteConfig;
end;

class function TFileBrowserOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

