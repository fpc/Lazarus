unit frmConfigFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  StdCtrls, Dialogs, EditBtn,
  LazLoggerBase,
  IDEOptionsIntf,
  lazIDEIntf, IDEOptEditorIntf,
  FileBrowserTypes, CtrlFileBrowser;

type

  { TFileBrowserOptionsFrame }

  TFileBrowserOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbSyncCurrentEditor: TCheckBox;
    deRootDir: TDirectoryEdit;
    gbStartDir: TGroupBox;
    GBFileTree: TGroupBox;
    rbRootFileSystemRoot: TRadioButton;
    rbRootUserDir: TRadioButton;
    rbRootThisDir: TRadioButton;
    rbRootUseProjectDir: TRadioButton;
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
  case C.RootDir of
    rdProjectDir   : RB := rbRootUseProjectDir;
    rdSystemRootDir: RB := rbRootFileSystemRoot;
    rdUserDir      : RB := rbRootUserDir;
    rdCustomDir    : RB := rbRootThisDir;
  end;
  RB.Checked := True;
  deRootDir.Directory := C.CustomRootDir;
  cbSyncCurrentEditor.Checked := C.SyncCurrentEditor;
end;

procedure TFileBrowserOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  C: TFileBrowserController;
  RD: TRootDir;
begin
  C:=LazarusIDE.OwningComponent.FindComponent('IDEFileBrowserController') as TFileBrowserController;
  if not Assigned(C) then
    exit;
  if rbRootUseProjectDir.Checked then
    RD := rdProjectDir
  else if rbRootFileSystemRoot.Checked then
    RD := rdSystemRootDir
  else if rbRootUserDir.Checked then
    RD := rdUserDir
  else
    RD := rdCustomDir;
  C.CustomRootDir := deRootDir.Directory;
  C.RootDir := RD;
  C.SyncCurrentEditor := cbSyncCurrentEditor.Checked;
  C.WriteConfig;
end;

class function TFileBrowserOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

