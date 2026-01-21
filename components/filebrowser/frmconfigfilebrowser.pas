unit frmConfigFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  // IdeIntf
  Classes, SysUtils, Forms, Controls, StdCtrls,
  Dialogs, FileCtrl, ComCtrls, ExtCtrls,  EditBtn,
  IDEOptionsIntf, IDEOptEditorIntf, IDEUtils, IDEDialogs;

type

  { TFileBrowserOptionsFrame }

  TFileBrowserOptionsFrame = class(TAbstractIDEOptionsEditor)
    CBShowDirectoriesBeforeFiles: TCheckBox;
    CBShowFilesInline: TCheckBox;
    CBSyncCurrentEditor: TCheckBox;
    CBMatchOnlyFilename: TCheckBox;
    CBUseAbsoluteFilenames: TCheckBox;
    CBUseLetters: TCheckBox;
    CBpartialMatch: TCheckBox;
    DEStartDir: TDirectoryEdit;
    DERootDir: TDirectoryEdit;
    GBStartDir: TGroupBox;
    GBStartDir1: TGroupBox;
    GBSearch: TGroupBox;
    GBFileTree: TGroupBox;
    RBLastDir: TRadioButton;
    RBRootFileSystemRoot: TRadioButton;
    RBRootUserDir: TRadioButton;
    RBThisDir: TRadioButton;
    RBRootThisDir: TRadioButton;
    RBUseProjectDir: TRadioButton;
    RBRootUseProjectDir: TRadioButton;
    procedure CBShowFilesInlineChange(Sender: TObject);
    procedure CBUseLettersChange(Sender: TObject);
  private
    procedure CheckDirsBeforeFiles;
    procedure CheckPartial;

  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;

  end;

implementation

uses lazIDEIntf, filebrowsertypes, ctrlfilebrowser;

{$R *.lfm}

{ TFileBrowserOptionsFrame }

procedure TFileBrowserOptionsFrame.CheckDirsBeforeFiles;

begin
  CBShowDirectoriesBeforeFiles.Enabled:=CBShowFilesInline.Checked;
  if Not CBShowDirectoriesBeforeFiles.Enabled then
    CBShowDirectoriesBeforeFiles.Checked:=False;
end;

procedure TFileBrowserOptionsFrame.CBShowFilesInlineChange(Sender: TObject);
begin
  CheckDirsBeforeFiles;
end;

procedure TFileBrowserOptionsFrame.CBUseLettersChange(Sender: TObject);
begin
  CheckPartial;
end;

procedure TFileBrowserOptionsFrame.CheckPartial;
begin
  CBPartialMatch.Enabled:=CBUseLetters.Checked;
  if not CBPartialMatch.Enabled then
    CBPartialMatch.Checked:=False;
end;

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
  CBShowFilesInline.Checked:=C.FilesInTree;
  CBShowDirectoriesBeforeFiles.Checked:=C.DirectoriesBeforeFiles;
  CBSyncCurrentEditor.Checked:=C.SyncCurrentEditor;
  CBUseAbsoluteFilenames.Checked:=fsoAbsolutePaths in C.SearchOptions;
  CBMatchOnlyFilename.Checked:=fsoMatchOnlyFileName in C.SearchOptions;
  CBUseLetters.Checked:=fsoUseLetters in C.SearchOptions;
  CBPartialMatch.Checked:=fsoMatchPartial in C.SearchOptions;
  CheckDirsBeforeFiles;
end;

procedure TFileBrowserOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  C : TFileBrowserController;
  SD : TStartDir;
  RD : TRootDir;
  SO : TFileSearchOptions;
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
  C.FilesInTree:=CBShowFilesInline.Checked;
  C.SyncCurrentEditor:=CBSyncCurrentEditor.Checked;
  SO:=[];
  if CBUseAbsoluteFilenames.Checked then
    Include(SO,fsoAbsolutePaths);
  if CBMatchOnlyFilename.Checked then
    Include(SO,fsoMatchOnlyFileName);
  if CBUseLetters.Checked then
    Include(SO,fsoUseLetters);
  if CBpartialMatch.Checked then
    Include(SO,fsoMatchPartial);
  C.SearchOptions:=SO;
  // Re-index
  if lRootDir<>C.GetResolvedRootDir then
    C.IndexRootDir;
  C.WriteConfig;
end;

class function TFileBrowserOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

