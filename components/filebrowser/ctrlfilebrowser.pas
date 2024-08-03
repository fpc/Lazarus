unit ctrlfilebrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, frmFileBrowser, forms, filebrowsertypes, SrcEditorIntf,
  LazIDEIntf, MenuIntf, IDECommands, ProjectIntf, IDEOptEditorIntf, IDEWindowIntf, BaseIDEIntf;

Type
   { TFileBrowserController }
    TFileBrowserController = class(TComponent)
    private
      FConfigFrame: TAbstractIDEOptionsEditorClass;
      FCustomStartDir: string;
      FDirectoriesBeforeFiles: Boolean;
      FLastOpenedDir: string;
      FStartDir: TStartDir;
      FRootDir : TRootDir;
      FFilesInTree : Boolean;
      FCustomRootDir : string;
      FNeedSave: Boolean;
      FSplitterPos: integer;
      FCurrentEditorFile : String;
      FSyncCurrentEditor: Boolean;
      procedure ActiveEditorChanged(Sender: TObject);
      function DoProjectChanged(Sender: TObject; AProject: TLazProject): TModalResult;
      procedure DoSelectDir(Sender: TObject);
      function GetProjectDir: String;
      function GetResolvedStartDir: String;
      procedure ReadConfig; virtual;
      procedure SetCustomRootDir(AValue: string);
      procedure SetCustomStartDir(AValue: string);
      procedure SetDirectoriesBeforeFiles(AValue: Boolean);
      procedure SetFilesInTree(AValue: Boolean);
      procedure SetLastOpenedDir(AValue: string);
      procedure SetRoootDir(AValue: TRootDir);
      procedure SetSplitterPos(AValue: integer);
      procedure SetStartDir(AValue: TStartDir);
      procedure SetSyncCurrentEditor(AValue: Boolean);
      procedure WriteConfig; virtual;
      procedure OnFormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    protected
      { Called by file browser window }
      procedure DoOpenFile(Sender: TObject; const AFileName: string); virtual;
      { Called by file browser window }
      procedure DoConfig(Sender: TObject);
      procedure SyncCurrentFile;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure ConfigWindow(AForm: TFileBrowserForm); virtual;
      function GetResolvedRootDir : String;
      function ShowConfig: Boolean;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      property StartDir: TStartDir read FStartDir write SetStartDir;
      property RootDir: TRootDir read FRootDir write SetRoootDir;
      property CustomStartDir: string read FCustomStartDir write SetCustomStartDir;
      property CustomRootDir: string read FCustomRootDir write SetCustomRootDir;
      property LastOpenedDir: string read FLastOpenedDir write SetLastOpenedDir;
      property SyncCurrentEditor : Boolean Read FSyncCurrentEditor Write SetSyncCurrentEditor;
      property SplitterPos: integer read FSplitterPos write SetSplitterPos;
      Property FilesInTree : Boolean Read FFilesInTree Write SetFilesInTree;
      Property DirectoriesBeforeFiles : Boolean Read FDirectoriesBeforeFiles Write SetDirectoriesBeforeFiles;
      Property ConfigFrame : TAbstractIDEOptionsEditorClass Read FConfigFrame Write FConfigFrame;
    end;


implementation

uses LazConfigStorage;

{ TFileBrowserController }

procedure TFileBrowserController.ReadConfig;
var
  Storage : TConfigStorage;
begin
  Storage:=GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      FStartDir  := TStartDir(GetValue(KeyStartDir, Ord(DefaultStartDir)));
      FRootDir  := TRootDir(GetValue(KeyRootDir, Ord(DefaultRootDir)));
      FCustomStartDir := GetValue(KeyCustomStartDir, '');
      FCustomRootDir := GetValue(KeyCustomRootDir, '');
      FSplitterPos:=GetValue(KeySplitterPos, DefaultSplitterPos);
      FFilesInTree:=GetValue(KeyFilesInTree, DefaultFilesInTree);
      FDirectoriesBeforeFiles:=GetValue(KeyDirectoriesBeforeFiles,DefaultDirectoriesBeforeFiles);
      FSyncCurrentEditor:=GetValue(KeySyncCurrentEditor,DefaultSyncCurrentEditor);
    finally
      Free;
    end;
end;


procedure TFileBrowserController.SetCustomRootDir(AValue: string);
begin
  if FCustomRootDir=AValue then Exit;
  FCustomRootDir:=AValue;
  FNeedSave:=true;
end;

procedure TFileBrowserController.SetCustomStartDir(AValue: string);
begin
  if FCustomStartDir=AValue then Exit;
  FCustomStartDir:=AValue;
  FNeedSave:=true;
end;

procedure TFileBrowserController.SetDirectoriesBeforeFiles(AValue: Boolean);
begin
  if FDirectoriesBeforeFiles=AValue then Exit;
  FDirectoriesBeforeFiles:=AValue;
  FNeedSave:=true;
end;

procedure TFileBrowserController.SetFilesInTree(AValue: Boolean);
begin
  if FFilesInTree=AValue then Exit;
  FFilesInTree:=AValue;
  FNeedSave:=true;
end;

procedure TFileBrowserController.SetLastOpenedDir(AValue: string);
begin
  if FLastOpenedDir=AValue then Exit;
  FLastOpenedDir:=AValue;
  FNeedSave:=True;
end;

procedure TFileBrowserController.SetRoootDir(AValue: TRootDir);
begin
  if FRootDir=AValue then Exit;
  FRootDir:=AValue;
  FNeedSave:=True;
end;

procedure TFileBrowserController.SetSplitterPos(AValue: integer);
begin
  if FSplitterPos=AValue then Exit;
  FSplitterPos:=AValue;
  FNeedSave:=true;
end;

procedure TFileBrowserController.SetStartDir(AValue: TStartDir);
begin
  if FStartDir=AValue then Exit;
  FStartDir:=AValue;
  FNeedSave:=True;
end;

procedure TFileBrowserController.SetSyncCurrentEditor(AValue: Boolean);
begin
  if FSyncCurrentEditor=AValue then Exit;
  FSyncCurrentEditor:=AValue;
  FNeedSave:=True;
  if aValue and (FCurrentEditorFile<>'') then
    SyncCurrentFile;
end;

procedure TFileBrowserController.WriteConfig;
var
  Storage : TConfigStorage;
begin
  Storage:=GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      SetDeleteValue(KeyStartDir, Ord(FStartDir), Ord(DefaultStartDir));
      SetDeleteValue(KeyRootDir, Ord(FRootDir), Ord(DefaultRootDir));
      SetDeleteValue(KeyCustomStartDir, CustomStartDir, '');
      SetDeleteValue(KeyCustomRootDir, CustomRootDir, '');
      SetDeleteValue(KeySplitterPos, FSplitterPos, DefaultSplitterPos);
      SetDeleteValue(KeyFilesInTree, FFilesInTree, DefaultFilesInTree);
      SetDeleteValue(KeyDirectoriesBeforeFiles, FDirectoriesBeforeFiles, DefaultDirectoriesBeforeFiles);
      SetDeleteValue(KeySyncCurrentEditor,FSyncCurrentEditor, DefaultSyncCurrentEditor);
      FNeedSave := False;
    finally
      Free;
    end;
end;

procedure TFileBrowserController.ConfigWindow(AForm: TFileBrowserForm);

begin
  aForm.Caption:=SFileBrowserIDEMenuCaption;
  aForm.FreeNotification(Self);
  aForm.OnOpenFile := @DoOpenFile;
  aForm.OnConfigure := @DoConfig;
  aForm.OnSelectDir := @DoSelectDir;
  aForm.AddHandlerClose(@OnFormClose);
  aForm.TV.Height:=FSplitterPos;

  aForm.RootDirectory:=GetResolvedRootDir;
  aForm.CurrentDirectory := GetResolvedStartDir;
  aForm.FilesInTree:=FilesInTree;
  aForm.DirectoriesBeforeFiles:=DirectoriesBeforeFiles;
  if FCurrentEditorFile<>'' then
    SyncCurrentFile;
end;

function TFileBrowserController.GetProjectDir : String;

begin
  if Assigned(LazarusIDE.ActiveProject) then
    Result:=ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile)
  else
    Result:=GetTempDir(False);
end;

function TFileBrowserController.GetResolvedStartDir: String;

begin
  Case StartDir of
    sdProjectDir : Result:=GetProjectDir;
    sdCustomDir : Result:=IncludeTrailingPathDelimiter(CustomStartDir);
    sdLastOpened : Result:=IncludeTrailingPathDelimiter(LastOpenedDir);
  end;
end;

function TFileBrowserController.GetResolvedRootDir: String;
begin
  Case RootDir of
    rdProjectDir : Result:=GetProjectDir;
    rdRootDir : Result:='/';
    rdCustomDir : Result:=IncludeTrailingPathDelimiter(CustomRootDir);
    rdUserDir : Result:=IncludeTrailingPathDelimiter(GetUserDir);
  end;
end;

procedure TFileBrowserController.DoOpenFile(Sender: TObject; const AFileName: string);
var
  Flags: TOpenFlags;
begin
  // Set up as desired. Maybe create config settings;
  Flags := [ofOnlyIfExists, ofAddToRecent, ofUseCache];
  LazarusIDE.DoOpenEditorFile(AFileName, 0, 0, Flags);
end;

procedure TFileBrowserController.OnFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SplitterPos:=FileBrowserForm.Splitter1.Top;
end;

procedure TFileBrowserController.DoSelectDir(Sender: TObject);
begin
  if StartDir = sdLastOpened then
    LastOpenedDir := FileBrowserForm.CurrentDirectory;
end;

function TFileBrowserController.DoProjectChanged(Sender: TObject; AProject: TLazProject): TModalResult;

var
  aPath : String;

begin
  if Assigned(FileBrowserForm) then
    begin
    APath:=ExcludeTrailingPathDelimiter(ExtractFilePath(aProject.ProjectInfoFile));
    if RootDir=rdProjectDir then
      FileBrowserForm.RootDirectory:=aPath;
    if StartDir=sdProjectDir then
      FileBrowserForm.CurrentDirectory:=aPath;
    end;

end;

procedure TFileBrowserController.ActiveEditorChanged(Sender: TObject);


begin
  if not Assigned(SourceEditorManagerIntf.ActiveEditor) then
    exit;
  FCurrentEditorFile:=SourceEditorManagerIntf.ActiveEditor.FileName;
  SyncCurrentFile;
end;

procedure TFileBrowserController.SyncCurrentFile;

begin
  if Not (Assigned(FileBrowserForm) and SyncCurrentEditor) then
    exit;
  FileBrowserForm.CurrentFile:=FCurrentEditorFile
end;

procedure TFileBrowserController.DoConfig(Sender: TObject);
begin
  // Maybe later some reconfiguration of FWindow is needed after ShowConfig ?
  if ShowConfig then
    WriteConfig;
end;

constructor TFileBrowserController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LazarusIDE.AddHandlerOnProjectOpened(@DoProjectChanged);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate,@ActiveEditorChanged);
  FDirectoriesBeforeFiles:=DefaultDirectoriesBeforeFiles;
  FFilesInTree:=DefaultFilesInTree;
  ReadConfig;
end;

destructor TFileBrowserController.Destroy;
begin
  LazarusIDE.RemoveAllHandlersOfObject(Self);
  if FNeedSave then
    WriteConfig;
  inherited;
end;

function TFileBrowserController.ShowConfig: Boolean;

begin
  Result:=LazarusIDE.DoOpenIDEOptions(ConfigFrame);
end;

procedure TFileBrowserController.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FileBrowserForm) and (opRemove = Operation) then
    FileBrowserForm := nil;
end;



end.

