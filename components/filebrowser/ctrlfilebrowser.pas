unit CtrlFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  Forms, Controls,
  Masks, LazConfigStorage,
  IDEExternToolIntf, ProjectIntf, BaseIDEIntf,
  SrcEditorIntf, LazIDEIntf, MenuIntf, IDEOptEditorIntf, IDEWindowIntf,
  frmFileBrowser, FileBrowserTypes;

Type
    TFilenameMatchOption = (fmoFileNameOnly,fmoLetters);
    TFilenameMatchOptions = set of TFilenameMatchOption;

    TMatchPosition = record
      pos,len : Integer;
    end;
    TMatchPositionArray = Array of TMatchPosition;

   { TFileBrowserController }

    TFileBrowserController = class(TComponent)
    private
      FConfigFrame: TAbstractIDEOptionsEditorClass;
      FCustomStartDir: string;
      FLastOpenedDir: string;
      FStartDir: TStartDir;
      FRootDir : TRootDir;
      FCustomRootDir : string;
      FNeedSave: Boolean;
      FSplitterPos: integer;
      FCurrentEditorFile : String;
      FSyncCurrentEditor: Boolean;
      FFileList : TStrings;
      procedure ActiveEditorChanged(Sender: TObject);
      function DoProjectChanged(Sender: TObject; AProject: TLazProject): TModalResult;
      procedure DoSelectDir(Sender: TObject);
      function GetProjectDir: String;
      function GetResolvedStartDir: String;
      procedure SetCustomRootDir(AValue: string);
      procedure SetCustomStartDir(AValue: string);
      procedure SetLastOpenedDir(AValue: string);
      procedure SetRootDir(AValue: TRootDir);
      procedure SetSplitterPos(AValue: integer);
      procedure SetStartDir(AValue: TStartDir);
      procedure SetSyncCurrentEditor(AValue: Boolean);
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
      procedure WriteConfig; virtual;
      procedure ReadConfig; virtual;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      property StartDir: TStartDir read FStartDir write SetStartDir;
      property RootDir: TRootDir read FRootDir write SetRootDir;
      property CustomStartDir: string read FCustomStartDir write SetCustomStartDir;
      property CustomRootDir: string read FCustomRootDir write SetCustomRootDir;
      property LastOpenedDir: string read FLastOpenedDir write SetLastOpenedDir;
      property SyncCurrentEditor : Boolean Read FSyncCurrentEditor Write SetSyncCurrentEditor;
      property SplitterPos: integer read FSplitterPos write SetSplitterPos;
      Property ConfigFrame : TAbstractIDEOptionsEditorClass Read FConfigFrame Write FConfigFrame;
    end;


implementation

procedure TFileBrowserController.ReadConfig;
var
  Storage: TConfigStorage;
begin
  Storage:=GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      FStartDir := TStartDir(GetValue(KeyStartDir, Ord(DefaultStartDir)));
      FRootDir  := TRootDir(GetValue(KeyRootDir, Ord(DefaultRootDir)));
      FCustomStartDir := GetValue(KeyCustomStartDir, '');
      FCustomRootDir := GetValue(KeyCustomRootDir, '');
      FSplitterPos:=GetValue(KeySplitterPos, DefaultSplitterPos);
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

procedure TFileBrowserController.SetLastOpenedDir(AValue: string);
begin
  if FLastOpenedDir=AValue then Exit;
  FLastOpenedDir:=AValue;
  FNeedSave:=True;
end;

procedure TFileBrowserController.SetRootDir(AValue: TRootDir);
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
  aForm.TV.Height := FSplitterPos;
  aForm.RootDirectory := GetResolvedRootDir;
  //aForm.CurrentDirectory := GetResolvedStartDir;
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
  Case FStartDir of
    sdProjectDir : Result:=GetProjectDir;
    sdCustomDir : Result:=IncludeTrailingPathDelimiter(CustomStartDir);
    sdLastOpened : Result:=IncludeTrailingPathDelimiter(LastOpenedDir);
  end;
end;

function TFileBrowserController.GetResolvedRootDir: String;
begin
  Case FRootDir of
    rdProjectDir: Result:=GetProjectDir;
    rdRootDir   : Result:='/';
    rdCustomDir : Result:=IncludeTrailingPathDelimiter(CustomRootDir);
    rdUserDir   : Result:=IncludeTrailingPathDelimiter(GetUserDir);
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
  if FStartDir = sdLastOpened then
    ; //LastOpenedDir := FileBrowserForm.CurrentDirectory;
end;

function TFileBrowserController.DoProjectChanged(Sender: TObject; AProject: TLazProject): TModalResult;
var
  aPath : String;
begin
  Result:=mrOK;
  if Assigned(FileBrowserForm) then
    begin
    APath:=ExcludeTrailingPathDelimiter(ExtractFilePath(aProject.ProjectInfoFile));
    if FRootDir=rdProjectDir then
      FileBrowserForm.RootDirectory:=aPath;
    if FStartDir=sdProjectDir then
      ; //FileBrowserForm.CurrentDirectory:=aPath;
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
  ; //FileBrowserForm.CurrentFile:=FCurrentEditorFile
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
  if SourceEditorManagerIntf <> nil then
    SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate,@ActiveEditorChanged);
  ReadConfig;
end;

destructor TFileBrowserController.Destroy;
begin
  FreeAndNil(FFileList);
  if SourceEditorManagerIntf <> nil then
    SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorActivate,@ActiveEditorChanged);
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

