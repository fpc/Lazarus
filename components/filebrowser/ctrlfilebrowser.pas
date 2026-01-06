unit CtrlFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  Forms, Controls,
  Masks, LazConfigStorage, LazLoggerBase,
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
      FLastOpenedDir: string;
      FRootDir: TRootDir;
      FCustomRootDir: string;
      FNeedSave: Boolean;
      FSplitterPos: integer;
      FCurrentEditorFile: String;
      FSyncCurrentEditor: Boolean;
      FFileList: TStrings;
      procedure ActiveEditorChanged(Sender: TObject);
      procedure DoFormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
      function DoProjectChanged(Sender: TObject; AProject: TLazProject): TModalResult;
      function GetProjectDir: String;
      procedure SetCustomRootDir(AValue: string);
      procedure SetLastOpenedDir(AValue: string);
      procedure SetRootDir(AValue: TRootDir);
      procedure SetSplitterPos(AValue: integer);
      procedure SetSyncCurrentEditor(AValue: Boolean);
      { Called by file browser window }
      procedure DoOpenFile(Sender: TObject; const AFileName: string);
      procedure DoSelectFile(Sender: TObject; const AFileName: string);
      procedure DoSelectDir(Sender: TObject);
      { Called by file browser window }
      procedure DoConfig(Sender: TObject);
      procedure SyncCurrentFile;
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure ConfigureWindow(AForm: TFileBrowserForm);
      procedure ReadConfig;
      procedure WriteConfig;
      function GetResolvedRootDir: String;
      function ShowConfig: Boolean;
      property RootDir: TRootDir read FRootDir write SetRootDir;
      property CustomRootDir: string read FCustomRootDir write SetCustomRootDir;
      property LastOpenedDir: string read FLastOpenedDir write SetLastOpenedDir;
      property SyncCurrentEditor: Boolean Read FSyncCurrentEditor Write SetSyncCurrentEditor;
      property SplitterPos: integer read FSplitterPos write SetSplitterPos;
      Property ConfigFrame: TAbstractIDEOptionsEditorClass Read FConfigFrame Write FConfigFrame;
    end;


implementation

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

procedure TFileBrowserController.ConfigureWindow(AForm: TFileBrowserForm);
begin
  aForm.Caption := SFileBrowserIDEMenuCaption;
  aForm.FreeNotification(Self);
  aForm.OnConfigure := @DoConfig;
  aForm.OnOpenFile := @DoOpenFile;
  aForm.OnSelectFile := @DoSelectFile;
  aForm.OnSelectDir := @DoSelectDir;
  aForm.AddHandlerClose(@DoFormClose);
  aForm.TV.Height := FSplitterPos;
  aForm.RootDir := GetResolvedRootDir;
  //aForm.CurrentDir := GetResolvedStartDir;
  if FCurrentEditorFile<>'' then
    SyncCurrentFile;
end;

procedure TFileBrowserController.ReadConfig;
var
  Storage: TConfigStorage;
begin
  Storage := GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      FRootDir := TRootDir(GetValue(KeyRootDir, Ord(DefaultRootDir)));
      FCustomRootDir := GetValue(KeyCustomRootDir, '');
      FSplitterPos := GetValue(KeySplitterPos, DefaultSplitterPos);
      FSyncCurrentEditor := GetValue(KeySyncCurrentEditor, False);
    finally
      Free;
    end;
end;

procedure TFileBrowserController.WriteConfig;
var
  Storage : TConfigStorage;
begin
  Storage:=GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      SetDeleteValue(KeyRootDir, Ord(FRootDir), Ord(DefaultRootDir));
      SetDeleteValue(KeyCustomRootDir, CustomRootDir, '');
      SetDeleteValue(KeySplitterPos, FSplitterPos, DefaultSplitterPos);
      SetDeleteValue(KeySyncCurrentEditor, FSyncCurrentEditor, False);
      FNeedSave := False;
    finally
      Free;
    end;
end;

procedure TFileBrowserController.SetCustomRootDir(AValue: string);
begin
  if FCustomRootDir = AValue then Exit;
  FCustomRootDir := AValue;
  FNeedSave := True;
  if FRootDir = rdCustomDir then
    FileBrowserForm.RootDir := IncludeTrailingPathDelimiter(FCustomRootDir);
end;

procedure TFileBrowserController.SetLastOpenedDir(AValue: string);
begin
  if FLastOpenedDir = AValue then Exit;
  FLastOpenedDir := AValue;
  FNeedSave := True;
  FileBrowserForm.CurrentDir := FLastOpenedDir;
end;

procedure TFileBrowserController.SetRootDir(AValue: TRootDir);
begin
  if FRootDir = AValue then Exit;
  FRootDir := AValue;
  FNeedSave := True;
  FileBrowserForm.RootDir := GetResolvedRootDir;
end;

procedure TFileBrowserController.SetSplitterPos(AValue: integer);
begin
  if FSplitterPos=AValue then Exit;
  FSplitterPos:=AValue;
  FNeedSave:=true;
end;

procedure TFileBrowserController.SetSyncCurrentEditor(AValue: Boolean);
begin
  if FSyncCurrentEditor=AValue then Exit;
  FSyncCurrentEditor:=AValue;
  FNeedSave:=True;
  if aValue and (FCurrentEditorFile<>'') then
    SyncCurrentFile;
end;

function TFileBrowserController.GetProjectDir : String;
begin
  if Assigned(LazarusIDE.ActiveProject) then
    Result:=LazarusIDE.ActiveProject.Directory
  else
    Result:=GetTempDir(False);
end;
{
function TFileBrowserController.GetResolvedStartDir: String;
begin
  Case FStartDir of
    sdProjectDir : Result:=GetProjectDir;
    sdCustomDir : Result:=IncludeTrailingPathDelimiter(CustomStartDir);
    sdLastOpened : Result:=IncludeTrailingPathDelimiter(LastOpenedDir);
  end;
end;
}
function TFileBrowserController.GetResolvedRootDir: String;
begin
  Case FRootDir of
    rdProjectDir   : Result := GetProjectDir;
    rdSystemRootDir: Result := '/';
    rdUserDir      : Result := IncludeTrailingPathDelimiter(GetUserDir);
    rdCustomDir    : Result := IncludeTrailingPathDelimiter(CustomRootDir);
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

procedure TFileBrowserController.DoSelectFile(Sender: TObject; const AFileName: string);
var
  i: Integer;
  SE: TSourceEditorInterface;
begin
  if SyncCurrentEditor then
    for i := 0 to SourceEditorManagerIntf.SourceEditorCount-1 do begin
      SE := SourceEditorManagerIntf.SourceEditors[i];
      //debugln(['TFileBrowserController.DoSelectFile SE=', SE.FileName]);
      if SE.FileName = AFileName then begin
        SourceEditorManagerIntf.ActiveEditor := SE;
        Break;
      end;
    end;
end;

procedure TFileBrowserController.DoSelectDir(Sender: TObject);
begin
  // ToDo:
  //if FStartDir = sdLastOpened then
  //  LastOpenedDir := FileBrowserForm.CurrentDirectory;
end;

procedure TFileBrowserController.DoFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SplitterPos := FileBrowserForm.Splitter1.Top;
end;

function TFileBrowserController.DoProjectChanged(Sender: TObject; AProject: TLazProject): TModalResult;
var
  aPath : String;
begin
  Result:=mrOK;
  if Assigned(FileBrowserForm) then
    begin
    APath:=aProject.Directory;
    if FRootDir=rdProjectDir then
      FileBrowserForm.RootDir:=aPath;
    // ToDo: xxx
    //if FStartDir=sdProjectDir then
    //  FileBrowserForm.CurrentDirectory:=aPath;
    end;
end;

procedure TFileBrowserController.ActiveEditorChanged(Sender: TObject);
begin
  if not Assigned(SourceEditorManagerIntf.ActiveEditor) then
    exit;
  FCurrentEditorFile := SourceEditorManagerIntf.ActiveEditor.FileName;
  //debugln(['TFileBrowserController.ActiveEditorChanged: Sync FCurrentEditorFile=', FCurrentEditorFile]);
  SyncCurrentFile;
end;

procedure TFileBrowserController.SyncCurrentFile;
begin
  if SyncCurrentEditor and Assigned(FileBrowserForm) then
    FileBrowserForm.CurrentFile := FCurrentEditorFile;
end;

procedure TFileBrowserController.DoConfig(Sender: TObject);
begin
  // Maybe later some reconfiguration of FWindow is needed after ShowConfig ?
  if ShowConfig then
    WriteConfig;
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

