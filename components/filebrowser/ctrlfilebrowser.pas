unit CtrlFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, frmFileBrowser, forms, filebrowsertypes, SrcEditorIntf, Masks,
  LazIDEIntf, MenuIntf, IDECommands, ProjectIntf, IDEOptEditorIntf, IDEWindowIntf, BaseIDEIntf;

Type
    TFileSearchOption = (fsoMatchOnlyFileName,fsoAbsolutePaths,fsoUseLetters);
    TFileSearchOptions = Set of TFileSearchOption;

    TFilenameMatchOption = (fmoFileNameOnly,fmoLetters);
    TFilenameMatchOptions = set of TFilenameMatchOption;

    TMatchPosition = record
      pos,len : Integer;
    end;
    TMatchPositionArray = Array of TMatchPosition;

    { TFileSearchMatch }

    TFileSearchMatch = Class(TObject)
    Private
      FEntry : TFileEntry;
      FFileName: string;
      FMatchPositions : TMatchPositionArray;
    Public
      Constructor create(const aFileName : string; aEntry : TFileEntry; const aPositions : TMatchPositionArray);
      Property FileName : string Read FFileName;
      Property Entry : TFileEntry Read FEntry;
      Property MatchPositions : TMatchPositionArray Read FMatchPositions;
    end;

    { TFileSearchResults }

    TFileSearchResults = Class(TFPObjectList)
    private
      function GetMatch(aIndex : Integer): TFileSearchMatch;
    Public
      Constructor Create;
      function Add(aMatch : TFileSearchMatch) : Integer;
      Property Match [aIndex : Integer] : TFileSearchMatch Read GetMatch; default;
    end;

   { TFileBrowserController }
    TFileBrowserController = class(TComponent)
    private
      FConfigFrame: TAbstractIDEOptionsEditorClass;
      FCustomStartDir: string;
      FDirectoriesBeforeFiles: Boolean;
      FLastOpenedDir: string;
      FRoot: TFileSystemEntry;
      FSearchOptions: TFileSearchOptions;
      FStartDir: TStartDir;
      FRootDir : TRootDir;
      FFilesInTree : Boolean;
      FCustomRootDir : string;
      FNeedSave: Boolean;
      FSplitterPos: integer;
      FCurrentEditorFile : String;
      FSyncCurrentEditor: Boolean;
      FTreeFiller : TTreeCreatorThread;
      FFileList : TStrings;
      FIndexingDoneEvent : Array of TNotifyEvent;
      procedure ActiveEditorChanged(Sender: TObject);
      procedure AddFileNodes(List: TStrings; aNode: TFileSystemEntry; aDir: String);
      procedure CreateFileList(aUseAbsolutePaths: boolean);
      function DoProjectChanged(Sender: TObject; AProject: TLazProject): TModalResult;
      procedure DoSelectDir(Sender: TObject);
      function GetProjectDir: String;
      function GetResolvedStartDir: String;
      procedure SetCustomRootDir(AValue: string);
      procedure SetCustomStartDir(AValue: string);
      procedure SetDirectoriesBeforeFiles(AValue: Boolean);
      procedure SetFilesInTree(AValue: Boolean);
      procedure SetLastOpenedDir(AValue: string);
      procedure SetRootDir(AValue: TRootDir);
      procedure SetSearchOptions(AValue: TFileSearchOptions);
      procedure SetSplitterPos(AValue: integer);
      procedure SetStartDir(AValue: TStartDir);
      procedure SetSyncCurrentEditor(AValue: Boolean);
      procedure OnFormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
      procedure TreeFillDone(Sender: TThread; aTree: TDirectoryEntry);
      procedure TreeFillError(Sender: TThread; const aError: String);
    protected
      { Called by file browser window }
      procedure DoOpenFile(Sender: TObject; const AFileName: string); virtual;
      { Called by file browser window }
      procedure DoConfig(Sender: TObject);
      procedure SyncCurrentFile;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function FillingTree: boolean;
      procedure ConfigWindow(AForm: TFileBrowserForm); virtual;
      procedure OpenFiles(aEntries : TFileEntryArray);
      function GetResolvedRootDir : String;
      function ShowConfig: Boolean;
      procedure WriteConfig; virtual;
      procedure ReadConfig; virtual;
      procedure IndexRootDir;
      function FindFiles(aPattern: String; aOutFileList: TFileSearchResults; aMatchOptions : TFilenameMatchOptions; aExtMask : TMaskList): Integer;
      procedure AddOnIndexingFinishedEvent(aEvent : TNotifyEvent);
      procedure RemoveOnIndexingFinishedEvent(aEvent : TNotifyEvent);
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      Property Root : TFileSystemEntry Read FRoot;
      property StartDir: TStartDir read FStartDir write SetStartDir;
      property RootDir: TRootDir read FRootDir write SetRootDir;
      property CustomStartDir: string read FCustomStartDir write SetCustomStartDir;
      property CustomRootDir: string read FCustomRootDir write SetCustomRootDir;
      property LastOpenedDir: string read FLastOpenedDir write SetLastOpenedDir;
      property SyncCurrentEditor : Boolean Read FSyncCurrentEditor Write SetSyncCurrentEditor;
      property SplitterPos: integer read FSplitterPos write SetSplitterPos;
      Property FilesInTree : Boolean Read FFilesInTree Write SetFilesInTree;
      Property DirectoriesBeforeFiles : Boolean Read FDirectoriesBeforeFiles Write SetDirectoriesBeforeFiles;
      Property ConfigFrame : TAbstractIDEOptionsEditorClass Read FConfigFrame Write FConfigFrame;
      property SearchOptions : TFileSearchOptions Read FSearchOptions Write SetSearchOptions;
    end;


implementation

uses Controls, StrUtils, IDEMsgIntf, IDEExternToolIntf, LazConfigStorage;

{ TFileSearchMatch }

constructor TFileSearchMatch.create(const aFileName: string;
  aEntry: TFileEntry; const aPositions: TMatchPositionArray);
begin
  FFileName:=aFileName;
  FEntry:=aEntry;
  FMatchPositions:=aPositions;
end;

{ TFileSearchResults }

function TFileSearchResults.GetMatch(aIndex : Integer): TFileSearchMatch;
begin
  Result:=TFileSearchMatch(Items[aIndex]);
end;

constructor TFileSearchResults.Create;
begin
  Inherited Create(True);
end;

function TFileSearchResults.Add(aMatch: TFileSearchMatch): Integer;
begin
  Result:=Inherited Add(aMatch);
end;

{ TFileBrowserController }


procedure TFileBrowserController.TreeFillError(Sender: TThread; const aError : String);

begin
  AddIDEMessage(mluError,Format(SErrSearching,[GetResolvedRootDir,aError]),'',0,0,SViewFilebrowser);
end;

procedure TFileBrowserController.TreeFillDone(Sender: TThread; aTree: TDirectoryEntry);

var
  i : integer;

begin
  if (FTreeFiller<>Sender) then exit;
  FTreeFiller:=Nil;
  FreeAndNil(FRoot);
  FRoot:=aTree;
  CreateFileList(fsoAbsolutePaths in SearchOptions);
  AddIDEMessage(mluProgress,Format(SFilesFound,[FFileList.Count,GetResolvedRootDir]),'',0,0,SViewFilebrowser);
  for I:=0 to Length(FIndexingDoneEvent)-1 do
    FIndexingDoneEvent[i](Self);
end;

procedure TFileBrowserController.AddFileNodes(List : TStrings; aNode : TFileSystemEntry; aDir : String);

var
  FN : String;
  i : Integer;

begin
  FN:=aDir;
  if FN<>'' then
     FN:=IncludeTrailingPathDelimiter(FN);
  FN:=FN+aNode.Name;
  case aNode.EntryType of
  etFile,
  etSymlink:
    List.AddObject(FN,aNode);
  etDirectory:
    For I:=0 to ANode.EntryCount-1 do
      AddFileNodes(List,ANode.Entries[I],FN);
  end;
end;

procedure TFileBrowserController.CreateFileList(aUseAbsolutePaths : boolean);

var
  lList,l2 : TStrings;
  lDir : String;
begin
  l2:=Nil;
  lList:=TStringList.Create;
  try
    if aUseAbsolutePaths then
      lDir:=GetResolvedRootDir
    else
      lDir:='';
    AddFileNodes(lList,FRoot,lDir);
    l2:=FFileList;
    FFileList:=lList;
  except
    lList.Free;
    Raise;
  end;
  FreeAndNil(L2);
end;

procedure TFileBrowserController.ReadConfig;
var
  Storage : TConfigStorage;
  Opts : TFileSearchOptions;


begin
  Opts:=[];
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
      if GetValue(KeySearchMatchOnlyFilename,False) then
        Include(Opts,fsoMatchOnlyFileName);
      if GetValue(KeySearchAbsoluteFilenames,False) then
        Include(Opts,fsoAbsolutePaths);
      if GetValue(KeySearchLetters,False) then
        Include(Opts,fsoUseLetters);
      SearchOptions:=Opts;
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

procedure TFileBrowserController.SetRootDir(AValue: TRootDir);
begin
  if FRootDir=AValue then Exit;
  FRootDir:=AValue;
  FNeedSave:=True;
end;

procedure TFileBrowserController.SetSearchOptions(AValue: TFileSearchOptions);
begin
  if FSearchOptions=AValue then Exit;
  FSearchOptions:=AValue;
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
      SetDeleteValue(KeySearchMatchOnlyFilename,fsoMatchOnlyFileName in SearchOptions,False);
      SetDeleteValue(KeySearchAbsoluteFilenames,fsoAbsolutePaths in SearchOptions,False);
      SetDeleteValue(KeySearchLetters,fsoUseLetters in SearchOptions,False);
      FNeedSave := False;
    finally
      Free;
    end;
end;

function TFileBrowserController.FillingTree : boolean;

begin
  Result:=Assigned(FTreeFiller);
end;

procedure TFileBrowserController.IndexRootDir;

var
  lDir : String;
begin
  if FillingTree then
    Exit;
  lDir:=GetResolvedRootDir;
  // Do not recurse, thread handles it, it needs to react to terminate...
  FTreeFiller:=TTreeCreatorThread.Create(lDir,[],@TreeFillDone,@TreeFillError);
  AddIDEMessage(mluVerbose,Format(SSearchingFiles,[lDir]),'',0,0,SViewFilebrowser);
end;

function TFileBrowserController.FindFiles(aPattern: String;
  aOutFileList: TFileSearchResults; aMatchOptions: TFilenameMatchOptions;
  aExtMask: TMaskList): Integer;
  
  function MatchesPattern(const aName: string; aStartPos: Integer; const aPtrn: string; var aPositions : TMatchPositionArray): Integer;
  var
    wasMatch : boolean;
    lPtrnLen,lNameLen,lPtrnPos, lNamePos: Integer;
  begin
    Result:=0;
    lPtrnPos := 1;
    lPtrnLen := Length(aPtrn);
    lNameLen := Length(aName);
    lNamePos := aStartPos;
    wasMatch := false;

    while (lPtrnPos <= lPtrnLen) and (lNamePos <= lNameLen) do
    begin
      if aName[lNamePos] = aPtrn[lPtrnPos] then
        begin
        if WasMatch then
          Inc(aPositions[Result].Len)
        else
          begin
          aPositions[Result].Pos:=lNamePos;
          aPositions[Result].Len:=1;
          Inc(Result);
          end;
        Inc(lPtrnPos);
        end;
      Inc(lNamePos);
    end;
    if (lPtrnPos < lPtrnLen) then
      Result := 0;
  end;

var
  lPtrn, lFilename : String;
  lMatchLen,lStartPos,lFileIdx: Integer;
  isMatch : Boolean;
  Match : TFileSearchMatch;
  lPositions : TMatchPositionArray;

begin
  Result:=0;
  if (FFileList=Nil) or (Length(aPattern)<2) then exit;

  lPtrn := Lowercase(aPattern);

  For lFileIdx:=0 to FFileList.Count-1 do
  begin
    // Reset positions array.
    lPositions:=[];
    if fmoLetters in aMatchOptions then
      SetLength(lPositions,Length(lPtrn))
    else
      SetLength(lPositions,1);
    // Determine Start position
    lFilename := Lowercase(FFileList[lFileIdx]);
    if fmoFileNameOnly in aMatchOptions then
      lStartPos:=rpos(PathDelim,lFileName)+1
    else
      lStartPos:=1;

    if (aExtMask=Nil) or (aExtMask.Matches(lFilename)) then
    begin
      if fmoLetters in aMatchOptions then
        begin
        lMatchLen:=MatchesPattern(lFilename, lStartPos, lPtrn, lPositions);
        isMatch:=lMatchLen>0;
        if IsMatch then
          SetLength(lPositions,lMatchLen);
        end
      else
        begin
        lPositions[0].Pos:=Pos(lPtrn,lFileName,lStartPos);
        lPositions[0].Len:=Length(lPtrn);
        IsMatch:=(lPositions[0].Pos>0);
        end;
      if IsMatch then
        begin
        Match:=TFileSearchMatch.Create(FFileList[lFileIdx],TFileEntry(FFileList.Objects[lFileIdx]),lPositions);
        aOutFileList.Add(Match);
        end;
    end;
  end;
end;

procedure TFileBrowserController.AddOnIndexingFinishedEvent(aEvent: TNotifyEvent);
var
  Len : Integer;
begin
  Len:=Length(FIndexingDoneEvent);
  SetLength(FIndexingDoneEvent,Len+1);
  FIndexingDoneEvent[len]:=aEvent;
end;

procedure TFileBrowserController.RemoveOnIndexingFinishedEvent(aEvent: TNotifyEvent);
var
  Idx,Len : Integer;
begin
  Len:=Length(FIndexingDoneEvent);
  Idx:=Len;
  While (Idx>=0) and (FIndexingDoneEvent[Idx]<>aEvent) do
    Dec(Idx);
  if idx>=0 then
    begin
    if Idx<(len-1) then
      FIndexingDoneEvent[Idx]:=FIndexingDoneEvent[Len-1];
    SetLength(FIndexingDoneEvent,Len-1);
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

procedure TFileBrowserController.OpenFiles(aEntries: TFileEntryArray);

var
  E : TFileEntry;

begin
  for E in aEntries do
    DoOpenFile(Self,E.AbsolutePath);
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
  Result:=mrOK;
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
  if SourceEditorManagerIntf <> nil then
    SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate,@ActiveEditorChanged);
  FDirectoriesBeforeFiles:=DefaultDirectoriesBeforeFiles;
  FFilesInTree:=DefaultFilesInTree;
  ReadConfig;
end;

destructor TFileBrowserController.Destroy;
begin
  if Assigned(FTreeFiller) then
    FTreeFiller.Terminate;
  FreeAndNil(FFileList);
  FreeAndNil(FRoot);
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

