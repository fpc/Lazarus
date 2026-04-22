unit EditableProject;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, System.UITypes,
  // LCL
  Forms,
  // LazUtils
  FileUtil, LazFileUtils, LazFileCache, LazUtilities, Maps, Laz2_XMLCfg, LazLoggerBase,
  // CodeTools
  CodeToolsConfig, ExprEval, DefineTemplates, BasicCodeTools,
  LinkScanner, CodeToolManager, CodeCache, StdCodeTools,
  // BuildIntf
  ProjectIntf, PackageIntf, LazMsgWorker,
  // IdeConfig
  ProjectBuildMode, IdeXmlConfigProcs, RecentListProcs, IdeConfStrConsts,
  // IDEIntf
  EditorSyntaxHighlighterDef, SrcEditorIntf,
  // IdeProject
  Project, ProjectDefs, IdeBookmark, RunParamOptions, IdeProjectStrConsts;

type
  TEditableUnitInfo = class;

  { TUnitEditorInfo }

  TUnitEditorInfo = class //(TLazUnitEditorInfo)
  private
    FEditorComponent: TSourceEditorInterface;
    FUnitInfo: TUnitInfo;
    procedure SetEditorComponent(const AValue: TSourceEditorInterface);
  private
    FIsLocked: Boolean;
    FIsVisibleTab: Boolean;
    FPageIndex: integer;
    FWindowID: integer;
    FTopLine: integer;
    FCursorPos: TPoint;  // physical (screen) position
    FFoldState: String;
    FCustomSyntaxHighlighter: TIdeSyntaxHighlighterID;
    function GetUnitInfo: TEditableUnitInfo; inline;
    procedure SetCursorPos(const AValue: TPoint);
    procedure SetFoldState(AValue: String);
    procedure SetIsLocked(const AValue: Boolean);
    procedure SetPageIndex(const AValue: Integer);
    procedure SetIsVisibleTab(const AValue: Boolean);
    procedure SetCustomSyntaxHighlighter(AValue: TIdeSyntaxHighlighterID);
    procedure SetTopLine(const AValue: Integer);
    procedure SetWindowIndex(const AValue: Integer);
  protected
    procedure Clear;
  public
    constructor Create(aUnitInfo: TEditableUnitInfo);
    destructor Destroy; override;
    property UnitInfo: TEditableUnitInfo read GetUnitInfo;
    property EditorComponent: TSourceEditorInterface read FEditorComponent
                                                  write SetEditorComponent;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string; SaveFold: Boolean);
  public
    property IsVisibleTab: Boolean read FIsVisibleTab write SetIsVisibleTab;
    property PageIndex: Integer read FPageIndex write SetPageIndex;
    property WindowID: Integer read FWindowID write SetWindowIndex;
    property TopLine: Integer read FTopLine write SetTopLine;
    property CursorPos: TPoint read FCursorPos write SetCursorPos;
    property FoldState: String read FFoldState write SetFoldState;
    property IsLocked: Boolean read FIsLocked  write SetIsLocked;
    property CustomSyntaxHighlighter: TIdeSyntaxHighlighterID read FCustomSyntaxHighlighter
               write SetCustomSyntaxHighlighter; // User-set HL to override default
  end;

  { TUnitEditorInfoList }

  TUnitEditorInfoList = class //(TLazUnitEditorInfoList)
  private
    FList: TFPList;
    FUnitInfo: TUnitInfo;
    function GetClosedEditorInfos(Index: Integer): TUnitEditorInfo;
    function GetEditorInfos(Index: Integer): TUnitEditorInfo;
    function GetOpenEditorInfos(Index: Integer): TUnitEditorInfo;
    function GetUnitInfo: TEditableUnitInfo;
  protected
    procedure ClearEachInfo;
    procedure SortByPageIndex;
    procedure SetLastUsedEditor(AEditor:TSourceEditorInterface);
    procedure MakeUsedEditorInfo(AEditorInfo: TUnitEditorInfo);
    procedure MakeUnUsedEditorInfo(AEditorInfo: TUnitEditorInfo);
    procedure Clear;
  public
    constructor Create(aUnitInfo: TUnitInfo);
    destructor Destroy; override;
    property EditorInfos[Index: Integer]: TUnitEditorInfo read GetEditorInfos; default;
    property OpenEditorInfos[Index: Integer]: TUnitEditorInfo read GetOpenEditorInfos;
    property ClosedEditorInfos[Index: Integer]: TUnitEditorInfo read GetClosedEditorInfos;
    function Count: Integer;
    function OpenCount: Integer;
    function ClosedCount: Integer;
    function IndexOfEditorComponent(anEditor: TSourceEditorInterface): Integer;
    function NewEditorInfo: TUnitEditorInfo;
    procedure Add(AEditorInfo: TUnitEditorInfo);
    procedure Delete(Index: Integer);
    procedure Remove(AEditorInfo: TUnitEditorInfo);
  public
    property UnitInfo: TEditableUnitInfo read GetUnitInfo;
  end;

  { TIdeCompilationToolOptions }

  TIdeCompilationToolOptions = class(TProjectCompilationToolOptions)
  protected
    procedure DoClearErrorLines; override;
  end;

  { TIdeProjCompilerOptions }

  TIdeProjCompilerOptions = class(TProjectCompilerOptions)
  public
    constructor Create(AOwner: TObject); override;
  end;

  TEditableProject = class;

  { TEditableUnitInfo }

  TEditableUnitInfo = class(TUnitInfo)
  private
    FBookmarks: TFileBookmarkList;
    FComponentState: TWindowState; // state of component when we save it
    FEditorInfoList: TUnitEditorInfoList;
    FSetBookmarkLock: Integer;
    function GetEditorInfo(Index: Integer): TUnitEditorInfo;
    function GetOpenEditorInfo(Index: Integer): TUnitEditorInfo;
    function GetEditableProject: TEditableProject;
    procedure UpdatePageIndex;
  protected
    procedure SetProject(const AValue: TProject); override;
  public
    constructor Create(ACodeBuffer: TCodeBuffer); override;
    destructor Destroy; override;
    procedure Clear; override;
    function HasOpenEditors: boolean; override;
    // At any time, any TEditableUnitInfo has at least one EditorInfo
    function EditorInfoCount: Integer;
    function OpenEditorInfoCount: Integer; // with EditorComponent assigned
    function GetClosedOrNewEditorInfo: TUnitEditorInfo;
    procedure SetLastUsedEditor(AEditor:TSourceEditorInterface);
    // Bookmarks
    function  AddBookmark(X, Y, ID: integer):integer;
    function  AddBookmark(X, Y, ALeft, ATop, ID: integer):integer;
    procedure DeleteBookmark(ID: integer);
    //
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
        Merge, IsExternalSessionFile: boolean; FileVersion: integer); override;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
        SaveData, SaveSession, IsExternalSessionFile: boolean; UsePathDelim: TPathDelimSwitch); override;
    procedure SetSourceText(const SourceText: string; Beautify: boolean = false); override;
  public
    property Bookmarks: TFileBookmarkList read FBookmarks write FBookmarks;
    property ComponentState: TWindowState read FComponentState write FComponentState;
    property EditorInfo[Index: Integer]: TUnitEditorInfo read GetEditorInfo;
    property OpenEditorInfo[Index: Integer]: TUnitEditorInfo read GetOpenEditorInfo;
    property EditableProject: TEditableProject read GetEditableProject;
  end;

  { TEditableProject }

  TEditableProject = class(TProject)
  private
    FAllEditorsInfoList: TUnitEditorInfoList;
    FAllEditorsInfoMap: TMap;
    FActiveWindowIndexAtStart: integer;
    FBookmarks: TProjectBookmarkList;
    FHistoryLists: THistoryLists;
    FJumpHistory: TProjectJumpHistory;
    function GetAllEditorsInfo(Index: Integer): TUnitEditorInfo;
    function GetFirstUnitWithEditorIndex: TEditableUnitInfo;
    function GetMainUnitInfo: TEditableUnitInfo;
    function GetUnits(Index: integer): TEditableUnitInfo;
    procedure EditorInfoAdd(EdInfo: TUnitEditorInfo);
    procedure EditorInfoRemove(EdInfo: TUnitEditorInfo);
    function JumpHistoryCheckPosition(APosition:TProjectJumpHistoryPosition): boolean;
    procedure LoadDefaultSession;
  protected
    procedure SortEditors; override;
    procedure UpdateVisibleEditor(PgIndex: integer);
    // Session
    procedure LoadSessionInfo(const Path: string); override;
    procedure LoadFromSession; override;
    function DoLoadSession(Filename: String): TModalResult; override;
    procedure SaveSessionInfo(const Path: string); override;
    procedure SaveToSession; override;
  public
    constructor Create(ProjectDescription: TProjectDescriptor); override;
    destructor Destroy; override;
    procedure Clear; override;
    function AllEditorsInfoCount: Integer;
    function GetAndUpdateVisibleUnit(AnEditor: TSourceEditorInterface;
      AWindowID: Integer): TEditableUnitInfo;
    function EditorInfoWithEditorComponent({%H-}AEditor:TSourceEditorInterface): TUnitEditorInfo;
    procedure RemoveUnit(Index: integer;
                         RemoveFromUsesSection: boolean = true); override;
    function UnitWithEditorComponent(AEditor:TSourceEditorInterface): TEditableUnitInfo;
    procedure UpdateAllVisibleUnits;
    // search
    function IndexOfUnitWithComponent(AComponent: TComponent;
      OnlyProjectUnits: boolean; IgnoreUnit: TEditableUnitInfo): integer;
    function IndexOfUnitWithComponentName(const AComponentName: string;
      OnlyProjectUnits: boolean; IgnoreUnit: TEditableUnitInfo): integer;
    // bookmarks
    function  AddBookmark(X, Y, ID: Integer; AUnitInfo: TUnitInfo): integer;
    function  AddBookmark(X, Y, ALeft, ATop, ID: Integer; AUnitInfo: TUnitInfo): integer;
    procedure DeleteBookmark(ID: Integer);
  public
    property ActiveWindowIndexAtStart: integer read FActiveWindowIndexAtStart
                                               write FActiveWindowIndexAtStart;
    property AllEditorsInfo[Index: Integer]: TUnitEditorInfo read GetAllEditorsInfo;
    property Bookmarks: TProjectBookmarkList read FBookmarks write FBookmarks;
    property FirstUnitWithEditorIndex: TEditableUnitInfo read GetFirstUnitWithEditorIndex;
    property HistoryLists: THistoryLists read FHistoryLists;
    property JumpHistory: TProjectJumpHistory read FJumpHistory write FJumpHistory;
    property MainUnitInfo: TEditableUnitInfo read GetMainUnitInfo;
    property Units[Index: integer]: TEditableUnitInfo read GetUnits;
  end;

var
  EditableProject1: TEditableProject absolute LazProject1;// the main project

implementation

{ TUnitEditorInfo }

constructor TUnitEditorInfo.Create(aUnitInfo: TEditableUnitInfo);
begin
  //inherited Create(aUnitInfo);
  FUnitInfo:=aUnitInfo;
  Clear;
  if UnitInfo.Project <> nil then
    (UnitInfo.Project as TEditableProject).EditorInfoAdd(Self);
end;

destructor TUnitEditorInfo.Destroy;
begin
  if UnitInfo.Project <> nil then
    (UnitInfo.Project as TEditableProject).EditorInfoRemove(Self);
  inherited Destroy;
end;

procedure TUnitEditorInfo.Clear;
begin
  FIsVisibleTab := False;
  FPageIndex := -1;
  FWindowID := -1;
  FTopLine := -1;
  FCursorPos.X := -1;
  FCursorPos.Y := -1;
  FFoldState := '';
  FCustomSyntaxHighlighter := IdeHighlighterNotSpecifiedId;
end;

function TUnitEditorInfo.GetUnitInfo: TEditableUnitInfo;
begin
  Result:=TEditableUnitInfo(FUnitInfo);
end;

procedure TUnitEditorInfo.SetEditorComponent(const AValue: TSourceEditorInterface);
begin
  if FEditorComponent = AValue then exit;
  if AValue = nil then begin
    (UnitInfo.Project as TEditableProject).FAllEditorsInfoMap.Delete(FEditorComponent);
    FEditorComponent := AValue;
    UnitInfo.FEditorInfoList.MakeUnUsedEditorInfo(Self);
    PageIndex := -1; // calls UnitInfo.UpdatePageIndex
    IsLocked := False;
  end
  else begin
    PageIndex := -1;
    with UnitInfo.Project as TEditableProject do // Map for lookup: Editor -> EditorInfo
      if not FAllEditorsInfoMap.HasId(AValue) then
        FAllEditorsInfoMap.Add(AValue, Self);
    FEditorComponent := AValue;
    UnitInfo.FEditorInfoList.MakeUsedEditorInfo(Self);
    AValue.UpdateProjectFile; // Set EditorIndex / calls UnitInfo.UpdatePageIndex
  end;
  UnitInfo.SessionModified:=true;
end;

procedure TUnitEditorInfo.SetPageIndex(const AValue: Integer);
begin
  if FPageIndex = AValue then exit;
  FPageIndex := AValue;
  UnitInfo.UpdatePageIndex;
  UnitInfo.SessionModified := True;
end;

procedure TUnitEditorInfo.SetFoldState(AValue: String);
begin
  if FFoldState = AValue then Exit;
  FFoldState := AValue;
  UnitInfo.SessionModified := True;
end;

procedure TUnitEditorInfo.SetIsLocked(const AValue: Boolean);
begin
  if FIsLocked=AValue then Exit;
  FIsLocked:=AValue;
  UnitInfo.SessionModified := True;
end;

procedure TUnitEditorInfo.SetCursorPos(const AValue: TPoint);
begin
  if ComparePoints(FCursorPos,AValue)=0 then Exit;
  FCursorPos:=AValue;
  UnitInfo.SessionModified := True;
end;

procedure TUnitEditorInfo.SetIsVisibleTab(const AValue: Boolean);
begin
  if FIsVisibleTab = AValue then exit;
  FIsVisibleTab := AValue;
  UnitInfo.SessionModified := True;
end;

procedure TUnitEditorInfo.SetCustomSyntaxHighlighter(AValue: TIdeSyntaxHighlighterID);
begin
  if FCustomSyntaxHighlighter = AValue then Exit;
  FCustomSyntaxHighlighter := AValue;
  UnitInfo.SessionModified := True;
end;

procedure TUnitEditorInfo.SetTopLine(const AValue: Integer);
begin
  if FTopLine=AValue then Exit;
  FTopLine:=AValue;
  UnitInfo.SessionModified := True;
end;

procedure TUnitEditorInfo.SetWindowIndex(const AValue: Integer);
begin
  if FWindowID = AValue then exit;
  FWindowID := AValue;
  UnitInfo.SessionModified := True;
end;

procedure TUnitEditorInfo.LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
begin
  IsVisibleTab := XMLConfig.GetValue(Path+'IsVisibleTab/Value', False);
  FPageIndex := XMLConfig.GetValue(Path+'EditorIndex/Value',0);
  WindowID := XMLConfig.GetValue(Path+'WindowIndex/Value',0);
  // update old data
  if (FPageIndex >= 0) and (FWindowID < 0) then
    WindowID := 1;
  FTopLine  := XMLConfig.GetValue(Path+'TopLine/Value',1);
  FCursorPos := Point(XMLConfig.GetValue(Path+'CursorPos/X',1),
                     XMLConfig.GetValue(Path+'CursorPos/Y',1));
  FFoldState := XMLConfig.GetValue(Path+'FoldState/Value', '');
  FIsLocked := XMLConfig.GetValue(Path+'IsLocked/Value', False);
  if IdeSyntaxHighlighters <> nil then
    FCustomSyntaxHighlighter := IdeSyntaxHighlighters.GetIdForName(
                     XMLConfig.GetValue(Path+'SyntaxHighlighter/Value',
                     IdeSyntaxHighlighters.Names[IdeHighlighterNotSpecifiedId]))
  else
    FCustomSyntaxHighlighter := IdeHighlighterUnknownId;
end;

procedure TUnitEditorInfo.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  SaveFold: Boolean);
begin
  XMLConfig.SetDeleteValue(Path+'IsVisibleTab/Value', FIsVisibleTab, False);
  XMLConfig.SetDeleteValue(Path+'EditorIndex/Value', FPageIndex, 0);
  XMLConfig.SetDeleteValue(Path+'WindowIndex/Value', FWindowID, 0);
  XMLConfig.SetDeleteValue(Path+'TopLine/Value', FTopLine, 1);
  XMLConfig.SetDeleteValue(Path+'CursorPos/X', FCursorPos.X, 1);
  XMLConfig.SetDeleteValue(Path+'CursorPos/Y', FCursorPos.Y, 1);
  XMLConfig.SetDeleteValue(Path+'IsLocked/Value', FIsLocked, False);
  if SaveFold then
    XMLConfig.SetDeleteValue(Path+'FoldState/Value', FoldState, '')
  else
    XMLConfig.DeletePath(Path+'FoldState');
  if (FCustomSyntaxHighlighter <> IdeHighlighterUnknownId) and // Don't overwrite, if the value is currently not registerd
     (IdeSyntaxHighlighters <> nil)
  then
    XMLConfig.SetDeleteValue(Path+'SyntaxHighlighter/Value',
                             IdeSyntaxHighlighters.Names[FCustomSyntaxHighlighter], '');
end;

{ TUnitEditorInfoList }

constructor TUnitEditorInfoList.Create(aUnitInfo: TUnitInfo);
begin
  //inherited Create(aUnitInfo);
  FUnitInfo := aUnitInfo;
  FList := TFPList.Create;
end;

destructor TUnitEditorInfoList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TUnitEditorInfoList.Clear;
begin
  while Count > 0 do begin
    EditorInfos[0].Free;
    Delete(0);
  end;
end;

function TUnitEditorInfoList.GetEditorInfos(Index: Integer): TUnitEditorInfo;
begin
  Result := TUnitEditorInfo(FList[Index]);
end;

function TUnitEditorInfoList.GetClosedEditorInfos(Index: Integer): TUnitEditorInfo;
var
  i: Integer;
begin
  i := 0;
  while (i < Count) and (Index >= 0) do begin
    Result := EditorInfos[i];
    if Result.EditorComponent = nil then dec(Index);
    inc(i);
  end;
  if Index >= 0 then
    Result := nil;
end;

function TUnitEditorInfoList.GetOpenEditorInfos(Index: Integer): TUnitEditorInfo;
var
  i: Integer;
begin
  i := 0;
  while (i < Count) and (Index >= 0) do begin
    Result := EditorInfos[i];
    if Result.EditorComponent <> nil then dec(Index);
    inc(i);
  end;
  if Index >= 0 then
    Result := nil;
end;

function TUnitEditorInfoList.GetUnitInfo: TEditableUnitInfo;
begin
  Result:=TEditableUnitInfo(FUnitInfo);
end;

procedure TUnitEditorInfoList.ClearEachInfo;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    EditorInfos[i].Clear;
end;

function CompareEditorInfoByPageIndex(EditorInfo1, EditorInfo2: TUnitEditorInfo): integer;
begin
  Result := EditorInfo1.WindowID - EditorInfo2.WindowID;
  if Result = 0 then
    Result := EditorInfo1.PageIndex - EditorInfo2.PageIndex;
end;

procedure TUnitEditorInfoList.SortByPageIndex;
begin
  FList.Sort(TListSortCompare(@CompareEditorInfoByPageIndex));
end;

procedure TUnitEditorInfoList.SetLastUsedEditor(AEditor: TSourceEditorInterface);
var
  i: Integer;
begin
  i := IndexOfEditorComponent(AEditor);
  if i <> 0 then
    FList.Move(i, 0);
end;

procedure TUnitEditorInfoList.MakeUsedEditorInfo(AEditorInfo: TUnitEditorInfo);
var
  i, j: Integer;
begin
  i := FList.IndexOf(AEditorInfo);
  j := OpenCount;
  if (i > j) and (j < Count) then
    FList.Move(i, j);
end;

procedure TUnitEditorInfoList.MakeUnUsedEditorInfo(AEditorInfo: TUnitEditorInfo);
var
  i: Integer;
begin
  i := FList.IndexOf(AEditorInfo);
  if i <> FList.Count - 1 then
    FList.Move(i, FList.Count - 1);
end;

function TUnitEditorInfoList.Count: Integer;
begin
  Result := FList.Count;
end;

function TUnitEditorInfoList.OpenCount: Integer;
var
  i: Integer;
begin
  i := Count - 1;
  Result := 0;
  while i >= 0 do begin
    if EditorInfos[i].EditorComponent <> nil then inc(Result);
    dec(i);
  end;
end;

function TUnitEditorInfoList.ClosedCount: Integer;
var
  i: Integer;
begin
  i := Count - 1;
  Result := 0;
  while i >= 0 do begin
    if EditorInfos[i].EditorComponent = nil then inc(Result);
    dec(i);
  end;
end;

function TUnitEditorInfoList.IndexOfEditorComponent(anEditor: TSourceEditorInterface): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (EditorInfos[Result].EditorComponent <> anEditor) do
    dec(Result);
end;

function TUnitEditorInfoList.NewEditorInfo: TUnitEditorInfo;
begin
  Result := TUnitEditorInfo.Create(UnitInfo);
  FList.Add(Result);
end;

procedure TUnitEditorInfoList.Add(AEditorInfo: TUnitEditorInfo);
begin
  FList.Add(AEditorInfo);
end;

procedure TUnitEditorInfoList.Delete(Index: Integer);
begin
  Flist.Delete(Index);
end;

procedure TUnitEditorInfoList.Remove(AEditorInfo: TUnitEditorInfo);
var
  i: LongInt;
begin
  i := FList.IndexOf(AEditorInfo);
  if i >= 0 then
    Delete(i);
end;

{ TIdeCompilationToolOptions }

procedure TIdeCompilationToolOptions.DoClearErrorLines;
begin
  Assert(Assigned(SourceEditorManagerIntf), 'TIdeCompilationToolOptions.DoClearErrorLines');
  SourceEditorManagerIntf.ClearErrorLines;
end;

{ TIdeProjCompilerOptions }

constructor TIdeProjCompilerOptions.Create(AOwner: TObject);
begin
  inherited Create(AOwner, TIdeCompilationToolOptions);
end;

{ TEditableUnitInfo }

constructor TEditableUnitInfo.Create(ACodeBuffer: TCodeBuffer);
begin
  FBookmarks := TFileBookmarkList.Create;
  FEditorInfoList := TUnitEditorInfoList.Create(Self);
  FEditorInfoList.NewEditorInfo;
  inherited Create(ACodeBuffer);
end;

destructor TEditableUnitInfo.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FEditorInfoList);
  FreeAndNil(FBookmarks);
end;

procedure TEditableUnitInfo.Clear;
begin
  inherited Clear;
  FSetBookmarkLock := 0;
  FBookmarks.Clear;
  FComponentState := wsNormal;
  FEditorInfoList.ClearEachInfo;
end;

function TEditableUnitInfo.GetEditorInfo(Index: Integer): TUnitEditorInfo;
begin
  Result := FEditorInfoList[Index];
end;

function TEditableUnitInfo.GetOpenEditorInfo(Index: Integer): TUnitEditorInfo;
begin
  Result := FEditorInfoList.OpenEditorInfos[Index];
end;

function TEditableUnitInfo.GetEditableProject: TEditableProject;
begin
  Result := TEditableProject(FProject);
end;

procedure TEditableUnitInfo.SetProject(const AValue: TProject);
var
  i: Integer;
begin
  if FProject=AValue then exit;
  if FProject<>nil then begin
    for i := 0 to FEditorInfoList.Count - 1 do
      (FProject as TEditableProject).EditorInfoRemove(FEditorInfoList[i]);
  end;
  inherited SetProject(AValue);
  if FProject<>nil then begin
    UpdatePageIndex;
    for i := 0 to FEditorInfoList.Count - 1 do
      TEditableProject(FProject).EditorInfoAdd(FEditorInfoList[i]);
  end;
end;

procedure TEditableUnitInfo.UpdatePageIndex;
var
  HasPageIndex: Boolean;
  i, j: integer;
  BM: TFileBookmark;
begin
  HasPageIndex := False;
  i := FEditorInfoList.Count - 1;
  while (i >= 0) and not HasPageIndex do begin
    if EditorInfo[i].PageIndex >= 0 then
      HasPageIndex := True;
    dec(i);
  end;
  UpdateList(uilWithEditorIndex, HasPageIndex);

  if Assigned(Project1) and Assigned(EditableProject1.Bookmarks) then
  begin
    if OpenEditorInfoCount > 0 then begin
      inc(FSetBookmarkLock);
      try
        // Adjust bookmarks
        for i := Bookmarks.Count-1 downto 0 do
        begin
          BM := Bookmarks[i];
          j := EditableProject1.Bookmarks.IndexOfID(BM.ID);
          if (j < 0) then
            OpenEditorInfo[0].EditorComponent.SetBookMark(BM.ID, BM.CursorPos.X, BM.CursorPos.Y, BM.Left, BM.Top);
        end;
      finally
        dec(FSetBookmarkLock);
      end;
    end
    else // OpenEditorInfoCount = 0
      EditableProject1.Bookmarks.DeleteAllWithUnitInfo(Self);
  end;
end;

function TEditableUnitInfo.EditorInfoCount: Integer;
begin
  Result := FEditorInfoList.Count;
end;

function TEditableUnitInfo.OpenEditorInfoCount: Integer;
begin
  Result := FEditorInfoList.OpenCount;
end;

function TEditableUnitInfo.HasOpenEditors: boolean;
begin
  Result := OpenEditorInfoCount > 0;
end;

function TEditableUnitInfo.GetClosedOrNewEditorInfo: TUnitEditorInfo;
begin
  if FEditorInfoList.ClosedCount > 0 then
    Result := FEditorInfoList.ClosedEditorInfos[0]
  else
    Result := FEditorInfoList.NewEditorInfo;
end;

procedure TEditableUnitInfo.SetLastUsedEditor(AEditor: TSourceEditorInterface);
begin
  FEditorInfoList.SetLastUsedEditor(AEditor);
end;

function TEditableUnitInfo.AddBookmark(X, Y, ID: integer): integer;
begin
  if FSetBookmarkLock = 0 then
    Result := Bookmarks.Add(X, Y, ID)
  else
    Result := -1;
  SessionModified := True;
  EditableProject1.AddBookmark(X, Y, ID, Self);
end;

function TEditableUnitInfo.AddBookmark(X, Y, ALeft, ATop, ID: integer): integer;
begin
  if FSetBookmarkLock = 0 then
    Result := Bookmarks.Add(X, Y, ALeft, ATop, ID)
  else
    Result := -1;
  SessionModified := True;
  EditableProject1.AddBookmark(X, Y, ALeft, ATop, ID, Self);
end;

procedure TEditableUnitInfo.DeleteBookmark(ID: integer);
var
  i: Integer;
begin
  i := Bookmarks.IndexOfID(ID);
  if i >= 0 then begin
    Bookmarks.Delete(i);
    SessionModified := True;
  end;
  EditableProject1.DeleteBookmark(ID);
end;

procedure TEditableUnitInfo.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; Merge, IsExternalSessionFile: boolean; FileVersion: integer);
var
  c, i: Integer;
begin
  inherited LoadFromXMLConfig(XMLConfig, Path, Merge, IsExternalSessionFile, FileVersion);
  FComponentState := TWindowState(XMLConfig.GetValue(Path+'ComponentState/Value',0));
  FEditorInfoList.Clear;
  FEditorInfoList.NewEditorInfo;
  FEditorInfoList[0].LoadFromXMLConfig(XMLConfig, Path);
  c := XMLConfig.GetValue(Path+'ExtraEditorCount/Value', 0);
  for i := 1 to c do
    FEditorInfoList.NewEditorInfo.LoadFromXMLConfig(XMLConfig, Path + 'ExtraEditor'+IntToStr(i)+'/');
  UpdatePageIndex;
  FBookmarks.LoadFromXMLConfig(XMLConfig,Path+'Bookmarks/');
end;

procedure TEditableUnitInfo.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string; SaveData, SaveSession, IsExternalSessionFile: boolean;
  UsePathDelim: TPathDelimSwitch);
var
  i, X, Y, L, T: Integer;
  BM: TFileBookmark;
  ProjBM: TProjectBookmark;
begin
  inherited SaveToXMLConfig(XMLConfig, Path, SaveData, SaveSession, IsExternalSessionFile, UsePathDelim);
  if SaveSession then
  begin
    XMLConfig.SetDeleteValue(Path+'ComponentState/Value',Ord(FComponentState),0);
    FEditorInfoList[0].SaveToXMLConfig(XMLConfig, Path, pfSaveFoldState in Project.Flags);
    XMLConfig.SetDeleteValue(Path+'ExtraEditorCount/Value', Int64(FEditorInfoList.Count)-1, 0);
    for i := 1 to FEditorInfoList.Count - 1 do
      FEditorInfoList[i].SaveToXMLConfig(XMLConfig, Path + 'ExtraEditor'+IntToStr(i)+'/',
                                         pfSaveFoldState in Project.Flags);
    if OpenEditorInfoCount > 0 then
      for i := Bookmarks.Count - 1 downto 0 do
      begin
        BM := Bookmarks[i];
        ProjBM := EditableProject.Bookmarks.BookmarkWithID(BM.ID);
        if (ProjBM = nil) or (ProjBM.UnitInfo <> self) then
          Bookmarks.Delete(i)
        else
        if OpenEditorInfo[0].EditorComponent.GetBookMark(BM.ID, X, Y, L, T) then begin
          BM.CursorPos := Point(X, Y);
          BM.Top  := T;
          BM.Left := L;
        end;
      end;
    FBookmarks.SaveToXMLConfig(XMLConfig,Path+'Bookmarks/');
  end;
end;

procedure TEditableUnitInfo.SetSourceText(const SourceText: string; Beautify: boolean);
var
  Src: String;
begin
  Src:=SourceText;
  if Beautify then
    Src:=SourceEditorManagerIntf.Beautify(Src);
  Source.Source:=Src;
end;

{ TEditableProject }

constructor TEditableProject.Create(ProjectDescription: TProjectDescriptor);
begin
  inherited Create(ProjectDescription);
  UnitInfoClass := TEditableUnitInfo;
  ProjectBuildMode.CompilerOptClass:=TIdeProjCompilerOptions;
  FAllEditorsInfoList := TUnitEditorInfoList.Create(nil);
  FAllEditorsInfoMap := TMap.Create(ituPtrSize, SizeOf(TObject));
  FBookmarks := TProjectBookmarkList.Create;
  FHistoryLists := THistoryLists.Create;
  FJumpHistory:=TProjectJumpHistory.Create;
  FJumpHistory.OnCheckPosition:=@JumpHistoryCheckPosition;
  FJumpHistory.OnLoadSaveFilename:=@LoadSaveFilenameHandler;
end;

destructor TEditableProject.Destroy;
begin
  FreeAndNil(FAllEditorsInfoMap);
  inherited Destroy;
  FreeAndNil(FAllEditorsInfoList);
  FreeAndNil(FJumpHistory);
  FreeAndNil(FHistoryLists);
  FreeAndNil(FBookmarks);
end;

procedure TEditableProject.Clear;
begin
  FActiveWindowIndexAtStart := -1;
  FJumpHistory.Clear;
  FBookmarks.Clear;
  inherited Clear;
end;

function TEditableProject.GetAllEditorsInfo(Index: Integer): TUnitEditorInfo;
begin
  Result := FAllEditorsInfoList[Index];
end;

function TEditableProject.GetFirstUnitWithEditorIndex: TEditableUnitInfo;
begin
  Result:=fFirst[uilWithEditorIndex] as TEditableUnitInfo;
end;

function TEditableProject.GetMainUnitInfo: TEditableUnitInfo;
begin
  Result := inherited MainUnitInfo as TEditableUnitInfo;
end;

function TEditableProject.GetUnits(Index: integer): TEditableUnitInfo;
begin
  Result := inherited Units[Index] as TEditableUnitInfo;
  //Result := inherited TEditableUnitInfo(Units[Index]);
end;

procedure TEditableProject.EditorInfoAdd(EdInfo: TUnitEditorInfo);
begin
  FAllEditorsInfoList.Add(EdInfo);
  Assert(not Assigned(EdInfo.EditorComponent),
         'TUnitEditorInfo.EditorComponent should not be assigned. It is set later.');
end;

procedure TEditableProject.EditorInfoRemove(EdInfo: TUnitEditorInfo);
begin
  FAllEditorsInfoList.Remove(EdInfo);
  if Assigned(EdInfo.EditorComponent) then
    FAllEditorsInfoMap.Delete(EdInfo.EditorComponent);
end;

function TEditableProject.JumpHistoryCheckPosition(
  APosition: TProjectJumpHistoryPosition): boolean;
var
  i: integer;
begin
  i:=IndexOfFilename(APosition.Filename);
  Result:=(i>=0) and (TEditableUnitInfo(Units[i]).OpenEditorInfoCount > 0);
end;

procedure TEditableProject.UpdateVisibleEditor(PgIndex: integer);
var
  i: Integer;
begin
  i := AllEditorsInfoCount - 1;
  while i >= 0 do begin
    if AllEditorsInfo[i].PageIndex = PgIndex then
      AllEditorsInfo[i].IsVisibleTab := True;
    dec(i);
  end;
end;

procedure TEditableProject.LoadDefaultSession;
var
  AnUnitInfo, BestUnitInfo: TEditableUnitInfo;
begin
  BestUnitInfo:=FirstUnitWithEditorIndex;
  if (BestUnitInfo<>nil) and (BestUnitInfo.Loaded)
  and FileExistsCached(BestUnitInfo.Filename) then
    exit;
  BestUnitInfo:=nil;

  if (MainUnitID>=0) then begin
    if (PackageGraphInterface.FindLCLDependency(FirstRequiredDependency)<>nil)
    and (Flags*[pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement,
                pfMainUnitHasScaledStatement]<>[])
    then begin
      // this is probably LCL project where the main source only contains automatic code
    end else
      BestUnitInfo:=MainUnitInfo as TEditableUnitInfo;
  end;

  if BestUnitInfo=nil then begin
    for TLazProjectFile(AnUnitInfo) in UnitsBelongingToProject do begin
      if FileExistsCached(AnUnitInfo.Filename) then begin
        if (BestUnitInfo=nil)
        or (FilenameHasPascalExt(AnUnitInfo.Filename)
             and (not FilenameHasPascalExt(BestUnitInfo.Filename)))
        then begin
          BestUnitInfo:=AnUnitInfo;
        end;
      end;
    end;
  end;
  if BestUnitInfo<>nil then begin
    BestUnitInfo.EditorInfo[0].PageIndex := 0;
    BestUnitInfo.EditorInfo[0].WindowID := 0;
    BestUnitInfo.EditorInfo[0].IsVisibleTab := True;
    ActiveWindowIndexAtStart:=0;
    BestUnitInfo.Loaded:=true;
  end;
end;

procedure TEditableProject.LoadSessionInfo(const Path: string);
// Note: the session can be stored in the lpi as well
// So this method is used for loading the lpi units as well
var
  i: integer;
begin
  // load editor info
  i := FXMLConfig.GetValue(Path+'General/ActiveEditorIndexAtStart/Value', -1);
  if (i >= 0) then
    UpdateVisibleEditor(i);     // Load old Config => No WindowIndex
  ActiveWindowIndexAtStart:=FXMLConfig.GetValue(Path+'General/ActiveWindowIndexAtStart/Value', 0);
  if Assigned(FJumpHistory) then
    FJumpHistory.LoadFromXMLConfig(FXMLConfig,Path+'');
  // load custom session data
  LoadStringToStringTree(FXMLConfig,CustomSessionData,Path+'CustomSessionData/');
end;

procedure TEditableProject.LoadFromSession;
const
  Path = 'ProjectSession/';
var
  pds: TPathDelimSwitch;
begin
  pds:=CheckPathDelim(FXMLConfig.GetValue(Path+'PathDelim/Value', '/'),
                      fPathDelimChanged);
  SessionStorePathDelim:=pds;
  fCurStorePathDelim:=pds;

  FFileVersion:=FXMLConfig.GetValue(Path+'Version/Value',0);

  // load MacroValues and compiler options
  BuildModes.LoadSessionFromXMLConfig(FXMLConfig, Path, FLoadAllOptions);

  // load defines used for custom options
  LoadOtherDefines(Path);
  // load unit and session info
  LoadUnits(Path,true);
  LoadSessionInfo(Path);

  if FFileVersion>=11 then
    RunParameterOptions.Load(FXMLConfig,Path+'RunParams/',fPathDelimChanged,rpsLPS);
  FHistoryLists.Clear;
  if FFileVersion>=12 then
    FHistoryLists.LoadFromXMLConfig(FXMLConfig,Path+'HistoryLists/');

  if Assigned(DebuggerLink) then
    DebuggerLink.LoadFromSession(FXMLConfig, Path);
  // call hooks to read their info (e.g. DebugBoss)
  if Assigned(OnLoadProjectInfo) then
    OnLoadProjectInfo(Self,FXMLConfig,true);
end;

function TEditableProject.DoLoadSession(Filename: String): TModalResult;
begin
  Result:=mrOK;
  if FileExistsUTF8(Filename) then
  begin
    //DebugLn('TProject.ReadProject loading Session Filename=',Filename);
    try
      FXMLConfig := TCodeBufXMLConfig.CreateWithCache(Filename);
      LoadFromSession;
    except
      LazMessageWorker(lisCCOErrorCaption,
        Format(lisUnableToReadTheProjectInfoFile, [LineEnding,Filename]),
        mtError,[mbOk]);
      Result:=mrCancel;
      exit;
    end;

    fPathDelimChanged:=false;
    try
      FXMLConfig.Modified:=false;
      FXMLConfig.Free;
    except
    end;
    fCurStorePathDelim:=StorePathDelim;
    FXMLConfig:=nil;
  end else
    // there is no .lps file -> create some defaults
    LoadDefaultSession;
end;

procedure TEditableProject.SaveSessionInfo(const Path: string);
begin
  FXMLConfig.DeleteValue(Path+'General/ActiveEditorIndexAtStart/Value');
  FXMLConfig.SetDeleteValue(Path+'General/ActiveWindowIndexAtStart/Value',
                            ActiveWindowIndexAtStart,0);
  FXMLConfig.SetDeleteValue('SkipCheckLCLInterfaces/Value',
                            FSkipCheckLCLInterfaces,false);
  FXMLConfig.SetDeleteValue(Path+'Build/CleanOutputFileMask/Value',
               CleanOutputFileMask,DefaultProjectCleanOutputFileMask);
  FXMLConfig.SetDeleteValue(Path+'Build/CleanSourcesFileMask/Value',
               CleanSourcesFileMask,DefaultProjectCleanSourcesFileMask);

  if (not (pfSaveOnlyProjectUnits in Flags))
  and (not (pwfSkipJumpPoints in FProjectWriteFlags))
  and Assigned(FJumpHistory) then begin
    if (pfSaveJumpHistory in Flags) then begin
      FJumpHistory.DeleteInvalidPositions;
      FJumpHistory.SaveToXMLConfig(FXMLConfig,Path,UseLegacyLists);
    end
    else
      FXMLConfig.DeletePath(Path+'JumpHistory');
  end;

  // save custom session data
  SaveStringToStringTree(FXMLConfig,CustomSessionData,Path+'CustomSessionData/');
end;

procedure TEditableProject.SaveToSession;
const
  Path = 'ProjectSession/';
begin
  FFileVersion:=ProjectInfoFileVersion;
  fCurStorePathDelim:=SessionStorePathDelim;
  FXMLConfig.SetDeleteValue(Path+'PathDelim/Value',
                          PathDelimSwitchToDelim[fCurStorePathDelim],'/');
  FXMLConfig.SetValue(Path+'Version/Value',ProjectInfoFileVersion);

  // Save the session build modes
  BuildModes.SaveSessionOptsToXMLConfig(FXMLConfig, Path, True, UseLegacyLists);
  BuildModes.SaveSessionData(Path);
  // save all units
  SaveUnits(Path,true,true);

  if Assigned(FDebuggerLink) then
    FDebuggerLink.SaveToSession(FXMLConfig, Path);

  // save defines used for custom options
  SaveOtherDefines(Path);
  // save session info
  SaveSessionInfo(Path);
  // save the Run and Build parameter options
  RunParameterOptions.Save(FXMLConfig,Path+'RunParams/',fCurStorePathDelim,rpsLPS, UseLegacyLists);
  // save history lists
  FHistoryLists.SaveToXMLConfig(FXMLConfig,Path+'HistoryLists/', UseLegacyLists);

  // Notifiy hooks
  if Assigned(OnSaveProjectInfo) then
    OnSaveProjectInfo(Self,FXMLConfig,FProjectWriteFlags+[pwfSkipProjectInfo]);
end;

function TEditableProject.AllEditorsInfoCount: Integer;
begin
  Result := FAllEditorsInfoList.Count;
end;

function TEditableProject.GetAndUpdateVisibleUnit(AnEditor: TSourceEditorInterface;
  AWindowID: Integer): TEditableUnitInfo;
var
  i: Integer;
  AnEditorInfo: TUnitEditorInfo;
begin
  for i := 0 to AllEditorsInfoCount - 1 do
    with AllEditorsInfo[i] do
      if AllEditorsInfo[i].WindowID = AWindowID then
        IsVisibleTab := (EditorComponent = AnEditor);
  AnEditorInfo := EditorInfoWithEditorComponent(AnEditor);
  if AnEditorInfo = nil then Exit(nil);
  Result := AnEditorInfo.UnitInfo;
  if Assigned(Result) then
    Result.SetLastUsedEditor(AnEditor);
end;

function TEditableProject.EditorInfoWithEditorComponent(AEditor: TSourceEditorInterface): TUnitEditorInfo;
begin
  Result := Nil;
  FAllEditorsInfoMap.GetData(AEditor, Result);
end;

procedure TEditableProject.RemoveUnit(Index: integer; RemoveFromUsesSection: boolean);
var
  OldUnitInfo: TEditableUnitInfo;
begin
  OldUnitInfo:=Units[Index];
  // delete bookmarks of this unit
  Bookmarks.DeleteAllWithUnitInfo(OldUnitInfo);
  inherited RemoveUnit(Index, RemoveFromUsesSection);
end;

procedure TEditableProject.SortEditors;
begin
  FAllEditorsInfoList.SortByPageIndex;
end;

function TEditableProject.UnitWithEditorComponent(
  AEditor: TSourceEditorInterface): TEditableUnitInfo;
var
  AnEditorInfo: TUnitEditorInfo;
begin
  if AEditor = nil then exit(nil);
  AnEditorInfo := EditorInfoWithEditorComponent(AEditor);
  if AnEditorInfo = nil then exit(nil);
  Result := AnEditorInfo.UnitInfo;
end;

procedure TEditableProject.UpdateAllVisibleUnits;
var
  i, j: Integer;
  aWndId: LongInt;
  Info: TUnitEditorInfo;
begin
  for i := 0 to AllEditorsInfoCount - 1 do begin
    Info:=AllEditorsInfo[i];
    aWndId:=Info.WindowID;
    j := SourceEditorManagerIntf.IndexOfSourceWindowWithID(aWndId);
    Info.IsVisibleTab := (aWndId>=0) and (j >= 0)
      and (Info.EditorComponent = SourceEditorManagerIntf.SourceWindows[j].ActiveEditor);
  end;
end;

function TEditableProject.IndexOfUnitWithComponent(AComponent: TComponent;
  OnlyProjectUnits: boolean; IgnoreUnit: TEditableUnitInfo): integer;
var
  lUnit: TUnitInfo;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    lUnit := Units[Result];
    if (lUnit.IsPartOfProject or not OnlyProjectUnits)
    and (IgnoreUnit<>lUnit) then begin
      if lUnit.Component=AComponent then
        exit;
    end;
    dec(Result);
  end;
end;

function TEditableProject.IndexOfUnitWithComponentName(const AComponentName: string;
  OnlyProjectUnits: boolean; IgnoreUnit: TEditableUnitInfo): integer;
var
  lUnit: TUnitInfo;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    lUnit := Units[Result];
    if (lUnit.IsPartOfProject or not OnlyProjectUnits)
    and (IgnoreUnit<>lUnit) then begin
      if (CompareText(lUnit.ComponentName,AComponentName)=0)
      or ((lUnit.Component<>nil)
        and (CompareText(lUnit.Component.Name,AComponentName)=0))
      then
        exit;
    end;
    dec(Result);
  end;
end;

function TEditableProject.AddBookmark(X, Y, ID: Integer; AUnitInfo:TUnitInfo): integer;
begin
  Result := Bookmarks.Add(X, Y, ID, AUnitInfo);
  SessionModified := true;
end;

function TEditableProject.AddBookmark(X, Y, ALeft, ATop, ID: Integer; AUnitInfo: TUnitInfo): integer;
begin
  Result := Bookmarks.Add(X, Y, ALeft, ATop, ID, AUnitInfo);
  SessionModified := true;
end;

procedure TEditableProject.DeleteBookmark(ID: Integer);
var
  i: Integer;
begin
  i := Bookmarks.IndexOfID(ID);
  if i < 0 then exit;
  Bookmarks.Delete(i);
  SessionModified := true;
end;

end.

