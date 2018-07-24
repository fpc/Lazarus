{
 /***************************************************************************
                          ideoptionsdefs.pp  -  Toolbar
                          -----------------------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit IDEOptionDefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types,
  // LCL
  LCLProc, Forms, Controls,
  // LazUtils
  LazFileUtils, LazConfigStorage, Laz2_XMLCfg, LazUTF8,
  // IdeIntf
  BaseIDEIntf, IDEExternToolIntf,
  // IDE
  LazConf;

type
  { TXMLOptionsStorage }

  TXMLOptionsStorage = class(TConfigStorage)
  private
    FFreeXMLConfig: boolean;
    FXMLConfig: TXMLConfig;
  protected
    function  GetFullPathValue(const APath, ADefault: String): String; override;
    function  GetFullPathValue(const APath: String; ADefault: Integer): Integer; override;
    function  GetFullPathValue(const APath: String; ADefault: Boolean): Boolean; override;
    procedure SetFullPathValue(const APath, AValue: String); override;
    procedure SetDeleteFullPathValue(const APath, AValue, DefValue: String); override;
    procedure SetFullPathValue(const APath: String; AValue: Integer); override;
    procedure SetDeleteFullPathValue(const APath: String; AValue, DefValue: Integer); override;
    procedure SetFullPathValue(const APath: String; AValue: Boolean); override;
    procedure SetDeleteFullPathValue(const APath: String; AValue, DefValue: Boolean); override;
    procedure DeleteFullPath(const APath: string); override;
    procedure DeleteFullPathValue(const APath: string); override;
  public
    constructor Create(const Filename: string; LoadFromDisk: Boolean); override;
    constructor Create(TheXMLConfig: TXMLConfig);
    constructor Create(TheXMLConfig: TXMLConfig; const StartPath: string);
    destructor Destroy; override;
    procedure Clear; override;
    property XMLConfig: TXMLConfig read FXMLConfig;
    property FreeXMLConfig: boolean read FFreeXMLConfig write FFreeXMLConfig;
    procedure WriteToDisk; override;
    function GetFilename: string; override;
  end;


  { non modal IDE windows }
type
  TNonModalIDEWindow = (
    nmiwNone, // empty/none/undefined
    nmiwMainIDEName,
    nmiwSourceNoteBookName,
    nmiwMessagesViewName,
    nmiwUnitDependenciesName,
    nmiwCodeExplorerName,
    nmiwFPDocEditorName,
    nmiwClipbrdHistoryName,
    nmiwPkgGraphExplorer,
    nmiwProjectInspector,
    nmiwEditorFileManager,
    nmiwSearchResultsViewName,
    nmiwAnchorEditor,
    nmiwTabOrderEditor,
    nmiwCodeBrowser,
    nmiwIssueBrowser,
    nmiwJumpHistory,
    nmiwComponentList
    );

const
  // This is the list of IDE windows, that will not be automatically reopened
  // on startup. These windows are opened automatically when needed.
{  NonModalIDEWindowManualOpen = [
    nmiwNone,
    nmiwMainIDEName,
    nmiwSourceNoteBookName,
    //nmiwDbgOutput,
    //nmiwDbgEvents,
    nmiwSearchResultsViewName,
    nmiwAnchorEditor
    ];
}
  // form names for non modal IDE windows:
  NonModalIDEWindowNames: array[TNonModalIDEWindow] of string = (
    '?',
    'MainIDE',
    'SourceNotebook',
    'MessagesView',
    'UnitDependencies',
    'CodeExplorerView',
    'FPDocEditor',
    'ClipBrdHistory',
    'PkgGraphExplorer',
    'ProjectInspector',
    'EditorFileManager',
    // not shown at startup
    'SearchResults',
    'AnchorEditor',
    'TabOrderEditor',
    'CodeBrowser',
    'IssueBrowser',
    'JumpHistory',
    'ComponentList'
   );

type
  TLMsgViewFilter = class;

  { TLMVFilterMsgType - read/write by main, read by worker thread }

  TLMVFilterMsgType = class
  private
    FFilter: TLMsgViewFilter;
    FIndex: integer;
    FMsgID: integer;
    FSubTool: string;
    procedure SetMsgID(AValue: integer);
    procedure SetSubTool(AValue: string);
    procedure Changed;
    procedure InternalAssign(Src: TLMVFilterMsgType);
  public
    constructor Create(aFilter: TLMsgViewFilter);
    function IsEqual(Src: TLMVFilterMsgType): boolean;
    procedure Assign(Src: TLMVFilterMsgType);
    property Filter: TLMsgViewFilter read FFilter;
    property SubTool: string read FSubTool write SetSubTool;
    property MsgID: integer read FMsgID write SetMsgID;
    property Index: integer read FIndex;
  end;

  { TLMsgViewFilter
    Note: The View.Filter is protected by View.Enter/LeaveCriticalSection,
          read/write by main thread, read by worker thread.
    }

  TLMsgViewFilter = class
  private
    FCaption: string;
    FFilterNotesWithoutPos: boolean;
    FMinUrgency: TMessageLineUrgency;
    FOnChanged: TNotifyEvent;
    fFilterMsgTypes: array of TLMVFilterMsgType; // sorted for SubTool, MsgID
    function GetFilterMsgTypes(Index: integer): TLMVFilterMsgType; inline;
    procedure SetCaption(AValue: string);
    procedure SetFilterNotesWithoutPos(AValue: boolean);
    procedure SetMinUrgency(AValue: TMessageLineUrgency);
    procedure Changed;
    procedure UpdateFilterMsgTypeIndex(Item: TLMVFilterMsgType);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetToFitsAll;
    function IsEqual(Src: TLMsgViewFilter): boolean; // does not check Caption
    procedure Assign(Src: TLMsgViewFilter); // does not copy Caption
    function LineFits(Line: TMessageLine): boolean; virtual;
    property Caption: string read FCaption write SetCaption;
    property MinUrgency: TMessageLineUrgency read FMinUrgency write SetMinUrgency;
    property FilterNotesWithoutPos: boolean read FFilterNotesWithoutPos write SetFilterNotesWithoutPos;
    function FilterMsgTypeCount: integer; inline;
    property FilterMsgTypes[Index: integer]: TLMVFilterMsgType read GetFilterMsgTypes;
    function AddFilterMsgType(SubTool: string; MsgID: integer): TLMVFilterMsgType;
    procedure DeleteFilterMsgType(Index: integer);
    procedure ClearFilterMsgTypes;
    function IndexOfFilterMsgType(Line: TMessageLine): integer;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure ConsistencyCheck;
  end;

  { TLMsgViewFilters }

  TLMsgViewFilters = class(TComponent)
  private
    FActiveFilter: TLMsgViewFilter;
    fFilters: TFPList; // list of TLMsgViewFilter
    FOnChanged: TNotifyEvent;
    function GetFilters(Index: integer): TLMsgViewFilter;
    procedure OnFilterChanged(Sender: TObject);
    procedure SetActiveFilter(AValue: TLMsgViewFilter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer; inline;
    property Filters[Index: integer]: TLMsgViewFilter read GetFilters; default;
    function GetFilter(aCaption: string; CreateIfNotExist: boolean): TLMsgViewFilter;
    procedure Delete(Index: integer);
    function IndexOf(Filter: TLMsgViewFilter): integer; inline;
    function Add(Filter: TLMsgViewFilter): integer;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    property ActiveFilter: TLMsgViewFilter read FActiveFilter write SetActiveFilter;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  function CompareFilterMsgType(FilterMsgType1, FilterMsgType2: Pointer): integer;
  function CompareLineAndFilterMsgType(MessageLine1, FilterMsgType1: Pointer): integer;

function CreateNiceWindowPosition(Width, Height: integer): TRect;
function NonModalIDEFormIDToEnum(const FormID: string): TNonModalIDEWindow;

function GetLazIDEConfigStorage(const Filename: string; LoadFromDisk: Boolean
                                ): TConfigStorage; // load errors: raises exceptions


implementation


function CreateNiceWindowPosition(Width, Height: integer): TRect;

  function FindFormAt(x,y: integer): TCustomForm;
  var
    i: Integer;
  begin
    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      Result := Screen.CustomForms[i];
      if Result.HandleAllocated and Result.Visible
      and (Result.Left >= x - 5) and (Result.Left <= x + 5)
      and (Result.Top >= y - 5) and (Result.Top <= y + 5)
      then
        exit;
    end;
    Result := nil;
  end;

var
  MinX: Integer;
  MinY: Integer;
  MaxX: Integer;
  MaxY: Integer;
  x: Integer;
  y: Integer;
  MidX: Integer;
  MidY: Integer;
  Step: Integer;
  ABounds: TRect;
begin
  if Screen.ActiveCustomForm <> nil then
    ABounds := Screen.ActiveCustomForm.Monitor.BoundsRect
  else
  if Application.MainForm <> nil then
    ABounds := Application.MainForm.Monitor.BoundsRect
  else
    ABounds := Screen.PrimaryMonitor.BoundsRect;

  MinX := ABounds.Left;
  MinY := ABounds.Top;
  MaxX := ABounds.Right - Width - 10;
  if MaxX < MinX + 10 then MaxX := MinX + 10;
  MaxY := ABounds.Bottom - Height - 100; // why -100?
  if MaxY < MinY + 10 then MaxY := MinY + 10;
  MidX := (MaxX + MinX) div 2;
  MidY := (MaxY + MinY) div 2;
  Step := 0;
  repeat
    x := MidX - Step * 20;
    y := MidY - Step * 20;
    if (x < MinX) or (x > MaxX) or (y < MinY) or (y > MaxY) then break;
    if (FindFormAt(x, y)=nil) or (Step > 1000) then break;
    inc(Step);
  until False;
  Result.Left := x;
  Result.Top := y;
  Result.Right := x + Width;
  Result.Bottom := y + Height;
end;

function NonModalIDEFormIDToEnum(const FormID: string): TNonModalIDEWindow;
begin
  for Result:=Low(TNonModalIDEWindow) to High(TNonModalIDEWindow) do
    if NonModalIDEWindowNames[Result]=FormID then
      exit;
  Result:=nmiwNone;
end;

function GetLazIDEConfigStorage(const Filename: string; LoadFromDisk: Boolean
  ): TConfigStorage;
var
  ConfigFilename: String;
begin
  if CompareFilenames(ExtractFilePath(Filename),GetPrimaryConfigPath)=0 then
    ConfigFilename:=ExtractFileName(Filename)
  else
    ConfigFilename:=Filename;

  if LoadFromDisk and (ExtractFilePath(ConfigFilename)='')
  then begin
    // copy template config file to users config directory
    CopySecondaryConfigFile(ConfigFilename);
  end;
  // create storage
  if not FilenameIsAbsolute(ConfigFilename) then
    ConfigFilename:=AppendPathDelim(GetPrimaryConfigPath)+ConfigFilename;
  Result:=TXMLOptionsStorage.Create(ConfigFilename,LoadFromDisk);
end;

function CompareFilterMsgType(FilterMsgType1, FilterMsgType2: Pointer): integer;
var
  Item1: TLMVFilterMsgType absolute FilterMsgType1;
  Item2: TLMVFilterMsgType absolute FilterMsgType2;
begin
  Result:=SysUtils.CompareText(Item1.SubTool,Item2.SubTool);
  if Result<>0 then exit;
  if Item1.MsgID<Item2.MsgID then
    exit(-1)
  else if Item1.MsgID>Item2.MsgID then
    exit(1);
  Result:=0;
end;

function CompareLineAndFilterMsgType(MessageLine1, FilterMsgType1: Pointer
  ): integer;
var
  Line: TMessageLine absolute MessageLine1;
  Item: TLMVFilterMsgType absolute FilterMsgType1;
begin
  Result:=SysUtils.CompareText(Line.SubTool,Item.SubTool);
  if Result<>0 then exit;
  if Line.MsgID<Item.MsgID then
    exit(-1)
  else if Line.MsgID>Item.MsgID then
    exit(1);
  Result:=0;
end;

{ TLMsgViewFilters }

// inline
function TLMsgViewFilters.Count: integer;
begin
  Result:=FFilters.Count;
end;

// inline
function TLMsgViewFilters.IndexOf(Filter: TLMsgViewFilter): integer;
begin
  Result:=fFilters.IndexOf(Filter);
end;

function TLMsgViewFilters.GetFilters(Index: integer): TLMsgViewFilter;

  procedure RaiseOutOfBounds;
  begin
    raise Exception.Create('TLMsgViewFilters.GetFilters '+IntToStr(Index)+' out of bounds '+IntToStr(Count));
  end;

begin
  if (Index<0) or (Index>=Count) then
    RaiseOutOfBounds;
  Result:=TLMsgViewFilter(fFilters[Index]);
end;

procedure TLMsgViewFilters.OnFilterChanged(Sender: TObject);
begin
  if csDestroying in ComponentState then exit;
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TLMsgViewFilters.SetActiveFilter(AValue: TLMsgViewFilter);
var
  i: Integer;
begin
  if FActiveFilter=AValue then Exit;
  i:=IndexOf(AValue);
  if i<0 then begin
    if FActiveFilter.IsEqual(AValue) then exit;
    FActiveFilter.Assign(AValue);
  end else
    FActiveFilter:=AValue;
  OnFilterChanged(AValue);
end;

constructor TLMsgViewFilters.Create(AOwner: TComponent);
begin
  inherited;
  fFilters:=TFPList.Create;
  FActiveFilter:=TLMsgViewFilter.Create;
  FActiveFilter.Caption:='Default';
  FActiveFilter.OnChanged:=@OnFilterChanged;
  fFilters.Add(FActiveFilter);
end;

destructor TLMsgViewFilters.Destroy;
begin
  Clear;
  ActiveFilter.Free;
  FreeAndNil(fFilters);
  inherited Destroy;
end;

procedure TLMsgViewFilters.Clear;
var
  i: Integer;
begin
  ActiveFilter:=Filters[0];
  for i:=Count-1 downto 1 do
    Delete(i);
  Filters[0].Clear;
end;

function TLMsgViewFilters.GetFilter(aCaption: string; CreateIfNotExist: boolean
  ): TLMsgViewFilter;
var
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    Result:=Filters[i];
    if SysUtils.CompareText(Result.Caption,aCaption)=0 then exit;
  end;
  if not CreateIfNotExist then
    exit(nil);
  Result:=TLMsgViewFilter.Create;
  Result.Caption:=aCaption;
  Result.OnChanged:=@OnFilterChanged;
  Add(Result);
end;

procedure TLMsgViewFilters.Delete(Index: integer);
var
  Filter: TLMsgViewFilter;
begin
  if (Index=0) and (Count=1) then begin
    ActiveFilter.Clear;
  end else begin
    Filter:=Filters[Index];
    Filter.OnChanged:=nil;
    fFilters.Delete(Index);
    if ActiveFilter=Filter then
      FActiveFilter:=Filters[0];
    Filter.Free;
    OnFilterChanged(Self);
  end;
end;

function TLMsgViewFilters.Add(Filter: TLMsgViewFilter): integer;
begin
  Filter.OnChanged:=@OnFilterChanged;
  Result:=fFilters.Add(Filter);
  OnFilterChanged(Self);
end;

procedure TLMsgViewFilters.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  NewCnt: Integer;
  ActiveIndex: Integer;
  i: Integer;
  Filter: TLMsgViewFilter;
begin
  Clear;
  NewCnt:=XMLConfig.GetValue(Path+'Count',1);
  ActiveIndex:=XMLConfig.GetValue(Path+'Active',1);
  for i:=1 to NewCnt do begin
    if i>Count then begin
      Filter:=TLMsgViewFilter.Create;
      Add(Filter);
    end else begin
      Filter:=Filters[i-1];
    end;
    Filter.LoadFromXMLConfig(XMLConfig,Path+'Filter'+IntToStr(i)+'/');
  end;
  if (ActiveIndex>0) and (ActiveIndex<=Count) then
    ActiveFilter:=Filters[ActiveIndex-1];
  for i:=Count downto NewCnt+1 do
    Delete(i-1);
end;

procedure TLMsgViewFilters.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  i: Integer;
begin
  XMLConfig.SetDeleteValue(Path+'Count',Count,1);
  XMLConfig.SetDeleteValue(Path+'Active',IndexOf(ActiveFilter)+1,1);
  for i:=1 to Count do
    Filters[i-1].SaveToXMLConfig(XMLConfig,Path+'Filter'+IntToStr(i)+'/');
end;

{ TXMLOptionsStorage }

function TXMLOptionsStorage.GetFullPathValue(const APath, ADefault: String): String;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

function TXMLOptionsStorage.GetFullPathValue(const APath: String;
  ADefault: Integer): Integer;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

function TXMLOptionsStorage.GetFullPathValue(const APath: String;
  ADefault: Boolean): Boolean;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

procedure TXMLOptionsStorage.SetFullPathValue(const APath, AValue: String);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLOptionsStorage.SetDeleteFullPathValue(const APath, AValue,
  DefValue: String);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLOptionsStorage.SetFullPathValue(const APath: String;
  AValue: Integer);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLOptionsStorage.SetDeleteFullPathValue(const APath: String;
  AValue, DefValue: Integer);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLOptionsStorage.SetFullPathValue(const APath: String;
  AValue: Boolean);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLOptionsStorage.SetDeleteFullPathValue(const APath: String;
  AValue, DefValue: Boolean);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLOptionsStorage.DeleteFullPath(const APath: string);
begin
  XMLConfig.DeletePath(APath);
end;

procedure TXMLOptionsStorage.DeleteFullPathValue(const APath: string);
begin
  XMLConfig.DeleteValue(APath);
end;

constructor TXMLOptionsStorage.Create(const Filename: string;
  LoadFromDisk: Boolean);
begin
  if LoadFromDisk then
    FXMLConfig:=TXMLConfig.Create(Filename)
  else
    FXMLConfig:=TXMLConfig.CreateClean(Filename);
  FFreeXMLConfig:=true;
end;

constructor TXMLOptionsStorage.Create(TheXMLConfig: TXMLConfig);
begin
  FXMLConfig:=TheXMLConfig;
  if FXMLConfig=nil then
    raise Exception.Create('');
end;

constructor TXMLOptionsStorage.Create(TheXMLConfig: TXMLConfig;
  const StartPath: string);
begin
  Create(TheXMLConfig);
  AppendBasePath(StartPath);
end;

destructor TXMLOptionsStorage.Destroy;
begin
  if FreeXMLConfig then FreeAndNil(FXMLConfig);
  inherited Destroy;
end;

procedure TXMLOptionsStorage.Clear;
begin
  FXMLConfig.Clear;
end;

procedure TXMLOptionsStorage.WriteToDisk;
begin
  FXMLConfig.Flush;
end;

function TXMLOptionsStorage.GetFilename: string;
begin
  Result:=FXMLConfig.Filename;
end;

{ TLMVFilterMsgType }

procedure TLMVFilterMsgType.SetMsgID(AValue: integer);
begin
  if FMsgID=AValue then Exit;
  FMsgID:=AValue;
  Changed;
end;

procedure TLMVFilterMsgType.SetSubTool(AValue: string);
begin
  if FSubTool=AValue then Exit;
  FSubTool:=AValue;
  Changed;
end;

procedure TLMVFilterMsgType.Changed;
begin
  Filter.UpdateFilterMsgTypeIndex(Self);
  Filter.Changed;
end;

procedure TLMVFilterMsgType.InternalAssign(Src: TLMVFilterMsgType);
begin
  fSubTool:=Src.SubTool;
  fMsgID:=Src.MsgID;
end;

constructor TLMVFilterMsgType.Create(aFilter: TLMsgViewFilter);
begin
  FFilter:=aFilter;
end;

function TLMVFilterMsgType.IsEqual(Src: TLMVFilterMsgType): boolean;
begin
  if Self=Src then exit(true);
  Result:=(SubTool=Src.SubTool)
      and (MsgID=Src.MsgID);
end;

procedure TLMVFilterMsgType.Assign(Src: TLMVFilterMsgType);
begin
  if IsEqual(Src) then exit;
  InternalAssign(Src);
  Changed;
end;

{ TLMsgViewFilter }

// inline
function TLMsgViewFilter.FilterMsgTypeCount: integer;
begin
  Result:=length(fFilterMsgTypes);
end;

// inline
function TLMsgViewFilter.GetFilterMsgTypes(Index: integer): TLMVFilterMsgType;
begin
  Result:=fFilterMsgTypes[Index];
end;

procedure TLMsgViewFilter.SetCaption(AValue: string);
begin
  AValue:=UTF8Trim(AValue,[]);
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TLMsgViewFilter.SetMinUrgency(AValue: TMessageLineUrgency);
begin
  if FMinUrgency=AValue then Exit;
  FMinUrgency:=AValue;
  Changed;
end;

procedure TLMsgViewFilter.SetFilterNotesWithoutPos(AValue: boolean);
begin
  if FFilterNotesWithoutPos=AValue then Exit;
  FFilterNotesWithoutPos:=AValue;
  Changed;
end;

procedure TLMsgViewFilter.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TLMsgViewFilter.UpdateFilterMsgTypeIndex(Item: TLMVFilterMsgType);
var
  OldIndex: Integer;
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
  StartIndex: Integer;
  EndIndex: Integer;
  NewIndex: Integer;
begin
  if FilterMsgTypeCount=1 then exit;
  OldIndex:=Item.FIndex;
  if (OldIndex>0) and (CompareFilterMsgType(Item,fFilterMsgTypes[OldIndex-1])<0)
  then begin
    StartIndex:=0;
    EndIndex:=OldIndex-1;
  end else if (OldIndex<FilterMsgTypeCount-1)
  and (CompareFilterMsgType(Item,fFilterMsgTypes[OldIndex+1])>0) then begin
    StartIndex:=OldIndex+1;
    EndIndex:=FilterMsgTypeCount-1;
  end else
    exit;

  l:=StartIndex;
  r:=EndIndex;
  m:=0;
  cmp:=0;
  while l<=r do begin
    m:=(l+r) div 2;
    cmp:=CompareFilterMsgType(Item,fFilterMsgTypes[m]);
    if cmp<0 then
      r:=m-1
    else if cmp>0 then
      l:=m+1
    else
      break;
  end;
  if cmp<=0 then
    NewIndex:=m
  else
    NewIndex:=m+1;
  if OldIndex<NewIndex then begin
    system.Move(fFilterMsgTypes[OldIndex+1],fFilterMsgTypes[OldIndex],
      SizeOf(TLMVFilterMsgType)*(NewIndex-OldIndex));
  end else if OldIndex>NewIndex then begin
    system.Move(fFilterMsgTypes[NewIndex],fFilterMsgTypes[NewIndex+1],
      SizeOf(TLMVFilterMsgType)*(OldIndex-NewIndex));
  end else
    exit;
  fFilterMsgTypes[NewIndex]:=Item;

  {$IFDEF CheckExtTools}
  ConsistencyCheck;
  {$ENDIF}
end;

constructor TLMsgViewFilter.Create;
begin
  FMinUrgency:=mluHint;
  FFilterNotesWithoutPos:=true;
end;

destructor TLMsgViewFilter.Destroy;
begin
  ClearFilterMsgTypes;
  inherited Destroy;
end;

procedure TLMsgViewFilter.Clear;
begin
  MinUrgency:=mluHint;
  FilterNotesWithoutPos:=true;
  ClearFilterMsgTypes;
end;

procedure TLMsgViewFilter.SetToFitsAll;
begin
  MinUrgency:=mluNone;
  FilterNotesWithoutPos:=false;
  ClearFilterMsgTypes;
end;

function TLMsgViewFilter.IsEqual(Src: TLMsgViewFilter): boolean;
var
  i: Integer;
begin
  Result:=false;
  if Self=Src then exit(true);
  if (MinUrgency<>Src.MinUrgency)
  or (FilterNotesWithoutPos<>Src.FilterNotesWithoutPos)
  or (FilterMsgTypeCount<>Src.FilterMsgTypeCount)
  then exit;
  for i:=0 to FilterMsgTypeCount-1 do
    if not FilterMsgTypes[i].IsEqual(Src.FilterMsgTypes[i]) then exit;
  Result:=true;
end;

procedure TLMsgViewFilter.Assign(Src: TLMsgViewFilter);
var
  NewCnt: Integer;
  OldCnt: Integer;
  i: Integer;
begin
  if IsEqual(Src) then exit;
  fMinUrgency:=Src.MinUrgency;
  FFilterNotesWithoutPos:=Src.FilterNotesWithoutPos;

  // filter msg type
  NewCnt:=Src.FilterMsgTypeCount;
  OldCnt:=FilterMsgTypeCount;
  for i:=NewCnt to OldCnt-1 do
    FreeAndNil(fFilterMsgTypes[i]);
  SetLength(fFilterMsgTypes,NewCnt);
  for i:=0 to NewCnt-1 do begin
    if fFilterMsgTypes[i]=nil then
      fFilterMsgTypes[i]:=TLMVFilterMsgType.Create(Self);
    fFilterMsgTypes[i].InternalAssign(Src.FilterMsgTypes[i]);
  end;

  Changed;
end;

function TLMsgViewFilter.LineFits(Line: TMessageLine): boolean;
begin
  Result:=false;

  if ord(Line.Urgency)<ord(MinUrgency) then exit;

  if [mlfHiddenByIDEDirective,mlfFixed]*Line.Flags<>[] then exit;

  if FilterNotesWithoutPos and (Line.Urgency<=mluNote)
  and ((Line.Filename='') or (Line.Line<1)) then exit;

  if IndexOfFilterMsgType(Line)>=0 then exit;

  Result:=true;
end;

function TLMsgViewFilter.AddFilterMsgType(SubTool: string;
  MsgID: integer): TLMVFilterMsgType;
var
  i: Integer;
begin
  i:=length(fFilterMsgTypes);
  SetLength(fFilterMsgTypes,i+1);
  Result:=TLMVFilterMsgType.Create(Self);
  fFilterMsgTypes[i]:=Result;
  Result.FSubTool:=SubTool;
  Result.FMsgID:=MsgID;
  UpdateFilterMsgTypeIndex(Result);
  Changed;
end;

procedure TLMsgViewFilter.DeleteFilterMsgType(Index: integer);
begin
  if (Index<0) or (Index>=FilterMsgTypeCount) then
    raise Exception.Create('');
  fFilterMsgTypes[Index].Free;
  if Index<FilterMsgTypeCount-1 then
    system.Move(fFilterMsgTypes[Index+1],fFilterMsgTypes[Index],
      SizeOf(TLMVFilterMsgType)*(FilterMsgTypeCount-Index-1));
  SetLength(fFilterMsgTypes,length(fFilterMsgTypes)-1);
  Changed;
end;

procedure TLMsgViewFilter.ClearFilterMsgTypes;
var
  i: Integer;
begin
  if FilterMsgTypeCount=0 then exit;
  for i:=0 to FilterMsgTypeCount-1 do
    fFilterMsgTypes[i].Free;
  SetLength(fFilterMsgTypes,0);
  Changed;
end;

function TLMsgViewFilter.IndexOfFilterMsgType(Line: TMessageLine): integer;
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
begin
  l:=0;
  r:=FilterMsgTypeCount-1;
  while l<=r do begin
    m:=(l+r) div 2;
    cmp:=CompareLineAndFilterMsgType(Line,fFilterMsgTypes[m]);
    if cmp<0 then
      r:=m-1
    else if cmp>0 then
      l:=m+1
    else
      exit(m);
  end;
  Result:=-1;
end;

procedure TLMsgViewFilter.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  NewCnt: Integer;
  i: Integer;
  p: String;
  SubTool: String;
  MsgId: Integer;
begin
  fCaption:=XMLConfig.GetValue(Path+'Caption','Default');
  FMinUrgency:=StrToMsgLineUrgency(XMLConfig.GetValue(Path+'MinUrgency',
    MessageLineUrgencyNames[mluHint]));
  FFilterNotesWithoutPos:=XMLConfig.GetValue(Path+'FilterNotesWithoutPos',true);
  NewCnt:=XMLConfig.GetValue(Path+'MsgType/Count',0);
  ClearFilterMsgTypes;
  for i:=1 to NewCnt do begin
    p:=Path+'MsgType/Item'+IntToStr(i)+'/';
    SubTool:=XMLConfig.GetValue(p+'SubTool',SubToolFPC);
    MsgId:=XMLConfig.GetValue(p+'MsgId',0);
    if (SubTool='') or (MsgId=0) then continue;
    AddFilterMsgType(SubTool,MsgId);
  end;
end;

procedure TLMsgViewFilter.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  i: Integer;
  p: String;
  Item: TLMVFilterMsgType;
begin
  XMLConfig.SetDeleteValue(Path+'Caption',Caption,'Default');
  XMLConfig.SetDeleteValue(Path+'MinUrgency',
    MessageLineUrgencyNames[MinUrgency],MessageLineUrgencyNames[mluHint]);
  XMLConfig.SetDeleteValue(Path+'FilterNotesWithoutPos',FilterNotesWithoutPos,true);
  XMLConfig.SetDeleteValue(Path+'MsgType/Count',FilterMsgTypeCount,0);
  for i:=1 to FilterMsgTypeCount do begin
    Item:=FilterMsgTypes[i-1];
    p:=Path+'MsgType/Item'+IntToStr(i)+'/';
    XMLConfig.SetDeleteValue(p+'SubTool',Item.SubTool,SubToolFPC);
    XMLConfig.SetDeleteValue(p+'MsgId',Item.MsgID,0);
  end;
end;

procedure TLMsgViewFilter.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create(Msg);
  end;

var
  i: Integer;
begin
  for i:=0 to FilterMsgTypeCount-2 do begin
    if CompareFilterMsgType(fFilterMsgTypes[i],fFilterMsgTypes[i+1])>0 then
      E(IntToStr(i));
  end;
end;

initialization
  DefaultConfigClass:=TXMLOptionsStorage;
  GetIDEConfigStorage:=@GetLazIDEConfigStorage;

end.

