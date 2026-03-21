unit RecentListProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg,
  // LazUtils
  LazFileUtils,
  // IdeConfig
  IdeXmlConfigProcs;

// Recent item lists
type
  TRecentListType = (
    rltCaseSensitive,
    rltCaseInsensitive,
    rltFile
    );
const
  RecentListTypeNames: array[TRecentListType] of string = (
    'CaseSensitive',
    'CaseInsensitive',
    'File'
    );

type
  { THistoryList - a TStringList to store a history list }

  THistoryList = class(TStringList)
  private
    FListType: TRecentListType;
    FMaxCount: integer;
    FName: string;
    procedure SetMaxCount(const AValue: integer);
    procedure SetName(const AValue: string);
  public
    constructor Create(TheListType: TRecentListType);
    destructor Destroy;  override;
    function Push(const Entry: string): integer;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure AppendEntry(const Entry: string);
    function IndexOf(const S: string): Integer; override;
  public
    property Name: string read FName write SetName;
    property MaxCount: integer read FMaxCount write SetMaxCount;
    property ListType: TRecentListType read FListType;
  end;

  { THistoryLists - list of THistoryList }

  THistoryLists = class
  private
    FItems: TList;
    function GetItems(Index: integer): THistoryList;
    function GetXMLListPath(const Path: string; i: integer; ALegacyList: Boolean): string;
  public
    constructor Create;
    destructor Destroy;  override;
    procedure Clear;
    function Count: integer;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string; const ALegacyList: Boolean);
    function IndexOfName(const Name: string): integer;
    function GetList(const Name: string;
      CreateIfNotExists: boolean; ListType: TRecentListType): THistoryList;
    procedure Add(const ListName, Entry: string; ListType: TRecentListType);
    property Items[Index: integer]: THistoryList read GetItems;
  end;


function IndexInRecentList(List: TStrings; ListType: TRecentListType;
  const Path: string): integer;
function StrToRecentListType(s: string): TRecentListType;
function CompareRecentListItem(s1, s2: string; ListType: TRecentListType): boolean;
procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStrings; const Path: string;
                         ListType: TRecentListType);
procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings;
                         const Path: string); overload;
procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings;
                         const Path: string; aMax: Integer); overload;
function AddToRecentList(const s: string; List: TStrings; aMax: integer;
                         ListType: TRecentListType): boolean;
procedure RemoveFromRecentList(const s: string; List: TStrings;
                               ListType: TRecentListType);
procedure CleanUpRecentList(List: TStrings; ListType: TRecentListType);


implementation

// Recent item lists :

function IndexInRecentList(List: TStrings; ListType: TRecentListType;
  const Path: string): integer;
begin
  Result:=List.Count-1;
  while (Result>=0) and (not CompareRecentListItem(List[Result],Path,ListType)) do
    dec(Result);
end;

function StrToRecentListType(s: string): TRecentListType;
begin
  for Result:=Low(TRecentListType) to high(TRecentListType)  do
    if SysUtils.CompareText(s,RecentListTypeNames[Result])=0 then exit;
  Result:=rltCaseSensitive;
end;

function CompareRecentListItem(s1, s2: string; ListType: TRecentListType): boolean;
begin
  case ListType of
  rltCaseInsensitive: Result:=AnsiCompareText(s1,s2)=0;
  rltFile: Result:=CompareFilenames(ChompPathDelim(s1),ChompPathDelim(s2))=0;
  else Result:=s1=s2;
  end;
end;

procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStrings;
  const Path: string; ListType: TRecentListType);
begin
  LoadStringList(XMLConfig,List,Path);
  CleanUpRecentList(List,ListType);
end;

procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
begin
  SaveStringList(XMLConfig,List,Path);
end;

procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings;
  const Path: string; aMax: Integer);
var
  i: Integer;
  s: String;
begin
  if aMax>0 then
    while List.Count>aMax do    // Truncate list to aMax items.
      List.Delete(List.Count-1);
  SaveStringList(XMLConfig,List,Path);
  i:=List.Count+1;
  while True do
  begin
    s:=Path+'Item'+IntToStr(i);
    if not XMLConfig.HasPath(s+'/Value',True) then Break;
    XMLConfig.DeletePath(s);    // Remove excess items from XML.
    Inc(i);
  end;
end;

function AddToRecentList(const s: string; List: TStrings; aMax: integer;
  ListType: TRecentListType): boolean;
begin
  if (List.Count>0) and CompareRecentListItem(List[0],s,ListType) then
    exit(false);
  Result:=true;
  RemoveFromRecentList(s,List,ListType);
  List.Insert(0,s);
  if aMax>0 then
    while List.Count>aMax do
      List.Delete(List.Count-1);
end;

procedure RemoveFromRecentList(const s: string; List: TStrings;
  ListType: TRecentListType);
var
  i: integer;
begin
  for i:=List.Count-1 downto 0 do
    if CompareRecentListItem(List[i],s,ListType) then
      List.Delete(i);
end;

procedure CleanUpRecentList(List: TStrings; ListType: TRecentListType);
var
  i: Integer;
begin
  for i:=List.Count-1 downto 1 do
    if (List[i]='') or CompareRecentListItem(List[i],List[i-1],ListType) then
      List.Delete(i);
end;

{ THistoryList }

procedure THistoryList.SetMaxCount(const AValue: integer);
begin
  if FMaxCount=AValue then exit;
  FMaxCount:=AValue;
end;

procedure THistoryList.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

constructor THistoryList.Create(TheListType: TRecentListType);
begin
  FListType:=TheListType;
  FMaxCount:=20;
end;

destructor THistoryList.Destroy;
begin
  inherited Destroy;
end;

function THistoryList.Push(const Entry: string): integer;
begin
  if Entry<>'' then
    AddToRecentList(Entry,Self,MaxCount,ListType);
  Result:=-1;
end;

procedure THistoryList.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  if FName='' then
    FName:=XMLConfig.GetValue(Path+'Name','');
  FMaxCount:=XMLConfig.GetValue(Path+'MaxCount',MaxCount);
  FListType:=StrToRecentListType(XMLConfig.GetValue(Path+'Type',''));
  LoadRecentList(XMLConfig,Self,Path,ListType);
end;

procedure THistoryList.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Name',Name,'');
  XMLConfig.SetDeleteValue(Path+'Type',RecentListTypeNames[ListType],
                           RecentListTypeNames[rltCaseSensitive]);
  XMLConfig.SetDeleteValue(Path+'MaxCount',MaxCount,20);
  SaveRecentList(XMLConfig,Self,Path);
end;

procedure THistoryList.AppendEntry(const Entry: string);
begin
  if (Count<MaxCount) and (IndexOf(Entry)<0) then
    Add(Entry);
end;

function THistoryList.IndexOf(const S: string): Integer;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    if CompareRecentListItem(S,Strings[i],ListType) then
      exit(i);
  Result:=-1;
end;

{ THistoryLists }

function THistoryLists.GetItems(Index: integer): THistoryList;
begin
  Result:=THistoryList(FItems[Index]);
end;

function THistoryLists.GetXMLListPath(const Path: string; i: integer;
  ALegacyList: Boolean): string;
begin
  Result:=Path+TXMLConfig.GetListItemXPath('List', i, ALegacyList, False)+'/';
end;

constructor THistoryLists.Create;
begin
  FItems:=TList.Create;
end;

destructor THistoryLists.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure THistoryLists.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].Free;
  FItems.Clear;
end;

function THistoryLists.Count: integer;
begin
  Result:=FItems.Count;
end;

procedure THistoryLists.LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
var
  MergeCount, i: integer;
  CurList: THistoryList;
  ListName, ListPath: string;
  ListType: TRecentListType;
  IsLegacyList: Boolean;
begin
  IsLegacyList:=XMLConfig.IsLegacyList(Path);
  MergeCount:=XMLConfig.GetListItemCount(Path, 'List', IsLegacyList);
  for i:=0 to MergeCount-1 do begin
    ListPath:=GetXMLListPath(Path,i,IsLegacyList);
    ListName:=XMLConfig.GetValue(ListPath+'Name','');
    if ListName='' then continue;
    ListType:=StrToRecentListType(XMLConfig.GetValue(ListPath+'Type',''));
    CurList:=GetList(ListName,true,ListType);
    CurList.LoadFromXMLConfig(XMLConfig,ListPath);
  end;
end;

procedure THistoryLists.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; const ALegacyList: Boolean);
var
  i, CurID: integer;
begin
  XMLConfig.SetListItemCount(Path,Count,ALegacyList);
  CurID:=0;
  for i:=0 to Count-1 do begin
    if Items[i].Count>0 then begin
      Items[i].SaveToXMLConfig(XMLConfig,GetXMLListPath(Path,CurID,ALegacyList));
      inc(CurID);
    end;
  end;
end;

function THistoryLists.IndexOfName(const Name: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(Items[Result].Name,Name)<>0) do
    dec(Result);
end;

function THistoryLists.GetList(const Name: string; CreateIfNotExists: boolean;
  ListType: TRecentListType): THistoryList;
var
  i: integer;
begin
  i:=IndexOfName(Name);
  if i>=0 then
    Result:=Items[i]
  else if CreateIfNotExists then begin
    Result:=THistoryList.Create(ListType);
    Result.Name:=Name;
    FItems.Add(Result);
  end else
    Result:=nil;
end;

procedure THistoryLists.Add(const ListName, Entry: string;
  ListType: TRecentListType);
begin
  GetList(ListName,true,ListType).Push(Entry);
end;

end.

