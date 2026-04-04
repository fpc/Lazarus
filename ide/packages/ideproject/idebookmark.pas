unit IdeBookmark;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // Codetools
  FileProcs,
  // LazUtils
  LazFileUtils, Laz2_XMLCfg,
  // BuildIntf
  ProjectIntf, PublishModuleIntf,
  // IdeProject
  Project;

type
  // bookmarks of a single file

  { TFileBookmark }

  TFileBookmark = class
  private
    fCursorPos: TPoint;
    fID: integer;
    FLeft: integer;
    FTop: integer;
  public
    constructor Create;
    constructor Create(NewX,NewY,AnID: integer);
    constructor Create(NewX,NewY,NewLeft,NewTop,AnID: integer);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    function X: integer;
    function Y: integer;
    property Top: integer read FTop write FTop;
    property Left: integer read FLeft write FLeft;
  public
    property CursorPos: TPoint read fCursorPos write fCursorPos;
    property ID: integer read fID write fID;
  end;

  { TFileBookmarkList }

  TFileBookmarkList = class
  private
    FBookmarks:TList;  // list of TFileBookmark
    function GetBookmarks(Index:integer):TFileBookmark;
    procedure SetBookmarks(Index:integer; ABookmark: TFileBookmark);
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index:integer]:TFileBookmark
       read GetBookmarks write SetBookmarks; default;
    function Count:integer;
    procedure Delete(Index:integer);
    procedure Clear;
    function Add(ABookmark: TFileBookmark):integer;
    function Add(X,Y,ID: integer):integer;
    function Add(X,Y,ALeft,ATop,ID: integer):integer;
    function IndexOfID(ID:integer):integer;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;

  //---------------------------------------------------------------------------
  // The currently visible bookmarks of the project

  { TProjectBookmark }

  TProjectBookmark = class
  private
    fCursorPos: TPoint;
    FLeft: integer;
    FTop: integer;
    FUnitInfo: TUnitInfo;
    fID: integer;
  public
    constructor Create(X,Y, AnID: integer; AUnitInfo: TObject);
    constructor Create(X,Y, ALeft, ATop, AnID: integer; AUnitInfo: TUnitInfo);
    function GetStoredSourceEditor: TObject;
    property CursorPos: TPoint read fCursorPos write fCursorPos;
    property Top: integer read FTop;
    property Left: integer read FLeft;
    property UnitInfo: TUnitInfo read FUnitInfo write FUnitInfo;
    property ID:integer read fID write fID;
  end;

  { TProjectBookmarkList }

  TProjectBookmarkList = class
  private
    FBookmarks: TList;  // list of TProjectBookmark
    function GetBookmarks(Index: integer): TProjectBookmark;
    procedure SetBookmarks(Index: integer; ABookmark: TProjectBookmark);
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index:integer]: TProjectBookmark
       read GetBookmarks write SetBookmarks; default;
    function Count:integer;
    procedure Delete(Index:integer);
    procedure Clear;
    function Add(ABookmark: TProjectBookmark): integer;
    function Add(X, Y, ID: integer; AUnitInfo: TUnitInfo): integer;
    function Add(X, Y, ALeft, ATop, ID: integer; AUnitInfo: TUnitInfo): integer;
    procedure DeleteAllWithUnitInfo(AUnitInfo: TUnitInfo);
    function IndexOfID(ID: integer): integer;
    function BookmarkWithID(ID: integer): TProjectBookmark;
    function UnitInfoForBookmarkWithIndex(ID: integer): TUnitInfo;
  end;


implementation

{ TFileBookmark }

constructor TFileBookmark.Create;
begin
  ;
end;

constructor TFileBookmark.Create(NewX, NewY, AnID: integer);
begin
  fCursorPos.X:=NewX;
  fCursorPos.Y:=NewY;
  FLeft := -1;
  FTop  := -1;
  fID:=AnID;
end;

constructor TFileBookmark.Create(NewX, NewY, NewLeft, NewTop, AnID: integer);
begin
  Create(NewX, NewY, AnID);
  FLeft := NewLeft;
  FTop := NewTop;
end;

procedure TFileBookmark.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'X',fCursorPos.X,1);
  XMLConfig.SetDeleteValue(Path+'Y',fCursorPos.Y,1);
  XMLConfig.SetDeleteValue(Path+'Left',FLeft,-1);
  XMLConfig.SetDeleteValue(Path+'Top',FTop,-1);
  XMLConfig.SetDeleteValue(Path+'ID',fID,0);
end;

procedure TFileBookmark.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  fCursorPos.X:=XMLConfig.GetValue(Path+'X',1);
  fCursorPos.Y:=XMLConfig.GetValue(Path+'Y',1);
  FLeft:=XMLConfig.GetValue(Path+'Left',-1);
  FTop :=XMLConfig.GetValue(Path+'Top',-1);
  fID:=XMLConfig.GetValue(Path+'ID',0);
end;

function TFileBookmark.X: integer;
begin
  Result:=fCursorPos.X;
end;

function TFileBookmark.Y: integer;
begin
  Result:=fCursorPos.Y;
end;

{ TFileBookmarkList }

constructor TFileBookmarkList.Create;
begin
  FBookmarks:=TList.Create;
  Clear;
end;

destructor TFileBookmarkList.Destroy;
begin
  Clear;
  FBookmarks.Free;
  inherited Destroy;
end;

function TFileBookmarkList.GetBookmarks(Index: integer): TFileBookmark;
begin
  Result:=TFileBookmark(FBookmarks[Index]);
end;

procedure TFileBookmarkList.SetBookmarks(Index: integer; ABookmark: TFileBookmark);
begin
  FBookmarks[Index]:=ABookmark;
end;

function TFileBookmarkList.Count: integer;
begin
  Result:=FBookmarks.Count;
end;

procedure TFileBookmarkList.Delete(Index: integer);
begin
  Items[Index].Free;
  FBookmarks.Delete(Index);
end;

procedure TFileBookmarkList.Clear;
var
  i: Integer;
begin
  for i:=0 to FBookmarks.Count-1 do Items[i].Free;
  FBookmarks.Clear;
end;

function TFileBookmarkList.Add(ABookmark: TFileBookmark): integer;
var
  i: Integer;
begin
  i:=IndexOfID(ABookmark.ID);
  if i>=0 then Delete(i);
  Result:=FBookmarks.Add(ABookmark);
end;

function TFileBookmarkList.Add(X, Y, ID: integer): integer;
begin
  Result:=Add(TFileBookmark.Create(X,Y,ID));
end;

function TFileBookmarkList.Add(X, Y, ALeft, ATop, ID: integer): integer;
begin
  Result:=Add(TFileBookmark.Create(X,Y,ALeft,ATop,ID));
end;

function TFileBookmarkList.IndexOfID(ID: integer): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].ID<>ID) do dec(Result);
end;

procedure TFileBookmarkList.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  i: Integer;
begin
  XMLConfig.SetDeleteValue(Path+'Count',Count,0);
  for i:=0 to Count-1 do
    Items[i].SaveToXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/');
end;

procedure TFileBookmarkList.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  NewCount: Integer;
  NewBookmark: TFileBookmark;
  i: Integer;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'Count',0);
  for i:=0 to NewCount-1 do begin
    NewBookmark:=TFileBookmark.Create;
    NewBookmark.LoadFromXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/');
    Add(NewBookmark);
  end;
end;

{ TProjectBookmark }

constructor TProjectBookmark.Create(X, Y, AnID: integer; AUnitInfo: TObject);
// TObject parameter allows TSourceEditor to be stored in UnitInfo field.
begin
  inherited Create;
  fCursorPos.X := X;
  fCursorPos.Y := Y;
  FLeft := -1;
  FTop := -1;
  FUnitInfo := TUnitInfo(AUnitInfo);
  fID := AnID;
end;

constructor TProjectBookmark.Create(X, Y, ALeft, ATop, AnID: integer; AUnitInfo: TUnitInfo);
begin
  Create(X, Y, AnID, AUnitInfo);
  FLeft := ALeft;
  FTop := ATop;
end;

function TProjectBookmark.GetStoredSourceEditor: TObject;
// A hack: TSourceEditor is stored in place of a UnitInfo.
begin
  if FUnitInfo is TUnitInfo then
    Result := nil        // Not a SourceEditor
  else
    Result := FUnitInfo; // SourceEditor, return it
end;

{ TProjectBookmarkList }

constructor TProjectBookmarkList.Create;
begin
  inherited Create;
  fBookmarks:=TList.Create;
end;

destructor TProjectBookmarkList.Destroy;
begin
  Clear;
  fBookmarks.Free;
  inherited Destroy;
end;

procedure TProjectBookmarkList.Clear;
var a:integer;
begin
  for a:=0 to fBookmarks.Count-1 do Items[a].Free;
  fBookmarks.Clear;
end;

function TProjectBookmarkList.Count:integer;
begin
  Result:=fBookmarks.Count;
end;

function TProjectBookmarkList.GetBookmarks(Index: integer): TProjectBookmark;
begin
  Result:=TProjectBookmark(fBookmarks[Index]);
end;

procedure TProjectBookmarkList.SetBookmarks(Index: integer;
  ABookmark: TProjectBookmark);
begin
  fBookmarks[Index]:=ABookmark;
end;

function TProjectBookmarkList.IndexOfID(ID: integer): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].ID<>ID) do dec(Result);
end;

function TProjectBookmarkList.BookmarkWithID(ID: integer): TProjectBookmark;
var
  i: Integer;
begin
  i:=IndexOfID(ID);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TProjectBookmarkList.UnitInfoForBookmarkWithIndex(ID: integer): TUnitInfo;
var
  Mark: TProjectBookmark;
begin
  Mark := BookmarkWithID(ID);
  if Mark <> nil then
    Result := Mark.UnitInfo
  else
    Result:=nil;
end;

procedure TProjectBookmarkList.Delete(Index: integer);
begin
  Items[Index].Free;
  fBookmarks.Delete(Index);
end;

procedure TProjectBookmarkList.DeleteAllWithUnitInfo(AUnitInfo: TUnitInfo);
var i:integer;
begin
  i:=Count-1;
  while (i>=0) do begin
    if Items[i].UnitInfo = AUnitInfo then Delete(i);
    dec(i);
  end;
end;

function TProjectBookmarkList.Add(ABookmark: TProjectBookmark): integer;
var
  i: Integer;
begin
  i:=IndexOfID(ABookmark.ID);
  if i>=0 then Delete(i);
  Result:=fBookmarks.Add(ABookmark);
end;

function TProjectBookmarkList.Add(X, Y, ID: integer; AUnitInfo: TUnitInfo): integer;
begin
  Result:=Add(TProjectBookmark.Create(X, Y, ID, AUnitInfo));
end;

function TProjectBookmarkList.Add(X, Y, ALeft, ATop, ID: integer; AUnitInfo: TUnitInfo): integer;
begin
  Result:=Add(TProjectBookmark.Create(X, Y, ALeft, ATop, ID, AUnitInfo));
end;

end.

