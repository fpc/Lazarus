unit SynEditMarks;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, SynEditMiscClasses;

const
// Max number of book/gutter marks returned from GetEditMarksForLine - that
// really should be enough.
  maxMarks = 16;

type

  { TSynEditMark }

  TSynEditMark = class
  protected
    FLine, FColumn, FImage, FPriority: Integer;
    FEdit: TSynEditBase;
    FVisible: boolean;
    FInternalImage: boolean;
    FBookmarkNum: integer;
    function GetEdit: TSynEditBase; virtual;
    procedure SetColumn(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetPriority(const AValue: integer); virtual;
    procedure SetVisible(const Value: boolean); virtual; //MWE: Laz needs to know when a line gets visible, so the editor color can be updated
    procedure SetInternalImage(const Value: boolean);
    function GetIsBookmark: boolean;
  public
    constructor Create(AOwner: TSynEditBase);
    property Line: integer read FLine write SetLine;
    property Column: integer read FColumn write SetColumn;
    property Priority: integer read FPriority write SetPriority;
    property ImageIndex: integer read FImage write SetImage;
    property BookmarkNumber: integer read FBookmarkNum write fBookmarkNum;
    property Visible: boolean read FVisible write SetVisible;
    property InternalImage: boolean read FInternalImage write SetInternalImage;
    property IsBookmark: boolean read GetIsBookmark;
  end;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark) of object;

  TSynEditMarks = array[1..maxMarks] of TSynEditMark;

  { A list of mark objects. Each object cause a litle picture to be drawn in the
    gutter. }

  { TSynEditMarkList }

  TSynEditMarkList = class(TList)
  protected
    FEdit: TSynEditBase;
    fOnChange: TNotifyEvent;
    procedure DoChange;
    function Get(Index: Integer): TSynEditMark;
    procedure Put(Index: Integer; Item: TSynEditMark);
  public
    constructor Create(AOwner: TSynEditBase);
    destructor Destroy; override;
    function Add(Item: TSynEditMark): Integer;
    procedure ClearLine(line: integer);
    procedure Delete(Index: Integer);
    function First: TSynEditMark;
    procedure GetMarksForLine(line: integer; var Marks: TSynEditMarks);
    procedure Insert(Index: Integer; Item: TSynEditMark);
    function Last: TSynEditMark;
    procedure Place(Mark: TSynEditMark);
    function Remove(Item: TSynEditMark): Integer;
  public
    property Items[Index: Integer]: TSynEditMark read Get write Put; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function DoMarksCompareBookmarksFirst(Item1, Item2: Pointer): Integer;
function DoMarksCompareBookmarksLast(Item1, Item2: Pointer): Integer;

implementation
uses SynEdit;

type  // This is until InvalidateGutterLines, can be moved to an accessible place
  SynEditAccess = Class(TCustomSynEdit);

function DoMarksCompareBookmarksFirst(Item1, Item2: Pointer): Integer;
var
  Mark1: TSynEditMark absolute Item1;
  Mark2: TSynEditMark absolute Item2;
begin
  Result := 0;
  if Mark1 = Mark2 then Exit;

  if Mark1.IsBookmark then
    Result := -1
  else
  if Mark2.IsBookmark then
    Result := 1
  else
  if Mark1.Priority < Mark2.Priority then
    Result := 1
  else
  if Mark1.Priority > Mark2.Priority then
    Result := -1;
end;

function DoMarksCompareBookmarksLast(Item1, Item2: Pointer): Integer;
var
  Mark1: TSynEditMark absolute Item1;
  Mark2: TSynEditMark absolute Item2;
begin
  Result := 0;
  if Mark1 = Mark2 then Exit;

  if Mark1.IsBookmark then
    Result := 1
  else
  if Mark2.IsBookmark then
    Result := -1
  else
  if Mark1.Priority < Mark2.Priority then
    Result := 1
  else
  if Mark1.Priority > Mark2.Priority then
    Result := -1;
end;

procedure SortMarks(var Marks: TSynEditMarks; Compare: TListSortCompare);
var
  i, j, LastMark: Integer;
  P: Pointer;
begin
  for i := Low(Marks) to High(Marks) do
    if Marks[i] = nil then
    begin
      LastMark := i - 1;
      break;
    end;
  // insert sort is the best for our items count
  for i := Low(Marks) + 1 to LastMark do
  begin
    P := Marks[i];
    j := i - 1;
    while (j >= Low(Marks)) and (Compare(P, Marks[j]) < 1) do
    begin
      Marks[j + 1] := Marks[j];
      j := j - 1;
    end;
    Marks[j + 1] := TSynEditMark(P);
  end;
end;

{ TSynEditMark }

procedure TSynEditMark.SetPriority(const AValue: integer);
begin
  FPriority := AValue;
end;

function TSynEditMark.GetEdit: TSynEditBase;
begin
  if FEdit <> nil then try
    if TCustomSynEdit(FEdit).Marks.IndexOf(self) = -1 then
      FEdit := nil;
  except
    FEdit := nil;
  end;
  Result := FEdit;
end;

function TSynEditMark.GetIsBookmark: boolean;
begin
  Result := (fBookmarkNum >= 0);
end;

procedure TSynEditMark.SetColumn(const Value: Integer);
begin
  FColumn := Value;
end;

procedure TSynEditMark.SetImage(const Value: Integer);
begin
  FImage := Value;
  if FVisible and Assigned(FEdit) then
    SynEditAccess(Pointer(FEdit)).InvalidateGutterLines(FLine, FLine);
end;

procedure TSynEditMark.SetInternalImage(const Value: boolean);
begin
  FInternalImage := Value;
  if FVisible and Assigned(FEdit) then
    SynEditAccess(Pointer(FEdit)).InvalidateGutterLines(FLine, FLine);
end;

procedure TSynEditMark.SetLine(const Value: Integer);
begin
  if FVisible and Assigned(FEdit) then
  begin
    if FLine > 0 then
      SynEditAccess(Pointer(FEdit)).InvalidateGutterLines(FLine, FLine);
    FLine := Value;
    SynEditAccess(Pointer(FEdit)).InvalidateGutterLines(FLine, FLine);
  end else
    FLine := Value;
end;

procedure TSynEditMark.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FEdit) then
      SynEditAccess(Pointer(FEdit)).InvalidateGutterLines(FLine, FLine);
  end;
end;

constructor TSynEditMark.Create(AOwner: TSynEditBase);
begin
  inherited Create;
  FBookmarkNum := -1;
  FEdit := AOwner;
  FPriority := 0;
end;

{ TSynEditMarkList }

function TSynEditMarkList.Add(Item: TSynEditMark): Integer;
begin
  Result := inherited Add(Item);
  DoChange;
end;

procedure TSynEditMarkList.ClearLine(Line: integer);
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    if not Items[i].IsBookmark and (Items[i].Line = Line) then Delete(i);
end;

constructor TSynEditMarkList.Create(AOwner: TSynEditBase);
begin
  inherited Create;
  FEdit := AOwner;
end;

destructor TSynEditMarkList.Destroy;
var
  i: integer;
begin
  for i := 0 to Pred(Count) do
    Get(i).Free;
  inherited Destroy;
end;

procedure TSynEditMarkList.Delete(Index: Integer);
begin
  inherited Delete(Index);
  DoChange;
end;

procedure TSynEditMarkList.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSynEditMarkList.First: TSynEditMark;
begin
  Result := TSynEditMark(inherited First);
end;

function TSynEditMarkList.Get(Index: Integer): TSynEditMark;
begin
  Result := TSynEditMark(inherited Get(Index));
end;

//Returns up to maxMarks book/gutter marks for a chosen line.

procedure TSynEditMarkList.GetMarksForLine(line: integer;
  var marks: TSynEditMarks);
var
  cnt: integer;
  i: integer;
begin
  FillChar(marks, SizeOf(marks), 0);
  cnt := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Line = line then
    begin
      Inc(cnt);
      marks[cnt] := Items[i];
      if cnt = maxMarks then break;
    end;
  end;
  if Assigned(FEdit) then
    if TSynEdit(FEdit).BookMarkOptions.DrawBookmarksFirst then
      SortMarks(marks, @DoMarksCompareBookmarksFirst)
    else
      SortMarks(marks, @DoMarksCompareBookmarksLast);
end;

procedure TSynEditMarkList.Insert(Index: Integer; Item: TSynEditMark);
begin
  inherited Insert(Index, Item);
  DoChange;
end;

function TSynEditMarkList.Last: TSynEditMark;
begin
  Result := TSynEditMark(inherited Last);
end;

procedure TSynEditMarkList.Place(mark: TSynEditMark);
begin
  if assigned(FEdit) then
    if assigned(TSynEdit(FEdit).OnPlaceBookmark) then
      TSynEdit(FEdit).OnPlaceBookmark(TSynEdit(FEdit), mark);
  if assigned(mark) then
    Add(mark);
  DoChange;
end;

procedure TSynEditMarkList.Put(Index: Integer; Item: TSynEditMark);
begin
  inherited Put(Index, Item);
  DoChange;
end;

function TSynEditMarkList.Remove(Item: TSynEditMark): Integer;
begin
  Result := inherited Remove(Item);
  DoChange;
end;

end.

