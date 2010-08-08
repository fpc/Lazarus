unit SynEditMarks;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, math, SynEditMiscClasses, SynEditTextBase;

const
// Max number of book/gutter marks returned from GetEditMarksForLine - that
// really should be enough.
  maxMarks = 16;

type

  TSynEditMarkList = class;
  TSynEditMark = class;

  TSynEditMarkChangeReason = (smcrAdded, smcrRemoved, smcrLine, smcrVisible, smcrChanged);
  TSynEditMarkChangeReasons = set of TSynEditMarkChangeReason;

  TSynEditMarkChangeEvent = procedure(Sender: TSynEditMark; Changes: TSynEditMarkChangeReasons)
    of object;

  { TSynEditMarkChangedHandlerList }

  TSynEditMarkChangedHandlerList = Class(TSynFilteredMethodList)
  public
    procedure Add(AHandler: TSynEditMarkChangeEvent; Changes: TSynEditMarkChangeReasons);
    procedure Remove(AHandler: TSynEditMarkChangeEvent);
    procedure CallMarkChangedHandlers(Sender: TSynEditMark; Changes: TSynEditMarkChangeReasons);
  end;

  { TSynEditMark }

  TSynEditMark = class
  private
    FOldLine: integer;
    procedure SetMarkList(const AValue: TSynEditMarkList);
  protected
    FLine, FColumn, FImage, FPriority: Integer;
    FEdit: TSynEditBase;
    FMarkList: TSynEditMarkList;
    FVisible: boolean;
    FInternalImage: boolean;
    FBookmarkNum: integer;
    FChangeLock: Integer;
    FChanges: TSynEditMarkChangeReasons;
    function  GetEdit: TSynEditBase; virtual;
    procedure SetColumn(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetPriority(const AValue: integer); virtual;
    procedure SetVisible(const Value: boolean); virtual;
    procedure SetInternalImage(const Value: boolean);
    function  GetIsBookmark: boolean;
    procedure DoChange(AChanges: TSynEditMarkChangeReasons); virtual;
    procedure ForceChange(AChanges: TSynEditMarkChangeReasons);
  public
    constructor Create(AOwner: TSynEditBase);
    destructor Destroy; override;
    procedure IncChangeLock;
    procedure DecChangeLock;
    property Line: integer read FLine write SetLine;
    property OldLine: integer read FOldLine;
    property Column: integer read FColumn write SetColumn;
    property Priority: integer read FPriority write SetPriority;
    property ImageIndex: integer read FImage write SetImage;
    property BookmarkNumber: integer read FBookmarkNum write fBookmarkNum;
    property Visible: boolean read FVisible write SetVisible;
    property InternalImage: boolean read FInternalImage write SetInternalImage;
    property IsBookmark: boolean read GetIsBookmark;
    property MarkList: TSynEditMarkList read FMarkList write SetMarkList;
  end;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark) of object;

  TSynEditMarks = array[1..maxMarks] of TSynEditMark;

  { A list of mark objects. Each object cause a litle picture to be drawn in the
    gutter. }

  { TSynEditMarkList }

  TSynEditMarkList = class(TList)
  protected
    FEdit: TSynEditBase;
    FLines: TSynEditStrings;
    fOnChange: TNotifyEvent;
    FChangeHandlers: TSynEditMarkChangedHandlerList;
    procedure DoChange;
    procedure MarkChanged(Sender: TSynEditMark; AChanges: TSynEditMarkChangeReasons); virtual;
    function Get(Index: Integer): TSynEditMark;
    procedure Put(Index: Integer; Item: TSynEditMark);
    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String);
  public
    constructor Create(AOwner: TSynEditBase; ALines: TSynEditStrings);
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
    procedure RegisterChangeHandler(Handler: TSynEditMarkChangeEvent; Filter: TSynEditMarkChangeReasons);
    procedure UnRegisterChangeHandler(Handler: TSynEditMarkChangeEvent);
  public
    property Items[Index: Integer]: TSynEditMark read Get write Put; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function DoMarksCompareBookmarksFirst(Item1, Item2: Pointer): Integer;
function DoMarksCompareBookmarksLast(Item1, Item2: Pointer): Integer;

implementation
uses SynEdit;

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

procedure TSynEditMark.SetMarkList(const AValue: TSynEditMarkList);
begin
  if AValue = nil then begin
    DoChange([smcrRemoved]);
    ForceChange(FChanges);
  end;
  FMarkList := AValue;
  if FMarkList <> nil then
    DoChange([smcrAdded]);
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

procedure TSynEditMark.DoChange(AChanges: TSynEditMarkChangeReasons);
begin
  if FChangeLock > 0 then begin
    FChanges := FChanges + AChanges;
    exit;
  end;
  ForceChange(AChanges);
end;

procedure TSynEditMark.ForceChange(AChanges: TSynEditMarkChangeReasons);
begin
  if (FMarkList <> nil) and (AChanges <> []) then
    FMarkList.MarkChanged(Self, AChanges);
  FChanges := [];
end;

procedure TSynEditMark.SetColumn(const Value: Integer);
begin
  if FColumn = Value then
    exit;
  FColumn := Value;
  DoChange([smcrChanged]);
end;

procedure TSynEditMark.SetImage(const Value: Integer);
begin
  if FImage = Value then
    exit;
  FImage := Value;
  DoChange([smcrChanged]);
end;

procedure TSynEditMark.SetInternalImage(const Value: boolean);
begin
  if FInternalImage = Value then
    exit;
  FInternalImage := Value;
  DoChange([smcrChanged]);
end;

procedure TSynEditMark.SetLine(const Value: Integer);
begin
  if FLine = Value then
    exit;
  FOldLine := FLine;
  FLine := Value;
  DoChange([smcrLine]);
end;

procedure TSynEditMark.SetVisible(const Value: boolean);
begin
  if FVisible = Value then
    exit;
  FVisible := Value;
  DoChange([smcrVisible]);
end;

constructor TSynEditMark.Create(AOwner: TSynEditBase);
begin
  inherited Create;
  FBookmarkNum := -1;
  FEdit := AOwner;
  FPriority := 0;
end;

destructor TSynEditMark.Destroy;
begin
  if FMarkList <> nil then begin
    DoChange([smcrRemoved]);
    FMarkList.Remove(self);
  end;
  inherited Destroy;
end;

procedure TSynEditMark.IncChangeLock;
begin
  inc(FChangeLock);
end;

procedure TSynEditMark.DecChangeLock;
begin
  dec(FChangeLock);
  if (FChangeLock = 0) and (FChanges <> []) then
    DoChange(FChanges);
end;

{ TSynEditMarkList }

function TSynEditMarkList.Add(Item: TSynEditMark): Integer;
begin
  Item.MarkList := self;;
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

constructor TSynEditMarkList.Create(AOwner: TSynEditBase; ALines: TSynEditStrings);
begin
  FChangeHandlers := TSynEditMarkChangedHandlerList.Create;
  inherited Create;
  FEdit := AOwner;
  FLines := ALines;
  FLines.AddEditHandler(@DoLinesEdited);
end;

destructor TSynEditMarkList.Destroy;
begin
  FLines.RemoveEditHandler(@DoLinesEdited);
  while Count > 0 do
    Get(0).Free;
  inherited Destroy;
  FreeAndNil(FChangeHandlers);
end;

procedure TSynEditMarkList.Delete(Index: Integer);
begin
  Items[Index].MarkList := nil;
  inherited Delete(Index);
  DoChange;
end;

procedure TSynEditMarkList.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynEditMarkList.MarkChanged(Sender: TSynEditMark;
  AChanges: TSynEditMarkChangeReasons);
begin
  FChangeHandlers.CallMarkChangedHandlers(Sender, AChanges);
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
  Item.MarkList := Self;
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

procedure TSynEditMarkList.DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
  aLineBrkCnt: Integer; aText: String);
var
  i, Col: Integer;
begin
  if Count = 0 then exit;
  Col := FLines.LogicalToPhysicalPos(Point(aBytePos, aLinePos)).x;
  if aLineBrkCnt > 0 then
  begin
    for i := 0 to Count - 1 do
      if (Items[i].Line > aLinePos) then
        Items[i].Line := Items[i].Line + aLineBrkCnt
      else
      if (Items[i].Line = aLinePos) and (Items[i].Column > col) then begin
        Items[i].Line := Items[i].Line + aLineBrkCnt;
        Items[i].Column := Items[i].Column - Col + 1;
      end;
  end
  else
  if aLineBrkCnt < 0 then
  begin
    for i := 0 to Count - 1 do
      if (Items[i].Line > aLinePos - aLineBrkCnt) then
        Items[i].Line := Items[i].Line + aLineBrkCnt
      else
      if (Items[i].Line > aLinePos) then begin
        Items[i].Line := aLinePos;
        Items[i].Column := Items[i].Column + Col - 1;
      end;
  end
  else
  if aCount > 0 then
  begin
    for i := 0 to Count - 1 do
      if (Items[i].Line = aLinePos) and (Items[i].Column > col) then
        Items[i].Column := Items[i].Column + aCount;
  end
  else
  if aCount < 0 then
  begin
    for i := 0 to Count - 1 do
      if (Items[i].Line = aLinePos) and (Items[i].Column > col) then
        Items[i].Column := Max(Col, Items[i].Column + aCount);
  end;
end;

function TSynEditMarkList.Remove(Item: TSynEditMark): Integer;
begin
  Item.MarkList := nil;
  Result := inherited Remove(Item);
  DoChange;
end;

procedure TSynEditMarkList.RegisterChangeHandler(Handler: TSynEditMarkChangeEvent;
  Filter: TSynEditMarkChangeReasons);
begin
  FChangeHandlers.Add(Handler, Filter);
end;

procedure TSynEditMarkList.UnRegisterChangeHandler(Handler: TSynEditMarkChangeEvent);
begin
  FChangeHandlers.Remove(Handler);
end;

{ TSynEditMarkChangedHandlerList }

procedure TSynEditMarkChangedHandlerList.Add(AHandler: TSynEditMarkChangeEvent;
  Changes: TSynEditMarkChangeReasons);
begin
  AddBitFilter(TMethod(AHandler), Pointer(PtrUInt(Changes)));
end;

procedure TSynEditMarkChangedHandlerList.Remove(AHandler: TSynEditMarkChangeEvent);
begin
  inherited Remove(TMethod(AHandler));
end;

procedure TSynEditMarkChangedHandlerList.CallMarkChangedHandlers(Sender: TSynEditMark;
  Changes: TSynEditMarkChangeReasons);
var
  i: Integer;
begin
  i:=Count;
  while NextDownIndexBitFilter(i, Pointer(PtrUInt(Changes))) do
    TSynEditMarkChangeEvent(FItems[i].FHandler)(Sender, Changes);
end;

end.

