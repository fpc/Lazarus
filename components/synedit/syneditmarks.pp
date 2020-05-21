unit SynEditMarks;

{$I synedit.inc}
{$IFOPT C+}
  {$DEFINE AssertSynMemIndex}
{$ENDIF}
{$IFDEF SynAssert}
  {$DEFINE AssertSynMemIndex}
{$ENDIF}

interface

uses
  Classes, Controls, SysUtils, math, SynEditMiscClasses, LazSynEditText,
  LCLProc, ImgList;

type

  TSynEditMark = class;
  TSynEditMarkLine = class;
  TSynEditMarkLineList = class;
  TSynEditMarkList = class;

  TSynEditMarkChangeReason =
    ( smcrAdded, smcrRemoved,
      smcrLine, smcrColumn,
      smcrVisible,
      smcrChanged
    );
  TSynEditMarkChangeReasons = set of TSynEditMarkChangeReason;

  TSynEditMarkSortOrder = (smsoUnsorted, smsoColumn, smsoPriority, smsoBookmarkFirst, smsoBookMarkLast);

  TSynEditMarkChangeEvent = procedure(Sender: TSynEditMark; Changes: TSynEditMarkChangeReasons)
    of object;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark) of object;

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
    FImageList: TCustomImageList;
    FMarkLine: TSynEditMarkLine;
    FMarkList: TSynEditMarkList;
    FLine: Integer; // Only valid, if not part of a TSynEditMarkLine
    FOldLine: integer;
    FOwnerEdit: TSynEditBase;
    function GetLine: integer;
    procedure SetMarkLine(const AValue: TSynEditMarkLine);
    procedure SetMarkList(const AValue: TSynEditMarkList);
    procedure SetOwnerEdit(const AValue: TSynEditBase);
  protected
    FColumn, FImage, FPriority: Integer;
    FVisible: boolean;
    FInternalImage: boolean;
    FBookmarkNum: integer;
    FChangeLock: Integer;
    FChanges: TSynEditMarkChangeReasons;
    procedure SetColumn(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetPriority(const AValue: integer); virtual;
    procedure SetVisible(const Value: boolean); virtual;
    procedure SetInternalImage(const Value: boolean);
    function  GetIsBookmark: boolean;

    procedure DoChange(AChanges: TSynEditMarkChangeReasons); virtual;
    procedure ForceChange(AChanges: TSynEditMarkChangeReasons);

    property  MarkLine: TSynEditMarkLine read FMarkLine write SetMarkLine;
    property  MarkList: TSynEditMarkList read FMarkList write SetMarkList;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;
    procedure IncChangeLock;
    procedure DecChangeLock;

    property OwnerEdit: TSynEditBase read FOwnerEdit write SetOwnerEdit;

    property OldLine: integer read FOldLine; // not used, if synedit insert/delete lines
    property Line: integer read GetLine write SetLine;
    property Column: integer read FColumn write SetColumn;  // Logical position
    property Priority: integer read FPriority write SetPriority;
    property Visible: boolean read FVisible write SetVisible;

    property BookmarkNumber: integer read FBookmarkNum write fBookmarkNum;
    property IsBookmark: boolean read GetIsBookmark;

    // InternalImage: Use Internal bookmark image 0..9;
    //                Ignore "BookMarkOpt.BookmarkImages" or "ImageList"
    property InternalImage: boolean read FInternalImage write SetInternalImage;
    // ImageIndex:    Index in "BookMarkOpt.BookmarkImages" or "ImageList"
    property ImageIndex: integer read FImage write SetImage;
    // ImageList:     If assigned, then use instead of "BookMarkOpt.BookmarkImages"
    //                Must have same width as "BookMarkOpt.BookmarkImages"
    property ImageList: TCustomImageList read FImageList write FImageList;
  end;

  { TSynEditMarkLine }

  TSynEditMarkLine = class(TSynSizedDifferentialAVLNode)
  private
    FMarks: TFPList;
    FLineList: TSynEditMarkLineList;
    function GetLineNum: Integer;
    function GetMark(Index: Integer): TSynEditMark;
    function GetLeft: TSynEditMarkLine;
    function GetRight: TSynEditMarkLine;
    procedure SetMark(Index: Integer; const AValue: TSynEditMark);
  protected
    FLockChangeSize: Integer;
    FCurrentSort1, FCurrentSort2: TSynEditMarkSortOrder;
    procedure ChangeSize;
    procedure IncLockChangeSize; // used in global destruction
    {$IFDEF SynDebug}
    function Debug: String; override;
    {$ENDIF}
  protected
    property Left: TSynEditMarkLine read GetLeft;
    property Right: TSynEditMarkLine read GetRight;
    property Size: Integer read FSize;
    property LeftSizeSum: Integer read FLeftSizeSum;
    property LineOffset: Integer read FPositionOffset write FPositionOffset;
  public
    constructor Create(ALineNum: Integer; AOwner: TSynEditMarkLineList);
    destructor  Destroy; override;

    procedure Sort(PrimaryOrder: TSynEditMarkSortOrder;
                   SecondaryOrder: TSynEditMarkSortOrder = smsoUnsorted);
    function  Add(Item: TSynEditMark): Integer;
    procedure Delete(Index: Integer);
    function  Remove(Item: TSynEditMark): Integer;
    procedure Clear(FreeMarks: Boolean = False);
    function  Count: Integer;
    function  IndexOf(AMark: TSynEditMark): Integer;
    property  Items[Index: Integer]: TSynEditMark read GetMark write SetMark; default;
    property  LineNum: Integer read GetLineNum;
  end;

  { TSynEditMarkIterator }

  TSynEditMarkIterator = class
  private
    FMarkList: TSynEditMarkList;
    FCurrentItem: TSynEditMark;
    FCurrentIndex: Integer;
    FBOL, FEOL: Boolean;
  public
    constructor Create(AOwner: TSynEditMarkList);

    procedure Invalidate;
    procedure GotoMark(AMark: TSynEditMark);
    procedure GotoBOL;
    procedure GotoEOL;

    function First: Boolean;
    function Last: Boolean;
    function Next: Boolean;
    function Previous: Boolean;

    function NextLine: Boolean;
    function PreviousLine: Boolean;

    function IsValid: Boolean;
    function IsValidAndNotModified: Boolean;

    property  BOL: Boolean read FBOL;
    property  EOL: Boolean read FEOL;

    property Mark: TSynEditMark read FCurrentItem;
  end;

  { TSynEditMarkLineList }

  TSynEditMarkLineList = class(TSynSizedDifferentialAVLTree)
  private
    FMarkList: TSynEditMarkList;
    FIndexIterator: TSynEditMarkIterator;
    // marks
    function  GetMark(Index : Integer): TSynEditMark;
    function  GetMarkCount: Integer;
    procedure SetMark(Index : Integer; const AValue: TSynEditMark);
    // lines
    function  GetMarkLineByMarkIndex(var AMarkIndex: Integer): TSynEditMarkLine;
    function  GetMarkLine(LineNum: Integer): TSynEditMarkLine;
    function  GetOrAddMarkLine(LineNum: Integer; AddIfNotExist: Boolean;
                               UseNext: Boolean = False): TSynEditMarkLine;
  protected
    // will be reset by TSynEditMarkLine.ChangeSize;
    FLastIteratorIndex: Integer;
    function CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode; override;
  public
    constructor Create(AOwner: TSynEditMarkList);
    destructor Destroy; override;

    procedure  Remove(Item: TSynEditMarkLine; FreeMarks: Boolean = False);
    procedure Clear; override; // FreeMarks=False;
    procedure Clear(FreeMarks: Boolean);
    function  GetOrAddLine(LineNum: Integer): TSynEditMarkLine;
    function  GetLineOrNext(LineNum: Integer): TSynEditMarkLine;
    property  Lines[LineNum: Integer]: TSynEditMarkLine read GetMarkLine;
  public
    function  AddMark(Item: TSynEditMark): Integer;
    procedure DeleteMark(Index: Integer);
    function  RemoveMark(Item: TSynEditMark): Integer;
    function  IndexOfMark(AMark: TSynEditMark): Integer;
    property  MarkCount: Integer read GetMarkCount;
    property  Mark[Index : Integer]: TSynEditMark read GetMark write SetMark;
  end;

  { TSynEditMarkList
    A list of mark objects. Each object cause a litle picture to be drawn in the
    gutter.
  }

  TSynEditMarkList = class
  private
    function GetMarkLine(LineNum: Integer): TSynEditMarkLine;
  protected
    FLines: TSynEditStringsLinked;
    FOwnerList: TFPList;
    FMarkLines: TSynEditMarkLineList;
    fOnChange: TNotifyEvent;
    FChangeHandlers: TSynEditMarkChangedHandlerList;
    FInternalIterator: TSynEditMarkIterator;
    procedure DoChange;
    procedure MarkChanged(Sender: TSynEditMark; AChanges: TSynEditMarkChangeReasons); virtual;
    function  Get(Index: Integer): TSynEditMark;
    procedure Put(Index: Integer; Item: TSynEditMark);
    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String);
    function HasOwnerEdit(AEdit: TSynEditBase): Boolean;
  public
    constructor Create(AOwner: TSynEditBase; ALines: TSynEditStringsLinked);
    destructor  Destroy; override;
    {$IFDEF SynDebug}
    procedure   Debug;
    {$ENDIF}


    function  Add(Item: TSynEditMark): Integer;
    procedure Delete(Index: Integer);
    function  Remove(Item: TSynEditMark): Integer;
    function  IndexOf(Item: TSynEditMark): Integer;
    function  Count: Integer;

    procedure ClearLine(line: integer);

    procedure RegisterChangeHandler(Handler: TSynEditMarkChangeEvent; Filter: TSynEditMarkChangeReasons);
    procedure UnRegisterChangeHandler(Handler: TSynEditMarkChangeEvent);
  public
    property Items[Index: Integer]: TSynEditMark read Get write Put; default;
    property Line[LineNum: Integer]: TSynEditMarkLine read GetMarkLine;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function DoMarksCompareBookmarksFirst(Item1, Item2: Pointer): Integer;
function DoMarksCompareBookmarksLast(Item1, Item2: Pointer): Integer;

implementation

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
  if Mark1.Priority <> Mark2.Priority then
    Result := Mark2.Priority - Mark1.Priority
  else
    Result := Mark2.Column - Mark1.Column;
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
  if Mark1.Priority <> Mark2.Priority then
    Result := Mark2.Priority - Mark1.Priority
  else
    Result := Mark2.Column - Mark1.Column;
end;

{ TSynEditMark }

procedure TSynEditMark.SetPriority(const AValue: integer);
begin
  FPriority := AValue;
end;

procedure TSynEditMark.SetMarkList(const AValue: TSynEditMarkList);
begin
  if AValue = FMarkList then
    exit;
  if FMarkList <> nil then begin
    DoChange([smcrRemoved]);
    ForceChange(FChanges);
  end;
  FMarkList := AValue;
  if FMarkList <> nil then
    DoChange([smcrAdded]);
end;

procedure TSynEditMark.SetOwnerEdit(const AValue: TSynEditBase);
begin
  if FOwnerEdit = AValue then exit;
  if (AValue = nil) or (FMarkList = nil) or
     (not FMarkList.HasOwnerEdit(AValue))
  then
    raise Exception.Create('Invalid Owner');
  FOwnerEdit := AValue;
end;

function TSynEditMark.GetLine: integer;
begin
  if FMarkLine <> nil then
    Result := FMarkLine.LineNum
  else
    Result := FLine;
end;

procedure TSynEditMark.SetMarkLine(const AValue: TSynEditMarkLine);
begin
  if FMarkLine = AValue then exit;
  // keep the current line, if we loose the markline
  if (AValue = nil) and (FMarkLine <> nil) then
    FLine := FMarkLine.LineNum;
  FMarkLine := AValue;
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
  DoChange([smcrColumn]);
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
var
  f: Boolean;
begin
  if Line = Value then
    exit;
  f := FMarkLine <> nil;
  if f then
    FMarkLine.Remove(self);
  FMarkLine := nil;;
  FOldLine := FLine;
  FLine := Value;
  if f and (FMarkList <> nil) then
    FMarkList.FMarkLines.AddMark(self);
  DoChange([smcrLine]);
end;

procedure TSynEditMark.SetVisible(const Value: boolean);
begin
  if FVisible = Value then
    exit;
  FVisible := Value;
  DoChange([smcrVisible]);
end;

constructor TSynEditMark.Create(ASynEdit: TSynEditBase);
begin
  inherited Create;
  FOwnerEdit := ASynEdit;
  FBookmarkNum := -1;
  FPriority := 0;
end;

destructor TSynEditMark.Destroy;
begin
  if FMarkList <> nil then begin
    DoChange([smcrRemoved]);
    FMarkList.Remove(self); // includes MarkLine
  end
  else
  if FMarkLine <> nil then
    FMarkLine.Remove(Self);
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

{ TSynEditMarkLine }

function TSynEditMarkLine.GetMark(Index: Integer): TSynEditMark;
begin
  Result := TSynEditMark(FMarks[Index]);
end;

function TSynEditMarkLine.GetLeft: TSynEditMarkLine;
begin
  Result := TSynEditMarkLine(FLeft);
end;

function TSynEditMarkLine.GetLineNum: Integer;
begin
  Result := LineOffset;
  if FParent <> nil then
    Result := Result + TSynEditMarkLine(FParent).GetLineNum;
end;

function TSynEditMarkLine.GetRight: TSynEditMarkLine;
begin
  Result := TSynEditMarkLine(FRight);
end;

procedure TSynEditMarkLine.SetMark(Index: Integer; const AValue: TSynEditMark);
begin
  if Items[Index] = AValue then
    exit;
  Items[Index].MarkLine := nil;
  FMarks[Index] := AValue;
  AValue.MarkLine := Self;
end;

procedure TSynEditMarkLine.ChangeSize;
var
  Cnt: SmallInt;
begin
  Cnt := Count;
  if (FLockChangeSize > 0) or (FSize = Cnt) then exit;
  FLineList.FLastIteratorIndex := -1;
  if Cnt = 0 then begin
    inc(FLockChangeSize);
    FLineList.Remove(Self);
  end else begin
    AdjustParentLeftCount(Cnt - FSize);
    FSize := Cnt;
  end;
end;

procedure TSynEditMarkLine.IncLockChangeSize;
begin
  inc(FLockChangeSize);
end;

{$IFDEF SynDebug}
function TSynEditMarkLine.Debug: String;
var
  i: Integer;
begin
  Result := inherited;
  Result := Result
          + Format('Cnt=%d, LineNm=%d  ', [Count, LineNum]);
  for i := 0 to Count-1 do
    Result := Result
            + Format(': L=%d, C=%d, Bk=%d %s, Img=%d %s :',
                     [Items[i].Line, Items[i].Column,
                      Items[i].BookmarkNumber, dbgs(Items[i].IsBookmark),
                      Items[i].ImageIndex, dbgs(Items[i].InternalImage) ]);
end;
{$ENDIF}

constructor TSynEditMarkLine.Create(ALineNum: Integer; AOwner: TSynEditMarkLineList);
begin
  inherited Create;;
  FMarks := TFPList.Create;
  FLineList := AOwner;
  FPositionOffset := ALineNum;
  FLockChangeSize := 0;
  FCurrentSort1 := smsoUnsorted;
end;

destructor TSynEditMarkLine.Destroy;
begin
  Clear(True);
  inherited Destroy;
  FreeAndNil(FMarks);
end;

function CompareSynEditMarks(Mark1, Mark2: Pointer): Integer;
var
  m1: TSynEditMark absolute Mark1;
  m2: TSynEditMark absolute Mark2;
begin
  case m1.MarkLine.FCurrentSort1 of
    smsoColumn:        Result := m2.Column - m1.Column;
    smsoPriority:      Result := m2.Priority - m1.Priority;
    smsoBookmarkFirst:
      if (m1.IsBookmark) and (not m2.IsBookmark) then Result := -1
      else if (not m1.IsBookmark) and (m2.IsBookmark) then Result := 1
      else Result := 0;
    smsoBookMarkLast:
      if (m1.IsBookmark) and (not m2.IsBookmark) then Result := 1
      else if (not m1.IsBookmark) and (m2.IsBookmark) then Result := -1
      else Result := 0;
    else
      Result := 0;
  end;
  if Result <> 0 then
    exit;

  case m1.MarkLine.FCurrentSort2 of
    smsoColumn:        Result := m2.Column - m1.Column;
    smsoPriority:      Result := m2.Priority - m1.Priority;
    smsoBookmarkFirst:
      if (m1.IsBookmark) and (not m2.IsBookmark) then Result := -1
      else if (not m1.IsBookmark) and (m2.IsBookmark) then Result := 1
      else Result := 0;
    smsoBookMarkLast:
      if (m1.IsBookmark) and (not m2.IsBookmark) then Result := 1
      else if (not m1.IsBookmark) and (m2.IsBookmark) then Result := -1
      else Result := 0;
    else
      Result := 0;
  end;
  if Result <> 0 then
    exit;

  Result := m2.Column - m1.Column;
  if Result <> 0 then
    exit;

  Result := m2.Priority - m1.Priority;
  if Result <> 0 then
    exit;

  if Mark2 > Mark1 then
    Result := 1
  else if Mark1 > Mark2 then
    Result := -1
  else
    Result := 0;
end;

procedure TSynEditMarkLine.Sort(PrimaryOrder: TSynEditMarkSortOrder;
  SecondaryOrder: TSynEditMarkSortOrder);
begin
  if PrimaryOrder = smsoUnsorted then
    PrimaryOrder := SecondaryOrder;
  if PrimaryOrder = SecondaryOrder then
    SecondaryOrder := smsoUnsorted;

  if (PrimaryOrder = FCurrentSort1) and (SecondaryOrder = FCurrentSort2) then
    exit;

  FCurrentSort1 := PrimaryOrder;
  FCurrentSort2 := SecondaryOrder;

  if PrimaryOrder = smsoUnsorted then
    exit;

  FMarks.Sort(@CompareSynEditMarks);
end;

function TSynEditMarkLine.Add(Item: TSynEditMark): Integer;
begin
  FCurrentSort1 := smsoUnsorted;
  Result := FMarks.Add(Item);
  Item.MarkLine := Self;
  ChangeSize;
end;

procedure TSynEditMarkLine.Delete(Index: Integer);
begin
  Items[Index].MarkLine := nil;
  FMarks.Delete(Index);
  ChangeSize;
end;

function TSynEditMarkLine.Remove(Item: TSynEditMark): Integer;
begin
  Item.MarkLine := nil;
  Result := FMarks.Remove(Item);
  ChangeSize;
end;

procedure TSynEditMarkLine.Clear(FreeMarks: Boolean = False);
begin
  inc(FLockChangeSize);
  try
    while Count > 0 do begin
      if FreeMarks then begin
        Items[0].MarkList := nil; // stop destroy from removing item from list
        Items[0].FMarkLine := nil; // stop destroy from removing item from self
        Items[0].Free
      end else
        Items[0].MarkLine := nil;
      FMarks.Delete(0);
    end;
  finally
    dec(FLockChangeSize);
  end;
  ChangeSize;
end;

function TSynEditMarkLine.Count: Integer;
begin
  Result := FMarks.Count;
end;

function TSynEditMarkLine.IndexOf(AMark: TSynEditMark): Integer;
begin
  Result := FMarks.IndexOf(AMark);
end;

{ TSynEditMarkLineList }

function TSynEditMarkLineList.GetMark(Index : Integer): TSynEditMark;
var
  MLine: TSynEditMarkLine;
  i: Integer;
begin
  (* If any Mark was added/removed from it's line then FLastIteratorIndex will be -1
     If Items in the current line changed order then IsValidAndNotModified is False
     If Items in another line changed order, it is valid to use the iterator
  *)
  if (FLastIteratorIndex >= 0) and
     ( (FLastIteratorIndex - 1 = Index) or     // Return previous
       (FLastIteratorIndex     = Index) or     // Return current
       (FLastIteratorIndex + 1 = Index) ) and  // Return next
     (FIndexIterator.IsValidAndNotModified)
  then begin
    // can use iterator for quick access
    if (FLastIteratorIndex + 1 = Index) then
      FIndexIterator.Next
    else if (FLastIteratorIndex - 1 = Index) then
      FIndexIterator.Previous;
    Result := FIndexIterator.Mark;
    FLastIteratorIndex := Index;
    exit;
  end;

  i := Index;
  MLine := GetMarkLineByMarkIndex(i);
  Result := MLine[i];

  FIndexIterator.GotoMark(Result);
  FLastIteratorIndex := Index;
end;

function TSynEditMarkLineList.GetMarkCount: Integer;
var
  n: TSynEditMarkLine;
begin
  n := TSynEditMarkLine(FRoot);
  Result := 0;
  while n <> nil do begin
    Result := Result + n.LeftSizeSum + n.Size;
    n := n.Right;
  end;
end;

function TSynEditMarkLineList.GetMarkLineByMarkIndex(var AMarkIndex: Integer): TSynEditMarkLine;
var
  d1, d2: Integer;
begin
  Result := TSynEditMarkLine(FindNodeAtLeftSize(AMarkIndex, d1, d2));
  AMarkIndex := AMarkIndex - d2;
end;

function TSynEditMarkLineList.GetMarkLine(LineNum: Integer): TSynEditMarkLine;
begin
  Result := GetOrAddMarkLine(LineNum, False);
end;

function TSynEditMarkLineList.GetOrAddMarkLine(LineNum: Integer;
  AddIfNotExist: Boolean; UseNext: Boolean = False): TSynEditMarkLine;
var
  d1, d2: Integer;
begin
  if AddIfNotExist then
    Result := TSynEditMarkLine(FindNodeAtPosition(LineNum, afmCreate, d1, d2))
  else
  if UseNext then
    Result := TSynEditMarkLine(FindNodeAtPosition(LineNum, afmNext, d1, d2))
  else
    Result := TSynEditMarkLine(FindNodeAtPosition(LineNum, afmNil, d1, d2));
end;

function TSynEditMarkLineList.CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode;
begin
  Result := TSynEditMarkLine.Create(APosition, Self);
end;

constructor TSynEditMarkLineList.Create(AOwner: TSynEditMarkList);
begin
  inherited Create;
  FMarkList := AOwner;
  FIndexIterator := TSynEditMarkIterator.Create(FMarkList);
  FLastIteratorIndex := -1;
end;

procedure TSynEditMarkLineList.SetMark(Index : Integer; const AValue: TSynEditMark);
var
  l: TSynEditMarkLine;
begin
  l := GetMarkLineByMarkIndex(Index);
  l[Index] := AValue;
end;

destructor TSynEditMarkLineList.Destroy;
begin
  Clear(True);
  FreeAndNil(FIndexIterator);
  inherited Destroy;
end;

procedure TSynEditMarkLineList.Remove(Item: TSynEditMarkLine; FreeMarks: Boolean);
begin
  Item.Clear(FreeMarks);
  RemoveNode(Item);
  Item.Free;
end;

procedure TSynEditMarkLineList.Clear;
begin
  Clear(False);
end;

procedure TSynEditMarkLineList.Clear(FreeMarks: Boolean);
  procedure DeleteNode(ANode: TSynEditMarkLine);
  begin
    ANode.IncLockChangeSize;
    ANode.Clear(FreeMarks);
    if ANode.Left  <> nil then DeleteNode(ANode.Left);
    if ANode.Right <> nil then DeleteNode(ANode.Right);
    DisposeNode(TSynSizedDifferentialAVLNode(ANode));
  end;
begin
  if FRoot <> nil then DeleteNode(TSynEditMarkLine(FRoot));
  SetRoot(nil);
end;

function TSynEditMarkLineList.GetOrAddLine(LineNum: Integer): TSynEditMarkLine;
begin
  Result := GetOrAddMarkLine(LineNum, True);
end;

function TSynEditMarkLineList.GetLineOrNext(LineNum: Integer): TSynEditMarkLine;
begin
  Result := GetOrAddMarkLine(LineNum, False, True);
end;

function TSynEditMarkLineList.AddMark(Item: TSynEditMark): Integer;
var
  l: TSynEditMarkLine;
begin
  l := GetOrAddLine(Item.Line);
  Result := l.Add(Item);
  Result := Result + l.GetSizesBeforeSum;
end;

procedure TSynEditMarkLineList.DeleteMark(Index: Integer);
var
  m: TSynEditMark;
begin
  m := Mark[Index];
  m.MarkLine.Remove(m)
end;

function TSynEditMarkLineList.RemoveMark(Item: TSynEditMark): Integer;
begin
  if Item.MarkLine = nil then
    exit(-1);
  Result := Item.MarkLine.GetSizesBeforeSum;
  Result := Result + Item.MarkLine.Remove(Item);
end;

function TSynEditMarkLineList.IndexOfMark(AMark: TSynEditMark): Integer;
var
  l: TSynEditMarkLine;
begin
  l := AMark.MarkLine;
  Result := l.GetSizesBeforeSum + l.IndexOf(AMark);
end;

{ TSynEditMarkList }

function TSynEditMarkList.Add(Item: TSynEditMark): Integer;
begin
  Result := FMarkLines.AddMark(Item);
  Item.MarkList := Self;
  DoChange;
end;

procedure TSynEditMarkList.ClearLine(Line: integer);
var
  l: TSynEditMarkLine;
begin
  // TODO: move to marklinelist
  l := FMarkLines.Lines[Line];
  if l <> nil then
    l.Clear(True);
end;

constructor TSynEditMarkList.Create(AOwner: TSynEditBase;
  ALines: TSynEditStringsLinked);
begin
  FOwnerList := TFPList.Create;
  FOwnerList.Add(AOwner);
  FMarkLines := TSynEditMarkLineList.Create(Self);
  FChangeHandlers := TSynEditMarkChangedHandlerList.Create;
  inherited Create;
  FLines := ALines;
  FLines.AddEditHandler(@DoLinesEdited);
  FInternalIterator := TSynEditMarkIterator.Create(Self);
end;

destructor TSynEditMarkList.Destroy;
begin
  FLines.RemoveEditHandler(@DoLinesEdited);
  inherited Destroy;
  // Todo: clear changehandlers first?
  FreeAndNil(FMarkLines);  // will free all Marks
  FreeAndNil(FChangeHandlers);
  FreeAndNil(FInternalIterator);
  FreeAndNil(FOwnerList);
end;

{$IFDEF SynDebug}
procedure TSynEditMarkList.Debug;
begin
  FMarkLines.Debug;
end;
{$ENDIF}

function TSynEditMarkList.GetMarkLine(LineNum: Integer): TSynEditMarkLine;
begin
  Result := FMarkLines.Lines[LineNum];
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

function TSynEditMarkList.Get(Index: Integer): TSynEditMark;
begin
  Result := FMarkLines.Mark[Index];
end;

//Returns up to maxMarks book/gutter marks for a chosen line.

procedure TSynEditMarkList.Delete(Index: Integer);
var
  Mrk: TSynEditMark;
begin
  Mrk := FMarkLines.Mark[Index];
  Mrk.MarkList := Self;
  FMarkLines.RemoveMark(Mrk);
  DoChange;
end;

procedure TSynEditMarkList.Put(Index: Integer; Item: TSynEditMark);
begin
  FMarkLines.Mark[Index] := Item;
  Item.MarkList := Self;
  DoChange;
end;

procedure TSynEditMarkList.DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
  aLineBrkCnt: Integer; aText: String);
var
  i: Integer;
  CurLine, NextLine: TSynEditMarkLine;
  LinePos, LineBSize: Integer;
  f: Boolean;
  Mrk: TSynEditMark;
begin
  CurLine := FMarkLines.GetLineOrNext(aLinePos);
  if (CurLine = nil) then
    exit;

  LinePos := CurLine.LineNum;
  LineBSize := 0;
  FInternalIterator.Invalidate; // TODO: better notification system

  if aLineBrkCnt > 0 then begin
    FMarkLines.AdjustForLinesInserted(aLinePos + 1, aLineBrkCnt);
    FInternalIterator.GotoMark(CurLine[0]); // TODO: better notification system
    if (LinePos = aLinePos) then begin
      i := CurLine.Count - 1;
      while i >= 0 do begin
        Mrk := CurLine.Items[i];
        if (Mrk.Column > aBytePos) then begin
          Mrk.Line   := Mrk.Line + aLineBrkCnt;
          Mrk.Column := Mrk.Column - aBytePos + 1;
        end;
        dec(i);
      end;
    end;
  end

  else
  if aLineBrkCnt < 0 then begin
    if (LinePos = aLinePos) then
      CurLine := TSynEditMarkLine(CurLine.Successor(LinePos, LineBSize));
    while (CurLine <> nil) and (LinePos <= aLinePos - aLineBrkCnt) do begin
      f := LinePos = aLinePos - aLineBrkCnt;
      NextLine := TSynEditMarkLine(CurLine.Successor(LinePos, LineBSize));
      i := CurLine.Count - 1;
      while i >= 0 do begin
        Mrk := CurLine.Items[i];
        Mrk.Line := aLinePos;
        if f then
          Mrk.Column := Mrk.Column + aBytePos - 1
        else
          Mrk.Column := aBytePos;  // or delete ?
        dec(i);
      end;
      CurLine := NextLine;
    end;
    if CurLine <> nil then FInternalIterator.GotoMark(CurLine[0]); // TODO: better notification system
    FMarkLines.AdjustForLinesDeleted(aLinePos + 1, -aLineBrkCnt);
  end

  else
  if aLinePos = LinePos then begin
    if aCount > 0 then begin
      for i := 0 to CurLine.Count - 1 do
        if (CurLine.Items[i].Column > aBytePos) then
          CurLine.Items[i].Column := CurLine.Items[i].Column + aCount
    end
    else if aCount < 0 then begin
      for i := 0 to CurLine.Count - 1 do
        if (CurLine.Items[i].Column > aBytePos) then
          CurLine.Items[i].Column := Max(aBytePos, CurLine.Items[i].Column + aCount);
    end;
  end;

  if FInternalIterator.Mark <> nil then begin // TODO: better notification system
    repeat
      FInternalIterator.Mark.DoChange([smcrLine]);
    until not FInternalIterator.Next;
  end;

end;

function TSynEditMarkList.HasOwnerEdit(AEdit: TSynEditBase): Boolean;
begin
  Result := FOwnerList.IndexOf(AEdit) >= 0;
end;

function TSynEditMarkList.Remove(Item: TSynEditMark): Integer;
begin
  Item.MarkList := nil;
  Result := FMarkLines.RemoveMark(Item);
  DoChange;
end;

function TSynEditMarkList.IndexOf(Item: TSynEditMark): Integer;
begin
  Result := FMarkLines.IndexOfMark(Item);
end;

function TSynEditMarkList.Count: Integer;
begin
  Result := FMarkLines.MarkCount;
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
  AddBitFilter(TMethod(AHandler), LongInt(Changes));
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
  while NextDownIndexBitFilter(i, LongInt(Changes)) do
    TSynEditMarkChangeEvent(FItems[i].FHandler)(Sender, Changes);
end;

{ TSynEditMarkIterator }

procedure TSynEditMarkIterator.GotoBOL;
begin
  FBOL := True;
  FEOL := False;
  FCurrentItem := nil;
end;

procedure TSynEditMarkIterator.GotoEOL;
begin
  FBOL := False;
  FEOL := True;
  FCurrentItem := nil;
end;

constructor TSynEditMarkIterator.Create(AOwner: TSynEditMarkList);
begin
  inherited Create;
  FMarkList := AOwner;
  FCurrentItem := nil;
  FBOL := False;
  FEOL := False;
end;

procedure TSynEditMarkIterator.Invalidate;
begin
  FCurrentItem := nil;
  FBOL := False;
  FEOL := False;
end;

procedure TSynEditMarkIterator.GotoMark(AMark: TSynEditMark);
begin
  if  (AMark <> nil) and (AMark.FMarkList <> FMarkList) then
    raise Exception.Create('Invalid list');

  FCurrentItem := AMark;
  FBOL := False;
  FEOL := False;
end;

function TSynEditMarkIterator.First: Boolean;
var
  ML: TSynEditMarkLine;
begin
  ML := TSynEditMarkLine(FMarkList.FMarkLines.First);
  if ML <> nil then
    FCurrentItem := ML[0]
  else
    FCurrentItem := nil;
  FCurrentIndex := 0;
  FBOL := FCurrentItem = nil;
  FEOL := FCurrentItem = nil;
  Result := FCurrentItem <> nil;
end;

function TSynEditMarkIterator.Last: Boolean;
var
  ML: TSynEditMarkLine;
begin
  ML := TSynEditMarkLine(FMarkList.FMarkLines.Last);
  if ML <> nil then
    FCurrentItem := ML[ML.Count - 1]
  else
    FCurrentItem := nil;
  FCurrentIndex := -1;
  FBOL := FCurrentItem = nil;
  FEOL := FCurrentItem = nil;
  Result := FCurrentItem <> nil;
end;

function TSynEditMarkIterator.Next: Boolean;
var
  ML: TSynEditMarkLine;
begin
  if FBOL then begin
    First;
    Result := FCurrentItem <> nil;
    exit;
  end
  else if FCurrentItem = nil then begin
    FBOL := False;
    FEOL := False;
    Result := False;
    exit;
  end;

  if (FCurrentIndex < 0) or (FCurrentIndex >= FCurrentItem.FMarkLine.Count) or
     (FCurrentItem.FMarkLine[FCurrentIndex] <> FCurrentItem)
  then
    FCurrentIndex := FCurrentItem.FMarkLine.IndexOf(FCurrentItem);

  inc(FCurrentIndex);
  if FCurrentIndex < FCurrentItem.FMarkLine.Count then begin
    FCurrentItem := FCurrentItem.FMarkLine[FCurrentIndex];
    Result := True;
  end else begin
    ML := TSynEditMarkLine(FCurrentItem.FMarkLine.Successor);
    if ML <> nil then begin
      FCurrentIndex := 0;
      FCurrentItem := ML[FCurrentIndex];
      Result := True;
    end else begin
      FCurrentItem := nil;
      FEOL := True;
      Result := False;
    end;
  end;
end;

function TSynEditMarkIterator.Previous: Boolean;
var
  ML: TSynEditMarkLine;
begin
  if FEOL then begin
    Last;
    Result := FCurrentItem <> nil;
    exit;
  end
  else if FCurrentItem = nil then begin
    FBOL := False;
    FEOL := False;
    Result := False;
    exit;
  end;

  if (FCurrentIndex < 0) or (FCurrentIndex >= FCurrentItem.FMarkLine.Count) or
     (FCurrentItem.FMarkLine[FCurrentIndex] <> FCurrentItem)
  then
    FCurrentIndex := FCurrentItem.FMarkLine.IndexOf(FCurrentItem);

  dec(FCurrentIndex);
  if FCurrentIndex >= 0 then begin
    FCurrentItem := FCurrentItem.FMarkLine[FCurrentIndex];
    Result := True;
  end else begin
    ML := TSynEditMarkLine(FCurrentItem.FMarkLine.Precessor);
    if ML <> nil then begin
      FCurrentIndex := ML.Count - 1;
      FCurrentItem := ML[FCurrentIndex];
      Result := True;
    end else begin
      FCurrentItem := nil;
      FBOL := True;
      Result := False;
    end;
  end;
end;

function TSynEditMarkIterator.NextLine: Boolean;
var
  ML: TSynEditMarkLine;
begin
  if FBOL then begin
    First;
    Result := FCurrentItem <> nil;
    exit;
  end
  else if FCurrentItem = nil then begin
    FBOL := False;
    FEOL := False;
    Result := False;
    exit;
  end;

  ML := TSynEditMarkLine(FCurrentItem.FMarkLine.Successor);
  if ML <> nil then begin
    FCurrentIndex := 0;
    FCurrentItem := ML[FCurrentIndex];
    Result := True;
  end else begin
    FCurrentItem := nil;
    FEOL := True;
    Result := False;
  end;
end;

function TSynEditMarkIterator.PreviousLine: Boolean;
var
  ML: TSynEditMarkLine;
begin
  if FEOL then begin
    Last;
    Result := FCurrentItem <> nil;
    exit;
  end
  else if FCurrentItem = nil then begin
    FBOL := False;
    FEOL := False;
    Result := False;
    exit;
  end;

  ML := TSynEditMarkLine(FCurrentItem.FMarkLine.Precessor);
  if ML <> nil then begin
    FCurrentIndex := ML.Count - 1;
    FCurrentItem := ML[FCurrentIndex];
    Result := True;
  end else begin
    FCurrentItem := nil;
    FBOL := True;
    Result := False;
  end;
end;

function TSynEditMarkIterator.IsValid: Boolean;
begin
  Result := (FCurrentItem <> nil) or FBOL or FEOL;
end;

function TSynEditMarkIterator.IsValidAndNotModified: Boolean;
begin
  Result := IsValid and
            (FCurrentIndex >= 0) and (FCurrentIndex < FCurrentItem.FMarkLine.Count) and
            (FCurrentItem.FMarkLine[FCurrentIndex] = FCurrentItem);
end;

end.

