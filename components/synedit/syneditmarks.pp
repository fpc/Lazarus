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

  TSynEditMark = class
  protected
    fLine, fColumn, fImage: Integer;
    FEdit: TSynEditBase;
    fVisible: boolean;
    fInternalImage: boolean;
    fBookmarkNum: integer;
    function GetEdit: TSynEditBase; virtual;
    procedure SetColumn(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetVisible(const Value: boolean); {$IFDEF SYN_LAZARUS}virtual;{$ENDIF} //MWE: Laz needs to know when a line gets visible, so the editor color can be updated
    procedure SetInternalImage(const Value: boolean);
    function GetIsBookmark: boolean;
  public
    constructor Create(AOwner: TSynEditBase);
    property Line: integer read fLine write SetLine;
    property Column: integer read fColumn write SetColumn;
    property ImageIndex: integer read fImage write SetImage;
    property BookmarkNumber: integer read fBookmarkNum write fBookmarkNum;
    property Visible: boolean read fVisible write SetVisible;
    property InternalImage: boolean read fInternalImage write SetInternalImage;
    property IsBookmark: boolean read GetIsBookmark;
  end;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark)
    of object;

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


implementation
uses SynEdit;

type  // This is until InvalidateGutterLines, can be moved to an accessible place
  SynEditAccess = Class(TCustomSynEdit);

{ TSynEditMark }

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
  if fVisible and Assigned(fEdit) then
    SynEditAccess(Pointer(FEdit)).InvalidateGutterLines(fLine, fLine);
end;

procedure TSynEditMark.SetInternalImage(const Value: boolean);
begin
  fInternalImage := Value;
  if fVisible and Assigned(fEdit) then
    SynEditAccess(Pointer(FEdit)).InvalidateGutterLines(fLine, fLine);
end;

procedure TSynEditMark.SetLine(const Value: Integer);
begin
  if fVisible and Assigned(fEdit) then begin
    if fLine > 0 then
      SynEditAccess(Pointer(FEdit)).InvalidateGutterLines(fLine, fLine);
    fLine := Value;
    SynEditAccess(Pointer(FEdit)).InvalidateGutterLines(fLine, fLine);
  end else
    fLine := Value;
end;

procedure TSynEditMark.SetVisible(const Value: boolean);
begin
  if fVisible <> Value then begin
    fVisible := Value;
    if Assigned(fEdit) then
      SynEditAccess(Pointer(FEdit)).InvalidateGutterLines(fLine, fLine);
  end;
end;

constructor TSynEditMark.Create(AOwner: TSynEditBase);
begin
  inherited Create;
  fBookmarkNum := -1;
  fEdit := AOwner;
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
  fEdit := AOwner;
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
  result := TSynEditMark(inherited First);
end;

function TSynEditMarkList.Get(Index: Integer): TSynEditMark;
begin
  result := TSynEditMark(inherited Get(Index));
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
  for i := 0 to Count - 1 do begin
    if Items[i].Line = line then begin
      Inc(cnt);
      marks[cnt] := Items[i];
      if cnt = maxMarks then break;
    end;
  end;
end;

procedure TSynEditMarkList.Insert(Index: Integer; Item: TSynEditMark);
begin
  inherited Insert(Index, Item);
  DoChange;
end;

function TSynEditMarkList.Last: TSynEditMark;
begin
  result := TSynEditMark(inherited Last);
end;

procedure TSynEditMarkList.Place(mark: TSynEditMark);
begin
  if assigned(fEdit) then
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

