unit SynGutter;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, LCLIntf,
  SynEditMiscClasses, SynEditMiscProcs, SynEditFoldedView, SynTextDrawer;

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

  TGutterClickEvent = procedure(Sender: TObject; X, Y, Line: integer;
    mark: TSynEditMark) of object;

  { TSynGutterPartBase }

  TSynGutterPartBase = class(TPersistent)
  private
    FAutoSize : boolean;
    FColor : TColor;
    FCursor: TCursor;
    FVisible: Boolean;
    FWidth : integer;
    FOnChange: TNotifyEvent;
    FOnGutterClick: TGutterClickEvent;
  protected
    procedure SetAutoSize(const AValue : boolean); virtual;
    procedure SetColor(const AValue : TColor); virtual;
    procedure SetVisible(const AValue : boolean); virtual;
    procedure SetWidth(const AValue : integer); virtual;
    procedure DoChange(Sender: TObject); virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      virtual abstract;
    function RealGutterWidth(CharWidth: integer): integer;  virtual; abstract;
    // X/Y are relative to the gutter, not the gutter part
    procedure DoOnGutterClick(X, Y: integer);  virtual;
    property AutoSize: boolean read FAutoSize write SetAutoSize default False;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property Width: integer read FWidth write SetWidth default 10;
    property Visible: boolean read FVisible write SetVisible default True;
    property OnGutterClick: TGutterClickEvent
      read FOnGutterClick write FOnGutterClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  { TSynGutter }

  TSynGutter = class(TPersistent)
  private
    // List of all gutters
    FGutterPartList: TList;
    // Access to inidvidual Gutters
    FLineNumGutter: TSynGutterPartBase;
    FCodeFoldGutter: TSynGutterPartBase;
    FMarkGutter: TSynGutterPartBase;
    FChangesGutter: TSynGutterPartBase;
    FSeparatorGutter: TSynGutterPartBase;

    FEdit: TSynEditBase;
//    FFoldView: TSynEditFoldedView;
    FTextDrawer: TheTextDrawer;

    FColor: TColor;
    FWidth: integer;
    FRightOffset, FLeftOffset: integer;
    FOnChange: TNotifyEvent;
    FCursor: TCursor;
    FVisible: boolean;
    FAutoSize: boolean;
    FOnGutterClick: TGutterClickEvent;
    FAllowSkipGutterSeparatorDraw: boolean;
    procedure SetAutoSize(const Value: boolean);
    procedure SetColor(const Value: TColor);
    procedure SetRightOffset(Value: integer);
    procedure SetVisible(Value: boolean);
    procedure SetWidth(Value: integer);
    procedure DoChange(Sender: TObject);
    function  GetPartGutter(Index : Integer) : TSynGutterPartBase;
    property  GutterPart[Index: Integer]: TSynGutterPartBase read GetPartGutter;
  private
    function GetGutterPartCount: integer;
    function GetSeparatorIndex: integer;
    procedure SetAllowSkipGutterSeparatorDraw(const AValue: Boolean);
    procedure SetLeftOffset(Value: integer);
    procedure SetSeparatorIndex(const AValue: integer);
    // Forward to Code Folding
    procedure SetShowCodeFolding(const Value: boolean);
    procedure SetCodeFoldingWidth(const AValue: integer);
    function  GetShowCodeFolding: boolean;
    function  GetCodeFoldingWidth: Integer;
    // Forward to Line Number
    procedure SetShowLineNumbers(const Value: boolean);
    procedure SetLeadingZeros(const Value: boolean);
    procedure SetDigitCount(Value: integer);
    procedure SetZeroStart(const Value: boolean);
    procedure SetShowOnlyLineNumbersMultiplesOf(const AValue: integer);
    function GetMarkupInfoLineNumber: TSynSelectedColor;
    function GetMarkupInfoModifiedLine: TSynSelectedColor;
    function GetMarkupInfoCodeFoldingTree: TSynSelectedColor;
    function GetDigitCount : Integer;
    function GetZeroStart : Boolean;
    function GetShowOnlyLineNumbersMultiplesOf : Integer;
    function GetShowLineNumbers : Boolean;
    function GetLeadingZeros : Boolean;
    // changes
    procedure SetShowChanges(const AValue: Boolean);
    function GetShowChanges: Boolean;
  protected
    procedure DoDefaultGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
  public
    constructor Create(AOwner : TSynEditBase; AFoldView : TSynEditFoldedView;
      ABookMarkOpt: TSynBookMarkOpt; ATextDrawer: TheTextDrawer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
    function RealGutterWidth(CharWidth: integer): integer;
    procedure DoOnGutterClick(X, Y: integer);
    procedure AutoSizeDigitCount(LinesCount: integer);    // Forward to Line Number
    property GutterPartCount: integer read GetGutterPartCount;
    property SeparatorIndex: integer read GetSeparatorIndex write SetSeparatorIndex;
    property OnGutterClick: TGutterClickEvent
      read FOnGutterClick write FOnGutterClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default FALSE;
    property AllowSkipGutterSeparatorDraw: boolean
      read FAllowSkipGutterSeparatorDraw write SetAllowSkipGutterSeparatorDraw default False;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property RightOffset: integer read FRightOffset write SetRightOffset
      default 2;
    property Visible: boolean read FVisible write SetVisible default TRUE;
    property Width: integer read FWidth write SetWidth default 30;
    // Forward to Marks (Bookmars / Breakpoints)
    property LeftOffset: integer read FLeftOffset write SetLeftOffset
      default 16;
    // Forward to Code Folding
    property ShowCodeFolding: boolean read GetShowCodeFolding
      write SetShowCodeFolding default False;
    property CodeFoldingWidth: integer read GetCodeFoldingWidth write SetCodeFoldingWidth
      default 14;
    // Forward to Line Number
    property ShowChanges: Boolean read GetShowChanges write SetShowChanges default False;
    property ShowLineNumbers: boolean read GetShowLineNumbers
      write SetShowLineNumbers default False;
    property ShowOnlyLineNumbersMultiplesOf: integer
      read GetShowOnlyLineNumbersMultiplesOf
      write SetShowOnlyLineNumbersMultiplesOf default 1;
    property ZeroStart: boolean read GetZeroStart write SetZeroStart;
    property MarkupInfoLineNumber: TSynSelectedColor read GetMarkupInfoLineNumber;
    property MarkupInfoModifiedLine: TSynSelectedColor read GetMarkupInfoModifiedLine;
    property MarkupInfoCodeFoldingTree: TSynSelectedColor read GetMarkupInfoCodeFoldingTree;
    property LeadingZeros: boolean read GetLeadingZeros write SetLeadingZeros
      default FALSE;
    property DigitCount: integer read GetDigitCount  write SetDigitCount
      default 2;
  end;

  { TSynGutterSeparator }

  TSynGutterSeparator = class(TSynGutterPartBase)
  public
    constructor Create(AOwner: TSynEditBase; AFoldView: TSynEditFoldedView);
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    function RealGutterWidth(CharWidth: integer): integer;  override;
  end;


implementation
uses
  SynEdit, SynGutterLineNumber, SynGutterCodeFolding, SynGutterMarks, SynGutterChanges;

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

{ TSynGutter }

constructor TSynGutter.Create(AOwner : TSynEditBase;
  AFoldView : TSynEditFoldedView; ABookMarkOpt: TSynBookMarkOpt;
  ATextDrawer: TheTextDrawer);
var
  i: Integer;
begin
  inherited Create;
  FGutterPartList := TList.Create;

  FEdit := AOwner;
//  FFoldView := AFoldView;
  FTextDrawer := ATextDrawer;

  FMarkGutter := TSynGutterMarks.Create(AOwner, AFoldView, ABookMarkOpt);
  FGutterPartList.Add(FMarkGutter);

  FLineNumGutter := TSynGutterLineNumber.Create(AOwner, AFoldView, ATextDrawer);
  FGutterPartList.Add(FLineNumGutter);

  FChangesGutter := TSynGutterChanges.Create(AOwner, AFoldView);
  FGutterPartList.Add(FChangesGutter);

  FSeparatorGutter := TSynGutterSeparator.Create(AOwner, AFoldView);
  FGutterPartList.Add(FSeparatorGutter);

  FCodeFoldGutter := TSynGutterCodeFolding.Create(AOwner, AFoldView);
  FGutterPartList.Add(FCodeFoldGutter);

  for i := 0 to FGutterPartList.Count-1 do begin
    GutterPart[i].OnChange := {$IFDEF FPC}@{$ENDIF}DoChange;
    GutterPart[i].OnGutterClick := {$IFDEF FPC}@{$ENDIF}DoDefaultGutterClick;
  end;

  Color := clBtnFace;
  Visible := True;
  Width := 30;
  LeftOffset := 0;
  FRightOffset := 0;
end;

destructor TSynGutter.Destroy;
var
  i: Integer;
begin
  for i := 0 to FGutterPartList.Count-1 do
    GutterPart[i].Free;
  FreeAndNil(FGutterPartList);
  inherited Destroy;
end;

procedure TSynGutter.Assign(Source: TPersistent);
var
  Src: TSynGutter;
begin
  if Assigned(Source) and (Source is TSynGutter) then
  begin
    Src := TSynGutter(Source);
    FColor := Src.FColor;
    FVisible := Src.FVisible;
    FWidth := Src.FWidth;
    FRightOffset := Src.FRightOffset;
    FAutoSize := Src.FAutoSize;

    FCodeFoldGutter.Assign(Src.FCodeFoldGutter);
    FMarkGutter.Assign(Src.FMarkGutter);
    FLineNumGutter.Assign(Src.FLineNumGutter);
    FChangesGutter.Assign(Src.FChangesGutter);
    DoChange(Self);
  end else
    inherited;
end;

function TSynGutter.RealGutterWidth(CharWidth: integer): integer;
var
  i: Integer;
begin
  if not FVisible then
  begin
    Result := 0;
    Exit;
  end;

  Result := FLeftOffset + FRightOffset;

  for i := FGutterPartList.Count-1 downto 0 do
    Result := Result + GutterPart[i].RealGutterWidth(CharWidth);
end;

function TSynGutter.GetPartGutter(Index : Integer) : TSynGutterPartBase;
begin
  Result := TSynGutterPartBase(FGutterPartList[Index]);
end;

function TSynGutter.GetGutterPartCount: integer;
begin
  result := FGutterPartList.Count;
end;

function TSynGutter.GetSeparatorIndex: integer;
begin
  if FSeparatorGutter.Visible then
    Result := FGutterPartList.IndexOf(FSeparatorGutter)
  else
    Result := -1;
end;

function TSynGutter.GetMarkupInfoCodeFoldingTree: TSynSelectedColor;
begin
  Result := TSynGutterCodeFolding(FCodeFoldGutter).MarkupInfoCodeFoldingTree;
end;

procedure TSynGutter.SetAllowSkipGutterSeparatorDraw(const AValue: Boolean);
begin
  if FAllowSkipGutterSeparatorDraw <> AValue then
  begin
    FAllowSkipGutterSeparatorDraw := AValue;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetLeftOffset(Value: integer);
begin
  Value := Max(0, Value);
  if FLeftOffset <> Value then
  begin
    FLeftOffset := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetSeparatorIndex(const AValue: integer);
begin
  if AValue < 0 then
    FSeparatorGutter.Visible := False
  else
  begin
    FGutterPartList.Move(FGutterPartList.IndexOf(FSeparatorGutter), AValue);
    FSeparatorGutter.Visible := True;
  end;
  DoChange(Self);
end;

function TSynGutter.GetShowChanges: Boolean;
begin
  Result := TSynGutterChanges(FChangesGutter).Visible;
end;

function TSynGutter.GetMarkupInfoModifiedLine: TSynSelectedColor;
begin
  Result := TSynGutterChanges(FChangesGutter).MarkupInfoModifiedLine;
end;

procedure TSynGutter.SetAutoSize(const Value: boolean);
var
  i: Integer;
begin
  for i := FGutterPartList.Count-1 downto 0 do
    GutterPart[i].AutoSize := Value;
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetColor(const Value: TColor);
var
  i: Integer;
begin
  for i := FGutterPartList.Count-1 downto 0 do
    GutterPart[i].Color := Value;
  if FColor <> Value then
  begin
    FColor := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetRightOffset(Value: integer);
begin
  Value := Max(0, Value);
  if FRightOffset <> Value then
  begin
    FRightOffset := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetVisible(Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetWidth(Value: integer);
begin
  Value := Max(0, Value);
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynGutter.DoDefaultGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
var
  i     : integer;
  offs  : integer;
  allmrk: TSynEditMarks;
begin
  line := TSynEdit(FEdit).PixelsToRowColumn(Point(X, Y)).Y;
  if line <= TSynEdit(FEdit).Lines.Count then begin
    mark := nil;
    TSynEdit(FEdit).Marks.GetMarksForLine(line, allmrk);
    offs := 0;
    for i := 1 to maxMarks do begin
      if assigned(allmrk[i]) then begin
        Inc(offs, TSynEdit(FEdit).BookMarkOptions.XOffset);
        if X < offs then begin
          mark := allmrk[i];
          break;
        end;
      end;
    end;
  end;
  if Assigned(FOnGutterClick) then begin
    // for compatibility invoke this only on the markable area
    FOnGutterClick(Self, X, Y, line, mark);
  end;
end;

procedure TSynGutter.DoOnGutterClick(X, Y: integer);
var
  i, x2 : integer;
begin
  i := 0;
  x2 := x;
  while i < FGutterPartList.Count-1 do begin
    if x2 >= GutterPart[i].Width then
      x2 := x2 - GutterPart[i].Width
    else
      break;
    inc(i)
  end;
  GutterPart[i].DoOnGutterClick(X, Y);
end;

procedure TSynGutter.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
var
  i: integer;
  rcLine: TRect;
  dc: HDC;
begin
  Canvas.Brush.Color := Color;
  dc := Canvas.Handle;
  {$IFDEF SYN_LAZARUS}
  LCLIntf.SetBkColor(dc,Canvas.Brush.Color);
  {$ENDIF}

  // currently redraw full gutter
  AClip.Left := FLeftOffset;

  // Clear all
  fTextDrawer.BeginDrawing(dc);
  fTextDrawer.SetBackColor(Color);
  fTextDrawer.SetForeColor(TSynEdit(FEdit).Font.Color);
  fTextDrawer.SetFrameColor(clNone);
   with AClip do
     fTextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, AClip, nil, 0);
  fTextDrawer.EndDrawing;

  rcLine := AClip;
  rcLine.Right := rcLine.Left;
  for i := 0 to FGutterPartList.Count -1 do
  begin
    if GutterPart[i].Visible then
    begin
      rcLine.Left := rcLine.Right;
      rcLine.Right := rcLine.Left + GutterPart[i].Width;
      GutterPart[i].Paint(Canvas, rcLine, FirstLine, LastLine);
    end;
  end;
end;

{ TSynGutterPartBase }

procedure TSynGutterPartBase.SetAutoSize(const AValue : boolean);
begin
  if FAutoSize=AValue then exit;
  FAutoSize:=AValue;
  DoChange(self);
end;

procedure TSynGutterPartBase.SetColor(const AValue : TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
  DoChange(self);
end;

procedure TSynGutterPartBase.SetVisible(const AValue : boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  DoChange(self);
end;

procedure TSynGutterPartBase.SetWidth(const AValue : integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
  DoChange(self);
end;

procedure TSynGutterPartBase.DoChange(Sender : TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSynGutterPartBase.Create;
begin
  inherited Create;
  FColor := clBtnFace;
  FVisible := True;
  FWidth := 10;
end;

procedure TSynGutterPartBase.Assign(Source : TPersistent);
var
  Src: TSynGutterPartBase;
begin
  if Assigned(Source) and (Source is TSynGutterPartBase) then
  begin
    Src := TSynGutterPartBase(Source);
    FColor := Src.FColor;
    FVisible := Src.FVisible;
    FWidth := Src.FWidth;
    FAutoSize := Src.FAutoSize;
    DoChange(Self);
  end else
    inherited;
end;

procedure TSynGutterPartBase.DoOnGutterClick(X, Y : integer);
begin
  FOnGutterClick(Self, X, Y, 0, nil);
end;

{ Forward to Line Number }

function TSynGutter.GetMarkupInfoLineNumber : TSynSelectedColor;
begin
  Result := TSynGutterLineNumber(FLineNumGutter).MarkupInfoLineNumber;
end;

function TSynGutter.GetDigitCount : Integer;
begin
  Result := TSynGutterLineNumber(FLineNumGutter).DigitCount;
end;

function TSynGutter.GetZeroStart : Boolean;
begin
  Result := TSynGutterLineNumber(FLineNumGutter).ZeroStart;
end;

function TSynGutter.GetLeadingZeros : Boolean;
begin
  Result := TSynGutterLineNumber(FLineNumGutter).LeadingZeros;
end;

function TSynGutter.GetShowOnlyLineNumbersMultiplesOf : Integer;
begin
  Result := TSynGutterLineNumber(FLineNumGutter).ShowOnlyLineNumbersMultiplesOf;
end;

function TSynGutter.GetShowLineNumbers : Boolean;
begin
  Result := TSynGutterLineNumber(FLineNumGutter).Visible;
end;

procedure TSynGutter.SetDigitCount(Value: integer);
begin
  TSynGutterLineNumber(FLineNumGutter).DigitCount := Value;
end;

procedure TSynGutter.SetZeroStart(const Value: boolean);
begin
  TSynGutterLineNumber(FLineNumGutter).ZeroStart := Value;
end;

procedure TSynGutter.SetLeadingZeros(const Value: boolean);
begin
  TSynGutterLineNumber(FLineNumGutter).LeadingZeros := Value;
end;

procedure TSynGutter.SetShowLineNumbers(const Value: boolean);
begin
  TSynGutterLineNumber(FLineNumGutter).Visible := Value;
end;

procedure TSynGutter.SetShowOnlyLineNumbersMultiplesOf(const AValue: integer);
begin
  TSynGutterLineNumber(FLineNumGutter).ShowOnlyLineNumbersMultiplesOf := AValue;
end;


procedure TSynGutter.AutoSizeDigitCount(LinesCount : integer);
begin
  TSynGutterLineNumber(FLineNumGutter).AutoSizeDigitCount(LinesCount);
end;

{ Forward to Code Folding }
procedure TSynGutter.SetShowCodeFolding(const Value: boolean);
begin
  FCodeFoldGutter.Visible := Value;
end;

procedure TSynGutter.SetCodeFoldingWidth(const AValue: integer);
begin
  FCodeFoldGutter.Width := AValue;
end;

function TSynGutter.GetShowCodeFolding : boolean;
begin
  Result := FCodeFoldGutter.Visible;
end;

function TSynGutter.GetCodeFoldingWidth : integer;
begin
  Result := FCodeFoldGutter.Width;
end;

procedure TSynGutter.SetShowChanges(const AValue: Boolean);
begin
  TSynGutterChanges(FChangesGutter).Visible := AValue;
end;


{ TSynGutterSeparator }

constructor TSynGutterSeparator.Create(AOwner: TSynEditBase; AFoldView: TSynEditFoldedView);
begin
  Inherited Create;
  Width := 2;
end;

procedure TSynGutterSeparator.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
var
  i: integer;
  rcLine: TRect;
  dc: HDC;
begin
  with Canvas do
  begin
    Pen.Color := {$IFDEF SYN_LAZARUS}clWhite{$ELSE}clBtnHighlight{$ENDIF};
    Pen.Width := 1;
    with AClip do
    begin
      MoveTo(AClip.Left, AClip.Top);
      LineTo(AClip.Left, AClip.Bottom);
      Pen.Color := {$IFDEF SYN_LAZARUS}clDkGray{$ELSE}clBtnShadow{$ENDIF};
      MoveTo(AClip.Left+1, AClip.Top);
      LineTo(AClip.Left+1, AClip.Bottom);
    end;
  end;
end;

function TSynGutterSeparator.RealGutterWidth(CharWidth: integer): integer;
begin
  If Visible then
    Result := Width
  else
    Result := 0;
end;

end.

