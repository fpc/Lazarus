unit SynGutter;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, LCLIntf, LCLProc,
  LCLClasses,
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

  TGutterClickEvent = procedure(Sender: TObject; X, Y, Line: integer;
    mark: TSynEditMark) of object;

type

  TSynGutter = class;
  TSynGutterPartBase = class;
  TSynGutterPartBaseClass = class of TSynGutterPartBase;

  { TSynGutterPartList }

  TSynGutterPartList = class(TSynObjectList)
  private
    FGutter: TSynGutter;
    function GetPart(Index: Integer): TSynGutterPartBase;
    function GetSynEdit: TSynEditBase;
    procedure PutPart(Index: Integer; const AValue: TSynGutterPartBase);
  protected
    procedure RegisterItem(AnItem: TSynObjectListItem); override;
    property Gutter: TSynGutter read FGutter;
    property SynEdit:TSynEditBase read GetSynEdit;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AGutter: TSynGutter);
    destructor  Destroy; override;
    property Items[Index: Integer]: TSynGutterPartBase
      read GetPart write PutPart; default;
  end;

  { TSynGutterPartBase }

  TSynGutterPartBase = class(TSynObjectListItem)
  private
    FSynEdit: TSynEditBase;
    FGutter: TsynGutter;
    FAutoSize : boolean;
    FMarkupInfo: TSynSelectedColor;
    FCursor: TCursor;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    FOnGutterClick: TGutterClickEvent;
    function GetGutterParts: TSynGutterPartList;
    procedure SetMarkupInfo(const AValue: TSynSelectedColor);
    procedure SetRealWidth(const AValue: Integer);
  protected
    FWidth : integer;
    procedure SetAutoSize(const AValue : boolean); virtual;
    procedure SetVisible(const AValue : boolean); virtual;
    procedure SetWidth(const AValue : integer); virtual;
    procedure DoChange(Sender: TObject); virtual;
    property GutterParts: TSynGutterPartList read GetGutterParts;
    property Gutter: TSynGutter read FGutter;
    property SynEdit:TSynEditBase read FSynEdit;
    property RealWidth: Integer write SetRealWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      virtual abstract;
  public
    function RealGutterWidth(CharWidth: integer): integer;  virtual; abstract;
    // X/Y are relative to the gutter, not the gutter part
    procedure DoOnGutterClick(X, Y: integer);  virtual;
    property OnGutterClick: TGutterClickEvent
      read FOnGutterClick write FOnGutterClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property MarkupInfo: TSynSelectedColor read FMarkupInfo write SetMarkupInfo;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default True;
    property Width: integer read FWidth write SetWidth default 10;
    property Visible: boolean read FVisible write SetVisible default True;
  end;

  { TSynGutter }

  TSynGutter = class(TPersistent)
  private
    // List of all gutters
    FGutterPartList: TSynGutterPartList;
    FEdit: TSynEditBase;
    FFoldView: TSynEditFoldedView;
    FTextDrawer: TheTextDrawer;

    FColor: TColor;
    FWidth: integer;
    FRightOffset, FLeftOffset: integer;
    FOnChange: TNotifyEvent;
    FCursor: TCursor;
    FVisible: boolean;
    FAutoSize: boolean;
    FOnGutterClick: TGutterClickEvent;
    procedure SetAutoSize(const Value: boolean);
    procedure SetColor(const Value: TColor);
    procedure SetLeftOffset(Value: integer);
    procedure SetRightOffset(Value: integer);
    procedure SetVisible(Value: boolean);
    procedure SetWidth(Value: integer);
    function  GetPartGutter(Index : Integer) : TSynGutterPartBase;
    function  GetGutterPartCount: integer;
    function  GetGutterPartCountByClass(AClass: TSynGutterPartBaseClass): integer;
    function  GetGutterPartVisibleByClass(AClass: TSynGutterPartBaseClass): Boolean;
    function  GetPartGutterByClass(AClass: TSynGutterPartBaseClass; Index: Integer): TSynGutterPartBase;
    procedure SetGutterParts(const AValue: TSynGutterPartList);
    procedure SetGutterPartVisibleByClass(AClass: TSynGutterPartBaseClass; const AValue: Boolean);
    procedure DoChange(Sender: TObject);
    procedure Clear;
  protected
    procedure DoDefaultGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure RegisterNewGutterPartList(APartList: TSynGutterPartList);
  public
    property SynEdit: TSynEditBase read FEdit;
    property FoldView: TSynEditFoldedView read FFoldView;
    property TextDrawer: TheTextDrawer read FTextDrawer;
  public
    constructor Create(AOwner : TSynEditBase; AFoldedLinesView: TSynEditFoldedView;
                      ATextDrawer: TheTextDrawer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
    function  RealGutterWidth(CharWidth: integer): integer;
    procedure DoOnGutterClick(X, Y: integer);
    property  OnGutterClick: TGutterClickEvent
      read FOnGutterClick write FOnGutterClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    property GutterPartCount: integer read GetGutterPartCount;
    property GutterPartCountByClass[AClass: TSynGutterPartBaseClass]: integer
      read GetGutterPartCountByClass;
    property GutterPart[Index: Integer]: TSynGutterPartBase read GetPartGutter;
    property GutterPartByClass[AClass: TSynGutterPartBaseClass; Index: Integer]:
      TSynGutterPartBase read GetPartGutterByClass;
    // Common PartProprties
    property GutterPartVisibleByClass[AClass: TSynGutterPartBaseClass]: Boolean
      read GetGutterPartVisibleByClass write SetGutterPartVisibleByClass;
    // Some Methods for convinience access to common GutterParts
    procedure AutoSizeDigitCount(LinesCount: integer); // Forward to Line Number
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default True;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property LeftOffset: integer read FLeftOffset write SetLeftOffset
      default 0;
    property RightOffset: integer read FRightOffset write SetRightOffset
      default 0;
    property Visible: boolean read FVisible write SetVisible default TRUE;
    property Width: integer read FWidth write SetWidth default 30;
    property GutterParts: TSynGutterPartList read FGutterPartList write SetGutterParts;
  end;

  { TSynGutterSeparator }

  TSynGutterSeparator = class(TSynGutterPartBase)
  public
    constructor Create(AOwner: TComponent); override;
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

constructor TSynGutter.Create(AOwner: TSynEditBase;
  AFoldedLinesView: TSynEditFoldedView; ATextDrawer: TheTextDrawer);
begin
  inherited Create;

  FEdit := TSynEdit(AOwner);
  FTextDrawer := ATextDrawer;
  FFoldView := AFoldedLinesView;
  TSynGutterPartList.Create(AOwner, self);
  Color := clBtnFace;
  Visible := True;
  Width := 30;
  LeftOffset := 0;
  FRightOffset := 0;
  AutoSize := True;

  if not(csLoading in AOwner.ComponentState) then begin
    TSynGutterMarks.Create(FGutterPartList);
    TSynGutterLineNumber.Create(FGutterPartList);
    TSynGutterChanges.Create(FGutterPartList);
    TSynGutterSeparator.Create(FGutterPartList);
    TSynGutterCodeFolding.Create(FGutterPartList);
  end;
end;

destructor TSynGutter.Destroy;
begin
  FOnChange := nil;
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
    FGutterPartList.Assign(Src.FGutterPartList);

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

  for i := GutterPartCount-1 downto 0 do
    Result := Result + GutterPart[i].RealGutterWidth(CharWidth);
end;

function TSynGutter.GetPartGutter(Index : Integer) : TSynGutterPartBase;
begin
  Result := TSynGutterPartBase(FGutterPartList[Index]);
end;

function TSynGutter.GetGutterPartCount: integer;
begin
  if FGutterPartList <> nil then
    result := FGutterPartList.Count
  else
    Result := 0;
end;

function TSynGutter.GetGutterPartCountByClass(AClass: TSynGutterPartBaseClass): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to GutterPartCount -1 do
    if GutterPart[i] is AClass then
      inc(Result);
end;

function TSynGutter.GetGutterPartVisibleByClass(AClass: TSynGutterPartBaseClass): Boolean;
var
  i: Integer;
begin
  for i := 0 to GutterPartCount -1 do
    if (GutterPart[i] is AClass) and (GutterPart[i].Visible) then
      exit(True);
  Result := False;
end;

function TSynGutter.GetPartGutterByClass(AClass: TSynGutterPartBaseClass;
  Index: Integer): TSynGutterPartBase;
var
  i: Integer;
begin
  for i := 0 to GutterPartCount -1 do
    if GutterPart[i] is AClass then begin
      if Index = 0 then
        exit(GutterPart[i]);
      dec(Index);
    end;
  Result := nil;
end;

procedure TSynGutter.SetGutterParts(const AValue: TSynGutterPartList);
begin
  FGutterPartList.Assign(AValue);
end;

procedure TSynGutter.SetGutterPartVisibleByClass(AClass: TSynGutterPartBaseClass; const AValue: Boolean);
var
  i: Integer;
begin
  for i := 0 to GutterPartCount -1 do
    if (GutterPart[i] is AClass) then
      GutterPart[i].Visible := AValue;
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

procedure TSynGutter.Clear;
var
  i: Integer;
begin
  if FGutterPartList = nil then exit;
  for i := FGutterPartList.Count-1 downto 0 do
    GutterPart[i].Free;
  FGutterPartList.Clear;
end;

procedure TSynGutter.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetColor(const Value: TColor);
begin
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
  if FAutoSize then
    Value := RealGutterWidth(FTextDrawer.CharWidth);
  Value := Max(0, Value);
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.DoChange(Sender: TObject);
begin
  If FAutoSize then
    FWidth := RealGutterWidth(FTextDrawer.CharWidth);
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

procedure TSynGutter.RegisterNewGutterPartList(APartList: TSynGutterPartList);
begin
  if (APartList = nil) or (APartList = FGutterPartList) then
    exit;
  if FGutterPartList <> nil then
    FreeAndNil(FGutterPartList);
  FGutterPartList := APartList;
  FGutterPartList.OnChange := {$IFDEF FPC}@{$ENDIF}DoChange;
end;

procedure TSynGutter.DoOnGutterClick(X, Y: integer);
var
  i, x2 : integer;
begin
  i := 0;
  x2 := x;
  while i < GutterPartCount-1 do begin
    if GutterPart[i].Visible then begin
      if x2 >= GutterPart[i].Width then
        x2 := x2 - GutterPart[i].Width
      else
        break;
    end;
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

  // Clear all
  fTextDrawer.BeginDrawing(dc);
  fTextDrawer.SetBackColor(Color);
  fTextDrawer.SetForeColor(TSynEdit(FEdit).Font.Color);
  fTextDrawer.SetFrameColor(clNone);
   with AClip do
     fTextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, AClip, nil, 0);
  fTextDrawer.EndDrawing;

  AClip.Left := FLeftOffset;
  rcLine := AClip;
  rcLine.Right := rcLine.Left;
  for i := 0 to GutterPartCount -1 do
  begin
    if rcLine.Right >= AClip.Right then break;
    if GutterPart[i].Visible then
    begin
      rcLine.Left := rcLine.Right;
      rcLine.Right := min(rcLine.Left + GutterPart[i].Width, AClip.Right);
      GutterPart[i].Paint(Canvas, rcLine, FirstLine, LastLine);
    end;
  end;
end;

procedure TSynGutter.AutoSizeDigitCount(LinesCount : integer);
var
  i: Integer;
begin
  for i := 0 to GutterPartCountByClass[TSynGutterLineNumber] - 1 do
    TSynGutterLineNumber(GutterPartByClass[TSynGutterLineNumber, i]).AutoSizeDigitCount(LinesCount);
end;

{ TSynGutterPartBase }

function TSynGutterPartBase.GetGutterParts: TSynGutterPartList;
begin
  Result := TSynGutterPartList(Owner);
end;

procedure TSynGutterPartBase.SetMarkupInfo(const AValue: TSynSelectedColor);
begin
  FMarkupInfo.Assign(AValue);
end;

procedure TSynGutterPartBase.SetRealWidth(const AValue: Integer);
begin
  if FWidth = AValue then exit;
  FWidth :=  AValue;
  DoChange(self);
end;

procedure TSynGutterPartBase.SetAutoSize(const AValue : boolean);
begin
  if FAutoSize=AValue then exit;
  FAutoSize:=AValue;
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
  if (FWidth=AValue) or (FAutoSize) then exit;
  FWidth:=AValue;
  DoChange(self);
end;

procedure TSynGutterPartBase.DoChange(Sender : TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSynGutterPartBase.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FGutter := GutterParts.Gutter;
  FSynEdit := GutterParts.SynEdit;

  FMarkupInfo := TSynSelectedColor.Create;
  FMarkupInfo.Background := clBtnFace;
  FMarkupInfo.Foreground := clNone;
  FMarkupInfo.FrameColor := clNone;
  FMarkupInfo.OnChange := {$IFDEF FPC}@{$ENDIF}DoChange;

  FVisible := True;
  FAutoSize := True;
  FWidth := 10;
end;

destructor TSynGutterPartBase.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FMarkupInfo);
end;

procedure TSynGutterPartBase.Assign(Source : TPersistent);
var
  Src: TSynGutterPartBase;
begin
  if Assigned(Source) and (Source is TSynGutterPartBase) then
  begin
    Src := TSynGutterPartBase(Source);
    FVisible := Src.FVisible;
    FWidth := Src.FWidth;
    FAutoSize := Src.FAutoSize;
    MarkupInfo.Assign(Src.MarkupInfo);
    DoChange(Self);
  end else
    inherited;
end;

procedure TSynGutterPartBase.DoOnGutterClick(X, Y : integer);
begin
  if Assigned(FOnGutterClick) then
    FOnGutterClick(Self, X, Y, 0, nil);
end;

{ TSynGutterSeparator }

constructor TSynGutterSeparator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 2;
end;

procedure TSynGutterSeparator.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
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

{ TSynGutterPartList }

constructor TSynGutterPartList.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  if assigned(TSynEdit(SynEdit).Gutter) then
    FGutter := TSynEdit(SynEdit).Gutter;
  Gutter.RegisterNewGutterPartList(self);
end;

constructor TSynGutterPartList.Create(AOwner: TComponent; AGutter: TSynGutter);
begin
  FGutter := AGutter;
  Create(AOwner);
end;

destructor TSynGutterPartList.Destroy;
begin
  FGutter.FGutterPartList := nil;
  OnChange := nil;
  inherited Destroy;
end;

procedure TSynGutterPartList.RegisterItem(AnItem: TSynObjectListItem);
begin
  TSynGutterPartBase(AnItem).OnChange := {$IFDEF FPC}@{$ENDIF}DoChange;
  TSynGutterPartBase(AnItem).OnGutterClick := {$IFDEF FPC}@{$ENDIF}Gutter.DoDefaultGutterClick;
  inherited RegisterItem(AnItem);
end;

function TSynGutterPartList.GetSynEdit: TSynEditBase;
begin
  Result := TSynEditBase(Owner);
end;

function TSynGutterPartList.GetPart(Index: Integer): TSynGutterPartBase;
begin
  Result := TSynGutterPartBase(BaseItems[Index]);
end;

procedure TSynGutterPartList.PutPart(Index: Integer; const AValue: TSynGutterPartBase);
begin
  BaseItems[Index] := TSynObjectListItem(AValue);
end;

end.

