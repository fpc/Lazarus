unit SynGutter;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, LCLIntf, LCLProc, LCLClasses,
  SynEditMarks, SynEditMiscClasses, SynEditMiscProcs, SynEditFoldedView,
  SynTextDrawer, SynGutterBase, SynGutterLineNumber, SynGutterCodeFolding,
  SynGutterMarks, SynGutterChanges;

type

  TSynGutterSeparator = class;

  { TSynGutter }

  TSynGutter = class(TSynGutterBase)
  private
    // List of all gutters
    FEdit: TSynEditBase;

    FWidth: integer;
    FRightOffset, FLeftOffset: integer;
    FOnChange: TNotifyEvent;
    FCursor: TCursor;
    FVisible: boolean;
    FAutoSize: boolean;
    FOnGutterClick: TGutterClickEvent;
    procedure SetAutoSize(const Value: boolean);
    procedure SetLeftOffset(Value: integer);
    procedure SetRightOffset(Value: integer);
    procedure SetVisible(Value: boolean);
    procedure SetWidth(Value: integer);
  protected
    procedure DoChange(Sender: TObject); override;
    procedure DoDefaultGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark); override;
  public
    property SynEdit: TSynEditBase read FEdit;
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
    // Some Methods for convinience access to common GutterParts
    procedure AutoSizeDigitCount(LinesCount: integer); // Forward to Line Number
  public
    // Access to well knonw parts
    Function LineNumberPart(Index: Integer = 0): TSynGutterLineNumber;
    Function CodeFoldPart(Index: Integer = 0): TSynGutterCodeFolding;
    Function ChangesPart(Index: Integer = 0): TSynGutterChanges;
    Function MarksPart(Index: Integer = 0): TSynGutterMarks;
    Function SeparatorPart(Index: Integer = 0): TSynGutterSeparator;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default True;
    property Color;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property LeftOffset: integer read FLeftOffset write SetLeftOffset
      default 0;
    property RightOffset: integer read FRightOffset write SetRightOffset
      default 0;
    property Visible: boolean read FVisible write SetVisible default TRUE;
    property Width: integer read FWidth write SetWidth default 30;
    property GutterParts;
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
  SynEdit;

{ TSynGutter }

constructor TSynGutter.Create(AOwner: TSynEditBase;
  AFoldedLinesView: TSynEditFoldedView; ATextDrawer: TheTextDrawer);
begin
  FEdit := TSynEdit(AOwner);
  inherited Create(AOwner, AFoldedLinesView, ATextDrawer);

  Visible := True;
  Width := 30;
  LeftOffset := 0;
  FRightOffset := 0;
  AutoSize := True;

  if not(csLoading in AOwner.ComponentState) then begin
    TSynGutterMarks.Create(GutterParts);
    TSynGutterLineNumber.Create(GutterParts);
    TSynGutterChanges.Create(GutterParts);
    TSynGutterSeparator.Create(GutterParts);
    TSynGutterCodeFolding.Create(GutterParts);
  end;
end;

destructor TSynGutter.Destroy;
begin
  FOnChange := nil;
  inherited Destroy;
end;

procedure TSynGutter.Assign(Source: TPersistent);
var
  Src: TSynGutter;
begin
  inherited;
  if Assigned(Source) and (Source is TSynGutter) then
  begin
    Src := TSynGutter(Source);
    FVisible := Src.FVisible;
    FWidth := Src.FWidth;
    FRightOffset := Src.FRightOffset;
    FAutoSize := Src.FAutoSize;

    DoChange(Self);
  end else
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

procedure TSynGutter.SetLeftOffset(Value: integer);
begin
  Value := Max(0, Value);
  if FLeftOffset <> Value then
  begin
    FLeftOffset := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutter.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
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
    Value := RealGutterWidth(TextDrawer.CharWidth);
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
    FWidth := RealGutterWidth(TextDrawer.CharWidth);
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
  TextDrawer.BeginDrawing(dc);
  TextDrawer.SetBackColor(Color);
  TextDrawer.SetForeColor(TSynEdit(FEdit).Font.Color);
  TextDrawer.SetFrameColor(clNone);
   with AClip do
     TextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, AClip, nil, 0);
  TextDrawer.EndDrawing;

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

function TSynGutter.LineNumberPart(Index: Integer = 0): TSynGutterLineNumber;
begin
  Result := TSynGutterLineNumber(GutterPartByClass[TSynGutterLineNumber, Index]);
end;

function TSynGutter.CodeFoldPart(Index: Integer = 0): TSynGutterCodeFolding;
begin
  Result := TSynGutterCodeFolding(GutterPartByClass[TSynGutterCodeFolding, Index]);
end;

function TSynGutter.ChangesPart(Index: Integer = 0): TSynGutterChanges;
begin
  Result := TSynGutterChanges(GutterPartByClass[TSynGutterChanges, Index]);
end;

function TSynGutter.MarksPart(Index: Integer = 0): TSynGutterMarks;
begin
  Result := TSynGutterMarks(GutterPartByClass[TSynGutterMarks, Index]);
end;

function TSynGutter.SeparatorPart(Index: Integer = 0): TSynGutterSeparator;
begin
  Result := TSynGutterSeparator(GutterPartByClass[TSynGutterSeparator, Index]);
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

end.

