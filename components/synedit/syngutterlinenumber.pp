unit SynGutterLineNumber;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, SynGutterBase,
  SynEditMiscProcs, SynTextDrawer, SynEditMouseCmds,
  LazSynEditText, SynEditTypes;

type

  TSynEditMouseActionsLineNum = class(TSynEditMouseInternalActions)
  public  // empty by default
  end;

  { TSynGutterLineNumber }

  TSynGutterLineNumber = class(TSynGutterPartBase)
  private
    FTextDrawer: TheTextDrawer;

    FDigitCount: integer;
    FAutoSizeDigitCount: integer;
    FShowOnlyLineNumbersMultiplesOf: integer;
    FLeadingZeros: boolean;
    FZeroStart: boolean;

    procedure SetDigitCount(AValue : integer);
    procedure SetLeadingZeros(const AValue : boolean);
    procedure SetShowOnlyLineNumbersMultiplesOf(const AValue : integer);
    procedure SetZeroStart(const AValue : boolean);
    function FormatLineNumber(Line: integer; IsDot: boolean): string;
  protected
    procedure Init; override;
    function  PreferedWidthAtCurrentPPI: Integer; override;
    procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
    procedure BufferChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure SetAutoSize(const AValue : boolean); override;
    procedure SetVisible(const AValue: boolean); override;
    function CreateMouseActions: TSynEditMouseInternalActions; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      override;
  published
    property MarkupInfo;
    property DigitCount: integer read FDigitCount write SetDigitCount;
    property ShowOnlyLineNumbersMultiplesOf: integer
             read FShowOnlyLineNumbersMultiplesOf write SetShowOnlyLineNumbersMultiplesOf;
    property ZeroStart: boolean read FZeroStart write SetZeroStart;
    property LeadingZeros: boolean read FLeadingZeros write SetLeadingZeros;
  end;

implementation
uses
  SynEdit;

{ TSynGutterLineNumber }

constructor TSynGutterLineNumber.Create(AOwner: TComponent);
begin
  FDigitCount := 2;
  FAutoSizeDigitCount := FDigitCount;
  FShowOnlyLineNumbersMultiplesOf := 1;
  FLeadingZeros := false;
  FZeroStart := False;
  inherited Create(AOwner);
end;

procedure TSynGutterLineNumber.Init;
begin
  inherited Init;
  FTextDrawer := Gutter.TextDrawer;
  ViewedTextBuffer.AddChangeHandler(senrLineCount, @LineCountChanged);
  ViewedTextBuffer.AddNotifyHandler(senrTextBufferChanged, @BufferChanged);
  FTextDrawer.RegisterOnFontChangeHandler(@FontChanged);
  LineCountchanged(nil, 0, 0);
end;

destructor TSynGutterLineNumber.Destroy;
begin
  ViewedTextBuffer.RemoveHanlders(self);
  FTextDrawer.UnRegisterOnFontChangeHandler(@FontChanged);
  inherited Destroy;
end;

procedure TSynGutterLineNumber.Assign(Source : TPersistent);
var
  Src: TSynGutterLineNumber;
begin
  if Source is TSynGutterLineNumber then
  begin
    Src := TSynGutterLineNumber(Source);
    FLeadingZeros := Src.FLeadingZeros;
    FZeroStart := Src.FZeroStart;
    FDigitCount := Src.FDigitCount;
    FAutoSizeDigitCount := Src.FAutoSizeDigitCount;
    FShowOnlyLineNumbersMultiplesOf := Src.FShowOnlyLineNumbersMultiplesOf;
  end;
  inherited;
end;

procedure TSynGutterLineNumber.SetDigitCount(AValue : integer);
begin
  AValue := MinMax(AValue, 2, 12);
  if FDigitCount <> AValue then
  begin
    FDigitCount := AValue;
    if AutoSize then begin
      FAutoSizeDigitCount := Max(FDigitCount, FAutoSizeDigitCount);
      DoAutoSize;
    end else
      FAutoSizeDigitCount := FDigitCount;
    DoChange(Self);
  end;
end;

procedure TSynGutterLineNumber.SetLeadingZeros(const AValue : boolean);
begin
  if FLeadingZeros <> AValue then
  begin
    FLeadingZeros := AValue;
    DoChange(Self);
  end;
end;

procedure TSynGutterLineNumber.SetShowOnlyLineNumbersMultiplesOf(const AValue : integer);
begin
  If FShowOnlyLineNumbersMultiplesOf <> AValue then
  begin
    FShowOnlyLineNumbersMultiplesOf := AValue;
    DoChange(self);
  end;
end;

procedure TSynGutterLineNumber.SetZeroStart(const AValue : boolean);
begin
  if FZeroStart <> AValue then
  begin
    FZeroStart := AValue;
    if AutoSize then
      DoAutoSize;
    DoChange(Self);
  end;
end;

function TSynGutterLineNumber.FormatLineNumber(Line: integer; IsDot: boolean): string;
var
  i: integer;
begin
  Result := '';
  // if a dot must be showed
  if IsDot then
    if Line mod 5 = 0 then // every 5 lines show '-' instead of '.'
      Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '-'
    else
      Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '.'
  // else format the line number
  else begin
    if FZeroStart then Dec(Line);
    Str(Line : FAutoSizeDigitCount, Result);
    if FLeadingZeros then
      for i := 1 to FAutoSizeDigitCount - 1 do begin
        if (Result[i] <> ' ') then break;
        Result[i] := '0';
      end;
  end;
end;

function TSynGutterLineNumber.PreferedWidthAtCurrentPPI: Integer;
begin
  Result := FAutoSizeDigitCount * FTextDrawer.CharWidth + 1;
end;

procedure TSynGutterLineNumber.LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
var
  LineCnt, nDigits: integer;
begin
  if not(Visible and AutoSize) then exit;

  LineCnt := TextBuffer.Count;
  if FZeroStart and (LineCnt > 0) then Dec(LineCnt);

  nDigits := Max(Length(IntToStr(LineCnt)), FDigitCount);
  if FAutoSizeDigitCount <> nDigits then begin
    FAutoSizeDigitCount := nDigits;
    DoAutoSize;
  end;
end;

procedure TSynGutterLineNumber.BufferChanged(Sender: TObject);
begin
  LineCountChanged(nil, 0, 0);
end;

procedure TSynGutterLineNumber.FontChanged(Sender: TObject);
begin
  DoAutoSize;
end;

procedure TSynGutterLineNumber.SetAutoSize(const AValue: boolean);
begin
  inherited SetAutoSize(AValue);
  if AValue then LineCountChanged(nil, 0, 0);
end;

procedure TSynGutterLineNumber.SetVisible(const AValue: boolean);
begin
  inherited SetVisible(AValue);
  if AValue then LineCountChanged(nil, 0, 0);
end;

function TSynGutterLineNumber.CreateMouseActions: TSynEditMouseInternalActions;
begin
  Result := TSynEditMouseActionsLineNum.Create(self);
end;

procedure TSynGutterLineNumber.Paint(Canvas : TCanvas; AClip : TRect; FirstLine, LastLine : integer);
var
  i, c, iLine: integer;
  rcLine: TRect;
  s: string;
  dc: HDC;
  ShowDot: boolean;
  LineHeight: Integer;
  IRange: TLineRange;
  t: TLinePos;

begin
  if not Visible then exit;

  LineHeight := TCustomSynEdit(SynEdit).LineHeight;
  c := TCustomSynEdit(SynEdit).Lines.Count;
  t := ToIdx(GutterArea.TextArea.TopLine);
  // Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of fTextDrawer)!
  if MarkupInfo.Background <> clNone then
    Canvas.Brush.Color := MarkupInfo.Background
  else
    Canvas.Brush.Color := Gutter.Color;
  dc := Canvas.Handle;
  LCLIntf.SetBkColor(dc, TColorRef(Canvas.Brush.Color));
  FTextDrawer.BeginDrawing(dc);
  try
    if MarkupInfo.Background <> clNone then
      FTextDrawer.SetBackColor(MarkupInfo.Background)
    else
      FTextDrawer.SetBackColor(Gutter.Color);
    if MarkupInfo.Foreground <> clNone then
      fTextDrawer.SetForeColor(MarkupInfo.Foreground)
    else
      fTextDrawer.SetForeColor(TCustomSynEdit(SynEdit).Font.Color);
    fTextDrawer.SetFrameColor(MarkupInfo.FrameColor);
    fTextDrawer.Style := MarkupInfo.Style;
    // prepare the rect initially
    rcLine := AClip;
    rcLine.Bottom := AClip.Top;
    for i := t + FirstLine to t + LastLine do
    begin
      iLine := ToPos(ViewedTextBuffer.DisplayView.ViewToTextIndexEx(i, IRange));
      if (iLine < 1) or (iLine > c) then break;
      // next line rect
      rcLine.Top := rcLine.Bottom;
      // Must show a dot instead of line number if
      // line number is not the first, the last, the current line
      // or a multiple of ShowOnlyLineNumbersMultiplesOf
      ShowDot := ((iLine mod ShowOnlyLineNumbersMultiplesOf) <> 0)
          and (iLine <> TCustomSynEdit(SynEdit).CaretY) and (iLine <> 1)
          and (iLine <> SynEdit.Lines.Count);
      // Get the formatted line number or dot
      s := FormatLineNumber(iLine, ShowDot);
      Inc(rcLine.Bottom, LineHeight);
      if i <> IRange.Top then
        s := '';
      // erase the background and draw the line number string in one go
      fTextDrawer.ExtTextOut(rcLine.Left, rcLine.Top, ETO_OPAQUE or ETO_CLIPPED, rcLine,
        PChar(Pointer(S)),Length(S));
    end;

    // now erase the remaining area if any
    if AClip.Bottom > rcLine.Bottom then
    begin
      rcLine.Top := rcLine.Bottom;
      rcLine.Bottom := AClip.Bottom;
      with rcLine do
        fTextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, rcLine, nil, 0);
    end;
  finally
    fTextDrawer.EndDrawing;
  end;
end;

end.

