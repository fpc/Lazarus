unit SynGutterLineNumber;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, SynGutterBase,
  SynEditMiscProcs, SynTextDrawer, SynEditFoldedView, SynEditMouseCmds;

type

  TSynEditMouseActionsLineNum = class(TSynEditMouseActions)
  public  // empty by default
  end;

  { TSynGutterLineNumber }

  TSynGutterLineNumber = class(TSynGutterPartBase)
  private
    FFoldView: TSynEditFoldedView;
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
    procedure DoChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      override;
    procedure AutoSizeDigitCount(LinesCount: integer);
    function RealGutterWidth(CharWidth: integer): integer;  override;
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
  inherited Create(AOwner);
  FFoldView := Gutter.FoldView;
  FTextDrawer := Gutter.TextDrawer;
  FMouseActions := TSynEditMouseActionsLineNum.Create(self);
  FMouseActions.ResetDefaults;

  FDigitCount := 2;
  FAutoSizeDigitCount := FDigitCount;
  FShowOnlyLineNumbersMultiplesOf := 1;
  FLeadingZeros := false;
  FZeroStart := False;
  FWidth := 25;
end;

destructor TSynGutterLineNumber.Destroy;
begin
  FreeAndNil(FMouseActions);
  inherited Destroy;
end;

procedure TSynGutterLineNumber.Assign(Source : TPersistent);
var
  Src: TSynGutterLineNumber;
begin
  if Assigned(Source) and (Source is TSynGutterLineNumber) then
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
    DoChange(Self);
  end;
end;

function TSynGutterLineNumber.RealGutterWidth(CharWidth : integer) : integer;
begin
  if not Visible then
  begin
    Result := 0;
    Exit;
  end;

  if AutoSize then
    RealWidth := FAutoSizeDigitCount * CharWidth + 1;
  Result := Width;
end;

procedure TSynGutterLineNumber.AutoSizeDigitCount(LinesCount: integer);
var
  nDigits: integer;
begin
  if Visible and AutoSize then
  begin
    if FZeroStart then Dec(LinesCount);
    nDigits := Max(Length(IntToStr(LinesCount)), FDigitCount);
    if FAutoSizeDigitCount <> nDigits then
    begin
      FAutoSizeDigitCount := nDigits;
      DoChange(Self);
    end;
  end
  else
  if FAutoSizeDigitCount <> FDigitCount then begin
    FAutoSizeDigitCount := FDigitCount;
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

procedure TSynGutterLineNumber.DoChange(Sender: TObject);
begin
  if AutoSize then
    FWidth := RealGutterWidth(FTextDrawer.CharWidth);
  inherited DoChange(Sender);
end;

procedure TSynGutterLineNumber.Paint(Canvas : TCanvas; AClip : TRect; FirstLine, LastLine : integer);
var
  i, iLine: integer;
  rcLine: TRect;
  s: string;
  dc: HDC;
  ShowDot: boolean;
  LineHeight: Integer;

begin
  if not Visible then exit;

  LineHeight := TSynEdit(SynEdit).LineHeight;
  // Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of fTextDrawer)!
  if MarkupInfo.Background <> clNone then
    Canvas.Brush.Color := MarkupInfo.Background
  else
    Canvas.Brush.Color := Gutter.Color;
  dc := Canvas.Handle;
  {$IFDEF SYN_LAZARUS}
  LCLIntf.SetBkColor(dc, Canvas.Brush.Color);
  {$ENDIF}
  FTextDrawer.BeginDrawing(dc);
  try
    if MarkupInfo.Background <> clNone then
      FTextDrawer.SetBackColor(MarkupInfo.Background)
    else
      FTextDrawer.SetBackColor(Gutter.Color);
    if MarkupInfo.Foreground <> clNone then
      fTextDrawer.SetForeColor(MarkupInfo.Foreground)
    else
      fTextDrawer.SetForeColor(TSynEdit(SynEdit).Font.Color);
    fTextDrawer.SetFrameColor(MarkupInfo.FrameColor);
    fTextDrawer.Style := MarkupInfo.Style;
    // prepare the rect initially
    rcLine := AClip;
    rcLine.Bottom := FirstLine * LineHeight;
    for i := FirstLine to LastLine do
    begin
      iLine := FFoldView.DisplayNumber[i];
      // next line rect
      rcLine.Top := rcLine.Bottom;
      // Must show a dot instead of line number if
      // line number is not the first, the last, the current line
      // or a multiple of ShowOnlyLineNumbersMultiplesOf
      ShowDot := ((iLine mod ShowOnlyLineNumbersMultiplesOf) <> 0)
          and (iLine <> TSynEdit(SynEdit).CaretY) and (iLine <> 1)
          and (iLine <> SynEdit.Lines.Count);
      // Get the formatted line number or dot
      s := FormatLineNumber(iLine, ShowDot);
      Inc(rcLine.Bottom, LineHeight);
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

