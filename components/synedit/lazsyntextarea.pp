unit LazSynTextArea;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLType, LCLIntf, LCLProc,
  SynEditTypes, SynEditMiscProcs, SynEditMiscClasses, LazSynEditText,
  SynEditMarkup, SynEditHighlighter, SynTextDrawer;


type

  { TLazSynTextArea }

  TLazSynTextArea = class(TLazSynSurface)
  private
    FCharsInWindow: Integer;
    FCharWidth: integer;
    FLinesInWindow: Integer;
    FTextHeight: integer;

    FCanvas: TCanvas;
    FTextDrawer: TheTextDrawer;
    FTheLinesView: TSynEditStrings;
    FHighlighter: TSynCustomHighlighter;
    FMarkupManager: TSynEditMarkupManager;
    FPaintLineColor, FPaintLineColor2: TSynSelectedColor;
    FForegroundColor: TColor;
    FBackgroundColor: TColor;
    FRightEdgeColor: TColor;

    FTextBounds: TRect;
    FPadding: array [TLazSynBorderSide] of Integer;
    FExtraCharSpacing: integer;
    FExtraLineSpacing: integer;
    FVisibleSpecialChars: TSynVisibleSpecialChars;
    FRightEdgeColumn: integer;
    FRightEdgeVisible: boolean;

    FTopLine: TLinePos;
    FLeftChar: Integer;

    function GetPadding(Side: TLazSynBorderSide): integer;
    procedure SetExtraCharSpacing(AValue: integer);
    procedure SetExtraLineSpacing(AValue: integer);
    procedure SetLeftChar(AValue: Integer);
    procedure SetPadding(Side: TLazSynBorderSide; AValue: integer);
    procedure SetTopLine(AValue: TLinePos);
    procedure DoDrawerFontChanged(Sender: TObject);
  protected
    procedure BoundsChanged; override;
    procedure DoPaint(ACanvas: TCanvas; AClip: TRect); override;
    procedure PaintTextLines(AClip: TRect; FirstLine, LastLine,
      FirstCol, LastCol: integer); virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TWinControl; ATextDrawer: TheTextDrawer);
    destructor Destroy; override;
    procedure Assign(Src: TLazSynSurface); override;
    procedure InvalidateLines(FirstTextLine, LastTextLine: TLineIdx); override;

    function ScreenColumnToXValue(Col: integer): integer;  // map screen column to screen pixel
    function RowColumnToPixels(const RowCol: TPoint): TPoint;
    function PixelsToRowColumn(Pixels: TPoint; aFlags: TSynCoordinateMappingFlags): TPoint; // ignores scmLimitToLines

    procedure FontChanged;

    // Settings controlled by SynEdit
    property Padding[Side: TLazSynBorderSide]: integer read GetPadding write SetPadding;
    property ForegroundColor: TColor read FForegroundColor write FForegroundColor;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property ExtraCharSpacing: integer read FExtraCharSpacing write SetExtraCharSpacing;
    property ExtraLineSpacing: integer read FExtraLineSpacing write SetExtraLineSpacing;
    property VisibleSpecialChars: TSynVisibleSpecialChars read FVisibleSpecialChars write FVisibleSpecialChars;
    property RightEdgeColumn: integer  read FRightEdgeColumn write FRightEdgeColumn;
    property RightEdgeVisible: boolean read FRightEdgeVisible write FRightEdgeVisible;
    property RightEdgeColor: TColor    read FRightEdgeColor write FRightEdgeColor;

    property TopLine: TLinePos read FTopLine write SetTopLine; // TopView
    property LeftChar: Integer read FLeftChar write SetLeftChar;

    property TheLinesView:  TSynEditStrings       read FTheLinesView  write FTheLinesView;
    property Highlighter:   TSynCustomHighlighter read FHighlighter   write FHighlighter;
    property MarkupManager: TSynEditMarkupManager read FMarkupManager write FMarkupManager;
    property TextDrawer: TheTextDrawer read FTextDrawer;
  public
    property TextBounds: TRect read FTextBounds;

    property LineHeight: integer read FTextHeight;
    property CharWidth: integer  read FCharWidth;
    property LinesInWindow: Integer read FLinesInWindow;
    property CharsInWindow: Integer read FCharsInWindow;
  end;

  { TLazSynSurfaceManager }

  TLazSynSurfaceManager = class(TLazSynSurface)
  private
    FLeftGutterArea: TLazSynSurface;
    FLeftGutterWidth: integer;
    FRightGutterArea: TLazSynSurface;
    FRightGutterWidth: integer;
    FTextArea: TLazSynTextArea;
    procedure SetLeftGutterArea(AValue: TLazSynSurface);
    procedure SetLeftGutterWidth(AValue: integer);
    procedure SetRightGutterArea(AValue: TLazSynSurface);
    procedure SetRightGutterWidth(AValue: integer);
    procedure SetTextArea(AValue: TLazSynTextArea);
  protected
    procedure DoPaint(ACanvas: TCanvas; AClip: TRect); override;
    procedure DoDisplayViewChanged; override;
    procedure BoundsChanged; override;
  public
    constructor Create(AOwner: TWinControl);
    procedure InvalidateLines(FirstTextLine, LastTextLine: TLineIdx); override;
    procedure InvalidateTextLines(FirstTextLine, LastTextLine: TLineIdx); virtual;
    procedure InvalidateGutterLines(FirstTextLine, LastTextLine: TLineIdx); virtual;
    property TextArea: TLazSynTextArea read FTextArea write SetTextArea;
    property LeftGutterArea: TLazSynSurface read FLeftGutterArea write SetLeftGutterArea;
    property RightGutterArea: TLazSynSurface read FRightGutterArea write SetRightGutterArea;
    property LeftGutterWidth: integer read FLeftGutterWidth write SetLeftGutterWidth;
    property RightGutterWidth: integer read FRightGutterWidth write SetRightGutterWidth;
  end;


implementation

{ TLazSynSurfaceManager }

procedure TLazSynSurfaceManager.SetLeftGutterWidth(AValue: integer);
begin
  if FLeftGutterWidth = AValue then Exit;
  FLeftGutterWidth := AValue;
  BoundsChanged;
end;

procedure TLazSynSurfaceManager.SetLeftGutterArea(AValue: TLazSynSurface);
begin
  if FLeftGutterArea = AValue then Exit;
  FLeftGutterArea := AValue;
  FLeftGutterArea.DisplayView := DisplayView;
end;

procedure TLazSynSurfaceManager.SetRightGutterArea(AValue: TLazSynSurface);
begin
  if FRightGutterArea = AValue then Exit;
  FRightGutterArea := AValue;
  FRightGutterArea.DisplayView := DisplayView;
end;

procedure TLazSynSurfaceManager.SetRightGutterWidth(AValue: integer);
begin
  if FRightGutterWidth = AValue then Exit;
  FRightGutterWidth := AValue;
  BoundsChanged;
end;

procedure TLazSynSurfaceManager.SetTextArea(AValue: TLazSynTextArea);
begin
  if FTextArea = AValue then Exit;
  FTextArea := AValue;
  FTextArea.DisplayView := DisplayView;
end;

procedure TLazSynSurfaceManager.DoPaint(ACanvas: TCanvas; AClip: TRect);
begin
  FLeftGutterArea.Paint(ACanvas, AClip);
  FTextArea.Paint(ACanvas, AClip);
  FRightGutterArea.Paint(ACanvas, AClip);
end;

procedure TLazSynSurfaceManager.DoDisplayViewChanged;
begin
  FLeftGutterArea.DisplayView  := DisplayView;
  FRightGutterArea.DisplayView := DisplayView;
  FTextArea.DisplayView        := DisplayView;
end;

procedure TLazSynSurfaceManager.BoundsChanged;
var
  l, r: Integer;
begin
  r := Max(Left, Right - RightGutterWidth);
  l := Min(r, Left + LeftGutterWidth);
  FLeftGutterArea.SetBounds(Top, Left, Bottom, l);
  FTextArea.SetBounds(Top, l, Bottom, r);
  FRightGutterArea.SetBounds(Top, r, Bottom, Right);
end;

constructor TLazSynSurfaceManager.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);
  FLeftGutterWidth := 0;
  FRightGutterWidth := 0;
end;

procedure TLazSynSurfaceManager.InvalidateLines(FirstTextLine, LastTextLine: TLineIdx);
var
  rcInval: TRect;
begin
  rcInval := Bounds;
  if (FirstTextLine >= 0) then
    rcInval.Top := Max(TextArea.TextBounds.Top,
                       TextArea.TextBounds.Top
                       + (DisplayView.TextToViewIndex(FirstTextLine).Top
                          - TextArea.TopLine + 1) * TextArea.LineHeight);
  if (LastTextLine >= 0) then
    rcInval.Bottom := Min(TextArea.TextBounds.Bottom,
                          TextArea.TextBounds.Top
                          + (DisplayView.TextToViewIndex(LastTextLine).Bottom
                             - TextArea.TopLine + 2)  * TextArea.LineHeight);

  {$IFDEF VerboseSynEditInvalidate}
  DebugLn(['TCustomSynEdit.InvalidateGutterLines ',DbgSName(self), ' FirstLine=',FirstTextLine, ' LastLine=',LastTextLine, ' rect=',dbgs(rcInval)]);
  {$ENDIF}
  if (rcInval.Top < rcInval.Bottom) and (rcInval.Left < rcInval.Right) then
    InvalidateRect(Handle, @rcInval, FALSE);
end;

procedure TLazSynSurfaceManager.InvalidateTextLines(FirstTextLine, LastTextLine: TLineIdx);
begin
  FTextArea.InvalidateLines(FirstTextLine, LastTextLine);
end;

procedure TLazSynSurfaceManager.InvalidateGutterLines(FirstTextLine, LastTextLine: TLineIdx);
begin
  FLeftGutterArea.InvalidateLines(FirstTextLine, LastTextLine);
  FRightGutterArea.InvalidateLines(FirstTextLine, LastTextLine);
end;

{ TLazSynTextArea }

function TLazSynTextArea.GetPadding(Side: TLazSynBorderSide): integer;
begin
  Result := FPadding[Side];
end;

procedure TLazSynTextArea.SetExtraCharSpacing(AValue: integer);
begin
  if FExtraCharSpacing = AValue then Exit;
  FExtraCharSpacing := AValue;
  FontChanged;
end;

procedure TLazSynTextArea.SetExtraLineSpacing(AValue: integer);
begin
  if FExtraLineSpacing = AValue then Exit;
  FExtraLineSpacing := AValue;
  FTextHeight := FTextDrawer.CharHeight + FExtraLineSpacing;
  FontChanged;
end;

procedure TLazSynTextArea.SetLeftChar(AValue: Integer);
begin
  if FLeftChar = AValue then Exit;
  FLeftChar := AValue;
end;

procedure TLazSynTextArea.SetPadding(Side: TLazSynBorderSide; AValue: integer);
begin
  FPadding[Side] := AValue;
  case Side of
    bsLeft:   FTextBounds.Left   := Left + FPadding[bsLeft];
    bsTop:    FTextBounds.Top    := Top + FPadding[bsTop];
    bsRight:  FTextBounds.Right  := Right - FPadding[bsRight];
    bsBottom: FTextBounds.Bottom := Bottom - FPadding[bsBottom];
  end;
  FontChanged;
end;

procedure TLazSynTextArea.SetTopLine(AValue: TLinePos);
begin
  if AValue < 1 then AValue := 1;
  if FTopLine = AValue then Exit;
  FTopLine := AValue;
end;

procedure TLazSynTextArea.DoDrawerFontChanged(Sender: TObject);
begin
  FontChanged;
end;

procedure TLazSynTextArea.BoundsChanged;
begin
  FTextBounds.Left   := Left + FPadding[bsLeft];
  FTextBounds.Top    := Top + FPadding[bsTop];
  FTextBounds.Right  := Right - FPadding[bsRight];
  FTextBounds.Bottom := Bottom - FPadding[bsBottom];
  FontChanged;
end;

function TLazSynTextArea.ScreenColumnToXValue(Col: integer): integer;
begin
  Result := FTextBounds.Left + (Col - LeftChar) * fCharWidth;
end;

function TLazSynTextArea.RowColumnToPixels(const RowCol: TPoint): TPoint;
begin
  // Inludes LeftChar, but not Topline
  Result.X := FTextBounds.Left + (RowCol.X - LeftChar) * CharWidth;
  Result.Y := FTextBounds.Top + RowCol.Y * LineHeight;
end;

function TLazSynTextArea.PixelsToRowColumn(Pixels: TPoint;
  aFlags: TSynCoordinateMappingFlags): TPoint;
begin
  // Inludes LeftChar, but not Topline
  Result.X := (Pixels.X
               - FTextBounds.Left
               + (CharWidth div 2)  // nearest side of char
              ) div CharWidth
              + LeftChar;
  Result.Y := (Pixels.Y - FTextBounds.Top) div LineHeight;

  if (not(scmIncludePartVisible in aFlags)) and (Result.Y >= LinesInWindow) then begin
    // don't return a partially visible last line
    Result.Y := LinesInWindow - 1;
  end;
  if Result.X < 0 then Result.X := 0;
  if Result.Y < 0 then Result.Y := 0;
end;

constructor TLazSynTextArea.Create(AOwner: TWinControl; ATextDrawer: TheTextDrawer);
var
  i: TLazSynBorderSide;
begin
  inherited Create(AOwner);
  FTextDrawer := ATextDrawer;
  FTextDrawer.RegisterOnFontChangeHandler(@DoDrawerFontChanged);
  FPaintLineColor := TSynSelectedColor.Create;
  FPaintLineColor2 := TSynSelectedColor.Create;
  for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do
    FPadding[i] := 0;
  FTopLine := 1;
  FLeftChar := 1;
  FRightEdgeColumn  := 80;
  FRightEdgeVisible := True;
  FRightEdgeColor   := clSilver;
  FontChanged;
end;

destructor TLazSynTextArea.Destroy;
begin
  FTextDrawer.UnRegisterOnFontChangeHandler(@DoDrawerFontChanged);
  FreeAndNil(FPaintLineColor);
  FreeAndNil(FPaintLineColor2);
  inherited Destroy;
end;

procedure TLazSynTextArea.Assign(Src: TLazSynSurface);
var
  i: TLazSynBorderSide;
begin
  inherited Assign(Src);

  FTextDrawer    := TLazSynTextArea(Src).FTextDrawer;
  FTheLinesView  := TLazSynTextArea(Src).FTheLinesView;
  DisplayView   := TLazSynTextArea(Src).DisplayView;
  FHighlighter   := TLazSynTextArea(Src).FHighlighter;
  FMarkupManager := TLazSynTextArea(Src).FMarkupManager;
  FForegroundColor := TLazSynTextArea(Src).FForegroundColor;
  FBackgroundColor := TLazSynTextArea(Src).FBackgroundColor;
  FRightEdgeColor  := TLazSynTextArea(Src).FRightEdgeColor;

  FExtraCharSpacing := TLazSynTextArea(Src).FExtraCharSpacing;
  FExtraLineSpacing := TLazSynTextArea(Src).FExtraLineSpacing;
  FVisibleSpecialChars := TLazSynTextArea(Src).FVisibleSpecialChars;
  FRightEdgeColumn := TLazSynTextArea(Src).FRightEdgeColumn;
  FRightEdgeVisible := TLazSynTextArea(Src).FRightEdgeVisible;

  for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do
    FPadding[i] := TLazSynTextArea(Src).FPadding[i];

  FTopLine := TLazSynTextArea(Src).FTopLine;
  FLeftChar := TLazSynTextArea(Src).FLeftChar;

  BoundsChanged;
end;

procedure TLazSynTextArea.InvalidateLines(FirstTextLine, LastTextLine: TLineIdx);
var
  rcInval: TRect;
begin
  rcInval := Bounds;
  if (FirstTextLine >= 0) then
    rcInval.Top := Max(TextBounds.Top,
                       TextBounds.Top
                       + (DisplayView.TextToViewIndex(FirstTextLine).Top
                          - TopLine + 1) * LineHeight);
  if (LastTextLine >= 0) then
    rcInval.Bottom := Min(TextBounds.Bottom,
                          TextBounds.Top
                          + (DisplayView.TextToViewIndex(LastTextLine).Bottom
                             - TopLine + 2)  * LineHeight);

  {$IFDEF VerboseSynEditInvalidate}
  DebugLn(['TCustomSynEdit.InvalidateTextLines ',DbgSName(self), ' FirstLine=',FirstTextLine, ' LastLine=',LastTextLine, ' rect=',dbgs(rcInval)]);
  {$ENDIF}
  if (rcInval.Top < rcInval.Bottom) and (rcInval.Left < rcInval.Right) then
    InvalidateRect(Handle, @rcInval, FALSE);
end;

procedure TLazSynTextArea.FontChanged;
begin
  // ToDo: wait for handle creation
  // Report FLinesInWindow=-1 if no handle
  FCharWidth := FTextDrawer.CharWidth;  // includes extra
  FTextHeight := FTextDrawer.CharHeight + FExtraLineSpacing;

  FCharsInWindow :=  0;
  FLinesInWindow :=  0;
  if FCharWidth > 0 then
    FCharsInWindow := Max(0, (FTextBounds.Right - FTextBounds.Left) div FCharWidth);
  if FTextHeight > 0 then
    FLinesInWindow := Max(0, (FTextBounds.Bottom - FTextBounds.Top) div FTextHeight);
end;

procedure TLazSynTextArea.DoPaint(ACanvas: TCanvas; AClip: TRect);
var
  PadRect, PadRect2: TRect;
  ScreenRow1, ScreenRow2, TextColumn1, TextColumn2: integer;
  dc: HDC;
begin

  // paint padding
  FCanvas := ACanvas;
  dc := ACanvas.Handle;
  SetBkColor(dc, ColorToRGB(BackgroundColor));

  if (AClip.Top < FTextBounds.Top) then begin
    PadRect2 := Bounds;
    PadRect2.Bottom := FTextBounds.Top;
    IntersectRect(PadRect, AClip, PadRect2);
    InternalFillRect(dc, PadRect);
  end;
  if (AClip.Bottom > FTextBounds.Bottom) then begin
    PadRect2 := Bounds;
    PadRect2.Top := FTextBounds.Bottom;
    IntersectRect(PadRect, AClip, PadRect2);
    InternalFillRect(dc, PadRect);
  end;
  if (AClip.Left < FTextBounds.Left) then begin
    PadRect2 := Bounds;
    PadRect2.Right := FTextBounds.Left;
    IntersectRect(PadRect, AClip, PadRect2);
    InternalFillRect(dc, PadRect);
  end;
  if (AClip.Right > FTextBounds.Right) then begin
    PadRect2 := Bounds;
    PadRect2.Left := FTextBounds.Right;
    IntersectRect(PadRect, AClip, PadRect2);
    InternalFillRect(dc, PadRect);
  end;

  if (AClip.Left   >= FTextBounds.Right) or
     (AClip.Right  <= FTextBounds.Left) or
     (AClip.Top    >= FTextBounds.Bottom) or
     (AClip.Bottom <= FTextBounds.Top)
  then
    exit;

  TextColumn1 := LeftChar;
  if (AClip.Left > FTextBounds.Left) then
    Inc(TextColumn1, (AClip.Left - FTextBounds.Left) div CharWidth);
  TextColumn2 := LeftChar +
    ( Min(AClip.Right, FTextBounds.Right) - FTextBounds.Left + CharWidth - 1) div CharWidth;
  // lines
  ScreenRow1 := Max((AClip.Top - FTextBounds.Top) div fTextHeight, 0);
  ScreenRow2 := Min((AClip.Bottom-1 - FTextBounds.Top) div fTextHeight, LinesInWindow + 1);

  AClip.Left   := Max(AClip.Left, FTextBounds.Left); // Todo: This is also checked in paintLines (together with right side)
  AClip.Right  := Min(AClip.Right, FTextBounds.Right);
  //AClip.Top    := Max(AClip.Top, FTextBounds.Top);
  //AClip.Bottom := Min(AClip.Bottom, FTextBounds.Bottom);

  SetBkMode(dc, TRANSPARENT);
  PaintTextLines(AClip, ScreenRow1, ScreenRow2, TextColumn1, TextColumn2);

  FCanvas := nil;
end;

procedure TLazSynTextArea.PaintTextLines(AClip: TRect; FirstLine, LastLine,
  FirstCol, LastCol: integer);
// FirstLine, LastLine are based 0
// FirstCol, LastCol are screen based 1 without scrolling (physical position).
//  i.e. the real screen position is fTextOffset+Pred(FirstCol)*CharWidth
var
  bDoRightEdge: boolean; // right edge
  nRightEdge: integer;
  colEditorBG: TColor;
    // painting the background and the text
  rcLine, rcToken: TRect;
  EraseLeft, DrawLeft: Integer;  // LeftSide for EraseBackground, Text
  CurLine: integer;         // Screen-line index for the loop
  CurTextIndex: Integer;    // Current Index in text
  CurPhysPos, CurLogIndex : Integer; // Physical Start Position of next token in current Line
  ForceEto: Boolean;
  TokenAccu: record
    Len, MaxLen: integer;
    PhysicalStartPos, PhysicalEndPos: integer;
    p: PChar;
    FG, BG: TColor;
    Style: TFontStyles;
    FrameColor: array[TLazSynBorderSide] of TColor;
    FrameStyle: array[TLazSynBorderSide] of TSynLineStyle;
  end;
  dc: HDC;

  ExpandedPaintToken: string; // used to create the string sent to TextDrawer
  CharWidths: TPhysicalCharWidths;

{ local procedures }

  procedure SetTokenAccuLength;
  begin
    ReAllocMem(TokenAccu.p,TokenAccu.MaxLen+1);
    TokenAccu.p[TokenAccu.MaxLen]:=#0;
  end;

  function ExpandSpecialChars(var p: PChar; var Count: integer;
    PhysicalStartPos: integer): Integer;
  // if there are no tabs or special chars: keep p and Count untouched
  // if there are special chars: copy p into ExpandedPaintToken buffer,
  //                             convert tabs to spaces, and return the buffer
  // Return DisplayCell-Count in Buffer
  var
    i: integer;
    LengthNeeded: Integer;
    DestPos: Integer;
    SrcPos: Integer;
    Dest: PChar;
    c: Char;
    CharLen: Integer;
    Special, SpecialTab1, SpecialTab2, SpecialSpace, HasTabs: boolean;
    Fill: Integer;
  begin
    LengthNeeded := 0;
    Result := 0;

    if CurLogIndex >= length(CharWidths) then begin
      // past eol (e.g. fold marker). No special handling. No support for double width chars
      // count utf8 chars
      for i := 0 to Count -1 do
        if p[i] <= #$7f then inc(Result);
      exit;
    end;

    HasTabs := False;
    SrcPos:=0;
    for i := CurLogIndex to CurLogIndex + Count -1 do begin
      Result := Result + CharWidths[i];
      if CharWidths[i] > 1 then
        LengthNeeded := LengthNeeded + CharWidths[i] - 1;
      if p[SrcPos] = #9 then HasTabs := True;
      inc(SrcPos);
    end;
    Special := VisibleSpecialChars <> [];
    if (not Special) and (LengthNeeded=0) and (not HasTabs)
    and (FindInvalidUTF8Character(p,Count)<0) then
      exit;
    SpecialTab1 := Special and (vscTabAtFirst in FVisibleSpecialChars);
    SpecialTab2 := Special and (vscTabAtLast in FVisibleSpecialChars);
    SpecialSpace := Special and (vscSpace in FVisibleSpecialChars);
    LengthNeeded := LengthNeeded + Count;
    if Special then LengthNeeded:=LengthNeeded*2;
    if length(ExpandedPaintToken)<LengthNeeded then
      SetLength(ExpandedPaintToken,LengthNeeded+CharsInWindow);
    //DebugLn(['ExpandSpecialChars Count=',Count,' TabCount=',TabCount,' Special=',Special,' LengthNeeded=',LengthNeeded]);
    SrcPos:=0;
    DestPos:=0;
    Dest:=PChar(Pointer(ExpandedPaintToken));
    if fTextDrawer.UseUTF8 then begin
      while SrcPos<Count do begin
        c:=p[SrcPos];
        Fill := CharWidths[CurLogIndex + SrcPos] - 1;
        if c = #9 then begin
          // tab char, fill with spaces
          if SpecialTab1 then begin
            // #194#187 looks like >>
            Dest[DestPos]   := #194;
            Dest[DestPos+1] := #187;
            inc(DestPos, 2);
            dec(Fill);
          end;
          while Fill >= 0 do begin //for i := 0 to Fill do begin
            Dest[DestPos]:= ' ';
            inc(DestPos);
            dec(Fill);
          end;
          if SpecialTab2 then begin
            // #194#187 looks like >>
            Dest[DestPos-1] := #194;
            Dest[DestPos]   := #187;
            inc(DestPos, 1);
          end;
          inc(SrcPos);
        end
        else begin
          // could be UTF8 char
          if c in [#128..#255]
          then CharLen := UTF8CharacterStrictLength(@p[SrcPos])
          else CharLen := 1;
          if CharLen=0 then begin
            // invalid character
            Dest[DestPos]:='?';
            inc(DestPos);
            inc(SrcPos);
          end else begin
            // normal UTF-8 character
            for i:=1 to CharLen do begin
              Dest[DestPos]:=p[SrcPos];
              inc(DestPos);
              inc(SrcPos);
            end;
            if (c = #32) and SpecialSpace then begin
              // #194#183 looks like .
              Dest[DestPos-1] := #194;
              Dest[DestPos]   := #183;
              inc(DestPos);
            end;
            for i := 1 to Fill do begin
              Dest[DestPos]:= ' ';
              inc(DestPos);
            end;
          end;
          // ToDo: pass the eto with to fTextDrawer, instead of filling with spaces
          if Fill > 0 then ForceEto := True;
        end;
      end;
    end else begin
      // non UTF-8
      while SrcPos<Count do begin
        c:=p[SrcPos];
        Fill := CharWidths[CurLogIndex + SrcPos] - 1;
        if c = #9 then // tab char
          Dest[DestPos] := ' '
        else begin
          Dest[DestPos] := p[SrcPos];
          if Fill > 0 then ForceEto := True;
        end;
        inc(DestPos);
        inc(SrcPos);
        for i := 1 to Fill do begin
          Dest[DestPos]:= ' ';
          inc(DestPos);
        end;
      end;
    end;
    p:=PChar(Pointer(ExpandedPaintToken));
    Count:=DestPos;
    //debugln('ExpandSpecialChars Token with Tabs: "',DbgStr(copy(ExpandedPaintToken,1,Count)),'"');
  end;

  const
    ETOOptions = ETO_OPAQUE; // Note: clipping is slow and not needed

  procedure PaintToken(Token: PChar; TokenLen, FirstPhysical: integer);
  // FirstPhysical is the physical (screen without scrolling)
  // column of the first character
  var
    nX: integer;
    tok: TRect;
  begin
    {debugln('PaintToken A TokenLen=',dbgs(TokenLen),
      ' FirstPhysical=',dbgs(FirstPhysical),
      ' Tok="'+copy(Token, 1, TokenLen),'"',
      ' rcToken='+dbgs(rcToken.Left)+'-'+dbgs(rcToken.Right));}
    if (rcToken.Right <= rcToken.Left) then exit;
    // Draw the right edge under the text if necessary
    nX := ScreenColumnToXValue(FirstPhysical); // == rcToken.Left
    if ForceEto then fTextDrawer.ForceNextTokenWithEto;
    if bDoRightEdge
    and (nRightEdge<rcToken.Right) and (nRightEdge>=rcToken.Left)
    then begin
      // draw background (use rcToken, so we do not delete the divider-draw-line)
      if rcToken.Left < nRightEdge then begin
        tok := rcToken;
        tok.Right := nRightEdge;
        InternalFillRect(dc, tok);
      end;
      if rcToken.Right > nRightEdge then begin
        tok := rcToken;
        tok.Left := nRightEdge;
        tok.Bottom := rcLine.Bottom;
        InternalFillRect(dc, tok);
      end;
      // draw edge (use rcLine / rcToken may be reduced)
      LCLIntf.MoveToEx(dc, nRightEdge, rcLine.Top, nil);
      LCLIntf.LineTo(dc, nRightEdge, rcLine.Bottom + 1);
      // draw text
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions-ETO_OPAQUE, rcToken,
                             Token, TokenLen, rcLine.Bottom);
    end else begin
      // draw text with background
      //debugln('PaintToken nX=',dbgs(nX),' Token=',dbgstr(copy(Token,1, TokenLen)),' rcToken=',dbgs(rcToken));
      tok := rcToken;
      if rcToken.Right > nRightEdge + 1 then
        tok.Bottom := rcLine.Bottom;
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions, tok,
                             Token, TokenLen, rcLine.Bottom);
    end;
    rcToken.Left := rcToken.Right;
  end;

  procedure PaintHighlightToken(bFillToEOL: boolean);
  var
    nX1, eolx: integer;
    NextPos : Integer;
    MarkupInfo: TSynSelectedColor;
    FillFCol, FillBCol, FillFrame: TColor;
    FrameStyle: TSynLineStyle;
    FillStyle: TFontStyles;
    tok: TRect;
    Attr: TSynHighlighterAttributes;
    s: TLazSynBorderSide;
  begin
    {debugln('PaintHighlightToken A TokenAccu: Len=',dbgs(TokenAccu.Len),
      ' PhysicalStartPos=',dbgs(TokenAccu.PhysicalStartPos),
      ' PhysicalEndPos=',dbgs(TokenAccu.PhysicalEndPos),
      ' "',copy(TokenAccu.p,1,TokenAccu.Len),'"');}

    // Any token chars accumulated?
    if (TokenAccu.Len > 0) then
    begin
      // Initialize the colors and the font style.
      with fTextDrawer do
      begin
        SetBackColor(TokenAccu.BG);
        SetForeColor(TokenAccu.FG);
        SetStyle(TokenAccu.Style);
        for s := low(TLazSynBorderSide) to high(TLazSynBorderSide) do begin
          FrameColor[s] := TokenAccu.FrameColor[s];
          FrameStyle[s] := TokenAccu.FrameStyle[s];
        end;
      end;
      // Paint the chars
      rcToken.Right := ScreenColumnToXValue(TokenAccu.PhysicalEndPos+1);
      if rcToken.Right > AClip.Right then begin
        rcToken.Right := AClip.Right;
        fTextDrawer.FrameColor[bsRight] := clNone; // right side of char is not painted
      end;
      with TokenAccu do PaintToken(p, Len, PhysicalStartPos);
    end;

    // Fill the background to the end of this line if necessary.
    if bFillToEOL and (rcToken.Left < rcLine.Right) then begin
      eolx := rcToken.Left; // remeber end of actual line, so we can decide to draw the right edge
      NextPos := Min(LastCol, TokenAccu.PhysicalEndPos+1);
      if Assigned(fHighlighter) then
        Attr := fHighlighter.GetEndOfLineAttribute
      else
        Attr := nil;
      repeat
        MarkupInfo := fMarkupManager.GetMarkupAttributeAtRowCol(CurTextIndex+1, NextPos);
        NextPos := fMarkupManager.GetNextMarkupColAfterRowCol(CurTextIndex+1, NextPos);

        if Assigned(Attr) then
        begin
          FillFCol := Attr.Foreground;
          FillBCol := Attr.Background;
          FillFrame := Attr.FrameColor;
          FillStyle := Attr.Style;
          FrameStyle := Attr.FrameStyle;
          if FillFCol = clNone then FillFCol := ForegroundColor;
          if FillBCol = clNone then FillBCol := colEditorBG;
        end else
        begin
          FillFCol := ForegroundColor;
          FillBCol := colEditorBG;
          FillFrame := clNone;
          FillStyle := []; // Font.Style; // currently always cleared
          FrameStyle := slsSolid;
        end;

        if Assigned(MarkupInfo) then
          MarkupInfo.ModifyColors(FillFCol, FillBCol, FillFrame, FillStyle, FrameStyle);

        fTextDrawer.BackColor := FillBCol;
        //fTextDrawer.ForeColor := FillFCol; // for underline
        //fTextDrawer.Style := FillStyle;

        if NextPos < 1
        then nX1 := rcLine.Right
        else begin
          nX1 := ScreenColumnToXValue(NextPos);
          if nX1 > rcLine.Right
          then nX1 := rcLine.Right;
        end;

        if nX1 > nRightEdge then begin
          if rcToken.Left < nRightEdge then begin
            tok := rcToken;
            tok.Right := nRightEdge;
            InternalFillRect(dc, tok);
            rcToken.Left := nRightEdge;
          end;
          rcToken.Bottom := rcLine.Bottom;
        end;
        rcToken.Right := nX1;
        InternalFillRect(dc, rcToken); {TODO: if style underline, then print spaces}
        rcToken.Left := nX1;
      until nX1 >= rcLine.Right;

      // Draw the right edge if necessary.
      if bDoRightEdge
      and (nRightEdge >= eolx) then begin // xx rc Token
        LCLIntf.MoveToEx(dc, nRightEdge, rcLine.Top, nil);
        LCLIntf.LineTo(dc, nRightEdge, rcLine.Bottom + 1);
      end;
    end;
  end;

  procedure AddHighlightToken(Token: PChar;
    TokenLen, PhysicalStartPos, PhysicalEndPos: integer;
    MarkupInfo : TSynSelectedColor);
  var
    CanAppend: boolean;
    SpacesTest, IsSpaces: boolean;
    i: integer;
    s: TLazSynBorderSide;

    function TokenIsSpaces: boolean;
    var
      pTok: PChar;
      Cnt: Integer;
    begin
      if not SpacesTest then begin
        SpacesTest := TRUE;
        IsSpaces := VisibleSpecialChars = [];
        pTok := PChar(Pointer(Token));
        Cnt := TokenLen;
        while IsSpaces and (Cnt > 0) do begin
          if not (pTok^ in [' ',#9])
          then IsSpaces := False;
          Inc(pTok);
          dec(Cnt);
        end;
      end;
      Result := IsSpaces;
    end;

  begin
    {DebugLn('AddHighlightToken A TokenLen=',dbgs(TokenLen),
      ' PhysicalStartPos=',dbgs(PhysicalStartPos),' PhysicalEndPos=',dbgs(PhysicalEndPos),
      ' Tok="',copy(Token,1,TokenLen),'"');}

    // Do we have to paint the old chars first, or can we just append?
    CanAppend := FALSE;
    SpacesTest := FALSE;

    if (TokenAccu.Len > 0) then
    begin
      CanAppend :=
        // Frame can be continued
        (TokenAccu.FrameColor[bsTop] = MarkupInfo.FrameSideColors[bsTop]) and
        (TokenAccu.FrameStyle[bsTop] = MarkupInfo.FrameSideStyles[bsTop]) and
        (TokenAccu.FrameColor[bsBottom] = MarkupInfo.FrameSideColors[bsBottom]) and
        (TokenAccu.FrameStyle[bsBottom] = MarkupInfo.FrameSideStyles[bsBottom]) and
        (TokenAccu.FrameColor[bsRight] = clNone) and
        (MarkupInfo.FrameSideColors[bsLeft] = clNone) and
        // colors
        (TokenAccu.BG = MarkupInfo.Background) and
        // space-dependent
        ( ( (TokenAccu.FG = MarkupInfo.Foreground) and
            (TokenAccu.Style = MarkupInfo.Style)
          ) or
          // whitechar only token, can ignore Foreground color and certain styles (yet must match underline)
          ( (TokenAccu.Style - [fsBold, fsItalic] = MarkupInfo.Style - [fsBold, fsItalic]) and
            ( (TokenAccu.Style * [fsUnderline, fsStrikeOut] = []) or
              (TokenAccu.FG = MarkupInfo.Foreground)
            ) and
            TokenIsSpaces
          )
        );
      // If we can't append it, then we have to paint the old token chars first.
      if not CanAppend then
        PaintHighlightToken(FALSE);
    end;

    // Don't use AppendStr because it's more expensive.
    //if (CurLine=TopLine) then debugln('      -t-Accu len ',dbgs(TokenAccu.Len),' pstart ',dbgs(TokenAccu.PhysicalStartPos),' p-end ',dbgs(TokenAccu.PhysicalEndPos));
    if CanAppend then begin
      if (TokenAccu.Len + TokenLen > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + TokenLen + 32;
        SetTokenAccuLength;
      end;
      // use move() ???
      for i := 0 to TokenLen-1 do begin
        TokenAccu.p[TokenAccu.Len + i] := Token[i];
      end;
      Inc(TokenAccu.Len, TokenLen);
      TokenAccu.PhysicalEndPos := PhysicalEndPos;
      TokenAccu.FrameColor[bsRight] := MarkupInfo.FrameSideColors[bsRight];
      TokenAccu.FrameStyle[bsRight] := MarkupInfo.FrameSideStyles[bsRight];
    end else begin
      TokenAccu.Len := TokenLen;
      if (TokenAccu.Len > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + 32;
        SetTokenAccuLength;
      end;
      for i := 0 to TokenLen-1 do begin
        TokenAccu.p[i] := Token[i];
      end;
      TokenAccu.PhysicalStartPos := PhysicalStartPos;
      TokenAccu.PhysicalEndPos := PhysicalEndPos;
      TokenAccu.FG := MarkupInfo.Foreground;
      TokenAccu.BG := MarkupInfo.Background;
      TokenAccu.Style := MarkupInfo.Style;
      for s := low(TLazSynBorderSide) to high(TLazSynBorderSide) do begin
        TokenAccu.FrameColor[s] := MarkupInfo.FrameSideColors[s];
        TokenAccu.FrameStyle[s] := MarkupInfo.FrameSideStyles[s];
      end;
    end;
    {debugln('AddHighlightToken END CanAppend=',dbgs(CanAppend),
      ' Len=',dbgs(TokenAccu.Len),
      ' PhysicalStartPos=',dbgs(TokenAccu.PhysicalStartPos),
      ' PhysicalEndPos=',dbgs(TokenAccu.PhysicalEndPos),
      ' "',copy(TokenAccu.s,1,TokenAccu.Len),'"');}
  end;

  procedure DrawHiLightMarkupToken(attr: TSynHighlighterAttributes;
    sToken: PChar; nTokenByteLen: integer);
  var
    DefaultFGCol, DefaultBGCol: TColor;
    PhysicalStartPos: integer;
    PhysicalEndPos: integer;
    len: Integer;
    SubTokenByteLen, SubCharLen, TokenCharLen : Integer;
    NextPhysPos : Integer;

    function CharToByteLen(aCharLen: Integer) : Integer;
    begin
      if not fTextDrawer.UseUTF8 then exit(aCharLen);
      // tabs and double-width chars are padded with spaces
      Result := UTF8CharToByteIndex(sToken, nTokenByteLen, aCharLen);
      if Result < 0 then begin
        debugln('ERROR: Could not convert CharLen (',dbgs(aCharLen),') to byteLen (maybe invalid UTF8?)',' len ',dbgs(nTokenByteLen),' Line ',dbgs(CurLine),' PhysPos ',dbgs(CurPhysPos));
        Result := aCharLen;
      end;
    end;

    procedure InitTokenColors;
    begin
      FPaintLineColor.Clear;
      if Assigned(attr) then
      begin
        DefaultFGCol := attr.Foreground;
        DefaultBGCol := attr.Background;
        if DefaultBGCol = clNone then DefaultBGCol := colEditorBG;
        if DefaultFGCol = clNone then DefaultFGCol := ForegroundColor;

        FPaintLineColor.Assign(attr);
        // TSynSelectedColor.Style and StyleMask describe how to modify a style,
        // but FPaintLineColor contains an actual style
        FPaintLineColor.MergeFinalStyle := True;
        FPaintLineColor.StyleMask  := [];
        if FPaintLineColor.Background = clNone then
          FPaintLineColor.Background := colEditorBG;
        if FPaintLineColor.Foreground = clNone then
          FPaintLineColor.Foreground := ForegroundColor;
        FPaintLineColor.StartX := PhysicalStartPos;
        FPaintLineColor.EndX   := PhysicalStartPos + TokenCharLen - 1;
      end else
      begin
        DefaultFGCol := ForegroundColor;
        DefaultBGCol := colEditorBG;
        FPaintLineColor.Style :=  []; // Font.Style; // currently always cleared
      end;

      FPaintLineColor.Foreground := DefaultFGCol;
      FPaintLineColor.Background := DefaultBGCol;
    end;

  begin
    if CurPhysPos > LastCol then exit;

    PhysicalStartPos := CurPhysPos;
    len := nTokenByteLen;
    TokenCharLen := ExpandSpecialChars(sToken, nTokenByteLen, PhysicalStartPos);
    CurLogIndex := CurLogIndex + len;
    // Prepare position for next token
    inc(CurPhysPos, TokenCharLen);
    if CurPhysPos <= FirstCol then exit;

    // Remove any Part of the Token that is before FirstCol
    if PhysicalStartPos < FirstCol then begin
      SubCharLen := FirstCol - PhysicalStartPos;
      len := CharToByteLen(SubCharLen);
      dec(TokenCharLen, SubCharLen);
      inc(PhysicalStartPos, SubCharLen);
      dec(nTokenByteLen, len);
      inc(sToken, len);
    end;

    // Remove any Part of the Token that is after LastCol
    SubCharLen := PhysicalStartPos + TokenCharLen - (LastCol + 1);
    if SubCharLen > 0 then begin
      dec(TokenCharLen, SubCharLen);
      nTokenByteLen := CharToByteLen(TokenCharLen);
    end;

    InitTokenColors;

    // Draw the token
    {TODO: cache NextPhysPos, and MarkupInfo between 2 calls }
    while (nTokenByteLen > 0) do begin
      FPaintLineColor2.Assign(FPaintLineColor);

      // Calculate Token Sublen for current Markup
      NextPhysPos := fMarkupManager.GetNextMarkupColAfterRowCol
                       (CurTextIndex+1, PhysicalStartPos);
      if NextPhysPos < 1
      then SubCharLen := TokenCharLen
      else SubCharLen := NextPhysPos - PhysicalStartPos;

      if SubCharLen > TokenCharLen then SubCharLen := TokenCharLen;
      if SubCharLen < 1 then begin // safety for broken input...
        debugln('ERROR: Got invalid SubCharLen ',dbgs(SubCharLen),' len ',dbgs(nTokenByteLen),' Line ',dbgs(CurLine),' PhysPos ',dbgs(CurPhysPos));
        SubCharLen:=1;
      end;

      SubTokenByteLen := CharToByteLen(SubCharLen);
      PhysicalEndPos:= PhysicalStartPos + SubCharLen - 1;

      FPaintLineColor2.CurrentStartX := PhysicalStartPos;
      FPaintLineColor2.CurrentEndX := PhysicalEndPos;

      // Calculate Markup
      fMarkupManager.MergeMarkupAttributeAtRowCol(CurTextIndex+1,
        PhysicalStartPos, PhysicalEndPos, FPaintLineColor2);

      // Deal with equal colors
      if (FPaintLineColor2.Background = FPaintLineColor2.Foreground) then begin // or if diff(gb,fg) < x
        if FPaintLineColor2.Background = DefaultBGCol then
          FPaintLineColor2.Foreground := not(FPaintLineColor2.Background) and $00ffffff // or maybe ForegroundColor ?
        else
          FPaintLineColor2.Foreground := DefaultBGCol;
      end;

      // Add to TokenAccu
      AddHighlightToken(sToken, SubTokenByteLen,
        PhysicalStartPos, PhysicalEndPos, FPaintLineColor2);

      PhysicalStartPos:=PhysicalEndPos + 1;
      dec(nTokenByteLen,SubTokenByteLen);
      dec(TokenCharLen, SubCharLen);
      inc(sToken, SubTokenByteLen);
    end;
  end;

  {$IFDEF SYNDEBUGPRINT}
  procedure DebugPrint(Txt: String; MinCol: Integer = 0);
  begin
    if CurPhysPos < MinCol then Txt := StringOfChar(' ', MinCol - CurPhysPos) + txt;
    Setlength(CharWidths, length(CharWidths) + length(Txt));
    FillChar(CharWidths[length(CharWidths)-length(Txt)], length(Txt), #1);
    DrawHiLightMarkupToken(nil, PChar(Pointer(Txt)), Length(Txt));
  end;
  {$ENDIF}

  procedure PaintLines;
  var
    ypos: Integer;
    DividerInfo: TSynDividerDrawConfigSetting;
    TV, cl: Integer;
    TokenInfo: TLazSynDisplayTokenInfo;
    MaxLine: Integer;
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Bottom := TextBounds.Top + FirstLine * fTextHeight;

    TV := TopLine - 1;

    // Now loop through all the lines. The indices are valid for Lines.
    MaxLine := DisplayView.GetLinesCount-1;

    CurLine := FirstLine-1;
    while CurLine<LastLine do begin
      inc(CurLine);
      if TV + CurLine > MaxLine then break;
      // Update the rcLine rect to this line.
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, fTextHeight);
      // Paint the lines depending on the assigned highlighter.
      rcToken := rcLine;
      TokenAccu.Len := 0;
      TokenAccu.PhysicalEndPos := FirstCol - 1; // in case of an empty line
      CurPhysPos := 1;
      CurLogIndex := 0;
      // Delete the whole Line
      fTextDrawer.BackColor := colEditorBG;
      SetBkColor(dc, ColorToRGB(colEditorBG));
      rcLine.Left := EraseLeft;
      InternalFillRect(dc, rcLine);
      rcLine.Left := DrawLeft;
      ForceEto := False;

      DisplayView.SetHighlighterTokensLine(TV + CurLine, CurTextIndex);
      CharWidths := FTheLinesView.GetPhysicalCharWidths(CurTextIndex);
      fMarkupManager.PrepareMarkupForRow(CurTextIndex+1);

      DividerInfo := DisplayView.GetDrawDividerInfo;
      if (DividerInfo.Color <> clNone) and (nRightEdge >= FTextBounds.Left) then
      begin
        ypos := rcToken.Bottom - 1;
        cl := DividerInfo.Color;
        if cl = clDefault then
          cl := RightEdgeColor;
        fTextDrawer.DrawLine(nRightEdge, ypos, FTextBounds.Left - 1, ypos, cl);
        dec(rcToken.Bottom);
      end;

      while DisplayView.GetNextHighlighterToken(TokenInfo) do begin
        DrawHiLightMarkupToken(TokenInfo.TokenAttr, TokenInfo.TokenStart, TokenInfo.TokenLength);
      end;

      // Draw anything that's left in the TokenAccu record. Fill to the end
      // of the invalid area with the correct colors.
      PaintHighlightToken(TRUE);

      fMarkupManager.FinishMarkupForRow(CurTextIndex+1);
    end;
    CurLine:=-1;
    AClip.Top := rcLine.Bottom;
  end;

{ end local procedures }

begin
  //if (AClip.Right < TextLeftPixelOffset(False)) then exit;
  //if (AClip.Left > ClientWidth - TextRightPixelOffset) then exit;

  //DebugLn(['TCustomSynEdit.PaintTextLines ',dbgs(AClip)]);
  CurLine:=-1;
  //DebugLn('TCustomSynEdit.PaintTextLines ',DbgSName(Self),' TopLine=',dbgs(TopLine),' AClip=',dbgs(AClip));
  colEditorBG := BackgroundColor;
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  bDoRightEdge := FALSE;
  if FRightEdgeVisible then begin // column value
    nRightEdge := FTextBounds.Left + (RightEdgeColumn - LeftChar + 1) * CharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then
      bDoRightEdge := TRUE;
    if nRightEdge > AClip.Right then
      nRightEdge := AClip.Right; // for divider draw lines (don't draw into right gutter)
  end
  else
    nRightEdge := AClip.Right;

  Canvas.Pen.Color := RightEdgeColor; // used for code folding too
  Canvas.Pen.Width := 1;
  // Do everything else with API calls. This (maybe) realizes the new pen color.
  dc := Canvas.Handle;
  SetBkMode(dc, TRANSPARENT);

  // Adjust the invalid area to not include the gutter (nor the 2 ixel offset to the guttter).
  EraseLeft := AClip.Left;
  if (AClip.Left < FTextBounds.Left) then
    AClip.Left := FTextBounds.Left ;
  DrawLeft := AClip.Left;

  if (LastLine >= FirstLine) then begin
    // Paint the visible text lines. To make this easier, compute first the
    // necessary information about the selected area: is there any visible
    // selected area, and what are its lines / columns?
    // Moved to two local procedures to make it easier to read.

    FillChar(TokenAccu,SizeOf(TokenAccu),0);
    if Assigned(fHighlighter) then begin
      fHighlighter.CurrentLines := FTheLinesView;
      // Make sure the token accumulator string doesn't get reassigned to often.
      TokenAccu.MaxLen := Max(128, fCharsInWindow * 4);
      SetTokenAccuLength;
    end;

    DisplayView.InitHighlighterTokens(FHighlighter);
    fTextDrawer.Style := []; //Font.Style;
    fTextDrawer.BeginDrawing(dc);
    try
      PaintLines;
    finally
      fTextDrawer.EndDrawing;
      DisplayView.FinishHighlighterTokens;
      ReAllocMem(TokenAccu.p,0);
    end;
  end;

  if (AClip.Top < AClip.Bottom) then begin
    // Delete the remaining area
    SetBkColor(dc, ColorToRGB(colEditorBG));
    AClip.Left := EraseLeft;
    InternalFillRect(dc, AClip);
    AClip.Left := DrawLeft;

    // Draw the right edge if necessary.
    if bDoRightEdge then begin
      LCLIntf.MoveToEx(dc, nRightEdge, AClip.Top, nil);
      LCLIntf.LineTo(dc, nRightEdge, AClip.Bottom + 1);
    end;
  end;

  fMarkupManager.EndMarkup;
end;

end.

