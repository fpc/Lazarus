unit LazSynTextArea;

{$mode objfpc}{$H+}
{ $INLINE OFF}

interface

uses
  Classes, SysUtils, Math,
  // LCL
  Graphics, Controls, LCLType, LCLIntf,
  // LazUtils
  LazMethodList,
  // SynEdit
  SynEditTypes, SynEditMiscProcs, SynEditMiscClasses, LazSynEditText, SynEditMarkup,
  SynEditHighlighter, LazEditMiscProcs, LazEditTextAttributes,
  LazEditTextGridPainter;


type
  TLazSynDisplayTokenInfoEx = record
    Tk: TLazSynDisplayTokenInfo;
    Attr: TSynSelectedColorMergeResult;
    StartPos: TLazSynDisplayTokenBound;  // Start according to Logical flow. Left for LTR, or Right for RTL
    EndPos: TLazSynDisplayTokenBound;    // End according to Logical flow.
    // SreenRect Bounds. Ltr/RTL independent. Start is always left. End Always right
    PhysicalCharStart: Integer;          // 1 based - Full char bound (Before StartPos.Physical (PaintStart))
    PhysicalClipStart: Integer;          // 1 based - PaintStart
    PhysicalCharEnd: Integer;            // 1 based - Full char bound (After EndPos.Physical (PaintEnd))
    PhysicalClipEnd: Integer;            // 1 based - PaintEnd
    RtlInfo: TLazSynDisplayRtlInfo;
    RtlExpandedExtraBytes: Integer;         // tab and space expansion
    RtlHasTabs: Boolean;
    RtlHasDoubleWidth: Boolean;

    ExpandedExtraBytes: Integer;         // tab and space expansion
    HasTabs: Boolean;                    // ExtraWidth may still be 0
    HasDoubleWidth: Boolean;
    NeedsEto: Boolean;

    NextPos: TLazSynDisplayTokenBound;   // Next toxen, may be BIDI
    NextRtlInfo: TLazSynDisplayRtlInfo;
  end;

  { TLazSynPaintTokenBreaker }

  TLazSynPaintTokenBreaker = class
  private
    FBackgroundColor: TColor;
    FForegroundColor: TColor;
    FSpaceExtraByteCount: Integer;
    FTabExtraByteCount: Integer;
    FFirstCol, FLastCol: integer; // Physical, Column-range after scroll and/or clip
    FVirtualFirstCol, FVirtualLastCol: integer; // Adjusted to simulate range for wrapped sub-line

    FDisplayView: TLazSynDisplayView;
    FLinesView:  TSynEditStrings;
    FMarkupManager: TSynEditMarkupManager;

    FCharWidths, FCharWidths2: TPhysicalCharWidths;
    FCharWidthsFromConverter: Boolean;
    FCharWidthsLen: Integer;
    FCurTxtLineIdx : Integer;
    FCurLineByteLen: Integer;
    FIsLastViewedSubLine: Boolean;
    FCurLinePhysStartOffset: integer;

    // Fields for GetNextHighlighterTokenFromView
    // Info about the token (from highlighter)
    FCurViewToken: TLazSynDisplayTokenInfo;
    FCurViewCurTokenStartPos: TLazSynDisplayTokenBound; // Start bound of the HL token
    FCurViewAttr: TSynSelectedColorMergeResult;
    FWrapStartBound, FWrapEndBound: TLazSynDisplayTokenBound;
    // Scanner Pos
    FCurViewScannerPos: TLazSynDisplayTokenBound;  // Start according to Logical flow. Left for LTR, or Right for RTL
    FCurViewScannerPhysCharPos: Integer;           // 1 based - Full char bound (Before FCurViewScannerPos.Physical (PaintStart))
    // RTL Run
    FCurViewinRTL: Boolean;
    FCurViewRtlPhysStart, FCurViewRtlPhysEnd: integer;
    FCurViewRtlLogStart,  FCurViewRtlLogEnd: integer;
    FCurViewRtlExpExtraBytes: Integer;         // tab and space expansion for entire RTL run
    FCurViewRtlHasTabs: Boolean;
    FCurViewRtlHasDoubleWidth: Boolean;

    FNextMarkupPhysPos, FNextMarkupLogPos: Integer;
    FCurMarkupNextStart: TLazSynDisplayTokenBound;
    FCurMarkupNextRtlInfo: TLazSynDisplayRtlInfo;
    FTokenOrigin: TLazSynDisplayTokenOrigin;
    FMarkupTokenAttr: TSynSelectedColorMergeResult;
    procedure InitCurMarkup;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Prepare(ADisplayView: TLazSynDisplayView; ALinesView:  TSynEditStrings;
                      AMarkupManager: TSynEditMarkupManager;
                      AFirstCol, ALastCol: integer
                     );
    procedure Finish;
    procedure SetHighlighterTokensLine(ALine: TLineIdx; out ARealLine: TLineIdx);
    function  GetNextHighlighterTokenFromView(out ATokenInfo: TLazSynDisplayTokenInfoEx;
                                              APhysEnd: Integer = -1;
                                              ALogEnd: Integer = -1
                                             ): Boolean;
    function  GetNextHighlighterTokenEx(out ATokenInfo: TLazSynDisplayTokenInfoEx): Boolean;
    property  CharWidths: TPhysicalCharWidths read FCharWidths;
    property  CharWidthsLen: integer read FCharWidthsLen;
    property ForegroundColor: TColor read FForegroundColor write FForegroundColor;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property SpaceExtraByteCount: Integer read FSpaceExtraByteCount write FSpaceExtraByteCount;
    property TabExtraByteCount: Integer read FTabExtraByteCount write FTabExtraByteCount;
  end;

  { TLazSynTextArea }

  TLazSynTextArea = class(TLazSynSurface)
  private
    FCharsInWindow: Integer;
    FCharWidth: integer;
    FHasPartiallyVisibleLastLine: boolean;
    FLinesInWindow: Integer;
    fOnStatusChange: TStatusChangeEvent;
    FTextSizeChangeList: TMethodList;
    FTextHeight: integer;

    FCanvas: TCanvas;
    FTextDrawer: TLazEditTextGridPainter;
    FEtoBuf: TEtoBuffer;
    FTheLinesView: TSynEditStrings;
    FHighlighter: TSynCustomHighlighter;
    FMarkupManager: TSynEditMarkupManager;
    FTokenBreaker: TLazSynPaintTokenBreaker;
    FPaintLineColor, FPaintLineColor2: TSynSelectedColor;
    FForegroundColor: TColor;
    FBackgroundColor: TColor;
    FRightEdgeColor: TColor;

    FTextBounds: TRect;
    FPadding: array [TLazSynBorderSide] of Integer;
    FExtraCharSpacing: integer;
    FExtraLineSpacing, FCurrentExtraLineSpacing: integer;
    FVisibleSpecialChars: TSynVisibleSpecialChars;
    FRightEdgeColumn: integer;
    FRightEdgeVisible: boolean;

    FViewedTopLine: TLinePos;
    FLeftChar: Integer;

    function GetExtraCharSpacing: integer;
    function GetPadding(Side: TLazSynBorderSide): integer;
    procedure SetExtraCharSpacing(AValue: integer);
    procedure SetExtraLineSpacing(AValue: integer);
    procedure SetLeftChar(AValue: Integer);
    procedure SetPadding(Side: TLazSynBorderSide; AValue: integer);
    procedure SetViewedTopLine(AValue: TLinePos);
    procedure DoDrawerFontChanged(Sender: TObject; Changes: TSynStatusChanges);
  protected
    procedure BoundsChanged; override;
    procedure DoPaint(ACanvas: TCanvas; AClip: TRect); override;
    procedure PaintTextLines(AClip: TRect; FirstLine, LastLine,
      FirstCol, LastCol: integer); virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TSynEditBase; ATextDrawer: TLazEditTextGridPainter);
    destructor Destroy; override;
    procedure Assign(Src: TLazSynSurface); override;
    procedure InvalidateLines(FirstTextLine, LastTextLine: TLineIdx; AScreenLineOffset: Integer = 0); override;

    function ScreenColumnToXValue(Col: integer): integer;  // map screen column to screen pixel
    function RowColumnToPixels(const RowCol: TScreenPoint_0): TPoint;
    function PixelsToRowColumn(Pixels: TPoint; aFlags: TSynCoordinateMappingFlags): TPhysScreenPoint_0; // ignores scmLimitToLines

    procedure FontChanged;
    procedure AddTextSizeChangeHandler(AHandler: TNotifyEvent);
    procedure RemoveTextSizeChangeHandler(AHandler: TNotifyEvent);

    // Settings controlled by SynEdit
    property Padding[Side: TLazSynBorderSide]: integer read GetPadding write SetPadding;
    property ForegroundColor: TColor read FForegroundColor write FForegroundColor;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property ExtraCharSpacing: integer read GetExtraCharSpacing write SetExtraCharSpacing;
    property ExtraLineSpacing: integer read FCurrentExtraLineSpacing write SetExtraLineSpacing;
    property VisibleSpecialChars: TSynVisibleSpecialChars read FVisibleSpecialChars write FVisibleSpecialChars;
    property RightEdgeColumn: integer  read FRightEdgeColumn write FRightEdgeColumn;
    property RightEdgeVisible: boolean read FRightEdgeVisible write FRightEdgeVisible;
    property RightEdgeColor: TColor    read FRightEdgeColor write FRightEdgeColor;

    property TopViewedLine: TLinePos read FViewedTopLine write SetViewedTopLine;
    property TopLine: TLinePos read FViewedTopLine write SetViewedTopLine; deprecated 'Renamed to TopViewedLine - To be removed in 5.99';
    property LeftChar: Integer read FLeftChar write SetLeftChar;

    property TheLinesView:  TSynEditStrings       read FTheLinesView  write FTheLinesView;
    property Highlighter:   TSynCustomHighlighter read FHighlighter   write FHighlighter;
    property MarkupManager: TSynEditMarkupManager read FMarkupManager write FMarkupManager;
    property TextDrawer: TLazEditTextGridPainter read FTextDrawer;
  public
    property TextBounds: TRect read FTextBounds;

    property LineHeight: integer read FTextHeight;
    property CharWidth: integer  read FCharWidth;
    property LinesInWindow: Integer read FLinesInWindow; // Excludes partially visible last line
    property HasPartiallyVisibleLastLine: boolean read FHasPartiallyVisibleLastLine;
    property CharsInWindow: Integer read FCharsInWindow;
    property OnStatusChange: TStatusChangeEvent read fOnStatusChange write fOnStatusChange;
  end;

  { TLazSynSurfaceWithText }

  TLazSynSurfaceWithText = class(TLazSynSurface)
  private
    FTextArea: TLazSynTextArea;
  protected
    procedure SetTextArea(AValue: TLazSynTextArea); virtual;
    function GetTextArea: TLazSynTextArea; virtual;
  public
    procedure Assign(Src: TLazSynSurface); override;
    property TextArea: TLazSynTextArea read GetTextArea write SetTextArea;
  end;

  { TLazSynSurfaceManager }

  TLazSynSurfaceManager = class(TLazSynSurfaceWithText)
  private
    FLeftGutterArea: TLazSynSurfaceWithText;
    FLeftGutterWidth: integer;
    FRightGutterArea: TLazSynSurfaceWithText;
    FRightGutterWidth: integer;
    procedure SetLeftGutterArea(AValue: TLazSynSurfaceWithText);
    procedure SetLeftGutterWidth(AValue: integer);
    procedure SetRightGutterArea(AValue: TLazSynSurfaceWithText);
    procedure SetRightGutterWidth(AValue: integer);
  protected
    function GetLeftGutterArea: TLazSynSurfaceWithText; virtual;
    function GetRightGutterArea: TLazSynSurfaceWithText; virtual;
    procedure SetTextArea(AValue: TLazSynTextArea); override;
    function  GetTextArea: TLazSynTextArea; override;
  protected
    procedure SetBackgroundColor(AValue: TColor); virtual;
    procedure SetExtraCharSpacing(AValue: integer); virtual;
    procedure SetExtraLineSpacing(AValue: integer); virtual;
    procedure SetForegroundColor(AValue: TColor); virtual;
    procedure SetPadding(Side: TLazSynBorderSide; AValue: integer); virtual;
    procedure SetRightEdgeColor(AValue: TColor); virtual;
    procedure SetRightEdgeColumn(AValue: integer); virtual;
    procedure SetRightEdgeVisible(AValue: boolean); virtual;
    procedure SetVisibleSpecialChars(AValue: TSynVisibleSpecialChars); virtual;
    procedure SetHighlighter(AValue: TSynCustomHighlighter); virtual;
  protected
    procedure DoPaint(ACanvas: TCanvas; AClip: TRect); override;
    procedure DoDisplayViewChanged; override;
    procedure BoundsChanged; override;
  public
    constructor Create(AOwner: TSynEditBase);
    procedure InvalidateLines(FirstTextLine, LastTextLine: TLineIdx; AScreenLineOffset: Integer = 0); override;
    procedure InvalidateTextLines(FirstTextLine, LastTextLine: TLineIdx; AScreenLineOffset: Integer = 0); virtual;
    procedure InvalidateGutterLines(FirstTextLine, LastTextLine: TLineIdx; AScreenLineOffset: Integer = 0); virtual;

    property LeftGutterArea:  TLazSynSurfaceWithText read GetLeftGutterArea  write SetLeftGutterArea;
    property RightGutterArea: TLazSynSurfaceWithText read GetRightGutterArea write SetRightGutterArea;
    property LeftGutterWidth:  integer read FLeftGutterWidth  write SetLeftGutterWidth;
    property RightGutterWidth: integer read FRightGutterWidth write SetRightGutterWidth;
  public
    // Settings forwarded to textarea
    property Padding[Side: TLazSynBorderSide]: integer write SetPadding;
    property ForegroundColor: TColor   write SetForegroundColor;
    property BackgroundColor: TColor   write SetBackgroundColor;
    property ExtraCharSpacing: integer write SetExtraCharSpacing;
    property ExtraLineSpacing: integer write SetExtraLineSpacing;
    property VisibleSpecialChars: TSynVisibleSpecialChars write SetVisibleSpecialChars;
    property RightEdgeColumn: integer  write SetRightEdgeColumn;
    property RightEdgeVisible: boolean write SetRightEdgeVisible;
    property RightEdgeColor: TColor    write SetRightEdgeColor;
    property Highlighter:   TSynCustomHighlighter write SetHighlighter;
  end;


implementation

{ TLazSynPaintTokenBreaker }

procedure TLazSynPaintTokenBreaker.InitCurMarkup;
var
  TmpTokenInfo: TLazSynDisplayTokenInfoEx;
begin
  if GetNextHighlighterTokenFromView(TmpTokenInfo, -1, 1) then begin
    FCurMarkupNextStart   := TmpTokenInfo.NextPos;
    FCurMarkupNextRtlInfo := TmpTokenInfo.NextRtlInfo;
  end else begin
    // past eol
    FCurMarkupNextStart          := TmpTokenInfo.StartPos;
    FCurMarkupNextStart.Logical  := FCurMarkupNextStart.Logical + (FVirtualFirstCol - FCurMarkupNextStart.Physical);
    FCurMarkupNextStart.Physical := FVirtualFirstCol;
    FCurMarkupNextRtlInfo.IsRtl  := False;
  end;
end;

constructor TLazSynPaintTokenBreaker.Create;
begin
  FCurViewAttr := TSynSelectedColorMergeResult.Create;
  FMarkupTokenAttr := TSynSelectedColorMergeResult.Create;
  FTabExtraByteCount := 0;
  FSpaceExtraByteCount := 0;
end;

destructor TLazSynPaintTokenBreaker.Destroy;
begin
  FreeAndNil(FCurViewAttr);
  FreeAndNil(FMarkupTokenAttr);
  inherited Destroy;
end;

procedure TLazSynPaintTokenBreaker.Prepare(ADisplayView: TLazSynDisplayView;
  ALinesView: TSynEditStrings; AMarkupManager: TSynEditMarkupManager; AFirstCol,
  ALastCol: integer);
begin
  FDisplayView   := ADisplayView;
  FLinesView     := ALinesView;
  FMarkupManager := AMarkupManager;
  FFirstCol      := AFirstCol;
  FLastCol       := ALastCol;
  FVirtualFirstCol := FFirstCol;
  FVirtualLastCol  := FLastCol;
end;

procedure TLazSynPaintTokenBreaker.Finish;
begin
  FCharWidths := nil;
  FCharWidths2 := nil;
  FCharWidthsFromConverter := False;
end;

procedure TLazSynPaintTokenBreaker.SetHighlighterTokensLine(ALine: TLineIdx; out
  ARealLine: TLineIdx);
var
  LogLeftPos, ASubLineIdx: Integer;
begin
  FDisplayView.SetHighlighterTokensLine(ALine, ARealLine, ASubLineIdx, LogLeftPos, FCurLinePhysStartOffset, FCurLineByteLen);
  if FLinesView.LogPhysConvertor.CurrentLine = ARealLine then begin
    if not FCharWidthsFromConverter then begin
      FCharWidthsFromConverter := True;
      FCharWidths2 := FCharWidths;
    end;
    FCharWidths :=FLinesView.LogPhysConvertor.CurrentWidthsDirect;
    FCharWidthsLen :=FLinesView.LogPhysConvertor.CurrentWidthsCount;
  end
  else begin
    if FCharWidthsFromConverter then begin
      FCharWidths := FCharWidths2;
      FCharWidths2 := nil;
      FCharWidthsFromConverter := False;
    end;
    FLinesView.GetPhysicalCharWidths(ARealLine, FCharWidths, FCharWidthsLen);
  end;
  FCurLineByteLen := FCurLineByteLen + LogLeftPos - 1;

  FCurViewToken.TokenLength     := 0;
  FCurViewScannerPos.Logical   := LogLeftPos;
  FCurViewScannerPos.Physical  := FCurLinePhysStartOffset; // FCurLinePhysStartOffset still one based, not yet offset
  FCurViewScannerPos.Offset    := 0;
  FCurViewScannerPhysCharPos  := FCurLinePhysStartOffset;
  FCurViewinRTL := False;

  dec(FCurLinePhysStartOffset); // 0 based
  FVirtualFirstCol := FCurLinePhysStartOffset + FFirstCol;
  FVirtualLastCol  := FCurLinePhysStartOffset + FLastCol;

  FNextMarkupPhysPos := low(FNextMarkupPhysPos); // Marker for first call to GetNextHighlighterTokenEx
  FNextMarkupLogPos  := -1;
  FCurTxtLineIdx     := ARealLine;
  FIsLastViewedSubLine := FCurLineByteLen >= FCharWidthsLen;
  FTokenOrigin     := dtoVirtualText;
end;

function TLazSynPaintTokenBreaker.GetNextHighlighterTokenEx(out
  ATokenInfo: TLazSynDisplayTokenInfoEx): Boolean;
const
  Space = '  ';
begin
  if FNextMarkupPhysPos = low(FNextMarkupPhysPos) then begin
    InitCurMarkup;
    FNextMarkupPhysPos := -1;
  end;

  FTokenOrigin := FCurViewToken.TokenOrigin;

  if FTokenOrigin in [dtoAfterWrapped] then begin
    FNextMarkupPhysPos := MaxInt;
    FNextMarkupLogPos := MaxInt;
  end
  else
  begin
    if (FNextMarkupPhysPos < 0) or
       (FCurMarkupNextRtlInfo.IsRtl       and (FNextMarkupPhysPos >= FCurMarkupNextStart.Physical)) or
       ((not FCurMarkupNextRtlInfo.IsRtl) and (FNextMarkupPhysPos <= FCurMarkupNextStart.Physical)) or
       (FNextMarkupLogPos < 0) or (FNextMarkupLogPos <= FCurMarkupNextStart.Logical)
    then begin
      FMarkupManager.GetNextMarkupColAfterRowCol(FCurTxtLineIdx+1,
        FCurMarkupNextStart, FCurMarkupNextRtlInfo, FNextMarkupPhysPos, FNextMarkupLogPos);

      if FNextMarkupPhysPos < 1 then
        if FCurMarkupNextRtlInfo.IsRtl
        then FNextMarkupPhysPos := 1
        else FNextMarkupPhysPos := MaxInt;
      if FNextMarkupLogPos < 1 then
        FNextMarkupLogPos := MaxInt;
    end;

    if (FTokenOrigin <> dtoAfterText) and (FCurLineByteLen < FCharWidthsLen) and
       (FNextMarkupLogPos > FCurLineByteLen + 1)
    then
      FNextMarkupLogPos := FCurLineByteLen + 1; // stop at WrapEnd / EOL // tokens should have a bound there anyway
  end;

  ATokenInfo.Attr := nil;
  Result := GetNextHighlighterTokenFromView(ATokenInfo, FNextMarkupPhysPos, FNextMarkupLogPos);

  if (not Result) or (FTokenOrigin = dtoBeforeText) then begin
    if ATokenInfo.Tk.TokenStart = nil then begin
      ATokenInfo.Tk.TokenStart      := @Space[1];
      ATokenInfo.Tk.TokenLength     := 1;
    end;

    if FTokenOrigin = dtoBeforeText then begin
      assert((FNextMarkupPhysPos <= 0) or (FNextMarkupPhysPos > ATokenInfo.StartPos.Physical), 'FNextMarkupPhysPos > ATokenInfo.StartPos.Physical');
      assert(ATokenInfo.EndPos.Physical > ATokenInfo.StartPos.Physical, 'ATokenInfo.EndPos.Physical > ATokenInfo.StartPos.Physical');
    end
    else begin
      // the first run StartPos is set by GetNextHighlighterTokenFromView
      if FTokenOrigin in [dtoAfterText, dtoAfterWrapped] then
        ATokenInfo.StartPos   := FCurMarkupNextStart
      else
      if FVirtualFirstCol > ATokenInfo.StartPos.Physical then begin
        ATokenInfo.StartPos.Logical := ATokenInfo.StartPos.Logical + (FVirtualFirstCol - ATokenInfo.StartPos.Physical);
        ATokenInfo.StartPos.Physical := FVirtualFirstCol;
      end;

      Result := (ATokenInfo.StartPos.Physical < FVirtualLastCol);
      if not Result then
        exit;
      assert((FNextMarkupPhysPos <= 0) or (FNextMarkupPhysPos > ATokenInfo.StartPos.Physical), 'FNextMarkupPhysPos > ATokenInfo.StartPos.Physical');
      assert(FTokenOrigin in [dtoAfterText, dtoAfterWrapped], 'TLazSynPaintTokenBreaker.GetNextHighlighterTokenEx: FTokenOrigin in [dtoAfterText, dtoAfterWrapped]');

      if FNextMarkupPhysPos > 0
      then ATokenInfo.EndPos.Physical    := Min(FNextMarkupPhysPos, FVirtualLastCol)
      else ATokenInfo.EndPos.Physical    := FVirtualLastCol;
      ATokenInfo.EndPos.Offset      := 0;
      ATokenInfo.EndPos.Logical     := ATokenInfo.StartPos.Logical + (ATokenInfo.EndPos.Physical - ATokenInfo.StartPos.Physical);

      if (FNextMarkupLogPos > 0) and (FNextMarkupLogPos < ATokenInfo.EndPos.Logical) then begin
        ATokenInfo.EndPos.Physical := ATokenInfo.EndPos.Physical - (ATokenInfo.EndPos.Logical - FNextMarkupLogPos);
        ATokenInfo.EndPos.Logical  := FNextMarkupLogPos;
      end;
      assert(ATokenInfo.EndPos.Physical > ATokenInfo.StartPos.Physical, 'ATokenInfo.EndPos.Physical > ATokenInfo.StartPos.Physical');
      assert(ATokenInfo.EndPos.Logical > ATokenInfo.StartPos.Logical, 'ATokenInfo.EndPos.Logical > ATokenInfo.StartPos.Logical');

      FCurMarkupNextStart := ATokenInfo.EndPos;
      if FCurMarkupNextRtlInfo.IsRtl then begin
        FNextMarkupPhysPos := -1;
        FNextMarkupLogPos  := -1;
      end;
      FCurMarkupNextRtlInfo.IsRtl := False;

      ATokenInfo.PhysicalCharStart  := ATokenInfo.StartPos.Physical - FCurLinePhysStartOffset;
      ATokenInfo.PhysicalClipStart  := ATokenInfo.StartPos.Physical - FCurLinePhysStartOffset;
      ATokenInfo.PhysicalCharEnd    := ATokenInfo.EndPos.Physical - FCurLinePhysStartOffset;
      ATokenInfo.PhysicalClipEnd    := ATokenInfo.EndPos.Physical - FCurLinePhysStartOffset;
      ATokenInfo.RtlInfo.IsRtl      := False;
    end;

    FMarkupTokenAttr.Clear;
    if ATokenInfo.Attr <> nil then begin
      FMarkupTokenAttr.Assign(ATokenInfo.Attr);
      if FTokenOrigin = dtoBeforeText then begin
        FMarkupTokenAttr.CurrentStartX := FWrapStartBound;
        FMarkupTokenAttr.CurrentEndX   := FWrapEndBound;
      end;
    end
    else begin
      FMarkupTokenAttr.Foreground := FForegroundColor;
      FMarkupTokenAttr.Background := FBackgroundColor;
    end;

    ATokenInfo.ExpandedExtraBytes := 0;
    ATokenInfo.HasTabs            := False;
    ATokenInfo.HasDoubleWidth     := False; // TODO: True, but needs charwidth for painter
    ATokenInfo.NeedsEto           := False;
  end
  else begin
    if ATokenInfo.NextRtlInfo.IsRtl <> FCurMarkupNextRtlInfo.IsRtl then begin
      FNextMarkupPhysPos := -1;
      FNextMarkupLogPos  := -1;
    end;
    FCurMarkupNextStart   := ATokenInfo.NextPos;
    FCurMarkupNextRtlInfo := ATokenInfo.NextRtlInfo;

    FMarkupTokenAttr.Assign(ATokenInfo.Attr);
    FMarkupTokenAttr.CurrentStartX := ATokenInfo.StartPos; // current sub-token
    FMarkupTokenAttr.CurrentEndX   := ATokenInfo.EndPos;
  end;

  if FTokenOrigin = dtoAfterWrapped then
    fMarkupManager.MergeMarkupAttributeAtWrapEnd(FCurTxtLineIdx + 1,
      FWrapEndBound, FMarkupTokenAttr)
  else
  if FTokenOrigin <> dtoBeforeText then
    fMarkupManager.MergeMarkupAttributeAtRowCol(FCurTxtLineIdx + 1,
      ATokenInfo.StartPos, ATokenInfo.EndPos, ATokenInfo.RtlInfo, FMarkupTokenAttr);
  FMarkupTokenAttr.ProcessMergeInfo;


  ATokenInfo.Attr := FMarkupTokenAttr;
  // Deal with equal colors
  // TODO: Map to RGB first
  if (FMarkupTokenAttr.Background = FMarkupTokenAttr.Foreground) then begin // or if diff(gb,fg) < x
    if FMarkupTokenAttr.Background = BackgroundColor then
      FMarkupTokenAttr.Foreground := not(FMarkupTokenAttr.Background) and $00ffffff // or maybe ForegroundColor ?
    else
      FMarkupTokenAttr.Foreground := BackgroundColor;
  end;

  // Todo merge attribute

end;

function TLazSynPaintTokenBreaker.GetNextHighlighterTokenFromView(out
  ATokenInfo: TLazSynDisplayTokenInfoEx; APhysEnd: Integer; ALogEnd: Integer): Boolean;

  procedure InitSynAttr(var ATarget: TSynSelectedColorMergeResult; const ASource: TLazCustomEditTextAttribute;
    const AnAttrStartX: TLazSynDisplayTokenBound);
  const
    NoEnd: TLazSynDisplayTokenBound = (Physical: -1; Logical: -1; Offset: 0);
  begin
    ATarget.Clear;
    if Assigned(ASource) then begin
      ATarget.Assign(ASource);
      if ATarget.Foreground = clNone then
        ATarget.Foreground := ForegroundColor;
      if ATarget.Background = clNone then
        ATarget.Background := BackgroundColor;
    end else
    begin
      ATarget.Foreground := ForegroundColor;
      ATarget.Background := BackgroundColor;
      ATarget.Style :=  []; // Font.Style; // currently always cleared
    end;
//    ATarget.MergeFinalStyle := True;
    if not ATarget.StartX.HasValue then
      ATarget.StartX := AnAttrStartX;
    if not ATarget.EndX.HasValue then
      ATarget.EndX   := NoEnd;
  end;

  function MaybeFetchToken: Boolean; inline;
  var
    CurTokenOrigin: TLazSynDisplayTokenOrigin;
  begin
    Result := FCurViewToken.TokenLength > 0;
    if Result or (FCurViewToken.TokenLength < 0) then exit;
    FCurViewCurTokenStartPos := FCurViewScannerPos;
    CurTokenOrigin := FCurViewToken.TokenOrigin;
    while FCurViewToken.TokenLength = 0 do begin // Todo: is SyncroEd-test a zero size token is returned
      Result := FDisplayView.GetNextHighlighterToken(FCurViewToken);
      if (not (CurTokenOrigin in [dtoAfterText, dtoAfterWrapped])) and
         (FCurViewToken.TokenOrigin in [dtoAfterText, dtoAfterWrapped])
      then
        FWrapEndBound := FCurViewScannerPos;

      if not Result then begin
        FCurViewToken.TokenStart := nil;
        FCurViewToken.TokenAttr := nil;
        FCurViewToken.TokenOrigin := dtoAfterText;
      end
      else
        Result := Result and (FCurViewToken.TokenStart <> nil); // False for end of line token
      if not Result then begin
        FCurViewToken.TokenLength := -1;
        exit;
      end;
      // Todo: concatenate with next token, if possible (only, if reaching token end)
    end;
  end;

  function GetCharWidthData(AIdx: Integer): TPhysicalCharWidth; inline;
  begin
    if (AIdx >= FCharWidthsLen) or (AIdx >= FCurLineByteLen)
    then Result := 1
    else Result := FCharWidths[AIdx];
  end;

  Procedure AdjustCurTokenLogStart(ANewLogStart: Integer); inline;
  // ANewLogStart = 1 based
  var
    j: integer;
  begin
    j := (ANewLogStart - FCurViewScannerPos.Logical);
    FCurViewToken.TokenLength := FCurViewToken.TokenLength - j;
    FCurViewToken.TokenStart  := FCurViewToken.TokenStart + j;
    FCurViewScannerPos.Logical   := ANewLogStart;
  end;

  procedure SkipLtrBeforeFirstCol(var ALogicIdx: integer; ALogicEnd: Integer); inline;
  var
    j: Integer;
    pcw: TPhysicalCharWidth;
  begin
    if  (FCurViewScannerPhysCharPos >= FVirtualFirstCol) then
      exit;

    pcw := GetCharWidthData(ALogicIdx);
    if (pcw and PCWFlagRTL <> 0) then exit;

    j := (pcw and PCWMask);
    while (ALogicIdx < ALogicEnd) and (FCurViewScannerPhysCharPos + j <= FVirtualFirstCol) and
          (pcw and PCWFlagRTL = 0)
    do begin
      inc(FCurViewScannerPhysCharPos, j);
      repeat
        inc(ALogicIdx);
      until (ALogicIdx >= ALogicEnd) or
            (ALogicIdx >= FCharWidthsLen) or ((FCharWidths[ALogicIdx] and PCWMask) <> 0);

      pcw := GetCharWidthData(ALogicIdx);
      j := pcw and PCWMask;
    end;

    if ALogicIdx <> FCurViewScannerPos.Logical - 1 then begin
      AdjustCurTokenLogStart(ALogicIdx + 1);
      assert(FCurViewToken.TokenLength >= 0, 'FCurViewToken.TokenLength > 0');
    end;

    if FCurViewScannerPhysCharPos > FCurViewScannerPos.Physical then
      FCurViewScannerPos.Physical := FCurViewScannerPhysCharPos;
    if (FCurViewScannerPos.Physical < FVirtualFirstCol) and
       (FCurViewScannerPos.Physical + j > FVirtualFirstCol)
    then
      FCurViewScannerPos.Physical := FVirtualFirstCol;
  end;

  procedure SkipRtlOffScreen(var ALogicIdx: integer; ALogicEnd: Integer); inline;
  var
    j: Integer;
    pcw: TPhysicalCharWidth;
  begin
    if  (FCurViewScannerPhysCharPos <= FVirtualFirstCol) then begin
// TODO: end, if FCurViewRtlPhysEnd >= FVirtualLastCol;
      if ALogicIdx + FCurViewToken.TokenLength < FCurViewRtlLogEnd then begin
        if FCurViewToken.TokenLength > 0 then begin
          ALogicIdx := ALogicIdx + FCurViewToken.TokenLength;
          FCurViewScannerPos.Logical := FCurViewScannerPos.Logical + FCurViewToken.TokenLength;
          FCurViewToken.TokenLength := 0;
        end;
      end
      else begin
        j :=  FCurViewRtlLogEnd - ALogicIdx;
        FCurViewScannerPos.Logical   := FCurViewScannerPos.Logical + j;
        FCurViewToken.TokenStart  := FCurViewToken.TokenStart + j;
        FCurViewToken.TokenLength := FCurViewToken.TokenLength - j;
        ALogicIdx := ALogicIdx + j;
        FCurViewScannerPhysCharPos      := FCurViewRtlPhysEnd;
        FCurViewScannerPos.Physical := FCurViewRtlPhysEnd;
        assert(FCurViewScannerPos.Logical - 1 = FCurViewRtlLogEnd, 'SkipRtlOffScreen: FCurViewScannerPos.Logical = FCurViewRtlLogEnd');
      end;
      exit;
    end;

    if  (FCurViewScannerPhysCharPos <= FVirtualLastCol) then
      exit;

    pcw := GetCharWidthData(ALogicIdx);
    if (pcw and PCWFlagRTL = 0) then exit;

    j := (pcw and PCWMask);
    while (ALogicIdx < ALogicEnd) and (FCurViewScannerPhysCharPos - j >= FVirtualLastCol) and
          (pcw and PCWFlagRTL <> 0)
    do begin
      dec(FCurViewScannerPhysCharPos, j);
      repeat
        inc(ALogicIdx);
      until (ALogicIdx >= ALogicEnd) or
              (ALogicIdx >= FCharWidthsLen) or ((FCharWidths[ALogicIdx] and PCWMask) <> 0);

      pcw := GetCharWidthData(ALogicIdx);
      j := pcw and PCWMask;
    end;

    if ALogicIdx <> FCurViewScannerPos.Logical - 1 then begin
      AdjustCurTokenLogStart(ALogicIdx + 1);
      assert(FCurViewToken.TokenLength >= 0, 'FCurViewToken.TokenLength > 0');
    end;
    if FCurViewScannerPos.Physical > FVirtualLastCol then
      FCurViewScannerPos.Physical := FVirtualLastCol;
  end;

  procedure ChangeToRtl(ALogicIdx, ALogicEnd: Integer);
  var
    RtlRunPhysWidth, TabExtra, i, j: Integer;
    pcw: TPhysicalCharWidth;
    HasTabs, HasDouble: Boolean;
    c: Char;
  begin
    FCurViewRtlLogStart := ALogicIdx;
    pcw := GetCharWidthData(ALogicIdx);

    RtlRunPhysWidth := 0;
    i         := 0;
    HasDouble := False;
    HasTabs   := False;
    TabExtra  := 0; // Extra bytes needed for expanded Tab/Space(utf8 visible space/dot)
    j := (pcw and PCWMask);
    // must go over token bounds
    //while (ALogicIdx < ALogicEnd) and (pcw and PCWFlagRTL <> 0) do begin
    while (ALogicIdx < FCharWidthsLen) and (ALogicIdx < FCurLineByteLen) and (pcw and PCWFlagRTL <> 0) do begin
      inc(RtlRunPhysWidth, j);

      if (j <> 0) and (FCurViewToken.TokenStart <> nil) then begin
        c := (FCurViewToken.TokenStart + i)^;
        if c = #9  then begin
          HasTabs := True;
          inc(TabExtra, j-1 + FTabExtraByteCount);
        end
        else
        if j > 1 then
          HasDouble := True;
        if c = ' ' then
          inc(TabExtra, FSpaceExtraByteCount);
      end;

      repeat
        inc(ALogicIdx);
        inc(i);
      until (ALogicIdx >= FCurLineByteLen) or
            (ALogicIdx >= FCharWidthsLen) or ((FCharWidths[ALogicIdx] and PCWMask) <> 0);

      pcw := GetCharWidthData(ALogicIdx);
      j := pcw and PCWMask;
    end;

    FCurViewinRTL               := True;
    FCurViewRTLLogEnd           := ALogicIdx;
    FCurViewRtlPhysStart        := FCurViewScannerPhysCharPos;
    FCurViewRtlPhysEnd          := FCurViewScannerPhysCharPos + RtlRunPhysWidth;
    FCurViewScannerPhysCharPos  := FCurViewRtlPhysEnd;
    FCurViewScannerPos.Physical := FCurViewRtlPhysEnd;
    FCurViewRtlExpExtraBytes    := TabExtra;
    FCurViewRtlHasTabs          := HasTabs;
    FCurViewRtlHasDoubleWidth   := HasDouble;
  end;

  function MaybeChangeToRtl(ALogicIdx, ALogicEnd: Integer): boolean; inline;
  begin
    Result := (GetCharWidthData(ALogicIdx) and PCWFlagRTL) <> 0;
    if Result then
      ChangeToRtl(ALogicIdx, ALogicEnd);
  end;

  procedure ChangeToLtr(ALogicIdx, ALogicEnd: Integer);
  begin
    FCurViewinRTL := False;
    FCurViewScannerPhysCharPos      := FCurViewRtlPhysEnd;
    FCurViewScannerPos.Physical := FCurViewRtlPhysEnd;
  end;

  function MaybeChangeToLtr(ALogicIdx, ALogicEnd: Integer): boolean; inline;
  begin
    Result := (GetCharWidthData(ALogicIdx) and PCWFlagRTL) = 0;
    if Result then
      ChangeToLtr(ALogicIdx, ALogicEnd);
  end;

var
  i, j: Integer;
  pcw: TPhysicalCharWidth;
  c: Char;
  LogicIdx, LogicEnd, PhysPos: Integer;
  PrevLogicIdx, PrevPhysPos: Integer;
  PhysTokenStop: Integer;
  TabExtra: Integer;
  HasTabs, HasDouble, NeedsEto: Boolean;
  FrameStartPos: TLazSynDisplayTokenBound;
begin
  ATokenInfo := Default(TLazSynDisplayTokenInfoEx);
  ATokenInfo.Attr := nil;
  while True do begin
    Result := MaybeFetchToken;    // Get token from View/Highlighter
    FTokenOrigin := FCurViewToken.TokenOrigin;

    if (FTokenOrigin in [{dtoAfterText,} dtoAfterWrapped]) and (APhysEnd >= 0) then begin
      APhysEnd := MaxInt;
      ALogEnd  := MaxInt;
    end;

    if (FTokenOrigin = dtoBeforeText) then begin
      ATokenInfo.RtlInfo.IsRtl      := False;
      ATokenInfo.NextRtlInfo.IsRtl  := False;

      if APhysEnd < 0 then begin
        ATokenInfo.NextPos          := FCurViewScannerPos;
        ATokenInfo.NextPos.Offset   := 0;
        exit;
      end;

      i := FCurViewToken.TokenLength;
      j := FVirtualFirstCol - FCurViewScannerPhysCharPos;
      if j > 0 then begin
        j := min(j, i);
        i := i - j;
        FCurViewToken.TokenLength := i;
        if FCurViewToken.TokenStart <> nil then
          FCurViewToken.TokenStart :=  FCurViewToken.TokenStart + j;

        FVirtualFirstCol := FVirtualFirstCol - j;
        FVirtualLastCol := FVirtualLastCol - j;
        dec(FCurLinePhysStartOffset, j);
        if i = 0 then
          continue;
      end;

      ATokenInfo.Tk                 := FCurViewToken;
      ATokenInfo.StartPos           := FCurViewScannerPos;
      ATokenInfo.StartPos.Logical           := FCharWidthsLen + 1;
      ATokenInfo.StartPos.Offset    := 0;

      ATokenInfo.EndPos.Logical     := FCurViewScannerPos.Logical + i;
      ATokenInfo.EndPos.Logical     := FCharWidthsLen + 1 + i;
      ATokenInfo.EndPos.Physical    := FCurViewScannerPos.Physical + i; // min right col
      ATokenInfo.EndPos.Offset      := 0;

      ATokenInfo.PhysicalCharStart  := FCurViewScannerPhysCharPos - FCurLinePhysStartOffset;
      ATokenInfo.PhysicalClipStart  := ATokenInfo.StartPos.Physical - FCurLinePhysStartOffset;
      ATokenInfo.PhysicalCharEnd    := Min(FCurViewScannerPhysCharPos - FCurLinePhysStartOffset+ i, FVirtualFirstCol);
      ATokenInfo.PhysicalClipEnd    := Min(ATokenInfo.StartPos.Physical - FCurLinePhysStartOffset+ i, FVirtualFirstCol);

      FWrapStartBound := FCurViewScannerPos;
      FWrapEndBound.Logical  := -1;
      FWrapEndBound.Offset   := 0;
      FWrapEndBound.Physical := FCurViewScannerPos.Physical + i + (FCurViewScannerPhysCharPos - (FCurLinePhysStartOffset + 1));

      if FCurViewToken.TokenAttr <> nil then begin
        FrameStartPos.Logical := 0;
        FrameStartPos.Physical := 1 + FCurLinePhysStartOffset;
        FrameStartPos.Offset  := 0;
        InitSynAttr(FCurViewAttr, FCurViewToken.TokenAttr, FrameStartPos);
        if not FCurViewAttr.EndX.HasValue then
          FCurViewAttr.EndX := FWrapEndBound;
        ATokenInfo.Attr := FCurViewAttr;
      end
      else
        ATokenInfo.Attr := nil;

      FVirtualFirstCol := FVirtualFirstCol - i;
      FVirtualLastCol := FVirtualLastCol - i;
      dec(FCurLinePhysStartOffset, i);

      ATokenInfo.ExpandedExtraBytes := 0;
      ATokenInfo.HasTabs            := False; /////////
      ATokenInfo.HasDoubleWidth     := False;
      ATokenInfo.NeedsEto           := False;

      ATokenInfo.NextPos            := ATokenInfo.StartPos;

      FCurViewToken.TokenLength := 0;
      exit;
    end;

    if not Result then begin
      ATokenInfo.StartPos      := FCurViewScannerPos;
      ATokenInfo.RtlInfo.IsRtl := False;
      ATokenInfo.NextRtlInfo.IsRtl := False;
      ATokenInfo.Tk := FCurViewToken;
      if FCurViewToken.TokenAttr <> nil then begin
        InitSynAttr(FCurViewAttr, FCurViewToken.TokenAttr, FCurViewCurTokenStartPos);
        ATokenInfo.Attr := FCurViewAttr;
      end
      else
        ATokenInfo.Attr := nil;
      exit;
    end;

    LogicIdx := FCurViewScannerPos.Logical - 1;
    LogicEnd := LogicIdx + FCurViewToken.TokenLength;

    NeedsEto := False;
    case FCurViewinRTL of
      False: // Left To Right
        begin
          SkipLtrBeforeFirstCol(LogicIdx, LogicEnd);    // Skip out of screen
          if FCurViewToken.TokenLength = 0 then
            continue;  // Get NEXT token

          if MaybeChangeToRtl(LogicIdx, LogicEnd) then
            continue;

          if APhysEnd > 0
          then PhysTokenStop := Min(FVirtualLastCol, APhysEnd)
          else PhysTokenStop := FVirtualLastCol;
          // TODO: APhysEnd should always allow some data. Compare with FVirtualLastCol? Assert for APhysEnd
          Result := PhysTokenStop > FCurViewScannerPos.Physical;
          if not Result then begin
            ATokenInfo.StartPos           := FCurViewScannerPos;
            exit;
          end;

          // Find end according to PhysTokenStop
          PhysPos      := FCurViewScannerPhysCharPos;
          PrevLogicIdx := LogicIdx;
          PrevPhysPos  := PhysPos;
          HasDouble := False;
          HasTabs := False;
          TabExtra      := 0; // Extra bytes needed for expanded Tab/Space(utf8 visible space/dot)
          i := 0;

          if (ALogEnd > 0) and (LogicEnd >= ALogEnd) then
            LogicEnd := ALogEnd - 1;

          pcw := GetCharWidthData(LogicIdx);
          while (LogicIdx < LogicEnd) and (PhysPos < PhysTokenStop) and
                (pcw and PCWFlagRTL = 0)
          do begin
            j := pcw and PCWMask;

            PrevLogicIdx := LogicIdx;
            PrevPhysPos  := PhysPos;
            inc(PhysPos, j);
            if j <> 0 then begin
              c := (FCurViewToken.TokenStart + i)^;
              if c = #9  then begin
                HasTabs := True;
                inc(TabExtra, j-1 + FTabExtraByteCount);
              end
              else
              if j > 1 then
                HasDouble := True;
              if c = ' ' then begin
                inc(TabExtra, FSpaceExtraByteCount);
                {$IfDef WINDOWS}
                if not NeedsEto then
                  NeedsEto := IsCombiningCodePoint(FCurViewToken.TokenStart + i + 1);
                {$ENDIF}
              end;
            end;

            repeat
              inc(LogicIdx);
              inc(i);
            until (LogicIdx >= FCharWidthsLen) or
                  (LogicIdx >= LogicEnd) or ((FCharWidths[LogicIdx] and PCWMask) <> 0);
            pcw := GetCharWidthData(LogicIdx);
          end;
          Assert((PhysPos > FCurViewScannerPhysCharPos) or (ALogEnd > 0), 'PhysPos > FCurViewScannerPhysCharPos');

          ATokenInfo.Tk                 := FCurViewToken;
          ATokenInfo.Tk.TokenLength     := LogicIdx + 1 - FCurViewScannerPos.Logical;

          ATokenInfo.StartPos           := FCurViewScannerPos;
          ATokenInfo.StartPos.Offset    := ATokenInfo.StartPos.Physical - FCurViewScannerPhysCharPos; // >= 0

          ATokenInfo.EndPos.Logical     := LogicIdx + 1;
          ATokenInfo.EndPos.Physical    := Min(PhysPos, PhysTokenStop);
          ATokenInfo.EndPos.Offset      := ATokenInfo.EndPos.Physical - PhysPos; // Zero or Negative. Paint ends before Logical

          ATokenInfo.PhysicalCharStart  := FCurViewScannerPhysCharPos - FCurLinePhysStartOffset;
          ATokenInfo.PhysicalClipStart  := ATokenInfo.StartPos.Physical - FCurLinePhysStartOffset;
          ATokenInfo.PhysicalCharEnd    := PhysPos - FCurLinePhysStartOffset;
          ATokenInfo.PhysicalClipEnd    := ATokenInfo.EndPos.Physical - FCurLinePhysStartOffset;
          ATokenInfo.RtlInfo.IsRtl      := False;
          //ATokenInfo.RtlInfo.PhysLeft   := FCurViewRtlPhysStart;
          //ATokenInfo.RtlInfo.PhysRight  := FCurViewRtlPhysEnd;
          //ATokenInfo.RtlInfo.LogFirst   := FCurViewRtlLogStart + 1;
          //ATokenInfo.RtlInfo.LogLast    := FCurViewRtlLogEnd + 1;
          //ATokenInfo.RtlExpandedExtraBytes := FCurViewRtlExpExtraBytes;
          //ATokenInfo.RtlHasDoubleWidth  := FCurViewRtlHasDoubleWidth;
          ATokenInfo.Attr               := FCurViewAttr;

          ATokenInfo.ExpandedExtraBytes := TabExtra;
          ATokenInfo.HasTabs            := HasTabs;
          ATokenInfo.HasDoubleWidth     := HasDouble;
          ATokenInfo.NeedsEto           := NeedsEto;
          assert(ATokenInfo.StartPos.Offset >= 0, 'FCurViewScannerPos.Offset >= 0');
          assert(ATokenInfo.EndPos.Offset   <= 0, 'FCurViewToken.EndPos.Offset <= 0');

          if PhysPos > PhysTokenStop then begin      // Last char goes over paint boundary
            LogicIdx := PrevLogicIdx;
            PhysPos  := PrevPhysPos;
          end
          else
            PhysTokenStop := PhysPos;
          AdjustCurTokenLogStart(LogicIdx + 1);
          FCurViewScannerPhysCharPos   := PhysPos;
          if PhysTokenStop > FCurViewScannerPos.Physical  then
            FCurViewScannerPos.Physical := PhysTokenStop;

          assert(FCurViewToken.TokenLength >= 0, 'FCurViewToken.TokenLength >= 0');

          InitSynAttr(FCurViewAttr, FCurViewToken.TokenAttr, FCurViewCurTokenStartPos);
          if (FCurViewToken.TokenLength = 0) and
             (not ATokenInfo.Attr.EndX.HasValue)
          then
            ATokenInfo.Attr.EndX := ATokenInfo.EndPos; // PhysPos-1;

          MaybeFetchToken;
          if MaybeChangeToRtl(LogicIdx, LogicEnd) then begin // get NextTokenPhysStart
            SkipRtlOffScreen(LogicIdx, LogicEnd);
            while FCurViewToken.TokenLength = 0 do
              if MaybeFetchToken then
                SkipRtlOffScreen(LogicIdx, LogicEnd);
          end;

          ATokenInfo.NextPos.Physical      := FCurViewScannerPos.Physical;
          ATokenInfo.NextPos.Logical       := FCurViewScannerPos.Logical;
          ATokenInfo.NextPos.Offset        := FCurViewScannerPos.Physical - FCurViewScannerPhysCharPos;
          ATokenInfo.NextRtlInfo.IsRtl     := FCurViewinRTL;
          ATokenInfo.NextRtlInfo.PhysLeft  := FCurViewRtlPhysStart;
          ATokenInfo.NextRtlInfo.PhysRight := FCurViewRtlPhysEnd;
          ATokenInfo.NextRtlInfo.LogFirst  := FCurViewRtlLogStart + 1;
          ATokenInfo.NextRtlInfo.LogLast   := FCurViewRtlLogEnd + 1;

          break;
        end; // case FCurViewinRTL = False;
      True: // Right To Left
        begin
          SkipRtlOffScreen(LogicIdx, LogicEnd);
          if FCurViewToken.TokenLength = 0 then
            continue;  // Get NEXT token

          if MaybeChangeToLtr(LogicIdx, LogicEnd) then
            continue;

          if APhysEnd >= FCurViewRtlPhysEnd
          then PhysTokenStop := FVirtualFirstCol
          else PhysTokenStop := Max(FVirtualFirstCol, APhysEnd);
          // TODO: APhysEnd should always allow some data. Assert for APhysEnd
          // FVirtualFirstCol must be less PPS. Otherwise it would have gone LTR
//          Result := PhysTokenStop < FCurViewScannerPos.Physical;
//          if not Result then exit;

          // Find end according to PhysTokenStop
          PhysPos      := FCurViewScannerPhysCharPos;
          PrevLogicIdx := LogicIdx;
          PrevPhysPos  := PhysPos;
          HasDouble := False;
          HasTabs := False;
          TabExtra      := 0; // Extra bytes needed for expanded Tab/Space(utf8 visible space/dot)
          i := 0;

          if (ALogEnd > 0) and (LogicEnd >= ALogEnd) then
            LogicEnd := ALogEnd - 1;

          pcw := GetCharWidthData(LogicIdx);
          while (LogicIdx < LogicEnd) and (PhysPos > PhysTokenStop) and
                (pcw and PCWFlagRTL <> 0)
          do begin
            j := pcw and PCWMask;

            PrevLogicIdx := LogicIdx;
            PrevPhysPos  := PhysPos;
            dec(PhysPos, j);
            if j <> 0 then begin
              c := (FCurViewToken.TokenStart + i)^;
              if c = #9  then begin
                HasTabs := True;
                inc(TabExtra, j-1 + FTabExtraByteCount);
              end
              else
              if j > 1 then
                HasDouble := True;
              if c = ' ' then begin
                inc(TabExtra, FSpaceExtraByteCount);
                {$IfDef WINDOWS}
                if not NeedsEto then
                  NeedsEto := IsCombiningCodePoint(FCurViewToken.TokenStart + i + 1);
                {$ENDIF}
              end;
            end;

            repeat
              inc(LogicIdx);
              inc(i);
            until (LogicIdx >= FCharWidthsLen) or
                  (LogicIdx >= LogicEnd) or ((FCharWidths[LogicIdx] and PCWMask) <> 0);
            pcw := GetCharWidthData(LogicIdx);
          end;
          Assert((PhysPos < FCurViewScannerPhysCharPos) or (ALogEnd > 0), 'PhysPos > FCurViewScannerPhysCharPos');

          ATokenInfo.Tk                 := FCurViewToken;
          ATokenInfo.Tk.TokenLength     := LogicIdx + 1 - FCurViewScannerPos.Logical;

          ATokenInfo.StartPos           := FCurViewScannerPos;
          //ATokenInfo.StartPos.Logical   := LogicIdx + 1;
          //ATokenInfo.StartPos.Physical  := ATokenInfo.StartPos.Physical;
          ATokenInfo.StartPos.Offset    := FCurViewScannerPhysCharPos - ATokenInfo.StartPos.Physical; //  >= 0

          ATokenInfo.EndPos.Logical     := LogicIdx + 1;
          ATokenInfo.EndPos.Physical    := Max(PhysPos, PhysTokenStop);
          ATokenInfo.EndPos.Offset      := PhysPos - ATokenInfo.EndPos.Physical; //  <= 0

          ATokenInfo.PhysicalCharStart  := PhysPos - FCurLinePhysStartOffset;
          ATokenInfo.PhysicalClipStart  := ATokenInfo.EndPos.Physical - FCurLinePhysStartOffset;
          ATokenInfo.PhysicalCharEnd    := FCurViewScannerPhysCharPos - FCurLinePhysStartOffset;
          ATokenInfo.PhysicalClipEnd    := ATokenInfo.StartPos.Physical - FCurLinePhysStartOffset;
          ATokenInfo.RtlInfo.IsRtl      := True;
          ATokenInfo.RtlInfo.PhysLeft   := FCurViewRtlPhysStart;
          ATokenInfo.RtlInfo.PhysRight  := FCurViewRtlPhysEnd;
          ATokenInfo.RtlInfo.LogFirst   := FCurViewRtlLogStart + 1;
          ATokenInfo.RtlInfo.LogLast    := FCurViewRtlLogEnd + 1;
          ATokenInfo.RtlExpandedExtraBytes := FCurViewRtlExpExtraBytes;
          ATokenInfo.RtlHasTabs         := FCurViewRtlHasTabs;
          ATokenInfo.RtlHasDoubleWidth  := FCurViewRtlHasDoubleWidth;
          ATokenInfo.Attr               := FCurViewAttr;

          ATokenInfo.ExpandedExtraBytes := TabExtra;
          ATokenInfo.HasTabs            := HasTabs;
          ATokenInfo.HasDoubleWidth     := HasDouble;
          ATokenInfo.NeedsEto           := NeedsEto;
          assert(ATokenInfo.StartPos.Offset >= 0, 'FCurViewScannerPos.Offset >= 0');
          assert(ATokenInfo.EndPos.Offset   <= 0, 'FCurViewToken.EndPos.Offset <= 0');

          if (PhysPos < PhysTokenStop) and (PhysTokenStop > FVirtualFirstCol) then begin      // Last char goes over paint boundary
            LogicIdx := PrevLogicIdx;
            PhysPos  := PrevPhysPos;
          end
          else
            PhysTokenStop := Max(PhysPos, PhysTokenStop);

          AdjustCurTokenLogStart(LogicIdx + 1);
          FCurViewScannerPhysCharPos   := PhysPos;
          if PhysTokenStop < FCurViewScannerPos.Physical then
            FCurViewScannerPos.Physical := PhysTokenStop;

          assert(FCurViewToken.TokenLength >= 0, 'FCurViewToken.TokenLength >= 0');

          InitSynAttr(FCurViewAttr, FCurViewToken.TokenAttr, FCurViewCurTokenStartPos);
          if (FCurViewToken.TokenLength = 0) and
             (not ATokenInfo.Attr.EndX.HasValue)
          then
            ATokenInfo.Attr.EndX := ATokenInfo.EndPos; // PhysPos-1;

          MaybeFetchToken;
          SkipRtlOffScreen(LogicIdx, LogicEnd);
          while FCurViewToken.TokenLength = 0 do
            if MaybeFetchToken then
              SkipRtlOffScreen(LogicIdx, LogicEnd);
          MaybeChangeToLtr(LogicIdx, LogicEnd);  // get NextTokenPhysStart

          // If the next token is RTL, then NextPos is the next EndPos
          ATokenInfo.NextPos.Physical      := FCurViewScannerPos.Physical;
          ATokenInfo.NextPos.Logical       := FCurViewScannerPos.Logical;
          ATokenInfo.NextPos.Offset        := FCurViewScannerPhysCharPos - FCurViewScannerPos.Physical;
          ATokenInfo.NextRtlInfo.IsRtl     := FCurViewinRTL;
          ATokenInfo.NextRtlInfo.PhysLeft  := FCurViewRtlPhysStart;
          ATokenInfo.NextRtlInfo.PhysRight := FCurViewRtlPhysEnd;
          ATokenInfo.NextRtlInfo.LogFirst  := FCurViewRtlLogStart + 1;
          ATokenInfo.NextRtlInfo.LogLast   := FCurViewRtlLogEnd + 1;

          break;
        end; // case FCurViewinRTL = True;
    end;


  end; // while True
end;

{ TLazSynSurfaceWithText }

procedure TLazSynSurfaceWithText.SetTextArea(AValue: TLazSynTextArea);
begin
  FTextArea := AValue;
end;

function TLazSynSurfaceWithText.GetTextArea: TLazSynTextArea;
begin
  Result := FTextArea;
end;

procedure TLazSynSurfaceWithText.Assign(Src: TLazSynSurface);
begin
  inherited Assign(Src);
  FTextArea := TLazSynSurfaceWithText(Src).FTextArea;
end;

{ TLazSynSurfaceManager }

procedure TLazSynSurfaceManager.SetLeftGutterWidth(AValue: integer);
begin
  if FLeftGutterWidth = AValue then Exit;
  FLeftGutterWidth := AValue;
  BoundsChanged;
end;

procedure TLazSynSurfaceManager.SetPadding(Side: TLazSynBorderSide; AValue: integer);
begin
  FTextArea.Padding[Side] := AValue;
end;

procedure TLazSynSurfaceManager.SetRightEdgeColor(AValue: TColor);
begin
  FTextArea.RightEdgeColor := AValue;
end;

procedure TLazSynSurfaceManager.SetRightEdgeColumn(AValue: integer);
begin
  FTextArea.RightEdgeColumn := AValue;
end;

procedure TLazSynSurfaceManager.SetRightEdgeVisible(AValue: boolean);
begin
  FTextArea.RightEdgeVisible := AValue;
end;

procedure TLazSynSurfaceManager.SetLeftGutterArea(AValue: TLazSynSurfaceWithText);
begin
  if FLeftGutterArea = AValue then Exit;
  FLeftGutterArea := AValue;
  FLeftGutterArea.DisplayView := DisplayView;
  FLeftGutterArea.TextArea := FTextArea;
end;

function TLazSynSurfaceManager.GetLeftGutterArea: TLazSynSurfaceWithText;
begin
  Result := FLeftGutterArea;
end;

function TLazSynSurfaceManager.GetRightGutterArea: TLazSynSurfaceWithText;
begin
  Result := FRightGutterArea;
end;

function TLazSynSurfaceManager.GetTextArea: TLazSynTextArea;
begin
  Result := FTextArea;
end;

procedure TLazSynSurfaceManager.SetBackgroundColor(AValue: TColor);
begin
  FTextArea.BackgroundColor := AValue;
end;

procedure TLazSynSurfaceManager.SetExtraCharSpacing(AValue: integer);
begin
  FTextArea.ExtraCharSpacing := AValue;
end;

procedure TLazSynSurfaceManager.SetExtraLineSpacing(AValue: integer);
begin
  FTextArea.ExtraLineSpacing := AValue;
end;

procedure TLazSynSurfaceManager.SetForegroundColor(AValue: TColor);
begin
  FTextArea.ForegroundColor := AValue;
end;

procedure TLazSynSurfaceManager.SetRightGutterArea(AValue: TLazSynSurfaceWithText);
begin
  if FRightGutterArea = AValue then Exit;
  FRightGutterArea := AValue;
  FRightGutterArea.DisplayView := DisplayView;
  FLeftGutterArea.TextArea := FTextArea;
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
  if FLeftGutterArea <> nil then
    FLeftGutterArea.TextArea := FTextArea;
  if FRightGutterArea <> nil then
    FRightGutterArea.TextArea := FTextArea;
end;

procedure TLazSynSurfaceManager.SetVisibleSpecialChars(AValue: TSynVisibleSpecialChars);
begin
  FTextArea.VisibleSpecialChars := AValue;
end;

procedure TLazSynSurfaceManager.SetHighlighter(AValue: TSynCustomHighlighter);
begin
  FTextArea.Highlighter := AValue;
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

constructor TLazSynSurfaceManager.Create(AOwner: TSynEditBase);
begin
  inherited Create(AOwner);
  FLeftGutterWidth := 0;
  FRightGutterWidth := 0;
end;

procedure TLazSynSurfaceManager.InvalidateLines(FirstTextLine,
  LastTextLine: TLineIdx; AScreenLineOffset: Integer);
var
  rcInval: TRect;
  ViewedRange: TLineRange;
begin
  rcInval := Bounds;
  if (FirstTextLine >= 0) then begin
    ViewedRange := DisplayView.TextToViewIndex(FirstTextLine);
    rcInval.Top := Max(TextArea.TextBounds.Top,
                       TextArea.TextBounds.Top + (ViewedRange.Top + AScreenLineOffset
                          - TextArea.TopViewedLine + 1) * TextArea.LineHeight);
  end;
  if (LastTextLine >= 0) then begin
    if LastTextLine <> FirstTextLine then
      ViewedRange := DisplayView.TextToViewIndex(LastTextLine);
    rcInval.Bottom := Min(TextArea.TextBounds.Bottom,
                          TextArea.TextBounds.Top + ({%H-}ViewedRange.Bottom + AScreenLineOffset
                             - TextArea.TopViewedLine + 2)  * TextArea.LineHeight);
  end;

  {$IFDEF VerboseSynEditInvalidate}
  DebugLn(['TCustomSynEdit.InvalidateGutterLines ',DbgSName(self), ' FirstLine=',FirstTextLine, ' LastLine=',LastTextLine, ' rect=',dbgs(rcInval)]);
  {$ENDIF}
  if (rcInval.Top < rcInval.Bottom) and (rcInval.Left < rcInval.Right) then
    InvalidateRect(Handle, @rcInval, FALSE);
end;

procedure TLazSynSurfaceManager.InvalidateTextLines(FirstTextLine,
  LastTextLine: TLineIdx; AScreenLineOffset: Integer);
begin
  FTextArea.InvalidateLines(FirstTextLine, LastTextLine, AScreenLineOffset);
end;

procedure TLazSynSurfaceManager.InvalidateGutterLines(FirstTextLine,
  LastTextLine: TLineIdx; AScreenLineOffset: Integer);
begin
  FLeftGutterArea.InvalidateLines(FirstTextLine, LastTextLine, AScreenLineOffset);
  FRightGutterArea.InvalidateLines(FirstTextLine, LastTextLine, AScreenLineOffset);
end;

{ TLazSynTextArea }

function TLazSynTextArea.GetPadding(Side: TLazSynBorderSide): integer;
begin
  Result := FPadding[Side];
end;

function TLazSynTextArea.GetExtraCharSpacing: integer;
begin
  Result := Max(FExtraCharSpacing, -CharWidth+1);
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
  FCurrentExtraLineSpacing := Max(AValue, -FTextDrawer.CharHeight+1);
  FTextHeight := FTextDrawer.CharHeight + FCurrentExtraLineSpacing;
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

procedure TLazSynTextArea.SetViewedTopLine(AValue: TLinePos);
begin
  if AValue < 1 then AValue := 1;
  if FViewedTopLine = AValue then Exit;
  FViewedTopLine := AValue;
end;

procedure TLazSynTextArea.DoDrawerFontChanged(Sender: TObject; Changes: TSynStatusChanges);
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

function TLazSynTextArea.RowColumnToPixels(const RowCol: TScreenPoint_0): TPoint;
begin
  // Inludes LeftChar, but not TopViewedline
  Result.X := FTextBounds.Left + (RowCol.X - LeftChar) * CharWidth;
  Result.Y := FTextBounds.Top + RowCol.Y * LineHeight;
end;

function TLazSynTextArea.PixelsToRowColumn(Pixels: TPoint;
  aFlags: TSynCoordinateMappingFlags): TPhysScreenPoint_0;
begin
  // Inludes LeftChar, but not TopViewedline
  if (Pixels.X >= FTextBounds.Left) and (Pixels.X < FTextBounds.Right) then begin
    if not (scmForceLeftSidePos in aFlags) then
      Pixels.X := Pixels.X +  (CharWidth div 2);  // nearest side of char
    Result.X := (Pixels.X - FTextBounds.Left) div CharWidth
                + LeftChar;
  end
  else
    Result.X := 0;
  Result.Y := (Pixels.Y - FTextBounds.Top) div LineHeight;

  if (not(scmIncludePartVisible in aFlags)) and (Result.Y >= LinesInWindow) then begin
    // don't return a partially visible last line
    Result.Y := LinesInWindow - 1;
  end;
  if Result.X < 0 then Result.X := 0;
  if Result.Y < 0 then Result.Y := 0;
end;

constructor TLazSynTextArea.Create(AOwner: TSynEditBase; ATextDrawer: TLazEditTextGridPainter);
var
  i: TLazSynBorderSide;
begin
  inherited Create(AOwner);
  FTextSizeChangeList := TMethodList.Create;
  FTokenBreaker := TLazSynPaintTokenBreaker.Create;
  FTextDrawer := ATextDrawer;

  Owner.RegisterStatusChangedHandler(@DoDrawerFontChanged, [scFontOrStyleChanged]);
  FPaintLineColor := TSynSelectedColor.Create;
  FPaintLineColor2 := TSynSelectedColor.Create;
  for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do
    FPadding[i] := 0;
  FViewedTopLine := 1;
  FLeftChar := 1;
  FRightEdgeColumn  := 80;
  FRightEdgeVisible := True;
  FRightEdgeColor   := clSilver;
  FontChanged;
end;

destructor TLazSynTextArea.Destroy;
begin
  FreeAndNil(FTokenBreaker);
  Owner.UnRegisterStatusChangedHandler(@DoDrawerFontChanged);
  FreeAndNil(FPaintLineColor);
  FreeAndNil(FPaintLineColor2);
  FreeAndNil(FTextSizeChangeList);
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
  FCurrentExtraLineSpacing := TLazSynTextArea(Src).FCurrentExtraLineSpacing;
  FVisibleSpecialChars := TLazSynTextArea(Src).FVisibleSpecialChars;
  FRightEdgeColumn := TLazSynTextArea(Src).FRightEdgeColumn;
  FRightEdgeVisible := TLazSynTextArea(Src).FRightEdgeVisible;

  for i := low(TLazSynBorderSide) to high(TLazSynBorderSide) do
    FPadding[i] := TLazSynTextArea(Src).FPadding[i];

  FViewedTopLine := TLazSynTextArea(Src).FViewedTopLine;
  FLeftChar := TLazSynTextArea(Src).FLeftChar;

  BoundsChanged;
end;

procedure TLazSynTextArea.InvalidateLines(FirstTextLine,
  LastTextLine: TLineIdx; AScreenLineOffset: Integer);
var
  rcInval: TRect;
  ViewedRange: TLineRange;
begin
  rcInval := Bounds;
  if (FirstTextLine >= 0) then begin
    ViewedRange := DisplayView.TextToViewIndex(FirstTextLine);
    rcInval.Top := Max(TextBounds.Top,
                       TextBounds.Top + (ViewedRange.Top + AScreenLineOffset - TopViewedLine + 1) * LineHeight);
  end;
  if (LastTextLine >= 0) then begin
    if LastTextLine <> FirstTextLine then
      ViewedRange := DisplayView.TextToViewIndex(LastTextLine);
    rcInval.Bottom := Min(TextBounds.Bottom,
                          TextBounds.Top + ({%H-}ViewedRange.Bottom + AScreenLineOffset - TopViewedLine + 2)  * LineHeight);
  end;

  {$IFDEF VerboseSynEditInvalidate}
  DebugLn(['TCustomSynEdit.InvalidateTextLines ',DbgSName(self), ' FirstLine=',FirstTextLine, ' LastLine=',LastTextLine, ' rect=',dbgs(rcInval)]);
  {$ENDIF}
  if (rcInval.Top < rcInval.Bottom) and (rcInval.Left < rcInval.Right) then
    InvalidateRect(Handle, @rcInval, FALSE);
end;

procedure TLazSynTextArea.FontChanged;
var
  OldChars, OldLines: Integer;
  Chg: TSynStatusChanges;
  t: LongInt;
begin
  // ToDo: wait for handle creation
  // Report FLinesInWindow=-1 if no handle
  FCharWidth := FTextDrawer.CharWidth;  // includes extra
  FTextHeight := FTextDrawer.CharHeight + ExtraLineSpacing;

  OldChars := FCharsInWindow;
  OldLines := FLinesInWindow;
  FCharsInWindow :=  0;
  FLinesInWindow :=  0;
  FHasPartiallyVisibleLastLine := False;
  if FCharWidth > 0 then
    FCharsInWindow := Max(0, (FTextBounds.Right - FTextBounds.Left) div FCharWidth);
  if FTextHeight > 0 then begin
    DivMod(Max(0, FTextBounds.Bottom - FTextBounds.Top), FTextHeight, FLinesInWindow, t);
    FHasPartiallyVisibleLastLine := t > 0;
  end;

  if assigned(fOnStatusChange) then begin
    Chg := [];
    if OldChars <> FCharsInWindow then
      Chg := Chg + [scCharsInWindow];
    if OldLines <> FLinesInWindow then
      Chg := Chg + [scLinesInWindow];
    if (Chg <> []) then
      fOnStatusChange(Self, Chg);
  end;
  FTextSizeChangeList.CallNotifyEvents(Self);
end;

procedure TLazSynTextArea.AddTextSizeChangeHandler(AHandler: TNotifyEvent);
begin
  FTextSizeChangeList.Add(TMethod(AHandler));
end;

procedure TLazSynTextArea.RemoveTextSizeChangeHandler(AHandler: TNotifyEvent);
begin
  FTextSizeChangeList.Remove(TMethod(AHandler));
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
    IntersectRect(PadRect{%H-}, AClip, PadRect2);
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
  CharWidths: TPhysicalCharWidths;
  CWLen: Integer;

  var
    LineBuffer: PChar;
    LineBufferLen: Integer;
    LineBufferRtlLogPos: Integer;

  procedure DrawHiLightMarkupToken(ATokenInfo: TLazSynDisplayTokenInfoEx);
  var
    HasFrame: Boolean;
    s: TLazSynBorderSide;
    Attr: TSynSelectedColorMergeResult;
    TxtFlags: Integer;
    tok: TRect;
    c, i, j, k, e, Len: Integer;
    pl, pt: PChar;
    TxtLeft: Integer;
    NeedExpansion, NeedTransform: Boolean;
    cl: TColor;
  begin
    Attr := ATokenInfo.Attr;
    cl := Attr.Foreground; if cl = clDefault then cl := ForegroundColor;
    FTextDrawer.ForeColor := cl;
    cl := Attr.Background; if cl = clDefault then cl := BackgroundColor;
    FTextDrawer.BackColor := cl;
    FTextDrawer.Style     := Attr.Style;
    HasFrame := False;
    for s := low(TLazSynBorderSide) to high(TLazSynBorderSide) do begin
      HasFrame := HasFrame or
        ( (Attr.FrameSideColors[s] <> clNone) and (Attr.FrameSideColors[s] <> clDefault) );
      FTextDrawer.FrameColor[s] := Attr.FrameSideColors[s];
      FTextDrawer.FrameStyle[s] := Attr.FrameSideStyles[s];
    end;

    rcToken.Right := ScreenColumnToXValue(ATokenInfo.PhysicalClipEnd);
    if rcToken.Right > AClip.Right then begin
      rcToken.Right := AClip.Right;
      FTextDrawer.FrameColor[bsRight] := clNone; // right side of char is not painted
    end;

    //if (rcToken.Right <= rcToken.Left) then exit;
    rcToken.Left := ScreenColumnToXValue(ATokenInfo.PhysicalClipStart); // because for the first token, this can be middle of a char, and lead to wrong frame

    (* rcToken.Bottom may be less that crLine.Bottom. If a Divider was drawn, then RcToken will not contain it *)
    TxtFlags := ETO_OPAQUE;

    (* If token includes RightEdge, draw background, and edge first *)
    if bDoRightEdge and (nRightEdge<rcToken.Right) and (nRightEdge>=rcToken.Left)
    then begin
      TxtFlags := 0;
      if rcToken.Left < nRightEdge then begin
        // draw background left of edge (use rcToken, so we do not delete the divider-draw-line)
        tok := rcToken;
        tok.Right := nRightEdge;
        FTextDrawer.FillRect(tok);
      end;
      if rcToken.Right > nRightEdge then begin
        // draw background right of edge (use rcLine, full height)
        tok := rcToken;
        tok.Left   := nRightEdge;
        tok.Bottom := rcLine.Bottom;
        FTextDrawer.FillRect(tok);
      end;
      // draw edge (use rcLine / rcToken may be reduced)
      FTextDrawer.DrawLine(nRightEdge, rcLine.Top, nRightEdge, rcLine.Bottom+1, ColorToRGB(RightEdgeColor));
    end
    else
    if HasFrame then begin
      (* Draw background for frame *)
      TxtFlags := 0;
      tok := rcToken;
      if rcToken.Right > nRightEdge + 1 then
        tok.Bottom := rcLine.Bottom;
      FTextDrawer.FillRect(tok);
    end;

    if HasFrame then begin
      // draw frame
      tok := rcToken;
      if rcToken.Right > nRightEdge + 1 then
        tok.Bottom := rcLine.Bottom;
      FTextDrawer.DrawFrame(tok);
    end;

    if ATokenInfo.RtlInfo.IsRtl then begin
      // Always draw the entire RTL run, to keep weak chars in context. Alternatively, could use ETO_RTLREADING
      // Need ETO_RTLREADING anyway when wrapping leads to leading weakchars in token
      TxtFlags := TxtFlags + ETO_RTLREADING;

      j :=  (ATokenInfo.StartPos.Logical - ATokenInfo.RtlInfo.LogFirst); // bytes in rtl-run, before TokenStart
      i :=  (ATokenInfo.RtlInfo.LogLast - ATokenInfo.EndPos.Logical);    // bytes in rtl-run, after TokenEnd
      ATokenInfo.Tk.TokenStart  := ATokenInfo.Tk.TokenStart - j;
      ATokenInfo.Tk.TokenLength := ATokenInfo.Tk.TokenLength + j + i;

      j :=  (ATokenInfo.EndPos.Physical - ATokenInfo.RtlInfo.PhysLeft);
      i :=  (ATokenInfo.RtlInfo.PhysRight - ATokenInfo.StartPos.Physical);
      ATokenInfo.PhysicalCharStart := ATokenInfo.PhysicalCharStart - j;
      ATokenInfo.PhysicalCharEnd   := ATokenInfo.PhysicalCharEnd + i;

      ATokenInfo.StartPos.Logical   := ATokenInfo.RtlInfo.LogFirst;
      ATokenInfo.ExpandedExtraBytes := ATokenInfo.RtlExpandedExtraBytes;
      ATokenInfo.HasTabs            := ATokenInfo.RtlHasTabs;
      ATokenInfo.HasDoubleWidth     := ATokenInfo.RtlHasDoubleWidth;
    end;

    NeedExpansion := (ATokenInfo.ExpandedExtraBytes > 0) or (ATokenInfo.HasTabs);
    NeedTransform := FTextDrawer.NeedsEto or ATokenInfo.HasDoubleWidth or ATokenInfo.NeedsEto or NeedExpansion
                     {$IFDEF Windows} or ATokenInfo.RtlInfo.IsRtl {$ENDIF}
                     ;
    Len := ATokenInfo.Tk.TokenLength;
    if (not ATokenInfo.RtlInfo.IsRtl) or (LineBufferRtlLogPos <> ATokenInfo.RtlInfo.LogFirst) then
      FEtoBuf := nil;

    If NeedTransform and ATokenInfo.RtlInfo.IsRtl and (LineBufferRtlLogPos = ATokenInfo.RtlInfo.LogFirst)
    then begin
      // allready done
      if NeedExpansion then begin
        ATokenInfo.Tk.TokenStart  := LineBuffer;
        ATokenInfo.Tk.TokenLength := Len + ATokenInfo.ExpandedExtraBytes;
      end;
    end
    else
    If NeedTransform then begin
      LineBufferRtlLogPos := ATokenInfo.RtlInfo.LogFirst;
      pt := ATokenInfo.Tk.TokenStart;
      // prepare LineBuffer
      if NeedExpansion then begin
        if (LineBufferLen < Len + ATokenInfo.ExpandedExtraBytes + 1) then begin
          LineBufferLen := Len + ATokenInfo.ExpandedExtraBytes + 1 + 128;
          ReAllocMem(LineBuffer, LineBufferLen);
        end;
        pl := LineBuffer;
      end;

      // Prepare FETOBuf
      if FTextDrawer.NeedsEto or ATokenInfo.HasDoubleWidth or ATokenInfo.NeedsEto
         {$IFDEF Windows} or ATokenInfo.RtlInfo.IsRtl {$ENDIF}  // RTL may have script with ligature
      then begin
        FEtoBuf := FTextDrawer.Eto;
        FEtoBuf.SetMinLength(Len + ATokenInfo.ExpandedExtraBytes + 1);
        c := FTextDrawer.CharWidth;
      end
      else
        c := 0;

      e := 0;
      // Copy to LineBuffer (and maybe FetoBuf
      if NeedExpansion then begin
        j := ATokenInfo.StartPos.Logical - 1;
        for i := 0 to Len - 1 do begin
          if j < CWLen
          then k := (CharWidths[j] and PCWMask)
          else k := 1;
          // combining chars will get 0 widths
          if (pt^ in [#0..#127, #192..#255]) and (FetoBuf <> nil) then begin
            FEtoBuf.EtoData[e] := k * c;
            inc(e);
          end;

          case pt^ of
            #9: begin
                if (not IsCombiningCodePoint(pt+1)) then begin
                  dec(e);
                  if (vscTabAtFirst in FVisibleSpecialChars) and (j < CWLen) then begin
                    pl^ := #194; inc(pl);
                    pl^ := #187; inc(pl);
                    dec(k);
                    if FetoBuf <> nil then FEtoBuf.EtoData[e] := c;
                    inc(e);
                  end;
                  while k > 0 do begin
                    pl^ := ' '; inc(pl);
                    dec(k);
                    if FetoBuf <> nil then FEtoBuf.EtoData[e] := c;
                    inc(e);
                  end;
                  if (vscTabAtLast in FVisibleSpecialChars) and ((pl-1)^=' ') and (j < CWLen) then begin
                    (pl-1)^ := #194;
                    pl^ := #187; inc(pl);
                  end;
                end
                else begin
                  pl^ := pt^;
                  inc(pl);
                end;
              end;
            ' ': begin
                if (vscSpace in FVisibleSpecialChars) and (j < CWLen) and
                   (not IsCombiningCodePoint(pt+1))
                then begin
                  pl^ := #194; inc(pl);
                  pl^ := #183; inc(pl);
                end
                else begin
                  pl^ := pt^;
                  inc(pl);
                end;
              end;
            else begin
                pl^ := pt^;
                inc(pl);
              end;
          end;
          inc(pt);
          inc(j);
        end;
        pl^ := #0;

      // Finish linebuffer
      ATokenInfo.Tk.TokenStart  := LineBuffer;
      ATokenInfo.Tk.TokenLength := Len + ATokenInfo.ExpandedExtraBytes;
      // TODO skip expanded half tab

      end
      else
      // FETOBuf only
      begin
        for j := ATokenInfo.StartPos.Logical - 1 to ATokenInfo.StartPos.Logical - 1 + Len do begin
          if pt^ in [#0..#127, #192..#255] then begin
            // combining chars will get 0 widths
            if j < CWLen
            then k := (CharWidths[j] and PCWMask)
            else k := 1;
            FEtoBuf.EtoData[e] := k * c;
            inc(e);
          end;
          inc(pt);
        end;
      end;
    end;

    if (ATokenInfo.PhysicalCharStart <> ATokenInfo.PhysicalClipStart) or
       (ATokenInfo.PhysicalCharEnd <> ATokenInfo.PhysicalClipEnd)
    then
      TxtFlags := TxtFlags + ETO_CLIPPED;

    tok := rcToken;
    if rcToken.Right > nRightEdge + 1 then
      tok.Bottom := rcLine.Bottom;
    TxtLeft := ScreenColumnToXValue(ATokenInfo.PhysicalCharStart); // because for the first token, this can be middle of a char, and lead to wrong frame
    fTextDrawer.NewTextOut(TxtLeft, rcToken.Top, TxtFlags, tok,
      ATokenInfo.Tk.TokenStart, ATokenInfo.Tk.TokenLength, FEtoBuf);


    rcToken.Left := rcToken.Right;
  end;

  procedure PaintLines;
  var
    ypos: Integer;
    DividerInfo: TSynDividerDrawConfigSetting;
    TV, cl: Integer;
    TokenInfoEx: TLazSynDisplayTokenInfoEx;
    MaxLine: Integer;
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Bottom := TextBounds.Top + FirstLine * fTextHeight;

    TV := TopViewedLine - 1;

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
      // Delete the whole Line
      fTextDrawer.BackColor := colEditorBG;
      rcLine.Left := EraseLeft;
      //rcLine.Right := DrawLeft; // TODO?
      FTextDrawer.FillRect(rcLine);
      rcLine.Left := DrawLeft;
      LineBufferRtlLogPos := -1;

      CharWidths := nil; // keep refcnt = 1 -- in case they get resized
      FTokenBreaker.SetHighlighterTokensLine(TV + CurLine, CurTextIndex);
      CharWidths := FTokenBreaker.CharWidths;
      CWLen := FTokenBreaker.CharWidthsLen;
      fMarkupManager.PrepareMarkupForRow(CurTextIndex+1);

      if FTokenBreaker.FIsLastViewedSubLine then begin
        DividerInfo := DisplayView.GetDrawDividerInfo;  // May call HL.SetRange
        if (DividerInfo.Color <> clNone) and (nRightEdge >= FTextBounds.Left) then
        begin
          ypos := rcToken.Bottom - 1;
          cl := DividerInfo.Color;
          if cl = clDefault then
            cl := RightEdgeColor;
          fTextDrawer.DrawLine(nRightEdge, ypos, FTextBounds.Left - 1, ypos, cl);
          dec(rcToken.Bottom);
        end;
      end;

      while FTokenBreaker.GetNextHighlighterTokenEx(TokenInfoEx) do begin
        DrawHiLightMarkupToken(TokenInfoEx);
      end;

      fMarkupManager.FinishMarkupForRow(CurTextIndex+1);
    end;
    CurLine := -1;
    AClip.Top := rcLine.Bottom;
  end;

{ end local procedures }

begin
  fMarkupManager.BeginMarkup;
  FTokenBreaker.Prepare(DisplayView, FTheLinesView, FMarkupManager, FirstCol, LastCol);
  FTokenBreaker.ForegroundColor := ForegroundColor;
  FTokenBreaker.BackgroundColor := BackgroundColor;
  FTokenBreaker.SpaceExtraByteCount := 0;
  FTokenBreaker.TabExtraByteCount := 0;
  if (vscSpace in FVisibleSpecialChars) then
    FTokenBreaker.SpaceExtraByteCount := 1;
  if (vscTabAtFirst in FVisibleSpecialChars) then
    FTokenBreaker.TabExtraByteCount := FTokenBreaker.TabExtraByteCount + 1;
  if (vscTabAtLast in FVisibleSpecialChars) then
    FTokenBreaker.TabExtraByteCount := FTokenBreaker.TabExtraByteCount + 1;
  //if (AClip.Right < TextLeftPixelOffset(False)) then exit;
  //if (AClip.Left > ClientWidth - TextRightPixelOffset) then exit;

  //DebugLn(['TCustomSynEdit.PaintTextLines ',dbgs(AClip)]);
  CurLine:=-1;
  //DebugLn('TCustomSynEdit.PaintTextLines ',DbgSName(Self),' TopViewedLine=',dbgs(TopViewedLine),' AClip=',dbgs(AClip));
  colEditorBG := BackgroundColor;
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  bDoRightEdge := False;
  if FRightEdgeVisible then begin // column value
    nRightEdge := FTextBounds.Left + (RightEdgeColumn - LeftChar + 1) * CharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then
      bDoRightEdge := True;
    if nRightEdge > AClip.Right then
      nRightEdge := AClip.Right; // for divider draw lines (don't draw into right gutter)
  end
  else
    nRightEdge := AClip.Right;

  Canvas.Pen.Color := RightEdgeColor; // used for code folding too
  Canvas.Pen.Width := 1;

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

    LineBufferLen := 0;
    LineBuffer := nil;
    if Assigned(fHighlighter) then begin
      fHighlighter.CurrentLines := FTheLinesView;
    end;

    DisplayView.InitHighlighterTokens(FHighlighter);
    fTextDrawer.Style := []; //Font.Style;
    fTextDrawer.BeginPaint;
    try
      PaintLines;

      if (AClip.Top < AClip.Bottom) then begin
        // Delete the remaining area
        AClip.Left := EraseLeft;
        FTextDrawer.BackColor := ColorToRGB(colEditorBG);
        FTextDrawer.FillRect(AClip);
        AClip.Left := DrawLeft;
        // Draw the right edge if necessary.
        if bDoRightEdge then
          FTextDrawer.DrawLine(nRightEdge, AClip.Top, nRightEdge, AClip.Bottom+1, ColorToRGB(RightEdgeColor));
      end;
    finally
      fTextDrawer.EndPaint;
      DisplayView.FinishHighlighterTokens;
      ReAllocMem(LineBuffer, 0);
    end;
  end;

  fMarkupManager.EndMarkup;
  FTokenBreaker.Finish;
end;

end.

