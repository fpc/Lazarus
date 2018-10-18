// Global defines
{$I IPDEFINE.INC}

unit ipHtmlBlockLayout;

interface

uses
  types, Classes, SysUtils, LCLPRoc, LCLIntf, Graphics,
  IpUtils, IpHtml, iphtmlprop;

type

  { TIpNodeBlockLayouter }

  TIpNodeBlockLayouter = class(TIpHtmlBaseLayouter)
  private
    FBlockOwner : TIpHtmlNodeBlock;
    FIpHtml : TIpHtml;
    FCanvas : TCanvas;
    FSizeOfSpace : TSize;
    FSizeOfHyphen : TSize;
    FLeftQueue, FRightQueue : TFPList;
    FVRemainL, FVRemainR : Integer;
    FLIdent, FRIdent : Integer;
    FTextWidth, FTotWidth : Integer;
    FFirstWord, FLastWord : Integer;
    FMaxAscent, FMaxDescent, FMaxHeight : Integer;
    FBlockAscent, FBlockDescent, FBlockHeight : Integer;
    FCurAscent, FCurDescent, FCurHeight : Integer;
    iElem, YYY : Integer;
    FBaseOffset : Integer;
    FLineBreak, FExpBreak, FCanBreak : Boolean;
    FIgnoreHardLF : Boolean;
    FTempCenter : Boolean;
    FLTrim : Boolean;
    FLastBreakpoint : Integer;
    FHyphenSpace : Integer;
    FSoftLF, FSoftBreak : Boolean;
    FAl, FSaveAl : TIpHtmlAlign;
    FVAL: TIpHtmlVAlign3;
    FWordInfo : PWordList;
    FWordInfoSize : Integer;
    FClear : (cNone, cLeft, cRight, cBoth);
    FxySize : TSize;
    procedure UpdSpaceHyphenSize(aProps: TIpHtmlProps);
    procedure UpdPropMetrics(aProps: TIpHtmlProps);
    // Used by LayoutQueue :
    procedure QueueInit(const TargetRect: TRect);
    procedure InitMetrics;
    function QueueLeadingObjects: Integer;
    function TrimTrailingBlanks(aFirstElem: Integer = 0): Integer;
    procedure DoQueueAlign(const TargetRect: TRect; aExpLIndent: Integer);
    procedure OutputQueueLine;
    procedure DoQueueClear;
    procedure ApplyQueueProps(aCurElem: PIpHtmlElement; var aPrefor : Boolean);
    procedure DoQueueElemWord(aCurElem: PIpHtmlElement);
    function DoQueueElemObject(var aCurElem: PIpHtmlElement): boolean;
    function DoQueueElemSoftLF(const W: Integer): boolean;
    function DoQueueElemHardLF: boolean;
    function DoQueueElemClear(aCurElem: PIpHtmlElement): boolean;
    procedure DoQueueElemIndentOutdent;
    procedure DoQueueElemSoftHyphen;
    function CalcVRemain(aVRemain: integer; var aIdent: integer): integer;
    procedure SetWordInfoLength(NewLength : Integer);
    function NextElemIsSoftLF: Boolean;
    // RelocateQueue and LayoutQueue
    procedure RelocateQueue(dx, dy: Integer);
    procedure LayoutQueue(TargetRect: TRect);
    {$IFDEF IP_LAZARUS_DBG}
    procedure DumpQueue(bStart: boolean=true);
    {$ENDIF}
    procedure UpdateCurrent(Start: Integer; const CurProps : TIpHtmlProps);
    procedure CalcMinMaxQueueWidth(var aMin, aMax: Integer);
    // Used by RenderQueue :
    procedure DoRenderFont(var aCurWord: PIpHtmlElement);
    procedure DoRenderElemWord(aCurWord: PIpHtmlElement; aCurTabFocus: TIpHtmlNode);
    procedure RenderQueue;
  public
    constructor Create(AOwner: TIpHtmlNodeCore); override;
    destructor Destroy; override;
    // Used by TIpHtmlNodeBlock descendants: Layout, CalcMinMaxPropWidth, Render
    procedure Layout(RenderProps: TIpHtmlProps; TargetRect: TRect); override;
    procedure CalcMinMaxPropWidth(RenderProps: TIpHtmlProps; var aMin, aMax: Integer); override;
    procedure Render(RenderProps: TIpHtmlProps); override;
  end;

  { TIpNodeTableElemLayouter }

  TIpNodeTableElemLayouter = class(TIpNodeBlockLayouter)
  private
    FTableElemOwner : TIpHtmlNodeTableHeaderOrCell;
  public
    constructor Create(AOwner: TIpHtmlNodeCore); override;
    destructor Destroy; override;
    // Methods for TIpHtmlNodeTableHeaderOrCell.
    procedure Layout(RenderProps: TIpHtmlProps; TargetRect: TRect); override;
    procedure CalcMinMaxPropWidth(RenderProps: TIpHtmlProps; var aMin, aMax: Integer); override;
    procedure Render(RenderProps: TIpHtmlProps); override;
  end;


implementation

function SameDimensions(const R1, R2 : TRect): Boolean;
begin
  Result := ( (R1.Bottom - R1.Top = R2.Bottom - R2.Top) or (R1.Top = R2.Top) )
        and ( R1.Right - R1.Left = R2.Right - R2.Left );
end;

function MaxI3(const I1, I2, I3: Integer) : Integer;
begin
  if I2 > I1 then
    if I3 > I2 then
      Result := I3
    else
      Result := I2
  else
    if I3 > I1 then
      Result := I3
    else
      Result := I1;
end;

{ TIpHtmlLayouter }

constructor TIpNodeBlockLayouter.Create(AOwner: TIpHtmlNodeCore);
begin
  inherited Create(AOwner);
  FIpHtml := FOwner.Owner;
  FBlockOwner := TIpHtmlNodeBlock(FOwner);
  FElementQueue := {$ifdef IP_LAZARUS}TFPList{$else}TList{$endif}.Create;
end;

destructor TIpNodeBlockLayouter.Destroy;
begin
  ClearWordList;
  FreeAndNil(FElementQueue);
  inherited Destroy;
end;

procedure TIpNodeBlockLayouter.UpdSpaceHyphenSize(aProps: TIpHtmlProps);
begin
  if aProps.PropA.SizeOfSpaceKnown then begin
    FSizeOfSpace := aProps.PropA.KnownSizeOfSpace;
    FSizeOfHyphen := aProps.PropA.KnownSizeOfHyphen;
  end else begin
    Assert(aProps.PropA.tmHeight = 0, 'UpdSpaceHyphenSize: PropA.tmHeight > 0');
    FCanvas.Font.Name := aProps.FontName;
    FCanvas.Font.Size := aProps.FontSize;
    FCanvas.Font.Style := aProps.FontStyle;
    FSizeOfSpace := FCanvas.TextExtent(' ');
    {$IFDEF IP_LAZARUS_DBG}
    if FSizeOfSpace.CX=0 then
      DebugLn('TIpHtmlNodeBlock.UpdSpaceHyphenSize Font not found "',FCanvas.Font.Name,'" Size=',dbgs(FCanvas.Font.Size));
    {$ENDIF}
    FSizeOfHyphen := FCanvas.TextExtent('-');
    aProps.PropA.SetKnownSizeOfSpace(FSizeOfSpace);
    aProps.PropA.KnownSizeOfHyphen := FSizeOfHyphen;
  end;
end;

procedure TIpNodeBlockLayouter.UpdPropMetrics(aProps: TIpHtmlProps);
var
  TextMetrics: TLCLTextMetric;  // TTextMetric;
begin             // Debug: remove assertions later
  Assert(aProps.PropA.tmHeight = 0, 'UpdPropMetrics: PropA.tmHeight > 0');
  Assert(FCanvas.Font.Name = aProps.FontName, 'UpdPropMetrics: FCanvas.Font.Name <> aProps.FontName');
  Assert(FCanvas.Font.Size = aProps.FontSize, 'UpdPropMetrics: FCanvas.Font.Size <> aProps.FontSize');
  Assert(FCanvas.Font.Style = aProps.FontStyle, 'UpdPropMetrics: FCanvas.Font.Style <> aProps.FontStyle');
  {$IFDEF IP_LAZARUS}
  FCanvas.GetTextMetrics(TextMetrics);
  aProps.PropA.tmAscent := TextMetrics.Ascender;
  aProps.PropA.tmDescent := TextMetrics.Descender;
  aProps.PropA.tmHeight := TextMetrics.Height;
  {$ELSE}
  GetTextMetrics(FCanvas.Handle, TextMetrics);
  aProps.PropA.tmAscent := TextMetrics.tmAscent;
  aProps.PropA.tmDescent := TextMetrics.tmDescent;
  aProps.PropA.tmHeight := TextMetrics.tmHeight;
  {$ENDIF}
end;

procedure TIpNodeBlockLayouter.Layout(RenderProps: TIpHtmlProps; TargetRect: TRect);
begin
  if EqualRect(TargetRect, FBlockOwner.PageRect) then
    exit;

  if FBlockOwner is TIpHtmlNodeBody then
    RemoveLeadingLFs;
  ProcessDuplicateLFs;

  if not RenderProps.IsEqualTo(Props) then
  begin
    Props.Assign(RenderProps);
    FOwner.LoadAndApplyCSSProps;
    FOwner.SetProps(Props);
  end;
  if FElementQueue.Count = 0 then
    FOwner.Enqueue;
  if SameDimensions(TargetRect, FBlockOwner.PageRect) then
    RelocateQueue(TargetRect.Left - FBlockOwner.PageRect.Left,
                  TargetRect.Top - FBlockOwner.PageRect.Top)
  else
    LayoutQueue(TargetRect);
end;

procedure TIpNodeBlockLayouter.RelocateQueue(dx, dy: Integer);
var
  i : Integer;
  CurElem : PIpHtmlElement;
  R : TRect;
begin
  OffsetRect(FPageRect, dx, dy);
  for i := 0 to Pred(FElementQueue.Count) do begin
    CurElem := PIpHtmlElement(FElementQueue[i]);
    R := CurElem^.WordRect2;
    if R.Bottom <> 0 then begin
      OffsetRect(R, dx, dy);
      SetWordRect(CurElem, R);
    end;
  end;
end;

procedure TIpNodeBlockLayouter.QueueInit(const TargetRect: TRect);
begin
  FWordInfoSize := 0;
  FWordInfo := nil;
  YYY := TargetRect.Top;
  FLeftQueue := TFPList.Create;
  FRightQueue := TFPList.Create;
  //FSizeOfSpace := Owner.Target.TextExtent(' ');
  //FSizeOfHyphen := Owner.Target.TextExtent('-');
  FCurProps := nil;
  FLIdent := 0;
  FRIdent := 0;
  FVRemainL := 0;
  FVRemainR := 0;
  FClear := cNone;
  FExpBreak := True;
  FTempCenter := False;
  FSaveAl := haLeft;
  FIgnoreHardLF := False;
  FLastBreakpoint := 0;
  FPageRect := TargetRect;
  FMaxHeight := 0;
  FMaxAscent := 0;
  FMaxDescent := 0;
  FLineBreak := False;
  FAl := haLeft;
  FVAL := hva3Top;
  FCurAscent := 0;
  FCurDescent := 0;
  FCurHeight := 0;
end;

procedure TIpNodeBlockLayouter.InitMetrics;
{$IFDEF IP_LAZARUS}
var
  TextMetrics : TLCLTextMetric;
begin
  FCanvas.GetTextMetrics(TextMetrics);
  FBlockAscent := TextMetrics.Ascender;
  FBlockDescent := TextMetrics.Descender;
  FBlockHeight := TextMetrics.Height;
end;
{$ELSE}
var
  TextMetrics : TTextMetric;
begin
  GetTextMetrics(aCanvas.Handle, TextMetrics);
  BlockAscent := TextMetrics.tmAscent;
  BlockDescent := TextMetrics.tmDescent;
  BlockHeight := TextMetrics.tmHeight;
end;
{$ENDIF}

function TIpNodeBlockLayouter.QueueLeadingObjects: Integer;
// Returns the first element index.
var
  CurObj : TIpHtmlNodeAlignInline;
  CurElem : PIpHtmlElement;
begin
  Result := 0;
  while Result <= FElementQueue.Count-1 do begin
    CurElem := PIpHtmlElement(FElementQueue[Result]);
    case CurElem.ElementType of
      etObject :
        begin
          CurObj := TIpHtmlNodeAlignInline(CurElem.Owner);
          case CurObj.Align of
          hiaLeft :
            begin
              FLeftQueue.Add(CurElem);
              Inc(Result);
            end;
          hiaRight :
            begin
              FRightQueue.Add(CurElem);
              Inc(Result);
            end;
          else
            break;
          end;
        end
      else
        break;
    end;
  end;
end;

function TIpNodeBlockLayouter.TrimTrailingBlanks(aFirstElem: Integer): Integer;
// Trim trailing blanks. Returns the last element index.
var
  CurElem: PIpHtmlElement;
begin
  Result := FElementQueue.Count - 1;
  repeat
    if (Result < aFirstElem) then Break;
    CurElem := PIpHtmlElement(FElementQueue[Result]);
    if (CurElem.ElementType <> etWord) or (CurElem.IsBlank = 0) then Break;
    Dec(Result)
  until false;
end;

procedure TIpNodeBlockLayouter.DoQueueAlign(const TargetRect: TRect; aExpLIndent: Integer);

  procedure DoQueueAlignSub(aQueue: TFPList; aRight: Boolean);
  var
    CurElem: PIpHtmlElement;
    CurObj: TIpHtmlNodeAlignInline;
    xLeft, xRight, ySize: Integer;
    RectWidth: Integer;
  begin
    if aRight then
      xLeft := FVRemainR
    else
      xLeft := FVRemainL;
    if (aQueue.Count > 0) and (xLeft = 0) then
      while aQueue.Count > 0 do begin
        CurElem := aQueue[0];
        CurObj := TIpHtmlNodeAlignInline(CurElem.Owner);
        RectWidth := TargetRect.Right - TargetRect.Left;
        FxySize := CurObj.GetDim(RectWidth);
        FTotWidth := RectWidth - FLIdent - FRIdent - FxySize.cx - aExpLIndent;
        if FTotWidth < 0 then
          break;
        if aRight then begin
          xRight := TargetRect.Right - FRIdent;
          xLeft := xRight - FxySize.cx;
          Inc(FRIdent, FxySize.cx);
          FVRemainR := MaxI2(FVRemainR, FxySize.cy)
        end
        else begin
          xLeft := TargetRect.Left + FLIdent;
          xRight := xLeft + FxySize.cx;
          Inc(FLIdent, FxySize.cx);
          FVRemainL := MaxI2(FVRemainL, FxySize.cy);
        end;
        ySize := FxySize.cy;
        SetWordRect(CurElem, Rect(xLeft, YYY, xRight, YYY+FxySize.cy));
        Assert(ySize = FxySize.cy, 'TIpNodeBlockLayouter.DoQueueAligned: ySize <> FSize.cy'); // Can be removed later.
        aQueue.Delete(0);
      end;
  end;

begin
  DoQueueAlignSub(FLeftQueue, False); // Left
  DoQueueAlignSub(FRightQueue, True); // Right
end;

procedure TIpNodeBlockLayouter.OutputQueueLine;
const
  NullRect : TRect = (Left:0; Top:0; Right:0; Bottom:0);
var
  WDelta, WMod : Integer;

  function CalcDelta: Integer;     // Returns dx
  begin
    WDelta := 0;
    WMod := 0;
    Result := 0;
    case FAl of
      haUnknown :  // by Juha
        Assert(False, 'TIpNodeBlockLayouter.OutputQueueLine: Align = Unknown.');
      haDefault, haLeft :
        ;
      haCenter :
        if FTotWidth >= FTextWidth then
          Result := (FTotWidth - FTextWidth) div 2;
      haRight :
        if FTotWidth >= FTextWidth then
          Result := FTotWidth - FTextWidth;
      haChar :
        if FTotWidth >= FTextWidth then
          Result := (FTotWidth - FTextWidth) div 2;
      {
      else
        //haJustify :
        if iElem < FElementQueue.Count then begin
          m := iElem - FFirstWord - 2;
          if m > 0 then begin
            WDelta := (FTotWidth - FTextWidth) div m;
            WMod := (FTotWidth - FTextWidth) mod m;
          end;
        end;
        }
    end;
  end;

var
  j, dx, ph : Integer;
  R : TRect;
  CurElem : PIpHtmlElement;
  CurWordInfo : PWordInfo;
begin
  dx := CalcDelta;
  ph := FIpHtml.PageHeight;
  if ph <> 0 then begin
    {if we're printing, adjust line's vertical offset to not straddle a page boundary}
    j := YYY mod ph;
    {only do this for 'small' objects, like text lines}
    if (FMaxAscent + FMaxDescent < 200)
    and (j + FMaxAscent + FMaxDescent > ph) then
      Inc(YYY, ((j + FMaxAscent + FMaxDescent) - ph));
  end;

  for j := FFirstWord to FLastWord do begin
    CurElem := PIpHtmlElement(FElementQueue[j]);
    CurWordInfo := @FWordInfo[j - FFirstWord];
    if CurWordInfo.Sz.cx <> 0 then begin
      R.Left := CurWordInfo.BaseX;
      R.Right := R.Left + CurWordInfo.Sz.cx;
      case CurWordInfo.VA of
      hva3Top :
        begin
          R.Top := YYY;
          R.Bottom := YYY + CurWordInfo.Sz.cy;
        end;
      hva3Middle :
        begin
          R.Top := YYY + (FMaxHeight - CurWordInfo.Sz.cy) div 2;
          R.Bottom := R.Top + CurWordInfo.Sz.cy;
        end;
      hva3Bottom :
        begin
          R.Top := YYY + FMaxHeight - CurWordInfo.Sz.cy;
          R.Bottom := R.Top + CurWordInfo.Sz.cy;
        end;
      hva3Default,
      hva3Baseline :
        begin
          if CurWordInfo.CurAsc >= 0 then
            R.Top := YYY + FMaxAscent - CurWordInfo.CurAsc
          else
            R.Top := YYY;
          R.Bottom := R.Top + CurWordInfo.Sz.cy;
        end;
      end;
      if WMod <> 0 then begin
        OffsetRect(R, dx + WDelta + 1, 0);
        Dec(WMod);
      end else
        OffsetRect(R, dx + WDelta, 0);
      SetWordRect(CurElem, R);
    end else
      SetWordRect(CurElem, NullRect);
  end;

  if FTempCenter then begin
    FAl := FSaveAl;
    FTempCenter := False;
  end;
end;

procedure TIpNodeBlockLayouter.DoQueueClear;
begin
  case FClear of
    cLeft :
      if FVRemainL > 0 then begin
        Inc(YYY, FVRemainL);
        FVRemainL := 0;
        FLIdent := 0;
      end;
    cRight :
      if FVRemainR > 0 then begin
        Inc(YYY, FVRemainR);
        FVRemainR := 0;
        FRIdent := 0;
      end;
    cBoth :
      begin
        Inc(YYY, MaxI2(FVRemainL, FVRemainR));
        FVRemainL := 0;
        FVRemainR := 0;
        FLIdent := 0;
        FRIdent := 0;
      end;
  end;
  FClear := cNone;
end;

procedure TIpNodeBlockLayouter.ApplyQueueProps(aCurElem: PIpHtmlElement; var aPrefor: Boolean);
begin
  with aCurElem.Props do begin
    if (FCurProps = nil) or not AIsEqualTo(FCurProps) then begin
      UpdSpaceHyphenSize(aCurElem.Props);
      if PropA.tmHeight = 0 then
        UpdPropMetrics(aCurElem.Props);
      FBlockHeight := PropA.tmHeight;
      FBlockAscent := PropA.tmAscent;
      FBlockDescent := PropA.tmDescent;
    end;
    if (FCurProps = nil) or not BIsEqualTo(FCurProps) then begin
      FAl := self.Props.Alignment;      // was: FAl := Alignment
      // wp: line was changed to "FAl := self.Props.Alignment" in order
      // to fix horizontal text alignment of table cells (r50145).
      // But with this change, something like "<p align="center"> does not work any more!
      // Alignment within cells still seems to work correctly after user to old code.
      FVAL := VAlignment;
      FBaseOffset := FontBaseline;
      aPrefor := Preformatted;
    end;
  end;
  FCurProps := aCurElem.Props;
end;

procedure TIpNodeBlockLayouter.DoQueueElemWord(aCurElem: PIpHtmlElement);
var
  lAlign: TIpHtmlAlign;
  node: TIpHtmlNode;
begin
  // wp: added to fix Align of <p> and <div> nodes in <tc>
  if Assigned(aCurElem.Owner) then begin
    lAlign := FAl;
    node := aCurElem.Owner.ParentNode;
    while Assigned(node) do begin
      if (node is TIpHtmlNodeCore) then
        lAlign := TIpHtmlNodeCore(node).Align
      {
      if (node is TIpHtmlNodeP) then
        lAlign := TIpHtmlNodeP(node).Align
      else
      if (node is TIpHtmlNodeDIV) then
        lAlign := TIpHtmlNodeDIV(node).Align
        }
      else
        break;
      if lAlign = haDefault then
        node := node.ParentNode
      else begin
        FAl := lAlign;
        break;
      end;
    end;
  end;

  FIgnoreHardLF := False;
  if FLTrim and (aCurElem.IsBlank <> 0) then
    FxySize := SizeRec(0, 0)
  else begin
    if aCurElem.IsBlank <> 0 then begin
      FxySize.cx := FSizeOfSpace.cx * aCurElem.IsBlank;
      FxySize.cy := FSizeOfSpace.cy;
      FCanBreak := True;
    end else begin
      if (aCurElem.SizeProp = FCurProps.PropA) then
        FxySize := aCurElem.Size
      else begin
        FCanvas.Font.Name := FCurProps.FontName;
        FCanvas.Font.Size := FCurProps.FontSize;
        FCanvas.Font.Style := FCurProps.FontStyle;
        aCurElem.Size := FCanvas.TextExtent(NoBreakToSpace(aCurElem.AnsiWord));
        FxySize := aCurElem.Size;
        aCurElem.SizeProp := FCurProps.PropA;
      end;
    end;
    FLTrim := False;
    FLineBreak := False;
    FExpBreak := False;
  end;
  FCurAscent := FBlockAscent;
  FCurDescent := FBlockDescent;
  FCurHeight := FBlockHeight;
end;

function TIpNodeBlockLayouter.DoQueueElemObject(var aCurElem: PIpHtmlElement): boolean;

  procedure ObjectVertical(Ascent, Descent: Integer);
  begin
    FExpBreak := False;
    FLTrim := False;
    FCurAscent := Ascent;
    FCurDescent := Descent;
  end;

  function ObjectHorizontal: boolean;
  begin
    aCurElem := nil;
    FCurHeight := 0;
    FxySize.cx := 0;
    Result := FLTrim;
    if Result then
      Inc(iElem);
  end;

var
  CurObj : TIpHtmlNodeAlignInline;
begin
  FIgnoreHardLF := False;
  FCurAscent := 0;
  FCurDescent := 0;
  FCanBreak := True;
  FLineBreak := False;
  CurObj := TIpHtmlNodeAlignInline(aCurElem.Owner);
  FxySize := CurObj.GetDim(FTotWidth);
  FCurHeight := FxySize.cy;
  case Curobj.Align of
  hiaCenter : begin
      ObjectVertical(FMaxAscent, FxySize.cy - FMaxAscent);
      FTempCenter := True;
      FSaveAl := FAl;
      FAl := haCenter;
    end;
  hiaTop :
      ObjectVertical(-1, FxySize.cy);
  hiaMiddle :
      ObjectVertical(FxySize.cy div 2, FxySize.cy div 2);
  hiaBottom :
      ObjectVertical(FxySize.cy, 0);
  hiaLeft : begin
      FLeftQueue.Add(aCurElem);
      if ObjectHorizontal then
        Exit(False);
    end;
  hiaRight : begin
      FRightQueue.Add(aCurElem);
      if ObjectHorizontal then
        Exit(False);
    end;
  end;
  Result := True;
end;

function TIpNodeBlockLayouter.DoQueueElemSoftLF(const W: Integer): boolean;
// Returns FIgnoreHardLF
var
  PendingLineBreak : Boolean;
begin
  if FLineBreak or FExpBreak then begin
    FMaxAscent := 0;
    FMaxDescent := 0;
    PendingLineBreak := False;
  end else begin
    if FMaxAscent = 0 then begin
      FMaxAscent := MaxI2(FMaxAscent, FBlockAscent);
      FMaxDescent := MaxI2(FMaxDescent, FBlockDescent);
    end;
    PendingLineBreak := True;
  end;
  FExpBreak := True;
  if FLineBreak then
    FMaxDescent := 0;
  Inc(iElem);
  FLastWord := iElem - 2;
  if PendingLineBreak then
    FLineBreak := True;
  Result := FIgnoreHardLF;
  if Result then begin
    FxySize.cx := W + 1;
    FSoftLF := True;
  end;
end;

function TIpNodeBlockLayouter.DoQueueElemHardLF: boolean;
// Returns FIgnoreHardLF
begin
  FExpBreak := True;
  if FMaxAscent = 0 then begin
    FMaxAscent := MaxI2(FMaxAscent, FBlockAscent);
    FMaxDescent := MaxI2(FMaxDescent, FBlockDescent);
  end;
  if FLineBreak then
    FMaxDescent := 0;
  FLastWord := iElem - 1;
  Result := FIgnoreHardLF;
  if not Result then begin
    if FLineBreak then  begin
      FMaxAscent := Round (FMaxAscent * FIpHtml.FactBAParag);
      FMaxDescent := Round (FMaxDescent * FIpHtml.FactBAParag);
    end;
    Inc(iElem);
  end;
end;

function TIpNodeBlockLayouter.DoQueueElemClear(aCurElem: PIpHtmlElement): boolean;
// Returns FIgnoreHardLF
begin
  FExpBreak := True;
  case aCurElem.ElementType of
    etClearLeft : FClear := cLeft;
    etClearRight : FClear := cRight;
    etClearBoth : FClear := cBoth;
  end;
  if FLineBreak then
    FMaxDescent := 0;
  Inc(iElem);
  FLastWord := iElem - 2;
  Result := FIgnoreHardLF;
end;

procedure TIpNodeBlockLayouter.DoQueueElemIndentOutdent;
begin
  FCurAscent := 1;
  FCurDescent := 0;
  FCurHeight := 1;
  FxySize := SizeRec(0, 0);
  FCanBreak := True;
end;

procedure TIpNodeBlockLayouter.DoQueueElemSoftHyphen;
begin
  FIgnoreHardLF := False;
  FxySize := FSizeOfHyphen;
  FxySize.cy := FSizeOfSpace.cy;
  FHyphenSpace := FxySize.cx;
  FCanBreak := True;
  FLTrim := False;
  FLineBreak := False;
  FExpBreak := False;
  FCurAscent := FBlockAscent;
  FCurDescent := FBlockDescent;
  FCurHeight := FBlockHeight;
end;

function TIpNodeBlockLayouter.CalcVRemain(aVRemain: integer; var aIdent: integer): integer;
begin
  if aVRemain > 0 then begin
    if FSoftBreak and (FTextWidth = 0) and (FMaxAscent + FMaxDescent = 0) then begin
      Inc(YYY, aVRemain);
      aVRemain := 0;
      aIdent := 0;
    end else begin
      Dec(aVRemain, FMaxAscent + FMaxDescent);
      if aVRemain <= 0 then begin
        aVRemain := 0;
        aIdent := 0;
      end;
    end;
  end;
  Result := aVRemain;
end;

procedure TIpNodeBlockLayouter.SetWordInfoLength(NewLength : Integer);
var
  NewWordInfoSize: Integer;
  {$IFNDEF IP_LAZARUS}
  NewWordInfo: PWordList;
  {$ENDIF}
begin
  if (FWordInfo = nil) or (NewLength > FWordInfoSize) then begin
    NewWordInfoSize := ((NewLength div 256) + 1) * 256;
    {$IFDEF IP_LAZARUS code below does not check if FWordInfo<>nil}
    ReallocMem(FWordInfo,NewWordInfoSize * sizeof(TWordInfo));
    {$ELSE}
    NewWordInfo := AllocMem(NewWordInfoSize * sizeof(TWordInfo));
    move(WordInfo^, NewWordInfo^, WordInfoSize);
    Freemem(WordInfo);
    WordInfo := NewWordInfo;
    {$ENDIF}
    FWordInfoSize := NewWordInfoSize;
  end;
end;

function TIpNodeBlockLayouter.NextElemIsSoftLF: Boolean;
var
  NextElem: PIpHtmlElement;
begin
  Result := False;
  if iElem < FElementQueue.Count-1 then begin
    NextElem := PIpHtmlElement(FElementQueue[iElem+1]);
    Result := NextElem.ElementType = etSoftLF;
  end;
end;

{$IFDEF IP_LAZARUS_DBG}
procedure TIpNodeBlockLayouter.DumpQueue(bStart: boolean=true);
var
  i: Integer;
  CurElem : PIpHtmlElement;
begin
  if bStart then WriteLn('<<<<<')
  else WriteLn('>>>>>');
  for i := 0 to FElementQueue.Count - 1 do begin
    CurElem := PIpHtmlElement(FElementQueue[i]);
    if CurElem.Owner <> nil then
       write(CurElem.Owner.ClassName,':');
    with CurElem.WordRect2 do
      write(Left,':', Top,':', Right,':', Bottom,':');
    case CurElem.ElementType of
    etWord :
      Write(' wrd:', CurElem.AnsiWord);
    etObject :
      Write(' obj');
    etSoftLF :
      Write(' softlf');
    etHardLF :
      Write(' hardlf');
    etClearLeft :
      Write(' clearleft');
    etClearRight :
      Write(' clearright');
    etClearBoth :
      Write(' clearboth');
    etIndent :
      Write(' indent');
    etOutdent :
      Write(' outdent');
    etSoftHyphen :
      Write(' softhyphen');
    end;
    WriteLn;
  end;
  if bStart then WriteLn('<<<<<')
  else WriteLn('>>>>>');
end;
{$ENDIF}

procedure TIpNodeBlockLayouter.LayoutQueue(TargetRect: TRect);
var
  WW, X0, ExpLIndent, RectWidth : Integer;
  FirstElem, LastElem : Integer;
  PendingIndent, PendingOutdent : Integer;
  Prefor : Boolean;
  CurElem : PIpHtmlElement;
  wi: PWordInfo;
  lfh: Integer;

  procedure InitInner;
  begin
    if PendingIndent > PendingOutdent then begin
      if ExpLIndent < RectWidth - FLIdent - FRIdent then
        Inc(ExpLIndent, (PendingIndent - PendingOutdent) * StdIndent);
    end
    else if PendingOutdent > PendingIndent then begin
      Dec(ExpLIndent, (PendingOutdent - PendingIndent) * StdIndent);
      if ExpLIndent < 0 then
        ExpLIndent := 0;
    end;
    PendingIndent := 0;
    PendingOutdent := 0;
    DoQueueAlign(TargetRect, ExpLIndent);
    FTotWidth := RectWidth - FLIdent - FRIdent - ExpLIndent;
    FLTrim := FLineBreak or (FExpBreak and not Prefor) or (ExpLIndent > 0);
    WW := FTotWidth; // total width we have
    X0 := TargetRect.Left + FLIdent + ExpLIndent;
    FTextWidth := 0;
    FFirstWord := iElem;
    FLastWord := iElem-1;
    FBaseOffset := 0;
    FSoftBreak := False;
    FHyphenSpace := 0;
    lfh := 0;
  end;

  procedure ContinueRow;
  var
    i: Integer;
  begin
    if FCanBreak then
      FLastBreakpoint := iElem;
    FMaxAscent := MaxI2(FMaxAscent, FCurAscent);
    FMaxDescent := MaxI2(FMaxDescent, FCurDescent);
    FMaxHeight := MaxI3(FMaxHeight, FCurHeight, FMaxAscent + FMaxDescent);
    // if word fits on line update width and height
    if CurElem.ElementType = etIndent then begin
      i := StdIndent;
      FxySize.cx := MinI2(WW, i - ((X0 - TargetRect.Left) mod i));
    end;
    Dec(WW, FxySize.cx);
    Inc(FTextWidth, FxySize.cx);
    if FHyphenSpace > 0 then
      for i := 0 to iElem - FFirstWord - 1 do begin
        Assert(i < FWordInfoSize);
        wi := @FWordInfo[i];
        if wi^.Hs > 0 then begin
          Inc(WW, wi^.Hs);
          Dec(FTextWidth, wi^.Hs);
          Dec(X0, wi^.Hs);
          wi^.Hs := 0;
          wi^.Sz.cx := 0;
        end;
      end;
    SetWordInfoLength(iElem - FFirstWord + 1);
    wi := @FWordInfo[iElem - FFirstWord];
    wi^.Sz := SizeRec(FxySize.cx, FCurHeight);
    wi^.BaseX := X0;
    wi^.BOff := FBaseOffset;
    wi^.CurAsc := FCurAscent + FBaseOffset;
    wi^.VA := FVAL;
    wi^.Hs := FHyphenSpace;
    FHyphenSpace := 0;
    Inc(X0, FxySize.cx);
    FLastWord := iElem;
  end;

  procedure EndRow;
  var
    i: Integer;
  begin
    if FHyphenSpace > 0 then
      for i := 0 to iElem - FFirstWord - 2 do begin
        wi := @FWordInfo[i];
        if wi^.Hs > 0 then begin
          Dec(FTextWidth, wi^.Hs);
          wi^.Hs := 0;
          wi^.Sz.cx := 0;
        end;
      end;
    if FCanBreak then
      FLastBreakpoint := iElem - 1;
    if (FLastWord >= 0) and (FLastWord < FElementQueue.Count) then begin
      CurElem := PIpHtmlElement(FElementQueue[FLastWord]);
      if (CurElem.ElementType = etWord)
      and (CurElem.IsBlank <> 0) then begin
        FWordInfo[FLastWord - FFirstWord].Sz.cx := 0;
        FLastWord := iElem - 2;
      end;
    end;
    FLineBreak := True;
    FSoftBreak := not FSoftLF;
  end;

begin
  FCanvas := FIpHtml.Target;
  if FElementQueue.Count = 0 then Exit;
  {$IFDEF IP_LAZARUS_DBG}
  DumpQueue; {debug}
  {$endif}
  try
    QueueInit(TargetRect);
    InitMetrics;
    FirstElem := QueueLeadingObjects;
    LastElem := TrimTrailingBlanks(FirstElem);
    DoQueueAlign(TargetRect, 0);
    Prefor := False;
    ExpLIndent := 0;
    PendingIndent := 0;
    PendingOutdent := 0;
    RectWidth := TargetRect.Right - TargetRect.Left;
    iElem := FirstElem;
    while iElem <= LastElem do begin
      InitInner;
      while iElem < FElementQueue.Count do begin
        FCanBreak := False;
        CurElem := PIpHtmlElement(FElementQueue[iElem]);
        if CurElem.Props <> nil then
          ApplyQueueProps(CurElem, Prefor);
        FSoftLF := False;
        case CurElem.ElementType of
          etWord :
            DoQueueElemWord(CurElem);
          etObject :
            if not DoQueueElemObject(CurElem) then
              Break;
          etSoftLF :
            if not DoQueueElemSoftLF(WW) then
            begin
              if CurElem.LFHeight > 0 then
                lfh := CurElem.LFHeight;
              Break;
            end;
          etHardLF :
            if not DoQueueElemHardLF then
            begin
              if CurElem.LFHeight > 0 then
                lfh := CurElem.LFHeight;
            //  raise EIpHtmlException.Create('TIpNodeBlockLayouter.LayoutQueue: FIgnoreHardLF is True after all.')
            //else
              Break;
            end;
          etClearLeft, etClearRight, etClearBoth :
            if not DoQueueElemClear(CurElem) then
              Break;
          etIndent : begin
              DoQueueElemIndentOutdent;
              if not NextElemIsSoftLF then
                FIgnoreHardLF := True;
              Inc(PendingIndent);
              FLTrim := True;
            end;
          etOutdent : begin
              DoQueueElemIndentOutdent;
              FIgnoreHardLF := False;
              Inc(PendingOutdent);
            end;
          etSoftHyphen :
            DoQueueElemSoftHyphen;
        end;
        FCanBreak := FCanBreak and Assigned(FCurProps) and not FCurProps.NoBreak;
        if (FxySize.cx <= WW) then begin
          ContinueRow;
          Inc(iElem);
        end
        else begin
          EndRow;
          Break;
        end;
      end;

      if FSoftBreak and (FLastBreakpoint > 0) then begin
        FLastWord := FLastBreakpoint;
        iElem := FLastBreakpoint + 1;
      end;
      OutputQueueLine;
      if (not FExpBreak) and (FTextWidth=0) and (FVRemainL=0) and (FVRemainR=0) then
        break;
      Inc(YYY, FMaxAscent + FMaxDescent + lfh);

      // Calculate VRemainL and VRemainR
      FVRemainL := CalcVRemain(FVRemainL, FLIdent);
      FVRemainR := CalcVRemain(FVRemainR, FRIdent);
      FMaxHeight := 0;
      FMaxAscent := 0;
      FMaxDescent := 0;
      // prepare for next line
      DoQueueClear;
    end;
    Inc(YYY, MaxI3(FMaxAscent div 2 + FMaxDescent, FVRemainL, FVRemainR));
    FVRemainL := 0;
    FVRemainR := 0;
    FLIdent := 0;
    FRIdent := 0;
    FMaxDescent := 0;

    DoQueueAlign(TargetRect, ExpLIndent);
    Inc(YYY, MaxI3(FMaxAscent + FMaxDescent, FVRemainL, FVRemainR));
    FPageRect.Bottom := YYY;
    {clean up}
  finally
    FLeftQueue.Free;
    FRightQueue.Free;
    if FWordInfo <> nil then
      FreeMem(FWordInfo);
    {$IFDEF IP_LAZARUS_DBG}
    DumpQueue(false); {debug}
    {$endif}
  end;
end;

procedure TIpNodeBlockLayouter.UpdateCurrent(Start: Integer; const CurProps : TIpHtmlProps);
{- update other words that use same properties as the one at Start with their lengths.
  Cuts down on the number of time the font properties need to be changed.}
var
  i : Integer;
  CurElem : PIpHtmlElement;
begin
  for i := FElementQueue.Count - 1 downto Start + 1 do begin
    CurElem := PIpHtmlElement(FElementQueue[i]);
    if (CurElem.ElementType = etWord) and (CurElem.IsBlank = 0)
    and ( (CurElem.Props = nil) or CurElem.Props.AIsEqualTo(CurProps) )
    and (CurElem.SizeProp <> CurProps.PropA) then begin
      CurElem.Size := FIpHtml.Target.TextExtent(NoBreakToSpace(CurElem.AnsiWord));
      if CurElem.AnsiWord = NAnchorChar then
        CurElem.Size.cx := 1;
      CurElem.SizeProp := CurProps.PropA;
    end;
  end;
end;

procedure TIpNodeBlockLayouter.CalcMinMaxQueueWidth(var aMin, aMax: Integer);
var
  CurElem : PIpHtmlElement;
  CurProps : TIpHtmlProps;
  CurFontName : string;
  CurFontSize : Integer;
  CurFontStyle : TFontStyles;
  i : Integer;
  MinW, MaxW, IndentW, TextWidth : Integer;
  LIndent, LIndentP : Integer;
  LastW, LastElement : Integer;
  NoBr : Boolean;

  procedure ApplyMinMaxProps;
  var
    Changed : Boolean;
  begin
    if (CurProps = nil) or not CurElem.Props.AIsEqualTo(CurProps) then begin
      Changed := False;
      if (CurProps = nil) or (CurFontName <> CurElem.Props.FontName)
      or (CurFontName = '') then begin
        CurFontName := CurElem.Props.FontName;
        FCanvas.Font.Name := CurFontName;
        Changed := True;
      end;
      if (CurProps = nil) or (CurFontSize <> CurElem.Props.FontSize)
      or (CurFontSize = 0) then begin
        CurFontSize := CurElem.Props.FontSize;
        FCanvas.Font.Size := CurFontSize;
        Changed := True;
      end;
      if (CurProps = nil) or (CurFontStyle <> CurElem.Props.FontStyle) then begin
        CurFontStyle := CurElem.Props.FontStyle;
        FCanvas.Font.Style := CurFontStyle;
        Changed := True;
      end;
      UpdSpaceHyphenSize(CurElem.Props);
      if Changed and (CurElem.Props.PropA.tmHeight = 0) then
        UpdPropMetrics(CurElem.Props);
    end;
  end;

begin
  FCanvas := FIpHtml.Target;
  aMin := 0;
  aMax := 0;
  if FElementQueue.Count = 0 then Exit;
  LIndent := 0;
  LIndentP := 0;
  LastElement := TrimTrailingBlanks;   // Trim trailing blanks
  CurProps := nil;
  CurFontName := '';
  CurFontSize := 0;
  CurFontStyle := [];
  FCanvas.Font.Style := CurFontStyle;
  FSizeOfSpace := FCanvas.TextExtent(' ');
  FSizeOfHyphen := FCanvas.TextExtent('-');
  i := 0;
  NoBr := False;
  while i <= LastElement do begin
    TextWidth := 0;
    IndentW := 0;
    LastW := 0;
    while (i <= LastElement) do begin
      MinW := 0;
      CurElem := PIpHtmlElement(FElementQueue[i]);
      if CurElem.Props <> nil then begin
        ApplyMinMaxProps;
        NoBr := CurElem.Props.NoBreak;
        CurProps := CurElem.Props;
      end;
      case CurElem.ElementType of
      etWord :
        begin
        {determine height and width of word}
        if CurElem.IsBlank <> 0 then begin
          MaxW := FSizeOfSpace.cx * CurElem.IsBlank;
          MinW := MaxW;
          if NoBr then
            MinW := MinW + LastW;
        end else begin
          if (CurElem.SizeProp = CurProps.PropA) then
            MaxW := CurElem.Size.cx
          else begin
            CurElem.Size := FCanvas.TextExtent(NoBreakToSpace(CurElem.AnsiWord));
            if CurElem.AnsiWord = NAnchorChar then
              CurElem.Size.cx := 1;
            MaxW := CurElem.Size.cx;
            CurElem.SizeProp := CurProps.PropA;
            UpdateCurrent(i, CurProps);
          end;
          MinW := MaxW + LastW;
        end;
        LastW := MinW;
        end;
      etObject :
        begin
          TIpHtmlNodeAlignInline(CurElem.Owner).CalcMinMaxWidth(MinW, MaxW);
          LastW := 0;
          CurProps := nil;
        end;
      etSoftLF..etClearBoth :
        begin
          if TextWidth + IndentW > aMax then
            aMax := TextWidth + IndentW;
          TextWidth := 0;
          MinW := 0;
          MaxW := 0;
          Inc(i);
          break;
        end;
      etIndent :
        begin
          Inc(LIndent);
          LIndentP := LIndent * StdIndent;
          if LIndentP > IndentW then
            IndentW := LIndentP;
          MinW := 0;
          MaxW := 0;
        end;
      etOutdent :
        begin
          if LIndent > 0 then begin
            Dec(LIndent);
            LIndentP := LIndent * StdIndent;
          end;
        MinW := 0;
        MaxW := 0;
        end;
      etSoftHyphen :
        begin
          MaxW := FSizeOfHyphen.cx;
          MinW := MaxW + LastW;
        end;
      end;
      Inc(MinW, LIndentP);
      if MinW > aMin then
        aMin := MinW;
      Inc(TextWidth, MaxW);
      Inc(i);
    end;

    aMax := MaxI2(aMax, TextWidth + IndentW);
  end;
end;

procedure TIpNodeBlockLayouter.CalcMinMaxPropWidth(RenderProps: TIpHtmlProps;
  var aMin, aMax: Integer);
begin
  if RenderProps.IsEqualTo(Props) and (FBlockMin <> -1) and (FBlockMax <> -1) then begin
    aMin := FBlockMin;
    aMax := FBlockMax;
    Exit;
  end;
  Props.Assign(RenderProps);
  FOwner.LoadAndApplyCSSProps;
  FOwner.SetProps(Props);
  if FElementQueue.Count = 0 then
    FOwner.Enqueue;
  CalcMinMaxQueueWidth(aMin, aMax);
  FBlockMin := aMin;
  FBlockMax := aMax;
end;

procedure TIpNodeBlockLayouter.DoRenderFont(var aCurWord: PIpHtmlElement);
begin
  {$IFDEF IP_LAZARUS}
  FCanvas.Font.BeginUpdate; // for speedup
  {$ENDIF}
  if (FCurProps = nil) or not FCurProps.AIsEqualTo(aCurWord.Props) then
    with aCurWord.Props do begin
      FCanvas.Font.Name := FontName;
      if ScaleFonts then
        FCanvas.Font.Size := round(FontSize * Aspect)
      else
        FCanvas.Font.Size := FontSize;
      FCanvas.Font.Style := FontStyle;
    end;
  if ScaleBitmaps and BWPRinter then
    FIpHtml.Target.Font.Color := clBlack
  else
    if (FCurProps = nil) or not FCurProps.BIsEqualTo(aCurWord.Props) then
      FCanvas.Font.Color := aCurWord.Props.FontColor;
  {$IFDEF IP_LAZARUS}
  FIpHtml.Target.Font.EndUpdate;
  {$ENDIF}
  FCurProps := aCurWord.Props;
end;

procedure TIpNodeBlockLayouter.DoRenderElemWord(aCurWord: PIpHtmlElement;
  aCurTabFocus: TIpHtmlNode);
var
  P : TPoint;
  R : TRect;
  {$IFDEF IP_LAZARUS}
  OldBrushcolor: TColor;
  OldFontColor: TColor;
  OldFontStyle: TFontStyles;
  OldBrushStyle: TBrushStyle;

  procedure saveCanvasProperties;
  begin
    OldBrushColor := FCanvas.Brush.Color;
    OldBrushStyle := FCanvas.Brush.Style;
    OldFontColor := FCanvas.Font.Color;
    OldFontStyle := FCanvas.Font.Style;
  end;

  procedure restoreCanvasProperties;
  begin
    FCanvas.Font.Color := OldFontColor;
    FCanvas.Brush.Color := OldBrushColor;
    FCanvas.Brush.Style := OldBrushStyle;
    FCanvas.Font.Style := OldFontStyle;
  end;
  {$ENDIF}

begin
  P := FIpHtml.PagePtToScreen(aCurWord.WordRect2.TopLeft);

  // We dont't want clipped lines at the top of the preview
  if (FIpHtml.RenderDevice = rdPreview) and
     (P.Y < 0) and (FIpHtml.PageViewRect.Top = FIpHtml.PageViewTop)
  then
    exit;

  {$IFDEF IP_LAZARUS}
  //if (LastOwner <> aCurWord.Owner) then LastPoint := P;
  saveCanvasProperties;
  if aCurWord.IsSelected or FIpHtml.AllSelected then begin
    FCanvas.Font.color := clHighlightText;
    FCanvas.brush.Style := bsSolid;
    FCanvas.brush.color := clHighLight;
    FIpHtml.PageRectToScreen(aCurWord.WordRect2, R);
    FCanvas.FillRect(R);
  end
  else if FCurProps.BgColor > 0 then
  begin
    FCanvas.brush.Style := bsSolid;
    FCanvas.brush.color := FCurProps.BgColor;
  end
  else
  {$ENDIF}
    FCanvas.Brush.Style := bsClear;

  //debugln(['TIpHtmlNodeBlock.RenderQueue ',aCurWord.AnsiWord]);
  FIpHtml.PageRectToScreen(aCurWord.WordRect2, R);

  {$IFDEF IP_LAZARUS}
  if aCurWord.Owner.ParentNode = aCurTabFocus then
    FCanvas.DrawFocusRect(R);
  if FCanvas.Font.color = -1 then
    FCanvas.Font.color := clBlack;
  {$ENDIF}
  if aCurWord.AnsiWord <> NAnchorChar then
    FCanvas.TextRect(R, P.x, P.y, NoBreakToSpace(aCurWord.AnsiWord));
  {$IFDEF IP_LAZARUS}
  RestoreCanvasProperties;
  {$ENDIF}

  FIpHtml.AddRect(aCurWord.WordRect2, aCurWord, FBlockOwner);
end;

procedure TIpNodeBlockLayouter.RenderQueue;
var
  CurWord : PIpHtmlElement;
  CurTabFocus: TIpHtmlNode;
  i : Integer;
  R : TRect;
  P : TPoint;
  L0 : Boolean;
  isVisible: Boolean;
begin
  L0 := FBlockOwner.Level0;
  FCurProps := nil;
  FCanvas := FIpHtml.Target;
  {$IFDEF IP_LAZARUS}
  // to draw focus rect
  i := FIpHtml.TabList.Index;
  if (FIpHtml.TabList.Count > 0) and (i <> -1) then
    CurTabFocus := TIpHtmlNode(FIpHtml.TabList[i])
  else
    CurTabFocus := nil;
  {$ENDIF}

  for i := 0 to pred(FElementQueue.Count) do begin
    CurWord := PIpHtmlElement(FElementQueue[i]);
    if (CurWord.Props <> nil) and (CurWord.Props <> FCurProps) then
      DoRenderFont(CurWord);

    {$IFDEF IP_LAZARUS_DBG}
    //DumpTIpHtmlProps(FCurProps);
    {$endif}
    //debugln(['TIpHtmlNodeBlock.RenderQueue ',i,' ',IntersectRect(R, CurWord.WordRect2, Owner.PageViewRect),' CurWord.WordRect2=',dbgs(CurWord.WordRect2),' Owner.PageViewRect=',dbgs(Owner.PageViewRect)]);

    isVisible := (CurWord.WordRect2.Top < FIpHtml.PageViewBottom);
    // Make sure that the printer does not duplicate clipped lines.
    if FIpHtml.RenderDevice = rdPrinter then
      isVisible := isVisible and (CurWord.WordRect2.Top >= FIpHtml.PageViewTop);
    isVisible := (isVisible or (CurWord.ElementType = etObject))
      and IntersectRect(R, CurWord.WordRect2, FIpHtml.PageViewRect);

    if isVisible then begin
      case CurWord.ElementType of
      etWord :
        DoRenderElemWord(CurWord, CurTabFocus);
      etObject :
        begin
          TIpHtmlNodeAlignInline(CurWord.Owner).Draw(FBlockOwner);
          //Owner.AddRect(CurWord.WordRect2, CurWord, Self);
          FCurProps := nil;
        end;
      etSoftHyphen :
        begin
          P := FIpHtml.PagePtToScreen(CurWord.WordRect2.TopLeft);
          FCanvas.Brush.Style := bsClear;
          FCanvas.TextOut(P.x, P.y, '-');
          FIpHtml.AddRect(CurWord.WordRect2, CurWord, FBlockOwner);
        end;
      end
    end
    else
      case CurWord.ElementType of
      etWord,
      etObject,
      etSoftHyphen :
        if (CurWord.WordRect2.Bottom <> 0) and
           (CurWord.WordRect2.Top > FIpHtml.PageViewRect.Bottom) and L0
        then
          break;
      end;
  end;
end;

procedure TIpNodeBlockLayouter.Render(RenderProps: TIpHtmlProps);
begin
  if not RenderProps.IsEqualTo(Props) then
  begin
    Props.Assign(RenderProps);
    FOwner.LoadAndApplyCSSProps;
    FOwner.SetProps(Props);
  end;
  if FElementQueue.Count = 0 then
    FOwner.Enqueue;
  RenderQueue;
end;


{ TIpNodeTableElemLayouter }

constructor TIpNodeTableElemLayouter.Create(AOwner: TIpHtmlNodeCore);
begin
  inherited Create(AOwner);
  FTableElemOwner := TIpHtmlNodeTableHeaderOrCell(FOwner);
end;

destructor TIpNodeTableElemLayouter.Destroy;
begin
  inherited Destroy;
end;

procedure GetAlignment(ANode: TIpHtmlNode; AProps: TIpHtmlProps; var Done: Boolean);
begin
  if (ANode is TIpHtmlNodeSpan) then
  begin
    if (ANode as TIpHtmlNodeSpan).Align <> haDefault then
    begin
      AProps.Alignment := (ANode as TIpHtmlNodeSpan).Align;
      Done := true;
    end;
  end else
  if (ANode is TIpHtmlNodeTableHeaderOrCell) then
  begin
    if (ANode as TIpHtmlNodeTableHeaderOrCell).Align <> haDefault then
    begin
      AProps.Alignment := (ANode as TIpHtmlNodeTableHeaderOrCell).Align;
      Done := true;
    end;
  end;
end;

procedure TIpNodeTableElemLayouter.Layout(RenderProps: TIpHtmlProps; TargetRect: TRect);
begin
  Props.Assign(RenderProps);

  IterateParents(@GetAlignment);
  if (Props.Alignment = haDefault) then
  begin
    if (FOwner is TIpHtmlNodeTD) then
      Props.Alignment := haLeft
    else
    if (FOwner is TIpHtmlNodeTH) then
      Props.Alignment := haCenter;
  end;

{
  if FTableElemOwner.Align <> haDefault then
    Props.Alignment := FTableElemOwner.Align
  else
    if FOwner is TIpHtmlNodeTH then
      Props.Alignment := haCenter;
 }

  if FOwner is TIpHtmlNodeTH then
    Props.FontStyle := Props.FontStyle + [fsBold];

  if FTableElemOwner.NoWrap then
    Props.NoBreak := True;

  case FTableElemOwner.VAlign of
  hva3Default :;
  else
    Props.VAlignment := FTableElemOwner.VAlign;
  end;

  if FTableElemOwner.BgColor <> -1 then
    Props.BgColor := FTableElemOwner.BgColor;

  inherited Layout(Props, TargetRect);
end;

procedure TIpNodeTableElemLayouter.CalcMinMaxPropWidth(RenderProps: TIpHtmlProps;
  var aMin, aMax: Integer);
var
  TmpBGColor, TmpFontColor: TColor;
begin
  TmpBGColor := Props.BgColor;
  TmpFontColor := Props.FontColor;
  Props.Assign(RenderProps);
  Props.BgColor := TmpBGColor;
  Props.FontColor := TmpFontColor;
  Props.Alignment := FTableElemOwner.Align;
  if FOwner is TIpHtmlNodeTH then
    Props.FontStyle := Props.FontStyle + [fsBold];
  Props.VAlignment := FTableElemOwner.VAlign;
  if FTableElemOwner.NoWrap then
    Props.NoBreak := True;
  inherited CalcMinMaxPropWidth(Props, aMin, aMax);
  if FTableElemOwner.NoWrap then
    aMin := aMax;
end;

procedure TIpNodeTableElemLayouter.Render(RenderProps: TIpHtmlProps);
var
  R : TRect;
begin
  Props.Assign(RenderProps);
  Props.DelayCache:=True;
  {$IFDEF IP_LAZARUS}
  FOwner.LoadAndApplyCSSProps;
  {$ENDIF}
//DebugLn('td :', IntToStr(Integer(Props.Alignment)));
  if FTableElemOwner.BgColor <> -1 then
    Props.BgColor := FTableElemOwner.BgColor;
  if FTableElemOwner.Align <> haDefault then
    Props.Alignment := FTableElemOwner.Align
  else if Props.Alignment = haDefault then
  begin
    if FOwner is TIpHtmlNodeTH then
      Props.Alignment := haCenter
    else
      Props.Alignment := haLeft;
  end;
  if FOwner is TIpHtmlNodeTH then
    Props.FontStyle := Props.FontStyle + [fsBold];
  Props.VAlignment := FTableElemOwner.VAlign;
  if FTableElemOwner.NoWrap then
    Props.NoBreak := True;
  {$IFDEF IP_LAZARUS_DBG}
  DebugBox(Owner.Target, PadRect, clYellow, True);
  {$ENDIF}
  if FOwner.PageRectToScreen(FTableElemOwner.PadRect, R) then
  begin
    if (Props.BgColor <> -1) then
    begin
      FIpHtml.Target.Brush.Color := Props.BGColor;
      FIpHtml.Target.FillRect(R);
    end else
      FIpHtml.Target.Brush.Style := bsClear;
  end;
  Props.DelayCache:=False;
  inherited Render(Props);
end;


initialization
  BlockLayouterClass := TIpNodeBlockLayouter;
  TableElemLayouterClass := TIpNodeTableElemLayouter;

end.

