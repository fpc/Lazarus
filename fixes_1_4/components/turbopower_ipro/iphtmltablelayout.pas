// Global defines
{$I IPDEFINE.INC}

unit ipHtmlTableLayout;

interface

uses
  types, Classes, LCLType, LCLIntf, IpHtml, iphtmlprop;

type

  { TIpNodeTableLayouter }

  TIpNodeTableLayouter = class(TIpHtmlBaseTableLayouter)
  private
    FTableOwner : TIpHtmlNodeTABLE;
    CellOverhead : Integer; // sum of col widths + CellOverhead = TableWidth
    RUH, RUV : Integer; // ruler width hor/vert
    BL, BR, BT, BB : Integer; // border width, left, right, top, bottom
    FColCount : Integer;
    ColTextWidthMin, ColTextWidthMax : TIntArr; // min and max column widths
    ColTextWidth : TIntArr; //actual column widths
    ColStart : TIntArr; // start of each column relative to table's left
  public
    constructor Create(AOwner: TIpHtmlNodeCore); override;
    destructor Destroy; override;
    procedure CalcMinMaxColTableWidth(RenderProps: TIpHtmlProps;
      var aMin, aMax: Integer); override;
    procedure CalcSize(ParentWidth: Integer; RenderProps: TIpHtmlProps); override;
    function GetColCount: Integer; override;
  public
    property ColCount : Integer read GetColCount;  // Same as FTableOwner.ColCount.
  end;


implementation

{ TIpNodeTableLayouter }

constructor TIpNodeTableLayouter.Create(AOwner: TIpHtmlNodeCore);
begin
  inherited Create(AOwner);
  FTableOwner := TIpHtmlNodeTABLE(FOwner);
  FColCount := -1;
  ColTextWidthMin := TIntArr.Create;
  ColTextWidthMax := TIntArr.Create;
  ColTextWidth := TIntArr.Create;
  ColStart := TIntArr.Create;
end;

destructor TIpNodeTableLayouter.Destroy;
begin
  ColTextWidth.Free;
  ColStart.Free;
  ColTextWidthMin.Free;
  ColTextWidthMax.Free;
  inherited Destroy;
end;

procedure TIpNodeTableLayouter.CalcMinMaxColTableWidth(RenderProps: TIpHtmlProps;
  var aMin, aMax: Integer);
var
  z, Min0, Max0: Integer;
  i, j, CurCol, k : Integer;
  TWMin, TWMax : Integer;
  PendSpanWidthMin,
  PendSpanWidthMax,
  PendSpanStart,
  PendSpanSpan : TIntArr;
  PendCol : Integer;
  CoreNode: TIpHtmlNodeCore;
  TrNode: TIpHtmlNodeTR;
  CellNode: TIpHtmlNodeTableHeaderOrCell;

  procedure DistributeColSpace(ColSpan: Integer);
  var
    i, Rest, MinNow : Integer;
  begin
    if ColSpan > 1 then begin
      PendSpanWidthMin[PendCol] := Min0;
      PendSpanWidthMax[PendCol] := Max0;
      PendSpanStart[PendCol] := CurCol;
      PendSpanSpan[PendCol] := ColSpan;
      Inc(PendCol);
      Exit;
    end;
    MinNow := 0;
    for i := CurCol to CurCol + ColSpan - 1 do
      Inc(MinNow, ColTextWidthMin[i]);
    if MinNow = 0 then begin
      for i := CurCol to CurCol + ColSpan - 1 do
         ColTextWidthMin[i] := Min0 div ColSpan;
    end else begin
      Rest := Min0 - MinNow;
      if Rest > 0 then begin
        for i := CurCol to CurCol + ColSpan - 1 do
           ColTextWidthMin[i] := ColTextWidthMin[i] +
             round(Rest * ColTextWidthMin[i] / MinNow);
        MinNow := 0;
        for i := CurCol to CurCol + ColSpan - 1 do
          Inc(MinNow, ColTextWidthMin[i]);
        Rest := Min0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do begin
            ColTextWidthMin[i] := ColTextWidthMin[i] + 1;
            Dec(Rest);
            if rest = 0 then
              break;
          end;
        end;
      end;
    end;
    MinNow := 0;
    for i := CurCol to CurCol + ColSpan - 1 do
      Inc(MinNow, ColTextWidthMax[i]);
    if MinNow = 0 then begin
      for i := CurCol to CurCol + ColSpan - 1 do
         ColTextWidthMax[i] := Max0 div ColSpan;
    end else begin
      Rest := Max0 - MinNow;
      if Rest > 0 then begin
        for i := CurCol to CurCol + ColSpan - 1 do
          ColTextWidthMax[i] := ColTextWidthMax[i] +
            round(Rest * ColTextWidthMax[i] / MinNow);
        MinNow := 0;
        for i := CurCol to CurCol + ColSpan - 1 do
          Inc(MinNow, ColTextWidthMax[i]);
        Rest := Max0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do begin
            ColTextWidthMax[i] := ColTextWidthMax[i] + 1;
            Dec(Rest);
            if rest = 0 then
              break;
          end;
        end;
      end;
    end;
    for i := 0 to Pred(ColCount) do begin
      ColTextWidthMin[i] := MinI2(ColTextWidthMin[i], ColTextWidthMax[i]);
      ColTextWidthMax[i] := MaxI2(ColTextWidthMin[i], ColTextWidthMax[i]);
    end;
  end;

  procedure DistributeSpannedColSpace;
  var
    z, i, Rest, MinNow, Min0, Max0, CurCol, ColSpan : Integer;
  begin
    for z := 0 to Pred(PendCol) do begin
      Min0 := PendSpanWidthMin[z];
      Max0 := PendSpanWidthMax[z];
      CurCol := PendSpanStart[z];
      ColSpan := PendSpanSpan[z];
      MinNow := 0;
      for i := CurCol to CurCol + ColSpan - 1 do
        Inc(MinNow, ColTextWidthMin[i]);
      if MinNow = 0 then begin
        Rest := 0;
        for i := CurCol to CurCol + ColSpan - 1 do begin
           ColTextWidthMin[i] := Min0 div ColSpan;
           Inc(Rest, ColTextWidthMin[i]);
        end;
        ColTextWidthMin[0] := ColTextWidthMin[0] + (Min0 - Rest);
      end else begin
        Rest := Min0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do
            ColTextWidthMin[i] := ColTextWidthMin[i] +
              round(Rest * ColTextWidthMin[i] / MinNow);
          MinNow := 0;
          for i := CurCol to CurCol + ColSpan - 1 do
            Inc(MinNow, ColTextWidthMin[i]);
          Rest := Min0 - MinNow;
          if Rest > 0 then begin
            for i := CurCol to CurCol + ColSpan - 1 do begin
              ColTextWidthMin[i] := ColTextWidthMin[i] + 1;
              Dec(Rest);
              if rest = 0 then
                break;
            end;
          end;
        end;
      end;
      MinNow := 0;
      for i := CurCol to CurCol + ColSpan - 1 do
        Inc(MinNow, ColTextWidthMax[i]);
      if MinNow = 0 then begin
        Rest := 0;
        for i := CurCol to CurCol + ColSpan - 1 do begin
           ColTextWidthMax[i] := Max0 div ColSpan;
           Inc(Rest, ColTextWidthMax[i]);
        end;
        ColTextWidthMax[0] := ColTextWidthMax[0] + (Max0 - Rest);
      end else begin
        Rest := Max0 - MinNow;
        if Rest > 0 then begin
          for i := CurCol to CurCol + ColSpan - 1 do
            ColTextWidthMax[i] := ColTextWidthMax[i] +
              round(Rest * ColTextWidthMax[i] / MinNow);
          MinNow := 0;
          for i := CurCol to CurCol + ColSpan - 1 do
            Inc(MinNow, ColTextWidthMax[i]);
          Rest := Max0 - MinNow;
          if Rest > 0 then begin
            for i := CurCol to CurCol + ColSpan - 1 do begin
              ColTextWidthMax[i] := ColTextWidthMax[i] + 1;
              Dec(Rest);
              if rest = 0 then
                break;
            end;
          end;
        end;
      end;
      for i := 0 to Pred(ColCount) do begin
        ColTextWidthMax[i] := MaxI2(ColTextWidthMin[i], ColTextWidthMax[i]);
      end;
    end;
  end;

begin
  if FMin <> -1 then begin
    aMin := FMin;
    aMax := FMax;
    Exit;
  end;

  FMin := 0;
  FMax := 0;
  if ColCount = 0 then
    Exit;

  PendSpanWidthMin := nil;
  PendSpanWidthMax := nil;
  PendSpanStart := nil;
  PendSpanSpan := nil;
  try
    PendSpanWidthMin := TIntArr.Create;
    PendSpanWidthMax := TIntArr.Create;
    PendSpanStart := TIntArr.Create;
    PendSpanSpan := TIntArr.Create;

    {calc col and table widths}
    for i := 0 to Pred(ColCount) do begin
      FRowSp[i] := 0;
      ColTextWidthMin[i] := 0;
      ColTextWidthMax[i] := 0;
    end;
    PendCol := 0;

    for z := 0 to Pred(FTableOwner.ChildCount) do
      if FTableOwner.ChildNode[z] is TIpHtmlNodeTHeadFootBody then
      begin
        CoreNode := TIpHtmlNodeCore(FTableOwner.ChildNode[z]);
        for i := 0 to Pred(CoreNode.ChildCount) do begin
          if CoreNode.ChildNode[i] is TIpHtmlNodeTR then
          begin
            TrNode := TIpHtmlNodeTR(CoreNode.ChildNode[i]);
            CurCol := 0;
            while FRowSp[CurCol] <> 0 do begin
              FRowSp[CurCol] := FRowSp[CurCol] - 1;
              Inc(CurCol);
            end;
            for j := 0 to Pred(TrNode.ChildCount) do
              if TrNode.ChildNode[j] is TIpHtmlNodeTableHeaderOrCell then
              begin
                CellNode := TIpHtmlNodeTableHeaderOrCell(TrNode.ChildNode[j]);

                while FRowSp[CurCol] <> 0 do begin
                  FRowSp[CurCol] := FRowSp[CurCol] - 1;
                  Inc(CurCol);
                end;

                CellNode.CalcMinMaxPropWidth(RenderProps, Min0, Max0);

                case CellNode.Width.LengthType of
                hlAbsolute :
                  begin
                    if CellNode.Width.LengthValue <= CellNode.ExpParentWidth then
                      Min0 := MaxI2(Min0, CellNode.Width.LengthValue - 2*FCellPadding
                      {$IFDEF IP_LAZARUS}
                        - FCellSpacing - RUH);
                      {$ELSE}
                        - 2*CS2 - RUH);
                      {$ENDIF}
                    Max0 := Min0;
                  end;
                end;

                CellNode.CalcWidthMin := Min0;
                CellNode.CalcWidthMax := Max0;

                DistributeColSpace(CellNode.ColSpan);

                for k := 0 to Pred(CellNode.ColSpan) do begin
                  while FRowSp[CurCol] <> 0 do begin
                    FRowSp[CurCol] := FRowSp[CurCol] - 1;
                    Inc(CurCol);
                  end;
                  FRowSp[CurCol] := CellNode.RowSpan - 1;
                  Inc(CurCol);
                end;
              end;
            for j := CurCol to Pred(ColCount) do
              if FRowSp[j] > 0 then
                FRowSp[j] := FRowSp[j] - 1;
          end;
        end;
      end;

    DistributeSpannedColSpace;
  finally
    PendSpanWidthMin.Free;
    PendSpanWidthMax.Free;
    PendSpanStart.Free;
    PendSpanSpan.Free;
  end;

  TWMin := 0;
  TWMax := 0;
  {$IFDEF IP_LAZARUS}
  CellOverhead := BL + FCellSpacing + BR;
  {$ELSE}
  CellOverhead := BL + 2*CS2 + RUH + BR;
  {$ENDIF}
  for i := 0 to Pred(ColCount) do begin
    Inc(TWMin, ColTextWidthMin[i]);
    Inc(TWMax, ColTextWidthMax[i]);
    {$IFDEF IP_LAZARUS}
    Inc(CellOverhead, RUH + 2*FCellPadding + FCellSpacing + RUH);
    {$ELSE}
    Inc(CellOverhead, 2*FCellPadding + 2*CS2 + RUH);
    {$ENDIF}
    FRowSp[i] := 0;
  end;

  FMin := MaxI2(FMin, TWMin + CellOverhead);
  FMax := MaxI2(FMax, TWMax + CellOverhead);
  aMin := FMin;
  aMax := FMax;
end;

procedure TIpNodeTableLayouter.CalcSize(ParentWidth: Integer; RenderProps: TIpHtmlProps);
var
  z, GrossCellSpace, NetCellSpace, CellExtra,
  NetCellSpaceExtraExtra,
  RelCellExtra,
  i, j, CurCol, k,
  CellSpace,
  MinW, MaxW : Integer;
  R : TRect;
  TargetRect : TRect;
  RowFixup : TRectRectArr;
  RowFixupCount : Integer;

  function GetSpanBottom(Row, Col: Integer): Integer;
  var
    R: PRect;
  begin
    R := RowFixup.Value[Row].Value[Col];
    if R <> nil then
      Result := R.Bottom
    else
      Result := 0;
  end;

  procedure SetSpanBottom(Row, Col, Value: Integer);
  var
    R: PRect;
  begin
    R := RowFixup.Value[Row].Value[Col];
    if R <> nil then
      R^.Bottom := Value;
  end;

  procedure SetSpanRect(Row,Col : Integer; const Rect: PRect);
  begin
    RowFixup[Row].Value[Col] := Rect;
  end;

  procedure DeleteFirstSpanRow;
  begin
    RowFixup.Delete(0);
  end;

  procedure AdjustCol(ColSpan, DesiredWidth: Integer);
  var
    i, Rest, WNow, Avail : Integer;
  begin
    WNow := 0;
    for i := CurCol to CurCol + ColSpan - 1 do
      Inc(WNow, ColTextWidth[i]);
    Avail := MinI2(DesiredWidth, CellSpace);
    if WNow = 0 then begin
      for i := CurCol to CurCol + ColSpan - 1 do
        ColTextWidth[i] := Avail div ColSpan;
    end else begin
      Rest := MinI2(CellSpace, DesiredWidth - WNow);
      if Rest > 0 then begin
        for i := CurCol to CurCol + ColSpan - 1 do
          ColTextWidth[i] := ColTextWidth[i] + round(Rest * ColTextWidth[i] / WNow);
      end;
    end;
  end;

  procedure DoBlock(BlockType : TIpHtmlNodeTABLEHEADFOOTBODYClass);
  var
    z, i, j, k, zz : Integer;
    RowSp2 : TIntArr;
    AL0, AL : TIpHtmlAlign;
    CellRect1 : TRect;
    HA, HB, Y0: Integer;
    maxY, maxYY: Integer;
    VA0, VA : TIpHtmlVAlign3;
    CoreNode: TIpHtmlNodeCore;
    TrNode: TIpHtmlNodeTR;
    CellNode: TIpHtmlNodeTableHeaderOrCell;
  begin
    RowSp2 := TIntArr.Create;
    try
      for z := 0 to Pred(FTableOwner.ChildCount) do
        if (TIpHtmlNode(FTableOwner.ChildNode[z]) is BlockType) then
        begin
          CoreNode := TIpHtmlNodeCore(FTableOwner.ChildNode[z]);
          for i := 0 to Pred(CoreNode.ChildCount) do begin
            if CoreNode.ChildNode[i] is TIpHtmlNodeTR then
            begin
              TrNode := TIpHtmlNodeTR(CoreNode.ChildNode[i]);

              for j := 0 to Pred(ColCount) do
                RowSp2[j] := FRowSp[j];
              CurCol := 0;
              while FRowSp[CurCol] <> 0 do begin
                FRowSp[CurCol] := FRowSp[CurCol] - 1;
                Inc(CurCol);
              end;

              VA0 := TrNode.Props.VAlignment;
              case TrNode.VAlign of
              hvaTop :
                VA0 := hva3Top;
              hvaMiddle :
                VA0 := hva3Middle;
              hvaBottom :
                VA0 := hva3Bottom;
              end;

              case TrNode.Align of
              haDefault :
                AL0 := haLeft;
              else
                AL0 := TrNode.Align;
              end;

              {determine height of cells and lay out with top alignment}
              for j := 0 to Pred(TrNode.ChildCount) do
                if TIpHtmlNode(TrNode.ChildNode[j]) is TIpHtmlNodeTableHeaderOrCell then
                begin
                  CellNode := TIpHtmlNodeTableHeaderOrCell(TrNode.ChildNode[j]);
                  while FRowSp[CurCol] <> 0 do begin
                    FRowSp[CurCol] := FRowSp[CurCol] - 1;
                    Inc(CurCol);
                  end;
                  AL := AL0;
                  CellNode.Props.Assign(FOwner.Props); // assign table props
                  CellRect1 := TargetRect;
                  Inc(CellRect1.Left, ColStart[CurCol]);

                  {$IFDEF IP_LAZARUS}
                  Inc(CellRect1.Top, FCellSpacing + RUV);
                  {$ELSE}
                  Inc(CellRect1.Top, CS2 + RUV);
                  {$ENDIF}

                  CellRect1.Right := CellRect1.Left + 2*FCellPadding + ColTextWidth[CurCol]
                    {$IFnDEF IP_LAZARUS}
                    + 2*CS2
                    {$ENDIF}
                    ;
                  for k := 1 to CellNode.ColSpan - 1 do
                    Inc(CellRect1.Right, ColTextWidth[CurCol + k] + 2*FCellPadding +
                      {$IFDEF IP_LAZARUS}
                      2*RUH + FCellSpacing);
                      {$ELSE}
                      2*CS2 + RUH);
                      {$ENDIF}
                  {$IFDEF IP_LAZARUS}
                  // PadRect area of cell excluding rules
                  // CellRect area of text contained in cell
                  CellNode.PadRect := CellRect1;
                  Inc(CellRect1.Top, FCellPadding);
                  inflateRect(CellRect1, -FCellPadding, 0);
                  {$ELSE}
                  FPadRect := CellRect1;
                  InflateRect(FPadRect, -CS2, 0);
                  Inc(CellRect1.Top, FCellPadding);
                  InflateRect(CellRect1, -(FCellPadding + CS2), 0);
                  {$ENDIF}

                  VA := CellNode.VAlign;
                  if VA = hva3Default then
                    VA := VA0;

                  case CellNode.Align of
                  haDefault : ;
                  else
                    AL := CellNode.Align;
                  end;

                  CellNode.Props.VAlignment := VA;
                  CellNode.Props.Alignment := AL;
                  CellNode.Layout(CellNode.Props, CellRect1);

                  if (CellNode.Height.PixelsType <> hpUndefined) {Height <> -1} then
                    if CellNode.PageRect.Bottom - CellNode.PageRect.Top < CellNode.Height.Value then
                      CellNode.Layouter.FPageRect.Bottom := CellRect1.Top + CellNode.Height.Value;

                  if (CellNode.Height.PixelsType = hpUndefined) {Height = -1}
                  and IsRectEmpty(CellNode.PageRect) then
                    CellNode.FPadRect.Bottom := CellRect1.Top + FCellPadding
                  else begin
                    CellNode.FPadRect.Bottom := CellNode.PageRect.Bottom + FCellPadding;
                  end;
                  SetSpanRect(CellNode.RowSpan - 1, CurCol, @CellNode.PadRect);

                  for k := 0 to Pred(CellNode.ColSpan) do begin
                    FRowSp[CurCol] := CellNode.RowSpan - 1;
                    Inc(CurCol);
                  end;
                end;

              {Adjust any trailing spanning columns}
              for j := CurCol to Pred(ColCount) do
                if FRowSp[j] > 0 then
                  FRowSp[j] := FRowSp[j] - 1;

              maxYY := 0;
              maxY := 0;
              for zz := 0 to Pred(ColCount) do
                maxY := MaxI2(GetSpanBottom(0, zz), maxY);
              for zz := 0 to Pred(ColCount) do
                SetSpanBottom(0, zz, maxY);
             if maxY > maxYY then
               maxYY := maxY;

              for j := 0 to Pred(ColCount) do
                FRowSp[j] := RowSp2[j];

              CurCol := 0;
              while FRowSp[CurCol] <> 0 do begin
                FRowSp[CurCol] := FRowSp[CurCol] - 1;
                Inc(CurCol);
              end;
              {relocate cells which are not top aligned}
              for j := 0 to Pred(TrNode.ChildCount) do
                if TrNode.ChildNode[j] is TIpHtmlNodeTableHeaderOrCell then
                begin
                  CellNode := TIpHtmlNodeTableHeaderOrCell(TrNode.ChildNode[j]);
                  while FRowSp[CurCol] <> 0 do begin
                    FRowSp[CurCol] := FRowSp[CurCol] - 1;
                    Inc(CurCol);
                  end;

                  AL := AL0;

                  {$IFDEF IP_LAZARUS}
                  HA := maxYY - (TargetRect.Top + FCellSpacing + RUV);
                  {$ELSE}
                  HA := maxYY - TargetRect.Top;
                  {$ENDIF}
                  HB := CellNode.PageRect.Bottom - CellNode.PageRect.Top;

                  VA := CellNode.VAlign;
                  if VA = hva3Default then
                    VA := VA0;

                  case VA of
                  hva3Middle :
                    Y0 := (HA - HB) div 2;
                  hva3Bottom :
                    Y0 := (HA - HB);
                  else
                    Y0 := 0;
                  end;

                  if Y0 > 0 then begin
                    CellRect1 := TargetRect;
                    Inc(CellRect1.Left, ColStart[CurCol]);
                    {$IFDEF IP_LAZARUS}
                    Inc(CellRect1.Top, FCellSpacing + RUV + Y0);
                    {$ELSE}
                    Inc(CellRect1.Top, CS2 + RUV + Y0);
                    {$ENDIF}
                    CellRect1.Right := CellRect1.Left + 2*FCellPadding + ColTextWidth[CurCol]
                      {$IFnDEF IP_LAZARUS}
                      + 2*CS2
                      {$ENDIF}
                      ;
                    for k := 1 to CellNode.ColSpan - 1 do
                      Inc(CellRect1.Right, ColTextWidth[CurCol + k] + 2*FCellPadding +
                        {$IFDEF IP_LAZARUS}
                        2*RUH + FCellSpacing);
                        {$ELSE}
                        2*CS2 + RUH);
                        {$ENDIF}

                    Inc(CellRect1.Top, FCellPadding);
                    {$IFDEF IP_LAZARUS}
                    inflateRect(CellRect1, -FCellPadding, 0);
                    {$ELSE}
                    InflateRect(CellRect1, -(FCellPadding + CS2), 0);
                    {$ENDIF}

                    case CellNode.Align of
                    haDefault : ;
                    else
                      AL := CellNode.Align;
                    end;

                    CellNode.Props.VAlignment := VA;
                    CellNode.Props.Alignment := AL;
                    CellNode.Layout(CellNode.Props, CellRect1);

                    if CellNode.Height.PixelsType <> hpUndefined then
                      if CellNode.PageRect.Bottom - CellNode.PageRect.Top < CellNode.Height.Value then
                        CellNode.Layouter.FPageRect.Bottom := CellRect1.Top + CellNode.Height.Value;

                    if (CellNode.Height.PixelsType = hpUndefined)
                    and IsRectEmpty(CellNode.PageRect) then
                      CellNode.FPadRect.Bottom := CellRect1.Top + FCellPadding
                    else begin
                      CellNode.FPadRect.Bottom := CellNode.PageRect.Bottom + FCellPadding;
                    end;
                    SetSpanRect(CellNode.RowSpan - 1, CurCol, @CellNode.PadRect);

                  end;

                  for k := 0 to Pred(CellNode.ColSpan) do begin
                    FRowSp[CurCol] := CellNode.RowSpan - 1;
                    Inc(CurCol);
                  end;
                end;

              maxYY := 0;
              maxY := 0;

              for zz := 0 to Pred(ColCount) do
                maxY := MaxI2(GetSpanBottom(0, zz), maxY);
              for zz := 0 to Pred(ColCount) do
                SetSpanBottom(0, zz, maxY);
              if maxY > maxYY then
                maxYY := maxY;

              {Adjust any trailing spanning columns}
              for j := CurCol to Pred(ColCount) do
                if FRowSp[j] > 0 then
                  FRowSp[j] := FRowSp[j] - 1;

              {$IFDEF IP_LAZARUS}
              TargetRect.Top := MaxI2(maxYY, TargetRect.Top) + RUV;
              {$ELSE}
              TargetRect.Top := MaxI2(maxYY, TargetRect.Top);
              {$ENDIF}
              DeleteFirstSpanRow;
            end;
          end;
        end;

      while RowFixupCount > 0  do begin
        maxYY := 0;
        maxY := 0;
        for zz := 0 to Pred(ColCount) do
          maxY := MaxI2(GetSpanBottom(0, zz), maxY);
        for zz := 0 to Pred(ColCount) do
          SetSpanBottom(0, zz, maxY);
        if maxY > maxYY then
          maxYY := maxY;

        TargetRect.Top := MaxI2(maxYY, TargetRect.Top);
        DeleteFirstSpanRow;
      end;

    finally
      RowSp2.Free;
    end;
  end;

var
  P : Integer;
  CoreNode: TIpHtmlNodeCore;
  TrNode: TIpHtmlNodeTR;
  CellNode: TIpHtmlNodeTableHeaderOrCell;
begin
  FTableWidth := 0;
  if ColCount = 0 then
    Exit;
  Props.Assign(RenderProps);
  CalcMinMaxColTableWidth(Props, MinW, MaxW);

  case FTableOwner.Width.LengthType of
  hlUndefined :
    begin
      P := 0;
      for z := 0 to Pred(FTableOwner.ChildCount) do
        if FTableOwner.ChildNode[z] is TIpHtmlNodeTHeadFootBody then
        begin
          CoreNode := TIpHtmlNodeCore(FTableOwner.ChildNode[z]);
          for i := 0 to Pred(CoreNode.ChildCount) do begin
            if CoreNode.ChildNode[i] is TIpHtmlNodeTR then
            begin
              TrNode := TIpHtmlNodeTR(CoreNode.ChildNode[i]);
              for j := 0 to Pred(TrNode.ChildCount) do
                if TrNode.ChildNode[j] is TIpHtmlNodeTableHeaderOrCell then
                begin
                  CellNode := TIpHtmlNodeTableHeaderOrCell(TrNode.ChildNode[j]);
                  case CellNode.Width.LengthType of
                  hlPercent :
                    Inc(P, CellNode.Width.LengthValue);
                  end;
                end;
            end;
          end;
        end;
      if P <> 0 then
        FTableWidth := MaxI2(MinW, round((P * ParentWidth) / 100))
      else
        FTableWidth := MaxI2(MinW, MinI2(MaxW, ParentWidth));
    end;
  hlAbsolute :
    FTableWidth := MaxI2(FTableOwner.Width.LengthValue, MinW);
  hlPercent :
    FTableWidth := MaxI2(MinW, round((FTableOwner.Width.LengthValue * ParentWidth) / 100));
  end;

  for i := 0 to Pred(ColCount) do
    ColTextWidth[i] := ColTextWidthMin[i];
  for z := 0 to Pred(ColCount) do
    FRowSp[z] := 0;

  for z := 0 to Pred(FTableOwner.ChildCount) do
    if FTableOwner.ChildNode[z] is TIpHtmlNodeTHeadFootBody then
    begin
      CoreNode := TIpHtmlNodeCore(FTableOwner.ChildNode[z]);
      for i := 0 to Pred(CoreNode.ChildCount) do begin
        if CoreNode.ChildNode[i] is TIpHtmlNodeTR then
        begin
          TrNode := TIpHtmlNodeTR(CoreNode.ChildNode[i]);
          CellSpace := FTableWidth - CellOverhead;
          for j := 0 to Pred(ColCount) do
            Dec(CellSpace, ColTextWidth[j]);

          if CellSpace > 0 then begin
            {distribute extra space}
            CurCol := 0;
            while FRowSp[CurCol] <> 0 do begin
              FRowSp[CurCol] := FRowSp[CurCol] - 1;
              Inc(CurCol);
            end;
            for j := 0 to Pred(TrNode.ChildCount) do
              if TrNode.ChildNode[j] is TIpHtmlNodeTableHeaderOrCell then
              begin
                CellNode := TIpHtmlNodeTableHeaderOrCell(TrNode.ChildNode[j]);
                case CellNode.Width.LengthType of
                hlAbsolute :
                  AdjustCol(CellNode.ColSpan, CellNode.Width.LengthValue - 2*FCellPadding
                  {$IFDEF IP_LAZARUS}
                    - FCellSpacing - RUH);
                  {$ELSE}
                    - 2*CS2 - RUH);
                  {$ENDIF}
                hlPercent :
                  AdjustCol(CellNode.Colspan,
                            round((FTableWidth - CellOverhead) *
                                  CellNode.Width.LengthValue / 100));
                end;

                CellSpace := FTableWidth - CellOverhead;
                for k := 0 to Pred(ColCount) do
                  Dec(CellSpace, ColTextWidth[k]);

                for k := 0 to Pred(CellNode.ColSpan) do begin
                  while FRowSp[CurCol] <> 0 do begin
                    FRowSp[CurCol] := FRowSp[CurCol] - 1;
                    Inc(CurCol);
                  end;
                  FRowSp[CurCol] := CellNode.RowSpan - 1;
                  Inc(CurCol);
                end;
              end;
            for j := CurCol to Pred(ColCount) do
              if FRowSp[j] > 0 then
                FRowSp[j] := FRowSp[j] - 1;
          end;
        end;
      end;
    end;

  GrossCellSpace := MaxI2(FTableWidth - CellOverhead, 0);
  NetCellSpace := 0;
  for i := 0 to Pred(ColCount) do
    Inc(NetCellSpace, ColTextWidth[i]);
  if NetCellSpace > 0 then begin
    CellExtra := GrossCellSpace - NetCellSpace;
    if CellExtra > 0 then
      for i := 0 to Pred(ColCount) do begin
        RelCellExtra := round(CellExtra / NetCellSpace * ColTextWidth[i] );
        if ColTextWidth[i] + RelCellExtra > ColTextWidthMax[i] then
          ColTextWidth[i] := MaxI2(ColTextWidth[i], ColTextWidthMax[i])
        else
          ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
      end;
  end;

  NetCellSpace := 0;
  for i := 0 to Pred(ColCount) do
    Inc(NetCellSpace, ColTextWidth[i]);
  CellExtra := GrossCellSpace - NetCellSpace;
  if CellExtra > 0 then begin
    RelCellExtra := CellExtra div ColCount;
    NetCellSpaceExtraExtra := CellExtra mod ColCount;
    for i := 0 to Pred(ColCount) do begin
      if (ColTextWidth[i] < ColTextWidthMax[i]) then begin
        ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
        if NetCellSpaceExtraExtra > 0 then begin
          ColTextWidth[i] := ColTextWidth[i] + 1;
          Dec(NetCellSpaceExtraExtra);
        end;
      end;
    end;
  end;
  NetCellSpace := 0;
  for i := 0 to Pred(ColCount) do
    Inc(NetCellSpace, ColTextWidth[i]);
  CellExtra := GrossCellSpace - NetCellSpace;
  if CellExtra > 0 then begin
    for i := 0 to Pred(ColCount) do begin
      RelCellExtra := MinI2(ColTextWidthMax[i] - ColTextWidth[i], CellExtra);
      if RelCellExtra > 0 then begin
        ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
        Dec(CellExtra, RelCellExtra);
      end;
    end;
  end;
  NetCellSpace := 0;
  for i := 0 to Pred(ColCount) do
    Inc(NetCellSpace, ColTextWidth[i]);
  CellExtra := GrossCellSpace - NetCellSpace;
  if CellExtra > 0 then begin
    RelCellExtra := CellExtra div ColCount;
    NetCellSpaceExtraExtra := CellExtra mod ColCount;
    for i := 0 to Pred(ColCount) do begin
      ColTextWidth[i] := ColTextWidth[i] + RelCellExtra;
      if NetCellSpaceExtraExtra > 0 then begin
        ColTextWidth[i] := ColTextWidth[i] + 1;
        Dec(NetCellSpaceExtraExtra);
      end;
    end;
  end;

  for i := 0 to Pred(ColCount) do
    FRowSp[i] := 0;

  TargetRect := Rect(0, 0, ParentWidth, MaxInt);

  with FTableOwner do
  begin
    BorderRect2 := TargetRect;
    BorderRect := TargetRect;
    for z := 0 to Pred(ChildCount) do
      if ChildNode[z] is TIpHtmlNodeCAPTION then begin
        FCaption := TIpHtmlNodeCAPTION(ChildNode[z]);
        if FCaption.Align <> hva2Bottom then begin
          FCaption.Layout(Props, BorderRect2);
          Inc(BorderRect.Top, FCaption.PageRect.Bottom - FCaption.PageRect.Top);
        end;
      end;
    TargetRect := BorderRect;
    R := BorderRect;
  end;

  {$IFDEF IP_LAZARUS}
  ColStart[0] := BL + FCellSpacing + RUH;
  {$ELSE}
  ColStart[0] := BL + CS2 + RUH;
  {$ENDIF}
  FRowSp[0] := 0;
  for i := 1 to Pred(ColCount) do begin
    ColStart[i] := ColStart[i-1] + 2*FCellPadding + ColTextWidth[i-1]
      {$IFDEF IP_LAZARUS}
      + FCellSpacing + 2*RUH;
      {$ELSE}
      + 2*CS2 + RUH;
      {$ENDIF}
    FRowSp[i] := 0;
  end;

  {calc size of table body}
  Inc(TargetRect.Top, BT);

  {calc rows}
  RowFixup := TRectRectArr.Create;
  try
    RowFixupCount := 0;
    DoBlock(TIpHtmlNodeTHEAD);
    DoBlock(TIpHtmlNodeTBODY);
    DoBlock(TIpHtmlNodeTFOOT);
  finally
    RowFixup.Free;
  end;

  {$IFDEF IP_LAZARUS}
  Inc(TargetRect.Top, FCellSpacing + RUV + BB);
  {$ELSE}
  Inc(TargetRect.Top, CS2 + RUV + BB);
  {$ENDIF}

  R.Right := R.Left + FTableWidth;
  R.Bottom := TargetRect.Top;

  if (R.Bottom > R.Top) and (R.Right = R.Left) then
    R.Right := R.Left + 1;

  with FTableOwner do
  begin
    BorderRect.BottomRight := R.BottomRight;
    BorderRect2.BottomRight := R.BottomRight;
    if assigned(FCaption) and (FCaption.Align = hva2Bottom) then begin
      R.Top := BorderRect.Bottom;
      R.Bottom := MaxInt;
      FCaption.Layout(Props, R);
      BorderRect2.Bottom := FCaption.PageRect.Bottom;
    end;
  end;
end;

function TIpNodeTableLayouter.GetColCount: Integer;
var
  z, i, j, c : Integer;
  Brd : Integer;         // Border
  CoreNode: TIpHtmlNodeCore;
  TrNode: TIpHtmlNodeTR;
  CellNode: TIpHtmlNodeTableHeaderOrCell;
begin
  if FColCount = -1 then
  begin
    FColCount := 0;
    for z := 0 to Pred(FTableOwner.ChildCount) do
      if FTableOwner.ChildNode[z] is TIpHtmlNodeTHeadFootBody then
      begin
        CoreNode := TIpHtmlNodeCore(FTableOwner.ChildNode[z]);
        for i := 0 to Pred(CoreNode.ChildCount) do begin
          c := 0;
          if CoreNode.ChildNode[i] is TIpHtmlNodeTR then
          begin
            TrNode := TIpHtmlNodeTR(CoreNode.ChildNode[i]);
            for j := 0 to Pred(TrNode.ChildCount) do
              if TrNode.ChildNode[j] is TIpHtmlNodeTableHeaderOrCell then
              begin
                CellNode := TIpHtmlNodeTableHeaderOrCell(TrNode.ChildNode[j]);
                Inc(c, CellNode.Colspan);
              end;
          end;
          if c > FColCount then
            FColCount := c;
        end;
      end;
    {$IFnDEF IP_LAZARUS}
    CS2 := FCellSpacing div 2;
    if (FCellSpacing > 0) and (CS2 = 0) then
      CS2 := 1;
    {$ENDIF}
    RUH := 0;
    RUV := 0;
    case FTableOwner.Rules of
      hrNone :;
      hrGroups : begin
          RUH := 1;
          RUV := 1;
        end;
      hrRows :
        RUV := 1;
      hrCols :
        RUH := 1;
      hrAll : begin
          RUH := 1;
          RUV := 1;
        end;
    end;
    BL := 0; BR := 0;
    BT := 0; BB := 0;
    Brd := FTableOwner.Border;
    case FTableOwner.Frame of
      hfVoid,
      hfAbove :
        BT := Brd;
      hfBelow :
        BB := Brd;
      hfHSides : begin
          BT := Brd;
          BB := Brd;
        end;
      hfLhs :
        BL := Brd;
      hfRhs :
        BR := Brd;
      hfvSides : begin
          BL := Brd;
          BR := Brd;
        end;
      hfBox,
      hfBorder : begin
          BT := Brd;
          BB := Brd;
          BL := Brd;
          BR := Brd;
        end;
    end;
  end;
  Result := FColCount;
end;


initialization
  TableLayouterClass := TIpNodeTableLayouter;

end.

