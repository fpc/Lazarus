{ $Id$ }
{******************************************************************************
                                  TInterfaceBase

                                   WinApi stuff

  !! In this file only winapi related code as defined in winapih.inc
     Most routines implement only the default

  !! Keep this alphabetical !!
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
//##apiwiz##sps##   // Do not remove

function TInterfaceBase.Arc(DC: HDC;
  x,y,width,height,angle1,angle2 : Integer): Boolean;
var
  Points : PPoint;
  Count : Longint;
begin
  Result := False;
  Points := nil;
  Count := 0;
  PolyBezierArcPoints(X,Y,Width,Height,Angle1, Angle2, 0, Points, Count);
  Polygon(DC, Points, Count, False);
  ReallocMem(Points, 0);
  Result := True;
end;

function TInterfaceBase.AngleChord(DC: HDC;
  x,y,width,height,angle1,angle2 : Integer): Boolean;
var
  Points : PPoint;
  Count : Longint;
begin
  Result := False;
  Points := nil;
  Count := 0;
  PolyBezierArcPoints(X,Y,Width,Height,Angle1, Angle2, 0, Points, Count);
  Inc(Count);
  ReallocMem(Points, Count*SizeOf(TPoint));
  Points[Count - 1] := Points[0];
  Polygon(DC, Points, Count, True);
  ReallocMem(Points, 0);
  Result := True;
end;

function TInterfaceBase.BeginPaint(Handle: hWnd; Var PS: TPaintStruct): hdc;
begin
  Result:=GetDC(Handle);
end;

function TInterfaceBase.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := False;
end;

Function TInterfaceBase.BringWindowToTop(hWnd : HWND): Boolean;
begin
  Result := false;
end;

function TInterfaceBase.CallNextHookEx(hhk : HHOOK; ncode : Integer;
  WParam: WParam; LParam: LParam) : Integer;
begin
  Result := 0;
end;

function TInterfaceBase.CallWindowProc(lpPrevWndFunc: TFarProc; Handle: HWND;
  Msg: UINT; WParam: WParam; LParam: LParam): Integer;
begin
  Result:=0;
end;

Function TInterfaceBase.ClienttoScreen(Handle : HWND; var P : TPoint) : Boolean;
Begin
  Result := False;
end;

Function TInterfaceBase.CombineRgn(Dest, Src1, Src2 : HRGN;
  fnCombineMode : Longint) : Longint;
begin
  Result := ERROR;
end;

function TInterfaceBase.CreateBitmap(Width, Height: Integer;
  Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
begin
  Result := 0;
end;

function TInterfaceBase.CreateDIBitmap(DC: HDC;
  var InfoHeader: TBitmapInfoHeader;
  dwUsage: DWORD; InitBits: PChar; var InitInfo: TBitmapInfo;
  wUsage: UINT): HBITMAP;
begin
  Result := 0;
end;

function TInterfaceBase.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
begin
  Result := 0;
end;

function TInterfaceBase.CreateCaret(Handle : HWND; Bitmap : hBitmap; width, Height : Integer) : Boolean;
begin
  Result := False;
end;

function TInterfaceBase.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  Result := 0;
end;

function TInterfaceBase.CreateCompatibleDC(DC: HDC): HDC;
begin
  Result := 0;
end;

function TInterfaceBase.CreateDIBSection(DC: HDC;
  const BitmapInfo: tagBitmapInfo; Usage: UINT;
  var Bits: Pointer; SectionHandle: THandle; Offset: DWORD): HBITMAP;
begin
  Result := 0;
end;

function TInterfaceBase.CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN;
begin
  Result:=ERROR;
  writeln('WARNING: CreateEllipticRgn not yet implemented.');
end;

function TInterfaceBase.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := 0;
end;

function TInterfaceBase.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  // this functions is needed, because the fontname in TLogFont is limited to
  // 32 characters. If the interface does not support long font names, it can
  // simple omitt this function
  Result := CreateFontIndirect(LogFont);
end;

function TInterfaceBase.CreatePalette(const LogPalette: TLogPalette): HPalette;
begin
  Result := 0;
end;

function TInterfaceBase.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result := 0;
end;

function TInterfaceBase.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
Begin
  Result := 0;
end;

function TInterfaceBase.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
begin
  Result := 0;
end;

procedure TInterfaceBase.DeleteCriticalSection(var CritSection: TCriticalSection);
begin
  writeln('TInterfaceBase.DeleteCriticalSection Not implemented yet');
end;

function TInterfaceBase.DeleteDC(hDC: HDC): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.DestroyCaret(Handle : HWND): Boolean;
begin
  Result := False;
end;

function TInterfacebase.DrawFrameControl(DC: HDC; var Rect : TRect; uType, uState : Cardinal) : Boolean;
begin
  Result := False;
end;

function TInterfaceBase.DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean;
Begin
  Result := False;
end;

function TInterfaceBase.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var Rect: TRect; Flags: Cardinal): Integer;
var
  AP : TSize;
  TM : TTextmetric;
  theRect     : TRect;
  aLeft,aTop,
  I       : Integer;
  Lines : TStrings;
  TDC : hDC;
  pStr : PChar;
  tmpString,
  AStr : String;
  pIndex,
  pX1, pX2, pY : Longint;
  B, P : Longint;
  LogP : TLogPen;
  MaxLength : Integer;
  Pt : TPoint;

  Function LeftOffset : Longint;
  begin
    If (Flags and DT_Right) = DT_Right then
      Result := DT_Right
    else
      If (Flags and DT_CENTER) = DT_CENTER then
        Result := DT_CENTER
    else
      Result := DT_LEFT;
  end;

  Function TopOffset : Longint;
  begin
    If (Flags and DT_BOTTOM) = DT_BOTTOM then
      Result := DT_BOTTOM
    else
      If (Flags and DT_VCENTER) = DT_VCENTER then
        Result := DT_VCENTER
    else
      Result := DT_Top;
  end;

  Function SingleLine : Boolean;
  begin
    Result := (Flags and DT_SingleLine) = DT_SingleLine;
  end;

  Function WordWrap : Boolean;
  begin
    Result := (Flags and DT_WordBreak) = DT_WordBreak;
  end;

  Function CalcRect : Boolean;
  begin
    Result := (Flags and DT_CalcRect) = DT_CalcRect;
  end;

  Function NOCLIP : Boolean;
  begin
    Result := (Flags and DT_NOCLIP) = DT_NOCLIP;
  end;

  Function NoPrefix : Boolean;
  begin
    Result := (Flags and DT_NoPrefix) = DT_NoPrefix;
  end;

  Function Breakable(Breaks : TList; Index : Integer) : Boolean;
  begin
    If not Assigned(Breaks) then
      exit;
    Result := Breaks.IndexOf(Pointer(Index)) <> -1;
  end;

  Function NextBreakable(Breaks : TList; Index : Integer) : Integer;
  begin
    Result := -1;
    If (not Assigned(Breaks)) or
      (not Breakable(Breaks,Index))
    then
      exit;
    If Breaks.IndexOf(Pointer(Index)) >= Breaks.Count - 1 then
      exit;
    Result := Longint(Breaks[Breaks.IndexOf(Pointer(Index)) + 1]);
  end;

  Function GetBreakablePoints(const Source : String) : TList;
  var
    I : Integer;
  begin
    Result := TList.Create;
    If Length(Source) < 1 then
      exit;
    For I := 1 to Length(Source) do
      If Source[I] = ' ' then
        If not Breakable(Result, I) then
          Result.Add(Pointer(I));
    If not Breakable(Result, Length(Source)) then
      Result.Add(Pointer(Length(Source)));
    If not Breakable(Result, 0) then
      Result.Insert(0,nil);
  end;

  Function TextExtent(Handle : hDC; const Source : String) : TSize;
  var
    pStr : PChar;
  begin
    pStr := StrAlloc(Length(Source)+1);
    try
      StrPCopy(pStr, Source);
      GetTextExtentPoint(Handle, pStr, Length(Source), Result);
    finally
      StrDispose(PStr);
    end;
  end;

  Function GetStringLength(Handle : hDC; const Source : String;
    FromPos, ToPos : Integer) : Integer;
  var
    Tmp : String;
  begin
    Tmp := Copy(Source,FromPos,ToPos - FromPos);
    Result := TextExtent(Handle, Tmp).cX;
    SetLength(Tmp,0);
  end;

  Function GetStringHeight(Handle : hDC; const Source : String;
    FromPos, ToPos : Integer) : Integer;
  var
    Tmp : String;
  begin
    Tmp := Copy(Source,FromPos,ToPos - FromPos);
    Result := TextExtent(Handle,Tmp).cY;
    SetLength(Tmp,0);
  end;

  Function BreakString(const Source : String) : TStrings;
  var
    I, FromPos, ToPos : Integer;
    Breaks : TList;
  begin
    Result := TStringList.Create;
    Breaks := GetBreakablePoints(Source);
    If Breaks.Count <= 0 then begin
      Result.Append(Source);
      Breaks.Free;
      exit;
    end;
    FromPos := 1;
    ToPos := 0;
    I := 1;
    Repeat
      If Breakable(Breaks,I) then begin
        If NextBreakable(Breaks,I) <> -1 then begin
          TmpString := Copy(Source, FromPos, NextBreakable(Breaks,I));
          If not NoPrefix then
            DeleteAmpersands(tmpString);
          If TextExtent(DC, TmpString).cX > MaxLength
          then begin
            ToPos := I;
            Result.Append(Copy(Source,FromPos,ToPos - FromPos + 1));
            FromPos := ToPos + 1;
            I := FromPos;
          end
          Else
            I := NextBreakable(Breaks,I);
        end
        else begin
          ToPos := I;
          Result.Append(Copy(Source,FromPos,ToPos - FromPos + 1));
          FromPos := ToPos + 1;
          I := FromPos;
        end;
      end else
        I := I + 1;
    until I > Length(Source);
    SetLength(TmpString,0);
    Breaks.Free;
  end;

  Function DoBreakString(const AStr : String) : TStrings;
  var
    TS : TStrings;
    Num : Longint;
    OldText, NewText : String;
  begin
    Result := TStringList.Create;
    If not SingleLine then begin
      OldText := AStr;
      Num := pos(#10,OldText);
      while Num > 0 do begin
        NewText := Copy(OldText,1,Num);
        Case OldText[Num] of
          #13,#10 : Delete(NewText,Num,1);
        end;
        If Num -1 > 0 then
          Case OldText[Num-1] of
            #13,#10 : Delete(NewText,Num-1,1);
          end;
        If WordWrap then begin
          TS := BreakString(Copy(NewText,1,Length(NewText)));
          Result.AddStrings(TS);
          TS.Free;
        end
        else
          Result.Append(Copy(NewText,1,Length(NewText)));
        Delete(OldText,1,Num);
        Num := pos(#10,OldText);
        SetLength(NewText,0);
      end;
      if OldText <> '' then
        If WordWrap then begin
          TS := BreakString(Copy(OldText,1,Length(OldText)));
          Result.AddStrings(TS);
          TS.Free;
        end
        else
          Result.Append(Copy(OldText,1,Length(OldText)));
    end
    else
      Result.Append(AStr);
  end;

  Procedure CalcTextRect;
  var
    J, wT,hT : Integer;
  begin
    Rect:=theRect;
    {Initialize text width/height}
    wT := Tm.tmAveCharWidth*StrLen(Str);
    hT := TM.tmHeight;

    If Lines.Count = 1 then begin
      {Get text width/height if only one line}
      AStr := Lines[0];
      If not NoPrefix then
        DeleteAmpersands(aStr);
      hT := TM.tmHeight;
      wT := GetStringLength(DC, AStr,1, Length(AStr) + 1);
    end
    else begin
      {Get text width/height if more than one line}
      hT := hT* (Lines.Count);
      wT := 0;
      For J := 0 to Lines.Count - 1 do begin
        AStr := Lines[J];
        If not NoPrefix then
          DeleteAmpersands(aStr);
        If wT < GetStringLength(DC, AStr,1, Length(AStr) + 1)
        then
          wT := GetStringLength(DC, AStr,1, Length(AStr) + 1);
      end;
    end;

    theRect.Right := theRect.Left + wT;
    If not CalcRect then
      Case LeftOffset of
        DT_CENTER :
          OffsetRect(theRect, (Rect.Right - theRect.Right) div 2, 0);
        DT_Right :
          OffsetRect(theRect, Rect.Right - theRect.Right, 0);
      end;

    theRect.Bottom := theRect.Top + hT;

    {If SingleLine allow vertical offset}
    If not CalcRect then
      If SingleLine then
        Case TopOffset of
          DT_VCENTER :
            OffsetRect(theRect, 0, (Rect.Bottom - theRect.Bottom) div 2);
          DT_Bottom :
            OffsetRect(theRect, 0, Rect.Bottom - theRect.Bottom);
        end;
  end;

begin
  Result := 0;
  Lines := nil;

  {Get accurate string length, if none was given}
  If Count < 0 then
    Count := StrLen(Str);

  {Calculate # Lines, etc.}
  pStr := StrAlloc(Count + 1);
  try
    StrLCopy(pStr, Str, Count);
    pStr[Count] := #0;
    AStr := String(pStr);

    tmpString := Copy(AStr, 1, Length(ASTR));

    {Get font & string metrics}
    GetTextMetrics(DC, TM);
    If not NoPrefix then
      DeleteAmpersands(tmpString);

    If tmpString > '' then begin
      AP:=TextExtent(DC, tmpString);
      AP.cX := AP.cX div Length(tmpString);
    end
    else begin
      AP.cY := TM.tmHeight;
      AP.cX := TM.tmAveCharWidth;
    end;

    {Break string into individual lines}
    MaxLength := (Rect.Right - Rect.Left);
    Lines := DoBreakString(AStr);
  finally
    StrDispose(pStr);
  end;

  {Error occcured...}
  If Lines = nil then
    exit;

  {Calculate the text's bounding rect}
  CalcTextRect;

  {If just calculating rect, finish up here}
  If CalcRect then begin
    theRect:=Rect;
    Lines.Free;
    exit;
  end;

  {Backup device-context}
  TDC := SaveDC(DC);

  {Set clipping area if enabled}
  If not NOCLIP then begin
    If theRect.Right > Rect.Right then
      theRect.Right := Rect.Right;
    If theRect.Bottom > Rect.Bottom then
      theRect.Bottom := Rect.Bottom;
    IntersectClipRect(DC, theRect.Left, theRect.Top,
      theRect.Right, theRect.Bottom);
  end;

  {Select NULL brush}
  B := SelectObject(DC, GetStockObject(NULL_BRUSH));

  {Create & select pen of font color}
  LogP.lopnStyle := PS_SOLID;
  LogP.lopnWidth.X := 1;
  LogP.lopnColor := GetTextColor(DC);
  P := SelectObject(DC, CreatePenIndirect(LogP));

  For I := 0 to Lines.Count - 1 do begin
    {Set vertical position for line}
    aTop := theRect.Top + I*TM.tmHeight;

    If (aTop >= Rect.Top) and (aTop <= Rect.Bottom - TM.tmHeight)
    then begin
      AStr := Lines[I];

      {Remove ampersands & get index of prefix}
      If not NoPrefix then
        pIndex := DeleteAmpersands(aStr)
      else
        pIndex := -1;

      {Offset line according to orientation}
      Case LeftOffset of
        DT_Left:
          aLeft := theRect.Left;
        DT_Center:
          aLeft := theRect.Left + (theRect.Right - theRect.Left) div 2
                   - TextExtent(DC, aStr).cX div 2;
        DT_Right:
          aLeft := theRect.Right - TextExtent(DC, AStr).cX;
      end;

      {Draw line of Text}
      TextOut(DC, aLeft, aTop, PChar(AStr), Length(AStr));

      {Prefix drawing}
      If pIndex > 0 then begin
        {Get prefix line position}
        pX1 := aLeft + GetStringLength(DC, AStr, 1, pIndex);
        pX2 := pX1 + GetStringLength(DC, AStr, pIndex, pIndex + 1);
        pY := aTop + tm.tmHeight - TM.tmDescent + 1;

        {Draw prefix line}
        MoveToEx(DC, pX1, PY, @Pt);
        LineTo(DC, pX2, pY);

        {Reset pen position}
        MoveToEx(DC, Pt.X, Pt.Y, nil);
      end;
    end;
  end;

  {Reset brush}
  SelectObject(DC, B);

  {Reset pen}
  DeleteObject(SelectObject(DC, P));

  {Finalize Lines}
  Lines.Free;

  {Restore device-context}
  RestoreDC(DC, TDC);

  Result := 1;
end;

function TInterfaceBase.DPtoLP(DC: HDC; var Points; Count: Integer): BOOL;
begin
  Result := False;
end;

function TInterfaceBase.EnableScrollBar(Wnd: HWND;
  wSBflags, wArrows: Cardinal): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Result := False;
end;

Function TInterfaceBase.EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
Begin
  Result:=1;
end;

procedure TInterfaceBase.EnterCriticalSection(var CritSection: TCriticalSection);
begin
  writeln('TInterfaceBase.EnterCriticalSection Not implemented yet');
end;

function TInterfaceBase.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
var
  Points : PPoint;
  Count : Longint;
  X, Y : Longint;
begin
  Result := False;
  Points := nil;
  Count := 0;
  If X2 < X1 then begin
    X := X2;
    X2 := X1;
    X1 := X;
  end;
  If Y2 < Y1 then begin
    Y := Y2;
    Y2 := Y1;
    Y1 := Y;
  end;
  If (ABS(Y2 - Y1) > 0) and (ABS(X2 - X1) > 0) then begin
    PolyBezierArcPoints(x1, y1, x2 - x1, y2 - y1,0,360*16,0,Points,Count);
    Polygon(DC, Points, Count, True);
    ReallocMem(Points, 0);
  end;
  Result := True;
end;

function TInterfaceBase.ExcludeClipRect(dc: hdc;
  Left, Top, Right, Bottom : Integer) : Integer;
var
  RRGN : hRGN;
begin
  If DCClipRegionValid(DC) then begin
    //writeln('TInterfaceBase.ExcludeClipRect A DC=',HexStr(Cardinal(DC),8),' Rect=',Left,',',Top,',',Right,',',Bottom);
    // create the rectangle region, that should be excluded
    RRGN := CreateRectRgn(Left,Top,Right,Bottom);
    Result := ExtSelectClipRGN(DC, RRGN, RGN_DIFF);
    //writeln('TInterfaceBase.ExcludeClipRect B Result=',Result);
    DeleteObject(RRGN);
  end else
    Result:=ERROR;
end;

function TInterfaceBase.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.ExtSelectClipRGN(dc: hdc; rgn: hrgn; Mode: Longint
  ): Integer;
var
  OldC, Clip : hRGN;
begin
  OldC := CreateEmptyRegion;
  GetClipRGN(DC, OldC);
  Clip := CreateEmptyRegion;
  //writeln('TInterfaceBase.ExtSelectClipRGN A OldC=',HexStr(Cardinal(OldC),8),
  //  ' Clip=',HexStr(Cardinal(Clip),8),' RGn=',HexStr(Cardinal(RGN),8),' Mode=',Mode);
  Result := CombineRGN(Clip, OldC, RGN, Mode);
  //writeln('TInterfaceBase.ExtSelectClipRGN B Result=',Result);
  If Result <> ERROR then
    Result := SelectClipRGN(DC, Clip);
  DeleteObject(Clip);
  DeleteObject(OldC);
end;

function TInterfaceBase.FillRect(DC: HDC; const Rect: TRect;
  Brush: HBRUSH): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.FloodFill(DC: HDC; X, Y: Integer;
  Color: TGraphicsColor;
  FillStyle: TGraphicsFillStyle;
  Brush: HBRUSH): Boolean;
begin
  Result := false;
end;

function TInterfaceBase.FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): BOOL;
begin
  Result := false;
end;

function TInterfaceBase.FrameRect(DC: HDC; const ARect: TRect;
  hBr: HBRUSH) : integer;
begin
  Result:= 0;
end;

Function TInterfaceBase.GetActiveWindow : HWND;
begin
  Result := 0;
end;

function TInterfaceBase.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;
  Bits: Pointer): Longint;
begin
  Result := 0;
end;

function TInterfaceBase.GetCapture : HWND;
begin
  Result := 0;
end;

function TInterfaceBase.GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.GetClientRect(handle : HWND; var Rect : TRect) : Boolean;
begin
  Result := False;
end;

Function TInterfaceBase.GetClipBox(DC : hDC; lpRect : PRect) : Longint;
begin
  lpRect^ := Rect(0,0,0,0);
  Result := SIMPLEREGION;
end;

Function TInterfaceBase.GetClipRGN(DC : hDC; RGN : hRGN) : Longint;
begin
  Result := -1;
end;

function TInterfaceBase.GetCursorPos(var lpPoint: TPoint): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.GetDC(hWnd: HWND): HDC;
begin
  Result := 0;
end;

function TInterfaceBase.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  Result := 0;
end;

function TInterfaceBase.GetDIBits(DC: HDC; Bitmap: HBitmap;
  StartScan, NumScans: UINT; Bits: Pointer;
  var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Result := 0;
end;

function TInterfaceBase.GetFocus: HWND;
begin
  Result := 0;
end;

function TInterfaceBase.GetFontLanguageInfo(DC: HDC): DWord;
begin
  Result := 0;
end;

function TInterfaceBase.GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result := 0;
end;

function TInterfaceBase.GetMapMode(DC: HDC): Integer;
begin
  Result := 0;
end;

function TInterfaceBase.GetObject(GDIObject: HGDIOBJ; BufSize: Integer;
  Buf: Pointer): Integer;
begin
  Result := 0;
end;

function TInterfaceBase.GetPaletteEntries(Palette: HPALETTE;
  StartIndex, NumEntries: UINT; var PaletteEntries): UINT;
begin
  Result := 0;
end;

Function TInterfaceBase.GetParent(Handle : HWND): HWND;
begin
  Result := 0;
end;

Function TInterfaceBase.GetProp(Handle : hwnd; Str : PChar): Pointer;
begin
  Result := nil;
end;

Function TInterfaceBase.GetRGNBox(RGN : HRGN; lpRect : PRect) : Longint;
begin
  Result := SIMPLEREGION;
end;

function TInterfaceBase.GetScrollInfo(Handle: HWND; SBStyle: Integer;
  var ScrollInfo: TScrollInfo): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.GetStockObject(Value: Integer): LongInt;
begin
  Result := 0;
end;

function TInterfaceBase.GetSysColor(nIndex: Integer): DWORD;
begin
  Result := 0;
end;

function TInterfaceBase.GetSystemMetrics(nIndex: Integer): Integer;
begin
  Result := 0;
end;

function TInterfaceBase.GetSystemPaletteEntries(DC: HDC;
  StartIndex, NumEntries: UINT; var PaletteEntries): UINT;
begin
  Result := 0;
end;

function TInterfaceBase.GetTextColor(DC: HDC) : TColorRef;
begin
  Result := 0;
end;

function TInterfaceBase.GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, p4: Integer; p5, p6: PInteger; var Size: TSize): BOOL;
begin
  Result := GetTextExtentPoint(DC,Str,Count,Size);
end;

function TInterfaceBase.GetTextExtentPoint(DC: HDC; Str: PChar;
  Count: Integer; var Size: TSize): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.GetTextExtentPoint32(DC: HDC; Str: PChar;
  Count: Integer; var Size: TSize): Boolean;
begin
  Result := GetTextExtentPoint(DC,Str,Count,Size);
end;

function TInterfaceBase.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.GetWindowLong(Handle : hwnd; int : Integer): Longint;
begin
  Result := 0;
end;

Function TInterfaceBase.GetWindowOrgEx(dc : hdc; P : PPoint): Integer;
Begin
  Result := 0;
  if P<>nil then
    P^:=Point(0,0);
end;

function TInterfaceBase.GetWindowRect(Handle : hwnd; var Rect : TRect): Integer;
{ After the call, ARect will be the control area in screen coordinates.
  That means, Left and Top will be the screen coordinate of the TopLeft pixel
  of the Handle object and Right and Bottom will be the screen coordinate of
  the BottomRight pixel. }
begin
  Result := 0;
end;

function TInterfaceBase.GradientFill(DC: HDC; Vertices: PTriVertex;
  NumVertices : Longint; Meshes: Pointer; NumMeshes : Longint;
  Mode : Longint): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.HideCaret(hWnd: HWND): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.InitHintFont(HintFont: TObject): Boolean;
begin
  Result := false;
end;

procedure TInterfaceBase.InitializeCriticalSection(var CritSection: TCriticalSection);
begin
  writeln('TInterfaceBase.InitializeCriticalSection Not implemented yet');
end;

function TInterfaceBase.IntersectClipRect(dc: hdc;
  Left, Top, Right, Bottom: Integer): Integer;
var
  RRGN : hRGN;
begin
  RRGN := CreateRectRgn(Left, Top, Right, Bottom);
  //writeln('TInterfaceBase.IntersectClipRect A RGN=',HexStr(Cardinal(RRGN),8),' ',Left,',',Top,',',Right,',',Bottom);
  If not DCClipRegionValid(DC) then
    Result := ExtSelectClipRGN(DC, RRGN, RGN_COPY)
  else
    Result := ExtSelectClipRGN(DC, RRGN, RGN_AND);
  DeleteObject(RRGN);
end;

Function TInterfaceBase.InvalidateRect(aHandle : HWND; ARect : pRect;
  bErase : Boolean) : Boolean;
begin
  Result := false;
end;

function TInterfaceBase.IsDBCSLeadByte(TestChar: Byte): boolean;
begin
  Result := false;
end;

function TInterfaceBase.IsWindowVisible(handle: HWND): boolean;
begin
  Result := false;
end;

procedure TInterfaceBase.LeaveCriticalSection(var CritSection: TCriticalSection);
begin
  writeln('TInterfaceBase.LeaveCriticalSection Not implemented yet');
end;

function TInterfaceBase.LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType : Cardinal): integer;
begin
  Result:= 0;
end;

function TInterfaceBase.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer;
  ROp: DWORD): Boolean;
begin
  Result := StretchMaskBlt(DestDC,X,Y,Width,Height,SrcDC,XSrc,YSrc,Width,Height,
                           Mask,XMask,YMask,ROp);
end;

function TInterfaceBase.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer): Boolean;
begin
  Result := MaskBlt(DestDC,X,Y,Width,Height,SrcDC,XSrc,YSrc,
                    Mask,XMask,YMask,SRCCOPY);
end;

function TInterfaceBase.MoveToEx(DC: HDC; X, Y: Integer;
  OldPoint: PPoint): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.PeekMessage(var lpMsg : TMsg; Handle : HWND;
  wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
Begin
  Result := False;
End;

function TInterfaceBase.Pie(DC: HDC;
  EllipseX1,EllipseY1,EllipseX2,EllipseY2,
  StartX,StartY,EndX,EndY: Integer): Boolean;
begin
  Result := RadialPie(DC,
                      Min(EllipseX1,EllipseX2), Min(EllipseY1,EllipseY2),
                      Abs(EllipseX2-EllipseX1), Abs(EllipseY2-EllipseY1),
                      StartX,StartY, EndX,EndY);
end;

function TInterfaceBase.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled,Continuous: boolean): boolean;
var
  APoints : PPoint;
  ACount : Longint;
Begin
  APoints := nil;
  ACount := 0;
  PolyBezier2Polyline(Points,NumPts,APoints,ACount,Continuous);
  If Filled then
    Result := Polygon(DC,APoints,ACount, False)
  else
    Result := Polyline(DC,APoints,ACount);
  ReallocMem(APoints,0);
End;

function TInterfaceBase.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: boolean): boolean;
begin
  Result := false;
end;

function TInterfaceBase.Polyline(DC: HDC; Points: PPoint;
  NumPts: Integer): boolean;
begin
  Result := false;
end;

function TInterfaceBase.PostMessage(Handle: HWND; Msg: Cardinal;
  WParam: WParam; LParam: LParam): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.PtInRegion(RGN: HRGN; X, Y: Integer): Boolean;
begin
  Result := false;
end;

function TInterfaceBase.RealizePalette(DC: HDC): Cardinal;
begin
  Result := 0;
end;

function TInterfaceBase.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: RectVisible
  Params:  dc : hdc; ARect: TRect
  Returns: True if ARect is not completely clipped away.
 ------------------------------------------------------------------------------}
function TInterfaceBase.RectVisible(dc : hdc; const ARect: TRect) : Boolean;
var
  ClipRGN, RectRgn: hRGN;
  Intersection: Cardinal;
  CombineResult: Integer;
begin
  Result:=false;
  if (ARect.Left>=ARect.Right) or (ARect.Top>=ARect.Bottom)
    or not DCClipRegionValid(DC)
  then exit;
  ClipRGN:=CreateEmptyRegion;
  if GetClipRGN(DC,ClipRGN)>0 then begin
    RectRgn:=CreateRectRGN(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
    Intersection:=CreateEmptyRegion;
    CombineResult:=CombineRGN(Intersection,RectRGN,ClipRGN,RGN_AND);
    if CombineResult in [SimpleRegion,ComplexRegion] then
      Result:=true;
    DeleteObject(Intersection);
    DeleteObject(RectRgn);
  end;
  DeleteObject(ClipRGN);
end;

Function TInterfaceBase.ReleaseCapture : Boolean;
Begin
  Result := True;
end;

function TInterfaceBase.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  Result := 0;
end;

function TInterfaceBase.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer) : Boolean;

  Procedure Switch(Var F,T : Integer);
  var
    Tmp : Integer;
  begin
    Tmp := F;
    F := T;
    T := Tmp
  end;

var
  pt : TPoint;
  Pen : hPen;
  Brush : hBrush;
begin
  Result := False;

  If X2 < X1 then
    Switch(X2,X1);

  If Y2 < Y1 then
    Switch(Y2,Y1);

  If ((X2 - X1) < 0) or ((Y2 - Y1) < 0) then
    exit;

  If not ((RX <= 0) or (RY <= 0)) then begin
    If ((X2 - X1) <= RX) or ((X2 - X1) div 2 < RX) then
      RX := (X2 - X1) div 2;

    If ((Y2 - Y1) <= RY) or ((Y2 - Y1) div 2 < RY) then
      RY := (Y2 - Y1) div 2;

    Pen := SelectObject(DC, GetStockObject(NULL_PEN));

    RadialPieWithAngles(DC, X1, Y1, RX, RY, 90*16,90*16);
    RadialPieWithAngles(DC, X2 - RX, Y1, RX, RY, 0, 90*16);
    RadialPieWithAngles(DC, X1, Y2 - RY, RX, RY, 180*16,90*16);
    RadialPieWithAngles(DC, X2 - RX, Y2 - RY, RX, RY, 270*16,90*16);

    Rectangle(DC, X1 + (RX div 2) - 1, Y1, X2 - (RX div 2) + 1, Y2 + 1);
    Rectangle(DC, X1, Y1 + (RY div 2) - 1, X2 + 1, Y2 - (RY div 2) + 1);

    SelectObject(DC, Pen);

    Brush := SelectObject(DC, GetStockObject(NULL_BRUSH));

    Arc(DC, X1, Y1, RX, RY, 90*16,90*16);
    Arc(DC, X2 - RX, Y1, RX, RY, 0, 90*16);
    Arc(DC, X1, Y2 - RY, RX, RY, 180*16,90*16);
    Arc(DC, X2 - RX, Y2 - RY, RX, RY, 270*16,90*16);

    RY := RY div 2;
    RX := RX div 2;

    MoveToEx(DC, X1 + RX, Y1, @pt);
    LineTo(DC, X2 - RX,Y1);

    MoveToEx(DC, X1 + RX, Y1, nil);
    LineTo(DC, X2 - RX, Y1);

    MoveToEx(DC, X1, Y1 + RY - 1,nil);
    LineTo(DC, X1, Y2 - RY);

    MoveToEx(DC, X1 + RX, Y2, nil);
    LineTo(DC, X2 - RX, Y2);

    MoveToEx(DC, X2, Y1 + RY, nil);
    LineTo(DC, X2, Y2 - RY);

    MoveToEx(DC, pt.X, pt.Y, nil);

    SelectObject(DC, Brush);
  end
  else
    Rectangle(DC, X1, Y1, X2, Y2);

  Result := True;
end;

function TInterfaceBase.SaveDC(DC: HDC) : Integer;
begin
  Result := 0;
end;

function TInterfaceBase.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
begin
  Result := 0;
end;

function TInterfaceBase.ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
begin
  Result := ERROR;
end;

function TInterfaceBase.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  Result := 0;
end;

function TInterfaceBase.SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
begin
  Result := 0;
end;

function TInterfaceBase.SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
begin
  Result := 0;
end;

function TInterfaceBase.SetActiveWindow(Handle: HWND): HWND;
begin
  Result:=GetActiveWindow;
end;

function TInterfaceBase.SetBkColor(DC: HDC; Color: TColorRef): TColorRef; //pbd
begin
  Result := 0;
end;

Function TInterfaceBase.SetBkMode(DC: HDC; bkMode : Integer) : Integer;
begin
  Result := 0;
end;

Function TInterfaceBase.SetCapture(value : Longint) : Longint;
Begin
  Result := 0;
End;

function TInterfaceBase.SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.SetCursor(hCursor: HICON): HCURSOR;
begin
  Result := 0;
end;

function TInterfaceBase.SetFocus(hWnd: HWND): HWND;
begin
  Result := 0;
end;

function TInterfacebase.SetMapMode(DC: HDC; MapMode: Integer): Integer;
begin
  Result := 0;
end;

Function TInterfacebase.SetProp(Handle: hwnd; Str : PChar;
  Data : Pointer) : Boolean;
Begin
  Result := True;
end;

function TInterfaceBase.SetScrollInfo(Handle : HWND; SBStyle : Integer;
  ScrollInfo: TScrollInfo; Redraw : Boolean): Integer;
begin
  Result := 0;
end;

function TInterfaceBase.SetStretchBltMode(DC: HDC;
  StretchMode: Integer): Integer;
begin
  Result := 0;
end;

function TInterfaceBase.SetSysColors(cElements: Integer; const lpaElements;
  const lpaRgbValues): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.SetTextAlign(DC: HDC; Flags: UINT): UINT;
begin
  Result := 0;
end;

function TInterfaceBase.SetTextCharacterExtra(_hdc : hdc;
  nCharExtra : Integer):Integer;
begin
  Result := 0;
end;

function TInterfaceBase.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result := CLR_INVALID;
end;

function TInterfacebase.SetWindowLong(Handle: HWND;
  Idx: Integer; NewLong : Longint): LongInt;
begin
  Result := -1;
end;

Function TInterfaceBase.SetWindowOrgEx(dc : hdc; NewX, NewY : Integer;
  OldPoint: PPoint) : Boolean;
Begin
  Result := False;
end;

function TInterfaceBase.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
       X, Y, cx, cy: Integer; uFlags: UINT): Boolean;
begin
  Result:=false;
end;

function TInterfaceBase.ShowCaret(hWnd: HWND): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.ShowScrollBar(Handle: HWND; wBar: Integer;
  bShow: Boolean): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  Result:=false;
end;

procedure TInterfaceBase.Sleep(dwMilliseconds: DWORD);
begin
end;

function TInterfaceBase.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.StretchDIBits(DC: HDC;
  DestX, DestY, DestWidth, DestHeight,
  SrcX, SrcY, SrcWidth, SrcHeight: Integer;
  Bits: Pointer; var BitsInfo: TBitmapInfo;
  Usage: UINT; Rop: DWORD): Integer;
begin
  Result := 0;
end;

Function TInterfaceBase.TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  Result := false;
end;

function TInterfaceBase.UpdateWindow(Handle: HWND): Boolean;
begin
  Result := false;
end;

function TInterfaceBase.VkKeyScan(AChar: Char): Short;
begin
  Result := -1; // $FFFF
end;

Function TInterfaceBase.WindowFromPoint(Point : TPoint) : HWND;
begin
  Result := 0;
end;

//##apiwiz##eps##   // Do not remove
{ =============================================================================

  $Log$
  Revision 1.6  2004/02/17 00:32:25  mattias
  fixed TCustomImage.DoAutoSize fixing uninitialized vars

  Revision 1.5  2004/02/10 00:05:03  mattias
  TSpeedButton now uses MaskBlt

  Revision 1.4  2004/01/10 22:34:20  mattias
  started double buffering for gtk intf

  Revision 1.3  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.2  2003/12/14 19:18:04  micha
  hint fixes: parentfont, font itself, showing/hiding + more

  Revision 1.1  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

}
