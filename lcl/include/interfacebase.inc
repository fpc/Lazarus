{******************************************************************************
                                  TInterfaceBase

                                   WinApi stuff

         No code here (just some dummy results to keep the compiler quiet :-)

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

function TInterfaceBase.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; 
begin
  Result := False;
end;

Function TInterfaceBase.BringWindowToTop(hWnd : HWND): Boolean;
begin
  Result := false;
end;

function TInterfaceBase.CallNextHookEx(hhk : HHOOK; ncode : Integer; wParam, lParam : Longint) : Integer;
begin
  Result := 0;
end;

Function TInterfaceBase.CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND;
  Msg : UINT; wParam : longint; lParam : longint) : Integer;
begin
  Result := 0;
end;

function TInterfaceBase.CheckMenuItem(hndMenu: HMENU; uIDEnableItem: Integer;
  bChecked: Boolean): Boolean;
begin
  Result := false;
end;

Function TInterfaceBase.ClienttoScreen(Handle : HWND; var P : TPoint) : Boolean;
Begin
  Result := False;
end;

// the clipboard functions are internally used by TClipboard

function TInterfaceBase.ClipboardFormatToMimeType(FormatID: TClipboardFormat
  ): string;
begin
  Result := '';
end;

function TInterfaceBase.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat;  Stream: TStream): boolean;
begin
  Result := false;
end;

// ! List will be created. You must free it yourself with FreeMem(List) !
function TInterfaceBase.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
begin
  Result := true;
  Count := 0;
  List := nil;
end;

function TInterfaceBase.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  Result := false;
end;

function TInterfaceBase.ClipboardRegisterFormat(
  const AMimeType: string): TClipboardFormat;
begin
  Result := 0;
end;


Function TInterfaceBase.CombineRgn(Dest, Src1, Src2 : HRGN;
  fnCombineMode : Longint) : Longint;
begin
  Result := ERROR;
end;

function TInterfaceBase.CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
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

function TInterfaceBase.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result := 0;
end;

function TInterfaceBase.CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP;
begin
  Result := 0;
end;

function TInterfaceBase.CreatePolygonRgn(Points: PPoint; NumPts: Integer; Winding : Boolean): HRGN;
Begin
  Result := 0;
end;

function TInterfaceBase.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
begin
  Result := 0;
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

function TInterfaceBase.DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;
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

  Function GetBreakablePoints(Source : String) : TList;
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

  Function TextExtent(Handle : hDC; Source : String) : TSize;
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

  Function GetStringLength(Handle : hDC; Source : String;
    FromPos, ToPos : Integer) : Integer;
  var
    Tmp : String;
  begin
    Tmp := Copy(Source,FromPos,ToPos - FromPos);
    Result := TextExtent(Handle, Tmp).cX;
    SetLength(Tmp,0);
  end;

  Function GetStringHeight(Handle : hDC; Source : String;
    FromPos, ToPos : Integer) : Integer;
  var
    Tmp : String;
  begin
    Tmp := Copy(Source,FromPos,ToPos - FromPos);
    Result := TextExtent(Handle,Tmp).cY;
    SetLength(Tmp,0);
  end;

  Function BreakString(Source : String) : TStrings;
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

  Function DoBreakString(AStr : String) : TStrings;
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
    CopyRect(theRect, Rect);
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

    If Str > '' then begin
      TextExtent(DC, tmpString);
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
    CopyRect(Rect,theRect);
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

function TInterfaceBase.EnableMenuItem(hndMenu: HMENU; uIDEnableItem: Integer; bEnable: Boolean): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Result := False;
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
  Clip, RRGN : hRGN;
  R : TRect;
begin
  Clip := CreateRectRGN(0,0,0,0);
  If GetClipRGN(DC, Clip) < 0 then begin
    GetClipBox(DC, @R);
    RRGN := CreateRectRgn(R.Left,R.Top,R.Right, R.Bottom);
    Result := SelectClipRGN(DC, RRGN);
    DeleteObject(RRGN);
  end;
  DeleteObject(Clip);
  If Result <> ERROR then begin
    RRGN := CreateRectRgn(Left, Top, Right, Bottom);
    Result := ExtSelectClipRGN(DC, RRGN, RGN_DIFF);
    DeleteObject(RRGN);
  end;
end;

function TInterfaceBase.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;
var
  OldC, Clip : hRGN;
begin
  OldC := CreateRectRGN(0,0,1,1);
  GetClipRGN(DC, OldC);
  Clip := CreateRectRGN(0,0,1,1);
  Result := CombineRGN(Clip, OldC, RGN, Mode);
  If Result <> ERROR then
    Result := SelectClipRGN(DC, Clip);
  DeleteObject(Clip);
  DeleteObject(oldC);
end;

function TInterfaceBase.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.FloodFill(DC: HDC; X, Y: Integer; Color: TColor;
  FillStyle: TFillStyle; Brush: HBRUSH): Boolean;
begin
  Result := false;
end;

function TInterfaceBase.Frame3d(DC: HDC; var Rect: TRect;
  const FrameWidth : integer; const Style : TBevelCut) : boolean;
begin
  Result:= false;
end;

Function TInterfaceBase.GetActiveWindow : HWND;
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

Function TInterfaceBase.GetClientBounds(handle : HWND; var Rect : TRect) : Boolean;
begin
  Result := False;
end;

Function TInterfaceBase.GetClientRect(handle : HWND; var Rect : TRect) : Boolean;
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

Function TInterfaceBase.GetCmdLineParamDescForInterface: string;
begin
  Result := '';
end;

function TInterfaceBase.GetCursorPos(var lpPoint: TPoint): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.GetCharABCWidths(DC: HDC; p2, p3: UINT;
  const ABCStructs): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.GetDC(hWnd: HWND): HDC;
begin
  Result := 0;
end;

function TInterfaceBase.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Result := 0;
end;

function TInterfaceBase.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
begin
  Result := 0;
end;

function TInterfaceBase.CreateDIBSection(DC: HDC; const p2: tagBitmapInfo; p3: UINT;
  var p4: Pointer; p5: THandle; p6: DWORD): HBITMAP;
begin
  Result := 0;
end;

function TInterfaceBase.GetFocus: HWND;
begin
  Result := 0;
end;

function TInterfaceBase.GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result := 0;
end;

function TInterfaceBase.GetObject(GDIObject: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
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

function TInterfaceBase.GetScrollInfo(Handle: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): Boolean;
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

function TInterfaceBase.GetTextColor(DC: HDC) : TColorRef;
begin
  Result := 0;
end;

function TInterfaceBase.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.GetWindowLong(Handle : hwnd; int : Integer): Longint;
begin
  Result := 0;
end;

Function TInterfaceBase.GetWindowOrgEx(dc : hdc; var P : TPoint): Integer;
Begin
  Result := 0;
end;

function TInterfaceBase.GetWindowRect(Handle : hwnd; var Rect : TRect): Integer;
begin
  Result := 0;
end;

Function TInterfaceBase.GetWindowSize(Handle : hwnd;
  var Width, Height: integer): boolean;
begin
  Result:=false;
end;

function TInterfaceBase.HideCaret(hWnd: HWND): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.IntersectClipRect(dc: hdc;
  Left, Top, Right, Bottom: Integer): Integer;
var
  Clip, RRGN : hRGN;
begin
  RRGN := CreateRectRgn(Left, Top, Right, Bottom);
  Clip := CreateRectRgn(0,0,0,0);
  If GetClipRGN(DC, Clip) < 0 then
    Result := ExtSelectClipRGN(DC, RRGN, RGN_COPY)
  else
    Result := ExtSelectClipRGN(DC, RRGN, RGN_AND);
  DeleteObject(RRGN);
  DeleteObject(Clip);
end;

Function TInterfaceBase.InvalidateRect(aHandle : HWND; Rect : pRect;
  bErase : Boolean) : Boolean;
begin
  Result := false;
end;

function TInterfaceBase.KillTimer (hWnd : HWND; uIDEvent : cardinal) : boolean;
begin
  Result := false; 
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
  Rop: DWORD): Boolean; 
begin
  Result := False;
end;

function TInterfaceBase.MoveToEx(DC: HDC; X, Y: Integer; 
  OldPoint: PPoint): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean;
var
  P : TPoint;
Begin
  GetWindowOrgEx(dc, P);
  Result:=SetWindowOrgEx(dc, P.x-dX, P.y-dY, P);
end;

function TInterfaceBase.PeekMessage(var lpMsg : TMsg; Handle : HWND;
  wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
Begin
  Result := False;
End;

function TInterfaceBase.Pie(DC: HDC; 
  x,y,width,height,angle1,angle2 : Integer): Boolean;
var
  Points : PPoint;
  Count : Longint;
begin
  Result := False;
  Points := nil;
  Count := 0;
  PolyBezierArcPoints(X,Y,Width,Height,Angle1, Angle2, 0, Points, Count);
  Inc(Count,2);
  ReallocMem(Points, Count*SizeOf(TPoint));
  Points[Count - 2] := CenterPoint(Rect(X,Y,X+Width,Y+Height));
  Points[Count - 1] := Points[0];
  ReallocMem(Points, Count*SizeOf(TPoint));
  Points[Count - 1] := Points[0];
  Polygon(DC, Points, Count, True);
  ReallocMem(Points, 0);
  Result := True;
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
  wParam: LongInt; lParam: LongInt): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.RadialArc(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
var
  A1, A2 : Extended;
Begin
  Coords2Angles(x,y,width,height,sx,sy,ex,ey,A1,A2);
  Result := Arc(DC, X, Y, Width, Height, Round(A1), Round(A2));
End;

function TInterfaceBase.RadialChord(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
var
  A1, A2 : Extended;
Begin
  Coords2Angles(x,y,width,height,sx,sy,ex,ey,A1,A2);
  Result := AngleChord(DC, X, Y, Width, Height, Round(A1), Round(A2));
End;

function TInterfaceBase.RadialPie(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean;
var
  A1, A2 : Extended;
Begin
  Coords2Angles(x,y,width,height,sx,sy,ex,ey,A1,A2);
  Result := Pie(DC, X, Y, Width, Height, Round(A1), Round(A2));
End;

function TInterfaceBase.RadioMenuItemGroup(hndMenu: HMENU; bRadio: Boolean): Boolean;
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

function TInterfaceBase.RectVisible(dc : hdc; ARect: TRect) : Boolean;
begin
  Result := true;
end;

function TInterfaceBase.RegroupMenuItem(hndMenu: HMENU; GroupIndex: Integer) : Boolean;
begin
  Result := false;
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

function TInterfaceBase.RightJustifyMenuItem(HndMenu: HMenu;
  bRightJustify: boolean): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.SaveDC(DC: HDC) : Integer;
begin
  Result := 0;  
end;

Function TInterfaceBase.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
begin
  // Your default here
  Result := 0;
end;

function TInterfaceBase.ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean;
begin
  Result := False;
end;

Function TInterfaceBase.SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
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

function TInterfaceBase.SendMessage(HandleWnd: HWND; Msg: Cardinal; wParam: LongInt; lParam: LongInt): Integer;
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
  // Your default here
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

function TInterfaceBase.SetCaretRespondToFocus(Handle: HWnd;
  ShowHideOnFocus: Boolean): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.SetFocus(hWnd: HWND): HWND;
begin
  Result := 0;
end;

Function TInterfacebase.SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean;
Begin
  Result := True;
end;

function TInterfaceBase.SetSCrollInfo(Handle : HWND; SBStyle : Integer; ScrollInfo: TScrollInfo; Redraw : Boolean): Integer;
begin
  Result := 0;
end;

function TInterfaceBase.SetSysColors(cElements: Integer; const lpaElements;
  const lpaRgbValues): Boolean;
begin
  Result := False;
end;

Function TInterfaceBase.SetTextCharacterExtra(_hdc : hdc;
  nCharExtra : Integer):Integer;
begin
  // Your default here
  Result := 0;
end;

function TInterfaceBase.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  Result := CLR_INVALID;
end;

function TInterfaceBase.SetTimer(hWnd: HWND; nIDEvent, uElapse: integer;
  lpTimerFunc: TFNTimerProc) : integer;
begin
  Result := 0; 
end;

function TInterfacebase.SetWindowLong(Handle: HWND;
  Idx: Integer; NewLong : Longint): LongInt;
begin
  Result := -1;
end;

Function TInterfaceBase.SetWindowOrgEx(dc : hdc; NewX, NewY : Integer;
  var OldPoint: TPoint) : Boolean;
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

function TInterfaceBase.ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
begin
  Result := False;
end;

function TInterfaceBase.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; 
begin
  Result := False;
end;

Function TInterfaceBase.TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  // Your default here
  Result := false;
end;

Function TInterfaceBase.WindowFromPoint(Point : TPoint) : HWND;
begin
  // Your default here
  Result := 0;
end;

Procedure TInterfaceBase.InitializeCriticalSection(var CritSection: TCriticalSection);
begin
end;

Procedure TInterfaceBase.EnterCriticalSection(var CritSection: TCriticalSection);
begin
end;

Procedure TInterfaceBase.LeaveCriticalSection(var CritSection: TCriticalSection);
begin
end;

Procedure TInterfaceBase.DeleteCriticalSection(var CritSection: TCriticalSection);
begin
end;

//##apiwiz##eps##   // Do not remove
{ =============================================================================

  $Log$
  Revision 1.46  2002/09/10 06:49:19  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.45  2002/09/09 14:01:05  lazarus
  MG: improved TScreen and ShowModal

  Revision 1.44  2002/08/30 12:32:20  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.43  2002/08/28 09:40:49  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.42  2002/08/25 13:31:35  lazarus
  MG: replaced C-style operators

  Revision 1.41  2002/08/21 10:46:37  lazarus
  MG: fixed unreleased gdiRegions

  Revision 1.40  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.39  2002/08/15 15:46:48  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.38  2002/08/13 07:08:24  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.37  2002/08/08 18:05:46  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.36  2002/08/08 17:26:37  lazarus
  MG: added property TMenuItems.RightJustify

  Revision 1.35  2002/08/08 09:07:06  lazarus
  MG: TMenuItem can now be created/destroyed/moved at any time

  Revision 1.34  2002/08/07 09:55:30  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.33  2002/08/05 10:45:02  lazarus
  MG: TMenuItem.Caption can now be set after creation

  Revision 1.32  2002/06/21 15:41:56  lazarus
  MG: moved RectVisible, ExcludeClipRect and IntersectClipRect to interface dependent functions

  Revision 1.31  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.30  2002/05/27 17:58:41  lazarus
  MG: added command line help

  Revision 1.29  2002/05/24 07:16:31  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.28  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.27  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.26  2002/03/08 16:16:55  lazarus
  MG: fixed parser of end blocks in initialization section added label sections

  Revision 1.25  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.24  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.23  2001/12/28 11:41:51  lazarus
  MG: added TCanvas.Ellipse, TCanvas.Pie

  Revision 1.22  2001/12/27 16:31:28  lazarus
  MG: implemented TCanvas.Arc

  Revision 1.21  2001/12/12 14:23:17  lazarus
  MG: implemented DestroyCaret

  Revision 1.20  2001/11/14 19:10:03  lazarus
  MG: fixes for parser and linkscanner and small cleanups

  Revision 1.19  2001/11/14 17:46:58  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.18  2001/11/12 16:56:07  lazarus
  MG: CLIPBOARD

  Revision 1.17  2001/10/10 17:55:04  lazarus
  MG: fixed caret lost, gtk cleanup, bracket lvls, bookmark saving

  Revision 1.16  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.15  2001/06/14 23:13:30  lazarus
  MWE:
    * Fixed some syntax errors for the latest 1.0.5 compiler

  Revision 1.14  2001/04/06 22:25:14  lazarus
  * TTimer uses winapi-interface now instead of sendmessage-interface, stoppok

  Revision 1.13  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.12  2001/03/12 12:17:01  lazarus
  MG: fixed random function results

  Revision 1.11  2001/02/16 19:13:30  lazarus
  Added some functions
  Shane

  Revision 1.10  2001/01/23 19:13:57  lazarus
  Fixxed the errors I commited with Unionrect
  Shane

  Revision 1.9  2001/01/23 19:01:10  lazarus
  Fixxed bug in RestoreDC
  Shane

  Revision 1.8  2001/01/23 18:42:10  lazarus
  Added InvalidateRect to gtkwinapi.inc
  Shane

  Revision 1.7  2000/12/06 14:54:38  lazarus
  Set some defaults in interfacebase.inc
  Shane

  Revision 1.6  2000/09/10 19:58:47  lazarus
  MWE:
    * Updated makefiles for FPC release 1.0  binary units
    * Changed creation, now LCL unit distributions are possible
    * Moved interfaces.pp from LCL to interface dirs

  Revision 1.5  2000/08/14 12:31:12  lazarus
  Minor modifications for SynEdit .
  Shane

  Revision 1.4  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.3  2000/08/10 18:56:24  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:26  michael
  + Initial import

  Revision 1.6  2000/05/11 22:04:15  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.5  2000/05/08 12:54:19  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane

  Revision 1.4  2000/05/03 00:27:05  lazarus
  MWE:
    + First rollout of the API wizzard.

  Revision 1.3  2000/04/10 14:03:07  lazarus
  Added SetProp and GetProp winapi calls.
  Added ONChange to the TEdit's published property list.
  Shane

  Revision 1.2  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.30  2000/03/31 18:41:03  lazarus
  Implemented MessageBox / Application.MessageBox calls. No icons yet, though...

  Revision 1.29  2000/03/30 18:07:54  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.28  2000/03/28 22:47:50  lazarus
  MWE:
    Started with the blt function family

  Revision 1.27  2000/03/19 23:01:43  lazarus
  MWE:
    = Changed splashscreen loading/colordepth
    = Chenged Save/RestoreDC to platform  dependent, since they are
      relative to a DC

  Revision 1.26  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.25  2000/03/14 19:49:05  lazarus
  Modified the painting process for TWincontrol.  Now it runs throug it's FCONTROLS list and paints all them
  Shane

  Revision 1.24  2000/03/10 18:31:10  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.23  2000/03/09 23:48:02  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.22  2000/03/08 23:57:39  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.21  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.20  2000/03/03 22:58:26  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.19  2000/02/22 23:26:13  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.18  2000/01/31 20:00:22  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.17  2000/01/25 00:38:25  lazarus
  MWE:
    Added GetFocus

  Revision 1.16  2000/01/16 23:23:07  lazarus
  MWE:
    Added/completed scrollbar API funcs

  Revision 1.15  1999/12/21 21:35:54  lazarus
  committed the latest toolbar code.  Currently it doesn't appear anywhere and I have to get it to add buttons correctly through (I think) setstyle.  I think I'll implement the LM_TOOLBARINSERTBUTTON call there.
  Shane

  Revision 1.14  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.13  1999/12/20 21:01:14  lazarus
  Added a few things for compatability with Delphi and TToolbar
  Shane

  Revision 1.12  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.11  1999/12/14 21:07:12  lazarus
  Added more stuff for TToolbar
  Shane

  Revision 1.10  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.9  1999/11/29 00:46:47  lazarus
  MWE:
    Added TBrush as gdiobject
    commented out some more mwedit MWE_FPC ifdefs

  Revision 1.8  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.7  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.6  1999/11/18 00:13:08  lazarus
  MWE:
    Partly Implemented SelectObject
    Added  ExTextOut
    Added  GetTextExtentPoint
    Added  TCanvas.TextExtent/TextWidth/TextHeight
    Added  TSize and HPEN

  Revision 1.5  1999/11/17 01:16:40  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.4  1999/11/16 01:32:22  lazarus
  MWE:
    Added some more DC functionality

}


