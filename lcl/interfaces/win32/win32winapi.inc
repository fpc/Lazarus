{******************************************************************************
                         All Windows API implementations.
                   Initial Revision : Sat Nov 13 12:53:53 1999


  !! Keep alphabetical !!

  Support routines go to win32proc.pp

 ******************************************************************************
 Implementation
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

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}
  
Const
  BOOL_TEXT: Array[Boolean] Of String = ('False', 'True');  

//##apiwiz##sps##   // Do not remove

{------------------------------------------------------------------------------
  Method:  Arc
  Params:  DC - handle to device context
           X - x-coordinate of bounding rectangle's upper-left corner
           Y - y-coordinate of bounding rectangle's upper-left corner
           Width - Width of the rectangle
           Height - height of the rectangle
           Angle1 - first angle
           Angle2 - second angle
  Returns: Whether the call was successful

  Use Arc to draw an elliptically curved line with the current Pen.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position. 
 ------------------------------------------------------------------------------}
Function TWin32Object.Arc(DC: HDC; X, Y, Width, Height, Angle1, Angle2: Integer): Boolean;
var
  SX, SY, EX, EY : Longint;
Begin
  Angles2Coords(X,Y,Width,Height,Angle1,Angle2, SX, SY, EX, EY);
  Result := Windows.Arc(DC, X, Y, X+Width, Y+Height, SX,SY,EX,EY);
End;

{------------------------------------------------------------------------------
  Method:   AngleChord
  Params:   DC,x,y,width,height,angle1,angle2
  Returns:  Nothing

  Use AngleChord to draw a filled Chord-shape on the canvas. The angles angle1
  and angle2 are 1/16th of a degree. For example, a full circle equals 5760
  16*360). Positive values of Angle and AngleLength mean counter-clockwise while
  negative values mean clockwise direction. Zero degrees is at the 3'o clock
  position.

------------------------------------------------------------------------------}
Function TWin32Object.AngleChord(DC: HDC; X, Y, Width, Height, Angle1,
  Angle2: Integer): Boolean;
var
  SX, SY, EX, EY : Longint;
Begin
  Angles2Coords(X,Y,Width,Height,Angle1,Angle2, SX, SY, EX, EY);
  Result := Windows.Chord(DC, X, Y, X+Width, Y+Height, SX,SY,EX,EY);
End;

{------------------------------------------------------------------------------
  Method:  BitBlt
  Params:  DestDC        - The destination device context
           X, Y          - The left/top corner of the destination rectangle
           Width, Height - The size of the destination rectangle
           SrcDC         - The source devicecontext 
           XSrc, YSrc    - The left/top corner of the source rectangle   
           Rop           - The raster operation to be performed 
  Returns: True if succesful

  The BitBlt function copies a bitmap from a source context into a destination 
  context using the specified raster operation.
 ------------------------------------------------------------------------------}
Function TWin32Object.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean; 
Begin
  Result := Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
End;

{------------------------------------------------------------------------------
  Method:  BringWindowToTop
  Params:  HWnd - The handle of the window
  Returns: True if succesful

  The BringWindowToTop function brings the specified window to top (highest z-
  index level).
 ------------------------------------------------------------------------------}
Function TWin32Object.BringWindowToTop(HWnd: HWND): Boolean;
Begin
  Assert(False, 'TWin32Object.BringWindowToTop - Start');
  Result := Windows.BringWindowToTop(HWnd);
  Assert(False, 'TWin32Object.BringWindowToTop - Exit');
End;

{------------------------------------------------------------------------------
  Method:  CallNextHookEx
  Params:  HHk    - handle of the current hook
           NCode  - Hook code
           WParam - Word parameter
           LParam - Long-integer parameter
  Returns: The handle of the next hook procedure

  Calls the next procedure in the hook chain
 ------------------------------------------------------------------------------}
Function TWin32Object.CallNextHookEx(HHk: HHOOK; NCode: Integer; WParam, LParam: LongInt): Integer;
Begin
  Result := Windows.CallNextHookEx(hhk, ncode, wParam, lParam);
End;

{------------------------------------------------------------------------------
  Method:  CallWindowProc
  Params: LPPrevWndFunc - Address of specified window procedure
          Handle        - Handle of window receiving messages
          Msg           - The message sent
          WParam        - Word parameter
          LParam        - Long-integer parameter
  Returns: Message result

  Passes message information to the specified window procedure
 ------------------------------------------------------------------------------}
Function TWin32Object.CallWindowProc(LPPrevWndFunc: TFarProc; Handle: HWND; Msg: UINT; WParam, LParam: LongInt): Integer;
Begin
  Result := Windows.CallWindowProc(WNDPROC(LPPrevWndFunc), Handle, Msg, WParam, LParam);
End;

{------------------------------------------------------------------------------
  Method:  ClientToScreen
  Params:  Handle - Handle of window
           P      - container that contains coordinates
  Returns: Whether the call was successful

  Converts client coordinates to screen coordinates
 ------------------------------------------------------------------------------}
Function TWin32Object.ClientToScreen(Handle: HWND; Var P: TPoint): Boolean;
Begin
  Result := Windows.ClientToScreen(Handle, @P);
End;

{------------------------------------------------------------------------------
  Method:  ClipboardFormatToMimeType
  Params:  FormatID - a registered format identifier (0 is invalid)
  Returns: the corresponding mime type as string
 ------------------------------------------------------------------------------}
Function TWin32Object.ClipboardFormatToMimeType(FormatID: TClipboardFormat): String;
Begin
  Assert(False, 'Trace:TWin32Object.ClipboardFormatToMimeType - Start');
  Windows.GetClipboardFormatName(FormatID, @Result, MAX_PATH);
  Assert(False, 'Trace:TWin32Object.ClipboardFormatToMimeType - Exit');
End;

{------------------------------------------------------------------------------
  Method:  ClipboardGetData
  Params:  ClipboardType - clipboard type
           FormatID      - a registered format identifier (0 is invalid)
           Stream        - If format is available, it will be appended to this
                           stream
  Returns: true on success
 ------------------------------------------------------------------------------}
Function TWin32Object.ClipboardGetData(ClipboardType: TClipboardType; FormatID: TClipboardFormat; Stream: TStream): Boolean;
Begin
  Assert(False, 'TWin32Object.ClipboardGetData - Start');
  Stream := TStream(Windows.GetClipBoardData(FormatID));
  Result := HANDLE(Stream) <> 0;
  Assert(False, 'TWin32Object.ClipboardGetData - Exit');
End;

{------------------------------------------------------------------------------
  Method:  ClipboardGetFormats
  Params:  ClipboardType - the type of clipboard operation (GTK; ignored here)
           Count         - the number of clipboard formats
           List          - Pointer to an array of supported formats
                           (you must free it yourself)
  Returns: true on success
 ------------------------------------------------------------------------------}
Function TWin32Object.ClipboardGetFormats(ClipboardType: TClipboardType; Var Count: Integer; Var List: PClipboardFormat): Boolean;
Type
  PCBList = ^TCBList;
  TCBList = Array[0..1] Of TClipboardFormat;
Var
  C, LastCount: Cardinal;
  CBList: PCBList;
Begin
  Result := True;
  Count := CountClipboardFormats;
  C := 0;
  LastCount := 0;
  GetMem(CBList, Count * SizeOf(TClipboardFormat));
  While True Do
  Begin
    LastCount := EnumClipboardFormats(LastCount);
    If LastCount = 0 Then
      Break;
    CBList^[C] := LastCount;
    Inc(C);
  End;
  List := PClipboardFormat(CBList);
End;

{------------------------------------------------------------------------------
  Method:  ClipboardGetOwnerShip
  Params:  ClipboardType - Type of clipboard (ignored)
           OnRequestProc - TClipboardRequestEvent is defined in LCLLinux.pp
                           If OnRequestProc is nil the onwership will end.
           FormatCount - number of formats
           Formats - array of TClipboardFormat. The supported formats the owner
                      provides.
                      
  Returns: true on success
  
  Sets the supported formats and requests ownership for the clipboard.
  Each time the clipboard is read the OnRequestProc will be executed.
  If someone else requests the ownership, the OnRequestProc will be executed
  with the invalid FormatID 0 to notify the old owner of the lost of ownership.
 ------------------------------------------------------------------------------}
Function TWin32Object.ClipboardGetOwnerShip(ClipboardType: TClipboardType; OnRequestProc: TClipboardRequestEvent; FormatCount: Integer; Formats: PClipboardFormat): Boolean;
Var
  I: Integer;
  P: PChar;
Begin
  Result := True;
  If GetClipboardOwner <> HWND(Nil) Then
    OnRequestProc(0, Nil);
  GetMem(Formats, FormatCount * SizeOf(TClipboardFormat));
  Try
    For I := 0 To FormatCount Do
    Begin
      GetClipboardFormatName(Formats[I], @P, MAX_PATH);
      RegisterClipboardFormat(@P);
    End;
  Except
    Result := False;
  End;
  FreeMem(Formats);
End;

{------------------------------------------------------------------------------
  Method:  ClipboardRegisterFormat
  Params:  AMimeType - a string (usually a MIME type) identifying a new format
                       type to register
  Returns: the registered Format identifier (TClipboardFormat)
 ------------------------------------------------------------------------------}
Function TWin32Object.ClipboardRegisterFormat(Const AMimeType: String): TClipboardFormat;
Begin
  Result := Windows.RegisterClipboardFormat(PChar(AMimeType));
End;

{------------------------------------------------------------------------------
  Function: CombineRgn
  Params:  Dest, Src1, Src2, fnCombineMode
  Returns: longint

  Combine the 2 Source Regions into the Destination Region using the specified
  Combine Mode. The Destination must already be initialized. The Return value 
  is the Destination's Region type, or ERROR.
  
  The Combine Mode can be one of the following:
      RGN_AND  : Gets a region of all points which are in both source regions

      RGN_COPY : Gets an exact copy of the first source region

      RGN_DIFF : Gets a region of all points which are in the first source 
                 region but not in the second.(Source1 - Source2)

      RGN_OR   : Gets a region of all points which are in either the first 
                 source region or in the second.(Source1 + Source2)

      RGN_XOR  : Gets all points which are in either the first Source Region 
                 or in the second, but not in both.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
Function TWin32Object.CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint;
begin
  Result := Windows.CombineRgn(Dest, Src1, Src2, fnCombineMode);
end;

{------------------------------------------------------------------------------
  Method:  CreateBitmap
  Params:  Width      - bitmap width, in pixels
           Height     - bitmap height, in pixels
           Planes     - number of color planes
           BitCount   - number of bits required to identify a color
           BitmapBits - pointer to array containing color data
  Returns: A handle to a bitmap

  The CreateBitmap function creates a bitmap with the specified width, height,
  and color format (color planes and bits per pixel).
 ------------------------------------------------------------------------------}
Function TWin32Object.CreateBitmap(Width, Height: Integer; Planes, BitCount: LongInt; BitmapBits: Pointer): HBITMAP;
Begin
  Assert(False, Format('Trace:> [TWin32Object.CreateBitmap] Width: %d, Height: %d, Planes: %d, BitCount: %d, BitmapBits: 0x%x', [Width, Height, Planes, BitCount, Longint(BitmapBits)]));
  Result := Windows.CreateBitmap(Width, Height, Planes, BitCount, BitmapBits);
  Assert(False, Format('Trace:< [TWin32Object.CreateBitmap] --> 0x%x', [Integer(Result)]));
End;

{------------------------------------------------------------------------------
  Method:   CreateBrushIndirect
  Params:  LogBrush - record describing brush
  Returns: identifier of a logical brush

  The CreateBrushIndirect function creates a logical brush that has the
  specified style, color, and pattern.
 ------------------------------------------------------------------------------}
Function TWin32Object.CreateBrushIndirect(Const LogBrush: TLogBrush): HBRUSH;
Var
  LB: TLogBrush;
Begin
  LB := LogBrush;
  LB.lbColor := ColorToRGB(LB.lbColor);
  Assert(False, Format('Trace:> [TWin32Object.CreateBrushIndirect]  Style: %d, Color: %8x', [lb.lbStyle, lb.lbColor]));
  Result := Windows.CreateBrushIndirect(Windows.LOGBRUSH(LB));
  Assert(False, Format('Trace:< [TWin32Object.CreateBrushIndirect] Got --> %x', [Result]));
End;

{------------------------------------------------------------------------------
  Method:  CreateCaret
  Params:  Handle - handle to owner window
           Bitmap - handle to bitmap for caret shape
           Width  - caret width
           Height - caret height
  Returns: Whether the function succeeded

  Creates a new shape for the system caret and assigns ownership of the caret
  to the specified window
 ------------------------------------------------------------------------------}
Function TWin32Object.CreateCaret(Handle: HWND; Bitmap: HBITMAP; Width, Height: Integer): Boolean;
Begin
  Result := CreateCaret(Handle, Bitmap, Width, Height);
  Assert(False, 'Trace:TODO: [TWin32Object.CreateCaret] Finish');
End;

{------------------------------------------------------------------------------
  Method:  CreateCompatibleBitmap
  Params: DC     - handle to device context
          Width  - width of bitmap, in pixels
          Height - height of bitmap, in pixels
  Returns: a handle to the bitmap

  Creates a bitmap compatible with the specified device context.
 ------------------------------------------------------------------------------}
Function TWin32Object.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
Begin
  Assert(False, Format('Trace:> [TWin32Object.CreateCompatibleBitmap] DC: 0x%x, W: %d, H: %d', [DC, Width, Height]));
  Result := Windows.CreateCompatibleBitmap(DC, Width, Height);
  Assert(False, Format('Trace:< [TWin32Object.CreateCompatibleBitmap] DC: 0x%x --> 0x%x', [DC, Result]));
End;

{------------------------------------------------------------------------------
  Method:  CreateCompatibleDC
  Params:  DC - handle to memory device context
  Returns: handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device.
 ------------------------------------------------------------------------------}
Function TWin32Object.CreateCompatibleDC(DC: HDC): HDC;
Begin
  Result := Windows.CreateCompatibleDC(DC);
  Assert(False, Format('Trace:[TWin32Object.CreateCompatibleDC] DC: 0x%x --> 0x%x', [Integer(DC), Integer(Result)]));
End;

{------------------------------------------------------------------------------
  Method:  CreateFontIndirect
  Params:  LogFont - logical font record
  Returns: a handle to a logical font

  Creates a logical font that has the characteristics specified in the
  specified record.
 ------------------------------------------------------------------------------}
Function TWin32Object.CreateFontIndirect(Const LogFont: TLogFont): HFONT;
Begin
  Result := Windows.CreateFontIndirect(@LogFont);
End;

{------------------------------------------------------------------------------
  Method:  CreatePenIndirect
  Params:  LogPen - record that defines the style, width, and color of a pen
  Returns: a handle that identifies a logical cosmetic pen

  Creates a logical cosmetic pen that has the style, width, and color specified
  in a record.
 ------------------------------------------------------------------------------}
Function TWin32Object.CreatePenIndirect(Const LogPen: TLogPen): HPEN;
Var
  LP: TLogPen;
Begin
  LP := LogPen;
  Lp.lopnColor := ColorToRGB(Lp.lopnColor);
  Assert(False, 'Trace:[TWin32Object.CreatePenIndirect]');
  Result := Windows.CreatePenIndirect(Windows.LOGPEN(LP));
End;

{------------------------------------------------------------------------------
  Method:  CreatePixmapIndirect
  Params:  Data       - Raw pixmap data
           TransColor - Color of transparent spots
  Returns: Handle to LCL bitmap

  Creates a bitmap from raw pixmap data.
 ------------------------------------------------------------------------------}
Function TWin32Object.CreatePixmapIndirect(Const Data: Pointer; Const TransColor: LongInt): HBITMAP;
Type
  PColorMap = ^TColorMap;
  TColorMap = Record
    Alias: String;
    Color: DWORD;
  End;
  PPixmapArray = ^TPixmapArray;
  TPixmapArray = Array[0..1000] Of PChar;
Var
  AliasLen      : Cardinal;
  AList         : TList;
  ColorCount    : Integer;
  hdcScreen     : HDC;
  hdcBitmap     : HDC;
  hbmBitmap     : HBITMAP ;
  Height, Width : Integer;
  OldObject     : HGDIOBJ;
  PixmapArray   : PPixmapArray;
  Info          : String;
  PixmapInfo    : TStringList;

  Procedure NormalizeString(Var Str: String);
  Var
    S: String;
  Const
    keyTab  = #9;
  Begin
    Assert(False, 'Trace:NormalizeString - Start');
    Str := Replace(Str, keyTab, ' ', True);
    S := '';
    While True Do Begin
      Str := Replace(Str, '  ', ' ', True);
      If Str = S Then Break;
      S := Str;
    End;
    Assert(False, 'Trace:NormalizeString - Exit');
  End;

  Function StrToInt(Const Str: String): DWORD;
  Var
    S: String;
  Begin
    Assert(False, 'Trace:StrToInt - Start');
    S := Trim(Str);
    Result := SysUtils.StrToInt(S);
    Assert(False, 'Trace:StrToInt - Exit');
  End;

  procedure CreateColorMap;
  Var
    Elem: String;
    I: Integer;
    Idx: Cardinal;
    ColorMap: PColorMap;
  Begin
    Assert(False, 'Trace:CreateColorMap - Start');
    If ColorCount = 0 Then Begin
      Assert(False, 'Trace:CreateColorMap - Color count was not retrieved; can''t create color map');
      AList := Nil;
      Exit;
    End;

    AList := TList.Create;
    For I := 1 To ColorCount Do Begin
      Try
        Elem := String(PixmapArray^[I]);

        While Pos(Elem[Length(Elem)],'",')>0 do Elem:=Copy(Elem,1,Length(Elem)-1);

        Idx := Length(Elem)-5;
        New(ColorMap);
        ColorMap^.Alias := Copy(Elem, 1, AliasLen);
        If Copy(Elem, Length(Elem)-3, 4) <> 'None' Then begin
          //ColorMap^.Color :=StrToInt('$'+Copy(Elem,Idx,6));
          ColorMap^.Color :=RGB(StrToInt('$'+Copy(Elem,Idx,2)),StrToInt('$'+Copy(Elem,Idx+2,2)),StrToInt('$'+Copy(Elem,Idx+4,2)));
        end
        Else
          ColorMap^.Color := TransColor;
        Assert(False, Format('Trace:CreateColorMap - color-map entry info --> item: %D, data: %S, alias: %S, color:0x%X', [I, String(PixmapArray^[I]), ColorMap^.Alias, ColorMap^.Color]));
        AList.Add(ColorMap);
      Except
        On E: Exception Do Assert(False, Format('Trace:CreateColorMap - Could not create color-map entry --> %S', [E.Message]));
      End;
    End;
    Assert(False, 'Trace:CreateColorMap - Exit');
  End;

  Procedure DestroyColorMap;
  var
    ColorMap : PColorMap;
  Begin
    Assert(False, 'Trace:DestroyColorMap - Start');
    While AList.Count>0 do begin
      ColorMap:=PColorMap(AList.Items[0]);
      Dispose(ColorMap);
      AList.Delete(0);
    end;
    If AList <> Nil Then Begin
      AList.Free;
      AList := Nil;
    End;
    Assert(False, 'Trace:DestroyColorMap - Exit');
  End;

  function GetColorFromAlias(Alias:String):DWORD;
  var
     i : Integer;
  begin
    result:=0;
    i     :=0;
    if AList.Count>0 then begin
      repeat
        if (TColorMap(AList.Items[i]^).Alias=Alias) then begin
          result:=TColorMap(AList.Items[i]^).Color;
          break;
        end;
        Inc(i);
      until (i>=ColorCount);
    end;
  end;

  Procedure DoDrawBitmap;
  Var
    CX,CY      : Cardinal;
    Line,Alias : String;
  Begin
    Assert(False, 'Trace:DoDrawBitmap - Start');

    If (ColorCount = 0) Or (AList = Nil) Then
    Begin
      Assert(False, 'Trace:DoDrawBitmap - No information to create bitmap');
      Exit;
    End;

    for CY:=0 to Height-1 do begin
      Line:=String(PixmapArray^[1+ColorCount+CY]);
      While Pos(Line[Length(Line)],'",')>0 do Line:=Copy(Line,1,Length(Line)-1);
      for CX:=0 to Width-1 do begin
        Alias:=Copy(Line,1+CX*AliasLen,AliasLen);
        Windows.SetPixel(hdcBitmap,CX,CY,GetColorFromAlias(Alias));
      end;
    end;
    Assert(False, 'Trace:DoDrawBitmap - Exit');
  End;

Begin
  Assert(False, 'Trace:TWin32Object.   - Start');

  Height      := 0;
  Width       := 0;
  ColorCount  := 0;
  AliasLen    := 0;
  Result      := HBITMAP(Nil);
  PixmapArray := PPixmapArray(Data);
  Info        := String(PixmapArray^[0]);
  PixmapInfo  := TStringList.Create;;

  NormalizeString(Info);
  //My own Split:
  while Pos(' ',Info)>0 do begin
    PixmapInfo.Add(Copy(Info,1,Pos(' ',Info)-1)); //Add first String to list
    Delete(Info,1,Pos(' ',Info));                 //Delete String + Space
  end;
  if Length(Info)>0 then PixmapInfo.Add(Info);     //Add last String;

  // I don't know where this Split is defines, but it does something weired
  //  PixmapInfo := Split(String(PixmapArray^[0]), ' ', 3, False);
  If PixmapInfo.Count = 6 Then Assert(False, 'Trace:TODO: TWin32Object.CreatePixmapIndirect - Get Pixmaps with six sections working');

  Try
    Width      := StrToInt(PixmapInfo[0]);             Assert(False, Format('Trace: Pixmap width --> %D', [Width]));
    Height     := StrToInt(PixmapInfo[1]);             Assert(False, Format('Trace: Pixmap height --> %D', [Height]));
    ColorCount := StrToInt(PixmapInfo[2]);             Assert(False, Format('Trace: number of colors --> %D', [ColorCount]));

    While Pos(PixmapInfo[3][Length(PixmapInfo[3])],'",')>0 do
      PixmapInfo[3]:=Copy(PixmapInfo[3],1,Length(PixMapInfo[3])-1);

    AliasLen := StrToInt(PixmapInfo[3]);

    Assert(False, Format('Trace: characters per pixel --> %D', [AliasLen]));
    Assert(False, Format('Trace:TWin32Object.CreatePixmapIndirect - Pixmap info: Width - %D; Height - %D; Number of Colors - %D; Characters per pixel - %D; Transparent color - 0x%X', [Width, Height, ColorCount, AliasLen, TransColor]));
  Except
    On E: Exception Do
    Begin
      Assert(False, 'Trace:Error: TWin32Object.CreatePixmapIndirect - could not retrieve pixmap info --> ' + E.Message);
    End;
  End;

  If (Width <> 0) And (Height <> 0) Then Begin
    hdcScreen := GetDC(GetDesktopWindow);
    hdcBitmap := CreateCompatibleDC(hdcScreen);
    hbmBitmap := CreateCompatibleBitmap(hdcScreen, Width, Height);
    OldObject := SelectObject(hdcBitmap, hbmBitmap);
    CreateColorMap;
    DoDrawBitmap;
    DestroyColorMap;
    ReleaseDC(GetDesktopWindow, hdcScreen);
    DeleteDC(hdcBitmap);
    Result:=hbmBitmap;
  end;
  PixmapInfo.Free;
  PixmapInfo  := Nil;
  PixmapArray := Nil;

  Assert(False, 'Trace:TWin32Object.CreatePixmapIndirect - Exit');
End;

{------------------------------------------------------------------------------
  Method:  CreatePolygonRgn
  Params:  Points, NumPts, Winding
  Returns: the handle to the region

  Creates a Polygon, a closed many-sided shaped region. The Points parameter is
  an array of points that give the vertices of the polygon. Winding determines
  what points are going to be included in the region. When Winding is True,
  points are selected by using the Winding fill algorithm. When Winding is
  False, points are selected by using using the even-odd (alternative) fill
  algorithm. NumPts indicates the number of points to use.
  The first point is always connected to the last point.
 ------------------------------------------------------------------------------}
Function TWin32Object.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  Winding : Boolean): HRGN;
var
  fm : Longint;
Begin
  If Winding then
    fm := Windows.Winding
  else
    fm := Windows.Alternate;
  Result := Windows.CreatePolygonRgn(LPPOINT(Points)^, NumPts, fm);
End;

{------------------------------------------------------------------------------
  Method:  CreateRectRgn
  Params:  X1 - x-coordinate of region's upper-left corner
           Y1 - y-coordinate of region's upper-left corner
           X2 - x-coordinate of region's lower-right corner
           Y2 - y-coordinate of region's lower-right corner
  Returns: the handle to the region

  Creates a rectangular region.
 ------------------------------------------------------------------------------}
Function TWin32Object.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
Begin
  Result := Windows.CreateRectRgn(X1, Y1, X2, Y2);
End;

{------------------------------------------------------------------------------
  Method:  DeleteDC
  Params:  HDC - handle to device context
  Returns: If the function succeeds.

  Deletes the specified device context (DC).
 ------------------------------------------------------------------------------}
Function TWin32Object.DeleteDC(HDC: HDC): Boolean;
Begin
  Result := Windows.DeleteDC(HDC);
End;

{------------------------------------------------------------------------------
  Method:  DeleteObject
  Params:  GDIObject - handle to graphic object
  Returns: If the function succeeds.

  Deletes a graphic object, freeing all system resources associated with the
  object.
 ------------------------------------------------------------------------------}
Function TWin32Object.DeleteObject(GDIObject: HGDIOBJ): Boolean;
Begin
  { Find out if we want to release internal GDI object }
  Result := Windows.DeleteObject(GDIObject);
End;

{------------------------------------------------------------------------------
  Method:  DestroyCaret
  Params:  Handle - handle to the window with a caret (on Windows, there is
           only one, global caret, so this parameter is ignored)
  Returns: If the function succeeds

  Destroys the caret but doesn't free the bitmap.
 ------------------------------------------------------------------------------}
Function TWin32Object.DestroyCaret(Handle: HWND): Boolean;
Begin
  Assert(False, 'Trace:TODO: [TWin32Object.DestroyCaret]');
  Result := Windows.DestroyCaret;
End;

{------------------------------------------------------------------------------
  Method:  DrawFrameControl
  Params: DC     - handle to device context
          Rect   - bounding rectangle
          UType  - frame-control type
          UState - frame-control state
  Returns: If the function succeeds

  Draws a frame control of the specified type and style.
 ------------------------------------------------------------------------------}
Function TWin32Object.DrawFrameControl(DC: HDC; Var Rect: TRect; UType, UState: Cardinal): Boolean;
Begin
  Result := Windows.DrawFrameControl(DC, @Rect, UType, UState);
End;

{------------------------------------------------------------------------------
  Method:  DrawEdge
  Params: DC       - handle to device context
          Rect     - rectangle coordinates
          Edge     - type of inner and outer edge to draw
          GrfFlags - type of border
  Returns: If the function succeeds.

  Draws one or more edges of a rectangle, not including the 
  right and bottom edge.
 ------------------------------------------------------------------------------}
Function TWin32Object.DrawEdge(DC: HDC; Var Rect: TRect; Edge: Cardinal; GrfFlags: Cardinal): Boolean;
Begin
  Assert(False, Format('trace:> [TWin32Object.DrawEdge] DC:0x%x, Rect = %d,%d,%d,%d', [DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom]));
  Result := Windows.DrawEdge(DC, @Rect, edge, grfFlags);
  Assert(False, Format('trace:< [TWin32Object.DrawEdge] DC:0x%x, Rect = %d,%d,%d,%d', [DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom]));
End;

{------------------------------------------------------------------------------
  Method:  DrawText
  Params:  DC, Str, Count, Rect, Flags
  Returns: If the string was drawn, or CalcRect run

 ------------------------------------------------------------------------------}
function TWin32Object.DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;
begin
  Assert(False, Format('trace:> [TWin32Object.DrawText] DC:0x%x, Str:''%s'', Count: %d, Rect = %d,%d,%d,%d, Flags:%d',
    [DC, Str, Count, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Flags]));
  Result := Windows.DrawText(DC, Str, Count, @Rect, Flags);
  Assert(False, Format('trace:> [TWin32Object.DrawText] DC:0x%x, Str:''%s'', Count: %d, Rect = %d,%d,%d,%d, Flags:%d',
    [DC, Str, Count, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Flags]));
end;

{------------------------------------------------------------------------------
  Method:   Ellipse
  Params:   
            DC - handle to device context
            X1 - x-coord. of bounding rectangle's upper-left corner
            Y1 - y-coord. of bounding rectangle's upper-left corner
            X2 - x-coord. of bounding rectangle's lower-right corner
            Y2 - y-coord. of bounding rectangle's lower-right corner
  Returns:  If the function succeeds
  
  Use Ellipse to draw a filled circle or ellipse.
 ------------------------------------------------------------------------------}
Function TWin32Object.Ellipse(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
Begin
  Result := Windows.Ellipse(DC, X1, Y1, X2, Y2);
End;

{------------------------------------------------------------------------------
  Method:  EmptyClipBoard
  Params: none
  Returns: If the function succeeds

  Empties the clipboard, frees handles to data in the clipboard, and ssigns
  ownership of the clipboard to the window that currently has the clipboard
  open.
 ------------------------------------------------------------------------------}
Function TWin32Object.EmptyClipBoard: Boolean;
Begin
  // Your code here
  Result := Windows.EmptyClipboard;
End;

{------------------------------------------------------------------------------
  Method:  EnableMenuItem
  Params: HMenu         - handle to menu
          UIDEnableItem - menu item to enable
          BEnable       - by position and grayed?
  Returns: The previous state of the menu item.

  Enables, disables, or grays the specified menu item.
 ------------------------------------------------------------------------------}
Function TWin32Object.EnableMenuItem(HMenu: HMENU; UIDEnableItem: Integer; BEnable: Boolean): Boolean;
Begin
  // Your code here
  Result := Windows.EnableMenuItem(HMenu, UIDEnableItem, DWORD(BEnable));
End;

{------------------------------------------------------------------------------
  Method:  EnableScrollBar
  Params:  Wnd      - handle to window or scroll bar
           WSBFlags - scroll bar type flag
           WArrows  - scroll bar arrow flag
  Returns: Nothing


 ------------------------------------------------------------------------------}
Function TWin32Object.EnableScrollBar(Wnd: HWND; WSBFlags, WArrows: Cardinal): Boolean;
Begin
  Assert(False, 'Trace:TODO: [TWin32Object.EnableScrollBar]');
  //TODO: Implement this;
  Result := Windows.EnableScrollBar(Wnd, WSBFlags, WArrows);
End;

{------------------------------------------------------------------------------
  Method:  EnableWindow
  Params: HWnd    - handle to window
          BEnable -  whether to enable the window
  Returns: If the window was previously disabled

  Enables or disables mouse and keyboard input to the specified window or
  control.
 ------------------------------------------------------------------------------}
Function TWin32Object.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
Begin
  Assert(False, Format('Trace:[TWin32Object.EnableWindow] HWnd: 0x%x, BEnable: %s', [HWnd, BOOL_TEXT[BEnable]]));
  Result := Windows.EnableWindow(HWnd, BEnable);
End;

{------------------------------------------------------------------------------
  Function: ExcludeClipRect
  Params:  dc, Left, Top, Right, Bottom
  Returns: integer

  Subtracts all intersecting points of the passed bounding rectangle
  (Left, Top, Right, Bottom) from the Current clipping region in the
  device context (dc).

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
function TWin32Object.ExcludeClipRect(dc: hdc;
  Left, Top, Right, Bottom : Integer) : Integer;
begin
  Result := Windows.ExcludeClipRect(dc, Left, Top, Right, Bottom);
end;

{------------------------------------------------------------------------------
  Method:  ExtTextOut
  Params:  DC      - handle to device context
           X       - x-coordinate of reference point
           Y       - x-coordinate of reference point
           Options - text-output options
           Rect    - optional clipping and/or opaquing rectangle
           Str     - character string to be drawn
           Count   - number of characters in string
           Dx      - pointer to array of intercharacter spacing values
  Returns: If the string was drawn.

  Draws a character string by using the currently selected font.
 ------------------------------------------------------------------------------}
Function TWin32Object.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
Begin
  Assert(False, Format('trace:> [TWin32Object.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
  Result := Windows.ExtTextOut(DC, X, Y, Options, LPRECT(Rect), Str, Count, Dx);
  Assert(False, Format('trace:< [TWin32Object.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
End;

{------------------------------------------------------------------------------
  Function: ExtSelectClipRGN
  Params:  dc, RGN, Mode
  Returns: integer

  Combines the passed Region with the current clipping region in the device
  context (dc), using the specified mode.

  The Combine Mode can be one of the following:
      RGN_AND  : all points which are in both regions

      RGN_COPY : an exact copy of the source region, same as SelectClipRGN

      RGN_DIFF : all points which are in the Clipping Region but
                 but not in the Source.(Clip - RGN)

      RGN_OR   : all points which are in either the Clip Region or
                 in the Source.(Clip + RGN)

      RGN_XOR  : all points which are in either the Clip Region
                 or in the Source, but not in both.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
function TWin32Object.ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;
begin
  Result := Windows.ExtSelectClipRGN(DC, RGN, Mode);
end;

{------------------------------------------------------------------------------
  Method:  FillRect
  Params:  DC    - handle to device context
           Rect  - record with rectangle
           Brush - handle to brush 
  Returns: If the function succeeds

  The FillRect Function fills a rectangle by using the specified brush.
  This Function includes the left and top borders, but excludes the right and
  bottom borders of the rectangle.
 ------------------------------------------------------------------------------}
Function TWin32Object.FillRect(DC: HDC; Const Rect: TRect; Brush: HBRUSH): Boolean;
Var
  R: TRect;
Begin
  R := Rect;
  Assert(False, Format('trace:> [TWin32Object.FillRect] DC: 0x%x; Rect: ((%d,%d)(%d,%d)); Brush: %x', [Integer(DC), R.left, R.top, R.right, R.bottom, brush]));
  Result := Boolean(Windows.FillRect(DC, Windows.RECT(r), Brush));
  Assert(False, Format('trace:< [TWin32Object.FillRect] DC: 0x%x; Rect: ((%d,%d)(%d,%d)); Brush: %x', [Integer(DC), R.left, R.top, R.right, R.bottom, brush]));
End;

{------------------------------------------------------------------------------
  Method: Frame3D
  Params: DC         - handle to device context
          Rect       - bounding rectangle
          FrameWidth - width of the frame (ignored on win32)
          Style      - frame style
  Returns: Whether the function was successful

  Draws a 3D border in GTK native style.
  NOTE: This function is mapped to DrawEdge on Windows.
------------------------------------------------------------------------------}
Function TWin32Object.Frame3D(DC: HDC; Var Rect: TRect; Const FrameWidth: Integer; Const Style: TBevelCut): Boolean;
Const
  Edge: Array[TBevelCut] Of Integer = (0, EDGE_ETCHED, EDGE_RAISED);
Begin
  Result := DrawEdge(DC, Rect, Edge[Style], BF_RECT);
End;

{------------------------------------------------------------------------------
  Method:  GetActiveWindow
  Params: none
  Returns: The handle to the active window

  Retrieves the window handle to the active window associated with the thread
  that calls the function.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetActiveWindow: HWND;
Begin
  Result := Windows.GetActiveWindow;
End;

{------------------------------------------------------------------------------
  Method:  GetCapture
  Params:  none
  Returns: the handle of the capture window

  Retrieves the handle of the window (if any) that has captured the mouse.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetCapture: HWND;
Begin
  Result := Windows.GetCapture;
End;

{------------------------------------------------------------------------------
  Method:  GetCaretPos
  Params:  LPPoint - record to receive coordinates
  Returns: If the function succeeds

  Gets the caret's position, in client coordinates.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetCaretPos(Var LPPoint: TPoint): Boolean;
Begin
  Result := Windows.GetCaretPos(@LPPoint);
End;

{------------------------------------------------------------------------------
  Method:  GetCharABCWidths
  Params:  DC         - handle of device context 
           P2         - first character in range to query 
           P3         - last character in range to query 
           ABCStructs - character-width record
  Returns: If the function succeeds

  Retrieves the widths, in logical units, of consecutive characters in a given
  range from the current TrueType font.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetCharABCWidths(DC: HDC; P2, P3: UINT; Const ABCStructs): Boolean;
Begin
  Result := Windows.GetCharABCWidths(DC, P2, P3, ABCStructs);
End;

{------------------------------------------------------------------------------
  Method:  GetClientRect
  Params: Handle - handle of window
          Rect   - record for client coordinates
  Returns: If the function succeeds

  Retrieves the coordinates of a window's client area.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetClientRect(Handle: HWND; Var Rect: TRect): Boolean;
Begin
  Result := Windows.GetClientRect(Handle, @Rect);
End;

{------------------------------------------------------------------------------
  Function: GetClipBox
  Params: dc, lprect
  Returns: Integer
 
  Returns the smallest rectangle which includes the entire current
  Clipping Region.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
Function TWin32Object.GetClipBox(DC : hDC; lpRect : PRect) : Longint;
begin
  Result := Windows.GetClipBox(DC, Windows.LPRECT(lpRect));
end;

{------------------------------------------------------------------------------
  Function: GetClipRGN
  Params: dc, rgn
  Returns: Integer
 
  Returns the current Clipping Region.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion


 ------------------------------------------------------------------------------}
Function TWin32Object.GetClipRGN(DC : hDC; RGN : hRGN) : Integer; 
begin
  Result := Windows.GetClipRGN(DC, RGN);
end;

{------------------------------------------------------------------------------
  Method:  GetDC
  Params:  HWND - handle of window
  Returns: value identifying the device context for the given window's client
           area

  Retrieves a handle of a display device context (DC) for the client area of
  the specified window.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetDC(HWnd: HWND): HDC;
Begin
  Assert(False, Format('Trace:> [TWin32Object.GetDC] HWND: 0x%x', [HWnd]));
  Result := Windows.GetDC(HWnd);
  Assert(False, Format('Trace:< [TWin32Object.GetDC] Got 0x%x', [Result]));
End;

function TWin32Object.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Result := Windows.GetDIBits(DC, Bitmap, StartScan, NumScans, Bits, Windows.PBitmapInfo(@BitInfo)^, Usage)
end;

function TWin32Object.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
begin
  Result := Windows.GetBitmapBits(Bitmap, Count, Bits);
end;

function TWin32Object.CreateDIBSection(DC: HDC; const p2: tagBitmapInfo; p3: UINT;
  var p4: Pointer; p5: THandle; p6: DWORD): HBITMAP;
begin
  Result := Windows.CreateDIBSection(DC, Windows.PBitmapInfo(@p2)^,  p3, p4, p5, p6)
end;
{------------------------------------------------------------------------------
  Method:  GetFocus
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetFocus: HWND;
Begin
  Result := Windows.GetFocus;
End;

{------------------------------------------------------------------------------
  Method:  GetKeyState
  Params:  NVirtKey - The requested key
  Returns: If the Function succeeds, the return value specifies the status of
           the given virtual key. If the high-order bit is 1, the key is down;
           otherwise, it is up. If the low-order bit is 1, the key is toggled.

  The GetKeyState Function retrieves the status of the specified virtual key.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetKeyState(nVirtKey: Integer): Smallint;
Begin
  Result := Windows.GetKeyState(nVirtKey);
End;

{------------------------------------------------------------------------------
  Method:  GetObject
  Params:  GDIObj  - handle to graphics object of interest
           BufSize - size of buffer for object information
           Buf     - pointer to buffer for object information
  Returns: the number of bytes stored into the buffer

  Gets information about a specified graphics object.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
Begin
  Assert(False, 'Trace:[TWin32Object.GetObject]');
  Result := Windows.GetObject(GDIObj, BufSize, Buf);
End;

{------------------------------------------------------------------------------
  Method:  GetParent
  Params: Handle - handle of child window
  Returns: the handle of the parent window

  Retrieves the handle of the specified child window's parent window.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetParent(Handle: HWND): HWND;
Begin
  Result := Windows.GetParent(Handle);
End;


{------------------------------------------------------------------------------
  Method:  GetProp
  Params: Handle - handle of window
          Str    - string
  Returns: the associated data

  Retrieves a pointer to data from the property list of the given window.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetProp(Handle: HWND; Str: PChar): Pointer;
Begin
  Result := Pointer(Windows.GetProp(Handle, Str));
End;


{------------------------------------------------------------------------------
  Method:  GetScrollInfo
  Params:  Handle     - handle of window with scroll bar
           BarFlag    - scroll bar flag
           ScrollInfo - record for scroll parameters
  Returns: If the function retrieved any values.

  Retrieves the parameters of a scroll bar.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean;
Begin
  Assert(False, 'Trace:TODO: [TWin32Object.GetScrollInfo]');
  Result := Windows.GetScrollInfo(Handle, BarFlag, Windows.SCROLLINFO(ScrollInfo));
End;

{------------------------------------------------------------------------------
  Method:  GetStockObject
  Params:  Value - type of stock object
  Returns: a value identifying the logical object requested

  Retrieves a handle to one of the predefined stock objects.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetStockObject(Value: Integer): LongInt;
Begin
  Assert(False, Format('Trace:> [TWin32Object.GetStockObject] %d ', [Value]));
  Result := Windows.GetStockObject(Value);
  Assert(False, Format('Trace:< [TWin32Object.GetStockObject] %d --> 0x%x', [Value, Result]));
End;

{------------------------------------------------------------------------------
  Method:  GetSysColor
  Params:   NIndex - display element whose color is to be retrieved
  Returns:  RGB value

  Retrieves the current color of the specified display element.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetSysColor(NIndex: Integer): DWORD;
Begin
  Result := Windows.GetSysColor(nIndex);
End;

{------------------------------------------------------------------------------
  Method:  GetSystemMetrics
  Params: NIndex - system metric to retrieve
  Returns: the requested system metric

  Retrieves various system metrics.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetSystemMetrics(NIndex: Integer): Integer;
Begin
  Assert(False, Format('Trace:[TWin32Object.GetSystemMetrics] %s', [IntToStr(NIndex)]));
  Result := Windows.GetSystemMetrics(NIndex);
  Assert(False, Format('Trace:[TWin32Object.GetSystemMetrics] %s --> 0x%S (%s)', [IntToStr(NIndex), IntToHex(Result, 8), IntToStr(Result)]));
End;

Function TWin32Object.GetTextColor(DC: HDC): TColorRef;
Begin
  Result := Windows.GetTextColor(DC);
End;

{------------------------------------------------------------------------------
  Method:  GetTextExtentPoint
  Params:  DC    - handle of device context 
           Str   - text string
           Count - number of characters in string
           Size  - TSize record in which the dimensions of the string are to be
                   returned
  Returns: If the function succeeded

  Computes the width and height of the specified string of text.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; Var Size: TSize): Boolean;
Begin
  Assert(False, 'Trace:[TWin32Object.GetTextExtentPoint] - Start');
  Result := Windows.GetTextExtentPoint32(DC, Str, Count, @Size);
  Assert(False, 'Trace:[TWin32Object.GetTextExtentPoint] - Exit');
End;

{------------------------------------------------------------------------------
  Method:  GetTextMetrics
  Params:  DC - handle of device context
           TM - text metrics record
  Returns: If the function succeeds

  Fills the specified buffer with the metrics for the currently selected font.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetTextMetrics(DC: HDC; Var TM: TTextMetric): Boolean;
Begin
  Assert(False, Format('Trace:> TODO FINISH[TWin32Object.GetTextMetrics] DC: 0x%x', [DC]));
  Result := Windows.GetTextMetrics(DC, @TM);
  Assert(False, Format('Trace:< TODO FINISH[TWin32Object.GetTextMetrics] DC: 0x%x', [DC]));
End;

{------------------------------------------------------------------------------
  Method:  GetWindowLong
  Params:  Handle - handle of window
           Int    - value to retrieve
  Returns: the requested 32-bit value

  Retrieves information about the specified window.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetWindowLong(Handle: HWND; Int: Integer): LongInt;
Begin
  //TODO:Started but not finished
  Assert(False, Format('Trace:> [TWin32Object.GETWINDOWLONG] HWND: 0x%x, int: 0x%x (%d)', [Handle, int, int]));
  Result := Windows.GetWindowLong(Handle, int);
  Assert(False, Format('Trace:< [TWin32Object.GETWINDOWLONG] HWND: 0x%x, int: 0x%x (%d) --> 0x%x (%d)', [Handle, int, int, Result, Result]));
End;

{------------------------------------------------------------------------------
  Method:  GetWindowOrgEx
  Params:  DC - handle of device context 
           P  - record receiving the window origin
  Returns: 0 if the function fails; non-zero integer otherwise

  Retrieves the x-coordinates and y-coordinates of the window origin for the
  specified device context.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetWindowOrgEx(DC: HDC; Var P: TPoint): Integer;
Begin
  Result := Integer(Windows.GetWindowOrgEx(DC, @P));
End;

{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - handle of window
           Rect   - record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the dimensions of the bounding rectangle of the specified window.
 ------------------------------------------------------------------------------}
Function TWin32Object.GetWindowRect(Handle: HWND; Var Rect: TRect): Integer;
Begin
  Result := Integer(Windows.GetWindowRect(Handle, @Rect));
End;

Function TWin32Object.GetWindowSize(Handle : hwnd; var Width, Height: integer): boolean;
var
  R: TRect;
begin
  Result := GetClientRect(Handle, R);
  with R Do
  begin
    Width := right - left;
    Height := bottom - top;
  end;
end;

{------------------------------------------------------------------------------
  Method:  HideCaret
  Params:  HWnd - handle to the window with the caret
  Returns: Whether the window owns the caret

  Removes the caret from the screen.
 ------------------------------------------------------------------------------}
Function TWin32Object.HideCaret(HWnd: HWND): Boolean;
Begin
  Assert(False, Format('Trace:  [TWin32Object.HideCaret] HWND: 0x%x', [HWnd]));
  Result := Windows.HideCaret(hWnd);
End;

{------------------------------------------------------------------------------
  Method:  InvalidateRect
  Params: AHandle - handle of window with changed update region
          Rect    - address of rectangle coordinates
          BErase  - specifies whether the background is to be erased
  Returns: if the function succeeds

  Adds a rectangle to the specified window's update region.
 ------------------------------------------------------------------------------}
Function TWin32Object.InvalidateRect(aHandle: HWND; Rect: PRect; BErase: Boolean): Boolean;
Begin
  Result := Windows.InvalidateRect(aHandle, Windows.RECT(Rect^), bErase);
End;

{------------------------------------------------------------------------------
  Function: IntersectClipRect
  Params:  dc, Left, Top, Right, Bottom
  Returns: Integer

  Shrinks the clipping region in the device context dc to a region of all 
  intersecting points between the boundary defined by Left, Top, Right, 
  Bottom , and the Current clipping region.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
function TWin32Object.IntersectClipRect(dc: hdc;
  Left, Top, Right, Bottom: Integer): Integer;
begin
  Result := Windows.IntersectClipRect(DC, Left, Top, Right, Bottom);
end;

{------------------------------------------------------------------------------
  Method:  LineTo
  Params:  DC - device context handle
           X  - x-coordinate of line's ending point 
           Y  - y-coordinate of line's ending point 
  Returns: if the function succeeds

  Draws a line from the current position up to, but not including, the specified point.
 ------------------------------------------------------------------------------}
Function TWin32Object.LineTo(DC: HDC; X, Y: Integer): Boolean;
Begin
  Assert(False, Format('Trace:> [TWin32Object.LineTo] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
  Result := Windows.LineTo(DC, X, Y);
  Assert(False, Format('Trace:< [TWin32Object.LineTo] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
End;

{------------------------------------------------------------------------------
  Method:  MaskBlt
  Params:  DestDC        - The destination device context
           X, Y          - The left/top corner of the destination rectangle
           Width, Height - The size of the destination rectangle
           SrcDC         - The source device context 
           XSrc, YSrc    - The left/top corner of the source rectangle   
           Mask          - The handle of a monochrome bitmap
           XMask, YMask  - The left/top corner of the mask rectangle
           Rop           - The raster operation to be performed 
  Returns: True if succesful

  The MaskBlt Function copies a bitmap from a source context into a destination 
  context using the specified mask and raster operation.
 ------------------------------------------------------------------------------}
Function TWin32Object.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; 
Begin
  Result := Windows.MaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Mask, XMask, YMask, Rop);
End;

{------------------------------------------------------------------------------
  Method:  MessageBox
  Params:  HWnd      - The handle of parent window
           LPText    - text in message box
           LPCaption - title of message box
           UType     - style of message box
  Returns: 0 if not successful (out of memory), otherwise one of the defined
           values:
             IDABORT
             IDCANCEL
             IDIGNORE
             IDNO
             IDOK
             IDRETRY
             IDYES

 The MessageBox function displays a modal dialog, with text and caption defined,
 and includes buttons.
 ------------------------------------------------------------------------------}
Function TWin32Object.MessageBox(HWnd: HWND; LPText, LPCaption: PChar; UType: Cardinal): Integer;
Begin
  Result := Windows.MessageBox(HWnd, LPText, LPCaption, UType);
End;

{------------------------------------------------------------------------------
  Method:  MoveToEx
  Params:  DC       - handle of device context
           X        - x-coordinate of new current position
           Y        - x-coordinate of new current position
           OldPoint - address of old current position 
  Returns: If the function succeeds.

  Updates the current position to the specified point.
 ------------------------------------------------------------------------------}
Function TWin32Object.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
Begin
  Assert(False, Format('Trace:> [TWin32Object.MoveToEx] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
  Result := Windows.MoveToEx(DC, X, Y, LPPOINT(OldPoint));
  Assert(False, Format('Trace:< [TWin32Object.MoveToEx] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
End;


{------------------------------------------------------------------------------
  Method:  PeekMessage
  Params:  LPMsg         - Where it should put the message
           Handle        - Handle of the window (thread)
           WMsgFilterMin - Lowest MSG to grab
           WMsgFilterMax - Highest MSG to grab
           WRemoveMsg    - Should message be pulled out of the queue
  Returns: Boolean if an event was there

  Checks a thread message queue for a message.
 ------------------------------------------------------------------------------}
Function TWin32Object.PeekMessage(Var LPMsg: TMsg; Handle: HWND; WMsgFilterMin, WMsgFilterMax, WRemoveMsg: UINT): Boolean;
Begin 
  Result := Windows.PeekMessage(@LPMsg, Handle, WMsgFilterMin, WMsgFilterMax, WRemoveMsg);
End;

{------------------------------------------------------------------------------
  Method:   Pie
  Params:   DC, X, Y, Width, Height, Angle1, Angle2
  Returns:  Nothing

  Use Pie to draw a filled pie-shaped wedge on the canvas.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.

  NOTE: This just calls Arc
 ------------------------------------------------------------------------------}
Function TWin32Object.Pie(DC: HDC;
  X, Y, Width, Height, Angle1, Angle2: Integer): Boolean;
var
  SX, SY, EX, EY : Longint;
Begin
  // Old: Result := Arc(DC, X, Y, Width, Height, Angle1, Angle2);
  Angles2Coords(X,Y,Width,Height,Angle1,Angle2, SX, SY, EX, EY);
  Result := Windows.Pie(DC, X, Y, X+Width, Y+Height, SX,SY,EX,EY);
End;

{------------------------------------------------------------------------------
  Method:  PolyBezier
  Params:  DC, Points, NumPts, Filled, Continous
  Returns: Boolean

  Use Polybezier to draw cubic Bézier curves. The first curve is drawn from the
  first point to the fourth point with the second and third points being the
  control points. If the Continuous flag is TRUE then each subsequent curve
  requires three more points, using the end-point of the previous Curve as its
  starting point, the first and second points being used as its control points,
  and the third point its end-point. If the continous flag is set to FALSE,
  then each subsequent Curve requires 4 additional points, which are used
  excatly as in the first curve. Any additonal points which do not add up to
  a full bezier(4 for Continuous, 3 otherwise) are ingored. There must be at
  least 4 points for an drawing to occur. If the Filled Flag is set to TRUE
  then the resulting Poly-Bézier will be drawn as a Polygon.

 ------------------------------------------------------------------------------}
Function TWin32Object.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: Boolean): Boolean;
Begin
  If Filled or (not Continuous) then
    Result := Inherited PolyBezier(DC,Points,NumPts, Filled, Continuous)
  else
    Result := Windows.PolyBezier(DC, LPPOINT(Points)^, NumPts);
End;

{------------------------------------------------------------------------------
  Method:  Polygon
  Params:  DC - handle to device context
           Points - pointer to polygon's vertices
           NumPts - count of polygon's vertices
           Winding
  Returns: If the function succeeds

  Use Polygon to draw a closed, many-sided shape on the canvas, using the value
  of Pen. After drawing the complete shape, Polygon fills the shape using the
  value of Brush.
  The Points parameter is an array of points that give the vertices of the
  polygon.
  Winding determines how the polygon is filled.
  When Winding is True, Polygon
  fills the shape using the Winding fill algorithm. When Winding is False,
  Polygon uses the even-odd (alternative) fill algorithm.
  NumPts indicates the number of points to use.
  The first point is always connected to the last point.
  To draw a polygon on the canvas, without filling it, use the Polyline method,
  specifying the first point a second time at the end.
 ------------------------------------------------------------------------------}
Function TWin32Object.Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean): Boolean;
var
  PFMode : Longint;
Begin
  Assert(False, Format('Trace:TWin32Object.Polygon --> DC: 0x%X, Number of points: %D, Use winding fill: %S', [DC, NumPts, BOOL_RESULT[Winding]]));
  If Winding then
    PFMode := SetPolyFillMode(DC, Windows.WINDING)
  else
    PFMode := SetPolyFillMode(DC, Windows.ALTERNATE);
  Result := Windows.Polygon(DC, LPPOINT(Points)^, NumPts);
  SetPolyFillMode(DC, PFMode);
End;

{------------------------------------------------------------------------------
  Method:  Polyline
  Params:  DC - handle of device context
           Points - address of array containing endpoints
           NumPts - number of points in the array
  Returns: If the function succeeds

  Draws a series of line segments by connecting the points in the specified
  array.
 ------------------------------------------------------------------------------}
Function TWin32Object.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): Boolean;
Begin
  Result := Windows.Polyline(DC, LPPOINT(Points)^, NumPts);
End;

{------------------------------------------------------------------------------
  Method:  PostMessage
  Params: Handle - handle of destination window
          Msg - message to post
          WParam - first message parameter
          LParam - second message parameter
  Returns: True if succesful

  The PostMessage Function places (posts) a message in the message queue and
  then returns without waiting.
 ------------------------------------------------------------------------------}
Function TWin32Object.PostMessage(Handle: HWND; Msg: Cardinal; WParam: LongInt; LParam: LongInt): Boolean;
Begin
  Result := Windows.PostMessage(Handle, Msg, wParam, lParam);
End;

{------------------------------------------------------------------------------
  Method:   RadialArc
  Params:   DC,x,y,width,height,sx,sy,ex,ey 
  Returns:  Nothing

  Use RadialArc to draw an elliptically curved line with the current Pen. The 
  values sx,sy, and ex,ey represent the starting and ending radial-points 
  between which the Arc is drawn.

------------------------------------------------------------------------------}
function TWin32Object.RadialArc(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; 
Begin
  Result := Windows.Arc(DC, X, Y, X+Width, Y+Height, SX,SY,EX,EY);
End;

{------------------------------------------------------------------------------
  Method:   RadialChord
  Params:   DC,x,y,width,height,sx,sy,ex,ey 
  Returns:  Nothing

  Use RadialChord to draw a filled Chord-shape on the canvas. The values sx,sy,
  and ex,ey represent the starting and ending radial-points between which
  the bounding-Arc is drawn.

------------------------------------------------------------------------------}
function TWin32Object.RadialChord(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; 
Begin
  Result := Windows.Chord(DC, X, Y, X+Width, Y+Height, SX,SY,EX,EY);
End;

{------------------------------------------------------------------------------
  Method:   RadialPie
  Params:   DC,x,y,width,height,sx,sy,ex,ey 
  Returns:  Nothing

  Use RadialPie to draw a filled Pie-shaped Wedge on the canvas. The values 
  sx,sy, and ex,ey represent the starting and ending radial-points between which
  the bounding-Arc is drawn.

------------------------------------------------------------------------------}
function TWin32Object.RadialPie(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; 
Begin
  Result := Windows.Pie(DC, X, Y, X+Width, Y+Height, SX,SY,EX,EY);
End;

{------------------------------------------------------------------------------
  Method:  RealizePalette
  Params:  DC - handle of device context
  Returns: number of entries in the logical palette mapped to the system
           palette

  Maps palette entries from the current logical palette to the system palette.
 ------------------------------------------------------------------------------}
Function TWin32Object.RealizePalette(DC: HDC): Cardinal;
Begin
  Assert(False, 'Trace:TODO: [TWin32Object.RealizePalette]');
  //TODO: Implement this;
  Result := Windows.RealizePalette(DC);
End;

{------------------------------------------------------------------------------
  Method:  Rectangle
  Params:  DC - handle of device context 
           X1 - x-coordinate of bounding rectangle's upper-left corner
           Y1 - y-coordinate of bounding rectangle's upper-left corner
           X2 - x-coordinate of bounding rectangle's lower-right corner
           Y2 - y-coordinate of bounding rectangle's lower-right corner
  Returns: If the function succeeds

  The Rectangle function draws a rectangle. The rectangle is outlined by using
  the current pen and filled by using the current brush.
 ------------------------------------------------------------------------------}
Function TWin32Object.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
Begin
  Assert(False, Format('Trace:> [TWin32Object.Rectangle] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d', [DC, X1, Y1, X2, Y2]));
  Result := Windows.Rectangle(DC, X1, Y1, X2, Y2);
  Assert(False, Format('Trace:< [TWin32Object.Rectangle] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d', [DC, X1, Y1, X2, Y2]));
End;

{------------------------------------------------------------------------------
  Method:  ReleaseCapture
  Params:  none
  Returns: True if succesful

  The ReleaseCapture Function releases the mouse capture from a window 
  and restores normal mouse input processing.
 ------------------------------------------------------------------------------}
Function TWin32Object.ReleaseCapture: Boolean;
Begin
  Result := Windows.ReleaseCapture;
End;

{------------------------------------------------------------------------------
  Method:  ReleaseDC
  Params:  HWnd - handle of window 
           DC   - handle of device context 
  Returns: 1 if the device context was released or 0 if it wasn't

  Releases a device context (DC), freeing it for use by other applications.
 ------------------------------------------------------------------------------}
Function TWin32Object.ReleaseDC(HWnd: HWND; DC: HDC): Integer;
Begin
//writeln('[TWin32Object.ReleaseDC] ',HexStr(DC, 8),'  ',FDeviceContexts.Count);
  Assert(False, Format('Trace:> [TWin32Object.ReleaseDC] DC:0x%x', [DC]));
  Result := Windows.ReleaseDC(HWnd, DC);
  Assert(False, Format('Trace:< [TWin32Object.ReleaseDC] FDeviceContexts DC:0x%x', [DC]));
End;

{------------------------------------------------------------------------------
  Method:  RestoreDC
  Params:  DC - handle of device context
           SavedDC - state to be restored 
  Returns: if the function succeeds

  Restores a device context (DC) to the specified state.
-------------------------------------------------------------------------------}
Function TWin32Object.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
Begin
  Assert(False, Format('Trace:> [TWin32Object.RestoreDC] DC:0x%x, SavedDC: %d', [DC, SavedDC]));
  Result := Windows.RestoreDC(DC, SavedDC);
  Assert(False, Format('Trace:< [TWin32Object.RestoreDC] DC:0x%x, Saved: %d --> %s', [Integer(DC), SavedDC, BOOL_TEXT[Result]]));
End;

{------------------------------------------------------------------------------
  Method:  SaveDC
  Params:  DC - a DC to save
  Returns: 0 if the functions fails otherwise a positive integer identifing 
           the saved DC

  The SaveDC function saves the current state of the specified device 
  context (DC) by copying its elements to a context stack.
-------------------------------------------------------------------------------}
Function TWin32Object.SaveDC(DC: HDC): Integer;
Begin
  Assert(False, Format('Trace:> [TWin32Object.SaveDC]  0x%x', [Integer(DC)]));
  Result := Windows.SaveDC(DC);
  Assert(False, Format('Trace:< [TWin32Object.SaveDC]  0x%x --> %d', [Integer(DC), Result]));
End;

{------------------------------------------------------------------------------
  Method:  ScreenToClient
  Params: Handle - window handle for source coordinates 
          P      - record containing coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Converts the screen coordinates of a specified point on the screen to client
  coordinates.
 ------------------------------------------------------------------------------}
Function TWin32Object.ScreenToClient(Handle: HWND; Var P: TPoint): Integer;
Begin
  Result := Integer(Windows.ScreenToClient(Handle, @P));
End;

{------------------------------------------------------------------------------
  Method:  ScrollWindowEx
  Params:  HWnd       - handle of window to scroll
           DX         - horizontal amount to scroll
           DY         - vertical amount to scroll
           PRcScroll  - pointer to scroll rectangle
           PRcClip    - pointer to clip rectangle
           HRgnUpdate - handle of update region
           PRcUpdate  - pointer to update rectangle
           Flags      - scrolling flags
           
  Returns: True if succesfull

  The ScrollWindowEx function scrolls the content of the specified window's 
  client area
 ------------------------------------------------------------------------------}
Function TWin32Object.ScrollWindowEx(HWnd: HWND; DX, DY: Integer; PRcScroll, PRcClip: PRect; HRgnUpdate: HRGN; PRcUpdate: PRect; Flags: UINT): Boolean; 
Begin
  Result := Windows.ScrollWindowEx(HWnd, DX, DY, Windows.RECT(PRcScroll^), Windows.RECT(PRcClip^), HRgnUpdate, LPRECT(PRcUpdate), Flags) <> ERROR;
End;

{------------------------------------------------------------------------------
  Function: SelectClipRGN
  Params:  DC, RGN
  Returns: longint

  Sets the DeviceContext's ClipRegion. The Return value 
  is the new clip regions type, or ERROR.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion

 ------------------------------------------------------------------------------}
Function TWin32Object.SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
begin
  Result := Windows.SelectClipRGN(DC, RGN);
end;

{------------------------------------------------------------------------------
  Method:  SelectObject
  Params:  DC - handle of device context
           GDIObj - handle of object
  Returns: he handle of the object being replaced

  Selects an object into the specified device context.
 ------------------------------------------------------------------------------}
Function TWin32Object.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
Begin
  //TODO: Finish this;
  Assert(False, Format('Trace:> [TWin32Object.SelectObject] DC: 0x%x', [DC]));
  Result := Windows.SelectObject(DC, GDIObj);
  Assert(False, Format('Trace:< [TWin32Object.SelectObject] DC: 0x%x  --> 0x%x', [DC, Result]));
End;

{------------------------------------------------------------------------------
  Method:  SelectPalette
  Params:  DC - handle of device context 
           Palette - handle of logical color palette
           ForceBackground - whether the logical palette is forced to be a
                             background palette
  Returns: the device context's previous logical palette

  Selects the specified logical palette into a device context.
 ------------------------------------------------------------------------------}
Function TWin32Object.SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
Begin
  Assert(False, 'Trace:TODO: [TWin32Object.SelectPalette]');
  //TODO: Implement this;
  Result := Windows.SelectPalette(DC, Palette, ForceBackground);
End;

{------------------------------------------------------------------------------
  Method:  SendMessage
  Params: HandleWnd   - handle of destination window
          Msg         - message to send
          WParam      - first message parameter
          LParam      - second message parameter
  Returns: the result of the message processing

  The SendMessage function sends the specified message to a window or windows. 
  The function calls the window procedure for the specified window and does 
  not return until the window procedure has processed the message.
 ------------------------------------------------------------------------------}
Function TWin32Object.SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: LongInt; LParam: LongInt): Integer;
Begin
  Result := Windows.SendMessage(HandleWnd, Msg, WParam, LParam);
End;

{------------------------------------------------------------------------------
  Method:  SetBkColor
  Params:  DC    - Device context to change the text background color
           Color - background color value
  Returns: Old Background color

  Sets the current background color to the specified color value.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetBKColor(DC: HDC; Color: TColorRef): TColorRef;
Begin
  Assert(False, Format('Trace:> [TWin32Object.SetBKColor] DC: 0x%x Color: %8x', [Integer(DC), Color]));
  Result := Windows.SetBkColor(DC, ColorToRGB(Color));
  Assert(False, Format('Trace:< [TWin32Object.SetBKColor] DC: 0x%x Color: %8x --> %8x', [Integer(DC), Color, Result]));
End;

{------------------------------------------------------------------------------
  Method:  SetBkMode
  Params: DC     - handle of device context
          BkMode - flag specifying background mode
  Returns: the previous background mode

  Sets the background mix mode of the specified device context.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetBkMode(DC: HDC; BkMode: Integer): Integer;
Begin
  // Your code here
  Result := Windows.SetBkMode(DC, BkMode);
End;

{------------------------------------------------------------------------------
  Method:  SetCapture
  Params:  Value - Handle of window to capture
  Returns: the handle of the window that had previously captured the mouse

  Sets the mouse capture to the specified window.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetCapture(Value: LongInt): LongInt;
Begin
  Assert(False, Format('Trace:> [TWin32Object.SetCapture] 0x%x', [Value]));
  Result := Windows.SetCapture(Value); 
  Assert(False, Format('Trace:< [TWin32Object.SetCapture] 0x%x --> 0x%x', [Value, Result]));
End;

{------------------------------------------------------------------------------
  Method:  SetCaretPos
  Params:  new position x, y 
  Returns: true on success

  Moves the caret to the specified coordinates.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetCaretPos(X, Y: Integer): Boolean;
Begin
  Result := Windows.SetCaretPos(X, Y);
End;

{------------------------------------------------------------------------------
  Method:  SetCaretPosEx
  Params:  Handle - handle of window
           X      - horizontal mouse coordinate
           Y      - vertical mouse coordinate
  Returns: true on success

  Moves the caret to the specified coordinates in the specified window.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetCaretPosEx(Handle: HWND; X, Y: Integer): Boolean;
Begin
  Result := ShowCaret(Handle) And SetCaretPos(X, Y);
End;

{------------------------------------------------------------------------------
  Method:  SetCaretRespondToFocus
  Params:  Handle          - Handle of a TWinControl
           ShowHideOnFocus - true = caret is hidden on focus lost
  Returns: true on success

 ------------------------------------------------------------------------------}
Function TWin32Object.SetCaretRespondToFocus(Handle: HWND; ShowHideOnFocus: Boolean): Boolean;
Begin
  If ShowHideOnFocus Then
    Result := ShowCaret(Handle)
  Else
    Result := HideCaret(Handle)
End;

{------------------------------------------------------------------------------
  Method:  SetFocus
  Params:  HWnd - Handle of new focus window
  Returns: The old focus window

  The SetFocus function sets the keyboard focus to the specified window
 ------------------------------------------------------------------------------}
Function TWin32Object.SetFocus(HWnd: HWND): HWND;
Begin
  Result := Windows.SetFocus(HWnd);
End;

{------------------------------------------------------------------------------
  Method:  SetProp
  Params:  Handle - handle of window
           Str    - string
           Data   - pointer to data
  Returns: Whether the string and data were successfully added to the property
           list.

  Adds a new entry or changes an existing entry in the property list of the
  specified window.

  NOTE: LCLLinux has no RemoveProp function but Windows API requires all set
  properties to be removed, so I'm keeping a list of windows with properties
  for a properties-enumeration function that's called when the program is quit.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetProp(Handle: HWND; Str: PChar; Data: Pointer): Boolean;
Var
  C: Cardinal;
  WndListed: Boolean;
Begin
  Assert(False, 'Trace:TWin32Object.SetProp - Start');
  WndListed := False;

  If WndList.Count > 0 Then
    For C := 0 To WndList.Count - 1 Do
      If HWND(WndList[C]) = Handle Then
        WndListed := True;

  If Not WndListed Then
  Begin
    WndList.Capacity := WndList.Count;
    WndList.Add(Pointer(Handle));
  End;

  Result := Windows.SetProp(Handle, Str, Integer(Data));
  Assert(False, Format('Trace:TWin32Object.SetProp --> Window handle: 0x%X, Propery to set: %S, Data to set: 0x%P, Window was previously in list: %S, Property was successfully set: %S', [Handle, String(Str), Data, BOOL_RESULT[WndListed], BOOL_RESULT[Result]]));
End;

{------------------------------------------------------------------------------
  Method:  SetScrollInfo
  Params:  Handle     - handle of window with scroll bar
           SBStyle    - scroll bar flag
           ScrollInfo - record with scroll parameters
           BRedraw    - is the scroll bar is redrawn?
  Returns: The old position value

  Sets the parameters of a scroll bar.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetScrollInfo(Handle: HWND; SBStyle: Integer; ScrollInfo: TScrollInfo; BRedraw: Boolean): Integer;
Begin
//  Assert(False, 'Trace:[TWin32Object.SetScrollInfo]');
  With ScrollInfo Do
   // Assert(False, Format('Trace:> [TWin32Object.SetScrollInfo] Mask:0x%x, Min:%d, Max:%d, Page:%d, Pos:%d', [FMask, NMin, NMax, NPage, NPos]));

  Result := Windows.SetScrollInfo(Handle, SBStyle, @ScrollInfo, BRedraw);
  With ScrollInfo Do
    Assert(False, Format('Trace:> [TWin32Object.SetScrollInfo] --> %d', [Result]));
End;

{------------------------------------------------------------------------------
  Method:  SetSysColors
  Params:  CElements    - the number of elements
           LPAElements  - array with element numbers
           LPARGBValues - array with colors
  Returns: 0 if unsuccesful 

  The SetSysColors function sets the colors for one or more display elements.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetSysColors(CElements: Integer; Const LPAElements; Const LPARGBValues): Boolean; 
Begin
  Result := Windows.SetSysColors(CElements, LPAElements, LPARGBValues);
End;

{------------------------------------------------------------------------------
  Method:  SetTextCharacterExtra
  Params: _HDC       - handle of device context
          NCharExtra - extra-space value
  Returns: the previous intercharacter spacing

  Sets the intercharacter spacing.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetTextCharacterExtra(_HDC: HDC; NCharExtra: Integer): Integer;
Begin
  // Your code here
  Result := Windows.SetTextCharacterExtra(_HDC, NCharExtra);
End;

{------------------------------------------------------------------------------
  Method:  SetTextColor
  Params:  DC    - Identifies the device context.
           Color - Specifies the color of the text.
  Returns: The previous color if succesful, CLR_INVALID otherwise

  The SetTextColor function sets the text color for the specified device
  context to the specified color.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
Begin
  Assert(False, Format('Trace:> [TWin32Object.SetTextColor] DC: 0x%x Color: %8x', [Integer(DC), Color]));
  Result := Windows.SetTextColor(DC, ColorToRGB(Color));
  Assert(False, Format('Trace:< [TWin32Object.SetTextColor] DC: 0x%x Color: %8x --> %8x', [Integer(DC), Color, Result]));
End;

{------------------------------------------------------------------------------
  Procedure: SetWindowLong
  Params:  Handle  - handle of window
           Idx     - value to set
           NewLong - new value
  Returns: Nothing

  Changes an attribute of the specified window.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetWindowLong(Handle: HWND; Idx: Integer; NewLong: LongInt): LongInt;
Begin
  //TODO: Finish this;
  Assert(False, Format('Trace:> [TWin32Object.SETWINDOWLONG] HWND: 0x%x, Idx: 0x%x(%d), Value: 0x%x(%d)', [Handle, Idx, Idx, NewLong, NewLong]));
  Result := Windows.SetWindowLong(Handle, Idx, NewLong);
  Assert(False, Format('Trace:< [TWin32Object.SETWINDOWLONG] HWND: 0x%x, Idx: 0x%x(%d), Value: 0x%x(%d) --> 0x%x(%d)', [Handle, Idx, Idx, NewLong, NewLong, Result, Result]));
End;

{------------------------------------------------------------------------------
  Method:  SetWindowOrgEx
  Params:  DC    - handle of device context 
           NewX  - new x-coordinate of window origin
           NewY  - new y-coordinate of window origin
           Point - record receiving original origin
  Returns: Whether the call was successful

  Sets the window origin of the device context by using the specified coordinates.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetWindowOrgEx(DC: HDC; NewX, NewY: Integer;
  Var OldPoint: TPoint): Boolean;
Begin
//writeln('[TWin32Object.SetWindowOrgEx] ', NewX, ' ', NewY);
// ToDo: move origin

  Result := Windows.SetWindowOrgEx(DC, NewX, NewY, @OldPoint);
End;

{------------------------------------------------------------------------------
  Method:  SetWindowPos
  Params: HWnd            - handle of window
          HWndInsertAfter - placement-order handle
          X               - horizontal position
          Y               - vertical position
          CX              - width
          CY              - height
          UFlags          - window-positioning flags
  Returns: If the function succeeds

  Changes the size, position, and Z order of a child, pop-up, or top-level
  window.
 ------------------------------------------------------------------------------}
Function TWin32Object.SetWindowPos(HWnd: HWND; HWndInsertAfter: HWND; X, Y, CX, CY: Integer; UFlags: UINT): Boolean;
Begin
//writeln('[TWin32Object.SetWindowPos] Top=',HWndInsertAfter=HWND_TOP);
  Result := Windows.SetWindowPos(HWnd, HWndInsertAfter, X, Y, CX, CY, UFlags);
End;

{------------------------------------------------------------------------------
  Method:  ShowCaret
  Params:  HWnd - handle of window with caret 
  Returns: if the function succeeds

  Makes the caret visible on the screen at the caret's current position.
 ------------------------------------------------------------------------------}
Function TWin32Object.ShowCaret(HWnd: HWND): Boolean;
Begin
//writeln('[TWin32Object.ShowCaret] A');
  Assert(False, Format('Trace:> [TWin32Object.ShowCaret] HWND: 0x%x', [HWnd]));
  Result := Windows.ShowCaret(HWnd);
  Assert(False, Format('Trace:< [TWin32Object.ShowCaret] HWND: 0x%x --> %s', [HWnd, BOOL_TEXT[Result]]));
End;

{------------------------------------------------------------------------------
  Method:   ShowScrollBar
  Params:  Handle - handle of window with scroll bar
           WBar   - scroll bar flag
           BShow  - is the scroll bar visible?
  Returns: If the function succeeds

  Shows or hides the specified scroll bar.
 ------------------------------------------------------------------------------}
Function TWin32Object.ShowScrollBar(Handle: HWND; WBar: Integer; BShow: Boolean): Boolean;
Begin
  Assert(False, 'Trace:[TWin32Object.ShowScrollBar]');
  Result := Windows.ShowScrollBar(Handle, WBar, BShow);
End;

{------------------------------------------------------------------------------
  Method:  StretchBlt
  Params:  DestDC              - The destination device context
           X, Y                - The left/top corner of the destination rectangle
           Width, Height       - The size of the destination rectangle
           SrcDC               - The source device context 
           XSrc, YSrc          - The left/top corner of the source rectangle   
           SrcWidth, SrcHeight - The size of the source rectangle
           Rop                 - The raster operation to be performed 
  Returns: True if succesful

  The StretchBlt function copies a bitmap from a source rectangle into a 
  destination rectangle using the specified raster operation. If needed it 
  resizes the bitmap to fit the dimensions of the destination rectangle. 
  Sizing is done according to the stretching mode currently set in the 
  destination device context.
 ------------------------------------------------------------------------------}
Function TWin32Object.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
Begin
  Assert(True, Format('Trace:> [TWin32Object.StretchBlt] DestDC:0x%x; X:%d, Y:%d, Width:%d, Height:%d; SrcDC:0x%x; XSrc:%d, YSrc:%d, SrcWidth:%d, SrcHeight:%d; Rop:0x%x', [DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop]));
  Result := Windows.StretchBlt(DestDc, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop);
  Assert(True, Format('Trace:< [TWin32Object.StretchBlt] DestDC:0x%x --> %s', [DestDC, BOOL_TEXT[Result]]));
End;

{------------------------------------------------------------------------------
  Method:  StretchMaskBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext 
           XSrc, YSrc:            The left/top corner of the source rectangle   
           SrcWidth, SrcHeight:   The size of the source rectangle
           Mask:                  The handle of a monochrome bitmap
           XMask, YMask:          The left/top corner of the mask rectangle
           Rop:                   The raster operation to be performed 
  Returns: True if succesful

  The StretchMaskBlt function copies a bitmap from a source rectangle into a 
  destination rectangle using the specified mask and raster operations. If
  needed it resizes the bitmap to fit the dimensions of the destination
  rectangle.  Sizing is done according to the stretching mode currently set in
  the destination device context.
 ------------------------------------------------------------------------------}
Function TWin32Object.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; 
Begin
  Result := MaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Mask, XMask, YMask, Rop) And
            StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop);
  
  //Result := False; //Windows.StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Mask, XMask, YMask, Rop);
End;

{------------------------------------------------------------------------------
  Method:  TextOut
  Params: DC    - handle of device context
          X     - x-coordinate of starting position
          Y     - y-coordinate of starting position
          Str   - string
          Count - number of characters in string 
  Returns: If the function succeeds

  Writes a character string at the specified location, using the currently
  selected font.
 ------------------------------------------------------------------------------}
Function TWin32Object.TextOut(DC: HDC; X, Y: Integer; Str: PChar; Count: Integer): Boolean;
Begin
  Result := Windows.TextOut(DC, X, Y, Str, Count);
End;

{------------------------------------------------------------------------------
  Method:  WindowFromPoint
  Params: Point: Specifies the x and y Coords
  Returns: The handle of the window.

  Retrieves the handle of the window that contains the specified point.
 ------------------------------------------------------------------------------}
Function TWin32Object.WindowFromPoint(Point: TPoint): HWND;
Begin
  Result := Windows.WindowFromPoint(Windows.POINT(Point));
End;

Procedure TWin32Object.InitializeCriticalSection(var CritSection: TCriticalSection);
var
  Crit : LPCRITICAL_SECTION;
begin
  { An OS Compatible TCriticalSection needs to be defined}
  If CritSection <> 0 then
    Try
      Crit := LPCRITICAL_SECTION(CritSection);
      Dispose(Crit);
    except
      CritSection := 0;
    end;
  New(Crit);
  Windows.InitializeCriticalSection(Crit^);
  CritSection := Longint(Crit);
end;

Procedure TWin32Object.EnterCriticalSection(var CritSection: TCriticalSection);
var
  Crit,
  tmp : LPCRITICAL_SECTION;
begin
  { An OS Compatible TCriticalSection needs to be defined}
  New(Crit);
  If CritSection <> 0 then
    Try
      Crit^ := LPCRITICAL_SECTION(CritSection)^;
    except
      begin
        CritSection := Longint(Crit);
        exit;
      end;
    end;
  Windows.EnterCriticalSection(Crit^);
  tmp := LPCRITICAL_SECTION(CritSection);
  CritSection := Longint(Crit);
  Dispose(Tmp);
end;

Procedure TWin32Object.LeaveCriticalSection(var CritSection: TCriticalSection);
var
  Crit,
  tmp : LPCRITICAL_SECTION;
begin
  { An OS Compatible TCriticalSection needs to be defined}
  New(Crit);
  If CritSection <> 0 then
    Try
      Crit^ := LPCRITICAL_SECTION(CritSection)^;
    except
      begin
        CritSection := Longint(Crit);
        exit;
      end;
    end;
  Windows.LeaveCriticalSection(Crit^);
  tmp := LPCRITICAL_SECTION(CritSection);
  CritSection := Longint(Crit);
  Dispose(Tmp);
end;

Procedure TWin32Object.DeleteCriticalSection(var CritSection: TCriticalSection);
var
  Crit,
  tmp : LPCRITICAL_SECTION;
begin
  { An OS Compatible TCriticalSection needs to be defined}
  New(Crit);
  If CritSection <> 0 then
    Try
      Crit^ := LPCRITICAL_SECTION(CritSection)^;
    except
      begin
        CritSection := Longint(Crit);
        exit;
      end;
    end;
  Windows.DeleteCriticalSection(Crit^);
  Dispose(Crit);
  tmp := LPCRITICAL_SECTION(CritSection);
  CritSection := 0;
  Dispose(Tmp);
end;

//##apiwiz##eps##   // Do not remove

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.23  2002/11/26 17:48:18  mattias
  patch from Martin Smat for uninitialised DCs

  Revision 1.22  2002/11/23 13:48:49  mattias
  added Timer patch from Vincent Snijders

  Revision 1.21  2002/10/27 20:05:08  lazarus
  AJ : Patch from Martin Smat fixing Pixmap loading on all Color Depths

  Revision 1.20  2002/10/01 10:16:34  lazarus
  MG: removed last clientrectbugfix switches

  Revision 1.19  2002/09/18 17:07:30  lazarus
  MG: added patch from Andrew

  Revision 1.18  2002/09/10 06:49:24  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.17  2002/08/30 12:32:25  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.16  2002/08/29 16:22:02  lazarus
  Make CreatePixmapIndirect work for Win32. Work done by Markus Luedin.

  Revision 1.15  2002/08/28 09:40:52  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.14  2002/08/19 20:34:48  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.13  2002/08/15 15:46:50  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.12  2002/08/13 07:08:25  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.11  2002/08/08 18:05:48  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.10  2002/05/13 22:00:24  lazarus
  Keith: Implemented GetWindowSize

  Revision 1.9  2002/05/10 07:43:49  lazarus
  MG: updated licenses

  Revision 1.8  2002/04/03 01:52:43  lazarus
  Keith: Removed obsolete code, in preperation of a pending TWin32Object cleanup

  Revision 1.7  2002/02/07 08:35:12  lazarus
  Keith: Fixed persistent label captions and a few less noticable things

  Revision 1.6  2002/02/03 06:06:26  lazarus
  Keith: Fixed Win32 compilation problems

  Revision 1.5  2002/01/29 18:55:27  lazarus
  Keith: Fixed duplicate compiler options on Win32

  Revision 1.4  2002/01/21 09:04:30  lazarus
  Keith: Removed redef of Assert

  Revision 1.3  2002/01/21 08:42:06  lazarus
  Keith: Fixed some run-time exceptions for FPC 1.1

  Revision 1.2  2002/01/17 03:17:44  lazarus
  Keith: Fixed TPage creation


}

