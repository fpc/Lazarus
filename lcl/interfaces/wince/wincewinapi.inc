{%MainUnit winceint.pp}

(******************************************************************************
  All Winapi related stuff goes here.
  This file is used by LCLIntf.pas
  if a procedure is platform dependent then it should call:
    WidgetSet.MyDependentProc

  if a procedure insn't platform dependent, it is no part of InterfaseBase has
  to be implementerd here

  !! Keep this alphabetical !!

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
 ******************************************************************************)

{******************************************************************************
  These functions redirect to the platform specific interface object.

  Note:
    the section for not referring WidgetSet is at the end
 ******************************************************************************}
//##apiwiz##sps##   // do not remove

{
------------------------------------------------------------------------------
  Method:  Arc
  Params:  DC - handle to device context
           Left - x-coordinate of bounding rectangle's upper-left corner
           Top - y-coordinate of bounding rectangle's upper-left corner
           Right - x-coordinate of bounding rectangle's lower-right corner
           Bottom - y-coordinate of bounding rectangle's lower-right corner
           Angle1 - first angle
           Angle2 - second angle
  Returns: Whether the call was successful

  Use Arc to draw an elliptically curved line with the current Pen.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.
 ------------------------------------------------------------------------------}
{
function TWinCEWidgetSet.Arc(DC: HDC; Left, Top, Right, Bottom, angle1,
  angle2: Integer): Boolean;
begin
  Result:=inherited Arc(DC, Left, Top, Right, Bottom, angle1, angle2);
end;}
{------------------------------------------------------------------------------
  Method:   AngleChord
  Params:   DC, x1, y1, x2, y2, angle1, angle2
  Returns:  Nothing

  Use AngleChord to draw a filled Chord-shape on the canvas. The angles angle1
  and angle2 are 1/16th of a degree. For example, a full circle equals 5760
  16*360). Positive values of Angle and AngleLength mean counter-clockwise while
  negative values mean clockwise direction. Zero degrees is at the 3'o clock
  position.

------------------------------------------------------------------------------}
{
function TWinCEWidgetSet.AngleChord(DC: HDC; x, y, width, height, angle1,
  angle2: Integer): Boolean;
begin
  Result:=inherited AngleChord(DC, x, y, width, height, angle1, angle2);
end;}

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
function TWinCEWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := Boolean(Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop));
end;

{------------------------------------------------------------------------------
  Method:  BeginPaint
  Params:  Handle        - Handle to window to begin painting in
           PS            - PAINTSTRUCT variable that will receive painting information.
  Returns: A device context for the specified window if succesful otherwise nil

  The BeginPaint function prepares the specified window for painting and fills
  a PAINTSTRUCT structure with information about the painting.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.BeginPaint(Handle: HWND; var PS: TPaintStruct): HDC;
begin
  Result := Windows.BeginPaint(Handle, @PS);
end;

{------------------------------------------------------------------------------
  Method:  CallDefaultWndHandler
  Params:  Sender - object, that sent the message
           Message - a TLMessage
  Returns: -

  Called by TWinControl.DefaultHandler to let the interface call some default
  functions for the message.
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.CallDefaultWndHandler(Sender: TObject; var Message);
var
  Handle: HWND;

  procedure CallWinCEPaintHandler;
  var
    ClientBoundRect: TRect;
    PaintMsg: TLMPaint absolute Message;
    Point: TPoint;
  begin
    // the LCL creates paint messages, with a DC origin set to the client
    // origin of the emitting control. The paint handler of win32 expects the
    // DC origin at the origin of the control.
    // -> move the windoworigin
    ClientBoundRect:= Classes.Rect(0,0,0,0);
    if Sender is TWinControl then
      if not GetClientBounds(Handle,ClientBoundRect) then exit;

    {$ifdef DEBUG_WINDOW_ORG}
    DebugLn(
      Format(':> [TWinCEWidgetSet.CallDefaultWndHandler] Sender=%s DC=%d DX=%d DY=%d',
      [TControl(Sender).Name, PaintMsg.DC,-ClientBoundRect.Left,-ClientBoundRect.Top]));
    {$endif}

    // Using MoveWindowOrgEx here causes the following bugs:
    // http://bugs.freepascal.org/view.php?id=15654
    // * Group Box caption doesn't show
    // * Label x position inside group box is too small
    // * Windows control y position inside group box is too highe
    // See: http://wiki.lazarus.freepascal.org/Windows_CE_Development_Notes#Regressions

    MoveWindowOrgEx(PaintMsg.DC,-ClientBoundRect.Left,-ClientBoundRect.Top);
    try
      {$IFDEF DEBUG_WINCE_LABELS}
      DebugLn('Before CallDefaultWindowProc WindowOrg Temporarely set to 0,0');
      {$ENDIF}
      // call wince paint handler
      CallDefaultWindowProc(Handle, WM_PAINT, PaintMsg.DC, 0);
    finally
      // restore DC origin
      MoveWindowOrgEx(PaintMsg.DC, ClientBoundRect.Left, ClientBoundRect.Top);
    end;
    {$IFDEF DEBUG_WINCE_LABELS}
    DebugLn(Format('After CallDefaultWindowProc WindowOrg: %d,%d',
      [Point.X, Point.Y]));
    {$ENDIF}
  end;

  procedure DrawCheckListBoxItem(CheckListBox: TCheckListBox; Data: PDrawItemStruct);
  const
    ThemeStateMap: array[TCheckBoxState, Boolean] of TThemedButton =
    (
     {cbUnchecked} (tbCheckBoxUncheckedDisabled, tbCheckBoxUncheckedNormal),
     {cbChecked  } (tbCheckBoxCheckedDisabled, tbCheckBoxCheckedNormal),
     {cbGrayed   } (tbCheckBoxMixedDisabled, tbCheckBoxMixedNormal)
    );
  var
    Enabled, Selected: Boolean;
    lgBrush: LOGBRUSH;
    Brush: HBRUSH;
    Rect: Windows.Rect;
    Details: TThemedElementDetails;
    OldColor: COLORREF;
    OldBkMode: Integer;
    WideBuffer: widestring;
  begin
    Selected := (Data^.itemState AND ODS_SELECTED)>0;
    Enabled := CheckListBox.Enabled;

    { fill the background }
    Rect := Data^.rcItem;
    if Selected then
      Windows.FillRect(Data^._HDC, Rect, GetSysColorBrush(COLOR_HIGHLIGHT))
    else
      Windows.FillRect(Data^._HDC, Rect, CheckListBox.Brush.Reference.Handle);

    // draw checkbox
    InflateRect(Rect, -1, -1);
    Rect.Right := Rect.Left + Rect.Bottom - Rect.Top;

    // draw all through ThemeServices. ThemeServices can decide itself hot to perform actual draw
    Details := ThemeServices.GetElementDetails(ThemeStateMap[CheckListBox.State[Data^.ItemID], Enabled]);
    ThemeServices.DrawElement(Data^._HDC, Details, Rect);

    // draw text
    Rect := Windows.Rect(Data^.rcItem);
    Rect.Left := Rect.Left + Rect.Bottom - Rect.Top + 5;

    { VERY IMPORTANT: (see bug 13387)

      Don't suppose anything about the current background color
      or text color in Windows CE. Always set them.
      
      LCLIntf.GetSysColor must be called instead of Windows.GetSysColor
      because the LCLIntf version makes sure that SYS_COLOR_INDEX_FLAG
      is added to the constant.
    }
    OldBkMode := Windows.SetBkMode(Data^._HDC, TRANSPARENT);

    if not Enabled then
      OldColor := Windows.SetTextColor(Data^._HDC, LCLIntf.GetSysColor(COLOR_GRAYTEXT))  // $00BFBFBF
    else if Selected then
      OldColor := Windows.SetTextColor(Data^._HDC, LCLIntf.GetSysColor(COLOR_HIGHLIGHTTEXT))
    else
    begin
      OldColor := CheckListBox.Font.Color;
      if OldColor = clDefault then
        OldColor := CheckListBox.GetDefaultColor(dctFont);
      OldColor := Windows.SetTextColor(Data^._HDC, TColor(ColorToRGB(OldColor)));
    end;

    WideBuffer := UTF8Decode(CheckListBox.Items[Data^.ItemID]);
    Windows.DrawTextW(Data^._HDC, PWideChar(WideBuffer), -1,
       Rect, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);

    // Return to default text and background colors
    Windows.SetTextColor(Data^._HDC, OldColor);
    Windows.SetBkMode(Data^._HDC, OldBkMode);
  end;

begin
  Handle := ObjectToHwnd(Sender);
  case TLMessage(Message).Msg of

    LM_PAINT:
      CallWinCEPaintHandler;

    LM_DRAWITEM:
    begin
      with TLMDrawItems(Message) do
      begin
        if Sender is TCheckListBox then
        begin
          // ItemID not UINT(-1)
          if DrawItemStruct^.ItemID <> DWORD($FFFFFFFF) then
            DrawCheckListBoxItem(TCheckListBox(Sender), DrawItemStruct);
        end;
      end;
    end;

    LM_MEASUREITEM:
    begin
      with TLMMeasureItem(Message).MeasureItemStruct^ do
      begin
        if Sender is TCustomListBox then
        begin
          itemHeight := TCustomListBox(Sender).ItemHeight;
          if TCustomListBox(Sender).Style = lbOwnerDrawVariable then
            TCustomListBox(Sender).MeasureItem(Integer(itemID), integer(itemHeight));
        end;
      end;
    end;

    LM_GETDLGCODE:
    begin
      TLMessage(Message).Result := CallDefaultWindowProc(Handle, WM_GETDLGCODE, WPARAM, 0);
    end;

  else
    if TLMessage(Message).Msg >= WM_USER then
    with TLMessage(Message) do
      Result := CallDefaultWindowProc(Handle, Msg, WParam, LParam);
  end;
end;

{------------------------------------------------------------------------------
  Method:  CallNextHookEx
  Params:  HHk    - handle of the current hook
           NCode  - Hook code
           WParam - Word parameter
           LParam - Long-integer parameter
  Returns: The handle of the next hook procedure

  Calls the next procedure in the hook chain
 ------------------------------------------------------------------------------}
{function TWinCEWidgetSet.CallNextHookEx(hHk: HHOOK; ncode: Integer;
  wParam: WParam; lParam: LParam): Integer;
begin
  Result:=inherited CallNextHookEx(hHk, ncode, wParam, lParam);
end;}

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
function TWinCEWidgetSet.CallWindowProc(LPPrevWndFunc: TFarProc; Handle: HWND;
    Msg: UINT; WParam: WParam; LParam: LParam): Integer;
begin
  Result := Windows.CallWindowProc(WNDPROC(LPPrevWndFunc), Handle, Msg, Windows.WPARAM(WParam), Windows.LPARAM(LParam));
end;

{------------------------------------------------------------------------------
  Method:  ClientToScreen
  Params:  Handle - Handle of window
           P      - container that contains coordinates
  Returns: Whether the call was successful

  Converts client coordinates to screen coordinates
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
var
  ORect: TRect;
begin
  Result := Boolean(Windows.ClientToScreen(Handle, @P));
  if not Result then exit;
  Result := GetLCLClientBoundsOffset(Handle, ORect);
  if not Result then exit;
  inc(P.X, ORect.Left);
  inc(P.Y, ORect.Top);
end;

{------------------------------------------------------------------------------
  Method:  ClipboardFormatToMimeType
  Params:  FormatID - a registered format identifier (0 is invalid)
  Returns: the corresponding mime type as string
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
var
  FormatLength: Integer;
  WideStr: widestring;
begin
  SetLength(WideStr, 1000);
  FormatLength:= Windows.GetClipboardFormatNameW(FormatID, PWideChar(WideStr), 1000);
  SetLength(WideStr, FormatLength);
  Result := UTF16ToUTF8(WideStr);
end;

{------------------------------------------------------------------------------
  Method:  ClipboardGetData
  Params:  ClipboardType - clipboard type
           FormatID      - a registered format identifier (0 is invalid)
           Stream        - if format is available, it will be appended to this
                           stream
  Returns: true on success
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
var
  DataHandle: HGLOBAL;
  Data: pointer;
  Size: integer;
  Bitmap: TBitmap;
  BufferStream: TMemoryStream;
  BufferWideString: widestring;
  BufferString: ansistring;

  function ReadClipboardToStream(DestStream: TStream): Boolean;
  begin
    Result := false;

    DataHandle := Windows.GetClipboardData(FormatID);
    if DataHandle<>HWND(0) then
    begin
      Size := Windows.GlobalSize(DataHandle);
      if Size>0 then
      begin
        Data := Windows.GlobalLock(DataHandle);
        try
          DestStream.Write(Data^, Size);
        finally
          Windows.GlobalUnlock(DataHandle);
        end;
        Result := true;
      end;
    end;
  end;

begin
  //DebugLn('TWin32WidgetSet.ClipboardGetData - Start');
  Result := false;
  if (FormatID=0) or (Stream=nil) or
    not Windows.IsClipboardFormatAvailable(FormatID) then exit;

  if Windows.OpenClipboard(Windows.HWND(nil)) then
    try
      case FormatID of
      Windows.CF_BITMAP:
        begin
          Bitmap:= TBitmap.Create;
          Bitmap.TransparentColor := clNone;
          DataHandle := Windows.GetClipboardData(FormatID);
          Bitmap.SetHandles(DataHandle, 0);
          Bitmap.SaveToStream(Stream);
          Bitmap.Free;
          Result := true;
        end;
      { In the case of unicode text, it's necessary to
        convert it from UTF-16 to UTF-8 }
      Windows.CF_UNICODETEXT, Windows.CF_TEXT:
        begin
          BufferStream := TMemoryStream.Create;
          try
            Result := ReadClipboardToStream(BufferStream);

            if Size>0 then
            begin
              BufferStream.Position := 0;
              if FormatID=Windows.CF_UNICODETEXT then
              begin;
                SetLength(BufferWideString, Size div 2);
                BufferStream.Read(BufferWideString[1], Size);
                BufferString := UTF16ToUTF8(BufferWideString);
              end
              else
              begin
                SetLength(BufferString, Size);
                BufferStream.Read(BufferString[1], Size);
                BufferString := AnsiToUtf8(BufferString);
              end;
              Stream.Write(BufferString[1], Length(BufferString));
            end;
          finally
            BufferStream.Free;
          end;
        end
      else
        Result := ReadClipboardToStream(Stream)
      end;
    finally
      Windows.CloseClipboard;
    end;
  //DebugLn('TWin32WidgetSet.ClipboardGetData - Exit');
end;

{------------------------------------------------------------------------------
  Method:  ClipboardGetFormats
  Params:  ClipboardType - the type of clipboard operation (GTK only; ignored here)
           Count         - the number of clipboard formats
           List          - Pointer to an array of supported formats
                           (you must free it yourself)
  Returns: true on success
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
var
  FormatID: UINT;
  c: integer;
begin
  Result := false;
  List := nil;
//  {$IFDEF VerboseWin32Clipbrd}
//  debugln('TWin32WidgetSet.ClipboardGetData ');
//  {$ENDIF}
  if not Windows.OpenClipboard(HWND(AppHandle)) then begin
//    {$IFDEF VerboseWin32Clipbrd}
//    debugln('TWin32WidgetSet.ClipboardGetData OpenClipboard failed');
//    {$ENDIF}
    exit;
  end;
  Count := CountClipboardFormats;
  GetMem(List, Count * SizeOf(TClipboardFormat));
  try
    c := 0;
    FormatID := 0;
    repeat
      FormatID := EnumClipboardFormats(FormatID);
      if (FormatID<>0) then begin
        List[c] := FormatID;
        inc(c);
      end;
    until (c>=Count) or (FormatID=0);
    Count := c;
  finally
    Windows.CloseClipboard;
  end;

  Result := true;
end;

{------------------------------------------------------------------------------
  Method:  ClipboardGetOwnerShip
  Params:  ClipboardType - Type of clipboard, the win32 interface only handles
                           ctClipBoard
           OnRequestProc - TClipboardRequestEvent is defined in LCLType.pp
                           if OnRequestProc is nil the onwership will end.
           FormatCount - number of formats
           Formats - array of TClipboardFormat. The supported formats the owner
                      provides.

  Returns: true on success

  Sets the supported formats and requests ownership for the clipboard.
  The OnRequestProc is used to get the data from the LCL and to put it on the
  clipboard.
  if someone else requests the ownership, the OnRequestProc will be executed
  with the invalid FormatID 0 to notify the old owner of the lost of ownership.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: integer;
  Formats: PClipboardFormat): boolean;

  procedure WriteStreamToClipBoard(FormatID: integer; SourceStream: TStream);
  var
    DataHandle : THandle;//Windows.HGLOBAL;
    DataPtr: pointer;
  begin
    DataHandle := Windows.GlobalAlloc(Windows.GMEM_MOVEABLE, SourceStream.Size);
    if (DataHandle=HWND(0)) then begin
      debugln('TWin32WidgetSet.ClipboardGetOwnerShip DataHandle=',dbgs(DataHandle),' DataSize=',dbgs(SourceStream.Size));
      Result := false;
      exit;
    end;
    DataPtr := GlobalLock(DataHandle);
    try
      SourceStream.Read(DataPtr^, SourceStream.Size);
    finally
      Windows.GlobalUnlock(DataHandle);
    end;
    Windows.SetClipboardData(FormatID, DataHandle);
  end;

  procedure PutOnClipBoard(FormatID: integer);
  var
    DataStream, BufferStream: TStream;
    Bitmap: TBitmap;
    BufferWideString: widestring;
    BufferString: ansistring;
    ScreenDC, MemDC: HDC;
    OldBitmap, NewBitmap, Mask: HBitmap;
  begin
    DataStream := TMemoryStream.Create;
    BufferStream := TMemoryStream.Create;
    try
      OnClipBoardRequest(FormatID, DataStream);
      DataStream.Position:=0;
      case FormatID of
      CF_BITMAP:
        begin
          Bitmap:= TBitmap.Create;
          try
            Bitmap.LoadFromStream(DataStream);
            ScreenDC := GetDC(0);
            try
              MemDC := Windows.CreateCompatibleDC(ScreenDC);
              NewBitmap := Windows.CreateCompatibleBitmap(ScreenDC, Bitmap.Width, Bitmap.Height);
              OldBitmap := Windows.SelectObject(MemDC, NewBitmap);
              if Bitmap.Masked then
                Mask := Bitmap.MaskHandle
              else
                Mask := 0;
              StretchMaskBlt(MemDC, 0, 0, Bitmap.Width, Bitmap.Height,
                Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height,
                Mask, 0, 0, SRCCOPY);
              Windows.SelectObject(MemDC, OldBitmap);
              Windows.DeleteDC(MemDC);
              Windows.SetClipboardData(FormatID, NewBitmap);
              // GDI objects count does not vary if we delete it or not
              // DeleteObject(NewBitmap);
            finally
              ReleaseDC(0, ScreenDC);
            end;
          finally
            Bitmap.Free;
          end;
        end;
      Windows.CF_UNICODETEXT, Windows.CF_TEXT:
        // CF_UNICODETEXT is used by UnicodeEnabledOS, CF_TEXT by others
        // we need to convert it from UTF8 to UTF16 or Ansi
        begin
          if DataStream.Size>0 then begin
            SetLength(BufferString, DataStream.Size);
            DataStream.Read(BufferString[1], DataStream.Size);
            if FormatID=Windows.CF_UNICODETEXT then
            begin
              BufferWideString := UTF8ToUTF16(BufferString);
              if BufferWideString<>'' then  // bufferstring may contain invalid UTF8
                BufferStream.Write(BufferWideString[1], Length(BufferWideString) * 2);
            end
            else
            begin
              BufferString := Utf8ToAnsi(BufferString);
              if BufferString<>'' then  // original string may contain invalid UTF8
                BufferStream.Write(BufferString[1], Length(BufferString));
            end;
            BufferStream.Position := 0;
          end;
          WriteStreamToClipBoard(FormatID, BufferStream);
        end
      else
        begin
          WriteStreamToClipBoard(FormatID, DataStream);
        end;
      end;
    finally
      DataStream.Free;
      BufferStream.Free;
    end;
  end;

var
  I: Integer;
begin
  Result := false;

//  {$IFDEF VerboseWin32Clipbrd}
//  debugln('TWin32WidgetSet.ClipboardGetOwnerShip START FormatCount=',dbgs(FormatCount),' OnRequestProc=',dbgs(OnRequestProc=nil));
//  {$ENDIF}

  if ClipboardType<>ctClipBoard then begin
  { the win32 interface does not support this kind of clipboard,
    so the application can have the ownership at any time.
    The TClipboard in clipbrd.pp has an internal cache system, so that an
    application can use all types of clipboards even if the underlying
    platform does not support it.
    Of course this will only be a local clipboard, invisible to other
    applications. }
//    {$IFDEF VerboseWin32Clipbrd}
//    debugln('TWin32WidgetSet.ClipboardGetOwnerShip unsupported ClipboardType under win32');
//    {$ENDIF}
    Result := true;
    exit;
  end;

  if (FormatCount=0) or (OnRequestProc=nil) then begin
  { The LCL indicates it doesn't have the clipboard data anymore
    and the interface can't use the OnRequestProc anymore.}
//    {$IFDEF VerboseWin32Clipbrd}
//    debugln('TWin32WidgetSet.ClipboardGetOwnerShip clearing OnClipBoardRequest');
//    {$ENDIF}
    OnClipBoardRequest := nil;
    Result := true;
  end else begin
  { clear OnClipBoardRequest to prevent destroying the LCL clipboard,
    when emptying the clipboard}
    OnClipBoardRequest := nil;
    if not Windows.OpenClipboard(FAppHandle) then begin
//      {$IFDEF VerboseWin32Clipbrd}
//      debugln('TWin32WidgetSet.ClipboardGetOwnerShip A OpenClipboard failed');
//      {$ENDIF}
      exit;
    end;
    try
      if not Windows.EmptyClipboard then begin
//        debugln('TWin32WidgetSet.ClipboardGetOwnerShip A EmptyClipboard failed');
        exit;
      end;
      Result := true;
      OnClipBoardRequest := OnRequestProc;
      for I := 0 To FormatCount-1 do begin
//        {$IFDEF VerboseWin32Clipbrd}
//        debugln('TWin32WidgetSet.ClipboardGetOwnerShip A Formats['+dbgs(i)+']=',dbgs(Formats[i]));
//        {$ENDIF}
        PutOnClipBoard(Formats[i]);
      end;
    finally
      Windows.CloseClipboard();
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  ClipboardRegisterFormat
  Params:  AMimeType - a string (usually a MIME type) identifying a new format
                       type to register
  Returns: the registered Format identifier (TClipboardFormat)
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
var
  WideStr: widestring;
begin
  if AMimeType=PredefinedClipboardMimeTypes[pcfText] then
    Result := Windows.CF_UNICODETEXT
  else if (AMimeType=PredefinedClipboardMimeTypes[pcfBitmap]) then
    Result := Windows.CF_BITMAP
  else
  begin
    WideStr := UTF8ToUTF16(AMimeType);
    Result := Windows.RegisterClipboardFormatW(PWideChar(WideStr));
  end;
end;

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
function TWinCEWidgetSet.CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint;
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
function TWinCEWidgetSet.CreateBitmap(Width, Height: Integer; Planes, BitCount: LongInt; BitmapBits: Pointer): HBITMAP;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.CreateBitmap] Width: %d, Height: %d, Planes: %d, BitCount: %d, BitmapBits: 0x%x', [Width, Height, Planes, BitCount, Longint(BitmapBits)]));
  Result := Windows.CreateBitmap(Width, Height, Planes, BitCount, BitmapBits);
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.CreateBitmap] --> 0x%x', [Integer(Result)]));
end;

function TWinCEWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
var
  LB: Windows.LogBrush;
begin
  LB.lbStyle := LogBrush.lbStyle;
  LB.lbColor := ColorToRGB(LogBrush.lbColor);
  LB.lbHatch := LogBrush.lbHatch;
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.CreateBrushIndirect]  Style: %d, Color: %8x', [lb.lbStyle, lb.lbColor]));

  case lb.lbStyle of
    BS_NULL: Result := Windows.GetStockObject(NULL_BRUSH);
    BS_DIBPATTERNPT: Result := CreateDIBPatternBrushPt(pointer(lb.lbHatch), lb.lbColor);
    BS_PATTERN: Result := CreatePatternBrush(lb.lbHatch);
  else { lb.lbStyle = BS_SOLID }
    Result := Windows.CreateSolidBrush(LB.lbColor)
  end

  //DebugLn(Format('Trace:< [TWinCEWidgetSet.CreateBrushIndirect] Got --> %x', [Result]));
end;

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
function TWinCEWidgetSet.CreateCaret(Handle: HWND; Bitmap: HBITMAP; Width, Height: Integer): Boolean;
begin
{$ifdef DEBUG_CARET}
  DebugLn('[CreateCaret] for window ', IntToHex(Handle, 8));
{$endif}
  Result := Boolean(Windows.CreateCaret(Handle, Bitmap, Width, Height));
  //DebugLn('Trace:TODO: [TWinCEWidgetSet.CreateCaret] Finish');
end;

{------------------------------------------------------------------------------
  Method:  CreateCompatibleBitmap
  Params: DC     - handle to device context
          Width  - width of bitmap, in pixels
          Height - height of bitmap, in pixels
  Returns: a handle to the bitmap

  Creates a bitmap compatible with the specified device context.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.CreateCompatibleBitmap] DC: 0x%x, W: %d, H: %d', [DC, Width, Height]));
  Result := Windows.CreateCompatibleBitmap(DC, Width, Height);
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.CreateCompatibleBitmap] DC: 0x%x --> 0x%x', [DC, Result]));
end;


{------------------------------------------------------------------------------
  Method:  CreateCompatibleDC
  Params:  DC - handle to memory device context
  Returns: handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  Result := Windows.CreateCompatibleDC(DC);
  //DebugLn(Format('Trace:[TWinCEWidgetSet.CreateCompatibleDC] DC: 0x%x --> 0x%x', [Integer(DC), Integer(Result)]));
end;

{------------------------------------------------------------------------------
  Method:  CreateFontIndirect
  Params:  LogFont - logical font record
  Returns: a handle to a logical font

  Creates a logical font that has the characteristics specified in the
  specified record.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
var
  TempLogFont: TLogFont;
begin
  TempLogFont := LogFont;
  if String(TempLogFont.lfFaceName) = DefFontData.Name then
  begin
    Move(FMetrics.lfMessageFont.lfFaceName, TempLogFont.lfFaceName, LF_FACESIZE);
    if TempLogFont.lfHeight = 0 then
      TempLogFont.lfHeight := FMetrics.lfMessageFont.lfHeight;
  end;
  Result := Windows.CreateFontIndirect(@TempLogFont);
end;

{------------------------------------------------------------------------------
  Method:  CreateIconIndirect
  Params:  IconInfo - pointer to Icon Information record
  Returns: handle to a created icon / cursor

  Creates an icon / cursor by color and mask bitmaps and other indo.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.CreateIconIndirect(IconInfo: PIconInfo): HICON;
begin
  Result := Windows.CreateIconIndirect(IconInfo);
end;

{function TWinCEWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  Result:=inherited CreateFontIndirectEx(LogFont, LongFontName);
end;

function TWinCEWidgetSet.CreatePalette(const LogPalette: TLogPalette
  ): HPALETTE;
begin
  Result:=inherited CreatePalette(LogPalette);
end;}

function TWinCEWidgetSet.CreatePatternBrush(ABitmap: HBITMAP): HBRUSH;
begin
  Result := Windows.CreatePatternBrush(ABitmap);
end;

{------------------------------------------------------------------------------
  Method:  CreatePenIndirect
  Params:  LogPen - record that defines the style, width, and color of a pen
  Returns: a handle that identifies a logical cosmetic pen

  Creates a logical cosmetic pen that has the style, width, and color specified
  in a record.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
var
  LP: TLogPen;
begin
  //DebugLn('Trace:[TWinCEWidgetSet.CreatePenIndirect]');
  LP := LogPen;
  LP.lopnColor := ColorToRGB(LP.lopnColor);
  LP.lopnStyle := LP.lopnStyle and PS_STYLE_MASK;
  Result := Windows.CreatePenIndirect(Windows.LOGPEN(LP));
end;

{------------------------------------------------------------------------------
  Method:  CreatePolygonRgn
  Params:  Points, NumPts, FillMode
  Returns: the handle to the region

  Creates a Polygon, a closed many-sided shaped region. The Points parameter is
  an array of points that give the vertices of the polygon. FillMode=Winding
  determines what points are going to be included in the region. When Winding
  is True, points are selected by using the Winding fill algorithm. When Winding
  is False, points are selected by using using the even-odd (alternative) fill
  algorithm. NumPts indicates the number of points to use.
  The first point is always connected to the last point.
 ------------------------------------------------------------------------------}
{function TWinCEWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
begin
  Result:=inherited CreatePolygonRgn(Points, NumPts, FillMode);
end;}

{------------------------------------------------------------------------------
  Method:  CreateRectRgn
  Params:  X1 - x-coordinate of region's upper-left corner
           Y1 - y-coordinate of region's upper-left corner
           X2 - x-coordinate of region's lower-right corner
           Y2 - y-coordinate of region's lower-right corner
  Returns: the handle to the region

  Creates a rectangular region.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  Result := Windows.CreateRectRgn(X1, Y1, X2, Y2);
end;


{------------------------------------------------------------------------------
  Method:  DeleteDC
  Params:  HDC - handle to device context
  Returns: if the function succeeds.

  Deletes the specified device context (DC).
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.DeleteDC(HDC: HDC): Boolean;
begin
  Result := Boolean(Windows.DeleteDC(HDC));
end;

{------------------------------------------------------------------------------
  Method:  DeleteObject
  Params:  GDIObject - handle to graphic object
  Returns: if the function succeeds.

  Deletes a graphic object, freeing all system resources associated with the
  object.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
  { Find out if we want to release internal GDI object }
  Result := Boolean(Windows.DeleteObject(GDIObject));
end;
{------------------------------------------------------------------------------
  Method:  DestroyCaret
  Params:  Handle - handle to the window with a caret (on Windows, there is
           only one, global caret, so this parameter is ignored)
  Returns: if the function succeeds

  Destroys the caret but doesn't free the bitmap.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.DestroyCaret(Handle: HWND): Boolean;
begin
  Result := Boolean(Windows.DestroyCaret);
end;

{------------------------------------------------------------------------------
  Method:  DestroyCursor
  Params:  Handle - handle to the cursor object
  Returns: if the function succeeds

  Destroys the cursor
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.DestroyCursor(Handle: hCursor): Boolean;
begin
  Result := False;
  //Result := Boolean(Windows.DestroyCursor(Handle));
end;

function TWinCEWidgetSet.DestroyIcon(Handle: HICON): Boolean;
begin
  Result := Windows.DestroyIcon(Handle);
end;

{------------------------------------------------------------------------------
  Method:  DrawFrameControl
  Params: DC     - handle to device context
          Rect   - bounding rectangle
          UType  - frame-control type
          UState - frame-control state
  Returns: if the function succeeds

  Draws a frame control of the specified type and style.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.DrawFrameControl(DC: HDC; const Rect: TRect; uType, uState: Cardinal): Boolean;
var
  OldBrush, OldPen: HGDIOBJ;
begin
  // Some WinCE elements have a very bad looking native look,
  // which causes trouble with controls which are painted
  // using this routine, such as TSpeedButton
  // So here we override some native frames
  if (uType = DFC_BUTTON) then
  begin
    if (uState = DFCS_BUTTONPUSH or DFCS_PUSHED) then
    begin
      // We draw the pushed button as a gray background and a black frame
      // The native look is a black background, which is ugly and doesn't
      // allow the text to be seen.
      OldBrush := Windows.SelectObject(DC, Windows.GetStockObject(GRAY_BRUSH));
      OldPen := Windows.SelectObject(DC, Windows.GetStockObject(BLACK_PEN));
      Windows.Rectangle(DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      Windows.SelectObject(DC, OldBrush);
      Windows.SelectObject(DC, OldPen);
      Exit(True);
    end;
    // implement DFCS_INACTIVE too?
  end;

  // Default native look for other cases
  Result := Boolean(Windows.DrawFrameControl(DC, @Rect, UType, UState));
end;


{------------------------------------------------------------------------------
  Method:  DrawEdge
  Params: DC       - handle to device context
          Rect     - rectangle coordinates
          Edge     - type of inner and outer edge to draw
          GrfFlags - type of border
  Returns: if the function succeeds.

  Draws one or more edges of a rectangle, not including the
  right and bottom edge.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.DrawEdge(DC: HDC; var ARect: TRect; Edge: Cardinal; GrfFlags: Cardinal): Boolean;
begin
  //DebugLn(Format('trace:> [TWinCEWidgetSet.DrawEdge] DC:0x%x, Rect = %d,%d,%d,%d', [DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom]));
  Result := Boolean(Windows.DrawEdge(DC, @ARect, edge, grfFlags));
  //DebugLn(Format('trace:< [TWinCEWidgetSet.DrawEdge] DC:0x%x, Rect = %d,%d,%d,%d', [DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom]));
end;

{------------------------------------------------------------------------------
  Method:  DrawText
  Params:  DC, Str, Count, Rect, Flags
  Returns: if the string was drawn, or CalcRect run

 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;
var
  s: AnsiString;
  w: WideString;
  {$IFDEF DEBUG_WINCE_LABELS}
  Point: TPoint;
  {$ENDIF}
begin
  {$IFDEF DEBUG_WINCE_LABELS}
  LCLIntf.GetWindowOrgEx(DC, @Point);
  DebugLn(
    Format('trace:> [TWinCEWidgetSet.DrawText] DC:0x%x, Str:''%s'','
    + 'Count: %d, Rect = %d,%d,%d,%d, Flags:%d WindowOrg: %d:%d',
    [DC, Str, Count, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Flags,
    Point.X, Point.Y]));
  {$ENDIF}

  // use temp buffer, if count is set, there might be no null terminator
  if count = -1 then
    s := str
  else
  begin
    SetLength(s, count);
    move(str^, s[1], count);
  end;
  // the length of utf8 vs Wide/Ansi the strings differ, so recalc.

  // TODO: use the real number of chars (and not the lenght)
  W := Utf8Decode(S);
  Result := Windows.DrawTextW(DC, PWideChar(W), Length(W), @Rect, Flags);

  {$IFDEF DEBUG_WINCE_LABELS}
  DebugLn(
    Format('trace:< [TWinCEWidgetSet.DrawText] DC:0x%x, Str:''%s'','
    + 'Count: %d, Rect = %d,%d,%d,%d, Flags:%d',
    [DC, Str, Count, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Flags]));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:   Ellipse
  Params:
            DC - handle to device context
            X1 - x-coord. of bounding rectangle's upper-left corner
            Y1 - y-coord. of bounding rectangle's upper-left corner
            X2 - x-coord. of bounding rectangle's lower-right corner
            Y2 - y-coord. of bounding rectangle's lower-right corner
  Returns:  if the function succeeds

  Use Ellipse to draw a filled circle or ellipse.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
begin
  Result := Boolean(Windows.Ellipse(DC, X1, Y1, X2, Y2));
end;
{------------------------------------------------------------------------------
  Method:  EnableScrollBar
  Params:  Wnd      - handle to window or scroll bar
           WSBFlags - scroll bar type flag
           WArrows  - scroll bar arrow flag
  Returns: Nothing


 ------------------------------------------------------------------------------}
{function TWinCEWidgetSet.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal
  ): Boolean;
begin
  Result:=inherited EnableScrollBar(Wnd, wSBflags, wArrows);
end;
}
{------------------------------------------------------------------------------
  Method:  EnableWindow
  Params: HWnd    - handle to window
          BEnable -  whether to enable the window
  Returns: if the window was previously disabled

  Enables or disables mouse and keyboard input to the specified window or
  control.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  //DebugLn(Format('Trace:[TWinCEWidgetSet.EnableWindow] HWnd: 0x%x, BEnable: %s', [HWnd, BoolToStr(BEnable)]));
  Result := Boolean(Windows.EnableWindow(HWnd, BEnable));
end;

{------------------------------------------------------------------------------
  Method:  EndPaint
  Params:  Handle        - Handle to window
           PS            - PAINTSTRUCT variable with painting information
  Returns: always nonzero.

  The EndPaint function marks the end of painting in the specified window.
  This function is required for each call to the BeginPaint function, but only
  after painting is complete.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
begin
  Result := Integer(Windows.EndPaint(Handle, @PS));
end;

function TWinCEWidgetSet.EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool;
begin
  Result := WinCEDef.EnumDisplayMonitors(hdc, lprcClip, lpfnEnum, dwData);
end;

{function TWinCEWidgetSet.EnumFontFamilies(DC: HDC; Family: Pchar;
  EnumFontFamProc: FontEnumProc; LParam: Lparam): longint;
begin
  Result:=inherited EnumFontFamilies(DC, Family, EnumFontFamProc, LParam);
end;

function TWinCEWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint;
begin
  Result:=inherited EnumFontFamiliesEx(DC, lpLogFont, Callback, Lparam, Flags);
end;}
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
function TWinCEWidgetSet.ExcludeClipRect(dc: hdc;  Left, Top, Right, Bottom: Integer): Integer;
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
  Returns: if the string was drawn.

  Draws a character string by using the currently selected font.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  s: AnsiString;
  w: WideString;
begin
  //DebugLn(Format('trace:> [TWinCEWidgetSet.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));

  // use temp buffer, if count is set, there might be no null terminator
  if count = -1 then
   s := str
  else
  begin
    SetLength(s, count);
    move(str^, s[1], count);
  end;
  // the length of utf8 vs Wide/Ansi the strings differ, so recalc.

  // TODO: use the real number of chars (and not the lenght)
  W := Utf8Decode(S);
  Result := Windows.ExtTextOutW(DC, X, Y, Options, LPRECT(Rect), PWideChar(W), Length(W), Dx);
  //DebugLn(Format('trace:< [TWinCEWidgetSet.ExtTextOut] DC:0x%x, X:%d, Y:%d, Options:%d, Str:''%s'', Count: %d', [DC, X, Y, Options, Str, Count]));
end;

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
function TWinCEWidgetSet.ExtSelectClipRGN(dc: hdc; rgn: hrgn; Mode: Longint
  ): Integer;
begin
  Result:=inherited ExtSelectClipRGN(dc, rgn, Mode);
end;

{------------------------------------------------------------------------------
  Method:  FillRect
  Params:  DC    - handle to device context
           Rect  - record with rectangle
           Brush - handle to brush
  Returns: if the function succeeds

  The FillRect function fills a rectangle by using the specified brush.
  This function includes the left and top borders, but excludes the right and
  bottom borders of the rectangle.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
var
  R: TRect;
begin
  R := Rect;
  //DebugLn(Format('trace:> [TWinCEWidgetSet.FillRect] DC: 0x%x; Rect: ((%d,%d)(%d,%d)); Brush: %x', [Integer(DC), R.left, R.top, R.right, R.bottom, brush]));
  Result := Boolean(Windows.FillRect(DC, Windows.RECT(r), Brush));
  //DebugLn(Format('trace:< [TWinCEWidgetSet.FillRect] DC: 0x%x; Rect: ((%d,%d)(%d,%d)); Brush: %x', [Integer(DC), R.left, R.top, R.right, R.bottom, brush]));
end;

{------------------------------------------------------------------------------
  Method: Frame3D
  Params: DC         - handle to device context
          Rect       - bounding rectangle
          FrameWidth - width of the frame (ignored on wince(?))
          Style      - frame style
  Returns: Whether the function was successful

  Draws a 3D border in native style.
  NOTE: This function is mapped to DrawEdge on Windows.
------------------------------------------------------------------------------}
function TWinCEWidgetSet.Frame3D(DC: HDC; var ARect: TRect;
  const FrameWidth: Integer; const Style: TBevelCut): Boolean;
const
  Edge: array[TBevelCut] of Integer =
  (
{bvNone   } 0,
{bvLowered} BDR_SUNKENOUTER,
{bvRaised } BDR_RAISEDINNER,
{bvSpace  } 0
  );
var
  I: Integer;
begin
  for I := 0 to FrameWidth - 1 do
    Result := Boolean(DrawEdge(DC, ARect, Edge[Style], BF_RECT or BF_ADJUST));
end;

function TWinCEWidgetSet.FrameRect(DC: HDC; const ARect: TRect;
  hBr: HBRUSH) : integer;
var
  res: Boolean;
  OldBrush: HBRUSH;
begin
  res := True;
  if (ARect.Top <= ARect.Bottom) and (ARect.Left <= ARect.Right) then
  begin
    OldBrush := Windows.SelectObject(DC, hBr);
    res := res and FillRect(DC, Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Top+1), hBr);
    res := res and FillRect(DC, Rect(ARect.Left, ARect.Bottom-1, ARect.Right, ARect.Bottom), hBr);
    res := res and FillRect(DC, Rect(ARect.Right-1, ARect.Top, ARect.Right, ARect.Bottom), hBr);
    res := res and FillRect(DC, Rect(ARect.Left, ARect.Top, ARect.Left+1, ARect.Bottom), hBr);
    Windows.SelectObject(DC, OldBrush);
  end;
  if res then
    Result := 1
  else
    Result := 0;
end;

{------------------------------------------------------------------------------
  Method:  GetActiveWindow
  Params: none
  Returns: The handle to the active window

  Retrieves the window handle to the active window associated with the thread
  that calls the function.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetActiveWindow: HWND;
begin
  Result := Windows.GetActiveWindow;
end;

{function TWinCEWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;
  Bits: Pointer): Longint;
begin
  Result:=inherited GetBitmapBits(Bitmap, Count, Bits);
end;

function TWinCEWidgetSet.GetBitmapRawImageDescription(Bitmap: HBITMAP;
  Desc: PRawImageDescription): boolean;
begin
  Result:=inherited GetBitmapRawImageDescription(Bitmap, Desc);
end;}
{------------------------------------------------------------------------------
  Method:  GetCapture
  Params:  none
  Returns: the handle of the capture window

  Retrieves the handle of the window (if any) that has captured the mouse.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetCapture: HWND;
begin
  Result := Windows.GetCapture;
end;

{------------------------------------------------------------------------------
  Method:  GetCaretPos
  Params:  LPPoint - record to receive coordinates
  Returns: if the function succeeds

  Gets the caret's position, in client coordinates.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result := Boolean(Windows.GetCaretPos(@LPPoint));
end;

{function TWinCEWidgetSet.GetCaretRespondToFocus(handle: HWND;
  var ShowHideOnFocus: boolean): Boolean;
begin
  Result:=inherited GetCaretRespondToFocus(handle, ShowHideOnFocus);
end;

function TWinCEWidgetSet.GetCharABCWidths(DC: HDC; p2, p3: UINT;
  const ABCStructs): Boolean;
begin
  Result:=inherited GetCharABCWidths(DC, p2, p3, ABCStructs);
end;
}
{------------------------------------------------------------------------------
  Method:  GetClientBounds
  Params: Handle - handle of window
          Rect   - record for client coordinates
  Returns: if the function succeeds

  Retrieves the coordinates of a window's client area.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetClientBounds(Handle: HWND; var Rect: TRect): Boolean;
var
  ARect: TRect;
begin
  Result := Boolean(Windows.GetClientRect(Handle, @Rect));
  if not Result then exit;
  if not GetLCLClientBoundsOffset(Handle, ARect) then exit;
  Inc(Rect.Left, ARect.Left);
  Inc(Rect.Top, ARect.Top);
  Inc(Rect.Right, ARect.Right);
  Inc(Rect.Bottom, ARect.Bottom);
end;

{------------------------------------------------------------------------------
  Method:  GetClientRect
  Params: Handle - handle of window
          Rect   - record for client coordinates
  Returns: if the function succeeds

  Retrieves the dimension of a window's client area.
  Left and Top are always 0,0
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetClientRect(Handle: HWND; var Rect: TRect): Boolean;
begin
  Result := GetClientBounds(Handle, Rect);
  OffsetRect(Rect, -Rect.Left, -Rect.Top);
end;

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
function TWinCEWidgetSet.GetClipBox(DC : hDC; lpRect : PRect) : Longint;
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
function TWinCEWidgetSet.GetClipRGN(DC : hDC; RGN : hRGN) : Integer;
begin
  Result := Windows.GetClipRGN(DC, RGN);
end;

{------------------------------------------------------------------------------
  Method: GetCurrentObject
  Params:
    DC - A handle to the DC
    uObjectType - The object type to be queried
  Returns: If the function succeeds, the return value is a handle to the specified object.
    If the function fails, the return value is NULL.
 ------------------------------------------------------------------------------}

function TWinCEWidgetSet.GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ;
begin
  Result := Windows.GetCurrentObject(DC, uObjectType);
end;

{function TWinCEWidgetSet.GetCmdLineParamDescForInterface: string;
begin
  Result:=inherited GetCmdLineParamDescForInterface;
end;}

function TWinCEWidgetSet.GetCursorPos(var lpPoint: TPoint): Boolean;
begin
  Result := Boolean(Windows.GetCursorPos(@LPPoint));
end;

{------------------------------------------------------------------------------
  Method:  GetDC
  Params:  HWND - handle of window
  Returns: value identifying the device context for the given window's client
           area

  Retrieves a handle of a display device context (DC) for the client area of
  the specified window.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetDC(HWnd: HWND): HDC;
var
  ORect: TRect;
  {$ifdef DEBUG_WINDOW_ORG}
  lControl: TControl;
  Point: TPoint;
  {$endif}
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.GetDC] HWND: 0x%x', [HWnd]));
  Result := Windows.GetDC(HWnd);
  if (Result<>0) and (HWnd<>0) and GetLCLClientBoundsOffset(HWnd, ORect) then
  begin
    {$ifdef DEBUG_WINDOW_ORG}
    lControl := TControl(LCLIntf.GetLCLOwnerObject(HWnd));
    LCLIntf.GetWindowOrgEx(Result, @Point);
    DebugLn(
      Format(':> [TWinCEWidgetSet.GetDC] Name=%s DC=%s Moving WindowOrg From %d,%d By %d,%d',
      [lControl.Name, IntToHex(Result, 8), Point.X, Point.Y, ORect.Left, ORect.Top]));
    {$endif}

    MoveWindowOrgEx(Result, ORect.Left, ORect.Top);
  end;
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.GetDC] Got 0x%x', [Result]));
end;


{function TWinCEWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
  WindowHandle: HWND; var OriginDiff: TPoint): boolean;
begin
  Result:=inherited GetDCOriginRelativeToWindow(PaintDC, WindowHandle,
    OriginDiff);
end;

function TWinCEWidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
begin
  Result:=inherited GetDesignerDC(WindowHandle);
end;}

{------------------------------------------------------------------------------
  Method: GetDeviceCaps
  Params: DC - display device context
          Index - index of needed capability

  Returns device specific information
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  Result := Windows.GetDeviceCaps(DC, Index);
end;

function TWinCEWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
  WindowHandle: HWND; var OriginDiff: TPoint): boolean;
var
  DCOrg, winOrg: Windows.POINT;
  ORect: TRect;
begin
  OriginDiff.X := 0;
  OriginDiff.Y := 0;
  //roozbeh changed
  //Result := Windows.GetDCOrgEx(PaintDC, DCOrg);
  DCOrg.X := 0;
  DCOrg.Y := 0;
  Result:=true;
  if not Result then exit;
  winOrg.X := 0;
  winOrg.Y := 0;
  Result := Windows.ClientToScreen(WindowHandle, @winOrg);
  if not Result then exit;
  Result := GetLCLClientBoundsOffset(WindowHandle, ORect);
  if not Result then exit;
  OriginDiff.X := DCOrg.X - winOrg.X - ORect.Left;
  OriginDiff.Y := DCOrg.Y - winOrg.Y - ORect.Top;
  Result := LCLIntf.GetWindowOrgEx(PaintDC, @winOrg) <> 0;
  if not Result then
  begin
    winOrg.X := 0;
    winOrg.Y := 0;
  end;
  dec(OriginDiff.X, winOrg.X);
  dec(OriginDiff.Y, winOrg.Y);
end;

function TWinCEWidgetSet.CreateDIBitmap(DC: HDC; var InfoHeader: TBitmapInfoHeader;
  dwUsage: DWORD; InitBits: PChar; var InitInfo: TBitmapInfo;
  wUsage: UINT): HBITMAP;
var
  hbit{,htargetbit} : HBITMAP;
  lpDestBits : PChar;
  dwLen : longint;
  lpH:TBitmapInfoHeader;
  orig_bitcount,nPadWidth,pad:integer;
  i, dwError: longint;
  d:PByte;
  s{,s0}:PWord;
//  pixel:word;
//  dc2:HDC;
begin
  nPadWidth := 4;
  lpH := InfoHeader;
  lpH.biSize:=sizeof(TBitmapInfoHeader);

  if lpH.biWidth mod nPadWidth <> 0  then
  pad := nPadWidth - lpH.biWidth mod nPadWidth;
  // there are problems with padding. i do not know the rules
  // anymore...
  pad := 0;
  // This is wrong when the infoheader is followed by color data...
  // We hope that biSizeImage is set then...
  dwLen := ((lpH.biWidth+pad) * abs(lpH.biHeight) * lpH.biBitCount) div 8;
  // includes colordata, if any...
  if lpH.biSizeImage <> 0 then
  dwLen := lpH.biSizeImage;
  orig_bitcount := lpH.biBitCount;

  {if unaligned(InitInfo.bmiHeader.biBitCount) = 16 then
  unaligned(InitInfo.bmiHeader.biBitCount) := 24;}

  {hbit := windows.CreateDIBSection( dc, windows.PBITMAPINFO(@lph)^,
          DIB_RGB_COLORS, lpDestBits, 0, 0);}
  //getmem(lpDestBits,dwLen);
  hbit := Windows.CreateBitmap( lpH.biWidth, abs(lpH.biHeight),1,InitInfo.bmiHeader.biBitCount,InitBits);

  //dc2:=windows.getdc(0);
  //htargetbit := Windows.CreateCompatibleBitmap( dc2,lpH.biWidth, abs(lpH.biHeight));

  //SelectObject(dc,hbit);
  //SelectObject(dc2,htargetbit);
  //BitBlt(dc2,0, 0, lpH.biWidth, abs(lpH.biHeight), Dc, 0, 0, SRCPAINT);
  result := hbit;
  //DeleteObject(dc2);
  //DeleteObject(hbit);

  exit;

  //hbit := CreateDIBSection(dc, InitInfo, DIB_RGB_COLORS, lpDestBits, 0, 0);
  //if (hbit <> 0) then
  begin

   if (orig_bitcount = 16) then
    begin
      if  (lpH.biCompression = BI_RGB) then
        begin
          s := PWord(InitBits);
	  d := PByte(lpDestBits);
	  //s0 := PWord(lpDestBits);
		// There is a bug in this code when padding was used!
		// how do you get the full color range from 5 bits???
		// shifting left seems to be ok...
          dwLen := dwLen shr 1;
	  for i := 0 to  dwLen-1 do
            begin
              d^ := ((s^ shr 0) and $1F) shl 3;
              inc(d);
              d^ := ((s^ shr 5) and $1F) shl 3;
              inc(d);
              d^ := ((s^ shr 10) and $1F) shl 3;
              inc(d);
              s:=s+2;
            end;
	  end else begin
                move(lpDestBits^, InitBits^, dwLen);
                //fillchar(lpDestBits^,100,dwlen);
            end;
	end else begin
	    move(lpDestBits^, InitBits^, dwLen);
        end;

        hbit := Windows.CreateBitmap( lpH.biWidth, abs(lpH.biHeight), InitInfo.bmiHeader.biPlanes,
                               InitInfo.bmiHeader.biBitCount, lpDestBits );
	result := hbit;
        freemem(lpDestBits);
        exit;
    end;
    dwError := GetLastError();
    //writeln('Cannot create bitmap: %d');
    result := HBITMAP(GDI_ERROR);
end;



function TWinCEWidgetSet.CreateDIBSection(DC: HDC; const p2: tagBitmapInfo; p3: UINT;
  var p4: Pointer; p5: THandle; p6: DWORD): HBITMAP;
begin
  Result := Windows.CreateDIBSection(DC, Windows.PBitmapInfo(@p2)^,  p3, p4, p5, p6)
end;

{------------------------------------------------------------------------------
  Function: GetDoubleClickTime
  Params: none
  Returns:

 ------------------------------------------------------------------------------}
function TWinceWidgetSet.GetDoubleClickTime: UINT;
begin
  Result := Windows.GetDoubleClickTime;
end;


{------------------------------------------------------------------------------
  Method:  GetFocus
  Params:  none
  Returns: The handle of the window with focus

  The GetFocus function retrieves the handle of the window that has the focus.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetFocus: HWND;
begin
  Result := Windows.GetFocus;
end;

{------------------------------------------------------------------------------
  Method:  GetForegroundWindow
  Params:  none
  Returns: The handle of the foreground window

  The GetForegroundWindow function returns the window that the user is
  currently working with, session wide.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetForegroundWindow: HWND;
begin
  Result := Windows.GetForegroundWindow;
end;
{function TWinCEWidgetSet.GetFontLanguageInfo(DC: HDC): DWord;
begin
  Result:=inherited GetFontLanguageInfo(DC);
end;}

function TWinCEWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
begin
  Result := Windows.GetKeyState(nVirtKey);
end;

function TWinCEWidgetSet.GetMonitorInfo(hMonitor: HMONITOR; lpmi: PMonitorInfo): Boolean;
var
  LocalInfo: TMonitorInfoExW;
begin
  if (lpmi <> nil) and (lpmi^.cbSize >= SizeOf(TMonitorInfoEx)) then
  begin
    LocalInfo.cbSize := SizeOf(TMonitorInfoExW);
    Result := WinCEDef.GetMonitorInfoW(hMonitor, @LocalInfo);
    lpmi^.rcMonitor := LocalInfo.rcMonitor;
    lpmi^.rcWork := LocalInfo.rcWork;
    lpmi^.dwFlags := LocalInfo.dwFlags;
    PMonitorInfoEx(lpmi)^.szDevice := UTF16ToUTF8(LocalInfo.szDevice);
  end
  else
    Result := WinCEDef.GetMonitorInfoW(hMonitor, lpmi);
end;

{------------------------------------------------------------------------------
  Method:  GetObject
  Params:  GDIObj  - handle to graphics object of interest
           BufSize - size of buffer for object information
           Buf     - pointer to buffer for object information
  Returns: the number of bytes stored into the buffer

  Gets information about a specified graphics object.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
begin
  //DebugLn('Trace:[TWinCEWidgetSet.GetObject]');
  Result := Windows.GetObject(GDIObj, BufSize, Buf);
end;

{------------------------------------------------------------------------------
  Method:  GetParent
  Params: Handle - handle of child window
  Returns: the handle of the parent window

  Retrieves the handle of the specified child window's parent window.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetParent(Handle: HWND): HWND;
begin
  Result := Windows.GetParent(Handle);
end;

{------------------------------------------------------------------------------
  Method:  GetProp
  Params: Handle - handle of window
          Str    - string
  Returns: the associated data

  Retrieves a pointer to data from the property list of the given window.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
var
  WideStr: widestring;
begin
  WideStr := UTF8Decode(String(str));
  {$ifdef win32}
  Result := Pointer(Windows.GetPropW(Handle, PWideChar(WideStr)));
  {$else}
  Result := Pointer(Windows.GetProp(Handle, PWideChar(WideStr)));
  {$endif}
end;

{function TWinCEWidgetSet.GetRawImageFromDevice(SrcDC: HDC;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
begin
  Result:=inherited GetRawImageFromDevice(SrcDC, SrcRect, NewRawImage);
end;

function TWinCEWidgetSet.GetRawImageFromBitmap(SrcBitmap,
  SrcMaskBitmap: HBITMAP; const SrcRect: TRect; var NewRawImage: TRawImage
  ): boolean;
begin
  Result:=inherited GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap, SrcRect,
    NewRawImage);
end;

function TWinCEWidgetSet.GetRgnBox(RGN: HRGN; lpRect: PRect): Longint;
begin
  Result:=inherited GetRgnBox(RGN, lpRect);
end;}

{------------------------------------------------------------------------------
  Method:  GetROP2
  Params: DC - Handle of the device context

  Returns: 0 if unsuccessful, the current Foreground Mixing Mode if successul

  Retrieves the current Foreground Mixing Mode in the given device context
 ------------------------------------------------------------------------------}
{function TWinCEWidgetSet.GetROP2(DC: HDC): Integer;
begin
  Result := Windows.GetROP2(DC); // not found
end;}

{function TWinCEWidgetSet.GetScrollBarSize(Handle: HWND; BarKind: Integer
  ): integer;
begin
  Result:=inherited GetScrollBarSize(Handle, BarKind);
end;}

function TWinCEWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer
  ): boolean;
var
  dwStyle: DWORD;
begin
  Result := False;
  dwStyle := GetWindowLong(Handle, GWL_STYLE);

  case SBStyle of
    SB_BOTH: Result := (dwStyle and (WS_VSCROLL or WS_HSCROLL)) <> 0;
    SB_VERT: Result := (dwStyle and WS_VSCROLL) <> 0;
    SB_HORZ: Result := (dwStyle and WS_HSCROLL) <> 0;
  else
    Result := False;
  end;
end;

{------------------------------------------------------------------------------
  Method:  GetScrollInfo
  Params:  Handle     - handle of window with scroll bar
           BarFlag    - scroll bar flag
           ScrollInfo - record for scroll parameters
  Returns: if the function retrieved any values.

  Retrieves the parameters of a scroll bar.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetScrollInfo(Handle: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): Boolean;
begin
  ScrollInfo.cbSize:=sizeof(ScrollInfo);
  //DebugLn('Trace:TODO: [TWinCEWidgetSet.GetScrollInfo]');
  Result := Boolean(Windows.GetScrollInfo(Handle, BarFlag, @ScrollInfo));
end;

{------------------------------------------------------------------------------
  Method:  GetStockObject
  Params:  Value - type of stock object
  Returns: a value identifying the logical object requested

  Retrieves a handle to one of the predefined stock objects.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetStockObject(Value: Integer): THandle;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.GetStockObject] %d ', [Value]));
  Result := Windows.GetStockObject(Value);
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.GetStockObject] %d --> 0x%x', [Value, Result]));
end;

{------------------------------------------------------------------------------
  Method:  GetSysColor
  Params:   NIndex - display element whose color is to be retrieved
  Returns:  RGB value

  Retrieves the current color of the specified display element.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetSysColor(NIndex: Integer): DWORD;
begin
  if NIndex = COLOR_FORM then
    NIndex := COLOR_BTNFACE;
  { SYS_COLOR_INDEX_FLAG is indispensable on Windows CE }
  Result := Windows.GetSysColor(nIndex or SYS_COLOR_INDEX_FLAG);
end;

function TWinCEWidgetSet.GetSysColorBrush(nIndex: Integer): HBrush;
begin
  if NIndex = COLOR_FORM then
    NIndex := COLOR_BTNFACE;
  Result := Windows.GetSysColorBrush(nIndex or SYS_COLOR_INDEX_FLAG);
end;

{------------------------------------------------------------------------------
  Method:  GetSystemMetrics
  Params: NIndex - system metric to retrieve
  Returns: the requested system metric

  Retrieves various system metrics.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetSystemMetrics(NIndex: Integer): Integer;
begin
  //DebugLn(Format('Trace:[TWinCEWidgetSet.GetSystemMetrics] %s', [IntToStr(NIndex)]));
  case NIndex of
  SM_LCLHasFormAlphaBlend: Result := 1;
  else
    Result := Windows.GetSystemMetrics(NIndex);
  end;
  //DebugLn(Format('Trace:[TWinCEWidgetSet.GetSystemMetrics] %s --> 0x%S (%s)', [IntToStr(NIndex), IntToHex(Result, 8), IntToStr(Result)]));
end;

function TWinCEWidgetSet.GetTextColor(DC: HDC): TColorRef;
begin
  Result := Windows.GetTextColor(DC);
end;

{------------------------------------------------------------------------------
  Function: RedrawWindow
  Params: Wnd:
          lprcUpdate:
          hrgnUpdate:
          flags:
  Returns:

 ------------------------------------------------------------------------------}
function TWinceWidgetSet.RedrawWindow(Wnd: HWND; lprcUpdate: PRECT; hrgnUpdate: HRGN; flags: UINT): Boolean;
begin
  Result := Windows.RedrawWindow(Wnd,lprcUpdate,hrgnUpdate,flags);
end;

function TWinCEWidgetSet.UpdateWindow(Handle: HWND): Boolean;
begin
Result := Windows.UpdateWindow(Handle);
end;
{------------------------------------------------------------------------------
  Method:  GetTextExtentPoint
  Params:  DC    - handle of device context
           Str   - text string
           Count - number of characters in string
           Size  - TSize record in which the dimensions of the string are to be
                   returned
  Returns: if the function succeeded

  Computes the width and height of the specified string of text.
 ------------------------------------------------------------------------------}

function TWinCEWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  s: AnsiString;
  w: WideString;
begin
  //DebugLn('Trace:[TWinCEWidgetSet.GetTextExtentPoint] - Start');
  if count = -1 then
    s := str
  else
  begin
    SetLength(s, count);
    move(str^, s[1], count);
  end;
  // the length of utf8 vs Wide/Ansi the strings differ, so recalc.

  // TODO: use the real number of chars (and not the lenght)
  w := Utf8Decode(S);
  Result := Windows.GetTextExtentPoint32W(DC, PWideChar(W), Length(W),
   {$ifdef Win32}@Size{$else}Size{$endif});
//  Result := Boolean(Windows.GetTextExtentExPointW(DC, WideStr, Count, 0,nil,nil,@Size));
  //DebugLn('Trace:[TWinCEWidgetSet.GetTextExtentPoint] - Exit');
end;

{------------------------------------------------------------------------------
  Method:  GetTextMetrics
  Params:  DC - handle of device context
           TM - text metrics record
  Returns: if the function succeeds

  Fills the specified buffer with the metrics for the currently selected font.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  tmw: TTextMetricW;
begin
  //DebugLn(Format('Trace:> TODO FINISH[TWinCEWidgetSet.GetTextMetrics] DC: 0x%x', [DC]));
  Result := Boolean(Windows.GetTextMetrics(DC, @TMw));
  TM.tmHeight:= TMW.tmHeight;
  TM.tmAscent:= TMW.tmAscent;
  TM.tmDescent:= TMW.tmDescent;
  TM.tmInternalLeading:= TMW.tmInternalLeading;
  TM.tmExternalLeading:= TMW.tmExternalLeading;
  TM.tmAveCharWidth:= TMW.tmAveCharWidth;
  TM.tmMaxCharWidth:= TMW.tmMaxCharWidth;
  TM.tmWeight:= TMW.tmWeight;
  TM.tmOverhang:= TMW.tmOverhang;
  TM.tmDigitizedAspectX:= TMW.tmDigitizedAspectX;
  TM.tmDigitizedAspectY:= TMW.tmDigitizedAspectY;
  TM.tmFirstChar:= TMW.tmFirstChar;
  TM.tmLastChar:= TMW.tmLastChar;
  TM.tmDefaultChar:= TMW.tmDefaultChar;
  TM.tmBreakChar:= TMW.tmBreakChar;
  TM.tmItalic:= TMW.tmItalic;
  TM.tmUnderlined:= TMW.tmUnderlined;
  TM.tmStruckOut:= TMW.tmStruckOut;
  TM.tmPitchAndFamily:= TMW.tmPitchAndFamily;
  TM.tmCharSet:= TMW.tmCharSet;
  //DebugLn(Format('Trace:< TODO FINISH[TWinCEWidgetSet.GetTextMetrics] DC: 0x%x', [DC]));
end;

function TWinCEWidgetSet.GetViewPortExtEx(DC: HDC; Size: PSize): Integer;
begin
  {$ifdef Win32}
    Result := Integer(Windows.GetViewPortExtEx(DC, LPSize(Size)));
  {$else}
    Result := 0;
  {$endif}
end;

function TWinCEWidgetSet.MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean;
var
  P: TPoint;
  lResult: Integer;
Begin
  lResult := GetViewPortOrgEx(dc, @P);
  if lResult <> 0 then
    Result := SetViewPortOrgEx(dc, P.x+dX, P.y+dY, @P)
  else
    Result := False;
end;

{ This routine isn't used directly by the LCL
  We implent it with SetViewPortOrgEx because GetViewPortOrgEx is
  only available in Windows Mobile 5.0 +
}
function TWinCEWidgetSet.GetViewPortOrgEx(DC: HDC; P: PPoint): Integer;
begin
  {$ifdef Win32}
    Result := Integer(Windows.GetViewPortOrgEx(DC, LPPoint(P)));
  {$else}
    Result := 0;
    if P = nil then Exit;
    Result := Integer(SetViewPortOrgEx(DC, 0, 0, P));
    SetViewPortOrgEx(DC, P^.x, P^.y, nil);
  {$endif}
end;

function TWinCEWidgetSet.GetWindowExtEx(DC: HDC; Size: PSize): Integer;
begin
  {$ifdef Win32}
    Result := Integer(Windows.GetWindowExtEx(DC, LPSize(Size)));
  {$else}
    Result := 0;
  {$endif}
end;


{------------------------------------------------------------------------------
  Method:  GetWindowLong
  Params:  Handle - handle of window
           Int    - value to retrieve
  Returns: the requested 32-bit value

  Retrieves information about the specified window.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetWindowLong(Handle: HWND; Int: Integer): PtrInt;
begin
  //TODO:Started but not finished
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.GETWINDOWLONG] HWND: 0x%x, int: 0x%x (%d)', [Handle, int, int]));
  Result := Windows.GetWindowLong(Handle, int);
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.GETWINDOWLONG] HWND: 0x%x, int: 0x%x (%d) --> 0x%x (%d)', [Handle, int, int, Result, Result]));
end;

{------------------------------------------------------------------------------
  Method:  GetWindowOrgEx
  Params:  DC - handle of device context
           P  - record receiving the window origin
  Returns: 0 if the function fails; non-zero integer otherwise

  Retrieves the x-coordinates and y-coordinates of the window origin for the
  specified device context.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetWindowOrgEx(DC: HDC; P: PPoint): Integer;
begin
  {$ifdef Win32}
    Result := Integer(Windows.GetWindowOrgEx(DC, P));
  {$else}
  if WinExt.GetWindowOrgEx <> nil then
    Result := Integer(WinExt.GetWindowOrgEx(DC, P))
  else
    Result := 0;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - handle of window
           Rect   - record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the dimensions of the bounding rectangle of the specified window.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetWindowRect(Handle: HWND; var Rect: TRect): Integer;
begin
  Result := Integer(Windows.GetWindowRect(Handle, @Rect));
end;

{------------------------------------------------------------------------------
  Function: GetWindowRelativePosition
  Params:  Handle : HWND;
  Returns: true on success

  returns the current widget Left, Top, relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetWindowRelativePosition(Handle : HWND;
  var Left, Top:integer): boolean;
var
  LeftTop:TPoint;
  R: TRect;
  ParentHandle: THandle;
//  WindowInfo: PWindowInfo;
begin
  Result:=false;
//  WindowInfo := GetWindowInfo(Handle);
//  if (WindowInfo^.WinControl is TCustomFloatSpinEdit) then
//    Handle := Windows.SendMessage(Handle, UDM_GETBUDDY, 0, 0);
  if not Windows.GetWindowRect(Handle,@R) then exit;
  LeftTop.X:=R.Left;
  LeftTop.Y:=R.Top;
  ParentHandle:=Windows.GetParent(Handle);
  if ParentHandle<>0 then
  begin
    if not Windows.ScreenToClient(ParentHandle,@LeftTop) then exit;
    if not GetLCLClientBoundsOffset(ParentHandle, R) then
      exit;
    dec(LeftTop.X, R.Left);
    dec(LeftTop.Y, R.Top);
  end;
  Left:=LeftTop.X;
  Top:=LeftTop.Y;
  Result:=true;
end;

{------------------------------------------------------------------------------
  Function: GetWindowSize
  Params:  Handle : hwnd;
  Returns: true on success

  Returns the current widget Width and Height

  Note: Windows.GetWindowInfo doesnt exist in wince, but
  we can use GetWindowLong and other APIs for most information

  Also GetWindowPlacement doesnt exist
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetWindowSize(Handle : hwnd;
  var Width, Height: integer): boolean;
var
  R: TRect;
  WindowInfo: PWindowInfo;
  Style, ExStyle: longint;
  {$IFDEF VerboseSizeMsg}
  lName: string;
  {$ENDIF}

  // This is for TCustomFloatSpinEdit
  // and the Buddy is the Edit component
  // the main component is the Spin
  // The total Width is from Buddy(=Edit) Left to Main(=Spin) Right
  procedure AdjustForBuddySize;
  {$IFDEF WinCE}
  var
    BuddyHandle: HWND;
    BuddyR: TRect;
  begin
    BuddyHandle := Windows.SendMessage(Handle, UDM_GETBUDDY, 0, 0);
    if (BuddyHandle<>HWND(nil)) then
      if Windows.GetWindowRect(BuddyHandle, BuddyR) then
        Width := R.Right - BuddyR.Left;
  end;
  {$ELSE}
  var
    BuddyHandle: HWND;
    BuddyWP, WP: WINDOWPLACEMENT;
  begin
    WP.length := SizeOf(WP);
    Windows.GetWindowPlacement(Handle, WP);
    BuddyHandle := Windows.SendMessage(Handle, UDM_GETBUDDY, 0, 0);
    if (BuddyHandle<>HWND(nil)) and Windows.GetWindowPlacement(BuddyHandle, BuddyWP)
      then Width := WP.rcNormalPosition.Right - BuddyWP.rcNormalPosition.Left;
  end;
  {$ENDIF}

begin
  Result := Boolean(Windows.GetWindowRect(Handle, R));

  if not Result then Exit;

  // No special handling for maximized windows
  // they do not exist in wince anyway, they are
  // emulated by calculating the desktop size
  Width := R.Right - R.Left;
  Height := R.Bottom - R.Top;

  WindowInfo := winceproc.GetWindowInfo(Handle);

  if WindowInfo <> nil then
  begin
    // convert top level lcl window coordinaties to win32 coord
    Style := Windows.GetWindowLongW(Handle, GWL_STYLE);
    ExStyle := Windows.GetWindowLongW(Handle, GWL_EXSTYLE);

    // Windows CE doesn't reliably return the styles, so
    // it returns form related styles for comboboxes for example
    // This extra check avoids problems with that
    if (WindowInfo^.WinControl is TCustomForm) then
    begin
      // The borders are not given by the same constants in Win32 and WinCE
      // Bug http://bugs.freepascal.org/view.php?id=11456
      //
      // SM_CXSIZEFRAME returns 3 in my tests, but the real border
      // is only 1 pixel wide, like SM_CXBORDER
      {$IFDEF WinCE}
        if (Style and WS_BORDER) <> 0 then
        begin
          // thin, non-sizing border
          Dec(Width, 2*Windows.GetSystemMetrics(SM_CXBORDER));
          Dec(Height, 2*Windows.GetSystemMetrics(SM_CYBORDER));
        end;
      {$ELSE}
        if (Style and WS_THICKFRAME) <> 0 then
        begin
          // thick, sizing border
          // add twice, top+bottom border
          Dec(Width, 2*Windows.GetSystemMetrics(SM_CXSIZEFRAME));
          Dec(Height, 2*Windows.GetSystemMetrics(SM_CYSIZEFRAME));
        end else
        if (Style and WS_BORDER) <> 0 then
        begin
          // thin, non-sizing border
          Dec(Width, 2*Windows.GetSystemMetrics(SM_CXFIXEDFRAME));
          Dec(Height, 2*Windows.GetSystemMetrics(SM_CYFIXEDFRAME));
        end;
      {$ENDIF}

      // ExcludeCaption
      if ((Style and WS_CAPTION) <> 0) then
        if (ExStyle and WS_EX_TOOLWINDOW) <> 0 then
          Dec(Height, Windows.GetSystemMetrics(SM_CYSMCAPTION))
        else
          Dec(Height, Windows.GetSystemMetrics(SM_CYCAPTION));
    end;

    if (WindowInfo^.WinControl is TCustomFloatSpinEdit) then
      AdjustForBuddySize;
  end;

  {$IFDEF VerboseSizeMsg}
  if (WindowInfo <> nil) and (WindowInfo^.WinControl <> nil) then
   lName := WindowInfo^.WinControl.Name
  else lName := 'NIL';
  DebugLn(Format('[TWinCEWidgetSet.GetWindowSize]: Name:%s %d:%d',
    [lName, Width, Height]));
  {$ENDIF}
end;

{
function TWinCEWidgetSet.GradientFill(DC: HDC; Vertices: PTriVertex;
  NumVertices: Longint; Meshes: Pointer; NumMeshes: Longint; Mode: Longint
  ): Boolean;
begin
  Result:=inherited GradientFill(DC, Vertices, NumVertices, Meshes, NumMeshes,
    Mode);
end;}

{------------------------------------------------------------------------------
  Method:  HideCaret
  Params:  HWnd - handle to the window with the caret
  Returns: Whether the window owns the caret

  Removes the caret from the screen.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.HideCaret(hWnd: HWND): Boolean;
begin
  Result := Boolean(Windows.HideCaret(hWnd));
end;

{------------------------------------------------------------------------------
  Method:  InvalidateRect
  Params: AHandle - handle of window with changed update region
          Rect    - address of rectangle coordinates
          BErase  - specifies whether the background is to be erased
  Returns: if the function succeeds

  Adds a rectangle to the specified window's update region.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.InvalidateRect(aHandle: HWND; Rect: PRect; BErase: Boolean): Boolean;
var
  Flags: UINT;
  ORect: TRect;
begin
  Flags := RDW_INVALIDATE or RDW_ALLCHILDREN;
  if BErase then
    Flags := Flags or RDW_ERASE;
  if Rect <> nil then
  begin
    GetLCLClientBoundsOffset(aHandle, ORect);
    OffsetRect(Rect^, ORect.Left, ORect.Top);
  end;
  Result := Boolean(Windows.RedrawWindow(aHandle, Rect, 0, Flags));
end;

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
function TWinCEWidgetSet.IntersectClipRect(dc: hdc;
  Left, Top, Right, Bottom: Integer): Integer;
begin
//  Result := Windows.IntersectClipRect(DC, Left, Top, Right, Bottom);
end;

{------------------------------------------------------------------------------
  Method: IsWindow
  Params: handle - window handle
  Returns: true if handle is window , false otherwise
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.IsWindow(handle: HWND): boolean; 
begin
  Result := Boolean(Windows.IsWindow(handle));
end;

{------------------------------------------------------------------------------
  Method: IsWindowEnabled
  Params: handle - window handle
  Returns: true if window is enabled, false otherwise
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.IsWindowEnabled(handle: HWND): boolean;
begin
  Result := Boolean(Windows.IsWindowEnabled(handle));
end;

{------------------------------------------------------------------------------
  Method: IsWindowVisible
  Params: handle - window handle
  Returns: true if window is visible, false otherwise
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.IsWindowVisible(handle: HWND): boolean;
begin
  Result := Boolean(Windows.IsWindowVisible(handle));
end;

{------------------------------------------------------------------------------
  Method:  LineTo
  Params:  DC - device context handle
           X  - x-coordinate of line's ending point
           Y  - y-coordinate of line's ending point
  Returns: if the function succeeds

  Draws a line from the current position up to, but not including, the specified point.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.LineTo] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
  Result := Boolean(Windows.LineTo(DC, X, Y));
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.LineTo] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
end;

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
function TWinCEWidgetSet.MessageBox(HWnd: HWND; LPText, LPCaption: PChar; UType: Cardinal): Integer;
var
  WideLPText, WideLPCaption: widestring;
begin
  WideLPText := UTF8Decode(string(LPText));
  WideLPCaption := UTF8Decode(string(LPCaption));

  Result := Windows.MessageBoxW(HWnd, PWideChar(WideLPText),
    PWideChar(WideLPCaption), UType);
end;

function TWinCEWidgetSet.MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR;
begin
  Result := WinCEDef.MonitorFromPoint(ptScreenCoords, dwFlags);
end;

function TWinCEWidgetSet.MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR;
begin
  Result := WinCEDef.MonitorFromRect(lprcScreenCoords, dwFlags);
end;

function TWinCEWidgetSet.MonitorFromWindow(hWnd: HWND; dwFlags: DWord): HMONITOR;
begin
  Result := WinCEDef.MonitorFromWindow(hWnd, dwFlags);
end;

{------------------------------------------------------------------------------
  Method:  MoveToEx
  Params:  DC       - handle of device context
           X        - x-coordinate of new current position
           Y        - x-coordinate of new current position
           OldPoint - address of old current position
  Returns: if the function succeeds.

  Updates the current position to the specified point.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.MoveToEx] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
  Result := Boolean(Windows.MoveToEx(DC, X, Y, LPPOINT(OldPoint)));
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.MoveToEx] DC:0x%x, X:%d, Y:%d', [DC, X, Y]));
end;

function TWinCEWidgetSet.OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer;
begin
  Result := Windows.OffsetRgn(RGN, nXOffset, nYOffset);
end;

{function TWinCEWidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: boolean): boolean;
begin
  Result:=inherited PolyBezier(DC, Points, NumPts, Filled, Continuous);
end;}

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
function TWinCEWidgetSet.PeekMessage(var LPMsg: TMsg; Handle: HWND; WMsgFilterMin, WMsgFilterMax, WRemoveMsg: UINT): Boolean;
begin
  Result := Boolean(Windows.PeekMessage(@LPMsg, Handle, WMsgFilterMin, WMsgFilterMax, WRemoveMsg));
end;

{------------------------------------------------------------------------------
  Method:  Polygon
  Params:  DC - handle to device context
           Points - pointer to polygon's vertices
           NumPts - count of polygon's vertices
           Winding
  Returns: if the function succeeds

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
function TWinCEWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean): Boolean;
//var
//  PFMode : Longint;
begin
  //DebugLn(Format('Trace:TWinCEWidgetSet.Polygon --> DC: 0x%X, Number of points: %D, Use winding fill: %S', [DC, NumPts, BOOL_RESULT[Winding]]));
//  if Winding then
//    PFMode := SetPolyFillMode(DC, Windows.WINDING)
//  else
//    PFMode := SetPolyFillMode(DC, Windows.ALTERNATE);
  Result := Boolean(Windows.Polygon(DC, LPPOINT(Points), NumPts));
//  SetPolyFillMode(DC, PFMode);
end;

{------------------------------------------------------------------------------
  Method:  Polyline
  Params:  DC - handle of device context
           Points - address of array containing endpoints
           NumPts - number of points in the array
  Returns: if the function succeeds

  Draws a series of line segments by connecting the points in the specified
  array.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): Boolean;
begin
  Result := Boolean(Windows.Polyline(DC, LPPOINT(Points), NumPts));
end;

{------------------------------------------------------------------------------
  Method:  PostMessage
  Params: Handle - handle of destination window
          Msg - message to post
          WParam - first message parameter
          LParam - second message parameter
  Returns: True if succesful

  The PostMessage function places (posts) a message in the message queue and
  then returns without waiting.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): Boolean;
begin
  Result := Boolean(Windows.PostMessage(Handle, Msg, WParam, LParam));
end;

{------------------------------------------------------------------------------
  Method:   RadialArc
  Params:   DC, left, top, right, bottom, sx, sy, ex, ey
  Returns:  Nothing

  Use RadialArc to draw an elliptically curved line with the current Pen. The
  values sx,sy, and ex,ey represent the starting and ending radial-points
  between which the Arc is drawn.

------------------------------------------------------------------------------}
{function TWinCEWidgetSet.RadialArc(DC: HDC; x, y, width, height, sx, sy, ex,
  ey: Integer): Boolean;
begin
  Result:=inherited RadialArc(DC, x, y, width, height, sx, sy, ex, ey);
end;}
{------------------------------------------------------------------------------
  Method:   RadialChord
  Params:   DC, x1, y1, x2, y2, sx, sy, ex, ey
  Returns:  Nothing

  Use RadialChord to draw a filled Chord-shape on the canvas. The values sx,sy,
  and ex,ey represent the starting and ending radial-points between which
  the bounding-Arc is drawn.

------------------------------------------------------------------------------}
{function TWinCEWidgetSet.RadialChord(DC: HDC; x, y, width, height, sx, sy, ex,
  ey: Integer): Boolean;
begin
  Result:=inherited RadialChord(DC, x, y, width, height, sx, sy, ex, ey);
end;}

{------------------------------------------------------------------------------
  Method:  RealizePalette
  Params:  DC - handle of device context
  Returns: number of entries in the logical palette mapped to the system
           palette

  Maps palette entries from the current logical palette to the system palette.
 ------------------------------------------------------------------------------}
{function TWinCEWidgetSet.RealizePalette(DC: HDC): Cardinal;
begin
  Result:=inherited RealizePalette(DC);
end;}

{------------------------------------------------------------------------------
  Method:  Rectangle
  Params:  DC - handle of device context
           X1 - x-coordinate of bounding rectangle's upper-left corner
           Y1 - y-coordinate of bounding rectangle's upper-left corner
           X2 - x-coordinate of bounding rectangle's lower-right corner
           Y2 - y-coordinate of bounding rectangle's lower-right corner
  Returns: if the function succeeds

  The Rectangle function draws a rectangle. The rectangle is outlined by using
  the current pen and filled by using the current brush.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.Rectangle] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d', [DC, X1, Y1, X2, Y2]));
  Result := Boolean(Windows.Rectangle(DC, X1, Y1, X2, Y2));
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.Rectangle] DC:0x%x, X1:%d, Y1:%d, X2:%d, Y2:%d', [DC, X1, Y1, X2, Y2]));
end;

// Determines if the specified rectangle is within the boundaries of a region.
function TWinCEWidgetSet.RectInRegion(RGN: HRGN; ARect: TRect): Boolean;
begin
  Result := Windows.RectInRegion(RGN, ARect);
end;

function TWinCEWidgetSet.RectVisible(dc : hdc; const ARect: TRect) : Boolean;
begin
  Result := Boolean(Windows.RectVisible(DC, LPRECT(@ARect)^));
//  Result := True;
  {$ifdef DEBUG_WINDOW_ORG}
  DebugLn(
    Format(':> [TWinCEWidgetSet.RectVisible] Result=%d',
    [Integer(Result)]));
  {$endif}
end;

{function TWinCEWidgetSet.RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer
  ): Boolean;
begin
  Result:=inherited RegroupMenuItem(hndMenu, GroupIndex);
end;}

{------------------------------------------------------------------------------
  Method:  ReleaseCapture
  Params:  none
  Returns: True if succesful

  The ReleaseCapture function releases the mouse capture from a window
  and restores normal mouse input processing.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ReleaseCapture: Boolean;
begin
  Result := Boolean(Windows.ReleaseCapture);
end;

{------------------------------------------------------------------------------
  Method:  ReleaseDC
  Params:  HWnd - handle of window
           DC   - handle of device context
  Returns: 1 if the device context was released or 0 if it wasn't

  Releases a device context (DC), freeing it for use by other applications.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ReleaseDC(Window: HWND; DC: HDC): Integer;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.ReleaseDC] DC:0x%x', [DC]));
  Result := Windows.ReleaseDC(Window, DC);
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.ReleaseDC] DC:0x%x', [DC]));
end;

{------------------------------------------------------------------------------
  Function: RemoveProp
  Params: Handle: Handle of the object
          Str: Name of the property to remove
  Returns: The handle of the property (0=failure)

 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.RemoveProp(Handle: hwnd; Str: PChar): THandle;
var
  WideStr: widestring;
begin
  WideStr := UTF8Decode(String(str));
  {$ifdef win32}
  Result := THandle(Windows.RemovePropW(Handle, PWideChar(WideStr)));
  {$else}
  Result := THandle(Windows.RemoveProp(Handle, PWideChar(WideStr)));
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  RestoreDC
  Params:  DC - handle of device context
           SavedDC - state to be restored
  Returns: if the function succeeds

  Restores a device context (DC) to the specified state.
-------------------------------------------------------------------------------}
function TWinCEWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.RestoreDC] DC:0x%x, SavedDC: %d', [DC, SavedDC]));
  Result := Boolean(Windows.RestoreDC(DC, SavedDC));
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.RestoreDC] DC:0x%x, Saved: %d --> %s', [Integer(DC), SavedDC, BOOL_TEXT[Result]]));
end;

{------------------------------------------------------------------------------
  Method:   RoundRect
  Params:   DC, X1, Y1, X2, Y2, RX, RY
  Returns:  true if succesfull, false otherwise

  Draws a Rectangle with optional rounded corners. RY is the radial height
  of the corner arcs, RX is the radial width.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.RoundRect(DC: HDC; X1, Y1, X2, Y2: Integer; RX, RY : Integer): Boolean;
begin
  Result := Windows.RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
end;

{------------------------------------------------------------------------------
  Method:  SaveDC
  Params:  DC - a DC to save
  Returns: 0 if the functions fails otherwise a positive integer identifing
           the saved DC

  The SaveDC function saves the current state of the specified device
  context (DC) by copying its elements to a context stack.
-------------------------------------------------------------------------------}
function TWinCEWidgetSet.SaveDC(DC: HDC): Integer;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.SaveDC]  0x%x', [Integer(DC)]));
  Result := Windows.SaveDC(DC);
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.SaveDC]  0x%x --> %d', [Integer(DC), Result]));
end;

{------------------------------------------------------------------------------
  Method:  ScreenToClient
  Params: Handle - window handle for source coordinates
          P      - record containing coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Converts the screen coordinates of a specified point on the screen to client
  coordinates.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ScreenToClient(Handle: HWND; var P: TPoint): Integer;
begin
  Result := Integer(Windows.ScreenToClient(Handle, @P));
end;

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
function TWinCEWidgetSet.ScrollWindowEx(HWnd: HWND; DX, DY: Integer; PRcScroll, PRcClip: PRect; HRgnUpdate: HRGN; PRcUpdate: PRect; Flags: UINT): Boolean;
begin
  Result := Windows.ScrollWindowEx(HWnd, DX, DY, Windows.RECT(PRcScroll^), Windows.RECT(PRcClip^), HRgnUpdate, LPRECT(PRcUpdate), Flags) <> ERROR;
end;

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
function TWinCEWidgetSet.SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
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
function TWinCEWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  //TODO: Finish this;
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.SelectObject] DC: 0x%x', [DC]));
  Result := Windows.SelectObject(DC, GDIObj);
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.SelectObject] DC: 0x%x  --> 0x%x', [DC, Result]));
end;

{------------------------------------------------------------------------------
  Method:  SelectPalette
  Params:  DC - handle of device context
           Palette - handle of logical color palette
           ForceBackground - whether the logical palette is forced to be a
                             background palette
  Returns: the device context's previous logical palette

  Selects the specified logical palette into a device context.
 ------------------------------------------------------------------------------}
{function TWinCEWidgetSet.SelectPalette(DC: HDC; Palette: HPALETTE;
  ForceBackground: Boolean): HPALETTE;
begin
  Result:=inherited SelectPalette(DC, Palette, ForceBackground);
end;}

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
function TWinCEWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
begin
  Result := Windows.SendMessage(HandleWnd, Msg, WParam, LParam);
end;

{------------------------------------------------------------------------------
  Method:  SetActiveWindow
  Params:  Window - Window to focus
  Returns: Old active window

  Sets focus to the specified window, if the current process is on top
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetActiveWindow(Window: HWND): HWND;
begin
  Result := Windows.SetActiveWindow(Window);
end;

{------------------------------------------------------------------------------
  Method:  SetBkColor
  Params:  DC    - Device context to change the text background color
           Color - background color value
  Returns: Old Background color

  Sets the current background color to the specified color value.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetBKColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.SetBKColor] DC: 0x%x Color: %8x', [Integer(DC), Color]));
  Result := Windows.SetBkColor(DC, ColorToRGB(Color));
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.SetBKColor] DC: 0x%x Color: %8x --> %8x', [Integer(DC), Color, Result]));
end;

{------------------------------------------------------------------------------
  Method:  SetBkMode
  Params: DC     - handle of device context
          BkMode - flag specifying background mode
  Returns: the previous background mode

  Sets the background mix mode of the specified device context.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetBkMode(DC: HDC; BkMode: Integer): Integer;
begin
  Result := Windows.SetBkMode(DC, BkMode);
end;

{function TWinCEWidgetSet.SetComboMinDropDownSize(Handle: HWND; MinItemsWidth,
  MinItemsHeight, MinItemCount: integer): boolean;
begin
  Result:=inherited SetComboMinDropDownSize(Handle, MinItemsWidth,
    MinItemsHeight, MinItemCount);
end;}

{------------------------------------------------------------------------------
  Method:  SetCapture
  Params:  Value - Handle of window to capture
  Returns: the handle of the window that had previously captured the mouse

  Sets the mouse capture to the specified window.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetCapture(AHandle: HWND): HWND;
begin
  Result := Windows.SetCapture(AHandle);
end;

{------------------------------------------------------------------------------
  Method:  SetCaretPos
  Params:  new position x, y
  Returns: true on success

  Moves the caret to the specified coordinates.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := Boolean(Windows.SetCaretPos(X, Y));
end;

{------------------------------------------------------------------------------
  Method:  SetCaretPosEx
  Params:  Handle - handle of window
           X      - horizontal mouse coordinate
           Y      - vertical mouse coordinate
  Returns: true on success

  Moves the caret to the specified coordinates in the specified window.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean;
begin
  Result := Windows.SetCaretPos(X, Y);
end;

{function TWinCEWidgetSet.SetCaretRespondToFocus(handle: HWND;
  ShowHideOnFocus: boolean): Boolean;
begin
  Result:=inherited SetCaretRespondToFocus(handle, ShowHideOnFocus);
end;}

function TWinCEWidgetSet.SetCursor(hCursor: HICON): HCURSOR;
begin
  Result := Windows.SetCursor(hCursor);
end;

{------------------------------------------------------------------------------
  Function: SetCursorPos
  Params: X:
          Y:
  Returns:

 ------------------------------------------------------------------------------}
{function TWinCEWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
begin
  Result:=inherited SetCursorPos(X, Y);
end;}

{------------------------------------------------------------------------------
  Method:  SetFocus
  Params:  HWnd - Handle of new focus window
  Returns: The old focus window

  The SetFocus function sets the keyboard focus to the specified window
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetFocus(hWnd: HWND): HWND;
begin
  Result := Windows.SetFocus(HWnd);
end;

{------------------------------------------------------------------------------
  Method:  SetForegroundWindow
  Params:  HWnd - The handle of the window
  Returns: True if succesful

  The SetForegroundWindow function brings the specified window to top
    (highest z-index level).
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetForegroundWindow(HWnd: HWND): boolean;
begin
  Result := Windows.SetForegroundWindow(HWnd);
end;

function TWinCEWidgetSet.SetParent(hWndChild: HWND; hWndParent: HWND): HWND;
begin
  Result := Windows.SetParent(hWndChild,hWndParent);
end;

function TWinCEWidgetSet.SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean;
{$ifdef Win32WithWin32Menus}
begin
  Result := Windows.SetMenu(AWindowHandle, AMenuHandle);
end;
{$else}
var
  lLCLMenu: TMenu;
  i: Integer;
begin
  {$ifdef VerboseWinCEMenu}
  DebugLn('[TWinCEWidgetSet.SetMenu]');
  {$endif}

  Result := False;

  if (AMenuHandle = 0) or (MenuLCLObjectList.Count = 0) then
    lLCLMenu := nil
  else
  begin
    for i := 0 to WinCEWSMenus.MenuHandleList.Count - 1 do
      if WinCEWSMenus.MenuHandleList.Items[i] = Pointer(AMenuHandle) then Break;
    lLCLMenu := TMenu(MenuLCLObjectList.Items[i]);
  end;

  {$ifdef Win32}
  CeSetMenuDesktop(AWindowHandle, AMenuHandle, lLCLMenu);
  {$else}
  CeSetMenu(AWindowHandle, AMenuHandle, lLCLMenu);
  {$endif}
  AddToChangedMenus(AWindowHandle);
  Result := True;
end;
{$endif}


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

  MWE: that is not really needed anymore since the RemoveProp is now implemented
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetProp(Handle: hwnd; Str: PChar; Data: Pointer): Boolean;
var
  WideStr: widestring;
begin
  WideStr := UTF8Decode(String(str));
  {$ifdef win32}
  Result := Boolean(Windows.SetPropW(Handle, PWideChar(WideStr), Windows.HANDLE(Data)));
  {$else}
  Result := Boolean(Windows.SetProp(Handle, PWideChar(WideStr), Windows.HANDLE(Data)));
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  SetROP2
  Params:  DC			- Device Context
           Mode		- Foreground mixing mode

  Returns: 0 if unsuccessful or the old Mode if successful
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetROP2(DC: HDC; Mode: Integer): Integer;
begin
  result := Windows.SetROP2(DC, Mode);
end;

{------------------------------------------------------------------------------
  Method:  SetScrollInfo
  Params:  Handle     - handle of window with scroll bar
           SBStyle    - scroll bar flag
           ScrollInfo - record with scroll parameters
           BRedraw    - is the scroll bar is redrawn?
  Returns: The new position value

  Sets the parameters of a scroll bar.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetScrollInfo(Handle: HWND; SBStyle: Integer; ScrollInfo: TScrollInfo; BRedraw: Boolean): Integer;
var
  dwStyle: DWORD;
  newPos: Integer; // New effective Pos, not the same as the desired Pos from ScrollInfo
  WasScrollBarVisible: Boolean;
  WindowInfo: PWindowInfo;
begin
  Result := 0;

  // We get some information before calling SetScrollInfo because it changes
  // the GWL_STYLE under Windows CE
  dwStyle := GetWindowLong(Handle, GWL_STYLE);
  WasScrollBarVisible := LCLIntf.GetScrollbarVisible(Handle, SBStyle);

  //DebugLn(Format(
  //  'Trace:> [TWinCEWidgetSet.SetScrollInfo] Mask:0x%x, ' +
  //  'Min:%d, Max:%d, BRedraw:%d, Pos:%d WasScrollBarVisible=%d',
  //  [ScrollInfo.FMask, ScrollInfo.NMin, ScrollInfo.NMax,
  //  Integer(BRedraw), ScrollInfo.NPos, Integer(WasScrollBarVisible)]));

  // Windows CE also shows the scrollbar if you use SetScrollInfo, and
  // if you hide it back, it will move the position to zero.
  //
  // Setting Pos to a valid, non-zero value with a hidden scrollbar will
  // show a non-working scrollbar. Hiding this scrollbar will revert the
  // position to zero.
  // In this case we also get mixed drawings of the new and old position,
  // regardless if you call SetScrollInfo or if you hide the scrollbar
  //
  // Tested on WM 6
  // See bug http://bugs.freepascal.org/view.php?id=14823
  if (ScrollInfo.fMask and SIF_POS <> 0) and (ScrollInfo.nPos > 0) and
    (not WasScrollBarVisible) then
  begin
    //DebugLn('A [TWinCEWidgetSet.SetScrollInfo]');

    WindowInfo := GetWindowInfo(Handle);
    case SBStyle of
      SB_VERT: Windows.SendMessage(Handle, WM_VSCROLL, SB_THUMBPOSITION, 0);
      SB_HORZ: Windows.SendMessage(Handle, WM_HSCROLL, SB_THUMBPOSITION, 0);
      SB_BOTH:
      begin
        Windows.SendMessage(Handle, WM_VSCROLL, SB_THUMBPOSITION, 0);
        Windows.SendMessage(Handle, WM_HSCROLL, SB_THUMBPOSITION, 0);
      end;
    end;
    if WindowInfo^.WinControl <> nil then
    begin
      WindowInfo^.WinControl.Invalidate;
    end;
    Exit;
  end;

  // The actual operation
  ScrollInfo.cbSize:=sizeof(ScrollInfo);
  if (ScrollInfo.fMask and SIF_Range > 0) then
    ScrollInfo.nMax := Max(ScrollInfo.nMin, ScrollInfo.nMax - 1);
  Result := Windows.SetScrollInfo(Handle, SBStyle, @ScrollInfo, BRedraw);

  // See previous comments
  newPos := Windows.GetScrollPos(Handle, SBStyle);
  if (ScrollInfo.fMask and SIF_POS <> 0) and (newPos = 0) and
    (dwStyle <> GetWindowLong(Handle, GWL_STYLE)) then
  begin
    SetWindowLong(Handle, GWL_STYLE, dwStyle);
  end;
end;

{------------------------------------------------------------------------------
  Method:  SetSysColors
  Params:  CElements    - the number of elements
           LPAElements  - array with element numbers
           LPARGBValues - array with colors
  Returns: 0 if unsuccesful

  The SetSysColors function sets the colors for one or more display elements.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetSysColors(CElements: Integer; const LPAElements; const LPARGBValues): Boolean;
begin
  Result := Boolean(Windows.SetSysColors(CElements, PInteger(@LPAElements)^, LPColorRef(@LPARGBValues)^));
end;

{------------------------------------------------------------------------------
  Method:  SetTextCharacterExtra
  Params: _HDC       - handle of device context
          NCharExtra - extra-space value
  Returns: the previous intercharacter spacing

  Sets the intercharacter spacing.
 ------------------------------------------------------------------------------}
{function TWinCEWidgetSet.SetTextCharacterExtra(_hdc: hdc; nCharExtra: Integer
  ): Integer;
begin
  Result:=inherited SetTextCharacterExtra(_hdc, nCharExtra);
end;}

{------------------------------------------------------------------------------
  Method:  SetTextColor
  Params:  DC    - Identifies the device context.
           Color - Specifies the color of the text.
  Returns: The previous color if succesful, CLR_INVALID otherwise

  The SetTextColor function sets the text color for the specified device
  context to the specified color.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.SetTextColor] DC: 0x%x Color: %8x', [Integer(DC), Color]));
  Result := Windows.SetTextColor(DC, ColorToRGB(Color));
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.SetTextColor] DC: 0x%x Color: %8x --> %8x', [Integer(DC), Color, Result]));
end;

{ This routine isn't used directly by the LCL }
function TWinCEWidgetSet.SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean;
begin
  Result := Boolean(Windows.SetViewPortOrgEx(DC, NewX, NewY, LPPoint(OldPoint)));
end;

{------------------------------------------------------------------------------
  Procedure: SetWindowLong
  Params:  Handle  - handle of window
           Idx     - value to set
           NewLong - new value
  Returns: Nothing

  Changes an attribute of the specified window.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetWindowLong(Handle: HWND; Idx: Integer; NewLong: PtrInt): PtrInt;
begin
  //TODO: Finish this;
  //DebugLn(Format('Trace:> [TWinCEWidgetSet.SETWINDOWLONG] HWND: 0x%x, Idx: 0x%x(%d), Value: 0x%x(%d)', [Handle, Idx, Idx, NewLong, NewLong]));
  Result := Windows.SetWindowLong(Handle, Idx, NewLong);
  //DebugLn(Format('Trace:< [TWinCEWidgetSet.SETWINDOWLONG] HWND: 0x%x, Idx: 0x%x(%d), Value: 0x%x(%d) --> 0x%x(%d)', [Handle, Idx, Idx, NewLong, NewLong, Result, Result]));
end;

{------------------------------------------------------------------------------
  Method:  SetWindowOrgEx
  Params:  DC    - handle of device context
           NewX  - new x-coordinate of window origin
           NewY  - new y-coordinate of window origin
           Point - record receiving original origin
  Returns: Whether the call was successful

  Sets the window origin of the device context by using the specified coordinates.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetWindowOrgEx(DC: HDC; NewX, NewY: Integer;
  OldPoint: PPoint): Boolean;
begin
  {$ifdef Win32}
    Result:= Windows.SetWindowOrgEx(dc, NewX, NewY, OldPoint);
  {$else}
    if WinExt.SetWindowOrgEx <> nil then
      Result:= WinExt.SetWindowOrgEx(dc, NewX, NewY, OldPoint)
    else
      Result := False;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  SetWindowPos
  Params: HWnd            - handle of window
          HWndInsertAfter - placement-order handle
          X               - horizontal position
          Y               - vertical position
          CX              - width
          CY              - height
          UFlags          - window-positioning flags
  Returns: if the function succeeds

  Changes the size, position, and Z order of a child, pop-up, or top-level
  window.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.SetWindowPos(HWnd: HWND; HWndInsertAfter: HWND; X, Y, CX, CY: Integer; UFlags: UINT): Boolean;
var
  Style, ExStyle: Integer;
  OldRect, OldClientRect: Windows.RECT;
  WindowInfo: PWindowInfo;
  {$IFDEF VerboseSizeMsg}
  lName: string;
  {$ENDIF}
begin
  //debugln('[TWinCEWidgetSet.SetWindowPos] Top=',HWndInsertAfter=HWND_TOP);
  Style := Windows.GetWindowLong(HWnd, GWL_STYLE);
  ExStyle := Windows.GetWindowLong(HWnd, GWL_EXSTYLE);
  Windows.GetWindowRect(HWnd, @OldRect);
  Windows.GetClientRect(HWnd, @OldClientRect);
  WindowInfo := winceproc.GetWindowInfo(HWnd);

  if Assigned(WindowInfo) and (WindowInfo^.AWinControl is TCustomForm) then
  begin
    if (Style and WS_BORDER) <> 0 then
    begin
      // convert top level lcl window coordinaties to win32 coord
      // add twice, top+bottom border
      Inc(CX, 2*Windows.GetSystemMetrics(SM_CXSIZEFRAME));
      Inc(CY, 2*Windows.GetSystemMetrics(SM_CYSIZEFRAME));
    end;
    if (Style and WS_CAPTION) <> 0 then
      if (ExStyle and WS_EX_TOOLWINDOW) <> 0 then
        Inc(CY, Windows.GetSystemMetrics(SM_CYSMCAPTION))
      else
        Inc(CY, Windows.GetSystemMetrics(SM_CYCAPTION));
  end;

  Result := Boolean(Windows.SetWindowPos(HWnd, HWndInsertAfter, X, Y, CX, CY, UFlags));

  {$IFDEF VerboseSizeMsg}
  if Assigned(WindowInfo) and (WindowInfo^.AWinControl <> nil) then
   lName := WindowInfo^.AWinControl.Name
  else lName := 'NIL';
  DebugLn(
    Format('[TWinCEWidgetSet.SetWindowPos]: Name:%s HWnd:%d Pos x:%d y:%d w:%d h:%d',
    [lName, HWnd, X, Y, CX, CY]));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  ShowCaret
  Params:  HWnd - handle of window with caret
  Returns: if the function succeeds

  Makes the caret visible on the screen at the caret's current position.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ShowCaret(hWnd: HWND): Boolean;
begin
  Result := Boolean(Windows.ShowCaret(HWnd));
end;

{------------------------------------------------------------------------------
  Method:   ShowScrollBar
  Params:  Handle - handle of window with scroll bar
           WBar   - scroll bar flag
           BShow  - is the scroll bar visible?
  Returns: if the function succeeds

  Shows or hides the specified scroll bar.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer;
  bShow: Boolean): Boolean;
var
  dwStyle: DWORD;
begin
  Result := True;
  dwStyle := GetWindowLong(Handle, GWL_STYLE);
  case wBar of
    SB_BOTH:
      if bShow then
        SetWindowLong(Handle, GWL_STYLE, dwStyle or WS_VSCROLL or WS_HSCROLL)
      else
        SetWindowLong(Handle, GWL_STYLE, (dwStyle and not WS_VSCROLL) and not WS_HSCROLL);
    SB_VERT:
      if bShow then
        SetWindowLong(Handle, GWL_STYLE, dwStyle or WS_VSCROLL)
      else
        SetWindowLong(Handle, GWL_STYLE, dwStyle and not WS_VSCROLL);
    SB_HORZ:
      if bShow then
        SetWindowLong(Handle, GWL_STYLE, dwStyle or WS_HSCROLL)
      else
        SetWindowLong(Handle, GWL_STYLE, dwStyle and not WS_HSCROLL);
  else
    Result := False;
  end;
end;

{------------------------------------------------------------------------------
  Method: ShowWindow
  Params: hWnd     - Window handle
          nCmdShow - (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED)
  Returns: if the function succeeds
------------------------------------------------------------------------------}
function TWinCEWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  Result := Boolean(Windows.ShowWindow(hWnd, nCmdShow));
end;

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
  destination rectangle using the specified raster operation. if needed it
  resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
begin
  if IsAlphaDC(DestDC) or IsAlphaDC(SrcDC) then
    Result := StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, 0, 0, 0, Rop)
  else
    Result := Boolean(Windows.StretchBlt(DestDc, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, Rop));
end;

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
  destination rectangle using the specified mask and raster operations. if
  needed it resizes the bitmap to fit the dimensions of the destination
  rectangle.  Sizing is done according to the stretching mode currently set in
  the destination device context.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;

function CreatePremultipliedBitmap(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; out AAlphaBmp: HBITMAP): Boolean;
var
  _I : Integer;
  Data : Pointer = nil;
  DestData : Pointer = nil;
  Pixel : PRGBAQuad;
  ByteCount: PtrUInt;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[0..3] of Cardinal; // reserve extra color for colormasks
  end;
  HasAlpha0, HasAlphaN, HasAlpha255: Boolean;

begin
  Result := False;
  // process only requested rectangle
  if not GetBitmapBytes(ABitmap, Rect(XSrc, YSrc, XSrc+SrcWidth, YSrc+SrcHeight), rileDWordBoundary, Data, ByteCount) then Exit;

  HasAlpha0 := False;
  HasAlphaN := False;
  HasAlpha255 := False;
  Pixel := Data;
  For _I := 1 To ByteCount shr 2 Do
  begin
    //Pixel^.Alpha := (Pixel^.Alpha * Alpha) div 255;
    If Pixel^.Alpha = 255 Then
      HasAlpha255 := True
    else
      If Pixel^.Alpha = 0 Then
      begin
        ZeroMemory(Pixel, SizeOf(TRGBAQuad));
        HasAlpha0 := True;
      end
      else
      begin
        Pixel^.Red   := (Pixel^.Red   * Pixel^.Alpha) div 255;
        Pixel^.Green := (Pixel^.Green * Pixel^.Alpha) div 255;
        Pixel^.Blue  := (Pixel^.Blue  * Pixel^.Alpha) div 255;
        HasAlphaN := True;
      end;
    //
    Inc(Pixel);
  end;

  // only create bitmap when not opaque or not fully transparent
  // (all zero alpha is unlikly for alpha bitmap, so it is probably a bitmap without alpha channel)
  Result := HasAlphaN or (HasAlpha0 and HasAlpha255);
  if Result then
  begin
    ZeroMemory(@Info.Header, SizeOf(Info.Header));
    Info.Header.biSize := SizeOf(Info.Header);
    Info.Header.biWidth := SrcWidth;
    Info.Header.biHeight := -SrcHeight;
    Info.Header.biPlanes := 1;
    Info.Header.biBitCount := 32;
    Info.Header.biSizeImage := (SrcWidth * SrcHeight) shl 2;
    Info.Header.biCompression := BI_BITFIELDS; // CE only supports bitfields
    Info.Colors[0] := $FF0000; {le-red}
    Info.Colors[1] := $00FF00; {le-green}
    Info.Colors[2] := $0000FF; {le-blue}
    AAlphaBmp := Windows.CreateDIBSection({SrcDC}0, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS, DestData, 0, 0);
    Result := (AAlphaBmp <> 0) and (Data <> nil) and (DestData <> nil);
    if Result Then MoveMemory(DestData, Data, ByteCount);
  end;

  if Data <> nil Then FreeMem(Data, ByteCount);
end;

var
  MaskDC, CopyDC, AlphaDC: HDC;
  MaskObj, CopyObj, AlphaObj : HGDIOBJ;
  PrevTextColor, PrevBkColor : COLORREF;
  WinBmp: Windows.TBitmap;
  Bmp, CopyBmp, AlphaBmp : HBITMAP;
  HasAlpha: Boolean;
  Blend: TBlendFunction;

begin
  Result := False;
  //if Alpha = 0 then Exit;
  // check if the Src has an alpha channel
  Bmp := Windows.GetCurrentObject(SrcDC, OBJ_BITMAP);
  // get info
  HasAlpha := (Windows.GetObject(Bmp, SizeOf(WinBmp), @WinBmp) <> 0)
          and (WinBmp.bmBitsPixel = 32)
          and CreatePremultipliedBitmap(WinBmp, Bmp, AlphaBmp);

  if HasAlpha then
  begin
    // premultiply pixels
    AlphaDC := Windows.CreateCompatibleDC(SrcDC);
    AlphaObj := Windows.SelectObject(AlphaDC, AlphaBmp);

    // init blendfunction
    Blend.BlendOp := AC_SRC_OVER;
    Blend.BlendFlags := 0;
    Blend.SourceConstantAlpha := 255;
    Blend.AlphaFormat := AC_SRC_ALPHA;
  end;

  Windows.SetBrushOrgEx(DestDC, 0, 0, nil);
  if Mask = 0 then
  begin
    if HasAlpha then
    begin
      Result := WinCEExtra.AlphaBlend(DestDC, X, Y, Width, Height, AlphaDC, 0, 0, SrcWidth, SrcHeight, Blend);
    end
    else
    begin
      if (Width = SrcWidth) and (Height = SrcHeight) then
        Result := Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SRCCOPY)
      else
        Result := Windows.StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SrcWidth, SrcHeight, SRCCOPY);
    end;
  end
  else
  begin
    MaskDC := Windows.CreateCompatibleDC(DestDC);
    MaskObj := Windows.SelectObject(MaskDC, Mask);

    PrevTextColor := Windows.SetTextColor(DestDC, $00000000);
    PrevBkColor := Windows.SetBkColor(DestDC, $00FFFFFF);

    if HasAlpha then
    begin
      // create copy of masked destination
      CopyDC := Windows.CreateCompatibleDC(DestDC);
      CopyBmp := Windows.CreateCompatibleBitmap(DestDC, Width, Height);
      CopyObj := Windows.SelectObject(CopyDC, CopyBmp);
      Windows.BitBlt(CopyDC, 0, 0, Width, Height, DestDC, X, Y, SRCCOPY);
      // wipe non masked area -> white
      Windows.SetTextColor(CopyDC, $00FFFFFF);
      Windows.SetBkColor(CopyDC, $00000000);
      if (Width = SrcWidth) and (Height = SrcHeight)
      then Windows.BitBlt(CopyDC, 0, 0, Width, Height, MaskDC, XSrc, YSrc, SRCPAINT)
      else Windows.StretchBlt(CopyDC, 0, 0, Width, Height, MaskDC, XSrc, YSrc, SrcWidth, SrcHeight, SRCPAINT);

      // copy source
      WinCEExtra.AlphaBlend(DestDC, X, Y, Width, Height, AlphaDC, 0, 0, SrcWidth, SrcHeight, Blend);
      // wipe masked area -> white
      if (Width = SrcWidth) and (Height = SrcHeight)
      then Windows.BitBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SRCPAINT)
      else Windows.StretchBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SrcWidth, SrcHeight, SRCPAINT);

      // paint copied destination
      Windows.BitBlt(DestDC, X, Y, Width, Height, CopyDC, 0, 0, SRCAND);

      // Restore stuff
      Windows.SelectObject(CopyDC, CopyObj);
      Windows.DeleteObject(CopyBmp);
      Windows.DeleteDC(CopyDC);
    end
    else
    begin
      if (Width = SrcWidth) and (Height = SrcHeight) then
      begin
        Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SRCINVERT);
        Windows.BitBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SRCAND);
        Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SRCINVERT);
      end
      else
      begin
        Windows.StretchBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SrcWidth, SrcHeight, SRCINVERT);
        Windows.StretchBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SrcWidth, SrcHeight, SRCAND);
        Windows.StretchBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SrcWidth, SrcHeight, SRCINVERT);
      end;
    end;
    Windows.SetTextColor(DestDC, PrevTextColor);
    Windows.SetBkColor(DestDC, PrevBkColor);
    Windows.SelectObject(MaskDC, MaskObj);
    Windows.DeleteDC(MaskDC);
  end;

  if HasAlpha then
  begin
    Windows.SelectObject(AlphaDC, AlphaObj);
    Windows.DeleteObject(AlphaBmp);
    Windows.DeleteDC(AlphaDC);
  end;

  Result := true;
end;

function TWinCEWidgetSet.SystemParametersInfo(uiAction: DWord; uiParam: DWord;
  pvParam: Pointer; fWinIni: DWord): LongBool;
begin
  Result := Windows.SystemParametersInfo(uiAction, uiParam, pvParam, fWinIni);
end;

{------------------------------------------------------------------------------
  Method:  TextOut
  Params: DC    - handle of device context
          X     - x-coordinate of starting position
          Y     - y-coordinate of starting position
          Str   - string
          Count - number of characters in string
  Returns: if the function succeeds

  Writes a character string at the specified location, using the currently
  selected font.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.TextOut(DC: HDC; X, Y: Integer; Str: PChar; Count: Integer): Boolean;
var
  WS: WideString;
begin
  // There is no Windows.TextOut in Windows CE, so we improvise with other routines
  WS := UTF8ToUTF16(Copy(Str, 1, Count));
  Result := Boolean(Windows.ExtTextOutW(DC, X, Y, 0, nil, PWideChar(WS), Length(WS), nil));
end;

{------------------------------------------------------------------------------
  Method:  WindowFromPoint
  Params: Point: Specifies the x and y Coords
  Returns: The handle of the window.

  Retrieves the handle of the window that contains the specified point.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.WindowFromPoint(Point: TPoint): HWND;
var
  ProcessID: DWORD;
begin
  Result := Windows.WindowFromPoint(Windows.POINT(Point));
  // check if window created by this process
  Windows.GetWindowThreadProcessId(Result, @ProcessID);
  if ProcessID <> Windows.GetCurrentProcessID then
    Result := 0;
end;

procedure TWinCEWidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
begin
  { An OS Compatible TCriticalSection needs to be defined}
  if CritSection<>0 then
  begin
    Windows.DeleteCriticalSection(LPCRITICAL_SECTION(CritSection));
    try
      Dispose(LPCRITICAL_SECTION(CritSection));
    finally
      CritSection := 0;
    end;
  end;
end;

procedure TWinCEWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
begin
  { An OS Compatible TCriticalSection needs to be defined}
  Windows.ENTERCRITICALSECTION(LPCRITICAL_SECTION(CritSection));
end;

{We interprete CritSection as a pointer to a LPCRITICAL_SECTION structure}
procedure TWinCEWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
var
  Crit : LPCRITICAL_SECTION;
begin
  { An OS Compatible TCriticalSection needs to be defined}
  if CritSection <> 0 then
    DeleteCriticalSection(CritSection);
  New(Crit);
  Windows.InitializeCriticalSection(Crit);
  CritSection := TCriticalSection(Crit);
end;

procedure TWinCEWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
begin
  { An OS Compatible TCriticalSection needs to be defined}
  Windows.LeaveCriticalSection(LPCRITICAL_SECTION(CritSection));
end;

//##apiwiz##epi##   // do not remove
