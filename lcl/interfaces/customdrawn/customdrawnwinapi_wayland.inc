{%MainUnit customdrawnint.pas}

{ Wayland-specific TCDWidgetSet winapi-shim methods.
  The generic customdrawnwinapi.inc covers most GDI-style operations
  (which target the abstract LazCanvas, not anything host-specific);
  this file fills in the host-specific operations and explicit
  fallbacks that the LCL expects per host. }

function TCDWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
var
  CDC: TCDBaseControl;
  WC: TWinControl;
begin
  { Wayland forbids the client from knowing its absolute screen
    position, so this returns *parent-form-local* coordinates rather
    than true screen coords. That's still a meaningful upgrade over
    leaving P unchanged: it accumulates the (Left, Top) of every
    TWinControl ancestor up to the form, which is enough for callers within
    a single window, such as xdg_popup placement and hit-test math.
    This is not a substitute for true global screen coordinates; Wayland does not expose those to clients. }
  Result := True;
  if Handle = 0 then Exit;
  CDC := TCDBaseControl(Handle);
  if CDC is TCDWinControl then WC := TCDWinControl(CDC).WinControl
  else                         WC := nil;
  while (WC <> nil) and (WC.Parent <> nil) do
  begin
    Inc(P.X, WC.Left);
    Inc(P.Y, WC.Top);
    WC := WC.Parent;
  end;
end;

{ Read everything from Fd into Stream; close Fd. The compositor writes
  the offered bytes and closes its end of the pipe; we read until EOF.
  Used by ClipboardGetData and reusable for any "read until EOF then
  close" pattern. }
procedure WLDrainFdIntoStream(Fd: cint; Stream: TStream);
var
  Buf: array[0..4095] of Byte;
  N:   ssize_t;
begin
  try
    repeat
      N := FpRead(Fd, Buf, SizeOf(Buf));
      if N > 0 then
        Stream.Write(Buf, N);
    until N <= 0;
  finally
    FpClose(Fd);
  end;
end;

{ Mime synonyms LCL's pcfText ('text/plain') maps to on Wayland/X.
  When other apps offer the clipboard they typically advertise some
  combination of these, NOT plain 'text/plain' on its own. Same idea
  as gtk2winapi.inc:360-381 (CheckAtomFormat list). }
const
  WlTextMimes: array[0..4] of string = (
    'text/plain;charset=utf-8',
    'UTF8_STRING',
    'text/plain',
    'STRING',
    'TEXT'
  );

function WLIsTextMime(const M: AnsiString): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(WlTextMimes) do
    if M = WlTextMimes[i] then Exit(True);
  Result := False;
end;

function WLClipFormatForMime(const MimeType: AnsiString;
  const Formats: array of TClipboardFormat): TClipboardFormat;
var
  i: Integer;
begin
  Result := 0;
  if WLIsTextMime(MimeType) then
  begin
    for i := 0 to High(Formats) do
      if ClipboardFormatToMimeType(Formats[i]) = 'text/plain' then
        Exit(Formats[i]);
  end
  else
    for i := 0 to High(Formats) do
      if ClipboardFormatToMimeType(Formats[i]) = MimeType then
        Exit(Formats[i]);
end;

{ wl_data_source.send fired -- a clipboard receiver picked our Mime
  type and is waiting for bytes on Fd. Resolve Mime back to a
  TClipboardFormat, hand it to the user's request callback (which
  fills a TStream), shovel the stream into the pipe, then close it.
  The source object survives the send: the same data may be requested
  multiple times for the same selection (different paste targets). }
procedure TCDWidgetSet.WLClipSourceSend(Source: TWlDataSource;
  const MimeType: AnsiString; Fd: cint);
var
  Stream: TMemoryStream;
  FormatID: TClipboardFormat;
  Buf: PByte;
  Remaining, Sent: ssize_t;
begin
  if not Assigned(FWlClipCallback) then
  begin
    FpClose(Fd);
    Exit;
  end;
  { Translate Mime back to a registered TClipboardFormat. We may have
    offered 'text/plain' to LCL but advertised several text aliases on
    the wire (UTF8_STRING etc.); any of those should map back to the
    single registered text format. Non-text formats must match exactly. }
  FormatID := WLClipFormatForMime(MimeType, FWlClipFormats);
  Stream := TMemoryStream.Create;
  try
    if FormatID <> 0 then
      FWlClipCallback(FormatID, Stream);
    Stream.Position := 0;
    Buf := PByte(Stream.Memory);
    Remaining := Stream.Size;
    while Remaining > 0 do
    begin
      Sent := FpWrite(Fd, Buf^, Remaining);
      if Sent <= 0 then Break;       { receiver closed early or pipe error }
      Inc(Buf, Sent);
      Dec(Remaining, Sent);
    end;
  finally
    Stream.Free;
    FpClose(Fd);
  end;
end;

procedure TCDWidgetSet.WLClipSourceCancelled(Source: TWlDataSource);
var
  Callback: TClipboardRequestEvent;
begin
  { Server replaced our selection (another app called set_selection,
    or we did). Drop our reference and destroy. If Source is already
    stale (we set a new selection and destroyed it ourselves), do
    nothing -- destroying twice triggers wl_display "invalid object". }
  if FWlClipSource = Source then
  begin
    Callback := FWlClipCallback;
    FWlClipSource := nil;
    FWlClipCallback := nil;
    SetLength(FWlClipFormats, 0);
    if Assigned(Callback) then
      Callback(0, nil);
    Source.DestroyRequest;
  end;
end;

function TCDWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): Boolean;
var
  Mime: string;
  Pipe: array[0..1] of cint;
  i: Integer;
  OwnFormatID: TClipboardFormat;
begin
  Result := False;
  { Only the system clipboard maps to wl_data_device.selection.
    Primary selection would need zwp_primary_selection_v1 (separate
    protocol); we return False for that. ctSecondarySelection has no
    Wayland equivalent. }
  if ClipboardType <> ctClipboard then Exit;
  if (FWlDataDevice = nil) or (FWlDataDevice.CurrentSelection = nil) then Exit;

  Mime := ClipboardFormatToMimeType(FormatID);
  if Mime = '' then Exit;
  { Receiver-side mime negotiation. For text/plain, walk our synonym
    list in priority order (UTF-8 variants first) and use whichever
    the offer advertises -- other apps rarely advertise bare 'text/plain'.
    For non-text formats, require exact match. }
  if Mime = 'text/plain' then
  begin
    Mime := '';
    for i := 0 to High(WlTextMimes) do
      if FWlDataDevice.CurrentSelection.MimeTypes.IndexOf(WlTextMimes[i]) >= 0 then
      begin
        Mime := WlTextMimes[i];
        Break;
      end;
    if Mime = '' then Exit;
  end
  else
    if FWlDataDevice.CurrentSelection.MimeTypes.IndexOf(Mime) < 0 then Exit;

  { If the selection is ours, serve it directly. Asking the compositor
    for our own offer can deadlock: receive() queues a wl_data_source.send
    event on this same connection, but the synchronous pipe read below
    would block before the event can be dispatched. }
  if (FWlClipSource <> nil) and Assigned(FWlClipCallback) then
  begin
    OwnFormatID := WLClipFormatForMime(Mime, FWlClipFormats);
    if OwnFormatID <> 0 then
    begin
      FWlClipCallback(OwnFormatID, Stream);
      Result := True;
      Exit;
    end;
  end;

  { Open a pipe; write end goes to the compositor via SCM_RIGHTS, we
    read from the read end. The compositor writes the bytes and
    closes its end -- we get EOF when done. }
  if FpPipe(Pipe) <> 0 then Exit;

  FWlDataDevice.CurrentSelection.Receive(Mime, Pipe[1]);
  FWlDisplay.Connection.Flush;
  FpClose(Pipe[1]);  { our copy of the write end -- we don't write }
  WLDrainFdIntoStream(Pipe[0], Stream);
  Result := True;
end;

function TCDWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: Integer;
  Formats: PClipboardFormat): Boolean;
var
  i, j: Integer;
  Mime: string;
begin
  Result := False;
  if ClipboardType <> ctClipboard then Exit;
  if (FWlDataDeviceManager = nil) or (FWlDataDevice = nil) then Exit;

  { Replace any previous source. Calling set_selection with a new
    source automatically cancels the old one; the cancelled event will
    arrive asynchronously and our handler will tidy up. }
  if FWlClipSource <> nil then
  begin
    FWlClipSource.DestroyRequest;
    FWlClipSource := nil;
  end;

  if FormatCount = 0 then
  begin
    { OnRequestProc=nil and FormatCount=0 is the LCL "release ownership"
      idiom. Honour it by clearing the selection. }
    FWlDataDevice.SetSelection(nil, FWlLastInputSerial);
    FWlDisplay.Connection.Flush;
    FWlClipCallback := nil;
    SetLength(FWlClipFormats, 0);
    Result := True;
    Exit;
  end;

  FWlClipCallback := OnRequestProc;
  SetLength(FWlClipFormats, FormatCount);
  for i := 0 to FormatCount - 1 do
    FWlClipFormats[i] := Formats[i];

  FWlClipSource := FWlDataDeviceManager.CreateDataSource;
  FWlClipSource.OnSend      := @WLClipSourceSend;
  FWlClipSource.OnCancelled := @WLClipSourceCancelled;
  for i := 0 to FormatCount - 1 do
  begin
    Mime := ClipboardFormatToMimeType(Formats[i]);
    { For text, advertise every synonym so other apps (which usually
      ask for text/plain;charset=utf-8 or UTF8_STRING, not bare
      text/plain) can paste from us. WLClipSourceSend maps any of
      these back to our single registered text/plain format. }
    if Mime = 'text/plain' then
      for j := 0 to High(WlTextMimes) do
        FWlClipSource.Offer(WlTextMimes[j])
    else
      FWlClipSource.Offer(Mime);
  end;

  FWlDataDevice.SetSelection(FWlClipSource, FWlLastInputSerial);
  FWlDisplay.Connection.Flush;
  Result := True;
end;

{ Run fc-match with a format that prints the matched font file and return
  its trimmed stdout. Use fontconfig here so pattern matching follows
  fontconfig configuration, aliases and substitutions. }

function CDWLFcMatch(const Pattern: string): string;
var
  P: TProcess;
  Buf: array[0..1023] of AnsiChar;
  N: LongInt;
  S: AnsiString;
begin
  Result := '';
  P := TProcess.Create(nil);
  try
    try
      P.Executable := 'fc-match';
      P.Parameters.Add('-f');
      P.Parameters.Add('%{file}');
      P.Parameters.Add(Pattern);
      P.Options := [poWaitOnExit, poUsePipes, poStderrToOutPut];
      P.Execute;
      S := '';
      while True do
      begin
        N := P.Output.Read(Buf, SizeOf(Buf));
        if N <= 0 then Break;
        SetString(Result, PAnsiChar(@Buf[0]), N);
        S := S + Result;
      end;
      Result := Trim(S);
      if (P.ExitStatus <> 0) or (not FileExists(Result)) then
        Result := '';
    except
      Result := '';   { fc-match not on PATH or any other failure -- skip. }
    end;
  finally
    P.Free;
  end;
end;

function CDWLFindAnyFont: string;
begin
  Result := CDWLFcMatch('sans-serif');
  if Result = '' then
    Result := CDWLFcMatch('sans');
  if Result = '' then
    Result := CDWLFcMatch('');   { fontconfig's "any font" }
end;

var
  CDWLCachedFontPath: string = '';
  CDWLFontPathSearched: Boolean = False;
  CDWLFamilyCache: TStringList = nil;   { family name -> resolved .ttf path }

procedure TCDWidgetSet.BackendListFontPaths(var AFontPaths: TStringList;
  var AFontList: THashedStringList);
const
  Candidates: array[0..7] of string = (
    '/run/current-system/profile/share/fonts',
    '/run/current-system/profile/share/fonts/truetype',
    '/usr/share/fonts',
    '/usr/share/fonts/truetype',
    '/usr/share/fonts/TTF',
    '/usr/share/fonts/dejavu',
    '/usr/share/fonts/liberation',
    '/usr/share/fonts/opentype'
  );
var
  i: Integer;
begin
  { FIXME: A real implementation would parse fontconfig to get the
    full search path AND would map font family names -> file paths
    (so that requesting "Liberation Sans" returns the right TTF).
    For now we register the standard locations as paths to scan;
    BackendGetFontPath does the actual file pick. }
  for i := 0 to High(Candidates) do
    if DirectoryExists(Candidates[i]) then
      AFontPaths.Add(IncludeTrailingPathDelimiter(Candidates[i]));
end;

function TCDWidgetSet.BackendGetFontPath(const LogFont: TLogFont;
  const LongFontName: string): string;
var
  Family: string;
  Idx: Integer;
begin
  { Resolve the requested family name through fc-match so that e.g.
    "Noto Sans CJK JP" actually picks the CJK .otf rather than
    falling back to a system sans-serif that may render hiragana /
    kanji as tofu. Empty / unknown families fall
    back to whatever fontconfig's "sans-serif" alias resolves to.
    Results are cached per family to keep the per-redraw cost a
    string lookup, not a child process. }
  Family := LongFontName;
  if Family = '' then
    Family := LogFont.lfFaceName;
  if Family = 'default' then
    Family := '';

  if CDWLFamilyCache = nil then
    CDWLFamilyCache := TStringList.Create;
  Idx := CDWLFamilyCache.IndexOfName(Family);
  if Idx >= 0 then
  begin
    Result := CDWLFamilyCache.ValueFromIndex[Idx];
    Exit;
  end;

  { FPC's pure-Pascal freetype reader only opens plain .ttf -- it fails
    on .otf (CFF outlines) and .ttc (font collections), even though
    fontconfig will happily return either as a "best match". Constrain
    the query to TrueType so we don't get back a file we can't load. }
  if Family <> '' then
  begin
    Result := CDWLFcMatch(Family + ':fontformat=TrueType');
    if (Result = '') or (LowerCase(ExtractFileExt(Result)) <> '.ttf') then
      Result := '';
  end
  else
    Result := '';

  if Result = '' then
  begin
    if not CDWLFontPathSearched then
    begin
      CDWLCachedFontPath := CDWLFindAnyFont;
      CDWLFontPathSearched := True;
      if CDWLCachedFontPath = '' then
        DebugLn('[WL BackendGetFontPath] no .ttf font found in any standard location')
      else
        DebugLn('[WL BackendGetFontPath] fallback ' + CDWLCachedFontPath);
    end;
    Result := CDWLCachedFontPath;
  end;
  CDWLFamilyCache.Add(Family + '=' + Result);
end;

function TCDWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint;
begin
  { FIXME: no font enumeration; needs fontconfig. }
  Result := 0;
end;

function TCDWidgetSet.BackendGetClientBounds(handle: HWND; var ARect: TRect): Boolean;
var
  WI: TWaylandWindowInfo;
begin
  Result := False;
  if handle = 0 then Exit;
  WI := TWaylandWindowInfo(handle);
  if WI.LCLForm = nil then Exit;
  ARect := Bounds(0, 0, WI.LCLForm.Width, WI.LCLForm.Height);
  Result := True;
end;

function TCDWidgetSet.GetCursorPos(var lpPoint: TPoint): Boolean;
begin
  { Wayland refuses to expose absolute screen coordinates, so the
    "cursor pos" we report here matches our ClientToScreen pseudo-
    convention: surface-local-of-the-pointer-focused window, which for
    a single-toplevel app collapses to form-local. The LCL hint
    mechanism (Application.DoOnMouseMove) gates on this returning
    True -- if we returned False the hint timer never started and
    TControl.Hint was effectively inactive. FindControlAtPosition,
    ScreenToClient etc. round-trip through the same pseudo-convention,
    so the reported position resolves back to the right control. }
  lpPoint.X := FWlPointerX;
  lpPoint.Y := FWlPointerY;
  Result := FWlPointerFocus <> nil;
end;

function TCDWidgetSet.WindowFromPoint(APoint: TPoint): HWND;
begin
  { LCL hint/current-mouse lookup goes through FindLCLWindow ->
    WindowFromPoint. Wayland does not expose absolute window positions
    or compositor hit-testing, so this backend cannot answer arbitrary
    screen points. It can answer the current-pointer case: GetCursorPos
    reports the cached wl_pointer surface-local position, and
    wl_pointer.enter/leave tells us which surface owns that position.
    LCL hint/current-mouse paths use that exact point; return 0 for any
    other APoint rather than pretending we can hit-test it. }
  if (FWlPointerFocus <> nil)
     and (APoint.X = FWlPointerX)
     and (APoint.Y = FWlPointerY) then
    Result := HWND(FWlPointerFocus)
  else
    Result := 0;
end;

{ Pick the "primary" output -- there's no Wayland concept of a primary
  monitor (compositor's job, not ours). First-advertised is the only
  deterministic proxy available to the client. Returns nil if no
  output is bound yet (very early startup, or a headless compositor). }
function TCDWidgetSet.WLPrimaryOutput: TWaylandOutput;
begin
  Result := nil;
  if (FWlOutputs = nil) or (FWlOutputs.Count = 0) then Exit;
  Result := TWaylandOutput(FWlOutputs[0]);
  { mode hasn't arrived yet -- treat as not-ready and let the caller fall
    back. Happens in the tiny window between bind and the first done event. }
  if (Result.CurrentWidthPx <= 0) or (Result.CurrentHeightPx <= 0) then
    Result := nil;
end;

function TCDWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
var
  Out_: TWaylandOutput;
begin
  Out_ := WLPrimaryOutput;
  case Index of
    HORZRES:
      if Out_ <> nil then Result := Out_.CurrentWidthPx  else Result := 1024;
    VERTRES:
      if Out_ <> nil then Result := Out_.CurrentHeightPx else Result := 1024;
    HORZSIZE:
      if (Out_ <> nil) and (Out_.PhysicalWidthMm > 0)
        then Result := Out_.PhysicalWidthMm else Result := 0;
    VERTSIZE:
      if (Out_ <> nil) and (Out_.PhysicalHeightMm > 0)
        then Result := Out_.PhysicalHeightMm else Result := 0;
    LOGPIXELSX, LOGPIXELSY:
      { Wayland output scale is tracked, but this backend does not
        expose it as LCL DPI yet. Report the LCL baseline DPI until
        buffer scale, drawing scale and layout scale are wired together. }
      Result := 96;
    BITSPIXEL:
      Result := 32;        { we always render to ARGB32 buffers }
    PLANES:
      Result := 1;
    NUMCOLORS:
      Result := -1;        { Win32 returns -1 for >256-color displays }
  else
    Result := 0;
  end;
end;

function TCDWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
begin
  { Win32 convention: high bit (0x8000) set if the key is currently
    physically down, low bit (0x0001) set when a toggle key is on.
    We populate these from wl_keyboard.modifiers (ModsDepressed for
    held, ModsLocked for toggled). Keys we don't track (most
    non-modifier keys, plus Scroll Lock, which doesn't have a
    universally-fixed XKB bit) return 0. }
  Result := 0;
  case nVirtKey of
    VK_SHIFT, VK_LSHIFT, VK_RSHIFT:
      if FWlShiftHeld then Result := SmallInt($8000);
    VK_CONTROL, VK_LCONTROL, VK_RCONTROL:
      if FWlCtrlHeld  then Result := SmallInt($8000);
    VK_MENU, VK_LMENU, VK_RMENU:           { Alt }
      if FWlAltHeld   then Result := SmallInt($8000);
    VK_CAPITAL:
      if FWlCapsLocked then Result := 1;
    VK_NUMLOCK:
      if FWlNumLocked  then Result := 1;
  end;
end;

function TCDWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
var
  Out_: TWaylandOutput;
  i: Integer;
  MaxR, MaxB, R, B: LongInt;
begin
  { Border / caption / menu metrics stay zero because Wayland clients
    draw their own decorations (CSD) and we don't draw any decoration
    yet -- when SetBorderIcons / decorations land here, plug in real
    values then. }
  Result := 0;
  case nIndex of
    SM_CXSCREEN, SM_CXFULLSCREEN:
      begin
        Out_ := WLPrimaryOutput;
        if Out_ <> nil then Result := Out_.CurrentWidthPx else Result := 1024;
      end;
    SM_CYSCREEN, SM_CYFULLSCREEN:
      begin
        Out_ := WLPrimaryOutput;
        if Out_ <> nil then Result := Out_.CurrentHeightPx else Result := 1024;
      end;
    SM_CXVIRTUALSCREEN, SM_CYVIRTUALSCREEN:
      begin
        { Bounding box across every output. Wayland places each output at
          a compositor-chosen origin (XPos, YPos); the right/bottom edge
          is XPos + Width / YPos + Height. The virtual screen is the union. }
        if (FWlOutputs = nil) or (FWlOutputs.Count = 0) then
        begin
          Result := 1024;
          Exit;
        end;
        MaxR := 0; MaxB := 0;
        for i := 0 to FWlOutputs.Count - 1 do
        begin
          Out_ := TWaylandOutput(FWlOutputs[i]);
          if (Out_.CurrentWidthPx <= 0) or (Out_.CurrentHeightPx <= 0) then Continue;
          R := Out_.XPos + Out_.CurrentWidthPx;
          B := Out_.YPos + Out_.CurrentHeightPx;
          if R > MaxR then MaxR := R;
          if B > MaxB then MaxB := B;
        end;
        if nIndex = SM_CXVIRTUALSCREEN then Result := MaxR else Result := MaxB;
      end;
    SM_CMONITORS:
      if FWlOutputs <> nil then Result := FWlOutputs.Count else Result := 1;
    { Scrollbar thickness -- referenced by TCustomScrollBar's preferred
      size (CalculatePreferredSize at scrollbar.inc:142) and by TCDScrollBars
      when sizing the per-control overlay bars. Native backends derive this
      from the OS theme; pick a Win2k-ish default that matches what
      TCDDrawerCommon.DrawScrollBar paints. }
    SM_CXVSCROLL, SM_CYVSCROLL,
    SM_CXHSCROLL, SM_CYHSCROLL:
      Result := 16;
  end;
end;

function TCDWidgetSet.BackendGetWindowRelativePosition(Handle: hwnd;
  var Left, Top: Integer): boolean;
begin
  { Wayland clients can't query their own surface position. Report the
    form-local origin; cross-window/screen-relative math is deliberately
    unavailable to normal clients. }
  Left := 0;
  Top  := 0;
  Result := True;
end;

function TCDWidgetSet.BackendGetWindowSize(Handle: hwnd;
  var Width, Height: Integer): boolean;
var
  WI: TWaylandWindowInfo;
begin
  Result := False;
  if Handle = 0 then Exit;
  WI := TWaylandWindowInfo(Handle);
  if WI.LCLForm = nil then Exit;
  Width  := WI.LCLForm.Width;
  Height := WI.LCLForm.Height;
  Result := True;
end;

function TCDWidgetSet.BackendInvalidateRect(aHandle: HWND; Rect: pRect;
  bErase: Boolean): Boolean;
var
  WI: TWaylandWindowInfo;
begin
  { TODO: optimization. This invalidates the whole window regardless of
    Rect; accumulate per-region damage and pass it to
    wl_surface.damage_buffer. }
  Result := False;
  if aHandle = 0 then Exit;
  WI := TWaylandWindowInfo(aHandle);
  WI.PendingDraw := True;
  Result := True;
end;

function TCDWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar;
  uType: Cardinal): integer;
begin
  { Use the widgetset-independent LCL message box. It creates an LCL
    modal dialog instead of relying on a compositor/native toolkit API. }
  Result := DefaultMessageBox(lpText, lpCaption, uType);
end;

function TCDWidgetSet.BackendSetFocus(hWnd: HWND): HWND;
begin
  { TODO: wire xdg_activation_v1 if/when the LCL can provide an
    activation token from recent user input. Arbitrary focus stealing is
    intentionally not available on Wayland. }
  Result := 0;
end;

function TCDWidgetSet.ScreenToClient(Handle: HWND; var P: TPoint): Integer;
begin
  { See ClientToScreen: normal Wayland clients don't know their own
    screen origin, so this inverse transform is unavailable. }
  Result := 0;
end;
