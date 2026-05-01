{%MainUnit customdrawnint.pas}

{ Wayland-specific TCDWidgetSet winapi-shim methods.
  The generic customdrawnwinapi.inc covers most GDI-style operations
  (which target the abstract LazCanvas, not anything host-specific);
  this file fills in the host-specific operations and explicit
  fallbacks that the LCL expects per host. }

function TCDWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
begin
  { FIXME: Wayland forbids the client from knowing its absolute screen
    position; we leave P unchanged and report success. Anything that
    relies on real screen coords (popup placement, drag tracking,
    multi-monitor layout) will be wrong. }
  Result := True;
end;

function TCDWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): Boolean;
begin
  { FIXME: not implemented. Needs wl_data_device_manager (selection) or
    zwp_primary_selection_device_manager_v1 (primary). }
  Result := False;
end;

function TCDWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent; FormatCount: Integer;
  Formats: PClipboardFormat): Boolean;
begin
  { FIXME: not implemented; see ClipboardGetData. }
  Result := False;
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
begin
  { FIXME: returns the same arbitrary TTF for every requested font
    family. Good enough for "the LCL stops crashing" -- bad enough
    that bold/italic/serif/sans all look identical. A real
    implementation should consult fontconfig (fc-match) or parse
    each TTF's NAME table to build a family->path map. }
  if not CDWLFontPathSearched then
  begin
    CDWLCachedFontPath := CDWLFindAnyFont;
    CDWLFontPathSearched := True;
    if CDWLCachedFontPath = '' then
      DebugLn('[WL BackendGetFontPath] no .ttf font found in any standard location')
    else
      DebugLn('[WL BackendGetFontPath] using ' + CDWLCachedFontPath);
  end;
  Result := CDWLCachedFontPath;
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
  { FIXME: no cursor tracking -- needs wl_seat / wl_pointer binding. }
  lpPoint.X := 0;
  lpPoint.Y := 0;
  Result := False;
end;

function TCDWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  { FIXME: hardcoded device caps. Real implementation should pull from
    wl_output (size, scale) and wp_fractional_scale_manager_v1. }
  case Index of
    HORZRES, VERTRES: Result := 1024;
    LOGPIXELSX, LOGPIXELSY: Result := 96;
    BITSPIXEL: Result := 32;
    PLANES:    Result := 1;
  else
    Result := 0;
  end;
end;

function TCDWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
begin
  { Win32 convention: high bit (0x8000) set if the key is currently
    physically down, low bit set for toggle keys. We only know down
    state for the modifier keys we track in WLHandleKey -- everything
    else returns 0. FIXME: caps lock / num lock toggle state would
    need wl_keyboard.modifiers ModsLocked, ignored for now. }
  Result := 0;
  case nVirtKey of
    VK_SHIFT, VK_LSHIFT, VK_RSHIFT:
      if FWlShiftHeld then Result := SmallInt($8000);
    VK_CONTROL, VK_LCONTROL, VK_RCONTROL:
      if FWlCtrlHeld  then Result := SmallInt($8000);
    VK_MENU, VK_LMENU, VK_RMENU:           { Alt }
      if FWlAltHeld   then Result := SmallInt($8000);
  end;
end;

function TCDWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
begin
  { FIXME: hardcoded screen metrics; should query wl_output. Border /
    caption / menu metrics are zero because Wayland clients draw their
    own decorations (CSD); plug in real values once decoration is done. }
  case nIndex of
    SM_CXSCREEN, SM_CYSCREEN: Result := 1024;
    SM_CXFULLSCREEN, SM_CYFULLSCREEN: Result := 1024;
    SM_CXFRAME, SM_CYFRAME: Result := 0;
    SM_CXBORDER, SM_CYBORDER: Result := 0;
    SM_CYCAPTION: Result := 0;
    SM_CYMENU: Result := 0;
  else
    Result := 0;
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
  { FIXME: no native message box -- logs to stderr and returns OK. Real
    implementation should pop up a CD-drawn modal toplevel. }
  if (lpCaption <> nil) and (lpText <> nil) then
    DebugLn('[WL MessageBox] %s: %s', [lpCaption, lpText]);
  Result := IDOK;
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
