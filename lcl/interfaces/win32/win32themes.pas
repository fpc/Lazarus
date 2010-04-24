unit Win32Themes;

{$mode objfpc}{$H+}
{$I win32defines.inc}

interface

uses
  // os
  Windows, Win32UxTheme, Win32Proc, Win32Extra, 
  // rtl
  Classes, SysUtils,
  // lcl
  Controls, Graphics, Themes, LCLProc, LCLType;
  
type

  TThemeData = array[TThemedElement] of HTHEME;
  { TWin32ThemeServices }

  TWin32ThemeServices = class(TThemeServices)
  private
    FThemeData: TThemeData;            // Holds a list of theme data handles.
  protected
    function GetTheme(Element: TThemedElement): HTHEME;
    function InitThemes: Boolean; override;
    procedure UnloadThemeData; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;
    
    function InternalColorToRGB(Details: TThemedElementDetails; Color: TColor): LongInt; override;
    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); override;
  public
    destructor Destroy; override;

    function GetDetailSize(Details: TThemedElementDetails): TSize; override;
    function GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean; override;
    function GetOption(AOption: TThemeOption): Integer; override;

    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect;
      ClipRect: PRect = nil); override;
    procedure DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
      AContentRect: PRect = nil); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect;
      himl: HIMAGELIST; Index: Integer); override;
    procedure DrawIcon(ACanvas: TPersistent; Details: TThemedElementDetails;
      const P: TPoint; AImageList: TPersistent; Index: Integer); override;

    procedure DrawText(DC: HDC; Details: TThemedElementDetails;
      const S: String; R: TRect; Flags, Flags2: Cardinal); override;
    procedure DrawText(ACanvas: TPersistent; Details: TThemedElementDetails;
      const S: String; R: TRect; Flags, Flags2: Cardinal); override;

    procedure DrawTextEx(DC: HDC; Details: TThemedElementDetails;
      const S: String; R: TRect; Flags: Cardinal; Options: PDTTOpts);

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
    procedure PaintBorder(Control: TObject; EraseLRCorner: Boolean); override;
    property Theme[Element: TThemedElement]: HTHEME read GetTheme;
  end;

implementation

uses
  TmSchema;

const
  ThemeDataNames: array[TThemedElement] of PWideChar = (
    'button',      // teButton
    'clock',       // teClock
    'combobox',    // teComboBox
    'edit',        // teEdit
    'explorerbar', // teExplorerBar
    'header',      // teHeader
    'listview',    // teListView
    'menu',        // teMenu
    'page',        // tePage
    'progress',    // teProgress
    'rebar',       // teRebar
    'scrollbar',   // teScrollBar
    'spin',        // teSpin
    'startpanel',  // teStartPanel
    'status',      // teStatus
    'tab',         // teTab
    'taskband',    // teTaskBand
    'taskbar',     // teTaskBar
    'toolbar',     // teToolBar
    'tooltip',     // teToolTip
    'trackbar',    // teTrackBar
    'traynotify',  // teTrayNotify
    'treeview',    // teTreeview
    'window'       // teWindow
  );

  ThemeDataNamesVista: array[TThemedElement] of PWideChar = (
    'button',      // teButton
    'clock',       // teClock
    'combobox',    // teComboBox
    'edit',        // teEdit
    'explorerbar', // teExplorerBar
    'header',      // teHeader
    'explorer::listview',    // teListView
    'menu',        // teMenu
    'page',        // tePage
    'progress',    // teProgress
    'rebar',       // teRebar
    'scrollbar',   // teScrollBar
    'spin',        // teSpin
    'startpanel',  // teStartPanel
    'status',      // teStatus
    'tab',         // teTab
    'taskband',    // teTaskBand
    'taskbar',     // teTaskBar
    'toolbar',     // teToolBar
    'tooltip',     // teToolTip
    'trackbar',    // teTrackBar
    'traynotify',  // teTrayNotify
    'explorer::treeview',    // teTreeview
    'window'       // teWindow
  );

  // standard windows icons (WinUser.h)
  // they are already defined in the rtl, however the
  // const = const defines after this fail with an illegal expression
  IDI_APPLICATION = System.MakeIntResource(32512);
  IDI_HAND        = System.MakeIntResource(32513);
  IDI_QUESTION    = System.MakeIntResource(32514);
  IDI_EXCLAMATION = System.MakeIntResource(32515);
  IDI_ASTERISK    = System.MakeIntResource(32516);
  IDI_WINLOGO     = System.MakeIntResource(32517); // XP only

  IDI_WARNING     = IDI_EXCLAMATION;
  IDI_ERROR       = IDI_HAND;
  IDI_INFORMATION = IDI_ASTERISK;

{ TWin32ThemeServices }

procedure TWin32ThemeServices.UnloadThemeData;
var
  Entry: TThemedElement;
begin
  for Entry := Low(TThemeData) to High(TThemeData) do
    if FThemeData[Entry] <> 0 then
    begin
      CloseThemeData(FThemeData[Entry]);
      FThemeData[Entry] := 0;
    end;
end;

function TWin32ThemeServices.InitThemes: Boolean;
begin
  Result := InitThemeLibrary;
  FillChar(FThemeData, SizeOf(FThemeData), 0);
end;

destructor TWin32ThemeServices.Destroy;
begin
  inherited Destroy;
  FreeThemeLibrary;
end;

function TWin32ThemeServices.GetDetailSize(Details: TThemedElementDetails): TSize;
var
  R: TRect;
begin
  // GetThemeInt(Theme[Details.Element], Details.Part, Details.State, TMT_HEIGHT, Result);
  // does not work for some reason
  if ThemesEnabled then
  begin
    if (Details.Element = teToolBar) and (Details.Part = TP_SPLITBUTTONDROPDOWN) then
       Result.cx := 12
    else
    if (Details.Element = teTreeview) and (Details.Part in [TVP_GLYPH, TVP_HOTGLYPH]) then
    begin
      R := Rect(0, 0, 800, 800);
      GetThemePartSize(GetTheme(Details.Element), 0, Details.Part, Details.State, @R, TS_TRUE, Result);
    end
    else
      Result := inherited GetDetailSize(Details);
  end
  else
    Result := inherited GetDetailSize(Details);
end;

function TWin32ThemeServices.GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean;
var
  IconHandle: HIcon;
  IconInfo: TIconInfo;
  Bitmap: Windows.TBitmap;
  x, y: Integer;
  LinePtr: PByte;
  Pixel: PRGBAQuad;
  SHIconInfo: TSHSTOCKICONINFO;
begin
  case StockID of
    idDialogWarning: IconHandle := LoadImage(0, IDI_WARNING, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE or LR_SHARED);
    idDialogError  : IconHandle := LoadImage(0, IDI_ERROR, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE or LR_SHARED);
    idDialogInfo   : IconHandle := LoadImage(0, IDI_INFORMATION, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE or LR_SHARED);
    idDialogConfirm: IconHandle := LoadImage(0, IDI_QUESTION, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE or LR_SHARED);
    idDialogShield:
      begin
        FillChar(SHIconInfo, SizeOf(SHIconInfo), 0);
        SHIconInfo.cbSize := SizeOf(SHIconInfo);
        if (SHGetStockIconInfo(SIID_SHIELD, SHGFI_ICON or SHGFI_LARGEICON, @SHIconInfo) = S_OK) then
          IconHandle := SHIconInfo.hIcon
        else
          IconHandle := 0;
      end;
    idButtonShield:
      begin
        FillChar(SHIconInfo, SizeOf(SHIconInfo), 0);
        SHIconInfo.cbSize := SizeOf(SHIconInfo);
        if (SHGetStockIconInfo(SIID_SHIELD, SHGFI_ICON or SHGFI_SMALLICON, @SHIconInfo) = S_OK) then
          IconHandle := SHIconInfo.hIcon
        else
          IconHandle := 0;
      end;
  else
    IconHandle := 0;
  end;
  Result := (IconHandle <> 0) and GetIconInfo(IconHandle, @IconInfo);
  if not Result then
  begin
    Result := inherited GetStockImage(StockID, Image, Mask);
    Exit;
  end;

  Image := IconInfo.hbmColor;
  Mask := IconInfo.hbmMask;

  if WindowsVersion >= wvXP then Exit; // XP and up return alpha bitmaps
  if GetObject(Image, SizeOf(Bitmap), @Bitmap) = 0 then Exit;
  if Bitmap.bmBitsPixel <> 32 then Exit; // we only need to "fix" 32bpp images

  Image := CopyImage(IconInfo.hbmColor, IMAGE_BITMAP, 0, 0, LR_COPYDELETEORG or LR_CREATEDIBSECTION);
  if WindowsVersion in [wv95, wv98, wvME]
  then begin
    // 95 or ME aren't tested, so if icons appear invisible remove them
    // only copying is enough
    Exit;
  end;

  // Others remain ( wvUnknown, wvNT4, wv2000 )

  if GetObject(Image, SizeOf(Bitmap), @Bitmap) = 0 then Exit; // ???
  if Bitmap.bmBits = nil then Exit; // ?? we requested a dibsection, but didn't get one ??

  LinePtr := Bitmap.bmBits;

  for y := Bitmap.bmHeight downto 1 do
  begin
    Pixel := Pointer(LinePtr);
    for x := Bitmap.bmWidth downto 1 do
    begin
      Pixel^.Alpha := 255;
      Inc(Pixel);
    end;
    Inc(LinePtr, Bitmap.bmWidthBytes);
  end;
end;

function TWin32ThemeServices.GetOption(AOption: TThemeOption): Integer;
begin
  case AOption of
    toShowButtonImages: Result := 0;
  else
    Result := inherited GetOption(AOption);
  end;
end;

function TWin32ThemeServices.UseThemes: Boolean;
begin
  Result := Win32UxTheme.UseThemes and (GetFileVersion(comctl32) >= ComCtlVersionIE6);
end;

function TWin32ThemeServices.ThemedControlsEnabled: Boolean;
var
  Flags: DWORD;
begin
  Flags := Win32UxTheme.GetThemeAppProperties();
  if (Flags and STAP_ALLOW_CONTROLS) = 0 then
    Result := False
  else
    Result := True;
end;

function TWin32ThemeServices.GetTheme(Element: TThemedElement): HTHEME;
begin
  if (FThemeData[Element] = 0) then
  begin
    if (WindowsVersion >= wvVista) then
      FThemeData[Element] := OpenThemeData(0, ThemeDataNamesVista[Element])
    else
      FThemeData[Element] := OpenThemeData(0, ThemeDataNames[Element]);
  end;
  Result := FThemeData[Element];
end;

function TWin32ThemeServices.InternalColorToRGB(Details: TThemedElementDetails; Color: TColor): LongInt;
begin
  if ThemesEnabled then
    Result := LongInt(GetThemeSysColor(Theme[Details.Element], Integer(Color and not $80000000)))
  else
    Result := inherited;
end;

function TWin32ThemeServices.ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  if ThemesEnabled then
    with Details do
      GetThemeBackgroundContentRect(Theme[Element], DC, Part, State, BoundingRect, @Result)
  else
    Result := inherited;
end;

procedure TWin32ThemeServices.DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
      AContentRect: PRect = nil);
begin
  if ThemesEnabled then
    with Details do
      DrawThemeEdge(Theme[Element], DC, Part, State, R, Edge, Flags, AContentRect)
  else
    inherited;
end;

procedure TWin32ThemeServices.DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil);
var
  ARect: TRect;
  Brush: HBrush;
begin
  if ThemesEnabled then
  begin
    if (Details.Element = teTreeview) and (Details.Part = TVP_HOTGLYPH) and (WindowsVersion < wvVista) then
      Details.Part := TVP_GLYPH;
    if (Details.Element = teTreeview) and (Details.Part = TVP_TREEITEM) and (WindowsVersion < wvVista) then
    begin
      inherited;
      Exit;
    end;
    with Details do
      DrawThemeBackground(Theme[Element], DC, Part, State, R, ClipRect);
    if (Details.Element = teToolTip) and (Details.Part = TTP_STANDARD) and (WindowsVersion < wvVista) then
    begin
      // use native background on windows vista
      ARect := ContentRect(DC, Details, R);
      Brush := CreateSolidBrush(ColorToRGB(clInfoBk));
      FillRect(DC, ARect, Brush);
      DeleteObject(Brush);
    end;
  end
  else
    inherited;
end;

procedure TWin32ThemeServices.DrawIcon(DC: HDC; Details: TThemedElementDetails;
  const R: TRect; himl: HIMAGELIST; Index: Integer);
begin
  if ThemesEnabled then
    with Details do
      DrawThemeIcon(Theme[Element], DC, Part, State, R, himl, Index)
  else
    inherited;
end;

procedure TWin32ThemeServices.DrawIcon(ACanvas: TPersistent;
  Details: TThemedElementDetails; const P: TPoint; AImageList: TPersistent;
  Index: Integer);
{var
  ImageList: TCustomImageList absolute AImageList;
}
begin
{  if ThemesEnabled then
    DrawIcon(TCanvas(ACanvas).Handle, Details,
      Rect(P.X, P.Y, P.X + ImageList.Width, P.Y + ImageList.Width),
      ImageList.Handle, Index)
  else}
    inherited;
end;

function TWin32ThemeServices.HasTransparentParts(Details: TThemedElementDetails): Boolean;
begin
  if ThemesEnabled then
    with Details do
      Result := IsThemeBackgroundPartiallyTransparent(Theme[Element], Part, State)
   else
     Result := inherited;
end;

procedure TWin32ThemeServices.PaintBorder(Control: TObject;
  EraseLRCorner: Boolean);
var
  EmptyRect,
  DrawRect: TRect;
  DC: HDC;
  H, W: Integer;
  AStyle,
  ExStyle: Integer;
  Details: TThemedElementDetails;
begin
  if not (Control is TWinControl) then
    Exit;

  if not ThemesEnabled then
  begin
    inherited;
    Exit;
  end;

  with TWinControl(Control) do
  begin
    ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then
    begin
      GetWindowRect(Handle, DrawRect);
      OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
      DC := GetWindowDC(Handle);
      try
        EmptyRect := DrawRect;
        if EraseLRCorner then
        begin
          AStyle := GetWindowLong(Handle, GWL_STYLE);
          if ((AStyle and WS_HSCROLL) <> 0) and ((AStyle and WS_VSCROLL) <> 0) then
          begin
            W := GetSystemMetrics(SM_CXVSCROLL);
            H := GetSystemMetrics(SM_CYHSCROLL);
            InflateRect(EmptyRect, -2, -2);
            with EmptyRect do
              EmptyRect := Rect(Right - W, Bottom - H, Right, Bottom);
            FillRect(DC, EmptyRect, GetSysColorBrush(COLOR_BTNFACE));
          end;
        end;
        with DrawRect do
          ExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);
        Details := ThemeServices.GetElementDetails(teEditTextNormal);
        DrawElement(DC, Details, DrawRect, nil);
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
  end;
end;

procedure TWin32ThemeServices.InternalDrawParentBackground(Window: HWND;
  Target: HDC; Bounds: PRect);
begin
  if ThemesEnabled then
    DrawThemeParentBackground(Window, Target, Bounds)
  else
    inherited;
end;

procedure TWin32ThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: String; R: TRect; Flags, Flags2: Cardinal);
{$IFDEF WindowsUnicodeSupport}
var
  w: widestring;
{$ENDIF}
begin
  if ThemesEnabled then
    with Details do
{$IFDEF WindowsUnicodeSupport}
    begin
      w := UTF8ToUTF16(S);
      DrawThemeText(Theme[Element], DC, Part, State, PWideChar(w), Length(w), Flags, Flags2, R);
    end
{$ELSE}
      DrawThemeText(Theme[Element], DC, Part, State, PWideChar(WideString(S)), Length(S), Flags, Flags2, R)
{$ENDIF}
  else
    inherited;
end;

procedure TWin32ThemeServices.DrawText(ACanvas: TPersistent;
  Details: TThemedElementDetails; const S: String; R: TRect; Flags,
  Flags2: Cardinal);
begin
  if (Details.Element = teTreeview) and (Details.Part = TVP_TREEITEM) and (WindowsVersion < wvVista) then
  begin
    inherited;
    Exit;
  end;
  if ThemesEnabled then
    DrawText(TCanvas(ACanvas).Handle, Details, S, R, Flags, Flags2)
  else
    inherited;
end;

procedure TWin32ThemeServices.DrawTextEx(DC: HDC;
  Details: TThemedElementDetails; const S: String; R: TRect; Flags: Cardinal;
  Options: PDTTOpts);
{$IFDEF WindowsUnicodeSupport}
var
  w: widestring;
{$ENDIF}
begin
  if ThemesEnabled and (DrawThemeTextEx <> nil) then
    with Details do
{$IFDEF WindowsUnicodeSupport}
    begin
      w := UTF8ToUTF16(S);
      DrawThemeTextEx(Theme[Element], DC, Part, State, PWideChar(w), Length(w), Flags, @R, Options);
    end
{$ELSE}
      DrawThemeTextEx(Theme[Element], DC, Part, State, PWideChar(WideString(S)), Length(S), Flags, @R, Options)
{$ENDIF}
  else
    DrawText(DC, Details, S, R, Flags, 0);
end;

end.
