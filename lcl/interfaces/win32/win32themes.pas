unit Win32Themes;

{$mode objfpc}{$H+}

interface

uses
  // os
  Windows, Win32UxTheme, Win32Proc, Win32Extra, 
  // rtl
  Classes, SysUtils,
  // lcl
  Controls, Graphics, ImgList, Themes;
  
type

  TThemeData = array[TThemedElement] of HTHEME;
  { TWin32ThemeServices }

  TWin32ThemeServices = class(TThemeServices)
  private
    FThemeData: TThemeData;            // Holds a list of theme data handles.
  protected
    function GetTheme(Element: TThemedElement): HTHEME;
    property Theme[Element: TThemedElement]: HTHEME read GetTheme;
    function InitThemes: Boolean; override;
    procedure UnloadThemeData; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;
    
    function InternalColorToRGB(Details: TThemedElementDetails; Color: TColor): LongInt; override;
    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); override;
  public
    destructor Destroy; override;

    function GetDetailSize(Details: TThemedElementDetails): Integer; override;

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

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
    procedure PaintBorder(Control: TObject; EraseLRCorner: Boolean); override;
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

function TWin32ThemeServices.GetDetailSize(Details: TThemedElementDetails
  ): Integer;
begin
  if ThemesEnabled then
    case Details.Element of
      teToolBar:
        if Details.Part = TP_SPLITBUTTONDROPDOWN then
          Result := 12;
    else
      Result:=inherited GetDetailSize(Details);
    end
  else
    Result:=inherited GetDetailSize(Details);
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
    FThemeData[Element] := OpenThemeData(0, ThemeDataNames[Element]);
  Result := FThemeData[Element];
end;

function TWin32ThemeServices.InternalColorToRGB(Details: TThemedElementDetails; Color: TColor): LongInt;
begin
  if ThemesEnabled then
    Result := GetThemeSysColor(Theme[Details.Element], Color and not $80000000)
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
begin
  if ThemesEnabled then
    with Details do
      DrawThemeBackground(Theme[Element], DC, Part, State, R, ClipRect)
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
begin
  if ThemesEnabled then
    with Details do
      DrawThemeText(Theme[Element], DC, Part, State, PWideChar(WideString(S)), Length(S), Flags, Flags2, R)
  else
    inherited;
end;

procedure TWin32ThemeServices.DrawText(ACanvas: TPersistent;
  Details: TThemedElementDetails; const S: String; R: TRect; Flags,
  Flags2: Cardinal);
begin
  if ThemesEnabled then
    DrawText(TCanvas(ACanvas).Handle, Details, S, R, Flags, Flags2)
  else
    inherited;
end;

end.
