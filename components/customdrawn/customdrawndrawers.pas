unit customdrawndrawers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  // LCL for types
  Graphics, ComCtrls;

const
  CDDRAWSTYLE_COUNT = 19;

  // Measures
  TCDEDIT_LEFT_TEXT_SPACING  = $400; // The space between the start of the text and the left end of the control
  TCDEDIT_RIGHT_TEXT_SPACING = $401; // The space between the end of the text and the right end of the control
  TCDEDIT_TOP_TEXT_SPACING   = $402;
  TCDEDIT_BOTTOM_TEXT_SPACING= $403;
  TCDTRACKBAR_LEFT_SPACING    = $1000;
  TCDTRACKBAR_RIGHT_SPACING   = $1001;

  // Measures Ex
  TCDCONTROL_CAPTION_WIDTH  = $100;
  TCDCONTROL_CAPTION_HEIGHT = $101;

  TCDCTABCONTROL_TAB_HEIGHT = $1100;
  TCDCTABCONTROL_TAB_WIDTH  = $1101;

  // Colors
  TCDEDIT_BACKGROUND_COLOR = $400;
  TCDEDIT_TEXT_COLOR = $401;
  TCDEDIT_SELECTED_BACKGROUND_COLOR = $402;
  TCDEDIT_SELECTED_TEXT_COLOR = $403;

  // Default Colors
  TCDBUTTON_DEFAULT_COLOR = $10000;

type

  TCDDrawStyle = (
    // The default is given by the DefaultStyle global variable
    // Don't implement anything for this drawer
    dsDefault = 0,
    // This is a common drawer, with a minimal implementation on which other
    // drawers base on
    dsCommon,
    // Operating system styles
    dsWinCE, dsWin2000, dsWinXP,
    dsKDE, dsGNOME, dsMacOSX,
    dsAndroid,
    // Other special styles for the user
    dsExtra1, dsExtra2, dsExtra3, dsExtra4, dsExtra5,
    dsExtra6, dsExtra7, dsExtra8, dsExtra9, dsExtra10
    );

  // Inspired by http://doc.qt.nokia.com/stable/qstyle.html#StateFlag-enum
  TCDControlStateFlag = (
    // Basic state flags
    csfEnabled,
    csfRaised, // Raised beyond the normal state, unlike Qt for buttons
    csfSunken,
    csfHasFocus,
    csfReadOnly,
    csfMouseOver,
    // for TCDCheckBox
    csfOn,
    csfOff,
    csfPartiallyOn
{    // for TCDPageControl
    csfDownArrow,
    // for tool button
    csfAutoRaise,
    csfHorizontal,
    csfTop,
    csfBottom,
    csfFocusAtBorder,
    csfUpArrow,
    csfSelected,
    csfActive,
    csfWindow,
    csfOpen,
    csfChildren,
    csfItem,
    csfSibling,
    csfEditing,
    csfKeyboardFocusChange,
    // For Mac OS X
    csfSmall,
    csfMini}
   );

  TCDControlState = set of TCDControlStateFlag;

  TCDControlStateEx = class
  public
    ParentRGBColor: TColor;
    RGBColor: TColor;
    Caption: string;
    Font: TFont; // Just a reference, never Free
    AutoSize: Boolean;
  end;

  TCDEditStateEx = class(TCDControlStateEx)
  public
    CaretIsVisible: Boolean;
    CaretPos: TPoint; // X is a zero-based position
    SelStart: TPoint; // X is a zero-based position
    SelLength: Integer; // zero means no selection. Negative numbers selection to the left from the start and positive ones to the right
    VisibleTextStart: TPoint; // X is 1-based
    EventArrived: Boolean; // Added by event handlers and used by the caret so that it stops blinking while events are incoming
  end;

  TCDTrackBarStateEx = class(TCDControlStateEx)
  public
    Min: integer;
    Max: integer;
    Position: integer;
  end;

  TCDCTabControlStateEx = class(TCDControlStateEx)
  public
    LeftmostTabVisibleIndex: Integer;
    Tabs: TStringList; // Just a reference, don't Free
    TabIndex: Integer;
    TabCount: Integer;
    Options: TNoteBookOptions;
    // Used internally by the drawers
    CurTabIndex: Integer;// For Tab routines, obtain the index
    CurStartLeftPos: Integer;
  end;

  TCDControlID = (
    cidControl,
    cidButton,
    cidEdit,
    cidCheckBox,
    cidGroupBox,
    cidTrackBar,
    cidCTabControl
    );

  TCDColorPalette = class
  public
    ScrollBar, Background, ActiveCaption, InactiveCaption,
    Menu, Window, WindowFrame, MenuText, WindowText, CaptionText,
    ActiveBorder, InactiveBorder, AppWorkspace, Highlight, HighlightText,
    BtnFace, BtnShadow, GrayText, BtnText, InactiveCaptionText,
    BtnHighlight, color3DDkShadow, color3DLight, InfoText, InfoBk,
    //
    HotLight, GradientActiveCaption, GradientInactiveCaption,
    MenuHighlight, MenuBar, Form: TColor;
  end;

  { There are 3 possible sources of input for color palettes:
   palDefault  - Uses palNative when the operating system matches the drawer style,
                 palFallback otherwise
   palNative   - Obtain from the operating system
   palFallback - Use the fallback colors of the drawer
   palUserConfig-Load it from the user configuration files, ToDo
   palCustom   - The user application has set its own palette
  }
  TCDPaletteKind = (palDefault, palNative, palFallback, palUserConfig, palCustom);

  { TCDDrawer }

  TCDDrawer = class
  protected
  public
    Palette: TCDColorPalette;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetPaletteKind(APaletteKind: TCDPaletteKind);
    procedure LoadNativePaletteColors;
    procedure LoadFallbackPaletteColors; virtual;
    function GetDrawStyle: TCDDrawStyle; virtual;
    // GetControlDefaultColor is used by customdrawncontrols to resolve clDefault
    function GetControlDefaultColor(AControlId: TCDControlID): TColor;
    // General
    function GetMeasures(AMeasureID: Integer): Integer; virtual; abstract;
    function GetMeasuresEx(ADest: TCanvas; AMeasureID: Integer;
      AState: TCDControlState; AStateEx: TCDControlStateEx): Integer; virtual; abstract;
    procedure CalculatePreferredSize(ADest: TCanvas; AControlId: TCDControlID;
      AState: TCDControlState; AStateEx: TCDControlStateEx;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); virtual; abstract;
    function GetColor(AColorID: Integer): TColor; virtual; abstract;
    function GetClientArea(ADest: TCanvas; ASize: TSize; AControlId: TCDControlID;
      AState: TCDControlState; AStateEx: TCDControlStateEx): TRect; virtual; abstract;
    procedure DrawControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AControl: TCDControlID; AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // General drawing routines
    procedure DrawFocusRect(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); virtual; abstract;
    // TCDButton
    procedure DrawButton(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // TCDEdit
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); virtual; abstract;
    procedure DrawCaret(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); virtual; abstract;
    procedure DrawEdit(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); virtual; abstract;
    // TCDCheckBox
    procedure DrawCheckBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // TCDGroupBox
    procedure DrawGroupBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // ===================================
    // Common Controls Tab
    // ===================================
    // TCDTrackBar
    procedure DrawTrackBar(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDTrackBarStateEx); virtual; abstract;
    // TCDCustomTabControl
    procedure DrawCTabControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); virtual; abstract;
    procedure DrawTabSheet(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); virtual; abstract;
    procedure DrawTabs(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); virtual; abstract;
    procedure DrawTab(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); virtual; abstract;
  end;

procedure RegisterDrawer(ADrawer: TCDDrawer; AStyle: TCDDrawStyle);
function GetDefaultDrawer: TCDDrawer;
function GetDrawer(AStyle: TCDDrawStyle): TCDDrawer;

var
  DefaultStyle: TCDDrawStyle = dsWinCE; // For now default to the most complete one, later per platform

implementation

var
  RegisteredDrawers: array[TCDDrawStyle] of TCDDrawer
    = (nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil);

procedure RegisterDrawer(ADrawer: TCDDrawer; AStyle: TCDDrawStyle);
begin
  if RegisteredDrawers[AStyle] <> nil then RegisteredDrawers[AStyle].Free;
  RegisteredDrawers[AStyle] := ADrawer;
end;

function GetDefaultDrawer: TCDDrawer;
begin
  Result := GetDrawer(dsDefault);
end;

function GetDrawer(AStyle: TCDDrawStyle): TCDDrawer;
var
  lDrawStyle: TCDDrawStyle;
begin
  if AStyle = dsDefault then lDrawStyle := DefaultStyle
  else lDrawStyle := AStyle;
  Result := RegisteredDrawers[lDrawStyle];
end;

var
  i: Integer;

{ TCDDrawer }

constructor TCDDrawer.Create;
begin
  inherited Create;

  Palette := TCDColorPalette.Create;
  SetPaletteKind(palDefault);
end;

destructor TCDDrawer.Destroy;
begin
  Palette.Free;

  inherited Destroy;
end;

procedure TCDDrawer.SetPaletteKind(APaletteKind: TCDPaletteKind);
var
  lIsOnNativeSystem: Boolean = False;
  lStyle: TCDDrawStyle;
begin
  case APaletteKind of
  palDefault:
  begin
    lStyle := GetDrawStyle();
    case lStyle of
    dsWinCE: lIsOnNativeSystem := {$ifdef WinCE}True{$else}False{$endif};
    dsCommon, dsWin2000, dsWinXP:
      lIsOnNativeSystem := {$ifdef MSWindows}True{$else}False{$endif};
    else
      lIsOnNativeSystem := False;
    end;

    if lIsOnNativeSystem then LoadNativePaletteColors()
    else LoadFallbackPaletteColors();
  end;
  palNative:   LoadNativePaletteColors();
  palFallback: LoadFallbackPaletteColors();
  //palUserConfig:
  end;
end;

procedure TCDDrawer.LoadNativePaletteColors;
begin
  Palette.ScrollBar := ColorToRGB(clScrollBar);
  Palette.Background := ColorToRGB(clBackground);
  Palette.ActiveCaption := ColorToRGB(clActiveCaption);
  Palette.InactiveCaption := ColorToRGB(clInactiveCaption);
  Palette.Menu := ColorToRGB(clMenu);
  Palette.Window := ColorToRGB(clWindow);
  Palette.WindowFrame := ColorToRGB(clWindowFrame);
  Palette.MenuText := ColorToRGB(clMenuText);
  Palette.WindowText := ColorToRGB(clWindowText);
  Palette.CaptionText := ColorToRGB(clCaptionText);
  Palette.ActiveBorder := ColorToRGB(clActiveBorder);
  Palette.InactiveBorder := ColorToRGB(clInactiveBorder);
  Palette.AppWorkspace := ColorToRGB(clAppWorkspace);
  Palette.Highlight := ColorToRGB(clHighlight);
  Palette.HighlightText := ColorToRGB(clHighlightText);
  Palette.BtnFace := ColorToRGB(clBtnFace);
  Palette.BtnShadow := ColorToRGB(clBtnShadow);
  Palette.GrayText := ColorToRGB(clGrayText);
  Palette.BtnText := ColorToRGB(clBtnText);
  Palette.InactiveCaptionText := ColorToRGB(clInactiveCaptionText);
  Palette.BtnHighlight := ColorToRGB(clBtnHighlight);
  Palette.color3DDkShadow := ColorToRGB(cl3DDkShadow);
  Palette.color3DLight := ColorToRGB(cl3DLight);
  Palette.InfoText := ColorToRGB(clInfoText);
  Palette.InfoBk := ColorToRGB(clInfoBk);

  Palette.HotLight := ColorToRGB(clHotLight);
  Palette.GradientActiveCaption := ColorToRGB(clGradientActiveCaption);
  Palette.GradientInactiveCaption := ColorToRGB(clGradientInactiveCaption);
  Palette.MenuHighlight := ColorToRGB(clMenuHighlight);
  Palette.MenuBar := ColorToRGB(clMenuBar);
  Palette.Form := ColorToRGB(clForm);
end;

procedure TCDDrawer.LoadFallbackPaletteColors;
begin

end;

function TCDDrawer.GetDrawStyle: TCDDrawStyle;
begin
  Result := dsCommon;
end;

function TCDDrawer.GetControlDefaultColor(AControlId: TCDControlID): TColor;
begin
  case AControlId of
  cidControl:     Result := Palette.Form;
  cidButton:      Result := Palette.BtnFace;
  cidEdit:        Result := Palette.Window;
  cidCheckBox:    Result := Palette.Form;
  cidGroupBox:    Result := Palette.Form;
  cidTrackBar:    Result := Palette.Form;
  cidCTabControl: Result := Palette.Form;
  else
    Result := Palette.Form;
  end;
end;

finalization
  // Free all drawers
  for i := 0 to CDDRAWSTYLE_COUNT-1 do
  begin
    if RegisteredDrawers[TCDDrawStyle(i)] <> nil then
    begin
      RegisteredDrawers[TCDDrawStyle(i)].Free;
      RegisteredDrawers[TCDDrawStyle(i)] := nil;
    end;
  end;
end.

