unit customdrawndrawers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  // LCL for types
  Graphics;

const
  CDDRAWSTYLE_COUNT = 19;

  // Measures
  TCDEDIT_LEFT_TEXT_SPACING  = $400; // The space between the start of the text and the left end of the control
  TCDEDIT_RIGHT_TEXT_SPACING = $401; // The space between the end of the text and the right end of the control

  // Measures Ex
  TCDCONTROL_CAPTION_WIDTH  = $100;
  TCDCONTROL_CAPTION_HEIGHT = $101;

  TCDCTABCONTROL_TAB_HEIGHT = $1000;
  TCDCTABCONTROL_TAB_WIDTH  = $1001;

  // Colors
  TCDEDIT_BACKGROUND_COLOR = $400;
  TCDEDIT_TEXT_COLOR = $401;
  TCDEDIT_SELECTED_BACKGROUND_COLOR = $402;
  TCDEDIT_SELECTED_TEXT_COLOR = $403;

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
    CaretPos: Integer; // zero-based position
    SelStart: Integer; // zero-based position
    SelLength: Integer; // zero means no selection. Negative numbers selection to the left from the start and positive ones to the right
    VisibleTextStart: Integer; // 1-based
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
    TabIndex: Integer; // For Tab routines, obtain the index
  end;

  TCDControlID = (
    cidControl,
    cidButton,
    cidEdit,
    cidCheckBox,
    cidGroupBox,
    cidTrackBar,
    cidCustomTabControl
    );

  { TCDDrawer }

  TCDDrawer = class
  public
    function GetControlColor(AControlId: TCDControlID): TColor;
    // General
    function GetMeasures(AMeasureID: Integer): Integer; virtual; abstract;
    function GetMeasuresEx(ADest: TCanvas; AMeasureID: Integer;
      AState: TCDControlState; AStateEx: TCDControlStateEx): Integer; virtual; abstract;
    function GetColor(AColorID: Integer): TColor; virtual; abstract;
    procedure DrawControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AControl: TCDControlID; AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // TCDButton
    procedure DrawButton(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // TCDEdit
    procedure CreateEditBackgroundBitmap(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); virtual; abstract;
    procedure DrawEdit(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); virtual; abstract;
    // TCDCheckBox
    procedure CalculateCheckBoxPreferredSize(ADest: TCanvas;
      AState: TCDControlState; AStateEx: TCDControlStateEx;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); virtual; abstract;
    procedure DrawCheckBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // TCDGroupBox
    procedure DrawGroupBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // ===================================
    // Common Controls Tab
    // ===================================
    // TCDCustomTabControl
    procedure DrawCTabControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); virtual; abstract;
    procedure DrawTabSheet(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); virtual; abstract;
    procedure DrawTabs(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); virtual; abstract;
    procedure DrawTab(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); virtual; abstract;
    function  GetPageIndexFromXY(x, y: integer): integer; virtual; abstract;
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

function TCDDrawer.GetControlColor(AControlId: TCDControlID): TColor;
begin
  case AControlId of
  cidControl:          Result := clSilver;
  cidButton:           Result := clSilver;
  cidEdit:             Result := clSilver;
  cidCheckBox:         Result := clSilver;
  cidGroupBox:         Result := clSilver;
  cidTrackBar:         Result := clSilver;
  cidCustomTabControl: Result := clSilver;
  else
    Result := clSilver;
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

