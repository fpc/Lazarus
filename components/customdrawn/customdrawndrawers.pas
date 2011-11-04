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

  { TCDDrawer }

  TCDDrawer = class
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //
    function GetControlColor(AControlId: TCDControlID): TColor;
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
    // TCDButton
    procedure DrawButton(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // TCDEdit
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
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


end;

destructor TCDDrawer.Destroy;
begin
  inherited Destroy;
end;

function TCDDrawer.GetControlColor(AControlId: TCDControlID): TColor;
begin
  case AControlId of
  cidControl:     Result := clSilver;
  cidButton:      Result := clSilver;
  cidEdit:        Result := clSilver;
  cidCheckBox:    Result := clSilver;
  cidGroupBox:    Result := clSilver;
  cidTrackBar:    Result := clSilver;
  cidCTabControl: Result := clSilver;
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

