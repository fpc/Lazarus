unit customdrawndrawers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  // LCL for types
  Graphics, ComCtrls;

const
  CDDRAWSTYLE_COUNT = 19;

  cddTestStr = 'ŹÇ'; // Used for testing text height

  // Measures
  TCDEDIT_LEFT_TEXT_SPACING  = $400; // The space between the start of the text and the left end of the control
  TCDEDIT_RIGHT_TEXT_SPACING = $401; // The space between the end of the text and the right end of the control
  TCDEDIT_TOP_TEXT_SPACING   = $402;
  TCDEDIT_BOTTOM_TEXT_SPACING= $403;

  TCDCHECKBOX_SQUARE_HALF_HEIGHT = $500;
  TCDCHECKBOX_SQUARE_HEIGHT = $501;

  TCDRADIOBUTTON_CIRCLE_HEIGHT = $601;

  TCDSCROLLBAR_BUTTON_WIDTH = $900;
  TCDSCROLLBAR_LEFT_SPACING = $901;   // Left and right are only read left and right for horizontal orientation
  TCDSCROLLBAR_RIGHT_SPACING= $902;   // in vertical orientation they are respectively top and bottom
  TCDSCROLLBAR_LEFT_BUTTON_POS =$903; // Positive Pos means it relative to the left margin,
  TCDSCROLLBAR_RIGHT_BUTTON_POS=$904; // negative that it is relative to the right margin

  TCDTRACKBAR_LEFT_SPACING    = $1000;
  TCDTRACKBAR_RIGHT_SPACING   = $1001;
  TCDTRACKBAR_TOP_SPACING     = $1002;
  TCDTRACKBAR_FRAME_HEIGHT    = $1003;

  TCDLISTVIEW_COLUMN_LEFT_SPACING  = $1200;
  TCDLISTVIEW_COLUMN_RIGHT_SPACING = $1201;
  TCDLISTVIEW_COLUMN_TEXT_LEFT_SPACING = $1202;
  TCDLISTVIEW_LINE_TOP_SPACING     = $1203;
  TCDLISTVIEW_LINE_BOTTOM_SPACING  = $1204;

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
    // for TCDCheckBox, TCDRadioButton
    csfOn,
    csfOff,
    csfPartiallyOn,
    // for TCDScrollBar, TCDProgressBar
    csfHorizontal,
    csfVertical,
    csfRightToLeft,
    csfTopDown,
    // for TCDProgressBar, TCDScrollBar, TCDComboBox
    csfLeftArrow,
    csfRightArrow,
    csfDownArrow,
    csfUpArrow
{    // for tool button
    csfAutoRaise,
    csfTop,
    csfBottom,
    csfFocusAtBorder,
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

  TCDPositionedCStateEx = class(TCDControlStateEx)
  public
    PosCount: integer; // The number of positions, calculated as Max - Min + 1
    Position: integer; // A zero-based position, therefore it is = Position - Min
    FloatPos: Double; // The same position, but as a float between 0.0 and 1.0
    FloatPageSize: Double; // The page size as a float between 0.0 and 1.0
  end;

  TCDProgressBarStateEx = class(TCDControlStateEx)
  public
    BarShowText: Boolean;
    PercentPosition: Double; // a float between 0.0 and 1.0 (1=full)
    Smooth: Boolean;
    Style: TProgressBarStyle;
  end;

  // TCDListItems are implemented as a tree with 2 levels beyond the first node
  TCDListItems = class
  private
    procedure DoFreeItem(data,arg:pointer);
  public
    // These fields are not used in the first node of the tree
    Caption: string;
    ImageIndex: Integer;
    StateIndex: Integer;
    //
    Childs: TFPList;
    constructor Create;
    destructor Destroy; override;
    function Add(ACaption: string; AImageIndex, AStateIndex: Integer): TCDListItems;
    function GetItem(AIndex: Integer): TCDListItems;
    function GetItemCount: Integer;
  end;

  TCDListViewStateEx = class(TCDControlStateEx)
  public
    Columns: TListColumns; // just a reference, never free
    Items: TCDListItems; // just a reference, never free
    ViewStyle: TViewStyle;
    FirstVisibleColumn: Integer; // 0-based index
    FirstVisibleLine: Integer; // 0-based index, remember that the header is always visible or always invisible
    ShowColumnHeader: Boolean;
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
    // Standard
    cidMenu, cidPopUp, cidButton, cidEdit, cidCheckBox, cidRadioButton,
    cidListBox, cidComboBox, cidScrollBar, cidGroupBox,
    // Additional
    cidStaticText,
    // Common Controls
    cidTrackBar, cidProgressBar, cidListView, cidCTabControl
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
    procedure CreateResources; virtual;
    procedure LoadResources; virtual;
    procedure FreeResources; virtual;
    procedure ScaleRasterImage(ARasterImage: TRasterImage; ASourceDPI, ADestDPI: Word);
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
      AControl: TCDControlID; AState: TCDControlState; AStateEx: TCDControlStateEx);
    // General drawing routines
    procedure DrawFocusRect(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); virtual; abstract;
    procedure DrawRaisedFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); virtual; abstract;
    procedure DrawSunkenFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); virtual; abstract;
    procedure DrawShallowSunkenFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); virtual; abstract;
    procedure DrawTickmark(ADest: TCanvas; ADestPos: TPoint); virtual; abstract;
    procedure DrawSlider(ADest: TCanvas; ADestPos: TPoint; ASize: TSize; AState: TCDControlState); virtual; abstract;
    procedure DrawCompactArrow(ADest: TCanvas; ADestPos: TPoint; ADirection: TCDControlState); virtual; abstract;
    // TCDControl
    procedure DrawControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
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
    procedure DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    procedure DrawCheckBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // TCDRadioButton
    procedure DrawRadioButtonCircle(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    procedure DrawRadioButton(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // TCDScrollBar
    procedure DrawScrollBar(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDPositionedCStateEx); virtual; abstract;
    // TCDGroupBox
    procedure DrawGroupBoxSquare(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    procedure DrawGroupBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // ===================================
    // Additional Tab
    // ===================================
    procedure DrawStaticText(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); virtual; abstract;
    // ===================================
    // Common Controls Tab
    // ===================================
    // TCDTrackBar
    procedure DrawTrackBar(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDPositionedCStateEx); virtual; abstract;
    // TCDProgressBar
    procedure DrawProgressBar(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDProgressBarStateEx); virtual; abstract;
    // TCDListView
    procedure DrawListView(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDListViewStateEx); virtual; abstract;
    procedure DrawReportListView(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDListViewStateEx); virtual; abstract;
    procedure DrawReportListViewItem(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      ACurItem: TCDListItems; AState: TCDControlState; AStateEx: TCDListViewStateEx); virtual; abstract;
    // TCDCustomTabControl
    procedure DrawCTabControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); virtual; abstract;
    procedure DrawCTabControlFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
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
  DefaultStyle: TCDDrawStyle = dsCommon; // For now default to the most complete one, later per platform

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

{ TCDListItems }

procedure TCDListItems.DoFreeItem(data, arg: pointer);
begin
  TCDListItems(data).Free;
end;

constructor TCDListItems.Create;
begin
  inherited Create;
  Childs := TFPList.Create;
end;

destructor TCDListItems.Destroy;
begin
  Childs.ForEachCall(@DoFreeItem, nil);
  Childs.Free;
  inherited Destroy;
end;

function TCDListItems.Add(ACaption: string; AImageIndex, AStateIndex: Integer
  ): TCDListItems;
begin
  Result := TCDListItems.Create;
  Result.Caption := ACaption;
  Result.ImageIndex := AImageIndex;
  Result.StateIndex := AStateIndex;
  Childs.Add(Pointer(Result));
end;

function TCDListItems.GetItem(AIndex: Integer): TCDListItems;
begin
  Result := TCDListItems(Childs.Items[AIndex]);
end;

function TCDListItems.GetItemCount: Integer;
begin
  Result := Childs.Count;
end;

{ TCDDrawer }

constructor TCDDrawer.Create;
begin
  inherited Create;

  Palette := TCDColorPalette.Create;
  SetPaletteKind(palDefault);

  CreateResources;
  LoadResources;
end;

destructor TCDDrawer.Destroy;
begin
  FreeResources;
  Palette.Free;

  inherited Destroy;
end;

procedure TCDDrawer.CreateResources;
begin

end;

procedure TCDDrawer.LoadResources;
begin

end;

procedure TCDDrawer.FreeResources;
begin

end;

procedure TCDDrawer.ScaleRasterImage(ARasterImage: TRasterImage; ASourceDPI, ADestDPI: Word);
var
  lNewWidth, lNewHeight: Int64;
begin
  lNewWidth := Round(ARasterImage.Width * ADestDPI / ASourceDPI);
  lNewHeight := Round(ARasterImage.Height * ADestDPI / ASourceDPI);
  ARasterImage.Canvas.StretchDraw(Bounds(0, 0, lNewWidth, lNewHeight), ARasterImage);
  ARasterImage.Width := lNewWidth;
  ARasterImage.Height := lNewHeight;
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

{ Control colors can refer to their background or foreground }
function TCDDrawer.GetControlDefaultColor(AControlId: TCDControlID): TColor;
begin
  case AControlId of
  cidControl:     Result := Palette.Form;
  cidButton:      Result := Palette.BtnFace;// foreground color
  cidEdit:        Result := Palette.Window; // foreground color
  cidCheckBox:    Result := Palette.Form;   // background color
  cidGroupBox:    Result := Palette.Form;   // ...
  //
  cidStaticText:  Result := Palette.Form;   // ...
  //
  cidTrackBar:    Result := Palette.Form;   // ...
  cidProgressBar: Result := Palette.Form;   // foreground color
  cidListView:    Result := Palette.Window; // foreground color
  cidCTabControl: Result := Palette.Form;   // foreground color
  else
    Result := Palette.Form;
  end;
end;

procedure TCDDrawer.DrawControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
  AControl: TCDControlID; AState: TCDControlState; AStateEx: TCDControlStateEx
    );
begin
  case AControl of
  cidControl:    DrawControl(ADest, ADestPos, ASize, AState, AStateEx);
  //
  cidButton:     DrawButton(ADest, ADestPos, ASize, AState, AStateEx);
  cidEdit:       DrawEdit(ADest, ADestPos, ASize, AState, TCDEditStateEx(AStateEx));
  cidCheckBox:   DrawCheckBox(ADest, ADestPos, ASize, AState, AStateEx);
  cidRadioButton:DrawRadioButton(ADest, ADestPos, ASize, AState, AStateEx);
  cidScrollBar:  DrawScrollBar(ADest, ADestPos, ASize, AState, TCDPositionedCStateEx(AStateEx));
  cidGroupBox:   DrawGroupBox(ADest, ADestPos, ASize, AState, AStateEx);
  //
  cidStaticText: DrawStaticText(ADest, ADestPos, ASize, AState, AStateEx);
  //
  cidTrackBar:   DrawTrackBar(ADest, ADestPos, ASize, AState, TCDPositionedCStateEx(AStateEx));
  cidProgressBar:DrawProgressBar(ADest, ADestPos, ASize, AState, TCDProgressBarStateEx(AStateEx));
  cidListView:   DrawListView(ADest, ADestPos, ASize, AState, TCDListViewStateEx(AStateEx));
  cidCTabControl:DrawCTabControl(ADest, ADestPos, ASize, AState, TCDCTabControlStateEx(AStateEx));
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

