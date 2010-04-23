{ $Id: $
                  -----------------------------------------
                  CarbonThemes.pas  -  Carbon Theme support
                  -----------------------------------------

  See Themes.pas for licencing and other further information.
}
unit CarbonThemes;

{$mode objfpc}{$H+}

interface

uses
  // rtl
  Types, Classes, SysUtils, Math,
  // carbon bindings
  MacOSAll,
  // lcl
  LCLType, LCLProc, LCLIntf, Graphics, Themes, TmSchema,
  // widgetset
  CarbonProc, CarbonCanvas, CarbonGDIObjects;
  
type
  { TCarbonThemeServices }

  TCarbonThemeServices = class(TThemeServices)
  private
  protected
    function InitThemes: Boolean; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;
    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); override;

    function GetDrawState(Details: TThemedElementDetails): ThemeDrawState;
    function DrawButtonElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
    function DrawHeaderElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
    function DrawRebarElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
    function DrawToolBarElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
    function DrawTreeviewElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
  public
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); override;
    procedure DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal; AContentRect: PRect); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST; Index: Integer); override;
    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: String; R: TRect; Flags, Flags2: Cardinal); override;

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
    function GetDetailSize(Details: TThemedElementDetails): TSize; override;
    function GetOption(AOption: TThemeOption): Integer; override;
  end;

implementation

{ TCarbonThemeServices }

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.GetDrawState
  Params:  Details - Details for themed element
  Returns: Draw state of the themed element passed
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.GetDrawState(Details: TThemedElementDetails): ThemeDrawState;
{
	kThemeStateInactive = 0;
	kThemeStateActive = 1;
	kThemeStatePressed = 2;
	kThemeStateRollover = 6;
	kThemeStateUnavailable = 7;
	kThemeStateUnavailableInactive = 8;

	kThemeStatePressedUp = 2;     draw with up pressed     (increment/decrement buttons)
	kThemeStatePressedDown = 3;      draw with down pressed (increment/decrement buttons)

}
begin
  if IsDisabled(Details) then
    Result := kThemeStateInactive
  else
  if IsPushed(Details) then
    Result := kThemeStatePressed
  else
  if IsHot(Details) then
    Result := kThemeStateRollover
  else
    Result := kThemeStateActive;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawButtonElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle
  Returns: ClientRect

  Draws a button element with native Carbon look
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.DrawButtonElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
const
  ButtonMap: array[BP_PUSHBUTTON..BP_USERBUTTON] of ThemeButtonKind =
  (
{BP_PUSHBUTTON } kThemeBevelButtonSmall,
{BP_RADIOBUTTON} kThemeRadioButton,
{BP_CHECKBOX   } kThemeCheckBox,
{BP_GROUPBOX   } kHIThemeGroupBoxKindPrimary, // ??
{BP_USERBUTTON } kThemeBevelButtonSmall
  );
var
  ButtonDrawInfo: HIThemeButtonDrawInfo;
  // we can do so because GroupDrawIndo have common fields with ButtonDrawInfo
  GroupDrawInfo: HIThemeGroupBoxDrawInfo absolute ButtonDrawInfo; 
  LabelRect: HIRect;
begin
  ButtonDrawInfo.version := 0;
  ButtonDrawInfo.State := GetDrawState(Details);
  ButtonDrawInfo.kind := ButtonMap[Details.Part];
  if IsMixed(Details) then
    ButtonDrawInfo.value := kThemeButtonMixed
  else
  if IsChecked(Details) then
    ButtonDrawInfo.value := kThemeButtonOn
  else
    ButtonDrawInfo.value := kThemeButtonOff;
  ButtonDrawInfo.adornment := kThemeAdornmentNone;

  LabelRect := RectToCGRect(R);

  if Details.Part = BP_GROUPBOX then
    OSError(
      HIThemeDrawGroupBox(LabelRect, GroupDrawInfo, DC.CGContext,
          kHIThemeOrientationNormal),
      Self, 'DrawButtonElement', 'HIThemeDrawGroupBox')
  else
    OSError(
      HIThemeDrawButton(LabelRect, ButtonDrawInfo, DC.CGContext,
        kHIThemeOrientationNormal, @LabelRect),
      Self, 'DrawButtonElement', 'HIThemeDrawButton');
      
  Result := CGRectToRect(LabelRect);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawHeaderElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle
  Returns: ClientRect

  Draws a header (THeaderControl same as ListView header) element with native Carbon look
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.DrawHeaderElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
var
  HeaderDrawInfo: HiThemeHeaderDrawInfo;
  PaintRect: HIRect;
begin
  if (R.Bottom - R.Top = 0) or (R.Right - R.Left = 0) then
  begin
    Result := R;
    Exit;
  end;

  HeaderDrawInfo.version := 0;
  HeaderDrawInfo.State := GetDrawState(Details);
  HeaderDrawInfo.kind := kHiThemeHeaderKindList;
  PaintRect := RectToCGRect(R);

  OSError(
    HIThemeDrawHeader(PaintRect, HeaderDrawInfo, DC.CGContext,
      kHIThemeOrientationNormal),
    Self, 'DrawHeaderElement', 'HIThemeDrawHeader');
    
  Result := CGRectToRect(PaintRect);
  InflateRect(Result, -2, -2);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawRebarElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle
  Returns: ClientRect

  Draws a rebar element (splitter) with native Carbon look
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.DrawRebarElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
var
  SplitterInfo: HIThemeSplitterDrawInfo;
  PlacardInfo: HIThemePlacardDrawInfo;
  ARect: HIRect;
const
  SName = 'DrawRebarElement';
begin
  ARect := RectToCGRect(R);
  if Details.Part in [RP_GRIPPER, RP_GRIPPERVERT] then
  begin
    SplitterInfo.version := 0;
    SplitterInfo.State := kThemeStateActive;
    SplitterInfo.adornment := kHiThemeSplitterAdornmentNone;
    
    OSError(
      HIThemeDrawPaneSplitter(ARect, SplitterInfo, DC.CGContext, kHIThemeOrientationNormal),
      Self, SName, 'HIThemeDrawPaneSplitter');
  end
  else
  if Details.Part = RP_BAND then
  begin
    PlacardInfo.version := 0;
    PlacardInfo.State := GetDrawState(Details);

    OSError(
      HIThemeDrawPlacard(ARect, PlacardInfo, DC.CGContext, kHIThemeOrientationNormal),
      Self, SName, 'HIThemeDrawPlacard');
  end;
  
  Result := CGRectToRect(ARect);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawToolBarElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle
  Returns: ClientRect

  Draws a tool bar element with native Carbon look
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.DrawToolBarElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
var
  ButtonDrawInfo: HIThemeButtonDrawInfo;
  LabelRect: HIRect;
begin
  if Details.Part in [TP_BUTTON, TP_DROPDOWNBUTTON, TP_SPLITBUTTON, TP_SPLITBUTTONDROPDOWN] then
  begin
    ButtonDrawInfo.version := 0;
    ButtonDrawInfo.State := GetDrawState(Details);
    case Details.Part of
      TP_BUTTON, TP_SPLITBUTTON: ButtonDrawInfo.kind := kThemeBevelButtonSmall;
      TP_DROPDOWNBUTTON: ButtonDrawInfo.kind := kThemePopupButtonSmall;
      TP_SPLITBUTTONDROPDOWN: ButtonDrawInfo.kind := kThemeDisclosureButton;
    end;

    if Details.Part = TP_SPLITBUTTONDROPDOWN then
    begin
      ButtonDrawInfo.value := kThemeDisclosureDown;
    end
    else
    begin
      if IsChecked(Details) then
        ButtonDrawInfo.value := kThemeButtonOn
      else
        ButtonDrawInfo.value := kThemeButtonOff;
    end;
    ButtonDrawInfo.adornment := kThemeAdornmentNone;

    LabelRect := RectToCGRect(R);

    // if button is normal or disabled, draw it to dummy context, to eliminate borders
    if ((ButtonDrawInfo.State = kThemeStateActive) or
      (ButtonDrawInfo.State = kThemeStateInActive)) and
      (ButtonDrawInfo.value = kThemeButtonOff) then
      OSError(
        HIThemeDrawButton(LabelRect, ButtonDrawInfo, DefaultContext.CGContext,
          kHIThemeOrientationNormal, @LabelRect),
        Self, 'DrawButtonElement', 'HIThemeDrawButton')
    else
      OSError(
        HIThemeDrawButton(LabelRect, ButtonDrawInfo, DC.CGContext,
          kHIThemeOrientationNormal, @LabelRect),
        Self, 'DrawButtonElement', 'HIThemeDrawButton');
        
    Result := CGRectToRect(LabelRect);
  end;
end;

function TCarbonThemeServices.DrawTreeviewElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect): TRect;
var
  ButtonDrawInfo: HIThemeButtonDrawInfo;
  LabelRect: HIRect;
  b: TCarbonBrush;
begin
  case Details.Part of
    TVP_TREEITEM:
    begin
      b:=TCarbonBrush.Create(False);
      b.SetColor( ColorToRGB(clHighlight), True);
      DC.FillRect(R, b);
      b.Free;
    end;
    TVP_GLYPH, TVP_HOTGLYPH:
    begin
      ButtonDrawInfo.version := 0;
      ButtonDrawInfo.State := GetDrawState(Details);
      ButtonDrawInfo.kind := kThemeDisclosureTriangle;
      if Details.State = GLPS_CLOSED then
        ButtonDrawInfo.value := kThemeDisclosureRight
      else
        ButtonDrawInfo.value := kThemeDisclosureDown;

      ButtonDrawInfo.adornment := kThemeAdornmentNone;
      LabelRect := RectToCGRect(R);

      OSError(
        HIThemeDrawButton(LabelRect, ButtonDrawInfo, DC.CGContext,
          kHIThemeOrientationNormal, @LabelRect),
        Self, 'DrawTreeviewElement', 'HIThemeDrawButton');

      Result := CGRectToRect(LabelRect);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.InitThemes
  Returns: If the themes are initialized
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.InitThemes: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.InitThemes
  Returns: If the themes have to be used
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.UseThemes: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.ThemedControlsEnabled
  Returns: If the themed controls are enabled
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.ThemedControlsEnabled: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.ContentRect
  Params:  DC           - Carbon device context
           Details      - Details for themed element
           BoundingRect - Bounding rectangle
  Returns: Content rectangle of the passed themed element
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.ContentRect(DC: HDC;
  Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  case Details.Element of
    teHeader: Result := DrawHeaderElement(DefaultContext, Details, BoundingRect, nil);
    teButton: Result := DrawButtonElement(DefaultContext, Details, BoundingRect, nil);
    teRebar: Result := DrawRebarElement(DefaultContext, Details, BoundingRect, nil);
    teToolBar: Result := DrawToolBarElement(DefaultContext, Details, BoundingRect, nil);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawEdge
  Params:  DC      - Carbon device context
           Details - Details for themed element
           R       - Bounding rectangle
           Edge    - Type of edge
           Flags   - Type of border

  Draws an edge with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawEdge(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
  AContentRect: PRect);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle

  Draws an element with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  Context: TCarbonDeviceContext absolute DC;
begin
  if CheckDC(DC, 'TCarbonThemeServices.DrawElement') then
  begin
    case Details.Element of
      teButton: DrawButtonElement(Context, Details, R, ClipRect);
      teHeader: DrawHeaderElement(Context, Details, R, ClipRect);
      teRebar: DrawRebarElement(Context, Details, R, ClipRect);
      teToolBar: DrawToolBarElement(Context, Details, R, ClipRect);
      teTreeview: DrawTreeviewElement(Context, Details, R, ClipRect);
    else
      inherited DrawElement(DC, Details, R, ClipRect);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawIcon
  Params:  DC      - Carbon device context
           Details - Details for themed element
           R       - Bounding rectangle
           himl    - Image list
           Index   - Icon index

  Draws an icon with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawIcon(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST;
  Index: Integer);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.HasTransparentParts
  Params:  Details - Details for themed element
  Returns: If the themed element has transparent parts
 ------------------------------------------------------------------------------}
function TCarbonThemeServices.HasTransparentParts(Details: TThemedElementDetails): Boolean;
begin
  Result := True;
end;

function TCarbonThemeServices.GetDetailSize(Details: TThemedElementDetails): TSize;
const
  DefaultPushButtonWidth = 70;
begin
  case Details.Element of
    teTreeView:
      if (Details.Part in [TVP_GLYPH, TVP_HOTGLYPH]) then
      begin
        Result := Types.Size(
          GetCarbonThemeMetric(kThemeMetricDisclosureTriangleWidth),
          GetCarbonThemeMetric(kThemeMetricDisclosureTriangleHeight)
        );
      end
      else
        Result := inherited GetDetailSize(Details);
    teButton:
      if Details.Part = BP_PUSHBUTTON then
      begin
        Result := Types.Size(
          DefaultPushButtonWidth,
          GetCarbonThemeMetric(kThemeMetricPushButtonHeight)
        );
      end else
        Result := inherited GetDetailSize(Details);
  else
    Result := inherited GetDetailSize(Details);
  end;
end;

function TCarbonThemeServices.GetOption(AOption: TThemeOption): Integer;
begin
  case AOption of
    toShowButtonImages: Result := 0;
    toShowMenuImages: Result := 0;
  else
    Result := inherited GetOption(AOption);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.InternalDrawParentBackground
  Params:  Window - Handle to window
           Target - Carbon device context
           Bounds - Bounding rectangle

  Draws the parent background with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.InternalDrawParentBackground(Window: HWND;
  Target: HDC; Bounds: PRect);
begin
  // ?
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawText
  Params:  DC      - Carbon device context
           Details - Details for themed element
           S       - Text string to darw
           R       - Bounding rectangle
           Flags   - Draw flags
           Flags2  - Extra draw flags

  Draws the passed text with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: String; R: TRect; Flags, Flags2: Cardinal);
begin
  //
end;

end.

