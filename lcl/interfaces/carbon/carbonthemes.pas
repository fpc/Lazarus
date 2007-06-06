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
  Types, Classes, SysUtils,
  // carbon bindings
  FPCMacOSAll,
  // lcl
  LCLType, LCLProc, LCLIntf, Graphics, Themes, TmSchema,
  // widgetset
  CarbonProc, CarbonCanvas;
  
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
    procedure DrawButtonElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; ClipRect: PRect);
    procedure DrawHeaderElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; ClipRect: PRect);
    procedure DrawRebarElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; ClipRect: PRect);
    procedure DrawToolBarElement(DC: TCarbonDeviceContext; Details: TThemedElementDetails; R: TRect; ClipRect: PRect);
  public
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); override;
    procedure DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal; AContentRect: PRect); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST; Index: Integer); override;
    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: WideString; R: TRect; Flags, Flags2: Cardinal); override;

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
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

	kThemeStatePressedUp = 2;    { draw with up pressed     (increment/decrement buttons) }
	kThemeStatePressedDown = 3;     { draw with down pressed (increment/decrement buttons) }

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

  Draws a button element with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawButtonElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect);
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
  ButtonDrawInfo.value := kThemeButtonOff;
  ButtonDrawInfo.adornment := kThemeAdornmentNone;

  //InflateRect(R, 0, -2); // HiThemeDrawButton can draw outside it rect
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
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawHeaderElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle

  Draws a header (THeaderControl same as ListView header) element with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawHeaderElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect);
var
  HeaderDrawInfo: HiThemeHeaderDrawInfo;
  PaintRect: HIRect;
begin
  HeaderDrawInfo.version := 0;
  HeaderDrawInfo.State := GetDrawState(Details);
  HeaderDrawInfo.kind := kHiThemeHeaderKindList;
  PaintRect := RectToCGRect(R);
  OSError(
    HIThemeDrawHeader(PaintRect, HeaderDrawInfo, DC.CGContext,
      kHIThemeOrientationNormal),
    Self, 'DrawHeaderElement', 'HIThemeDrawHeader')
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawRebarElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle

  Draws a rebar element (splitter) with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawRebarElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect);
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
end;

{------------------------------------------------------------------------------
  Method:  TCarbonThemeServices.DrawToolBarElement
  Params:  DC       - Carbon device context
           Details  - Details for themed element
           R        - Bounding rectangle
           ClipRect - Clipping rectangle

  Draws a tool bar element with native Carbon look
 ------------------------------------------------------------------------------}
procedure TCarbonThemeServices.DrawToolBarElement(DC: TCarbonDeviceContext;
  Details: TThemedElementDetails; R: TRect; ClipRect: PRect);
var
  ButtonDrawInfo: HIThemeButtonDrawInfo;
  LabelRect: HIRect;
begin
  if Details.Part = TP_BUTTON then
  begin

    // TODO: if state is inactive or normal button should not have borders (or maybe I am wrong for mac?)

    ButtonDrawInfo.version := 0;
    ButtonDrawInfo.State := GetDrawState(Details);
    ButtonDrawInfo.kind := kThemeBevelButtonSmall;
    ButtonDrawInfo.value := kThemeButtonOff;
    ButtonDrawInfo.adornment := kThemeAdornmentNone;

    //InflateRect(R, 0, -2); // HiThemeDrawButton can draw outside it rect
    LabelRect := RectToCGRect(R);

    OSError(
      HIThemeDrawButton(LabelRect, ButtonDrawInfo, DC.CGContext,
        kHIThemeOrientationNormal, @LabelRect),
      Self, 'DrawButtonElement', 'HIThemeDrawButton');
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
  Result := BoundingRect;

  // If you know how to get actual value, please do
  // This should be one of theme metric

  case Details.Element of
    teHeader: InflateRect(Result, -2, -2);
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
  const S: WideString; R: TRect; Flags, Flags2: Cardinal);
begin
  //
end;

end.

