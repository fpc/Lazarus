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

function TCarbonThemeServices.InitThemes: Boolean;
begin
  Result := True;
end;

function TCarbonThemeServices.UseThemes: Boolean;
begin
  Result := True;
end;

function TCarbonThemeServices.ThemedControlsEnabled: Boolean;
begin
  Result := True;
end;

function TCarbonThemeServices.ContentRect(DC: HDC;
  Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  Result := BoundingRect;
end;

procedure TCarbonThemeServices.DrawEdge(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
  AContentRect: PRect);
begin

end;

procedure TCarbonThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  Context: TCarbonDeviceContext absolute DC;
begin
  if CheckDC(DC, 'TCarbonThemeServices.DrawElement') then
  begin
    case Details.Element of
      teButton: DrawButtonElement(Context, Details, R, ClipRect);
      teToolBar: DrawToolBarElement(Context, Details, R, ClipRect);
    end;
  end;
end;

procedure TCarbonThemeServices.DrawIcon(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST;
  Index: Integer);
begin

end;

function TCarbonThemeServices.HasTransparentParts(Details: TThemedElementDetails): Boolean;
begin
  Result := True;
end;

procedure TCarbonThemeServices.InternalDrawParentBackground(Window: HWND;
  Target: HDC; Bounds: PRect);
begin
  // ?
end;

procedure TCarbonThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: WideString; R: TRect; Flags, Flags2: Cardinal);
begin
  //
end;

end.

