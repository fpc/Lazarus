unit QtThemes;

{$mode objfpc}{$H+}

interface

uses
  // rtl
  Types, Classes, SysUtils,
  // qt bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  // lcl
  LCLType, LCLProc, LCLIntf, Graphics, Themes, TmSchema,
  // widgetset
  InterfaceBase, QtObjects
  ;
  
type
  TQtDrawVariant =
  (
    qdvNone,
    qdvPrimitive,
    qdvControl,
    qdvComplexControl
  );
  TQtDrawElement = record
    case DrawVariant: TQtDrawVariant of
      qdvPrimitive     : (PrimitiveElement: QStylePrimitiveElement);
      qdvControl       : (ControlElement  : QStyleControlElement);
      qdvComplexControl: (ComplexElement  : QStyleComplexControl);
  end;

  { TQtThemeServices }
  TQtThemeServices = class(TThemeServices)
  private
    FStyle: QStyleH;
    function GetStyle: QStyleH;
  protected
    function InitThemes: Boolean; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;
    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); override;
    
    function GetControlState(Details: TThemedElementDetails): QStyleState;
    function GetDrawElement(Details: TThemedElementDetails): TQtDrawElement;
    property Style: QStyleH read GetStyle;
  public
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); override;
    procedure DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal; AContentRect: PRect); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST; Index: Integer); override;

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
  end;

implementation

{ TQtThemeServices }

function TQtThemeServices.GetStyle: QStyleH;
begin
  if FStyle = nil then
    FStyle := QApplication_style();
  Result := FStyle;
end;

function TQtThemeServices.InitThemes: Boolean;
begin
  FStyle := nil;
  Result := True;
end;

function TQtThemeServices.UseThemes: Boolean;
begin
  Result := True;
end;

function TQtThemeServices.ThemedControlsEnabled: Boolean;
begin
  Result := True;
end;

function TQtThemeServices.ContentRect(DC: HDC;
  Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  Result := BoundingRect;
  InflateRect(Result, -1, -1);
end;

procedure TQtThemeServices.DrawEdge(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
  AContentRect: PRect);
begin

end;

procedure TQtThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  Context: TQtDeviceContext absolute DC;
  opt: QStyleOptionH;
  ARect: TRect;
  Element: TQtDrawElement;
  Features: QStyleOptionButtonButtonFeatures;
begin
  if (Context <> nil) then
  begin
    ARect := R;
    Element := GetDrawElement(Details);
    case Element.DrawVariant of
      qdvControl:
      begin
        if (Element.ControlElement in [QStyleCE_PushButton, QStyleCE_RadioButton, QStyleCE_CheckBox]) then
        begin
          opt := QStyleOptionButton_create();
          Features := QStyleOptionButtonNone;
          if Details.Element = teToolBar then
            Features := Features or QStyleOptionButtonFlat;
          QStyleOptionButton_setFeatures(QStyleOptionButtonH(opt), Features);
        end
        else
          opt := QStyleOptionComplex_create(LongInt(QStyleOptionVersion), LongInt(QStyleOptionSO_Default));

        QStyleOption_setState(opt, GetControlState(Details));
        QStyleOption_setRect(opt, @ARect);

        QStyle_drawControl(Style, Element.ControlElement, opt, Context.Widget);
        QStyleOption_Destroy(opt);
      end;
      qdvComplexControl:
      begin
        if Element.ComplexElement = QStyleCC_ToolButton then
          opt := QStyleOptionToolButton_create()
        else
          opt := QStyleOptionComplex_create(LongInt(QStyleOptionVersion), LongInt(QStyleOptionSO_Default));

        QStyleOption_setState(opt, GetControlState(Details));
        QStyleOption_setRect(opt, @ARect);
        QStyle_drawComplexControl(Style, Element.ComplexElement, QStyleOptionComplexH(opt), Context.Widget);
        QStyleOption_Destroy(opt);
      end;
      qdvPrimitive:
      begin
        opt := QStyleOption_create(Integer(QStyleOptionVersion), Integer(QStyleOptionSO_Default));
        QStyleOption_setState(opt, GetControlState(Details));
        QStyleOption_setRect(opt, @ARect);
        QStyle_drawPrimitive(Style, Element.PrimitiveElement, opt, Context.Widget);
        QStyleOption_Destroy(opt);
      end;
    end;
  end;
end;

procedure TQtThemeServices.DrawIcon(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST;
  Index: Integer);
begin

end;

function TQtThemeServices.HasTransparentParts(Details: TThemedElementDetails): Boolean;
begin
  Result := True;
end;

procedure TQtThemeServices.InternalDrawParentBackground(Window: HWND;
  Target: HDC; Bounds: PRect);
begin
  // ?
end;

function TQtThemeServices.GetControlState(Details: TThemedElementDetails): QStyleState;
begin
{
  QStyleState_None
  QStyleState_Enabled
  QStyleState_Raised
  QStyleState_Sunken
  QStyleState_Off
  QStyleState_NoChange
  QStyleState_On
  QStyleState_DownArrow
  QStyleState_Horizontal
  QStyleState_HasFocus
  QStyleState_Top
  QStyleState_Bottom
  QStyleState_FocusAtBorder
  QStyleState_AutoRaise
  QStyleState_MouseOver
  QStyleState_UpArrow
  QStyleState_Selected
  QStyleState_Active
  QStyleState_Open
  QStyleState_Children
  QStyleState_Item
  QStyleState_Sibling
  QStyleState_Editing
  QStyleState_KeyboardFocusChange
  QStyleState_ReadOnly
}
  Result := QStyleState_None;
  
  if not IsDisabled(Details) then
    Result := Result or QStyleState_Enabled;

  if IsHot(Details) then
    Result := Result or QStyleState_MouseOver;
    
  if IsPushed(Details) then
    Result := Result or QStyleState_Sunken;

  if IsChecked(Details) then
    Result := Result or QStyleState_On
  else
  if IsMixed(Details) then
    Result := Result or QStyleState_NoChange;
end;

function TQtThemeServices.GetDrawElement(Details: TThemedElementDetails): TQtDrawElement;
const
  ButtonMap: array[BP_PUSHBUTTON..BP_USERBUTTON] of QStyleControlElement =
  (
{BP_PUSHBUTTON } QStyleCE_PushButton,
{BP_RADIOBUTTON} QStyleCE_RadioButton,
{BP_CHECKBOX   } QStyleCE_CheckBox,
{BP_GROUPBOX   } QStyleCE_PushButton,
{BP_USERBUTTON } QStyleCE_PushButton
  );
begin
  Result.DrawVariant := qdvNone;
  case Details.Element of
    teButton:
      begin
        if Details.Part <> BP_GROUPBOX then
        begin
          Result.DrawVariant := qdvControl;
          Result.ControlElement := ButtonMap[Details.Part]
        end
        else
        begin
          Result.DrawVariant := qdvComplexControl;
          Result.ComplexElement := QStyleCC_GroupBox;
        end;
      end;
    teHeader:
      begin
        case Details.Part of
          HP_HEADERITEM:
            begin
              Result.DrawVariant := qdvControl;
              Result.ControlElement := QStyleCE_HeaderSection;
            end;
          HP_HEADERITEMLEFT,
          HP_HEADERITEMRIGHT:
            begin
              Result.DrawVariant := qdvControl;
            {$ifdef USE_QT_4_3}
              Result.ControlElement := QStyleCE_HeaderEmptyArea;
            {$else}
              Result.ControlElement := QStyleCE_HeaderSection;
            {$endif}
            end;
          HP_HEADERSORTARROW:
            begin
              Result.DrawVariant := qdvPrimitive;
              Result.PrimitiveElement := QStylePE_IndicatorHeaderArrow;
            end;
        end;
      end;
    teToolBar:
      begin
        if Details.Part = TP_BUTTON then
        begin
          Result.DrawVariant := qdvPrimitive;
          Result.PrimitiveElement := QStylePE_PanelButtonTool;
        end;
      end;
  end;
end;

end.


