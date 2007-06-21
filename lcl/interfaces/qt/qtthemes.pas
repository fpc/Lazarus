unit QtThemes;

{$mode objfpc}{$H+}

{
  This theme services class is not ready. It waiting for some bindings. Without
  them it is not useful.
  
  Waiting for:
      QStyleOption_setState
      QStyleOption_setRect
      ... and other QStyleOption setters and getters
}

interface

uses
  // rtl
  Types, Classes, SysUtils,
  // qt bindings
{$ifdef USE_QT_4_2}
  qt42,
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
    
    function CreateQRect(R: TRect): QRectH;
    function GetControlState(Details: TThemedElementDetails): QStyleState;
    function GetDrawElement(Details: TThemedElementDetails): TQtDrawElement;
    property Style: QStyleH read GetStyle;
  public
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); override;
    procedure DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal; AContentRect: PRect); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST; Index: Integer); override;
    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: WideString; R: TRect; Flags, Flags2: Cardinal); override;

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
  ARect: QRectH;
  Element: TQtDrawElement;
  QVariant: QVariantH;
  Features: QStyleOptionButtonButtonFeatures;
begin
  if (Context <> nil) then
  begin
    ARect := CreateQRect(R);
    Element := GetDrawElement(Details);
    if (Element.DrawVariant = qdvControl) and (Element.ControlElement = QStyleCE_PushButton) then
    begin
      opt := QStyleOptionButton_create();
      Features := 0;
      if Details.Element = teToolBar then
        Features := Features or QStyleOptionButtonFlat
      else
        Features := Features or QStyleOptionButtonNone;

{ // TODO: wait for bindings

      QStyleOptionButton_setButtonFeatures(opt, Features);
      QStyleOption_setState(opt, GetControlState(Details));
      QStyleOption_setRect(opt, ARect);
      
}
      QStyle_drawControl(Style, Element.ControlElement, opt, Context.Widget);
      QStyleOption_Destroy(opt);
    end
    else
    if (Element.DrawVariant = qdvComplexControl) then
    begin
      if Element.ComplexElement = QStyleCC_ToolButton then
        opt := QStyleOptionToolButton_create()
      else
        opt := QStyleOptionComplex_create(LongInt(QStyleOptionVersion), LongInt(QStyleOptionSO_Default));
        
{ // TODO: wait for bindings

      QStyleOption_setState(opt, GetControlState(Details));
      QStyleOption_setRect(opt, ARect);
}
      QStyle_drawComplexControl(Style, Element.ComplexElement, QStyleOptionComplexH(opt), Context.Widget);
      QStyleOption_Destroy(opt);
    end;
    QRect_destroy(ARect);
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

function TQtThemeServices.CreateQRect(R: TRect): QRectH;
begin
  Result := QRect_create();
  with R do
  begin
    QRect_setLeft(Result, Left);
    QRect_setRight(Result, Right);
    QRect_setTop(Result, Top);
    QRect_setBottom(Result, Bottom);
  end;
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
    teToolBar:
      begin
        if Details.Part = TP_BUTTON then
        begin
          Result.DrawVariant := qdvComplexControl;
          Result.ComplexElement := QStyleCC_ToolButton;
        end;
      end;
  end;
end;

procedure TQtThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: WideString; R: TRect; Flags, Flags2: Cardinal);
begin
  //
end;

end.

