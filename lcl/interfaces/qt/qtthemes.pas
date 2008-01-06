unit QtThemes;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

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
    qdvComplexControl,
    qdvStandardPixmap
  );
  TQtDrawElement = record
    case DrawVariant: TQtDrawVariant of
      qdvPrimitive:
        (PrimitiveElement: QStylePrimitiveElement);
      qdvControl:
        (ControlElement: QStyleControlElement);
      qdvComplexControl:
        (ComplexControl: QStyleComplexControl;
         SubControls: QStyleSubControls);
      qdvStandardPixmap:
        (StandardPixmap: QStyleStandardPixmap);
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
    function GetDetailSize(Details: TThemedElementDetails): Integer; override;
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
  AIcon: QIconH;
  Element: TQtDrawElement;
  Features: QStyleOptionButtonButtonFeatures;
  Position: QStyleOptionHeaderSectionPosition;
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
        if (Element.ControlElement = QStyleCE_HeaderSection) then
        begin
          opt := QStyleOptionHeader_create();
          case Details.Part of
            HP_HEADERITEM: Position := QStyleOptionHeaderMiddle;
            HP_HEADERITEMLEFT: Position := QStyleOptionHeaderBeginning;
            HP_HEADERITEMRIGHT: Position := QStyleOptionHeaderEnd;
          end;
          QStyleOptionHeader_setPosition(QStyleOptionHeaderH(opt), Position);
          QStyleOptionHeader_setOrientation(QStyleOptionHeaderH(opt), QtHorizontal);
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
        Context.save;
        case Element.ComplexControl of
          QStyleCC_ToolButton: opt := QStyleOptionToolButton_create();
          QStyleCC_TitleBar,
          QStyleCC_MdiControls:
          begin
            opt := QStyleOptionTitleBar_create();
            QStyleOptionTitleBar_setTitleBarFlags(QStyleOptionTitleBarH(opt), QtWindow or QtWindowSystemMenuHint);
            // workaround: qt has own minds about position of requested part -
            // but we need a way to draw it at our position
            Context.translate(ARect.Left, ARect.Top);
            OffsetRect(ARect, -ARect.Left, -ARect.Top);
          end;
        else
          opt := QStyleOptionComplex_create(LongInt(QStyleOptionVersion), LongInt(QStyleOptionSO_Default));
        end;
        
        QStyleOptionComplex_setSubControls(QStyleOptionComplexH(opt), Element.SubControls);
        
        QStyleOption_setState(opt, GetControlState(Details));
        QStyleOption_setRect(opt, @ARect);
        QStyle_drawComplexControl(Style, Element.ComplexControl, QStyleOptionComplexH(opt), Context.Widget);
        QStyleOption_Destroy(opt);
        Context.restore;
      end;
      qdvPrimitive:
      begin
        opt := QStyleOption_create(Integer(QStyleOptionVersion), Integer(QStyleOptionSO_Default));
        QStyleOption_setState(opt, GetControlState(Details));
        QStyleOption_setRect(opt, @ARect);
        QStyle_drawPrimitive(Style, Element.PrimitiveElement, opt, Context.Widget);
        QStyleOption_Destroy(opt);
      end;
      qdvStandardPixmap:
      begin
        opt := QStyleOption_create(Integer(QStyleOptionVersion), Integer(QStyleOptionSO_Default));
        AIcon := QIcon_create();
        QStyle_standardIcon(Style, AIcon, Element.StandardPixmap, opt);
        QIcon_paint(AIcon, Context.Widget, ARect.Left, ARect.Top,
          ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
        QIcon_destroy(AIcon);
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
    
  // specific states

  // define orientations
  if ((Details.Element = teRebar) and (Details.Part = RP_GRIPPER)) or
     ((Details.Element = teToolBar) and (Details.Part = TP_SEPARATOR)) then
    Result := Result or QStyleState_Horizontal;
end;

function TQtThemeServices.GetDetailSize(Details: TThemedElementDetails): Integer;
begin
  case Details.Element of
    teRebar :
      if Details.Part in [RP_GRIPPER, RP_GRIPPERVERT] then
        Result := -1;
    else
      Result := inherited;
  end;
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
          Result.ComplexControl := QStyleCC_GroupBox;
          Result.SubControls := QStyleSC_GroupBoxFrame;
        end;
      end;
    teHeader:
      begin
        case Details.Part of
          HP_HEADERITEM,
          HP_HEADERITEMLEFT,
          HP_HEADERITEMRIGHT:
            begin
              Result.DrawVariant := qdvControl;
              Result.ControlElement := QStyleCE_HeaderSection;
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
        case Details.Part of
          TP_BUTTON,
          TP_DROPDOWNBUTTON,
          TP_SPLITBUTTON: // there is another positibility to draw TP_SPLITBUTTON by CC_ToolButton
            begin
              Result.DrawVariant := qdvPrimitive;
              Result.PrimitiveElement := QStylePE_PanelButtonTool;
            end;
          TP_SPLITBUTTONDROPDOWN:
            begin
              Result.DrawVariant := qdvPrimitive;
              Result.PrimitiveElement := QStylePE_IndicatorButtonDropDown;
            end;
          TP_SEPARATOR,
          TP_SEPARATORVERT:
            begin
              Result.DrawVariant := qdvPrimitive;
              Result.PrimitiveElement := QStylePE_IndicatorToolBarSeparator;
            end;
        end;
      end;
    teRebar:
      begin
        case Details.Part of
          RP_GRIPPER, RP_GRIPPERVERT: // used in splitter
            begin
              Result.DrawVariant := qdvControl;
              Result.ControlElement := QStyleCE_Splitter;
            end;
        end;
      end;
    teWindow:
      begin
        case Details.Part of
          WP_SYSBUTTON: Result.SubControls := QStyleSC_TitleBarSysMenu;
          WP_MINBUTTON: Result.SubControls := QStyleSC_TitleBarMinButton;
          WP_MDIMINBUTTON: Result.SubControls := QStyleSC_MdiMinButton;
          WP_MAXBUTTON: Result.SubControls := QStyleSC_TitleBarMaxButton;
          WP_CLOSEBUTTON: Result.SubControls := QStyleSC_TitleBarCloseButton;
          WP_SMALLCLOSEBUTTON: Result.SubControls := QStyleSC_TitleBarCloseButton;
          WP_MDICLOSEBUTTON: Result.SubControls := QStyleSC_MdiCloseButton;
          WP_RESTOREBUTTON: Result.SubControls := QStyleSC_TitleBarNormalButton;
          WP_MDIRESTOREBUTTON: Result.SubControls := QStyleSC_MdiNormalButton;
          WP_HELPBUTTON: Result.SubControls := QStyleSC_TitleBarContextHelpButton;
          WP_MDIHELPBUTTON: Result.SubControls := QStyleSC_TitleBarContextHelpButton;
        else
          Result.SubControls := QStyleSC_None;
        end;
        if Result.SubControls >= QStyleSC_MdiMinButton then
          Result.ComplexControl := QStyleCC_MdiControls
        else
          Result.ComplexControl := QStyleCC_TitleBar;
        Result.DrawVariant := qdvComplexControl;
{
        // maybe through icon
        Result.DrawVariant := qdvStandardPixmap;
        case Details.Part of
          WP_MINBUTTON: Result.StandardPixmap := QStyleSP_TitleBarMinButton;
          WP_MDIMINBUTTON: Result.StandardPixmap := QStyleSP_TitleBarMinButton;
          WP_MAXBUTTON: Result.StandardPixmap := QStyleSP_TitleBarMaxButton;
          WP_CLOSEBUTTON: Result.StandardPixmap := QStyleSP_TitleBarCloseButton;
          WP_SMALLCLOSEBUTTON: Result.StandardPixmap := QStyleSP_TitleBarCloseButton;
          WP_MDICLOSEBUTTON: Result.StandardPixmap := QStyleSP_TitleBarCloseButton;
          WP_RESTOREBUTTON: Result.StandardPixmap := QStyleSP_TitleBarNormalButton;
          WP_MDIRESTOREBUTTON: Result.StandardPixmap := QStyleSP_TitleBarNormalButton;
          WP_HELPBUTTON: Result.StandardPixmap := QStyleSP_TitleBarContextHelpButton;
          WP_MDIHELPBUTTON: Result.StandardPixmap := QStyleSP_TitleBarContextHelpButton;
        else
          Result.StandardPixmap := QStyleSP_TitleBarCloseButton;
        end;
}
      end;
  end;
end;

end.


