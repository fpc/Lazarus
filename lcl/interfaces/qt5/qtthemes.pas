unit QtThemes;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // rtl
  Types, Classes, SysUtils,
  // qt bindings
  qt5,
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
         SubControls: QStyleSubControls;
         Features: Cardinal);
      qdvStandardPixmap:
        (StandardPixmap: QStyleStandardPixmap);
  end;

  { TQtThemeServices }
  
  TQtThemeServices = class(TThemeServices)
  private
    FStyle: QStyleH;
    function GetStyle: QStyleH;
    function GetStyleName: WideString;
  protected
    function InitThemes: Boolean; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;
    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); override;
    
    function GetControlState(Details: TThemedElementDetails): QStyleState;
    function GetDrawElement(Details: TThemedElementDetails): TQtDrawElement;
    property Style: QStyleH read GetStyle;
    property StyleName: WideString read GetStyleName;
  public
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); override;
    procedure DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal; AContentRect: PRect); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST; Index: Integer); override;
    procedure DrawText(ACanvas: TPersistent; Details: TThemedElementDetails; const S: String; R: TRect; Flags, Flags2: Cardinal); overload; override;
    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: String; R: TRect; Flags, Flags2: Cardinal); overload; override;
    function GetDetailSize(Details: TThemedElementDetails): TSize; override;
    function GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean; override;

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
  end;

implementation
uses qtint, qtproc;

{ TQtThemeServices }

function TQtThemeServices.GetStyle: QStyleH;
begin
  FStyle := QApplication_style();
  Result := FStyle;
end;

function TQtThemeServices.GetStyleName: WideString;
begin
  QObject_objectName(Style, @Result);
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
  DebugLn('WARNING: TQtThemeServices.DrawEdge is not implemented.');
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
  Palette: QPaletteH;
  ABrush: QBrushH;
  Widget: QWidgetH;
  AViewportPaint: Boolean;
  StyleState: QStyleState;
  {$IFDEF DARWIN}
  ClipR: TRect; // fix branch indicators (treeviews)
  {$ENDIF}
  dx, dy: integer;
  APalette: QPaletteH;
  W: WideString;

  procedure DrawSplitterInternal;
  var
    lx, ly, d, lt, i: Integer;
    r1: TRect;
    AQtColor: QColorH;
    ADarkColor, ALightColor: TQColor;
    APalette: QPaletteH;
    NumDots: integer;
    APen: QPenH;
  begin
    r1 := ARect;
    NumDots := 10;
    APalette := QPalette_create;
    try
      QStyleOption_palette(opt, APalette);
      AQtColor := QColor_create(QBrush_color(QPalette_background(APalette)));

      QColor_darker(AQtColor, @ADarkColor);
      QColor_lighter(AQtColor, @ALightColor);

      if StyleState and QStyleState_Horizontal = 0 then
      begin
        if (r1.Right - r1.Left) <= (NumDots * 2) then
          NumDots := (r1.Right - r1.Left) div 2;
        lx := ((r1.Right - r1.Left) div 2) - (NumDots * 2);
        d := (r1.Bottom - r1.Top - 2) div 2;
        lt := r1.Top + d + 1;
        APen := QPen_create(QPainter_pen(Context.Widget));
        for i := 0 to NumDots - 1 do
        begin
          QPen_setColor(APen, PQColor(@ALightColor));
          QPainter_setPen(Context.Widget, APen);
          QPainter_drawPoint(Context.Widget, lx, lt);
          QPen_setColor(APen, PQColor(@ADarkColor));
          QPainter_setPen(Context.Widget, APen);
          QPainter_drawPoint(Context.Widget, lx + 1, lt);
          QPainter_drawPoint(Context.Widget, lx, lt + 1);
          QPainter_drawPoint(Context.Widget, lx + 1, lt + 1);
          lx := lx + 4;
        end;
        QPen_destroy(APen);
      end else
      begin
        if (r1.Bottom - r1.Top) <= (NumDots * 2) then
          NumDots := (r1.Bottom - r1.Top) div 2;
        ly := ((r1.Bottom - r1.Top) div 2) + (NumDots * 2);
        d := (r1.Right - r1.Left - 2) div 2;
        lt := r1.Left + d + 1;
        APen := QPen_create(QPainter_pen(Context.Widget));
        for i := 0 to NumDots - 1 do
        begin
          QPen_setColor(APen, PQColor(@ALightColor));
          QPainter_setPen(Context.Widget, APen);
          QPainter_drawPoint(Context.Widget, lt, ly);
          QPen_setColor(APen, PQColor(@ADarkColor));
          QPainter_setPen(Context.Widget, APen);
          QPainter_drawPoint(Context.Widget, lt + 1, ly);
          QPainter_drawPoint(Context.Widget, lt, ly + 1);
          QPainter_drawPoint(Context.Widget, lt + 1, ly + 1);
          ly := ly - 4;
        end;
        QPen_destroy(APen);
      end;
    finally
      QPalette_destroy(APalette);
      QColor_destroy(AQtColor);
    end;
  end;
begin
  if (Context <> nil) and not IsRectEmpty(R) then
  begin
    AViewportPaint := False;
    Context.save;
    try
      if Context.Parent <> nil then
      begin
        Widget := QWidget_parentWidget(Context.Parent);
        if (Widget <> nil) and QObject_inherits(Widget,'QAbstractScrollArea') then
        begin
          {do not set any palette on QAbstractScrollArea viewport ! }
          AViewportPaint := True;
        end else
        begin
          Palette := QWidget_palette(Context.Parent);
          QPainter_setBackground(Context.Widget, QPalette_background(Palette));
        end;
      end else
      begin
        Palette := QPalette_create();
        QGUIApplication_palette(Palette);
        QPainter_setBackground(Context.Widget, QPalette_background(Palette));
        QPalette_destroy(Palette);
      end;

      if HasTransparentParts(Details) then
        QPainter_setBackgroundMode(Context.Widget, QtTransparentMode);

      ARect := R;
      Element := GetDrawElement(Details);
      StyleState := GetControlState(Details);
      case Element.DrawVariant of
        qdvNone:
          inherited DrawElement(DC, Details, R, ClipRect);
        qdvControl:
        begin
          if (Element.ControlElement in [QStyleCE_ProgressBar, QStyleCE_ProgressBarContents,
            QStyleCE_ProgressBarGroove]) then
          begin
            opt := QStyleOptionProgressBar_create;

            if Element.ControlElement <> QStyleCE_ProgressBarContents then
            begin
              QStyleOptionProgressBar_setMinimum(QStyleOptionProgressBarH(opt), 0);
              QStyleOptionProgressBar_setMaximum(QStyleOptionProgressBarH(opt), 100);
            end;

            if Element.Features = QtVertical then
            begin
              QStyleOptionProgressBar_setOrientation(QStyleOptionProgressBarH(opt), QtVertical);
              QStyleOptionProgressBar_setInvertedAppearance(QStyleOptionProgressBarH(opt), True);
              QStyleOptionProgressBar_setBottomToTop(QStyleOptionProgressBarH(opt), True);
            end else
              QStyleOptionProgressBar_setOrientation(QStyleOptionProgressBarH(opt), QtHorizontal);
          end else
          if (Element.ControlElement = QStyleCE_TabBarTabShape) then
          begin
            opt := QStyleOptionTab_create();
            QStyleOptionTab_setShape(QStyleOptionTabH(opt), QTabBarShape(Element.Features));
          end else
          if (Element.ControlElement in [QStyleCE_PushButton, QStyleCE_RadioButton, QStyleCE_CheckBox]) then
          begin
            opt := QStyleOptionButton_create();
            Features := QStyleOptionButtonNone;
            if Details.Element = teToolBar then
              Features := Features or QStyleOptionButtonFlat;
            QStyleOptionButton_setFeatures(QStyleOptionButtonH(opt), Features);

            // workaround for qt QStyle bug. QStyle does not set disable flag (palette).
            // see issue #24413,#26586
            if Details.State in [CBS_UNCHECKEDDISABLED,CBS_CHECKEDDISABLED,CBS_MIXEDDISABLED] then
            begin
              APalette := QPalette_create();
              try
                QStyleOption_palette(opt, APalette);
                QPalette_setCurrentColorGroup(APalette, QPaletteDisabled);
                QStyleOption_setPalette(opt, APalette);
              finally
                QPalette_destroy(APalette);
              end;
            end;
          end else
          if (Element.ControlElement = QStyleCE_HeaderSection) then
          begin
            opt := QStyleOptionHeader_create();
            case Details.Part of
              HP_HEADERITEM: Position := QStyleOptionHeaderMiddle;
              HP_HEADERITEMLEFT: Position := QStyleOptionHeaderBeginning;
              HP_HEADERITEMRIGHT: Position := QStyleOptionHeaderEnd;
            end;

            W := GetStyleName;
            // fix for oxygen and breeze weird drawing of header sections. issue #23143
            if ((W = 'oxygen') or (W = 'breeze')) and (Position = QStyleOptionHeaderMiddle) then
            begin
              // see if this is needed (in case of fixedRows in grids)
              // if (ARect.Left > 0) or ((ARect.Left = 0) and (ARect.Top = 0)) then
              Position := QStyleOptionHeaderBeginning;
            end;

            QStyleOptionHeader_setPosition(QStyleOptionHeaderH(opt), Position);
            QStyleOptionHeader_setOrientation(QStyleOptionHeaderH(opt), QtHorizontal);
          end
          else
          if (Element.ControlElement = QStyleCE_ItemViewItem) then
          begin
            opt := QStyleOptionViewItem_create();
            QStyleOptionViewItem_setShowDecorationSelected(QStyleOptionViewItemH(opt), True);
          end
          else
          if (Element.ControlElement = QStyleCE_SizeGrip) then
          begin
            opt := QStyleOptionSizeGrip_create();
            QStyleOptionSizeGrip_setCorner(QStyleOptionSizeGripH(opt), QtBottomRightCorner);
            QStyleOption_setDirection(QStyleOptionH(opt), QtLeftToRight);
          end
          else
            opt := QStyleOptionComplex_create(LongInt(QStyleOptionVersion), LongInt(QStyleOptionSO_Default));

          QStyleOption_setState(opt, StyleState);
          dx := ARect.Left;
          dy := ARect.Top;
          Context.translate(dx, dy);
          OffsetRect(ARect, -dx, -dy);
          QStyleOption_setRect(opt, @ARect);

          // issue #27182 qt5 does not implement splitter grabber in some themes.
          if (Element.ControlElement = QStyleCE_Splitter) and
            ( (GetStyleName = 'windows') or (GetStyleName = 'breeze')
              {$IFDEF MSWINDOWS} or true {$ENDIF}) then
            drawSplitterInternal
          else
            QStyle_drawControl(Style, Element.ControlElement, opt, Context.Widget, Context.Parent);

          Context.translate(-dx, -dy);
          QStyleOption_Destroy(opt);
        end;
        qdvComplexControl:
        begin
          case Element.ComplexControl of
            QStyleCC_GroupBox:
            begin
              opt := QStyleOptionGroupBox_create();
              Context.translate(ARect.Left, ARect.Top);
              OffsetRect(ARect, -ARect.Left, -ARect.Top);
            end;
            QStyleCC_ToolButton:
            begin
              opt := QStyleOptionToolButton_create();
              QStyleOptionToolButton_setFeatures(QStyleOptionToolButtonH(opt),
                Element.Features);
            end;
            QStyleCC_ComboBox:
            begin
              opt := QStyleOptionComboBox_create();
              if Element.Features = Ord(QtRightToLeft) then
                QStyleOption_setDirection(opt, QtRightToLeft);
              if Details.State in [CBXS_DISABLED] then
              begin
                APalette := QPalette_create();
                try
                  QStyleOption_palette(opt, APalette);
                  QPalette_setCurrentColorGroup(APalette, QPaletteDisabled);
                  QStyleOption_setPalette(opt, APalette);
                finally
                  QPalette_destroy(APalette);
                end;
              end;
            end;
            QStyleCC_SpinBox:
            begin
              opt := QStyleOptionSpinBox_create;
            end;
            QStyleCC_TitleBar, QStyleCC_MdiControls:
            begin
              opt := QStyleOptionTitleBar_create();
              if Element.SubControls = QStyleSC_TitleBarLabel then
                QStyleOptionTitleBar_setTitleBarFlags(QStyleOptionTitleBarH(opt),
                  QtWindow or QtWindowTitleHint)
              else
                QStyleOptionTitleBar_setTitleBarFlags(QStyleOptionTitleBarH(opt),
                  QtWindow or QtWindowSystemMenuHint);
              // workaround: qt has own minds about position of requested part -
              // but we need a way to draw it at our position
              Context.translate(ARect.Left, ARect.Top);
              OffsetRect(ARect, -ARect.Left, -ARect.Top);
            end;
            QStyleCC_Slider, QStyleCC_ScrollBar:
            begin
              opt := QStyleOptionSlider_create();
              QStyleOptionSlider_setMinimum(QStyleOptionSliderH(opt), 0);
              QStyleOptionSlider_setMaximum(QStyleOptionSliderH(opt), 100);
              if Element.ComplexControl = QStyleCC_Slider then
              begin
                if Element.Features = QtVertical then
                  QStyleOptionSlider_setOrientation(QStyleOptionSliderH(opt), QtVertical)
                else
                  QStyleOptionSlider_setOrientation(QStyleOptionSliderH(opt), QtHorizontal);
              end;
            end;
          else
            opt := QStyleOptionComplex_create(LongInt(QStyleOptionVersion),
              LongInt(QStyleOptionSO_Default));
          end;

          if Element.SubControls > QStyleSC_None then
            QStyleOptionComplex_setSubControls(QStyleOptionComplexH(opt),
              Element.SubControls);

          QStyleOption_setState(opt, StyleState);
          QStyleOption_setRect(opt, @ARect);
          QStyle_drawComplexControl(Style, Element.ComplexControl,
            QStyleOptionComplexH(opt), Context.Widget, Context.Parent);
          QStyleOption_Destroy(opt);
        end;
        qdvPrimitive:
        begin
          case Element.PrimitiveElement of
            QStylePE_FrameTabWidget:
              begin
                opt := QStyleOptionTabWidgetFrame_create();
                // need widget to draw gradient
              end;
            QStylePE_FrameFocusRect:
              begin
                opt := QStyleOptionFocusRect_create();
              end;
            QStylePE_PanelTipLabel:
              begin
                opt := QStyleOptionFrame_create();
              end;
            QStylePE_IndicatorBranch:
              begin

                opt := QStyleOption_create(Ord(QStyleOptionVersion), Ord(QStyleOptionSO_Default));

                QStyleOption_setState(opt, StyleState);
                if AViewPortPaint then
                begin
                  {$IFDEF DARWIN}
                  if (AnsiPos('macintosh', StyleName) > 0) and
                    Context.getClipping then
                  begin
                    ClipR := Context.getClipRegion.getBoundingRect;
                    if (ClipR.Left = 0) and (ClipR.Top = 0) and (ARect.Left > 0)
                      and (ARect.Top > 0) then
                    begin
                      ClipR.Left := (ARect.Right - ARect.Left + 1) div 3;
                      OffsetRect(ARect, -ClipR.Left, -1);
                    end;
                  end;
                  {$ENDIF}
                  Context.translate(-1, -1);
                  ABrush := QBrush_create(QPainter_brush(Context.Widget));
                  QBrush_setStyle(ABrush, QtNoBrush);
                  QPainter_setBrush(Context.Widget, ABrush);
                  QBrush_destroy(ABrush);
                end;
              end;
            else
              opt := QStyleOption_create(Ord(QStyleOptionVersion), Ord(QStyleOptionSO_Default));
          end;

          QStyleOption_setState(opt, StyleState);
          QStyleOption_setRect(opt, @ARect);
          QStyle_drawPrimitive(Style, Element.PrimitiveElement, opt, Context.Widget,
            Context.Parent);
          QStyleOption_Destroy(opt);
        end;
        qdvStandardPixmap:
        begin
          AIcon := QIcon_create();
          if Element.StandardPixmap = QStyleSP_TitleBarCloseButton then
          begin
            opt := QStyleOptionDockWidget_create();
            QStyle_standardIcon(Style, AIcon, Element.StandardPixmap, opt, Context.Parent);
          end
          else
          begin
            opt := QStyleOption_create(Ord(QStyleOptionVersion), Ord(QStyleOptionSO_Default));
            QStyle_standardIcon(Style, AIcon, Element.StandardPixmap, opt);
          end;
          QIcon_paint(AIcon, Context.Widget, ARect.Left, ARect.Top,
            ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
          QIcon_destroy(AIcon);
          QStyleOption_Destroy(opt);
        end;
      end;
    finally
      Context.restore;
    end;
  end;
end;

procedure TQtThemeServices.DrawIcon(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST;
  Index: Integer);
begin
  DebugLn('WARNING: TQtThemeServices.DrawIcon is not implemented.');
end;

procedure TQtThemeServices.DrawText(ACanvas: TPersistent;
  Details: TThemedElementDetails; const S: String; R: TRect; Flags,
  Flags2: Cardinal);
var
  AQColor, AOldColor: TQColor;
  B: Boolean;
  OldCanvasFontColor: TColor;
  APalette: QPaletteH;
begin
  B := False;

  // issue #25253
  if (Details.Element in [teButton, teComboBox]) then
  begin
    B := True;
    AOldColor := TQtDeviceContext(TCanvas(ACanvas).Handle).pen.getColor;
    OldCanvasFontColor := TCanvas(ACanvas).Font.Color;

    // issue #25922
    if IsDisabled(Details) then
    begin
      APalette := QPalette_create;
      try
        QApplication_palette(APalette,'QPushButton');
        AQColor := QPalette_color(APalette, QPaletteDisabled, QPaletteButtonText)^;
        TQtDeviceContext(TCanvas(ACanvas).Handle).pen.setColor(AQColor);
      finally
        QPalette_destroy(APalette);
      end;
    end else
    begin
      if TCanvas(ACanvas).Font.Color = clDefault then
        TCanvas(ACanvas).Font.Color := clBtnText;
      ColorRefToTQColor(ColorToRGB(TCanvas(ACanvas).Font.Color), AQColor);
      TQtDeviceContext(TCanvas(ACanvas).Handle).pen.setColor(AQColor);
    end;
  end;

  DrawText(TCanvas(ACanvas).Handle, Details, S, R, Flags, Flags2);

  if B then
  begin
    TQtDeviceContext(TCanvas(ACanvas).Handle).pen.setColor(AOldColor);
    TCanvas(ACanvas).Font.Color := OldCanvasFontColor;
  end;
end;

procedure TQtThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: String; R: TRect; Flags, Flags2: Cardinal);
var
  Palette: QPaletteH;
  Context: TQtDeviceContext;
  Widget: QWidgetH;
  W: WideString;
  TextRect: TRect;
  AOldMode: Integer;
  ATextPalette: Cardinal;
  AQColor: TQColor;
begin
  // DebugLn('TQtThemeServices.DrawText ');
  Context := TQtDeviceContext(DC);
  case Details.Element of
    teToolTip:
      begin
        W := GetUTF8String(S);
        Context.save;
        AOldMode := Context.SetBkMode(TRANSPARENT);
        try
          if Context.Parent <> nil then
            Palette := QPalette_create(QWidget_palette(Context.Parent))
          else
            Palette := nil;

          if Palette = nil then
          begin
            inherited;
            exit;
          end;
          QStyle_drawItemText(Style, Context.Widget, @R,
            DTFlagsToQtFlags(Flags), Palette,
            not IsDisabled(Details), @W, QPaletteToolTipText);
          QPalette_destroy(Palette);
        finally
          Context.SetBkMode(AOldMode);
          Context.restore;
        end;

      end;
    teTreeView, teListView:
      begin
        if Details.Part = TVP_TREEITEM then
        begin
          Palette := nil;
          if Context.Parent <> nil then
          begin
            Widget := QWidget_parentWidget(Context.Parent);
            if (Widget <> nil) and QObject_inherits(Widget,'QAbstractScrollArea') then
              Palette := QPalette_create(QWidget_palette(Widget))
            else
              Palette := QPalette_create(QWidget_palette(Context.Parent));
          end;

          if Palette = nil then
          begin
            inherited;
            exit;
          end;

          W := GetUTF8String(S);
          Context.save;
          try
            Context.SetBkMode(TRANSPARENT);
            if Details.State = TREIS_SELECTEDNOTFOCUS then
              QPalette_setCurrentColorGroup(Palette, QPaletteInactive)
            else
              QPalette_setCurrentColorGroup(Palette, QPaletteActive);

            if Details.State in
              [TREIS_SELECTED, TREIS_HOTSELECTED, TREIS_SELECTEDNOTFOCUS] then
            begin
              // fix qt motif style behaviour which does not fillrect
              // when drawing itemview if it doesn't have text
              // assigned via QStyleViewItemViewV4_setText()
              if Details.State = TREIS_SELECTED then
              begin
                if StyleName = 'motif' then
                begin
                  TextRect := R;
                  with TextRect do
                  begin
                    Left := Left + 2;
                    Top := Top + 2;
                    Right := Right - 2;
                    Bottom := Bottom - 2;
                  end;
                  QPainter_fillRect(Context.Widget, @TextRect, QtSolidPattern);
                end;
              end;

              QStyle_drawItemText(Style, Context.Widget, @R,
                DTFlagsToQtFlags(Flags), Palette,
                not IsDisabled(Details), @W, QPaletteHighlightedText)
            end else
              QStyle_drawItemText(Style, Context.Widget, @R,
                DTFlagsToQtFlags(Flags), Palette,
                not IsDisabled(Details), @W, QPaletteText);
          finally
            Context.restore;
          end;
          QPalette_destroy(Palette);
        end else
          inherited;
      end;

    else
    begin // default text drawing for all !
      W := GetUTF8String(S);
      Context.save;
      AOldMode := Context.SetBkMode(TRANSPARENT);
      if Context.Parent <> nil then
        Palette := QPalette_create(QWidget_palette(Context.Parent))
      else
      begin
        Palette := QPalette_create;
        QGUIApplication_palette(Palette);
      end;
      try
        if Details.Element in [teButton, teComboBox] then
          AQColor := TQtDeviceContext(DC).pen.getColor; // issue #25253

        if Details.Element in [teEdit, teListView, teTreeView, teWindow] then
          ATextPalette := QPaletteWindowText
        else
        if Details.Element in [teButton, teComboBox] then
          ATextPalette := QPaletteButtonText
        else
          ATextPalette := QPaletteText;

        if Details.Element in [teButton, teComboBox] then
          QPalette_setColor(Palette, ATextPalette, @AQColor); // issue #25253

        if Context.font.Angle <> 0 then
        begin
          Context.Translate(R.Left, R.Top);
          Context.Rotate(-0.1 * Context.Font.Angle);
          OffsetRect(R, -R.Left, -R.Top);
        end;

        QStyle_drawItemText(Style, Context.Widget, @R,
          DTFlagsToQtFlags(Flags), Palette,
          not IsDisabled(Details), @W, ATextPalette);

        if Context.font.Angle <> 0 then
        begin
          Context.Translate(-R.Left, -R.Top);
          Context.Rotate(0.1 * Context.Font.Angle);
        end;
        Context.SetBkMode(AOldMode);
      finally
        QPalette_destroy(Palette);
        Context.restore;
      end;
    end;
  end;
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

  if IsMixed(Details) then
    Result := Result or QStyleState_NoChange
  else
  if IsChecked(Details) then
    Result := Result or QStyleState_On
  else
    Result := Result or QStyleState_Off;


  if (Details.Element = teHeader) then
  begin
    if not IsPushed(Details) then
      Result := Result or QStyleState_Raised;
    if not IsDisabled(Details) and (Result and QStyleState_Off <> 0) then
      Result := Result and not QStyleState_Off;
  end;

  // specific states
  {when toolbar = flat, toolbar buttons should be flat too.}
  if (Details.Element = teToolBar) and
     (Details.State in [TS_NORMAL, TS_DISABLED]) then
    Result := QStyleState_AutoRaise;

  // define orientations
  if ((Details.Element = teRebar) and (Details.Part = RP_GRIPPER)) or
     ((Details.Element = teToolBar) and (Details.Part = TP_SEPARATOR)) or
     ((Details.Element = teScrollBar) and (Details.Part in
       [SBP_UPPERTRACKHORZ, SBP_LOWERTRACKHORZ, SBP_THUMBBTNHORZ, SBP_GRIPPERHORZ])) or
     ((Details.Element = teTrackbar) and not (Details.Part in
       [TKP_TRACKVERT, TKP_THUMBVERT])) then
    Result := Result or QStyleState_Horizontal;

  if (Details.Element in [teTreeview, teListView]) then
  begin
    if (Details.Element = teTreeView) and
      (Details.Part in [TVP_GLYPH, TVP_HOTGLYPH]) then
    begin
      Result := Result or QStyleState_Children;
      if Details.State = GLPS_OPENED then
        Result := Result or QStyleState_Open;
    end else
    if Details.Part in [TVP_TREEITEM] then
    begin
      Result := Result or QStyleState_Item;
      case Details.State of
        TREIS_SELECTED:
          Result := Result or QStyleState_Selected or QStyleState_HasFocus or QStyleState_Active;
        TREIS_SELECTEDNOTFOCUS:
          Result := Result or QStyleState_Selected;
        TREIS_HOTSELECTED:
          Result := Result or QStyleState_Selected or QStyleState_MouseOver;
        TREIS_HOT:
          Result := Result or QStyleState_MouseOver;
      end;
    end;
  end;
  if (Details.Element = teTrackBar) then
  begin
    if Details.Part in [TKP_THUMB, TKP_THUMBVERT] then
    begin
      if Details.State in [TUS_PRESSED, TUS_HOT] then
        Result := Result or QStyleState_Active or QStyleState_HasFocus or QStyleState_MouseOver;
    end;
  end;
  if (Details.Element = teWindow) then
  begin
    if Details.Part in [WP_SMALLCAPTION,
          WP_FRAMELEFT,
          WP_FRAMERIGHT,
          WP_FRAMEBOTTOM,
          WP_SMALLFRAMELEFT,
          WP_SMALLFRAMERIGHT,
          WP_SMALLFRAMEBOTTOM] then
    begin
      if Details.State = FS_ACTIVE then
        Result := Result or QStyleState_Active or QStyleState_HasFocus;
    end;
  end;
end;

function TQtThemeServices.GetDetailSize(Details: TThemedElementDetails): TSize;
begin
  case Details.Element of
    teButton:
      begin
        if Details.Part = BP_CHECKBOX then
        begin
          Result.cy := QStyle_pixelMetric(Style, QStylePM_IndicatorHeight, nil, nil);
          Result.cx := QStyle_pixelMetric(Style, QStylePM_IndicatorWidth, nil, nil);
        end else
        if Details.Part = BP_RADIOBUTTON then
        begin
          Result.cy := QStyle_pixelMetric(Style, QStylePM_ExclusiveIndicatorHeight, nil, nil);
          Result.cx := QStyle_pixelMetric(Style, QStylePM_ExclusiveIndicatorWidth, nil, nil);
        end else
          Result := inherited;
      end;
    teRebar :
      if Details.Part in [RP_GRIPPER, RP_GRIPPERVERT] then
        Result := Size(-1, -1)
      else
        Result := inherited;
    teTreeView:
      begin
        Result := inherited;
        if Details.Part in [TVP_GLYPH, TVP_HOTGLYPH] then
        begin
          inc(Result.cx);
          inc(Result.cy);
        end;
      end;
    teToolBar:
      if (Details.Part = TP_DROPDOWNBUTTON) or (Details.Part = TP_SPLITBUTTONDROPDOWN) then
      begin
        Result.cy := -1;
        Result.cx := QStyle_pixelMetric(Style, QStylePM_MenuButtonIndicator, nil, nil);
      end else
        Result := inherited;
    teHeader:
      if Details.Part = HP_HEADERSORTARROW then
        Result := Size(-1, -1) // not yet supported
      else
        Result := inherited;
    else
      Result := inherited;
  end;
end;

function TQtThemeServices.GetStockImage(StockID: LongInt; out Image,
  Mask: HBitmap): Boolean;
var
  APixmap: QPixmapH;
  AImage: QImageH;
  AScaledImage: QImageH;
  AStdPixmap: QStyleStandardPixmap;
  opt: QStyleOptionH;
  IconSize: Integer;
  AIcon: QIconH;
  ASize: TSize;
begin
  case StockID of
    idButtonOk: AStdPixmap := QStyleSP_DialogOkButton;
    idButtonCancel: AStdPixmap := QStyleSP_DialogCancelButton;
    idButtonYes: AStdPixmap := QStyleSP_DialogYesButton;
    idButtonYesToAll: AStdPixmap := QStyleSP_DialogYesButton;
    idButtonNo: AStdPixmap := QStyleSP_DialogNoButton;
    idButtonNoToAll: AStdPixmap := QStyleSP_DialogNoButton;
    idButtonHelp: AStdPixmap := QStyleSP_DialogHelpButton;
    idButtonClose: AStdPixmap := QStyleSP_DialogCloseButton;
    idButtonAbort: AStdPixmap := QStyleSP_DialogResetButton;
    idButtonAll: AStdPixmap := QStyleSP_DialogApplyButton;
    idButtonIgnore: AStdPixmap := QStyleSP_DialogDiscardButton;
    idButtonRetry: AStdPixmap := QStyleSP_BrowserReload; // ?
    idButtonOpen: AStdPixmap := QStyleSP_DialogOpenButton;
    idButtonSave: AStdPixmap := QStyleSP_DialogSaveButton;
    idButtonShield: AStdPixmap := QStyleSP_VistaShield;

    idDialogWarning : AStdPixmap := QStyleSP_MessageBoxWarning;
    idDialogError: AStdPixmap := QStyleSP_MessageBoxCritical;
    idDialogInfo: AStdPixmap := QStyleSP_MessageBoxInformation;
    idDialogConfirm: AStdPixmap := QStyleSP_MessageBoxQuestion;
  else
    begin
       Result := inherited GetStockImage(StockID, Image, Mask);
       Exit;
    end;
  end;

  opt := QStyleOption_create(Ord(QStyleOptionVersion), Ord(QStyleOptionSO_Default));
  AIcon := QIcon_create();
  if StockID in [idButtonOk..idButtonShield] then
    IconSize := GetPixelMetric(QStylePM_ButtonIconSize, opt, nil)
  else
  if (StockID >= idDialogWarning) and (StockID <= idDialogShield) then
    IconSize := GetPixelMetric(QStylePM_MessageBoxIconSize, opt, nil)
  else
    IconSize := 0;
  QStyle_standardIcon(QApplication_style(), AIcon, AStdPixmap, opt);
  QStyleOption_Destroy(opt);

  if QIcon_isNull(AIcon) then
  begin
    QIcon_destroy(AIcon);
    Result := inherited GetStockImage(StockID, Image, Mask);
    Exit;
  end;

  // convert from what we have to QImageH
  APixmap := QPixmap_create();
  if IconSize > 0 then
  begin
    ASize.cx := IconSize;
    ASize.cy := IconSize;
  end else
    QIcon_actualSize(AIcon, @ASize, @ASize);
  QIcon_pixmap(AIcon, APixmap, PSize(@ASize));
  QIcon_destroy(AIcon);
  AImage := QImage_create();
  QPixmap_toImage(APixmap, AImage);
  QPixmap_destroy(APixmap);

  // we must respect theme size , qt is buggy somehow, some icons are 22 some 16px.
  if IconSize > 0 then
  begin
    if (QImage_width(AImage) > IconSize) and (QImage_height(AImage) > IconSize) then
    begin
      AScaledImage := QImage_create();
      QImage_scaled(AImage, AScaledImage, IconSize, IconSize, QtKeepAspectRatio, QtSmoothTransformation);
      QImage_destroy(AImage);
      AImage := QImage_create(AScaledImage);
      QImage_destroy(AScaledImage);
    end;
  end;

  Image := HBitmap(TQtImage.Create(AImage));
  Mask := 0;
  Result := True;
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
    teComboBox:
      begin
        if Byte(Details.Part) in [CP_DROPDOWNBUTTON, CP_DROPDOWNBUTTONRIGHT, CP_DROPDOWNBUTTONLEFT] then
        begin
          Result.DrawVariant := qdvComplexControl;
          Result.ComplexControl := QStyleCC_ComboBox;
          Result.SubControls := QStyleSC_ComboBoxArrow;
          if Details.Part = CP_DROPDOWNBUTTONLEFT then
            Result.Features := Ord(QtRightToLeft)
          else
            Result.Features := 0;
        end else
        if not (Details.Part = CP_READONLY) then
        begin
          Result.DrawVariant := qdvComplexControl;
          Result.ComplexControl := QStyleCC_ComboBox;
          Result.SubControls := QStyleSC_ComboBoxEditField;
        end else
        if Byte(Details.Part) in [0, CP_BORDER] then
        begin
          Result.DrawVariant := qdvComplexControl;
          Result.ComplexControl := QStyleCC_ComboBox;
          Result.SubControls := QStyleSC_ComboBoxFrame;
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
              Result.DrawVariant := qdvComplexControl;
              Result.ComplexControl := QStyleCC_ToolButton;
              Result.SubControls := QStyleSC_None;
              Result.Features := QStyleOptionToolButtonMenuButtonPopup;
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
    teSpin:
      begin
        Result.DrawVariant := qdvComplexControl;
        Result.ComplexControl := QStyleCC_SpinBox;
        if Details.Part = 0 then
          Result.SubControls := QStyleSC_SpinBoxFrame
        else
        if Byte(Details.Part) in [SPNP_UP, SPNP_UPHORZ] then
          Result.SubControls := QStyleSC_SpinBoxUp
        else
        if Byte(Details.Part) in [SPNP_DOWN, SPNP_DOWNHORZ] then
          Result.SubControls := QStyleSC_SpinBoxDown;
        if Byte(Details.Part) in [SPNP_UPHORZ, SPNP_DOWNHORZ] then
          Result.Features := QtVertical;
      end;
    teWindow:
      begin
        case Details.Part of
          WP_FRAMELEFT,
          WP_FRAMERIGHT,
          WP_FRAMEBOTTOM,
          WP_SMALLFRAMELEFT,
          WP_SMALLFRAMERIGHT,
          WP_SMALLFRAMEBOTTOM:
          begin
            Result.PrimitiveElement := QStylePE_FrameWindow;
            Result.DrawVariant := qdvPrimitive;
            exit;
          end;
          WP_SMALLCLOSEBUTTON:
          begin
            Result.DrawVariant := qdvStandardPixmap;
            Result.StandardPixmap := QStyleSP_TitleBarCloseButton;
            exit;
          end;
          WP_SMALLCAPTION: Result.SubControls := QStyleSC_TitleBarLabel;
          WP_SYSBUTTON: Result.SubControls := QStyleSC_TitleBarSysMenu;
          WP_MINBUTTON: Result.SubControls := QStyleSC_TitleBarMinButton;
          WP_MAXBUTTON: Result.SubControls := QStyleSC_TitleBarMaxButton;
          WP_CLOSEBUTTON: Result.SubControls := QStyleSC_TitleBarCloseButton;
          WP_RESTOREBUTTON: Result.SubControls := QStyleSC_TitleBarNormalButton;
          WP_HELPBUTTON: Result.SubControls := QStyleSC_TitleBarContextHelpButton;
          WP_MDIHELPBUTTON: Result.SubControls := QStyleSC_TitleBarContextHelpButton;
          WP_MDIMINBUTTON: Result.SubControls := QStyleSC_MdiMinButton;
          WP_MDICLOSEBUTTON: Result.SubControls := QStyleSC_MdiCloseButton;
          WP_MDIRESTOREBUTTON: Result.SubControls := QStyleSC_MdiNormalButton;
        else
          Result.SubControls := QStyleSC_None;
        end;

        if Result.SubControls <= QStyleSC_MdiCloseButton then
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
    teTab:
      begin
        if Byte(Details.Part) in [TABP_TABITEM, TABP_TABITEMLEFTEDGE, TABP_TABITEMRIGHTEDGE, TABP_TOPTABITEM] then
        begin
          Result.DrawVariant := qdvControl;
          Result.ControlElement := QStyleCE_TabBarTabShape;
          if Details.Part = TABP_TABITEM then
            Result.Features := Ord(QTabBarRoundedNorth)
          else
          if Details.Part = TABP_TABITEMLEFTEDGE then
            Result.Features := Ord(QTabBarRoundedWest)
          else
          if Details.Part = TABP_TABITEMRIGHTEDGE then
            Result.Features := Ord(QTabBarRoundedEast)
          else
            Result.Features := Ord(QTabBarRoundedNorth);
        end else
        if Details.Part = TABP_PANE then
        begin
          Result.DrawVariant := qdvPrimitive;
          Result.PrimitiveElement := QStylePE_FrameTabWidget;
        end else
        if Details.Part = TABP_BODY then
        begin
          Result.DrawVariant := qdvPrimitive;
          Result.PrimitiveElement := QStylePE_FrameTabBarBase;
        end;
      end;
    teProgress:
      begin
        Result.DrawVariant := qdvControl;
        case Details.Part of
          PP_CHUNK,PP_CHUNKVERT: Result.ControlElement := QStyleCE_ProgressBarContents;
          PP_FILL,PP_FILLVERT: Result.ControlElement := QStyleCE_ProgressBarGroove;
          else
            Result.ControlElement := QStyleCE_ProgressBar;
        end;
        if Byte(Details.Part) in [PP_BARVERT, PP_FILLVERT, PP_CHUNKVERT] then
          Result.Features := QtVertical;
      end;
    teScrollBar:
      begin
        Result.DrawVariant := qdvComplexControl;
        Result.ComplexControl := QStyleCC_ScrollBar;
        case Details.Part of
          SBP_ARROWBTN: Result.SubControls := QStyleSC_ScrollBarAddLine;
          SBP_THUMBBTNHORZ,
          SBP_THUMBBTNVERT,
          SBP_GRIPPERHORZ,
          SBP_GRIPPERVERT: Result.SubControls := QStyleSC_ScrollBarSlider;
          SBP_LOWERTRACKHORZ,
          SBP_LOWERTRACKVERT: Result.SubControls := QStyleSC_ScrollBarAddPage;
          SBP_UPPERTRACKHORZ,
          SBP_UPPERTRACKVERT: Result.SubControls := QStyleSC_ScrollBarSubPage;
        else
          Result.SubControls := QStyleSC_None;
        end;
      end;
    teTrackBar:
      begin
        Result.DrawVariant := qdvComplexControl;
        Result.ComplexControl := QStyleCC_Slider;
        if Details.Part in [TKP_TRACKVERT, TKP_THUMBVERT, TKP_TICSVERT] then
          Result.Features := QtVertical
        else
          Result.Features := QtHorizontal;
        case Details.Part of
          TKP_TRACK,
          TKP_TRACKVERT: Result.SubControls := QStyleSC_SliderGroove;
          TKP_THUMB,
          TKP_THUMBBOTTOM,
          TKP_THUMBTOP,
          TKP_THUMBVERT,
          TKP_THUMBLEFT,
          TKP_THUMBRIGHT: Result.SubControls := QStyleSC_SliderHandle;
          TKP_TICS,
          TKP_TICSVERT: Result.SubControls := QStyleSC_SliderTickmarks;
        end;
      end;
    teStatus:
      begin
        case Details.Part of
          SP_PANE:
            begin
              Result.DrawVariant := qdvPrimitive;
              Result.PrimitiveElement := QStylePE_FrameStatusBar;
            end;
          SP_GRIPPER:
            begin
              Result.DrawVariant := qdvControl;
              Result.ControlElement := QStyleCE_SizeGrip;
            end;
          SP_GRIPPERPANE:
            begin
              Result.DrawVariant := qdvPrimitive;
              Result.PrimitiveElement := QStylePE_FrameStatusBarItem;
            end;
        end;
      end;
    teTreeView, teListView:
      begin
        if (Details.Element = teTreeView) and
          (Details.Part in [TVP_GLYPH, TVP_HOTGLYPH]) then
        begin
          Result.DrawVariant := qdvPrimitive;
          Result.PrimitiveElement := QStylePE_IndicatorBranch;
        end else
        if Details.Part in [TVP_TREEITEM] then
        begin
          Result.DrawVariant := qdvControl;
          Result.ControlElement := QStyleCE_ItemViewItem;
        end;
      end;
    teToolTip:
      begin
        if Details.Part = TTP_STANDARD then
        begin
          Result.DrawVariant := qdvPrimitive;
          Result.PrimitiveElement := QStylePE_PanelTipLabel;
        end;
      end;
  end;
end;

end.


