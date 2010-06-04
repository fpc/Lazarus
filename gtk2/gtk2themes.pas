unit Gtk2Themes;

{$mode objfpc}{$H+}

interface

uses
  // rtl
  Types, Classes, SysUtils,
  // os
  glib2,  gdk2, gtk2, gdk2pixbuf, Pango,
  // lcl
  LCLType, LCLProc, LCLIntf, Graphics, Themes, TmSchema, Forms,
  // widgetset
  Gtk2Def, Gtk2Int, Gtk2Proc, GtkThemes, Gtk2Globals;
  
type
  { TGtk2ThemeServices }

  TGtk2ThemeServices = class(TGtkThemeServices)
  protected
    function GetGtkStyleParams(DC: HDC; Details: TThemedElementDetails; AIndex: Integer): TGtkStyleParams; override;
    function GetParamsCount(Details: TThemedElementDetails): Integer; override;
  public
    function GetDetailSize(Details: TThemedElementDetails): TSize; override;
    function GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean; override;
    function GetOption(AOption: TThemeOption): Integer; override;
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails;
       const R: TRect; ClipRect: PRect); override;
  end;

implementation

function GetColumnButtonFromTreeView(AWidget: PGtkWidget): PGtkWidget;
var
  AColumn: PGtkTreeViewColumn;
begin
  Result := nil;
  if not GTK_IS_TREE_VIEW(AWidget) then
    exit;

  AColumn := gtk_tree_view_get_column(PGtkTreeView(AWidget), 0);
  if AColumn = nil then
    Exit;
  Result := AColumn^.button;
end;

{ TGtk2ThemeServices }

function TGtk2ThemeServices.GetGtkStyleParams(DC: HDC;
  Details: TThemedElementDetails; AIndex: Integer): TGtkStyleParams;
begin
  Result := inherited GetGtkStyleParams(DC, Details, AIndex);
  
  // override some styles
  if Result.Style <> nil then
    case Details.Element of
      teHeader:
        begin
          Result.Widget := GetColumnButtonFromTreeView(GetStyleWidget(lgsTreeView));
          if Result.Widget = nil then
            Result.Widget := GetStyleWidget(lgsTreeView);
          Result.State := GtkButtonMap[Details.State];
          if Details.State = PBS_PRESSED then
            Result.Shadow := GTK_SHADOW_IN
          else
            Result.Shadow := GTK_SHADOW_OUT;

          Result.IsHot:= Result.State = GTK_STATE_PRELIGHT;

          Result.Detail := 'button';
          Result.Painter := gptBox;
        end;
      teRebar:
        begin
          case Details.Part of
            RP_GRIPPER, RP_GRIPPERVERT:
              begin
                Result.State := GTK_STATE_NORMAL;
                Result.Shadow := GTK_SHADOW_NONE;
                Result.Detail := 'paned';
                Result.Painter := gptHandle;
                if Details.Part = RP_GRIPPER then
                begin
                  Result.Orientation := GTK_ORIENTATION_VERTICAL;
                  Result.Widget := GetStyleWidget(lgsVerticalPaned);
                end
                else
                begin
                  Result.Orientation := GTK_ORIENTATION_HORIZONTAL;
                  Result.Widget := GetStyleWidget(lgsHorizontalPaned);
                end;
              end;
            RP_BAND:
              begin
                Result.Widget := GetStyleWidget(lgsVerticalPaned);
                Result.State := GtkButtonMap[Details.State];
                Result.Shadow := GTK_SHADOW_NONE;
                Result.Detail := 'paned';
                Result.Painter := gptFlatBox;
              end;
          end;
        end;
      teTreeview:
        begin
          if Details.Part in [TVP_GLYPH, TVP_HOTGLYPH] then
          begin
            Result.Painter := gptExpander;
            Result.Shadow := GTK_SHADOW_NONE;
            if Details.Part = TVP_GLYPH then
              Result.State := GTK_STATE_NORMAL
            else
              Result.State := GTK_STATE_PRELIGHT;
            Result.Widget := GetStyleWidget(lgsTreeView);
            Result.Detail := 'treeview';
            if Details.State = GLPS_CLOSED then
              Result.Expander := GTK_EXPANDER_COLLAPSED
            else
              Result.Expander := GTK_EXPANDER_EXPANDED;

            Result.ExpanderSize := GetDetailSize(Details).cx;
          end
          else
          if Details.Part = TVP_TREEITEM then
          begin
            Result.Widget := GetStyleWidget(lgsTreeView);
            Result.Shadow := GTK_SHADOW_NONE;
            if AIndex = 0 then
            begin
              Result.Painter := gptFlatBox;
              case Details.State of
                TREIS_SELECTED,
                TREIS_HOTSELECTED: Result.State := GTK_STATE_SELECTED;
                TREIS_SELECTEDNOTFOCUS: Result.State := GTK_STATE_ACTIVE;
                TREIS_HOT: Result.State := GTK_STATE_PRELIGHT;
                TREIS_DISABLED: Result.State := GTK_STATE_INSENSITIVE;
              else
                Result.State := GTK_STATE_NORMAL;
              end;
              Result.Detail := 'cell_even';
            end
            else
            if AIndex = 1 then
            begin
              Result.Detail := 'treeview';
              if Details.State = TREIS_SELECTED then
              begin
                Result.State := GTK_STATE_SELECTED;
                Result.Painter := gptFocus
              end
              else
              begin
                Result.State := GTK_STATE_NORMAL;
                Result.Painter := gptNone;
              end;
            end;
          end;
        end;
    end;
end;

function TGtk2ThemeServices.GetParamsCount(Details: TThemedElementDetails): Integer;
begin
  if (Details.Element = teTreeview) and (Details.Part = TVP_TREEITEM) then
    Result := 2
  else
    Result := inherited GetParamsCount(Details);
end;

function TGtk2ThemeServices.GetDetailSize(Details: TThemedElementDetails): TSize;
var
  AValue: TGValue;
begin
  if (Details.Element = teTreeView) and (Details.Part in [TVP_GLYPH, TVP_HOTGLYPH]) then
  begin
    FillChar(AValue, SizeOf(AValue), 0);
    g_value_init(@AValue, G_TYPE_INT);
    gtk_widget_style_get_property(GetStyleWidget(lgsTreeView), 'expander-size', @AValue);
    Result := Size(AValue.data[0].v_int, AValue.data[0].v_int);
  end
  else
    Result := GetBaseDetailsSize(Details);
end;

function TGtk2ThemeServices.GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean;
var
  GDIObj: PGDIObject;
  StockName: PChar;
  Style: PGtkStyle;
  IconSet: PGtkIconSet;
  Pixbuf: PGDKPixbuf;
begin
  case StockID of
    idButtonOk: StockName := GTK_STOCK_OK;
    idButtonCancel: StockName := GTK_STOCK_CANCEL;
    idButtonYes: StockName := GTK_STOCK_YES;
    idButtonYesToAll: StockName := GTK_STOCK_YES;
    idButtonNo: StockName := GTK_STOCK_NO;
    idButtonNoToAll: StockName := GTK_STOCK_NO;
    idButtonHelp: StockName := GTK_STOCK_HELP;
    idButtonAbort: StockName := GTK_STOCK_STOP;
    idButtonClose: StockName := GTK_STOCK_CLOSE;
    // this is disputable but anyway stock icons looks like our own
    idButtonAll: StockName := GTK_STOCK_APPLY;
    idButtonIgnore: StockName := GTK_STOCK_DELETE;
    idButtonRetry: StockName := GTK_STOCK_REFRESH;
    idButtonOpen: StockName := GTK_STOCK_OPEN;
    idButtonSave: StockName := GTK_STOCK_SAVE;
    idButtonShield: StockName := GTK_STOCK_DIALOG_AUTHENTICATION;

    idDialogWarning : StockName := GTK_STOCK_DIALOG_WARNING;
    idDialogError: StockName := GTK_STOCK_DIALOG_ERROR;
    idDialogInfo: StockName := GTK_STOCK_DIALOG_INFO;
    idDialogConfirm: StockName := GTK_STOCK_DIALOG_QUESTION;
    idDialogShield: StockName := GTK_STOCK_DIALOG_AUTHENTICATION;
  else
    begin
       Result := inherited GetStockImage(StockID, Image, Mask);
       Exit;
    end;
  end;

  if (StockID >= idButtonBase) and (StockID <= idDialogBase) then
    Style := GetStyle(lgsButton)
  else
    Style := GetStyle(lgsWindow);

  if (Style = nil) or (not GTK_IS_STYLE(Style)) then
  begin
    Result := inherited GetStockImage(StockID, Image, Mask);
    Exit;
  end;

  IconSet := gtk_style_lookup_icon_set(Style, StockName);

  if (IconSet = nil) then
  begin
    Result := inherited GetStockImage(StockID, Image, Mask);
    Exit;
  end;

  if (StockID >= idButtonBase) and (StockID <= idDialogBase) then
    Pixbuf := gtk_icon_set_render_icon(IconSet, Style,
      GTK_TEXT_DIR_NONE, GTK_STATE_NORMAL, GTK_ICON_SIZE_BUTTON, GetStyleWidget(lgsbutton), nil)
  else
    Pixbuf := gtk_icon_set_render_icon(IconSet, Style,
      GTK_TEXT_DIR_NONE, GTK_STATE_NORMAL, GTK_ICON_SIZE_DIALOG, GetStyleWidget(lgswindow), nil);

  GDIObj := Gtk2Widgetset.NewGDIObject(gdiBitmap);
  with GDIObj^ do
  begin
    GDIBitmapType := gbPixbuf;
    visual := gdk_visual_get_system();
    gdk_visual_ref(visual);
    colormap := gdk_colormap_get_system();
    gdk_colormap_ref(colormap);
    GDIPixbufObject := Pixbuf;
  end;

  Image := HBitmap(PtrUInt(GDIObj));
  Mask := 0;
  Result := True;
end;

procedure ButtonImagesChange(ASettings: PGtkSettings; pspec: PGParamSpec; Services: TGtk2ThemeServices); cdecl;
begin
  Application.IntfThemeOptionChange(Services, toShowButtonImages);
  Services.IntfDoOnThemeChange;
end;

procedure MenuImagesChange(ASettings: PGtkSettings; pspec: PGParamSpec; Services: TGtk2ThemeServices); cdecl;
begin
  Application.IntfThemeOptionChange(Services, toShowMenuImages);
  Services.IntfDoOnThemeChange;
end;

function TGtk2ThemeServices.GetOption(AOption: TThemeOption): Integer;
var
  ASettings: PGtkSettings;
  BoolSetting: gboolean;
  Widget: PGtkWidget;
  Signal: guint;
begin
  case AOption of
    toShowButtonImages:
      begin
        Widget := GetStyleWidget(lgsButton);
        ASettings := gtk_widget_get_settings(Widget);
        BoolSetting := True; // default
        g_object_get(ASettings, 'gtk-button-images', @BoolSetting, nil);
        Result := Ord(BoolSetting = True);
        if g_object_get_data(PGObject(Widget), 'lcl-images-change-callback') = nil then
        begin
          Signal := g_signal_connect(ASettings, 'notify::gtk-button-images', TGCallback(@ButtonImagesChange), Self);
          g_object_set_data(PGObject(Widget), 'lcl-images-change-callback', Pointer(Signal))
        end;
      end;
    toShowMenuImages:
      begin
        Widget := GetStyleWidget(lgsMenuitem);
        ASettings := gtk_widget_get_settings(Widget);
        BoolSetting := False; // default
        g_object_get(ASettings, 'gtk-menu-images', @BoolSetting, nil);
        Result := Ord(BoolSetting = True);
        if g_object_get_data(PGObject(Widget), 'lcl-images-change-callback') = nil then
        begin
          Signal := g_signal_connect(ASettings, 'notify::gtk-menu-images', TGCallback(@MenuImagesChange), Self);
          g_object_set_data(PGObject(Widget), 'lcl-images-change-callback', Pointer(Signal))
        end;
      end;
  else
    Result := inherited GetOption(AOption);
  end;
end;

procedure TGtk2ThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  Widget: PGtkWidget;
begin
  if (Details.Element = teTreeview) and (Details.Part = TVP_TREEITEM) and
     (Details.State = TREIS_SELECTED) then
  begin
    // lie to cleanlooks theme
    Widget := GetStyleWidget(lgsTreeView);
    GTK_WIDGET_SET_FLAGS(Widget, GTK_HAS_FOCUS);
    inherited DrawElement(DC, Details, R, ClipRect);
    GTK_WIDGET_UNSET_FLAGS(Widget, GTK_HAS_FOCUS);
  end
  else
    inherited DrawElement(DC, Details, R, ClipRect);
end;

end.

