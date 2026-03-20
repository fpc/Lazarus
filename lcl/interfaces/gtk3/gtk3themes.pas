{
 *****************************************************************************
 *                               gtk3themes.pas                              *
 *                               -------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit gtk3themes;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, LazCairo1, LazGtk3, LazGdk3, LazGObject2, LazGLib2,
  Themes, TmSchema,
  LazPango1, LazPangoCairo1, LCLType,
  LazUTF8;

type

  { TGTK3ThemeServices }

  TGTK3ThemeServices = class(TThemeServices)
  private
    function GetControlState(Details: TThemedElementDetails): TGtkStateFlags;
    function MakeIndicatorContext(ParentType: TGType; const AClassName, IndicatorName: PChar;
      State: TGtkStateFlags; AScreen: PGdkScreen = nil): PGtkStyleContext;
  protected
    function InitThemes: Boolean; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;
    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); override;
    function GetElementDetails(Details: TThemedElementDetails; AScreen: PGdkScreen = nil): PGtkStyleContext;
    procedure DrawElement(Cr: Pcairo_t; Details: TThemedElementDetails; X, Y, Width, Height: Integer; AScreen: PGdkScreen = nil);
    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: String; X, Y, Width, Height: Integer; Flags: Cardinal);
  public

    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); override;
    procedure DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal; AContentRect: PRect); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST; Index: Integer); override;
    procedure DrawText(ACanvas: TPersistent; Details: TThemedElementDetails; const S: String; R: TRect; Flags, Flags2: Cardinal); overload; override;
    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: String; R: TRect; Flags, Flags2: Cardinal); overload; override;
    function GetDetailSizeForPPI(Details: TThemedElementDetails; PPI: Integer): TSize; override;
    function GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean; override;

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
  end;

implementation
uses LazGdkPixbuf2, Graphics, LazLogger, Math, gtk3procs, gtk3objects, gtk3int, LCLIntf;

function RgbaToCSS(const C: TGdkRGBA): string;
begin
  Result := Format('rgba(%d,%d,%d,%.3f)',
    [Round(C.red*255), Round(C.green*255), Round(C.blue*255), C.alpha]);
end;

// Create a single-node style context. Caller must g_object_unref the result.
function MakeCtx1(AType: TGType; AName, AClass: PChar;
  State: TGtkStateFlags; AScreen: PGdkScreen): PGtkStyleContext;
var
  Path: PGtkWidgetPath;
  Ctx: PGtkStyleContext;
begin
  Ctx := gtk_style_context_new();
  gtk_style_context_set_screen(Ctx, AScreen);
  Path := gtk_widget_path_new();
  gtk_widget_path_append_type(Path, AType);
  if AName <> nil then gtk_widget_path_iter_set_object_name(Path, 0, AName);
  if AClass <> nil then gtk_widget_path_iter_add_class(Path, 0, AClass);
  gtk_widget_path_iter_set_state(Path, 0, State);
  gtk_style_context_set_path(Ctx, Path);
  gtk_style_context_set_state(Ctx, State);
  gtk_widget_path_unref(Path);
  Result := Ctx;
end;

// Create a two-node style context (parent > child). Caller must g_object_unref.
function MakeCtx2(AType0: TGType; AName0, AClass0: PChar;
  AType1: TGType; AName1, AClass1: PChar;
  State: TGtkStateFlags; AScreen: PGdkScreen): PGtkStyleContext;
var
  Path: PGtkWidgetPath;
  Ctx: PGtkStyleContext;
  Pos: gint;
begin
  Ctx := gtk_style_context_new();
  gtk_style_context_set_screen(Ctx, AScreen);
  Path := gtk_widget_path_new();
  gtk_widget_path_append_type(Path, AType0);
  if AName0 <> nil then gtk_widget_path_iter_set_object_name(Path, 0, AName0);
  if AClass0 <> nil then gtk_widget_path_iter_add_class(Path, 0, AClass0);
  Pos := gtk_widget_path_append_type(Path, AType1);
  if AName1 <> nil then gtk_widget_path_iter_set_object_name(Path, Pos, AName1);
  if AClass1 <> nil then gtk_widget_path_iter_add_class(Path, Pos, AClass1);
  gtk_widget_path_iter_set_state(Path, Pos, State);
  gtk_style_context_set_path(Ctx, Path);
  gtk_style_context_set_state(Ctx, State);
  gtk_widget_path_unref(Path);
  Result := Ctx;
end;

// Create a three-node style context (grandparent > parent > child). Caller must g_object_unref.
function MakeCtx3(AType0: TGType; AName0, AClass0: PChar;
  AType1: TGType; AName1, AClass1: PChar;
  AType2: TGType; AName2, AClass2: PChar;
  State: TGtkStateFlags; AScreen: PGdkScreen): PGtkStyleContext;
var
  Path: PGtkWidgetPath;
  Ctx: PGtkStyleContext;
  Pos: gint;
begin
  Ctx := gtk_style_context_new();
  gtk_style_context_set_screen(Ctx, AScreen);
  Path := gtk_widget_path_new();
  gtk_widget_path_append_type(Path, AType0);
  if AName0 <> nil then
    gtk_widget_path_iter_set_object_name(Path, 0, AName0);
  if AClass0 <> nil then
    gtk_widget_path_iter_add_class(Path, 0, AClass0);
  Pos := gtk_widget_path_append_type(Path, AType1);
  if AName1 <> nil then
    gtk_widget_path_iter_set_object_name(Path, Pos, AName1);
  if AClass1 <> nil then
    gtk_widget_path_iter_add_class(Path, Pos, AClass1);
  Pos := gtk_widget_path_append_type(Path, AType2);
  if AName2 <> nil then
    gtk_widget_path_iter_set_object_name(Path, Pos, AName2);
  if AClass2 <> nil then
    gtk_widget_path_iter_add_class(Path, Pos, AClass2);
  gtk_widget_path_iter_set_state(Path, Pos, State);
  gtk_style_context_set_path(Ctx, Path);
  gtk_style_context_set_state(Ctx, State);
  gtk_widget_path_unref(Path);
  Result := Ctx;
end;

// Create a four-node style context. Caller must g_object_unref.
function MakeCtx4(AType0: TGType; AName0, AClass0: PChar;
  AType1: TGType; AName1, AClass1: PChar;
  AType2: TGType; AName2, AClass2: PChar;
  AType3: TGType; AName3, AClass3: PChar;
  State: TGtkStateFlags; AScreen: PGdkScreen): PGtkStyleContext;
var
  Path: PGtkWidgetPath;
  Ctx: PGtkStyleContext;
  Pos: gint;
begin
  Ctx := gtk_style_context_new();
  gtk_style_context_set_screen(Ctx, AScreen);
  Path := gtk_widget_path_new();
  gtk_widget_path_append_type(Path, AType0);
  if AName0 <> nil then
    gtk_widget_path_iter_set_object_name(Path, 0, AName0);
  if AClass0 <> nil then
    gtk_widget_path_iter_add_class(Path, 0, AClass0);
  Pos := gtk_widget_path_append_type(Path, AType1);
  if AName1 <> nil then
    gtk_widget_path_iter_set_object_name(Path, Pos, AName1);
  if AClass1 <> nil then
    gtk_widget_path_iter_add_class(Path, Pos, AClass1);
  Pos := gtk_widget_path_append_type(Path, AType2);
  if AName2 <> nil then
    gtk_widget_path_iter_set_object_name(Path, Pos, AName2);
  if AClass2 <> nil then
    gtk_widget_path_iter_add_class(Path, Pos, AClass2);
  Pos := gtk_widget_path_append_type(Path, AType3);
  if AName3 <> nil then
    gtk_widget_path_iter_set_object_name(Path, Pos, AName3);
  if AClass3 <> nil then
    gtk_widget_path_iter_add_class(Path, Pos, AClass3);
  gtk_widget_path_iter_set_state(Path, Pos, State);
  gtk_style_context_set_path(Ctx, Path);
  gtk_style_context_set_state(Ctx, State);
  gtk_widget_path_unref(Path);
  Result := Ctx;
end;

//draws thin arrow, instead of using pan-down-symbolic as icon,
//since normal combos in gtk3 uses same arrow draw pattern.
procedure DrawThinArrow(Cr: PCairo_t; Context: PGtkStyleContext;
  State: TGtkStateFlags; X, Y, Size: Integer; Angle: Double);
var
  FgColor: TGdkRGBA;
  HalfSz, LineW: Double;
begin
  gtk_style_context_get_color(Context, State, @FgColor);
  FgColor.alpha := 0.6;
  gdk_cairo_set_source_rgba(Cr, @FgColor);
  HalfSz := Size * 0.5;
  LineW  := Max(1.0, Size / 5.0 / Sqrt(2.0));
  cairo_save(Cr);
  cairo_set_line_width(Cr, LineW);
  cairo_set_line_join(Cr, CAIRO_LINE_JOIN_ROUND);
  cairo_set_line_cap(Cr, CAIRO_LINE_CAP_ROUND);
  cairo_translate(Cr, X + HalfSz, Y + HalfSz);
  cairo_rotate(Cr, Angle - Pi);
  cairo_move_to(Cr, -HalfSz, -Size / 4.0);
  cairo_line_to(Cr,       0,  Size / 4.0);
  cairo_line_to(Cr,  HalfSz, -Size / 4.0);
  cairo_stroke(Cr);
  cairo_restore(Cr);
end;

function TGTK3ThemeServices.InitThemes: Boolean;
begin
  Result := True;
end;

function TGTK3ThemeServices.UseThemes: Boolean;
begin
  Result := True;
end;

function TGTK3ThemeServices.ThemedControlsEnabled: Boolean;
begin
  Result := True;
end;

procedure TGTK3ThemeServices.InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect);
begin
  // TODO: Implement background drawing if necessary
end;

procedure TGTK3ThemeServices.DrawEdge(DC: HDC; Details: TThemedElementDetails;
  const R: TRect; Edge, Flags: Cardinal; AContentRect: PRect);
begin

end;

procedure TGTK3ThemeServices.DrawIcon(DC: HDC; Details: TThemedElementDetails;
  const R: TRect; himl: HIMAGELIST; Index: Integer);
begin
  inherited DrawIcon(DC, Details, R, himl, Index);
end;

procedure TGTK3ThemeServices.DrawText(ACanvas: TPersistent;
  Details: TThemedElementDetails; const S: String; R: TRect; Flags,
  Flags2: Cardinal);
begin
  (*DOES NOT WORK YET !
  writeln('TGTK3ThemeServices.DrawText: ACanvas ',dbgsName(ACanvas),' IsCanva=',(ACanvas is TCanvas));
  if (ACanvas is TCanvas) then
  begin
    inherited DrawText(TCanvas(ACanvas).Handle, Details, S, R, Flags, Flags2);
  end else
  *)
  inherited DrawText(ACanvas, Details, S, R, Flags, Flags2);
  //DrawText(DC, Details, S, R.Left, R.Top, R.Width, R.Height, Flags);
end;

procedure TGTK3ThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: String; R: TRect; Flags, Flags2: Cardinal);
begin
  inherited DrawText(DC, Details, S, R, Flags, Flags2);
  // DrawText(DC, Details, S, R.Left, R.Top, R.Width, R.Height, Flags);
end;

function TGTK3ThemeServices.GetDetailSizeForPPI(Details: TThemedElementDetails;
  PPI: Integer): TSize;
var
  AValue: TGValue;
  Context: PGtkStyleContext;
  min_width, min_height: gint;
begin
  Result := Size(0, 0);
  if Details.Element = teButton then
  begin
    if (Byte(Details.Part) in [BP_CHECKBOX, BP_RADIOBUTTON]) then
    begin
      if Byte(Details.Part) = BP_CHECKBOX then
        Context := GetStyleWidget(lgsCheckBox)^.get_style_context
      else
        Context := GetStyleWidget(lgsRadioButton)^.get_style_context;

      gtk_style_context_save(Context);

      if Byte(Details.Part) = BP_CHECKBOX then
        gtk_style_context_add_class(Context, 'check')
      else
        gtk_style_context_add_class(Context, 'radio');

      gtk_style_context_get(Context, gtk_style_context_get_state(Context),
        ['min-width', @min_width, 'min-height', @min_height, nil]);

      if (min_width <= 0) or (min_height <= 0) then
        Result := Size(16, 16)
      else
        Result := Size(min_width, min_height);
      gtk_style_context_restore(Context);

    end else
      Result := inherited;
  end else
  if Details.Element = teTreeview then
  begin
    Result := inherited;
    if Details.Part in [TVP_GLYPH, TVP_HOTGLYPH] then
    begin
      inc(Result.cx);
      inc(Result.cy);
    end;
  end else
  (* NOT YET READY
  if Details.Element = teToolBar then
  begin
    if (Details.Part = TP_DROPDOWNBUTTON) or (Details.Part = TP_SPLITBUTTONDROPDOWN) then
    begin
      // GTK3 has no named style metric for the toolbar dropdown indicator.
      // The indicator area is square, so use the toolbar button's preferred
      // minimum height as the indicator width.  Fall back to 16px at 96 Dpi.
      Result.cy := -1;
      Result.cx := 0;
      gtk_widget_get_preferred_height(GetStyleWidget(lgsToolButton), @Result.cx, nil);
      if Result.cx <= 0 then
        Result.cx := MulDiv(16, PPI, 96);
    end else
      Result := inherited;
  end else
  *)
    Result := inherited;
end;

function TGTK3ThemeServices.GetStockImage(StockID: LongInt; out Image,
  Mask: HBitmap): Boolean;
var
  IconName: PChar;
  IconTheme: PGtkIconTheme;
  Pixbuf: PGdkPixbuf;
  IconSize: Integer;
begin
  case StockID of
    idButtonOk: IconName := 'dialog-ok';
    idButtonCancel: IconName := 'process-stop';
    idButtonHelp: IconName := 'help-contents';
    idButtonYes: IconName := 'dialog-ok';
    idButtonNo: IconName := 'dialog-cancel';
    idButtonYesToAll: IconName := 'dialog-ok';
    idButtonNoToAll: IconName := 'dialog-cancel';
    idButtonClose: IconName := 'window-close';
    idButtonAbort: IconName := 'process-stop';
    idButtonRetry: IconName := 'view-refresh';
    idButtonIgnore: IconName := '';
    idButtonAll: IconName := 'edit-select-all';
    idButtonOpen: IconName := 'document-open';
    idButtonSave: IconName := 'document-save';
    idButtonShield: IconName := 'system-lock-screen';
    idDialogWarning: IconName := 'dialog-warning';
    idDialogError: IconName := 'dialog-error';
    idDialogInfo: IconName := 'dialog-information';
    idDialogConfirm: IconName := 'dialog-question';
    idDialogShield: IconName := 'dialog-password';
  else
    Result := inherited GetStockImage(StockID, Image, Mask);
    exit;
  end;

  if IconName = '' then
  begin
    Result := inherited GetStockImage(StockID, Image, Mask);
    exit;
  end;

  if StockID >= idDialogWarning then
  begin
    if not gtk_icon_size_lookup(Ord(GTK_ICON_SIZE_DIALOG), nil, @IconSize) then
      IconSize := 48;
  end else
  begin
    if not gtk_icon_size_lookup(Ord(GTK_ICON_SIZE_BUTTON), nil, @IconSize) then
      IconSize := 16;
  end;

  IconTheme := gtk_icon_theme_get_default();
  Pixbuf := gtk_icon_theme_load_icon(IconTheme, IconName, IconSize,
    [GTK_ICON_LOOKUP_FORCE_SIZE], nil);

  if Pixbuf = nil then
  begin
    Result := inherited GetStockImage(StockID, Image, Mask);
    exit;
  end;

  Image := HBitmap(TGtk3Image.Create(Pixbuf));
  g_object_unref(Pixbuf);
  Mask := 0;
  Result := True;
end;

function TGTK3ThemeServices.ContentRect(DC: HDC;
  Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  Result := inherited ContentRect(DC, Details, BoundingRect);
end;

function TGTK3ThemeServices.HasTransparentParts(Details: TThemedElementDetails
  ): Boolean;
begin
  Result := True;
end;

procedure TGTK3ThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  GtkDC: TGtk3DeviceContext absolute DC;
  AScreen: PGdkScreen;
begin
  if GtkDC.Parent <> nil then
    AScreen := gtk_widget_get_screen(GtkDC.Parent)
  else if GtkDC.Window <> nil then
    AScreen := gdk_window_get_screen(GtkDC.Window)
  else
    AScreen := gdk_screen_get_default();

  //if (Details.Element = teButton) and (Details.Part in [BP_CHECKBOX, BP_RADIOBUTTON]) then
  //  inherited DrawElement(DC, Details, R, ClipRect)
  //else
    Self.DrawElement(GtkDC.pcr, Details, R.Left, R.Top, R.Width, R.Height, AScreen);
end;

function GetThemeColor(Ctx: PGtkStyleContext; const Name: PChar; Default: TGdkRGBA): TGdkRGBA;
begin
  Result := Default;
  if not gtk_style_context_lookup_color(Ctx, Name, @Result) then
  begin
    // Fallback for some older themes that don't use 'theme_' prefix
    if StrPas(Name).StartsWith('theme_') then
      gtk_style_context_lookup_color(Ctx, PChar(Copy(Name, 7, 255)), @Result);
  end;
end;

function GdkColorToCSS(C: TGdkRGBA; A: Double = -1): string;
var
  AlphaValue: Double;
begin
  if A < 0 then AlphaValue := C.alpha else AlphaValue := A;
  Result := Format('rgba(%d,%d,%d,%.2f)', [Round(C.red * 255), Round(C.green * 255),
    Round(C.blue * 255), AlphaValue]);
end;

function TGTK3ThemeServices.MakeIndicatorContext(ParentType: TGType;
  const AClassName, IndicatorName: PChar; State: TGtkStateFlags;
  AScreen: PGdkScreen): PGtkStyleContext;
var
  Path: PGtkWidgetPath;
  Context: PGtkStyleContext;
  Provider: PGtkCssProvider;
  CSS: string;
  APos: gint;
  BaseBg, BaseFg, SelBg, SelFg, InsBg, InsFg: TGdkRGBA;
begin
  if AScreen = nil then
    AScreen := gdk_screen_get_default();

  Context := gtk_style_context_new();
  gtk_style_context_set_screen(Context, AScreen);

  Path := gtk_widget_path_new();
  gtk_widget_path_append_type(Path, ParentType);
  gtk_widget_path_iter_set_object_name(Path, 0, AClassName);
  gtk_widget_path_iter_add_class(Path, 0, 'toggle');
  gtk_widget_path_iter_set_state(Path, 0, State);
  APos := gtk_widget_path_append_type(Path, G_TYPE_NONE);
  gtk_widget_path_iter_set_object_name(Path, APos, IndicatorName);
  gtk_widget_path_iter_add_class(Path, APos, IndicatorName);
  gtk_widget_path_iter_set_state(Path, APos, State);
  gtk_style_context_set_path(Context, Path);
  gtk_style_context_add_class(Context, IndicatorName);
  gtk_style_context_set_state(Context, State);
  gtk_widget_path_unref(Path);

  if Pos('BREEZE', UpperCase(Gtk3WidgetSet.GetThemeName)) = 0 then
    exit(Context);

  {$note could not find the way to draw proper checkbox and radio,
   so use this CSS hack for now. Željan.
   This fix below belongs to Breeze theme only}
  CSS := Format(

  '%0:s { ' +
  '  background-image: none; ' +
  '  box-shadow: none; ' +
  '  outline: none; ' +
  '  background-color: transparent; ' +
  '  border-style: solid; ' +
  '  border-width: 1px; ' +
  '  border-color: alpha(@theme_fg_color, 0.3); ' + // Subtle default border
  '} ' +

  '%0:s:checked, %0:s:checked:focus { ' +
  '  background-image: none; ' +
  '  background-color: alpha(@theme_selected_bg_color, 0.3); ' +
  '  border-color: @theme_selected_bg_color; ' +
  '  color: @theme_fg_color; ' +
  '} ' +

  '%0:s:focus:not(:checked) { ' +
  '  border-color: alpha(@theme_selected_bg_color, 0.8); ' +
  '  background-color: transparent; ' +
  '} ' +

  '%0:s:disabled { ' +
  '  background-color: transparent; ' +
  '  border-color: alpha(@theme_fg_color, 0.3); ' +
  '  color: alpha(@theme_fg_color, 0.2); ' +

  '} ' +

  '%0:s:indeterminate, %0:s:indeterminate:focus { ' +
  '  background-image: none; ' +
  '  background-color: alpha(@theme_selected_bg_color, 0.3); ' +
  '  border-color: @theme_selected_bg_color; ' +
  '  color: @theme_fg_color; ' +
  '} ' +

  '%0:s:indeterminate:disabled { ' +
  '  background-color: transparent; ' +
  '  border-color: alpha(@theme_fg_color, 0.3); ' +
  '  color: alpha(@theme_fg_color, 0.2); ' +

  '}', [IndicatorName]);

  Provider := gtk_css_provider_new();
  gtk_css_provider_load_from_data(Provider, PChar(CSS), -1, nil);
  gtk_style_context_add_provider(Context, PGtkStyleProvider(Provider),
    GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref(Provider);

  Result := Context;
end;


function TGTK3ThemeServices.GetElementDetails(Details: TThemedElementDetails;
  AScreen: PGdkScreen): PGtkStyleContext;
var
  Path: PGtkWidgetPath;
  Context: PGtkStyleContext;
begin
  if AScreen = nil then
    AScreen := gdk_screen_get_default();
  Context := gtk_style_context_new();
  Path := gtk_widget_path_new();
  case Details.Element of
    teComboBox: gtk_widget_path_append_type(Path, gtk_combo_box_accessible_get_type);
    teButton:
      gtk_widget_path_append_type(Path, gtk_button_get_type());
    teToolBar: gtk_widget_path_append_type(Path, gtk_toolbar_get_type());
    teHeader: gtk_widget_path_append_type(Path, gtk_header_bar_get_type());
    teWindow: gtk_widget_path_append_type(Path, gtk_window_get_type());
    tePage: gtk_widget_path_append_type(Path, gtk_frame_get_type());
    teListView: gtk_widget_path_append_type(Path, gtk_icon_view_get_type);
    teTreeview: gtk_widget_path_append_type(Path, gtk_tree_view_get_type);
    teRebar: gtk_widget_path_append_type(Path, gtk_paned_get_type());
    else
      DebugLn(Format('WARNING: TGTK3ThemeServices.GetElementDetails cannot create context for element %d',[Ord(Details.Element)]));
  end;
  gtk_style_context_set_path(Context, Path);
  gtk_style_context_set_screen(Context, AScreen);
  gtk_widget_path_unref(Path);
  Result := Context;
end;

function TGTK3ThemeServices.GetControlState(Details: TThemedElementDetails): TGtkStateFlags;
begin
  Result := GTK_STATE_FLAG_NORMAL;
  if IsDisabled(Details) then
    Include(Result, GTK_STATE_FLAG_INSENSITIVE);
  if IsHot(Details) then
    Include(Result, GTK_STATE_FLAG_PRELIGHT);
  if IsPushed(Details) then
    Include(Result, GTK_STATE_FLAG_ACTIVE);
  if IsChecked(Details) then
  begin
    Include(Result, GTK_STATE_FLAG_CHECKED);
    if (Details.Element = teButton) and
       (Details.Part in [BP_CHECKBOX, BP_RADIOBUTTON]) then
      Include(Result, GTK_STATE_FLAG_ACTIVE);
  end;

  if (Details.Element = teButton) and
     (Details.Part = BP_CHECKBOX) and
     IsMixed(Details) then
    Include(Result, GTK_STATE_FLAG_INCONSISTENT);

  // For checkbox/radio indicators the LCL "Hot" state covers both mouse-hover
  // and keyboard focus. Map it to PRELIGHT + FOCUSED so Adwaita renders both
  // the hover highlight and the focus ring correctly.
  if (Details.Element = teButton) and
     (Details.Part in [BP_CHECKBOX, BP_RADIOBUTTON]) and
     IsHot(Details) then
    Include(Result, GTK_STATE_FLAG_FOCUSED);

  // define orientations
  if ((Details.Element = teRebar) and (Details.Part = RP_GRIPPER)) or
     ((Details.Element = teToolBar) and (Details.Part = TP_SEPARATOR)) or
     ((Details.Element = teScrollBar) and (Details.Part in
       [SBP_UPPERTRACKHORZ, SBP_LOWERTRACKHORZ, SBP_THUMBBTNHORZ, SBP_GRIPPERHORZ])) or
     ((Details.Element = teTrackbar) and not (Details.Part in
       [TKP_TRACKVERT, TKP_THUMBVERT])) then
       Include(Result, GTK_STATE_FLAG_DIR_LTR);

  if (Details.Element in [teTreeview, teListView]) then
  begin
    if (Details.Element = teTreeView) and
      (Details.Part in [TVP_GLYPH, TVP_HOTGLYPH]) then
    begin
      Include(Result, GTK_STATE_FLAG_PRELIGHT);
      if Details.State = GLPS_OPENED then
        Include(Result, GTK_STATE_FLAG_CHECKED);
    end else
    if Details.Part in [TVP_TREEITEM] then
    begin
      case Details.State of
        TREIS_SELECTED:
        begin
          Include(Result, GTK_STATE_FLAG_ACTIVE);
          Include(Result, GTK_STATE_FLAG_SELECTED);
          Include(Result, GTK_STATE_FLAG_FOCUSED);
        end;
        TREIS_SELECTEDNOTFOCUS:
          Include(Result, GTK_STATE_FLAG_SELECTED);
        TREIS_HOTSELECTED:
        begin
          Include(Result, GTK_STATE_FLAG_SELECTED);
          Include(Result, GTK_STATE_FLAG_PRELIGHT);
        end;
        TREIS_HOT:
          Include(Result, GTK_STATE_FLAG_PRELIGHT);
      end;
    end;
  end else
  if (Details.Element = teTrackBar) then
  begin
    if Details.Part in [TKP_THUMB, TKP_THUMBVERT] then
    begin
      if Details.State in [TUS_PRESSED, TUS_HOT] then
      begin
        Include(Result, GTK_STATE_FLAG_ACTIVE);
        Include(Result, GTK_STATE_FLAG_PRELIGHT);
        Include(Result, GTK_STATE_FLAG_FOCUSED);
      end;
    end;
  end else
  if (Details.Element = teEdit) and (Details.Part in [EP_EDITTEXT, EP_BACKGROUND, EP_BACKGROUNDWITHBORDER]) then
  begin
    if Details.State = ETS_FOCUSED then
    begin
      Include(Result, GTK_STATE_FLAG_ACTIVE);
      Include(Result, GTK_STATE_FLAG_SELECTED);
      Include(Result, GTK_STATE_FLAG_FOCUSED);
    end;

    if Details.State = ETS_HOT then
      Include(Result, GTK_STATE_FLAG_PRELIGHT)
    else
    if Details.State = ETS_READONLY then
      Include(Result, GTK_STATE_FLAG_INCONSISTENT)
    else
    if Details.State = ETS_SELECTED then
      Include(Result, GTK_STATE_FLAG_SELECTED)
  end else
  if (Details.Element = teWindow) then
  begin
    case Details.Part of
      WP_CAPTION, WP_SMALLCAPTION,
      WP_MINCAPTION, WP_SMALLMINCAPTION,
      WP_MAXCAPTION, WP_SMALLMAXCAPTION,
      WP_FRAMELEFT, WP_FRAMERIGHT, WP_FRAMEBOTTOM,
      WP_SMALLFRAMELEFT, WP_SMALLFRAMERIGHT, WP_SMALLFRAMEBOTTOM:
      begin
        case Details.State of
          CS_ACTIVE: Include(Result, GTK_STATE_FLAG_FOCUSED);
          CS_INACTIVE: Include(Result, GTK_STATE_FLAG_BACKDROP);
        end;
      end;
      WP_CLOSEBUTTON, WP_SMALLCLOSEBUTTON, WP_MDICLOSEBUTTON,
      WP_MINBUTTON, WP_MDIMINBUTTON, WP_MAXBUTTON,
      WP_RESTOREBUTTON, WP_MDIRESTOREBUTTON,
      WP_HELPBUTTON, WP_MDIHELPBUTTON,
      WP_SYSBUTTON, WP_MDISYSBUTTON:
      begin
        case Details.State of
          2: Include(Result, GTK_STATE_FLAG_PRELIGHT);
          3:
          begin
            Include(Result, GTK_STATE_FLAG_ACTIVE);
            Include(Result, GTK_STATE_FLAG_PRELIGHT);
          end;
          4: Include(Result, GTK_STATE_FLAG_INSENSITIVE);
        end;
      end;
    end;
  end else
  if (Details.Element = teTab) then
  begin
    case Details.State of
      TIS_HOT: Include(Result, GTK_STATE_FLAG_PRELIGHT);
      TIS_SELECTED: Include(Result, GTK_STATE_FLAG_CHECKED);
      TIS_DISABLED: Include(Result, GTK_STATE_FLAG_INSENSITIVE);
      TIS_FOCUSED:
      begin
        Include(Result, GTK_STATE_FLAG_CHECKED);
        Include(Result, GTK_STATE_FLAG_FOCUSED);
      end;
    end;
  end else
  if Details.Element = teComboBox then
  begin
    case Details.State of
      CBXS_HOT: Include(Result, GTK_STATE_FLAG_PRELIGHT);
      CBXS_PRESSED:
      begin
        Include(Result, GTK_STATE_FLAG_ACTIVE);
        Include(Result, GTK_STATE_FLAG_PRELIGHT);
      end;
      CBXS_DISABLED: Include(Result, GTK_STATE_FLAG_INSENSITIVE);
    end;
  end else
  if Details.Element = teToolBar then
  begin
    case Details.State of
      TS_HOT:
      begin
        Include(Result, GTK_STATE_FLAG_PRELIGHT);
      end;
      TS_PRESSED:
      begin
        Include(Result, GTK_STATE_FLAG_ACTIVE);
        Include(Result, GTK_STATE_FLAG_PRELIGHT);
      end;
      TS_CHECKED:
        Include(Result, GTK_STATE_FLAG_CHECKED);
      TS_HOTCHECKED:
      begin
        Include(Result, GTK_STATE_FLAG_CHECKED);
        Include(Result, GTK_STATE_FLAG_PRELIGHT);
      end;
      TS_DISABLED:
        Include(Result, GTK_STATE_FLAG_INSENSITIVE);
    end;
  end else
  if (Details.Element = teScrollBar) and (Details.Part = SBP_ARROWBTN) then
  begin
    if Details.State in [ABS_UPDISABLED, ABS_DOWNDISABLED,
      ABS_LEFTDISABLED, ABS_RIGHTDISABLED] then
        Include(Result, GTK_STATE_FLAG_INSENSITIVE)
    else
    if Details.State in [ABS_UPHOT, ABS_DOWNHOT, ABS_LEFTHOT, ABS_RIGHTHOT,
      ABS_UPHOVER, ABS_DOWNHOVER, ABS_LEFTHOVER, ABS_RIGHTHOVER] then
        Include(Result, GTK_STATE_FLAG_PRELIGHT)
    else
    if Details.State in [ABS_UPPRESSED, ABS_DOWNPRESSED,
      ABS_LEFTPRESSED, ABS_RIGHTPRESSED] then
    begin
      Include(Result, GTK_STATE_FLAG_ACTIVE);
      Include(Result, GTK_STATE_FLAG_PRELIGHT);
    end;
  end;
end;

procedure TGTK3ThemeServices.DrawElement(Cr: Pcairo_t; Details: TThemedElementDetails; X, Y, Width, Height: Integer; AScreen: PGdkScreen);
var
  Context: PGtkStyleContext;
  IconTheme: PGtkIconTheme;
  IconInfo: PGtkIconInfo;
  Pixbuf: PGdkPixbuf;
  WasSymbolic: gboolean;
  State: TGtkStateFlags;
  ArrowAngle: Double;
  ArrowSz, ArrowX, ArrowY: Integer;
  AOldOperator: Tcairo_operator_t;
  ScrollOrient: PChar;
  BtnClass, BtnIconName: PChar;
begin
  State := GetControlState(Details);
  AOldOperator := cairo_get_operator(cr);
  cairo_set_operator(Cr, CAIRO_OPERATOR_OVER);
  try
    case Details.Element of
      teButton:
      begin
        if Details.Part = BP_CHECKBOX then
        begin
          Context := MakeIndicatorContext(gtk_check_button_get_type(), 'checkbutton', 'check', State, AScreen);
          gtk_render_background(Context, Cr, X, Y, Width, Height);
          gtk_render_frame(Context, Cr, X, Y, Width, Height);
          gtk_render_check(Context, Cr, X, Y, Width, Height);
          if GTK_STATE_FLAG_FOCUSED in State then
            gtk_render_focus(Context, Cr, X, Y, Width, Height);
          g_object_unref(Context);
        end else
        if Details.Part = BP_RADIOBUTTON then
        begin
          Context := MakeIndicatorContext(gtk_radio_button_get_type(), 'radiobutton', 'radio', State, AScreen);
          gtk_render_background(Context, Cr, X, Y, Width, Height);
          gtk_render_frame(Context, Cr, X, Y, Width, Height);
          gtk_render_option(Context, Cr, X, Y, Width, Height);
          if GTK_STATE_FLAG_FOCUSED in State then
            gtk_render_focus(Context, Cr, X, Y, Width, Height);
          g_object_unref(Context);
        end else
        begin
          Context := MakeCtx1(gtk_button_get_type, 'button', 'button', State, AScreen);
          gtk_style_context_add_class(Context, 'text-button');
          gtk_render_background(Context, Cr, X, Y, Width, Height);
          gtk_render_frame(Context, Cr, X, Y, Width, Height);
          if GTK_STATE_FLAG_FOCUSED in State then
            gtk_render_focus(Context, Cr, X, Y, Width, Height);
          g_object_unref(Context);
        end;
      end;

      teToolBar:
      begin
        case Details.Part of
          TP_BUTTON, TP_SPLITBUTTON:
          begin
            // toolbar > toolbutton > button.toolbutton
            Context := MakeCtx3(
              gtk_toolbar_get_type, 'toolbar', nil,
              G_TYPE_NONE, 'toolbutton', nil,
              gtk_button_get_type, 'button', 'toolbutton',
              State, AScreen);
            if Details.State in [1, 4] then
              gtk_style_context_add_class(Context, 'flat');
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            if GTK_STATE_FLAG_FOCUSED in State then
              gtk_render_focus(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;
          TP_DROPDOWNBUTTON, TP_SPLITBUTTONDROPDOWN:
          begin
            // Arrow-only area: button background/frame + pan-down-symbolic icon.
            Context := MakeCtx3(gtk_toolbar_get_type, 'toolbar', nil, G_TYPE_NONE, 'toolbutton', nil,
              gtk_button_get_type, 'button', 'toolbutton', State, AScreen);
            if Details.State in [1, 4] then
              gtk_style_context_add_class(Context, 'flat');
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            ArrowSz := Min(Width, Height) div 2;
            ArrowX := X + (Width  - ArrowSz) div 2;
            ArrowY := Y + (Height - ArrowSz) div 2;
            // Use pan-down-symbolic, fallback to DrawThinArrow if not in theme.
            IconInfo := nil;
            (* Works ok, but we'll use drawn arrow because all ctls in gtk3 uses such.
            IconTheme := gtk_icon_theme_get_for_screen(AScreen);
            IconInfo := gtk_icon_theme_lookup_icon(IconTheme, 'pan-down-symbolic',
              ArrowSz, [GTK_ICON_LOOKUP_FORCE_SYMBOLIC]); *)
            if IconInfo <> nil then
            begin
              WasSymbolic := False;
              Pixbuf := gtk_icon_info_load_symbolic_for_context(IconInfo, Context, @WasSymbolic, nil);
              g_object_unref(IconInfo);
              if Pixbuf <> nil then
              begin
                gdk_cairo_set_source_pixbuf(Cr, Pixbuf, ArrowX, ArrowY);
                cairo_paint(Cr);
                g_object_unref(Pixbuf);
              end else
                DrawThinArrow(Cr, Context, State, ArrowX, ArrowY, ArrowSz, Pi);
            end else
              DrawThinArrow(Cr, Context, State, ArrowX, ArrowY, ArrowSz, Pi);
            g_object_unref(Context);
          end;
        end;
      end;

      teHeader:
      begin
        case Details.Part of
          HP_HEADERITEM, HP_HEADERITEMLEFT, HP_HEADERITEMRIGHT:
          begin
            Context := MakeCtx3(gtk_tree_view_get_type, 'treeview', 'view',
              G_TYPE_NONE, 'header', nil, gtk_button_get_type, 'button', 'button', State, AScreen);
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            if GTK_STATE_FLAG_FOCUSED in State then
              gtk_render_focus(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;
          HP_HEADERSORTARROW:
          begin
            Context := MakeCtx3(gtk_tree_view_get_type, 'treeview', 'view',
              G_TYPE_NONE, 'header', nil, gtk_button_get_type, 'button', 'button',
              GTK_STATE_FLAG_NORMAL, AScreen);
            ArrowSz := Min(Width, Height);
            ArrowX := X + (Width  - ArrowSz) div 2;
            ArrowY := Y + (Height - ArrowSz) div 2;
            if Details.State = HSAS_SORTEDUP then
              DrawThinArrow(Cr, Context, State, ArrowX, ArrowY, ArrowSz, 0)
            else
              DrawThinArrow(Cr, Context, State, ArrowX, ArrowY, ArrowSz, Pi);
            g_object_unref(Context);
          end;
          HP_HEADERDROPDOWN, HP_HEADERDROPDOWNFILTER:
          begin
            Context := MakeCtx3(gtk_tree_view_get_type, 'treeview', 'view',
              G_TYPE_NONE, 'header', nil, gtk_button_get_type, 'button', 'button', State, AScreen);
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            ArrowSz := Min(Width, Height) div 2;
            ArrowX := X + (Width  - ArrowSz) div 2;
            ArrowY := Y + (Height - ArrowSz) div 2;
            DrawThinArrow(Cr, Context, State, ArrowX, ArrowY, ArrowSz, Pi);
            g_object_unref(Context);
          end;
          HP_HEADEROVERFLOW:
          begin
            Context := MakeCtx3(gtk_tree_view_get_type, 'treeview', 'view',
              G_TYPE_NONE, 'header', nil, gtk_button_get_type, 'button', 'button', State, AScreen);
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            ArrowSz := Min(Width, Height) div 2;
            ArrowX := X + (Width  - ArrowSz) div 2;
            ArrowY := Y + (Height - ArrowSz) div 2;
            DrawThinArrow(Cr, Context, State, ArrowX, ArrowY, ArrowSz, Pi / 2);
            g_object_unref(Context);
          end;
        end;
      end;

      teComboBox:
      begin
        case Details.Part of
          CP_DROPDOWNBUTTON, CP_DROPDOWNBUTTONRIGHT, CP_DROPDOWNBUTTONLEFT:
          begin
            Context := MakeCtx3(gtk_combo_box_get_type, 'combobox', nil, G_TYPE_NONE, 'box', 'linked',
              gtk_button_get_type, 'button', 'combo', State, AScreen);
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);

            ArrowSz := Min(Width, Height) div 2;
            ArrowX := X + (Width  - ArrowSz) div 2;
            ArrowY := Y + (Height - ArrowSz) div 2;
            DrawThinArrow(Cr, Context, State, ArrowX, ArrowY, ArrowSz, Pi);
            g_object_unref(Context);
          end;
          CP_READONLY:
          begin
            Context := MakeCtx1(gtk_button_get_type, 'button', 'button', State, AScreen);
            gtk_style_context_add_class(Context, 'combo');
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);

            ArrowSz := Height div 2;
            ArrowX := X + Width - Height + (Height - ArrowSz) div 2;
            ArrowY := Y + (Height - ArrowSz) div 2;
            DrawThinArrow(Cr, Context, State, ArrowX, ArrowY, ArrowSz, Pi);
            if GTK_STATE_FLAG_FOCUSED in State then
              gtk_render_focus(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;
          else // default case
          begin
            Context := MakeCtx3(
              gtk_combo_box_get_type, 'combobox', nil,
              G_TYPE_NONE, 'box', 'linked',
              gtk_entry_get_type, 'entry', 'combo',
              State, AScreen);
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            if GTK_STATE_FLAG_FOCUSED in State then
              gtk_render_focus(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;
        end;
      end;

      teRebar:
      begin
        case Details.Part of
          RP_BAND:
          begin
            Context := MakeCtx1(gtk_toolbar_get_type, 'toolbar', 'toolbar', State, AScreen);
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;
          RP_GRIPPER, RP_GRIPPERVERT:
          begin
            Context := MakeCtx2(gtk_toolbar_get_type, 'toolbar', 'toolbar', G_TYPE_NONE, 'separator', 'separator', State, AScreen);
            gtk_render_handle(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;
        end;
      end;

      teScrollBar:
      begin
        if Details.Part in [SBP_THUMBBTNHORZ, SBP_UPPERTRACKHORZ,
          SBP_LOWERTRACKHORZ, SBP_GRIPPERHORZ] then
            ScrollOrient := 'horizontal'
        else
        if Details.Part in [SBP_THUMBBTNVERT, SBP_UPPERTRACKVERT,
          SBP_LOWERTRACKVERT, SBP_GRIPPERVERT] then
            ScrollOrient := 'vertical'
        else

        if Details.State in [ABS_LEFTNORMAL, ABS_LEFTHOT, ABS_LEFTPRESSED, ABS_LEFTDISABLED,
          ABS_RIGHTNORMAL, ABS_RIGHTHOT, ABS_RIGHTPRESSED, ABS_RIGHTDISABLED,
          ABS_LEFTHOVER, ABS_RIGHTHOVER] then
            ScrollOrient := 'horizontal'
        else
          ScrollOrient := 'vertical';

        case Details.Part of
          SBP_ARROWBTN:
          begin
            if Details.State in [ABS_UPNORMAL, ABS_UPHOT, ABS_UPPRESSED,
              ABS_UPDISABLED, ABS_UPHOVER] then
                ArrowAngle := 0
            else
            if Details.State in [ABS_DOWNNORMAL, ABS_DOWNHOT, ABS_DOWNPRESSED,
              ABS_DOWNDISABLED, ABS_DOWNHOVER] then
                ArrowAngle := Pi
            else
            if Details.State in [ABS_LEFTNORMAL, ABS_LEFTHOT, ABS_LEFTPRESSED,
              ABS_LEFTDISABLED, ABS_LEFTHOVER] then
                ArrowAngle := 3 * Pi / 2
            else
              ArrowAngle := Pi / 2;  // right
            Context := MakeCtx1(gtk_button_get_type, 'button', 'button', State, AScreen);
            gtk_style_context_add_class(Context, 'text-button');
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            ArrowSz := Min(Width, Height) div 2;
            ArrowX := X + (Width  - ArrowSz) div 2;
            ArrowY := Y + (Height - ArrowSz) div 2;
            DrawThinArrow(Cr, Context, State, ArrowX, ArrowY, ArrowSz, ArrowAngle);
            g_object_unref(Context);
          end;

          SBP_UPPERTRACKHORZ, SBP_LOWERTRACKHORZ,
          SBP_UPPERTRACKVERT, SBP_LOWERTRACKVERT:
          begin
            Context := MakeCtx2(gtk_scrollbar_get_type, 'scrollbar', ScrollOrient, G_TYPE_NONE, 'trough', nil, State, AScreen);
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;

          SBP_THUMBBTNHORZ, SBP_THUMBBTNVERT:
          begin
            Context := MakeCtx3(gtk_scrollbar_get_type, 'scrollbar', ScrollOrient, G_TYPE_NONE, 'trough', nil,
              G_TYPE_NONE, 'slider', nil, State, AScreen);
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;
        end;
      end;

      teTab:
      begin
        if Details.Part in [TABP_TABITEM, TABP_TABITEMLEFTEDGE,
          TABP_TABITEMRIGHTEDGE, TABP_TABITEMBOTHEDGE, TABP_TOPTABITEM] then
        begin
          Context := MakeCtx4(
            gtk_notebook_get_type, 'notebook', nil,
            G_TYPE_NONE, 'header', 'top',
            G_TYPE_NONE, 'tabs', nil,
            G_TYPE_NONE, 'tab', nil,
            State, AScreen);
          gtk_render_background(Context, Cr, X, Y, Width, Height);
          gtk_render_frame(Context, Cr, X, Y, Width, Height);
          if GTK_STATE_FLAG_FOCUSED in State then
            gtk_render_focus(Context, Cr, X, Y, Width, Height);
          g_object_unref(Context);
        end;
      end;

      teTreeview, teListView:
      begin
        case Details.Part of
          TVP_TREEITEM:
          begin
            Context := MakeCtx2(
              gtk_tree_view_get_type, 'treeview', 'view', G_TYPE_NONE, 'row', nil,
              State, AScreen);
            if GTK_STATE_FLAG_SELECTED in State then
              gtk_render_background(Context, Cr, X, Y, Width, Height);
            if GTK_STATE_FLAG_FOCUSED in State then
              gtk_render_focus(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;
          TVP_GLYPH, TVP_HOTGLYPH:
          begin
            Context := MakeCtx1(gtk_tree_view_get_type, 'treeview', 'view', State, AScreen);
            gtk_render_expander(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;
        end;
      end;

      teEdit:
      begin
        if GTK_STATE_FLAG_PRELIGHT in State then
          Include(State, GTK_STATE_FLAG_FOCUSED);
        Context := MakeCtx1(gtk_entry_get_type, 'entry', 'entry', State, AScreen);
        if Details.State = ETS_READONLY then
          gtk_style_context_add_class(Context, 'read-only');
        gtk_render_background(Context, Cr, X, Y, Width, Height);
        gtk_render_frame(Context, Cr, X, Y, Width, Height);
        if GTK_STATE_FLAG_FOCUSED in State then
          gtk_render_focus(Context, Cr, X, Y, Width, Height);
        g_object_unref(Context);
      end;

      teWindow:
      begin
        case Details.Part of
          WP_CAPTION, WP_SMALLCAPTION,
          WP_MINCAPTION, WP_SMALLMINCAPTION,
          WP_MAXCAPTION, WP_SMALLMAXCAPTION:
          begin
            // headerbar.titlebar
            Context := MakeCtx1(gtk_header_bar_get_type, 'headerbar', 'titlebar', State, AScreen);
            // Belt-and-suspenders: also add "backdrop" CSS class for inactive state,
            // since some themes match :backdrop via class rather than state flags.
            if GTK_STATE_FLAG_BACKDROP in State then
              gtk_style_context_add_class(Context, 'backdrop');
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;

          WP_FRAMELEFT, WP_FRAMERIGHT, WP_FRAMEBOTTOM,
          WP_SMALLFRAMELEFT, WP_SMALLFRAMERIGHT, WP_SMALLFRAMEBOTTOM:
          begin
            // window.csd border
            Context := MakeCtx1(gtk_window_get_type, 'window', 'csd', State, AScreen);
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            g_object_unref(Context);
          end;

          WP_CLOSEBUTTON, WP_SMALLCLOSEBUTTON, WP_MDICLOSEBUTTON,
          WP_MINBUTTON, WP_MDIMINBUTTON,
          WP_MAXBUTTON,
          WP_RESTOREBUTTON, WP_MDIRESTOREBUTTON,
          WP_HELPBUTTON, WP_MDIHELPBUTTON,
          WP_SYSBUTTON, WP_MDISYSBUTTON:
          begin
            case Details.Part of
              WP_CLOSEBUTTON, WP_SMALLCLOSEBUTTON, WP_MDICLOSEBUTTON:
              begin
                BtnClass := 'close';
                BtnIconName := 'window-close-symbolic';
              end;
              WP_MINBUTTON, WP_MDIMINBUTTON:
              begin
                BtnClass := 'minimize';
                BtnIconName := 'window-minimize-symbolic';
              end;
              WP_MAXBUTTON:
              begin
                BtnClass := 'maximize';
                BtnIconName := 'window-maximize-symbolic';
              end;
              WP_RESTOREBUTTON, WP_MDIRESTOREBUTTON:
              begin
                BtnClass := 'maximize';
                BtnIconName := 'window-restore-symbolic';
              end;
              WP_HELPBUTTON, WP_MDIHELPBUTTON:
              begin
                BtnClass := '';
                BtnIconName := 'dialog-question-symbolic';
              end;
            else
              BtnClass := '';
              BtnIconName := '';
            end;

            // headerbar.titlebar > button.titlebutton[.close/.minimize/.maximize]
            Context := MakeCtx2(gtk_header_bar_get_type, 'headerbar', 'titlebar',
              gtk_button_get_type, 'button', 'titlebutton', State, AScreen);
            if BtnClass <> '' then
              gtk_style_context_add_class(Context, BtnClass);
            // Clip background/frame to button rect, the theme CSS for hover/pressed
            // renders the icon via -gtk-icon-source at a fixed native size that
            // overflows our rect. Clipping prevents that overflow.
            cairo_save(Cr);
            cairo_rectangle(Cr, X, Y, Width, Height);
            cairo_clip(Cr);
            gtk_render_background(Context, Cr, X, Y, Width, Height);
            gtk_render_frame(Context, Cr, X, Y, Width, Height);
            cairo_restore(Cr);

            if BtnIconName <> '' then
            begin
              ArrowSz := Min(Width, Height) * 2 div 3;
              ArrowX := X + (Width  - ArrowSz) div 2;
              ArrowY := Y + (Height - ArrowSz) div 2;
              IconTheme := gtk_icon_theme_get_default();
              Pixbuf := gtk_icon_theme_load_icon(IconTheme, BtnIconName, ArrowSz,
                [GTK_ICON_LOOKUP_FORCE_SIZE], nil);
              if Pixbuf <> nil then
              begin
                // Render icon in Normal state so PRELIGHT/ACTIVE context state
                // does not apply gtk_render_icon's state effects on top of the
                // already-drawn hover background.
                if GTK_STATE_FLAG_PRELIGHT in State then
                  gtk_style_context_set_state(Context, GTK_STATE_FLAG_NORMAL);
                gtk_render_icon(Context, Cr, Pixbuf, ArrowX, ArrowY);
                g_object_unref(Pixbuf);
              end;
            end;
            g_object_unref(Context);
          end;
        end;
      end;

      else
        DebugLn(Format('WARNING: TGtk3ThemeServices.DrawElement: Drawing for element %d not implemented.',[Ord(Details.Element)]));
    end;
  finally
    cairo_set_operator(Cr, AOldOperator);
  end;

end;

procedure TGTK3ThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: String; X, Y, Width, Height: Integer; Flags: Cardinal);
var
  Layout: PPangoLayout;
  Context: PGtkStyleContext;
  FontDesc: PPangoFontDescription;
  GtkDC: TGtk3DeviceContext absolute DC;
  SafeS: String;
begin
  Context := GetElementDetails(Details);
  gtk_style_context_set_state(Context, GetControlState(Details));
  Layout := pango_cairo_create_layout(GtkDc.pcr);
  if g_utf8_validate(PChar(S), Length(S), nil) then
    pango_layout_set_text(Layout, PChar(S), Length(S))
  else
  begin
    SafeS := S;
    UTF8FixBroken(SafeS);
    pango_layout_set_text(Layout, PChar(SafeS), Length(SafeS));
  end;

  FontDesc := GtkDC.CurrentFont.Handle;
  pango_layout_set_font_description(Layout, FontDesc);
  cairo_move_to(GtkDc.pcr, X, Y);
  pango_cairo_show_layout(GtkDc.pcr, Layout);
  g_object_unref(Context);
  g_object_unref(Layout);
end;

end.

