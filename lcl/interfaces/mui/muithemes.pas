{
 *****************************************************************************
 *                             MUIThemes.pas                                 *
 *                              --------------                               *
 *                         Basic MUI Themes support                          *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit MUIThemes;

{$mode objfpc}{$H+}

interface

uses
  // rtl
  Types, Classes, SysUtils,
  // os
  MUI, agraphics, intuition,
  // lcl
  LCLType, LCLProc, LCLIntf, Graphics, Themes, TmSchema, Forms, Controls,
  // widgetset
  MUIdrawing, MUIBaseUnit;

type
  { TMUIThemeServices }

  TMUIThemeServices = class(TThemeServices)
  protected

    function InitThemes: Boolean; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;

    procedure InternalDrawParentBackground({%H-}Window: HWND; {%H-}Target: HDC; {%H-}Bounds: PRect); override;
    function GetBaseDetailsSize(Details: TThemedElementDetails): TSize;

    function GetParamsCount(Details: TThemedElementDetails): Integer; virtual;
  public

    function GetDetailSize(Details: TThemedElementDetails): TSize; override;
    function GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean; override;
    function GetOption(AOption: TThemeOption): Integer; override;
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails;
       const R: TRect; ClipRect: PRect); override;

    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: String; R: TRect; Flags, {%H-}Flags2: Cardinal); override;
    procedure DrawText(ACanvas: TPersistent; Details: TThemedElementDetails; const S: String; R: TRect; Flags, Flags2: Cardinal); virtual; overload; reintroduce;

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts({%H-}Details: TThemedElementDetails): Boolean; override;
  end;

implementation
uses math, muiint;

{ TMUIThemeServices }

function TMUIThemeServices.InitThemes: Boolean;
begin
  Result:=True;
end;

function TMUIThemeServices.UseThemes: Boolean;
begin
  Result:=True;
end;

function TMUIThemeServices.ThemedControlsEnabled: Boolean;
begin
  Result:=True;
end;

procedure TMUIThemeServices.InternalDrawParentBackground(Window: HWND;
  Target: HDC; Bounds: PRect);
begin
  // ToDo: TMUIThemeServices.InternalDrawParentBackground: What to do?
end;

function TMUIThemeServices.GetBaseDetailsSize(Details: TThemedElementDetails): TSize;
begin
  Result := inherited GetDetailSize(Details);
end;

function TMUIThemeServices.GetParamsCount(Details: TThemedElementDetails): Integer;
begin
  if (Details.Element = teTreeview) and (Details.Part = TVP_TREEITEM) then
    Result := 2
  else
  begin
    Result := 1;
    if (Details.Element = teToolBar) and (Details.Part = TP_SPLITBUTTONDROPDOWN) then
      inc(Result); // + Arrow
  end;
end;

function TMUIThemeServices.GetDetailSize(Details: TThemedElementDetails): TSize;
//var
//  AValue: TGValue;
begin
  Result := inherited GetDetailSize(Details);
  (*
  if (Details.Element = teTreeView) and (Byte(Details.Part) in [TVP_GLYPH, TVP_HOTGLYPH]) then
  begin
    FillChar(AValue{%H-}, SizeOf(AValue), 0);
    g_value_init(@AValue, G_TYPE_INT);
    gtk_widget_style_get_property(GetStyleWidget(lgsTreeView), 'expander-size', @AValue);
    Result := Size(AValue.data[0].v_int, AValue.data[0].v_int);
  end else
  if (Details.Element = teButton) and (Byte(Details.Part) in [BP_CHECKBOX, BP_RADIOBUTTON]) then
  begin
      FillChar(AValue{%H-}, SizeOf(AValue), 0);
    g_value_init(@AValue, G_TYPE_INT);
    if Details.Part = BP_CHECKBOX then
      gtk_widget_style_get_property(GetStyleWidget(lgsCheckbox),'indicator-size', @AValue)
    else
      gtk_widget_style_get_property(GetStyleWidget(lgsRadioButton),'indicator-size', @AValue);
    Result := Size(AValue.data[0].v_int, AValue.data[0].v_int);
  end else
    Result := GetBaseDetailsSize(Details);
  Result := Size(1,1);*)
end;

function TMUIThemeServices.GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean;
{var
  GDIObj: PGDIObject;
  StockName: PChar;
  Style: PGtkStyle;
  IconSet: PGtkIconSet;
  Pixbuf: PGDKPixbuf;}
begin
(*  case StockID of
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

  GDIObj := MUIWidgetset.NewGDIObject(gdiBitmap);
  with GDIObj^ do
  begin
    GDIBitmapType := gbPixbuf;
    visual := gdk_visual_get_system();
    gdk_visual_ref(visual);
    colormap := gdk_colormap_get_system();
    gdk_colormap_ref(colormap);
    GDIPixbufObject := Pixbuf;
  end;

  Image := HBitmap({%H-}PtrUInt(GDIObj));
  Mask := 0;
  Image := 0;
  Mask := 0;
  Result := False;*)
  Result := inherited GetStockImage(StockID, Image, Mask);
end;

function TMUIThemeServices.GetOption(AOption: TThemeOption): Integer;
{var
  ASettings: PGtkSettings;
  BoolSetting: gboolean;
  Widget: PGtkWidget;
  Signal: guint;}
begin
(*  case AOption of
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
          g_object_set_data(PGObject(Widget), 'lcl-images-change-callback', {%H-}Pointer(PtrUInt(Signal)))
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
          g_object_set_data(PGObject(Widget), 'lcl-images-change-callback', {%H-}Pointer(PtrUInt(Signal)))
        end;
      end;
  else*)
    Result := inherited GetOption(AOption);
  //end;
end;


procedure TMUIThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  r1: TRect;
begin
  //writeln('element: ', Details.Element);
  if Details.Element = teButton then
  begin
    r1 := r;
    if Details.Part = 1 then
    begin
      if Details.State = 1 then
        Frame3d(DC, r1, 1, bvRaised);
      if Details.State = 3 then
        Frame3d(DC, r1, 1, bvLowered);
    end;
  end
  else
    inherited DrawElement(DC, Details, R, ClipRect);
end;

procedure TMUIThemeServices.DrawText(ACanvas: TPersistent;
  Details: TThemedElementDetails; const S: String; R: TRect; Flags,
  Flags2: Cardinal);
begin
  if ThemesEnabled then
    DrawText(TCanvas(ACanvas).Handle, Details, S, R, Flags, Flags2)
  else
    inherited;
end;

procedure TMUIThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: String; R: TRect; Flags, Flags2: Cardinal);
var
  P: PChar;
  tmpRect: TRect;
begin
  P := PChar(S);
  tmpRect := R;
  MUIWidgetset.DrawText(DC, P, Length(S), tmpRect, Flags);
end;

function TMUIThemeServices.ContentRect(DC: HDC;
  Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  Result := BoundingRect;
end;

function TMUIThemeServices.HasTransparentParts(Details: TThemedElementDetails): Boolean;
begin
  Result := True; // ?
end;

end.

