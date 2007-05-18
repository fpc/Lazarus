unit Gtk2Themes;

{$mode objfpc}{$H+}

interface

uses
  // rtl
  Types, Classes, SysUtils,
  // os
  glib2,  gdk2, gtk2, Pango,
  // lcl
  LCLType, LCLProc, LCLIntf, Graphics, Themes, TmSchema,
  // widgetset
  gtkdef, gtk2int, gtkproc;
  
type
  // todo: more common painter
  TGtkPainter = procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; 
     area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint); cdecl;

  TGtkPainterType =
  (
    gptDefault,
    gptHLine,
    gptVLine,
    gptShadow,
    gptBox,
    gptFlatBox,
    gptCheck,
    gptOption,
    gptTab
//    gptSlider,
//    gptHandle,
//    gptExpander,
//    gptResizeGrip
  );

  TGtkStyleParams = record
    Style  : PGtkStyle;       // paint style
    Painter: TGtkPainterType; // type of paint handler
    Widget : PGtkWidget;      // widget
    Window : PGdkWindow;      // paint window
    Origin : TPoint;          // offset
    State  : TGtkStateType;   // Style state
    Shadow : TGtkShadowType;  // Shadow
    Detail : String;          // Detail (button, checkbox, ...)
  end;

  { TGtk2ThemeServices }

  TGtk2ThemeServices = class(TThemeServices)
  private
  protected
    class function GtkStateFromDetails(Details: TThemedElementDetails): TGtkStateType;
    class function GdkRectFromRect(R: TRect): TGdkRectangle;
    class function GetGtkStyleParams(DC: HDC; Details: TThemedElementDetails): TGtkStyleParams;

    function InitThemes: Boolean; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;

    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); override;
  public

    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); override;
    procedure DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal; AContentRect: PRect); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST; Index: Integer); override;
    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: WideString; R: TRect; Flags, Flags2: Cardinal); override;

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
  end;

  procedure wrap_gtk_paint_hline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType;
     area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint); cdecl;

  procedure wrap_gtk_paint_vline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType;
     area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint); cdecl;

implementation

const
  GtkPainterMap: array[TGtkPainterType] of TGtkPainter =
  (
{ gptDefault    } @gtk_paint_box,          // maybe smth else ??
{ gptHLine      } @wrap_gtk_paint_hline,
{ gptVLine      } @wrap_gtk_paint_vline,
{ gptShadow     } @gtk_paint_shadow,
{ gptBox        } @gtk_paint_box,
{ gptFlatBox    } @gtk_paint_flat_box,
{ gptCheck      } @gtk_paint_check,
{ gptOption     } @gtk_paint_option,
{ gptTab,       } @gtk_paint_tab
// { gptSlider     } @gtk_paint_slider,
// { gptHandle     } @gtk_paint_handle,
// { gptExpander   } @gtk_paint_expander,
// { gptResizeGrip } @gtk_paint_resize_grip
  );

  // most common map
  GtkStatesMap_0: array[0..6] of TGtkStateType =
  (
{ filter ?          } GTK_STATE_NORMAL,
{ normal            } GTK_STATE_NORMAL,
{ hot               } GTK_STATE_PRELIGHT,
{ pressed           } GTK_STATE_ACTIVE,
{ disabled          } GTK_STATE_INSENSITIVE,
{ defaulted/checked } GTK_STATE_SELECTED,
{ hot + checked     } GTK_STATE_SELECTED
  );

procedure wrap_gtk_paint_hline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType;
   area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint); cdecl;
begin
  gtk_paint_hline(style, window, state_type, area, widget, detail, x, width, y);
end;

procedure wrap_gtk_paint_vline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType;
   area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint); cdecl;
begin
  gtk_paint_vline(style, window, state_type, area, widget, detail, y, height, x);
end;

{ TGtk2ThemeServices }

class function TGtk2ThemeServices.GtkStateFromDetails(
  Details: TThemedElementDetails): TGtkStateType;
begin
  Result := GTK_STATE_NORMAL;
  case Details.Element of
    teButton:
      begin
        case Details.Part of
          BP_PUSHBUTTON: Result := GtkStatesMap_0[Details.State];
        end;
      end;
    teToolBar:
      begin
        case Details.Part of
          BP_PUSHBUTTON: Result := GtkStatesMap_0[Details.State];
        end;
      end;
  end;
end;

class function TGtk2ThemeServices.GdkRectFromRect(R: TRect): TGdkRectangle;
begin
  with Result, R do
  begin
    x := Left;
    y := Top;
    width := Right-Left;
    height := Bottom-Top;
  end;
end;

class function TGtk2ThemeServices.GetGtkStyleParams(DC: HDC;
  Details: TThemedElementDetails): TGtkStyleParams;
var
  ClientWidget: PGtkWidget;
begin
  Result.Style := nil;
  
  if GTK2WidgetSet.IsValidDC(DC) then
    with TDeviceContext(DC) do
    begin
      Result.Widget := DCWidget;
      ClientWidget := GetFixedWidget(Result.Widget);
      if ClientWidget <> nil then
        Result.Widget := ClientWidget;
      Result.Window := Drawable;
      Result.Origin := GetDCOffset(TDeviceContext(DC));
      Result.Style := gtk_widget_get_style(Result.Widget);
      if Result.Style = nil then
        Result.Style := gtk_widget_get_default_style();
      Result.State := GtkStateFromDetails(Details);
      
      // todo: shadow customization
      Result.Shadow := GTK_SHADOW_IN;
      
      // todo: detail customization
      Result.Detail := 'button';
      
      case Details.Element of
        teButton : Result.Painter := gptBox;
        teToolBar: Result.Painter := gptFlatBox;
      else
        Result.Painter := gptDefault;
      end;
    end;
end;

function TGtk2ThemeServices.InitThemes: Boolean;
begin
  Result := True;
end;

function TGtk2ThemeServices.UseThemes: Boolean;
begin
  Result := True;
end;

function TGtk2ThemeServices.ThemedControlsEnabled: Boolean;
begin
  Result := True;
end;

function TGtk2ThemeServices.ContentRect(DC: HDC;
  Details: TThemedElementDetails; BoundingRect: TRect): TRect;
var
  StyleParams: TGtkStyleParams;
begin
  Result := BoundingRect;
  StyleParams := GetGtkStyleParams(DC, Details);
  if StyleParams.Style <> nil then
    InflateRect(Result,
      -StyleParams.Style^.xthickness,
      -StyleParams.Style^.ythickness);
end;

procedure TGtk2ThemeServices.DrawEdge(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
  AContentRect: PRect);
begin

end;

procedure TGtk2ThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  ClipArea: TGdkRectangle;
  p_ClipArea: PGdkRectangle;
  StyleParams: TGtkStyleParams;
begin
  // todo:
  // - gtk_paint_... customization
  // - draw focus when needed
  StyleParams := GetGtkStyleParams(DC, Details);
  if StyleParams.Style <> nil then
  begin
    if ClipRect <> nil then
    begin
      ClipArea := GdkRectFromRect(ClipRect^);
      inc(ClipArea.x, StyleParams.Origin.x);
      inc(ClipArea.y, StyleParams.Origin.y);
      p_ClipArea := @ClipArea;
    end
    else
      p_ClipArea := nil;

    with StyleParams do
    begin
      GtkPainterMap[Painter](
          Style, Window,
          State, Shadow,
          p_ClipArea, Widget, PChar(Detail),
          R.Left + Origin.x, R.Top + Origin.y,
          R.Right - R.Left, R.Bottom - R.Top);

     {  // for test
      gtk_paint_focus(Style, Window,
        State,
        p_ClipArea, Widget, 'button',
        R.Left + Origin.x, R.Top + Origin.y,
        R.Right - R.Left, R.Bottom - R.Top);}
    end;
  end;
end;

procedure TGtk2ThemeServices.DrawIcon(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST;
  Index: Integer);
begin

end;

function TGtk2ThemeServices.HasTransparentParts(Details: TThemedElementDetails): Boolean;
begin
  Result := True; // ?
end;

procedure TGtk2ThemeServices.InternalDrawParentBackground(Window: HWND;
  Target: HDC; Bounds: PRect);
begin
  // ?
end;

procedure TGtk2ThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: WideString; R: TRect; Flags, Flags2: Cardinal);
var
  StyleParams: TGtkStyleParams;
  P: PChar;
  tmpRect: TRect;
begin
  StyleParams := GetGtkStyleParams(DC, Details);
  if StyleParams.Style <> nil then
    with StyleParams do
    begin
      P := PChar(String(S));
      tmpRect := R;
      Gtk2Widgetset.DrawText(DC, P, Length(S), tmpRect, Flags);
      // TODO: parse flags
      //gtk_draw_string(Style, Window, State, R.Left + Origin.x, R.Top + Origin.y, P);
    end;
end;

end.

