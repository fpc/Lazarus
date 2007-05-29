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
  TGtkPainterType =
  (
    gptNone,
    gptDefault,
    gptHLine,
    gptVLine,
    gptShadow,
    gptBox,
    gptFlatBox,
    gptCheck,
    gptOption,
    gptTab,
//    gptSlider,
    gptHandle
//    gptExpander,
//    gptResizeGrip
  );

  TGtkStyleParams = record
    Style      : PGtkStyle;       // paint style
    Painter    : TGtkPainterType; // type of paint handler
    Widget     : PGtkWidget;      // widget
    Window     : PGdkWindow;      // paint window
    Origin     : TPoint;          // offset
    State      : TGtkStateType;   // Style state
    Shadow     : TGtkShadowType;  // Shadow
    Detail     : String;          // Detail (button, checkbox, ...)
    Orientation: TGtkOrientation; // Orientation (horizontal/vertical)
    IsHot      : Boolean;
  end;

  { TGtk2ThemeServices }

  TGtk2ThemeServices = class(TThemeServices)
  private
  protected
    function GdkRectFromRect(R: TRect): TGdkRectangle;
    function GetGtkStyleParams(DC: HDC; Details: TThemedElementDetails): TGtkStyleParams;

    function InitThemes: Boolean; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;

    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); override;
  public
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST; Index: Integer); override;
    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: WideString; R: TRect; Flags, Flags2: Cardinal); override;

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
  end;

implementation

const
  // most common maps
  GtkButtonMap: array[0..6] of TGtkStateType =
  (
{ filter ?          } GTK_STATE_NORMAL,
{ normal            } GTK_STATE_NORMAL,
{ hot               } GTK_STATE_PRELIGHT,
{ pressed           } GTK_STATE_ACTIVE,
{ disabled          } GTK_STATE_INSENSITIVE,
{ defaulted/checked } GTK_STATE_ACTIVE,
{ hot + checked     } GTK_STATE_INSENSITIVE // PRELIGHT IS TOO LIGHT
  );

{ TGtk2ThemeServices }

function TGtk2ThemeServices.GdkRectFromRect(R: TRect): TGdkRectangle;
begin
  with Result, R do
  begin
    x := Left;
    y := Top;
    width := Right-Left;
    height := Bottom-Top;
  end;
end;

function TGtk2ThemeServices.GetGtkStyleParams(DC: HDC;
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

      Result.Painter := gptDefault;
      Result.State := GTK_STATE_NORMAL;
      Result.Detail := '';
      Result.Shadow := GTK_SHADOW_NONE;
      Result.IsHot := False;

      case Details.Element of
        teButton, teHeader:
          begin
            case Details.Part of
              BP_PUSHBUTTON:
                begin
                  Result.State := GtkButtonMap[Details.State];
                  if Details.State = PBS_PRESSED then
                    Result.Shadow := GTK_SHADOW_IN
                  else
                    Result.Shadow := GTK_SHADOW_OUT;
                    
                  Result.IsHot:= Result.State = GTK_STATE_PRELIGHT;

                  Result.Detail := 'button';
                  Result.Painter := gptBox;
                end;
            end;
          end;
        teToolBar:
          begin
            case Details.Part of
              TP_BUTTON:
                begin
                  Result.State := GtkButtonMap[Details.State];
                  if Details.State in [TS_PRESSED, TS_CHECKED, TS_HOTCHECKED] then
                    Result.Shadow := GTK_SHADOW_IN
                  else
                  if Details.State in [TS_HOT] then
                    Result.Shadow := GTK_SHADOW_ETCHED_IN
                  else
                    Result.Shadow := GTK_SHADOW_NONE;

                  Result.IsHot := Details.State in [TS_HOT, TS_HOTCHECKED];

                  Result.Detail := 'togglebutton';
                  if Result.Shadow = GTK_SHADOW_NONE then
                    Result.Painter := gptNone
                  else
                    Result.Painter := gptBox;
                end;
            end;
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
                    Result.Orientation := GTK_ORIENTATION_VERTICAL
                  else
                    Result.Orientation := GTK_ORIENTATION_HORIZONTAL;
                end;
              RP_BAND:
                begin
                  Result.State := GtkButtonMap[Details.State];
                  Result.Shadow := GTK_SHADOW_NONE;
                  Result.Detail := 'paned';
                  Result.Painter := gptFlatBox;
                end;
            end;
          end;
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

procedure TGtk2ThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  ClipArea: TGdkRectangle;
  p_ClipArea: PGdkRectangle;
  StyleParams: TGtkStyleParams;
  R1: TRect;
begin
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
      R1 := R;
      if IsHot then
      begin
        // todo: draw rectanle with selected state
        {gtk_paint_box(
          Style, Window,
          GTK_STATE_SELECTED, Shadow,
          p_ClipArea, Widget, PChar(Detail),
          R1.Left + Origin.x, R1.Top + Origin.y,
          R1.Right - R1.Left, R1.Bottom - R1.Top);
        inflateRect(R1, -1, -1);
        }
      end;

      case Painter of
        gptBox,
        gptDefault: gtk_paint_box(
            Style, Window,
            State, Shadow,
            p_ClipArea, Widget, PChar(Detail),
            R1.Left + Origin.x, R1.Top + Origin.y,
            R1.Right - R1.Left, R1.Bottom - R1.Top);
        gptHLine  : gtk_paint_hline(
            Style, Window,
            State, p_ClipArea,
            Widget, PChar(Detail),
            R1.Left + Origin.x, R1.Right + Origin.x, R1.Top + Origin.y);
        gptVLine  : gtk_paint_vline(
            Style, Window,
            State, p_ClipArea,
            Widget, PChar(Detail),
            R1.Top + Origin.y, R1.Bottom + Origin.y, R1.Left + Origin.x);
        gptShadow : gtk_paint_shadow(
            Style, Window,
            State, Shadow,
            p_ClipArea, Widget, PChar(Detail),
            R1.Left + Origin.x, R1.Top + Origin.y,
            R1.Right - R1.Left, R1.Bottom - R1.Top);
        gptFlatBox: gtk_paint_flat_box(
            Style, Window,
            State, Shadow,
            p_ClipArea, Widget, PChar(Detail),
            R1.Left + Origin.x, R1.Top + Origin.y,
            R1.Right - R1.Left, R1.Bottom - R1.Top);
        gptCheck  : gtk_paint_check(
            Style, Window,
            State, Shadow,
            p_ClipArea, Widget, PChar(Detail),
            R1.Left + Origin.x, R1.Top + Origin.y,
            R1.Right - R1.Left, R1.Bottom - R1.Top);
        gptOption : gtk_paint_option(
            Style, Window,
            State, Shadow,
            p_ClipArea, Widget, PChar(Detail),
            R1.Left + Origin.x, R1.Top + Origin.y,
            R1.Right - R1.Left, R1.Bottom - R1.Top);
        gptTab    : gtk_paint_tab(
            Style, Window,
            State, Shadow,
            p_ClipArea, Widget, PChar(Detail),
            R1.Left + Origin.x, R1.Top + Origin.y,
            R1.Right - R1.Left, R1.Bottom - R1.Top);
        gptHandle : gtk_paint_handle(
            Style, Window,
            State, Shadow,
            p_ClipArea, Widget, PChar(Detail),
            R1.Left + Origin.x, R1.Top + Origin.y,
            R1.Right - R1.Left, R1.Bottom - R1.Top,
            Orientation);
      end;
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

