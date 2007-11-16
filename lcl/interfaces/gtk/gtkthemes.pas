unit GtkThemes;

{$mode objfpc}{$H+}

interface

uses
  // rtl
  Types, Classes, SysUtils,
  // os
{$ifdef gtk1}
  glib,  gdk, gtk,
{$else}
  glib2,  gdk2, gtk2, Pango,
{$endif}
  // lcl
  InterfaceBase, LCLType, LCLProc, LCLIntf, Graphics, Themes, TmSchema,
  // widgetset
  gtkdef, gtkint, gtkproc, gtkglobals;
  
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
    gptSlider,
    gptHandle,
    gptExpander,
    gptResizeGrip,
    gptFocus,
    gptArrow
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
    ArrowType  : TGtkArrowType;   // type of arrow
    Fill       : Boolean;         // fill inside area
{$ifdef gtk2}
    Expander   : TGtkExpanderStyle; // treeview expander
    Edge       : TGdkWindowEdge;
{$endif}
    IsHot      : Boolean;
  end;

  { TGtk1ThemeServices }

  { TGtkThemeServices }

  TGtkThemeServices = class(TThemeServices)
  private
  protected
    function GdkRectFromRect(R: TRect): TGdkRectangle;
    function GetParamsCount(Details: TThemedElementDetails): Integer;
    function GetGtkStyleParams(DC: HDC; Details: TThemedElementDetails; AIndex: Integer): TGtkStyleParams; virtual;

    function InitThemes: Boolean; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;

    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); override;
    function GetBaseDetailsSize(Details: TThemedElementDetails): Integer;
  public
    function GetDetailSize(Details: TThemedElementDetails): Integer; override;
    
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST; Index: Integer); override;
    procedure DrawText(DC: HDC; Details: TThemedElementDetails; const S: String; R: TRect; Flags, Flags2: Cardinal); override;

    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
  end;

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
{ hot + checked     } GTK_STATE_ACTIVE
  );
  GtkRadioCheckBoxMap: array[0..12] of TGtkStateType =
  (
{ Filler            } GTK_STATE_NORMAL,
{ UNCHECKEDNORMAL   } GTK_STATE_NORMAL,
{ UNCHECKEDHOT      } GTK_STATE_PRELIGHT,
{ UNCHECKEDPRESSED  } GTK_STATE_ACTIVE,
{ UNCHECKEDDISABLED } GTK_STATE_INSENSITIVE,
{ CHECKEDNORMAL     } GTK_STATE_NORMAL,
{ CHECKEDHOT        } GTK_STATE_PRELIGHT,
{ CHECKEDPRESSED    } GTK_STATE_ACTIVE,
{ CHECKEDDISABLED   } GTK_STATE_INSENSITIVE,
{ MIXEDNORMAL       } GTK_STATE_NORMAL,
{ MIXEDHOT          } GTK_STATE_PRELIGHT,
{ MIXEDPRESSED      } GTK_STATE_ACTIVE,
{ MIXEDDISABLED     } GTK_STATE_INSENSITIVE
  );

  
implementation

{ TGtkThemeServices }

function TGtkThemeServices.GdkRectFromRect(R: TRect): TGdkRectangle;
begin
  with Result, R do
  begin
    x := Left;
    y := Top;
    width := Right-Left;
    height := Bottom-Top;
  end;
end;

function TGtkThemeServices.GetParamsCount(Details: TThemedElementDetails
  ): Integer;
begin
  Result := 1;
  if (Details.Element = teToolBar) and (Details.Part = TP_SPLITBUTTONDROPDOWN) then
    inc(Result); // + Arrow
end;

function TGtkThemeServices.GetGtkStyleParams(DC: HDC;
  Details: TThemedElementDetails; AIndex: Integer): TGtkStyleParams;
var
  ClientWidget: PGtkWidget;
begin
  Result.Style := nil;
  
  if GTKWidgetSet.IsValidDC(DC) then
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
      Result.ArrowType := GTK_ARROW_UP;
      Result.Fill := False;
      Result.IsHot := False;

      case Details.Element of
        teButton:
          begin
            case Details.Part of
              BP_PUSHBUTTON:
                begin
                  Result.Widget := GetStyleWidget(lgsButton);
                  Result.State := GtkButtonMap[Details.State];
                  if Details.State = PBS_PRESSED then
                    Result.Shadow := GTK_SHADOW_IN
                  else
                    Result.Shadow := GTK_SHADOW_OUT;
                    
                  Result.IsHot:= Result.State = GTK_STATE_PRELIGHT;

                  Result.Detail := 'button';
                  Result.Painter := gptBox;
                end;
              BP_RADIOBUTTON:
                begin
                  Result.Widget := GetStyleWidget(lgsRadiobutton);
                  Result.State := GtkRadioCheckBoxMap[Details.State];
                  if Details.State >= RBS_CHECKEDNORMAL then
                    Result.Shadow := GTK_SHADOW_IN
                  else
                    Result.Shadow := GTK_SHADOW_OUT;
                  Result.Detail := 'radiobutton';
                  Result.Painter := gptOption;
                end;
              BP_CHECKBOX:
                begin
                  Result.Widget := GetStyleWidget(lgsCheckbox);
                  Result.State := GtkRadioCheckBoxMap[Details.State];
                  Result.Detail := 'checkbutton';
                  if Details.State >= CBS_MIXEDNORMAL then
                    result.Shadow := GTK_SHADOW_ETCHED_IN
                  else
                  if Details.State >= CBS_CHECKEDNORMAL then
                    Result.Shadow := GTK_SHADOW_IN
                  else
                    Result.Shadow := GTK_SHADOW_OUT;
                  Result.Painter := gptCheck;
                end;
            end;
          end;
        teHeader:
          begin
            Result.Widget := GetStyleWidget(lgsButton);
            Result.State := GtkButtonMap[Details.State];
            if Details.State = PBS_PRESSED then
              Result.Shadow := GTK_SHADOW_IN
            else
              Result.Shadow := GTK_SHADOW_OUT;

            Result.IsHot:= Result.State = GTK_STATE_PRELIGHT;

            Result.Detail := 'button';
            Result.Painter := gptBox;
          end;
        teToolBar:
          begin
            case Details.Part of
              TP_BUTTON,
              TP_DROPDOWNBUTTON,
              TP_SPLITBUTTON,
              TP_SPLITBUTTONDROPDOWN:
                begin
                  if (Details.Part = TP_SPLITBUTTONDROPDOWN) and (AIndex = 1) then
                  begin
                    Result.Detail := 'arrow';
                    Result.ArrowType := GTK_ARROW_DOWN;
                    Result.Fill := True;
                    Result.Painter := gptArrow;
                  end
                  else
                  begin
                    Result.Widget := GetStyleWidget(lgsToolButton);
                    Result.State := GtkButtonMap[Details.State];
                    if Details.State in [TS_PRESSED, TS_CHECKED, TS_HOTCHECKED] then
                      Result.Shadow := GTK_SHADOW_IN
                    else
                    if Details.State in [TS_HOT] then
                      Result.Shadow := GTK_SHADOW_ETCHED_IN
                    else
                      Result.Shadow := GTK_SHADOW_NONE;

                    Result.IsHot := Details.State in [TS_HOT, TS_HOTCHECKED];

                    Result.Detail := 'button';
                    if Result.Shadow = GTK_SHADOW_NONE then
                      Result.Painter := gptNone
                    else
                      Result.Painter := gptBox;
                  end;
                end;
              TP_SEPARATOR,
              TP_SEPARATORVERT:
                begin
                  Result.State := GTK_STATE_NORMAL;
                  Result.Shadow := GTK_SHADOW_NONE;
                  Result.Detail := 'toolbar';
                  if Details.Part = TP_SEPARATOR then
                    Result.Painter := gptVLine
                  else
                    Result.Painter := gptHLine;
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
{ This code has problems with some (is not most) of gtk1 themes.
  But at least Ubuntu >= 6.10 works fine. So it is commented out and switched
  to alternate splitter painting}

                  if Details.Part = RP_GRIPPER then
                  begin
                    Result.Detail := 'hpaned';
                    Result.Widget := GetStyleWidget(lgsHorizontalPaned);
                  end
                  else
                  begin
                    Result.Detail := 'vpaned';
                    Result.Widget := GetStyleWidget(lgsVerticalPaned);
                  end;
                  Result.Painter := gptBox;

{                  Result.Detail := 'paned';
                  Result.Painter := gptHandle;
                  if Details.Part = RP_GRIPPER then
                    Result.Orientation := GTK_ORIENTATION_VERTICAL
                  else
                    Result.Orientation := GTK_ORIENTATION_HORIZONTAL;}
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

function TGtkThemeServices.InitThemes: Boolean;
begin
  Result := True;
end;

function TGtkThemeServices.UseThemes: Boolean;
begin
  Result := True;
end;

function TGtkThemeServices.ThemedControlsEnabled: Boolean;
begin
  Result := True;
end;

function TGtkThemeServices.ContentRect(DC: HDC;
  Details: TThemedElementDetails; BoundingRect: TRect): TRect;
var
  StyleParams: TGtkStyleParams;
begin
  Result := BoundingRect;
  StyleParams := GetGtkStyleParams(DC, Details, 0);
  if StyleParams.Style <> nil then
    InflateRect(Result,
      -StyleParams.Style^.{$ifndef gtk2}klass^.{$endif}xthickness,
      -StyleParams.Style^.{$ifndef gtk2}klass^.{$endif}ythickness);
end;

procedure TGtkThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  Area: TGdkRectangle;
  StyleParams: TGtkStyleParams;
  i: integer;
begin
  for i := 0 to GetParamsCount(Details) - 1 do
  begin
    StyleParams := GetGtkStyleParams(DC, Details, i);
    if StyleParams.Style <> nil then
    begin
      if ClipRect <> nil then
        Area := GdkRectFromRect(ClipRect^)
      else
        Area := GdkRectFromRect(R);

      // move to origin
      inc(Area.x, StyleParams.Origin.x);
      inc(Area.y, StyleParams.Origin.y);
      
      with StyleParams do
      begin
        case Painter of
          gptBox,
          gptDefault: gtk_paint_box(
              Style, Window,
              State, Shadow,
              @Area, Widget, PChar(Detail),
              Area.x, Area.y,
              Area.Width, Area.Height);
          gptHLine  : gtk_paint_hline(
              Style, Window,
              State, @Area,
              Widget, PChar(Detail),
              Area.x, Area.x + Area.Width, Area.y);
          gptVLine  : gtk_paint_vline(
              Style, Window,
              State, @Area,
              Widget, PChar(Detail),
              Area.y, Area.y + Area.Height, Area.x);
          gptShadow : gtk_paint_shadow(
              Style, Window,
              State, Shadow,
              @Area, Widget, PChar(Detail),
              Area.x, Area.y,
              Area.Width, Area.Height);
          gptFlatBox: gtk_paint_flat_box(
              Style, Window,
              State, Shadow,
              @Area, Widget, PChar(Detail),
              Area.x, Area.y,
              Area.Width, Area.Height);
          gptCheck  : gtk_paint_check(
              Style, Window,
              State, Shadow,
              @Area, Widget, PChar(Detail),
              Area.x, Area.y,
              Area.Width, Area.Height);
          gptOption : gtk_paint_option(
              Style, Window,
              State, Shadow,
              @Area, Widget, PChar(Detail),
              Area.x, Area.y,
              Area.Width, Area.Height);
          gptTab    : gtk_paint_tab(
              Style, Window,
              State, Shadow,
              @Area, Widget, PChar(Detail),
              Area.x, Area.y,
              Area.Width, Area.Height);
          gptSlider : gtk_paint_slider(
              Style, Window,
              State, Shadow,
              @Area, Widget, PChar(Detail),
              Area.x, Area.y,
              Area.Width, Area.Height,
              Orientation);
          gptHandle : gtk_paint_handle(
              Style, Window,
              State, Shadow,
              @Area, Widget, PChar(Detail),
              Area.x, Area.y,
              Area.Width, Area.Height,
              Orientation);
  {$ifdef gtk2}
          gptExpander: gtk_paint_expander(
              Style, Window, State,
              @Area, Widget, PChar(Detail),
              Area.x, Area.y,
              Expander);
          gptResizeGrip: gtk_paint_resize_grip(
              Style, Window, State,
              @Area, Widget,
              PChar(Detail), Edge,
              Area.x, Area.y,
              Area.Width, Area.Height);
  {$endif}
          gptFocus : gtk_paint_focus(
              Style, Window, {$ifdef gtk2}State,{$endif}
              @Area, Widget, PChar(Detail),
              Area.x, Area.y,
              Area.Width, Area.Height);
          gptArrow: gtk_paint_arrow(
             Style, Window,
             State, Shadow,
             @Area, Widget, PChar(Detail),
             ArrowType, Fill,
             Area.x, Area.y, Area.width, Area.height
           );
        end;
      end;
    end;
  end;
end;

procedure TGtkThemeServices.DrawIcon(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; himl: HIMAGELIST;
  Index: Integer);
begin

end;

function TGtkThemeServices.HasTransparentParts(Details: TThemedElementDetails): Boolean;
begin
  Result := True; // ?
end;

procedure TGtkThemeServices.InternalDrawParentBackground(Window: HWND;
  Target: HDC; Bounds: PRect);
begin
  // ?
end;

function TGtkThemeServices.GetBaseDetailsSize(Details: TThemedElementDetails): Integer;
begin
  Result := inherited GetDetailSize(Details);
end;

function TGtkThemeServices.GetDetailSize(Details: TThemedElementDetails): Integer;
begin
  case Details.Element of
    teRebar :
      if Details.Part in [RP_GRIPPER, RP_GRIPPERVERT] then
        Result := -1;
    else
      Result := GetBaseDetailsSize(Details);
  end;
end;

procedure TGtkThemeServices.DrawText(DC: HDC; Details: TThemedElementDetails;
  const S: String; R: TRect; Flags, Flags2: Cardinal);
var
  StyleParams: TGtkStyleParams;
  P: PChar;
  tmpRect: TRect;
begin
  StyleParams := GetGtkStyleParams(DC, Details, 0);
  if StyleParams.Style <> nil then
    with StyleParams do
    begin
      P := PChar(S);
      tmpRect := R;
      Widgetset.DrawText(DC, P, Length(S), tmpRect, Flags);
      // TODO: parse flags
      //gtk_draw_string(Style, Window, State, R.Left + Origin.x, R.Top + Origin.y, P);
    end;
end;

end.

