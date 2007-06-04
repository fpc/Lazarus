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
  GtkDef, Gtk2Int, GtkProc, GtkThemes;
  
type
  { TGtk2ThemeServices }

  TGtk2ThemeServices = class(TGtkThemeServices)
  protected
    function GetGtkStyleParams(DC: HDC; Details: TThemedElementDetails): TGtkStyleParams; override;
  public
    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
  end;

implementation

{ TGtk2ThemeServices }

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
        teButton:
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
              BP_RADIOBUTTON:
                begin
                  Result.State := GtkRadioMap[Details.State];
                  if Details.State >= RBS_CHECKEDNORMAL then
                    Result.Shadow := GTK_SHADOW_IN
                  else
                    Result.Shadow := GTK_SHADOW_OUT;
                  Result.Detail := 'radiobutton';
                  Result.Painter := gptOption;
                end;
              BP_CHECKBOX:
                begin
                  Result.State := GtkRadioMap[Details.State];
                  Result.Detail := 'checkbutton';
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
            Result.State := GtkButtonMap[Details.State];
            if Details.State = PBS_PRESSED then
              Result.Shadow := GTK_SHADOW_IN
            else
              Result.Shadow := GTK_SHADOW_OUT;

            Result.IsHot:= Result.State = GTK_STATE_PRELIGHT;

            Result.Detail := 'treeview';
            Result.Painter := gptBox;
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

end.

