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
  end;

implementation

{ TGtk2ThemeServices }

function TGtk2ThemeServices.GetGtkStyleParams(DC: HDC;
  Details: TThemedElementDetails): TGtkStyleParams;
var
  ClientWidget: PGtkWidget;
begin
  Result := inherited GetGtkStyleParams(DC, Details);
  
  // override some styles
  if Result.Style <> nil then
    case Details.Element of
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

end.

