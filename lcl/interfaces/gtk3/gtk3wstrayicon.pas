{
 *****************************************************************************
 *                               gtk3WSTrayIcon.pas                          *
 *                               ------------------                          *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

A unit that uses LibAppIndicator3-1 or libAyatana-AppIndicator3-1 to display a 
TrayIcon in GTK3.   Based on a GTK2 version by Anthony Walter, now works with 
many common Linux systems "out of the box" or with the addition of one of the
above mentioned libraries and possibly gnome-shell-extension-appindicator.

See Wiki for details and Limitations (Menu only, one Icon only....)
Also refer to discussion in ../gtk2/UnityWSCtrls.pas
}


unit Gtk3WSTrayIcon;

interface

{$mode delphi}
uses
  GLib2, LazGtk3, LazGdkPixbuf2, gtk3widgets,
  Classes, SysUtils, ayatana_appindicator,
  Graphics, Controls, Forms, ExtCtrls, WSExtCtrls, LCLType, LazUTF8,
  FileUtil;

type
  TGtk3WSTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function GetPosition(const {%H-}ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

implementation

{X$define DEBUGAPPIND}

uses gtk3objects;     // TGtk3Image


var
  IconThemePath : string;                    // We must write our icon to a file.

{ TAppIndTrayIconHandle }

type
  TAppIndTrayIconHandle = class
  private
    FTrayIcon: TCustomTrayIcon;
    FName: string;
    FIconName: string;
  public
    constructor Create(TrayIcon: TCustomTrayIcon);
    destructor Destroy; override;
    procedure Update;
  end;


var
  GlobalAppIndicator: PAppIndicator;
  GlobalIcon: Pointer;
  GlobalIconPath: string;

constructor TAppIndTrayIconHandle.Create(TrayIcon: TCustomTrayIcon);
var
  NewIcon: Pointer;
begin
  inherited Create;
  FTrayIcon := TrayIcon;
  FName := 'app-' + IntToHex(IntPtr(Application), SizeOf(IntPtr) * 2);
  NewIcon := {%H-}Pointer(TGtk3Image(FTrayIcon.Icon.Handle).handle);
  if NewIcon = nil then
    NewIcon := {%H-}Pointer(Application.Icon.Handle);
  if NewIcon <> GlobalIcon then
  begin
    GlobalIcon := NewIcon;
    ForceDirectories(IconThemePath);
    FIconName := FName + '-' + IntToHex({%H-}IntPtr(GlobalIcon), SizeOf(GlobalIcon) * 2);
    if FileExists(GlobalIconPath) then
      DeleteFile(GlobalIconPath);
    GlobalIconPath := IconThemePath + FIconName + '.' + IconType;
    gdk_pixbuf_save(GlobalIcon, PChar(GlobalIconPath), IconType, nil, [nil]);
    if GlobalAppIndicator <> nil then
        app_indicator_set_icon(GlobalAppIndicator, PChar(FIconName));
  end
  else
    FIconName := FName + '-' + IntToHex({%H-}IntPtr(GlobalIcon), SizeOf(GlobalIcon) * 2);
  { Only the first created AppIndicator is functional }
  if GlobalAppIndicator = nil then
    { It seems that icons can only come from files :( }
    GlobalAppIndicator := app_indicator_new_with_path(PChar(FName), PChar(FIconName),
      APP_INDICATOR_CATEGORY_APPLICATION_STATUS, PChar(IconThemePath));
  Update;
  {$ifdef DEBUGAPPIND}
  case app_indicator_get_status(GlobalAppIndicator) of
	      APP_INDICATOR_STATUS_PASSIVE : writeln('AppInd statis is Passive');
	      APP_INDICATOR_STATUS_ACTIVE : writeln('AppInd status is Active');
	      APP_INDICATOR_STATUS_ATTENTION  : writeln('AppInd is Attention');
  else writeln('AppInd status is unknown');
  end;
  {$endif}
end;

destructor TAppIndTrayIconHandle.Destroy;
begin
  { Hide the global AppIndicator }
  app_indicator_set_status(GlobalAppIndicator, APP_INDICATOR_STATUS_PASSIVE);
  inherited Destroy;
end;

procedure TAppIndTrayIconHandle.Update;
var
  NewIcon: Pointer;
begin
  NewIcon := {%H-}Pointer(TGTK3Image(FTrayIcon.Icon.Handle).Handle);
  if NewIcon = nil then
    NewIcon := {%H-}Pointer(Application.Icon.Handle);
  if NewIcon <> GlobalIcon then
  begin
    GlobalIcon := NewIcon;
    FIconName := FName + '-' + IntToHex({%H-}IntPtr(GlobalIcon), SizeOf(GlobalIcon) * 2);
    ForceDirectories(IconThemePath);
    if FileExists(GlobalIconPath) then
      DeleteFile(GlobalIconPath);
    GlobalIconPath := IconThemePath + FIconName + '.' + IconType;
    gdk_pixbuf_save(GlobalIcon, PChar(GlobalIconPath), IconType, nil, [nil]);
    { Again it seems that icons can only come from files }
    app_indicator_set_icon(GlobalAppIndicator, PChar(FIconName));
  end;
  { It seems to me you can only set the menu once for an AppIndicator }
  if (app_indicator_get_menu(GlobalAppIndicator) = nil) and (FTrayIcon.PopUpMenu <> nil) then
    //app_indicator_set_menu(GlobalAppIndicator, {%H-}PGtkMenu(FTrayIcon.PopUpMenu.Handle));
    app_indicator_set_menu(GlobalAppIndicator, {%H-}PGtkMenu(TGTK3Menu(FTrayIcon.PopUpMenu.Handle).Widget));
  app_indicator_set_status(GlobalAppIndicator, APP_INDICATOR_STATUS_ACTIVE);
end;

{ TAppIndWSCustomTrayIcon }

class function TGtk3WSTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  T: TAppIndTrayIconHandle;
begin
  if ATrayIcon.Handle <> 0 then
  begin
    T := TAppIndTrayIconHandle(ATrayIcon.Handle);
    ATrayIcon.Handle := 0;
    T.Free;
  end;
  Result := True;
end;

class function TGtk3WSTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  T: TAppIndTrayIconHandle;
begin
  if ATrayIcon.Handle = 0 then
  begin
    T := TAppIndTrayIconHandle.Create(ATrayIcon);
    ATrayIcon.Handle := HWND(T);
  end;
  Result := True;
end;

class procedure TGtk3WSTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
var
  T: TAppIndTrayIconHandle;
begin
  if ATrayIcon.Handle <> 0 then
  begin
    T := TAppIndTrayIconHandle(ATrayIcon.Handle);
    T.Update;
  end;
end;

class function TGtk3WSTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
end;

{ AppIndicatorInit }

var
  Loaded: Boolean;
  Initialized: Boolean;

function Gtk3AppIndicatorInit: Boolean;
var
  Module: HModule;

  function TryLoad(const ProcName: string; var Proc: Pointer): Boolean;
  begin
    Proc := GetProcAddress(Module, ProcName);
    Result := Proc <> nil;
  end;

begin
  Result := False;
  if Loaded then
    Exit(Initialized);
  Loaded := True;
  if Initialized then
    Exit(True);
  Module := LoadLibrary(libappindicator_3);
  if Module = 0 then begin
    Module := LoadLibrary(LibAyatanaAppIndicator);
    if Module = 0 then begin                    // Sorry, no TrayIcon !
      {$ifdef DEBUGAPPIND}
      writeln('Failed to load an appindicator library');{$endif}
      Exit;
    end
    {$ifdef DEBUGAPPIND} else writeln('Loaded ' + LibAyatanaAppIndicator){$endif};
  end {$ifdef DEBUGAPPIND} else writeln('Loaded ' + libappindicator_3){$endif};
  Result :=
    TryLoad('app_indicator_get_type', @app_indicator_get_type) and
    TryLoad('app_indicator_new', @app_indicator_new) and
    TryLoad('app_indicator_new_with_path', @app_indicator_new_with_path) and
    TryLoad('app_indicator_set_status', @app_indicator_set_status) and
    TryLoad('app_indicator_set_attention_icon', @app_indicator_set_attention_icon) and
    TryLoad('app_indicator_set_menu', @app_indicator_set_menu) and
    TryLoad('app_indicator_set_icon', @app_indicator_set_icon) and
    TryLoad('app_indicator_set_label', @app_indicator_set_label) and
    TryLoad('app_indicator_set_icon_theme_path', @app_indicator_set_icon_theme_path) and
    TryLoad('app_indicator_set_ordering_index', @app_indicator_set_ordering_index) and
    TryLoad('app_indicator_get_id', @app_indicator_get_id) and
    TryLoad('app_indicator_get_category', @app_indicator_get_category) and
    TryLoad('app_indicator_get_status', @app_indicator_get_status) and
    TryLoad('app_indicator_get_icon', @app_indicator_get_icon) and
    TryLoad('app_indicator_get_icon_theme_path', @app_indicator_get_icon_theme_path) and
    TryLoad('app_indicator_get_attention_icon', @app_indicator_get_attention_icon) and
    TryLoad('app_indicator_get_menu', @app_indicator_get_menu) and
    TryLoad('app_indicator_get_label', @app_indicator_get_label) and
    TryLoad('app_indicator_get_label_guide', @app_indicator_get_label_guide) and
    TryLoad('app_indicator_get_ordering_index', @app_indicator_get_ordering_index);
  Initialized := Result;
end;

initialization
  GlobalAppIndicator := nil;
  GlobalIconPath := '';
  IconThemePath := '/tmp/appindicators-' + GetEnvironmentVariable('USER') + '/';

finalization
  if FileExists(GlobalIconPath) then
    DeleteFile(GlobalIconPath);
  if GlobalAppIndicator <> nil then
    g_object_unref(GlobalAppIndicator);
end.
