{%MainUnit ../gdk2x.pas}
{$IFDEF read_interface_rest}
type
  PXSettingsAction = ^TXSettingsAction;
  TXSettingsAction =  Longint;
  
Const
  XSETTINGS_ACTION_NEW = 0;
  XSETTINGS_ACTION_CHANGED = 1;
  XSETTINGS_ACTION_DELETED = 2;

type
  // this PXSettingsClient needs to be removed when there are xcl.h bindings for fpc
  PXSettingsClient = pointer;

type
  TXSettingsNotifyFunc = procedure (name:Pchar; action:TXSettingsAction; setting:PXSettingsSetting; cb_data:pointer);cdecl;

  TXSettingsWatchFunc = procedure (window:TWindow; is_start:TBool; mask:longint; cb_data:pointer);cdecl;

function xsettings_client_new(display:PDisplay; screen:longint; notify:TXSettingsNotifyFunc; watch:TXSettingsWatchFunc; cb_data:pointer):PXSettingsClient;cdecl;external;
procedure xsettings_client_destroy(client:PXSettingsClient);cdecl;external;
function xsettings_client_process_event(client:PXSettingsClient; xev:PXEvent):TBool;cdecl;external;
function xsettings_client_get_setting(client:PXSettingsClient; name:Pchar; setting:PPXSettingsSetting):TXSettingsResult;cdecl;external;

{$ENDIF read_interface_rest}

