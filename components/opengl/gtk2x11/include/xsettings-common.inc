{%MainUnit ../gdk2x.pas}

{$IFDEF read_interface_rest}

{ Renames for GDK inclusion  }

{const
   xsettings_byte_order = _gdk_xsettings_byte_order;
   xsettings_client_destroy = _gdk_xsettings_client_destroy;
   xsettings_client_get_setting = _gdk_xsettings_client_get_setting;
   xsettings_client_new = _gdk_xsettings_client_new;
   xsettings_client_process_event = _gdk_xsettings_client_process_event;
   xsettings_list_copy = _gdk_xsettings_list_copy;
   xsettings_list_delete = _gdk_xsettings_list_delete;
   xsettings_list_free = _gdk_xsettings_list_free;
   xsettings_list_insert = _gdk_xsettings_list_insert;
   xsettings_list_lookup = _gdk_xsettings_list_lookup;
   xsettings_setting_copy = _gdk_xsettings_setting_copy;
   xsettings_setting_equal = _gdk_xsettings_setting_equal;
   xsettings_setting_free = _gdk_xsettings_setting_free;}

type
{ Types of settings possible. Enum values correspond to protocol values. }

   PXSettingsType = ^TXSettingsType;
   TXSettingsType =  Longint;

Const
   XSETTINGS_TYPE_INT = 0;
   XSETTINGS_TYPE_STRING = 1;
   XSETTINGS_TYPE_COLOR = 2;

type
   PXSettingsResult = ^TXSettingsResult;
   TXSettingsResult =  Longint;

Const
   XSETTINGS_SUCCESS = 0;
   XSETTINGS_NO_MEM = 1;
   XSETTINGS_ACCESS = 2;
   XSETTINGS_FAILED = 3;
   XSETTINGS_NO_ENTRY = 4;
   XSETTINGS_DUPLICATE_ENTRY = 5;

type
   PXSettingsBuffer = ^TXSettingsBuffer;
   TXSettingsBuffer = record
        byte_order : char;
        len : size_t;
        data : Pbyte;
        pos : Pbyte;
     end;

   PXSettingsColor = ^TXSettingsColor;
   TXSettingsColor = record
        red : word;
        green : word;
        blue : word;
        alpha : word;
     end;

   PXSettingsSetting = ^TXSettingsSetting;
   TXSettingsSetting = record
        name : Pchar;
        _type : TXSettingsType;
        data : record
            case longint of
               0 : ( v_int : longint );
               1 : ( v_string : Pchar );
               2 : ( v_color : TXSettingsColor );
            end;
        last_change_serial : dword;
     end;
   PPXSettingsSetting = ^PXSettingsSetting;

   PXSettingsList = ^TXSettingsList;
   TXSettingsList = record
        setting : PXSettingsSetting;
        next : PXSettingsList;
     end;
   PPXSettingsList = ^PXSettingsList;


function xsettings_setting_copy(setting:PXSettingsSetting):PXSettingsSetting;cdecl;external;
procedure xsettings_setting_free(setting:PXSettingsSetting);cdecl;external;
function xsettings_setting_equal(setting_a:PXSettingsSetting; setting_b:PXSettingsSetting):longint;cdecl;external;
procedure xsettings_list_free(list:PXSettingsList);cdecl;external;
function xsettings_list_copy(list:PXSettingsList):PXSettingsList;cdecl;external;
function xsettings_list_insert(list:PPXSettingsList; setting:PXSettingsSetting):TXSettingsResult;cdecl;external;

(* Const before type ignored *)
function xsettings_list_lookup(list:PXSettingsList; name:Pchar):PXSettingsSetting;cdecl;external;

(* Const before type ignored *)
function xsettings_list_delete(list:PPXSettingsList; name:Pchar):TXSettingsResult;cdecl;external;

function xsettings_byte_order:char;cdecl;external;

function XSETTINGS_PAD(n,m : longint) : longint;

{$ENDIF read_interface_rest}

{$IFDEF read_implementation}

function XSETTINGS_PAD(n,m : longint) : longint;
  begin
     XSETTINGS_PAD:=((n + m) - 1) and ( not (m - 1));
  end;

{$ENDIF read_implementation}

