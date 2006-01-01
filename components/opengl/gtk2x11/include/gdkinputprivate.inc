{%MainUnit ../gdk2x.pas}

{$IFDEF read_forward_definitions}
   PPPGdkTimeCoord = ^PPGdkTimeCoord;
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_rest}

type
{ information about a device axis  }
{ reported x resolution  }
{ reported x minimum/maximum values  }
{ calibrated resolution (for aspect ration) - only relative values
     between axes used  }
{ calibrated minimum/maximum values  }
   PGdkAxisInfo = ^TGdkAxisInfo;
   TGdkAxisInfo = record
        xresolution : gint;
        xmin_value : gint;
        xmax_value : gint;
        resolution : gint;
        min_value : gint;
        max_value : gint;
     end;


const
   GDK_INPUT_NUM_EVENTC = 6;
{$ifndef XINPUT_NONE}
{ information about the axes  }
{ Information about XInput device  }
{ minimum key code for device  }
{ true if we need to select a different set of events, but
     can't because this is the core pointer  }
{ Mask of buttons (used for button grabs)  }
{ true if we've claimed the device as active. (used only for XINPUT_GXI)  }
{$endif}
{ !XINPUT_NONE  }

type
   PGdkDevicePrivate = ^TGdkDevicePrivate;
   TGdkDevicePrivate = record
        info : TGdkDevice;
        deviceid : guint32;
        display : PGdkDisplay;
        axes : PGdkAxisInfo;
        xdevice : Pointer; // PXDevice;
        min_keycode : gint;
        buttonpress_type : longint;
        buttonrelease_type : longint;
        keypress_type : longint;
        keyrelease_type : longint;
        motionnotify_type : longint;
        proximityin_type : longint;
        proximityout_type : longint;
        changenotify_type : longint;
        needs_update : gint;
        button_state : gint;
        claimed : gint;
     end;

   P_GdkDeviceClass = ^T_GdkDeviceClass;
   T_GdkDeviceClass = record
        parent_class : TGObjectClass;
     end;

{ gdk window  }
{ Extension mode (GDK_EXTENSION_EVENTS_ALL/CURSOR)  }
{ position relative to root window  }
{ rectangles relative to window of windows obscuring this one  }
{ Is there a pointer grab for this window ?  }
   PGdkInputWindow = ^TGdkInputWindow;
   TGdkInputWindow = record
        window : PGdkWindow;
        mode : TGdkExtensionMode;
        root_x : gint;
        root_y : gint;
        obscuring : PGdkRectangle;
        num_obscuring : gint;
        grabbed : gint;
     end;
     
{ Global data  }
// Private function GDK_IS_CORE(d : pointer) : boolean;

{ Function declarations  }
function gdk_input_window_find(window:PGdkWindow):PGdkInputWindow;cdecl;external;

procedure gdk_input_window_destroy(window:PGdkWindow);cdecl;external;

function _gdk_device_allocate_history(device:PGdkDevice; n_events:gint):PPGdkTimeCoord;cdecl;external;

procedure _gdk_init_input_core;cdecl;external;

{ The following functions are provided by each implementation
   (xfree, gxi, and none)
  }
function _gdk_input_enable_window(window:PGdkWindow; gdkdev:PGdkDevicePrivate):gint;cdecl;external;

function _gdk_input_disable_window(window:PGdkWindow; gdkdev:PGdkDevicePrivate):gint;cdecl;external;

function _gdk_input_window_none_event(event:PGdkEvent; xevent:PXEvent):gint;cdecl;external;

procedure _gdk_input_configure_event(xevent:PXConfigureEvent; window:PGdkWindow);cdecl;external;

procedure _gdk_input_enter_event(xevent:PXCrossingEvent; window:PGdkWindow);cdecl;external;

function _gdk_input_other_event(event:PGdkEvent; xevent:PXEvent; window:PGdkWindow):gint;cdecl;external;

function _gdk_input_grab_pointer(window:PGdkWindow; owner_events:gint; event_mask:TGdkEventMask; confine_to:PGdkWindow; time:guint32):gint;cdecl;external;

procedure _gdk_input_ungrab_pointer(display:PGdkDisplay; time:guint32);cdecl;external;

function _gdk_device_get_history(device:PGdkDevice; window:PGdkWindow; start:guint32; stop:guint32; events:PPPGdkTimeCoord;
           n_events:Pgint):gboolean;cdecl;external;

{$ifndef XINPUT_NONE}

const
   GDK_MAX_DEVICE_CLASSES = 13;

function gdk_input_common_init(display:PGdkDisplay; include_core:gint):gint;cdecl;external;

function gdk_input_find_device(display:PGdkDisplay; id:guint32):PGdkDevicePrivate;cdecl;external;

procedure gdk_input_get_root_relative_geometry(display:PDisplay; w:TWindow; x_ret:Plongint; y_ret:Plongint; width_ret:Plongint; 
            height_ret:Plongint);cdecl;external;

procedure gdk_input_common_find_events(window:PGdkWindow; gdkdev:PGdkDevicePrivate; mask:gint; classes:Pointer {PXEventClass}; num_classes:Plongint);cdecl;external;

procedure gdk_input_common_select_events(window:PGdkWindow; gdkdev:PGdkDevicePrivate);cdecl;external;

function gdk_input_common_other_event(event:PGdkEvent; xevent:PXEvent; input_window:PGdkInputWindow; gdkdev:PGdkDevicePrivate):gint;cdecl;external;

{$endif}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
{ Private
function GDK_IS_CORE(d : pointer) : boolean;
begin
   GDK_IS_CORE:= (PGdkDevice(d)) = _gdk_core_pointer;
end;}

{$ENDIF read_implementation}

