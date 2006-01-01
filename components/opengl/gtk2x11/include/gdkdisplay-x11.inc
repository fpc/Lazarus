{%MainUnit ../gdk2x.pas}

{$IFDEF read_interface_rest}
{ Keyboard related information  }
{ Whether we were able to turn on detectable-autorepeat using
     XkbSetDetectableAutorepeat. If FALSE, we'll fall back
     to checking the next event with XPending().  }
{ Information about current pointer and keyboard grabs held by this
     client. If gdk_pointer_xgrab_window or gdk_keyboard_xgrab_window
     window is NULL, then the other associated fields are ignored
    }
{ drag and drop information  }
{ data needed for MOTIF DnD  }
{ Mapping to/from virtual atoms  }
{ Session Management leader window see ICCCM  }
{ list of filters for client messages  }
{ X ID hashtable  }
{ translation queue  }
{ Input device  }
{ input GdkDevice list  }
{ input GdkWindow list  }
{ information about network port and host for gxid daemon  }

type
   PGdkDisplayX11 = ^TGdkDisplayX11;
   TGdkDisplayX11 = record
        parent_instance : TGdkDisplay;
        xdisplay : PDisplay;
        default_screen : PGdkScreen;
        screens : ^PGdkScreen;
        grab_count : gint;
        xkb_event_type : gint;
        use_xkb : gboolean;
        have_xkb_autorepeat : gboolean;
        keymap : PGdkKeymap;
        keymap_serial : guint;
        use_xshm : gboolean;
        have_shm_pixmaps : gboolean;
        have_shape : gint;
        pointer_xgrab_window : PGdkWindowObject;
        pointer_xgrab_serial : gulong;
        pointer_xgrab_owner_events : gboolean;
        keyboard_xgrab_window : PGdkWindowObject;
        keyboard_xgrab_serial : gulong;
        keyboard_xgrab_owner_events : gboolean;
        current_dest_drag : PGdkDragContext;
        motif_drag_window : TWindow;
        motif_drag_gdk_window : PGdkWindow;
        motif_target_lists : ^PGList;
        motif_n_target_lists : gint;
        atom_from_virtual : PGHashTable;
        atom_to_virtual : PGHashTable;
        leader_window : TWindow;
        client_filters : PGList;
        xid_ht : PGHashTable;
        translate_queue : PGQueue;
        input_devices : PGList;
        input_windows : PGList;
        input_ignore_core : gint;
        input_gxid_host : Pgchar;
        input_gxid_port : gint;
        use_xft : gint;
     end;

   PGdkDisplayX11Class = ^TGdkDisplayX11Class;
   TGdkDisplayX11Class = record
        parent_class : TGdkDisplayClass;
     end;


// Private function GDK_TYPE_DISPLAY_X11 : GType;
function GDK_DISPLAY_X11(obj : pointer) : PGdkDisplayX11;
function GDK_DISPLAY_X11_CLASS(klass : pointer) : PGdkDisplayX11Class;
// Private function GDK_IS_DISPLAY_X11(obj : pointer) : boolean;
// Private function GDK_IS_DISPLAY_X11_CLASS(klass : pointer) : boolean;
// Private function GDK_DISPLAY_X11_GET_CLASS(obj : pointer) : PGdkDisplayX11Class;
// Private function _gdk_display_x11_get_type:GType;cdecl;external gdklib;
// Private function _gdk_x11_display_screen_for_xrootwin(display:PGdkDisplay; xrootwin:TWindow):PGdkScreen;cdecl;external;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

{ Private
function GDK_TYPE_DISPLAY_X11 : GType;
begin
   GDK_TYPE_DISPLAY_X11:=_gdk_display_x11_get_type;
end;}

function GDK_DISPLAY_X11(obj : pointer) : PGdkDisplayX11;
begin
   GDK_DISPLAY_X11:=PGdkDisplayX11(obj);
   //GDK_DISPLAY_X11:=PGdkDisplayX11(G_TYPE_CHECK_INSTANCE_CAST(obj,GDK_TYPE_DISPLAY_X11));
end;

function GDK_DISPLAY_X11_CLASS(klass : pointer) : PGdkDisplayX11Class;
begin
   GDK_DISPLAY_X11_CLASS:=PGdkDisplayX11Class(klass);
   //GDK_DISPLAY_X11_CLASS:=PGdkDisplayX11Class(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_DISPLAY_X11));
end;

{ Private
function GDK_IS_DISPLAY_X11(obj : pointer) : boolean;
begin
   GDK_IS_DISPLAY_X11:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GDK_TYPE_DISPLAY_X11);
end;

function GDK_IS_DISPLAY_X11_CLASS(klass : pointer) : boolean;
begin
   GDK_IS_DISPLAY_X11_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_DISPLAY_X11);
end;

function GDK_DISPLAY_X11_GET_CLASS(obj : pointer) : PGdkDisplayX11Class;
begin
   GDK_DISPLAY_X11_GET_CLASS:=PGdkDisplayX11Class(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_DISPLAY_X11));
end;}

{$ENDIF read_implementation}

