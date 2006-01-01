{%MainUnit ../gdk2x.pas}

{$IFDEF read_interface_rest}

{ Visual Part  }
{ Colormap Part  }
{ X settings  }
{ Xinerama  }

type
   PGdkScreenX11 = ^TGdkScreenX11;
   TGdkScreenX11 = record
        parent_instance : TGdkScreen;
        display : PGdkDisplay;
        xdisplay : PDisplay;
        xscreen : PScreen;
        screen_num : gint;
        xroot_window : TWindow;
        root_window : PGdkWindow;
        wmspec_check_window : TWindow;
        system_visual : PGdkVisualPrivate;
        visuals : ^PGdkVisualPrivate;
        nvisuals : gint;
        available_depths : array[0..6] of gint;
        navailable_depths : gint;
        available_types : array[0..5] of TGdkVisualType;
        navailable_types : gint;
        visual_hash : PGHashTable;
        default_colormap : PGdkColormap;
        system_colormap : PGdkColormap;
        xsettings_client : PXSettingsClient;
        num_monitors : gint;
        monitors : PGdkRectangle;
     end;

   PGdkScreenX11Class = ^TGdkScreenX11Class;
   TGdkScreenX11Class = record
        parent_class : TGdkScreenClass;
     end;


// Private function GDK_TYPE_SCREEN_X11 : GType;
function GDK_SCREEN_X11(obj : pointer) : PGdkScreenX11;
function GDK_SCREEN_X11_CLASS(klass : pointer) : PGdkScreenX11Class;
// Private function GDK_IS_SCREEN_X11(obj : pointer) : boolean;
// Private function GDK_IS_SCREEN_X11_CLASS(klass : pointer) : boolean;
// Private function GDK_SCREEN_X11_GET_CLASS(obj : pointer) : PGdkScreenX11Class;


// Private function _gdk_screen_x11_get_type:GType;cdecl;external;

function _gdk_x11_screen_new(display:PGdkDisplay; screen_number:gint):PGdkScreen;cdecl;external;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

{ Private
function GDK_TYPE_SCREEN_X11 : GType;
begin
   GDK_TYPE_SCREEN_X11:=_gdk_screen_x11_get_type;
end;}

function GDK_SCREEN_X11(obj : pointer) : PGdkScreenX11;
begin
   GDK_SCREEN_X11:=PGdkScreenX11(obj);
   //GDK_SCREEN_X11:=PGdkScreenX11(G_TYPE_CHECK_INSTANCE_CAST(obj,GDK_TYPE_SCREEN_X11));
end;

function GDK_SCREEN_X11_CLASS(klass : pointer) : PGdkScreenX11Class;
begin
   GDK_SCREEN_X11_CLASS:=PGdkScreenX11Class(klass);
   //GDK_SCREEN_X11_CLASS:=PGdkScreenX11Class(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_SCREEN_X11));
end;

{ Private
function GDK_IS_SCREEN_X11(obj : pointer) : boolean;
begin
   GDK_IS_SCREEN_X11:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GDK_TYPE_SCREEN_X11);
end;

function GDK_IS_SCREEN_X11_CLASS(klass : pointer) : boolean;
begin
   GDK_IS_SCREEN_X11_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_SCREEN_X11);
end;

function GDK_SCREEN_X11_GET_CLASS(obj : pointer) : PGdkScreenX11Class;
begin
   GDK_SCREEN_X11_GET_CLASS:=PGdkScreenX11Class(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_SCREEN_X11));
end;}

{$ENDIF read_implementation}

