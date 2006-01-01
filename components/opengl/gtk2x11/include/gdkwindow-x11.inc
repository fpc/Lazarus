{%MainUnit ../gdk2x.pas}

{$IFDEF read_interface_rest}

type
{ Offsets to add to X coordinates within window  }
{   to get GDK coodinates within window  }
{ Set when the window background is temporarily
   unset during resizing and scaling  }
{ visible rectangle of window  }
   PGdkXPositionInfo = ^TGdkXPositionInfo;
   TGdkXPositionInfo = record
        x : gint;
        y : gint;
        width : gint;
        height : gint;
        x_offset : gint;
        y_offset : gint;
        flag0 : word;
        clip_rect : TGdkRectangle;
     end;

const
   bm_TGdkXPositionInfo_big = $1;
   bp_TGdkXPositionInfo_big = 0;
   bm_TGdkXPositionInfo_mapped = $2;
   bp_TGdkXPositionInfo_mapped = 1;
   bm_TGdkXPositionInfo_no_bg = $4;
   bp_TGdkXPositionInfo_no_bg = 2;
function big(var a : TGdkXPositionInfo) : guint;
procedure set_big(var a : TGdkXPositionInfo; __big : guint);
function mapped(var a : TGdkXPositionInfo) : guint;
procedure set_mapped(var a : TGdkXPositionInfo; __mapped : guint);
function no_bg(var a : TGdkXPositionInfo) : guint;
procedure set_no_bg(var a : TGdkXPositionInfo; __no_bg : guint);
{ Window implementation for X11 }


type
   PGdkWindowImplX11 = ^TGdkWindowImplX11;
   TGdkWindowImplX11 = record
        parent_instance : TGdkDrawableImplX11;
        width : gint;
        height : gint;
        position_info : TGdkXPositionInfo;
        flag0 : word;
        focus_window : TWindow;
     end;

type
   PGdkWindowImplX11Class = ^TGdkWindowImplX11Class;
   TGdkWindowImplX11Class = record
        parent_class : TGdkDrawableImplX11Class;
     end;


function GDK_TYPE_WINDOW_IMPL_X11 : GType;

function GDK_WINDOW_IMPL_X11(obj : pointer) : PGdkWindowImplX11;

function GDK_WINDOW_IMPL_X11_CLASS(klass : pointer) : PGdkWindowImplX11Class;

function GDK_IS_WINDOW_IMPL_X11(obj : pointer) : boolean;

function GDK_IS_WINDOW_IMPL_X11_CLASS(klass : pointer) : boolean;

function GDK_WINDOW_IMPL_X11_GET_CLASS(obj : pointer) : PGdkWindowImplX11Class;

{ Set if the window, or any descendent of it, has the focus }
{ Set if !window_has_focus, but events are being sent to the
     window because the pointer is in it. (Typically, no window
     manager is running. }
{ We use an extra X window for toplevel windows that we XSetInputFocus()
     to in order to avoid getting keyboard events redirected to subwindows
     that might not even be part of this app }

const
   bm_TGdkWindowImplX11_has_focus = $1;
   bp_TGdkWindowImplX11_has_focus = 0;
   bm_TGdkWindowImplX11_has_pointer_focus = $2;
   bp_TGdkWindowImplX11_has_pointer_focus = 1;
function has_focus(var a : TGdkWindowImplX11) : guint;
procedure set_has_focus(var a : TGdkWindowImplX11; __has_focus : guint);
function has_pointer_focus(var a : TGdkWindowImplX11) : guint;
procedure set_has_pointer_focus(var a : TGdkWindowImplX11; __has_pointer_focus : guint);


function gdk_window_impl_x11_get_type:GType;cdecl;external;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

function big(var a : TGdkXPositionInfo) : guint;
begin
   big:=(a.flag0 and bm_TGdkXPositionInfo_big) shr bp_TGdkXPositionInfo_big;
end;

procedure set_big(var a : TGdkXPositionInfo; __big : guint);
begin
   a.flag0:=a.flag0 or ((__big shl bp_TGdkXPositionInfo_big) and bm_TGdkXPositionInfo_big);
end;

function mapped(var a : TGdkXPositionInfo) : guint;
begin
   mapped:=(a.flag0 and bm_TGdkXPositionInfo_mapped) shr bp_TGdkXPositionInfo_mapped;
end;

procedure set_mapped(var a : TGdkXPositionInfo; __mapped : guint);
begin
   a.flag0:=a.flag0 or ((__mapped shl bp_TGdkXPositionInfo_mapped) and bm_TGdkXPositionInfo_mapped);
end;

function no_bg(var a : TGdkXPositionInfo) : guint;
begin
   no_bg:=(a.flag0 and bm_TGdkXPositionInfo_no_bg) shr bp_TGdkXPositionInfo_no_bg;
end;

procedure set_no_bg(var a : TGdkXPositionInfo; __no_bg : guint);
begin
   a.flag0:=a.flag0 or ((__no_bg shl bp_TGdkXPositionInfo_no_bg) and bm_TGdkXPositionInfo_no_bg);
end;

function GDK_TYPE_WINDOW_IMPL_X11 : GType;
begin
   GDK_TYPE_WINDOW_IMPL_X11:=gdk_window_impl_x11_get_type;
end;

function GDK_WINDOW_IMPL_X11(obj : pointer) : PGdkWindowImplX11;
begin
   GDK_WINDOW_IMPL_X11:=PGdkWindowImplX11(G_TYPE_CHECK_INSTANCE_CAST(obj,GDK_TYPE_WINDOW_IMPL_X11));
end;

function GDK_WINDOW_IMPL_X11_CLASS(klass : pointer) : PGdkWindowImplX11Class;
begin
   GDK_WINDOW_IMPL_X11_CLASS:=PGdkWindowImplX11Class(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_WINDOW_IMPL_X11));
end;

function GDK_IS_WINDOW_IMPL_X11(obj : pointer) : boolean;
begin
   GDK_IS_WINDOW_IMPL_X11:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GDK_TYPE_WINDOW_IMPL_X11);
end;

function GDK_IS_WINDOW_IMPL_X11_CLASS(klass : pointer) : boolean;
begin
   GDK_IS_WINDOW_IMPL_X11_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_WINDOW_IMPL_X11);
end;

function GDK_WINDOW_IMPL_X11_GET_CLASS(obj : pointer) : PGdkWindowImplX11Class;
begin
   GDK_WINDOW_IMPL_X11_GET_CLASS:=PGdkWindowImplX11Class(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_WINDOW_IMPL_X11));
end;

function has_focus(var a : TGdkWindowImplX11) : guint;
begin
   has_focus:=(a.flag0 and bm_TGdkWindowImplX11_has_focus) shr bp_TGdkWindowImplX11_has_focus;
end;

procedure set_has_focus(var a : TGdkWindowImplX11; __has_focus : guint);
begin
   a.flag0:=a.flag0 or ((__has_focus shl bp_TGdkWindowImplX11_has_focus) and bm_TGdkWindowImplX11_has_focus);
end;

function has_pointer_focus(var a : TGdkWindowImplX11) : guint;
begin
   has_pointer_focus:=(a.flag0 and bm_TGdkWindowImplX11_has_pointer_focus) shr bp_TGdkWindowImplX11_has_pointer_focus;
end;

procedure set_has_pointer_focus(var a : TGdkWindowImplX11; __has_pointer_focus : guint);
begin
   a.flag0:=a.flag0 or ((__has_pointer_focus shl bp_TGdkWindowImplX11_has_pointer_focus) and bm_TGdkWindowImplX11_has_pointer_focus);
end;

{$ENDIF read_implementation}

