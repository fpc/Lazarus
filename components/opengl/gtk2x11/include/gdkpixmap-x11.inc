{%MainUnit ../gdk2x.pas}

{$IFDEF read_interface_rest}

{ Pixmap implementation for X11 }

type
   PGdkPixmapImplX11 = ^TGdkPixmapImplX11;
   TGdkPixmapImplX11 = record
        parent_instance : TGdkDrawableImplX11;
        width : gint;
        height : gint;
        flag0 : word;
     end;

type
   PGdkPixmapImplX11Class = ^TGdkPixmapImplX11Class;
   TGdkPixmapImplX11Class = record
        parent_class : TGdkDrawableImplX11Class;
     end;

// Private function GDK_TYPE_PIXMAP_IMPL_X11 : GType;
function GDK_PIXMAP_IMPL_X11(obj : pointer) : PGdkPixmapImplX11;
function GDK_PIXMAP_IMPL_X11_CLASS(klass : pointer) : PGdkPixmapImplX11Class;
// Private function GDK_IS_PIXMAP_IMPL_X11(obj : pointer) : boolean;
// Private function GDK_IS_PIXMAP_IMPL_X11_CLASS(klass : pointer) : boolean;
// Private function GDK_PIXMAP_IMPL_X11_GET_CLASS(obj : pointer) : PGdkPixmapImplX11Class;


const
   bm_TGdkPixmapImplX11_is_foreign = $1;
   bp_TGdkPixmapImplX11_is_foreign = 0;

function is_foreign(a : PGdkPixmapImplX11) : guint;
procedure set_is_foreign(a : PGdkPixmapImplX11; __is_foreign : guint);

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

{ Private
function gdk_pixmap_impl_x11_get_type: GType; cdecl; external;

function GDK_TYPE_PIXMAP_IMPL_X11 : GType;
begin
   GDK_TYPE_PIXMAP_IMPL_X11:=gdk_pixmap_impl_x11_get_type;
end;     }

function GDK_PIXMAP_IMPL_X11(obj : pointer) : PGdkPixmapImplX11;
begin
   GDK_PIXMAP_IMPL_X11:=PGdkPixmapImplX11(obj);
   //GDK_PIXMAP_IMPL_X11:=PGdkPixmapImplX11(G_TYPE_CHECK_INSTANCE_CAST(obj,GDK_TYPE_PIXMAP_IMPL_X11));
end;

function GDK_PIXMAP_IMPL_X11_CLASS(klass : pointer) : PGdkPixmapImplX11Class;
begin
   GDK_PIXMAP_IMPL_X11_CLASS:=PGdkPixmapImplX11Class(klass);
   //GDK_PIXMAP_IMPL_X11_CLASS:=PGdkPixmapImplX11Class(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_PIXMAP_IMPL_X11));
end;

{ Private
function GDK_IS_PIXMAP_IMPL_X11(obj : pointer) : boolean;
begin
   GDK_IS_PIXMAP_IMPL_X11:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GDK_TYPE_PIXMAP_IMPL_X11);
end;

function GDK_IS_PIXMAP_IMPL_X11_CLASS(klass : pointer) : boolean;
begin
   GDK_IS_PIXMAP_IMPL_X11_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_PIXMAP_IMPL_X11);
end;

function GDK_PIXMAP_IMPL_X11_GET_CLASS(obj : pointer) : PGdkPixmapImplX11Class;
begin
   GDK_PIXMAP_IMPL_X11_GET_CLASS:=PGdkPixmapImplX11Class(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_PIXMAP_IMPL_X11));
end; }

function is_foreign(a : PGdkPixmapImplX11) : guint;
begin
   is_foreign:=(a^.flag0 and bm_TGdkPixmapImplX11_is_foreign) shr bp_TGdkPixmapImplX11_is_foreign;
end;

procedure set_is_foreign(a : PGdkPixmapImplX11; __is_foreign : guint);
begin
   a^.flag0:=a^.flag0 or ((__is_foreign shl bp_TGdkPixmapImplX11_is_foreign) and bm_TGdkPixmapImplX11_is_foreign);
end;

{$ENDIF read_implementation}

