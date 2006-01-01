{%MainUnit ../gdk2x.pas}

{$IFDEF read_interface_rest}
{ Drawable implementation for X11 }

type
   PGdkDrawableImplX11 = ^TGdkDrawableImplX11;
   TGdkDrawableImplX11 = record
        parent_instance : TGdkDrawable;
        wrapper : PGdkDrawable;
        colormap : PGdkColormap;
        xid : TWindow;
        screen : PGdkScreen;
        {$IFDEF Has_XFT}
        picture : TPicture;
        {$ENDIF}
     end;

   PGdkDrawableImplX11Class = ^TGdkDrawableImplX11Class;
   TGdkDrawableImplX11Class = record
        parent_class : TGdkDrawableClass;
     end;

// Private function GDK_TYPE_DRAWABLE_IMPL_X11 : GType;
function GDK_DRAWABLE_IMPL_X11(obj : pointer) : PGdkDrawableImplX11;
function GDK_DRAWABLE_IMPL_X11_CLASS(klass : pointer) : PGdkDrawableImplX11Class;
// Private function GDK_IS_DRAWABLE_IMPL_X11(obj : pointer) : boolean;
// Private function GDK_IS_DRAWABLE_IMPL_X11_CLASS(klass : pointer) : boolean;
// Private function GDK_DRAWABLE_IMPL_X11_GET_CLASS(obj : pointer) : PGdkDrawableImplX11Class;

// Private function _gdk_drawable_impl_x11_get_type:GType;cdecl;external;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

{ Private
function GDK_TYPE_DRAWABLE_IMPL_X11 : GType;
begin
   GDK_TYPE_DRAWABLE_IMPL_X11:=_gdk_drawable_impl_x11_get_type;
end;}

function GDK_DRAWABLE_IMPL_X11(obj : pointer) : PGdkDrawableImplX11;
begin
   GDK_DRAWABLE_IMPL_X11:=PGdkDrawableImplX11(obj);
   //GDK_DRAWABLE_IMPL_X11:=PGdkDrawableImplX11(G_TYPE_CHECK_INSTANCE_CAST(obj,GDK_TYPE_DRAWABLE_IMPL_X11));
end;

function GDK_DRAWABLE_IMPL_X11_CLASS(klass : pointer) : PGdkDrawableImplX11Class;
begin
   GDK_DRAWABLE_IMPL_X11_CLASS:=PGdkDrawableImplX11Class(klass);
   //GDK_DRAWABLE_IMPL_X11_CLASS:=PGdkDrawableImplX11Class(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_DRAWABLE_IMPL_X11));
end;

{ Private
function GDK_IS_DRAWABLE_IMPL_X11(obj : pointer) : boolean;
begin
   GDK_IS_DRAWABLE_IMPL_X11:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GDK_TYPE_DRAWABLE_IMPL_X11);
end;

function GDK_IS_DRAWABLE_IMPL_X11_CLASS(klass : pointer) : boolean;
begin
   GDK_IS_DRAWABLE_IMPL_X11_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_DRAWABLE_IMPL_X11);
end;

function GDK_DRAWABLE_IMPL_X11_GET_CLASS(obj : pointer) : PGdkDrawableImplX11Class;
begin
   GDK_DRAWABLE_IMPL_X11_GET_CLASS:=PGdkDrawableImplX11Class(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_DRAWABLE_IMPL_X11));
end;
}

{$ENDIF read_implementation}

