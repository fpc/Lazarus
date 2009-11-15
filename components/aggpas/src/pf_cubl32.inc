//
// AggPas 2.4 RM3 pixel format definition file
//
{ cubl_copy_pixel }
procedure cubl_copy_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr );
begin
 this.blender(
  this ,this.m_comp_op ,
  int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) ) ,
  c.r ,c.g ,c.b ,c.a ,255 );

end;

{ cubl_blend_pixel }
procedure cubl_blend_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr; cover : int8u );
begin
 this.blender(
  this ,this.m_comp_op ,
  int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) ) ,
  c.r ,c.g ,c.b ,c.a ,cover );

end;

{ cubl_pixel }
function cubl_pixel(this : pixel_formats_ptr; x ,y : int ) : aggclr;
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 result.ConstrInt(
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^ ,
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^ ,
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^ ,
  int8u_ptr(ptrcomp(p ) + this.m_order.A )^ );

end;

{ cubl_copy_hline }
procedure cubl_copy_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 repeat
  this.blender(this ,this.m_comp_op ,p ,c.r ,c.g ,c.b ,c.a ,255 );

  inc(ptrcomp(p ) ,4 );
  dec(len );

 until len = 0;

end;

{ cubl_copy_vline }
procedure cubl_copy_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 repeat
  this.blender(this ,this.m_comp_op ,p ,c.r ,c.g ,c.b ,c.a ,255 );

  p:=this.m_rbuf.next_row(p );

  dec(len );

 until len = 0;

end;

{ cubl_blend_hline }
procedure cubl_blend_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 repeat
  this.blender(this ,this.m_comp_op ,p ,c.r ,c.g ,c.b ,c.a ,cover );

  inc(ptrcomp(p ) ,4 );
  dec(len );

 until len = 0;

end;

{ cubl_blend_vline }
procedure cubl_blend_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 repeat
  this.blender(this ,this.m_comp_op ,p ,c.r ,c.g ,c.b ,c.a ,cover );

  p:=this.m_rbuf.next_row(p );

  dec(len );

 until len = 0;

end;

{ cubl_blend_solid_hspan }
procedure cubl_blend_solid_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 repeat
  this.blender(this ,this.m_comp_op ,p ,c.r ,c.g ,c.b ,c.a ,covers^ );

  inc(ptrcomp(covers ) ,sizeof(int8u ) );
  inc(ptrcomp(p ) ,4 );
  dec(len );

 until len = 0;

end;

{ cubl_blend_solid_vspan }
procedure cubl_blend_solid_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 repeat
  this.blender(this ,this.m_comp_op ,p ,c.r ,c.g ,c.b ,c.a ,covers^ );

  inc(ptrcomp(covers ) ,sizeof(int8u ) );

  p:=this.m_rbuf.next_row(p );

  dec(len );

 until len = 0;

end;

{ cubl_blend_color_hspan }
procedure cubl_blend_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 repeat
  if covers <> NIL then
   begin
    this.blender(this ,this.m_comp_op ,p ,colors.r ,colors.g ,colors.b ,colors.a ,covers^ );

    inc(ptrcomp(covers ) ,sizeof(int8u ) );

   end
  else
   this.blender(this ,this.m_comp_op ,p ,colors.r ,colors.g ,colors.b ,colors.a ,cover );

  inc(ptrcomp(p ) ,4 );
  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  dec(len );

 until len = 0;

end;

{ cubl_blend_color_vspan }
procedure cubl_blend_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 repeat
  if covers <> NIL then
   begin
    this.blender(this ,this.m_comp_op ,p ,colors.r ,colors.g ,colors.b ,colors.a ,covers^ );

    inc(ptrcomp(covers ) ,sizeof(int8u ) );

   end
  else
   this.blender(this ,this.m_comp_op ,p ,colors.r ,colors.g ,colors.b ,colors.a ,cover );

  p:=this.m_rbuf.next_row(p );

  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  dec(len );

 until len = 0;

end;

{ cubl_blend_from }
procedure cubl_blend_from(this : pixel_formats_ptr; from : pixel_formats_ptr; psrc_ : int8u_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 psrc ,
 pdst : int8u_ptr;
 incp : int;

begin
 psrc:=psrc_;
 pdst:=int8u_ptr(ptrcomp(this.m_rbuf.row(ydst ) ) + (xdst shl 2 ) * sizeof(int8u ) );
 incp:=4;

 if xdst > xsrc then
  begin
   inc(ptrcomp(psrc ) ,(len - 1 ) shl 2 );
   inc(ptrcomp(pdst ) ,(len - 1 ) shl 2 );

   incp:=-4;

  end;

 repeat
  this.blender(
   this ,this.m_comp_op ,pdst ,
   int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
   int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
   int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
   int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^ ,
   cover );

  inc(ptrcomp(psrc ) ,incp );
  inc(ptrcomp(pdst ) ,incp );
  dec(len );

 until len = 0;

end;

{ cubl_copy_color_hspan }
procedure cubl_copy_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 repeat
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=colors.r;
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=colors.g;
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=colors.b;
  int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=colors.a;

  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  inc(ptrcomp(p ) ,4 );
  dec(len );

 until len = 0;

end;

{ cubl_copy_color_vspan }
procedure cubl_copy_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) * sizeof(int8u ) );

 repeat
  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=colors.r;
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=colors.g;
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=colors.b;
  int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=colors.a;

  p:=this.m_rbuf.next_row(p );

  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  dec(len );

 until len = 0;

end;

{ cubl_blend_from_color }
procedure cubl_blend_from_color(this : pixel_formats_ptr; from : pixel_formats_ptr; color : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 ppsz : unsigned;

 psrc ,
 pdst : int8u_ptr;

begin
 ppsz:=from._pix_width;
 psrc:=from.row_ptr(ysrc );

 if psrc <> NIL then
  begin
   pdst:=int8u_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + (xdst shl 2 ) );

   repeat
    this.blender(
     this ,this.m_comp_op ,pdst ,
     color.r ,color.g ,color.b ,color.a ,
     shr_int32(psrc^ * cover + base_mask ,base_shift ) );

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,4 );
    dec(len );

   until len = 0;

  end;

end;

{ cubl_blend_from_lut }
procedure cubl_blend_from_lut(this : pixel_formats_ptr; from : pixel_formats_ptr; color_lut : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 ppsz : unsigned;

 psrc ,
 pdst : int8u_ptr;

 color : aggclr_ptr;

begin
 ppsz:=from._pix_width;
 psrc:=from.row_ptr(ysrc );

 if psrc <> NIL then
  begin
   pdst:=int8u_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + (xdst shl 2 ) );

   repeat
    color:=aggclr_ptr(ptrcomp(color_lut ) + psrc^ * sizeof(aggclr ) );

    this.blender(
     this ,this.m_comp_op ,pdst ,
     color.r ,color.g ,color.b ,color.a ,
     shr_int32(psrc^ * cover + base_mask ,base_shift ) );

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,4 );
    dec(len );

   until len = 0;

  end;

end;



