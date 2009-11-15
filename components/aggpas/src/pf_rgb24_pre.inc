//
// AggPas 2.4 RM3 pixel format definition file
//
{ blend_pix_rgb_pre }
procedure blend_pix_rgb_pre(p : int8u_ptr; cr ,cg ,cb ,alpha ,cover : unsigned ); overload;
begin
 alpha:=base_mask - alpha;
 cover:=(cover + 1 ) shl (base_shift - 8 );

 order_rgb(pointer(p )^ ).R:=int8u((order_rgb(pointer(p )^ ).R * alpha + cr * cover ) shr base_shift );
 order_rgb(pointer(p )^ ).G:=int8u((order_rgb(pointer(p )^ ).G * alpha + cg * cover ) shr base_shift );
 order_rgb(pointer(p )^ ).B:=int8u((order_rgb(pointer(p )^ ).B * alpha + cb * cover ) shr base_shift );

end;

{ blend_pix_rgb_pre }
procedure blend_pix_rgb_pre(p : int8u_ptr; cr ,cg ,cb ,alpha : unsigned ); overload;
begin
 alpha:=base_mask - alpha;

 order_rgb(pointer(p )^ ).R:=int8u(((order_rgb(pointer(p )^ ).R * alpha ) shr base_shift ) + cr );
 order_rgb(pointer(p )^ ).G:=int8u(((order_rgb(pointer(p )^ ).G * alpha ) shr base_shift ) + cg );
 order_rgb(pointer(p )^ ).B:=int8u(((order_rgb(pointer(p )^ ).B * alpha ) shr base_shift ) + cb );

end;

{ copy_or_blend_pix_rgb_pre }
procedure copy_or_blend_pix_rgb_pre(p : int8u_ptr; c : aggclr_ptr; cover : unsigned ); overload;
var
 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    begin
     order_rgb(pointer(p )^ ).R:=c.r;
     order_rgb(pointer(p )^ ).G:=c.g;
     order_rgb(pointer(p )^ ).B:=c.b;

    end
   else
    blend_pix_rgb_pre(p ,c.r ,c.g ,c.b ,alpha ,cover );

  end;

end;

{ copy_or_blend_pix_rgb_pre }
procedure copy_or_blend_pix_rgb_pre(p : int8u_ptr; c : aggclr_ptr ); overload;
begin
 if c.a <> 0 then
  if c.a = base_mask then
   begin
    order_rgb(pointer(p )^ ).R:=c.r;
    order_rgb(pointer(p )^ ).G:=c.g;
    order_rgb(pointer(p )^ ).B:=c.b;

   end
  else
   blend_pix_rgb_pre(p ,c.r ,c.g ,c.b ,c.a );

end;

{ rgb24_pre_blend_pixel }
procedure rgb24_pre_blend_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr; cover : int8u );
begin
 copy_or_blend_pix_rgb_pre(int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x ) ,c ,cover );

end;

{ rgb24_pre_blend_hline }
procedure rgb24_pre_blend_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : int8u_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x );

   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    repeat
     order_rgb(pointer(p )^ ).R:=c.r;
     order_rgb(pointer(p )^ ).G:=c.g;
     order_rgb(pointer(p )^ ).B:=c.b;

     inc(ptrcomp(p ) ,3 );
     dec(len );

    until len = 0
   else
    repeat
     blend_pix_rgb_pre(p ,c.r ,c.g ,c.b ,alpha );

     inc(ptrcomp(p ) ,3 );
     dec(len );

    until len = 0;

  end;

end;

{ rgb24_pre_blend_vline }
procedure rgb24_pre_blend_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : int8u_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x );

   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    repeat
     order_rgb(pointer(p )^ ).R:=c.r;
     order_rgb(pointer(p )^ ).G:=c.g;
     order_rgb(pointer(p )^ ).B:=c.b;

     p:=int8u_ptr(this.m_rbuf.next_row(p ) );

     dec(len );

    until len = 0
   else
    repeat
     blend_pix_rgb_pre(p ,c.r ,c.g ,c.b ,alpha ,cover );

     p:=int8u_ptr(this.m_rbuf.next_row(p ) );

     dec(len );

    until len = 0;

  end;

end;

{ rgb24_pre_blend_solid_hspan }
procedure rgb24_pre_blend_solid_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : int8u_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x );

   repeat
    alpha:=(c.a * (covers^ + 1 ) ) shr 8;

    if alpha = base_mask then
     begin
      order_rgb(pointer(p )^ ).R:=c.r;
      order_rgb(pointer(p )^ ).G:=c.g;
      order_rgb(pointer(p )^ ).B:=c.b;

     end
    else
     blend_pix_rgb_pre(p ,c.r ,c.g ,c.b ,alpha ,covers^ );

    inc(ptrcomp(p ) ,3 );
    inc(ptrcomp(covers ) );
    dec(len );

   until len = 0;

  end;

end;

{ rgb24_pre_blend_solid_vspan }
procedure rgb24_pre_blend_solid_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : int8u_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x );

   repeat
    alpha:=(c.a * (covers^ + 1 ) ) shr 8;

    if alpha = base_mask then
     begin
      order_rgb(pointer(p )^ ).R:=c.r;
      order_rgb(pointer(p )^ ).G:=c.g;
      order_rgb(pointer(p )^ ).B:=c.b;

     end
    else
     blend_pix_rgb_pre(p ,c.r ,c.g ,c.b ,alpha,covers^ );

    p:=int8u_ptr(this.m_rbuf.next_row(p ) );

    inc(ptrcomp(covers ) );
    dec(len );

   until len = 0;

  end;

end;

{ rgb24_pre_blend_color_hspan }
procedure rgb24_pre_blend_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x );

 if covers <> NIL then
  repeat
   copy_or_blend_pix_rgb_pre(p ,colors ,covers^ );

   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   inc(ptrcomp(covers ) ,sizeof(int8u ) );
   inc(ptrcomp(p ) ,3 );
   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    copy_or_blend_pix_rgb_pre(p ,colors );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    inc(ptrcomp(p ) ,3 );
    dec(len );

   until len = 0
  else
   repeat
    copy_or_blend_pix_rgb_pre(p ,colors ,cover );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    inc(ptrcomp(p ) ,3 );
    dec(len );

   until len = 0;

end;

{ rgb24_pre_blend_color_vspan }
procedure rgb24_pre_blend_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x );

 if covers <> NIL then
  repeat
   copy_or_blend_pix_rgb_pre(p ,colors ,covers^ );

   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   inc(ptrcomp(covers ) ,sizeof(int8u ) );

   p:=int8u_ptr(this.m_rbuf.next_row(p ) );

   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    copy_or_blend_pix_rgb_pre(p ,colors );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );

    p:=int8u_ptr(this.m_rbuf.next_row(p ) );

    dec(len );

   until len = 0
  else
   repeat
    copy_or_blend_pix_rgb_pre(p ,colors ,cover );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );

    p:=int8u_ptr(this.m_rbuf.next_row(p ) );

    dec(len );

   until len = 0;

end;

{ rgb24_pre_blend_from }
procedure rgb24_pre_blend_from(this : pixel_formats_ptr; from : pixel_formats_ptr; psrc_ : int8u_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 psrc ,pdst : int8u_ptr;

 color : aggclr;
 alpha : unsigned;

begin
 psrc:=psrc_;
 pdst:=int8u_ptr(ptrcomp(this.m_rbuf.row(ydst ) ) + xdst * 3 * sizeof(int8u ) );

 if cover = 255 then
  repeat
   alpha:=int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^;

   if alpha <> 0 then
    if alpha = base_mask then
     begin
      order_rgb(pointer(pdst )^ ).R:=int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^;
      order_rgb(pointer(pdst )^ ).G:=int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^;
      order_rgb(pointer(pdst )^ ).B:=int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^;

     end
    else
     blend_pix_rgb_pre(
      pdst ,
      int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
      int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
      int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
      alpha );

   inc(ptrcomp(psrc ) ,4 );
   inc(ptrcomp(pdst ) ,3 );
   dec(len );

  until len = 0
 else
  repeat
   color.ConstrInt(
    int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
    int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
    int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
    int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^ );

   copy_or_blend_pix_rgb_pre(pdst ,@color ,cover );

   inc(ptrcomp(psrc ) ,4 );
   inc(ptrcomp(pdst ) ,3 );
   dec(len );

  until len = 0;

end;

{ rgb24_pre_blend_from_color }
procedure rgb24_pre_blend_from_color(this : pixel_formats_ptr; from : pixel_formats_ptr; color : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 ppsz : unsigned;

 psrc ,pdst : int8u_ptr;

begin
 ppsz:=from._pix_width;
 psrc:=from.row_ptr(ysrc );

 if psrc <> NIL then
  begin
   pdst:=int8u_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + xdst * 3 * sizeof(int8u ) );

   repeat
    copy_or_blend_pix_rgb_pre(
     pdst ,color ,
     shr_int32(psrc^ * cover + base_mask ,base_shift ) );

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,3 );
    dec(len );

   until len = 0;

  end;

end;

{ rgb24_pre_blend_from_lut }
procedure rgb24_pre_blend_from_lut(this : pixel_formats_ptr; from : pixel_formats_ptr; color_lut : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 ppsz : unsigned;

 psrc ,pdst : int8u_ptr;

 color : aggclr_ptr;

begin
 ppsz:=from._pix_width;
 psrc:=from.row_ptr(ysrc );

 if psrc <> NIL then
  begin
   pdst:=int8u_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + xdst * 3 * sizeof(int8u ) );

   if cover = 255 then
    repeat
     color:=aggclr_ptr(ptrcomp(color_lut ) + psrc^ * sizeof(aggclr ) );

     blend_pix_rgb_pre(pdst ,color.r ,color.g ,color.b ,color.a );

     inc(ptrcomp(psrc ) ,ppsz );
     inc(ptrcomp(pdst ) ,3 );
     dec(len );

    until len = 0
   else
    repeat
     copy_or_blend_pix_rgb_pre(
      pdst ,aggclr_ptr(ptrcomp(color_lut ) + psrc^ * sizeof(aggclr ) ) ,cover);

     inc(ptrcomp(psrc ) ,ppsz );
     inc(ptrcomp(pdst ) ,3 );
     dec(len );

    until len = 0;

  end;

end;

