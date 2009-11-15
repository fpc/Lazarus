//
// AggPas 2.4 RM3 pixel format definition file
//
{ blend_pix_bgr_gamma }
procedure blend_pix_bgr_gamma(gamma : gamma_ptr; p : int8u_ptr; cr ,cg ,cb ,alpha : int; cover : unsigned = 0 );
var
 r ,g ,b : unsigned;

begin
 r:=gamma.dir(order_bgr(pointer(p )^ ).R );
 g:=gamma.dir(order_bgr(pointer(p )^ ).G );
 b:=gamma.dir(order_bgr(pointer(p )^ ).B );

 order_bgr(pointer(p )^ ).R:=int8u(int(gamma.inv((((int(gamma.dir(cr ) ) - r ) * alpha ) shr base_shift ) + r ) ) );
 order_bgr(pointer(p )^ ).G:=int8u(int(gamma.inv((((int(gamma.dir(cg ) ) - g ) * alpha ) shr base_shift ) + g ) ) );
 order_bgr(pointer(p )^ ).B:=int8u(int(gamma.inv((((int(gamma.dir(cb ) ) - b ) * alpha ) shr base_shift ) + b ) ) );

end;

{ copy_or_blend_pix_bgr_gamma }
procedure copy_or_blend_pix_bgr_gamma(gamma : gamma_ptr; p : int8u_ptr; c : aggclr_ptr; cover : unsigned ); overload;
var
 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    begin
     order_bgr(pointer(p )^ ).R:=c.r;
     order_bgr(pointer(p )^ ).G:=c.g;
     order_bgr(pointer(p )^ ).B:=c.b;

    end
   else
    blend_pix_bgr_gamma(gamma ,p ,c.r ,c.g ,c.b ,alpha ,cover );

  end;

end;

{ copy_or_blend_pix_bgr_gamma }
procedure copy_or_blend_pix_bgr_gamma(gamma : gamma_ptr; p : int8u_ptr; c : aggclr_ptr ); overload;
begin
 if c.a <> 0 then
  if c.a = base_mask then
   begin
    order_bgr(pointer(p )^ ).R:=c.r;
    order_bgr(pointer(p )^ ).G:=c.g;
    order_bgr(pointer(p )^ ).B:=c.b;

   end
  else
   blend_pix_bgr_gamma(gamma ,p ,c.r ,c.g ,c.b ,c.a );

end;

{ bgr24_gamma_blend_pixel }
procedure bgr24_gamma_blend_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr; cover : int8u );
begin
 copy_or_blend_pix_bgr_gamma(this.m_gamma ,int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x ) ,c ,cover );

end;

{ bgr24_gamma_blend_hline }
procedure bgr24_gamma_blend_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
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
     order_bgr(pointer(p )^ ).R:=c.r;
     order_bgr(pointer(p )^ ).G:=c.g;
     order_bgr(pointer(p )^ ).B:=c.b;

     inc(ptrcomp(p ) ,3 );
     dec(len );

    until len = 0
   else
    repeat
     blend_pix_bgr_gamma(this.m_gamma ,p ,c.r ,c.g ,c.b ,alpha );

     inc(ptrcomp(p ) ,3 );
     dec(len );

    until len = 0;

  end;

end;

{ bgr24_gamma_blend_vline }
procedure bgr24_gamma_blend_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
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
     order_bgr(pointer(p )^ ).R:=c.r;
     order_bgr(pointer(p )^ ).G:=c.g;
     order_bgr(pointer(p )^ ).B:=c.b;

     p:=int8u_ptr(this.m_rbuf.next_row(p ) );

     dec(len );

    until len = 0
   else
    repeat
     blend_pix_bgr_gamma(this.m_gamma ,p ,c.r ,c.g ,c.b ,alpha ,cover );

     p:=int8u_ptr(this.m_rbuf.next_row(p ) );

     dec(len );

    until len = 0;

  end;

end;

{ bgr24_gamma_blend_solid_hspan }
procedure bgr24_gamma_blend_solid_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
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
      order_bgr(pointer(p )^ ).R:=c.r;
      order_bgr(pointer(p )^ ).G:=c.g;
      order_bgr(pointer(p )^ ).B:=c.b;

     end
    else
     blend_pix_bgr_gamma(this.m_gamma ,p ,c.r ,c.g ,c.b ,alpha ,covers^ );

    inc(ptrcomp(p ) ,3 );
    inc(ptrcomp(covers ) );
    dec(len );

   until len = 0;

  end;

end;

{ bgr24_gamma_blend_solid_vspan }
procedure bgr24_gamma_blend_solid_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
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
      order_bgr(pointer(p )^ ).R:=c.r;
      order_bgr(pointer(p )^ ).G:=c.g;
      order_bgr(pointer(p )^ ).B:=c.b;

     end
    else
     blend_pix_bgr_gamma(this.m_gamma ,p ,c.r ,c.g ,c.b ,alpha,covers^ );

    p:=int8u_ptr(this.m_rbuf.next_row(p ) );

    inc(ptrcomp(covers ) );
    dec(len );

   until len = 0;

  end;

end;

{ bgr24_gamma_blend_color_hspan }
procedure bgr24_gamma_blend_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x );

 if covers <> NIL then
  repeat
   copy_or_blend_pix_bgr_gamma(this.m_gamma ,p ,colors ,covers^ );

   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   inc(ptrcomp(covers ) ,sizeof(int8u ) );
   inc(ptrcomp(p ) ,3 );
   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    copy_or_blend_pix_bgr_gamma(this.m_gamma ,p ,colors );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    inc(ptrcomp(p ) ,3 );
    dec(len );

   until len = 0
  else
   repeat
    copy_or_blend_pix_bgr_gamma(this.m_gamma ,p ,colors ,cover );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    inc(ptrcomp(p ) ,3 );
    dec(len );

   until len = 0;

end;

{ bgr24_gamma_blend_color_vspan }
procedure bgr24_gamma_blend_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x + x + x );

 if covers <> NIL then
  repeat
   copy_or_blend_pix_bgr_gamma(this.m_gamma ,p ,colors ,covers^ );

   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   inc(ptrcomp(covers ) ,sizeof(int8u ) );

   p:=int8u_ptr(this.m_rbuf.next_row(p ) );

   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    copy_or_blend_pix_bgr_gamma(this.m_gamma ,p ,colors );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );

    p:=int8u_ptr(this.m_rbuf.next_row(p ) );

    dec(len );

   until len = 0
  else
   repeat
    copy_or_blend_pix_bgr_gamma(this.m_gamma ,p ,colors ,cover );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );

    p:=int8u_ptr(this.m_rbuf.next_row(p ) );

    dec(len );

   until len = 0;

end;

{ bgr24_gamma_blend_from }
procedure bgr24_gamma_blend_from(this : pixel_formats_ptr; from : pixel_formats_ptr; psrc_ : int8u_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
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
      order_bgr(pointer(pdst )^ ).R:=int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^;
      order_bgr(pointer(pdst )^ ).G:=int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^;
      order_bgr(pointer(pdst )^ ).B:=int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^;

     end
    else
     blend_pix_bgr_gamma(
      this.m_gamma ,pdst ,
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

   copy_or_blend_pix_bgr_gamma(this.m_gamma ,pdst ,@color ,cover );

   inc(ptrcomp(psrc ) ,4 );
   inc(ptrcomp(pdst ) ,3 );
   dec(len );

  until len = 0;

end;

{ bgr24_gamma_blend_from_color }
procedure bgr24_gamma_blend_from_color(this : pixel_formats_ptr; from : pixel_formats_ptr; color : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
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
    copy_or_blend_pix_bgr_gamma(
     this.m_gamma ,
     pdst ,color ,
     shr_int32(psrc^ * cover + base_mask ,base_shift ) );

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,3 );
    dec(len );

   until len = 0;

  end;

end;

{ bgr24_gamma_blend_from_lut }
procedure bgr24_gamma_blend_from_lut(this : pixel_formats_ptr; from : pixel_formats_ptr; color_lut : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
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

     blend_pix_bgr_gamma(this.m_gamma ,pdst ,color.r ,color.g ,color.b ,color.a );

     inc(ptrcomp(psrc ) ,ppsz );
     inc(ptrcomp(pdst ) ,3 );
     dec(len );

    until len = 0
   else
    repeat
     copy_or_blend_pix_bgr_gamma(
      this.m_gamma ,
      pdst ,aggclr_ptr(ptrcomp(color_lut ) + psrc^ * sizeof(aggclr ) ) ,cover);

     inc(ptrcomp(psrc ) ,ppsz );
     inc(ptrcomp(pdst ) ,3 );
     dec(len );

    until len = 0;

  end;

end;

