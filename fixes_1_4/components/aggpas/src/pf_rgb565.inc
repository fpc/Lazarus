//
// AggPas 2.4 RM3 pixel format definition file
//
{ make_pix_565 }
function make_pix_565(r ,g ,b : unsigned ) : int16u;
begin
 result:=
  ((r and $F8 ) shl 8 ) or
  ((g and $FC ) shl 3 ) or
   (b shr 3 );

end;

{ make_color_565 }
procedure make_color_565(var color : aggclr; p : int16u_ptr );
begin
 color.ConstrInt(
  (p^ shr 8 ) and $F8 ,
  (p^ shr 3 ) and $FC ,
  (p^ shl 3 ) and $F8 );

end;

{ blend_pix_565 }
procedure blend_pix_565(p : int16u_ptr; cr ,cg ,cb ,alpha : int );
var
 rgb : int16u;

 r ,g ,b : int;

begin
 rgb:=p^;

 r:=(rgb shr 8 ) and $F8;
 g:=(rgb shr 3 ) and $FC;
 b:=(rgb shl 3 ) and $F8;

 p^:=
 ((((cr - r ) * alpha + (r shl 8 ) )        ) and $F800 ) or
 ((((cg - g ) * alpha + (g shl 8 ) ) shr 5  ) and $07E0 ) or
  (((cb - b ) * alpha + (b shl 8 ) ) shr 11 );

end;

{ copy_or_blend_pix_565 }
procedure copy_or_blend_pix_565(p : int16u_ptr; c : aggclr_ptr; cover : unsigned );
var
 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    p^:=make_pix_565(c.r ,c.g ,c.b )
   else
    blend_pix_565(p ,c.r ,c.g ,c.b ,alpha );

  end;

end;

{ rgb565_copy_pixel }
procedure rgb565_copy_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr );
begin
 int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) )^:=make_pix_565(c.r ,c.g ,c.b );

end;

{ rgb565_blend_pixel }
procedure rgb565_blend_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr; cover : int8u );
begin
 copy_or_blend_pix_565(int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) ) ,c ,cover );

end;

{ rgb565_pixel }
function rgb565_pixel(this : pixel_formats_ptr; x ,y : int ) : aggclr;
begin
 make_color_565(
  result ,
  int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) ) );

end;

{ rgb565_copy_hline }
procedure rgb565_copy_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
var
 p : int16u_ptr;
 v : int16u;

begin
 p:=int16u_ptr  (ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) );
 v:=make_pix_565(c.r ,c.g ,c.b );

 repeat
  p^:=v;

  inc(ptrcomp(p ) ,sizeof(int16u ) );
  dec(len );

 until len = 0;

end;

{ rgb565_copy_vline }
procedure rgb565_copy_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
var
 p : int16u_ptr;
 v : int16u;

begin
 p:=int16u_ptr  (ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) );
 v:=make_pix_565(c.r ,c.g ,c.b );

 repeat
  p^:=v;
  p :=int16u_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

  dec(len );

 until len = 0;

end;

{ rgb565_blend_hline }
procedure rgb565_blend_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : int16u_ptr;
 v : int16u;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) );

   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    begin
     v:=make_pix_565(c.r ,c.g ,c.b );

     repeat
      p^:=v;

      inc(ptrcomp(p ) ,sizeof(int16u ) );
      dec(len );

     until len = 0;

    end
   else
    repeat
     blend_pix_565(p ,c.r ,c.g ,c.b ,alpha );

     inc(ptrcomp(p ) ,sizeof(int16u ) );
     dec(len );

    until len = 0;

  end;

end;

{ rgb565_blend_vline }
procedure rgb565_blend_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : int16u_ptr;
 v : int16u;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) );

   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    begin
     v:=make_pix_565(c.r ,c.g ,c.b );

     repeat
      p^:=v;
      p :=int16u_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

      dec(len );

     until len = 0;

    end
   else
    repeat
     blend_pix_565(p ,c.r ,c.g ,c.b ,alpha );

     p:=int16u_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

     dec(len );

    until len = 0;

  end;

end;

{ rgb565_blend_solid_hspan }
procedure rgb565_blend_solid_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : int16u_ptr;

begin
 p:=int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) );

 repeat
  copy_or_blend_pix_565(p ,c ,covers^ );

  inc(ptrcomp(covers ) );
  inc(ptrcomp(p ) ,sizeof(int16u ) );
  dec(len );

 until len = 0;

end;

{ rgb565_blend_solid_vspan }
procedure rgb565_blend_solid_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : int16u_ptr;

begin
 p:=int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) );

 repeat
  copy_or_blend_pix_565(p ,c ,covers^ );

  inc(ptrcomp(covers ) );

  p:=int16u_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

  dec(len );

 until len = 0;

end;

{ rgb565_blend_color_hspan }
procedure rgb565_blend_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : int16u_ptr;

begin
 p:=int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) );

 repeat
  if covers <> NIL then
   begin
    copy_or_blend_pix_565(p ,colors ,covers^ );

    inc(ptrcomp(covers ) ,sizeof(int8u ) );

   end
  else
   copy_or_blend_pix_565(p ,colors ,cover );

  inc(ptrcomp(p ) ,sizeof(int16u ) );
  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  dec(len );

 until len = 0;

end;

{ rgb565_blend_color_vspan }
procedure rgb565_blend_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : int16u_ptr;

begin
 p:=int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) );

 repeat
  if covers <> NIL then
   begin
    copy_or_blend_pix_565(p ,colors ,covers^ );

    inc(ptrcomp(covers ) ,sizeof(int8u ) );

   end
  else
   copy_or_blend_pix_565(p ,colors ,cover );

  p:=int16u_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  dec(len );

 until len = 0;

end;

{ rgb565_copy_from }
procedure rgb565_copy_from(this : pixel_formats_ptr; from : rendering_buffer_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned );
begin
 move(
  int16u_ptr(ptrcomp(from.row(ysrc ) ) + xsrc * sizeof(int16u ) )^ ,
  int16u_ptr(ptrcomp(this.m_rbuf.row(ydst ) ) + xdst * sizeof(int16 ) )^ ,
  len * sizeof(int16u ) );

end;

{ rgb565_blend_from }
procedure rgb565_blend_from(this : pixel_formats_ptr; from : pixel_formats_ptr; psrc_ : int8u_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 psrc : int8u_ptr;
 pdst : int16u_ptr;

 alpha : unsigned;

begin
 psrc:=psrc_;
 pdst:=int16u_ptr(ptrcomp(this.m_rbuf.row(ydst ) ) + xdst * sizeof(int16u ) );

 repeat
  alpha:=int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^;

  if alpha <> 0 then
   if (alpha = base_mask ) and
      (cover = 255 ) then
    pdst^:=make_pix_565(
     int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
     int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
     int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ )
   else
    blend_pix_565(
     pdst ,
     int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
     int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
     int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
     alpha );

  inc(ptrcomp(psrc ) ,4 );
  inc(ptrcomp(pdst ) ,sizeof(int16u ) );
  dec(len );

 until len = 0;

end;

{ rgb565_copy_color_hspan }
procedure rgb565_copy_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
var
 p : int16u_ptr;

begin
 p:=int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) );

 repeat
  p^:=make_pix_565(colors.r ,colors.g ,colors.b );

  inc(ptrcomp(p ) ,sizeof(int16u ) );
  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  dec(len );

 until len = 0;

end;

{ rgb565_copy_color_vspan }
procedure rgb565_copy_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
var
 p : int16u_ptr;

begin
 p:=int16u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * sizeof(int16u ) );

 repeat
  p^:=make_pix_565(colors.r ,colors.g ,colors.b );
  p :=int16u_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  dec(len );

 until len = 0;

end;

{ rgb565_blend_from_color }
procedure rgb565_blend_from_color(this : pixel_formats_ptr; from : pixel_formats_ptr; color : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 ppsz : unsigned;

 psrc : int8u_ptr;
 pdst : int16u_ptr;

begin
 ppsz:=from._pix_width;
 psrc:=from.row_ptr(ysrc );

 if psrc <> NIL then
  begin
   pdst:=int16u_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + xdst * sizeof(int16u ) );

   repeat
    blend_pix_565(pdst ,color.r ,color.g ,color.b ,shr_int32(psrc^ * cover + base_mask ,base_shift ) );

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,sizeof(int16u ) );
    dec(len );

   until len = 0;

  end;

end;

{ rgb565_blend_from_lut }
procedure rgb565_blend_from_lut(this : pixel_formats_ptr; from : pixel_formats_ptr; color_lut : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 ppsz : unsigned;

 psrc : int8u_ptr;
 pdst : int16u_ptr;

 color : aggclr_ptr;

begin
 ppsz:=from._pix_width;
 psrc:=from.row_ptr(ysrc );

 if psrc <> NIL then
  begin
   pdst:=int16u_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + xdst * sizeof(int16u ) );

   repeat
    color:=aggclr_ptr(ptrcomp(color_lut ) + psrc^ * sizeof(aggclr ) );

    blend_pix_565(pdst ,color.r ,color.g ,color.b ,shr_int32(psrc^ * cover + base_mask ,base_shift ) );

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,sizeof(int16u ) );
    dec(len );

   until len = 0;

  end;

end;

