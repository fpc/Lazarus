//
// AggPas 2.4 RM3 pixel format definition file
//
{ blend_pix_argb }
procedure blend_pix_argb(p : unsigned_ptr; cr ,cg ,cb ,alpha : int; cover : unsigned = 0 );
var
 r ,g ,b ,a : int8u;

begin
 r:=order_argb(p^ ).R;
 g:=order_argb(p^ ).G;
 b:=order_argb(p^ ).B;
 a:=order_argb(p^ ).A;

 order_argb(p^ ).R:=((cr - r ) * alpha + (r shl base_shift ) ) shr base_shift;
 order_argb(p^ ).G:=((cg - g ) * alpha + (g shl base_shift ) ) shr base_shift;
 order_argb(p^ ).B:=((cb - b ) * alpha + (b shl base_shift ) ) shr base_shift;
 order_argb(p^ ).A:=(alpha + a ) - ((alpha * a + base_mask ) shr base_shift );

end;

{ copy_or_blend_pix_argb }
procedure copy_or_blend_pix_argb(p : unsigned_ptr; cr ,cg ,cb ,alpha : unsigned ); overload;
begin
 if alpha <> 0 then
  if alpha = base_mask then
   begin
    order_argb(p^ ).R:=cr;
    order_argb(p^ ).G:=cg;
    order_argb(p^ ).B:=cb;
    order_argb(p^ ).A:=base_mask;

   end
  else
   blend_pix_argb(p ,cr ,cg ,cb ,alpha );

end;

{ copy_or_blend_pix_argb }
procedure copy_or_blend_pix_argb(p : unsigned_ptr; cr ,cg ,cb ,alpha ,cover : unsigned ); overload;
begin
 if cover = 255 then
  copy_or_blend_pix_argb(p ,cr ,cg ,cb ,alpha )
 else
  if alpha <> 0 then
   begin
    alpha:=(alpha * (cover + 1 ) ) shr 8;

    if alpha = base_mask then
     begin
      order_argb(p^ ).R:=cr;
      order_argb(p^ ).G:=cg;
      order_argb(p^ ).B:=cb;
      order_argb(p^ ).A:=base_mask;

     end
    else
     blend_pix_argb(p ,cr ,cg ,cb ,alpha ,cover );

   end;

end;

{ argb32_copy_pixel }
procedure argb32_copy_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr );
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

 order_argb(p^ ).R:=c.r;
 order_argb(p^ ).G:=c.g;
 order_argb(p^ ).B:=c.b;
 order_argb(p^ ).A:=c.a;

end;

{ argb32_blend_pixel }
procedure argb32_blend_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr; cover : int8u );
begin
 copy_or_blend_pix_argb(
  unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) ) ,
  c.r ,c.g ,c.b ,c.a ,cover );

end;

{ argb32_pixel }
function argb32_pixel(this : pixel_formats_ptr; x ,y : int ) : aggclr;
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

 result.ConstrInt(
  order_argb(p^ ).R ,
  order_argb(p^ ).G ,
  order_argb(p^ ).B ,
  order_argb(p^ ).A );

end;

{ argb32_copy_hline }
procedure argb32_copy_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
var
 p : int8u_ptr;
 v : unsigned;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

 order_argb(v ).R:=c.r;
 order_argb(v ).G:=c.g;
 order_argb(v ).B:=c.b;
 order_argb(v ).A:=c.a;

 if len > 0 then
  repeat
   unsigned(pointer(p )^ ):=v;

   inc(ptrcomp(p ) ,4 );
   dec(len );

  until len = 0;

end;

{ argb32_copy_vline }
procedure argb32_copy_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
var
 p : unsigned_ptr;
 v : unsigned;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

 order_argb(v ).R:=c.r;
 order_argb(v ).G:=c.g;
 order_argb(v ).B:=c.b;
 order_argb(v ).A:=c.a;

 if len > 0 then
  repeat
   p^:=v;
   p :=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

   dec(len );

  until len = 0;

end;

{ argb32_blend_hline }
procedure argb32_blend_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : unsigned_ptr;
 v : unsigned;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    begin
     order_argb(v ).R:=c.r;
     order_argb(v ).G:=c.g;
     order_argb(v ).B:=c.b;
     order_argb(v ).A:=c.a;

     repeat
      p^:=v;

      inc(ptrcomp(p ) ,sizeof(unsigned ) );
      dec(len );

     until len = 0;

    end
   else
    if cover = 255 then
     repeat
      blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha );

      inc(ptrcomp(p ) ,sizeof(unsigned ) );
      dec(len );

     until len = 0
    else
     repeat
      blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha ,cover );

      inc(ptrcomp(p ) ,sizeof(unsigned ) );
      dec(len );

     until len = 0;

  end;

end;

{ argb32_blend_vline }
procedure argb32_blend_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : unsigned_ptr;
 v : unsigned;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    begin
     order_argb(v ).R:=c.r;
     order_argb(v ).G:=c.g;
     order_argb(v ).B:=c.b;
     order_argb(v ).A:=c.a;

     repeat
      p^:=v;
      p :=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

     until len = 0;

    end
   else
    if cover = 255 then
     repeat
      blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha );

      p:=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

      dec(len );

     until len = 0
    else
     repeat
      blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha ,cover );

      p:=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

      dec(len );

     until len = 0;

  end;

end;

{ argb32_blend_solid_hspan }
procedure argb32_blend_solid_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : unsigned_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

   repeat
    alpha:=(c.a * (covers^ + 1 ) ) shr 8;

    if alpha = base_mask then
     begin
      order_argb(p^ ).R:=c.r;
      order_argb(p^ ).G:=c.g;
      order_argb(p^ ).B:=c.b;
      order_argb(p^ ).A:=base_mask;

     end
    else
     blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha ,covers^ );

    inc(ptrcomp(p ) ,4 );
    inc(ptrcomp(covers ) );

    dec(len );

   until len = 0;

  end;

end;

{ argb32_blend_solid_vspan }
procedure argb32_blend_solid_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : unsigned_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

   repeat
    alpha:=(c.a * (covers^ + 1 ) ) shr 8;

    if alpha = base_mask then
     begin
      order_argb(p^ ).R:=c.r;
      order_argb(p^ ).G:=c.g;
      order_argb(p^ ).B:=c.b;
      order_argb(p^ ).A:=base_mask;

     end
    else
     blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha ,covers^ );

    p:=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

    inc(ptrcomp(covers ) ,sizeof(int8u ) );
    dec(len );

   until len = 0;

  end;

end;

{ argb32_blend_color_hspan }
procedure argb32_blend_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

 if covers <> NIL then
  repeat
   copy_or_blend_pix_argb(p ,colors.r ,colors.g ,colors.b ,colors.a ,covers^ );

   inc(ptrcomp(covers ) ,sizeof(int8u ) );
   inc(ptrcomp(p ) ,4 );
   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    copy_or_blend_pix_argb(p ,colors.r ,colors.g ,colors.b ,colors.a );

    inc(ptrcomp(p ) ,4 );
    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    dec(len );

   until len = 0
  else
   repeat
    copy_or_blend_pix_argb(p ,colors.r ,colors.g ,colors.b ,colors.a ,cover );

    inc(ptrcomp(p ) ,4 );
    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    dec(len );
    
   until len = 0;

end;

{ argb32_blend_color_vspan }
procedure argb32_blend_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

 if covers <> NIL then
  repeat
   copy_or_blend_pix_argb(
    p ,
    colors.r ,
    colors.g ,
    colors.b ,
    colors.a ,
    covers^ );

   inc(ptrcomp(covers ) ,sizeof(int8u ) );

   p:=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    copy_or_blend_pix_argb(
     p ,
     colors.r ,
     colors.g ,
     colors.b ,
     colors.a );

    p:=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    dec(len );

   until len = 0
  else
   repeat
    copy_or_blend_pix_argb(
     p ,
     colors.r ,
     colors.g ,
     colors.b ,
     colors.a ,
     cover );

    p:=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    dec(len );

   until len = 0;

end;

{ argb32_blend_from }
procedure argb32_blend_from(this : pixel_formats_ptr; from : pixel_formats_ptr; psrc_ : int8u_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 psrc ,pdst : unsigned_ptr;

 incp : int;

begin
 psrc:=unsigned_ptr(psrc_ );
 pdst:=unsigned_ptr(ptrcomp(this.m_rbuf.row(ydst ) ) + (xdst shl 2 ) * sizeof(int8u ) );
 incp:=4;

 if ptrcomp(xdst ) > ptrcomp(xsrc ) then
  begin
   inc(ptrcomp(psrc ) ,((len - 1 ) shl 2 ) * sizeof(int8u ) );
   inc(ptrcomp(pdst ) ,((len - 1 ) shl 2 ) * sizeof(int8u ) );

   incp:=-4;

  end;

 if cover = 255 then
  repeat
   copy_or_blend_pix_argb(
    pdst ,
    order_argb(psrc^ ).R ,
    order_argb(psrc^ ).G ,
    order_argb(psrc^ ).B ,
    order_argb(psrc^ ).A );

   inc(ptrcomp(psrc ) ,incp );
   inc(ptrcomp(pdst ) ,incp );
   dec(len );

  until len = 0
 else
  repeat
   copy_or_blend_pix_argb(
    pdst ,
    order_argb(psrc^ ).R ,
    order_argb(psrc^ ).G ,
    order_argb(psrc^ ).B ,
    order_argb(psrc^ ).A ,
    cover );

   inc(ptrcomp(psrc ) ,incp );
   inc(ptrcomp(pdst ) ,incp );
   dec(len );

  until len = 0;

end;

{ argb32_copy_color_hspan }
procedure argb32_copy_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

 repeat
  order_argb(pointer(p )^ ).R:=colors.r;
  order_argb(pointer(p )^ ).G:=colors.g;
  order_argb(pointer(p )^ ).B:=colors.b;
  order_argb(pointer(p )^ ).A:=colors.a;

  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  inc(ptrcomp(p ) ,4 );
  dec(len );

 until len = 0;

end;

{ argb32_copy_color_vspan }
procedure argb32_copy_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

 repeat
  order_argb(pointer(p )^ ).R:=colors.r;
  order_argb(pointer(p )^ ).G:=colors.g;
  order_argb(pointer(p )^ ).B:=colors.b;
  order_argb(pointer(p )^ ).A:=colors.a;

  p:=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  dec(len );

 until len = 0;

end;

{ argb32_blend_from_color }
procedure argb32_blend_from_color(this : pixel_formats_ptr; from : pixel_formats_ptr; color : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 ppsz : unsigned;

 psrc : int8u_ptr;
 pdst : unsigned_ptr;

begin
 ppsz:=from._pix_width;
 psrc:=from.row_ptr(ysrc );

 if psrc <> NIL then
  begin
   pdst:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + (xdst shl 2 ) );

   repeat
    copy_or_blend_pix_argb(
     pdst ,color.r, color.g, color.b, color.a,
     shr_int32(psrc^ * cover + base_mask ,base_shift ) );

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,4 );
    dec(len );

   until len = 0;

  end;

end;

{ argb32_blend_from_lut }
procedure argb32_blend_from_lut(this : pixel_formats_ptr; from : pixel_formats_ptr; color_lut : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 ppsz : unsigned;

 psrc : int8u_ptr;
 pdst : unsigned_ptr;

 color : aggclr_ptr;

begin
 ppsz:=from._pix_width;
 psrc:=from.row_ptr(ysrc );

 if psrc <> NIL then
  begin
   pdst:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + (xdst shl 2 ) );

   if cover = 255 then
    repeat
     color:=aggclr_ptr(ptrcomp(color_lut ) + psrc^ * sizeof(aggclr ) );

     copy_or_blend_pix_argb(pdst ,color.r, color.g, color.b, color.a );

     inc(ptrcomp(psrc ) ,ppsz );
     inc(ptrcomp(pdst ) ,4 );
     dec(len );

    until len = 0
   else
    repeat
     color:=aggclr_ptr(ptrcomp(color_lut ) + psrc^ * sizeof(aggclr ) );

     copy_or_blend_pix_argb(pdst ,color.r, color.g, color.b, color.a ,cover );

     inc(ptrcomp(psrc ) ,ppsz );
     inc(ptrcomp(pdst ) ,4 );
     dec(len );

    until len = 0;

  end;

end;

