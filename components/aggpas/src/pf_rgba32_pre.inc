//
// AggPas 2.4 RM3 pixel format definition file
//
{ blend_pix_rgba_pre }
procedure blend_pix_rgba_pre(p : unsigned_ptr; cr ,cg ,cb ,alpha : unsigned ); overload;
begin
 alpha:=base_mask - alpha;

 order_rgba(p^ ).R:=int8u(((order_rgba(p^ ).R * alpha) shr base_shift ) + cr );
 order_rgba(p^ ).G:=int8u(((order_rgba(p^ ).G * alpha) shr base_shift ) + cg );
 order_rgba(p^ ).B:=int8u(((order_rgba(p^ ).B * alpha) shr base_shift ) + cb );
 order_rgba(p^ ).A:=int8u(base_mask - ((alpha * (base_mask - order_rgba(p^ ).A ) ) shr base_shift) );

end;

{ blend_pix_rgba_pre }
procedure blend_pix_rgba_pre(p : unsigned_ptr; cr ,cg ,cb ,alpha ,cover : unsigned ); overload;
begin
 alpha:=base_mask - alpha;
 cover:=(cover + 1 ) shl (base_shift - 8 );

 order_rgba(p^ ).R:=int8u((order_rgba(p^ ).R * alpha + cr * cover ) shr base_shift );
 order_rgba(p^ ).G:=int8u((order_rgba(p^ ).G * alpha + cg * cover ) shr base_shift );
 order_rgba(p^ ).B:=int8u((order_rgba(p^ ).B * alpha + cb * cover ) shr base_shift );
 order_rgba(p^ ).A:=int8u(base_mask - ((alpha * (base_mask - order_rgba(p^ ).A ) ) shr base_shift ) );

end;

{ copy_or_blend_pix_rgba_pre }
procedure copy_or_blend_pix_rgba_pre(p : unsigned_ptr; cr ,cg ,cb ,alpha : unsigned ); overload;
begin
 if alpha <> 0 then
  if alpha = base_mask then
   begin
    order_rgba(p^ ).R:=cr;
    order_rgba(p^ ).G:=cg;
    order_rgba(p^ ).B:=cb;
    order_rgba(p^ ).A:=base_mask;

   end
  else
   blend_pix_rgba_pre(p ,cr ,cg ,cb ,alpha );

end;

{ copy_or_blend_pix_rgba_pre }
procedure copy_or_blend_pix_rgba_pre(p : unsigned_ptr; cr ,cg ,cb ,alpha ,cover : unsigned ); overload;
begin
 if cover = 255 then
  copy_or_blend_pix_rgba_pre(p ,cr ,cg ,cb ,alpha )
 else
  if alpha <> 0 then
   begin
    alpha:=(alpha * (cover + 1 ) ) shr 8;

    if alpha = base_mask then
     begin
      order_rgba(p^ ).R:=cr;
      order_rgba(p^ ).G:=cg;
      order_rgba(p^ ).B:=cb;
      order_rgba(p^ ).A:=base_mask;

     end
    else
     blend_pix_rgba_pre(p ,cr ,cg ,cb ,alpha ,cover );

   end;

end;

{ rgba32_pre_blend_pixel }
procedure rgba32_pre_blend_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr; cover : int8u );
begin
 copy_or_blend_pix_rgba_pre(
  unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) ) ,
  c.r ,c.g ,c.b ,c.a ,cover );

end;

{ rgba32_pre_blend_hline }
procedure rgba32_pre_blend_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
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
     order_rgba(v ).R:=c.r;
     order_rgba(v ).G:=c.g;
     order_rgba(v ).B:=c.b;
     order_rgba(v ).A:=c.a;

     repeat
      p^:=v;

      inc(ptrcomp(p ) ,sizeof(unsigned ) );
      dec(len );

     until len = 0;

    end
   else
    if cover = 255 then
     repeat
      blend_pix_rgba_pre(p ,c.r ,c.g ,c.b ,alpha );

      inc(ptrcomp(p ) ,sizeof(unsigned ) );
      dec(len );

     until len = 0
    else
     repeat
      blend_pix_rgba_pre(p ,c.r ,c.g ,c.b ,alpha ,cover );

      inc(ptrcomp(p ) ,sizeof(unsigned ) );
      dec(len );

     until len = 0;

  end;

end;

{ rgba32_pre_blend_vline }
procedure rgba32_pre_blend_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
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
     order_rgba(v ).R:=c.r;
     order_rgba(v ).G:=c.g;
     order_rgba(v ).B:=c.b;
     order_rgba(v ).A:=c.a;

     repeat
      p^:=v;
      p :=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

     until len = 0;

    end
   else
    if cover = 255 then
     repeat
      blend_pix_rgba_pre(p ,c.r ,c.g ,c.b ,alpha );

      p:=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

      dec(len );

     until len = 0
    else
     repeat
      blend_pix_rgba_pre(p ,c.r ,c.g ,c.b ,alpha ,cover );

      p:=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

      dec(len );

     until len = 0;

  end;

end;

{ rgba32_pre_blend_solid_hspan }
procedure rgba32_pre_blend_solid_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
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
      order_rgba(p^ ).R:=c.r;
      order_rgba(p^ ).G:=c.g;
      order_rgba(p^ ).B:=c.b;
      order_rgba(p^ ).A:=base_mask;

     end
    else
     blend_pix_rgba_pre(p ,c.r ,c.g ,c.b ,alpha ,covers^ );

    inc(ptrcomp(p ) ,4 );
    inc(ptrcomp(covers ) );

    dec(len );

   until len = 0;

  end;

end;

{ rgba32_pre_blend_solid_vspan }
procedure rgba32_pre_blend_solid_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
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
      order_rgba(p^ ).R:=c.r;
      order_rgba(p^ ).G:=c.g;
      order_rgba(p^ ).B:=c.b;
      order_rgba(p^ ).A:=base_mask;

     end
    else
     blend_pix_rgba_pre(p ,c.r ,c.g ,c.b ,alpha ,covers^ );

    p:=unsigned_ptr(this.m_rbuf.next_row(int8u_ptr(p ) ) );

    inc(ptrcomp(covers ) ,sizeof(int8u ) );
    dec(len );

   until len = 0;

  end;

end;

{ rgba32_pre_blend_color_hspan }
procedure rgba32_pre_blend_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

 if covers <> NIL then
  repeat
   copy_or_blend_pix_rgba_pre(p ,colors.r ,colors.g ,colors.b ,colors.a ,covers^ );

   inc(ptrcomp(covers ) ,sizeof(int8u ) );
   inc(ptrcomp(p ) ,4 );
   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    copy_or_blend_pix_rgba_pre(p ,colors.r ,colors.g ,colors.b ,colors.a );

    inc(ptrcomp(p ) ,4 );
    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    dec(len );

   until len = 0
  else
   repeat
    copy_or_blend_pix_rgba_pre(p ,colors.r ,colors.g ,colors.b ,colors.a ,cover );

    inc(ptrcomp(p ) ,4 );
    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    dec(len );

   until len = 0;

end;

{ rgba32_pre_blend_color_vspan }
procedure rgba32_pre_blend_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row(y ) ) + (x shl 2 ) );

 if covers <> NIL then
  repeat
   copy_or_blend_pix_rgba_pre(
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
    copy_or_blend_pix_rgba_pre(
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
    copy_or_blend_pix_rgba_pre(
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

{ rgba32_pre_blend_from }
procedure rgba32_pre_blend_from(this : pixel_formats_ptr; from : pixel_formats_ptr; psrc_ : int8u_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
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
   copy_or_blend_pix_rgba_pre(
    pdst ,
    order_rgba(psrc^ ).R ,
    order_rgba(psrc^ ).G ,
    order_rgba(psrc^ ).B ,
    order_rgba(psrc^ ).A );

   inc(ptrcomp(psrc ) ,incp );
   inc(ptrcomp(pdst ) ,incp );
   dec(len );

  until len = 0
 else
  repeat
   copy_or_blend_pix_rgba_pre(
    pdst ,
    order_rgba(psrc^ ).R ,
    order_rgba(psrc^ ).G ,
    order_rgba(psrc^ ).B ,
    order_rgba(psrc^ ).A ,
    cover );

   inc(ptrcomp(psrc ) ,incp );
   inc(ptrcomp(pdst ) ,incp );
   dec(len );

  until len = 0;

end;

{ rgba32_pre_blend_from_color }
procedure rgba32_pre_blend_from_color(this : pixel_formats_ptr; from : pixel_formats_ptr; color : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
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
    copy_or_blend_pix_rgba_pre(
     pdst ,color.r, color.g, color.b, color.a,
     shr_int32(psrc^ * cover + base_mask ,base_shift ) );

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,4 );
    dec(len );

   until len = 0;

  end;

end;

{ rgba32_pre_blend_from_lut }
procedure rgba32_pre_blend_from_lut(this : pixel_formats_ptr; from : pixel_formats_ptr; color_lut : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
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

     copy_or_blend_pix_rgba_pre(pdst ,color.r, color.g, color.b, color.a );

     inc(ptrcomp(psrc ) ,ppsz );
     inc(ptrcomp(pdst ) ,4 );
     dec(len );

    until len = 0
   else
    repeat
     color:=aggclr_ptr(ptrcomp(color_lut ) + psrc^ * sizeof(aggclr ) );

     copy_or_blend_pix_rgba_pre(pdst ,color.r, color.g, color.b, color.a ,cover );

     inc(ptrcomp(psrc ) ,ppsz );
     inc(ptrcomp(pdst ) ,4 );
     dec(len );

    until len = 0;

  end;

end;

