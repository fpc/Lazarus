//
// AggPas 2.4 RM3 pixel format definition file
//
{ blend_pix_gray }
procedure blend_pix_gray(p : int8u_ptr; cv ,alpha : unsigned; cover : unsigned = 0 );
begin
 p^:=int8u((((cv - p^ ) * alpha ) + (p^ shl base_shift ) ) shr base_shift );

end;

{ copy_or_blend_pix_gray }
procedure copy_or_blend_pix_gray(p : int8u_ptr; c : aggclr_ptr; cover : unsigned ); overload;
var
 alpha : unsigned;

begin
 if  c.a <> 0 then
  begin
   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    p^:=c.v
   else
    blend_pix_gray(p ,c.v ,alpha ,cover );

  end;

end;

{ copy_or_blend_pix_gray }
procedure copy_or_blend_pix_gray(p : int8u_ptr; c : aggclr_ptr ); overload;
begin
 if c.a <> 0 then
  if c.a = base_mask then
   p^:=c.v
  else
   blend_pix_gray(p ,c.v ,c.a );

end;

{ gray8_copy_pixel }
procedure gray8_copy_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr );
begin
 int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset )^:=c.v;

end;

{ gray8_blend_pixel }
procedure gray8_blend_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr; cover : int8u );
begin
 copy_or_blend_pix_gray(
  int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset ) ,
  c ,cover );

end;

{ gray8_pixel }
function gray8_pixel(this : pixel_formats_ptr; x ,y : int ) : aggclr;
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

 result.ConstrInt(p^ );

end;

{ gray8_copy_hline }
procedure gray8_copy_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

 repeat
  p^:=c.v;

  inc(ptrcomp(p ) ,this.m_step );
  dec(len );

 until len = 0;

end;

{ gray8_copy_vline }
procedure gray8_copy_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

 repeat
  p^:=c.v;
  p :=this.m_rbuf.next_row(p );

  dec(len );

 until len = 0;

end;

{ gray8_blend_hline }
procedure gray8_blend_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : int8u_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    repeat
     p^:=c.v;

     inc(ptrcomp(p ) ,this.m_step );
     dec(len );

    until len = 0
   else
    repeat
     blend_pix_gray(p ,c.v ,alpha ,cover );

     inc(ptrcomp(p ) ,this.m_step );
     dec(len );

    until len = 0;

  end;

end;

{ gray8_blend_vline }
procedure gray8_blend_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : int8u_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    repeat
     p^:=c.v;
     p :=this.m_rbuf.next_row(p );

     dec(len );

    until len = 0
   else
    repeat
     blend_pix_gray(p ,c.v ,alpha ,cover );

     p:=this.m_rbuf.next_row(p );

     dec(len );

    until len = 0;

  end;

end;

{ gray8_blend_solid_hspan }
procedure gray8_blend_solid_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : int8u_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

   repeat
    alpha:=(c.a * (covers^ + 1 ) ) shr 8;

    if alpha = base_mask then
     p^:=c.v
    else
     blend_pix_gray(p ,c.v ,alpha ,covers^ );

    inc(ptrcomp(p ) ,this.m_step );
    inc(ptrcomp(covers ) );
    dec(len );

   until len = 0;

  end;

end;

{ gray8_blend_solid_vspan }
procedure gray8_blend_solid_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : int8u_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

   repeat
    alpha:=(c.a * (covers^ + 1 ) ) shr 8;

    if alpha = base_mask then
     p^:=c.v
    else
     blend_pix_gray(p ,c.v ,alpha ,covers^ );

    p:=this.m_rbuf.next_row(p );

    inc(ptrcomp(covers ) ,sizeof(int8u ) );
    dec(len );

   until len = 0;

  end;

end;

{ gray8_blend_color_hspan }
procedure gray8_blend_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

 if covers <> NIL then
  repeat
   copy_or_blend_pix_gray(p ,colors ,covers^ );

   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   inc(ptrcomp(covers ) ,sizeof(int8u ) );
   inc(ptrcomp(p ) ,this.m_step );
   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    if colors.a = base_mask then
     p^:=colors.v
    else
     copy_or_blend_pix_gray(p ,colors );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    inc(ptrcomp(p ) ,this.m_step );
    dec(len );

   until len = 0
  else
   repeat
    copy_or_blend_pix_gray(p ,colors ,cover );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    inc(ptrcomp(p ) ,this.m_step );
    dec(len );

   until len = 0;

end;

{ gray8_blend_color_vspan }
procedure gray8_blend_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

 if covers <> NIL then
  repeat
   copy_or_blend_pix_gray(p ,colors ,covers^ );

   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   inc(ptrcomp(covers ) ,sizeof(int8u ) );

   p:=this.m_rbuf.next_row(p );

   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    if colors.a = base_mask then
     p^:=colors.v
    else
     copy_or_blend_pix_gray(p ,colors );

    p:=this.m_rbuf.next_row(p );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    dec(len );

   until len = 0

  else
   repeat
    copy_or_blend_pix_gray(p ,colors ,cover );

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );

    p:=this.m_rbuf.next_row(p );

    dec(len );

   until len = 0;

end;

{ gray8_copy_from }
procedure gray8_copy_from(this : pixel_formats_ptr; from : rendering_buffer_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned );
begin
 move(
  int8u_ptr(ptrcomp(from.row(ysrc ) ) + xsrc * sizeof(int8u ) )^ ,
  int8u_ptr(ptrcomp(this.m_rbuf.row(ydst ) ) + xdst * sizeof(int8u ) )^ ,
  len * sizeof(int8u ) );

end;

{ gray8_copy_color_hspan }
procedure gray8_copy_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

 repeat
  p^:=colors.v;

  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  inc(ptrcomp(p ) ,this.m_step );
  dec(len );

 until len = 0;

end;

{ gray8_copy_color_vspan }
procedure gray8_copy_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
var
 p : int8u_ptr;

begin
 p:=int8u_ptr(ptrcomp(this.m_rbuf.row(y ) ) + x * this.m_step + this.m_offset );

 repeat
  p^:=colors.v;

  inc(ptrcomp(colors ) ,sizeof(aggclr ) );

  p:=this.m_rbuf.next_row(p );

  dec(len );

 until len = 0;

end;

{ gray8_blend_from_color }
procedure gray8_blend_from_color(this : pixel_formats_ptr; from : pixel_formats_ptr; color : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 ppsz : unsigned;

 psrc ,pdst : int8u_ptr;

begin
 ppsz:=from._pix_width;
 psrc:=from.row_ptr(ysrc );

 if psrc <> NIL then
  begin
   pdst:=int8u_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + xdst * this.m_step + this.m_offset );

   repeat
    copy_or_blend_pix_gray(
     pdst ,color ,
     shr_int32(psrc^ * cover + base_mask ,base_shift ) );

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,this.m_step );
    dec(len );

   until len = 0;

  end;

end;

{ gray8_blend_from_lut }
procedure gray8_blend_from_lut(this : pixel_formats_ptr; from : pixel_formats_ptr; color_lut : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 ppsz : unsigned;

 psrc ,pdst : int8u_ptr;

begin
 ppsz:=from._pix_width;
 psrc:=from.row_ptr(ysrc );

 if psrc <> NIL then
  begin
   pdst:=int8u_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + xdst * this.m_step + this.m_offset );

   repeat
    copy_or_blend_pix_gray(
     pdst ,aggclr_ptr(ptrcomp(color_lut ) + psrc^ * sizeof(aggclr ) ) ,cover );

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,this.m_step );
    dec(len );

   until len = 0;

  end;

end;

