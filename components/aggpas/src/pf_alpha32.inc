//
// AggPas 2.4 RM3 pixel format definition file
//
{ alpha32_copy_hline }
procedure alpha32_copy_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
var
 p : unsigned_ptr;
 v : unsigned;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,len ) ) + (x shl 2 ) );

 int8u_ptr(ptrcomp(@v ) + this.m_order.R )^:=c.r;
 int8u_ptr(ptrcomp(@v ) + this.m_order.G )^:=c.g;
 int8u_ptr(ptrcomp(@v ) + this.m_order.B )^:=c.b;
 int8u_ptr(ptrcomp(@v ) + this.m_order.A )^:=c.a;

 if len > 0 then
  repeat
   p^:=v;

   inc(ptrcomp(p ) ,4 );
   dec(len );

  until len = 0;

end;

{ alpha32_blend_solid_hspan }
procedure alpha32_blend_solid_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : unsigned_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,len ) ) + (x shl 2 ) );

   repeat
    alpha:=(c.a * (covers^ + 1 ) ) shr 8;

    if alpha = base_mask then
     begin
      int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=c.r;
      int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=c.g;
      int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=c.b;
      int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=base_mask;

     end
    else
     case this.m_order.R of
      0 : blend_pix_rgba(p ,c.r ,c.g ,c.b ,alpha ,covers^ );
      1 : blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha ,covers^ );
      2 : blend_pix_bgra(p ,c.r ,c.g ,c.b ,alpha ,covers^ );
      3 : blend_pix_abgr(p ,c.r ,c.g ,c.b ,alpha ,covers^ );

     end;

    inc(ptrcomp(p ) ,4 );
    inc(ptrcomp(covers ) );

    dec(len );

   until len = 0;

  end;

end;

{ alpha32_blend_color_hspan }
procedure alpha32_blend_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,len ) ) + (x shl 2 ) );

 if covers <> NIL then
  repeat
   case this.m_order.R of
    0 : copy_or_blend_pix_rgba(p ,colors.r ,colors.g ,colors.b ,colors.a ,covers^ );
    1 : copy_or_blend_pix_argb(p ,colors.r ,colors.g ,colors.b ,colors.a ,covers^ );
    2 : copy_or_blend_pix_bgra(p ,colors.r ,colors.g ,colors.b ,colors.a ,covers^ );
    3 : copy_or_blend_pix_abgr(p ,colors.r ,colors.g ,colors.b ,colors.a ,covers^ );

   end;

   inc(ptrcomp(covers ) ,sizeof(int8u ) );
   inc(ptrcomp(p ) ,4 );
   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    case this.m_order.R of
     0 : copy_or_blend_pix_rgba(p ,colors.r ,colors.g ,colors.b ,colors.a );
     1 : copy_or_blend_pix_argb(p ,colors.r ,colors.g ,colors.b ,colors.a );
     2 : copy_or_blend_pix_bgra(p ,colors.r ,colors.g ,colors.b ,colors.a );
     3 : copy_or_blend_pix_abgr(p ,colors.r ,colors.g ,colors.b ,colors.a );

    end;

    inc(ptrcomp(p ) ,4 );
    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    dec(len );

   until len = 0
  else
   repeat
    case this.m_order.R of
     0 : copy_or_blend_pix_rgba(p ,colors.r ,colors.g ,colors.b ,colors.a ,cover );
     1 : copy_or_blend_pix_argb(p ,colors.r ,colors.g ,colors.b ,colors.a ,cover );
     2 : copy_or_blend_pix_bgra(p ,colors.r ,colors.g ,colors.b ,colors.a ,cover );
     3 : copy_or_blend_pix_abgr(p ,colors.r ,colors.g ,colors.b ,colors.a ,cover );

    end;

    inc(ptrcomp(p ) ,4 );
    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    dec(len );

   until len = 0;

end;

{ alpha32_blend_from }
procedure alpha32_blend_from(this : pixel_formats_ptr; from : pixel_formats_ptr; psrc_ : int8u_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
var
 psrc ,pdst : unsigned_ptr;

 incp : int;

begin
 psrc:=unsigned_ptr(psrc_ );
 pdst:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(xdst ,ydst ,len ) ) + (xdst shl 2 ) * sizeof(int8u ) );
 incp:=4;

 if ptrcomp(xdst ) > ptrcomp(xsrc ) then
  begin
   inc(ptrcomp(psrc ) ,((len - 1 ) shl 2 ) * sizeof(int8u ) );
   inc(ptrcomp(pdst ) ,((len - 1 ) shl 2 ) * sizeof(int8u ) );

   incp:=-4;

  end;

 if cover = 255 then
  repeat
   case this.m_order.R of
    0 : copy_or_blend_pix_rgba(
         pdst ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^ );

    1 : copy_or_blend_pix_argb(
         pdst ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^ );

    2 : copy_or_blend_pix_bgra(
         pdst ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^ );

    3 : copy_or_blend_pix_abgr(
         pdst ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^ );

   end;

   inc(ptrcomp(psrc ) ,incp );
   inc(ptrcomp(pdst ) ,incp );
   dec(len );

  until len = 0
 else
  repeat
   case this.m_order.R of
    0 : copy_or_blend_pix_rgba(
         pdst ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^ ,
         cover );

    1 : copy_or_blend_pix_argb(
         pdst ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^ ,
         cover );

    2 : copy_or_blend_pix_bgra(
         pdst ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^ ,
         cover );

    3 : copy_or_blend_pix_abgr(
         pdst ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.R )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.G )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.B )^ ,
         int8u_ptr(ptrcomp(psrc ) + from.m_order.A )^ ,
         cover );

   end;

   inc(ptrcomp(psrc ) ,incp );
   inc(ptrcomp(pdst ) ,incp );
   dec(len );

  until len = 0;

end;

{ alpha32_pixel }
function alpha32_pixel(this : pixel_formats_ptr; x ,y : int ) : aggclr;
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(this.m_rbuf.row(y ) );

 if p <> NIL then
  begin
   inc(ptrcomp(p ) ,x shl 2 );

   result.ConstrInt(
    int8u_ptr(ptrcomp(p ) + this.m_order.R )^ ,
    int8u_ptr(ptrcomp(p ) + this.m_order.G )^ ,
    int8u_ptr(ptrcomp(p ) + this.m_order.B )^ ,
    int8u_ptr(ptrcomp(p ) + this.m_order.A )^ );

  end
 else
  result.Construct;

end;

{ alpha32_copy_pixel }
procedure alpha32_copy_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr );
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) );

 int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=c.r;
 int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=c.g;
 int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=c.b;
 int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=c.a;

end;

{ alpha32_blend_pixel }
procedure alpha32_blend_pixel(this : pixel_formats_ptr; x ,y : int; c : aggclr_ptr; cover : int8u );
begin
 case this.m_order.R of
  0 : copy_or_blend_pix_rgba(
       unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) ) ,
       c.r ,c.g ,c.b ,c.a ,cover );

  1 : copy_or_blend_pix_argb(
       unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) ) ,
       c.r ,c.g ,c.b ,c.a ,cover );

  2 : copy_or_blend_pix_bgra(
       unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) ) ,
       c.r ,c.g ,c.b ,c.a ,cover );

  3 : copy_or_blend_pix_abgr(
       unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) ) ,
       c.r ,c.g ,c.b ,c.a ,cover );

 end;

end;

{ alpha32_copy_vline }
procedure alpha32_copy_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr );
var
 p : unsigned_ptr;
 v : unsigned;

begin
 int8u_ptr(ptrcomp(@v ) + this.m_order.R )^:=c.r;
 int8u_ptr(ptrcomp(@v ) + this.m_order.G )^:=c.g;
 int8u_ptr(ptrcomp(@v ) + this.m_order.B )^:=c.b;
 int8u_ptr(ptrcomp(@v ) + this.m_order.A )^:=c.a;

 if len > 0 then
  repeat
   p :=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) );
   p^:=v;

   inc(y );
   dec(len );

  until len = 0;

end;

{ alpha32_blend_hline }
procedure alpha32_blend_hline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : unsigned_ptr;
 v : unsigned;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,len ) ) + (x shl 2 ) );

   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    begin
     int8u_ptr(ptrcomp(@v ) + this.m_order.R )^:=c.r;
     int8u_ptr(ptrcomp(@v ) + this.m_order.G )^:=c.g;
     int8u_ptr(ptrcomp(@v ) + this.m_order.B )^:=c.b;
     int8u_ptr(ptrcomp(@v ) + this.m_order.A )^:=c.a;

     repeat
      p^:=v;

      inc(ptrcomp(p ) ,sizeof(unsigned ) );
      dec(len );

     until len = 0;

    end
   else
    if cover = 255 then
     repeat
      case this.m_order.R of
       0 : blend_pix_rgba(p ,c.r ,c.g ,c.b ,alpha );
       1 : blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha );
       2 : blend_pix_bgra(p ,c.r ,c.g ,c.b ,alpha );
       3 : blend_pix_abgr(p ,c.r ,c.g ,c.b ,alpha );

      end;

      inc(ptrcomp(p ) ,sizeof(unsigned ) );
      dec(len );

     until len = 0
    else
     repeat
      case this.m_order.R of
       0 : blend_pix_rgba(p ,c.r ,c.g ,c.b ,alpha ,cover );
       1 : blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha ,cover );
       2 : blend_pix_bgra(p ,c.r ,c.g ,c.b ,alpha ,cover );
       3 : blend_pix_abgr(p ,c.r ,c.g ,c.b ,alpha ,cover );

      end;

      inc(ptrcomp(p ) ,sizeof(unsigned ) );
      dec(len );

     until len = 0;

  end;

end;

{ alpha32_blend_vline }
procedure alpha32_blend_vline(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
var
 p : unsigned_ptr;
 v : unsigned;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   alpha:=(c.a * (cover + 1 ) ) shr 8;

   if alpha = base_mask then
    begin
     int8u_ptr(ptrcomp(@v ) + this.m_order.R )^:=c.r;
     int8u_ptr(ptrcomp(@v ) + this.m_order.G )^:=c.g;
     int8u_ptr(ptrcomp(@v ) + this.m_order.B )^:=c.b;
     int8u_ptr(ptrcomp(@v ) + this.m_order.A )^:=c.a;

     repeat
      p :=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) );
      p^:=v;

      inc(y );
      dec(len );

     until len = 0;

    end
   else
    if cover = 255 then
     repeat
      p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) );

      case this.m_order.R of
       0 : blend_pix_rgba(p ,c.r ,c.g ,c.b ,alpha );
       1 : blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha );
       2 : blend_pix_bgra(p ,c.r ,c.g ,c.b ,alpha );
       3 : blend_pix_abgr(p ,c.r ,c.g ,c.b ,alpha );

      end;

      inc(y );
      dec(len );

     until len = 0
    else
     repeat
      p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) );

      case this.m_order.R of
       0 : blend_pix_rgba(p ,c.r ,c.g ,c.b ,alpha ,cover );
       1 : blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha ,cover );
       2 : blend_pix_bgra(p ,c.r ,c.g ,c.b ,alpha ,cover );
       3 : blend_pix_abgr(p ,c.r ,c.g ,c.b ,alpha ,cover );

      end;

      inc(y );
      dec(len );

     until len = 0;

  end;

end;

{ alpha32_blend_solid_vspan }
procedure alpha32_blend_solid_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
var
 p : unsigned_ptr;

 alpha : unsigned;

begin
 if c.a <> 0 then
  begin
   repeat
    p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) );

    alpha:=(c.a * (covers^ + 1 ) ) shr 8;

    if alpha = base_mask then
     begin
      int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=c.r;
      int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=c.g;
      int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=c.b;
      int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=base_mask;

     end
    else
     case this.m_order.R of
      0 : blend_pix_rgba(p ,c.r ,c.g ,c.b ,alpha ,covers^ );
      1 : blend_pix_argb(p ,c.r ,c.g ,c.b ,alpha ,covers^ );
      2 : blend_pix_bgra(p ,c.r ,c.g ,c.b ,alpha ,covers^ );
      3 : blend_pix_abgr(p ,c.r ,c.g ,c.b ,alpha ,covers^ );

     end;

    inc(ptrcomp(covers ) ,sizeof(int8u ) );
    inc(y );
    dec(len );

   until len = 0;

  end;

end;

{ alpha32_blend_color_vspan }
procedure alpha32_blend_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
var
 p : unsigned_ptr;

begin
 if covers <> NIL then
  repeat
   p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) );

   case this.m_order.R of
    0 : copy_or_blend_pix_rgba(
         p ,
         colors.r ,
         colors.g ,
         colors.b ,
         colors.a ,
         covers^ );

    1 : copy_or_blend_pix_argb(
         p ,
         colors.r ,
         colors.g ,
         colors.b ,
         colors.a ,
         covers^ );

    2 : copy_or_blend_pix_bgra(
         p ,
         colors.r ,
         colors.g ,
         colors.b ,
         colors.a ,
         covers^ );

    3 : copy_or_blend_pix_abgr(
         p ,
         colors.r ,
         colors.g ,
         colors.b ,
         colors.a ,
         covers^ );

   end;

   inc(ptrcomp(covers ) ,sizeof(int8u ) );
   inc(ptrcomp(colors ) ,sizeof(aggclr ) );
   inc(y );
   dec(len );

  until len = 0
 else
  if cover = 255 then
   repeat
    p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) );

    case this.m_order.R of
     0 : copy_or_blend_pix_rgba(
          p ,
          colors.r ,
          colors.g ,
          colors.b ,
          colors.a );

     1 : copy_or_blend_pix_argb(
          p ,
          colors.r ,
          colors.g ,
          colors.b ,
          colors.a );

     2 : copy_or_blend_pix_bgra(
          p ,
          colors.r ,
          colors.g ,
          colors.b ,
          colors.a );

     3 : copy_or_blend_pix_abgr(
          p ,
          colors.r ,
          colors.g ,
          colors.b ,
          colors.a );

    end;

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    inc(y );
    dec(len );

   until len = 0
  else
   repeat
    p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) );

    case this.m_order.R of
     0 : copy_or_blend_pix_rgba(
          p ,
          colors.r ,
          colors.g ,
          colors.b ,
          colors.a ,
          cover );

     1 : copy_or_blend_pix_argb(
          p ,
          colors.r ,
          colors.g ,
          colors.b ,
          colors.a ,
          cover );

     2 : copy_or_blend_pix_bgra(
          p ,
          colors.r ,
          colors.g ,
          colors.b ,
          colors.a ,
          cover );

     3 : copy_or_blend_pix_abgr(
          p ,
          colors.r ,
          colors.g ,
          colors.b ,
          colors.a ,
          cover );

    end;

    inc(ptrcomp(colors ) ,sizeof(aggclr ) );
    inc(y );
    dec(len );

   until len = 0;

end;

{ alpha32_copy_color_hspan }
procedure alpha32_copy_color_hspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
var
 p : unsigned_ptr;

begin
 p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,len ) ) + (x shl 2 ) );

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

{ alpha32_copy_color_vspan }
procedure alpha32_copy_color_vspan(this : pixel_formats_ptr; x ,y : int; len : unsigned; colors : aggclr_ptr );
var
 p : unsigned_ptr;

begin
 repeat
  p:=unsigned_ptr(ptrcomp(this.m_rbuf.row_xy(x ,y ,1 ) ) + (x shl 2 ) );

  int8u_ptr(ptrcomp(p ) + this.m_order.R )^:=colors.r;
  int8u_ptr(ptrcomp(p ) + this.m_order.G )^:=colors.g;
  int8u_ptr(ptrcomp(p ) + this.m_order.B )^:=colors.b;
  int8u_ptr(ptrcomp(p ) + this.m_order.A )^:=colors.a;

  inc(ptrcomp(colors ) ,sizeof(aggclr ) );
  inc(y );
  dec(len );

 until len = 0;

end;

{ alpha32_blend_from_color }
procedure alpha32_blend_from_color(this : pixel_formats_ptr; from : pixel_formats_ptr; color : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
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
    case this.m_order.R of
     0 : copy_or_blend_pix_rgba(
          pdst ,color.r, color.g, color.b, color.a,
          shr_int32(psrc^ * cover + base_mask ,base_shift ) );

     1 : copy_or_blend_pix_argb(
          pdst ,color.r, color.g, color.b, color.a,
          shr_int32(psrc^ * cover + base_mask ,base_shift ) );

     2 : copy_or_blend_pix_bgra(
          pdst ,color.r, color.g, color.b, color.a,
          shr_int32(psrc^ * cover + base_mask ,base_shift ) );

     3 : copy_or_blend_pix_abgr(
          pdst ,color.r, color.g, color.b, color.a,
          shr_int32(psrc^ * cover + base_mask ,base_shift ) );

    end;

    inc(ptrcomp(psrc ) ,ppsz );
    inc(ptrcomp(pdst ) ,4 );
    dec(len );

   until len = 0;

  end;

end;

{ alpha32_blend_from_lut }
procedure alpha32_blend_from_lut(this : pixel_formats_ptr; from : pixel_formats_ptr; color_lut : aggclr_ptr; xdst ,ydst ,xsrc ,ysrc : int; len : unsigned; cover : int8u );
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

     case this.m_order.R of
      0 : copy_or_blend_pix_rgba(pdst ,color.r, color.g, color.b, color.a );
      1 : copy_or_blend_pix_argb(pdst ,color.r, color.g, color.b, color.a );
      2 : copy_or_blend_pix_bgra(pdst ,color.r, color.g, color.b, color.a );
      3 : copy_or_blend_pix_abgr(pdst ,color.r, color.g, color.b, color.a );

     end;

     inc(ptrcomp(psrc ) ,ppsz );
     inc(ptrcomp(pdst ) ,4 );
     dec(len );

    until len = 0
   else
    repeat
     color:=aggclr_ptr(ptrcomp(color_lut ) + psrc^ * sizeof(aggclr ) );

     case this.m_order.R of
      0 : copy_or_blend_pix_rgba(pdst ,color.r, color.g, color.b, color.a ,cover );
      1 : copy_or_blend_pix_argb(pdst ,color.r, color.g, color.b, color.a ,cover );
      2 : copy_or_blend_pix_bgra(pdst ,color.r, color.g, color.b, color.a ,cover );
      3 : copy_or_blend_pix_abgr(pdst ,color.r, color.g, color.b, color.a ,cover );

     end;

     inc(ptrcomp(psrc ) ,ppsz );
     inc(ptrcomp(pdst ) ,4 );
     dec(len );

    until len = 0;

  end;

end;

