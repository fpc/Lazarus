//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2005-2007
//
// Permission to copy, use, modify, sell and distribute this software
// is granted provided this copyright notice appears in all copies.
// This software is provided "as is" without express or implied
// warranty, and with no claim as to its suitability for any purpose.
//
//----------------------------------------------------------------------------
// Contact: mcseem@antigrain.com
//          mcseemagg@yahoo.com
//          http://www.antigrain.com
//
//----------------------------------------------------------------------------
//
// Class renderer_base
//
// [Pascal Port History] -----------------------------------------------------
//
// 19.10.2007-Milano: blend_from_color & blend_from_lut
// 23.06.2006-Milano: ptrcomp adjustments
// 03.02.2006-Milano: blend_color_h(v)span
// 24.11.2005-Milano: renderer_base.blends...
// 28.09.2005-Milano: Unit port establishment
//
{ agg_renderer_base.pas }
unit
 agg_renderer_base ;

INTERFACE

{$I agg_mode.inc }

uses
 agg_basics ,
 agg_rendering_buffer ,
 agg_pixfmt ,
 agg_color ;

{ TYPES DEFINITION }
type
 renderer_base_ptr = ^renderer_base;
 renderer_base = object
   m_ren      : pixel_formats_ptr;
   m_clip_box : rect;

   constructor Construct(ren_ : pixel_formats_ptr );

   function  ren    : pixel_formats_ptr;
   function  width  : unsigned;
   function  height : unsigned;

   function  clip_box_     (x1 ,y1 ,x2 ,y2 : int ) : boolean;
   procedure reset_clipping(visibility : boolean ); virtual;
   procedure clip_box_naked(x1 ,y1 ,x2 ,y2 : int );

   function  inbox(x ,y : int ) : boolean;

   procedure first_clip_box; virtual;
   function  next_clip_box : boolean; virtual;

   function  _clip_box : rect_ptr;
   function  _xmin : int;
   function  _ymin : int;
   function  _xmax : int;
   function  _ymax : int;

   function  bounding_clip_box : rect_ptr; virtual;
   function  bounding_xmin : int; virtual;
   function  bounding_ymin : int; virtual;
   function  bounding_xmax : int; virtual;
   function  bounding_ymax : int; virtual;

   procedure clear(c : aggclr_ptr );

   procedure copy_pixel (x ,y : int; c : aggclr_ptr ); virtual;
   procedure blend_pixel(x ,y : int; c : aggclr_ptr; cover : int8u ); virtual;
   function  pixel      (x ,y : int ) : aggclr; virtual;

   procedure copy_hline(x1 ,y ,x2 : int; c : aggclr_ptr ); virtual;
   procedure copy_vline(x ,y1 ,y2 : int; c : aggclr_ptr ); virtual;

   procedure blend_hline(x1 ,y ,x2 : int; c : aggclr_ptr; cover : int8u ); virtual;
   procedure blend_vline(x ,y1 ,y2 : int; c : aggclr_ptr; cover : int8u ); virtual;

   procedure copy_bar (x1 ,y1 ,x2 ,y2 : int; c : aggclr_ptr ); virtual;
   procedure blend_bar(x1 ,y1 ,x2 ,y2 : int; c : aggclr_ptr; cover : int8u ); virtual;

   function  span(x ,y : int; len : unsigned ) : pointer;

   procedure blend_solid_hspan(x ,y ,len : int; c : aggclr_ptr; covers : int8u_ptr ); virtual;
   procedure blend_solid_vspan(x ,y ,len : int; c : aggclr_ptr; covers : int8u_ptr ); virtual;

   procedure copy_color_hspan (x ,y ,len : int; colors : aggclr_ptr ); virtual;
   procedure blend_color_hspan(x ,y ,len : int; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u = cover_full ); virtual;
   procedure blend_color_vspan(x ,y ,len : int; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u = cover_full ); virtual;

   procedure copy_color_hspan_no_clip (x ,y ,len : int; colors : aggclr_ptr );
   procedure blend_color_hspan_no_clip(x ,y ,len : int; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u = cover_full );
   procedure blend_color_vspan_no_clip(x ,y ,len : int; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u = cover_full );

   function  clip_rect_area(dst ,src : rect_ptr; wsrc ,hsrc : int ) : rect;

   procedure copy_from (src : rendering_buffer_ptr; rect_src_ptr : rect_ptr = NIL; dx : int = 0; dy : int = 0 ); virtual;
   procedure blend_from(src : pixel_formats_ptr; rect_src_ptr : rect_ptr = NIL; dx : int = 0; dy : int = 0; cover : int8u = cover_full ); virtual;

   procedure blend_from_color(
              src : pixel_formats_ptr;
              color : aggclr_ptr;
              rect_src_ptr : rect_ptr = NIL;
              dx : int = 0;
              dy : int = 0;
              cover : int8u = cover_full );

   procedure blend_from_lut(
              src : pixel_formats_ptr;
              color_lut : aggclr_ptr;
              rect_src_ptr : rect_ptr = NIL;
              dx : int = 0;
              dy : int = 0;
              cover : int8u = cover_full );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor renderer_base.Construct;
var
 w ,h : int;

begin
 m_ren:=ren_;

 if (ren_ <> NIL ) and
    (ren_._width > 0 ) then
  w:=ren_._width - 1
 else
  w:=0;

 if (ren_ <> NIL ) and
    (ren_._height > 0 ) then
  h:=ren_._height - 1
 else
  h:=0;

 m_clip_box.Construct(0 ,0 ,w ,h );

end;

{ REN }
function renderer_base.ren;
begin
 result:=m_ren;
 
end;

{ WIDTH }
function renderer_base.width;
begin
 result:=m_ren._width;

end;

{ HEIGHT }
function renderer_base.height;
begin
 result:=m_ren._height;

end;

{ CLIP_BOX_NAKED }
procedure renderer_base.clip_box_naked;
begin
 m_clip_box.x1:=x1;
 m_clip_box.y1:=y1;
 m_clip_box.x2:=x2;
 m_clip_box.y2:=y2;

end;

{ INBOX }
function renderer_base.inbox;
begin
 result:=
  (x >= m_clip_box.x1 ) and
  (y >= m_clip_box.y1 ) and
  (x <= m_clip_box.x2 ) and
  (y <= m_clip_box.y2 );

end;

{ CLIP_BOX_ }
function renderer_base.clip_box_;
var
 cb ,
 rc : rect;

begin
 cb.Construct(x1 ,y1 ,x2 ,y2 );
 cb.normalize;

 rc.Construct(0 ,0 ,width - 1 ,height - 1 );

 if cb.clip(@rc ) then
  begin
   m_clip_box:=cb;

   result:=true;

   exit;

  end;

 m_clip_box.x1:=1;
 m_clip_box.y1:=1;
 m_clip_box.x2:=0;
 m_clip_box.y2:=0;

 result:=false;

end;

{ RESET_CLIPPING }
procedure renderer_base.reset_clipping;
begin
 if visibility then
  begin
   m_clip_box.x1:=0;
   m_clip_box.y1:=0;
   m_clip_box.x2:=width - 1;
   m_clip_box.y2:=height - 1;

  end
 else
  begin
   m_clip_box.x1:=1;
   m_clip_box.y1:=1;
   m_clip_box.x2:=0;
   m_clip_box.y2:=0;

  end;

end;

{ FIRST_CLIP_BOX }
procedure renderer_base.first_clip_box;
begin
end;

{ NEXT_CLIP_BOX }
function renderer_base.next_clip_box;
begin
 result:=false;

end;

{ _CLIP_BOX }
function renderer_base._clip_box;
begin
 result:=@m_clip_box;

end;

{ _XMIN }
function renderer_base._xmin;
begin
 result:=m_clip_box.x1;

end;

{ _YMIN }
function renderer_base._ymin;
begin
 result:=m_clip_box.y1;

end;

{ _XMAX }
function renderer_base._xmax;
begin
 result:=m_clip_box.x2;

end;

{ _YMAX }
function renderer_base._ymax;
begin
 result:=m_clip_box.y2;

end;

{ BOUNDING_CLIP_BOX }
function renderer_base.bounding_clip_box;
begin
 result:=@m_clip_box;

end;

{ BOUNDING_XMIN }
function renderer_base.bounding_xmin;
begin
 result:=m_clip_box.x1;

end;

{ BOUNDING_YMIN }
function renderer_base.bounding_ymin;
begin
 result:=m_clip_box.y1;

end;

{ BOUNDING_XMAX }
function renderer_base.bounding_xmax;
begin
 result:=m_clip_box.x2;

end;

{ BOUNDING_YMAX }
function renderer_base.bounding_ymax;
begin
 result:=m_clip_box.y2;

end;

{ CLEAR }
procedure renderer_base.clear;
var
 y : unsigned;

begin
 if (width > 0 ) and
    (height > 0 ) then
  for y:=0 to height - 1 do
   m_ren.copy_hline(m_ren ,0 ,y ,width ,c );

end;

{ COPY_PIXEL }
procedure renderer_base.copy_pixel;
begin
 if inbox(x ,y ) then
  m_ren.copy_pixel(m_ren ,x ,y ,c );

end;

{ BLEND_PIXEL }
procedure renderer_base.blend_pixel;
begin
 if inbox(x ,y ) then
  m_ren.blend_pixel(m_ren ,x ,y ,c ,cover );

end;

{ PIXEL }
function renderer_base.pixel;
begin
 if inbox(x ,y ) then
  result:=m_ren.pixel(m_ren ,x ,y )
 else
  result.Construct;

end;

{ COPY_HLINE }
procedure renderer_base.copy_hline;
var
 t : int;

begin
 if x1 > x2 then
  begin
   t :=x2;
   x2:=x1;
   x1:=t;

  end;

 if y > _ymax then
  exit;

 if y < _ymin then
  exit;

 if x1 > _xmax then
  exit;

 if x2 < _xmin then
  exit;

 if x1 < _xmin then
  x1:=_xmin;

 if x2 > _xmax then
  x2:=_xmax;

 m_ren.copy_hline(m_ren ,x1 ,y ,x2 - x1 + 1 ,c );

end;

{ COPY_VLINE }
procedure renderer_base.copy_vline;
var
 t : int;

begin
 if y1 > y2 then
  begin
   t :=y2;
   y2:=y1;
   y1:=t;

  end;

 if x > _xmax then
  exit;

 if x < _xmin then
  exit;

 if y1 > _ymax then
  exit;

 if y2 < _ymin then
  exit;

 if y1 < _ymin then
  y1:=_ymin;

 if y2 > _ymax then
  y2:=_ymax;

 m_ren.copy_vline(m_ren ,x ,y1 ,y2 - y1 + 1 ,c );

end;

{ BLEND_HLINE }
procedure renderer_base.blend_hline;
var
 t : int;

begin
 if x1 > x2 then
  begin
   t :=x2;
   x2:=x1;
   x1:=t;

  end;

 if y  > _ymax then
  exit;

 if y  < _ymin then
  exit;

 if x1 > _xmax then
  exit;

 if x2 < _xmin then
  exit;

 if x1 < _xmin then
  x1:=_xmin;

 if x2 > _xmax then
  x2:=_xmax;

 m_ren.blend_hline(m_ren ,x1 ,y ,x2 - x1 + 1 ,c ,cover );

end;

{ BLEND_VLINE }
procedure renderer_base.blend_vline;
var
 t : int;

begin
 if y1 > y2 then
  begin
   t :=y2;
   y2:=y1;
   y1:=t;

  end;

 if x  > _xmax then
  exit;

 if x  < _xmin then
  exit;

 if y1 > _ymax then
  exit;

 if y2 < _ymin then
  exit;

 if y1 < _ymin then
  y1:=_ymin;

 if y2 > _ymax then
  y2:=_ymax;

 m_ren.blend_vline(m_ren ,x ,y1 ,y2 - y1 + 1 ,c ,cover );

end;

{ COPY_BAR }
procedure renderer_base.copy_bar;
var
 y  : int;
 rc : rect;

begin
 rc.Construct(x1 ,y1 ,x2 ,y2 );
 rc.normalize;

 if rc.clip(_clip_box ) then
  begin
   y:=rc.y1;

   while y <= rc.y2 do
    begin
     m_ren.copy_hline(m_ren ,rc.x1 ,y ,rc.x2 - rc.x1 + 1 ,c );

     inc(y );

    end;

  end;

end;

{ BLEND_BAR }
procedure renderer_base.blend_bar;
var
 rc : rect;
 y  : int;

begin
 rc.Construct(x1 ,y1 ,x2 ,y2 );
 rc.normalize;

 if rc.clip(_clip_box ) then
  begin
   y:=rc.y1;

   while y <= rc.y2 do
    begin
     m_ren.blend_hline(m_ren ,rc.x1 ,y ,unsigned(rc.x2 - rc.x1 + 1 ) ,c ,cover );

     inc(y );

    end;

  end;

end;

{ SPAN {not_implemented}
function renderer_base.span;
begin
end;

{ BLEND_SOLID_HSPAN }
procedure renderer_base.blend_solid_hspan;
begin
 if y > _ymax then
  exit;

 if y < _ymin then
  exit;

 if x < _xmin then
  begin
   dec(len ,_xmin - x );

   if len <= 0 then
    exit;

   inc(ptrcomp(covers ) ,(_xmin - x ) * sizeof(int8u ) );

   x:=_xmin;

  end;

 if x + len > _xmax then
  begin
   len:=_xmax - x + 1;

   if len <= 0 then
    exit;

  end;

 m_ren.blend_solid_hspan(m_ren ,x ,y ,len ,c ,covers );

end;

{ BLEND_SOLID_VSPAN }
procedure renderer_base.blend_solid_vspan;
begin
 if x > _xmax then
  exit;

 if x < _xmin then
  exit;

 if y < _ymin then
  begin
   dec(len ,_ymin - y );

   if len <= 0 then
    exit;

   inc(ptrcomp(covers ) ,(_ymin - y ) * sizeof(int8u ) );

   y:=_ymin;

  end;

 if y + len > _ymax then
  begin
   len:=_ymax - y + 1;

   if len <= 0 then
    exit;

  end;

 m_ren.blend_solid_vspan(m_ren ,x ,y ,len ,c ,covers );

end;

{ COPY_COLOR_HSPAN }
procedure renderer_base.copy_color_hspan;
var
 d : int;

begin
 if y > _ymax then
  exit;

 if y < _ymin then
  exit;

 if x < _xmin then
  begin
   d:=_xmin - x;

   dec(len ,d );

   if len <= 0 then
    exit;

   inc(ptrcomp(colors ) ,d * sizeof(aggclr ) );

   x:=_xmin;

  end;

 if x + len > _xmax then
  begin
   len:=_xmax - x + 1;

   if len <= 0 then
    exit;

  end;

 m_ren.copy_color_hspan(m_ren ,x ,y ,len ,colors );

end;

{ BLEND_COLOR_HSPAN }
procedure renderer_base.blend_color_hspan;
var
 d : int;

begin
 if y > _ymax then
  exit;

 if y < _ymin then
  exit;

 if x < _xmin then
  begin
   d:=_xmin - x;

   dec(len ,d );

   if len <= 0 then
    exit;

   if covers <> NIL then
    inc(ptrcomp(covers ) ,d * sizeof(int8u ) );

   inc(ptrcomp(colors ) ,d * sizeof(aggclr ) );

   x:=_xmin;

  end;

 if x + len > _xmax then
  begin
   len:=_xmax - x + 1;

   if len <= 0 then
    exit;

  end;

 m_ren.blend_color_hspan(m_ren ,x ,y ,len ,colors ,covers ,cover );

end;

{ BLEND_COLOR_VSPAN }
procedure renderer_base.blend_color_vspan;
var
 d : int;

begin
 if x > _xmax then
  exit;

 if x < _xmin then
  exit;

 if y < _ymin then
  begin
   d:=_ymin - y;

   dec(len ,d );

   if len <= 0 then
    exit;

   if covers <> NIL then
    inc(ptrcomp(covers ) ,d * sizeof(int8u ) );

   inc(ptrcomp(colors ) ,d * sizeof(aggclr ) );

   y:=_ymin;

  end;

 if y + len > _ymax then
  begin
   len:=_ymax - y + 1;

   if len <= 0 then
    exit;

  end;

 m_ren.blend_color_vspan(m_ren ,x ,y ,len ,colors ,covers ,cover );

end;

{ COPY_COLOR_HSPAN_NO_CLIP {not_implemented}
procedure renderer_base.copy_color_hspan_no_clip;
begin
end;

{ BLEND_COLOR_HSPAN_NO_CLIP }
procedure renderer_base.blend_color_hspan_no_clip;
begin
 m_ren.blend_color_hspan(m_ren ,x ,y ,len ,colors ,covers ,cover );

end;

{ BLEND_COLOR_VSPAN_NO_CLIP }
procedure renderer_base.blend_color_vspan_no_clip;
begin
 m_ren.blend_color_vspan(m_ren ,x ,y ,len ,colors ,covers ,cover );

end;

{ CLIP_RECT_AREA }
function renderer_base.clip_rect_area;
var
 rc ,cb : rect;

begin
 rc.Construct(0 ,0 ,0 ,0 );

 cb:=_clip_box^;

 inc(cb.x2 );
 inc(cb.y2 );

 if src.x1 < 0 then
  begin
   dst.x1:=dst.x1 - src.x1;
   src.x1:=0;

  end;

 if src.y1 < 0 then
  begin
   dst.y1:=dst.y1 - src.y1;
   src.y1:=0;

  end;

 if src.x2 > wsrc then
  src.x2:=wsrc;

 if src.y2 > hsrc then
  src.y2:=hsrc;

 if dst.x1 < cb.x1 then
  begin
   src.x1:=src.x1 + (cb.x1 - dst.x1 );
   dst.x1:=cb.x1;

  end;

 if dst.y1 < cb.y1 then
  begin
   src.y1:=src.y1 + (cb.y1 - dst.y1 );
   dst.y1:=cb.y1;

  end;

 if dst.x2 > cb.x2 then
  dst.x2:=cb.x2;

 if dst.y2 > cb.y2 then
  dst.y2:=cb.y2;

 rc.x2:=dst.x2 - dst.x1;
 rc.y2:=dst.y2 - dst.y1;

 if rc.x2 > src.x2 - src.x1 then
  rc.x2:=src.x2 - src.x1;

 if rc.y2 > src.y2 - src.y1 then
  rc.y2:=src.y2 - src.y1;

 result:=rc;

end;

{ COPY_FROM }
procedure renderer_base.copy_from;
var
 rsrc ,rdst ,rc : rect;

 incy : int;

begin
 rsrc.Construct(0 ,0 ,src._width ,src._height );

 if rect_src_ptr <> NIL then
  begin
   rsrc.x1:=rect_src_ptr.x1;
   rsrc.y1:=rect_src_ptr.y1;
   rsrc.x2:=rect_src_ptr.x2 + 1;
   rsrc.y2:=rect_src_ptr.y2 + 1;

  end;

 rdst.Construct(rsrc.x1 + dx ,rsrc.y1 + dy ,rsrc.x2 + dx ,rsrc.y2 + dy );

 rc:=clip_rect_area(@rdst ,@rsrc ,src._width ,src._height );

 if rc.x2 > 0 then
  begin
   incy:=1;

   if rdst.y1 > rsrc.y1 then
    begin
     rsrc.y1:=rsrc.y1 + (rc.y2 - 1 );
     rdst.y1:=rdst.y1 + (rc.y2 - 1 );

     incy:=-1;

    end;

   while rc.y2 > 0 do
    begin
     m_ren.copy_from(m_ren ,src ,rdst.x1 ,rdst.y1 ,rsrc.x1 ,rsrc.y1 ,rc.x2 );

     rdst.y1:=rdst.y1 + incy;
     rsrc.y1:=rsrc.y1 + incy;

     dec(rc.y2 );

    end;

  end;

end;

{ BLEND_FROM }
procedure renderer_base.blend_from;
var
 rsrc ,rdst ,rc : rect;

 incy ,x1src ,x1dst ,len : int;

 rw : row_data_type;

begin
 rsrc.Construct(0 ,0 ,src._width ,src._height );

 if rect_src_ptr <> NIL then
  begin
   rsrc.x1:=rect_src_ptr.x1;
   rsrc.y1:=rect_src_ptr.y1;
   rsrc.x2:=rect_src_ptr.x2 + 1;
   rsrc.y2:=rect_src_ptr.y2 + 1;

  end;

 rdst.Construct(rsrc.x1 + dx ,rsrc.y1 + dy ,rsrc.x2 + dx ,rsrc.y2 + dy );

 rc:=clip_rect_area(@rdst ,@rsrc ,src._width ,src._height );

 if rc.x2 > 0 then
  begin
   incy:=1;

   if rdst.y1 > rsrc.y1 then
    begin
     rsrc.y1:=rsrc.y1 + (rc.y2 - 1 );
     rdst.y1:=rdst.y1 + (rc.y2 - 1 );

     incy:=-1;

    end;

   while rc.y2 > 0 do
    begin
     rw:=src.row(src ,rsrc.x1 ,rsrc.y1 );

     if rw.ptr <> NIL then
      begin
       x1src:=rsrc.x1;
       x1dst:=rdst.x1;
       len  :=rc.x2;

       if rw.x1 > x1src then
        begin
         inc(x1dst ,rw.x1 - x1src );
         dec(len ,rw.x1 - x1src );

         x1src:=rw.x1;

        end;

       if len > 0 then
        begin
         if x1src + len - 1 > rw.x2 then
          dec(len ,x1src + len - rw.x2 - 1 );

         if len > 0 then
          m_ren.blend_from(
           m_ren ,src ,rw.ptr ,
           x1dst ,rdst.y1 ,
           x1src ,rsrc.y1 ,
           len ,cover );

        end;

      end;

     inc(rdst.y1 ,incy );
     inc(rsrc.y1 ,incy );
     dec(rc.y2 );

    end;

  end;

end;

{ BLEND_FROM_COLOR }
procedure renderer_base.blend_from_color(
           src : pixel_formats_ptr;
           color : aggclr_ptr;
           rect_src_ptr : rect_ptr = NIL;
           dx : int = 0;
           dy : int = 0;
           cover : int8u = cover_full );
var
 rsrc ,rdst ,rc : rect;

 rw : row_data_type;

 incy ,x1src ,x1dst ,len : int;

begin
 rsrc.Construct(0 ,0 ,src._width ,src._height );

 if rect_src_ptr <> NIL then
  begin
   rsrc.x1:=rect_src_ptr.x1;
   rsrc.y1:=rect_src_ptr.y1;
   rsrc.x2:=rect_src_ptr.x2 + 1;
   rsrc.y2:=rect_src_ptr.y2 + 1;

  end;

 rdst.Construct(rsrc.x1 + dx ,rsrc.y1 + dy ,rsrc.x2 + dx ,rsrc.y2 + dy );

 rc:=clip_rect_area(@rdst ,@rsrc ,src._width ,src._height );

 if rc.x2 > 0 then
  begin
   incy:=1;

   if rdst.y1 > rsrc.y1 then
    begin
     rsrc.y1:=rsrc.y1 + rc.y2 - 1;
     rdst.y1:=rdst.y1 + rc.y2 - 1;
     incy   :=-1;

    end;

   while rc.y2 > 0 do
    begin
     rw:=src.row(src ,0 ,rsrc.y1 );

     if rw.ptr <> NIL then
      begin
       x1src:=rsrc.x1;
       x1dst:=rdst.x1;
       len  :=rc.x2;

       if rw.x1 > x1src then
        begin
         inc(x1dst ,rw.x1 - x1src );
         dec(len ,rw.x1 - x1src );

         x1src:=rw.x1;

        end;

       if len > 0 then
        begin
         if x1src + len - 1 > rw.x2 then
          dec(len ,x1src + len - rw.x2 - 1 );

         if len > 0 then
          m_ren.blend_from_color(m_ren ,src ,color ,x1dst ,rdst.y1 ,x1src ,rsrc.y1 ,len ,cover );

        end;

      end;

     inc(rdst.y1 ,incy );
     inc(rsrc.y1 ,incy );
     dec(rc.y2 );

    end;

  end;

end;

{ BLEND_FROM_LUT }
procedure renderer_base.blend_from_lut(
           src : pixel_formats_ptr;
           color_lut : aggclr_ptr;
           rect_src_ptr : rect_ptr = NIL;
           dx : int = 0;
           dy : int = 0;
           cover : int8u = cover_full );
var
 rsrc ,rdst ,rc : rect;

 rw : row_data_type;

 incy ,x1src ,x1dst ,len : int;

begin
 rsrc.Construct(0 ,0 ,src._width ,src._height );

 if rect_src_ptr <> NIL then
  begin
   rsrc.x1:=rect_src_ptr.x1;
   rsrc.y1:=rect_src_ptr.y1;
   rsrc.x2:=rect_src_ptr.x2 + 1;
   rsrc.y2:=rect_src_ptr.y2 + 1;

  end;

 rdst.Construct(rsrc.x1 + dx ,rsrc.y1 + dy ,rsrc.x2 + dx ,rsrc.y2 + dy );

 rc:=clip_rect_area(@rdst ,@rsrc ,src._width ,src._height );

 if rc.x2 > 0 then
  begin
   incy:=1;

   if rdst.y1 > rsrc.y1 then
    begin
     rsrc.y1:=rsrc.y1 + rc.y2 - 1;
     rdst.y1:=rdst.y1 + rc.y2 - 1;
     incy   :=-1;

    end;

   while rc.y2 > 0 do
    begin
     rw:=src.row(src ,0 ,rsrc.y1 );

     if rw.ptr <> NIL then
      begin
       x1src:=rsrc.x1;
       x1dst:=rdst.x1;
       len  :=rc.x2;

       if rw.x1 > x1src then
        begin
         inc(x1dst ,rw.x1 - x1src );
         dec(len ,rw.x1 - x1src );

         x1src:=rw.x1;

        end;

       if len > 0 then
        begin
         if x1src + len - 1 > rw.x2 then
          dec(len ,x1src + len - rw.x2 - 1 );

         if len > 0 then
          m_ren.blend_from_lut(m_ren ,src ,color_lut ,x1dst ,rdst.y1 ,x1src ,rsrc.y1 ,len ,cover );

        end;

      end;

     inc(rdst.y1 ,incy );
     inc(rsrc.y1 ,incy );
     dec(rc.y2 );

    end;

  end;

end;

END.

