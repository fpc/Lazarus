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
// [Pascal Port History] -----------------------------------------------------
//
// 17.09.2007-Milano: Porting & Finished OK
// 16.09.2007-Milano: Unit port establishment
//
{ agg_font_freetype.pas }
unit
 agg_font_freetype ;

INTERFACE

{$I agg_mode.inc }

uses
 SysUtils ,Math ,
 agg_font_freetype_lib ,
 agg_basics ,
 agg_font_engine ,
 agg_font_cache_manager ,
 agg_trans_affine ,
 agg_vertex_source ,
 agg_path_storage_integer ,
 agg_conv_curve ,
 agg_scanline ,
 agg_scanline_u ,
 agg_scanline_bin ,
 agg_rasterizer_scanline_aa ,
 agg_renderer_scanline ,
 agg_render_scanlines ,
 agg_bitset_iterator ;

{ GLOBAL VARIABLES & CONSTANTS }
{ TYPES DEFINITION }
type
 face_name_ptr = ^face_name;
 face_name = record
   name : char_ptr; //PChar;
   size : unsigned;

  end;

//-----------------------------------------------font_engine_freetype_base
 font_engine_freetype_base = object(font_engine )
   m_flag32 : boolean;

   m_change_stamp ,
   m_last_error   : int;

   m_name       : PChar;
   m_name_len   ,
   m_face_index : unsigned;
   m_char_map   : FT_Encoding;
   m_signature  : face_name;

   m_height ,
   m_width  : unsigned;

   m_hinting             ,
   m_flip_y              ,
   m_library_initialized : boolean;

   m_library : FT_Library_ptr;    // handle to library
   m_faces   : FT_Face_ptr_ptr;   // A pool of font faces

   m_face_names : face_name_ptr;
   m_num_faces  ,
   m_max_faces  : unsigned;
   m_cur_face   : FT_Face_ptr;    // handle to the current face object
   m_resolution : int;

   m_glyph_rendering : glyph_rendering;
   m_glyph_index     ,
   m_data_size       ,
   m_data_type       : unsigned;

   m_bounds    : agg_basics.rect;
   m_advance_x ,
   m_advance_y : double;
   m_affine    : trans_affine;

   m_path16   : path_storage_int16;
   m_path32   : path_storage_int32;
   m_curves16 ,
   m_curves32 : conv_curve;

   m_scanline_aa   : scanline_u8;
   m_scanline_bin  : scanline_bin;
   m_scanlines_aa  : scanlines_aa_type;
   m_scanlines_bin : scanlines_bin_type;
   m_rasterizer    : rasterizer_scanline_aa;

   constructor Construct(flag32_ : boolean; max_faces : unsigned = 32 );
   destructor  Destruct;

  // Set font parameters
   procedure resolution_(dpi : unsigned );

   function  load_font(
              font_name : PChar; face_index : unsigned; ren_type : glyph_rendering;
              font_mem : PChar = NIL; font_mem_size : int = 0 ) : boolean;

   function  attach(file_name : PChar ) : boolean;

   function  char_map_ (map : FT_Encoding ) : boolean;
   function  height_   (h : double ) : boolean;
   function  width_    (w : double ) : boolean;
   procedure hinting_  (h : boolean );
   procedure flip_y_   (flip : boolean );
   procedure transform_(affine : trans_affine_ptr );

  // Set Gamma
   procedure gamma_(f : vertex_source_ptr );

  // Accessors
   function  _last_error : int;
   function  _resolution : unsigned;
   function  _name : PChar;
   function  _num_faces : unsigned;
   function  _char_map : FT_Encoding;
   function  _height : double;
   function  _width : double;
   function  _ascender : double;
   function  _descender : double;
   function  _hinting : boolean;
   function  _flip_y : boolean;

  // Interface mandatory to implement for font_cache_manager
   function  font_signature : PChar; virtual;
   function  change_stamp : int; virtual;

   function  prepare_glyph(glyph_code : unsigned ) : boolean; virtual;

   function  glyph_index : unsigned; virtual;
   function  data_size : unsigned; virtual;
   function  data_type : unsigned; virtual;
   function  bounds : rect_ptr; virtual;
   function  advance_x : double; virtual;
   function  advance_y : double; virtual;

   procedure write_glyph_to(data : int8u_ptr ); virtual;
   function  add_kerning   (first ,second : unsigned; x ,y : double_ptr ) : boolean; virtual;

   function  flag32 : boolean; virtual;

  // private
   procedure update_char_size;
   procedure update_signature;

   function  find_face(name : PChar ) : int;

  end;

//------------------------------------------------font_engine_freetype_int16
// This class uses values of type int16 (10.6 format) for the vector cache.
// The vector cache is compact, but when rendering glyphs of height
// more that 200 there integer overflow can occur.
 font_engine_freetype_int16 = object(font_engine_freetype_base )
   constructor Construct(max_faces : unsigned = 32 );

  end;

//------------------------------------------------font_engine_freetype_int32
// This class uses values of type int32 (26.6 format) for the vector cache.
// The vector cache is twice larger than in font_engine_freetype_int16,
// but it allows you to render glyphs of very large sizes.
 font_engine_freetype_int32 = object(font_engine_freetype_base )
   constructor Construct(max_faces : unsigned = 32 );

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
//------------------------------------------------------------------------------
//
// This code implements the AUTODIN II polynomial
// The variable corresponding to the macro argument "crc" should
// be an unsigned long.
// Oroginal code  by Spencer Garrett <srg@quick.com>
//
// generated using the AUTODIN II polynomial
//   x^32 + x^26 + x^23 + x^22 + x^16 +
//   x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x^1 + 1
//
//------------------------------------------------------------------------------
const
 crc32tab : array[0..255 ] of unsigned = (
  $00000000, $77073096, $ee0e612c, $990951ba,
  $076dc419, $706af48f, $e963a535, $9e6495a3,
  $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988,
  $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
  $1db71064, $6ab020f2, $f3b97148, $84be41de,
  $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
  $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
  $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
  $3b6e20c8, $4c69105e, $d56041e4, $a2677172,
  $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
  $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940,
  $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
  $26d930ac, $51de003a, $c8d75180, $bfd06116,
  $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
  $2802b89e, $5f058808, $c60cd9b2, $b10be924,
  $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
  $76dc4190, $01db7106, $98d220bc, $efd5102a,
  $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
  $7807c9a2, $0f00f934, $9609a88e, $e10e9818,
  $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
  $6b6b51f4, $1c6c6162, $856530d8, $f262004e,
  $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
  $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c,
  $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
  $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2,
  $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
  $4369e96a, $346ed9fc, $ad678846, $da60b8d0,
  $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
  $5005713c, $270241aa, $be0b1010, $c90c2086,
  $5768b525, $206f85b3, $b966d409, $ce61e49f,
  $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4,
  $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
  $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a,
  $ead54739, $9dd277af, $04db2615, $73dc1683,
  $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
  $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
  $f00f9344, $8708a3d2, $1e01f268, $6906c2fe,
  $f762575d, $806567cb, $196c3671, $6e6b06e7,
  $fed41b76, $89d32be0, $10da7a5a, $67dd4acc,
  $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
  $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252,
  $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
  $d80d2bda, $af0a1b4c, $36034af6, $41047a60,
  $df60efc3, $a867df55, $316e8eef, $4669be79,
  $cb61b38c, $bc66831a, $256fd2a0, $5268e236,
  $cc0c7795, $bb0b4703, $220216b9, $5505262f,
  $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04,
  $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
  $9b64c2b0, $ec63f226, $756aa39c, $026d930a,
  $9c0906a9, $eb0e363f, $72076785, $05005713,
  $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38,
  $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
  $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e,
  $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
  $88085ae6, $ff0f6a70, $66063bca, $11010b5c,
  $8f659eff, $f862ae69, $616bffd3, $166ccf45,
  $a00ae278, $d70dd2ee, $4e048354, $3903b3c2,
  $a7672661, $d06016f7, $4969474d, $3e6e77db,
  $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0,
  $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
  $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6,
  $bad03605, $cdd70693, $54de5729, $23d967bf,
  $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94,
  $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d );

{ UNIT IMPLEMENTATION }
{ calc_crc32 }
function calc_crc32(buf : int8u_ptr; size : unsigned ) : unsigned;
var
 crc ,len ,nr : unsigned;

 p : int8u_ptr;

begin
 crc:=unsigned(not 0 );
 len:=0;
 nr :=size;
 len:=len + nr;
 p  :=buf;

 while nr <> 0 do
  begin
   dec(nr );

   crc:=(crc shr 8 ) xor crc32tab[(crc xor p^ ) and $ff ];

   inc(ptrcomp(p ) ,sizeof(int8u ) );

  end;

 result:=not crc;

end;

{ dbl_to_plain_fx }
function dbl_to_plain_fx(d : double ) : int;
begin
 result:=Trunc(d * 65536.0 )

end;

{ int26p6_to_dbl }
function int26p6_to_dbl(p : int ) : double;
begin
 result:=p / 64.0;

end;

{ dbl_to_int26p6 }
function dbl_to_int26p6(p : double ) : int;
begin
 result:=Trunc(p * 64.0 + 0.5 );

end;

{ decompose_ft_outline }
function decompose_ft_outline(
          outline : FT_Outline_ptr;
          flip_y : boolean;
          mtx : trans_affine_ptr;
          path : path_storage_integer_ptr ) : boolean;
var
 v_last ,v_control ,v_start ,vec ,v_middle ,vec1 ,vec2 : FT_Vector;

 x1 ,y1 ,x2 ,y2 ,x3 ,y3 : double;

 point ,limit : FT_Vector_ptr;

 tags : char_ptr;

 n     ,      // index of contour in outline
 first ,      // index of first point in contour
 last  : int; // index of last point in contour

 tag : char;  // current point's state

label
 Do_Conic ,Close ;

begin
 first:=0;
 n    :=0;

 while n < outline.n_contours do
  begin
   last :=FT_Short_ptr(ptrcomp(outline.contours ) + n * sizeof(FT_Short ) )^;
   limit:=FT_Vector_ptr(ptrcomp(outline.points ) + last * sizeof(FT_Vector ) );

   v_start:=FT_Vector_ptr(ptrcomp(outline.points ) + first * sizeof(FT_Vector) )^;
   v_last :=FT_Vector_ptr(ptrcomp(outline.points ) + last * sizeof(FT_Vector) )^;

   v_control:=v_start;

   point:=FT_Vector_ptr(ptrcomp(outline.points ) + first * sizeof(FT_Vector ) );
   tags :=char_ptr     (ptrcomp(outline.tags ) + first * sizeof(char ) );
   tag  :=FT_CURVE_TAG (tags^ );

  // A contour cannot start with a cubic control point!
   if tag = char(FT_CURVE_TAG_CUBIC ) then
    begin
     result:=false;

     exit;

    end;

  // check first point to determine origin
   if tag = char(FT_CURVE_TAG_CONIC ) then
    begin
    // first point is conic control. Yes, this happens.
     if FT_CURVE_TAG(char_ptr(ptrcomp(outline.tags ) + last )^ ) = char(FT_CURVE_TAG_ON ) then
      begin
      // start at last point if it is on the curve
       v_start:=v_last;

       dec(limit );

      end
     else
      begin
      // if both first and last points are conic,
      // start at their middle and record its position
      // for closure
       v_start.x:=(v_start.x + v_last.x ) div 2;
       v_start.y:=(v_start.y + v_last.y ) div 2;

       v_last:=v_start;

      end;

     dec(ptrcomp(point ) ,sizeof(FT_Vector ) );
     dec(ptrcomp(tags ) );

    end;

   x1:=int26p6_to_dbl(v_start.x );
   y1:=int26p6_to_dbl(v_start.y );

   if flip_y then
    y1:=-y1;

   mtx.transform(mtx ,@x1 ,@y1 );
   path.move_to (dbl_to_int26p6(x1 ) ,dbl_to_int26p6(y1 ) );

   while ptrcomp(point ) < ptrcomp(limit ) do
    begin
     inc(ptrcomp(point ) ,sizeof(FT_Vector ) );
     inc(ptrcomp(tags ) );

     tag:=FT_CURVE_TAG(tags^ );

     case tag of
     // emit a single line_to
      char(FT_CURVE_TAG_ON ) :
       begin
        x1:=int26p6_to_dbl(point.x );
        y1:=int26p6_to_dbl(point.y );

        if flip_y then
         y1:=-y1;

        mtx.transform(mtx ,@x1 ,@y1 );
        path.line_to (dbl_to_int26p6(x1 ) ,dbl_to_int26p6(y1 ) );

        continue;

       end;

     // consume conic arcs
      char(FT_CURVE_TAG_CONIC ) :
       begin
        v_control.x:=point.x;
        v_control.y:=point.y;

       Do_Conic:
        if ptrcomp(point ) < ptrcomp(limit ) then
         begin
          inc(ptrcomp(point ) ,sizeof(FT_Vector ) );
          inc(ptrcomp(tags ) );

          tag:=FT_CURVE_TAG(tags^ );

          vec.x:=point.x;
          vec.y:=point.y;

          if tag = char(FT_CURVE_TAG_ON ) then
           begin
            x1:=int26p6_to_dbl(v_control.x );
            y1:=int26p6_to_dbl(v_control.y );
            x2:=int26p6_to_dbl(vec.x );
            y2:=int26p6_to_dbl(vec.y );

            if flip_y then
             begin
              y1:=-y1;
              y2:=-y2;

             end;

            mtx.transform(mtx ,@x1 ,@y1 );
            mtx.transform(mtx ,@x2 ,@y2 );

            path.curve3(
             dbl_to_int26p6(x1 ) ,
             dbl_to_int26p6(y1 ) ,
             dbl_to_int26p6(x2 ) ,
             dbl_to_int26p6(y2 ) );

            continue;

           end;

          if tag <> char(FT_CURVE_TAG_CONIC ) then
           begin
            result:=false;

            exit;

           end;

          v_middle.x:=(v_control.x + vec.x ) div 2;
          v_middle.y:=(v_control.y + vec.y ) div 2;

          x1:=int26p6_to_dbl(v_control.x );
          y1:=int26p6_to_dbl(v_control.y );
          x2:=int26p6_to_dbl(v_middle.x );
          y2:=int26p6_to_dbl(v_middle.y );

          if flip_y then
           begin
            y1:=-y1;
            y2:=-y2;

           end;

          mtx.transform(mtx ,@x1 ,@y1 );
          mtx.transform(mtx ,@x2 ,@y2 );

          path.curve3(
           dbl_to_int26p6(x1 ) ,
           dbl_to_int26p6(y1 ) ,
           dbl_to_int26p6(x2 ) ,
           dbl_to_int26p6(y2 ) );

          v_control:=vec;

          goto Do_Conic;

         end;

        x1:=int26p6_to_dbl(v_control.x );
        y1:=int26p6_to_dbl(v_control.y );
        x2:=int26p6_to_dbl(v_start.x );
        y2:=int26p6_to_dbl(v_start.y );

        if flip_y then
         begin
          y1:=-y1;
          y2:=-y2;

         end;

        mtx.transform(mtx ,@x1 ,@y1 );
        mtx.transform(mtx ,@x2 ,@y2 );

        path.curve3(
         dbl_to_int26p6(x1 ) ,
         dbl_to_int26p6(y1 ) ,
         dbl_to_int26p6(x2 ) ,
         dbl_to_int26p6(y2 ) );

        goto Close;

       end;

     // FT_CURVE_TAG_CUBIC
      else
       begin
        if (ptrcomp(point ) + sizeof(FT_Vector ) > ptrcomp(limit ) ) or
           (FT_CURVE_TAG(char_ptr(ptrcomp(tags ) + 1 )^ ) <> char(FT_CURVE_TAG_CUBIC ) ) then
         begin
          result:=false;

          exit;

         end;

        vec1.x:=point.x;
        vec1.y:=point.y;
        vec2.x:=FT_Vector_ptr(ptrcomp(point ) + sizeof(FT_Vector ) ).x;
        vec2.y:=FT_Vector_ptr(ptrcomp(point ) + sizeof(FT_Vector ) ).y;

        inc(ptrcomp(point ) ,2 * sizeof(FT_Vector ) );
        inc(ptrcomp(tags ) ,2 );

        if ptrcomp(point ) <= ptrcomp(limit ) then
         begin
          vec.x:=point.x;
          vec.y:=point.y;

          x1:=int26p6_to_dbl(vec1.x );
          y1:=int26p6_to_dbl(vec1.y );
          x2:=int26p6_to_dbl(vec2.x );
          y2:=int26p6_to_dbl(vec2.y );
          x3:=int26p6_to_dbl(vec.x );
          y3:=int26p6_to_dbl(vec.y );

          if flip_y then
           begin
            y1:=-y1;
            y2:=-y2;
            y3:=-y3;

           end;

          mtx.transform(mtx ,@x1 ,@y1 );
          mtx.transform(mtx ,@x2 ,@y2 );
          mtx.transform(mtx ,@x3 ,@y3 );

          path.curve4(
           dbl_to_int26p6(x1 ) ,
           dbl_to_int26p6(y1 ) ,
           dbl_to_int26p6(x2 ) ,
           dbl_to_int26p6(y2 ) ,
           dbl_to_int26p6(x3 ) ,
           dbl_to_int26p6(y3 ) );

          continue;

         end;

        x1:=int26p6_to_dbl(vec1.x );
        y1:=int26p6_to_dbl(vec1.y );
        x2:=int26p6_to_dbl(vec2.x );
        y2:=int26p6_to_dbl(vec2.y );
        x3:=int26p6_to_dbl(v_start.x );
        y3:=int26p6_to_dbl(v_start.y );

        if flip_y then
         begin
          y1:=-y1;
          y2:=-y2;
          y3:=-y3;

         end;

        mtx.transform(mtx ,@x1 ,@y1 );
        mtx.transform(mtx ,@x2 ,@y2 );
        mtx.transform(mtx ,@x3 ,@y3 );

        path.curve4(
         dbl_to_int26p6(x1) ,
         dbl_to_int26p6(y1) ,
         dbl_to_int26p6(x2) ,
         dbl_to_int26p6(y2) ,
         dbl_to_int26p6(x3) ,
         dbl_to_int26p6(y3) );

        goto Close;

       end;

     end;

    end;

   path.close_polygon;

  Close:
   first:=last + 1;

   inc(n );

  end;

 result:=true;

end;

{ decompose_ft_bitmap_mono }
procedure decompose_ft_bitmap_mono(
           bitmap : FT_Bitmap_ptr;
           x ,y : int;
           flip_y : boolean;
           sl : scanline_ptr;
           storage : renderer_scanline_ptr );
var
 i ,pitch ,j : int;

 buf  : int8u_ptr;
 bits : bitset_iterator;

begin
 buf  :=int8u_ptr(bitmap.buffer );
 pitch:=bitmap.pitch;

 sl.reset       (x ,x + bitmap.width );
 storage.prepare(bitmap.width + 2 );

 if flip_y then
  begin
   inc(ptrcomp(buf ) ,bitmap.pitch * (bitmap.rows - 1 ) );
   inc(y ,bitmap.rows );

   pitch:=-pitch;

  end;

 i:=0;

 while i < bitmap.rows do
  begin
   sl.reset_spans;

   bits.Construct(buf ,0 );

   j:=0;

   while j < bitmap.width do
    begin
     if bits.bit <> 0 then
      sl.add_cell(x + j ,cover_full );

     bits.inc_operator;

     inc(j );

    end;

   inc(ptrcomp(buf ) ,pitch * sizeof(int8u ) );

   if sl.num_spans <> 0 then
    begin
     sl.finalize   (y - i - 1 );
     storage.render(sl );

    end;

   inc(i );

  end;

end;

{ decompose_ft_bitmap_gray8 }
procedure decompose_ft_bitmap_gray8(
           bitmap : FT_Bitmap_ptr;
           x ,y : int;
           flip_y : boolean;
           ras : rasterizer_scanline_aa_ptr;
           sl : scanline_ptr;
           storage : renderer_scanline_ptr );
var
 i ,j ,pitch : int;

 buf ,p : int8u_ptr;

begin
 buf  :=int8u_ptr(bitmap.buffer );
 pitch:=bitmap.pitch;

 sl.reset       (x ,x + bitmap.width );
 storage.prepare(bitmap.width + 2 );

 if flip_y then
  begin
   inc(ptrcomp(buf ) ,bitmap.pitch * (bitmap.rows - 1 ) );
   inc(y ,bitmap.rows );

   pitch:=-pitch;

  end;

 i:=0;

 while i < bitmap.rows do
  begin
   sl.reset_spans;

   p:=buf;
   j:=0;

   while j < bitmap.width do
    begin
     if p^ <> 0 then
      sl.add_cell(x + j ,ras.apply_gamma(p^ ) );

     inc(ptrcomp(p ) ,sizeof(int8u ) );
     inc(j );

    end;

   inc(ptrcomp(buf ) ,pitch * sizeof(int8u ) );

   if sl.num_spans <> 0 then
    begin
     sl.finalize   (y - i - 1 );
     storage.render(sl );

    end;

   inc(i );

  end;

end;

{ CONSTRUCT }
constructor font_engine_freetype_base.Construct(flag32_ : boolean; max_faces : unsigned = 32 );
begin
 m_flag32:=flag32_;

 m_change_stamp:=0;
 m_last_error  :=0;

 m_name      :=NIL;
 m_name_len  :=256 - 16 - 1;
 m_face_index:=0;

 m_char_map:=FT_ENCODING_NONE;

 m_signature.size:=256 + 256 - 16;

 agg_getmem(pointer(m_signature.name ) ,m_signature.size );

 m_height :=0;
 m_width  :=0;
 m_hinting:=true;
 m_flip_y :=false;

 m_library_initialized:=false;

 m_library:=NIL;

 agg_getmem(pointer(m_faces ) ,max_faces * sizeof(FT_Face_ptr ) );
 agg_getmem(pointer(m_face_names ) ,max_faces * sizeof(face_name ) );

 m_num_faces :=0;
 m_max_faces :=max_faces;
 m_cur_face  :=NIL;
 m_resolution:=0;

 m_glyph_rendering:=glyph_ren_native_gray8;
 m_glyph_index    :=0;
 m_data_size      :=0;
 m_data_type      :=glyph_data_invalid;

 m_bounds.Construct(1 ,1 ,0 ,0 );

 m_advance_x:=0.0;
 m_advance_y:=0.0;

 m_affine.Construct;

 m_path16.Construct;
 m_path32.Construct;
 m_curves16.Construct(@m_path16 );
 m_curves32.Construct(@m_path32 );
 m_scanline_aa.Construct;
 m_scanline_bin.Construct;
 m_scanlines_aa.Construct;
 m_scanlines_bin.Construct;
 m_rasterizer.Construct;

 m_curves16.approximation_scale_(4.0 );
 m_curves32.approximation_scale_(4.0 );

 m_last_error:=FT_Init_FreeType(@m_library );

 if m_last_error = 0 then
  m_library_initialized:=true;

end;

{ DESTRUCT }
destructor font_engine_freetype_base.Destruct;
var
 i : unsigned;
 n : face_name_ptr;

begin
 i:=0;
 n:=m_face_names;

 while i < m_num_faces do
  begin
   agg_freemem (pointer(n.name ) ,n.size );
   FT_Done_Face(FT_Face_ptr_ptr(ptrcomp(m_faces ) + i * sizeof(FT_Face_ptr ) )^ );

   inc(ptrcomp(n ) ,sizeof(face_name ) );
   inc(i );

  end;

 agg_freemem(pointer(m_face_names ) ,m_max_faces * sizeof(face_name ) );
 agg_freemem(pointer(m_faces ) ,m_max_faces * sizeof(FT_Face_ptr ) );
 agg_freemem(pointer(m_signature.name ) ,m_signature.size );

 if m_library_initialized then
  FT_Done_FreeType(m_library );

 m_path16.Destruct;
 m_path32.Destruct;
 m_curves16.Destruct;
 m_curves32.Destruct;
 m_scanline_aa.Destruct;
 m_scanline_bin.Destruct;
 m_scanlines_aa.Destruct;
 m_scanlines_bin.Destruct;
 m_rasterizer.Destruct;

end;

{ RESOLUTION_ }
procedure font_engine_freetype_base.resolution_(dpi : unsigned );
begin
 m_resolution:=dpi;

 update_char_size;

end;

{ LOAD_FONT }
function font_engine_freetype_base.load_font(
          font_name : PChar; face_index : unsigned; ren_type : glyph_rendering;
          font_mem : PChar = NIL; font_mem_size : int = 0 ) : boolean;
var
 idx : int;

begin
 result:=false;

 if m_library_initialized then
  begin
   m_last_error:=0;

   idx:=find_face(font_name );

   if idx >= 0 then
    begin
     m_cur_face:=FT_Face_ptr_ptr(ptrcomp(m_faces ) + idx * sizeof(FT_Face_ptr ) )^;
     m_name    :=PChar(face_name_ptr(ptrcomp(m_face_names ) + idx * sizeof(face_name ) ).name );

    end
   else
    begin
     if m_num_faces >= m_max_faces then
      begin
       agg_freemem (pointer(m_face_names.name ) ,m_face_names.size );
       FT_Done_Face(m_faces^ );

       move(
        FT_Face_ptr_ptr(ptrcomp(m_faces ) + sizeof(FT_Face_ptr ) )^ ,
        m_faces^ ,
        (m_max_faces - 1 ) * sizeof(FT_Face_ptr ) );

       move(
        face_name_ptr(ptrcomp(m_face_names ) + sizeof(face_name ) )^ ,
        m_face_names^ ,
        (m_max_faces - 1 ) * sizeof(face_name ) );

       dec(m_num_faces );

      end;

     if (font_mem <> NIL ) and
        (font_mem_size > 0 ) then
      m_last_error:=
       FT_New_Memory_Face(
        m_library ,
        FT_Byte_ptr(font_mem ) ,
        font_mem_size ,
        face_index ,
        FT_Face_ptr_ptr(ptrcomp(m_faces ) + m_num_faces * sizeof(FT_Face_ptr ) )^ )
     else
      m_last_error:=
       FT_New_Face(
        m_library ,
        font_name ,
        face_index ,
        FT_Face_ptr_ptr(ptrcomp(m_faces ) + m_num_faces * sizeof(FT_Face_ptr ) )^ );

     if m_last_error = 0 then
      begin
       face_name_ptr(ptrcomp(m_face_names ) + m_num_faces * sizeof(face_name ) ).size:=
        StrLen(font_name ) + 1;

       agg_getmem(
        pointer(face_name_ptr(ptrcomp(m_face_names ) + m_num_faces * sizeof(face_name ) ).name ) ,
        face_name_ptr(ptrcomp(m_face_names ) + m_num_faces * sizeof(face_name ) ).size );

       StrCopy(
        PChar(face_name_ptr(ptrcomp(m_face_names ) + m_num_faces * sizeof(face_name ) ).name ) ,
        font_name );

       m_cur_face:=FT_Face_ptr_ptr(ptrcomp(m_faces ) + m_num_faces * sizeof(FT_Face_ptr ) )^;
       m_name    :=PChar(face_name_ptr(ptrcomp(m_face_names ) + m_num_faces * sizeof(face_name ) ).name );

       inc(m_num_faces );

      end
     else
      begin
       PChar(face_name_ptr(ptrcomp(m_face_names ) + m_num_faces * sizeof(face_name ) ).name ):=NIL;

       m_cur_face:=NIL;
       m_name    :=NIL;

      end;

    end;

   if m_last_error = 0 then
    begin
     result:=true;

     case ren_type of
      glyph_ren_native_mono :
       m_glyph_rendering:=glyph_ren_native_mono;

      glyph_ren_native_gray8 :
       m_glyph_rendering:=glyph_ren_native_gray8;

      glyph_ren_outline :
       if FT_IS_SCALABLE(m_cur_face ) then
        m_glyph_rendering:=glyph_ren_outline
       else
        m_glyph_rendering:=glyph_ren_native_gray8;

      glyph_ren_agg_mono :
       if FT_IS_SCALABLE(m_cur_face ) then
        m_glyph_rendering:=glyph_ren_agg_mono
       else
        m_glyph_rendering:=glyph_ren_native_mono;

      glyph_ren_agg_gray8 :
       if FT_IS_SCALABLE(m_cur_face ) then
        m_glyph_rendering:=glyph_ren_agg_gray8
       else
        m_glyph_rendering:=glyph_ren_native_gray8;

     end;

     update_signature;

    end;

  end;

end;

{ ATTACH }
function font_engine_freetype_base.attach(file_name : PChar ) : boolean;
begin
 result:=false;

 if m_cur_face <> NIL then
  begin
   m_last_error:=FT_Attach_File(m_cur_face ,file_name );

   result:=m_last_error = 0;

  end;

end;

{ CHAR_MAP_ }
function font_engine_freetype_base.char_map_(map : FT_Encoding ) : boolean;
begin
 result:=false;

 if m_cur_face <> NIL then
  begin
   m_last_error:=FT_Select_Charmap(m_cur_face ,map );

   if m_last_error = 0 then
    begin
     m_char_map:=map;

     update_signature;

     result:=true;

    end;

  end;

end;

{ HEIGHT_ }
function font_engine_freetype_base.height_(h : double ) : boolean;
begin
 result  :=false;
 m_height:=Trunc(h * 64.0 );

 if m_cur_face <> NIL then
  begin
   update_char_size;

   result:=true;

  end;

end;

{ WIDTH_ }
function font_engine_freetype_base.width_(w : double ) : boolean;
begin
 result :=false;
 m_width:=Trunc(w * 64.0 );

 if m_cur_face <> NIL then
  begin
   update_char_size;

   result:=true;

  end;

end;

{ HINTING_ }
procedure font_engine_freetype_base.hinting_(h : boolean );
begin
 m_hinting:=h;

 if m_cur_face <> NIL then
  update_signature;

end;

{ FLIP_Y_ }
procedure font_engine_freetype_base.flip_y_(flip : boolean );
begin
 m_flip_y:=flip;

 if m_cur_face <> NIL then
  update_signature;

end;

{ TRANSFORM_ }
procedure font_engine_freetype_base.transform_(affine : trans_affine_ptr );
begin
 m_affine.assign_all(affine );

 if m_cur_face <> NIL then
  update_signature;

end;

{ GAMMA_ }
procedure font_engine_freetype_base.gamma_(f : vertex_source_ptr );
begin
 m_rasterizer.gamma(f );

end;

{ LAST_ERROR_ }
function font_engine_freetype_base._last_error : int;
begin
 result:=m_last_error;

end;

{ _RESOLUTION }
function font_engine_freetype_base._resolution : unsigned;
begin
 result:=m_resolution;

end;

{ _NAME }
function font_engine_freetype_base._name : PChar;
begin
 result:=m_name;

end;

{ _NUM_FACES }
function font_engine_freetype_base._num_faces : unsigned;
begin
 if m_cur_face <> NIL then
  result:=m_cur_face.num_faces
 else
  result:=0;

end;

{ _CHAR_MAP }
function font_engine_freetype_base._char_map : FT_Encoding;
begin
 result:=m_char_map;

end;

{ _HEIGHT }
function font_engine_freetype_base._height : double;
begin
 result:=m_height / 64.0;

end;

{ _WIDTH }
function font_engine_freetype_base._width : double;
begin
 result:=m_width / 64.0;

end;

{ _ASCENDER }
function font_engine_freetype_base._ascender : double;
begin
 if m_cur_face <> NIL then
  result:=m_cur_face.ascender * _height / m_cur_face.height
 else
  result:=0.0;

end;

{ _DESCENDER }
function font_engine_freetype_base._descender : double;
begin
 if m_cur_face <> NIL then
  result:=m_cur_face.descender * _height / m_cur_face.height
 else
  result:=0.0;

end;

{ _HINTING }
function font_engine_freetype_base._hinting : boolean;
begin
 result:=m_hinting;

end;

{ _FLIP_Y }
function font_engine_freetype_base._flip_y : boolean;
begin
 result:=m_flip_y;

end;

{ FONT_SIGNATURE }
function font_engine_freetype_base.font_signature : PChar;
begin
 result:=PChar(m_signature.name );

end;

{ CHANGE_STAMP }
function font_engine_freetype_base.change_stamp : int;
begin
 result:=m_change_stamp;

end;

{ PREPARE_GLYPH }
function font_engine_freetype_base.prepare_glyph(glyph_code : unsigned ) : boolean;
var
 fl : int;

 bnd : rect_d; 

begin
 result:=false;

 m_glyph_index:=FT_Get_Char_Index(m_cur_face ,glyph_code );

 if m_hinting then
  m_last_error:=FT_Load_Glyph(m_cur_face ,m_glyph_index ,FT_LOAD_DEFAULT{} {FT_LOAD_FORCE_AUTOHINT{} )
 else
  m_last_error:=FT_Load_Glyph(m_cur_face ,m_glyph_index ,FT_LOAD_NO_HINTING );

 if m_last_error = 0 then
  case m_glyph_rendering of
   glyph_ren_native_mono :
    begin
     m_last_error:=FT_Render_Glyph(m_cur_face.glyph ,FT_RENDER_MODE_MONO );

     if m_last_error = 0 then
      begin
       if m_flip_y then
        fl:=-m_cur_face.glyph.bitmap_top
       else
        fl:=m_cur_face.glyph.bitmap_top;

       decompose_ft_bitmap_mono(
        @m_cur_face.glyph.bitmap ,
        m_cur_face.glyph.bitmap_left ,
        fl ,
        m_flip_y ,
        @m_scanline_bin ,
        @m_scanlines_bin );

       m_bounds.x1:=m_scanlines_bin._min_x;
       m_bounds.y1:=m_scanlines_bin._min_y;
       m_bounds.x2:=m_scanlines_bin._max_x;
       m_bounds.y2:=m_scanlines_bin._max_y;
       m_data_size:=m_scanlines_bin.byte_size;
       m_data_type:=glyph_data_mono;
       m_advance_x:=int26p6_to_dbl(m_cur_face.glyph.advance.x );
       m_advance_y:=int26p6_to_dbl(m_cur_face.glyph.advance.y );

       result:=true;

      end;

    end;

   glyph_ren_native_gray8 :
    begin
     m_last_error:=FT_Render_Glyph(m_cur_face.glyph ,FT_RENDER_MODE_NORMAL );

     if m_last_error = 0 then
      begin
       if m_flip_y then
        fl:=-m_cur_face.glyph.bitmap_top
       else
        fl:=m_cur_face.glyph.bitmap_top;

       decompose_ft_bitmap_gray8(
        @m_cur_face.glyph.bitmap ,
        m_cur_face.glyph.bitmap_left ,
        fl ,
        m_flip_y ,
        @m_rasterizer ,
        @m_scanline_aa ,
        @m_scanlines_aa );

       m_bounds.x1:=m_scanlines_aa._min_x;
       m_bounds.y1:=m_scanlines_aa._min_y;
       m_bounds.x2:=m_scanlines_aa._max_x;
       m_bounds.y2:=m_scanlines_aa._max_y;
       m_data_size:=m_scanlines_aa.byte_size;
       m_data_type:=glyph_data_gray8;
       m_advance_x:=int26p6_to_dbl(m_cur_face.glyph.advance.x );
       m_advance_y:=int26p6_to_dbl(m_cur_face.glyph.advance.y );

       result:=true;

      end;

    end;

   glyph_ren_outline :
    if m_last_error = 0 then
     if m_flag32 then
      begin
       m_path32.remove_all;

       if decompose_ft_outline(
           @m_cur_face.glyph.outline ,
           m_flip_y ,
           @m_affine ,
           @m_path32 ) then
        begin
         bnd:=m_path32.bounding_rect;

         m_data_size:=m_path32.byte_size;
         m_data_type:=glyph_data_outline;
         m_bounds.x1:=Floor(bnd.x1 );
         m_bounds.y1:=Floor(bnd.y1 );
         m_bounds.x2:=Ceil(bnd.x2 );
         m_bounds.y2:=Ceil(bnd.y2 );
         m_advance_x:=int26p6_to_dbl(m_cur_face.glyph.advance.x );
         m_advance_y:=int26p6_to_dbl(m_cur_face.glyph.advance.y );

         m_affine.transform(@m_affine ,@m_advance_x ,@m_advance_y );

         result:=true;

        end;

      end
     else
      begin
       m_path16.remove_all;

       if decompose_ft_outline(
           @m_cur_face.glyph.outline ,
           m_flip_y ,
           @m_affine ,
           @m_path16 ) then
        begin
         bnd:=m_path16.bounding_rect;

         m_data_size:=m_path16.byte_size;
         m_data_type:=glyph_data_outline;
         m_bounds.x1:=Floor(bnd.x1 );
         m_bounds.y1:=Floor(bnd.y1 );
         m_bounds.x2:=Ceil(bnd.x2 );
         m_bounds.y2:=Ceil(bnd.y2 );
         m_advance_x:=int26p6_to_dbl(m_cur_face.glyph.advance.x );
         m_advance_y:=int26p6_to_dbl(m_cur_face.glyph.advance.y );

         m_affine.transform(@m_affine ,@m_advance_x ,@m_advance_y );

         result:=true;

        end;

      end;

   glyph_ren_agg_mono :
    if m_last_error = 0 then
     begin
      m_rasterizer.reset;

      if m_flag32 then
       begin
        m_path32.remove_all;

        decompose_ft_outline(
         @m_cur_face.glyph.outline ,
         m_flip_y ,
         @m_affine ,
         @m_path32 );

        m_rasterizer.add_path(@m_curves32 );

       end
      else
       begin
        m_path16.remove_all;

        decompose_ft_outline(
         @m_cur_face.glyph.outline ,
         m_flip_y ,
         @m_affine ,
         @m_path16 );

        m_rasterizer.add_path(@m_curves16 );

       end;

      m_scanlines_bin.prepare(1 ); // Remove all

      render_scanlines(@m_rasterizer ,@m_scanline_bin ,@m_scanlines_bin );

      m_bounds.x1:=m_scanlines_bin._min_x;
      m_bounds.y1:=m_scanlines_bin._min_y;
      m_bounds.x2:=m_scanlines_bin._max_x;
      m_bounds.y2:=m_scanlines_bin._max_y;
      m_data_size:=m_scanlines_bin.byte_size;
      m_data_type:=glyph_data_mono;
      m_advance_x:=int26p6_to_dbl(m_cur_face.glyph.advance.x );
      m_advance_y:=int26p6_to_dbl(m_cur_face.glyph.advance.y );

      m_affine.transform(@m_affine ,@m_advance_x ,@m_advance_y );

      result:=true;

     end;

   glyph_ren_agg_gray8 :
    if m_last_error = 0 then
     begin
      m_rasterizer.reset;

      if m_flag32 then
       begin
        m_path32.remove_all;

        decompose_ft_outline(
         @m_cur_face.glyph.outline ,
         m_flip_y ,
         @m_affine ,
         @m_path32 );

        m_rasterizer.add_path(@m_curves32 );

       end
      else
       begin
        m_path16.remove_all;

        decompose_ft_outline(
         @m_cur_face.glyph.outline ,
         m_flip_y ,
         @m_affine ,
         @m_path16 );

        m_rasterizer.add_path(@m_curves16 );

       end;

      m_scanlines_aa.prepare(1 ); // Remove all

      render_scanlines(@m_rasterizer ,@m_scanline_aa ,@m_scanlines_aa );

      m_bounds.x1:=m_scanlines_aa._min_x;
      m_bounds.y1:=m_scanlines_aa._min_y;
      m_bounds.x2:=m_scanlines_aa._max_x;
      m_bounds.y2:=m_scanlines_aa._max_y;
      m_data_size:=m_scanlines_aa.byte_size;
      m_data_type:=glyph_data_gray8;
      m_advance_x:=int26p6_to_dbl(m_cur_face.glyph.advance.x );
      m_advance_y:=int26p6_to_dbl(m_cur_face.glyph.advance.y );

      m_affine.transform(@m_affine ,@m_advance_x ,@m_advance_y );

      result:=true;

     end;

  end;

end;

{ GLYPH_INDEX }
function font_engine_freetype_base.glyph_index : unsigned;
begin
 result:=m_glyph_index;

end;

{ DATA_SIZE }
function font_engine_freetype_base.data_size : unsigned;
begin
 result:=m_data_size;

end;

{ DATA_TYPE }
function font_engine_freetype_base.data_type : unsigned;
begin
 result:=m_data_type;

end;

{ BOUNDS }
function font_engine_freetype_base.bounds : rect_ptr;
begin
 result:=@m_bounds;

end;

{ ADVANCE_X }
function font_engine_freetype_base.advance_x : double;
begin
 result:=m_advance_x;

end;

{ ADVANCE_Y }
function font_engine_freetype_base.advance_y : double;
begin
 result:=m_advance_y;

end;

{ WRITE_GLYPH_TO }
procedure font_engine_freetype_base.write_glyph_to(data : int8u_ptr );
begin
 if (data <> NIL ) and
    (m_data_size <> 0 ) then
  case m_data_type of
   glyph_data_mono :
    m_scanlines_bin.serialize(data );

   glyph_data_gray8 :
    m_scanlines_aa.serialize(data );

   glyph_data_outline :
    if m_flag32 then
     m_path32.serialize(data )
    else
     m_path16.serialize(data );

  end;

end;

{ ADD_KERNING }
function font_engine_freetype_base.add_kerning(first ,second : unsigned; x ,y : double_ptr ) : boolean;
var
 delta  : FT_Vector;
 dx ,dy : double;

begin
 if (m_cur_face <> NIL ) and
    (first <> 0 ) and
    (second <> 0 ) and
    FT_HAS_KERNING(m_cur_face ) then
  begin
   FT_Get_Kerning(m_cur_face ,first ,second ,FT_KERNING_DEFAULT ,@delta );

   dx:=int26p6_to_dbl(delta.x );
   dy:=int26p6_to_dbl(delta.y );

   if (m_glyph_rendering = glyph_ren_outline ) or
      (m_glyph_rendering = glyph_ren_agg_mono ) or
      (m_glyph_rendering = glyph_ren_agg_gray8 ) then
    m_affine.transform_2x2(@m_affine ,@dx ,@dy );

   x^:=x^ + dx;
   y^:=y^ + dy;

   result:=true;

  end
 else
  result:=false;

end;

{ FLAG32 }
function font_engine_freetype_base.flag32;
begin
 result:=m_flag32;

end;

{ UPDATE_CHAR_SIZE }
procedure font_engine_freetype_base.update_char_size;
begin
 if m_cur_face <> NIL then
  begin
   if m_resolution <> 0 then
    FT_Set_Char_Size(
     m_cur_face ,
     m_width ,       // char_width in 1/64th of points
     m_height ,      // char_height in 1/64th of points
     m_resolution ,  // horizontal device resolution
     m_resolution )  // vertical device resolution
   else
    FT_Set_Pixel_Sizes(
     m_cur_face ,
     m_width shr 6 ,    // pixel_width
     m_height shr 6 );  // pixel_height

   update_signature;

  end;

end;

{ UPDATE_SIGNATURE }
procedure font_engine_freetype_base.update_signature;
var
 name_len ,gamma_hash ,i : unsigned;

 gamma_table : array[0..aa_num - 1 ] of int8u;

 mtx : array[0..5 ] of double;
 buf : array[0..99 ] of char;
 str : char_ptr;

begin
 if (m_cur_face <> NIL ) and
    (m_name <> NIL ) then
  begin
   name_len:=StrLen(m_name );

   if name_len > m_name_len then
    begin
     agg_freemem(pointer(m_signature.name ) ,m_signature.size );

     m_signature.size:=name_len + 32 + 256;

     agg_getmem(pointer(m_signature.name ) ,m_signature.size );

     m_name_len:=name_len + 32 - 1;

    end;

   gamma_hash:=0;

   if (m_glyph_rendering = glyph_ren_native_gray8 ) or
      (m_glyph_rendering = glyph_ren_agg_mono ) or
      (m_glyph_rendering = glyph_ren_agg_gray8 ) then
    begin
     for i:=0 to aa_num - 1 do
      gamma_table[i ]:=m_rasterizer.apply_gamma(i );

     gamma_hash:=calc_crc32(@gamma_table ,sizeof(gamma_table ) );

    end;

   str:=m_signature.name;

   sprintf(str                                             ,'%s,'  ,ptrcomp(m_name ) );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%u,'  ,int(m_char_map ) );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,m_face_index );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,int(m_glyph_rendering ) );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d:'  ,m_resolution );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%dx'  ,m_height );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,m_width );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,int(m_hinting ) );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,int(m_flip_y ) );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%08X' ,gamma_hash );

   if (m_glyph_rendering = glyph_ren_outline ) or
      (m_glyph_rendering = glyph_ren_agg_mono ) or
      (m_glyph_rendering = glyph_ren_agg_gray8 ) then
    begin
     m_affine.store_to(@mtx );

     sprintf(@buf[0 ]                 ,',%08X' ,dbl_to_plain_fx(mtx[0 ] ) );
     sprintf(@buf[StrLen(@buf[0 ] ) ] ,'%08X'  ,dbl_to_plain_fx(mtx[1 ] ) );
     sprintf(@buf[StrLen(@buf[0 ] ) ] ,'%08X'  ,dbl_to_plain_fx(mtx[2 ] ) );
     sprintf(@buf[StrLen(@buf[0 ] ) ] ,'%08X'  ,dbl_to_plain_fx(mtx[3 ] ) );
     sprintf(@buf[StrLen(@buf[0 ] ) ] ,'%08X'  ,dbl_to_plain_fx(mtx[4 ] ) );
     sprintf(@buf[StrLen(@buf[0 ] ) ] ,'%08X'  ,dbl_to_plain_fx(mtx[5 ] ) );

     StrCat(PChar(m_signature.name ) ,buf );

    end;

   inc(m_change_stamp );

  end;

end;

{ FIND_FACE }
function font_engine_freetype_base.find_face(name : PChar ) : int;
var
 i : unsigned;
 n : face_name_ptr;

begin
 result:=-1;

 n:=m_face_names;
 i:=0;

 while i < m_num_faces do
  begin
   if StrComp(name ,PChar(n.name ) ) = 0 then
    begin
     result:=i;

     exit;

    end;

   inc(ptrcomp(n ) ,sizeof(face_name ) );
   inc(i );

  end;

end;

{ CONSTRUCT }
constructor font_engine_freetype_int16.Construct(max_faces : unsigned = 32 );
begin
 inherited Construct(false ,max_faces );

end;

{ CONSTRUCT }
constructor font_engine_freetype_int32.Construct(max_faces : unsigned = 32 );
begin
 inherited Construct(true ,max_faces );

end;

END.

