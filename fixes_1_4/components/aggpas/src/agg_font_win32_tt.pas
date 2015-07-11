//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
//
// Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.aggpas.org
// Copyright (c) 2005-2006
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
// 23.06.2006-Milano: ptrcomp adjustments
// 18.02.2006-Milano: Complete unit port
// 17.02.2006-Milano: Unit port establishment
//
{ agg_font_win32_tt.pas }
unit
 agg_font_win32_tt ;

INTERFACE

{$I agg_mode.inc }

uses
 Windows ,SysUtils ,Math ,
 agg_basics ,
 agg_array ,
 agg_bitset_iterator ,
 agg_scanline_storage_aa ,
 agg_scanline_storage_bin ,
 agg_scanline ,
 agg_scanline_u ,
 agg_scanline_bin ,
 agg_renderer_scanline ,
 agg_render_scanlines ,
 agg_path_storage_integer ,
 agg_rasterizer_scanline_aa ,
 agg_conv_curve ,
 agg_trans_affine ,
 agg_font_cache_manager ,
 agg_font_engine ,
 agg_vertex_source ;

{ TYPES DEFINITION }
const
 buf_size = 32768 - 32;

type
 winapi_GetGlyphOutlineX = function (DC: HDC; p2, p3: UINT; const p4: TGlyphMetrics; p5: DWORD; p6: Pointer; const p7: TMat2): DWORD; stdcall;

 HFONT_ptr = ^HFONT;
 PChar_ptr = ^PChar;

 KERNINGPAIR_ptr = ^KERNINGPAIR;

 FIXED_ptr = ^FIXED;

 TTPOLYGONHEADER_ptr = ^TTPOLYGONHEADER;

 TTPOLYCURVE_ptr = ^TTPOLYCURVE;

 POINTFX_ptr = ^POINTFX;

 font_name_ptr = ^font_name;
 font_name = record
   name : char_ptr; //PChar;
   size : unsigned;

  end;

//-----------------------------------------------font_engine_win32_tt_base
 font_engine_win32_tt_base = object(font_engine )
   m_flag32 : boolean;
   m_dc     : HDC;

   m_old_font : HFONT;
   m_fonts    : HFONT_ptr;

   m_num_fonts  ,
   m_max_fonts  : unsigned;
   m_font_names : font_name_ptr;
   m_cur_font   : HFONT;

   m_change_stamp : int;
   m_typeface     : font_name;
   m_typeface_len : unsigned;
   m_signature    : font_name;

   m_height   ,
   m_width    : unsigned;
   m_weight   : int;
   m_italic   : boolean;
   m_char_set : DWORD;

   m_pitch_and_family : DWORD;

   m_hinting ,
   m_flip_y  : boolean;

   m_font_created    : boolean;
   m_resolution      : unsigned;
   m_glyph_rendering : glyph_rendering;
   m_glyph_index     : unsigned;

   m_data_size ,
   m_data_type : unsigned;
   m_bounds    : rect;
   m_advance_x ,
   m_advance_y : double;

   m_matrix : TMAT2;
   m_gbuf   : int8u_ptr;

   m_kerning_pairs     : KERNINGPAIR_ptr;
   m_num_kerning_pairs ,
   m_max_kerning_pairs : unsigned;

   m_affine : trans_affine;

   m_path16   : path_storage_int16;
   m_path32   : path_storage_int32;
   m_curves16 ,
   m_curves32 : conv_curve;

   m_scanline_aa   : scanline_u8;
   m_scanline_bin  : scanline_bin;
   m_scanlines_aa  : scanlines_aa_type;
   m_scanlines_bin : scanlines_bin_type;
   m_rasterizer    : rasterizer_scanline_aa;

   constructor Construct(flag32_ : boolean; dc : HDC; max_fonts : unsigned = 32 );
   destructor  Destruct;

  // Set font parameters
   procedure resolution_(dpi : unsigned );
   procedure height_    (h : double );
   procedure width_     (w : double );
   procedure weight_    (w : int );
   procedure italic_    (it : boolean );
   procedure char_set_  (c : DWORD );

   procedure pitch_and_family_(p : DWORD );

   procedure flip_y_ (flip : boolean );
   procedure hinting_(h : boolean );

   function  create_font (typeface_ : PChar; ren_type : glyph_rendering ) : boolean;
   function  create_font_(
              typeface : PChar;
              ren_type : glyph_rendering;
              height : double;
              width : double = 0.0;
              weight : int = FW_REGULAR;
              italic : boolean = false;
              char_set : DWORD = ANSI_CHARSET;
              pitch_and_family : DWORD = FF_DONTCARE ) : boolean;

  // Set Gamma
   procedure gamma_    (f : vertex_source_ptr );
   procedure transform_(mtx : trans_affine_ptr );

  // Accessors
   function  _resolution : unsigned;
   function  _typeface : PChar;
   function  _height : double;
   function  _width : double;
   function  _weight : int;
   function  _italic : boolean;
   function  _char_set : DWORD;
   function  _pitch_and_family : DWORD;
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

  // Private
   procedure update_signature;
   function  pair_less(v1 ,v2 : KERNINGPAIR_ptr ) : boolean;

   procedure load_kerning_pairs;
   procedure sort_kerning_pairs;

   function  find_font(name : PChar ) : int;

  end;

//------------------------------------------------font_engine_win32_tt_int16
// This class uses values of type int16 (10.6 format) for the vector cache.
// The vector cache is compact, but when rendering glyphs of height
// more that 200 there integer overflow can occur.
 font_engine_win32_tt_int16 = object(font_engine_win32_tt_base )
   constructor Construct(dc : HDC; max_fonts : unsigned = 32 );

  end;


//------------------------------------------------font_engine_win32_tt_int32
// This class uses values of type int32 (26.6 format) for the vector cache.
// The vector cache is twice larger than in font_engine_win32_tt_int16,
// but it allows you to render glyphs of very large sizes.
 font_engine_win32_tt_int32 = object(font_engine_win32_tt_base )
   constructor Construct(dc : HDC; max_fonts : unsigned = 32 );

  end;

var
 GetGlyphOutlineX : winapi_GetGlyphOutlineX;

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

{ dbl_to_fx }
function dbl_to_fx(d : double ) : FIXED;
var
 l : int;

begin
 l:=trunc(d * 65536.0 );

 move(l ,result ,sizeof(int ) );

end;

{ dbl_to_plain_fx }
function dbl_to_plain_fx(d : double ) : int;
begin
 result:=trunc(d * 65536.0 );

end;

{ negate_fx }
function negate_fx(fx : FIXED_ptr ) : FIXED;
var
 l : int;

begin
 l:=-int(fx );

 move(l ,result ,sizeof(int ) );

end;

{ fx_to_dbl }
function fx_to_dbl(p : FIXED_ptr ) : double;
begin
 result:=p.value + p.fract * (1.0 / 65536.0 );

end;

{ fx_to_plain_int }
function fx_to_plain_int(fx : FIXED_ptr ) : int;
begin
 result:=int(fx );

end;

{ fx_to_int26p6 }
function fx_to_int26p6(p : FIXED_ptr ) : int;
begin
 result:=(int(p.value ) shl 6 ) + (shr_int32(int(p.fract ) ,10 ) );

end;

{ dbl_to_int26p6 }
function dbl_to_int26p6(p : double ) : int;
begin
 result:=trunc(p * 64.0 + 0.5 );

end;

{ decompose_win32_glyph_bitmap_mono }
procedure decompose_win32_glyph_bitmap_mono(
           gbuf : int8u_ptr;
           w ,h ,x ,y : int;
           flip_y : boolean;
           sl : scanline_ptr;
           storage : renderer_scanline_ptr );
var
 i ,pitch ,j : int;

 buf  : int8u_ptr;
 bits : bitset_iterator;

begin
 pitch:=shr_int32(w + 31 ,5 ) shl 2;
 buf  :=gbuf;

 sl.reset       (x ,x + w );
 storage.prepare(w + 2 );

 if flip_y then
  begin
   inc(ptrcomp(buf ) ,(pitch * (h - 1 ) ) * sizeof(int8u ) );
   inc(y ,h );

   pitch:=-pitch;

  end;

 i:=0;

 while i < h do
  begin
   sl.reset_spans;

   bits.Construct(buf ,0 );

   j:=0;

   while j < w do
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

{ decompose_win32_glyph_bitmap_gray8 }
procedure decompose_win32_glyph_bitmap_gray8(
           gbuf : int8u_ptr;
           w ,h ,x ,y : int;
           flip_y : boolean;
           ras : rasterizer_scanline_aa_ptr;
           sl : scanline_ptr;
           storage : renderer_scanline_ptr );
var
 i ,j ,pitch : int;

 buf ,p : int8u_ptr;

 v : unsigned;

begin
 pitch:=shr_int32(w + 3 ,2 ) shl 2;
 buf  :=gbuf;

 sl.reset       (x ,x + w );
 storage.prepare(w + 2 );

 if flip_y then
  begin
   inc(ptrcomp(buf ) ,(pitch * (h - 1 ) ) * sizeof(int8u ) );
   inc(y ,h );

   pitch:=-pitch;

  end;

 i:=0;

 while i < h do
  begin
   sl.reset_spans;

   p:=buf;
   j:=0;

   while j < w do
    begin
     if p^ <> 0 then
      begin
       v:=p^;

       if v = 64 then
        v:=255
       else
        v:=v shl 2;

       sl.add_cell(x + j ,ras.apply_gamma(v ) );

      end;

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

{ decompose_win32_glyph_outline }
function decompose_win32_glyph_outline(
          gbuf : int8u_ptr;
          total_size : unsigned;
          flip_y : boolean;
          mtx : trans_affine_ptr;
          path : path_storage_integer_ptr ) : boolean;
var
 cur_glyph ,end_glyph ,end_poly ,cur_poly : int8u_ptr;

 x ,y ,x2 ,y2 : double;

 i ,u : int;

 th : TTPOLYGONHEADER_ptr;
 pc : TTPOLYCURVE_ptr;

 pnt_b ,pnt_c : POINTFX;

 cx ,cy ,bx ,by : int;

begin
 cur_glyph:=gbuf;
 end_glyph:=int8u_ptr(ptrcomp(gbuf ) + total_size * sizeof(int8u ) );

 while ptrcomp(cur_glyph ) < ptrcomp(end_glyph ) do
  begin
   th:=TTPOLYGONHEADER_ptr(cur_glyph );

   end_poly:=int8u_ptr(ptrcomp(cur_glyph ) + th.cb );
   cur_poly:=int8u_ptr(ptrcomp(cur_glyph ) + sizeof(TTPOLYGONHEADER ) );

   x:=fx_to_dbl(@th.pfxStart.x );
   y:=fx_to_dbl(@th.pfxStart.y );

   if flip_y then
    y:=-y;

   mtx.transform(mtx ,@x ,@y );
   path.move_to (dbl_to_int26p6(x ) ,dbl_to_int26p6(y ) );

   while ptrcomp(cur_poly ) < ptrcomp(end_poly ) do
    begin
     pc:=TTPOLYCURVE_ptr(cur_poly );

     if pc.wType = TT_PRIM_LINE then
      begin
       i:=0;

       while i < pc.cpfx do
        begin
         x:=fx_to_dbl(@POINTFX_ptr(ptrcomp(@pc.apfx ) + i * sizeof(POINTFX ) ).x );
         y:=fx_to_dbl(@POINTFX_ptr(ptrcomp(@pc.apfx ) + i * sizeof(POINTFX ) ).y );

         if flip_y then
          y:=-y;

         mtx.transform(mtx ,@x ,@y );
         path.line_to (dbl_to_int26p6(x ) ,dbl_to_int26p6(y ) );

         inc(i );

        end;

      end;

     if pc.wType = TT_PRIM_QSPLINE then
      begin
       u:=0;

       while u < pc.cpfx - 1 do         // Walk through points in spline
        begin
        // B is always the current point
         pnt_b:=POINTFX_ptr(ptrcomp(@pc.apfx ) + u * sizeof(POINTFX ) )^;
         pnt_c:=POINTFX_ptr(ptrcomp(@pc.apfx ) + (u + 1 ) * sizeof(POINTFX ) )^;

         if u < pc.cpfx - 2 then        // If not on last spline, compute C
          begin
          // midpoint (x,y)
          // int(pnt_c.x ):=int(pnt_b.x ) + int(pnt_c.x ) div 2;
          // int(pnt_c.y ):=int(pnt_b.y ) + int(pnt_c.y ) div 2;
           move(pnt_b.x ,bx ,sizeof(int ) );
           move(pnt_b.y ,by ,sizeof(int ) );
           move(pnt_c.x ,cx ,sizeof(int ) );
           move(pnt_c.y ,cy ,sizeof(int ) );

           cx:=(bx + cx ) div 2;
           cy:=(by + cy ) div 2;

           move(cx ,pnt_c.x ,sizeof(int ) );
           move(cy ,pnt_c.y ,sizeof(int ) );

          end;

         x :=fx_to_dbl(@pnt_b.x );
         y :=fx_to_dbl(@pnt_b.y );
         x2:=fx_to_dbl(@pnt_c.x );
         y2:=fx_to_dbl(@pnt_c.y );

         if flip_y then
          begin
           y :=-y;
           y2:=-y2;

          end;

         mtx.transform(mtx ,@x  ,@y );
         mtx.transform(mtx ,@x2 ,@y2 );

         path.curve3(
          dbl_to_int26p6(x )  ,dbl_to_int26p6(y ) ,
          dbl_to_int26p6(x2 ) ,dbl_to_int26p6(y2 ) );

         inc(u );

        end;

      end;

     inc(ptrcomp(cur_poly ) ,sizeof(WORD ) * 2 + sizeof(POINTFX ) * pc.cpfx );

    end;

   inc(ptrcomp(cur_glyph ) ,th.cb );

  end;

 result:=true;

end;

{ CONSTRUCT }
constructor font_engine_win32_tt_base.Construct;
begin
 m_flag32:=flag32_;
 m_dc    :=dc;

 if m_dc <> 0 then
  m_old_font:=GetCurrentObject(m_dc ,OBJ_FONT )
 else
  m_old_font:=0;

 agg_getmem(pointer(m_fonts ) ,sizeof(HFONT ) * max_fonts );

 m_num_fonts:=0;
 m_max_fonts:=max_fonts;

 agg_getmem(pointer(m_font_names ) ,sizeof(font_name ) * max_fonts );

 m_cur_font    :=0;
 m_change_stamp:=0;

 m_typeface.size:=sizeof(char ) * (256 - 16 );

 agg_getmem(pointer(m_typeface.name ) ,m_typeface.size );

 m_typeface_len:=256 - 16 - 1;

 m_signature.size:=sizeof(char ) *  (256 + 256 - 16 );

 agg_getmem(pointer(m_signature.name ) ,m_signature.size );

 m_height  :=0;
 m_width   :=0;
 m_weight  :=FW_REGULAR;
 m_italic  :=false;
 m_char_set:=DEFAULT_CHARSET;

 m_pitch_and_family:=FF_DONTCARE;

 m_hinting:=true;
 m_flip_y :=false;

 m_font_created   :=false;
 m_resolution     :=0;
 m_glyph_rendering:=glyph_ren_native_gray8;

 m_glyph_index:=0;
 m_data_size  :=0;
 m_data_type  :=glyph_data_invalid;

 m_bounds.Construct(1 ,1 ,0 ,0 );

 m_advance_x:=0.0;
 m_advance_y:=0.0;

 agg_getmem(pointer(m_gbuf ) ,sizeof(int8u ) * buf_size );

 m_kerning_pairs    :=NIL;
 m_num_kerning_pairs:=0;
 m_max_kerning_pairs:=0;

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

 fillchar(m_matrix ,sizeof(m_matrix ) ,0 );

 m_matrix.eM11.value:=1;
 m_matrix.eM22.value:=1;

end;

{ DESTRUCT }
destructor font_engine_win32_tt_base.Destruct;
var
 i : unsigned;
 f : HFONT_ptr;
 n : font_name_ptr;

begin
 agg_freemem(pointer(m_kerning_pairs ) ,m_max_kerning_pairs * sizeof(KERNINGPAIR ) );
 agg_freemem(pointer(m_gbuf ) ,sizeof(int8u ) * buf_size );
 agg_freemem(pointer(m_signature.name ) ,m_signature.size );
 agg_freemem(pointer(m_typeface.name ) ,m_typeface.size );

 if (m_dc <> 0 ) and
    (m_old_font <> 0 ) then
  SelectObject(m_dc ,m_old_font );

 i:=0;
 f:=m_fonts;
 n:=m_font_names;

 while i < m_num_fonts do
  begin
   agg_freemem (pointer(n.name ) ,n.size );
   DeleteObject(f^ );

   inc(ptrcomp(n ) ,sizeof(font_name ) );
   inc(ptrcomp(f ) ,sizeof(HFONT ) );
   inc(i );

  end;

 agg_freemem(pointer(m_font_names ) ,sizeof(font_name ) * m_max_fonts );
 agg_freemem(pointer(m_fonts ) ,sizeof(HFONT ) * m_max_fonts );

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
procedure font_engine_win32_tt_base.resolution_;
begin
 m_resolution:=dpi;

end;

{ HEIGHT_ }
procedure font_engine_win32_tt_base.height_;
begin
 m_height:=trunc(h );

end;

{ WIDTH_ }
procedure font_engine_win32_tt_base.width_;
begin
 m_width:=trunc(w );

end;

{ WEIGHT_ }
procedure font_engine_win32_tt_base.weight_;
begin
 m_weight:=w;

end;

{ ITALIC_ }
procedure font_engine_win32_tt_base.italic_;
begin
 m_italic:=it;

end;

{ CHAR_SET_ }
procedure font_engine_win32_tt_base.char_set_;
begin
 m_char_set:=c;

end;

{ PITCH_AND_FAMILY_ }
procedure font_engine_win32_tt_base.pitch_and_family_;
begin
 m_pitch_and_family:=p;

end;

{ FLIP_Y_ }
procedure font_engine_win32_tt_base.flip_y_;
begin
 m_flip_y:=flip;

end;

{ HINTING_ }
procedure font_engine_win32_tt_base.hinting_;
begin
 m_hinting:=h;

end;

{ CREATE_FONT }
function font_engine_win32_tt_base.create_font;
var
 len : unsigned;

 h ,w ,idx : int;

 f : HFONT_ptr;
 n : font_name_ptr;

begin
 if m_dc <> 0 then
  begin
   len:=StrLen(typeface_ );

   if len > m_typeface_len then
    begin
     agg_freemem(pointer(m_signature.name ) ,m_signature.size );
     agg_freemem(pointer(m_typeface.name ) ,m_typeface.size );

     m_typeface.size :=sizeof(char ) * (len + 32 );
     m_signature.size:=sizeof(char ) * (len + 32 + 256 );

     agg_getmem(pointer(m_typeface.name ) ,m_typeface.size );
     agg_getmem(pointer(m_signature.name ) ,m_signature.size );

     m_typeface_len:=len + 32 - 1;

    end;

   StrCopy(PChar(m_typeface.name ) ,typeface_ );

   h:=m_height;
   w:=m_width;

   if m_resolution <> 0 then
    begin
     h:=MulDiv(m_height ,m_resolution ,72 );
     w:=MulDiv(m_width  ,m_resolution ,72 );

    end;

   m_glyph_rendering:=ren_type;

   update_signature;

   idx:=find_font(PChar(m_signature.name ) );

   if idx >= 0 then
    begin
     m_cur_font:=HFONT_ptr(ptrcomp(m_fonts ) + idx * sizeof(HFONT ) )^;

     SelectObject(m_dc ,m_cur_font );

     m_num_kerning_pairs:=0;

     result:=true;

     exit;

    end
   else
    begin
     m_cur_font:=
      CreateFont(
       -h ,                     // height of font
       w ,                      // average character width
       0 ,                      // angle of escapement
       0 ,                      // base-line orientation angle
       m_weight ,               // font weight
       unsigned(m_italic ) ,    // italic attribute option
       0 ,                      // underline attribute option
       0 ,                      // strikeout attribute option
       m_char_set ,             // character set identifier
       OUT_DEFAULT_PRECIS ,     // output precision
       CLIP_DEFAULT_PRECIS ,    // clipping precision
       ANTIALIASED_QUALITY ,    // output quality
       m_pitch_and_family ,     // pitch and family
       PChar(m_typeface.name )  // typeface name
       );

     if m_cur_font <> 0 then
      begin
       if m_num_fonts >= m_max_fonts then
        begin
         agg_freemem(pointer(m_font_names.name ) ,m_font_names.size );

         if m_old_font <> 0 then
          SelectObject(m_dc ,m_old_font );

         DeleteObject(m_fonts^ );

         move(
          HFONT_ptr(ptrcomp(m_fonts ) + 1 * sizeof(HFONT ) )^ ,
          m_fonts^ ,
          (m_max_fonts - 1 ) * sizeof(HFONT ) );

         move(
          font_name_ptr(ptrcomp(m_font_names ) + 1 * sizeof(font_name ) )^ ,
          m_font_names^ ,
          (m_max_fonts - 1 ) * sizeof(font_name ) );

         m_num_fonts:=m_max_fonts - 1;

        end;

       update_signature;

       n:=font_name_ptr(ptrcomp(m_font_names ) + m_num_fonts * sizeof(font_name ) );
       f:=HFONT_ptr(ptrcomp(m_fonts ) + m_num_fonts * sizeof(HFONT ) );

       n.size:=sizeof(char ) * (StrLen(PChar(m_signature.name ) ) + 1 );

       agg_getmem(pointer(n.name ) ,n.size );
       StrCopy   (PChar(n.name ) ,PChar(m_signature.name ) );

       f^:=m_cur_font;

       inc(m_num_fonts );

       SelectObject(m_dc ,m_cur_font );

       m_num_kerning_pairs:=0;

       result:=true;

       exit;

      end;

    end;

  end;

 result:=false;

end;

{ CREATE_FONT_ }
function font_engine_win32_tt_base.create_font_;
begin
 height_  (height );
 width_   (width );
 weight_  (weight );
 italic_  (italic );
 char_set_(char_set );

 pitch_and_family_(pitch_and_family );

 result:=create_font(typeface ,ren_type );

end;

{ GAMMA_ }
procedure font_engine_win32_tt_base.gamma_;
begin
 m_rasterizer.gamma(f );

end;

{ TRANSFORM_ }
procedure font_engine_win32_tt_base.transform_;
begin
 m_affine.assign_all(mtx );

end;

{ _RESOLUTION }
function font_engine_win32_tt_base._resolution;
begin
 result:=m_resolution;

end;

{ _TYPEFACE }
function font_engine_win32_tt_base._typeface;
begin
 result:=PChar(m_typeface.name );

end;

{ _HEIGHT }
function font_engine_win32_tt_base._height;
begin
 result:=m_height;

end;

{ _WIDTH }
function font_engine_win32_tt_base._width;
begin
 result:=m_width;

end;

{ _WEIGHT }
function font_engine_win32_tt_base._weight;
begin
 result:=m_weight;

end;

{ _ITALIC }
function font_engine_win32_tt_base._italic;
begin
 result:=m_italic;

end;

{ _CHAR_SET }
function font_engine_win32_tt_base._char_set;
begin
 result:=m_char_set;

end;

{ _PITCH_AND_FAMILY }
function font_engine_win32_tt_base._pitch_and_family;
begin
 result:=m_pitch_and_family;

end;

{ _HINTING }
function font_engine_win32_tt_base._hinting;
begin
 result:=m_hinting;

end;

{ _FLIP_Y }
function font_engine_win32_tt_base._flip_y;
begin
 result:=m_flip_y;

end;

{ FONT_SIGNATURE }
function font_engine_win32_tt_base.font_signature;
begin
 result:=PChar(m_signature.name );

end;

{ CHANGE_STAMP }
function font_engine_win32_tt_base.change_stamp;
begin
 result:=m_change_stamp;

end;

{ PREPARE_GLYPH }
function font_engine_win32_tt_base.prepare_glyph;
const
 GGO_UNHINTED = $0100; // For compatibility with old SDKs.

var
 format ,total_size : int;

 gm : TGLYPHMETRICS;
 fl : longint;
 ts : DWORD;

 bnd : rect_d;

begin
 if (m_dc <> 0 ) and
    (m_cur_font <> 0 ) then
  begin
   format:=GGO_BITMAP;

   case m_glyph_rendering of
    glyph_ren_native_gray8 :
     format:=GGO_GRAY8_BITMAP;

    glyph_ren_outline   ,
    glyph_ren_agg_mono  ,
    glyph_ren_agg_gray8 :
     format:=GGO_NATIVE;

   end;

   if not m_hinting then
    format:=format or GGO_UNHINTED;

   ts:=
    GetGlyphOutlineX(
     m_dc ,
     glyph_code ,
     format ,
     gm ,
     buf_size ,
     m_gbuf ,
     m_matrix );

   move(ts ,total_size ,sizeof(int ) );

   if total_size < 0 then
    begin
    // GetGlyphOutline() fails when being called for
    // GGO_GRAY8_BITMAP and white space (stupid Microsoft).
    // It doesn't even initialize the glyph metrics
    // structure. So, we have to query the metrics
    // separately (basically we need gmCellIncX).
     ts:=
      GetGlyphOutlineX(
       m_dc ,
       glyph_code ,
       GGO_METRICS ,
       gm ,
       buf_size ,
       m_gbuf ,
       m_matrix );

     move(ts ,total_size ,sizeof(int ) );

     if total_size < 0 then
      begin
       result:=false;

       exit;

      end;

     gm.gmBlackBoxX:=0;
     gm.gmBlackBoxY:=0;

     total_size:=0;

    end;

   m_glyph_index:=glyph_code;
   m_advance_x  :=gm.gmCellIncX;
   m_advance_y  :=-gm.gmCellIncY;

   case m_glyph_rendering of
    glyph_ren_native_mono :
     begin
      if m_flip_y then
       fl:=-gm.gmptGlyphOrigin.y
      else
       fl:=gm.gmptGlyphOrigin.y;

      decompose_win32_glyph_bitmap_mono(
       m_gbuf ,
       gm.gmBlackBoxX ,
       gm.gmBlackBoxY ,
       gm.gmptGlyphOrigin.x ,
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

      result:=true;

      exit;

     end;

    glyph_ren_native_gray8 :
     begin
      if m_flip_y then
       fl:=-gm.gmptGlyphOrigin.y
      else
       fl:=gm.gmptGlyphOrigin.y;

      decompose_win32_glyph_bitmap_gray8(
       m_gbuf ,
       gm.gmBlackBoxX ,
       gm.gmBlackBoxY ,
       gm.gmptGlyphOrigin.x ,
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

      result:=true;

      exit;

     end;

    glyph_ren_outline :
     begin
      m_affine.transform(@m_affine ,@m_advance_x ,@m_advance_y );

      if m_flag32 then
       begin
        m_path32.remove_all;

        if decompose_win32_glyph_outline(
            m_gbuf ,
            total_size ,
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

          result:=true;

          exit;
         
         end;

       end
      else
       begin
        m_path16.remove_all;

        if decompose_win32_glyph_outline(
            m_gbuf ,
            total_size ,
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

          result:=true;

          exit;

         end;

       end;

     end;

    glyph_ren_agg_mono :
     begin
      m_rasterizer.reset;
      m_affine.transform(@m_affine ,@m_advance_x ,@m_advance_y );

      if m_flag32 then
       begin
        m_path32.remove_all;

        decompose_win32_glyph_outline(
         m_gbuf ,
         total_size ,
         m_flip_y ,
         @m_affine ,
         @m_path32 );

        m_rasterizer.add_path(@m_curves32 );

       end
      else
       begin
        m_path16.remove_all;

        decompose_win32_glyph_outline(
         m_gbuf ,
         total_size ,
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

      result:=true;

      exit;

     end;

    glyph_ren_agg_gray8 :
     begin
      m_rasterizer.reset;
      m_affine.transform(@m_affine ,@m_advance_x ,@m_advance_y );

      if m_flag32 then
       begin
        m_path32.remove_all;

        decompose_win32_glyph_outline(
         m_gbuf ,
         total_size ,
         m_flip_y ,
         @m_affine ,
         @m_path32 );

        m_rasterizer.add_path(@m_curves32 );

       end
      else
       begin
        m_path16.remove_all;

        decompose_win32_glyph_outline(
         m_gbuf ,
         total_size,
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

      result:=true;

      exit;

     end;

   end;

  end;

 result:=false;

end;

{ GLYPH_INDEX }
function font_engine_win32_tt_base.glyph_index;
begin
 result:=m_glyph_index;

end;

{ DATA_SIZE }
function font_engine_win32_tt_base.data_size;
begin
 result:=m_data_size;

end;

{ DATA_TYPE }
function font_engine_win32_tt_base.data_type;
begin
 result:=m_data_type;

end;

{ BOUNDS }
function font_engine_win32_tt_base.bounds;
begin
 result:=@m_bounds;

end;

{ ADVANCE_X }
function font_engine_win32_tt_base.advance_x;
begin
 result:=m_advance_x;

end;

{ ADVANCE_Y }
function font_engine_win32_tt_base.advance_y;
begin
 result:=m_advance_y;

end;

{ WRITE_GLYPH_TO }
procedure font_engine_win32_tt_base.write_glyph_to;
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
function font_engine_win32_tt_base.add_kerning;
var
 end_ ,beg_ ,mid : int;

 dx ,dy : double;

 t  : KERNINGPAIR;
 kp : KERNINGPAIR_ptr;

begin
 if (m_dc <> 0 ) and
    (m_cur_font <> 0 ) then
  begin
   if m_num_kerning_pairs = 0 then
    load_kerning_pairs;

   end_:=m_num_kerning_pairs; dec(end_ );
   beg_:=0;

   t.wFirst :=WORD(first );
   t.wSecond:=WORD(second );

   while beg_ <= end_ do
    begin
     mid:=(end_ + beg_ ) div 2;
     kp :=KERNINGPAIR_ptr(ptrcomp(m_kerning_pairs ) + mid * sizeof(KERNINGPAIR ) );

     if (kp.wFirst  = t.wFirst ) and
        (kp.wSecond = t.wSecond ) then
      begin
       dx:=kp.iKernAmount;
       dy:=0.0;

       if (m_glyph_rendering = glyph_ren_outline ) or
          (m_glyph_rendering = glyph_ren_agg_mono ) or
          (m_glyph_rendering = glyph_ren_agg_gray8 ) then
        m_affine.transform_2x2(@m_affine ,@dx ,@dy );

       x^:=x^ + dx;
       y^:=y^ + dy;

       result:=true;

       exit;

      end
     else
      if pair_less(@t ,kp ) then
       end_:=mid - 1
      else
       beg_:=mid + 1;

    end;

  end;

 result:=false;

end;

{ FLAG32 }
function font_engine_win32_tt_base.flag32;
begin
 result:=m_flag32;

end;

{ UPDATE_SIGNATURE }
procedure font_engine_win32_tt_base.update_signature;
var
 gamma_hash ,i : unsigned;

 gamma_table : array[0..aa_num - 1 ] of int8u;

 mtx : array[0..5 ] of double;
 buf : array[0..99 ] of char;
 str : char_ptr;

begin
 m_signature.name^:=#0;

 if (m_dc <> 0 ) and
    (m_cur_font <> 0 ) then
  begin
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

   sprintf(str                                             ,'%s,'  ,ptrcomp(m_typeface.name ) );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%u,'  ,m_char_set );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,int(m_glyph_rendering ) );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d:'  ,m_resolution );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%dx'  ,m_height );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,m_width );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,m_weight );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,int(m_italic ) );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,int(m_hinting) );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,int(m_flip_y ) );
   sprintf(char_ptr(ptrcomp(str ) + StrLen(PChar(str ) ) ) ,'%d,'  ,int(m_pitch_and_family ) );
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

{ PAIR_LESS }
function font_engine_win32_tt_base.pair_less;
begin
 if v1.wFirst <> v2.wFirst then
  result:=v1.wFirst < v2.wFirst
 else
  result:=v1.wSecond < v2.wSecond;

end;

{ LOAD_KERNING_PAIRS }
procedure font_engine_win32_tt_base.load_kerning_pairs;
var
 i : unsigned;

begin
 if (m_dc <> 0 ) and
    (m_cur_font <> 0 ) then
  begin
   if m_kerning_pairs = NIL then
    begin
     m_max_kerning_pairs:=16384 - 16;

     agg_getmem(pointer(m_kerning_pairs ) ,m_max_kerning_pairs * sizeof(KERNINGPAIR ) );

    end;

   m_num_kerning_pairs:=
    GetKerningPairs(m_dc ,m_max_kerning_pairs ,m_kerning_pairs^ );

   if m_num_kerning_pairs <> 0 then
    begin
    // Check to see if the kerning pairs are sorted and
    // sort them if they are not.
     i:=1;

     while i < m_num_kerning_pairs do
      begin
       if not pair_less(
           KERNINGPAIR_ptr(ptrcomp(m_kerning_pairs ) + (i - 1 ) * sizeof(KERNINGPAIR ) ) ,
           KERNINGPAIR_ptr(ptrcomp(m_kerning_pairs ) + i * sizeof(KERNINGPAIR ) ) ) then
        begin
         sort_kerning_pairs;

         break;

        end;

       inc(i );

      end;

    end;

  end;

end;

{ _pair_less }
function _pair_less(v1 ,v2 : KERNINGPAIR_ptr ) : boolean;
begin
 if v1.wFirst <> v2.wFirst then
  result:=v1.wFirst < v2.wFirst
 else
  result:=v1.wSecond < v2.wSecond;

end;

{ SORT_KERNING_PAIRS }
procedure font_engine_win32_tt_base.sort_kerning_pairs;
var
 pairs : pod_array_adaptor;

begin
 pairs.Construct(m_kerning_pairs ,m_num_kerning_pairs ,sizeof(KERNINGPAIR ) );

 quick_sort(@pairs ,@_pair_less );

end;

{ FIND_FONT }
function font_engine_win32_tt_base.find_font;
var
 i : unsigned;
 n : font_name_ptr;

begin
 n:=m_font_names;
 i:=0;

 while i < m_num_fonts do
  begin
   if StrComp(name ,PChar(n.name ) ) = 0 then
    begin
     result:=i;

     exit;

    end;

   inc(ptrcomp(n ) ,sizeof(font_name ) );
   inc(i );

  end;

 result:=-1;

end;

{ CONSTRUCT }
constructor font_engine_win32_tt_int16.Construct;
begin
 inherited Construct(false ,dc ,max_fonts );

end;

{ CONSTRUCT }
constructor font_engine_win32_tt_int32.Construct;
begin
 inherited Construct(true ,dc ,max_fonts );

end;

{ UNIT CONSTRUCTOR }
INITIALIZATION
{$IFDEF AGG_WIN9X_COMPLIANT }
 GetGlyphOutlineX:=@GetGlyphOutline;

{$ELSE }
 GetGlyphOutlineX:=@GetGlyphOutlineW;

{$ENDIF }

END.

