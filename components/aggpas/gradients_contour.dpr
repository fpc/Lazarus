//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
// To flood England (4 colors):
//
//   C2: 512    D2: 226
//   C1: 17     D1: 0
//
program
 gradients_contour ;

uses
 SysUtils ,Math ,

 agg_basics ,
 agg_array ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,
 agg_pixfmt_gray ,

 agg_ctrl ,
 agg_slider_ctrl ,
 agg_cbox_ctrl ,
 agg_rbox_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_renderer_primitives ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_scanline_p ,
 agg_render_scanlines ,

 agg_conv_curve ,
 agg_conv_stroke ,
 agg_conv_transform ,
 agg_gsv_text ,
 agg_pixfmt_amask_adaptor ,
 agg_alpha_mask_u8 ,
 agg_vertex_source ,
 agg_path_storage ,
 agg_trans_affine ,
 agg_math_stroke ,
 agg_bounding_rect ,
 agg_gradient_lut ,
 agg_span_allocator ,
 agg_span_gradient ,
 agg_span_gradient_image ,
 agg_span_gradient_contour ,
 agg_span_interpolator_linear ,

 make_gb_poly_ ,
 make_arrows_ ;

{$I agg_mode.inc }

const
 flip_y = true;

type
 gradient_conic_angle = object(gradient )
   function  calculate(x ,y ,d : int ) : int; virtual;

  end;

 spiral = object(vertex_source )
   m_x    ,
   m_y    ,
   m_r1   ,
   m_r2   ,
   m_step ,

   m_start_angle ,

   m_angle  ,
   m_curr_r ,
   m_da     ,
   m_dr     : double;
   m_start  : boolean;

   constructor Construct(x ,y ,r1 ,r2 ,step : double; start_angle : double = 0 );

   procedure rewind(path_id : unsigned ); virtual;
   function  vertex(x ,y : double_ptr ) : unsigned; virtual;

  end;

 the_application = object(platform_support )
   m_polygons ,
   m_gradient : rbox_ctrl;

   m_stroke ,
   m_refl   : cbox_ctrl;

   m_c1   ,
   m_c2   ,
   m_d1   ,
   m_d2   ,
   m_clrs : slider_ctrl;

   m_ras : rasterizer_scanline_aa;
   m_sl  : scanline_u8;

   m_gamma : double;

   m_colors02 ,
   m_colors03 ,
   m_colors04 ,
   m_colors05 ,
   m_colors06 ,
   m_colors07 ,
   m_colors08 ,
   m_colors09 ,
   m_colors10 ,
   m_colors11 : gradient_lut;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure draw_text(x ,y : double; str : char_ptr );

   procedure perform_rendering(vs : vertex_source_ptr; contour : path_storage_ptr );
   function  render : unsigned;

   procedure on_draw; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CALCULATE }
function gradient_conic_angle.calculate;
var
 res : double;

begin
 res:=ArcTan2(y ,x );

 if res < 0 then
  result:=Abs(1600 - trunc(Abs(res ) * d / pi / 2 ) )
 else
  result:=trunc(res * d / pi / 2 )

end;
  
{ CONSTRUCT }
constructor spiral.Construct;
begin
 m_x :=x;
 m_y :=y;
 m_r1:=r1;
 m_r2:=r2;

 m_step       :=step;
 m_start_angle:=start_angle;
 m_angle      :=start_angle;

 m_da:=deg2rad(4.0 );
 m_dr:=m_step / 90.0;

end;

{ REWIND }
procedure spiral.rewind;
begin
 m_angle :=m_start_angle;
 m_curr_r:=m_r1;
 m_start :=true;

end;

{ VERTEX }
function spiral.vertex;
begin
 if m_curr_r > m_r2 then
  begin
   result:=path_cmd_stop;

   exit;

  end;

 x^:=m_x + Cos(m_angle ) * m_curr_r;
 y^:=m_y + Sin(m_angle ) * m_curr_r;

 m_curr_r:=m_curr_r + m_dr;
 m_angle :=m_angle + m_da;

 if m_start then
  begin
   m_start:=false;

   result:=path_cmd_move_to;

  end
 else
  result:=path_cmd_line_to;

end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 rgba : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

 m_polygons.Construct(5.0   ,5.0  ,5.0 + 130.0  ,90.0 ,not flip_y_ );
 m_gradient.Construct(145.0 ,5.0  ,220.0 + 80.0 ,90.0 ,not flip_y_ );
 m_stroke.Construct  (305.0 ,77.0 ,'Bitmap Gradient' ,not flip_y_ );
 m_refl.Construct    (440.0 ,77.0 ,'Reflect' ,not flip_y_ );
 m_c1.Construct      (310.0 ,10.0 ,400.0 ,16.0 ,not flip_y_ );
 m_c2.Construct      (310.0 ,30.0 ,400.0 ,36.0 ,not flip_y_ );
 m_d1.Construct      (410.0 ,10.0 ,500.0 ,16.0 ,not flip_y_ );
 m_d2.Construct      (410.0 ,30.0 ,500.0 ,36.0 ,not flip_y_ );
 m_clrs.Construct    (310.0 ,50.0 ,500.0 ,58.0 ,not flip_y_ );

 m_ras.Construct;
 m_sl.Construct;

 m_gradient.add_item ('Contour' );
 m_gradient.add_item ('Auto Contour' );
 m_gradient.add_item ('Assymetric Conic' );
 m_gradient.add_item ('Flat Fill' );
 m_gradient.cur_item_(1 );

 add_ctrl(@m_gradient );

 m_gradient.no_transform;

 m_polygons.add_item ('Simple Path' );
 m_polygons.add_item ('Great Britain' );
 m_polygons.add_item ('Spiral' );
 m_polygons.add_item ('Glyph' );
 m_polygons.cur_item_(0 );

 add_ctrl(@m_polygons );

 m_polygons.no_transform;

 add_ctrl(@m_stroke );

 m_stroke.status_(false );

 m_stroke.no_transform;

 add_ctrl(@m_refl );

 m_refl.status_(true );

 m_refl.no_transform;

 add_ctrl(@m_c1 );

 m_c1.label_('C1=%1.0f' );
 m_c1.range_(0.0 ,512.0 );
 m_c1.value_(0.0 );
 m_c1.no_transform;

 add_ctrl(@m_c2 );

 m_c2.label_('C2=%1.0f' );
 m_c2.range_(0.0 ,512.0 );
 m_c2.value_(512.0 );
 m_c2.no_transform;

 add_ctrl(@m_d1 );

 m_d1.label_('D1=%1.0f' );
 m_d1.range_(0.0 ,512.0 );
 m_d1.value_(0.0 );
 m_d1.no_transform;

 add_ctrl(@m_d2 );

 m_d2.label_('D2=%1.0f' );
 m_d2.range_(0.0 ,512.0 );
 m_d2.value_(100.0 );
 m_d2.no_transform;

 add_ctrl(@m_clrs );

 m_clrs.label_('Colors=%1.0f' );
 m_clrs.range_(2.0 ,11.0 );
 m_clrs.value_(2.0 );
 m_clrs.num_steps_(9 );
 m_clrs.no_transform;

{ 2 colors gradient }
 m_colors02.Construct(1024 );
 m_colors02.remove_all;

  rgba.ConstrInt      (178 ,34 ,34 );
  m_colors02.add_color(0.0 ,@rgba );

  rgba.ConstrInt      (255 ,255 ,0 );
  m_colors02.add_color(1.0 ,@rgba );

 m_colors02.build_lut;

{ 3 colors gradient }
 m_colors03.Construct(1024 );
 m_colors03.remove_all;

  rgba.ConstrInt      (245 ,233 ,131 );
  m_colors03.add_color(0.0 ,@rgba );

  rgba.ConstrInt      (146 ,35 ,219 );
  m_colors03.add_color(0.5 ,@rgba );

  rgba.ConstrInt      (255 ,35 ,0 );
  m_colors03.add_color(1.0 ,@rgba );

 m_colors03.build_lut;

{ 4 colors gradient }
 m_colors04.Construct(1024 );
 m_colors04.remove_all;

  rgba.ConstrInt      (0 ,255 ,0 );
  m_colors04.add_color(1.0 ,@rgba );

  rgba.ConstrInt      (120 ,0 ,0 );
  m_colors04.add_color(0.7 ,@rgba );

  rgba.ConstrInt      (120 ,120 ,0 );
  m_colors04.add_color(0.2 ,@rgba );

  rgba.ConstrInt      (0 ,0 ,255 );
  m_colors04.add_color(0.0 ,@rgba );

 m_colors04.build_lut;

{ 5 colors gradient }
 m_colors05.Construct(1024 );
 m_colors05.remove_all;

  rgba.ConstrInt      (230 ,188 ,106 );
  m_colors05.add_color(0.2 ,@rgba );

  rgba.ConstrInt      (207 ,148 ,31 );
  m_colors05.add_color(0.4 ,@rgba );

  rgba.ConstrInt      (69 ,56 ,30 );
  m_colors05.add_color(0.6 ,@rgba );

  rgba.ConstrInt      (43 ,33 ,13 );
  m_colors05.add_color(0.8 ,@rgba );

  rgba.ConstrInt      (227 ,221 ,209 );
  m_colors05.add_color(1.0 ,@rgba );

 m_colors05.build_lut;

{ 6 colors gradient }
 m_colors06.Construct(1024 );
 m_colors06.remove_all;

  rgba.ConstrInt      (125 ,99 ,255 );
  m_colors06.add_color(0.0 ,@rgba );

  rgba.ConstrInt      (118 ,79 ,210 );
  m_colors06.add_color(0.2 ,@rgba );

  rgba.ConstrInt      (105 ,58 ,81 );
  m_colors06.add_color(0.4 ,@rgba );

  rgba.ConstrInt      (217 ,74 ,102 );
  m_colors06.add_color(0.6 ,@rgba );

  rgba.ConstrInt      (242 ,148 ,90 );
  m_colors06.add_color(0.8 ,@rgba );

  rgba.ConstrInt      (242 ,200 ,102 );
  m_colors06.add_color(1.0 ,@rgba );

 m_colors06.build_lut;

{ 7 colors gradient }
 m_colors07.Construct(1024 );
 m_colors07.remove_all;

  rgba.ConstrInt      (216 ,237 ,232 );
  m_colors07.add_color(0.0 ,@rgba );

  rgba.ConstrInt      (196 ,214 ,226 );
  m_colors07.add_color(0.16 ,@rgba );

  rgba.ConstrInt      (175 ,194 ,217 );
  m_colors07.add_color(0.32 ,@rgba );

  rgba.ConstrInt      (155 ,176 ,210 );
  m_colors07.add_color(0.48 ,@rgba );

  rgba.ConstrInt      (140 ,162 ,202 );
  m_colors07.add_color(0.64 ,@rgba );

  rgba.ConstrInt      (130 ,149 ,193 );
  m_colors07.add_color(0.80 ,@rgba );

  rgba.ConstrInt      (72 ,102 ,165 );
  m_colors07.add_color(1.0 ,@rgba );

 m_colors07.build_lut;

{ 8 colors gradient }
 m_colors08.Construct(1024 );
 m_colors08.remove_all;

  rgba.ConstrInt      (255 ,223 ,168 );
  m_colors08.add_color(0.0 ,@rgba );

  rgba.ConstrInt      (255 ,199 ,162 );
  m_colors08.add_color(0.14 ,@rgba );

  rgba.ConstrInt      (255 ,175 ,156 );
  m_colors08.add_color(0.28 ,@rgba );

  rgba.ConstrInt      (255 ,151 ,151 );
  m_colors08.add_color(0.42 ,@rgba );

  rgba.ConstrInt      (255 ,127 ,145 );
  m_colors08.add_color(0.56 ,@rgba );

  rgba.ConstrInt      (255 ,104 ,140 );
  m_colors08.add_color(0.70 ,@rgba );

  rgba.ConstrInt      (255 ,80 ,133 );
  m_colors08.add_color(0.84 ,@rgba );

  rgba.ConstrInt      (255 ,56 ,128 );
  m_colors08.add_color(1.0 ,@rgba );

 m_colors08.build_lut;

{ 9 colors gradient }
 m_colors09.Construct(1024 );
 m_colors09.remove_all;

  rgba.ConstrInt      (255 ,4 ,163 );
  m_colors09.add_color(0.0 ,@rgba );

  rgba.ConstrInt      (255 ,4 ,109 );
  m_colors09.add_color(0.125 ,@rgba );

  rgba.ConstrInt      (255 ,4 ,46 );
  m_colors09.add_color(0.25 ,@rgba );

  rgba.ConstrInt      (255 ,75 ,75 );
  m_colors09.add_color(0.375 ,@rgba );

  rgba.ConstrInt      (255 ,120 ,83 );
  m_colors09.add_color(0.5 ,@rgba );

  rgba.ConstrInt      (255 ,143 ,83 );
  m_colors09.add_color(0.625 ,@rgba );

  rgba.ConstrInt      (255 ,180 ,83 );
  m_colors09.add_color(0.75 ,@rgba );

  rgba.ConstrInt      (255 ,209 ,83 );
  m_colors09.add_color(0.875 ,@rgba );

  rgba.ConstrInt      (255 ,246 ,83 );
  m_colors09.add_color(1.0 ,@rgba );

 m_colors09.build_lut;

{ 10 colors gradient }
 m_colors10.Construct(1024 );
 m_colors10.remove_all;

  rgba.ConstrInt      (255 ,0 ,0 );
  m_colors10.add_color(0.0 ,@rgba );

  rgba.ConstrInt      (255 ,198 ,198 );
  m_colors10.add_color(0.11 ,@rgba );

  rgba.ConstrInt      (255 ,255 ,0 );
  m_colors10.add_color(0.22 ,@rgba );

  rgba.ConstrInt      (255 ,255 ,226 );
  m_colors10.add_color(0.33 ,@rgba );

  rgba.ConstrInt      (85 ,85 ,255 );
  m_colors10.add_color(0.44 ,@rgba );

  rgba.ConstrInt      (226 ,226 ,255 );
  m_colors10.add_color(0.55 ,@rgba );

  rgba.ConstrInt      (28 ,255 ,28 );
  m_colors10.add_color(0.66 ,@rgba );

  rgba.ConstrInt      (226 ,255 ,226 );
  m_colors10.add_color(0.77 ,@rgba );

  rgba.ConstrInt      (255 ,72 ,255 );
  m_colors10.add_color(0.88 ,@rgba );

  rgba.ConstrInt      (255 ,227 ,255 );
  m_colors10.add_color(1.0 ,@rgba );

 m_colors10.build_lut;

{ 11 colors gradient }
 m_colors11.Construct(1024 );
 m_colors11.remove_all;

  m_gamma:=1.8;

  rgba.from_wavelength(380 ,m_gamma );
  m_colors11.add_color(0.0 ,@rgba );

  rgba.from_wavelength(420 ,m_gamma );
  m_colors11.add_color(0.1 ,@rgba );

  rgba.from_wavelength(460 ,m_gamma );
  m_colors11.add_color(0.2 ,@rgba );

  rgba.from_wavelength(500 ,m_gamma );
  m_colors11.add_color(0.3 ,@rgba );

  rgba.from_wavelength(540 ,m_gamma );
  m_colors11.add_color(0.4 ,@rgba );

  rgba.from_wavelength(580 ,m_gamma );
  m_colors11.add_color(0.5 ,@rgba );

  rgba.from_wavelength(620 ,m_gamma );
  m_colors11.add_color(0.6 ,@rgba );

  rgba.from_wavelength(660 ,m_gamma );
  m_colors11.add_color(0.7 ,@rgba );

  rgba.from_wavelength(700 ,m_gamma );
  m_colors11.add_color(0.8 ,@rgba );

  rgba.from_wavelength(740 ,m_gamma );
  m_colors11.add_color(0.9 ,@rgba );

  rgba.from_wavelength(780 ,m_gamma );
  m_colors11.add_color(1.0 ,@rgba );

 m_colors11.build_lut;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_polygons.Destruct;
 m_gradient.Destruct;
 m_stroke.Destruct;
 m_refl.Destruct;
 m_c1.Destruct;
 m_c2.Destruct;
 m_d1.Destruct;
 m_d2.Destruct;
 m_clrs.Destruct;

 m_ras.Destruct;
 m_sl.Destruct;

 m_colors02.Destruct;
 m_colors03.Destruct;
 m_colors04.Destruct;
 m_colors05.Destruct;
 m_colors06.Destruct;
 m_colors07.Destruct;
 m_colors08.Destruct;
 m_colors09.Destruct;
 m_colors10.Destruct;
 m_colors11.Destruct;

end;

{ DRAW_TEXT }
procedure the_application.draw_text;
var
 pf  : pixel_formats;
 rb  : renderer_base;
 ren : renderer_scanline_aa_solid;
 txt : gsv_text;

 txt_stroke : conv_stroke;

 rgba : aggclr;

begin
 pixfmt_bgr24(pf ,rbuf_window );

 rb.Construct (@pf );
 ren.Construct(@rb );

 txt.Construct;

 txt_stroke.Construct(@txt );
 txt_stroke.width_   (1.5 );
 txt_stroke.line_cap_(round_cap );

 txt.size_       (10.0 );
 txt.start_point_(x ,y );
 txt.text_       (PChar(str ) );

 m_ras.add_path  (@txt_stroke);
 rgba.ConstrDbl  (0.0 ,0.0 ,0.0 );
 ren.color_      (@rgba );
 render_scanlines(@m_ras ,@m_sl ,@ren );

 txt.Destruct;
 txt_stroke.Destruct;

end;

const
 puzzle : array[0..16383 ] of byte = (
  $02,$51,$AE,$FF,$00,$5C,$B0,$FF,$0A,$85,$FF,$FF,$3B,$87,$95,$FF,$28,$27,$12,$FF,
  $5C,$6F,$03,$FF,$88,$AD,$08,$FF,$90,$AC,$00,$FF,$8E,$AC,$00,$FF,$8B,$AE,$00,$FF,
  $8C,$AD,$00,$FF,$9E,$BE,$05,$FF,$A5,$C7,$2B,$FF,$5A,$89,$67,$FF,$17,$45,$52,$FF,
  $00,$36,$80,$FF,$03,$53,$AA,$FF,$00,$54,$A4,$FF,$00,$52,$B1,$FF,$04,$55,$B1,$FF,
  $00,$50,$AC,$FF,$00,$52,$A9,$FF,$00,$4F,$98,$FF,$00,$43,$84,$FF,$00,$48,$9C,$FF,
  $06,$55,$A4,$FF,$03,$4F,$A5,$FF,$01,$50,$AC,$FF,$00,$54,$AE,$FF,$05,$4E,$AD,$FF,
  $01,$54,$BA,$FF,$00,$51,$AC,$FF,$00,$51,$AC,$FF,$03,$52,$AF,$FF,$00,$50,$B1,$FF,
  $03,$4F,$AF,$FF,$04,$4F,$AA,$FF,$00,$56,$A7,$FF,$00,$55,$AE,$FF,$05,$4E,$B5,$FF,
  $00,$53,$A9,$FF,$00,$52,$A4,$FF,$00,$51,$AF,$FF,$01,$53,$A7,$FF,$01,$53,$A9,$FF,
  $04,$51,$AB,$FF,$03,$58,$B2,$FF,$00,$74,$F6,$FF,$14,$79,$D1,$FF,$34,$49,$22,$FF,
  $5D,$75,$00,$FF,$8A,$B0,$01,$FF,$8F,$A4,$00,$FF,$8E,$B0,$02,$FF,$8B,$AC,$07,$FF,
  $8D,$AC,$00,$FF,$C0,$D9,$04,$FF,$91,$CF,$2E,$FF,$11,$39,$43,$FF,$00,$28,$58,$FF,
  $00,$4C,$99,$FF,$00,$54,$AA,$FF,$00,$50,$A9,$FF,$03,$53,$AA,$FF,$05,$52,$AE,$FF,
  $00,$59,$B0,$FF,$00,$72,$EE,$FF,$35,$76,$92,$FF,$2C,$2C,$12,$FF,$64,$7A,$00,$FF,
  $89,$B0,$00,$FF,$88,$AE,$00,$FF,$91,$AD,$00,$FF,$84,$AD,$06,$FF,$8F,$AE,$0A,$FF,
  $9D,$BB,$00,$FF,$BE,$F0,$13,$FF,$57,$8D,$76,$FF,$08,$2E,$41,$FF,$01,$44,$8A,$FF,
  $00,$52,$A8,$FF,$01,$4F,$B3,$FF,$05,$4D,$95,$FF,$00,$40,$7E,$FF,$04,$3F,$8F,$FF,
  $0C,$41,$8F,$FF,$00,$20,$3E,$FF,$00,$07,$12,$FF,$08,$23,$4E,$FF,$00,$3D,$8C,$FF,
  $00,$58,$A8,$FF,$03,$56,$A6,$FF,$04,$50,$B0,$FF,$03,$51,$A6,$FF,$04,$51,$AB,$FF,
  $00,$53,$A6,$FF,$00,$52,$AC,$FF,$04,$56,$A2,$FF,$04,$55,$AE,$FF,$02,$53,$AE,$FF,
  $00,$50,$AC,$FF,$00,$53,$A6,$FF,$06,$50,$A7,$FF,$06,$50,$A5,$FF,$01,$52,$AE,$FF,
  $01,$55,$B5,$FF,$00,$55,$B1,$FF,$02,$4F,$AB,$FF,$07,$52,$B7,$FF,$00,$4F,$AC,$FF,
  $00,$51,$A5,$FF,$01,$6D,$D0,$FF,$01,$72,$D8,$FF,$2B,$45,$20,$FF,$3E,$4C,$00,$FF,
  $82,$A6,$08,$FF,$8E,$AA,$00,$FF,$8B,$AD,$01,$FF,$8C,$AD,$00,$FF,$8D,$AE,$00,$FF,
  $A3,$CD,$00,$FF,$B6,$F7,$17,$FF,$34,$6C,$6B,$FF,$01,$16,$2B,$FF,$04,$31,$6C,$FF,
  $00,$47,$9B,$FF,$00,$58,$A9,$FF,$06,$50,$A9,$FF,$00,$45,$87,$FF,$00,$4D,$98,$FF,
  $03,$56,$B2,$FF,$31,$60,$6A,$FF,$44,$55,$0F,$FF,$70,$97,$00,$FF,$8B,$B1,$00,$FF,
  $8D,$AA,$02,$FF,$8E,$AB,$03,$FF,$8D,$AD,$02,$FF,$89,$AB,$0C,$FF,$93,$BA,$09,$FF,
  $C2,$EB,$37,$FF,$3B,$77,$91,$FF,$05,$3A,$6E,$FF,$00,$4F,$99,$FF,$00,$53,$A7,$FF,
  $00,$51,$A0,$FF,$07,$37,$83,$FF,$0C,$26,$41,$FF,$00,$21,$46,$FF,$16,$56,$71,$FF,
  $32,$6B,$65,$FF,$14,$27,$2D,$FF,$00,$08,$02,$FF,$00,$1B,$29,$FF,$00,$38,$82,$FF,
  $00,$50,$A8,$FF,$01,$55,$AB,$FF,$07,$53,$B3,$FF,$00,$51,$A3,$FF,$06,$52,$B4,$FF,
  $00,$58,$A8,$FF,$00,$4F,$AB,$FF,$00,$4D,$B9,$FF,$07,$51,$B2,$FF,$06,$52,$AA,$FF,
  $03,$50,$B8,$FF,$09,$49,$A0,$FF,$00,$3C,$7B,$FF,$00,$2F,$62,$FF,$02,$27,$5E,$FF,
  $04,$30,$5D,$FF,$00,$38,$74,$FF,$00,$48,$9D,$FF,$04,$56,$AC,$FF,$02,$51,$AE,$FF,
  $06,$67,$D0,$FF,$00,$67,$E5,$FF,$40,$5C,$45,$FF,$49,$54,$06,$FF,$78,$9D,$00,$FF,
  $8E,$B0,$00,$FF,$8B,$AB,$00,$FF,$8B,$A9,$00,$FF,$8E,$AD,$09,$FF,$93,$B3,$0A,$FF,
  $CD,$FD,$07,$FF,$AB,$E7,$4B,$FF,$29,$49,$58,$FF,$01,$0E,$17,$FF,$0A,$23,$42,$FF,
  $00,$30,$69,$FF,$00,$46,$90,$FF,$01,$12,$2C,$FF,$00,$24,$56,$FF,$19,$52,$70,$FF,
  $4A,$72,$3E,$FF,$74,$92,$0C,$FF,$84,$A9,$00,$FF,$8D,$A9,$00,$FF,$91,$AC,$05,$FF,
  $8D,$AE,$05,$FF,$8A,$A7,$00,$FF,$90,$B5,$05,$FF,$9C,$C9,$0A,$FF,$A8,$C3,$34,$FF,
  $13,$4B,$70,$FF,$00,$35,$5D,$FF,$00,$49,$92,$FF,$02,$4D,$9E,$FF,$07,$51,$98,$FF,
  $16,$57,$7F,$FF,$29,$59,$5D,$FF,$47,$6C,$39,$FF,$93,$BC,$2F,$FF,$D0,$FF,$15,$FF,
  $CF,$F7,$36,$FF,$5E,$85,$59,$FF,$07,$10,$1F,$FF,$07,$0A,$29,$FF,$00,$43,$84,$FF,
  $03,$56,$B2,$FF,$04,$49,$A4,$FF,$00,$55,$A9,$FF,$00,$55,$B6,$FF,$01,$50,$AF,$FF,
  $08,$55,$AF,$FF,$00,$58,$A6,$FF,$02,$56,$AE,$FF,$00,$50,$AA,$FF,$00,$53,$B6,$FF,
  $0A,$40,$6C,$FF,$00,$18,$2C,$FF,$02,$09,$0F,$FF,$04,$00,$02,$FF,$04,$02,$05,$FF,
  $04,$0B,$15,$FF,$00,$18,$37,$FF,$03,$42,$85,$FF,$00,$55,$AF,$FF,$00,$6E,$CE,$FF,
  $17,$68,$A9,$FF,$42,$5D,$28,$FF,$69,$7D,$00,$FF,$82,$A7,$01,$FF,$8B,$AD,$00,$FF,
  $8A,$AC,$00,$FF,$90,$AD,$05,$FF,$8A,$AC,$00,$FF,$8B,$AE,$00,$FF,$A3,$CE,$00,$FF,
  $CF,$F8,$30,$FF,$AE,$D0,$57,$FF,$31,$5F,$45,$FF,$01,$15,$2D,$FF,$00,$14,$1F,$FF,
  $03,$0C,$2B,$FF,$28,$5E,$68,$FF,$3E,$78,$50,$FF,$74,$AA,$3A,$FF,$82,$A2,$19,$FF,
  $85,$A5,$02,$FF,$87,$B0,$00,$FF,$8E,$AF,$00,$FF,$8B,$A8,$02,$FF,$8B,$AE,$00,$FF,
  $8B,$B0,$01,$FF,$94,$B8,$00,$FF,$B1,$D8,$01,$FF,$AA,$C5,$2A,$FF,$1E,$46,$5F,$FF,
  $0C,$2D,$4C,$FF,$07,$3D,$85,$FF,$16,$56,$79,$FF,$1B,$59,$72,$FF,$4B,$83,$52,$FF,
  $93,$B9,$2E,$FF,$AD,$CA,$10,$FF,$A8,$CB,$00,$FF,$B0,$D8,$06,$FF,$CD,$FF,$03,$FF,
  $D1,$FF,$11,$FF,$7D,$A2,$42,$FF,$04,$08,$14,$FF,$00,$24,$51,$FF,$00,$53,$AE,$FF,
  $06,$54,$A7,$FF,$05,$51,$B1,$FF,$00,$53,$A1,$FF,$00,$50,$AC,$FF,$00,$52,$B2,$FF,
  $00,$51,$AB,$FF,$03,$53,$A8,$FF,$01,$56,$B0,$FF,$0B,$5A,$95,$FF,$21,$49,$48,$FF,
  $36,$4A,$2F,$FF,$5F,$78,$25,$FF,$6C,$A2,$42,$FF,$53,$8F,$5C,$FF,$33,$4A,$36,$FF,
  $00,$05,$00,$FF,$00,$21,$40,$FF,$07,$43,$81,$FF,$00,$4B,$9D,$FF,$1A,$41,$3E,$FF,
  $3D,$4F,$05,$FF,$7B,$98,$00,$FF,$89,$AD,$05,$FF,$8E,$AA,$0A,$FF,$8B,$AE,$00,$FF,
  $8D,$AD,$04,$FF,$8A,$AB,$00,$FF,$90,$AE,$00,$FF,$84,$B6,$00,$FF,$AC,$D7,$03,$FF,
  $DA,$FA,$1D,$FF,$B7,$F7,$49,$FF,$8C,$BA,$72,$FF,$4D,$78,$6F,$FF,$3B,$6D,$54,$FF,
  $D6,$FF,$1A,$FF,$B4,$E0,$11,$FF,$A3,$D0,$03,$FF,$91,$B4,$00,$FF,$8B,$AC,$00,$FF,
  $8F,$AC,$04,$FF,$8D,$A9,$00,$FF,$8F,$AE,$00,$FF,$8B,$AB,$02,$FF,$8B,$AA,$06,$FF,
  $8D,$B3,$00,$FF,$A9,$D4,$00,$FF,$D3,$FE,$2A,$FF,$72,$A9,$43,$FF,$05,$33,$40,$FF,
  $0D,$40,$6D,$FF,$33,$74,$8A,$FF,$5B,$82,$53,$FF,$95,$B6,$0F,$FF,$9C,$C4,$0B,$FF,
  $87,$B7,$00,$FF,$8A,$B3,$00,$FF,$89,$A9,$00,$FF,$92,$B4,$00,$FF,$CE,$F4,$03,$FF,
  $D0,$FF,$17,$FF,$0F,$29,$28,$FF,$05,$1D,$39,$FF,$00,$48,$A0,$FF,$00,$55,$AD,$FF,
  $09,$50,$AC,$FF,$03,$52,$AF,$FF,$08,$53,$AE,$FF,$00,$52,$AE,$FF,$00,$4D,$B5,$FF,
  $00,$5E,$B2,$FF,$11,$77,$CB,$FF,$33,$71,$6E,$FF,$57,$6C,$0F,$FF,$87,$A5,$00,$FF,
  $9E,$CF,$02,$FF,$C4,$F7,$06,$FF,$CA,$FF,$0E,$FF,$DA,$FE,$22,$FF,$7C,$9C,$31,$FF,
  $0E,$40,$73,$FF,$00,$3C,$83,$FF,$1A,$5D,$65,$FF,$4C,$5B,$06,$FF,$66,$79,$05,$FF,
  $87,$A6,$02,$FF,$89,$AB,$00,$FF,$8F,$AC,$04,$FF,$8E,$B0,$00,$FF,$87,$A9,$00,$FF,
  $8E,$AD,$07,$FF,$8F,$AB,$09,$FF,$90,$AC,$00,$FF,$93,$AF,$02,$FF,$A8,$CE,$00,$FF,
  $BF,$F6,$07,$FF,$CD,$FF,$06,$FF,$DA,$FB,$14,$FF,$D2,$FF,$20,$FF,$99,$BF,$00,$FF,
  $98,$B3,$02,$FF,$92,$AA,$00,$FF,$89,$A8,$04,$FF,$8D,$AF,$00,$FF,$8A,$AF,$00,$FF,
  $89,$AA,$03,$FF,$90,$A9,$07,$FF,$8E,$AF,$00,$FF,$8F,$A9,$00,$FF,$89,$AF,$00,$FF,
  $92,$BB,$05,$FF,$B7,$E3,$08,$FF,$C0,$F7,$28,$FF,$76,$B8,$7B,$FF,$55,$7F,$73,$FF,
  $8E,$BA,$27,$FF,$93,$BF,$0A,$FF,$88,$A9,$00,$FF,$8D,$A8,$01,$FF,$8B,$AA,$04,$FF,
  $89,$AE,$00,$FF,$90,$B1,$0A,$FF,$8B,$A8,$00,$FF,$9C,$D1,$00,$FF,$CE,$FB,$00,$FF,
  $3A,$59,$4A,$FF,$00,$07,$00,$FF,$04,$3C,$79,$FF,$01,$53,$A9,$FF,$00,$51,$AB,$FF,
  $00,$54,$B1,$FF,$00,$4F,$A9,$FF,$01,$56,$A7,$FF,$04,$53,$B2,$FF,$00,$62,$C8,$FF,
  $28,$7A,$B4,$FF,$47,$66,$2D,$FF,$6A,$80,$04,$FF,$81,$A6,$00,$FF,$8E,$B0,$00,$FF,
  $9C,$B0,$03,$FF,$95,$B9,$01,$FF,$C0,$EF,$01,$FF,$CD,$F7,$00,$FF,$B8,$DF,$20,$FF,
  $AA,$CE,$26,$FF,$9E,$C5,$14,$FF,$9D,$BB,$01,$FF,$8D,$AC,$00,$FF,$8D,$AB,$00,$FF,
  $8E,$AF,$00,$FF,$8B,$AD,$00,$FF,$8E,$AC,$00,$FF,$8E,$AC,$00,$FF,$8D,$AD,$00,$FF,
  $84,$AC,$00,$FF,$93,$B1,$04,$FF,$83,$B0,$00,$FF,$8D,$A4,$00,$FF,$85,$B2,$00,$FF,
  $92,$B6,$00,$FF,$99,$C1,$00,$FF,$99,$BC,$00,$FF,$88,$AF,$08,$FF,$8E,$AB,$01,$FF,
  $8E,$AD,$00,$FF,$8A,$AE,$02,$FF,$8E,$AE,$03,$FF,$89,$A7,$00,$FF,$8D,$B4,$01,$FF,
  $87,$AC,$00,$FF,$8C,$AD,$00,$FF,$89,$AD,$05,$FF,$8C,$AD,$00,$FF,$94,$A8,$09,$FF,
  $96,$B6,$00,$FF,$BE,$DA,$00,$FF,$BE,$FB,$00,$FF,$B5,$DE,$00,$FF,$97,$BF,$00,$FF,
  $8E,$B8,$00,$FF,$89,$B0,$00,$FF,$8F,$AE,$08,$FF,$90,$AB,$00,$FF,$8F,$AD,$00,$FF,
  $8A,$AA,$00,$FF,$8C,$AD,$04,$FF,$99,$B2,$00,$FF,$CF,$FF,$0C,$FF,$48,$78,$46,$FF,
  $02,$07,$0B,$FF,$00,$36,$7F,$FF,$07,$51,$B0,$FF,$00,$54,$A9,$FF,$03,$4F,$AD,$FF,
  $01,$52,$AD,$FF,$05,$51,$AF,$FF,$00,$55,$B1,$FF,$02,$70,$EB,$FF,$38,$74,$98,$FF,
  $3D,$48,$03,$FF,$76,$9C,$08,$FF,$97,$B7,$00,$FF,$88,$AE,$00,$FF,$91,$A8,$00,$FF,
  $8C,$B1,$01,$FF,$88,$AF,$00,$FF,$91,$B4,$00,$FF,$AB,$D9,$00,$FF,$B0,$DE,$00,$FF,
  $A5,$CE,$06,$FF,$8E,$B7,$05,$FF,$8D,$AE,$09,$FF,$91,$AC,$00,$FF,$8A,$A9,$03,$FF,
  $87,$AF,$01,$FF,$8E,$AB,$01,$FF,$8F,$A8,$06,$FF,$8B,$AD,$00,$FF,$90,$AD,$00,$FF,
  $86,$AC,$00,$FF,$90,$AC,$02,$FF,$94,$AC,$00,$FF,$8B,$AB,$00,$FF,$90,$AD,$03,$FF,
  $89,$AB,$00,$FF,$94,$AB,$03,$FF,$8A,$B0,$00,$FF,$90,$AC,$02,$FF,$91,$AC,$00,$FF,
  $8B,$A6,$00,$FF,$8E,$AF,$00,$FF,$8C,$AF,$00,$FF,$8A,$AD,$00,$FF,$8A,$AA,$07,$FF,
  $8D,$AF,$00,$FF,$8E,$AB,$01,$FF,$8E,$AA,$00,$FF,$88,$AC,$04,$FF,$89,$AE,$00,$FF,
  $8B,$AC,$00,$FF,$91,$AD,$03,$FF,$8B,$AB,$00,$FF,$83,$A4,$00,$FF,$86,$A2,$02,$FF,
  $89,$A3,$02,$FF,$85,$A4,$00,$FF,$84,$A5,$00,$FF,$8D,$AE,$00,$FF,$8D,$AA,$00,$FF,
  $8C,$AC,$09,$FF,$8F,$B8,$00,$FF,$C2,$ED,$08,$FF,$49,$63,$4A,$FF,$00,$1A,$2D,$FF,
  $00,$44,$85,$FF,$01,$4D,$AD,$FF,$00,$55,$AB,$FF,$01,$52,$AB,$FF,$04,$4F,$B5,$FF,
  $03,$51,$A6,$FF,$00,$5D,$AA,$FF,$0D,$84,$FF,$FF,$4A,$79,$73,$FF,$1D,$29,$00,$FF,
  $6B,$80,$07,$FF,$8F,$AF,$02,$FF,$84,$B1,$00,$FF,$8F,$AC,$04,$FF,$90,$AA,$00,$FF,
  $8E,$AE,$01,$FF,$8A,$AD,$00,$FF,$8B,$AC,$05,$FF,$8D,$B0,$00,$FF,$8A,$AE,$00,$FF,
  $8A,$AD,$00,$FF,$8C,$AD,$04,$FF,$8E,$AC,$00,$FF,$8A,$AB,$04,$FF,$8E,$B1,$00,$FF,
  $8E,$A8,$06,$FF,$91,$AD,$03,$FF,$89,$AB,$00,$FF,$8D,$A9,$00,$FF,$8F,$AD,$00,$FF,
  $8D,$AC,$08,$FF,$8A,$AB,$06,$FF,$8C,$AD,$00,$FF,$8D,$AE,$00,$FF,$8A,$AA,$00,$FF,
  $8E,$AD,$07,$FF,$88,$AE,$03,$FF,$8D,$AD,$02,$FF,$8B,$A8,$02,$FF,$8E,$B0,$04,$FF,
  $8A,$AF,$00,$FF,$89,$A6,$00,$FF,$93,$AC,$08,$FF,$93,$AB,$00,$FF,$8E,$AD,$07,$FF,
  $84,$AA,$00,$FF,$8C,$B1,$00,$FF,$91,$AE,$08,$FF,$8F,$AB,$00,$FF,$83,$A4,$0B,$FF,
  $7A,$9C,$00,$FF,$69,$85,$00,$FF,$52,$6B,$03,$FF,$47,$5E,$04,$FF,$54,$70,$00,$FF,
  $62,$7E,$05,$FF,$75,$90,$03,$FF,$8C,$AD,$00,$FF,$89,$AA,$03,$FF,$82,$A9,$00,$FF,
  $90,$A9,$0F,$FF,$85,$A6,$0F,$FF,$25,$47,$49,$FF,$00,$29,$5D,$FF,$03,$4C,$99,$FF,
  $04,$51,$AD,$FF,$01,$55,$AB,$FF,$04,$4F,$B4,$FF,$00,$56,$B0,$FF,$03,$4F,$A7,$FF,
  $04,$5A,$BD,$FF,$08,$7F,$FF,$FF,$46,$7E,$71,$FF,$1B,$19,$04,$FF,$5D,$7C,$00,$FF,
  $8A,$AB,$00,$FF,$8E,$AC,$00,$FF,$8B,$AA,$04,$FF,$8C,$B2,$00,$FF,$86,$AA,$00,$FF,
  $8B,$AC,$05,$FF,$8C,$AD,$06,$FF,$8E,$AD,$00,$FF,$93,$A8,$0D,$FF,$8A,$AF,$07,$FF,
  $85,$AC,$00,$FF,$8C,$AD,$00,$FF,$91,$B0,$00,$FF,$89,$AA,$00,$FF,$8A,$AD,$00,$FF,
  $89,$AB,$00,$FF,$8F,$B0,$01,$FF,$8C,$AC,$00,$FF,$8E,$AC,$00,$FF,$8E,$AB,$03,$FF,
  $8B,$AB,$00,$FF,$93,$AE,$01,$FF,$8B,$A8,$02,$FF,$8D,$B0,$00,$FF,$8A,$AB,$00,$FF,
  $8B,$AB,$00,$FF,$88,$AA,$00,$FF,$90,$B1,$00,$FF,$8F,$A8,$02,$FF,$89,$AA,$00,$FF,
  $8A,$B5,$06,$FF,$87,$B1,$00,$FF,$8C,$AB,$00,$FF,$93,$A9,$00,$FF,$8C,$A9,$00,$FF,
  $8C,$AB,$05,$FF,$8A,$A8,$00,$FF,$86,$A9,$00,$FF,$71,$93,$01,$FF,$5F,$7E,$00,$FF,
  $3D,$49,$0B,$FF,$35,$40,$16,$FF,$3F,$57,$0B,$FF,$37,$4D,$0D,$FF,$1C,$28,$00,$FF,
  $3C,$48,$0A,$FF,$56,$68,$04,$FF,$64,$7C,$04,$FF,$52,$6B,$10,$FF,$57,$7E,$2F,$FF,
  $46,$71,$43,$FF,$1D,$55,$70,$FF,$08,$46,$93,$FF,$01,$4F,$A1,$FF,$05,$53,$A5,$FF,
  $03,$56,$A4,$FF,$00,$4D,$AC,$FF,$02,$4D,$B2,$FF,$00,$54,$AB,$FF,$03,$59,$B0,$FF,
  $05,$85,$FF,$FF,$44,$8B,$79,$FF,$01,$12,$00,$FF,$53,$6D,$00,$FF,$88,$A3,$00,$FF,
  $93,$A9,$06,$FF,$8E,$AC,$00,$FF,$8D,$AB,$00,$FF,$90,$AC,$00,$FF,$90,$AC,$09,$FF,
  $89,$B1,$01,$FF,$73,$98,$00,$FF,$75,$95,$03,$FF,$7F,$99,$00,$FF,$85,$A3,$05,$FF,
  $8B,$AF,$07,$FF,$93,$B2,$00,$FF,$8D,$A9,$06,$FF,$8B,$B2,$01,$FF,$89,$AE,$00,$FF,
  $8D,$AA,$00,$FF,$83,$B3,$00,$FF,$8C,$AD,$00,$FF,$90,$AC,$02,$FF,$8A,$AA,$00,$FF,
  $90,$AA,$09,$FF,$89,$AF,$00,$FF,$88,$AE,$01,$FF,$91,$AA,$06,$FF,$86,$A5,$02,$FF,
  $83,$A9,$00,$FF,$85,$AE,$00,$FF,$88,$AD,$05,$FF,$8E,$AF,$00,$FF,$8D,$A8,$03,$FF,
  $8F,$AC,$06,$FF,$8D,$AC,$06,$FF,$8B,$AE,$0A,$FF,$8C,$AD,$00,$FF,$8B,$B1,$02,$FF,
  $89,$AC,$00,$FF,$74,$94,$0D,$FF,$56,$6E,$00,$FF,$56,$7A,$20,$FF,$31,$59,$50,$FF,
  $00,$59,$B3,$FF,$00,$76,$FF,$FF,$2C,$89,$B5,$FF,$3F,$5E,$3F,$FF,$24,$34,$19,$FF,
  $22,$2E,$00,$FF,$2E,$3D,$14,$FF,$35,$64,$5A,$FF,$19,$67,$8D,$FF,$0D,$5A,$92,$FF,
  $05,$53,$93,$FF,$04,$4D,$AB,$FF,$03,$4F,$AF,$FF,$02,$52,$AB,$FF,$00,$52,$AA,$FF,
  $02,$55,$B5,$FF,$08,$54,$AA,$FF,$00,$50,$B0,$FF,$00,$56,$AE,$FF,$01,$77,$FB,$FF,
  $3A,$94,$D2,$FF,$22,$2B,$10,$FF,$2E,$2B,$00,$FF,$57,$78,$03,$FF,$7A,$99,$00,$FF,
  $7D,$A6,$00,$FF,$8B,$B1,$0C,$FF,$86,$A5,$02,$FF,$80,$9A,$00,$FF,$61,$75,$00,$FF,
  $4E,$62,$0D,$FF,$52,$75,$1B,$FF,$46,$51,$03,$FF,$4A,$51,$0B,$FF,$6E,$8E,$00,$FF,
  $89,$B0,$00,$FF,$8F,$AD,$00,$FF,$8C,$AC,$00,$FF,$90,$AF,$00,$FF,$8D,$AB,$00,$FF,
  $86,$AC,$00,$FF,$91,$AA,$04,$FF,$8A,$AC,$00,$FF,$8A,$AE,$02,$FF,$85,$A4,$00,$FF,
  $7B,$9E,$00,$FF,$74,$98,$00,$FF,$84,$9B,$05,$FF,$3C,$45,$06,$FF,$54,$68,$07,$FF,
  $74,$92,$00,$FF,$85,$A1,$00,$FF,$90,$AB,$06,$FF,$8B,$AB,$00,$FF,$8B,$AE,$00,$FF,
  $8B,$AB,$00,$FF,$84,$AA,$00,$FF,$91,$B0,$00,$FF,$90,$B4,$0A,$FF,$90,$A8,$08,$FF,
  $6D,$8A,$1C,$FF,$2A,$48,$48,$FF,$0D,$59,$AF,$FF,$00,$73,$D8,$FF,$00,$5C,$B6,$FF,
  $07,$71,$E9,$FF,$00,$86,$F9,$FF,$0A,$8E,$F2,$FF,$24,$78,$D2,$FF,$2A,$5C,$8F,$FF,
  $14,$5E,$8F,$FF,$00,$64,$CA,$FF,$09,$72,$DB,$FF,$02,$63,$C2,$FF,$00,$53,$AD,$FF,
  $02,$51,$AD,$FF,$06,$55,$B4,$FF,$00,$53,$AD,$FF,$00,$4E,$AA,$FF,$00,$53,$AF,$FF,
  $02,$4C,$AB,$FF,$06,$55,$B1,$FF,$03,$52,$B1,$FF,$02,$75,$F2,$FF,$29,$91,$FF,$FF,
  $40,$84,$99,$FF,$38,$42,$1D,$FF,$20,$30,$03,$FF,$3D,$49,$0B,$FF,$4F,$6A,$00,$FF,
  $56,$71,$00,$FF,$5B,$73,$03,$FF,$4B,$5D,$13,$FF,$37,$47,$20,$FF,$23,$46,$6E,$FF,
  $1F,$74,$AB,$FF,$30,$5B,$3F,$FF,$17,$22,$00,$FF,$47,$5A,$09,$FF,$7B,$9E,$02,$FF,
  $8B,$AC,$00,$FF,$8B,$AE,$00,$FF,$8F,$AA,$00,$FF,$8C,$AA,$0A,$FF,$94,$A9,$04,$FF,
  $94,$AB,$05,$FF,$81,$A5,$00,$FF,$6F,$95,$02,$FF,$51,$68,$00,$FF,$38,$46,$08,$FF,
  $2B,$38,$0A,$FF,$28,$2F,$05,$FF,$2C,$35,$1A,$FF,$18,$20,$0B,$FF,$38,$4C,$05,$FF,
  $61,$81,$06,$FF,$81,$A5,$00,$FF,$90,$AD,$07,$FF,$8C,$AA,$00,$FF,$8A,$AF,$00,$FF,
  $93,$AF,$05,$FF,$89,$A7,$00,$FF,$9F,$D4,$00,$FF,$A7,$CF,$31,$FF,$42,$77,$4B,$FF,
  $08,$40,$6F,$FF,$00,$54,$AA,$FF,$00,$56,$B7,$FF,$00,$50,$AB,$FF,$00,$58,$B9,$FF,
  $06,$68,$C9,$FF,$00,$68,$DE,$FF,$09,$86,$FF,$FF,$00,$6E,$DE,$FF,$02,$55,$BB,$FF,
  $03,$56,$B6,$FF,$00,$58,$BE,$FF,$00,$59,$B2,$FF,$02,$50,$B2,$FF,$00,$52,$A3,$FF,
  $01,$51,$A8,$FF,$00,$55,$A7,$FF,$02,$54,$AA,$FF,$01,$51,$A8,$FF,$02,$54,$A8,$FF,
  $05,$4E,$AA,$FF,$00,$54,$A4,$FF,$00,$57,$B2,$FF,$00,$76,$F1,$FF,$0A,$88,$EE,$FF,
  $3C,$94,$D3,$FF,$46,$7E,$71,$FF,$44,$61,$31,$FF,$40,$5C,$29,$FF,$49,$65,$28,$FF,
  $54,$84,$54,$FF,$4D,$94,$82,$FF,$20,$67,$93,$FF,$06,$60,$C0,$FF,$00,$87,$FF,$FF,
  $36,$85,$C8,$FF,$32,$50,$2C,$FF,$47,$58,$11,$FF,$7B,$9B,$00,$FF,$8B,$AC,$00,$FF,
  $8A,$B0,$05,$FF,$90,$AC,$00,$FF,$8B,$AC,$00,$FF,$87,$AB,$00,$FF,$8B,$B0,$00,$FF,
  $8D,$B3,$06,$FF,$52,$74,$38,$FF,$30,$5D,$48,$FF,$33,$61,$5E,$FF,$35,$5A,$52,$FF,
  $2E,$4F,$32,$FF,$05,$7C,$DA,$FF,$2B,$64,$81,$FF,$2B,$33,$1E,$FF,$38,$40,$05,$FF,
  $68,$88,$00,$FF,$90,$AE,$01,$FF,$8D,$A9,$00,$FF,$8C,$B1,$01,$FF,$88,$AF,$00,$FF,
  $90,$AB,$00,$FF,$B0,$D7,$00,$FF,$C7,$E2,$35,$FF,$2E,$5A,$4B,$FF,$00,$33,$62,$FF,
  $01,$53,$A5,$FF,$02,$51,$AD,$FF,$09,$50,$A8,$FF,$04,$50,$B2,$FF,$01,$4F,$A1,$FF,
  $02,$5A,$B4,$FF,$00,$63,$CA,$FF,$00,$67,$BD,$FF,$03,$5D,$B5,$FF,$00,$4D,$A8,$FF,
  $05,$54,$B1,$FF,$00,$54,$AE,$FF,$02,$4D,$B2,$FF,$00,$53,$AA,$FF,$04,$50,$B0,$FF,
  $00,$51,$AF,$FF,$04,$51,$AB,$FF,$01,$51,$B0,$FF,$00,$51,$AB,$FF,$04,$59,$AC,$FF,
  $00,$52,$A8,$FF,$00,$52,$B7,$FF,$01,$5E,$AE,$FF,$0A,$76,$DB,$FF,$00,$6E,$E9,$FF,
  $04,$7B,$FD,$FF,$00,$74,$E7,$FF,$00,$76,$D7,$FF,$00,$68,$C8,$FF,$00,$6C,$DF,$FF,
  $00,$76,$EE,$FF,$00,$6C,$C9,$FF,$00,$5A,$BA,$FF,$01,$69,$E6,$FF,$25,$73,$B1,$FF,
  $41,$5F,$2B,$FF,$5B,$74,$18,$FF,$87,$A6,$02,$FF,$8F,$A8,$06,$FF,$87,$AC,$04,$FF,
  $8F,$AC,$04,$FF,$8C,$AF,$00,$FF,$89,$AD,$03,$FF,$90,$BD,$08,$FF,$7D,$AA,$0F,$FF,
  $19,$48,$72,$FF,$00,$5D,$AD,$FF,$02,$7C,$FF,$FF,$01,$7E,$FF,$FF,$02,$82,$FF,$FF,
  $00,$7A,$FF,$FF,$17,$89,$F9,$FF,$29,$71,$BB,$FF,$32,$52,$21,$FF,$43,$4D,$08,$FF,
  $7A,$9D,$00,$FF,$88,$AD,$08,$FF,$92,$AA,$00,$FF,$88,$AB,$06,$FF,$93,$AF,$00,$FF,
  $9D,$CD,$00,$FF,$CB,$F8,$2B,$FF,$29,$55,$58,$FF,$01,$28,$51,$FF,$08,$4E,$A6,$FF,
  $00,$54,$AD,$FF,$00,$52,$AC,$FF,$02,$56,$B0,$FF,$00,$52,$AC,$FF,$00,$50,$B4,$FF,
  $02,$55,$B5,$FF,$00,$59,$B1,$FF,$00,$51,$B4,$FF,$09,$51,$B7,$FF,$02,$4E,$A4,$FF,
  $00,$55,$B0,$FF,$00,$53,$A9,$FF,$00,$54,$A3,$FF,$07,$4F,$B1,$FF,$03,$51,$B5,$FF,
  $02,$50,$A3,$FF,$00,$54,$AD,$FF,$03,$51,$B3,$FF,$00,$50,$AD,$FF,$06,$53,$AD,$FF,
  $07,$52,$AF,$FF,$00,$51,$AE,$FF,$01,$4D,$AB,$FF,$00,$54,$AE,$FF,$00,$5A,$B4,$FF,
  $05,$62,$CA,$FF,$00,$64,$C8,$FF,$02,$5E,$B5,$FF,$00,$4F,$B2,$FF,$00,$55,$AF,$FF,
  $05,$4E,$B7,$FF,$00,$4F,$B4,$FF,$00,$6F,$D8,$FF,$37,$62,$8C,$FF,$27,$32,$10,$FF,
  $5C,$7E,$06,$FF,$8B,$AD,$00,$FF,$94,$A7,$00,$FF,$8F,$B2,$00,$FF,$8A,$A7,$00,$FF,
  $8A,$AF,$00,$FF,$99,$B6,$00,$FF,$A2,$D5,$14,$FF,$5A,$76,$4E,$FF,$06,$24,$56,$FF,
  $04,$46,$94,$FF,$00,$53,$B1,$FF,$00,$59,$B2,$FF,$00,$73,$D6,$FF,$00,$59,$B9,$FF,
  $01,$72,$DA,$FF,$0B,$84,$F9,$FF,$30,$72,$92,$FF,$25,$32,$00,$FF,$60,$72,$06,$FF,
  $82,$A4,$05,$FF,$8B,$AD,$00,$FF,$8C,$AA,$00,$FF,$8D,$AE,$05,$FF,$A6,$CD,$00,$FF,
  $AE,$F9,$2A,$FF,$43,$72,$62,$FF,$07,$28,$5D,$FF,$00,$53,$9E,$FF,$05,$51,$B1,$FF,
  $00,$55,$AB,$FF,$03,$52,$AE,$FF,$05,$55,$AE,$FF,$00,$53,$AB,$FF,$00,$51,$AC,$FF,
  $00,$53,$AF,$FF,$00,$52,$AE,$FF,$07,$4F,$B1,$FF,$00,$53,$B5,$FF,$01,$53,$A7,$FF,
  $00,$54,$A5,$FF,$00,$51,$AB,$FF,$00,$54,$B3,$FF,$02,$52,$B1,$FF,$03,$54,$B0,$FF,
  $00,$53,$A8,$FF,$00,$54,$AE,$FF,$02,$51,$AD,$FF,$01,$51,$AA,$FF,$00,$54,$AB,$FF,
  $00,$52,$AC,$FF,$07,$54,$B0,$FF,$00,$4E,$AB,$FF,$00,$55,$B1,$FF,$05,$54,$B1,$FF,
  $04,$4D,$A9,$FF,$00,$54,$B0,$FF,$04,$51,$AD,$FF,$07,$4F,$B3,$FF,$00,$53,$B1,$FF,
  $03,$58,$B5,$FF,$00,$74,$E8,$FF,$2D,$63,$87,$FF,$2C,$32,$18,$FF,$69,$82,$03,$FF,
  $8B,$AC,$00,$FF,$91,$AE,$00,$FF,$87,$B0,$00,$FF,$8D,$AB,$00,$FF,$8D,$AC,$08,$FF,
  $A9,$CE,$0E,$FF,$BF,$E6,$35,$FF,$59,$69,$4C,$FF,$0A,$2B,$56,$FF,$00,$47,$94,$FF,
  $00,$53,$AA,$FF,$03,$54,$B0,$FF,$02,$4F,$A7,$FF,$00,$54,$AB,$FF,$00,$52,$B1,$FF,
  $00,$79,$EC,$FF,$1D,$7D,$C7,$FF,$33,$40,$15,$FF,$40,$52,$00,$FF,$78,$9D,$00,$FF,
  $90,$AB,$00,$FF,$8B,$B1,$00,$FF,$8D,$A2,$05,$FF,$9E,$C3,$04,$FF,$C0,$ED,$16,$FF,
  $60,$8F,$5B,$FF,$04,$37,$79,$FF,$00,$58,$A7,$FF,$05,$51,$B1,$FF,$02,$51,$AE,$FF,
  $00,$50,$A9,$FF,$00,$50,$AB,$FF,$00,$51,$AB,$FF,$01,$53,$A9,$FF,$02,$53,$AE,$FF,
  $00,$52,$AF,$FF,$00,$52,$AD,$FF,$02,$51,$AE,$FF,$02,$51,$B0,$FF,$04,$51,$A9,$FF,
  $08,$54,$AA,$FF,$06,$4E,$B0,$FF,$06,$4C,$AC,$FF,$02,$4F,$A7,$FF,$00,$53,$B1,$FF,
  $00,$50,$AC,$FF,$06,$53,$AF,$FF,$06,$51,$AE,$FF,$01,$52,$AD,$FF,$01,$52,$AE,$FF,
  $03,$4D,$AC,$FF,$04,$4E,$AD,$FF,$01,$52,$AD,$FF,$00,$52,$B3,$FF,$00,$52,$AC,$FF,
  $02,$53,$AF,$FF,$00,$52,$A8,$FF,$00,$54,$AE,$FF,$00,$53,$B0,$FF,$00,$5D,$BA,$FF,
  $0B,$7D,$D5,$FF,$3F,$7C,$5B,$FF,$38,$47,$06,$FF,$76,$93,$05,$FF,$8D,$AD,$00,$FF,
  $8D,$A9,$00,$FF,$88,$AF,$00,$FF,$8E,$AC,$00,$FF,$8D,$AD,$00,$FF,$A3,$C9,$00,$FF,
  $C6,$ED,$38,$FF,$4E,$6D,$4D,$FF,$00,$1D,$39,$FF,$04,$4B,$99,$FF,$00,$51,$AA,$FF,
  $02,$56,$AE,$FF,$00,$50,$AF,$FF,$08,$55,$AF,$FF,$03,$50,$AC,$FF,$00,$73,$DC,$FF,
  $13,$7A,$D6,$FF,$44,$58,$27,$FF,$4E,$69,$02,$FF,$80,$9A,$01,$FF,$8E,$AF,$00,$FF,
  $8B,$B1,$00,$FF,$90,$AA,$08,$FF,$92,$B1,$00,$FF,$B3,$DD,$0D,$FF,$60,$8D,$4A,$FF,
  $01,$41,$71,$FF,$00,$49,$9A,$FF,$01,$51,$AA,$FF,$07,$4F,$B3,$FF,$02,$53,$AF,$FF,
  $00,$51,$B4,$FF,$04,$52,$B7,$FF,$06,$50,$AF,$FF,$05,$4E,$AC,$FF,$05,$50,$AD,$FF,
  $00,$54,$A5,$FF,$04,$4F,$AA,$FF,$00,$52,$AE,$FF,$00,$54,$B0,$FF,$00,$55,$AD,$FF,
  $00,$55,$A5,$FF,$01,$56,$A9,$FF,$02,$53,$AE,$FF,$01,$50,$AF,$FF,$00,$53,$AD,$FF,
  $00,$4E,$AB,$FF,$06,$52,$B4,$FF,$00,$4F,$AD,$FF,$00,$55,$AC,$FF,$00,$55,$A8,$FF,
  $00,$53,$A9,$FF,$00,$55,$AC,$FF,$00,$52,$AE,$FF,$00,$55,$A9,$FF,$00,$52,$B0,$FF,
  $02,$53,$AC,$FF,$03,$55,$A9,$FF,$01,$4F,$B3,$FF,$02,$5C,$B4,$FF,$10,$78,$CD,$FF,
  $33,$65,$5A,$FF,$36,$45,$0A,$FF,$75,$93,$01,$FF,$8C,$AE,$00,$FF,$8E,$A9,$02,$FF,
  $8A,$B0,$05,$FF,$8E,$AB,$01,$FF,$8C,$AD,$00,$FF,$AA,$CF,$03,$FF,$CB,$F4,$2E,$FF,
  $54,$83,$59,$FF,$01,$1B,$3E,$FF,$01,$42,$82,$FF,$04,$54,$AD,$FF,$00,$4D,$AF,$FF,
  $01,$55,$AF,$FF,$03,$4F,$AF,$FF,$01,$55,$AD,$FF,$00,$69,$DC,$FF,$16,$68,$A0,$FF,
  $47,$56,$11,$FF,$75,$97,$02,$FF,$84,$A1,$09,$FF,$88,$AE,$01,$FF,$87,$AC,$00,$FF,
  $8F,$AC,$00,$FF,$8D,$B9,$02,$FF,$9C,$C5,$13,$FF,$68,$B0,$40,$FF,$0F,$3C,$65,$FF,
  $00,$44,$85,$FF,$04,$52,$A7,$FF,$01,$51,$A8,$FF,$00,$55,$A7,$FF,$00,$53,$A5,$FF,
  $00,$53,$A6,$FF,$00,$53,$A6,$FF,$00,$51,$A9,$FF,$00,$53,$AF,$FF,$00,$55,$B1,$FF,
  $00,$53,$AD,$FF,$00,$52,$A8,$FF,$02,$53,$AE,$FF,$00,$4E,$AB,$FF,$02,$54,$AA,$FF,
  $00,$53,$AD,$FF,$00,$53,$B3,$FF,$00,$50,$A7,$FF,$00,$57,$AA,$FF,$00,$50,$AC,$FF,
  $04,$54,$B5,$FF,$01,$52,$AE,$FF,$02,$54,$AA,$FF,$02,$53,$A5,$FF,$03,$50,$A8,$FF,
  $05,$4F,$AE,$FF,$0B,$51,$B0,$FF,$09,$50,$A8,$FF,$00,$55,$AA,$FF,$00,$53,$AD,$FF,
  $03,$54,$A6,$FF,$00,$58,$BD,$FF,$0D,$62,$A5,$FF,$1E,$62,$A3,$FF,$34,$5A,$5D,$FF,
  $50,$63,$14,$FF,$79,$97,$01,$FF,$8D,$AF,$01,$FF,$90,$AE,$01,$FF,$8A,$AE,$06,$FF,
  $8C,$A9,$00,$FF,$8B,$AC,$00,$FF,$9A,$BF,$00,$FF,$C9,$F2,$2C,$FF,$63,$96,$4B,$FF,
  $03,$1A,$3C,$FF,$04,$37,$76,$FF,$00,$51,$AF,$FF,$03,$54,$AD,$FF,$00,$50,$AB,$FF,
  $00,$52,$AE,$FF,$00,$56,$AB,$FF,$08,$65,$E6,$FF,$30,$68,$69,$FF,$53,$5A,$00,$FF,
  $82,$A7,$00,$FF,$82,$AE,$01,$FF,$90,$AE,$00,$FF,$8D,$AE,$09,$FF,$87,$AA,$00,$FF,
  $91,$B8,$03,$FF,$89,$B5,$08,$FF,$89,$D7,$12,$FF,$18,$4C,$71,$FF,$00,$3C,$73,$FF,
  $02,$51,$B6,$FF,$00,$50,$A9,$FF,$05,$51,$B1,$FF,$04,$53,$B0,$FF,$01,$55,$AF,$FF,
  $00,$53,$B0,$FF,$00,$53,$AD,$FF,$00,$53,$A9,$FF,$03,$50,$AC,$FF,$00,$58,$A3,$FF,
  $00,$4F,$A8,$FF,$03,$4D,$A6,$FF,$04,$4F,$AA,$FF,$01,$54,$B4,$FF,$00,$53,$AF,$FF,
  $00,$54,$AB,$FF,$05,$51,$AF,$FF,$02,$53,$AC,$FF,$01,$52,$AB,$FF,$00,$4E,$A3,$FF,
  $03,$53,$A8,$FF,$01,$51,$A6,$FF,$00,$53,$AB,$FF,$00,$57,$B1,$FF,$00,$52,$B1,$FF,
  $00,$51,$AF,$FF,$06,$50,$A9,$FF,$01,$52,$AD,$FF,$04,$4D,$B3,$FF,$06,$50,$BB,$FF,
  $00,$70,$D3,$FF,$16,$81,$B7,$FF,$34,$68,$5C,$FF,$46,$69,$27,$FF,$7C,$9B,$02,$FF,
  $88,$A9,$02,$FF,$8E,$AF,$08,$FF,$8C,$AD,$00,$FF,$89,$AB,$00,$FF,$8E,$AC,$00,$FF,
  $8E,$AE,$03,$FF,$90,$B5,$03,$FF,$C1,$EA,$14,$FF,$8D,$B9,$2E,$FF,$00,$17,$2F,$FF,
  $00,$28,$56,$FF,$00,$51,$AB,$FF,$04,$51,$A9,$FF,$07,$54,$AE,$FF,$00,$50,$AC,$FF,
  $00,$55,$AC,$FF,$1A,$60,$A8,$FF,$47,$68,$31,$FF,$66,$7B,$00,$FF,$7E,$A7,$00,$FF,
  $87,$AD,$00,$FF,$92,$AA,$00,$FF,$8E,$B0,$00,$FF,$8A,$B4,$00,$FF,$97,$AF,$01,$FF,
  $89,$B1,$12,$FF,$A3,$D0,$04,$FF,$30,$84,$6C,$FF,$00,$32,$69,$FF,$00,$51,$A5,$FF,
  $00,$57,$B0,$FF,$02,$4E,$AE,$FF,$00,$4F,$B0,$FF,$00,$51,$B1,$FF,$06,$53,$AF,$FF,
  $05,$50,$A3,$FF,$00,$43,$8A,$FF,$04,$34,$7C,$FF,$04,$26,$4B,$FF,$05,$22,$40,$FF,
  $01,$22,$41,$FF,$00,$34,$66,$FF,$00,$43,$8E,$FF,$04,$50,$A4,$FF,$01,$51,$A8,$FF,
  $00,$52,$AC,$FF,$07,$4F,$B3,$FF,$00,$50,$A9,$FF,$02,$51,$9C,$FF,$00,$43,$88,$FF,
  $00,$34,$7A,$FF,$01,$41,$88,$FF,$00,$42,$8D,$FF,$00,$4A,$98,$FF,$01,$54,$B4,$FF,
  $03,$54,$AF,$FF,$05,$50,$AD,$FF,$02,$52,$A9,$FF,$00,$50,$B3,$FF,$00,$6A,$C3,$FF,
  $0C,$7A,$D1,$FF,$35,$5F,$49,$FF,$3A,$52,$04,$FF,$7F,$9F,$00,$FF,$8A,$AC,$00,$FF,
  $8D,$AD,$04,$FF,$8B,$AD,$00,$FF,$8C,$AC,$01,$FF,$8D,$AD,$04,$FF,$8B,$AA,$04,$FF,
  $8F,$B4,$04,$FF,$B3,$DC,$00,$FF,$D7,$FF,$31,$FF,$12,$30,$2E,$FF,$00,$24,$3E,$FF,
  $00,$52,$A3,$FF,$09,$55,$B3,$FF,$00,$4E,$A7,$FF,$00,$55,$B2,$FF,$0E,$61,$A7,$FF,
  $3E,$66,$44,$FF,$64,$7D,$08,$FF,$80,$A2,$03,$FF,$89,$B1,$00,$FF,$92,$AA,$0A,$FF,
  $90,$AE,$01,$FF,$8A,$A9,$05,$FF,$8C,$AD,$04,$FF,$93,$B5,$00,$FF,$90,$AC,$00,$FF,
  $A4,$CD,$05,$FF,$6A,$AA,$38,$FF,$05,$31,$60,$FF,$01,$40,$8D,$FF,$00,$4A,$97,$FF,
  $04,$48,$91,$FF,$05,$55,$AA,$FF,$00,$51,$A6,$FF,$01,$42,$82,$FF,$03,$40,$6D,$FF,
  $0A,$45,$65,$FF,$2D,$52,$5B,$FF,$27,$4F,$51,$FF,$22,$3D,$38,$FF,$19,$2F,$3C,$FF,
  $08,$2B,$4B,$FF,$00,$2C,$4B,$FF,$00,$2A,$4F,$FF,$01,$3B,$7B,$FF,$04,$52,$A5,$FF,
  $00,$4F,$B2,$FF,$00,$53,$B4,$FF,$03,$51,$A4,$FF,$00,$32,$69,$FF,$00,$1B,$30,$FF,
  $0C,$24,$28,$FF,$07,$22,$35,$FF,$01,$1D,$45,$FF,$00,$28,$5A,$FF,$00,$3B,$75,$FF,
  $00,$43,$8C,$FF,$01,$52,$A4,$FF,$03,$53,$B2,$FF,$03,$57,$AD,$FF,$0D,$56,$9A,$FF,
  $27,$36,$23,$FF,$3F,$4A,$06,$FF,$67,$7F,$03,$FF,$7F,$9E,$00,$FF,$84,$A3,$00,$FF,
  $85,$A6,$00,$FF,$8D,$AE,$00,$FF,$8E,$AE,$01,$FF,$8C,$AE,$00,$FF,$86,$AB,$00,$FF,
  $9C,$BF,$03,$FF,$CE,$FE,$08,$FF,$5E,$84,$5B,$FF,$05,$28,$48,$FF,$01,$49,$93,$FF,
  $00,$50,$A2,$FF,$00,$54,$B4,$FF,$04,$6C,$C1,$FF,$2F,$71,$93,$FF,$6A,$79,$10,$FF,
  $7F,$A4,$00,$FF,$8F,$AB,$01,$FF,$91,$AF,$01,$FF,$8B,$A8,$00,$FF,$87,$AE,$00,$FF,
  $92,$AE,$01,$FF,$80,$A3,$00,$FF,$80,$A5,$00,$FF,$77,$9D,$00,$FF,$8F,$B4,$02,$FF,
  $87,$A4,$16,$FF,$01,$21,$30,$FF,$00,$2D,$52,$FF,$00,$21,$3D,$FF,$02,$1C,$35,$FF,
  $00,$30,$6E,$FF,$02,$3D,$83,$FF,$26,$4E,$5A,$FF,$47,$75,$47,$FF,$65,$9F,$39,$FF,
  $9E,$CC,$23,$FF,$C0,$ED,$20,$FF,$B7,$E0,$00,$FF,$B4,$E5,$0D,$FF,$85,$C1,$2E,$FF,
  $50,$7E,$4F,$FF,$08,$23,$34,$FF,$00,$27,$5C,$FF,$00,$4C,$9A,$FF,$00,$56,$B2,$FF,
  $02,$61,$CB,$FF,$00,$52,$B3,$FF,$1D,$59,$7E,$FF,$4A,$71,$45,$FF,$5D,$83,$2E,$FF,
  $62,$8B,$4F,$FF,$30,$5C,$4B,$FF,$0A,$29,$48,$FF,$14,$3B,$5C,$FF,$0A,$32,$4B,$FF,
  $00,$29,$5B,$FF,$00,$31,$6C,$FF,$03,$3E,$84,$FF,$1B,$46,$4D,$FF,$1F,$26,$16,$FF,
  $2C,$2E,$00,$FF,$27,$33,$05,$FF,$43,$51,$04,$FF,$43,$54,$06,$FF,$47,$59,$03,$FF,
  $59,$6F,$02,$FF,$63,$79,$0A,$FF,$6A,$85,$00,$FF,$7D,$9C,$00,$FF,$89,$A4,$07,$FF,
  $BD,$F1,$00,$FF,$60,$88,$54,$FF,$00,$25,$46,$FF,$05,$40,$84,$FF,$00,$53,$A8,$FF,
  $00,$5A,$B4,$FF,$00,$77,$D5,$FF,$38,$74,$36,$FF,$45,$4C,$00,$FF,$6F,$85,$07,$FF,
  $77,$A1,$00,$FF,$80,$A5,$00,$FF,$86,$A7,$00,$FF,$85,$A4,$00,$FF,$79,$9A,$01,$FF,
  $6A,$7D,$07,$FF,$4F,$69,$08,$FF,$53,$69,$00,$FF,$5B,$74,$0C,$FF,$60,$88,$28,$FF,
  $0D,$25,$27,$FF,$0B,$34,$50,$FF,$22,$4B,$47,$FF,$0F,$2B,$2F,$FF,$3B,$5A,$21,$FF,
  $6D,$8F,$42,$FF,$97,$BC,$1E,$FF,$B2,$D7,$0D,$FF,$A9,$D9,$00,$FF,$A1,$CD,$00,$FF,
  $A7,$CC,$00,$FF,$92,$C1,$00,$FF,$A4,$CE,$00,$FF,$C9,$F1,$05,$FF,$C1,$F0,$02,$FF,
  $4A,$75,$25,$FF,$03,$24,$4D,$FF,$00,$4B,$9D,$FF,$03,$53,$AA,$FF,$00,$69,$E0,$FF,
  $17,$65,$AD,$FF,$73,$9B,$16,$FF,$A6,$CC,$05,$FF,$C3,$EE,$0B,$FF,$CB,$FF,$00,$FF,
  $CD,$FE,$02,$FF,$A9,$D6,$17,$FF,$87,$BF,$20,$FF,$66,$97,$56,$FF,$31,$83,$6D,$FF,
  $31,$6D,$6E,$FF,$4B,$8D,$7F,$FF,$74,$99,$25,$FF,$53,$89,$2B,$FF,$1C,$3D,$20,$FF,
  $16,$31,$3A,$FF,$16,$5E,$77,$FF,$36,$71,$6F,$FF,$42,$62,$31,$FF,$3E,$47,$1C,$FF,
  $1E,$29,$09,$FF,$29,$30,$00,$FF,$4B,$58,$09,$FF,$60,$76,$00,$FF,$75,$8E,$0E,$FF,
  $35,$4D,$19,$FF,$00,$22,$48,$FF,$04,$43,$93,$FF,$00,$4F,$AA,$FF,$01,$58,$B7,$FF,
  $10,$8D,$F9,$FF,$34,$77,$A4,$FF,$33,$46,$19,$FF,$3B,$48,$03,$FF,$4C,$69,$00,$FF,
  $59,$74,$00,$FF,$5B,$6E,$00,$FF,$64,$7E,$00,$FF,$54,$6B,$10,$FF,$41,$5A,$18,$FF,
  $49,$69,$3A,$FF,$54,$8F,$63,$FF,$48,$96,$86,$FF,$2E,$92,$CD,$FF,$33,$76,$90,$FF,
  $53,$83,$37,$FF,$99,$CA,$30,$FF,$A0,$D0,$24,$FF,$9F,$CF,$0B,$FF,$AA,$DC,$00,$FF,
  $99,$C1,$00,$FF,$8F,$B0,$01,$FF,$8B,$B1,$00,$FF,$8B,$AE,$0C,$FF,$92,$A8,$00,$FF,
  $8E,$AA,$00,$FF,$8D,$B2,$00,$FF,$91,$B4,$02,$FF,$7D,$AB,$0A,$FF,$1E,$3C,$1A,$FF,
  $03,$28,$5C,$FF,$00,$53,$A5,$FF,$01,$50,$AD,$FF,$00,$74,$EF,$FF,$10,$64,$AA,$FF,
  $3D,$4A,$2E,$FF,$5C,$6D,$03,$FF,$81,$A9,$00,$FF,$94,$B7,$01,$FF,$99,$BF,$00,$FF,
  $A4,$C9,$00,$FF,$B7,$E0,$00,$FF,$C8,$EF,$02,$FF,$BB,$F2,$01,$FF,$BF,$F0,$00,$FF,
  $C2,$FB,$00,$FF,$CF,$F6,$00,$FF,$A4,$DB,$35,$FF,$32,$7A,$86,$FF,$02,$46,$99,$FF,
  $00,$70,$EB,$FF,$00,$7F,$FF,$FF,$15,$88,$FB,$FF,$42,$97,$CE,$FF,$4C,$8B,$78,$FF,
  $35,$60,$2B,$FF,$43,$59,$10,$FF,$4C,$61,$26,$FF,$27,$47,$30,$FF,$03,$37,$4F,$FF,
  $00,$45,$81,$FF,$00,$4F,$9E,$FF,$00,$58,$B1,$FF,$01,$64,$C2,$FF,$00,$72,$E5,$FF,
  $09,$85,$FF,$FF,$1E,$77,$D1,$FF,$48,$73,$48,$FF,$4F,$5C,$16,$FF,$4C,$59,$24,$FF,
  $42,$59,$25,$FF,$54,$73,$3A,$FF,$4C,$85,$5A,$FF,$1F,$73,$98,$FF,$0E,$6D,$CB,$FF,
  $00,$75,$EF,$FF,$03,$77,$FE,$FF,$00,$7C,$F8,$FF,$4D,$AD,$A2,$FF,$70,$8D,$17,$FF,
  $85,$A4,$09,$FF,$8C,$B4,$00,$FF,$8B,$B2,$01,$FF,$86,$A5,$01,$FF,$90,$AF,$00,$FF,
  $8F,$AF,$06,$FF,$8F,$AC,$06,$FF,$8A,$AE,$00,$FF,$8C,$B0,$06,$FF,$93,$AB,$00,$FF,
  $92,$B0,$02,$FF,$8B,$AB,$0C,$FF,$4A,$85,$55,$FF,$12,$4A,$6F,$FF,$0A,$47,$98,$FF,
  $00,$54,$A9,$FF,$00,$53,$A9,$FF,$00,$71,$E3,$FF,$00,$6B,$BE,$FF,$25,$25,$0D,$FF,
  $27,$35,$00,$FF,$71,$92,$00,$FF,$8B,$B0,$0A,$FF,$90,$AE,$00,$FF,$8B,$AB,$08,$FF,
  $8F,$AB,$00,$FF,$96,$B4,$00,$FF,$98,$B6,$08,$FF,$9B,$BE,$02,$FF,$98,$BB,$00,$FF,
  $AF,$D1,$08,$FF,$AA,$EA,$1A,$FF,$3A,$5F,$4D,$FF,$00,$30,$6C,$FF,$00,$58,$AA,$FF,
  $00,$5C,$B0,$FF,$00,$69,$DF,$FF,$06,$83,$FF,$FF,$12,$90,$F7,$FF,$0A,$76,$D8,$FF,
  $37,$98,$CF,$FF,$3D,$99,$D8,$FF,$1F,$72,$B4,$FF,$02,$4D,$90,$FF,$00,$44,$8F,$FF,
  $00,$48,$9F,$FF,$00,$53,$AB,$FF,$00,$5B,$AE,$FF,$02,$56,$B6,$FF,$07,$6B,$CB,$FF,
  $03,$76,$F3,$FF,$09,$86,$FC,$FF,$1C,$98,$BE,$FF,$20,$7A,$B9,$FF,$20,$79,$B1,$FF,
  $0E,$78,$CE,$FF,$0A,$6B,$C9,$FF,$01,$65,$BD,$FF,$0F,$55,$B4,$FF,$03,$52,$B1,$FF,
  $00,$54,$B3,$FF,$04,$76,$F0,$FF,$25,$89,$E1,$FF,$4D,$71,$33,$FF,$41,$4F,$00,$FF,
  $6A,$86,$00,$FF,$82,$A5,$03,$FF,$91,$AC,$00,$FF,$8D,$A9,$00,$FF,$89,$AB,$00,$FF,
  $91,$AE,$00,$FF,$8A,$AC,$00,$FF,$84,$B1,$00,$FF,$93,$B3,$00,$FF,$9E,$BE,$00,$FF,
  $70,$8D,$16,$FF,$13,$55,$6D,$FF,$00,$56,$A2,$FF,$00,$4F,$B0,$FF,$03,$50,$AA,$FF,
  $00,$52,$B6,$FF,$02,$69,$E0,$FF,$00,$7A,$F8,$FF,$32,$48,$31,$FF,$07,$15,$00,$FF,
  $61,$79,$00,$FF,$87,$AB,$03,$FF,$8D,$A8,$03,$FF,$8E,$AF,$00,$FF,$8C,$A6,$04,$FF,
  $8A,$AD,$00,$FF,$87,$AE,$05,$FF,$87,$AD,$08,$FF,$8C,$B3,$00,$FF,$9F,$C3,$00,$FF,
  $84,$BE,$2C,$FF,$1C,$4B,$69,$FF,$00,$3C,$79,$FF,$00,$52,$9E,$FF,$04,$4E,$A3,$FF,
  $00,$52,$B2,$FF,$00,$5C,$B7,$FF,$00,$67,$C2,$FF,$01,$73,$E5,$FF,$00,$73,$DD,$FF,
  $00,$73,$EB,$FF,$07,$76,$EC,$FF,$04,$62,$C2,$FF,$00,$51,$A0,$FF,$05,$53,$A5,$FF,
  $03,$52,$AF,$FF,$05,$50,$B5,$FF,$00,$55,$AC,$FF,$00,$51,$AE,$FF,$00,$59,$AE,$FF,
  $06,$71,$D3,$FF,$00,$67,$DE,$FF,$00,$68,$E1,$FF,$08,$78,$DA,$FF,$00,$6C,$CB,$FF,
  $03,$55,$B9,$FF,$00,$55,$B3,$FF,$00,$51,$A7,$FF,$02,$4C,$AF,$FF,$04,$52,$B7,$FF,
  $00,$58,$B0,$FF,$00,$78,$EC,$FF,$20,$76,$B3,$FF,$26,$38,$28,$FF,$40,$55,$00,$FF,
  $73,$94,$00,$FF,$91,$AA,$08,$FF,$90,$AA,$08,$FF,$8B,$AF,$07,$FF,$8F,$AD,$00,$FF,
  $89,$AB,$00,$FF,$8B,$A8,$02,$FF,$9D,$C3,$00,$FF,$A0,$CA,$10,$FF,$33,$52,$42,$FF,
  $00,$34,$63,$FF,$00,$50,$A0,$FF,$02,$4F,$A7,$FF,$06,$55,$B4,$FF,$01,$50,$AD,$FF,
  $00,$63,$BE,$FF,$00,$81,$F9,$FF,$38,$78,$9C,$FF,$10,$15,$00,$FF,$51,$6D,$00,$FF,
  $8C,$A8,$00,$FF,$8D,$AF,$00,$FF,$8A,$AD,$00,$FF,$8D,$AC,$06,$FF,$91,$AE,$00,$FF,
  $89,$AD,$03,$FF,$8E,$A9,$04,$FF,$91,$B9,$00,$FF,$A2,$CB,$05,$FF,$4E,$82,$5C,$FF,
  $13,$37,$5B,$FF,$00,$46,$8A,$FF,$00,$51,$AF,$FF,$04,$53,$AF,$FF,$03,$51,$A6,$FF,
  $03,$4F,$A7,$FF,$04,$4D,$A9,$FF,$03,$53,$AA,$FF,$01,$54,$B2,$FF,$00,$53,$B3,$FF,
  $00,$56,$B4,$FF,$00,$5B,$B7,$FF,$00,$56,$B8,$FF,$02,$4F,$B7,$FF,$02,$51,$B0,$FF,
  $00,$53,$A3,$FF,$00,$57,$A5,$FF,$02,$51,$AD,$FF,$00,$54,$B6,$FF,$00,$4F,$A9,$FF,
  $08,$55,$A7,$FF,$00,$4C,$AE,$FF,$00,$55,$B3,$FF,$00,$55,$A4,$FF,$04,$56,$AA,$FF,
  $00,$50,$AA,$FF,$00,$56,$AC,$FF,$05,$58,$B6,$FF,$00,$53,$AC,$FF,$07,$52,$AD,$FF,
  $00,$6F,$D5,$FF,$21,$89,$E2,$FF,$36,$68,$5F,$FF,$2F,$40,$14,$FF,$67,$85,$00,$FF,
  $84,$9F,$02,$FF,$90,$AD,$00,$FF,$88,$B1,$00,$FF,$8C,$AA,$00,$FF,$8E,$AE,$0F,$FF,
  $8D,$AD,$00,$FF,$A1,$CB,$07,$FF,$8D,$B9,$30,$FF,$10,$3A,$60,$FF,$05,$38,$79,$FF,
  $05,$52,$AA,$FF,$08,$52,$AB,$FF,$00,$53,$B1,$FF,$05,$53,$B5,$FF,$00,$58,$AC,$FF,
  $00,$7B,$FA,$FF,$28,$7C,$D2,$FF,$28,$32,$1A,$FF,$44,$5B,$03,$FF,$88,$A2,$01,$FF,
  $87,$A9,$00,$FF,$8A,$AF,$00,$FF,$8B,$AD,$01,$FF,$8D,$A9,$00,$FF,$87,$AD,$00,$FF,
  $8F,$AC,$00,$FF,$8E,$B7,$03,$FF,$A2,$CC,$20,$FF,$32,$6C,$60,$FF,$00,$2A,$5A,$FF,
  $00,$4B,$92,$FF,$02,$53,$AF,$FF,$02,$50,$B2,$FF,$03,$52,$B9,$FF,$00,$52,$B3,$FF,
  $00,$56,$AD,$FF,$00,$50,$A9,$FF,$00,$4E,$AA,$FF,$05,$52,$AC,$FF,$05,$52,$AC,$FF,
  $05,$52,$AC,$FF,$05,$52,$AC,$FF,$04,$51,$A9,$FF,$01,$55,$AB,$FF,$00,$56,$A7,$FF,
  $04,$50,$B0,$FF,$04,$4C,$B0,$FF,$08,$53,$AE,$FF,$02,$51,$AE,$FF,$00,$4F,$AB,$FF,
  $03,$55,$AB,$FF,$00,$4E,$AF,$FF,$03,$4F,$AF,$FF,$01,$50,$AC,$FF,$00,$52,$AA,$FF,
  $00,$53,$AD,$FF,$00,$4F,$A9,$FF,$00,$59,$A4,$FF,$01,$4C,$A9,$FF,$03,$6F,$CA,$FF,
  $13,$7B,$DE,$FF,$31,$7E,$9C,$FF,$3C,$55,$11,$FF,$51,$6A,$05,$FF,$88,$A8,$05,$FF,
  $8F,$AD,$00,$FF,$88,$AF,$00,$FF,$8B,$AB,$02,$FF,$8F,$AD,$00,$FF,$8B,$B5,$00,$FF,
  $AB,$D8,$03,$FF,$79,$A6,$41,$FF,$09,$34,$47,$FF,$00,$37,$7D,$FF,$00,$4F,$B2,$FF,
  $01,$54,$B0,$FF,$00,$51,$AC,$FF,$01,$51,$B4,$FF,$02,$52,$A9,$FF,$00,$75,$FB,$FF,
  $2A,$7B,$CC,$FF,$35,$4C,$2F,$FF,$38,$49,$03,$FF,$7E,$9E,$00,$FF,$8C,$AC,$00,$FF,
  $8E,$AB,$01,$FF,$91,$A9,$09,$FF,$8F,$AC,$02,$FF,$8A,$AC,$00,$FF,$8D,$AF,$03,$FF,
  $A2,$CA,$07,$FF,$96,$BB,$37,$FF,$23,$54,$74,$FF,$00,$39,$82,$FF,$01,$4C,$9F,$FF,
  $0A,$4C,$A3,$FF,$00,$53,$A4,$FF,$00,$54,$AE,$FF,$00,$51,$AA,$FF,$02,$53,$A5,$FF,
  $00,$53,$A9,$FF,$06,$55,$B1,$FF,$00,$4E,$AE,$FF,$00,$52,$B2,$FF,$03,$52,$AE,$FF,
  $00,$51,$A5,$FF,$00,$55,$A6,$FF,$00,$53,$AC,$FF,$04,$53,$B2,$FF,$02,$53,$AE,$FF,
  $00,$52,$A7,$FF,$03,$52,$B7,$FF,$00,$52,$A8,$FF,$00,$55,$AE,$FF,$01,$54,$A4,$FF,
  $03,$50,$AC,$FF,$00,$55,$B0,$FF,$01,$50,$B5,$FF,$00,$54,$A5,$FF,$07,$52,$AF,$FF,
  $02,$4D,$B3,$FF,$04,$53,$B0,$FF,$04,$4E,$AD,$FF,$00,$5A,$B8,$FF,$06,$73,$D2,$FF,
  $1E,$7C,$BA,$FF,$35,$4F,$22,$FF,$47,$5F,$00,$FF,$78,$9A,$00,$FF,$8F,$A8,$04,$FF,
  $8A,$B1,$00,$FF,$8B,$AC,$00,$FF,$90,$AB,$04,$FF,$95,$B2,$02,$FF,$C1,$EE,$00,$FF,
  $77,$A7,$44,$FF,$05,$21,$2D,$FF,$00,$35,$62,$FF,$02,$4F,$A9,$FF,$00,$57,$A7,$FF,
  $05,$4E,$AA,$FF,$00,$4F,$AB,$FF,$00,$57,$A6,$FF,$00,$68,$E3,$FF,$34,$81,$D3,$FF,
  $3A,$4C,$34,$FF,$32,$4C,$0B,$FF,$75,$91,$00,$FF,$8F,$AD,$00,$FF,$89,$B0,$00,$FF,
  $8C,$A9,$00,$FF,$87,$AF,$00,$FF,$8F,$AB,$00,$FF,$87,$B2,$00,$FF,$AF,$D9,$09,$FF,
  $6B,$95,$43,$FF,$06,$40,$65,$FF,$01,$49,$9B,$FF,$00,$53,$A7,$FF,$05,$4F,$AE,$FF,
  $00,$59,$AF,$FF,$03,$4F,$A7,$FF,$05,$51,$AF,$FF,$01,$51,$B2,$FF,$02,$51,$B0,$FF,
  $00,$53,$A8,$FF,$05,$51,$B1,$FF,$04,$4E,$AF,$FF,$01,$52,$AB,$FF,$00,$56,$AA,$FF,
  $00,$52,$B0,$FF,$04,$50,$B2,$FF,$05,$50,$AD,$FF,$00,$51,$A9,$FF,$07,$53,$AB,$FF,
  $00,$52,$A6,$FF,$00,$53,$B4,$FF,$01,$4F,$B1,$FF,$05,$53,$A8,$FF,$00,$54,$AF,$FF,
  $00,$52,$B2,$FF,$00,$50,$B0,$FF,$03,$52,$AE,$FF,$04,$4F,$AC,$FF,$00,$54,$AE,$FF,
  $00,$53,$AD,$FF,$02,$51,$B6,$FF,$00,$5A,$B7,$FF,$0C,$67,$AD,$FF,$13,$75,$D6,$FF,
  $32,$58,$4B,$FF,$3C,$47,$03,$FF,$78,$9D,$00,$FF,$8B,$AC,$05,$FF,$8E,$AA,$00,$FF,
  $8A,$AC,$00,$FF,$8E,$AF,$06,$FF,$93,$B4,$01,$FF,$B7,$DC,$00,$FF,$AC,$DB,$3D,$FF,
  $20,$46,$39,$FF,$00,$25,$45,$FF,$04,$3E,$90,$FF,$01,$53,$A5,$FF,$03,$51,$B6,$FF,
  $03,$50,$AC,$FF,$02,$57,$A8,$FF,$01,$68,$DF,$FF,$29,$87,$CF,$FF,$44,$63,$44,$FF,
  $4F,$59,$12,$FF,$79,$9B,$00,$FF,$91,$AE,$04,$FF,$8B,$AC,$00,$FF,$8B,$AD,$00,$FF,
  $88,$AF,$00,$FF,$90,$AD,$05,$FF,$99,$AF,$02,$FF,$AF,$E2,$0B,$FF,$2E,$5E,$46,$FF,
  $00,$29,$5A,$FF,$00,$53,$A6,$FF,$02,$51,$BA,$FF,$00,$54,$AF,$FF,$00,$51,$AC,$FF,
  $05,$51,$AF,$FF,$00,$53,$AB,$FF,$00,$54,$AC,$FF,$04,$50,$AE,$FF,$04,$4E,$AD,$FF,
  $04,$51,$AB,$FF,$00,$4F,$AC,$FF,$00,$56,$AF,$FF,$02,$4F,$BB,$FF,$00,$59,$A9,$FF,
  $06,$52,$AA,$FF,$04,$50,$B2,$FF,$00,$51,$AC,$FF,$00,$52,$AC,$FF,$03,$55,$A9,$FF,
  $00,$53,$A3,$FF,$01,$51,$AA,$FF,$04,$50,$B0,$FF,$00,$53,$AA,$FF,$00,$51,$A3,$FF,
  $00,$53,$AF,$FF,$00,$52,$A9,$FF,$04,$4D,$A9,$FF,$00,$50,$AC,$FF,$00,$54,$AE,$FF,
  $02,$5C,$BC,$FF,$0A,$60,$B7,$FF,$0E,$59,$92,$FF,$00,$77,$D3,$FF,$2B,$64,$5E,$FF,
  $29,$37,$02,$FF,$73,$93,$02,$FF,$8D,$AD,$02,$FF,$91,$AD,$01,$FF,$88,$AD,$00,$FF,
  $8B,$AE,$00,$FF,$91,$AD,$01,$FF,$A3,$CC,$02,$FF,$D1,$FD,$20,$FF,$70,$99,$57,$FF,
  $0B,$21,$1E,$FF,$00,$27,$48,$FF,$00,$31,$59,$FF,$04,$3F,$8F,$FF,$00,$48,$9F,$FF,
  $00,$52,$A1,$FF,$04,$5B,$BC,$FF,$08,$6A,$C5,$FF,$43,$59,$44,$FF,$52,$76,$0A,$FF,
  $86,$A3,$00,$FF,$85,$AB,$00,$FF,$8F,$AC,$02,$FF,$8F,$AD,$00,$FF,$8A,$A9,$03,$FF,
  $89,$B1,$00,$FF,$95,$C0,$11,$FF,$91,$BA,$36,$FF,$20,$41,$50,$FF,$08,$33,$81,$FF,
  $04,$56,$AC,$FF,$05,$4D,$A1,$FF,$04,$4F,$AA,$FF,$05,$55,$AA,$FF,$02,$4A,$9F,$FF,
  $00,$46,$8B,$FF,$03,$4E,$92,$FF,$00,$42,$89,$FF,$00,$46,$8B,$FF,$00,$4E,$9D,$FF,
  $01,$52,$AB,$FF,$0B,$52,$AE,$FF,$02,$52,$B1,$FF,$00,$4A,$A2,$FF,$01,$51,$A6,$FF,
  $00,$54,$AA,$FF,$05,$55,$B4,$FF,$00,$51,$AE,$FF,$00,$50,$AE,$FF,$00,$50,$A9,$FF,
  $07,$53,$AB,$FF,$01,$51,$AA,$FF,$00,$52,$AA,$FF,$0A,$45,$A1,$FF,$02,$37,$87,$FF,
  $00,$37,$5C,$FF,$07,$39,$78,$FF,$04,$3C,$86,$FF,$03,$4E,$88,$FF,$02,$56,$B0,$FF,
  $0D,$64,$C5,$FF,$0D,$4E,$8E,$FF,$01,$77,$D7,$FF,$31,$6B,$5D,$FF,$35,$48,$06,$FF,
  $7D,$99,$08,$FF,$8D,$AE,$00,$FF,$8E,$AC,$00,$FF,$8D,$AF,$03,$FF,$8A,$AB,$00,$FF,
  $87,$AD,$00,$FF,$93,$B4,$03,$FF,$B7,$E9,$0C,$FF,$DA,$FF,$2C,$FF,$6B,$91,$6C,$FF,
  $1A,$2C,$3A,$FF,$00,$17,$26,$FF,$00,$10,$22,$FF,$00,$13,$2E,$FF,$00,$1C,$34,$FF,
  $01,$26,$5A,$FF,$04,$34,$64,$FF,$45,$52,$26,$FF,$71,$92,$00,$FF,$94,$B2,$02,$FF,
  $88,$AF,$00,$FF,$8C,$A9,$03,$FF,$86,$AD,$00,$FF,$90,$B4,$00,$FF,$91,$B4,$00,$FF,
  $AB,$D4,$14,$FF,$7B,$94,$5D,$FF,$08,$27,$44,$FF,$00,$39,$78,$FF,$03,$52,$B1,$FF,
  $00,$50,$AC,$FF,$04,$55,$AE,$FF,$00,$41,$7C,$FF,$00,$25,$40,$FF,$05,$1F,$52,$FF,
  $04,$20,$45,$FF,$00,$19,$30,$FF,$00,$0C,$13,$FF,$00,$1D,$44,$FF,$00,$50,$84,$FF,
  $00,$4C,$AE,$FF,$00,$53,$A3,$FF,$00,$59,$AD,$FF,$00,$57,$B7,$FF,$03,$4F,$AD,$FF,
  $00,$4F,$A2,$FF,$08,$54,$B6,$FF,$00,$55,$B2,$FF,$00,$52,$A8,$FF,$05,$52,$AA,$FF,
  $00,$50,$AB,$FF,$00,$47,$8C,$FF,$01,$2C,$3F,$FF,$00,$10,$1A,$FF,$04,$09,$0D,$FF,
  $00,$03,$14,$FF,$00,$0E,$1D,$FF,$00,$1A,$36,$FF,$00,$42,$8D,$FF,$05,$6C,$C8,$FF,
  $08,$53,$97,$FF,$0B,$4D,$9B,$FF,$3D,$64,$2F,$FF,$62,$75,$02,$FF,$85,$A4,$01,$FF,
  $8B,$AD,$00,$FF,$89,$A9,$00,$FF,$8D,$AD,$02,$FF,$90,$AC,$00,$FF,$94,$AB,$03,$FF,
  $88,$B1,$00,$FF,$97,$B3,$00,$FF,$BA,$E8,$00,$FF,$DA,$FE,$2A,$FF,$B1,$D9,$52,$FF,
  $78,$91,$58,$FF,$2F,$5B,$44,$FF,$24,$3F,$38,$FF,$18,$36,$38,$FF,$21,$49,$4B,$FF,
  $4E,$6F,$40,$FF,$74,$A0,$05,$FF,$8A,$A5,$00,$FF,$8C,$A8,$00,$FF,$8F,$A9,$0A,$FF,
  $8B,$AC,$03,$FF,$8C,$AD,$06,$FF,$8D,$A7,$00,$FF,$9C,$BA,$00,$FF,$B2,$E9,$1E,$FF,
  $6A,$9C,$5D,$FF,$00,$29,$28,$FF,$05,$25,$5E,$FF,$00,$42,$88,$FF,$00,$4C,$97,$FF,
  $00,$36,$81,$FF,$21,$45,$55,$FF,$32,$4A,$3A,$FF,$3C,$64,$49,$FF,$40,$6E,$54,$FF,
  $4D,$73,$4C,$FF,$06,$07,$0B,$FF,$00,$01,$02,$FF,$05,$21,$49,$FF,$00,$47,$93,$FF,
  $04,$54,$A9,$FF,$05,$4F,$A6,$FF,$00,$4F,$AE,$FF,$00,$51,$AF,$FF,$00,$52,$AA,$FF,
  $07,$51,$AA,$FF,$00,$51,$A8,$FF,$00,$51,$A7,$FF,$00,$58,$B9,$FF,$01,$5C,$AD,$FF,
  $14,$40,$67,$FF,$1C,$2F,$3D,$FF,$39,$5A,$2D,$FF,$5D,$85,$49,$FF,$49,$76,$4B,$FF,
  $2B,$4A,$4C,$FF,$00,$07,$0E,$FF,$00,$23,$3D,$FF,$05,$57,$A0,$FF,$0A,$59,$92,$FF,
  $23,$41,$39,$FF,$5C,$77,$04,$FF,$86,$9F,$03,$FF,$88,$AB,$00,$FF,$8E,$AF,$00,$FF,
  $8D,$AA,$00,$FF,$8A,$AA,$01,$FF,$8E,$AE,$01,$FF,$8A,$AB,$06,$FF,$89,$AF,$00,$FF,
  $8C,$AE,$02,$FF,$96,$B4,$06,$FF,$A4,$DC,$00,$FF,$D0,$FF,$07,$FF,$D2,$FC,$16,$FF,
  $DC,$FE,$35,$FF,$DA,$FB,$38,$FF,$D0,$FF,$21,$FF,$B9,$E1,$1E,$FF,$9B,$C6,$14,$FF,
  $8E,$B5,$00,$FF,$88,$AD,$07,$FF,$8E,$AD,$00,$FF,$8B,$AF,$03,$FF,$89,$AD,$03,$FF,
  $8F,$AB,$01,$FF,$91,$AE,$00,$FF,$9A,$BC,$03,$FF,$BA,$F0,$16,$FF,$A8,$E8,$4B,$FF,
  $1F,$46,$57,$FF,$04,$1C,$38,$FF,$00,$1D,$45,$FF,$06,$3B,$7F,$FF,$2D,$70,$76,$FF,
  $69,$9F,$41,$FF,$B6,$E3,$28,$FF,$C2,$EF,$22,$FF,$CC,$FB,$23,$FF,$D5,$FF,$19,$FF,
  $CB,$E6,$39,$FF,$2B,$45,$2C,$FF,$00,$01,$00,$FF,$07,$1D,$44,$FF,$00,$47,$92,$FF,
  $02,$51,$B6,$FF,$01,$55,$AB,$FF,$03,$52,$B1,$FF,$02,$52,$AB,$FF,$00,$4F,$AC,$FF,
  $00,$53,$B1,$FF,$01,$56,$A7,$FF,$00,$61,$CC,$FF,$1B,$64,$A9,$FF,$41,$58,$20,$FF,
  $5F,$7E,$07,$FF,$8D,$B3,$00,$FF,$BD,$EF,$02,$FF,$D0,$FD,$0C,$FF,$D0,$FF,$1C,$FF,
  $88,$BD,$2D,$FF,$18,$49,$74,$FF,$02,$44,$98,$FF,$4A,$9C,$6B,$FF,$6C,$8C,$11,$FF,
  $80,$A3,$00,$FF,$8B,$AF,$05,$FF,$8D,$B0,$00,$FF,$8E,$AD,$00,$FF,$90,$AB,$04,$FF,
  $8C,$AC,$01,$FF,$87,$AE,$00,$FF,$90,$AC,$00,$FF,$8B,$AD,$00,$FF,$8A,$AC,$00,$FF,
  $91,$AA,$06,$FF,$8D,$A7,$00,$FF,$97,$B4,$00,$FF,$9B,$BD,$04,$FF,$A1,$CF,$0D,$FF,
  $A9,$CE,$02,$FF,$A4,$D0,$01,$FF,$96,$B3,$03,$FF,$89,$B1,$00,$FF,$8D,$AB,$00,$FF,
  $89,$AF,$02,$FF,$90,$AB,$06,$FF,$89,$AB,$00,$FF,$8C,$B1,$01,$FF,$8F,$AB,$00,$FF,
  $8A,$AC,$00,$FF,$90,$BA,$01,$FF,$AD,$D0,$02,$FF,$D5,$FF,$11,$FF,$B2,$E6,$46,$FF,
  $6C,$99,$5E,$FF,$57,$73,$4B,$FF,$70,$96,$41,$FF,$A8,$CE,$2B,$FF,$A9,$CA,$00,$FF,
  $9C,$B9,$00,$FF,$9E,$BB,$00,$FF,$99,$B8,$00,$FF,$B6,$DD,$06,$FF,$D4,$FD,$01,$FF,
  $C2,$E6,$3C,$FF,$07,$22,$35,$FF,$06,$0F,$36,$FF,$00,$44,$8A,$FF,$00,$52,$A8,$FF,
  $05,$50,$AB,$FF,$00,$53,$B0,$FF,$02,$53,$AE,$FF,$01,$51,$B2,$FF,$00,$4E,$B0,$FF,
  $00,$5A,$BB,$FF,$19,$79,$CF,$FF,$49,$76,$7B,$FF,$5A,$6C,$00,$FF,$70,$93,$00,$FF,
  $8E,$B3,$03,$FF,$8A,$B4,$00,$FF,$A1,$B9,$09,$FF,$B9,$DE,$00,$FF,$C4,$F4,$00,$FF,
  $B3,$DE,$13,$FF,$A7,$D3,$0A,$FF,$AD,$D4,$00,$FF,$9B,$BE,$0A,$FF,$8B,$B2,$00,$FF,
  $87,$AB,$01,$FF,$8E,$AE,$01,$FF,$8A,$A8,$00,$FF,$8D,$AD,$04,$FF,$8E,$B0,$01,$FF,
  $89,$AE,$00,$FF,$8A,$AE,$02,$FF,$8A,$AA,$00,$FF,$91,$AE,$08,$FF,$8A,$AF,$00,$FF,
  $8F,$AC,$06,$FF,$8B,$AB,$00,$FF,$8E,$A8,$00,$FF,$97,$A8,$02,$FF,$87,$AC,$00,$FF,
  $8E,$AB,$05,$FF,$89,$A9,$00,$FF,$92,$AB,$0F,$FF,$8F,$AD,$00,$FF,$8A,$A6,$00,$FF,
  $8D,$AA,$02,$FF,$94,$AF,$02,$FF,$8B,$A8,$00,$FF,$92,$AB,$09,$FF,$8E,$AC,$00,$FF,
  $82,$AA,$00,$FF,$90,$BB,$00,$FF,$A3,$CD,$00,$FF,$CB,$F8,$05,$FF,$D1,$FF,$14,$FF,
  $BE,$EA,$11,$FF,$A0,$CB,$09,$FF,$8F,$B5,$00,$FF,$8F,$AD,$00,$FF,$8E,$AF,$00,$FF,
  $93,$AF,$03,$FF,$8F,$AD,$00,$FF,$86,$B6,$00,$FF,$B1,$E2,$00,$FF,$CC,$FF,$04,$FF,
  $41,$87,$51,$FF,$00,$07,$11,$FF,$02,$30,$61,$FF,$00,$54,$AA,$FF,$04,$4E,$AD,$FF,
  $02,$52,$A7,$FF,$00,$54,$A4,$FF,$04,$52,$B4,$FF,$01,$51,$A6,$FF,$00,$73,$CC,$FF,
  $33,$8A,$B7,$FF,$4A,$61,$2B,$FF,$51,$69,$00,$FF,$82,$A7,$02,$FF,$8B,$B2,$00,$FF,
  $8C,$AF,$0A,$FF,$8B,$AD,$00,$FF,$8B,$B3,$03,$FF,$93,$B7,$00,$FF,$AB,$CD,$00,$FF,
  $93,$BD,$00,$FF,$94,$B5,$02,$FF,$92,$AB,$00,$FF,$89,$AA,$00,$FF,$8E,$AE,$01,$FF,
  $8E,$AB,$01,$FF,$8B,$AD,$01,$FF,$8C,$B0,$06,$FF,$89,$AA,$00,$FF,$8F,$AA,$03,$FF,
  $8C,$AB,$05,$FF,$91,$AC,$00,$FF,$89,$AB,$00,$FF,$89,$AE,$00,$FF,$8E,$AB,$01,$FF,
  $8C,$AD,$00,$FF,$88,$B3,$02,$FF,$88,$AD,$00,$FF,$89,$AF,$00,$FF,$8E,$AD,$00,$FF,
  $8C,$AE,$02,$FF,$8A,$AA,$00,$FF,$8D,$AE,$00,$FF,$8D,$AF,$00,$FF,$8A,$B1,$06,$FF,
  $88,$AA,$00,$FF,$92,$AD,$00,$FF,$87,$AD,$00,$FF,$8D,$AF,$00,$FF,$95,$AB,$0A,$FF,
  $8C,$AB,$00,$FF,$8B,$AA,$06,$FF,$94,$B1,$00,$FF,$9D,$C1,$00,$FF,$93,$B4,$01,$FF,
  $8C,$B7,$00,$FF,$8D,$AE,$05,$FF,$8F,$AB,$01,$FF,$86,$AD,$00,$FF,$87,$AD,$02,$FF,
  $8D,$AA,$00,$FF,$8E,$AF,$00,$FF,$92,$B3,$00,$FF,$D0,$FE,$05,$FF,$7F,$A1,$40,$FF,
  $04,$09,$0F,$FF,$00,$33,$60,$FF,$02,$56,$B8,$FF,$00,$50,$A9,$FF,$04,$58,$BA,$FF,
  $00,$52,$AE,$FF,$03,$53,$AC,$FF,$03,$51,$B5,$FF,$01,$77,$F5,$FF,$40,$99,$BB,$FF,
  $33,$3E,$1E,$FF,$41,$55,$00,$FF,$7C,$9B,$02,$FF,$8C,$AF,$00,$FF,$91,$AB,$02,$FF,
  $86,$AC,$00,$FF,$95,$AA,$03,$FF,$82,$AF,$00,$FF,$8D,$AE,$00,$FF,$8A,$AB,$00,$FF,
  $8C,$AC,$00,$FF,$8E,$AB,$03,$FF,$89,$B1,$01,$FF,$8E,$A9,$00,$FF,$8E,$AF,$06,$FF,
  $8B,$AC,$00,$FF,$93,$AA,$04,$FF,$84,$AF,$00,$FF,$8D,$AA,$04,$FF,$8D,$AE,$00,$FF,
  $8D,$AE,$00,$FF,$8D,$A8,$01,$FF,$8E,$A8,$06,$FF,$8A,$B0,$03,$FF,$92,$A9,$01,$FF,
  $84,$B0,$00,$FF,$8A,$AC,$00,$FF,$8B,$AA,$04,$FF,$8B,$B1,$06,$FF,$86,$AC,$01,$FF,
  $8E,$AD,$00,$FF,$8F,$AA,$03,$FF,$86,$AA,$02,$FF,$8C,$AF,$00,$FF,$92,$AA,$00,$FF,
  $87,$AD,$00,$FF,$8F,$AB,$01,$FF,$92,$AD,$00,$FF,$89,$AE,$00,$FF,$87,$AC,$06,$FF,
  $94,$A7,$00,$FF,$8D,$A6,$00,$FF,$72,$91,$07,$FF,$5C,$74,$00,$FF,$5D,$6F,$03,$FF,
  $70,$96,$05,$FF,$89,$B1,$00,$FF,$8E,$AB,$03,$FF,$8F,$AC,$04,$FF,$8D,$A8,$00,$FF,
  $88,$AF,$00,$FF,$8F,$B3,$00,$FF,$BC,$F0,$07,$FF,$7F,$99,$36,$FF,$00,$10,$1F,$FF,
  $00,$32,$5F,$FF,$07,$53,$B5,$FF,$00,$51,$AB,$FF,$00,$57,$B8,$FF,$00,$50,$A5,$FF,
  $00,$53,$AE,$FF,$00,$50,$A5,$FF,$02,$7C,$FB,$FF,$3C,$95,$B3,$FF,$2C,$31,$00,$FF,
  $40,$54,$00,$FF,$74,$90,$07,$FF,$8A,$AA,$00,$FF,$8F,$A8,$04,$FF,$8E,$AA,$00,$FF,
  $89,$AF,$00,$FF,$8C,$AD,$04,$FF,$90,$A6,$00,$FF,$8A,$B3,$0C,$FF,$8D,$AB,$00,$FF,
  $90,$AC,$02,$FF,$84,$AB,$00,$FF,$91,$B0,$00,$FF,$8D,$AC,$00,$FF,$87,$A9,$00,$FF,
  $8C,$AE,$02,$FF,$8D,$AE,$07,$FF,$8F,$AC,$00,$FF,$8E,$AC,$00,$FF,$8B,$AA,$04,$FF,
  $8E,$AD,$00,$FF,$8F,$AC,$02,$FF,$86,$B0,$00,$FF,$90,$A7,$01,$FF,$8C,$B1,$01,$FF,
  $91,$AC,$00,$FF,$8E,$AC,$00,$FF,$8F,$A6,$00,$FF,$91,$AE,$00,$FF,$8B,$AD,$00,$FF,
  $8A,$AC,$00,$FF,$8C,$AE,$00,$FF,$89,$AD,$03,$FF,$8D,$AB,$00,$FF,$8C,$AD,$00,$FF,
  $8D,$A9,$00,$FF,$8F,$A9,$00,$FF,$8D,$AD,$02,$FF,$86,$B2,$00,$FF,$7D,$A7,$00,$FF,
  $59,$72,$17,$FF,$34,$3F,$15,$FF,$14,$17,$02,$FF,$00,$02,$07,$FF,$2B,$30,$00,$FF,
  $6A,$7D,$00,$FF,$84,$A8,$00,$FF,$8A,$B3,$01,$FF,$8C,$AE,$02,$FF,$91,$AC,$05,$FF,
  $92,$B6,$00,$FF,$B2,$E3,$15,$FF,$47,$5F,$47,$FF,$02,$22,$39,$FF,$02,$40,$89,$FF,
  $00,$50,$A1,$FF,$02,$56,$B0,$FF,$03,$53,$AA,$FF,$07,$51,$B0,$FF,$01,$52,$AD,$FF,
  $03,$51,$A6,$FF,$00,$7B,$FF,$FF,$2A,$92,$C7,$FF,$22,$2A,$13,$FF,$1D,$29,$00,$FF,
  $6E,$86,$00,$FF,$8B,$B3,$03,$FF,$87,$AD,$00,$FF,$8D,$AE,$00,$FF,$8F,$AB,$01,$FF,
  $8D,$AA,$00,$FF,$8B,$B1,$02,$FF,$81,$A2,$00,$FF,$7A,$9B,$06,$FF,$78,$92,$00,$FF,
  $6F,$85,$00,$FF,$72,$8F,$01,$FF,$82,$A4,$00,$FF,$91,$AD,$0B,$FF,$91,$AD,$00,$FF,
  $8F,$AC,$00,$FF,$88,$AB,$00,$FF,$88,$AC,$02,$FF,$8E,$B1,$00,$FF,$8D,$AA,$00,$FF,
  $92,$AD,$00,$FF,$8E,$AF,$00,$FF,$87,$A0,$07,$FF,$76,$9D,$00,$FF,$7C,$9B,$02,$FF,
  $75,$A3,$00,$FF,$83,$A2,$00,$FF,$90,$AC,$00,$FF,$8C,$AA,$0A,$FF,$8B,$AE,$00,$FF,
  $8A,$B0,$05,$FF,$89,$AB,$00,$FF,$93,$A7,$07,$FF,$91,$AC,$05,$FF,$8E,$B0,$00,$FF,
  $8A,$AD,$09,$FF,$88,$A7,$01,$FF,$93,$B6,$00,$FF,$6F,$9A,$2F,$FF,$3B,$72,$52,$FF,
  $1D,$60,$7A,$FF,$22,$61,$8A,$FF,$1D,$43,$56,$FF,$0C,$1B,$00,$FF,$28,$37,$0C,$FF,
  $47,$58,$00,$FF,$6E,$83,$00,$FF,$75,$9C,$01,$FF,$79,$96,$00,$FF,$8B,$AA,$06,$FF,
  $80,$A8,$13,$FF,$18,$34,$37,$FF,$01,$2C,$4F,$FF,$03,$48,$93,$FF,$00,$55,$A7,$FF,
  $00,$52,$AE,$FF,$02,$50,$A5,$FF,$02,$50,$B2,$FF,$00,$52,$A6,$FF,$01,$56,$A9,$FF,
  $00,$7A,$ED,$FF,$31,$93,$EE,$FF,$43,$56,$38,$FF,$0C,$0F,$04,$FF,$3E,$4F,$01,$FF,
  $7F,$96,$00,$FF,$81,$A5,$00,$FF,$88,$B1,$00,$FF,$95,$A7,$07,$FF,$90,$AC,$00,$FF,
  $7B,$9C,$07,$FF,$65,$77,$07,$FF,$63,$7D,$10,$FF,$35,$5D,$21,$FF,$43,$55,$17,$FF,
  $44,$4F,$0A,$FF,$67,$84,$04,$FF,$8F,$AD,$00,$FF,$8B,$A6,$00,$FF,$8A,$AA,$00,$FF,
  $8F,$B2,$00,$FF,$8D,$A9,$00,$FF,$8E,$AE,$03,$FF,$89,$AA,$01,$FF,$89,$AC,$00,$FF,
  $73,$95,$0D,$FF,$52,$6C,$07,$FF,$28,$34,$0E,$FF,$23,$22,$03,$FF,$26,$25,$07,$FF,
  $42,$54,$00,$FF,$5D,$78,$03,$FF,$7D,$9D,$00,$FF,$8F,$AB,$08,$FF,$91,$A9,$00,$FF,
  $91,$AD,$01,$FF,$88,$AD,$05,$FF,$89,$AC,$00,$FF,$8A,$AC,$00,$FF,$89,$AF,$04,$FF,
  $8D,$B2,$00,$FF,$AD,$D0,$00,$FF,$58,$7E,$4D,$FF,$19,$5C,$86,$FF,$00,$6A,$DF,$FF,
  $00,$88,$FD,$FF,$0E,$8B,$F7,$FF,$32,$77,$A0,$FF,$20,$39,$1C,$FF,$00,$01,$0E,$FF,
  $0E,$17,$02,$FF,$3A,$49,$12,$FF,$46,$55,$02,$FF,$50,$5C,$14,$FF,$30,$48,$28,$FF,
  $11,$38,$59,$FF,$08,$40,$8A,$FF,$01,$4E,$9E,$FF,$02,$57,$B1,$FF,$00,$51,$AF,$FF,
  $03,$53,$AA,$FF,$00,$53,$A8,$FF,$01,$50,$AC,$FF,$00,$51,$9F,$FF,$04,$70,$D5,$FF,
  $28,$92,$FF,$FF,$4B,$90,$C9,$FF,$2D,$52,$27,$FF,$24,$2B,$00,$FF,$3C,$4B,$08,$FF,
  $54,$67,$0B,$FF,$56,$74,$02,$FF,$5A,$72,$04,$FF,$65,$86,$04,$FF,$37,$56,$06,$FF,
  $19,$59,$7C,$FF,$32,$7C,$85,$FF,$16,$59,$90,$FF,$20,$56,$78,$FF,$26,$33,$17,$FF,
  $44,$52,$00,$FF,$80,$A7,$00,$FF,$8D,$AE,$07,$FF,$89,$AC,$08,$FF,$8F,$AA,$00,$FF,
  $8D,$AE,$00,$FF,$89,$AA,$03,$FF,$86,$A6,$05,$FF,$77,$95,$03,$FF,$53,$6F,$18,$FF,
  $45,$6C,$35,$FF,$2B,$47,$4B,$FF,$20,$3A,$1D,$FF,$10,$13,$00,$FF,$01,$12,$02,$FF,
  $15,$1B,$01,$FF,$4F,$55,$00,$FF,$7C,$9B,$02,$FF,$8A,$AF,$00,$FF,$8C,$AA,$00,$FF,
  $87,$B0,$00,$FF,$8C,$AF,$0A,$FF,$8E,$AA,$00,$FF,$8D,$AB,$00,$FF,$9A,$C1,$04,$FF,
  $A6,$CC,$27,$FF,$26,$4F,$7B,$FF,$04,$3C,$77,$FF,$00,$56,$B0,$FF,$00,$6D,$D2,$FF,
  $00,$6F,$EC,$FF,$0A,$84,$FF,$FF,$3A,$96,$D7,$FF,$49,$73,$4B,$FF,$18,$22,$0A,$FF,
  $0E,$0F,$00,$FF,$1B,$1C,$0C,$FF,$19,$2F,$3A,$FF,$01,$26,$52,$FF,$03,$41,$8A,$FF,
  $00,$51,$AD,$FF,$00,$52,$AD,$FF,$00,$51,$A3,$FF,$01,$52,$AE,$FF,$00,$50,$AE,$FF,
  $00,$53,$B3,$FF,$01,$53,$A7,$FF,$00,$55,$A8,$FF,$02,$59,$C2,$FF,$00,$6F,$F1,$FF,
  $11,$93,$F3,$FF,$38,$93,$E6,$FF,$47,$89,$87,$FF,$3D,$61,$31,$FF,$4E,$5B,$16,$FF,
  $3E,$52,$0B,$FF,$49,$63,$22,$FF,$54,$90,$76,$FF,$2E,$6F,$69,$FF,$05,$61,$C4,$FF,
  $0A,$7B,$D7,$FF,$0F,$64,$C1,$FF,$14,$79,$D3,$FF,$30,$5D,$48,$FF,$39,$45,$05,$FF,
  $70,$96,$05,$FF,$8B,$AB,$00,$FF,$8D,$AD,$00,$FF,$8B,$AB,$00,$FF,$8D,$AD,$04,$FF,
  $91,$B8,$00,$FF,$83,$A7,$11,$FF,$3F,$60,$35,$FF,$10,$45,$67,$FF,$11,$74,$CF,$FF,
  $0C,$7F,$FF,$FF,$06,$87,$FC,$FF,$2A,$8C,$E3,$FF,$47,$93,$91,$FF,$2E,$4C,$26,$FF,
  $18,$1C,$0D,$FF,$43,$61,$00,$FF,$7E,$A0,$03,$FF,$91,$A8,$02,$FF,$89,$AC,$00,$FF,
  $8D,$AA,$02,$FF,$8C,$AC,$00,$FF,$8E,$AE,$03,$FF,$AC,$D7,$01,$FF,$9B,$C7,$2C,$FF,
  $06,$42,$88,$FF,$01,$3E,$7F,$FF,$08,$50,$B4,$FF,$01,$51,$B4,$FF,$07,$5A,$C0,$FF,
  $02,$6F,$EE,$FF,$05,$87,$FF,$FF,$0C,$8C,$FA,$FF,$49,$A8,$DE,$FF,$4C,$8D,$91,$FF,
  $24,$73,$84,$FF,$10,$59,$9D,$FF,$02,$4D,$90,$FF,$00,$50,$A0,$FF,$00,$56,$A9,$FF,
  $06,$53,$AF,$FF,$07,$50,$AE,$FF,$02,$58,$AF,$FF,$00,$59,$BB,$FF,$03,$53,$B2,$FF,
  $00,$50,$AA,$FF,$00,$51,$A6,$FF,$04,$52,$A7,$FF,$00,$5C,$BB,$FF,$00,$68,$C4,$FF,
  $02,$71,$F6,$FF,$03,$7D,$FF,$FF,$08,$85,$FD,$FF,$0B,$72,$CE,$FF,$02,$64,$BD,$FF,
  $0A,$65,$B8,$FF,$02,$72,$FA,$FF,$00,$6C,$D9,$FF,$00,$56,$BA,$FF,$0B,$66,$C3,$FF,
  $09,$61,$B8,$FF,$18,$7A,$B9,$FF,$3B,$6D,$52,$FF,$4C,$5C,$1B,$FF,$77,$9A,$00,$FF,
  $90,$AD,$00,$FF,$90,$AC,$02,$FF,$85,$AE,$00,$FF,$8F,$A8,$02,$FF,$A8,$D2,$02,$FF,
  $98,$C6,$32,$FF,$27,$4B,$57,$FF,$00,$34,$77,$FF,$00,$6A,$C7,$FF,$00,$6E,$E4,$FF,
  $01,$76,$E0,$FF,$04,$7A,$F5,$FF,$00,$85,$FE,$FF,$28,$93,$F3,$FF,$2A,$54,$60,$FF,
  $29,$2E,$00,$FF,$58,$70,$00,$FF,$7E,$A5,$00,$FF,$98,$AE,$01,$FF,$93,$AE,$07,$FF,
  $8C,$AD,$06,$FF,$86,$AC,$00,$FF,$A7,$D4,$00,$FF,$9C,$C2,$1F,$FF,$01,$46,$67,$FF,
  $00,$42,$83,$FF,$00,$56,$AF,$FF,$02,$50,$B2,$FF,$06,$54,$9F,$FF,$00,$58,$AF,$FF,
  $00,$67,$C6,$FF,$03,$74,$EC,$FF,$02,$7F,$FF,$FF,$00,$80,$F8,$FF,$00,$76,$F1,$FF,
  $00,$62,$D1,$FF,$00,$57,$B6,$FF,$00,$55,$AF,$FF,$00,$53,$AD,$FF,$04,$4D,$A9,$FF,
  $05,$4C,$B4,$FF,$00,$52,$A9,$FF,$00,$5C,$B6,$FF,$00,$4F,$AE,$FF,$09,$54,$A5,$FF,
  $05,$45,$9F,$FF,$00,$52,$A6,$FF,$00,$59,$A6,$FF,$01,$50,$AF,$FF,$09,$53,$AC,$FF,
  $00,$58,$B0,$FF,$00,$6E,$CF,$FF,$00,$6A,$CF,$FF,$04,$64,$CA,$FF,$02,$58,$AF,$FF,
  $03,$4F,$B1,$FF,$00,$52,$B9,$FF,$00,$5E,$B2,$FF,$0B,$57,$AB,$FF,$01,$5E,$AC,$FF,
  $32,$66,$8B,$FF,$50,$66,$37,$FF,$70,$87,$15,$FF,$82,$AA,$00,$FF,$88,$AD,$00,$FF,
  $8D,$AB,$00,$FF,$8D,$AC,$08,$FF,$8F,$AD,$00,$FF,$AE,$DD,$01,$FF,$AA,$D2,$60,$FF,
  $25,$49,$49,$FF,$00,$2A,$54,$FF,$00,$54,$A8,$FF,$03,$53,$B6,$FF,$00,$4F,$AC,$FF,
  $04,$53,$BC,$FF,$00,$69,$CB,$FF,$03,$85,$FF,$FF,$34,$90,$F7,$FF,$29,$49,$22,$FF,
  $2B,$2D,$08,$FF,$7A,$9E,$00,$FF,$87,$AB,$03,$FF,$85,$AA,$00,$FF,$8F,$B0,$00,$FF,
  $8C,$AF,$00,$FF,$A4,$CE,$08,$FF,$A9,$C9,$28,$FF,$0B,$3E,$69,$FF,$05,$38,$77,$FF,
  $00,$51,$A9,$FF,$00,$53,$AD,$FF,$00,$55,$AE,$FF,$06,$50,$B1,$FF,$02,$51,$B0,$FF,
  $00,$59,$B3,$FF,$02,$69,$D0,$FF,$00,$6E,$D4,$FF,$01,$61,$C5,$FF,$00,$5E,$B0,$FF,
  $02,$4D,$A8,$FF,$02,$51,$AE,$FF,$00,$56,$AF,$FF,$00,$53,$AE,$FF,$02,$54,$AA,$FF,
  $01,$52,$AE,$FF,$04,$50,$A6,$FF,$00,$52,$B2,$FF,$09,$55,$B3,$FF,$01,$4C,$9F,$FF,
  $00,$4A,$9A,$FF,$01,$50,$AC,$FF,$01,$51,$B0,$FF,$00,$53,$AF,$FF,$01,$51,$B4,$FF,
  $00,$5B,$B8,$FF,$03,$58,$B5,$FF,$02,$52,$B3,$FF,$03,$54,$AD,$FF,$01,$53,$A7,$FF,
  $00,$57,$BE,$FF,$05,$57,$BA,$FF,$12,$58,$96,$FF,$1E,$64,$98,$FF,$2E,$4A,$4D,$FF,
  $4F,$67,$0F,$FF,$80,$A7,$00,$FF,$91,$AC,$05,$FF,$8D,$A9,$00,$FF,$8A,$AE,$04,$FF,
  $8F,$AD,$00,$FF,$8F,$AB,$01,$FF,$B5,$DE,$06,$FF,$C6,$E1,$56,$FF,$18,$40,$4C,$FF,
  $00,$2B,$51,$FF,$01,$53,$A7,$FF,$05,$4F,$AE,$FF,$00,$52,$AC,$FF,$00,$55,$AE,$FF,
  $04,$51,$AD,$FF,$02,$5F,$C6,$FF,$02,$78,$F6,$FF,$32,$6E,$66,$FF,$2B,$35,$01,$FF,
  $6B,$94,$00,$FF,$90,$AC,$02,$FF,$88,$AD,$00,$FF,$92,$B0,$00,$FF,$85,$AA,$02,$FF,
  $9D,$BC,$06,$FF,$AB,$E8,$0F,$FF,$1C,$51,$71,$FF,$00,$31,$63,$FF,$01,$54,$A4,$FF,
  $00,$53,$A6,$FF,$00,$54,$AE,$FF,$00,$52,$B2,$FF,$00,$55,$A8,$FF,$01,$52,$AD,$FF,
  $01,$50,$AC,$FF,$04,$52,$B4,$FF,$00,$51,$AC,$FF,$00,$54,$AB,$FF,$00,$51,$AE,$FF,
  $04,$55,$B0,$FF,$03,$52,$B1,$FF,$00,$4E,$AF,$FF,$02,$53,$AE,$FF,$00,$53,$B3,$FF,
  $00,$52,$AC,$FF,$00,$55,$AD,$FF,$00,$58,$B4,$FF,$00,$51,$A9,$FF,$01,$52,$A3,$FF,
  $04,$54,$AD,$FF,$02,$53,$AE,$FF,$04,$51,$A9,$FF,$09,$50,$AC,$FF,$00,$53,$A8,$FF,
  $03,$55,$AB,$FF,$00,$51,$A4,$FF,$00,$54,$A7,$FF,$03,$54,$AF,$FF,$00,$50,$A1,$FF,
  $07,$5E,$A1,$FF,$1B,$66,$A7,$FF,$36,$73,$74,$FF,$2E,$49,$16,$FF,$64,$7B,$06,$FF,
  $83,$A8,$02,$FF,$8D,$AA,$00,$FF,$8F,$AD,$00,$FF,$89,$AD,$05,$FF,$8A,$AB,$00,$FF,
  $8E,$AD,$00,$FF,$AB,$D4,$00,$FF,$CF,$EC,$46,$FF,$1F,$50,$4C,$FF,$00,$14,$46,$FF,
  $00,$4C,$99,$FF,$02,$4E,$AC,$FF,$00,$54,$AC,$FF,$00,$50,$A5,$FF,$00,$54,$B1,$FF,
  $00,$55,$B6,$FF,$01,$7B,$FF,$FF,$30,$65,$87,$FF,$3E,$47,$10,$FF,$78,$9A,$07,$FF,
  $90,$AD,$03,$FF,$8E,$AC,$00,$FF,$8D,$A3,$02,$FF,$8D,$AF,$00,$FF,$97,$B5,$07,$FF,
  $B3,$E4,$0C,$FF,$22,$4F,$79,$FF,$00,$28,$58,$FF,$00,$4B,$9D,$FF,$07,$51,$AA,$FF,
  $04,$50,$B0,$FF,$00,$51,$AD,$FF,$00,$51,$B1,$FF,$00,$51,$AB,$FF,$00,$51,$B1,$FF,
  $03,$54,$B0,$FF,$03,$54,$B0,$FF,$09,$4F,$B1,$FF,$01,$51,$AA,$FF,$00,$51,$A3,$FF,
  $00,$52,$A8,$FF,$00,$55,$AA,$FF,$00,$55,$A6,$FF,$00,$51,$AA,$FF,$00,$55,$B2,$FF,
  $00,$58,$A1,$FF,$00,$5A,$B0,$FF,$00,$53,$AB,$FF,$06,$54,$A9,$FF,$01,$50,$AC,$FF,
  $00,$52,$AE,$FF,$00,$54,$AD,$FF,$00,$55,$AF,$FF,$05,$50,$B5,$FF,$08,$50,$B6,$FF,
  $01,$51,$B0,$FF,$00,$53,$B3,$FF,$04,$4E,$AF,$FF,$0F,$59,$A0,$FF,$0E,$5F,$8E,$FF,
  $2C,$74,$AE,$FF,$43,$70,$51,$FF,$4B,$61,$0F,$FF,$7B,$91,$02,$FF,$8A,$AB,$02,$FF,
  $90,$AA,$0B,$FF,$8E,$AD,$00,$FF,$8B,$B0,$01,$FF,$8C,$AC,$03,$FF,$8D,$AB,$00,$FF,
  $A7,$CF,$00,$FF,$DA,$FD,$2D,$FF,$1B,$4A,$52,$FF,$06,$0B,$1F,$FF,$00,$43,$8A,$FF,
  $04,$52,$B4,$FF,$00,$52,$A8,$FF,$05,$50,$AB,$FF,$00,$55,$A5,$FF,$00,$50,$B3,$FF,
  $00,$72,$F8,$FF,$3A,$68,$5D,$FF,$48,$59,$00,$FF,$8D,$AE,$00,$FF,$8A,$AB,$00,$FF,
  $8A,$B0,$03,$FF,$8C,$AE,$02,$FF,$8C,$AF,$00,$FF,$90,$B2,$06,$FF,$B8,$DA,$07,$FF,
  $23,$86,$67,$FF,$00,$28,$50,$FF,$02,$47,$94,$FF,$02,$51,$B0,$FF,$01,$50,$AF,$FF,
  $04,$54,$AD,$FF,$04,$53,$B2,$FF,$03,$55,$AB,$FF,$03,$54,$AF,$FF,$00,$53,$A6,$FF,
  $00,$52,$AC,$FF,$01,$53,$A7,$FF,$00,$57,$AA,$FF,$00,$54,$AE,$FF,$02,$53,$AF,$FF,
  $00,$52,$AE,$FF,$01,$4D,$AB,$FF,$06,$4D,$A7,$FF,$03,$53,$A8,$FF,$03,$56,$B2,$FF,
  $01,$53,$B6,$FF,$00,$4F,$AE,$FF,$01,$50,$AC,$FF,$03,$51,$B3,$FF,$05,$54,$B3,$FF,
  $03,$53,$AA,$FF,$00,$50,$A9,$FF,$00,$51,$AC,$FF,$00,$50,$A7,$FF,$00,$53,$AE,$FF,
  $01,$5B,$BB,$FF,$04,$55,$AE,$FF,$14,$5F,$A3,$FF,$0F,$57,$89,$FF,$27,$67,$8D,$FF,
  $3F,$5C,$3E,$FF,$6B,$81,$03,$FF,$88,$A1,$00,$FF,$8C,$AD,$06,$FF,$8E,$AB,$01,$FF,
  $8D,$AC,$00,$FF,$8A,$AE,$02,$FF,$8C,$AE,$00,$FF,$90,$AD,$07,$FF,$96,$BF,$00,$FF,
  $D5,$FF,$13,$FF,$4C,$72,$65,$FF,$02,$04,$00,$FF,$00,$2F,$65,$FF,$03,$4F,$AF,$FF,
  $01,$52,$AB,$FF,$03,$53,$AC,$FF,$04,$52,$B6,$FF,$01,$5C,$AB,$FF,$0E,$6B,$C8,$FF,
  $3F,$58,$3A,$FF,$58,$73,$00,$FF,$8F,$AC,$02,$FF,$8B,$AC,$03,$FF,$8D,$AA,$02,$FF,
  $8B,$AC,$00,$FF,$8C,$AD,$00,$FF,$87,$AC,$07,$FF,$BD,$ED,$0B,$FF,$71,$B9,$65,$FF,
  $0D,$41,$66,$FF,$01,$46,$8B,$FF,$00,$55,$B2,$FF,$00,$55,$A5,$FF,$02,$54,$A8,$FF,
  $02,$50,$A3,$FF,$02,$4E,$AC,$FF,$03,$54,$A6,$FF,$00,$51,$AC,$FF,$00,$53,$B3,$FF,
  $01,$51,$B4,$FF,$00,$52,$AF,$FF,$01,$4F,$B1,$FF,$07,$51,$B0,$FF,$00,$54,$AF,$FF,
  $01,$54,$B2,$FF,$06,$55,$B1,$FF,$00,$51,$AB,$FF,$04,$56,$BA,$FF,$01,$51,$B0,$FF,
  $00,$53,$A4,$FF,$00,$52,$A2,$FF,$00,$53,$AD,$FF,$00,$4E,$AA,$FF,$03,$50,$A8,$FF,
  $03,$53,$B2,$FF,$00,$57,$B1,$FF,$03,$53,$A8,$FF,$00,$52,$AC,$FF,$02,$5B,$B7,$FF,
  $0D,$5F,$B1,$FF,$0D,$58,$92,$FF,$22,$5D,$7D,$FF,$29,$50,$57,$FF,$4A,$63,$2A,$FF,
  $7A,$99,$00,$FF,$8F,$B0,$01,$FF,$8B,$B0,$01,$FF,$8C,$AE,$00,$FF,$8E,$AF,$00,$FF,
  $8D,$AE,$07,$FF,$8A,$AB,$00,$FF,$8F,$AB,$01,$FF,$8B,$B4,$00,$FF,$CD,$FF,$06,$FF,
  $AA,$CF,$5D,$FF,$00,$07,$07,$FF,$00,$1E,$3B,$FF,$01,$4C,$9F,$FF,$00,$51,$AF,$FF,
  $02,$52,$A7,$FF,$00,$51,$AE,$FF,$0D,$59,$A4,$FF,$15,$49,$5F,$FF,$43,$57,$10,$FF,
  $77,$93,$02,$FF,$8B,$A8,$00,$FF,$8B,$B1,$00,$FF,$89,$AE,$00,$FF,$89,$B0,$00,$FF,
  $88,$AD,$00,$FF,$89,$AB,$00,$FF,$A5,$D1,$02,$FF,$75,$DF,$3F,$FF,$16,$52,$6C,$FF,
  $00,$35,$75,$FF,$00,$4F,$AE,$FF,$05,$53,$A8,$FF,$06,$53,$AD,$FF,$02,$51,$AE,$FF,
  $02,$51,$B8,$FF,$01,$55,$AF,$FF,$02,$51,$AE,$FF,$04,$50,$AE,$FF,$04,$52,$A5,$FF,
  $01,$53,$9F,$FF,$04,$4F,$A0,$FF,$05,$4E,$AA,$FF,$00,$52,$AB,$FF,$00,$53,$A8,$FF,
  $05,$52,$AC,$FF,$04,$50,$B0,$FF,$00,$58,$B2,$FF,$00,$51,$A4,$FF,$00,$49,$8C,$FF,
  $02,$44,$80,$FF,$07,$44,$87,$FF,$05,$45,$8D,$FF,$00,$49,$96,$FF,$00,$51,$AA,$FF,
  $03,$4E,$AB,$FF,$04,$51,$A9,$FF,$00,$55,$AC,$FF,$03,$5B,$B4,$FF,$13,$66,$B2,$FF,
  $23,$72,$99,$FF,$35,$70,$6C,$FF,$32,$4D,$2E,$FF,$60,$7C,$0D,$FF,$85,$AA,$00,$FF,
  $89,$AD,$01,$FF,$84,$A9,$00,$FF,$8D,$AF,$01,$FF,$89,$A9,$00,$FF,$8E,$AE,$03,$FF,
  $8C,$AB,$07,$FF,$8C,$AA,$00,$FF,$8F,$B6,$01,$FF,$C4,$F8,$02,$FF,$DA,$FF,$34,$FF,
  $29,$47,$47,$FF,$02,$1A,$32,$FF,$01,$46,$91,$FF,$00,$52,$AD,$FF,$03,$50,$BA,$FF,
  $03,$6D,$C3,$FF,$36,$63,$8D,$FF,$3B,$4D,$23,$FF,$62,$81,$00,$FF,$89,$A4,$00,$FF,
  $8F,$AC,$02,$FF,$89,$AE,$00,$FF,$91,$A7,$00,$FF,$95,$AD,$00,$FF,$8F,$AB,$08,$FF,
  $92,$AD,$00,$FF,$89,$B2,$00,$FF,$B6,$E5,$09,$FF,$40,$7A,$51,$FF,$09,$31,$65,$FF,
  $00,$4B,$91,$FF,$01,$4E,$96,$FF,$00,$4A,$8E,$FF,$00,$47,$98,$FF,$00,$46,$8D,$FF,
  $00,$39,$7C,$FF,$00,$2F,$60,$FF,$02,$28,$57,$FF,$00,$25,$3E,$FF,$00,$27,$55,$FF,
  $01,$35,$67,$FF,$02,$43,$85,$FF,$04,$52,$9D,$FF,$01,$57,$AA,$FF,$00,$50,$B1,$FF,
  $00,$53,$AD,$FF,$00,$5B,$B7,$FF,$00,$58,$BA,$FF,$00,$3F,$87,$FF,$03,$20,$42,$FF,
  $00,$0C,$1F,$FF,$00,$19,$32,$FF,$00,$1F,$43,$FF,$05,$29,$5B,$FF,$00,$39,$7E,$FF,
  $02,$4A,$96,$FF,$01,$58,$B5,$FF,$06,$5F,$BD,$FF,$0E,$62,$AB,$FF,$24,$75,$AC,$FF,
  $1C,$59,$6C,$FF,$1F,$39,$1E,$FF,$53,$69,$00,$FF,$67,$85,$03,$FF,$66,$82,$07,$FF,
  $65,$7E,$08,$FF,$65,$7D,$01,$FF,$69,$85,$00,$FF,$7B,$97,$04,$FF,$83,$A7,$00,$FF,
  $87,$A6,$02,$FF,$8A,$AC,$00,$FF,$A4,$CE,$00,$FF,$D0,$FA,$1A,$FF,$52,$82,$68,$FF,
  $06,$17,$35,$FF,$00,$40,$89,$FF,$03,$57,$AF,$FF,$00,$5B,$B5,$FF,$1C,$82,$D6,$FF,
  $62,$83,$32,$FF,$7B,$91,$12,$FF,$84,$A9,$00,$FF,$8B,$AC,$00,$FF,$8C,$AC,$01,$FF,
  $8A,$AC,$00,$FF,$8C,$AE,$00,$FF,$8C,$A5,$00,$FF,$87,$AF,$00,$FF,$86,$9F,$05,$FF,
  $87,$A0,$00,$FF,$9E,$D7,$00,$FF,$54,$81,$24,$FF,$00,$18,$40,$FF,$00,$27,$5A,$FF,
  $00,$27,$5E,$FF,$00,$25,$53,$FF,$08,$36,$67,$FF,$13,$49,$6D,$FF,$16,$48,$6B,$FF,
  $21,$4F,$5F,$FF,$2D,$52,$58,$FF,$24,$3F,$36,$FF,$1A,$32,$3E,$FF,$0F,$3B,$3E,$FF,
  $00,$2E,$4F,$FF,$00,$22,$5F,$FF,$03,$3A,$7B,$FF,$01,$4A,$A8,$FF,$07,$54,$AE,$FF,
  $02,$5A,$B4,$FF,$05,$65,$CB,$FF,$06,$53,$97,$FF,$26,$49,$4B,$FF,$39,$4C,$2C,$FF,
  $41,$63,$4B,$FF,$17,$36,$38,$FF,$02,$0D,$1F,$FF,$00,$10,$28,$FF,$00,$2B,$52,$FF,
  $00,$42,$8D,$FF,$0D,$52,$97,$FF,$19,$5A,$76,$FF,$1E,$5D,$7E,$FF,$25,$51,$5C,$FF,
  $3D,$49,$09,$FF,$2B,$32,$00,$FF,$27,$30,$1D,$FF,$3C,$4B,$20,$FF,$3B,$46,$1B,$FF,
  $1F,$27,$10,$FF,$43,$54,$06,$FF,$5B,$73,$01,$FF,$71,$93,$00,$FF,$74,$95,$00,$FF,
  $7C,$99,$03,$FF,$84,$A4,$05,$FF,$BD,$EB,$16,$FF,$3D,$74,$4D,$FF,$04,$15,$31,$FF,
  $01,$44,$8A,$FF,$00,$51,$AC,$FF,$00,$69,$CC,$FF,$10,$64,$AD,$FF,$5D,$75,$07,$FF,
  $78,$9A,$08,$FF,$90,$AB,$00,$FF,$86,$AF,$00,$FF,$8C,$AC,$03,$FF,$8E,$AC,$00,$FF,
  $84,$AE,$04,$FF,$8A,$A6,$00,$FF,$79,$AB,$00,$FF,$7A,$9C,$00,$FF,$6F,$84,$00,$FF,
  $81,$98,$12,$FF,$49,$63,$24,$FF,$07,$22,$37,$FF,$18,$43,$53,$FF,$24,$48,$46,$FF,
  $30,$4E,$32,$FF,$58,$7D,$2D,$FF,$7C,$A8,$39,$FF,$87,$BB,$2A,$FF,$9F,$D3,$28,$FF,
  $AF,$E3,$1C,$FF,$A6,$D7,$19,$FF,$9C,$C5,$13,$FF,$8B,$C2,$1F,$FF,$48,$6E,$3D,$FF,
  $08,$0E,$24,$FF,$00,$1B,$28,$FF,$00,$45,$83,$FF,$03,$52,$B1,$FF,$00,$5C,$B1,$FF,
  $02,$6C,$D9,$FF,$31,$7B,$7E,$FF,$73,$9E,$2A,$FF,$AD,$E7,$16,$FF,$BA,$F1,$22,$FF,
  $A3,$E1,$38,$FF,$46,$9E,$64,$FF,$1E,$4A,$4B,$FF,$13,$35,$37,$FF,$16,$38,$54,$FF,
  $17,$54,$70,$FF,$30,$6C,$6D,$FF,$4B,$74,$4C,$FF,$4A,$71,$32,$FF,$59,$75,$0E,$FF,
  $25,$34,$15,$FF,$0D,$2B,$29,$FF,$1A,$58,$7F,$FF,$2C,$82,$A3,$FF,$2F,$76,$78,$FF,
  $32,$52,$2D,$FF,$3A,$47,$01,$FF,$37,$47,$08,$FF,$3E,$55,$00,$FF,$55,$64,$09,$FF,
  $54,$6A,$06,$FF,$69,$8D,$11,$FF,$22,$41,$3C,$FF,$01,$34,$61,$FF,$06,$50,$A7,$FF,
  $00,$52,$A5,$FF,$00,$6A,$E4,$FF,$26,$75,$9E,$FF,$36,$46,$07,$FF,$50,$59,$0A,$FF,
  $6F,$97,$00,$FF,$87,$A3,$01,$FF,$81,$A3,$04,$FF,$79,$9E,$00,$FF,$80,$A8,$0A,$FF,
  $74,$90,$00,$FF,$5E,$73,$00,$FF,$4F,$6B,$00,$FF,$42,$68,$1D,$FF,$66,$8A,$40,$FF,
  $45,$70,$66,$FF,$36,$5D,$3E,$FF,$58,$84,$25,$FF,$74,$A8,$44,$FF,$7C,$A7,$19,$FF,
  $90,$B7,$0C,$FF,$9A,$CA,$12,$FF,$92,$BB,$07,$FF,$98,$BD,$00,$FF,$9D,$C6,$14,$FF,
  $9F,$BE,$00,$FF,$98,$BB,$00,$FF,$BD,$EE,$06,$FF,$BB,$F4,$2B,$FF,$64,$8A,$4D,$FF,
  $0A,$2F,$49,$FF,$00,$35,$81,$FF,$03,$53,$AA,$FF,$00,$62,$C7,$FF,$03,$73,$F3,$FF,
  $42,$74,$57,$FF,$52,$71,$00,$FF,$8F,$B8,$02,$FF,$AE,$D8,$00,$FF,$C0,$ED,$00,$FF,
  $B6,$FA,$29,$FF,$98,$E1,$32,$FF,$88,$BC,$34,$FF,$79,$A3,$33,$FF,$83,$BD,$33,$FF,
  $8B,$C5,$33,$FF,$94,$C2,$2B,$FF,$99,$D4,$20,$FF,$67,$9A,$35,$FF,$42,$72,$62,$FF,
  $0C,$41,$6D,$FF,$00,$4F,$AB,$FF,$02,$78,$E4,$FF,$0C,$8B,$F6,$FF,$2D,$91,$CC,$FF,
  $4D,$92,$80,$FF,$48,$78,$46,$FF,$48,$6D,$39,$FF,$5A,$7C,$26,$FF,$4D,$75,$2C,$FF,
  $17,$41,$4D,$FF,$00,$32,$5C,$FF,$00,$44,$99,$FF,$00,$53,$B1,$FF,$00,$51,$B5,$FF,
  $00,$6D,$E0,$FF,$0C,$8B,$F4,$FF,$2D,$6F,$63,$FF,$3D,$49,$0B,$FF,$41,$4F,$00,$FF,
  $48,$52,$09,$FF,$52,$67,$00,$FF,$60,$72,$02,$FF,$5C,$71,$00,$FF,$54,$72,$1C,$FF,
  $56,$83,$40,$FF,$58,$8F,$7C,$FF,$2B,$81,$9C,$FF,$21,$8B,$E3,$FF,$2B,$A1,$D3,$FF,
  $43,$8E,$77,$FF,$4F,$73,$29,$FF,$69,$8B,$12,$FF,$7D,$9A,$02,$FF,$8C,$AC,$0B,$FF,
  $8B,$B0,$00,$FF,$94,$B7,$01,$FF,$91,$B5,$0B,$FF,$91,$B7,$00,$FF,$93,$B3,$00,$FF,
  $97,$BB,$01,$FF,$9B,$BE,$00,$FF,$BA,$E0,$01,$FF,$8A,$B0,$0B,$FF,$12,$3A,$46,$FF,
  $00,$39,$7C,$FF,$03,$52,$AF,$FF,$00,$64,$C2,$FF,$00,$70,$F0,$FF,$34,$4F,$40,$FF,
  $20,$24,$01,$FF,$75,$91,$08,$FF,$91,$B5,$00,$FF,$9C,$BA,$04,$FF,$A0,$CC,$00,$FF,
  $AC,$DB,$01,$FF,$AB,$D1,$08,$FF,$AB,$D5,$09,$FF,$A4,$D0,$0B,$FF,$A2,$CE,$09,$FF,
  $AB,$D6,$00,$FF,$C6,$F8,$00,$FF,$76,$A6,$43,$FF,$12,$59,$79,$FF,$0C,$50,$91,$FF,
  $03,$4F,$A3,$FF,$00,$59,$B3,$FF,$00,$6F,$D3,$FF,$00,$76,$F0,$FF,$00,$77,$FE,$FF,
  $05,$81,$FB,$FF,$11,$79,$D2,$FF,$2A,$8C,$E5,$FF,$2F,$8F,$E5,$FF,$0C,$62,$C7,$FF,
  $00,$4C,$84,$FF,$00,$4B,$91,$FF,$00,$53,$9E,$FF,$00,$55,$AF,$FF,$07,$5B,$BB,$FF,
  $00,$72,$EB,$FF,$0A,$82,$FC,$FF,$01,$74,$CF,$FF,$2B,$7D,$79,$FF,$3D,$62,$2F,$FF,
  $41,$60,$25,$FF,$4A,$6E,$26,$FF,$3D,$71,$4B,$FF,$15,$5B,$A1,$FF,$00,$72,$D9,$FF,
  $00,$78,$F0,$FF,$00,$70,$DC,$FF,$00,$6F,$EF,$FF,$08,$86,$FF,$FF,$1F,$7F,$B9,$FF,
  $4F,$73,$35,$FF,$68,$80,$00,$FF,$92,$AF,$07,$FF,$87,$A8,$00,$FF,$8E,$A8,$06,$FF,
  $90,$AE,$00,$FF,$8A,$AE,$04,$FF,$8C,$AB,$00,$FF,$8D,$AF,$00,$FF,$85,$A8,$04,$FF,
  $8F,$B4,$02,$FF,$7A,$9A,$08,$FF,$38,$5C,$2C,$FF,$00,$35,$49,$FF,$00,$47,$88,$FF,
  $02,$50,$B2,$FF,$00,$5B,$BA,$FF,$04,$76,$FD,$FF,$33,$62,$5C,$FF,$0C,$0E,$00,$FF,
  $5C,$74,$04,$FF,$83,$AC,$00,$FF,$8B,$B2,$01,$FF,$8F,$B6,$05,$FF,$9E,$B0,$04,$FF,
  $96,$B0,$00,$FF,$95,$B7,$01,$FF,$96,$B6,$00,$FF,$90,$B3,$00,$FF,$9D,$BF,$08,$FF,
  $C5,$EB,$04,$FF,$4D,$76,$30,$FF,$0B,$35,$4B,$FF,$05,$3F,$89,$FF,$00,$4F,$A4,$FF,
  $00,$53,$A6,$FF,$01,$55,$AF,$FF,$01,$56,$B0,$FF,$02,$56,$B8,$FF,$06,$5A,$BA,$FF,
  $00,$6D,$D4,$FF,$00,$6B,$E8,$FF,$00,$69,$E4,$FF,$00,$71,$DB,$FF,$00,$54,$BB,$FF,
  $00,$4B,$A8,$FF,$02,$52,$A9,$FF,$01,$4F,$B1,$FF,$00,$4F,$A8,$FF,$00,$5B,$BB,$FF,
  $07,$62,$CD,$FF,$02,$74,$F1,$FF,$02,$87,$FD,$FF,$24,$92,$F3,$FF,$1E,$7A,$CD,$FF,
  $22,$7D,$B6,$FF,$0A,$6D,$CA,$FF,$04,$55,$B1,$FF,$00,$54,$BB,$FF,$00,$50,$AA,$FF,
  $05,$50,$B5,$FF,$06,$56,$B5,$FF,$00,$69,$DF,$FF,$09,$61,$B8,$FF,$44,$62,$2E,$FF,
  $73,$93,$01,$FF,$88,$B4,$00,$FF,$8D,$B6,$02,$FF,$8E,$A9,$04,$FF,$8A,$AA,$01,$FF,
  $8C,$AD,$00,$FF,$8F,$AB,$00,$FF,$85,$AF,$05,$FF,$93,$B1,$03,$FF,$86,$A7,$00,$FF,
  $68,$87,$2A,$FF,$2A,$54,$62,$FF,$02,$33,$76,$FF,$0A,$51,$A9,$FF,$00,$52,$A9,$FF,
  $00,$5B,$B5,$FF,$00,$7C,$F6,$FF,$2C,$8C,$B2,$FF,$17,$26,$05,$FF,$41,$51,$00,$FF,
  $86,$A7,$00,$FF,$8A,$AB,$00,$FF,$8A,$AA,$01,$FF,$8C,$AE,$00,$FF,$8B,$B0,$00,$FF,
  $8A,$B0,$05,$FF,$8E,$A9,$00,$FF,$8F,$AD,$00,$FF,$98,$C1,$03,$FF,$92,$BC,$28,$FF,
  $2D,$5F,$66,$FF,$05,$31,$4E,$FF,$00,$37,$7D,$FF,$00,$4B,$99,$FF,$07,$55,$AA,$FF,
  $00,$4D,$AA,$FF,$00,$51,$A6,$FF,$00,$55,$AF,$FF,$00,$4F,$AB,$FF,$02,$4F,$AB,$FF,
  $01,$4E,$AA,$FF,$00,$58,$A7,$FF,$00,$5C,$B0,$FF,$03,$58,$B5,$FF,$01,$55,$B8,$FF,
  $00,$54,$AD,$FF,$00,$51,$AC,$FF,$01,$52,$AE,$FF,$00,$57,$AF,$FF,$07,$51,$B0,$FF,
  $00,$4E,$B3,$FF,$00,$60,$C6,$FF,$03,$74,$D4,$FF,$00,$71,$DB,$FF,$00,$6F,$CF,$FF,
  $01,$56,$B0,$FF,$06,$53,$A3,$FF,$00,$51,$AC,$FF,$03,$52,$AF,$FF,$07,$51,$A6,$FF,
  $00,$55,$A5,$FF,$00,$73,$DD,$FF,$11,$65,$AE,$FF,$31,$40,$19,$FF,$63,$7D,$00,$FF,
  $88,$BA,$01,$FF,$87,$AD,$00,$FF,$8E,$AD,$00,$FF,$8B,$B0,$01,$FF,$8A,$A7,$00,$FF,
  $91,$AF,$01,$FF,$8E,$B4,$00,$FF,$A7,$D1,$03,$FF,$6B,$A1,$31,$FF,$25,$62,$5D,$FF,
  $06,$56,$AD,$FF,$06,$54,$A9,$FF,$01,$55,$AD,$FF,$01,$51,$AA,$FF,$01,$56,$B3,$FF,
  $00,$73,$FE,$FF,$0C,$86,$FF,$FF,$24,$43,$31,$FF,$22,$23,$01,$FF,$78,$8D,$00,$FF,
  $91,$AC,$00,$FF,$90,$AD,$05,$FF,$89,$B1,$01,$FF,$82,$A9,$00,$FF,$89,$B0,$00,$FF,
  $8F,$A9,$08,$FF,$90,$AD,$00,$FF,$A8,$D8,$08,$FF,$6E,$97,$5D,$FF,$16,$4B,$75,$FF,
  $02,$38,$74,$FF,$00,$44,$8D,$FF,$00,$48,$9E,$FF,$04,$4C,$B0,$FF,$09,$52,$B1,$FF,
  $03,$55,$AB,$FF,$00,$51,$AB,$FF,$02,$52,$AB,$FF,$04,$56,$AC,$FF,$04,$53,$B2,$FF,
  $00,$53,$AF,$FF,$00,$4F,$A9,$FF,$05,$50,$AD,$FF,$06,$51,$B6,$FF,$00,$52,$AC,$FF,
  $00,$52,$A2,$FF,$00,$55,$A6,$FF,$04,$54,$A9,$FF,$00,$52,$A5,$FF,$00,$57,$B0,$FF,
  $01,$4F,$B4,$FF,$02,$52,$A9,$FF,$00,$51,$A3,$FF,$07,$4F,$B1,$FF,$00,$55,$AD,$FF,
  $05,$55,$B6,$FF,$00,$51,$AC,$FF,$01,$50,$AF,$FF,$05,$50,$AB,$FF,$00,$52,$B3,$FF,
  $00,$6B,$E8,$FF,$1D,$7B,$B8,$FF,$1B,$23,$0E,$FF,$4B,$54,$03,$FF,$85,$AE,$00,$FF,
  $8F,$AF,$02,$FF,$8E,$AB,$00,$FF,$89,$B0,$00,$FF,$8F,$AA,$03,$FF,$89,$AB,$00,$FF,
  $95,$B3,$03,$FF,$B1,$E6,$00,$FF,$4A,$79,$73,$FF,$0E,$41,$6C,$FF,$03,$53,$AA,$FF,
  $00,$51,$B1,$FF,$00,$56,$A9,$FF,$06,$4F,$AD,$FF,$05,$52,$AE,$FF,$02,$6C,$D0,$FF,
  $00,$78,$FB,$FF,$39,$6B,$6A,$FF,$1C,$20,$05,$FF,$66,$7B,$02,$FF,$90,$AA,$09,$FF,
  $85,$AC,$00,$FF,$8B,$A7,$00,$FF,$95,$B3,$05,$FF,$89,$A8,$02,$FF,$8C,$AF,$00,$FF,
  $91,$B8,$00,$FF,$91,$BC,$24,$FF,$45,$71,$62,$FF,$0A,$39,$4B,$FF,$00,$3B,$79,$FF,
  $00,$4B,$9F,$FF,$00,$52,$AB,$FF,$00,$59,$AC,$FF,$00,$53,$B0,$FF,$00,$52,$AC,$FF,
  $00,$53,$A4,$FF,$00,$52,$B8,$FF,$02,$4C,$AD,$FF,$05,$50,$AB,$FF,$00,$51,$A8,$FF,
  $00,$5B,$AC,$FF,$04,$4F,$B5,$FF,$04,$4D,$AB,$FF,$04,$50,$AE,$FF,$08,$51,$B0,$FF,
  $05,$4E,$AD,$FF,$00,$51,$B1,$FF,$00,$51,$AF,$FF,$02,$4C,$A5,$FF,$07,$56,$A5,$FF,
  $00,$50,$A5,$FF,$06,$56,$AD,$FF,$00,$52,$AC,$FF,$00,$50,$B1,$FF,$04,$54,$A9,$FF,
  $01,$54,$B2,$FF,$00,$52,$AD,$FF,$03,$52,$AF,$FF,$07,$53,$A9,$FF,$00,$68,$D7,$FF,
  $15,$8E,$F9,$FF,$19,$3F,$42,$FF,$31,$31,$00,$FF,$84,$9F,$04,$FF,$87,$A8,$00,$FF,
  $92,$AB,$07,$FF,$89,$AC,$00,$FF,$91,$AE,$00,$FF,$88,$AC,$02,$FF,$9A,$B9,$03,$FF,
  $A4,$D2,$27,$FF,$1B,$53,$62,$FF,$00,$34,$67,$FF,$00,$5A,$A0,$FF,$00,$52,$AF,$FF,
  $00,$57,$A7,$FF,$05,$4D,$B9,$FF,$00,$50,$AC,$FF,$00,$66,$CB,$FF,$08,$86,$F4,$FF,
  $48,$8E,$96,$FF,$1D,$29,$13,$FF,$55,$71,$02,$FF,$8C,$AA,$00,$FF,$8C,$B0,$06,$FF,
  $8F,$AD,$00,$FF,$89,$A9,$00,$FF,$8F,$AD,$00,$FF,$8A,$B6,$00,$FF,$94,$BB,$24,$FF,
  $7B,$A7,$48,$FF,$2B,$65,$67,$FF,$11,$42,$60,$FF,$00,$42,$8A,$FF,$09,$52,$A1,$FF,
  $03,$4D,$AC,$FF,$01,$51,$A8,$FF,$01,$52,$AD,$FF,$03,$53,$B6,$FF,$00,$52,$A6,$FF,
  $02,$52,$AB,$FF,$00,$57,$AF,$FF,$04,$54,$A9,$FF,$04,$51,$AD,$FF,$00,$4E,$AB,$FF,
  $03,$53,$AA,$FF,$00,$57,$A9,$FF,$00,$54,$AC,$FF,$00,$51,$AA,$FF,$00,$51,$AF,$FF,
  $08,$53,$AE,$FF,$00,$55,$A8,$FF,$00,$56,$B0,$FF,$00,$53,$AE,$FF,$00,$4E,$B9,$FF,
  $00,$50,$AF,$FF,$01,$53,$A9,$FF,$03,$54,$A6,$FF,$06,$4E,$B4,$FF,$00,$4F,$A4,$FF,
  $01,$56,$B1,$FF,$00,$53,$AE,$FF,$03,$4F,$A5,$FF,$01,$6D,$DC,$FF,$07,$82,$F9,$FF,
  $2F,$7F,$98,$FF,$3B,$46,$02,$FF,$6E,$83,$00,$FF,$88,$B0,$02,$FF,$8D,$A8,$01,$FF,
  $8E,$AE,$05,$FF,$8F,$AB,$00,$FF,$8C,$AE,$02,$FF,$AD,$DA,$03,$FF,$94,$B9,$1C,$FF,
  $17,$3C,$73,$FF,$05,$34,$7A,$FF,$03,$52,$A1,$FF,$0B,$4C,$B0,$FF,$00,$54,$AE,$FF,
  $00,$53,$A6,$FF );

{ PERFORM_RENDERING }
procedure the_application.perform_rendering;
var
 pf  : pixel_formats;
 rb  : renderer_base;
 rs  : renderer_scanline_aa_solid;
 ren : renderer_scanline_aa;

 rgba : aggclr;
 path : path_storage;

 x1 ,y1 ,x2 ,y2 ,scale ,d1 ,d2 : double;

 ide ,mtx : trans_affine;

 tas : trans_affine_scaling;
 
 tat ,gmt : trans_affine_translation;

 t1 ,t2 : conv_transform;

 gimag : gradient_image;
 gcont : ^gradient_contour;
 gcona : gradient_conic_angle;

 al : span_allocator;
 sg : span_gradient;

 interpolator : span_interpolator_linear;

 gfunction : gradient_ptr;
 cfunction : array_base_ptr;

 gr_ref : gradient_reflect_adaptor;
 stroke : conv_stroke;
 bitmap : pointer;

begin
 pixfmt_bgr24(pf ,rbuf_window );

 rb.Construct(@pf );
 rs.Construct(@rb );

 gcont:=NIL;

 if bounding_rect_single(vs ,0 ,@x1 ,@y1 ,@x2 ,@y2 ) then
  begin
  { Create Path }
   scale:=(_width - 120 ) / (x2 - x1 );

   if scale > (_height - 120 ) / (y2 - y1 ) then
    scale:=(_height - 120 ) / (y2 - y1 );

   mtx.Construct;
   tat.Construct(-x1 ,-y1 );     mtx.multiply(@tat );
   tas.Construct(scale ,scale ); mtx.multiply(@tas );
   t1.Construct (vs ,@mtx );

   tat.Construct(100 ,105 );
   t2.Construct (@t1 ,@tat );

   path.Construct;
   path.add_path(@t1 );

  { Color function }
   case trunc(m_clrs._value ) of
    2 :
     cfunction:=@m_colors02;

    3 :
     cfunction:=@m_colors03;

    4 :
     cfunction:=@m_colors04;

    5 :
     cfunction:=@m_colors05;

    6 :
     cfunction:=@m_colors06;

    7 :
     cfunction:=@m_colors07;

    8 :
     cfunction:=@m_colors08;

    9 :
     cfunction:=@m_colors09;

    10 :
     cfunction:=@m_colors10;

    else
     cfunction:=@m_colors11;

   end;

  { Create Gradient Function }
   gfunction:=NIL;

   gmt.Construct(0 ,0 );

   d1:=0;
   d2:=100;

   case m_gradient._cur_item of
   // contour
    0 ,1 :
     begin
      new(gcont ,Construct );

      gcont.frame(0 );
      gcont.d1(m_c1._value );
      gcont.d2(m_c2._value );

      gmt.multiply(@tat );

      case m_gradient._cur_item of
      // contour
       0 :
        if gcont.contour_create(contour ) <> NIL then
         gfunction:=gcont;

      // auto contour
       else
        if gcont.contour_create(@path ) <> NIL then
         gfunction:=gcont;

      end;

      d1:=m_d1._value;
      d2:=m_d2._value;

     end;

   // assymetric conic
    2 :
     begin
      gcona.Construct;

      gfunction:=@gcona;

      gmt.Construct(270 ,300 );

     end;
     
   end;

  { Render }
   if gfunction <> NIL then
    begin
     ide.Construct;
     ide.multiply(@gmt );
     ide.invert;

     interpolator.Construct(@ide );

     if m_refl._status then
      begin
       gr_ref.Construct(gfunction );

       gfunction:=@gr_ref;

      end;

     al.Construct;
     sg.Construct(
      @al ,
      @interpolator ,
      gfunction ,
      cfunction ,
      d1 ,d2 );

     ren.Construct(@rb ,@sg );

     m_ras.reset;
     m_ras.add_path  (@t2 );
     render_scanlines(@m_ras ,@m_sl ,@ren ); {}

     al.Destruct;

    end
   else
    begin
     m_ras.reset;
     m_ras.add_path  (@t2 );
     rgba.ConstrDbl  (0 ,0.6 ,0 ,0.5 );
     rs.color_       (@rgba );
     render_scanlines(@m_ras ,@m_sl ,@rs );

    end;

  { Stroke }
   if m_stroke._status then
    begin
     gimag.Construct;

     bitmap:=gimag.image_create(64 ,64 );

     if bitmap <> NIL then
      begin
       move(puzzle[0 ] ,bitmap^ ,64 * 64 * 4 );

       stroke.Construct(@t2 );
       stroke.width_   (10.0 );

       ide.Construct;

       case m_gradient._cur_item of
        0 ,1 :
         begin
          ide.multiply(@mtx );
          ide.multiply(@gmt );

         end; 

       end;

       ide.invert;

       interpolator.Construct(@ide );

       al.Construct;
       sg.Construct(
        @al ,
        @interpolator ,
        @gimag ,
        gimag.color_function ,
        d1 ,d2 );

       ren.Construct(@rb ,@sg );

       m_ras.reset;
       m_ras.add_path  (@stroke );
       render_scanlines(@m_ras ,@m_sl ,@ren );

       al.Destruct;

       stroke.Destruct;

      end;

     gimag.Destruct;

    end;

  { Free }
   path.Destruct;

   if gcont <> NIL then
    dispose(gcont ,Destruct );

  end;

end;

{ RENDER }
function the_application.render;
var
 star   ,
 gb_poly ,
 glyph   : path_storage;

 stroke : conv_stroke;
 curve  : conv_curve;

 sp : spiral;

begin
 star.Construct;

 star.move_to(12 ,40 );
 star.line_to(52 ,40 );
 star.line_to(72 ,6 );
 star.line_to(92 ,40 );
 star.line_to(132 ,40 );
 star.line_to(112 ,76 );
 star.line_to(132 ,112 );
 star.line_to(92 ,112 );
 star.line_to(72 ,148 );
 star.line_to(52 ,112 );
 star.line_to(12 ,112 );
 star.line_to(32 ,76 );
 star.close_polygon;

 case m_polygons._cur_item of
  0 : // Simple path
   perform_rendering(@star ,@star );

  1 : // Great Britain
   begin
    gb_poly.Construct;

    make_gb_poly(@gb_poly );

    perform_rendering(@gb_poly ,@star );

    gb_poly.Destruct;

   end;

  2 : // Spiral
   begin
    sp.Construct    (0 ,0 ,10 ,150 ,30 ,0.0 );
    stroke.Construct(@sp );
    stroke.width_   (22.0 );

    perform_rendering(@stroke ,@star );

    stroke.Destruct;

   end;

  3 : // Glyph
   begin
    glyph.Construct;
    glyph.move_to(28.47 ,6.45 );
    glyph.curve3 (21.58 ,1.12  ,19.82 ,0.29 );
    glyph.curve3 (17.19 ,-0.93 ,14.21 ,-0.93 );
    glyph.curve3 (9.57  ,-0.93 ,6.57  ,2.25 );
    glyph.curve3 (3.56  ,5.42  ,3.56  ,10.60 );
    glyph.curve3 (3.56  ,13.87 ,5.03  ,16.26 );
    glyph.curve3 (7.03  ,19.58 ,11.99 ,22.51 );
    glyph.curve3 (16.94 ,25.44 ,28.47 ,29.64 );
    glyph.line_to(28.47 ,31.40 );
    glyph.curve3 (28.47 ,38.09 ,26.34 ,40.58 );
    glyph.curve3 (24.22 ,43.07 ,20.17 ,43.07 );
    glyph.curve3 (17.09 ,43.07 ,15.28 ,41.41 );
    glyph.curve3 (13.43 ,39.75 ,13.43 ,37.60 );
    glyph.line_to(13.53 ,34.77 );
    glyph.curve3 (13.53 ,32.52 ,12.38 ,31.30 );
    glyph.curve3 (11.23 ,30.08 ,9.38  ,30.08 );
    glyph.curve3 (7.57  ,30.08 ,6.42  ,31.35 );
    glyph.curve3 (5.27  ,32.62 ,5.27  ,34.81 );
    glyph.curve3 (5.27  ,39.01 ,9.57  ,42.53 );
    glyph.curve3 (13.87 ,46.04 ,21.63 ,46.04 );
    glyph.curve3 (27.59 ,46.04 ,31.40 ,44.04 );
    glyph.curve3 (34.28 ,42.53 ,35.64 ,39.31 );
    glyph.curve3 (36.52 ,37.21 ,36.52 ,30.71 );
    glyph.line_to(36.52 ,15.53 );
    glyph.curve3 (36.52 ,9.13  ,36.77 ,7.69 );
    glyph.curve3 (37.01 ,6.25  ,37.57 ,5.76 );
    glyph.curve3 (38.13 ,5.27  ,38.87 ,5.27 );
    glyph.curve3 (39.65 ,5.27  ,40.23 ,5.62 );
    glyph.curve3 (41.26 ,6.25  ,44.19 ,9.18 );
    glyph.line_to(44.19 ,6.45 );
    glyph.curve3 (38.72 ,-0.88 ,33.74 ,-0.88 );
    glyph.curve3 (31.35 ,-0.88 ,29.93 ,0.78 );
    glyph.curve3 (28.52 ,2.44  ,28.47 ,6.45 );
    glyph.close_polygon;

    glyph.move_to(28.47 ,9.62 );
    glyph.line_to(28.47 ,26.66 );
    glyph.curve3 (21.09 ,23.73 ,18.95 ,22.51 );
    glyph.curve3 (15.09 ,20.36 ,13.43 ,18.02 );
    glyph.curve3 (11.77 ,15.67 ,11.77 ,12.89 );
    glyph.curve3 (11.77 ,9.38  ,13.87 ,7.06 );
    glyph.curve3 (15.97 ,4.74  ,18.70 ,4.74 );
    glyph.curve3 (22.41 ,4.74  ,28.47 ,9.62 );
    glyph.close_polygon;

    curve.Construct(@glyph );

    curve.approximation_scale_(10 );

    perform_rendering(@curve ,@star );

    glyph.Destruct;
    curve.Destruct;

   end;

 end;

 star.Destruct;

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pf : pixel_formats;

 rgba : aggclr;

 ren_base  : renderer_base;
 ren_solid : renderer_scanline_aa_solid;

begin
// Initialize structures
 pixfmt_bgr24(pf ,rbuf_window );

 ren_base.Construct (@pf );
 ren_solid.Construct(@ren_base );

 rgba.ConstrDbl(1 ,1 ,1 );
 ren_base.clear(@rgba );

// Render
 render;

// Render the controls
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_polygons );
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_gradient );
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_stroke );
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_refl );
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_c1 );
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_c2 );
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_d1 );
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_d2 );
 render_ctrl(@m_ras ,@m_sl ,@ren_solid ,@m_clrs );

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'Here are some more gradient functions. Firstly the Contour gradient, makes'#13 +
   'color transitions from shape defined by an arbitrary (in fact any) path.'#13 +
   'It computes so called Distance Transform (DT) from image produced by only'#13 +
   'stroking the path, which is then the source of color level in the underlying'#13 +
   'gradient function. Contour gradient can be used in two forms. One is to define   '#13 +
   'shape for contour different from shape of object being drawn. Second is to'#13 +
   'use the same shape for contour and for drawing (AutoContour).'#13#13 +

   'Assymetric conic gradient (also called angle) is the same as conic, but the ray'#13 +
   'of light with color transitions going counter clockwise travels whole circle'#13 +
   'instead of just half (as with conic).'#13#13 +

   'Bitmap gradient is very similar to pattern fill, but works in the framework of'#13 +
   'gradient functions interfaces. Because of that all interpolator transformations'#13 +
   'from gradient span generator can be applied to this kind of fill.'#13#13 +

   'How to play with:'#13#13 +

   'Try to fiddle with C1,C2 & D1,D2 parameters to see, how interestingly'#13 +
   'the gradient transitions changes (each time a new DT is computed).'#13 +
   'DT is reused when the color ramp changes.'#13#13 +

   'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ ('AGG Example. More gradients: Contour, Bitmap & Assymetric Conic (F1-Help)' );

 if app.init(520 ,520 ,window_resize ) then
  app.run;

 app.Destruct;

END.