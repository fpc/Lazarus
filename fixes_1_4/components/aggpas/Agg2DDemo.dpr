{mac_copy:spheres2.bmp}
//
// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo
//
// Paths: src;src\ctrl;src\svg;src\util;src\platform\win;expat-wrap
//
program
 Agg2DDemo ;

uses
 SysUtils ,
 agg_basics ,
 agg_platform_support ,
 file_utils_ ,
 agg_2D ;

{$I agg_mode.inc }

const
 flip_y     = true;
 angle_step = 5;
 gamma_step = 0.1;

var
 FONT_TIMES : AnsiString = 'Times New Roman';
 FONT_ARIAL : AnsiString = 'Arial';
 FONT_VERDA : AnsiString = 'Verdana';

type
 the_application = object(platform_support )
   m_graphics ,
   m_timer    : Agg2D;
   m_angle    ,
   m_gamma    : double;
   m_image    : int;
   m_gmText   : AnsiString;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_mouse_move(x ,y : int; flags : unsigned ); virtual;

   procedure on_mouse_button_down(x ,y : int; flags : unsigned ); virtual;
   procedure on_mouse_button_up  (x ,y : int; flags : unsigned ); virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ CONSTRUCT }
constructor the_application.Construct;
begin
 inherited Construct(format_ ,flip_y_ );

 m_graphics.Construct;
 m_timer.Construct;

 m_angle:=0;
 m_gamma:=1.4;
 m_image:=6;

 Str(m_gamma:0:2 ,m_gmText ); m_gmText:='Gamma: ' + m_gmText;

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 inherited Destruct;

 m_graphics.Destruct;
 m_timer.Destruct;

 Finalize(m_gmText );

end;

{ ON_INIT }
procedure the_application.on_init;
begin
end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 xb1 ,yb1 ,xb2 ,yb2 : double;

 clr ,c1 ,c2 ,c3 : Color;

 img : Image;

 parl : array[0..5 ] of double;
 poly : array[0..11 ] of double;

 tm  : double;
 fps : int;

 timer ,rate : AnsiString;

begin
 start_timer;

 m_graphics.attach(
  rbuf_window._buf ,
  rbuf_window._width ,
  rbuf_window._height ,
  rbuf_window._stride );

 m_graphics.clearAll(255 ,255 ,255 );

 //m_graphics.clearAll(0 ,0 ,0 );

 //m_graphics.blendMode(BlendSub );
 //m_graphics.blendMode(BlendAdd );

 m_graphics.antiAliasGamma(m_gamma );

// Set flipText(true) if you have the Y axis upside down.
 m_graphics.flipText(not flip_y );

// ClipBox.
 //m_graphics.clipBox(50 ,50 ,rbuf_window._width - 50 ,rbuf_window._height - 50 );

// Transfornations - Rotate around (300,300) to 5 degree
 m_graphics.translate(-300 ,-300 );
 m_graphics.rotate   (deg2Rad(m_angle ) );
 m_graphics.translate(300 ,300 );
 //m_graphics.skew     (0.1 ,0.1 );

// Viewport - set 0,0,600,600 to the actual window size
// preserving aspect ratio and placing the viewport in the center.
// To ignore aspect ratio use Agg2D::Anisotropic
// Note that the viewport just adds transformations to the current
// affine matrix. So that, set the viewport *after* all transformations!
 m_graphics.viewport(
  0 ,0 ,600 ,600 ,
  0 ,0 ,_width ,_height ,
  //Anisotropic );
  XMidYMid );
  //XMinYMin );
  //XMidYMin );
  //XMaxYMin );
  //XMinYMid );
  //XMaxYMid );
  //XMinYMax );
  //XMidYMax );
  //XMaxYMax );

// Rounded Rect
 m_graphics.lineColor(0 ,0 ,0 );
 m_graphics.noFill;
 m_graphics.roundedRect(0.5 ,0.5 ,600 - 0.5 ,600 - 0.5 ,20.0 );

// Regular Text
{ m_graphics.font(char_ptr(FONT_TIMES ) ,14.0 ,false ,false );
 m_graphics.fillColor(0 ,0 ,0 );
 m_graphics.noLine;
 m_graphics.text(100 ,20 ,char_ptr(PChar('Regular Raster Text -- Fast, but can''t be rotated' ) ) );{}

// Outlined Text
 m_graphics.font(char_ptr(FONT_TIMES ) ,50.0 ,false ,false ,{RasterFontCache}VectorFontCache );
 m_graphics.lineColor(50 ,0 ,0 );
 m_graphics.fillColor(180 ,200 ,100 );
 m_graphics.lineWidth(1.0 );
 m_graphics.text(100.5 ,50.5 ,char_ptr(PChar('Outlined Text' ) ) );

// Gamma Text
(* m_graphics.font(char_ptr(FONT_ARIAL ) ,38.0 ,true ,true ,VectorFontCache );

 c1.Construct                 (255 ,0   ,0   ,255 );
 c2.Construct                 (0   ,255 ,0   ,255 );
 m_graphics.fillLinearGradient(50 ,1 ,300 ,10 ,c1 ,c2 );

 m_graphics.noLine;
 m_graphics.text(12.5 ,565.5 ,char_ptr(@m_gmText[1 ] ) );
 //m_graphics.rectangle(12.5 ,565.5 ,290 ,590 ); (**)

// Text Alignment
 m_graphics.lineColor(0 ,0 ,0 );
 m_graphics.line(250.5 - 150 ,150.5      ,250.5 + 150 ,150.5      );
 m_graphics.line(250.5       ,150.5 - 20 ,250.5       ,150.5 + 20 );
 m_graphics.line(250.5 - 150 ,200.5      ,250.5 + 150 ,200.5      );
 m_graphics.line(250.5       ,200.5 - 20 ,250.5       ,200.5 + 20 );
 m_graphics.line(250.5 - 150 ,250.5      ,250.5 + 150 ,250.5      );
 m_graphics.line(250.5       ,250.5 - 20 ,250.5       ,250.5 + 20 );
 m_graphics.line(250.5 - 150 ,300.5      ,250.5 + 150 ,300.5      );
 m_graphics.line(250.5       ,300.5 - 20 ,250.5       ,300.5 + 20 );
 m_graphics.line(250.5 - 150 ,350.5      ,250.5 + 150 ,350.5      );
 m_graphics.line(250.5       ,350.5 - 20 ,250.5       ,350.5 + 20 );
 m_graphics.line(250.5 - 150 ,400.5      ,250.5 + 150 ,400.5      );
 m_graphics.line(250.5       ,400.5 - 20 ,250.5       ,400.5 + 20 );
 m_graphics.line(250.5 - 150 ,450.5      ,250.5 + 150 ,450.5      );
 m_graphics.line(250.5       ,450.5 - 20 ,250.5       ,450.5 + 20 );
 m_graphics.line(250.5 - 150 ,500.5      ,250.5 + 150 ,500.5      );
 m_graphics.line(250.5       ,500.5 - 20 ,250.5       ,500.5 + 20 );
 m_graphics.line(250.5 - 150 ,550.5      ,250.5 + 150 ,550.5      );
 m_graphics.line(250.5       ,550.5 - 20 ,250.5       ,550.5 + 20 );

 m_graphics.fillColor(100 ,50 ,50 );
 m_graphics.noLine;
 //m_graphics.textHints(false );
 m_graphics.font(char_ptr(FONT_TIMES ) ,40.0 ,false ,false ,VectorFontCache );

 m_graphics.textAlignment(AlignLeft ,AlignBottom );
 m_graphics.text(250.0 ,150.0 ,char_ptr(PChar('Left-Bottom' ) ) ,true ,0 ,0 );

 m_graphics.textAlignment(AlignCenter ,AlignBottom );
 m_graphics.text(250.0 ,200.0 ,char_ptr(PChar('Center-Bottom' ) ) ,true ,0 ,0 );

 m_graphics.textAlignment(AlignRight ,AlignBottom );
 m_graphics.text(250.0 ,250.0 ,char_ptr(PChar('Right-Bottom' ) ) ,true ,0 ,0 );

 m_graphics.textAlignment(AlignLeft ,AlignCenter );
 m_graphics.text(250.0 ,300.0 ,char_ptr(PChar('Left-Center' ) ) ,true ,0 ,0 );

 m_graphics.textAlignment(AlignCenter ,AlignCenter );
 m_graphics.text(250.0 ,350.0 ,char_ptr(PChar('Center-Center' ) ) ,true ,0 ,0 );

 m_graphics.textAlignment(AlignRight ,AlignCenter );
 m_graphics.text(250.0 ,400.0 ,char_ptr(PChar('Right-Center' ) ) ,true ,0 ,0 );

 m_graphics.textAlignment(AlignLeft ,AlignTop );
 m_graphics.text(250.0 ,450.0 ,char_ptr(PChar('Left-Top' ) ) ,true ,0 ,0 );

 m_graphics.textAlignment(AlignCenter ,AlignTop );
 m_graphics.text(250.0 ,500.0 ,char_ptr(PChar('Center-Top' ) ) ,true ,0 ,0 );

 m_graphics.textAlignment(AlignRight ,AlignTop );
 m_graphics.text(250.0 ,550.0 ,char_ptr(PChar('Right-Top' ) ) ,true ,0 ,0 );

// Gradients (Aqua Buttons)
//=======================================
 m_graphics.font(char_ptr(FONT_VERDA ) ,20.0 ,false ,false ,VectorFontCache );

 xb1:=400;
 yb1:=80;
 xb2:=xb1 + 150;
 yb2:=yb1 + 36;

 clr.Construct         (0 ,50 ,180 ,180 );
 m_graphics.fillColor  (clr );
 clr.Construct         (0 ,0 ,80 ,255 );
 m_graphics.lineColor  (clr );
 m_graphics.lineWidth  (1.0);
 m_graphics.roundedRect(xb1 ,yb1 ,xb2 ,yb2 ,12 ,18 );

 clr.Construct       (0 ,0 ,0 ,0 );
 m_graphics.lineColor(clr );

 c1.Construct                 (100 ,200 ,255 ,255 );
 c2.Construct                 (255 ,255 ,255 ,0 );
 m_graphics.fillLinearGradient(xb1 ,yb1 ,xb1 ,yb1 + 30 ,c1 ,c2 );
 m_graphics.roundedRect       (xb1 + 3 ,yb1 + 2.5 ,xb2 - 3 ,yb1 + 30 ,9 ,18 ,1 ,1 );

 clr.Construct       (0 ,0 ,50 ,200 );
 m_graphics.fillColor(clr );
 m_graphics.noLine;
 m_graphics.textAlignment(AlignCenter ,AlignCenter );
 m_graphics.text(
  (xb1 + xb2 ) / 2.0 ,
  (yb1 + yb2 ) / 2.0 ,
  char_ptr(PChar('Aqua Button' ) ) ,
  true ,0.0 ,0.0 );

 c1.Construct(0   ,0   ,255 ,0 );
 c2.Construct(100 ,255 ,255 ,255 );
 m_graphics.fillLinearGradient(xb1 ,yb2 - 20 ,xb1 ,yb2 - 3 ,c1 ,c2 );
 m_graphics.roundedRect       (xb1 + 3 ,yb2 - 20 ,xb2 - 3 ,yb2 - 2 ,1 ,1 ,9 ,18 );

// Aqua Button Pressed
 xb1:=400;
 yb1:=30;
 xb2:=xb1 + 150;
 yb2:=yb1 + 36;

 clr.Construct         (0 ,50 ,180 ,180 );
 m_graphics.fillColor  (clr );
 clr.Construct         (0 ,0 ,0 ,255 );
 m_graphics.lineColor  (clr );
 m_graphics.lineWidth  (2.0 );
 m_graphics.roundedRect(xb1 ,yb1 ,xb2 ,yb2 ,12 ,18 );

 clr.Construct       (0 ,0 ,0 ,0 );
 m_graphics.lineColor(clr );

 c1.Construct                 (60  ,160 ,255 ,255 );
 c2.Construct                 (100 ,255 ,255 ,0 );
 m_graphics.fillLinearGradient(xb1 ,yb1 + 2 ,xb1 ,yb1 + 25 ,c1 ,c2 );
 m_graphics.roundedRect       (xb1 + 3 ,yb1 + 2.5 ,xb2 - 3 ,yb1 + 30 ,9 ,18 ,1 ,1 );

 clr.Construct       (0 ,0 ,50 ,255 );
 m_graphics.fillColor(clr );
 m_graphics.noLine;
 m_graphics.textAlignment(AlignCenter ,AlignCenter );
 m_graphics.text(
  (xb1 + xb2 ) / 2.0 ,
  (yb1 + yb2 ) / 2.0 ,
  char_ptr(PChar('Aqua Pressed' ) ) ,
  false ,0.0 );

 c1.Construct                 (0 ,180 ,255 ,0 );
 c2.Construct                 (0 ,200 ,255 ,255 );
 m_graphics.fillLinearGradient(xb1 ,yb2 - 25 ,xb1 ,yb2 - 5 ,c1 ,c2 );
 m_graphics.roundedRect       (xb1 + 3 ,yb2 - 25 ,xb2 - 3 ,yb2 - 2 ,1 ,1 ,9 ,18 );

// Basic Shapes -- Ellipse
//===========================================
 m_graphics.lineWidth(3.5 );
 m_graphics.lineColor(20  ,80  ,80 );
 m_graphics.fillColor(200 ,255 ,80 ,200 );
 m_graphics.ellipse  (450 ,200 ,50 ,90 );

// Paths
//===========================================
 m_graphics.resetPath;
 m_graphics.fillColor (255 ,0 ,0   ,100 );
 m_graphics.lineColor (0   ,0 ,255 ,100 );
 m_graphics.lineWidth (2 );
 m_graphics.moveTo    (300 / 2 ,200 / 2 );
 m_graphics.horLineRel(-150 / 2 );
 m_graphics.arcRel    (150 / 2 ,150 / 2 ,0 ,true ,false ,150 / 2 ,-150 / 2 );
 m_graphics.closePolygon;
 m_graphics.drawPath;

 m_graphics.resetPath;
 m_graphics.fillColor (255 ,255 ,0   ,100 );
 m_graphics.lineColor (0   ,0   ,255 ,100 );
 m_graphics.lineWidth (2 );
 m_graphics.moveTo    (275 / 2 ,175 / 2 );
 m_graphics.verLineRel(-150 / 2 );
 m_graphics.arcRel    (150 / 2 ,150 / 2 ,0 ,false ,false ,-150 / 2 ,150 / 2 );
 m_graphics.closePolygon;
 m_graphics.drawPath;

 m_graphics.resetPath;
 m_graphics.noFill;
 m_graphics.lineColor(127 ,0 ,0 );
 m_graphics.lineWidth(5 );
 m_graphics.moveTo   (600 / 2 ,350 / 2 );
 m_graphics.lineRel  (50 / 2  ,-25 / 2 );
 m_graphics.arcRel   (25 / 2  ,25 / 2 ,deg2Rad(-30 ) ,false ,true ,50 / 2 ,-25 / 2 );
 m_graphics.lineRel  (50 / 2  ,-25 / 2);
 m_graphics.arcRel   (25 / 2  ,50 / 2 ,deg2Rad(-30 ) ,false ,true ,50 / 2 ,-25 / 2 );
 m_graphics.lineRel  (50 / 2  ,-25 / 2 );
 m_graphics.arcRel   (25 / 2  ,75 / 2 ,deg2Rad(-30 ) ,false ,true ,50 / 2 ,-25 / 2 );
 m_graphics.lineRel  (50 ,-25 );
 m_graphics.arcRel   (25 / 2 ,100 / 2 ,deg2Rad(-30 ) ,false ,true ,50 / 2 ,-25 / 2 );
 m_graphics.lineRel  (50 / 2 ,-25 / 2 );
 m_graphics.drawPath;

// Master Alpha. From now on everything will be translucent
//===========================================
 m_graphics.masterAlpha(0.85 );

// Image Transformations
//===========================================
 img.Construct(
  rbuf_img(0 )._buf ,
  rbuf_img(0 )._width ,
  rbuf_img(0 )._height ,
  rbuf_img(0 )._stride );

 m_graphics.imageFilter(Bilinear );

 //m_graphics.imageResample(NoResample );
 //m_graphics.imageResample(ResampleAlways );
 m_graphics.imageResample(ResampleOnZoomOut );

// Set the initial image blending operation as BlendDst, that actually
// does nothing.
//-----------------
 m_graphics.imageBlendMode(BlendDst );

// Transform the whole image to the destination rectangle
//-----------------
 if m_image = 1 then
  m_graphics.transformImage(@img ,450 ,200 ,595 ,350 );{1}

// Transform the rectangular part of the image to the destination rectangle
//-----------------
 if m_image = 2 then
  m_graphics.transformImage(
   @img ,60 ,60 ,img.width - 60 ,img.height - 60 ,
   450 ,200 ,595 ,350 );{2}

// Transform the whole image to the destination parallelogram
//-----------------
 if m_image = 3 then
  begin
   parl[0 ]:=450;
   parl[1 ]:=200;
   parl[2 ]:=595;
   parl[3 ]:=220;
   parl[4 ]:=575;
   parl[5 ]:=350;

   m_graphics.transformImage(@img ,@parl[0 ] );{3}

  end;

// Transform the rectangular part of the image to the destination parallelogram
//-----------------
 if m_image = 4 then
  begin
   parl[0 ]:=450;
   parl[1 ]:=200;
   parl[2 ]:=595;
   parl[3 ]:=220;
   parl[4 ]:=575;
   parl[5 ]:=350;

   m_graphics.transformImage(@img ,60 ,60 ,img.width - 60 ,img.height - 60 ,@parl[0 ] );{4}

  end;

// Transform image to the destination path. The scale is determined by a rectangle
//-----------------
 if m_image = 5 then
  begin
   m_graphics.resetPath;
   m_graphics.moveTo            (450 ,200 );
   m_graphics.cubicCurveTo      (595 ,220 ,575 ,350 ,595 ,350 );
   m_graphics.lineTo            (470 ,340 );
   m_graphics.transformImagePath(@img ,450 ,200 ,595 ,350 );{5}

  end;

// Transform image to the destination path.
// The scale is determined by a rectangle
//-----------------
 if m_image = 6 then
  begin
   m_graphics.resetPath;
   m_graphics.moveTo      (450 ,200 );
   m_graphics.cubicCurveTo(595 ,220 ,575 ,350 ,595 ,350 );
   m_graphics.lineTo      (470 ,340 );
   m_graphics.transformImagePath(
    @img ,60 ,60 ,img.width - 60 ,img.height - 60 ,
    450 ,200 ,595 ,350 );{6}

  end;

// Transform image to the destination path.
// The transformation is determined by a parallelogram
 if m_image = 7 then
  begin
   m_graphics.resetPath;
   m_graphics.moveTo      (450 ,200 );
   m_graphics.cubicCurveTo(595 ,220 ,575 ,350 ,595 ,350 );
   m_graphics.lineTo      (470 ,340 );

   parl[0 ]:=450;
   parl[1 ]:=200;
   parl[2 ]:=595;
   parl[3 ]:=220;
   parl[4 ]:=575;
   parl[5 ]:=350;

   m_graphics.transformImagePath(@img ,@parl[0 ] );{7}

  end;

// Transform the rectangular part of the image to the destination path.
// The transformation is determined by a parallelogram
 if m_image = 8 then
  begin
   m_graphics.resetPath;
   m_graphics.moveTo      (450 ,200 );
   m_graphics.cubicCurveTo(595 ,220 ,575 ,350 ,595 ,350 );
   m_graphics.lineTo      (470 ,340 );

   parl[0 ]:=450;
   parl[1 ]:=200;
   parl[2 ]:=595;
   parl[3 ]:=220;
   parl[4 ]:=575;
   parl[5 ]:=350;

   m_graphics.transformImagePath(@img ,60 ,60 ,img.width - 60 ,img.height - 60 ,@parl[0 ] );{8}

  end;

// Free Image
 img.Destruct;

// Add/Sub/Contrast Blending Modes
 m_graphics.noLine;
 m_graphics.fillColor(70 ,70 ,0 );
 m_graphics.blendMode(BlendAdd );
 m_graphics.ellipse  (500 ,280 ,20 ,40 );

 m_graphics.fillColor(255 ,255 ,255 );
 m_graphics.blendMode(BlendContrast );
 m_graphics.ellipse  (500 + 40 ,280 ,20 ,40 );

// Radial gradient.
 m_graphics.blendMode         (BlendAlpha );
 c1.Construct                 (255 ,255 ,0 , 0);
 c2.Construct                 (0   ,0   ,127 );
 c3.Construct                 (0   ,255 ,0 , 0);
 m_graphics.fillRadialGradient(400 ,500 ,40 ,c1 ,c2 ,c3 );
 m_graphics.ellipse           (400 ,500 ,40 ,40 );

// More ...
(* m_graphics.masterAlpha(1 );

 //m_graphics.lineColor(50 ,60 ,70 );

 c1.Construct                 (255 ,0   ,0 ,255 );
 c2.Construct                 (0   ,255 ,0   ,255 );
 m_graphics.lineLinearGradient(0 ,0 ,500 ,0 ,c1 ,c2 );{}

 m_graphics.fillColor(255 ,0 ,0 );
 m_graphics.lineJoin (JoinMiter );
 m_graphics.lineWidth(15 );
 m_graphics.triangle (10 ,10 ,100 ,20 ,50 ,150 );

 m_graphics.lineJoin (JoinRound );
 m_graphics.lineWidth(4 );
 m_graphics.noFill;
 m_graphics.rectangle(55 ,540 ,135 ,495 );

 m_graphics.masterAlpha(0.5 );

 m_graphics.fillColor(255 ,127 ,65 );
 m_graphics.star     (300 ,300 ,30 ,70 ,55 ,5 );
 m_graphics.arc      (400 ,400 ,30 ,30 ,300 ,1150 );

 m_graphics.lineWidth(20 );
 m_graphics.lineCap  (CapRound );
 m_graphics.curve    (80 ,400 ,90 ,220 ,190 ,390 );
 m_graphics.curve    (80 ,500 ,90 ,320 ,190 ,490 ,310 ,330 );

 poly[0 ]:=400;
 poly[1 ]:=580;

 poly[2 ]:=530;
 poly[3 ]:=400;

 poly[4 ]:=590;
 poly[5 ]:=500;

 poly[6 ]:=450;
 poly[7 ]:=380;

 poly[8 ]:=490;
 poly[9 ]:=570;

 poly[10 ]:=420;
 poly[11 ]:=420;

 m_graphics.fillEvenOdd(false );
 m_graphics.lineWidth  (3 );
 m_graphics.polygon    (@poly[0 ] ,6 );

 m_graphics.lineColor(221 ,160 ,221 );
 m_graphics.lineWidth(6 );
 m_graphics.polyline (@poly[0 ] ,6 ); (**)

// TIMER DRAW
// ----------
 tm:=elapsed_time;

 m_timer.attach(
  rbuf_window._buf ,
  rbuf_window._width ,
  rbuf_window._height ,
  rbuf_window._stride );

 m_timer.antiAliasGamma(1.4 );

 m_timer.flipText(not flip_y );
 m_timer.viewport(
  0 ,0 ,600 ,600 ,
  0 ,0 ,_width ,_height ,
  //Anisotropic );
  XMidYMid );

 Str(tm:0:2 ,timer );

 timer:='Frame time: ' + timer + ' ms';

 fps:=Trunc(1000 / tm );

 Str(fps ,rate );

 timer:=timer + ' (' + rate + ' FPS)';

 m_timer.font(char_ptr(FONT_ARIAL ) ,15.0 ,true ,false ,VectorFontCache );
 m_timer.noLine;
 m_timer.fillColor(255 ,0 ,0 );
 m_timer.text(350 ,8 ,char_ptr(@timer[1 ] ) );

end;

{ ON_MOUSE_MOVE }
procedure the_application.on_mouse_move;
begin
end;

{ ON_MOUSE_BUTTON_DOWN }
procedure the_application.on_mouse_button_down;
begin
end;

{ ON_MOUSE_BUTTON_UP }
procedure the_application.on_mouse_button_up;
begin
end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   '"Quick and dirty prototype" of 2D drawing API for AGG.'#13#13 +
   'Written and published by Maxim Shemanarev (c) 2005 - 2006.   '#13 +
   'Ported to Object Pascal by Milan Marusinec (c) 2007.'#13#13 +
   'How to play with:'#13#13 +
   'Key Down - Rotate clockwise'#13 +
   'Key Up - Rotate counterclockwise'#13 +
   'Key Right - Next image transformation'#13 +
   'Key Left - Previous image transformation'#13 +
   'Key Plus - Increase gamma'#13 +
   'Key Minus - Decrease gamma' );

 if key = key_down then
  begin
   m_angle:=m_angle - angle_step;

   if m_angle < 0 then
    m_angle:=360 - angle_step;

   force_redraw;

  end;

 if key = key_up then
  begin
   m_angle:=m_angle + angle_step;

   if m_angle > 360 then
    m_angle:=angle_step;

   force_redraw;

  end;

 if key = key_right then
  begin
   inc(m_image );

   if m_image > 8 then
    m_image:=1;

   force_redraw;

  end;

 if key = key_left then
  begin
   dec(m_image );

   if m_image < 1 then
    m_image:=8;

   force_redraw;

  end;

 if key = key_kp_plus then
  begin
   m_gamma:=m_gamma + gamma_step;

   Str(m_gamma:0:2 ,m_gmText ); m_gmText:='Gamma: ' + m_gmText;

   force_redraw;

  end;

 if key = key_kp_minus then
  begin
   m_gamma:=m_gamma - gamma_step;

   Str(m_gamma:0:2 ,m_gmText ); m_gmText:='Gamma: ' + m_gmText;
   
   force_redraw;

  end;

end;

VAR
 app : the_application;
 buf : array [0..255 ] of char;
 ext : string[10 ];

 img_name ,p ,n ,x : shortstring;

BEGIN
 if Agg2DUsesFreeType then
  begin
   FONT_TIMES:='times.ttf';
   FONT_ARIAL:='arial.ttf';
   FONT_VERDA:='verdana.ttf';

  end;

 app.Construct(pix_format_bgra32 ,flip_y );
 app.caption_ ('Agg2DDemo (F1-Help)' );

 img_name:='spheres2';

{$IFDEF WIN32 }
 if ParamCount > 0 then
  begin
   spread_name(ParamStr(1 ) ,p ,n ,x );

   img_name:=fold_name(p ,n ,'' );

  end;

{$ENDIF }

 if not app.load_img(0 ,img_name ) then
  begin
   img_name:=img_name + #0;
   ext     :=app._img_ext + #0;

   if img_name = 'spheres2'#0 then
    begin
     sprintf(@buf[0 ]             ,'File not found: %s' ,ptrcomp(@img_name[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s. '#13'Download http://www.antigrain.com/' ,ptrcomp(@ext[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s' ,ptrcomp(@img_name[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s'#13'or copy it from another directory if available.' ,ptrcomp(@ext[1 ] ) );

    end
   else
    begin
     sprintf(@buf[0 ]             ,'File not found: %s' ,ptrcomp(@img_name[1 ] ) );
     sprintf(@buf[StrLen(@buf ) ] ,'%s' ,ptrcomp(@ext[1 ] ) );

    end;

   app.message_(@buf[0 ] );

  end
 else
  if app.init(600 ,600 ,window_resize ) then
   app.run;

 app.Destruct;

END.