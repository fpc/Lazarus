 ,agg_pixfmt
 ,agg_color

{$IFDEF AGG_GRAY8 }
 ,agg_pixfmt_gray ;

const
 pix_format = pix_format_gray8;

 pixfmt     : define_pixfmt = pixfmt_gray8;
 pixfmt_pre : define_pixfmt = pixfmt_gray8_pre;

{$ELSE }
{$IFDEF AGG_BGR24 }
 ,agg_pixfmt_rgb ;

const
 pix_format = pix_format_bgr24;

 pixfmt       : define_pixfmt       = pixfmt_bgr24;
 pixfmt_pre   : define_pixfmt       = pixfmt_bgr24_pre;
 pixfmt_gamma : define_pixfmt_gamma = pixfmt_bgr24_gamma;

 component_order : order_type = (R:2; G:1; B:0; A:3 );

{$ELSE }
{$IFDEF AGG_RGB24 }
 ,agg_pixfmt_rgb ;

const
 pix_format = pix_format_rgb24;

 pixfmt       : define_pixfmt       = pixfmt_rgb24;
 pixfmt_pre   : define_pixfmt       = pixfmt_rgb24_pre;
 pixfmt_gamma : define_pixfmt_gamma = pixfmt_rgb24_gamma;

 component_order : order_type = (R:0; G:1; B:2; A:3 );

{$ELSE }
{$IFDEF AGG_BGRA32 }
 ,agg_pixfmt_rgba ;

const
 pix_format = pix_format_bgra32;

 pixfmt     : define_pixfmt = pixfmt_bgra32;
 pixfmt_pre : define_pixfmt = pixfmt_bgra32_pre;

 component_order : order_type = (R:2; G:1; B:0; A:3 );

{$ELSE }
{$IFDEF AGG_RGBA32 }
 ,agg_pixfmt_rgba ;

const
 pix_format = pix_format_rgba32;

 pixfmt     : define_pixfmt = pixfmt_rgba32;
 pixfmt_pre : define_pixfmt = pixfmt_rgba32_pre;

 component_order : order_type = (R:0; G:1; B:2; A:3 );

{$ELSE }
{$IFDEF AGG_ARGB32 }
 ,agg_pixfmt_rgba ;

const
 pix_format = pix_format_argb32;

 pixfmt     : define_pixfmt = pixfmt_argb32;
 pixfmt_pre : define_pixfmt = pixfmt_argb32_pre;

 component_order : order_type = (R:1; G:2; B:3; A:0 );

{$ELSE }
{$IFDEF AGG_ABGR32 }
 ,agg_pixfmt_rgba ;

const
 pix_format = pix_format_abgr32;

 pixfmt     : define_pixfmt = pixfmt_abgr32;
 pixfmt_pre : define_pixfmt = pixfmt_abgr32_pre;

 component_order : order_type = (R:3; G:2; B:1; A:0 );

{$ELSE }
{$IFDEF AGG_RGB555 }
 ,agg_pixfmt_rgb_packed ;

const
 pix_format = pix_format_rgb555;

 pixfmt       : define_pixfmt       = pixfmt_rgb555;
 pixfmt_pre   : define_pixfmt       = pixfmt_rgb555_pre;
 pixfmt_gamma : define_pixfmt_gamma = pixfmt_rgb555_gamma;

 component_order : order_type = (R:0; G:1; B:2; A:3 );

{$ELSE }
{$IFDEF AGG_RGB565 }
 ,agg_pixfmt_rgb_packed ;

const
 pix_format = pix_format_rgb565;

 pixfmt       : define_pixfmt       = pixfmt_rgb565;
 pixfmt_pre   : define_pixfmt       = pixfmt_rgb565_pre;
 pixfmt_gamma : define_pixfmt_gamma = pixfmt_rgb565_gamma;

 component_order : order_type = (R:0; G:1; B:2; A:3 );

{$ELSE }
;
{$ENDIF }
{$ENDIF }
{$ENDIF }
{$ENDIF }
{$ENDIF }
{$ENDIF }
{$ENDIF }
{$ENDIF }
{$ENDIF }
//
// Please define pixel format: AGG_GRAY8, AGG_BGR24, AGG_RGB24, etc. See this file above
//


