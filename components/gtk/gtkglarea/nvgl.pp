// File: NVGL.pp        
// modified: 11-03-2002
//
// FreePascal Bindings for libGL by NVIDIA (based on MESA Bindings from Sebastian Günther)
// Version 0.0.3
// supported NVIDIA Driver Version: 2802
// this is UNPUBLISHED source code and may not be used by anyone (except matti) for anything
// later it will be GPL'ed  :)
// Copyright (C) 2001 Satan


{$MODE delphi}  // objfpc would not work because of direct proc var assignments

unit NVGL;

interface

// =======================================================
//   Unit specific extensions
// =======================================================

function InitGLFromLibrary(libname: PChar): Boolean;

// determines automatically which libraries to use:
function InitGL: Boolean;


var
  GLDumpUnresolvedFunctions,
  GLInitialized: Boolean;


// =======================================================
//   GL consts, types and functions
// =======================================================


// -------------------------------------------------------
//   GL types
// -------------------------------------------------------

type
  PSingle   = ^Single;
  PDouble   = ^Double;
  PShortInt = ^ShortInt;
  PLongword = ^Longword;

  GLvoid    = Pointer;
  GLboolean = Byte;

  GLbyte    = ShortInt; // 1-byte signed
  GLshort   = Integer;  // 2-byte signed
  GLint     = LongInt;  // 4-byte signed

  GLubyte   = Byte;     // 1-byte unsigned
  GLushort  = Word;     // 2-byte unsigned
  GLuint    = DWord;    // 4-byte signed

  GLsizei   = LongInt;  // 4-byte signed

  GLfloat   = Single;   // single precision float
  GLclampf  = Single;   // single precision float in [0,1]
  GLdouble  = Double;   // double precision float
  GLclampd  = Double;   // double precision float in [0,1]

  GLenum    = DWord;

  PGLBoolean = ^GLBoolean;
  PGLFloat   = ^GLfloat;
  PGLDouble  = ^GLDouble;

// -------------------------------------------------------
  PGLubyte = ^GLubyte;
  PGLuint = ^GLuint;
  PGLvoid = ^GLvoid;
  PGLint = ^GLint;
  PGLshort = ^GLshort;
  PGLbyte = ^GLbyte;
  PGLushort = ^GLushort;
  PGLSizei = ^GLSizei;
// -------------------------------------------------------

type
  GLbitfield = DWord;  { was an enum - no corresponding thing in pascal }

// -------------------------------------------------------
//   const
// -------------------------------------------------------

  const
     GL_VERSION_1_1 = 1;
     GL_VERSION_1_2 = 1;
     GL_VERSION_1_3 = 1;
  { Extensions  }
     GL_ARB_imaging = 1;
     GL_ARB_multisample = 1;
     GL_ARB_multitexture = 1;
     GL_ARB_texture_border_clamp = 1;
     GL_ARB_texture_compression = 1;
     GL_ARB_texture_cube_map = 1;
     GL_ARB_texture_env_add = 1;
     GL_ARB_texture_env_combine = 1;
     GL_ARB_texture_env_dot3 = 1;
     GL_ARB_transpose_matrix = 1;
     GL_Autodesk_valid_back_buffer_hint = 1;
     GL_EXT_abgr = 1;
     GL_EXT_bgra = 1;
     GL_EXT_blend_color = 1;
     GL_EXT_blend_minmax = 1;
     GL_EXT_blend_subtract = 1;
     GL_EXT_clip_volume_hint = 1;
     GL_EXT_compiled_vertex_array = 1;
     GL_EXT_color_table = 1;
     GL_EXT_draw_range_elements = 1;
     GL_EXT_fog_coord = 1;
     GL_EXT_multi_draw_arrays = 1;
     GL_EXT_packed_pixels = 1;
     GL_EXT_paletted_texture = 1;
     GL_EXT_point_parameters = 1;
     GL_EXT_rescale_normal = 1;
     GL_EXT_secondary_color = 1;
     GL_EXT_separate_specular_color = 1;
     GL_EXT_shared_texture_palette = 1;
     GL_EXT_stencil_wrap = 1;
     GL_EXT_texture3D = 1;
     GL_EXT_texture_compression_s3tc = 1;
     GL_EXT_texture_cube_map = 1;
     GL_EXT_texture_edge_clamp = 1;
     GL_EXT_texture_env_add = 1;
     GL_EXT_texture_env_combine = 1;
     GL_EXT_texture_env_dot3 = 1;
     GL_EXT_texture_filter_anisotropic = 1;
     GL_EXT_texture_lod_bias = 1;
     GL_EXT_texture_object = 1;
     GL_EXT_vertex_array = 1;
     GL_EXT_vertex_weighting = 1;
     GL_HP_occlusion_test = 1;
     GL_IBM_texture_mirrored_repeat = 1;
     GL_NV_blend_square = 1;
     GL_NV_copy_depth_to_color = 1;
     GL_NV_depth_clamp = 1;
     GL_NV_draw_mesh = 1;
     GL_NV_evaluators = 1;
     GL_NV_fence = 1;
     GL_NV_flusHold = 1;
     GL_NV_fog_distance = 1;
     GL_NV_light_max_exponent = 1;
     GL_NV_mac_get_proc_address = 1;
     GL_NV_multisample_filter_hint = 1;
     GL_NV_occlusion_query = 1;
     GL_NV_packed_depth_stencil = 1;
     GL_NV_point_sprite = 1;
     GL_NV_register_combiners = 1;
     GL_NV_register_combiners2 = 1;
     GL_NV_set_window_stereomode = 1;
     GL_NV_texgen_emboss = 1;
     GL_NV_texgen_reflection = 1;
     GL_NV_texture_compression_vtc = 1;
     GL_NV_texture_env_combine4 = 1;
     GL_NV_texture_rectangle = 1;
     GL_NV_texture_shader = 1;
     GL_NV_texture_shader2 = 1;
     GL_NV_texture_shader3 = 1;
     GL_NV_vertex_array_range = 1;
     GL_NV_vertex_array_range2 = 1;
     GL_NV_vertex_program = 1;
     GL_NV_vertex_program1_1 = 1;
     GL_S3_s3tc = 1;
     GL_SGIS_generate_mipmap = 1;
     GL_SGIS_multitexture = 1;
     GL_SGIS_texture_lod = 1;
     GL_SGIX_depth_texture = 1;
     GL_SGIX_shadow = 1;
     GL_APPLE_transform_hint = 1;
     GL_WIN_swap_hint = 1;
  { AttribMask  }
     GL_CURRENT_BIT = $00000001;
     GL_POINT_BIT = $00000002;
     GL_LINE_BIT = $00000004;
     GL_POLYGON_BIT = $00000008;
     GL_POLYGON_STIPPLE_BIT = $00000010;
     GL_PIXEL_MODE_BIT = $00000020;
     GL_LIGHTING_BIT = $00000040;
     GL_FOG_BIT = $00000080;
     GL_DEPTH_BUFFER_BIT = $00000100;
     GL_ACCUM_BUFFER_BIT = $00000200;
     GL_STENCIL_BUFFER_BIT = $00000400;
     GL_VIEWPORT_BIT = $00000800;
     GL_TRANSFORM_BIT = $00001000;
     GL_ENABLE_BIT = $00002000;
     GL_COLOR_BUFFER_BIT = $00004000;
     GL_HINT_BIT = $00008000;
     GL_EVAL_BIT = $00010000;
     GL_LIST_BIT = $00020000;
     GL_TEXTURE_BIT = $00040000;
     GL_SCISSOR_BIT = $00080000;
     GL_ALL_ATTRIB_BITS = $FFFFFFFF;
  { ClearBufferMask  }
  {      GL_COLOR_BUFFER_BIT  }
  {      GL_ACCUM_BUFFER_BIT  }
  {      GL_STENCIL_BUFFER_BIT  }
  {      GL_DEPTH_BUFFER_BIT  }
  { ClientAttribMask  }
     GL_CLIENT_PIXEL_STORE_BIT = $00000001;
     GL_CLIENT_VERTEX_ARRAY_BIT = $00000002;
     GL_CLIENT_ALL_ATTRIB_BITS = $FFFFFFFF;
  { Boolean  }
     GL_FALSE = 0;
     GL_TRUE = 1;
  { BeginMode  }
     GL_POINTS = $0000;
     GL_LINES = $0001;
     GL_LINE_LOOP = $0002;
     GL_LINE_STRIP = $0003;
     GL_TRIANGLES = $0004;
     GL_TRIANGLE_STRIP = $0005;
     GL_TRIANGLE_FAN = $0006;
     GL_QUADS = $0007;
     GL_QUAD_STRIP = $0008;
     GL_POLYGON = $0009;
  { AccumOp  }
     GL_ACCUM = $0100;
     GL_LOAD = $0101;
     GL_RETURN = $0102;
     GL_MULT = $0103;
     GL_ADD = $0104;
  { AlphaFunction  }
     GL_NEVER = $0200;
     GL_LESS = $0201;
     GL_EQUAL = $0202;
     GL_LEQUAL = $0203;
     GL_GREATER = $0204;
     GL_NOTEQUAL = $0205;
     GL_GEQUAL = $0206;
     GL_ALWAYS = $0207;
  { BlendingFactorDest  }
     GL_ZERO = 0;
     GL_ONE = 1;
     GL_SRC_COLOR = $0300;
     GL_ONE_MINUS_SRC_COLOR = $0301;
     GL_SRC_ALPHA = $0302;
     GL_ONE_MINUS_SRC_ALPHA = $0303;
     GL_DST_ALPHA = $0304;
     GL_ONE_MINUS_DST_ALPHA = $0305;
  { BlendingFactorSrc  }
  {      GL_ZERO  }
  {      GL_ONE  }
     GL_DST_COLOR = $0306;
     GL_ONE_MINUS_DST_COLOR = $0307;
     GL_SRC_ALPHA_SATURATE = $0308;
  {      GL_SRC_ALPHA  }
  {      GL_ONE_MINUS_SRC_ALPHA  }
  {      GL_DST_ALPHA  }
  {      GL_ONE_MINUS_DST_ALPHA  }
  { ColorMaterialFace  }
  {      GL_FRONT  }
  {      GL_BACK  }
  {      GL_FRONT_AND_BACK  }
  { ColorMaterialParameter  }
  {      GL_AMBIENT  }
  {      GL_DIFFUSE  }
  {      GL_SPECULAR  }
  {      GL_EMISSION  }
  {      GL_AMBIENT_AND_DIFFUSE  }
  { ColorPointerType  }
  {      GL_BYTE  }
  {      GL_UNSIGNED_BYTE  }
  {      GL_SHORT  }
  {      GL_UNSIGNED_SHORT  }
  {      GL_INT  }
  {      GL_UNSIGNED_INT  }
  {      GL_FLOAT  }
  {      GL_DOUBLE  }
  { CullFaceMode  }
  {      GL_FRONT  }
  {      GL_BACK  }
  {      GL_FRONT_AND_BACK  }
  { DepthFunction  }
  {      GL_NEVER  }
  {      GL_LESS  }
  {      GL_EQUAL  }
  {      GL_LEQUAL  }
  {      GL_GREATER  }
  {      GL_NOTEQUAL  }
  {      GL_GEQUAL  }
  {      GL_ALWAYS  }
  { DrawBufferMode  }
     GL_NONE = 0;
     GL_FRONT_LEFT = $0400;
     GL_FRONT_RIGHT = $0401;
     GL_BACK_LEFT = $0402;
     GL_BACK_RIGHT = $0403;
     GL_FRONT = $0404;
     GL_BACK = $0405;
     GL_LEFT = $0406;
     GL_RIGHT = $0407;
     GL_FRONT_AND_BACK = $0408;
     GL_AUX0 = $0409;
     GL_AUX1 = $040A;
     GL_AUX2 = $040B;
     GL_AUX3 = $040C;
  { EnableCap  }
  {      GL_FOG  }
  {      GL_LIGHTING  }
  {      GL_TEXTURE_1D  }
  {      GL_TEXTURE_2D  }
  {      GL_LINE_STIPPLE  }
  {      GL_POLYGON_STIPPLE  }
  {      GL_CULL_FACE  }
  {      GL_ALPHA_TEST  }
  {      GL_BLEND  }
  {      GL_INDEX_LOGIC_OP  }
  {      GL_COLOR_LOGIC_OP  }
  {      GL_DITHER  }
  {      GL_STENCIL_TEST  }
  {      GL_DEPTH_TEST  }
  {      GL_CLIP_PLANE0  }
  {      GL_CLIP_PLANE1  }
  {      GL_CLIP_PLANE2  }
  {      GL_CLIP_PLANE3  }
  {      GL_CLIP_PLANE4  }
  {      GL_CLIP_PLANE5  }
  {      GL_LIGHT0  }
  {      GL_LIGHT1  }
  {      GL_LIGHT2  }
  {      GL_LIGHT3  }
  {      GL_LIGHT4  }
  {      GL_LIGHT5  }
  {      GL_LIGHT6  }
  {      GL_LIGHT7  }
  {      GL_TEXTURE_GEN_S  }
  {      GL_TEXTURE_GEN_T  }
  {      GL_TEXTURE_GEN_R  }
  {      GL_TEXTURE_GEN_Q  }
  {      GL_MAP1_VERTEX_3  }
  {      GL_MAP1_VERTEX_4  }
  {      GL_MAP1_COLOR_4  }
  {      GL_MAP1_INDEX  }
  {      GL_MAP1_NORMAL  }
  {      GL_MAP1_TEXTURE_COORD_1  }
  {      GL_MAP1_TEXTURE_COORD_2  }
  {      GL_MAP1_TEXTURE_COORD_3  }
  {      GL_MAP1_TEXTURE_COORD_4  }
  {      GL_MAP2_VERTEX_3  }
  {      GL_MAP2_VERTEX_4  }
  {      GL_MAP2_COLOR_4  }
  {      GL_MAP2_INDEX  }
  {      GL_MAP2_NORMAL  }
  {      GL_MAP2_TEXTURE_COORD_1  }
  {      GL_MAP2_TEXTURE_COORD_2  }
  {      GL_MAP2_TEXTURE_COORD_3  }
  {      GL_MAP2_TEXTURE_COORD_4  }
  {      GL_POINT_SMOOTH  }
  {      GL_LINE_SMOOTH  }
  {      GL_POLYGON_SMOOTH  }
  {      GL_SCISSOR_TEST  }
  {      GL_COLOR_MATERIAL  }
  {      GL_NORMALIZE  }
  {      GL_AUTO_NORMAL  }
  {      GL_POLYGON_OFFSET_POINT  }
  {      GL_POLYGON_OFFSET_LINE  }
  {      GL_POLYGON_OFFSET_FILL  }
  {      GL_VERTEX_ARRAY  }
  {      GL_NORMAL_ARRAY  }
  {      GL_COLOR_ARRAY  }
  {      GL_INDEX_ARRAY  }
  {      GL_TEXTURE_COORD_ARRAY  }
  {      GL_EDGE_FLAG_ARRAY  }
  { ErrorCode  }
     GL_NO_ERROR = 0;
     GL_INVALID_ENUM = $0500;
     GL_INVALID_VALUE = $0501;
     GL_INVALID_OPERATION = $0502;
     GL_STACK_OVERFLOW = $0503;
     GL_STACK_UNDERFLOW = $0504;
     GL_OUT_OF_MEMORY = $0505;
     GL_TABLE_TOO_LARGE = $8031;
  { FeedbackType  }
     GL_2D = $0600;
     GL_3D = $0601;
     GL_3D_COLOR = $0602;
     GL_3D_COLOR_TEXTURE = $0603;
     GL_4D_COLOR_TEXTURE = $0604;
  { FeedBackToken  }
     GL_PASS_THROUGH_TOKEN = $0700;
     GL_POINT_TOKEN = $0701;
     GL_LINE_TOKEN = $0702;
     GL_POLYGON_TOKEN = $0703;
     GL_BITMAP_TOKEN = $0704;
     GL_DRAW_PIXEL_TOKEN = $0705;
     GL_COPY_PIXEL_TOKEN = $0706;
     GL_LINE_RESET_TOKEN = $0707;
  { FogMode  }
  {      GL_LINEAR  }
     GL_EXP = $0800;
     GL_EXP2 = $0801;
  { FogParameter  }
  {      GL_FOG_COLOR  }
  {      GL_FOG_DENSITY  }
  {      GL_FOG_END  }
  {      GL_FOG_INDEX  }
  {      GL_FOG_MODE  }
  {      GL_FOG_START  }
  { FrontFaceDirection  }
     GL_CW = $0900;
     GL_CCW = $0901;
  { GetColorTableParameterPNameEXT  }
  {      GL_COLOR_TABLE_FORMAT_EXT  }
  {      GL_COLOR_TABLE_WIDTH_EXT  }
  {      GL_COLOR_TABLE_RED_SIZE_EXT  }
  {      GL_COLOR_TABLE_GREEN_SIZE_EXT  }
  {      GL_COLOR_TABLE_BLUE_SIZE_EXT  }
  {      GL_COLOR_TABLE_ALPHA_SIZE_EXT  }
  {      GL_COLOR_TABLE_LUMINANCE_SIZE_EXT  }
  {      GL_COLOR_TABLE_INTENSITY_SIZE_EXT  }
  { GetMapQuery  }
     GL_COEFF = $0A00;
     GL_ORDER = $0A01;
     GL_DOMAIN = $0A02;
  { GetPixelMap  }
     GL_PIXEL_MAP_I_TO_I = $0C70;
     GL_PIXEL_MAP_S_TO_S = $0C71;
     GL_PIXEL_MAP_I_TO_R = $0C72;
     GL_PIXEL_MAP_I_TO_G = $0C73;
     GL_PIXEL_MAP_I_TO_B = $0C74;
     GL_PIXEL_MAP_I_TO_A = $0C75;
     GL_PIXEL_MAP_R_TO_R = $0C76;
     GL_PIXEL_MAP_G_TO_G = $0C77;
     GL_PIXEL_MAP_B_TO_B = $0C78;
     GL_PIXEL_MAP_A_TO_A = $0C79;
  { GetPointervPName  }
     GL_VERTEX_ARRAY_POINTER = $808E;
     GL_NORMAL_ARRAY_POINTER = $808F;
     GL_COLOR_ARRAY_POINTER = $8090;
     GL_INDEX_ARRAY_POINTER = $8091;
     GL_TEXTURE_COORD_ARRAY_POINTER = $8092;
     GL_EDGE_FLAG_ARRAY_POINTER = $8093;
  { GetPName  }
     GL_CURRENT_COLOR = $0B00;
     GL_CURRENT_INDEX = $0B01;
     GL_CURRENT_NORMAL = $0B02;
     GL_CURRENT_TEXTURE_COORDS = $0B03;
     GL_CURRENT_RASTER_COLOR = $0B04;
     GL_CURRENT_RASTER_INDEX = $0B05;
     GL_CURRENT_RASTER_TEXTURE_COORDS = $0B06;
     GL_CURRENT_RASTER_POSITION = $0B07;
     GL_CURRENT_RASTER_POSITION_VALID = $0B08;
     GL_CURRENT_RASTER_DISTANCE = $0B09;
     GL_POINT_SMOOTH = $0B10;
     GL_POINT_SIZE = $0B11;
     GL_SMOOTH_POINT_SIZE_RANGE = $0B12;
     GL_SMOOTH_POINT_SIZE_GRANULARITY = $0B13;
     GL_POINT_SIZE_RANGE = GL_SMOOTH_POINT_SIZE_RANGE;
     GL_POINT_SIZE_GRANULARITY = GL_SMOOTH_POINT_SIZE_GRANULARITY;
     GL_LINE_SMOOTH = $0B20;
     GL_LINE_WIDTH = $0B21;
     GL_SMOOTH_LINE_WIDTH_RANGE = $0B22;
     GL_SMOOTH_LINE_WIDTH_GRANULARITY = $0B23;
     GL_LINE_WIDTH_RANGE = GL_SMOOTH_LINE_WIDTH_RANGE;
     GL_LINE_WIDTH_GRANULARITY = GL_SMOOTH_LINE_WIDTH_GRANULARITY;
     GL_LINE_STIPPLE = $0B24;
     GL_LINE_STIPPLE_PATTERN = $0B25;
     GL_LINE_STIPPLE_REPEAT = $0B26;
     GL_LIST_MODE = $0B30;
     GL_MAX_LIST_NESTING = $0B31;
     GL_LIST_BASE = $0B32;
     GL_LIST_INDEX = $0B33;
     GL_POLYGON_MODE = $0B40;
     GL_POLYGON_SMOOTH = $0B41;
     GL_POLYGON_STIPPLE = $0B42;
     GL_EDGE_FLAG = $0B43;
     GL_CULL_FACE = $0B44;
     GL_CULL_FACE_MODE = $0B45;
     GL_FRONT_FACE = $0B46;
     GL_LIGHTING = $0B50;
     GL_LIGHT_MODEL_LOCAL_VIEWER = $0B51;
     GL_LIGHT_MODEL_TWO_SIDE = $0B52;
     GL_LIGHT_MODEL_AMBIENT = $0B53;
     GL_SHADE_MODEL = $0B54;
     GL_COLOR_MATERIAL_FACE = $0B55;
     GL_COLOR_MATERIAL_PARAMETER = $0B56;
     GL_COLOR_MATERIAL = $0B57;
     GL_FOG = $0B60;
     GL_FOG_INDEX = $0B61;
     GL_FOG_DENSITY = $0B62;
     GL_FOG_START = $0B63;
     GL_FOG_END = $0B64;
     GL_FOG_MODE = $0B65;
     GL_FOG_COLOR = $0B66;
     GL_DEPTH_RANGE = $0B70;
     GL_DEPTH_TEST = $0B71;
     GL_DEPTH_WRITEMASK = $0B72;
     GL_DEPTH_CLEAR_VALUE = $0B73;
     GL_DEPTH_FUNC = $0B74;
     GL_ACCUM_CLEAR_VALUE = $0B80;
     GL_STENCIL_TEST = $0B90;
     GL_STENCIL_CLEAR_VALUE = $0B91;
     GL_STENCIL_FUNC = $0B92;
     GL_STENCIL_VALUE_MASK = $0B93;
     GL_STENCIL_FAIL = $0B94;
     GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
     GL_STENCIL_PASS_DEPTH_PASS = $0B96;
     GL_STENCIL_REF = $0B97;
     GL_STENCIL_WRITEMASK = $0B98;
     GL_MATRIX_MODE = $0BA0;
     GL_NORMALIZE = $0BA1;
     GL_VIEWPORT = $0BA2;
     GL_MODELVIEW_STACK_DEPTH = $0BA3;
     GL_PROJECTION_STACK_DEPTH = $0BA4;
     GL_TEXTURE_STACK_DEPTH = $0BA5;
     GL_MODELVIEW_MATRIX = $0BA6;
     GL_PROJECTION_MATRIX = $0BA7;
     GL_TEXTURE_MATRIX = $0BA8;
     GL_ATTRIB_STACK_DEPTH = $0BB0;
     GL_CLIENT_ATTRIB_STACK_DEPTH = $0BB1;
     GL_ALPHA_TEST = $0BC0;
     GL_ALPHA_TEST_FUNC = $0BC1;
     GL_ALPHA_TEST_REF = $0BC2;
     GL_DITHER = $0BD0;
     GL_BLEND_DST = $0BE0;
     GL_BLEND_SRC = $0BE1;
     GL_BLEND = $0BE2;
     GL_LOGIC_OP_MODE = $0BF0;
     GL_INDEX_LOGIC_OP = $0BF1;
     GL_LOGIC_OP = GL_INDEX_LOGIC_OP;
     GL_COLOR_LOGIC_OP = $0BF2;
     GL_AUX_BUFFERS = $0C00;
     GL_DRAW_BUFFER = $0C01;
     GL_READ_BUFFER = $0C02;
     GL_SCISSOR_BOX = $0C10;
     GL_SCISSOR_TEST = $0C11;
     GL_INDEX_CLEAR_VALUE = $0C20;
     GL_INDEX_WRITEMASK = $0C21;
     GL_COLOR_CLEAR_VALUE = $0C22;
     GL_COLOR_WRITEMASK = $0C23;
     GL_INDEX_MODE = $0C30;
     GL_RGBA_MODE = $0C31;
     GL_DOUBLEBUFFER = $0C32;
     GL_STEREO = $0C33;
     GL_RENDER_MODE = $0C40;
     GL_PERSPECTIVE_CORRECTION_HINT = $0C50;
     GL_POINT_SMOOTH_HINT = $0C51;
     GL_LINE_SMOOTH_HINT = $0C52;
     GL_POLYGON_SMOOTH_HINT = $0C53;
     GL_FOG_HINT = $0C54;
     GL_TEXTURE_GEN_S = $0C60;
     GL_TEXTURE_GEN_T = $0C61;
     GL_TEXTURE_GEN_R = $0C62;
     GL_TEXTURE_GEN_Q = $0C63;
     GL_PIXEL_MAP_I_TO_I_SIZE = $0CB0;
     GL_PIXEL_MAP_S_TO_S_SIZE = $0CB1;
     GL_PIXEL_MAP_I_TO_R_SIZE = $0CB2;
     GL_PIXEL_MAP_I_TO_G_SIZE = $0CB3;
     GL_PIXEL_MAP_I_TO_B_SIZE = $0CB4;
     GL_PIXEL_MAP_I_TO_A_SIZE = $0CB5;
     GL_PIXEL_MAP_R_TO_R_SIZE = $0CB6;
     GL_PIXEL_MAP_G_TO_G_SIZE = $0CB7;
     GL_PIXEL_MAP_B_TO_B_SIZE = $0CB8;
     GL_PIXEL_MAP_A_TO_A_SIZE = $0CB9;
     GL_UNPACK_SWAP_BYTES = $0CF0;
     GL_UNPACK_LSB_FIRST = $0CF1;
     GL_UNPACK_ROW_LENGTH = $0CF2;
     GL_UNPACK_SKIP_ROWS = $0CF3;
     GL_UNPACK_SKIP_PIXELS = $0CF4;
     GL_UNPACK_ALIGNMENT = $0CF5;
     GL_PACK_SWAP_BYTES = $0D00;
     GL_PACK_LSB_FIRST = $0D01;
     GL_PACK_ROW_LENGTH = $0D02;
     GL_PACK_SKIP_ROWS = $0D03;
     GL_PACK_SKIP_PIXELS = $0D04;
     GL_PACK_ALIGNMENT = $0D05;
     GL_MAP_COLOR = $0D10;
     GL_MAP_STENCIL = $0D11;
     GL_INDEX_SHIFT = $0D12;
     GL_INDEX_OFFSET = $0D13;
     GL_RED_SCALE = $0D14;
     GL_RED_BIAS = $0D15;
     GL_ZOOM_X = $0D16;
     GL_ZOOM_Y = $0D17;
     GL_GREEN_SCALE = $0D18;
     GL_GREEN_BIAS = $0D19;
     GL_BLUE_SCALE = $0D1A;
     GL_BLUE_BIAS = $0D1B;
     GL_ALPHA_SCALE = $0D1C;
     GL_ALPHA_BIAS = $0D1D;
     GL_DEPTH_SCALE = $0D1E;
     GL_DEPTH_BIAS = $0D1F;
     GL_MAX_EVAL_ORDER = $0D30;
     GL_MAX_LIGHTS = $0D31;
     GL_MAX_CLIP_PLANES = $0D32;
     GL_MAX_TEXTURE_SIZE = $0D33;
     GL_MAX_PIXEL_MAP_TABLE = $0D34;
     GL_MAX_ATTRIB_STACK_DEPTH = $0D35;
     GL_MAX_MODELVIEW_STACK_DEPTH = $0D36;
     GL_MAX_NAME_STACK_DEPTH = $0D37;
     GL_MAX_PROJECTION_STACK_DEPTH = $0D38;
     GL_MAX_TEXTURE_STACK_DEPTH = $0D39;
     GL_MAX_VIEWPORT_DIMS = $0D3A;
     GL_MAX_CLIENT_ATTRIB_STACK_DEPTH = $0D3B;
     GL_SUBPIXEL_BITS = $0D50;
     GL_INDEX_BITS = $0D51;
     GL_RED_BITS = $0D52;
     GL_GREEN_BITS = $0D53;
     GL_BLUE_BITS = $0D54;
     GL_ALPHA_BITS = $0D55;
     GL_DEPTH_BITS = $0D56;
     GL_STENCIL_BITS = $0D57;
     GL_ACCUM_RED_BITS = $0D58;
     GL_ACCUM_GREEN_BITS = $0D59;
     GL_ACCUM_BLUE_BITS = $0D5A;
     GL_ACCUM_ALPHA_BITS = $0D5B;
     GL_NAME_STACK_DEPTH = $0D70;
     GL_AUTO_NORMAL = $0D80;
     GL_MAP1_COLOR_4 = $0D90;
     GL_MAP1_INDEX = $0D91;
     GL_MAP1_NORMAL = $0D92;
     GL_MAP1_TEXTURE_COORD_1 = $0D93;
     GL_MAP1_TEXTURE_COORD_2 = $0D94;
     GL_MAP1_TEXTURE_COORD_3 = $0D95;
     GL_MAP1_TEXTURE_COORD_4 = $0D96;
     GL_MAP1_VERTEX_3 = $0D97;
     GL_MAP1_VERTEX_4 = $0D98;
     GL_MAP2_COLOR_4 = $0DB0;
     GL_MAP2_INDEX = $0DB1;
     GL_MAP2_NORMAL = $0DB2;
     GL_MAP2_TEXTURE_COORD_1 = $0DB3;
     GL_MAP2_TEXTURE_COORD_2 = $0DB4;
     GL_MAP2_TEXTURE_COORD_3 = $0DB5;
     GL_MAP2_TEXTURE_COORD_4 = $0DB6;
     GL_MAP2_VERTEX_3 = $0DB7;
     GL_MAP2_VERTEX_4 = $0DB8;
     GL_MAP1_GRID_DOMAIN = $0DD0;
     GL_MAP1_GRID_SEGMENTS = $0DD1;
     GL_MAP2_GRID_DOMAIN = $0DD2;
     GL_MAP2_GRID_SEGMENTS = $0DD3;
     GL_TEXTURE_1D = $0DE0;
     GL_TEXTURE_2D = $0DE1;
     GL_FEEDBACK_BUFFER_POINTER = $0DF0;
     GL_FEEDBACK_BUFFER_SIZE = $0DF1;
     GL_FEEDBACK_BUFFER_TYPE = $0DF2;
     GL_SELECTION_BUFFER_POINTER = $0DF3;
     GL_SELECTION_BUFFER_SIZE = $0DF4;
     GL_POLYGON_OFFSET_UNITS = $2A00;
     GL_POLYGON_OFFSET_POINT = $2A01;
     GL_POLYGON_OFFSET_LINE = $2A02;
     GL_POLYGON_OFFSET_FILL = $8037;
     GL_POLYGON_OFFSET_FACTOR = $8038;
     GL_TEXTURE_BINDING_1D = $8068;
     GL_TEXTURE_BINDING_2D = $8069;
     GL_TEXTURE_BINDING_3D = $806A;
     GL_VERTEX_ARRAY = $8074;
     GL_NORMAL_ARRAY = $8075;
     GL_COLOR_ARRAY = $8076;
     GL_INDEX_ARRAY = $8077;
     GL_TEXTURE_COORD_ARRAY = $8078;
     GL_EDGE_FLAG_ARRAY = $8079;
     GL_VERTEX_ARRAY_SIZE = $807A;
     GL_VERTEX_ARRAY_TYPE = $807B;
     GL_VERTEX_ARRAY_STRIDE = $807C;
     GL_NORMAL_ARRAY_TYPE = $807E;
     GL_NORMAL_ARRAY_STRIDE = $807F;
     GL_COLOR_ARRAY_SIZE = $8081;
     GL_COLOR_ARRAY_TYPE = $8082;
     GL_COLOR_ARRAY_STRIDE = $8083;
     GL_INDEX_ARRAY_TYPE = $8085;
     GL_INDEX_ARRAY_STRIDE = $8086;
     GL_TEXTURE_COORD_ARRAY_SIZE = $8088;
     GL_TEXTURE_COORD_ARRAY_TYPE = $8089;
     GL_TEXTURE_COORD_ARRAY_STRIDE = $808A;
     GL_EDGE_FLAG_ARRAY_STRIDE = $808C;
  {      GL_VERTEX_ARRAY_COUNT_EXT  }
  {      GL_NORMAL_ARRAY_COUNT_EXT  }
  {      GL_COLOR_ARRAY_COUNT_EXT  }
  {      GL_INDEX_ARRAY_COUNT_EXT  }
  {      GL_TEXTURE_COORD_ARRAY_COUNT_EXT  }
  {      GL_EDGE_FLAG_ARRAY_COUNT_EXT  }
  {      GL_ARRAY_ELEMENT_LOCK_COUNT_EXT  }
  {      GL_ARRAY_ELEMENT_LOCK_FIRST_EXT  }
  { GetTextureParameter  }
  {      GL_TEXTURE_MAG_FILTER  }
  {      GL_TEXTURE_MIN_FILTER  }
  {      GL_TEXTURE_WRAP_S  }
  {      GL_TEXTURE_WRAP_T  }
     GL_TEXTURE_WIDTH = $1000;
     GL_TEXTURE_HEIGHT = $1001;
     GL_TEXTURE_INTERNAL_FORMAT = $1003;
     GL_TEXTURE_COMPONENTS = GL_TEXTURE_INTERNAL_FORMAT;
     GL_TEXTURE_BORDER_COLOR = $1004;
     GL_TEXTURE_BORDER = $1005;
     GL_TEXTURE_RED_SIZE = $805C;
     GL_TEXTURE_GREEN_SIZE = $805D;
     GL_TEXTURE_BLUE_SIZE = $805E;
     GL_TEXTURE_ALPHA_SIZE = $805F;
     GL_TEXTURE_LUMINANCE_SIZE = $8060;
     GL_TEXTURE_INTENSITY_SIZE = $8061;
     GL_TEXTURE_PRIORITY = $8066;
     GL_TEXTURE_RESIDENT = $8067;
  { HintMode  }
     GL_DONT_CARE = $1100;
     GL_FASTEST = $1101;
     GL_NICEST = $1102;
  { HintTarget  }
  {      GL_PERSPECTIVE_CORRECTION_HINT  }
  {      GL_POINT_SMOOTH_HINT  }
  {      GL_LINE_SMOOTH_HINT  }
  {      GL_POLYGON_SMOOTH_HINT  }
  {      GL_FOG_HINT  }
  { IndexMaterialParameterSGI  }
  {      GL_INDEX_OFFSET  }
  { IndexPointerType  }
  {      GL_SHORT  }
  {      GL_INT  }
  {      GL_FLOAT  }
  {      GL_DOUBLE  }
  { IndexFunctionSGI  }
  {      GL_NEVER  }
  {      GL_LESS  }
  {      GL_EQUAL  }
  {      GL_LEQUAL  }
  {      GL_GREATER  }
  {      GL_NOTEQUAL  }
  {      GL_GEQUAL  }
  {      GL_ALWAYS  }
  { LightModelParameter  }
  {      GL_LIGHT_MODEL_AMBIENT  }
  {      GL_LIGHT_MODEL_LOCAL_VIEWER  }
  {      GL_LIGHT_MODEL_TWO_SIDE  }
  { LightParameter  }
     GL_AMBIENT = $1200;
     GL_DIFFUSE = $1201;
     GL_SPECULAR = $1202;
     GL_POSITION = $1203;
     GL_SPOT_DIRECTION = $1204;
     GL_SPOT_EXPONENT = $1205;
     GL_SPOT_CUTOFF = $1206;
     GL_CONSTANT_ATTENUATION = $1207;
     GL_LINEAR_ATTENUATION = $1208;
     GL_QUADRATIC_ATTENUATION = $1209;
  { ListMode  }
     GL_COMPILE = $1300;
     GL_COMPILE_AND_EXECUTE = $1301;
  { DataType  }
     GL_BYTE = $1400;
     GL_UNSIGNED_BYTE = $1401;
     GL_SHORT = $1402;
     GL_UNSIGNED_SHORT = $1403;
     GL_INT = $1404;
     GL_UNSIGNED_INT = $1405;
     GL_FLOAT = $1406;
     GL_2_BYTES = $1407;
     GL_3_BYTES = $1408;
     GL_4_BYTES = $1409;
     GL_DOUBLE = $140A;
     GL_DOUBLE_EXT = $140A;
  { ListNameType  }
  {      GL_BYTE  }
  {      GL_UNSIGNED_BYTE  }
  {      GL_SHORT  }
  {      GL_UNSIGNED_SHORT  }
  {      GL_INT  }
  {      GL_UNSIGNED_INT  }
  {      GL_FLOAT  }
  {      GL_2_BYTES  }
  {      GL_3_BYTES  }
  {      GL_4_BYTES  }
  { LogicOp  }
     GL_CLEAR = $1500;
     GL_AND = $1501;
     GL_AND_REVERSE = $1502;
     GL_COPY = $1503;
     GL_AND_INVERTED = $1504;
     GL_NOOP = $1505;
     GL_XOR = $1506;
     GL_OR = $1507;
     GL_NOR = $1508;
     GL_EQUIV = $1509;
     GL_INVERT = $150A;
     GL_OR_REVERSE = $150B;
     GL_COPY_INVERTED = $150C;
     GL_OR_INVERTED = $150D;
     GL_NAND = $150E;
     GL_SET = $150F;
  { MapTarget  }
  {      GL_MAP1_COLOR_4  }
  {      GL_MAP1_INDEX  }
  {      GL_MAP1_NORMAL  }
  {      GL_MAP1_TEXTURE_COORD_1  }
  {      GL_MAP1_TEXTURE_COORD_2  }
  {      GL_MAP1_TEXTURE_COORD_3  }
  {      GL_MAP1_TEXTURE_COORD_4  }
  {      GL_MAP1_VERTEX_3  }
  {      GL_MAP1_VERTEX_4  }
  {      GL_MAP2_COLOR_4  }
  {      GL_MAP2_INDEX  }
  {      GL_MAP2_NORMAL  }
  {      GL_MAP2_TEXTURE_COORD_1  }
  {      GL_MAP2_TEXTURE_COORD_2  }
  {      GL_MAP2_TEXTURE_COORD_3  }
  {      GL_MAP2_TEXTURE_COORD_4  }
  {      GL_MAP2_VERTEX_3  }
  {      GL_MAP2_VERTEX_4  }
  { MaterialFace  }
  {      GL_FRONT  }
  {      GL_BACK  }
  {      GL_FRONT_AND_BACK  }
  { MaterialParameter  }
     GL_EMISSION = $1600;
     GL_SHININESS = $1601;
     GL_AMBIENT_AND_DIFFUSE = $1602;
     GL_COLOR_INDEXES = $1603;
  {      GL_AMBIENT  }
  {      GL_DIFFUSE  }
  {      GL_SPECULAR  }
  { MatrixMode  }
     GL_MODELVIEW = $1700;
     GL_PROJECTION = $1701;
     GL_TEXTURE = $1702;
  { MeshMode1  }
  {      GL_POINT  }
  {      GL_LINE  }
  { MeshMode2  }
  {      GL_POINT  }
  {      GL_LINE  }
  {      GL_FILL  }
  { NormalPointerType  }
  {      GL_BYTE  }
  {      GL_SHORT  }
  {      GL_INT  }
  {      GL_FLOAT  }
  {      GL_DOUBLE  }
  { PixelCopyType  }
     GL_COLOR = $1800;
     GL_DEPTH = $1801;
     GL_STENCIL = $1802;
  { PixelFormat  }
     GL_COLOR_INDEX = $1900;
     GL_STENCIL_INDEX = $1901;
     GL_DEPTH_COMPONENT = $1902;
     GL_RED = $1903;
     GL_GREEN = $1904;
     GL_BLUE = $1905;
     GL_ALPHA = $1906;
     GL_RGB = $1907;
     GL_RGBA = $1908;
     GL_LUMINANCE = $1909;
     GL_LUMINANCE_ALPHA = $190A;
  {      GL_ABGR_EXT  }
  {      GL_BGR_EXT  }
  {      GL_BGRA_EXT  }
  { PixelMap  }
  {      GL_PIXEL_MAP_I_TO_I  }
  {      GL_PIXEL_MAP_S_TO_S  }
  {      GL_PIXEL_MAP_I_TO_R  }
  {      GL_PIXEL_MAP_I_TO_G  }
  {      GL_PIXEL_MAP_I_TO_B  }
  {      GL_PIXEL_MAP_I_TO_A  }
  {      GL_PIXEL_MAP_R_TO_R  }
  {      GL_PIXEL_MAP_G_TO_G  }
  {      GL_PIXEL_MAP_B_TO_B  }
  {      GL_PIXEL_MAP_A_TO_A  }
  { PixelStoreParameter  }
  {      GL_UNPACK_SWAP_BYTES  }
  {      GL_UNPACK_LSB_FIRST  }
  {      GL_UNPACK_ROW_LENGTH  }
  {      GL_UNPACK_SKIP_ROWS  }
  {      GL_UNPACK_SKIP_PIXELS  }
  {      GL_UNPACK_ALIGNMENT  }
  {      GL_PACK_SWAP_BYTES  }
  {      GL_PACK_LSB_FIRST  }
  {      GL_PACK_ROW_LENGTH  }
  {      GL_PACK_SKIP_ROWS  }
  {      GL_PACK_SKIP_PIXELS  }
  {      GL_PACK_ALIGNMENT  }
  { PixelTransferParameter  }
  {      GL_MAP_COLOR  }
  {      GL_MAP_STENCIL  }
  {      GL_INDEX_SHIFT  }
  {      GL_INDEX_OFFSET  }
  {      GL_RED_SCALE  }
  {      GL_RED_BIAS  }
  {      GL_GREEN_SCALE  }
  {      GL_GREEN_BIAS  }
  {      GL_BLUE_SCALE  }
  {      GL_BLUE_BIAS  }
  {      GL_ALPHA_SCALE  }
  {      GL_ALPHA_BIAS  }
  {      GL_DEPTH_SCALE  }
  {      GL_DEPTH_BIAS  }
  { PixelType  }
     GL_BITMAP = $1A00;
  {      GL_BYTE  }
  {      GL_UNSIGNED_BYTE  }
  {      GL_SHORT  }
  {      GL_UNSIGNED_SHORT  }
  {      GL_INT  }
  {      GL_UNSIGNED_INT  }
  {      GL_FLOAT  }
  {      GL_UNSIGNED_BYTE_3_3_2_EXT  }
  {      GL_UNSIGNED_SHORT_4_4_4_4_EXT  }
  {      GL_UNSIGNED_SHORT_5_5_5_1_EXT  }
  {      GL_UNSIGNED_INT_8_8_8_8_EXT  }
  {      GL_UNSIGNED_INT_10_10_10_2_EXT  }
  { PolygonMode  }
     GL_POINT = $1B00;
     GL_LINE = $1B01;
     GL_FILL = $1B02;
  { ReadBufferMode  }
  {      GL_FRONT_LEFT  }
  {      GL_FRONT_RIGHT  }
  {      GL_BACK_LEFT  }
  {      GL_BACK_RIGHT  }
  {      GL_FRONT  }
  {      GL_BACK  }
  {      GL_LEFT  }
  {      GL_RIGHT  }
  {      GL_AUX0  }
  {      GL_AUX1  }
  {      GL_AUX2  }
  {      GL_AUX3  }
  { RenderingMode  }
     GL_RENDER = $1C00;
     GL_FEEDBACK = $1C01;
     GL_SELECT = $1C02;
  { ShadingModel  }
     GL_FLAT = $1D00;
     GL_SMOOTH = $1D01;
  { StencilFunction  }
  {      GL_NEVER  }
  {      GL_LESS  }
  {      GL_EQUAL  }
  {      GL_LEQUAL  }
  {      GL_GREATER  }
  {      GL_NOTEQUAL  }
  {      GL_GEQUAL  }
  {      GL_ALWAYS  }
  { StencilOp  }
  {      GL_ZERO  }
     GL_KEEP = $1E00;
     GL_REPLACE = $1E01;
     GL_INCR = $1E02;
     GL_DECR = $1E03;
  {      GL_INVERT  }
  { StringName  }
     GL_VENDOR = $1F00;
     GL_RENDERER = $1F01;
     GL_VERSION = $1F02;
     GL_EXTENSIONS = $1F03;
  { TexCoordPointerType  }
  {      GL_SHORT  }
  {      GL_INT  }
  {      GL_FLOAT  }
  {      GL_DOUBLE  }
  { TextureCoordName  }
     GL_S = $2000;
     GL_T = $2001;
     GL_R = $2002;
     GL_Q = $2003;
  { TextureEnvMode  }
     GL_MODULATE = $2100;
     GL_DECAL = $2101;
  {      GL_BLEND  }
  {      GL_REPLACE  }
  {      GL_ADD  }
  { TextureEnvParameter  }
     GL_TEXTURE_ENV_MODE = $2200;
     GL_TEXTURE_ENV_COLOR = $2201;
  { TextureEnvTarget  }
     GL_TEXTURE_ENV = $2300;
  { TextureGenMode  }
     GL_EYE_LINEAR = $2400;
     GL_OBJECT_LINEAR = $2401;
     GL_SPHERE_MAP = $2402;
  { TextureGenParameter  }
     GL_TEXTURE_GEN_MODE = $2500;
     GL_OBJECT_PLANE = $2501;
     GL_EYE_PLANE = $2502;
  { TextureMagFilter  }
     GL_NEAREST = $2600;
     GL_LINEAR = $2601;
  { TextureMinFilter  }
  {      GL_NEAREST  }
  {      GL_LINEAR  }
     GL_NEAREST_MIPMAP_NEAREST = $2700;
     GL_LINEAR_MIPMAP_NEAREST = $2701;
     GL_NEAREST_MIPMAP_LINEAR = $2702;
     GL_LINEAR_MIPMAP_LINEAR = $2703;
  { TextureParameterName  }
     GL_TEXTURE_MAG_FILTER = $2800;
     GL_TEXTURE_MIN_FILTER = $2801;
     GL_TEXTURE_WRAP_S = $2802;
     GL_TEXTURE_WRAP_T = $2803;
  {      GL_TEXTURE_BORDER_COLOR  }
  {      GL_TEXTURE_PRIORITY  }
  { TextureTarget  }
  {      GL_TEXTURE_1D  }
  {      GL_TEXTURE_2D  }
     GL_PROXY_TEXTURE_1D = $8063;
     GL_PROXY_TEXTURE_2D = $8064;
  { TextureWrapMode  }
     GL_CLAMP = $2900;
     GL_REPEAT = $2901;
  { PixelInternalFormat  }
     GL_R3_G3_B2 = $2A10;
     GL_ALPHA4 = $803B;
     GL_ALPHA8 = $803C;
     GL_ALPHA12 = $803D;
     GL_ALPHA16 = $803E;
     GL_LUMINANCE4 = $803F;
     GL_LUMINANCE8 = $8040;
     GL_LUMINANCE12 = $8041;
     GL_LUMINANCE16 = $8042;
     GL_LUMINANCE4_ALPHA4 = $8043;
     GL_LUMINANCE6_ALPHA2 = $8044;
     GL_LUMINANCE8_ALPHA8 = $8045;
     GL_LUMINANCE12_ALPHA4 = $8046;
     GL_LUMINANCE12_ALPHA12 = $8047;
     GL_LUMINANCE16_ALPHA16 = $8048;
     GL_INTENSITY = $8049;
     GL_INTENSITY4 = $804A;
     GL_INTENSITY8 = $804B;
     GL_INTENSITY12 = $804C;
     GL_INTENSITY16 = $804D;
     GL_RGB4 = $804F;
     GL_RGB5 = $8050;
     GL_RGB8 = $8051;
     GL_RGB10 = $8052;
     GL_RGB12 = $8053;
     GL_RGB16 = $8054;
     GL_RGBA2 = $8055;
     GL_RGBA4 = $8056;
     GL_RGB5_A1 = $8057;
     GL_RGBA8 = $8058;
     GL_RGB10_A2 = $8059;
     GL_RGBA12 = $805A;
     GL_RGBA16 = $805B;
  {      GL_COLOR_INDEX1_EXT  }
  {      GL_COLOR_INDEX2_EXT  }
  {      GL_COLOR_INDEX4_EXT  }
  {      GL_COLOR_INDEX8_EXT  }
  {      GL_COLOR_INDEX12_EXT  }
  {      GL_COLOR_INDEX16_EXT  }
  { InterleavedArrayFormat  }
     GL_V2F = $2A20;
     GL_V3F = $2A21;
     GL_C4UB_V2F = $2A22;
     GL_C4UB_V3F = $2A23;
     GL_C3F_V3F = $2A24;
     GL_N3F_V3F = $2A25;
     GL_C4F_N3F_V3F = $2A26;
     GL_T2F_V3F = $2A27;
     GL_T4F_V4F = $2A28;
     GL_T2F_C4UB_V3F = $2A29;
     GL_T2F_C3F_V3F = $2A2A;
     GL_T2F_N3F_V3F = $2A2B;
     GL_T2F_C4F_N3F_V3F = $2A2C;
     GL_T4F_C4F_N3F_V4F = $2A2D;
  { VertexPointerType  }
  {      GL_SHORT  }
  {      GL_INT  }
  {      GL_FLOAT  }
  {      GL_DOUBLE  }
  { ClipPlaneName  }
     GL_CLIP_PLANE0 = $3000;
     GL_CLIP_PLANE1 = $3001;
     GL_CLIP_PLANE2 = $3002;
     GL_CLIP_PLANE3 = $3003;
     GL_CLIP_PLANE4 = $3004;
     GL_CLIP_PLANE5 = $3005;
  { LightName  }
     GL_LIGHT0 = $4000;
     GL_LIGHT1 = $4001;
     GL_LIGHT2 = $4002;
     GL_LIGHT3 = $4003;
     GL_LIGHT4 = $4004;
     GL_LIGHT5 = $4005;
     GL_LIGHT6 = $4006;
     GL_LIGHT7 = $4007;
  { EXT_abgr  }
     GL_ABGR_EXT = $8000;
  { EXT_blend_color  }
     GL_CONSTANT_COLOR_EXT = $8001;
     GL_ONE_MINUS_CONSTANT_COLOR_EXT = $8002;
     GL_CONSTANT_ALPHA_EXT = $8003;
     GL_ONE_MINUS_CONSTANT_ALPHA_EXT = $8004;
     GL_BLEND_COLOR_EXT = $8005;
  { EXT_blend_minmax  }
     GL_FUNC_ADD_EXT = $8006;
     GL_MIN_EXT = $8007;
     GL_MAX_EXT = $8008;
     GL_BLEND_EQUATION_EXT = $8009;
  { EXT_blend_subtract  }
     GL_FUNC_SUBTRACT_EXT = $800A;
     GL_FUNC_REVERSE_SUBTRACT_EXT = $800B;
  { EXT_packed_pixels  }
     GL_UNSIGNED_BYTE_3_3_2_EXT = $8032;
     GL_UNSIGNED_SHORT_4_4_4_4_EXT = $8033;
     GL_UNSIGNED_SHORT_5_5_5_1_EXT = $8034;
     GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
     GL_UNSIGNED_INT_10_10_10_2_EXT = $8036;
  { OpenGL12  }
     GL_PACK_SKIP_IMAGES = $806B;
     GL_PACK_IMAGE_HEIGHT = $806C;
     GL_UNPACK_SKIP_IMAGES = $806D;
     GL_UNPACK_IMAGE_HEIGHT = $806E;
     GL_TEXTURE_3D = $806F;
     GL_PROXY_TEXTURE_3D = $8070;
     GL_TEXTURE_DEPTH = $8071;
     GL_TEXTURE_WRAP_R = $8072;
     GL_MAX_3D_TEXTURE_SIZE = $8073;
     GL_BGR = $80E0;
     GL_BGRA = $80E1;
     GL_UNSIGNED_BYTE_3_3_2 = $8032;
     GL_UNSIGNED_BYTE_2_3_3_REV = $8362;
     GL_UNSIGNED_SHORT_5_6_5 = $8363;
     GL_UNSIGNED_SHORT_5_6_5_REV = $8364;
     GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
     GL_UNSIGNED_SHORT_4_4_4_4_REV = $8365;
     GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
     GL_UNSIGNED_SHORT_1_5_5_5_REV = $8366;
     GL_UNSIGNED_INT_8_8_8_8 = $8035;
     GL_UNSIGNED_INT_8_8_8_8_REV = $8367;
     GL_UNSIGNED_INT_10_10_10_2 = $8036;
     GL_UNSIGNED_INT_2_10_10_10_REV = $8368;
     GL_RESCALE_NORMAL = $803A;
     GL_LIGHT_MODEL_COLOR_CONTROL = $81F8;
     GL_SINGLE_COLOR = $81F9;
     GL_SEPARATE_SPECULAR_COLOR = $81FA;
     GL_CLAMP_TO_EDGE = $812F;
     GL_TEXTURE_MIN_LOD = $813A;
     GL_TEXTURE_MAX_LOD = $813B;
     GL_TEXTURE_BASE_LEVEL = $813C;
     GL_TEXTURE_MAX_LEVEL = $813D;
     GL_MAX_ELEMENTS_VERTICES = $80E8;
     GL_MAX_ELEMENTS_INDICES = $80E9;
     GL_ALIASED_POINT_SIZE_RANGE = $846D;
     GL_ALIASED_LINE_WIDTH_RANGE = $846E;
  { ARB_imaging  }
     GL_CONSTANT_COLOR = $8001;
     GL_ONE_MINUS_CONSTANT_COLOR = $8002;
     GL_CONSTANT_ALPHA = $8003;
     GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
     GL_BLEND_COLOR = $8005;
     GL_FUNC_ADD = $8006;
     GL_MIN = $8007;
     GL_MAX = $8008;
     GL_BLEND_EQUATION = $8009;
     GL_FUNC_SUBTRACT = $800A;
     GL_FUNC_REVERSE_SUBTRACT = $800B;
     GL_COLOR_MATRIX = $80B1;
     GL_COLOR_MATRIX_STACK_DEPTH = $80B2;
     GL_MAX_COLOR_MATRIX_STACK_DEPTH = $80B3;
     GL_POST_COLOR_MATRIX_RED_SCALE = $80B4;
     GL_POST_COLOR_MATRIX_GREEN_SCALE = $80B5;
     GL_POST_COLOR_MATRIX_BLUE_SCALE = $80B6;
     GL_POST_COLOR_MATRIX_ALPHA_SCALE = $80B7;
     GL_POST_COLOR_MATRIX_RED_BIAS = $80B8;
     GL_POST_COLOR_MATRIX_GREEN_BIAS = $80B9;
     GL_POST_COLOR_MATRIX_BLUE_BIAS = $80BA;
     GL_POST_COLOR_MATRIX_ALPHA_BIAS = $80BB;
     GL_COLOR_TABLE = $80D0;
     GL_POST_CONVOLUTION_COLOR_TABLE = $80D1;
     GL_POST_COLOR_MATRIX_COLOR_TABLE = $80D2;
     GL_PROXY_COLOR_TABLE = $80D3;
     GL_PROXY_POST_CONVOLUTION_COLOR_TABLE = $80D4;
     GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE = $80D5;
     GL_COLOR_TABLE_SCALE = $80D6;
     GL_COLOR_TABLE_BIAS = $80D7;
     GL_COLOR_TABLE_FORMAT = $80D8;
     GL_COLOR_TABLE_WIDTH = $80D9;
     GL_COLOR_TABLE_RED_SIZE = $80DA;
     GL_COLOR_TABLE_GREEN_SIZE = $80DB;
     GL_COLOR_TABLE_BLUE_SIZE = $80DC;
     GL_COLOR_TABLE_ALPHA_SIZE = $80DD;
     GL_COLOR_TABLE_LUMINANCE_SIZE = $80DE;
     GL_COLOR_TABLE_INTENSITY_SIZE = $80DF;
     GL_CONVOLUTION_1D = $8010;
     GL_CONVOLUTION_2D = $8011;
     GL_SEPARABLE_2D = $8012;
     GL_CONVOLUTION_BORDER_MODE = $8013;
     GL_CONVOLUTION_FILTER_SCALE = $8014;
     GL_CONVOLUTION_FILTER_BIAS = $8015;
     GL_REDUCE = $8016;
     GL_CONVOLUTION_FORMAT = $8017;
     GL_CONVOLUTION_WIDTH = $8018;
     GL_CONVOLUTION_HEIGHT = $8019;
     GL_MAX_CONVOLUTION_WIDTH = $801A;
     GL_MAX_CONVOLUTION_HEIGHT = $801B;
     GL_POST_CONVOLUTION_RED_SCALE = $801C;
     GL_POST_CONVOLUTION_GREEN_SCALE = $801D;
     GL_POST_CONVOLUTION_BLUE_SCALE = $801E;
     GL_POST_CONVOLUTION_ALPHA_SCALE = $801F;
     GL_POST_CONVOLUTION_RED_BIAS = $8020;
     GL_POST_CONVOLUTION_GREEN_BIAS = $8021;
     GL_POST_CONVOLUTION_BLUE_BIAS = $8022;
     GL_POST_CONVOLUTION_ALPHA_BIAS = $8023;
     GL_IGNORE_BORDER = $8150;
     GL_CONSTANT_BORDER = $8151;
     GL_REPLICATE_BORDER = $8153;
     GL_CONVOLUTION_BORDER_COLOR = $8154;
     GL_HISTOGRAM = $8024;
     GL_PROXY_HISTOGRAM = $8025;
     GL_HISTOGRAM_WIDTH = $8026;
     GL_HISTOGRAM_FORMAT = $8027;
     GL_HISTOGRAM_RED_SIZE = $8028;
     GL_HISTOGRAM_GREEN_SIZE = $8029;
     GL_HISTOGRAM_BLUE_SIZE = $802A;
     GL_HISTOGRAM_ALPHA_SIZE = $802B;
     GL_HISTOGRAM_LUMINANCE_SIZE = $802C;
     GL_HISTOGRAM_SINK = $802D;
     GL_MINMAX = $802E;
     GL_MINMAX_FORMAT = $802F;
     GL_MINMAX_SINK = $8030;
  { OpenGL13 }
     GL_ACTIVE_TEXTURE  = $84E0;
     GL_CLIENT_ACTIVE_TEXTURE = $84E1;
     GL_MAX_TEXTURE_UNITS = $84E2;
     GL_TEXTURE0 = $84C0;
     GL_TEXTURE1 = $84C1;
     GL_TEXTURE2 = $84C2;
     GL_TEXTURE3 = $84C3;
     GL_TEXTURE4 = $84C4;
     GL_TEXTURE5 = $84C5;
     GL_TEXTURE6 = $84C6;
     GL_TEXTURE7 = $84C7;
     GL_TEXTURE8 = $84C8;
     GL_TEXTURE9 = $84C9;
     GL_TEXTURE10 = $84CA;
     GL_TEXTURE11 = $84CB;
     GL_TEXTURE12 = $84CC;
     GL_TEXTURE13 = $84CD;
     GL_TEXTURE14 = $84CE;
     GL_TEXTURE15 = $84CF;
     GL_TEXTURE16 = $84D0;
     GL_TEXTURE17 = $84D1;
     GL_TEXTURE18 = $84D2;
     GL_TEXTURE19 = $84D3;
     GL_TEXTURE20 = $84D4;
     GL_TEXTURE21 = $84D5;
     GL_TEXTURE22 = $84D6;
     GL_TEXTURE23 = $84D7;
     GL_TEXTURE24 = $84D8;
     GL_TEXTURE25 = $84D9;
     GL_TEXTURE26 = $84DA;
     GL_TEXTURE27 = $84DB;
     GL_TEXTURE28 = $84DC;
     GL_TEXTURE29 = $84DD;
     GL_TEXTURE30 = $84DE;
     GL_TEXTURE31 = $84DF;
     GL_NORMAL_MAP = $8511;
     GL_REFLECTION_MAP = $8512;
     GL_TEXTURE_CUBE_MAP = $8513;
     GL_TEXTURE_BINDING_CUBE_MAP = $8514;
     GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
     GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
     GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
     GL_PROXY_TEXTURE_CUBE_MAP = $851B;
     GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
     GL_COMBINE = $8570;
     GL_COMBINE_RGB = $8571;
     GL_COMBINE_ALPHA = $8572;
     GL_RGB_SCALE = $8573;
     GL_ADD_SIGNED = $8574;
     GL_INTERPOLATE = $8575;
     GL_CONSTANT = $8576;
     GL_PRIMARY_COLOR = $8577;
     GL_PREVIOUS = $8578;
     GL_SOURCE0_RGB = $8580;
     GL_SOURCE1_RGB = $8581;
     GL_SOURCE2_RGB = $8582;
     GL_SOURCE0_ALPHA = $8588;
     GL_SOURCE1_ALPHA = $8589;
     GL_SOURCE2_ALPHA = $858A;
     GL_OPERAND0_RGB = $8590;
     GL_OPERAND1_RGB = $8591;
     GL_OPERAND2_RGB = $8592;
     GL_OPERAND0_ALPHA = $8598;
     GL_OPERAND1_ALPHA = $8599;
     GL_OPERAND2_ALPHA = $859A;
     GL_SUBTRACT = $84E7;
     GL_TRANSPOSE_MODELVIEW_MATRIX = $84E3;
     GL_TRANSPOSE_PROJECTION_MATRIX = $84E4;
     GL_TRANSPOSE_TEXTURE_MATRIX = $84E5;
     GL_TRANSPOSE_COLOR_MATRIX = $84E6;
     GL_COMPRESSED_ALPHA = $84E9;
     GL_COMPRESSED_LUMINANCE = $84EA;
     GL_COMPRESSED_LUMINANCE_ALPHA = $84EB;
     GL_COMPRESSED_INTENSITY = $84EC;
     GL_COMPRESSED_RGB = $84ED;
     GL_COMPRESSED_RGBA = $84EE;
     GL_TEXTURE_COMPRESSION_HINT = $84EF;
     GL_TEXTURE_COMPRESSED_IMAGE_SIZE = $86A0;
     GL_TEXTURE_COMPRESSED = $86A1;
     GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
     GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
     GL_DOT3_RGB = $86AE;
     GL_DOT3_RGBA = $86AF;
     GL_CLAMP_TO_BORDER = $812D;
     GL_MULTISAMPLE = $809D;
     GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
     GL_SAMPLE_ALPHA_TO_ONE = $809F;
     GL_SAMPLE_COVERAGE = $80A0;
     GL_SAMPLE_BUFFERS = $80A8;
     GL_SAMPLES = $80A9;
     GL_SAMPLE_COVERAGE_VALUE = $80AA;
     GL_SAMPLE_COVERAGE_INVERT = $80AB;
     GL_MULTISAMPLE_BIT = $20000000;
  { EXT_vertex_array  }
     GL_VERTEX_ARRAY_EXT = $8074;
     GL_NORMAL_ARRAY_EXT = $8075;
     GL_COLOR_ARRAY_EXT = $8076;
     GL_INDEX_ARRAY_EXT = $8077;
     GL_TEXTURE_COORD_ARRAY_EXT = $8078;
     GL_EDGE_FLAG_ARRAY_EXT = $8079;
     GL_VERTEX_ARRAY_SIZE_EXT = $807A;
     GL_VERTEX_ARRAY_TYPE_EXT = $807B;
     GL_VERTEX_ARRAY_STRIDE_EXT = $807C;
     GL_VERTEX_ARRAY_COUNT_EXT = $807D;
     GL_NORMAL_ARRAY_TYPE_EXT = $807E;
     GL_NORMAL_ARRAY_STRIDE_EXT = $807F;
     GL_NORMAL_ARRAY_COUNT_EXT = $8080;
     GL_COLOR_ARRAY_SIZE_EXT = $8081;
     GL_COLOR_ARRAY_TYPE_EXT = $8082;
     GL_COLOR_ARRAY_STRIDE_EXT = $8083;
     GL_COLOR_ARRAY_COUNT_EXT = $8084;
     GL_INDEX_ARRAY_TYPE_EXT = $8085;
     GL_INDEX_ARRAY_STRIDE_EXT = $8086;
     GL_INDEX_ARRAY_COUNT_EXT = $8087;
     GL_TEXTURE_COORD_ARRAY_SIZE_EXT = $8088;
     GL_TEXTURE_COORD_ARRAY_TYPE_EXT = $8089;
     GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = $808A;
     GL_TEXTURE_COORD_ARRAY_COUNT_EXT = $808B;
     GL_EDGE_FLAG_ARRAY_STRIDE_EXT = $808C;
     GL_EDGE_FLAG_ARRAY_COUNT_EXT = $808D;
     GL_VERTEX_ARRAY_POINTER_EXT = $808E;
     GL_NORMAL_ARRAY_POINTER_EXT = $808F;
     GL_COLOR_ARRAY_POINTER_EXT = $8090;
     GL_INDEX_ARRAY_POINTER_EXT = $8091;
     GL_TEXTURE_COORD_ARRAY_POINTER_EXT = $8092;
     GL_EDGE_FLAG_ARRAY_POINTER_EXT = $8093;
  { EXT_texture3D  }
    // GL_PACK_SKIP_IMAGES = $806B;
     GL_PACK_SKIP_IMAGES_EXT = $806B;
    // GL_PACK_IMAGE_HEIGHT = $806C;
     GL_PACK_IMAGE_HEIGHT_EXT = $806C;
    // GL_UNPACK_SKIP_IMAGES = $806D;
     GL_UNPACK_SKIP_IMAGES_EXT = $806D;
    // GL_UNPACK_IMAGE_HEIGHT = $806E;
     GL_UNPACK_IMAGE_HEIGHT_EXT = $806E;
    // GL_TEXTURE_3D = $806F;
     GL_TEXTURE_3D_EXT = $806F;
    // GL_PROXY_TEXTURE_3D = $8070;
     GL_PROXY_TEXTURE_3D_EXT = $8070;
    // GL_TEXTURE_DEPTH = $8071;
     GL_TEXTURE_DEPTH_EXT = $8071;
    // GL_TEXTURE_WRAP_R = $8072;
     GL_TEXTURE_WRAP_R_EXT = $8072;
    // GL_MAX_3D_TEXTURE_SIZE = $8073;
     GL_MAX_3D_TEXTURE_SIZE_EXT = $8073;
  { EXT_color_table  }
     GL_TABLE_TOO_LARGE_EXT = $8031;
     GL_COLOR_TABLE_FORMAT_EXT = $80D8;
     GL_COLOR_TABLE_WIDTH_EXT = $80D9;
     GL_COLOR_TABLE_RED_SIZE_EXT = $80DA;
     GL_COLOR_TABLE_GREEN_SIZE_EXT = $80DB;
     GL_COLOR_TABLE_BLUE_SIZE_EXT = $80DC;
     GL_COLOR_TABLE_ALPHA_SIZE_EXT = $80DD;
     GL_COLOR_TABLE_LUMINANCE_SIZE_EXT = $80DE;
     GL_COLOR_TABLE_INTENSITY_SIZE_EXT = $80DF;
  { EXT_bgra  }
     GL_BGR_EXT = $80E0;
     GL_BGRA_EXT = $80E1;
  { SGIS_texture_lod  }
     GL_TEXTURE_MIN_LOD_SGIS = $813A;
     GL_TEXTURE_MAX_LOD_SGIS = $813B;
     GL_TEXTURE_BASE_LEVEL_SGIS = $813C;
     GL_TEXTURE_MAX_LEVEL_SGIS = $813D;
  { EXT_paletted_texture  }
     GL_COLOR_INDEX1_EXT = $80E2;
     GL_COLOR_INDEX2_EXT = $80E3;
     GL_COLOR_INDEX4_EXT = $80E4;
     GL_COLOR_INDEX8_EXT = $80E5;
     GL_COLOR_INDEX12_EXT = $80E6;
     GL_COLOR_INDEX16_EXT = $80E7;
     GL_TEXTURE_INDEX_SIZE_EXT = $80ED;
  { EXT_clip_volume_hint }
     GL_CLIP_VOLUME_CLIPPING_HINT_EXT = $80F0;
  { EXT_point_parameters  }
     GL_POINT_SIZE_MIN_EXT = $8126;
     GL_POINT_SIZE_MAX_EXT = $8127;
     GL_POINT_FADE_THRESHOLD_SIZE_EXT = $8128;
     GL_DISTANCE_ATTENUATION_EXT = $8129;
  { EXT_compiled_vertex_array  }
     GL_ARRAY_ELEMENT_LOCK_FIRST_EXT = $81A8;
     GL_ARRAY_ELEMENT_LOCK_COUNT_EXT = $81A9;
  { EXT_shared_texture_palette  }
     GL_SHARED_TEXTURE_PALETTE_EXT = $81FB;
  { SGIS_multitexture  }
     GL_SELECTED_TEXTURE_SGIS = $835C;
     GL_MAX_TEXTURES_SGIS = $835D;
     GL_TEXTURE0_SGIS = $835E;
     GL_TEXTURE1_SGIS = $835F;
     GL_TEXTURE2_SGIS = $8360;
     GL_TEXTURE3_SGIS = $8361;
  { ARB_multitexture  }
     GL_ACTIVE_TEXTURE_ARB = $84E0;
     GL_CLIENT_ACTIVE_TEXTURE_ARB = $84E1;
     GL_MAX_TEXTURE_UNITS_ARB = $84E2;
     GL_TEXTURE0_ARB = $84C0;
     GL_TEXTURE1_ARB = $84C1;
     GL_TEXTURE2_ARB = $84C2;
     GL_TEXTURE3_ARB = $84C3;
     GL_TEXTURE4_ARB = $84C4;
     GL_TEXTURE5_ARB = $84C5;
     GL_TEXTURE6_ARB = $84C6;
     GL_TEXTURE7_ARB = $84C7;
     GL_TEXTURE8_ARB = $84C8;
     GL_TEXTURE9_ARB = $84C9;
     GL_TEXTURE10_ARB = $84CA;
     GL_TEXTURE11_ARB = $84CB;
     GL_TEXTURE12_ARB = $84CC;
     GL_TEXTURE13_ARB = $84CD;
     GL_TEXTURE14_ARB = $84CE;
     GL_TEXTURE15_ARB = $84CF;
     GL_TEXTURE16_ARB = $84D0;
     GL_TEXTURE17_ARB = $84D1;
     GL_TEXTURE18_ARB = $84D2;
     GL_TEXTURE19_ARB = $84D3;
     GL_TEXTURE20_ARB = $84D4;
     GL_TEXTURE21_ARB = $84D5;
     GL_TEXTURE22_ARB = $84D6;
     GL_TEXTURE23_ARB = $84D7;
     GL_TEXTURE24_ARB = $84D8;
     GL_TEXTURE25_ARB = $84D9;
     GL_TEXTURE26_ARB = $84DA;
     GL_TEXTURE27_ARB = $84DB;
     GL_TEXTURE28_ARB = $84DC;
     GL_TEXTURE29_ARB = $84DD;
     GL_TEXTURE30_ARB = $84DE;
     GL_TEXTURE31_ARB = $84DF;
  { EXT_fog_coord  }
     GL_FOG_COORDINATE_SOURCE_EXT = $8450;
     GL_FOG_COORDINATE_EXT = $8451;
     GL_FRAGMENT_DEPTH_EXT = $8452;
     GL_CURRENT_FOG_COORDINATE_EXT = $8453;
     GL_FOG_COORDINATE_ARRAY_TYPE_EXT = $8454;
     GL_FOG_COORDINATE_ARRAY_STRIDE_EXT = $8455;
     GL_FOG_COORDINATE_ARRAY_POINTER_EXT = $8456;
     GL_FOG_COORDINATE_ARRAY_EXT = $8457;
  { EXT_secondary_color  }
     GL_COLOR_SUM_EXT = $8458;
     GL_CURRENT_SECONDARY_COLOR_EXT = $8459;
     GL_SECONDARY_COLOR_ARRAY_SIZE_EXT = $845A;
     GL_SECONDARY_COLOR_ARRAY_TYPE_EXT = $845B;
     GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT = $845C;
     GL_SECONDARY_COLOR_ARRAY_POINTER_EXT = $845D;
     GL_SECONDARY_COLOR_ARRAY_EXT = $845E;
  { EXT_separate_specular_color  }
     GL_SINGLE_COLOR_EXT = $81F9;
     GL_SEPARATE_SPECULAR_COLOR_EXT = $81FA;
     GL_LIGHT_MODEL_COLOR_CONTROL_EXT = $81F8;
  { EXT_rescale_normal  }
     GL_RESCALE_NORMAL_EXT = $803A;
  { EXT_stencil_wrap  }
     GL_INCR_WRAP_EXT = $8507;
     GL_DECR_WRAP_EXT = $8508;
  { EXT_vertex_weighting  }
     GL_MODELVIEW0_MATRIX_EXT = GL_MODELVIEW_MATRIX;
     GL_MODELVIEW1_MATRIX_EXT = $8506;
     GL_MODELVIEW0_STACK_DEPTH_EXT = GL_MODELVIEW_STACK_DEPTH;
     GL_MODELVIEW1_STACK_DEPTH_EXT = $8502;
     GL_VERTEX_WEIGHTING_EXT = $8509;
     GL_MODELVIEW0_EXT = GL_MODELVIEW;
     GL_MODELVIEW1_EXT = $850A;
     GL_CURRENT_VERTEX_WEIGHT_EXT = $850B;
     GL_VERTEX_WEIGHT_ARRAY_EXT = $850C;
     GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT = $850D;
     GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT = $850E;
     GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT = $850F;
     GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT = $8510;
  { NV_texgen_reflection  }
     GL_NORMAL_MAP_NV = $8511;
     GL_REFLECTION_MAP_NV = $8512;
  { EXT_texture_cube_map  }
     GL_NORMAL_MAP_EXT = $8511;
     GL_REFLECTION_MAP_EXT = $8512;
     GL_TEXTURE_CUBE_MAP_EXT = $8513;
     GL_TEXTURE_BINDING_CUBE_MAP_EXT = $8514;
     GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT = $8515;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT = $8516;
     GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT = $8517;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT = $8518;
     GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT = $8519;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT = $851A;
     GL_PROXY_TEXTURE_CUBE_MAP_EXT = $851B;
     GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT = $851C;
  { ARB_texture_cube_map  }
     GL_NORMAL_MAP_ARB = $8511;
     GL_REFLECTION_MAP_ARB = $8512;
     GL_TEXTURE_CUBE_MAP_ARB = $8513;
     GL_TEXTURE_BINDING_CUBE_MAP_ARB = $8514;
     GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $8515;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $8516;
     GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $8517;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $8518;
     GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $8519;
     GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $851A;
     GL_PROXY_TEXTURE_CUBE_MAP_ARB = $851B;
     GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB = $851C;
  { NV_vertex_array_range  }
     GL_VERTEX_ARRAY_RANGE_NV = $851D;
     GL_VERTEX_ARRAY_RANGE_LENGTH_NV = $851E;
     GL_VERTEX_ARRAY_RANGE_VALID_NV = $851F;
     GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV = $8520;
     GL_VERTEX_ARRAY_RANGE_POINTER_NV = $8521;
  { NV_vertex_array_range2  }
     GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV = $8533;
  { NV_register_combiners  }
     GL_REGISTER_COMBINERS_NV = $8522;
     GL_COMBINER0_NV = $8550;
     GL_COMBINER1_NV = $8551;
     GL_COMBINER2_NV = $8552;
     GL_COMBINER3_NV = $8553;
     GL_COMBINER4_NV = $8554;
     GL_COMBINER5_NV = $8555;
     GL_COMBINER6_NV = $8556;
     GL_COMBINER7_NV = $8557;
     GL_VARIABLE_A_NV = $8523;
     GL_VARIABLE_B_NV = $8524;
     GL_VARIABLE_C_NV = $8525;
     GL_VARIABLE_D_NV = $8526;
     GL_VARIABLE_E_NV = $8527;
     GL_VARIABLE_F_NV = $8528;
     GL_VARIABLE_G_NV = $8529;
  {      GL_ZERO  }
     GL_CONSTANT_COLOR0_NV = $852A;
     GL_CONSTANT_COLOR1_NV = $852B;
  {      GL_FOG  }
     GL_PRIMARY_COLOR_NV = $852C;
     GL_SECONDARY_COLOR_NV = $852D;
     GL_SPARE0_NV = $852E;
     GL_SPARE1_NV = $852F;
  {      GL_TEXTURE0_ARB  }
  {      GL_TEXTURE1_ARB  }
     GL_UNSIGNED_IDENTITY_NV = $8536;
     GL_UNSIGNED_INVERT_NV = $8537;
     GL_EXPAND_NORMAL_NV = $8538;
     GL_EXPAND_NEGATE_NV = $8539;
     GL_HALF_BIAS_NORMAL_NV = $853A;
     GL_HALF_BIAS_NEGATE_NV = $853B;
     GL_SIGNED_IDENTITY_NV = $853C;
     GL_SIGNED_NEGATE_NV = $853D;
     GL_E_TIMES_F_NV = $8531;
     GL_SPARE0_PLUS_SECONDARY_COLOR_NV = $8532;
  {      GL_NONE  }
     GL_SCALE_BY_TWO_NV = $853E;
     GL_SCALE_BY_FOUR_NV = $853F;
     GL_SCALE_BY_ONE_HALF_NV = $8540;
     GL_BIAS_BY_NEGATIVE_ONE_HALF_NV = $8541;
     GL_DISCARD_NV = $8530;
     GL_COMBINER_INPUT_NV = $8542;
     GL_COMBINER_MAPPING_NV = $8543;
     GL_COMBINER_COMPONENT_USAGE_NV = $8544;
     GL_COMBINER_AB_DOT_PRODUCT_NV = $8545;
     GL_COMBINER_CD_DOT_PRODUCT_NV = $8546;
     GL_COMBINER_MUX_SUM_NV = $8547;
     GL_COMBINER_SCALE_NV = $8548;
     GL_COMBINER_BIAS_NV = $8549;
     GL_COMBINER_AB_OUTPUT_NV = $854A;
     GL_COMBINER_CD_OUTPUT_NV = $854B;
     GL_COMBINER_SUM_OUTPUT_NV = $854C;
     GL_MAX_GENERAL_COMBINERS_NV = $854D;
     GL_NUM_GENERAL_COMBINERS_NV = $854E;
     GL_COLOR_SUM_CLAMP_NV = $854F;
  { NV_fog_distance  }
     GL_FOG_DISTANCE_MODE_NV = $855A;
     GL_EYE_RADIAL_NV = $855B;
  {      GL_EYE_PLANE  }
     GL_EYE_PLANE_ABSOLUTE_NV = $855C;
  { NV_fragment_program }
     GL_FRAGMENT_PROGRAM_NV = $8870;
     GL_MAX_TEXTURE_COORDS_NV = $8871;
     GL_MAX_TEXTURE_IMAGE_UNITS_NV = $8872;
  { NV_texgen_emboss  }
     GL_EMBOSS_LIGHT_NV = $855D;
     GL_EMBOSS_CONSTANT_NV = $855E;
     GL_EMBOSS_MAP_NV = $855F;
  { NV_light_max_exponent  }
     GL_MAX_SHININESS_NV = $8504;
     GL_MAX_SPOT_EXPONENT_NV = $8505;
  { ARB_texture_env_combine  }
     GL_COMBINE_ARB = $8570;
     GL_COMBINE_RGB_ARB = $8571;
     GL_COMBINE_ALPHA_ARB = $8572;
     GL_RGB_SCALE_ARB = $8573;
     GL_ADD_SIGNED_ARB = $8574;
     GL_INTERPOLATE_ARB = $8575;
     GL_CONSTANT_ARB = $8576;
     GL_PRIMARY_COLOR_ARB = $8577;
     GL_PREVIOUS_ARB = $8578;
     GL_SOURCE0_RGB_ARB = $8580;
     GL_SOURCE1_RGB_ARB = $8581;
     GL_SOURCE2_RGB_ARB = $8582;
     GL_SOURCE0_ALPHA_ARB = $8588;
     GL_SOURCE1_ALPHA_ARB = $8589;
     GL_SOURCE2_ALPHA_ARB = $858A;
     GL_OPERAND0_RGB_ARB = $8590;
     GL_OPERAND1_RGB_ARB = $8591;
     GL_OPERAND2_RGB_ARB = $8592;
     GL_OPERAND0_ALPHA_ARB = $8598;
     GL_OPERAND1_ALPHA_ARB = $8599;
     GL_OPERAND2_ALPHA_ARB = $859A;
     GL_SUBTRACT_ARB = $84E7;
  { EXT_texture_env_combine  }
     GL_COMBINE_EXT = $8570;
     GL_COMBINE_RGB_EXT = $8571;
     GL_COMBINE_ALPHA_EXT = $8572;
     GL_RGB_SCALE_EXT = $8573;
     GL_ADD_SIGNED_EXT = $8574;
     GL_INTERPOLATE_EXT = $8575;
     GL_CONSTANT_EXT = $8576;
     GL_PRIMARY_COLOR_EXT = $8577;
     GL_PREVIOUS_EXT = $8578;
     GL_SOURCE0_RGB_EXT = $8580;
     GL_SOURCE1_RGB_EXT = $8581;
     GL_SOURCE2_RGB_EXT = $8582;
     GL_SOURCE0_ALPHA_EXT = $8588;
     GL_SOURCE1_ALPHA_EXT = $8589;
     GL_SOURCE2_ALPHA_EXT = $858A;
     GL_OPERAND0_RGB_EXT = $8590;
     GL_OPERAND1_RGB_EXT = $8591;
     GL_OPERAND2_RGB_EXT = $8592;
     GL_OPERAND0_ALPHA_EXT = $8598;
     GL_OPERAND1_ALPHA_EXT = $8599;
     GL_OPERAND2_ALPHA_EXT = $859A;
  { NV_texture_env_combine4  }
     GL_COMBINE4_NV = $8503;
     GL_SOURCE3_RGB_NV = $8583;
     GL_SOURCE3_ALPHA_NV = $858B;
     GL_OPERAND3_RGB_NV = $8593;
     GL_OPERAND3_ALPHA_NV = $859B;
  { EXT_texture_filter_anisotropic  }
     GL_TEXTURE_MAX_ANISOTROPY_EXT = $84FE;
     GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;
  { EXT_texture_lod_bias  }
     GL_MAX_TEXTURE_LOD_BIAS_EXT = $84FD;
     GL_TEXTURE_FILTER_CONTROL_EXT = $8500;
     GL_TEXTURE_LOD_BIAS_EXT = $8501;
  { EXT_texture_edge_clamp  }
     GL_CLAMP_TO_EDGE_EXT = $812F;
  { S3_s3tc  }
     GL_RGB_S3TC = $83A0;
     GL_RGB4_S3TC = $83A1;
     GL_RGBA_S3TC = $83A2;
     GL_RGBA4_S3TC = $83A3;
  { ARB_transpose_matrix  }
     GL_TRANSPOSE_MODELVIEW_MATRIX_ARB = $84E3;
     GL_TRANSPOSE_PROJECTION_MATRIX_ARB = $84E4;
     GL_TRANSPOSE_TEXTURE_MATRIX_ARB = $84E5;
     GL_TRANSPOSE_COLOR_MATRIX_ARB = $84E6;
  { ARB_texture_compression  }
     GL_COMPRESSED_ALPHA_ARB = $84E9;
     GL_COMPRESSED_LUMINANCE_ARB = $84EA;
     GL_COMPRESSED_LUMINANCE_ALPHA_ARB = $84EB;
     GL_COMPRESSED_INTENSITY_ARB = $84EC;
     GL_COMPRESSED_RGB_ARB = $84ED;
     GL_COMPRESSED_RGBA_ARB = $84EE;
     GL_TEXTURE_COMPRESSION_HINT_ARB = $84EF;
     GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB = $86A0;
     GL_TEXTURE_COMPRESSED_ARB = $86A1;
     GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB = $86A2;
     GL_COMPRESSED_TEXTURE_FORMATS_ARB = $86A3;
  { EXT_texture_compression_s3tc  }
     GL_COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
     GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;
     GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = $83F2;
     GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = $83F3;
  { NV_fence  }
     GL_ALL_COMPLETED_NV = $84F2;
     GL_FENCE_STATUS_NV = $84F3;
     GL_FENCE_CONDITION_NV = $84F4;
  { NV_mac_get_proc_address  }
     GL_ALL_EXTENSIONS_NV = $84FB;
     GL_MAC_GET_PROC_ADDRESS_NV = $84FC;
  { NV_vertex_program  }
     GL_VERTEX_PROGRAM_NV = $8620;
     GL_VERTEX_STATE_PROGRAM_NV = $8621;
     GL_ATTRIB_ARRAY_SIZE_NV = $8623;
     GL_ATTRIB_ARRAY_STRIDE_NV = $8624;
     GL_ATTRIB_ARRAY_TYPE_NV = $8625;
     GL_CURRENT_ATTRIB_NV = $8626;
     GL_PROGRAM_LENGTH_NV = $8627;
     GL_PROGRAM_STRING_NV = $8628;
     GL_MODELVIEW_PROJECTION_NV = $8629;
     GL_IDENTITY_NV = $862A;
     GL_INVERSE_NV = $862B;
     GL_TRANSPOSE_NV = $862C;
     GL_INVERSE_TRANSPOSE_NV = $862D;
     GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV = $862E;
     GL_MAX_TRACK_MATRICES_NV = $862F;
     GL_MATRIX0_NV = $8630;
     GL_MATRIX1_NV = $8631;
     GL_MATRIX2_NV = $8632;
     GL_MATRIX3_NV = $8633;
     GL_MATRIX4_NV = $8634;
     GL_MATRIX5_NV = $8635;
     GL_MATRIX6_NV = $8636;
     GL_MATRIX7_NV = $8637;
     GL_CURRENT_MATRIX_STACK_DEPTH_NV = $8640;
     GL_CURRENT_MATRIX_NV = $8641;
     GL_VERTEX_PROGRAM_POINT_SIZE_NV = $8642;
     GL_VERTEX_PROGRAM_TWO_SIDE_NV = $8643;
     GL_PROGRAM_PARAMETER_NV = $8644;
     GL_ATTRIB_ARRAY_POINTER_NV = $8645;
     GL_PROGRAM_TARGET_NV = $8646;
     GL_PROGRAM_RESIDENT_NV = $8647;
     GL_TRACK_MATRIX_NV = $8648;
     GL_TRACK_MATRIX_TRANSFORM_NV = $8649;
     GL_VERTEX_PROGRAM_BINDING_NV = $864A;
     GL_PROGRAM_ERROR_POSITION_NV = $864B;
     GL_VERTEX_ATTRIB_ARRAY0_NV = $8650;
     GL_VERTEX_ATTRIB_ARRAY1_NV = $8651;
     GL_VERTEX_ATTRIB_ARRAY2_NV = $8652;
     GL_VERTEX_ATTRIB_ARRAY3_NV = $8653;
     GL_VERTEX_ATTRIB_ARRAY4_NV = $8654;
     GL_VERTEX_ATTRIB_ARRAY5_NV = $8655;
     GL_VERTEX_ATTRIB_ARRAY6_NV = $8656;
     GL_VERTEX_ATTRIB_ARRAY7_NV = $8657;
     GL_VERTEX_ATTRIB_ARRAY8_NV = $8658;
     GL_VERTEX_ATTRIB_ARRAY9_NV = $8659;
     GL_VERTEX_ATTRIB_ARRAY10_NV = $865A;
     GL_VERTEX_ATTRIB_ARRAY11_NV = $865B;
     GL_VERTEX_ATTRIB_ARRAY12_NV = $865C;
     GL_VERTEX_ATTRIB_ARRAY13_NV = $865D;
     GL_VERTEX_ATTRIB_ARRAY14_NV = $865E;
     GL_VERTEX_ATTRIB_ARRAY15_NV = $865F;
     GL_MAP1_VERTEX_ATTRIB0_4_NV = $8660;
     GL_MAP1_VERTEX_ATTRIB1_4_NV = $8661;
     GL_MAP1_VERTEX_ATTRIB2_4_NV = $8662;
     GL_MAP1_VERTEX_ATTRIB3_4_NV = $8663;
     GL_MAP1_VERTEX_ATTRIB4_4_NV = $8664;
     GL_MAP1_VERTEX_ATTRIB5_4_NV = $8665;
     GL_MAP1_VERTEX_ATTRIB6_4_NV = $8666;
     GL_MAP1_VERTEX_ATTRIB7_4_NV = $8667;
     GL_MAP1_VERTEX_ATTRIB8_4_NV = $8668;
     GL_MAP1_VERTEX_ATTRIB9_4_NV = $8669;
     GL_MAP1_VERTEX_ATTRIB10_4_NV = $866A;
     GL_MAP1_VERTEX_ATTRIB11_4_NV = $866B;
     GL_MAP1_VERTEX_ATTRIB12_4_NV = $866C;
     GL_MAP1_VERTEX_ATTRIB13_4_NV = $866D;
     GL_MAP1_VERTEX_ATTRIB14_4_NV = $866E;
     GL_MAP1_VERTEX_ATTRIB15_4_NV = $866F;
     GL_MAP2_VERTEX_ATTRIB0_4_NV = $8670;
     GL_MAP2_VERTEX_ATTRIB1_4_NV = $8671;
     GL_MAP2_VERTEX_ATTRIB2_4_NV = $8672;
     GL_MAP2_VERTEX_ATTRIB3_4_NV = $8673;
     GL_MAP2_VERTEX_ATTRIB4_4_NV = $8674;
     GL_MAP2_VERTEX_ATTRIB5_4_NV = $8675;
     GL_MAP2_VERTEX_ATTRIB6_4_NV = $8676;
     GL_MAP2_VERTEX_ATTRIB7_4_NV = $8677;
     GL_MAP2_VERTEX_ATTRIB8_4_NV = $8678;
     GL_MAP2_VERTEX_ATTRIB9_4_NV = $8679;
     GL_MAP2_VERTEX_ATTRIB10_4_NV = $867A;
     GL_MAP2_VERTEX_ATTRIB11_4_NV = $867B;
     GL_MAP2_VERTEX_ATTRIB12_4_NV = $867C;
     GL_MAP2_VERTEX_ATTRIB13_4_NV = $867D;
     GL_MAP2_VERTEX_ATTRIB14_4_NV = $867E;
     GL_MAP2_VERTEX_ATTRIB15_4_NV = $867F;
  { NV_evaluators  }
     GL_EVAL_2D_NV = $86C0;
     GL_EVAL_TRIANGULAR_2D_NV = $86C1;
     GL_MAP_TESSELLATION_NV = $86C2;
     GL_MAP_ATTRIB_U_ORDER_NV = $86C3;
     GL_MAP_ATTRIB_V_ORDER_NV = $86C4;
     GL_EVAL_FRACTIONAL_TESSELLATION_NV = $86C5;
     GL_EVAL_VERTEX_ATTRIB0_NV = $86C6;
     GL_EVAL_VERTEX_ATTRIB1_NV = $86C7;
     GL_EVAL_VERTEX_ATTRIB2_NV = $86C8;
     GL_EVAL_VERTEX_ATTRIB3_NV = $86C9;
     GL_EVAL_VERTEX_ATTRIB4_NV = $86CA;
     GL_EVAL_VERTEX_ATTRIB5_NV = $86CB;
     GL_EVAL_VERTEX_ATTRIB6_NV = $86CC;
     GL_EVAL_VERTEX_ATTRIB7_NV = $86CD;
     GL_EVAL_VERTEX_ATTRIB8_NV = $86CE;
     GL_EVAL_VERTEX_ATTRIB9_NV = $86CF;
     GL_EVAL_VERTEX_ATTRIB10_NV = $86D0;
     GL_EVAL_VERTEX_ATTRIB11_NV = $86D1;
     GL_EVAL_VERTEX_ATTRIB12_NV = $86D2;
     GL_EVAL_VERTEX_ATTRIB13_NV = $86D3;
     GL_EVAL_VERTEX_ATTRIB14_NV = $86D4;
     GL_EVAL_VERTEX_ATTRIB15_NV = $86D5;
     GL_MAX_MAP_TESSELLATION_NV = $86D6;
     GL_MAX_RATIONAL_EVAL_ORDER_NV = $86D7;
  { NV_texture_shader  }
     GL_OFFSET_TEXTURE_RECTANGLE_NV = $864C;
     GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV = $864D;
     GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV = $864E;
     GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV = $86D9;
     GL_UNSIGNED_INT_S8_S8_8_8_NV = $86DA;
     GL_UNSIGNED_INT_8_8_S8_S8_REV_NV = $86DB;
     GL_DSDT_MAG_INTENSITY_NV = $86DC;
     GL_SHADER_CONSISTENT_NV = $86DD;
     GL_TEXTURE_SHADER_NV = $86DE;
     GL_SHADER_OPERATION_NV = $86DF;
     GL_CULL_MODES_NV = $86E0;
     GL_OFFSET_TEXTURE_MATRIX_NV = $86E1;
     GL_OFFSET_TEXTURE_SCALE_NV = $86E2;
     GL_OFFSET_TEXTURE_BIAS_NV = $86E3;
     GL_OFFSET_TEXTURE_2D_MATRIX_NV = GL_OFFSET_TEXTURE_MATRIX_NV;
     GL_OFFSET_TEXTURE_2D_SCALE_NV = GL_OFFSET_TEXTURE_SCALE_NV;
     GL_OFFSET_TEXTURE_2D_BIAS_NV = GL_OFFSET_TEXTURE_BIAS_NV;
     GL_PREVIOUS_TEXTURE_INPUT_NV = $86E4;
     GL_CONST_EYE_NV = $86E5;
     GL_PASS_THROUGH_NV = $86E6;
     GL_CULL_FRAGMENT_NV = $86E7;
     GL_OFFSET_TEXTURE_2D_NV = $86E8;
     GL_DEPENDENT_AR_TEXTURE_2D_NV = $86E9;
     GL_DEPENDENT_GB_TEXTURE_2D_NV = $86EA;
     GL_DOT_PRODUCT_NV = $86EC;
     GL_DOT_PRODUCT_DEPTH_REPLACE_NV = $86ED;
     GL_DOT_PRODUCT_TEXTURE_2D_NV = $86EE;
     GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV = $86F0;
     GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV = $86F1;
     GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV = $86F2;
     GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV = $86F3;
     GL_HILO_NV = $86F4;
     GL_DSDT_NV = $86F5;
     GL_DSDT_MAG_NV = $86F6;
     GL_DSDT_MAG_VIB_NV = $86F7;
     GL_HILO16_NV = $86F8;
     GL_SIGNED_HILO_NV = $86F9;
     GL_SIGNED_HILO16_NV = $86FA;
     GL_SIGNED_RGBA_NV = $86FB;
     GL_SIGNED_RGBA8_NV = $86FC;
     GL_SIGNED_RGB_NV = $86FE;
     GL_SIGNED_RGB8_NV = $86FF;
     GL_SIGNED_LUMINANCE_NV = $8701;
     GL_SIGNED_LUMINANCE8_NV = $8702;
     GL_SIGNED_LUMINANCE_ALPHA_NV = $8703;
     GL_SIGNED_LUMINANCE8_ALPHA8_NV = $8704;
     GL_SIGNED_ALPHA_NV = $8705;
     GL_SIGNED_ALPHA8_NV = $8706;
     GL_SIGNED_INTENSITY_NV = $8707;
     GL_SIGNED_INTENSITY8_NV = $8708;
     GL_DSDT8_NV = $8709;
     GL_DSDT8_MAG8_NV = $870A;
     GL_DSDT8_MAG8_INTENSITY8_NV = $870B;
     GL_SIGNED_RGB_UNSIGNED_ALPHA_NV = $870C;
     GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV = $870D;
     GL_HI_SCALE_NV = $870E;
     GL_LO_SCALE_NV = $870F;
     GL_DS_SCALE_NV = $8710;
     GL_DT_SCALE_NV = $8711;
     GL_MAGNITUDE_SCALE_NV = $8712;
     GL_VIBRANCE_SCALE_NV = $8713;
     GL_HI_BIAS_NV = $8714;
     GL_LO_BIAS_NV = $8715;
     GL_DS_BIAS_NV = $8716;
     GL_DT_BIAS_NV = $8717;
     GL_MAGNITUDE_BIAS_NV = $8718;
     GL_VIBRANCE_BIAS_NV = $8719;
     GL_TEXTURE_BORDER_VALUES_NV = $871A;
     GL_TEXTURE_HI_SIZE_NV = $871B;
     GL_TEXTURE_LO_SIZE_NV = $871C;
     GL_TEXTURE_DS_SIZE_NV = $871D;
     GL_TEXTURE_DT_SIZE_NV = $871E;
     GL_TEXTURE_MAG_SIZE_NV = $871F;
  { NV_texture_shader2 }
     GL_DOT_PRODUCT_TEXTURE_3D_NV = $86EF;
  { NV_texture_shader3 }
     GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV = $8850;
     GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV = $8851;
     GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8852;
     GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV = $8853;
     GL_OFFSET_HILO_TEXTURE_2D_NV = $8854;
     GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV = $8855;
     GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV = $8856;
     GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8857;
     GL_DEPENDENT_HILO_TEXTURE_2D_NV = $8858;
     GL_DEPENDENT_RGB_TEXTURE_3D_NV = $8859;
     GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV = $885A;
     GL_DOT_PRODUCT_PASS_THROUGH_NV = $885B;
     GL_DOT_PRODUCT_TEXTURE_1D_NV = $885C;
     GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV = $885D;
     GL_HILO8_NV = $885E;
     GL_SIGNED_HILO8_NV = $885F;
     GL_FORCE_BLUE_TO_ONE_NV = $8860;
  { NV_register_combiners2  }
     GL_PER_STAGE_CONSTANTS_NV = $8535;
  { IBM_texture_mirrored_repeat  }
     GL_MIRRORED_REPEAT_IBM = $8370;
  { ARB_texture_env_dot3  }
     GL_DOT3_RGB_ARB = $86AE;
     GL_DOT3_RGBA_ARB = $86AF;
  { EXT_texture_env_dot3  }
     GL_DOT3_RGB_EXT = $8740;
     GL_DOT3_RGBA_EXT = $8741;
  { APPLE_transform_hint  }
     GL_TRANSFORM_HINT_APPLE = $85B1;
  { ARB_texture_border_clamp  }
     GL_CLAMP_TO_BORDER_ARB = $812D;
  { NV_texture_rectangle  }
     GL_TEXTURE_RECTANGLE_NV = $84F5;
     GL_TEXTURE_BINDING_RECTANGLE_NV = $84F6;
     GL_PROXY_TEXTURE_RECTANGLE_NV = $84F7;
     GL_MAX_RECTANGLE_TEXTURE_SIZE_NV = $84F8;
  { SGIX_shadow  }
     GL_TEXTURE_COMPARE_SGIX = $819A;
     GL_TEXTURE_COMPARE_OPERATOR_SGIX = $819B;
     GL_TEXTURE_LEQUAL_R_SGIX = $819C;
     GL_TEXTURE_GEQUAL_R_SGIX = $819D;
  { SGIX_depth_texture  }
     GL_DEPTH_COMPONENT16_SGIX = $81A5;
     GL_DEPTH_COMPONENT24_SGIX = $81A6;
     GL_DEPTH_COMPONENT32_SGIX = $81A7;
  { ARB_multisample  }
     GL_MULTISAMPLE_ARB = $809D;
     GL_SAMPLE_ALPHA_TO_COVERAGE_ARB = $809E;
     GL_SAMPLE_ALPHA_TO_ONE_ARB = $809F;
     GL_SAMPLE_COVERAGE_ARB = $80A0;
     GL_SAMPLE_BUFFERS_ARB = $80A8;
     GL_SAMPLES_ARB = $80A9;
     GL_SAMPLE_COVERAGE_VALUE_ARB = $80AA;
     GL_SAMPLE_COVERAGE_INVERT_ARB = $80AB;
     GL_MULTISAMPLE_BIT_ARB = $20000000;
  { NV_multisample_filter_hint  }
     GL_MULTISAMPLE_FILTER_HINT_NV = $8534;
  { NV_packed_depth_stencil  }
     GL_DEPTH_STENCIL_NV = $84F9;
     GL_UNSIGNED_INT_24_8_NV = $84FA;
  { EXT_draw_range_elements  }
     GL_MAX_ELEMENTS_VERTICES_EXT = $80E8;
     GL_MAX_ELEMENTS_INDICES_EXT = $80E9;
  { SGIS_generate_mipmap  }
     GL_GENERATE_MIPMAP_SGIS = $8191;
     GL_GENERATE_MIPMAP_HINT_SGIS = $8192;
  { NV_pixel_data_range  }
     GL_WRITE_PIXEL_DATA_RANGE_NV = $6001;
     GL_READ_PIXEL_DATA_RANGE_NV = $6002;
     GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV = $6003;
     GL_READ_PIXEL_DATA_RANGE_LENGTH_NV = $6004;
     GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV = $6005;
     GL_READ_PIXEL_DATA_RANGE_POINTER_NV = $6006;
  { NV_packed_normal }
     GL_UNSIGNED_INT_S10_S11_S11_REV_NV = $886B;
 // NV_half_float
     GL_HALF_FLOAT_NV = $886C;
 // NV_copy_depth_to_color
     GL_DEPTH_STENCIL_TO_RGBA_NV = $886E;
     GL_DEPTH_STENCIL_TO_BGRA_NV = $886F;
 // HP_occlusion_test
     GL_OCCLUSION_TEST_HP = $8165;
     GL_OCCLUSION_TEST_RESULT_HP = $8166;
 // NV_occlusion_query
     GL_PIXEL_COUNTER_BITS_NV = $8864;
     GL_CURRENT_OCCLUSION_QUERY_ID_NV = $8865;
     GL_PIXEL_COUNT_NV = $8866;
     GL_PIXEL_COUNT_AVAILABLE_NV = $8867;
 // NV_point_sprite
     GL_POINT_SPRITE_NV = $8861;
     GL_COORD_REPLACE_NV = $8862;
     GL_POINT_SPRITE_R_MODE_NV = $8863;
 // 3DFX_tbuffer
     GL_TBUFFER_WRITE_MASK_3DFX = $86D8;
 // NV_depth_clamp
     GL_DEPTH_CLAMP_NV = $864F;
  {                                                            }

// -------------------------------------------------------
//   OpenGL procs and funcs
// -------------------------------------------------------

var

glAccum: procedure(op: GLenum; value: Single); cdecl;
glActiveTexture: procedure(texture: GLenum); cdecl;
glActiveTextureARB: procedure(texture: GLenum); cdecl;
glAddSwapHintRectWIN: procedure(x, y: GLint; width, height: GLsizei); cdecl;
glAlphaFunc: procedure(func: GLenum; ref: GLclampf); cdecl;
glAreProgramsResidentNV: function(n: GLsizei; const programs: PGLuint; residences: PGLboolean): GLboolean; cdecl;
glAreTexturesResident: function(n: LongInt; var textures: LongWord; var residences: Boolean): Boolean; cdecl;
glAreTexturesResidentEXT: function(n: LongInt; var textures: LongWord; var residences: Boolean): Boolean; cdecl;
glArrayElement: procedure(i: LongInt); cdecl;
glArrayElementEXT: procedure(i: LongInt); cdecl;
glBegin: procedure(mode: GLenum); cdecl;
glBeginOcclusionQueryNV: procedure(id: GLuint); cdecl;
glBindProgramNV: procedure(target: GLenum; id: GLuint); cdecl;
glBindTexture: procedure(target: GLenum; texture: LongWord); cdecl;
glBindTextureEXT: procedure(target: GLenum; texture: LongWord); cdecl;
glBitmap: procedure(width, height: LongInt; xorig, yorig, xmove, ymove: Single; var bitmap); cdecl;
glBlendColor: procedure(red, green, blue, alpha: GLclampf); cdecl;
glBlendColorEXT: procedure(red, green, blue, alpha: GLclampf); cdecl;
glBlendEquation: procedure(mode: GLenum); cdecl;
glBlendEquationEXT: procedure(mode: GLenum); cdecl;
glBlendFunc: procedure(sfactor, dfactor: GLenum); cdecl;
glCallList: procedure(list: LongWord); cdecl;
glCallLists: procedure(n: LongInt; _Type: GLenum; var lists); cdecl;
glClear: procedure(mask: GLbitfield); cdecl;
glClearAccum: procedure(red, green, blue, alpha: Single); cdecl;
glClearColor: procedure(red, green, blue, alpha: GLclampf); cdecl;
glClearIndex: procedure(c: Single); cdecl;
glClearDepth: procedure(depth: GLclampd); cdecl;
glClearStencil: procedure(s: LongInt); cdecl;
glClientActiveTexture: procedure(texture: GLenum); cdecl;
glClientActiveTextureARB: procedure(texture: GLenum); cdecl;
glClipPlane: procedure(plane: GLenum; var equation: Double); cdecl;
glColor3b: procedure (red, green, blue: ShortInt); cdecl;
glColor3bv: procedure (v: PShortInt); cdecl;
glColor3d: procedure (red, green, blue: Double); cdecl;
glColor3dv: procedure (v: PDouble); cdecl;
glColor3f: procedure (red, green, blue: Single); cdecl;
glColor3fv: procedure (v:PSingle); cdecl;
glColor3i: procedure (red, green, blue: LongInt); cdecl;
glColor3iv: procedure (v: PLongInt); cdecl;
glColor3s: procedure (red, green, blue: SmallInt); cdecl;
glColor3sv: procedure (v: PSmallInt); cdecl;
glColor3ub: procedure(red, green, blue: Byte); cdecl;
glColor3ubv: procedure(v: PByte); cdecl;
glColor3ui: procedure(red, green, blue: LongWord); cdecl;
glColor3uiv: procedure(v: PLongWord); cdecl;
glColor3us: procedure(red, green, blue: Word); cdecl;
glColor3usv: procedure(v: PWord); cdecl;
glColor4b: procedure (red, green, blue, alpha: ShortInt); cdecl;
glColor4bv: procedure (v: PShortInt); cdecl;
glColor4d: procedure (red, green, blue, alpha: Double); cdecl;
glColor4dv: procedure (v: PDouble); cdecl;
glColor4f: procedure (red, green, blue, alpha: Single); cdecl;
glColor4fv: procedure (v: PSingle); cdecl;
glColor4i: procedure (red, green, blue, alpha: LongInt); cdecl;
glColor4iv: procedure (v: PLongInt); cdecl;
glColor4s: procedure (red, green, blue, alpha: SmallInt); cdecl;
glColor4sv: procedure (v: PSmallInt); cdecl;
glColor4ub: procedure(red, green, blue, alpha: Byte); cdecl;
glColor4ubv: procedure(v: PByte); cdecl;
glColor4ui: procedure(red, green, blue, alpha: LongWord); cdecl;
glColor4uiv: procedure(v: PLongWord); cdecl;
glColor4us: procedure(red, green, blue, alpha: Word); cdecl;
glColor4usv: procedure(v: PWord); cdecl;
glColorMask: procedure(red, green, blue, alpha: GLboolean); cdecl;
glColorMaterial: procedure(face, mode: GLenum); cdecl;
glColorPointer: procedure(size: LongInt; _Type: GLenum; stride: LongInt; var ptr); cdecl;
glColorPointerEXT: procedure(size: LongInt; _Type: GLenum; stride, count: LongInt; var ptr); cdecl;
glColorSubTable: procedure(target: GLenum; start, count: GLsizei; format, _type: GLenum; const data: PGLvoid); cdecl;
glColorSubTableEXT: procedure(target: GLenum; start, count: LongInt; format, _Type: GLEnum; var data); cdecl;
glColorTable: procedure(target, internalformat: GLenum; width: GLsizei; format, _type: GLenum; const table: PGLvoid); cdecl;
glColorTableEXT: procedure(target, internalformat: GLenum; width: LongInt; format, _Type: GLenum; var table); cdecl;
glColorTableParameterfv: procedure(target, pname: GLenum; const params: PGLfloat); cdecl;
glColorTableParameteriv: procedure(target, pname: GLenum; const params: PGLint); cdecl;
glCombinerInputNV: procedure(stage, portion, variable, input, mapping, componentUsage: GLenum); cdecl;
glCombinerOutputNV: procedure(stage, portion, abOutput, cdOutput, sumOutput, scale, bias: GLenum; abDotProduct, cdDotProduct, muxSum: GLboolean); cdecl;
glCombinerParameterfNV: procedure(pname: GLenum; param: GLfloat); cdecl;
glCombinerParameterfvNV: procedure(pname: GLenum; const params: PGLfloat); cdecl;
glCombinerParameteriNV: procedure(pname: GLenum; param: GLint); cdecl;
glCombinerParameterivNV: procedure(pname: GLenum; const params: PGLint); cdecl;
glCombinerStageParameterfvNV: procedure(stage, pname: GLenum; const params: PGLfloat); cdecl;
glCompressedTexImage1D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize:  GLsizei; const data: PGLvoid); cdecl;
glCompressedTexImage1DARB: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize:  GLsizei; const data: PGLvoid); cdecl;
glCompressedTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width, height: GLsizei; border: GLint; imageSize:  GLsizei; const data: PGLvoid); cdecl;
glCompressedTexImage2DARB: procedure(target: GLenum; level: GLint; internalformat: GLenum; width, height: GLsizei; border: GLint; imageSize:  GLsizei; const data: PGLvoid); cdecl;
glCompressedTexImage3D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width, height, depth: GLsizei; border: GLint; imageSize:  GLsizei; const data: PGLvoid); cdecl;
glCompressedTexImage3DARB: procedure(target: GLenum; level: GLint; internalformat: GLenum; width, height, depth: GLsizei; border: GLint; imageSize:  GLsizei; const data: PGLvoid); cdecl;
glCompressedTexSubImage1D: procedure(target: GLenum; level, xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); cdecl;
glCompressedTexSubImage1DARB: procedure(target: GLenum; level, xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); cdecl;
glCompressedTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); cdecl;
glCompressedTexSubImage2DARB: procedure(target: GLenum; level, xoffset, yoffset: GLint; width, height: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); cdecl;
glCompressedTexSubImage3D: procedure(target: GLenum; level, xoffset, yoffset, zoffset: GLint; width, height, depth: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); cdecl;
glCompressedTexSubImage3DARB: procedure(target: GLenum; level, xoffset, yoffset, zoffset: GLint; width, height, depth: GLsizei; format: GLenum; imageSize: GLsizei; const data: PGLvoid); cdecl;
glConvolutionFilter1D: procedure(target, internalformat: GLenum; width: GLsizei; format, _type: GLenum; const image: PGLvoid); cdecl;
glConvolutionFilter2D: procedure(target, internalformat: GLenum; width, height: GLsizei; format, _type: GLenum; const image: PGLvoid); cdecl;
glConvolutionParameterf: procedure(target, pname: GLenum; params: GLfloat); cdecl;
glConvolutionParameterfv: procedure(target, pname: GLenum; const params: PGLfloat); cdecl;
glConvolutionParameteri: procedure(target, pname: GLenum; params: GLint); cdecl;
glConvolutionParameteriv: procedure(target, pname: GLenum; const params: PGLint); cdecl;
glCopyColorSubTable: procedure(target: GLenum; start: GLsizei; x, y: GLint; width: GLsizei); cdecl;
glCopyColorTable: procedure(target, internalformat: GLenum; x, y: GLint; width: GLsizei); cdecl;
glCopyConvolutionFilter1D: procedure(target, internalformat: GLenum; x, y: GLint; width:  GLsizei); cdecl;
glCopyConvolutionFilter2D: procedure(target, internalformat: GLenum; x, y: GLint; width, height: GLsizei); cdecl;
glCopyPixels: procedure(x, y, width, height: LongInt; _Type: GLenum); cdecl;
glCopyTexImage1D: procedure(target: GLenum; level: LongInt; format: GLenum; x, y, width, border: LongInt); cdecl;
glCopyTexImage2D: procedure(target: GLenum; level: LongInt; format: GLenum; x, y, width, height, border: LongInt); cdecl;
glCopyTexSubImage1D: procedure(target: GLenum; level, xoffset, x, y, width: LongInt); cdecl;
glCopyTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, x, y, width, height: LongInt); cdecl;
glCopyTexSubImage3D: procedure(target: GLenum; level: LongInt; xoffset, yoffset, zoffset, x, y, width, height: LongInt); cdecl;
glCopyTexSubImage3DEXT: procedure(target: GLenum; level: LongInt; xoffset, yoffset, zoffset, x, y, width, height: LongInt); cdecl;
glCullFace: procedure(mode: GLenum); cdecl;
glDeleteFencesNV: procedure(n: GLsizei; const fences: PGLuint); cdecl;
glDeleteLists: procedure(list: LongWord; range: LongInt); cdecl;
glDeleteOcclusionQueriesNV: procedure(n: GLsizei; const ids: PGLuint); cdecl;
glDeleteProgramsNV: procedure(n: GLsizei; const programs: PGLuint); cdecl;
glDeleteTextures: procedure(n: LongInt; var textures: LongWord); cdecl;
glDeleteTexturesEXT: procedure(n: LongInt; var textures: LongWord); cdecl;
glDepthFunc: procedure(func: LongInt); cdecl;
glDepthMask: procedure(flag: GLBoolean); cdecl;
glDepthRange: procedure(near_val, far_val: GLclampd); cdecl;
glDisable: procedure(cap: LongInt); cdecl;
glDisableClientState: procedure(cap: GLenum); cdecl;
glDrawArrays: procedure(mode: GLenum; first, count: LongInt); cdecl;
glDrawArraysEXT: procedure(mode: GLEnum; first, count: LongInt); cdecl;
glDrawBuffer: procedure(mode: GLenum); cdecl;
glDrawElements: procedure(mode: GLenum; count: Integer; _Type: GLenum; var indices); cdecl;
glDrawMeshNV: procedure(mode: GLenum; count: GLsizei; _type: GLenum; stride: GLsizei; const indicesTexCoord, indicesNormal, indicesVertex: PGLvoid); cdecl;
glDrawPixels: procedure(width, height: LongInt; format, _Type: GLenum; var pixels); cdecl;
glDrawRangeElements: procedure(mode: GLenum; _Start, _End: LongWord; count: LongInt; _Type: GLenum; var indices); cdecl;
glDrawRangeElementsEXT: procedure(mode: GLenum; start, _end: GLuint; count: GLsizei; _type: GLenum; const indices: PGLvoid); cdecl;
glEdgeFlag: procedure(flag: GLBoolean); cdecl;
glEdgeFlagPointer: procedure(stride: LongInt; var ptr); cdecl;
glEdgeFlagPointerEXT: procedure(stride, count: LongInt; var ptr: Boolean); cdecl;
glEdgeFlagv: procedure(var flag: GLBoolean); cdecl;
glEnable: procedure(cap: LongInt); cdecl;
glEnableClientState: procedure(cap: GLenum); cdecl;
glEnd: procedure; cdecl;
glEndList: procedure; cdecl;
glEndOcclusionQueryNV: procedure; cdecl;
glEvalCoord1d: procedure(u: Double); cdecl;
glEvalCoord1dv: procedure(var u: Double); cdecl;
glEvalCoord1f: procedure(u: Single); cdecl;
glEvalCoord1fv: procedure(var u: Single); cdecl;
glEvalCoord2d: procedure(u, v: Double); cdecl;
glEvalCoord2dv: procedure(var u, v: Double); cdecl;
glEvalCoord2f: procedure(u, v: Single); cdecl;
glEvalCoord2fv: procedure(var u, v: Single); cdecl;
glEvalMapsNV: procedure(target, mode: GLenum); cdecl;
glEvalMesh1: procedure(mode: GLenum; i1, i2: LongInt); cdecl;
glEvalMesh2: procedure(mode: GLenum; i1, i2, j1, j2: LongInt); cdecl;
glEvalPoint1: procedure(i: LongInt); cdecl;
glEvalPoint2: procedure(i, j: LongInt); cdecl;
glExecuteProgramNV: procedure(target: GLenum; id: GLuint; const params: PGLfloat); cdecl;
glFeedbackBuffer: procedure(size: LongInt; _Type: GLenum; var buffer: Single); cdecl;
glFinalCombinerInputNV: procedure(variable, input, mapping, componentUsage: GLenum); cdecl;
glFinish: procedure; cdecl;
glFinishFenceNV: procedure(fence: GLuint); cdecl;
glFlush: procedure; cdecl;
glFlushPixelDataRangeNV: procedure(target: GLenum); cdecl;
glFlushVertexArrayRangeNV: procedure; cdecl;
glFlushHold: function: PGLvoid; cdecl;
glFogCoordPointerEXT: procedure(_type: GLenum; stride: GLsizei; const pointer: PGLvoid); cdecl;
glFogCoorddEXT: procedure(fog: GLdouble); cdecl;
glFogCoorddvEXT: procedure(const fog: PGLdouble); cdecl;
glFogCoordfEXT: procedure(fog: GLfloat); cdecl;
glFogCoordfvEXT: procedure(const fog: PGLfloat); cdecl;
glFogf: procedure(pname: GLenum; param: Single); cdecl;
glFogfv: procedure(pname: GLenum; params : PSingle); cdecl;
glFogi: procedure(pname: GLenum; param: LongInt); cdecl;
glFogiv: procedure(pname: GLenum; params : PLongInt); cdecl;
glFrontFace: procedure(mode: GLenum); cdecl;
glFrustum: procedure(left, right, bottom, top, near_val, far_val: Double); cdecl;
glGenFencesNV: procedure(n: GLsizei; fences: PGLuint); cdecl;
glGenOcclusionQueriesNV: procedure(n: GLsizei; ids: PGLuint); cdecl;
glGenLists: function(range: LongInt): LongWord; cdecl;
glGenProgramsNV: procedure(n: GLsizei; programs: PGLuint); cdecl;
glGenTextures: procedure(n: LongInt; var textures: LongWord); cdecl;
glGenTexturesEXT: procedure(n: LongInt; var textures: LongWord); cdecl;
glGetBooleanv: procedure(pname: GLenum; params : PGLBoolean); cdecl;
glGetClipPlane: procedure(plane: GLenum; var equation: Double); cdecl;
glGetColorTable: procedure(target, format, _type: GLenum; table: PGLvoid); cdecl;
glGetColorTableEXT: procedure(target, format, _Type: GLenum; var table); cdecl;
glGetColorTableParameterfv: procedure(target, pname: GLenum; params: PGLfloat); cdecl;
glGetColorTableParameterfvEXT: procedure(target, pname: GLenum; var params: Single); cdecl;
glGetColorTableParameteriv: procedure(target, pname: GLenum; params: PGLint); cdecl;
glGetColorTableParameterivEXT: procedure(target, pname: GLenum; var params: LongInt); cdecl;
glGetCombinerInputParameterfvNV: procedure(stage, portion, variable, pname: GLenum; params: PGLfloat); cdecl;
glGetCombinerInputParameterivNV: procedure(stage, portion, variable, pname: GLenum; params: PGLint); cdecl;
glGetCombinerOutputParameterfvNV: procedure(stage, portion, pname: GLenum; params: PGLfloat); cdecl;
glGetCombinerOutputParameterivNV: procedure(stage, portion, pname: GLenum; params: PGLint); cdecl;
glGetCombinerStageParameterfvNV: procedure(stage, pname: GLenum; params: PGLfloat); cdecl;
glGetCompressedTexImage: procedure(target: GLenum; lod: GLint; img: PGLvoid); cdecl;
glGetCompressedTexImageARB: procedure(target: GLenum; lod: GLint; img: PGLvoid); cdecl;
glGetConvolutionFilter: procedure(target, format, _type: GLenum; image: PGLvoid); cdecl;
glGetConvolutionParameterfv: procedure(target, pname: GLenum; params: PGLfloat); cdecl;
glGetConvolutionParameteriv: procedure(target, pname: GLenum; params: PGLint); cdecl;
glGetDoublev: procedure(pname: GLenum; params : PDouble); cdecl;
glGetError: function: GLenum; cdecl;
glGetFenceivNV: procedure(fence: GLuint; pname: GLenum; params: PGLint); cdecl;
glGetFinalCombinerInputParameterfvNV: procedure(variable, pname: GLenum; params: PGLfloat); cdecl;
glGetFinalCombinerInputParameterivNV: procedure(variable, pname: GLenum; params: PGLint); cdecl;
glGetFloatv: procedure(pname: GLenum; params : PSingle); cdecl;
glGetHistogram: procedure(target: GLenum; reset: GLboolean; format, _type: GLenum; values: PGLvoid); cdecl;
glGetHistogramParameterfv: procedure(target, pname: GLenum; params: PGLfloat); cdecl;
glGetHistogramParameteriv: procedure(target, pname: GLenum; params: PGLint); cdecl;
glGetIntegerv: procedure(pname: GLenum; params : PLongInt); cdecl;
glGetLightfv: procedure(light, pname: GLenum; params : PSingle); cdecl;
glGetLightiv: procedure(light, pname: GLenum; params : PLongInt); cdecl;
glGetMapAttribParameterfvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLfloat); cdecl;
glGetMapAttribParameterivNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLint); cdecl;
glGetMapControlPointsNV: procedure(target: GLenum; index: GLuint; _type: GLenum; ustride, vstride: GLsizei; _packed: GLboolean; points: PGLvoid); cdecl;
glGetMapParameterfvNV: procedure(target, pname: GLenum; params: PGLfloat); cdecl;
glGetMapParameterivNV: procedure(target, pname: GLenum; params: PGLint); cdecl;
glGetMapdv: procedure(target, query: GLenum; var v: Double); cdecl;
glGetMapfv: procedure(target, query: GLenum; var v: Single); cdecl;
glGetMapiv: procedure(target, query: GLenum; var v: LongInt); cdecl;
glGetMaterialfv: procedure(face, pname: GLenum; params : PSingle); cdecl;
glGetMaterialiv: procedure(face, pname: GLenum; params : PLongInt); cdecl;
glGetMinmax: procedure(target: GLenum; reset: GLboolean; format, _type: GLenum; values: PGLvoid); cdecl;
glGetMinmaxParameterfv: procedure(target, pname: GLenum; params: PGLfloat); cdecl;
glGetMinmaxParameteriv: procedure(target, pname: GLenum; params: PGLint); cdecl;
glGetOcclusionQueryivNV: procedure(id: GLuint; pname: GLenum; params: PGLint);
glGetOcclusionQueryuivNV: procedure(id: GLuint; pname: GLenum; params: PGLuint);
glGetPixelMapfv: procedure(map: GLenum; var values: Single); cdecl;
glGetPixelMapuiv: procedure(map: GLenum; var values: LongWord); cdecl;
glGetPixelMapusv: procedure(map: GLenum; var values: Word); cdecl;
glGetPointerv: procedure(pname: GLenum; var params: Pointer); cdecl;
glGetPointervEXT: procedure(pname: GLenum; var params: Pointer); cdecl;
glGetPolygonStipple: procedure(var mask: Byte); cdecl;
glGetProgramLocalParameterdvNV: procedure(target: GLenum; len: GLsizei; const name: PGLubyte; params: PGLdouble);
glGetProgramLocalParameterfvNV: procedure(target: GLenum; len: GLsizei; const name: PGLubyte; params: PGLfloat);
glGetProgramParameterSigneddvNV: procedure(target: GLenum; index: GLint; pname: GLenum; params: PGLdouble);
glGetProgramParameterSignedfvNV: procedure(target: GLenum; index: GLint; pname: GLenum; params: PGLfloat);
glGetProgramParameterdvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLdouble); cdecl;
glGetProgramParameterfvNV: procedure(target: GLenum; index: GLuint; pname: GLenum; params: PGLfloat); cdecl;
glGetProgramStringNV: procedure(id: GLuint; pname: GLenum; _program: PGLubyte); cdecl;
glGetProgramivNV: procedure(id: GLuint; pname: GLenum; params: PGLint); cdecl;
glGetSeparableFilter: procedure(target, format, _type: GLenum; row, column, span: PGLvoid); cdecl;
glGetString: function(name: GLenum): PChar; cdecl;
glGetTexEnvfv: procedure(target, pname: GLenum; params : PSingle); cdecl;
glGetTexEnviv: procedure(target, pname: GLenum; params : PLongInt); cdecl;
glGetTexGendv: procedure(cord, pname: GLenum; params : PDouble); cdecl;
glGetTexGenfv: procedure(cord, pname: GLenum; params : PSingle); cdecl;
glGetTexGeniv: procedure(cord, pname: GLenum; params : PLongInt); cdecl;
glGetTexImage: procedure(target: GLenum; level: LongInt; format, _Type: GLenum; var pixels); cdecl;
glGetTexParameterfv: procedure(target, pname: GLenum; params : PSingle); cdecl;
glGetTexParameteriv: procedure(target, pname: GLenum; params : PLongInt); cdecl;
glGetTexLevelParameterfv: procedure(target: GLenum; level: LongInt; pname: GLenum; params : PSingle); cdecl;
glGetTexLevelParameteriv: procedure(target: GLenum; level: LongInt; pname: GLenum; params : PLongInt); cdecl;
glGetTrackMatrixivNV: procedure(target: GLenum; address: GLuint; pname: GLenum; params: PGLint); cdecl;
glGetVertexAttribPointervNV: procedure(index: GLuint; pname: GLenum; var _pointer: pointer); cdecl;
glGetVertexAttribdvNV: procedure(index: GLuint; pname: GLenum; params: PGLdouble); cdecl;
glGetVertexAttribfvNV: procedure(index: GLuint; pname: GLenum; params: PGLfloat); cdecl;
glGetVertexAttribivNV: procedure(index: GLuint; pname: GLenum; params: PGLint); cdecl;
glHint: procedure(target, mode: GLenum); cdecl;
glHistogram: procedure(target: GLenum; width: GLsizei; internalformat: GLenum; sink: GLboolean); cdecl;
glIndexMask: procedure(mask: LongWord); cdecl;
glIndexPointer: procedure(_Type: GLenum; stride: LongInt; var ptr); cdecl;
glIndexPointerEXT: procedure(_Type: GLenum; stride, count: LongInt; var ptr); cdecl;
glIndexd: procedure(c: Double); cdecl;
glIndexdv: procedure(var c: Double); cdecl;
glIndexf: procedure(c: Single); cdecl;
glIndexfv: procedure(var c: Single); cdecl;
glIndexi: procedure(c: LongInt); cdecl;
glIndexiv: procedure(var c: LongInt); cdecl;
glIndexs: procedure(c: SmallInt); cdecl;
glIndexsv: procedure(var c: SmallInt); cdecl;
glIndexub: procedure(c: Byte); cdecl;
glIndexubv: procedure(var c: Byte); cdecl;
glInitNames: procedure; cdecl;
glInterleavedArrays: procedure(format: GLenum; stride: LongInt; var pointer); cdecl;
glIsEnabled: function(cap: GLenum): GLBoolean; cdecl;
glIsFenceNV: function(fence: GLuint): GLBoolean; cdecl;
glIsList: function(list: LongWord): GLBoolean; cdecl;
glIsOcclusionQueryNV: function(id: GLuint): GLBoolean; cdecl;
glIsProgramNV: function(id: GLuint): GLBoolean; cdecl;
glIsTexture: function(texture: LongWord): Boolean; cdecl;
glIsTextureEXT: function(texture: LongWord): Boolean; cdecl;
glLightModelf: procedure(pname: GLenum; param: Single); cdecl;
glLightModelfv: procedure(pname: GLenum; params : PSingle); cdecl;
glLightModeli: procedure(pname: GLenum; param: LongInt); cdecl;
glLightModeliv: procedure(pname: GLenum; params : PLongInt); cdecl;
glLightf: procedure(light, pname: GLenum; param: Single); cdecl;
glLightfv: procedure(light, pname: GLenum; params : PSingle); cdecl;
glLighti: procedure(light, pname: GLenum; param: LongInt); cdecl;
glLightiv: procedure(light, pname: GLenum; params : PLongInt); cdecl;
glLineStipple: procedure(factor: LongInt; pattern: Word); cdecl;
glLineWidth: procedure(width: Single); cdecl;
glListBase: procedure(base: LongWord); cdecl;
glLoadIdentity: procedure; cdecl;
glLoadMatrixd: procedure(m: PGLDouble); cdecl;
glLoadMatrixf: procedure(m: PGLFloat); cdecl;
glLoadName: procedure(name: LongWord); cdecl;
glLoadProgramNV: procedure(target: GLenum; id: GLuint; len: GLsizei; const _program: PGLubyte); cdecl;
glLoadTransposeMatrixd: procedure(const m: PGLdouble); cdecl;
glLoadTransposeMatrixdARB: procedure(const m: PGLdouble); cdecl;
glLoadTransposeMatrixf: procedure(const m: PGLfloat); cdecl;
glLoadTransposeMatrixfARB: procedure(const m: PGLfloat); cdecl;
glLockArraysEXT: procedure(first: GLint; count: GLsizei); cdecl;
glLogicOp: procedure(opcode: GLenum); cdecl;
glMap1d: procedure(target: GLenum; u1, u2: Double; stride, order: LongInt; var points: Double); cdecl;
glMap1f: procedure(target: GLenum; u1, u2: Single; stride, order: LongInt; var points: Single); cdecl;
glMap2d: procedure(target: GLenum; u1, u2: Double; ustride, uorder: LongInt; v1, v2: Double; vstride, vorder: LongInt; var points: Double); cdecl;
glMap2f: procedure(target: GLenum; u1, u2: Single; ustride, uorder: LongInt; v1, v2: Single; vstride, vorder: LongInt; var points: Single); cdecl;
glMapControlPointsNV: procedure(target: GLenum; index: GLuint; _type: GLenum; ustride, vstride: GLsizei; uorder, vorder: GLint; _packed: GLboolean; const points: PGLvoid); cdecl;
glMapGrid1d: procedure(un: LongInt; u1, u2: Double); cdecl;
glMapGrid1f: procedure(un: LongInt; u1, u2: Single); cdecl;
glMapGrid2d: procedure(un: LongInt; u1, u2: Double; vn: LongInt; v1, v2: Double); cdecl;
glMapGrid2f: procedure(un: LongInt; u1, u2: Single; vn: LongInt; v1, v2: Single); cdecl;
glMapParameterfvNV: procedure(target, pname: GLenum; const params: PGLfloat); cdecl;
glMapParameterivNV: procedure(target, pname: GLenum; const params: PGLint); cdecl;
glMaterialf: procedure(face, pname: GLenum; param: Single); cdecl;
glMaterialfv: procedure(face, pname: GLenum; params : PSingle); cdecl;
glMateriali: procedure(face, pname: GLenum; param: LongInt); cdecl;
glMaterialiv: procedure(face, pname: GLenum; params : PLongInt); cdecl;
glMatrixMode: procedure(mode: GLenum); cdecl;
glMinmax: procedure(target, internalformat: GLenum; sink: GLboolean); cdecl;
glMultMatrixd: procedure(m: PGLDouble); cdecl;
glMultMatrixf: procedure(m: PGLFloat); cdecl;
glMultTransposeMatrixd: procedure(const m: PGLdouble); cdecl;
glMultTransposeMatrixdARB: procedure(const m: PGLdouble); cdecl;
glMultTransposeMatrixf: procedure(const m: PGLfloat); cdecl;
glMultTransposeMatrixfARB: procedure(const m: PGLfloat); cdecl;
glMultiDrawArraysEXT: procedure(mode: GLenum; const first: PGLint; const count: PGLsizei; primcount: GLsizei); cdecl;
glMultiDrawElementsEXT: procedure(mode: GLenum; const count: PGLsizei; _type: GLenum; const indices: PGLVoid; primcount: GLsizei); cdecl;
glMultiTexCoord1d: procedure(target: GLenum; s: GLdouble); cdecl;
glMultiTexCoord1dARB: procedure(target: GLenum; s: GLdouble); cdecl;
glMultiTexCoord1dSGIS: procedure(target: GLenum; s: Double); cdecl;
glMultiTexCoord1dv: procedure(target: GLenum; const v: PGLdouble); cdecl;
glMultiTexCoord1dvARB: procedure(target: GLenum; const v: PGLdouble); cdecl;
glMultiTexCoord1dvSGIS: procedure(target: GLenum; var v: Double); cdecl;
glMultiTexCoord1f: procedure(target: GLenum; s: GLfloat); cdecl;
glMultiTexCoord1fARB: procedure(target: GLenum; s: GLfloat); cdecl;
glMultiTexCoord1fSGIS: procedure(target: GLenum; s: Single); cdecl;
glMultiTexCoord1fv: procedure(target: GLenum; const v: PGLfloat); cdecl;
glMultiTexCoord1fvARB: procedure(target: GLenum; const v: PGLfloat); cdecl;
glMultiTexCoord1fvSGIS: procedure(target: GLenum; var v: Single); cdecl;
glMultiTexCoord1i: procedure(target: GLenum; s: GLint); cdecl;
glMultiTexCoord1iARB: procedure(target: GLenum; s: GLint); cdecl;
glMultiTexCoord1iSGIS: procedure(target: GLenum; s: LongInt); cdecl;
glMultiTexCoord1iv: procedure(target: GLenum; const v: PGLint); cdecl;
glMultiTexCoord1ivARB: procedure(target: GLenum; const v: PGLint); cdecl;
glMultiTexCoord1ivSGIS: procedure(target: GLenum; var v: LongInt); cdecl;
glMultiTexCoord1s: procedure(target: GLenum; s: GLshort); cdecl;
glMultiTexCoord1sARB: procedure(target: GLenum; s: GLshort); cdecl;
glMultiTexCoord1sSGIS: procedure(target: GLenum; s: ShortInt); cdecl;
glMultiTexCoord1sv: procedure(target: GLenum; const v: PGLshort); cdecl;
glMultiTexCoord1svARB: procedure(target: GLenum; const v: PGLshort); cdecl;
glMultiTexCoord1svSGIS: procedure(target: GLenum; var v: ShortInt); cdecl;
glMultiTexCoord2d: procedure(target: GLenum; s, t: GLdouble); cdecl;
glMultiTexCoord2dARB: procedure(target: GLenum; s, t: GLdouble); cdecl;
glMultiTexCoord2dSGIS: procedure(target: GLenum; s, t: Double); cdecl;
glMultiTexCoord2dv: procedure(target: GLenum; const v: PGLdouble); cdecl;
glMultiTexCoord2dvARB: procedure(target: GLenum; const v: PGLdouble); cdecl;
glMultiTexCoord2dvSGIS: procedure(target: GLenum; var v: Double); cdecl;
glMultiTexCoord2f: procedure(target: GLenum; s, t: GLfloat); cdecl;
glMultiTexCoord2fARB: procedure(target: GLenum; s, t: GLfloat); cdecl;
glMultiTexCoord2fSGIS: procedure(target: GLenum; s, t: Single); cdecl;
glMultiTexCoord2fv: procedure(target: GLenum; const v: PGLfloat); cdecl;
glMultiTexCoord2fvARB: procedure(target: GLenum; const v: PGLfloat); cdecl;
glMultiTexCoord2fvSGIS: procedure(target: GLenum; var v: Single); cdecl;
glMultiTexCoord2i: procedure(target: GLenum; s, t: GLint); cdecl;
glMultiTexCoord2iARB: procedure(target: GLenum; s, t: GLint); cdecl;
glMultiTexCoord2iSGIS: procedure(target: GLenum; s, t: LongInt); cdecl;
glMultiTexCoord2iv: procedure(target: GLenum; const v: PGLint); cdecl;
glMultiTexCoord2ivARB: procedure(target: GLenum; const v: PGLint); cdecl;
glMultiTexCoord2ivSGIS: procedure(target: GLenum; var v: LongInt); cdecl;
glMultiTexCoord2s: procedure(target: GLenum; s, t: GLshort); cdecl;
glMultiTexCoord2sARB: procedure(target: GLenum; s, t: GLshort); cdecl;
glMultiTexCoord2sSGIS: procedure(target: GLenum; s, t: ShortInt); cdecl;
glMultiTexCoord2sv: procedure(target: GLenum; const v: PGLshort); cdecl;
glMultiTexCoord2svARB: procedure(target: GLenum; const v: PGLshort); cdecl;
glMultiTexCoord2svSGIS: procedure(target: GLenum; var v: ShortInt); cdecl;
glMultiTexCoord3d: procedure(target: GLenum; s, t, r: GLdouble); cdecl;
glMultiTexCoord3dARB: procedure(target: GLenum; s, t, r: GLdouble); cdecl;
glMultiTexCoord3dSGIS: procedure(target: GLenum; s, t, r: Double); cdecl;
glMultiTexCoord3dv: procedure(target: GLenum; const v: PGLdouble); cdecl;
glMultiTexCoord3dvARB: procedure(target: GLenum; const v: PGLdouble); cdecl;
glMultiTexCoord3dvSGIS: procedure(target: GLenum; var v: Double); cdecl;
glMultiTexCoord3f: procedure(target: GLenum; s, t, r: GLfloat); cdecl;
glMultiTexCoord3fARB: procedure(target: GLenum; s, t, r: GLfloat); cdecl;
glMultiTexCoord3fSGIS: procedure(target: GLenum; s, t, r: Single); cdecl;
glMultiTexCoord3fv: procedure(target: GLenum; const v: PGLfloat); cdecl;
glMultiTexCoord3fvARB: procedure(target: GLenum; const v: PGLfloat); cdecl;
glMultiTexCoord3fvSGIS: procedure(target: GLenum; var v: Single); cdecl;
glMultiTexCoord3i: procedure(target: GLenum; s, t, r:  GLint); cdecl;
glMultiTexCoord3iARB: procedure(target: GLenum; s, t, r:  GLint); cdecl;
glMultiTexCoord3iSGIS: procedure(target: GLenum; s, t, r: LongInt); cdecl;
glMultiTexCoord3iv: procedure(target: GLenum; const v: PGLint); cdecl;
glMultiTexCoord3ivARB: procedure(target: GLenum; const v: PGLint); cdecl;
glMultiTexCoord3ivSGIS: procedure(target: GLenum; var v: LongInt); cdecl;
glMultiTexCoord3s: procedure(target: GLenum; s, t, r: GLshort); cdecl;
glMultiTexCoord3sARB: procedure(target: GLenum; s, t, r: GLshort); cdecl;
glMultiTexCoord3sSGIS: procedure(target: GLenum; s, t, r: ShortInt); cdecl;
glMultiTexCoord3sv: procedure(target: GLenum; const v: PGLshort); cdecl;
glMultiTexCoord3svARB: procedure(target: GLenum; const v: PGLshort); cdecl;
glMultiTexCoord3svSGIS: procedure(target: GLenum; var v: ShortInt); cdecl;
glMultiTexCoord4d: procedure(target: GLenum; s, t, r, q: GLdouble); cdecl;
glMultiTexCoord4dARB: procedure(target: GLenum; s, t, r, q: GLdouble); cdecl;
glMultiTexCoord4dSGIS: procedure(target: GLenum; s, t, r, q: Double); cdecl;
glMultiTexCoord4dv: procedure(target: GLenum; const v: PGLdouble); cdecl;
glMultiTexCoord4dvARB: procedure(target: GLenum; const v: PGLdouble); cdecl;
glMultiTexCoord4dvSGIS: procedure(target: GLenum; var v: Double); cdecl;
glMultiTexCoord4f: procedure(target: GLenum; s, t, r, q: GLfloat); cdecl;
glMultiTexCoord4fARB: procedure(target: GLenum; s, t, r, q: GLfloat); cdecl;
glMultiTexCoord4fSGIS: procedure(target: GLenum; s, t, r, q: Single); cdecl;
glMultiTexCoord4fv: procedure(target: GLenum; const v: PGLfloat); cdecl;
glMultiTexCoord4fvARB: procedure(target: GLenum; const v: PGLfloat); cdecl;
glMultiTexCoord4fvSGIS: procedure(target: GLenum; var v: Single); cdecl;
glMultiTexCoord4i: procedure(target: GLenum; s, t, r, q: GLint); cdecl;
glMultiTexCoord4iARB: procedure(target: GLenum; s, t, r, q: GLint); cdecl;
glMultiTexCoord4iSGIS: procedure(target: GLenum; s, t, r, q: LongInt); cdecl;
glMultiTexCoord4iv: procedure(target: GLenum; const v: PGLint); cdecl;
glMultiTexCoord4ivARB: procedure(target: GLenum; const v: PGLint); cdecl;
glMultiTexCoord4ivSGIS: procedure(target: GLenum; var v: LongInt); cdecl;
glMultiTexCoord4s: procedure(target: GLenum; s, t, r, q: GLshort); cdecl;
glMultiTexCoord4sARB: procedure(target: GLenum; s, t, r, q: GLshort); cdecl;
glMultiTexCoord4sSGIS: procedure(target: GLenum; s, t, r, q: ShortInt); cdecl;
glMultiTexCoord4sv: procedure(target: GLenum; const v: PGLshort); cdecl;
glMultiTexCoord4svARB: procedure(target: GLenum; const v: PGLshort); cdecl;
glMultiTexCoord4svSGIS: procedure(target: GLenum; var v: ShortInt); cdecl;
glMultiTexCoordPointerSGIS: procedure(target: GLenum; size: LongInt; _Type: GLEnum; stride: LongInt; var _Pointer); cdecl;
glNewList: procedure(list: LongWord; mode: GLenum); cdecl;
glNormal3b: procedure(nx, ny, nz: Byte); cdecl;
glNormal3bv: procedure(var v: ShortInt); cdecl;
glNormal3d: procedure(nx, ny, nz: Double); cdecl;
glNormal3dv: procedure(var v: Double); cdecl;
glNormal3f: procedure(nx, ny, nz: Single); cdecl;
glNormal3fv: procedure(var v: Single); cdecl;
glNormal3i: procedure(nx, ny, nz: LongInt); cdecl;
glNormal3iv: procedure(var v: LongInt); cdecl;
glNormal3s: procedure(nx, ny, nz: SmallInt); cdecl;
glNormal3sv: procedure(var v: SmallInt); cdecl;
glNormalPointer: procedure(_Type: GLenum; stride: LongInt; var ptr); cdecl;
glNormalPointerEXT: procedure(_Type: GLenum; stride, count: LongInt; var ptr); cdecl;
glOrtho: procedure(left, right, bottom, top, near_val, far_val: Double); cdecl;
glPassThrough: procedure(token: Single); cdecl;
glPixelDataRangeNV: procedure(target: GLenum; size: GLsizei; const pointer: PGLvoid); cdecl;
glPixelMapfv: procedure(map: GLenum; mapsize: LongInt; var values: Single); cdecl;
glPixelMapuiv: procedure(map: GLenum; mapsize: LongInt; var values: LongWord); cdecl;
glPixelMapusv: procedure(map: GLenum; mapsize: LongInt; var values: Word); cdecl;
glPixelStoref: procedure(pname: GLenum; param: Single); cdecl;
glPixelStorei: procedure(pname: GLenum; param: LongInt); cdecl;
glPixelTransferf: procedure(pname: GLenum; param: Single); cdecl;
glPixelTransferi: procedure(pname: GLenum; param: LongInt); cdecl;
glPixelZoom: procedure(xfactor, yfactor: Single); cdecl;
glPointParameteriNV: procedure(pname: GLenum; param: GLint); cdecl;
glPointParameterivNV: procedure(pname: GLenum; const params: PGLint); cdecl;
glPointParameterfEXT: procedure(pname: GLenum; param: Single); cdecl;
glPointParameterfvEXT: procedure(pname: GLenum; var params: Single); cdecl;
glPointSize: procedure(size: Single); cdecl;
glPolygonMode: procedure(face, mode: GLenum); cdecl;
glPolygonOffset: procedure(factor, units: Single); cdecl;
glPolygonStipple: procedure(var mask: Byte); cdecl;
glPopAttrib: procedure; cdecl;
glPopClientAttrib: procedure; cdecl;
glPopMatrix: procedure; cdecl;
glPopName: procedure; cdecl;
glPrioritizeTextures: procedure(n: LongInt; var textures: LongWord; var priorities: GLclampf); cdecl;
glPrioritizeTexturesEXT: procedure(n: LongInt; var textures: LongWord; var priorities: GLClampf); cdecl;
glProgramLocalParameter4dNV: procedure(target: GLenum; len: GLsizei; const name: PGLubyte; x, y, z, w: GLdouble); cdecl;
glProgramLocalParameter4dvNV: procedure(target: GLenum; len: GLsizei; const name: PGLubyte; const v: PGLdouble); cdecl;
glProgramLocalParameter4fNV: procedure(target: GLenum; len: GLsizei; const name: PGLubyte; x, y, z, w: GLfloat); cdecl;
glProgramLocalParameter4fvNV: procedure(target: GLenum; len: GLsizei; const name: PGLubyte; const v: PGLfloat); cdecl;
glProgramParameter4dNV: procedure(target: GLenum; index: GLuint; x, y, z, w: GLdouble); cdecl;
glProgramParameter4dvNV: procedure(target: GLenum; index: GLuint; const v: PGLdouble); cdecl;
glProgramParameter4fNV: procedure(target: GLenum; index: GLuint; x, y, z, w: GLfloat); cdecl;
glProgramParameter4fvNV: procedure(target: GLenum; index: GLuint; const v: PGLfloat); cdecl;
glProgramParameterSigned4dNV: procedure(target: GLenum; index: GLint; x, y, z, w: GLdouble);
glProgramParameterSigned4dvNV: procedure(target: GLenum; index: GLint; const v: PGLdouble);
glProgramParameterSigned4fNV: procedure(target: GLenum; index: GLint; x, y, z, w: GLfloat);
glProgramParameterSigned4fvNV: procedure(target: GLenum; index: GLint; const v: PGLfloat);
glProgramParameters4dvNV: procedure(target: GLenum; index: GLuint; count: GLsizei; const v: PGLdouble); cdecl;
glProgramParameters4fvNV: procedure(target: GLenum; index: GLuint; count: GLsizei; const v: PGLfloat); cdecl;
glProgramParametersSigned4dvNV: procedure(target: GLenum; index: GLint; count: GLsizei; const v: PGLdouble);
glProgramParametersSigned4fvNV: procedure(target: GLenum; index: GLint; count: GLsizei; const v: PGLfloat);
glPushAttrib: procedure(mask: GLbitfield); cdecl;
glPushClientAttrib: procedure(mask: GLbitfield); cdecl;
glPushMatrix: procedure; cdecl;
glPushName: procedure(name: LongWord); cdecl;
glRasterPos2d: procedure(x, y: Double); cdecl;
glRasterPos2dv: procedure(var v: Double); cdecl;
glRasterPos2f: procedure(x, y: Single); cdecl;
glRasterPos2fv: procedure(var v: Single); cdecl;
glRasterPos2i: procedure(x, y: LongInt); cdecl;
glRasterPos2iv: procedure(var v: LongInt); cdecl;
glRasterPos2s: procedure(x, y: SmallInt); cdecl;
glRasterPos2sv: procedure(var v: SmallInt); cdecl;
glRasterPos3d: procedure(x, y, z: Double); cdecl;
glRasterPos3dv: procedure(var v: Double); cdecl;
glRasterPos3f: procedure(x, y, z: Single); cdecl;
glRasterPos3fv: procedure(var v: Single); cdecl;
glRasterPos3i: procedure(x, y, z: LongInt); cdecl;
glRasterPos3iv: procedure(var v: LongInt); cdecl;
glRasterPos3s: procedure(x, y, z: SmallInt); cdecl;
glRasterPos3sv: procedure(var v: SmallInt); cdecl;
glRasterPos4d: procedure(x, y, z, w: Double); cdecl;
glRasterPos4dv: procedure(var v: Double); cdecl;
glRasterPos4f: procedure(x, y, z, w: Single); cdecl;
glRasterPos4fv: procedure(var v: Single); cdecl;
glRasterPos4i: procedure(x, y, z, w: LongInt); cdecl;
glRasterPos4iv: procedure(var v: LongInt); cdecl;
glRasterPos4s: procedure(x, y, z, w: SmallInt); cdecl;
glRasterPos4sv: procedure(var v: SmallInt); cdecl;
glReadBuffer: procedure(mode: GLenum); cdecl;
glReadPixels: procedure(x, y, width, height: LongInt; format, _Type: GLenum; var pixels); cdecl;
glRectd: procedure(x1, y1, x2, y2: Double); cdecl;
glRectf: procedure(x1, y1, x2, y2: Single); cdecl;
glRecti: procedure(x1, y1, x2, y2: LongInt); cdecl;
glRects: procedure(x1, y1, x2, y2: SmallInt); cdecl;
glRectdv: procedure(var v1, v2: Double); cdecl;
glRectfv: procedure(var v1, v2: Single); cdecl;
glRectiv: procedure(var v1, v2: LongInt); cdecl;
glRectsv: procedure(var v1, v2: SmallInt); cdecl;
glReleaseFlushHold: function(const id: PGLvoid): GLenum; cdecl;
glRenderMode: function(mode: GLenum): LongInt; cdecl;
glRequestResidentProgramsNV: procedure(n: GLsizei; const programs: PGLuint); cdecl;
glResetHistogram: procedure(target: GLenum); cdecl;
glResetMinmax: procedure(target: GLenum); cdecl;
glRotated: procedure(angle, x, y, z: Double); cdecl;
glRotatef: procedure(angle, x, y, z: Single); cdecl;
glSampleCoverage: procedure(value: GLclampf; invert: GLboolean); cdecl;
glSampleCoverageARB: procedure(value: GLclampf; invert: GLboolean); cdecl;
glScaled: procedure(x, y, z: Double); cdecl;
glScalef: procedure(x, y, z: Single); cdecl;
glScissor: procedure(x, y, width, height: LongInt); cdecl;
glSecondaryColor3bEXT: procedure(red, green, blue: GLbyte); cdecl;
glSecondaryColor3bvEXT: procedure(const v: PGLbyte); cdecl;
glSecondaryColor3dEXT: procedure(red, green, blue: GLdouble); cdecl;
glSecondaryColor3dvEXT: procedure(const v: PGLdouble); cdecl;
glSecondaryColor3fEXT: procedure(red, green, blue: GLfloat); cdecl;
glSecondaryColor3fvEXT: procedure(const v: PGLfloat); cdecl;
glSecondaryColor3iEXT: procedure(red, green, blue: GLint); cdecl;
glSecondaryColor3ivEXT: procedure(const v: PGLint); cdecl;
glSecondaryColor3sEXT: procedure(red, green, blue: GLshort); cdecl;
glSecondaryColor3svEXT: procedure(const v: PGLshort); cdecl;
glSecondaryColor3ubEXT: procedure(red, green, blue: GLubyte); cdecl;
glSecondaryColor3ubvEXT: procedure(const v: PGLubyte); cdecl;
glSecondaryColor3uiEXT: procedure(red, green, blue: GLuint); cdecl;
glSecondaryColor3uivEXT: procedure(const v: PGLuint); cdecl;
glSecondaryColor3usEXT: procedure(red, green, blue: GLushort); cdecl;
glSecondaryColor3usvEXT: procedure(const v: PGLushort); cdecl;
glSecondaryColorPointerEXT: procedure(size: GLint; _type: GLenum; stride: GLsizei; const pointer: PGLvoid); cdecl;
glSelectBuffer: procedure(size: LongInt; var buffer: LongWord); cdecl;
glSelectTextureSGIS: procedure(target: GLenum); cdecl;
glSelectTextureCoordSetSGIS: procedure(target: GLenum); cdecl;
glSeparableFilter2D: procedure(target, internalformat: GLenum; width, height: GLsizei; format, _type: GLenum; const row, column: PGLvoid); cdecl;
glSetFenceNV: procedure(fence: GLuint; condition: GLenum); cdecl;
glSetWindowStereoModeNV: procedure(displayMode: GLboolean); cdecl;
glShadeModel: procedure(mode: GLenum); cdecl;
glStencilFunc: procedure(func: GLenum; ref: LongInt; mask: LongWord); cdecl;
glStencilMask: procedure(mask: LongWord); cdecl;
glStencilOp: procedure(fail, zfail, zpass: GLenum); cdecl;
glTbufferMask3DFX: procedure(mask :GLuint); cdecl;
glTestFenceNV: function(fence: GLuint): GLboolean; cdecl;
glTexCoord1d: procedure(s: Double); cdecl;
glTexCoord1dv: procedure(var v: Double); cdecl;
glTexCoord1f: procedure(s: Single); cdecl;
glTexCoord1fv: procedure(var v: Single); cdecl;
glTexCoord1i: procedure(s: LongInt); cdecl;
glTexCoord1iv: procedure(var v: LongInt); cdecl;
glTexCoord1s: procedure(s: SmallInt); cdecl;
glTexCoord1sv: procedure(var v: SmallInt); cdecl;
glTexCoord2d: procedure(s, t: Double); cdecl;
glTexCoord2dv: procedure(var v: Double); cdecl;
glTexCoord2f: procedure(s, t: Single); cdecl;
glTexCoord2fv: procedure(var v: Single); cdecl;
glTexCoord2i: procedure(s, t: LongInt); cdecl;
glTexCoord2iv: procedure(var v: LongInt); cdecl;
glTexCoord2s: procedure(s, t: SmallInt); cdecl;
glTexCoord2sv: procedure(var v: SmallInt); cdecl;
glTexCoord3d: procedure(s, t, r: Double); cdecl;
glTexCoord3dv: procedure(var v: Double); cdecl;
glTexCoord3f: procedure(s, t, r: Single); cdecl;
glTexCoord3fv: procedure(var v: Single); cdecl;
glTexCoord3i: procedure(s, t, r: LongInt); cdecl;
glTexCoord3iv: procedure(var v: LongInt); cdecl;
glTexCoord3s: procedure(s, t, r: SmallInt); cdecl;
glTexCoord3sv: procedure(var v: SmallInt); cdecl;
glTexCoord4d: procedure(s, t, r, q: Double); cdecl;
glTexCoord4dv: procedure(var v: Double); cdecl;
glTexCoord4f: procedure(s, t, r, q: Single); cdecl;
glTexCoord4fv: procedure(var v: Single); cdecl;
glTexCoord4i: procedure(s, t, r, q: LongInt); cdecl;
glTexCoord4iv: procedure(var v: LongInt); cdecl;
glTexCoord4s: procedure(s, t, r, q: SmallInt); cdecl;
glTexCoord4sv: procedure(var v: SmallInt); cdecl;
glTexCoordPointer: procedure(size: LongInt; _Type: GLenum; stride: LongInt; var ptr); cdecl;
glTexCoordPointerEXT: procedure(size: LongInt; _Type: GLenum; stride, count: LongInt; var ptr); cdecl;
glTexEnvf: procedure(target, pname: GLenum; param: Single); cdecl;
glTexEnvfv: procedure(target, pname: GLenum; params : PSingle); cdecl;
glTexEnvi: procedure(target, pname: GLenum; param: LongInt); cdecl;
glTexEnviv: procedure(target, pname: GLenum; params : PLongInt); cdecl;
glTexGend: procedure(cord, pname: GLenum; param: Double); cdecl;
glTexGendv: procedure(cord, pname: GLenum; params : PDouble); cdecl;
glTexGenf: procedure(cord, pname: GLenum; param: Single); cdecl;
glTexGenfv: procedure(cord, pname: GLenum; params : PSingle); cdecl;
glTexGeni: procedure(cord, pname: GLenum; param: LongInt); cdecl;
glTexGeniv: procedure(cord, pname: GLenum; params : PLongInt); cdecl;
glTexImage1D: procedure(target: GLenum; level, internalFormat, width, border: LongInt; format, _Type: GLenum; var pixels); cdecl;
glTexImage2D: procedure(target: GLenum; level, internalFormat, width, height, border: LongInt; format, _Type: GLenum; var pixels); cdecl;
glTexImage3D: procedure(target: GLenum; level: LongInt; internalFormat: GLenum; width, height, depth, border: LongInt; format, _Type: GLEnum; var pixels); cdecl;
glTexImage3DEXT: procedure(target: GLenum; level: LongInt; internalFormat: GLenum; width, height, depth, border: LongInt; format, _Type: GLEnum; var pixels); cdecl;
glTexParameterf: procedure(target, pname: GLenum; param: Single); cdecl;
glTexParameterfv: procedure(target, pname: GLenum; params : PSingle); cdecl;
glTexParameteri: procedure(target, pname: GLenum; param: LongInt); cdecl;
glTexParameteriv: procedure(target, pname: GLenum; params : PLongInt); cdecl;
glTexSubImage1D: procedure(target: GLenum; level, xoffset, width: LongInt; format, _Type: GLenum; var pixels); cdecl;
glTexSubImage2D: procedure(target: GLenum; level, xoffset, yoffset, width, height: LongInt; format, _Type: GLenum; var pixels); cdecl;
glTexSubImage3D: procedure(target: GLenum; level: LongInt; xoffset, yoffset, zoffset, width, height, depth: LongInt; format, _Type: GLEnum; var pixels); cdecl;
glTexSubImage3DEXT: procedure(target: GLenum; level: LongInt; xoffset, yoffset, zoffset, width, height, depth: LongInt; format, _Type: GLEnum; var pixels); cdecl;
glTrackMatrixNV: procedure(target: GLenum; address: GLuint; matrix, transform: GLenum); cdecl;
glTranslated: procedure(x, y, z: Double); cdecl;
glTranslatef: procedure(x, y, z: Single); cdecl;
glUnlockArraysEXT: procedure; cdecl;
glValidBackBufferHintAutodesk: function(x, y: GLint; width, height: GLsizei): GLboolean; cdecl;
glVertex2d: procedure(x, y: Double); cdecl;
glVertex2dv: procedure(var v: Double); cdecl;
glVertex2f: procedure(x, y: Single); cdecl;
glVertex2fv: procedure(var v: Single); cdecl;
glVertex2i: procedure(x, y: LongInt); cdecl;
glVertex2iv: procedure(var v: LongInt); cdecl;
glVertex2s: procedure(x, y: SmallInt); cdecl;
glVertex2sv: procedure(var v: SmallInt); cdecl;
glVertex3d: procedure(x, y, z: Double); cdecl;
glVertex3dv: procedure(var v: Double); cdecl;
glVertex3f: procedure(x, y, z: Single); cdecl;
glVertex3fv: procedure(var v: Single); cdecl;
glVertex3i: procedure(x, y, z: LongInt); cdecl;
glVertex3iv: procedure(var v: LongInt); cdecl;
glVertex3s: procedure(x, y, z: SmallInt); cdecl;
glVertex3sv: procedure(var v: SmallInt); cdecl;
glVertex4d: procedure(x, y, z, w: Double); cdecl;
glVertex4dv: procedure(var v: Double); cdecl;
glVertex4f: procedure(x, y, z, w: Single); cdecl;
glVertex4fv: procedure(var v: Single); cdecl;
glVertex4i: procedure(x, y, z, w: LongInt); cdecl;
glVertex4iv: procedure(var v: LongInt); cdecl;
glVertex4s: procedure(x, y, z, w: SmallInt); cdecl;
glVertex4sv: procedure(var v: SmallInt); cdecl;
glVertexArrayRangeNV: procedure(size: GLsizei; const pointer: PGLvoid); cdecl;
glVertexAttrib1dNV: procedure(index: GLuint; x: GLdouble); cdecl;
glVertexAttrib1dvNV: procedure(index: GLuint; const v: PGLdouble); cdecl;
glVertexAttrib1fNV: procedure(index: GLuint; x: GLfloat); cdecl;
glVertexAttrib1fvNV: procedure(index: GLuint; const v: PGLfloat); cdecl;
glVertexAttrib1sNV: procedure(index: GLuint; x: GLshort); cdecl;
glVertexAttrib1svNV: procedure(index: GLuint; const v: PGLshort); cdecl;
glVertexAttrib2dNV: procedure(index: GLuint; x, y: GLdouble); cdecl;
glVertexAttrib2dvNV: procedure(index: GLuint; const v: PGLdouble); cdecl;
glVertexAttrib2fNV: procedure(index: GLuint; x, y: GLfloat); cdecl;
glVertexAttrib2fvNV: procedure(index: GLuint; const v: PGLfloat); cdecl;
glVertexAttrib2sNV: procedure(index: GLuint; x, y: GLshort); cdecl;
glVertexAttrib2svNV: procedure(index: GLuint; const v: PGLshort); cdecl;
glVertexAttrib3dNV: procedure(index: GLuint; x, y, z: GLdouble); cdecl;
glVertexAttrib3dvNV: procedure(index: GLuint; const v: PGLdouble); cdecl;
glVertexAttrib3fNV: procedure(index: GLuint; x, y, z: GLfloat); cdecl;
glVertexAttrib3fvNV: procedure(index: GLuint; const v: PGLfloat); cdecl;
glVertexAttrib3sNV: procedure(index: GLuint; x, y, z: GLshort); cdecl;
glVertexAttrib3svNV: procedure(index: GLuint; const v: PGLshort); cdecl;
glVertexAttrib4dNV: procedure(index: GLuint; x, y, z, w: GLdouble); cdecl;
glVertexAttrib4dvNV: procedure(index: GLuint; const v: PGLdouble); cdecl;
glVertexAttrib4fNV: procedure(index: GLuint; x, y, z, w: GLfloat); cdecl;
glVertexAttrib4fvNV: procedure(index: GLuint; const v: PGLfloat); cdecl;
glVertexAttrib4sNV: procedure(index: GLuint; x, y, z, w: GLshort); cdecl;
glVertexAttrib4svNV: procedure(index: GLuint; const v: PGLshort); cdecl;
glVertexAttrib4ubNV: procedure(index: GLuint; x, y, z, w: GLubyte); cdecl;
glVertexAttrib4ubvNV: procedure(index: GLuint; const v: PGLubyte); cdecl;
glVertexAttribPointerNV: procedure(index: GLuint; fsize: GLint; _type: GLenum; stride: GLsizei; const pointer: PGLvoid); cdecl;
glVertexAttribs1dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); cdecl;
glVertexAttribs1fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); cdecl;
glVertexAttribs1svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); cdecl;
glVertexAttribs2dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); cdecl;
glVertexAttribs2fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); cdecl;
glVertexAttribs2svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); cdecl;
glVertexAttribs3dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); cdecl;
glVertexAttribs3fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); cdecl;
glVertexAttribs3svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); cdecl;
glVertexAttribs4dvNV: procedure(index: GLuint; count: GLsizei; const v: PGLdouble); cdecl;
glVertexAttribs4fvNV: procedure(index: GLuint; count: GLsizei; const v: PGLfloat); cdecl;
glVertexAttribs4svNV: procedure(index: GLuint; count: GLsizei; const v: PGLshort); cdecl;
glVertexAttribs4ubvNV: procedure(index:GLuint; count: GLsizei; const v: PGLubyte); cdecl;
glVertexPointer: procedure(size: LongInt; _Type: GLenum; stride: LongInt; var ptr); cdecl;
glVertexPointerEXT: procedure(size: LongInt; _Type: GLenum; stride, count: LongInt; var ptr); cdecl;
glVertexWeightPointerEXT: procedure(size: GLsizei; _type: GLenum; stride: GLsizei; const pointer: PGLvoid); cdecl;
glVertexWeightfEXT: procedure(weight: GLfloat); cdecl;
glVertexWeightfvEXT: procedure(const weight: PGLfloat); cdecl;
glViewport: procedure(x, y, width, height: LongInt); cdecl;
glWindowBackBufferHintAutodesk: procedure; cdecl;
glWindowPos2dARB: procedure(x, y: GLdouble); cdecl;
glWindowPos2dvARB: procedure(const p: PGLdouble); cdecl;
glWindowPos2fARB: procedure(x, y: GLfloat); cdecl;
glWindowPos2fvARB: procedure(const p: PGLfloat); cdecl;
glWindowPos2iARB: procedure(x, y: GLint); cdecl;
glWindowPos2ivARB: procedure(const p: PGLint); cdecl;
glWindowPos2sARB: procedure(x, y: GLshort); cdecl;
glWindowPos2svARB: procedure(const p: PGLshort); cdecl;
glWindowPos3dARB: procedure(x, y, z: GLdouble); cdecl;
glWindowPos3dvARB: procedure(const p: PGLdouble); cdecl;
glWindowPos3fARB: procedure(x, y, z: GLfloat); cdecl;
glWindowPos3fvARB: procedure(const p: PGLfloat); cdecl;
glWindowPos3iARB: procedure(x, y, z: GLint); cdecl;
glWindowPos3ivARB: procedure(const p: PGLint); cdecl;
glWindowPos3sARB: procedure(x, y, z: GLshort); cdecl;
glWindowPos3svARB: procedure(const p: PGLshort); cdecl;

// =======================================================
// -------------------------------------------------------
// =======================================================

implementation


{$LINKLIB m}

function dlopen(_File: PChar; mode: LongInt): Pointer; external 'dl';
function dlclose(handle: Pointer): LongInt; external 'dl';
function dlsym(handle: Pointer; name: PChar): Pointer; external 'dl';

function LoadLibrary(name: PChar): Pointer;
begin
  Result := dlopen(name, $101 {RTLD_GLOBAL or RTLD_LAZY});
end;

function GetProc(handle: Pointer; name: PChar): Pointer;
begin
  Result := dlsym(handle, name);
  if (Result = nil) and  GLDumpUnresolvedFunctions then
    WriteLn('Unresolved: ', name);
end;

var
  libGL : Pointer;

function InitGLFromLibrary(libname: PChar): Boolean;
begin
  Result := False;
  libGL := LoadLibrary(libname);
  if not Assigned(libGL) then exit;

// Loading OpenGL Procedures and Functions

  glAccum := GetProc(libgl, 'glAccum');
  glActiveTexture := GetProc(libgl, 'glActiveTexture');
  glActiveTextureARB := GetProc(libgl, 'glActiveTextureARB');
  glAddSwapHintRectWIN := GetProc(libgl, 'glAddSwapHintRectWIN');
  glAlphaFunc := GetProc(libgl, 'glAlphaFunc');
  glAreProgramsResidentNV := GetProc(libgl, 'glAreProgramsResidentNV');
  glAreTexturesResident := GetProc(libgl, 'glAreTexturesResident');
  glAreTexturesResidentEXT := GetProc(libgl, 'glAreTexturesResidentEXT');
  glArrayElement := GetProc(libgl, 'glArrayElement');
  glArrayElementEXT := GetProc(libgl, 'glArrayElementEXT');
  glBegin := GetProc(libgl, 'glBegin');
  glBeginOcclusionQueryNV := GetProc(libgl, 'glBeginOcclusionQueryNV');
  glBindProgramNV := GetProc(libgl, 'glBindProgramNV');
  glBindTexture := GetProc(libgl, 'glBindTexture');
  glBindTextureEXT := GetProc(libgl, 'glBindTextureEXT');
  glBitmap := GetProc(libgl, 'glBitmap');
  glBlendColor := GetProc(libgl, 'glBlendColor');
  glBlendColorEXT := GetProc(libgl, 'glBlendColorEXT');
  glBlendEquation := GetProc(libgl, 'glBlendEquation');
  glBlendEquationEXT := GetProc(libgl, 'glBlendEquationEXT');
  glBlendFunc := GetProc(libgl, 'glBlendFunc');
  glCallList := GetProc(libgl, 'glCallList');
  glCallLists := GetProc(libgl, 'glCallLists');
  glClear := GetProc(libgl, 'glClear');
  glClearAccum := GetProc(libgl, 'glClearAccum');
  glClearColor := GetProc(libgl, 'glClearColor');
  glClearDepth := GetProc(libgl, 'glClearDepth');
  glClearIndex := GetProc(libgl, 'glClearIndex');
  glClearStencil := GetProc(libgl, 'glClearStencil');
  glClientActiveTexture := GetProc(libgl, 'glClientActiveTexture');
  glClientActiveTextureARB := GetProc(libgl, 'glClientActiveTextureARB');
  glClipPlane := GetProc(libgl, 'glClipPlane');
  glColor3b := GetProc(libgl, 'glColor3b');
  glColor3bv := GetProc(libgl, 'glColor3bv');
  glColor3d := GetProc(libgl, 'glColor3d');
  glColor3dv := GetProc(libgl, 'glColor3dv');
  glColor3f := GetProc(libgl, 'glColor3f');
  glColor3fv := GetProc(libgl, 'glColor3fv');
  glColor3i := GetProc(libgl, 'glColor3i');
  glColor3iv := GetProc(libgl, 'glColor3iv');
  glColor3s := GetProc(libgl, 'glColor3s');
  glColor3sv := GetProc(libgl, 'glColor3sv');
  glColor3ub := GetProc(libgl, 'glColor3ubv');
  glColor3ubv := GetProc(libgl, 'glColor3ubv');
  glColor3ui := GetProc(libgl, 'glColor3ui');
  glColor3uiv := GetProc(libgl, 'glColor3uiv');
  glColor3us := GetProc(libgl, 'glColor3us');
  glColor3usv := GetProc(libgl, 'glColor3usv');
  glColor4b := GetProc(libgl, 'glColor4b');
  glColor4bv := GetProc(libgl, 'glColor4bv');
  glColor4d := GetProc(libgl, 'glColor4d');
  glColor4dv := GetProc(libgl, 'glColor4dv');
  glColor4f := GetProc(libgl, 'glColor4f');
  glColor4fv := GetProc(libgl, 'glColor4fv');
  glColor4i := GetProc(libgl, 'glColor4i');
  glColor4iv := GetProc(libgl, 'glColor4iv');
  glColor4s := GetProc(libgl, 'glColor4s');
  glColor4sv := GetProc(libgl, 'glColor4sv');
  glColor4ub := GetProc(libgl, 'glColor4ub');
  glColor4ubv := GetProc(libgl, 'glColor4ubv');
  glColor4ui := GetProc(libgl, 'glColor4ui');
  glColor4uiv := GetProc(libgl, 'glColor4uiv');
  glColor4us := GetProc(libgl, 'glColor4us');
  glColor4usv := GetProc(libgl, 'glColor4usv');
  glColorMask := GetProc(libgl, 'glColorMask');
  glColorMaterial := GetProc(libgl, 'glColorMaterial');
  glColorPointer := GetProc(libgl, 'glColorPointer');
  glColorPointerEXT := GetProc(libgl, 'glColorPointerEXT');
  glColorSubTable  := GetProc(libgl, 'glColorSubTable');
  glColorSubTableEXT := GetProc(libgl, 'glColorSubTableEXT');
  glColorTable  := GetProc(libgl, 'glColorTable');
  glColorTableEXT := GetProc(libgl, 'glColorTableEXT');
  glColorTableParameterfv := GetProc(libgl, 'glColorTableParameterfv');
  glColorTableParameteriv := GetProc(libgl, 'glColorTableParameteriv');
  glCombinerInputNV := GetProc(libgl, 'glCombinerInputNV');
  glCombinerOutputNV := GetProc(libgl, 'glCombinerOutputNV');
  glCombinerParameterfNV := GetProc(libgl, 'glCombinerParameterfNV');
  glCombinerParameterfvNV := GetProc(libgl, 'glCombinerParameterfvNV');
  glCombinerParameteriNV := GetProc(libgl, 'glCombinerParameteriNV');
  glCombinerParameterivNV := GetProc(libgl, 'glCombinerParameterivNV');
  glCombinerStageParameterfvNV := GetProc(libgl, 'glCombinerStageParameterfvNV');
  glCompressedTexImage1D := GetProc(libgl, 'glCompressedTexImage1D');
  glCompressedTexImage1DARB := GetProc(libgl, 'glCompressedTexImage1DARB');
  glCompressedTexImage2D := GetProc(libgl, 'glCompressedTexImage2D');
  glCompressedTexImage2DARB := GetProc(libgl, 'glCompressedTexImage2DARB');
  glCompressedTexImage3D := GetProc(libgl, 'glCompressedTexImage3D');
  glCompressedTexImage3DARB := GetProc(libgl, 'glCompressedTexImage3DARB');
  glCompressedTexSubImage1D := GetProc(libgl, 'glCompressedTexSubImage1D');
  glCompressedTexSubImage1DARB := GetProc(libgl, 'glCompressedTexSubImage1DARB');
  glCompressedTexSubImage2D := GetProc(libgl, 'glCompressedTexSubImage2D');
  glCompressedTexSubImage2DARB := GetProc(libgl, 'glCompressedTexSubImage2DARB');
  glCompressedTexSubImage3D := GetProc(libgl, 'glCompressedTexSubImage3D');
  glCompressedTexSubImage3DARB := GetProc(libgl, 'glCompressedTexSubImage3DARB');
  glConvolutionFilter1D := GetProc(libgl, 'glConvolutionFilter1D');
  glConvolutionFilter2D := GetProc(libgl, 'glConvolutionFilter2D');
  glConvolutionParameterf := GetProc(libgl, 'glConvolutionParameterf'); 
  glConvolutionParameterfv := GetProc(libgl, 'glConvolutionParameterfv');
  glConvolutionParameteri := GetProc(libgl, 'glConvolutionParameteri');
  glConvolutionParameteriv := GetProc(libgl, 'glConvolutionParameteriv');
  glCopyColorSubTable := GetProc(libgl, 'glCopyColorSubTable');
  glCopyColorTable := GetProc(libgl, 'glCopyColorTable');
  glCopyConvolutionFilter1D := GetProc(libgl, 'glCopyConvolutionFilter1D');
  glCopyConvolutionFilter2D := GetProc(libgl, 'glCopyConvolutionFilter2D');
  glCopyPixels := GetProc(libgl, 'glCopyPixels');
  glCopyTexImage1D := GetProc(libgl, 'glCopyTexImage1D');
  glCopyTexImage2D := GetProc(libgl, 'glCopyTexImage2D');
  glCopyTexSubImage1D := GetProc(libgl, 'glCopyTexSubImage1D');
  glCopyTexSubImage2D := GetProc(libgl, 'glCopyTexSubImage2D');
  glCopyTexSubImage3D := GetProc(libgl, 'glCopyTexSubImage3D');
  glCopyTexSubImage3DEXT := GetProc(libgl, 'glCopyTexSubImage3DEXT');
  glCullFace := GetProc(libgl, 'glCullFace');
  glDeleteFencesNV := GetProc(libgl, 'glDeleteFencesNV');
  glDeleteLists := GetProc(libgl, 'glDeleteLists');
  glDeleteOcclusionQueriesNV := GetProc(libgl, 'glDeleteOcclusionQueriesNV');
  glDeleteProgramsNV := GetProc(libgl, 'glDeleteProgramsNV');
  glDeleteTextures := GetProc(libgl, 'glDeleteTextures');  
  glDeleteTexturesEXT := GetProc(libgl, 'glDeleteTexturesEXT');
  glDepthFunc := GetProc(libgl, 'glDepthFunc');
  glDepthMask := GetProc(libgl, 'glDepthMask');
  glDepthRange := GetProc(libgl, 'glDepthRange');
  glDisable := GetProc(libgl, 'glDisable');
  glDisableClientState := GetProc(libgl, 'glDisableClientState');
  glDrawArrays := GetProc(libgl, 'glDrawArrays');
  glDrawArraysEXT := GetProc(libgl, 'glDrawArraysEXT');
  glDrawBuffer := GetProc(libgl, 'glDrawBuffer');
  glDrawElements := GetProc(libgl, 'glDrawElements');
  glDrawMeshNV := GetProc(libgl, 'glDrawMeshNV');
  glDrawPixels := GetProc(libgl, 'glDrawPixels');
  glDrawRangeElements := GetProc(libgl, 'glDrawRangeElements');
  glDrawRangeElementsEXT := GetProc(libgl, 'glDrawRangeElementsEXT');
  glEdgeFlag := GetProc(libgl, 'glEdgeFlag');
  glEdgeFlagPointer := GetProc(libgl, 'glEdgeFlagPointer');
  glEdgeFlagPointerEXT := GetProc(libgl, 'glEdgeFlagPointerEXT');
  glEdgeFlagv := GetProc(libgl, 'glEdgeFlagv');
  glEnable := GetProc(libgl, 'glEnable');
  glEnableClientState := GetProc(libgl, 'glEnableClientState');
  glEnd := GetProc(libgl, 'glEnd');
  glEndList := GetProc(libgl, 'glEndList');
  glEndOcclusionQueryNV := GetProc(libgl, 'glEndOcclusionQueryNV');
  glEvalCoord1d := GetProc(libgl, 'glEvalCoord1d');
  glEvalCoord1dv := GetProc(libgl, 'glEvalCoord1dv');
  glEvalCoord1f := GetProc(libgl, 'glEvalCoord1f');
  glEvalCoord1fv := GetProc(libgl, 'glEvalCoord1fv');
  glEvalCoord2d := GetProc(libgl, 'glEvalCoord2d');
  glEvalCoord2dv := GetProc(libgl, 'glEvalCoord2dv');
  glEvalCoord2f := GetProc(libgl, 'glEvalCoord2f');
  glEvalCoord2fv := GetProc(libgl, 'glEvalCoord2fv');
  glEvalMapsNV := GetProc(libgl, 'glEvalMapsNV');
  glEvalMesh1 := GetProc(libgl, 'glEvalMesh1');
  glEvalMesh2 := GetProc(libgl, 'glEvalMesh2');
  glEvalPoint1 := GetProc(libgl, 'glEvalPoint1');
  glEvalPoint2 := GetProc(libgl, 'glEvalPoint2');
  glExecuteProgramNV := GetProc(libgl, 'glExecuteProgramNV');
  glFeedbackBuffer := GetProc(libgl, 'glFeedbackBuffer');
  glFinalCombinerInputNV := GetProc(libgl, 'glFinalCombinerInputNV');
  glFinish := GetProc(libgl, 'glFinish');  
  glFinishFenceNV := GetProc(libgl, 'glFinishFenceNV');
  glFlush := GetProc(libgl, 'glFlush');
  glFlushPixelDataRangeNV := GetProc(libgl, 'glFlushPixelDataRangeNV'); 
  glFlushVertexArrayRangeNV := GetProc(libgl, 'glFlushVertexArrayRangeNV');
  glFlushHold := GetProc(libgl, 'glFlushHold');
  glFogCoordPointerEXT := GetProc(libgl, 'glFogCoordPointerEXT');
  glFogCoorddEXT := GetProc(libgl, 'glFogCoorddEXT');
  glFogCoorddvEXT := GetProc(libgl, 'glFogCoorddvEXT');
  glFogCoordfEXT := GetProc(libgl, 'glFogCoordfEXT');
  glFogCoordfvEXT := GetProc(libgl, 'glFogCoordfvEXT');
  glFogf := GetProc(libgl, 'glFogf');
  glFogfv := GetProc(libgl, 'glFogfv');
  glFogi := GetProc(libgl, 'glFogi');
  glFogiv := GetProc(libgl, 'glFogiv');
  glFrontFace := GetProc(libgl, 'glFrontFace');
  glFrustum := GetProc(libgl, 'glFrustum');
  glGenFencesNV := GetProc(libgl, 'glGenFencesNV');
  glGenLists := GetProc(libgl, 'glGenLists');
  glGenProgramsNV := GetProc(libgl, 'glGenProgramsNV');
  glGenTextures := GetProc(libgl, 'glGenTextures');
  glGenTexturesEXT := GetProc(libgl, 'glGenTexturesEXT');
  glGetBooleanv := GetProc(libgl, 'glGetBooleanv');
  glGetClipPlane := GetProc(libgl, 'glGetClipPlane');
  glGetColorTable := GetProc(libgl, 'glGetColorTable');
  glGetColorTableEXT := GetProc(libgl, 'glGetColorTableEXT');
  glGetColorTableParameterfv := GetProc(libgl, 'glGetColorTableParameterfv');
  glGetColorTableParameterfvEXT := GetProc(libgl, 'glGetColorTableParameterfvEXT');
  glGetColorTableParameteriv := GetProc(libgl, 'glGetColorTableParameteriv');
  glGetColorTableParameterivEXT := GetProc(libgl, 'glGetColorTableParameterivEXT');
  glGetCombinerInputParameterfvNV := GetProc(libgl, 'glGetCombinerInputParameterfvNV');
  glGetCombinerInputParameterivNV := GetProc(libgl, 'glGetCombinerInputParameterivNV');
  glGetCombinerOutputParameterfvNV := GetProc(libgl, 'glGetCombinerOutputParameterfvNV');
  glGetCombinerOutputParameterivNV := GetProc(libgl, 'glGetCombinerOutputParameterivNV');
  glGetCombinerStageParameterfvNV := GetProc(libgl, 'glGetCombinerStageParameterfvNV');
  glGetCompressedTexImage := GetProc(libgl, 'glGetCompressedTexImage');
  glGetCompressedTexImageARB := GetProc(libgl, 'glGetCompressedTexImageARB');
  glGetConvolutionFilter :=GetProc(libgl, 'glGetConvolutionFilter');
  glGetConvolutionParameterfv := GetProc(libgl, 'glGetConvolutionParameterfv');
  glGetConvolutionParameteriv := GetProc(libgl, 'glGetConvolutionParameteriv');
  glGetDoublev := GetProc(libgl, 'glGetDoublev');
  glGetError := GetProc(libgl, 'glGetError');
  glGetFenceivNV := GetProc(libgl, 'glGetFenceivNV');
  glGetFinalCombinerInputParameterfvNV := GetProc(libgl, 'glGetFinalCombinerInputParameterfvNV');
  glGetFinalCombinerInputParameterivNV := GetProc(libgl, 'glGetFinalCombinerInputParameterivNV');
  glGetFloatv := GetProc(libgl, 'glGetFloatv');
  glGetHistogram := GetProc(libgl, 'glGetHistogram');
  glGetHistogramParameterfv := GetProc(libgl, 'glGetHistogramParameterfv');
  glGetHistogramParameteriv := GetProc(libgl, 'glGetHistogramParameteriv');
  glGetIntegerv := GetProc(libgl, 'glGetIntegerv');
  glGetLightfv := GetProc(libgl, 'glGetLightfv');
  glGetLightiv := GetProc(libgl, 'glGetLightiv');
  glGetMapAttribParameterfvNV := GetProc(libgl, 'glGetMapAttribParameterfvNV');
  glGetMapAttribParameterivNV := GetProc(libgl, 'glGetMapAttribParameterivNV');
  glGetMapControlPointsNV := GetProc(libgl, 'glGetMapControlPointsNV');
  glGetMapParameterfvNV := GetProc(libgl, 'glGetMapParameterfvNV');
  glGetMapParameterivNV := GetProc(libgl, 'glGetMapParameterivNV');
  glGetMapdv := GetProc(libgl, 'glGetMapdv');
  glGetMapfv := GetProc(libgl, 'glGetMapfv');
  glGetMapiv := GetProc(libgl, 'glGetMapiv');
  glGetMaterialfv := GetProc(libgl, 'glGetMaterialfv');
  glGetMaterialiv := GetProc(libgl, 'glGetMaterialiv');
  glGetMinmax := GetProc(libgl, 'glGetMinmax');
  glGetMinmaxParameterfv := GetProc(libgl, 'glGetMinmaxParameterfv');
  glGetMinmaxParameteriv := GetProc(libgl, 'glGetMinmaxParameteriv');
  glGetOcclusionQueryivNV := GetProc(libgl, 'glGetOcclusionQueryivNV');
  glGetOcclusionQueryuivNV := GetProc(libgl, 'glGetOcclusionQueryuivNV');
  glGetPixelMapfv := GetProc(libgl, 'glGetPixelMapfv');
  glGetPixelMapuiv := GetProc(libgl, 'glGetPixelMapuiv');
  glGetPixelMapusv := GetProc(libgl, 'glGetPixelMapusv');
  glGetPointerv := GetProc(libgl, 'glGetPointerv');
  glGetPointervEXT := GetProc(libgl, 'glGetPointervEXT');
  glGetPolygonStipple := GetProc(libgl, 'glGetPolygonStipple');
  glGetProgramLocalParameterdvNV := GetProc(libgl, 'glGetProgramLocalParameterdvNV');
  glGetProgramLocalParameterfvNV := GetProc(libgl, 'glGetProgramLocalParameterfvNV');
  glGetProgramParameterSigneddvNV := GetProc(libgl, 'glGetProgramParameterSigneddvNV');
  glGetProgramParameterSignedfvNV := GetProc(libgl, 'glGetProgramParameterSignedfvNV');
  glGetProgramParameterdvNV := GetProc(libgl, 'glGetProgramParameterdvNV');
  glGetProgramParameterfvNV := GetProc(libgl, 'glGetProgramParameterfvNV');
  glGetProgramStringNV := GetProc(libgl, 'glGetProgramStringNV');
  glGetProgramivNV := GetProc(libgl, 'glGetProgramivNV');
  glGetSeparableFilter := GetProc(libgl, 'glGetSeparableFilter');
  glGetString := GetProc(libgl, 'glGetString');
  glGetTexEnvfv := GetProc(libgl, 'glGetTexEnvfv');
  glGetTexEnviv := GetProc(libgl, 'glGetTexEnviv');
  glGetTexGendv := GetProc(libgl, 'glGetTexGendv');
  glGetTexGenfv := GetProc(libgl, 'glGetTexGenfv');
  glGetTexGeniv := GetProc(libgl, 'glGetTexGeniv');
  glGetTexImage := GetProc(libgl, 'glGetTexImage');
  glGetTexParameterfv := GetProc(libgl, 'glGetTexParameterfv');
  glGetTexParameteriv := GetProc(libgl, 'glGetTexParameteriv');
  glGetTexLevelParameterfv := GetProc(libgl, 'glGetTexLevelParameterfv');
  glGetTexLevelParameteriv := GetProc(libgl, 'glGetTexLevelParameteriv');
  glGetTrackMatrixivNV := GetProc(libgl, 'glGetTrackMatrixivNV');
  glGetVertexAttribPointervNV := GetProc(libgl, 'glGetVertexAttribPointervNV');
  glGetVertexAttribdvNV := GetProc(libgl, 'glGetVertexAttribdvNV');
  glGetVertexAttribfvNV := GetProc(libgl, 'glGetVertexAttribfvNV');
  glGetVertexAttribivNV := GetProc(libgl, 'glGetVertexAttribivNV');
  glHint := GetProc(libgl, 'glHint');
  glHistogram := GetProc(libgl, 'glHistogram');
  glIndexMask := GetProc(libgl, 'glIndexMask');
  glIndexPointer := GetProc(libgl, 'glIndexPointer');
  glIndexPointerEXT := GetProc(libgl, 'glIndexPointerEXT');
  glIndexd := GetProc(libgl, 'glIndexd');
  glIndexdv := GetProc(libgl, 'glIndexdv');
  glIndexf := GetProc(libgl, 'glIndexf');
  glIndexfv := GetProc(libgl, 'glIndexfv');
  glIndexi := GetProc(libgl, 'glIndexi');
  glIndexiv := GetProc(libgl, 'glIndexiv');
  glIndexs := GetProc(libgl, 'glIndexs');
  glIndexsv := GetProc(libgl, 'glIndexsv');
  glIndexub := GetProc(libgl, 'glIndexub');
  glIndexubv := GetProc(libgl, 'glIndexubv');
  glInitNames := GetProc(libgl, 'glInitNames');  
  glInterleavedArrays := GetProc(libgl, 'glInterleavedArrays');
  glIsEnabled := GetProc(libgl, 'glIsEnabled');  
  glIsFenceNV := GetProc(libgl, 'glIsFenceNV');
  glIsList := GetProc(libgl, 'glIsList');
  glIsOcclusionQueryNV := GetProc(libgl, 'glIsOcclusionQueryNV');
  glIsProgramNV := GetProc(libgl, 'glIsProgramNV');
  glIsTexture := GetProc(libgl, 'glIsTexture');
  glIsTextureEXT := GetProc(libgl, 'glIsTextureEXT');
  glLightModelf := GetProc(libgl, 'glLightModelf');
  glLightModelfv := GetProc(libgl, 'glLightModelfv');
  glLightModeli := GetProc(libgl, 'glLightModeli');
  glLightModeliv := GetProc(libgl, 'glLightModeliv');
  glLightf := GetProc(libgl, 'glLightf');
  glLightfv := GetProc(libgl, 'glLightfv');
  glLighti := GetProc(libgl, 'glLighti');
  glLightiv := GetProc(libgl, 'glLightiv');
  glLineStipple := GetProc(libgl, 'glLineStipple');
  glLineWidth := GetProc(libgl, 'glLineWidth');
  glListBase := GetProc(libgl, 'glListBase');
  glLoadIdentity := GetProc(libgl, 'glLoadIdentity');
  glLoadMatrixd := GetProc(libgl, 'glLoadMatrixd');
  glLoadMatrixf := GetProc(libgl, 'glLoadMatrixf');
  glLoadName := GetProc(libgl, 'glLoadName');
  glLoadProgramNV := GetProc(libgl, 'glLoadProgramNV');
  glLoadTransposeMatrixd := GetProc(libgl, 'glLoadTransposeMatrixd');
  glLoadTransposeMatrixdARB := GetProc(libgl, 'glLoadTransposeMatrixdARB');
  glLoadTransposeMatrixf := GetProc(libgl, 'glLoadTransposeMatrixf');
  glLoadTransposeMatrixfARB := GetProc(libgl, 'glLoadTransposeMatrixfARB');
  glLockArraysEXT := GetProc(libgl, 'glLockArraysEXT');
  glLogicOp := GetProc(libgl, 'glLogicOp');  
  glMap1d := GetProc(libgl, 'glMap1d');
  glMap1f := GetProc(libgl, 'glMap1f');
  glMap2d := GetProc(libgl, 'glMap2d');
  glMap2f := GetProc(libgl, 'glMap2f');
  glMapControlPointsNV := GetProc(libgl, 'glMapControlPointsNV');
  glMapGrid1d := GetProc(libgl, 'glMapGrid1d');
  glMapGrid1f := GetProc(libgl, 'glMapGrid1f');
  glMapGrid2d := GetProc(libgl, 'glMapGrid2d');
  glMapGrid2f := GetProc(libgl, 'glMapGrid2f');
  glMapParameterfvNV := GetProc(libgl, 'glMapParameterfvNV');
  glMapParameterivNV := GetProc(libgl, 'glMapParameterivNV');
  glMaterialf := GetProc(libgl, 'glMaterialf');
  glMateriali := GetProc(libgl, 'glMateriali');
  glMaterialfv := GetProc(libgl, 'glMaterialfv');
  glMaterialiv := GetProc(libgl, 'glMaterialiv');
  glMatrixMode := GetProc(libgl, 'glMatrixMode');
  glMinmax := GetProc(libgl, 'glMinmax');
  glMultMatrixd := GetProc(libgl, 'glMultMatrixd');
  glMultMatrixf := GetProc(libgl, 'glMultMatrixf');
  glMultTransposeMatrixd := GetProc(libgl, 'glMultTransposeMatrixd');
  glMultTransposeMatrixdARB := GetProc(libgl, 'glMultTransposeMatrixdARB');
  glMultTransposeMatrixf := GetProc(libgl, 'glMultTransposeMatrixf');
  glMultTransposeMatrixfARB := GetProc(libgl, 'glMultTransposeMatrixfARB');
  glMultiDrawArraysEXT := GetProc(libgl, 'glMultiDrawArraysEXT');
  glMultiDrawElementsEXT := GetProc(libgl, 'glMultiDrawElementsEXT');
  glMultiTexCoord1d := GetProc(libgl, 'glMultiTexCoord1d');
  glMultiTexCoord1dARB := GetProc(libgl, 'glMultiTexCoord1dARB');
  glMultiTexCoord1dSGIS := GetProc(libgl, 'glMultiTexCoord1dSGIS');
  glMultiTexCoord1dv := GetProc(libgl, 'glMultiTexCoord1dv');
  glMultiTexCoord1dvARB := GetProc(libgl, 'glMultiTexCoord1dvARB');  
  glMultiTexCoord1dvSGIS := GetProc(libgl, 'glMultiTexCoord1dvSGIS');
  glMultiTexCoord1f := GetProc(libgl, 'glMultiTexCoord1f');
  glMultiTexCoord1fARB := GetProc(libgl, 'glMultiTexCoord1fARB');  
  glMultiTexCoord1fSGIS := GetProc(libgl, 'glMultiTexCoord1fSGIS');
  glMultiTexCoord1fv := GetProc(libgl, 'glMultiTexCoord1fv');
  glMultiTexCoord1fvARB := GetProc(libgl, 'glMultiTexCoord1fvARB');  
  glMultiTexCoord1fvSGIS := GetProc(libgl, 'glMultiTexCoord1fvSGIS');
  glMultiTexCoord1i := GetProc(libgl, 'glMultiTexCoord1i');
  glMultiTexCoord1iARB := GetProc(libgl, 'glMultiTexCoord1iARB');  
  glMultiTexCoord1iSGIS := GetProc(libgl, 'glMultiTexCoord1iSGIS');
  glMultiTexCoord1iv := GetProc(libgl, 'glMultiTexCoord1iv');
  glMultiTexCoord1ivARB := GetProc(libgl, 'glMultiTexCoord1ivARB');  
  glMultiTexCoord1ivSGIS := GetProc(libgl, 'glMultiTexCoord1ivSGIS');
  glMultiTexCoord1s := GetProc(libgl, 'glMultiTexCoord1s');
  glMultiTexCoord1sARB := GetProc(libgl, 'glMultiTexCoord1sARB');  
  glMultiTexCoord1sSGIS := GetProc(libgl, 'glMultiTexCoord1sSGIS');
  glMultiTexCoord1sv := GetProc(libgl, 'glMultiTexCoord1sv');
  glMultiTexCoord1svARB := GetProc(libgl, 'glMultiTexCoord1svARB');  
  glMultiTexCoord1svSGIS := GetProc(libgl, 'glMultiTexCoord1svSGIS');
  glMultiTexCoord2d := GetProc(libgl, 'glMultiTexCoord2d');
  glMultiTexCoord2dARB := GetProc(libgl, 'glMultiTexCoord2dARB');  
  glMultiTexCoord2dSGIS := GetProc(libgl, 'glMultiTexCoord2dSGIS');
  glMultiTexCoord2dv := GetProc(libgl, 'glMultiTexCoord2dv');
  glMultiTexCoord2dvARB := GetProc(libgl, 'glMultiTexCoord2dvARB');  
  glMultiTexCoord2dvSGIS := GetProc(libgl, 'glMultiTexCoord2dvSGIS');
  glMultiTexCoord2f := GetProc(libgl, 'glMultiTexCoord2f');
  glMultiTexCoord2fARB := GetProc(libgl, 'glMultiTexCoord2fARB');  
  glMultiTexCoord2fSGIS := GetProc(libgl, 'glMultiTexCoord2fSGIS');
  glMultiTexCoord2fv := GetProc(libgl, 'glMultiTexCoord2fv');
  glMultiTexCoord2fvARB := GetProc(libgl, 'glMultiTexCoord2fvARB');  
  glMultiTexCoord2fvSGIS := GetProc(libgl, 'glMultiTexCoord2fvSGIS');
  glMultiTexCoord2i := GetProc(libgl, 'glMultiTexCoord2i');
  glMultiTexCoord2iARB := GetProc(libgl, 'glMultiTexCoord2iARB');  
  glMultiTexCoord2iSGIS := GetProc(libgl, 'glMultiTexCoord2iSGIS');
  glMultiTexCoord2iv := GetProc(libgl, 'glMultiTexCoord2iv');
  glMultiTexCoord2ivARB := GetProc(libgl, 'glMultiTexCoord2ivARB');  
  glMultiTexCoord2ivSGIS := GetProc(libgl, 'glMultiTexCoord2ivSGIS');
  glMultiTexCoord2s := GetProc(libgl, 'glMultiTexCoord2s');
  glMultiTexCoord2sARB := GetProc(libgl, 'glMultiTexCoord2sARB');  
  glMultiTexCoord2sSGIS := GetProc(libgl, 'glMultiTexCoord2sSGIS');
  glMultiTexCoord2sv := GetProc(libgl, 'glMultiTexCoord2sv');
  glMultiTexCoord2svARB := GetProc(libgl, 'glMultiTexCoord2svARB');  
  glMultiTexCoord2svSGIS := GetProc(libgl, 'glMultiTexCoord2svSGIS');
  glMultiTexCoord3d := GetProc(libgl, 'glMultiTexCoord3d');
  glMultiTexCoord3dARB := GetProc(libgl, 'glMultiTexCoord3dARB');  
  glMultiTexCoord3dSGIS := GetProc(libgl, 'glMultiTexCoord3dSGIS');
  glMultiTexCoord3dv := GetProc(libgl, 'glMultiTexCoord3dv');
  glMultiTexCoord3dvARB := GetProc(libgl, 'glMultiTexCoord3dvARB');  
  glMultiTexCoord3dvSGIS := GetProc(libgl, 'glMultiTexCoord3dvSGIS');
  glMultiTexCoord3f := GetProc(libgl, 'glMultiTexCoord3f');
  glMultiTexCoord3fARB := GetProc(libgl, 'glMultiTexCoord3fARB');  
  glMultiTexCoord3fSGIS := GetProc(libgl, 'glMultiTexCoord3fSGIS');
  glMultiTexCoord3fv := GetProc(libgl, 'glMultiTexCoord3fv');
  glMultiTexCoord3fvARB := GetProc(libgl, 'glMultiTexCoord3fvARB');  
  glMultiTexCoord3fvSGIS := GetProc(libgl, 'glMultiTexCoord3fvSGIS');
  glMultiTexCoord3i := GetProc(libgl, 'glMultiTexCoord3i');
  glMultiTexCoord3iARB := GetProc(libgl, 'glMultiTexCoord3iARB');  
  glMultiTexCoord3iSGIS := GetProc(libgl, 'glMultiTexCoord3iSGIS');
  glMultiTexCoord3iv := GetProc(libgl, 'glMultiTexCoord3iv');
  glMultiTexCoord3ivARB := GetProc(libgl, 'glMultiTexCoord3ivARB');  
  glMultiTexCoord3ivSGIS := GetProc(libgl, 'glMultiTexCoord3ivSGIS');
  glMultiTexCoord3s := GetProc(libgl, 'glMultiTexCoord3s');
  glMultiTexCoord3sARB := GetProc(libgl, 'glMultiTexCoord3sARB');  
  glMultiTexCoord3sSGIS := GetProc(libgl, 'glMultiTexCoord3sSGIS');
  glMultiTexCoord3sv := GetProc(libgl, 'glMultiTexCoord3sv');
  glMultiTexCoord3svARB := GetProc(libgl, 'glMultiTexCoord3svARB');  
  glMultiTexCoord3svSGIS := GetProc(libgl, 'glMultiTexCoord3svSGIS');
  glMultiTexCoord4d := GetProc(libgl, 'glMultiTexCoord4d');
  glMultiTexCoord4dARB := GetProc(libgl, 'glMultiTexCoord4dARB');  
  glMultiTexCoord4dSGIS := GetProc(libgl, 'glMultiTexCoord4dSGIS');
  glMultiTexCoord4dv := GetProc(libgl, 'glMultiTexCoord4dv');
  glMultiTexCoord4dvARB := GetProc(libgl, 'glMultiTexCoord4dvARB');  
  glMultiTexCoord4dvSGIS := GetProc(libgl, 'glMultiTexCoord4dvSGIS');
  glMultiTexCoord4f := GetProc(libgl, 'glMultiTexCoord4f');
  glMultiTexCoord4fARB := GetProc(libgl, 'glMultiTexCoord4fARB');  
  glMultiTexCoord4fSGIS := GetProc(libgl, 'glMultiTexCoord4fSGIS');
  glMultiTexCoord4fv := GetProc(libgl, 'glMultiTexCoord4fv');
  glMultiTexCoord4fvARB := GetProc(libgl, 'glMultiTexCoord4fvARB');  
  glMultiTexCoord4fvSGIS := GetProc(libgl, 'glMultiTexCoord4fvSGIS');
  glMultiTexCoord4i := GetProc(libgl, 'glMultiTexCoord4i');
  glMultiTexCoord4iARB := GetProc(libgl, 'glMultiTexCoord4iARB');  
  glMultiTexCoord4iSGIS := GetProc(libgl, 'glMultiTexCoord4iSGIS');
  glMultiTexCoord4iv := GetProc(libgl, 'glMultiTexCoord4iv');
  glMultiTexCoord4ivARB := GetProc(libgl, 'glMultiTexCoord4ivARB');  
  glMultiTexCoord4ivSGIS := GetProc(libgl, 'glMultiTexCoord4ivSGIS');
  glMultiTexCoord4s := GetProc(libgl, 'glMultiTexCoord4s');
  glMultiTexCoord4sARB := GetProc(libgl, 'glMultiTexCoord4sARB');  
  glMultiTexCoord4sSGIS := GetProc(libgl, 'glMultiTexCoord4sSGIS');
  glMultiTexCoord4sv := GetProc(libgl, 'glMultiTexCoord4sv');
  glMultiTexCoord4svARB := GetProc(libgl, 'glMultiTexCoord4svARB');  
  glMultiTexCoord4svSGIS := GetProc(libgl, 'glMultiTexCoord4svSGIS');
  glMultiTexCoordPointerSGIS := GetProc(libgl, 'glMultiTexCoordPointerSGIS');
  glNewList := GetProc(libgl, 'glNewList');
  glNormal3b := GetProc(libgl, 'glNormal3b');
  glNormal3bv := GetProc(libgl, 'glNormal3bv');
  glNormal3d := GetProc(libgl, 'glNormal3d');
  glNormal3dv := GetProc(libgl, 'glNormal3dv');
  glNormal3f := GetProc(libgl, 'glNormal3f');
  glNormal3fv := GetProc(libgl, 'glNormal3fv');
  glNormal3i := GetProc(libgl, 'glNormal3i');
  glNormal3iv := GetProc(libgl, 'glNormal3iv');
  glNormal3s := GetProc(libgl, 'glNormal3s');
  glNormal3sv := GetProc(libgl, 'glNormal3sv');
  glNormalPointer := GetProc(libgl, 'glNormalPointer');
  glNormalPointerEXT := GetProc(libgl, 'glNormalPointerEXT');
  glOrtho := GetProc(libgl, 'glOrtho');
  glPassThrough := GetProc(libgl, 'glPassThrough');
  glPixelDataRangeNV := GetProc(libgl, 'glPixelDataRangeNV');
  glPixelMapfv := GetProc(libgl, 'glPixelMapfv');
  glPixelMapuiv := GetProc(libgl, 'glPixelMapuiv');
  glPixelMapusv := GetProc(libgl, 'glPixelMapusv');
  glPixelStoref := GetProc(libgl, 'glPixelStoref');
  glPixelStorei := GetProc(libgl, 'glPixelStorei');
  glPixelTransferf := GetProc(libgl, 'glPixelTransferf');
  glPixelTransferi := GetProc(libgl, 'glPixelTransferi');
  glPixelZoom := GetProc(libgl, 'glPixelZoom');
  glPointParameterfEXT := GetProc(libgl, 'glPointParameterfEXT');
  glPointParameterfvEXT := GetProc(libgl, 'glPointParameterfvEXT');
  glPointParameteriNV := GetProc(libgl, 'glPointParameteriNV');
  glPointParameterivNV := GetProc(libgl, 'glPointParameterivNV');
  glPointSize := GetProc(libgl, 'glPointSize');
  glPolygonMode := GetProc(libgl, 'glPolygonMode');
  glPolygonOffset := GetProc(libgl, 'glPolygonOffset');
  glPolygonStipple := GetProc(libgl, 'glPolygonStipple');  
  glPopAttrib := GetProc(libgl, 'glPopAttrib');
  glPopClientAttrib := GetProc(libgl, 'glPopClientAttrib');
  glPopMatrix := GetProc(libgl, 'glPopMatrix');
  glPopName := GetProc(libgl, 'glPopName');
  glPrioritizeTextures := GetProc(libgl, 'glPrioritizeTextures');  
  glPrioritizeTexturesEXT := GetProc(libgl, 'glPrioritizeTexturesEXT');  
  glProgramLocalParameter4dNV := GetProc(libgl, 'glProgramLocalParameter4dNV');
  glProgramLocalParameter4dvNV := GetProc(libgl, 'glProgramLocalParameter4dvNV');
  glProgramLocalParameter4fNV := GetProc(libgl, 'glProgramLocalParameter4fNV');
  glProgramLocalParameter4fvNV := GetProc(libgl, 'glProgramLocalParameter4fvNV');
  glProgramParameter4dNV := GetProc(libgl, 'glProgramParameter4dNV');
  glProgramParameter4dvNV := GetProc(libgl, 'glProgramParameter4dvNV');
  glProgramParameter4fNV := GetProc(libgl, 'glProgramParameter4fNV');
  glProgramParameter4fvNV := GetProc(libgl, 'glProgramParameter4fvNV');
  glProgramParameterSigned4dNV := GetProc(libgl, 'glProgramParameterSigned4dNV');
  glProgramParameterSigned4dvNV := GetProc(libgl, 'glProgramParameterSigned4dvNV');
  glProgramParameterSigned4fNV := GetProc(libgl, 'glProgramParameterSigned4fNV');
  glProgramParameterSigned4fvNV := GetProc(libgl, 'glProgramParameterSigned4fvNV');
  glProgramParameters4dvNV := GetProc(libgl, 'glProgramParameters4dvNV');
  glProgramParameters4fvNV := GetProc(libgl, 'glProgramParameters4fvNV');
  glProgramParametersSigned4dvNV := GetProc(libgl, 'glProgramParameters4dvNV');
  glProgramParametersSigned4fvNV := GetProc(libgl, 'glProgramParameters4fvNV');
  glPushAttrib := GetProc(libgl, 'glPushAttrib');
  glPushClientAttrib := GetProc(libgl, 'glPushClientAttrib');
  glPushMatrix := GetProc(libgl, 'glPushMatrix');  
  glPushName := GetProc(libgl, 'glPushName');  
  glRasterPos2d := GetProc(libgl, 'glRasterPos2d');
  glRasterPos2dv := GetProc(libgl, 'glRasterPos2dv');
  glRasterPos2f := GetProc(libgl, 'glRasterPos2f');
  glRasterPos2fv := GetProc(libgl, 'glRasterPos2fv');
  glRasterPos2i := GetProc(libgl, 'glRasterPos2i');
  glRasterPos2iv := GetProc(libgl, 'glRasterPos2iv');
  glRasterPos2s := GetProc(libgl, 'glRasterPos2s');
  glRasterPos2sv := GetProc(libgl, 'glRasterPos2sv');
  glRasterPos3d := GetProc(libgl, 'glRasterPos3d');
  glRasterPos3dv := GetProc(libgl, 'glRasterPos3dv');
  glRasterPos3f := GetProc(libgl, 'glRasterPos3f');
  glRasterPos3fv := GetProc(libgl, 'glRasterPos3fv');
  glRasterPos3i := GetProc(libgl, 'glRasterPos3i');
  glRasterPos3iv := GetProc(libgl, 'glRasterPos3iv');
  glRasterPos3s := GetProc(libgl, 'glRasterPos3s');
  glRasterPos3sv := GetProc(libgl, 'glRasterPos3sv');
  glRasterPos4d := GetProc(libgl, 'glRasterPos4d');
  glRasterPos4dv := GetProc(libgl, 'glRasterPos4dv');
  glRasterPos4f := GetProc(libgl, 'glRasterPos4f');
  glRasterPos4fv := GetProc(libgl, 'glRasterPos4fv');
  glRasterPos4i := GetProc(libgl, 'glRasterPos4i');
  glRasterPos4iv := GetProc(libgl, 'glRasterPos4iv');
  glRasterPos4s := GetProc(libgl, 'glRasterPos4s');
  glRasterPos4sv := GetProc(libgl, 'glRasterPos4sv');
  glReadBuffer := GetProc(libgl, 'glReadBuffer');
  glReadPixels := GetProc(libgl, 'glReadPixels');  
  glRectd := GetProc(libgl, 'glRectd');
  glRectdv := GetProc(libgl, 'glRectdv');
  glRectf := GetProc(libgl, 'glRectf');
  glRectfv := GetProc(libgl, 'glRectfv');
  glRecti := GetProc(libgl, 'glRecti');
  glRectiv := GetProc(libgl, 'glRectiv');
  glRects := GetProc(libgl, 'glRects');
  glRectsv := GetProc(libgl, 'glRectsv');
  glReleaseFlushHold := GetProc(libgl, 'glReleaseFlushold');
  glRenderMode := GetProc(libgl, 'glRenderMode');  
  glRequestResidentProgramsNV := GetProc(libgl,'glRequestResidentProgramsNV');
  glResetHistogram := GetProc(libgl,'glResetHistogram');
  glResetMinmax := GetProc(libgl,'glResetMinmax');
  glRotated := GetProc(libgl, 'glRotated');
  glRotatef := GetProc(libgl, 'glRotatef');
  glSampleCoverage := GetProc(libgl, 'glSampleCoverage');
  glSampleCoverageARB := GetProc(libgl, 'glSampleCoverageARB');
  glScaled := GetProc(libgl, 'glScaled');
  glScalef := GetProc(libgl, 'glScalef');
  glScissor := GetProc(libgl, 'glScissor');
  glSecondaryColor3bEXT := GetProc(libgl, 'glSecondaryColor3bEXT');
  glSecondaryColor3bvEXT := GetProc(libgl, 'glSecondaryColor3bvEXT');
  glSecondaryColor3dEXT := GetProc(libgl, 'glSecondaryColor3dEXT');
  glSecondaryColor3dvEXT := GetProc(libgl, 'glSecondaryColor3dvEXT');
  glSecondaryColor3fEXT := GetProc(libgl, 'glSecondaryColor3fEXT');
  glSecondaryColor3fvEXT := GetProc(libgl, 'glSecondaryColor3fvEXT');
  glSecondaryColor3iEXT := GetProc(libgl, 'glSecondaryColor3iEXT');
  glSecondaryColor3ivEXT := GetProc(libgl, 'glSecondaryColor3ivEXT');
  glSecondaryColor3sEXT := GetProc(libgl, 'glSecondaryColor3sEXT');
  glSecondaryColor3svEXT := GetProc(libgl, 'glSecondaryColor3svEXT');
  glSecondaryColor3ubEXT := GetProc(libgl, 'glSecondaryColor3ubEXT');
  glSecondaryColor3ubvEXT := GetProc(libgl, 'glSecondaryColor3ubvEXT');
  glSecondaryColor3uiEXT := GetProc(libgl, 'glSecondaryColor3uiEXT');
  glSecondaryColor3uivEXT := GetProc(libgl, 'glSecondaryColor3uivEXT');
  glSecondaryColor3usEXT := GetProc(libgl, 'glSecondaryColor3usEXT');
  glSecondaryColor3usvEXT := GetProc(libgl, 'glSecondaryColor3usvEXT');
  glSecondaryColorPointerEXT := GetProc(libgl, 'glSecondaryColorPointerEXT');
  glSelectBuffer := GetProc(libgl, 'glSelectBuffer');
  glSelectTextureSGIS := GetProc(libgl, 'glSelectTextureSGIS');
  glSelectTextureCoordSetSGIS := GetProc(libgl, 'glSelectTextureCoordSetSGIS');
  glSeparableFilter2D := GetProc(libgl, 'glSeparableFilter2D');
  glSetFenceNV := GetProc(libgl, 'glSetFenceNV');
  glSetWindowStereoModeNV := GetProc(libgl, 'glSetWindowStereoModeNV');
  glShadeModel := GetProc(libgl, 'glShadeModel');
  glStencilFunc := GetProc(libgl, 'glStencilFunc');
  glStencilMask := GetProc(libgl, 'glStencilMask');
  glStencilOp := GetProc(libgl, 'glStencilOp');
  glTbufferMask3DFX := GetProc(libgl, 'glTbufferMask3DFX');
  glTestFenceNV := GetProc(libgl, 'glTestFenceNV');
  glTexCoord1d := GetProc(libgl, 'glTexCoord1d');
  glTexCoord1dv := GetProc(libgl, 'glTexCoord1dv');
  glTexCoord1f := GetProc(libgl, 'glTexCoord1f');
  glTexCoord1fv := GetProc(libgl, 'glTexCoord1fv');
  glTexCoord1i := GetProc(libgl, 'glTexCoord1i');
  glTexCoord1iv := GetProc(libgl, 'glTexCoord1iv');
  glTexCoord1s := GetProc(libgl, 'glTexCoord1s');
  glTexCoord1sv := GetProc(libgl, 'glTexCoord1sv');
  glTexCoord2d := GetProc(libgl, 'glTexCoord2d');
  glTexCoord2dv := GetProc(libgl, 'glTexCoord2dv');
  glTexCoord2f := GetProc(libgl, 'glTexCoord2f');
  glTexCoord2fv := GetProc(libgl, 'glTexCoord2fv');
  glTexCoord2i := GetProc(libgl, 'glTexCoord2i');
  glTexCoord2iv := GetProc(libgl, 'glTexCoord2iv');
  glTexCoord2s := GetProc(libgl, 'glTexCoord2s');
  glTexCoord2sv := GetProc(libgl, 'glTexCoord2sv');
  glTexCoord3d := GetProc(libgl, 'glTexCoord3d');
  glTexCoord3dv := GetProc(libgl, 'glTexCoord3dv');
  glTexCoord3f := GetProc(libgl, 'glTexCoord3f');
  glTexCoord3fv := GetProc(libgl, 'glTexCoord3fv');
  glTexCoord3i := GetProc(libgl, 'glTexCoord3i');
  glTexCoord3iv := GetProc(libgl, 'glTexCoord3iv');
  glTexCoord3s := GetProc(libgl, 'glTexCoord3s');
  glTexCoord3sv := GetProc(libgl, 'glTexCoord3sv');
  glTexCoord4d := GetProc(libgl, 'glTexCoord4d');
  glTexCoord4dv := GetProc(libgl, 'glTexCoord4dv');
  glTexCoord4f := GetProc(libgl, 'glTexCoord4f');
  glTexCoord4fv := GetProc(libgl, 'glTexCoord4fv');
  glTexCoord4i := GetProc(libgl, 'glTexCoord4i');
  glTexCoord4iv := GetProc(libgl, 'glTexCoord4iv');
  glTexCoord4s := GetProc(libgl, 'glTexCoord4s');
  glTexCoord4sv := GetProc(libgl, 'glTexCoord4sv');
  glTexCoordPointer := GetProc(libgl, 'glTexCoordPointer');
  glTexCoordPointerEXT := GetProc(libgl, 'glTexCoordPointerEXT');  
  glTexEnvf := GetProc(libgl, 'glTexEnvf');
  glTexEnvfv := GetProc(libgl, 'glTexEnvfv');
  glTexEnvi := GetProc(libgl, 'glTexEnvi');
  glTexEnviv := GetProc(libgl, 'glTexEnviv');
  glTexGend := GetProc(libgl, 'glTexGend');
  glTexGendv := GetProc(libgl, 'glTexGendv');
  glTexGenf := GetProc(libgl, 'glTexGenf');
  glTexGenfv := GetProc(libgl, 'glTexGenfv');
  glTexGeni := GetProc(libgl, 'glTexGeni');
  glTexGeniv := GetProc(libgl, 'glTexGeniv');
  glTexImage1D := GetProc(libgl, 'glTexImage1D');
  glTexImage2D := GetProc(libgl, 'glTexImage2D');
  glTexImage3D := GetProc(libgl, 'glTexImage3D');
  glTexImage3DEXT := GetProc(libgl, 'glTexImage3DEXT');
  glTexParameterf := GetProc(libgl, 'glTexParameterf');
  glTexParameterfv := GetProc(libgl, 'glTexParameterfv');
  glTexParameteri := GetProc(libgl, 'glTexParameteri');
  glTexParameteriv := GetProc(libgl, 'glTexParameteriv');
  glTexSubImage1D := GetProc(libgl, 'glTexSubImage1D');
  glTexSubImage2D := GetProc(libgl, 'glTexSubImage2D');
  glTexSubImage3D := GetProc(libgl, 'glTexSubImage3D');
  glTexSubImage3DEXT := GetProc(libgl, 'glTexSubImage3DEXT');
  glTrackMatrixNV := GetProc(libgl, 'glTrackMatrixNV');
  glTranslated := GetProc(libgl, 'glTranslated');
  glTranslatef := GetProc(libgl, 'glTranslatef');
  glUnlockArraysEXT := GetProc(libgl, 'glUnlockArraysEXT');
  glValidBackBufferHintAutodesk := GetProc(libgl, 'glValidBackBufferHintAutodesk');
  glVertex2d := GetProc(libgl, 'glVertex2d');
  glVertex2dv := GetProc(libgl, 'glVertex2dv');
  glVertex2f := GetProc(libgl, 'glVertex2f');
  glVertex2fv := GetProc(libgl, 'glVertex2fv');
  glVertex2i := GetProc(libgl, 'glVertex2i');
  glVertex2iv := GetProc(libgl, 'glVertex2iv');
  glVertex2s := GetProc(libgl, 'glVertex2s');
  glVertex2sv := GetProc(libgl, 'glVertex2sv');
  glVertex3d := GetProc(libgl, 'glVertex3d');
  glVertex3dv := GetProc(libgl, 'glVertex3dv');
  glVertex3f := GetProc(libgl, 'glVertex3f');
  glVertex3fv := GetProc(libgl, 'glVertex3fv');
  glVertex3i := GetProc(libgl, 'glVertex3i');
  glVertex3iv := GetProc(libgl, 'glVertex3iv');
  glVertex3s := GetProc(libgl, 'glVertex3s');
  glVertex3sv := GetProc(libgl, 'glVertex3sv');
  glVertex4d := GetProc(libgl, 'glVertex4d');
  glVertex4dv := GetProc(libgl, 'glVertex4dv');
  glVertex4f := GetProc(libgl, 'glVertex4f');
  glVertex4fv := GetProc(libgl, 'glVertex4fv');
  glVertex4i := GetProc(libgl, 'glVertex4i');
  glVertex4iv := GetProc(libgl, 'glVertex4iv');
  glVertex4s := GetProc(libgl, 'glVertex4s');
  glVertex4sv := GetProc(libgl, 'glVertex4sv');
  glVertexArrayRangeNV := GetProc(libgl, 'glVertexArrayRangeNV');
  glVertexAttrib1dNV := GetProc(libgl, 'glVertexAttrib1dNV');
  glVertexAttrib1dvNV := GetProc(libgl, 'glVertexAttrib1dvNV');
  glVertexAttrib1fNV := GetProc(libgl, 'glVertexAttrib1fNV');
  glVertexAttrib1fvNV := GetProc(libgl, 'glVertexAttrib1fvNV');
  glVertexAttrib1sNV := GetProc(libgl, 'glVertexAttrib1sNV');
  glVertexAttrib1svNV := GetProc(libgl, 'glVertexAttrib1svNV');
  glVertexAttrib2dNV := GetProc(libgl, 'glVertexAttrib2dNV');
  glVertexAttrib2dvNV := GetProc(libgl, 'glVertexAttrib2dvNV');
  glVertexAttrib2fNV := GetProc(libgl, 'glVertexAttrib2fNV');
  glVertexAttrib2fvNV := GetProc(libgl, 'glVertexAttrib2fvNV');
  glVertexAttrib2sNV := GetProc(libgl, 'glVertexAttrib2sNV');
  glVertexAttrib2svNV := GetProc(libgl, 'glVertexAttrib2svNV');
  glVertexAttrib3dNV := GetProc(libgl, 'glVertexAttrib3dNV');
  glVertexAttrib3dvNV := GetProc(libgl, 'glVertexAttrib3dvNV');
  glVertexAttrib3fNV := GetProc(libgl, 'glVertexAttrib3fNV');
  glVertexAttrib3fvNV := GetProc(libgl, 'glVertexAttrib3fvNV');
  glVertexAttrib3sNV := GetProc(libgl, 'glVertexAttrib3sNV');
  glVertexAttrib3svNV := GetProc(libgl, 'glVertexAttrib3svNV');
  glVertexAttrib4dNV := GetProc(libgl, 'glVertexAttrib4dNV');
  glVertexAttrib4dvNV := GetProc(libgl, 'glVertexAttrib4dvNV');
  glVertexAttrib4fNV := GetProc(libgl, 'glVertexAttrib4fNV');
  glVertexAttrib4fvNV := GetProc(libgl, 'glVertexAttrib4fvNV');
  glVertexAttrib4sNV := GetProc(libgl, 'glVertexAttrib4sNV');
  glVertexAttrib4svNV := GetProc(libgl, 'glVertexAttrib4svNV');
  glVertexAttrib4ubNV := GetProc(libgl, 'glVertexAttrib4ubNV');
  glVertexAttrib4ubvNV := GetProc(libgl, 'glVertexAttrib4ubvNV');
  glVertexAttribPointerNV := GetProc(libgl, 'glVertexAttribPointerNV');
  glVertexAttribs1dvNV := GetProc(libgl, 'glVertexAttribs1dvNV');
  glVertexAttribs1fvNV := GetProc(libgl, 'glVertexAttribs1fvNV');
  glVertexAttribs1svNV := GetProc(libgl, 'glVertexAttribs1svNV');
  glVertexAttribs2dvNV := GetProc(libgl, 'glVertexAttribs2dvNV');
  glVertexAttribs2fvNV := GetProc(libgl, 'glVertexAttribs2fvNV');
  glVertexAttribs2svNV := GetProc(libgl, 'glVertexAttribs2svNV');
  glVertexAttribs3dvNV := GetProc(libgl, 'glVertexAttribs3dvNV');
  glVertexAttribs3fvNV := GetProc(libgl, 'glVertexAttribs3fvNV');
  glVertexAttribs3svNV := GetProc(libgl, 'glVertexAttribs3svNV');
  glVertexAttribs4dvNV := GetProc(libgl, 'glVertexAttribs4dvNV');
  glVertexAttribs4fvNV := GetProc(libgl, 'glVertexAttribs4fvNV');
  glVertexAttribs4svNV := GetProc(libgl, 'glVertexAttribs4svNV');
  glVertexAttribs4ubvNV := GetProc(libgl, 'glVertexAttribs4ubvNV');
  glVertexPointer := GetProc(libgl, 'glVertexPointer');
  glVertexPointerEXT := GetProc(libgl, 'glVertexPointerEXT');
  glVertexWeightPointerEXT := GetProc(libgl, 'glVertexWeightPointerEXT');
  glVertexWeightfEXT := GetProc(libgl, 'glVertexWeightfEXT');
  glVertexWeightfvEXT := GetProc(libgl, 'glVertexWeightfvEXT');
  glViewport := GetProc(libgl, 'glViewport');
  glWindowBackBufferHintAutodesk := GetProc(libgl, 'glWindowBackBufferHintAutodesk');
  glWindowPos2dARB := GetProc(libgl, 'glWindowPos2dARB');
  glWindowPos2dvARB := GetProc(libgl, 'glWindowPos2dvARB');
  glWindowPos2fARB := GetProc(libgl, 'glWindowPos2fARB');
  glWindowPos2fvARB := GetProc(libgl, 'glWindowPos2fvARB');
  glWindowPos2iARB := GetProc(libgl, 'glWindowPos2iARB');
  glWindowPos2ivARB := GetProc(libgl, 'glWindowPos2ivARB');
  glWindowPos2sARB := GetProc(libgl, 'glWindowPos2sARB');
  glWindowPos2svARB := GetProc(libgl, 'glWindowPos2svARB');
  glWindowPos3dARB := GetProc(libgl, 'glWindowPos3dARB');
  glWindowPos3dvARB := GetProc(libgl, 'glWindowPos3dvARB');
  glWindowPos3fARB := GetProc(libgl, 'glWindowPos3fARB');
  glWindowPos3fvARB := GetProc(libgl, 'glWindowPos3fvARB');
  glWindowPos3iARB := GetProc(libgl, 'glWindowPos3iARB');
  glWindowPos3ivARB := GetProc(libgl, 'glWindowPos3ivARB');
  glWindowPos3sARB := GetProc(libgl, 'glWindowPos3sARB');
  glWindowPos3svARB := GetProc(libgl, 'glWindowPos3svARB');

  GLInitialized := True;
  Result := True;
end;

function InitGL: Boolean;
begin
  Result := InitGLFromLibrary('libGL.so') or
            InitGLFromLibrary('libGL.so.1');
end;

initialization
  InitGL;
finalization
  if Assigned(libGL) then dlclose(libGL);
end.
{
  $Log$
  Revision 1.3  2002/04/15 10:54:58  lazarus
  MG: fixes from satan

  Revision 1.2  2001/11/12 17:36:47  lazarus
  MG: new version from satan

  Revision 1.2  2001/06/20 14:22:48  marco
   * Introduced Unix dir structure for opengl.

  Revision 1.6  2001/06/20 13:59:20  marco
   * Fixed breaking of Freebsd. Still requires copying linux to freebsd dir.

  Revision 1.5  2000/10/01 22:17:58  peter
    * new bounce demo

  Revision 1.4.2.1  2000/10/01 22:12:27  peter
    * new demo

  Revision 1.1  2000/07/13 06:34:17  michael
  + Initial import

  Revision 1.2  2000/05/31 00:34:28  alex
  made templates work

}

