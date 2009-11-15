//----------------------------------------------------------------------------
// Anti-Grain Geometry - Version 2.4 (Public License)
// Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)
// Copyright (C) 2002 Hansruedi Baer (MacOS support)
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
//          baer@karto.baug.eth.ch
//
// [Pascal Port History] -----------------------------------------------------
//
// 24.03.2006-Milano: Finished MacOS port
// 20.03.2006-Milano: Unit port establishment
//
{ agg_mac_pmap.pas }
unit
 agg_mac_pmap ;

INTERFACE

{$I agg_mode.inc }

uses
 QuickTimeComponents ,ImageCompression ,Carbon ,
 agg_basics ;

{ TYPES DEFINITION }
const
 org_mono8   = 8;
 org_color16 = 16;
 org_color24 = 24;
 org_color32 = 32;

type
 pixel_map = object
   m_pmap : ^CGrafPort;
   m_buf  : pointer;

   m_bpp      ,
   m_img_size : unsigned;

   constructor Construct;
   destructor  Destruct;

   procedure destroy;
   procedure create(width ,height ,org : unsigned; clear_val : unsigned = 255 );
   procedure clear (clear_val : unsigned = 255 );

   function  load_from_qt(filename : shortstring ) : boolean;
   function  save_as_qt  (filename : shortstring ) : boolean;

   procedure draw(window : WindowRef; device_rect : RectPtr = NIL; bmp_rect : RectPtr = NIL ); overload;
   procedure draw(window : WindowRef; x ,y : int; scale : double = 1.0 ); overload;

   procedure blend(window : WindowRef; device_rect : RectPtr = NIL; bmp_rect : RectPtr = NIL ); overload;
   procedure blend(window : WindowRef; x ,y : int; scale : double = 1.0 ); overload;

   function  _buf : pointer;
   function  _width : unsigned;
   function  _height : unsigned;
   function  _row_bytes : int;
   function  _bpp : unsigned;

  //Auxiliary static functions
   function  calc_row_len(width ,bits_per_pixel : unsigned ) : unsigned;

  end;

{ GLOBAL PROCEDURES }


IMPLEMENTATION
{ LOCAL VARIABLES & CONSTANTS }
{ UNIT IMPLEMENTATION }
{ CONSTRUCT }
constructor pixel_map.Construct;
begin
 m_pmap:=NIL;
 m_buf :=NIL;
 m_bpp :=0;
 
 m_img_size:=0;
 
end;

{ DESTRUCT }
destructor pixel_map.Destruct;
begin
 destroy;

end;

{ DESTROY }
procedure pixel_map.destroy;
begin
 agg_freemem(m_buf ,m_img_size );

 m_buf:=NIL;

 if m_pmap <> NIL then
  begin 
   DisposeGWorld(GrafPtr(m_pmap ) );
   
   m_pmap:=NIL;
   
  end;
    
end;

{ CREATE }
procedure pixel_map.create;
var
 r : Carbon.Rect;
 
 row_bytes : int;

begin
 destroy;
 
 if width = 0 then
  width:=1;
  
 if height = 0 then
  height:=1;
  
 m_bpp    :=org;
 row_bytes:=calc_row_len(width ,m_bpp );
 
 SetRect(r ,0 ,0 ,width ,height );
 
 m_img_size:=row_bytes * height;

 agg_getmem(m_buf ,m_img_size );

// The Quicktime version for creating GWorlds is more flexible than the classical function.
 QTNewGWorldFromPtr(m_pmap ,m_bpp ,ImageCompression.Rect(r ) ,NIL ,NIL ,0 ,m_buf ,row_bytes );

// create_gray_scale_palette(m_pmap);  I didn't care about gray scale palettes so far.
 if clear_val <= 255 then
  fillchar(m_buf^ ,m_img_size ,clear_val );

end;

{ CLEAR }
procedure pixel_map.clear;
begin
 if m_buf <> NIL then
  fillchar(m_buf^ ,m_img_size ,clear_val );

end;

{ LOAD_FROM_QT }
function pixel_map.load_from_qt;
var
 fss : FSSpec;
 err : OSErr;
 gi  : GraphicsImportComponent;
 buf : int8u_ptr;

 desc  : ImageDescriptionHandle;
 depth : int16;
 size  : unsigned;

begin
// get file specification to application directory
 err:=HGetVol(NIL ,fss.vRefNum ,fss.parID );
 
 if err = noErr then
  begin
  // CopyCStringToPascal(filename ,fss.name );
   fss.name:=filename;  
   
   err:=GetGraphicsImporterForFile(ImageCompression.FSSpec(fss ) ,gi );
   
   if err = noErr then
    begin
     GraphicsImportGetImageDescription(gi ,desc );
 
    // For simplicity, all images are currently converted to 32 bit.
    // create an empty pixelmap
     depth:=24;
     
     create       (desc^.width ,desc^.height ,depth ,$ff );
     DisposeHandle(Handle(desc ) );
 
    // let Quicktime draw to pixelmap
     GraphicsImportSetGWorld(gi ,m_pmap ,NIL );
     GraphicsImportDraw     (gi );
  
    end;

  end;

 result:=err = noErr;

end;

{ SAVE_AS_QT }
function pixel_map.save_as_qt;
var
 fss : FSSpec;
 err : OSErr;
 ge  : GraphicsExportComponent;
 cnt : UInt32;
 
begin
// get file specification to application directory
 err:=HGetVol(NIL ,fss.vRefNum ,fss.parID );
 
 if err = noErr then
  begin
  // CopyCStringToPascal(filename ,fss.name );
   fss.name:=filename;
  
  // I decided to use PNG as output image file type.
  // There are a number of other available formats.
  // Should I check the file suffix to choose the image file format?
   err:=
    OpenADefaultComponent(
     LongWord(int32_ptr(@GraphicsExporterComponentType[1 ] )^ ) ,
     LongWord(int32_ptr(@kQTFileTypePNG[1 ] )^ )	,
     Carbon.ComponentInstance(ge ) );
 
   if err = noErr then
    begin
     err:=GraphicsExportSetInputGWorld(ge ,m_pmap );

     if err = noErr then
      begin 
       err:=GraphicsExportSetOutputFile(ge ,ImageCompression.FSSpec(fss ) );
       cnt:=0;
   
       if err = noErr then
        GraphicsExportDoExport(ge ,cnt );

      end;
  
     CloseComponent(Carbon.ComponentInstance(ge ) );
 
    end;

  end;

 result:=err = noErr;

end;

{ DRAW }
procedure pixel_map.draw(window : WindowRef; device_rect : RectPtr = NIL; bmp_rect : RectPtr = NIL );
var
 pm   : PixMapHandle;
 port : CGrafPtr;
 
 src_rect ,dest_rect : Carbon.Rect;

 image_description : ImageDescriptionHandle;

begin
 if (m_pmap = NIL ) or 
    (m_buf = NIL ) then
  exit;

 pm  :=GetGWorldPixMap(GrafPtr(m_pmap ) );
 port:=GetWindowPort  (window );

// Again, I used the Quicktime version.
// Good old 'CopyBits' does better interpolation when scaling
// but does not support all pixel depths.
 SetRect(dest_rect ,0 ,0 ,_width ,_height );
 
 MakeImageDescriptionForPixMap(ImageCompression.PixMapHandle(pm ) ,image_description );
 
 if image_description <> NIL then
  begin
   SetRect(src_rect ,0 ,0 ,image_description^.width ,image_description^.height );  
  
   DecompressImage(
    GetPixBaseAddr(pm ) ,
    image_description ,
    ImageCompression.PixMapHandle(GetPortPixMap(port ) ) ,
    ImageCompression.Rect(src_rect ) ,
    ImageCompression.Rect(dest_rect ) ,
    ditherCopy ,NIL );

   DisposeHandle(Handle(image_description ) );
   
  end;

end;

{ DRAW }
procedure pixel_map.draw(window : WindowRef; x ,y : int; scale : double = 1.0 );
var
 width ,height : unsigned;

 r : Carbon.Rect;

begin
 if (m_pmap = NIL ) or
    (m_buf = NIL ) then
  exit;

 width :=System.Trunc(_width * scale );
 height:=System.Trunc(_height * scale );

 SetRect(r ,x ,y ,x + width ,y + height );
 draw   (window ,@r );
 
end;

{ BLEND }
procedure pixel_map.blend(window : WindowRef; device_rect : RectPtr = NIL; bmp_rect : RectPtr = NIL );
begin
 draw(window ,device_rect ,bmp_rect ); // currently just mapped to drawing method
 
end;

{ BLEND }
procedure pixel_map.blend(window : WindowRef; x ,y : int; scale : double = 1.0 );
begin
 draw(window ,x ,y ,scale ); // currently just mapped to drawing method
 
end;

{ _BUF }
function pixel_map._buf;
begin
 result:=m_buf;
 
end;

{ _WIDTH }
function pixel_map._width;
var
 pm : PixMapHandle;
 
 bounds : Carbon.Rect;

begin
 if m_pmap = NIL then
  begin
   result:=0;
   
   exit;
   
  end; 
  
 pm:=GetGWorldPixMap(GrafPtr(m_pmap ) );
 
 GetPixBounds(pm ,bounds );
 
 result:=bounds.right - bounds.left;

end;

{ _HEIGHT }
function pixel_map._height;
var
 pm : PixMapHandle;
 
 bounds : Carbon.Rect;

begin
 if m_pmap = NIL then
  begin
   result:=0;
  
   exit;
 
  end;
   
 pm:=GetGWorldPixMap(GrafPtr(m_pmap ) );
 
 GetPixBounds(pm ,bounds );
 
 result:=bounds.bottom - bounds.top;

end;

{ _ROW_BYTES }
function pixel_map._row_bytes;
var
 pm : PixMapHandle;

begin
 if m_pmap = NIL then
  begin
   result:=0;
   
   exit;
   
  end;
   
 pm:=GetGWorldPixMap(GrafPtr(m_pmap ) );

 result:=calc_row_len(_width ,GetPixDepth(pm ) );

end;

{ _BPP }
function pixel_map._bpp;
begin
 result:=m_bpp;
 
end;

{ CALC_ROW_LEN }
function pixel_map.calc_row_len;
var
 n ,k : unsigned;
 
begin
 n:=width;

 case bits_per_pixel of
  1 : 
   begin
    k:=n;
    n:=n shr 3;

    if k and 7 <> 0 then
     inc(n ); 

   end;

  4 :
   begin
    k:=n;
    n:=n shr 1;

    if k and 3 <> 0 then
     inc(n ); 

   end;

  8 :
   NoP;

  16 :
   n:=n shl 1;
   
  24 : 
   n:=(n shl 1 ) + n; 

  32 : 
   n:=n shl 2;

  else
   n:=0;
   
 end;
 
 result:=((n + 3 ) shr 2 ) shl 2;

end;

END.

