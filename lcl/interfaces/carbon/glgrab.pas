{
 * Converted from glGrab.c by Felipe Monteiro de Carvalho
 * Found in http://lists.apple.com/archives/cocoa-dev/2005/Aug/msg00901.html
 * Line 186 (from the original C source)
   has a fix for Intel processors: http://lists.apple.com/archives/quartz-dev/2006/May/msg00100.html

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit glgrab;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

interface

{#import <CoreFoundation/CoreFoundation.h>
#import <ApplicationServices/ApplicationServices.h>
#import <OpenGL/OpenGL.h>
#import <OpenGL/gl.h>}
uses
  MacOSAll,
  GL, OpenGL;
  
function grabViaOpenGL(display: CGDirectDisplayID; srcRect: CGRect): CGImageRef;

implementation

{ Missing constants from FPCMacOSAll }
const
  kCGNullDirectDisplay = CGDirectDisplayID(0);

{
 * perform an in-place swap from Quadrant 1 to Quadrant III format
 * (upside-down PostScript/GL to right side up QD/CG raster format)
 * We do this in-place, which requires more copying, but will touch
 * only half the pages.  (Display grabs are BIG!)
 *
 * Pixel reformatting may optionally be done here if needed.
 }
procedure swizzleBitmap(data: Pointer; rowBytes, height: Integer);
var
  top, bottom: Integer;
  buffer: Pointer;
  topP: Pointer;
  bottomP: Pointer;
  base: Pointer;
begin
  top := 0;
  bottom := height - 1;
  base := data;
  buffer := System.GetMem(rowBytes);

  while ( top < bottom ) do
  begin
        topP := Pointer((top * rowBytes) + PtrInt(base));
        bottomP := Pointer((bottom * rowBytes) + PtrInt(base));

        {
         * Save and swap scanlines.
         *
         * This code does a simple in-place exchange with a temp buffer.
         * If you need to reformat the pixels, replace the first two bcopy()
         * calls with your own custom pixel reformatter.
         }
        System.Move(topP^, buffer^, rowBytes );
        System.Move(bottomP^, topP^, rowBytes );
        System.Move(buffer^, bottomP^, rowBytes );

        Inc(top);
        Dec(bottom);
    end;
    System.FreeMem( buffer );
end;

{
 * Given a display ID and a rectangle on that display, generate a CGImageRef
 * containing the display contents.
 *
 * srcRect is display-origin relative.
 *
 * This function uses a full screen OpenGL read-only context.
 * By using OpenGL, we can read the screen using a DMA transfer
 * when it's in millions of colors mode, and we can correctly read
 * a microtiled full screen OpenGL context, such as a game or full
 * screen video display.
 *
 * Returns a CGImageRef. When you are done with the CGImageRef, release it
 * using CFRelease().
 * Returns NULL on an error.
 }
function grabViaOpenGL(display: CGDirectDisplayID; srcRect: CGRect): CGImageRef;
var
  bitmap: CGContextRef;
  image: CGImageRef;
  data: Pointer;
  bytewidth: Longint;
  width, height: GLint;
  bytes: Longint;
  cSpace: CGColorSpaceRef;

  glContextObj: CGLContextObj;
  pixelFormatObj: CGLPixelFormatObj;
  numPixelFormats: Longint;
  attribs: array[0..3] of CGLPixelFormatAttribute =
  (
      kCGLPFAFullScreen,
      kCGLPFADisplayMask,
      CGLPixelFormatAttribute(0),    { Display mask bit goes here }
      CGLPixelFormatAttribute(0)
  );
begin
  cSpace := CGColorSpaceCreateWithName (kCGColorSpaceGenericRGB);

  if ( display = kCGNullDirectDisplay ) then
        display := CGMainDisplayID();
  attribs[2] := CGLPixelFormatAttribute(CGDisplayIDToOpenGLDisplayMask(display));


    { Build a full-screen GL context }
    CGLChoosePixelFormat( attribs, @pixelFormatObj, @numPixelFormats );
    if ( pixelFormatObj = nil ) then   // No full screen context support
        Exit(nil);
    CGLCreateContext( pixelFormatObj, nil, @glContextObj ) ;
    CGLDestroyPixelFormat( pixelFormatObj ) ;
    if ( glContextObj = nil ) then
        Exit(nil);


    CGLSetCurrentContext( glContextObj ) ;
    CGLSetFullScreen( glContextObj ) ;


    glReadBuffer(GL_FRONT);


    width := Round(srcRect.size.width);
    height := Round(srcRect.size.height);


    bytewidth := width * 4; // Assume 4 bytes/pixel for now
    bytewidth := (bytewidth + 3) and (not 3); // Align to 4 bytes
    bytes := bytewidth * height; // width * height

    { Build bitmap context }
    data := System.GetMem(height * bytewidth);
    if ( data = nil ) then
    begin
        CGLSetCurrentContext( nil );
        CGLClearDrawable( glContextObj ); // disassociate from full screen
        CGLDestroyContext( glContextObj ); // and destroy the context
        Result := nil;
        Exit;
    end;
    bitmap := CGBitmapContextCreate(data, width, height, 8, bytewidth,
                                   cSpace, kCGImageAlphaNoneSkipFirst { XRGB });
    CFRelease(cSpace);


    { Read framebuffer into our bitmap }
    glFinish(); { Finish all OpenGL commands }
    glPixelStorei(GL_PACK_ALIGNMENT, 4); { Force 4-byte alignment }
    glPixelStorei(GL_PACK_ROW_LENGTH, 0);
    glPixelStorei(GL_PACK_SKIP_ROWS, 0);
    glPixelStorei(GL_PACK_SKIP_PIXELS, 0);

    {
     * Fetch the data in XRGB format, matching the bitmap context.
     }
    glReadPixels(Round(srcRect.origin.x), Round(srcRect.origin.y), width, height,
                 GL_BGRA,
{$ifdef __BIG_ENDIAN__}
                 GL_UNSIGNED_INT_8_8_8_8_REV, // for PPC
{$else}
                 GL_UNSIGNED_INT_8_8_8_8, // for Intel! http://lists.apple.com/archives/quartz-dev/2006/May/msg00100.html
{$endif}
                 data);
  {
   * glReadPixels generates a quadrant I raster, with origin in the lower left
   * This isn't a problem for signal processing routines such as compressors,
   * as they can simply use a negative 'advance' to move between scanlines.
   * CGImageRef and CGBitmapContext assume a quadrant III raster, though, so we need to
   * invert it. Pixel reformatting can also be done here.
   }
  swizzleBitmap(data, bytewidth, height);

  { Make an image out of our bitmap; does a cheap vm_copy of the bitmap }
  image := CGBitmapContextCreateImage(bitmap);

  { Get rid of bitmap }
  CFRelease(bitmap);
  System.FreeMem(data);

  { Get rid of GL context }
  CGLSetCurrentContext( nil );
  CGLClearDrawable( glContextObj ); // disassociate from full screen
  CGLDestroyContext( glContextObj ); // and destroy the context

  { Returned image has a reference count of 1 }
  Result := image;
end;

end.

