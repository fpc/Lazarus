{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

}
unit GLCarbonAGLContext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, gl,
  FPCMacOSAll, InterfaceBase, CarbonInt,
  Controls;
  
procedure LOpenGLViewport(Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLCreateContext(AWinControl: TWinControl;
                          SharedControl: TWinControl; AttrList: PInteger): HWND;

const
  DefaultOpenGLContextInitAttrList: array [0..10] of LongInt = (
    GDK_GL_RGBA,
    GDK_GL_RED_SIZE, 1,
    GDK_GL_GREEN_SIZE, 1,
    GDK_GL_BLUE_SIZE, 1,
    GDK_GL_DEPTH_SIZE, 1,
    GDK_GL_DOUBLEBUFFER,
    GDK_GL_None
    );


implementation

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
begin

end;

procedure LOpenGLSwapBuffers(Handle: HWND);
begin

end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
begin

end;

function LOpenGLCreateContext(AWinControl: TWinControl;
  SharedControl: TWinControl; AttrList: PInteger): HWND;
begin

end;

end.

