{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

}
unit gtkglarea;

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, VCLGlobals, LCLLinux, LCLType, glib, gdk, gtk,
  gtkglarea_int, gl, Controls, gtkint, gtkwinapiwindow, LMessages;
  
type
  TCustomGTKGLAreaControl = class(TWinControl)
  protected
    function GetWidget: PGtkGLArea;
    procedure CreateWnd; override;
    procedure AttachSignals; override;
  public
    property Widget: PGtkGLArea read GetWidget;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
  TGTKGLAreaControl = class(TCustomGTKGLAreaControl)
  published
  end;

implementation

{ TCustomGTKGLArea }

const
   InitAttrList: array [1..11] of LongInt=
                    ( GDK_GL_RGBA,
                    GDK_GL_RED_SIZE, 1,
                    GDK_GL_GREEN_SIZE, 1,
                    GDK_GL_BLUE_SIZE, 1,
                    GDK_GL_DEPTH_SIZE,1,
                    GDK_GL_DOUBLEBUFFER,
                    GDK_GL_None
                    );  

constructor TCustomGTKGLAreaControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(1, 1, 75, 25);
end;

destructor TCustomGTKGLAreaControl.Destroy;
begin
  inherited Destroy;
end;

function TCustomGTKGLAreaControl.GetWidget: PGtkGLArea;
begin
  if HandleAllocated then
    Result:=PGtkGLArea(Handle)
  else
    Result:=nil;
end;

procedure TCustomGTKGLAreaControl.CreateWnd;
var
  Params: TCreateParams;
begin
  CreateParams(Params);
  with Params do begin
    if (WndParent = 0) and (Style and WS_CHILD <> 0) then exit;
  end;
  
  Handle := longint(gtk_gl_area_new(pgint(@InitAttrList)));
  if Widget <> nil then begin
    gtk_object_set_data(pgtkobject(Widget),'Sender',Self);
    gtk_object_set_data(pgtkobject(Widget), 'Class', Pointer(Self));
    gtk_object_set_data(pgtkObject(Widget),'Style',0);
    gtk_object_set_data(pgtkObject(Widget),'ExStyle',0);
  end else begin
    writeln('Creation of gtkglarea failed.');
    Halt(1);
  end;
  if Parent <> nil then AddControl;
  
  AttachSignals;
  InitializeWnd;
end;

procedure TCustomGTKGLAreaControl.AttachSignals;
begin
  // Attach callbacks
  SetCallback(LM_DESTROY);
  SetCallback(LM_SHOWWINDOW);
  SetCallback(LM_FOCUS);
  SetCallback(LM_WINDOWPOSCHANGED);
  SetCallback(LM_PAINT);
  SetCallback(LM_EXPOSEEVENT);
  SetCallback(LM_KEYDOWN);
  SetCallback(LM_KEYUP);
  SetCallback(LM_CHAR);
  SetCallback(LM_MOUSEMOVE);
  SetCallback(LM_LBUTTONDOWN); 
  SetCallback(LM_LBUTTONUP); 
  SetCallback(LM_RBUTTONDOWN);
  SetCallback(LM_RBUTTONUP);
  SetCallback(LM_MBUTTONDOWN);
  SetCallback(LM_MBUTTONUP); 
  SetCallback(LM_MOUSEWHEEL);
end;

//-----------------------------------------------------------------------------

procedure InternalInit;
begin
  (* OpenGL functions can be called only if make_current returns true *)
  if not InitGl then begin
    WriteLn('OpenGL is not supported on this system');
    Halt(2);
  end;
end;


initialization

InternalInit;

finalization


end.
