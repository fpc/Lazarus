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
    gtk_object_set_data(pgtkobject(Widget),'Class', Pointer(Self));
    gtk_object_set_data(pgtkObject(Widget),'Style',0);
    gtk_object_set_data(pgtkObject(Widget),'ExStyle',0);
  end else begin
    writeln('Creation of gtkglarea failed.');
    Halt(1);
  end;
  if Parent <> nil then AddControl;
  
  InitializeWnd;
end;

//-----------------------------------------------------------------------------

procedure InternalInit;
begin
  if not InitGl then begin
    WriteLn('OpenGL is not supported on this system');
    Halt(2);
  end;
end;


initialization
  InternalInit;

finalization


end.
