{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

}
unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, GL, GLU;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
  private
  public
    cube_rotationx: GLFloat;
    cube_rotationy: GLFloat;
    cube_rotationz: GLFloat;
    OpenGLControl1: TOpenGLControl;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenGLControl1:=TOpenGLControl.Create(Self);
  with OpenGLControl1 do begin
    Name:='OpenGLControl1';
    Align:=alClient;
    Parent:=Self;
    OnPaint:=@OpenGLControl1Paint;
    OnResize:=@OpenGLControl1Resize;
    AutoResizeViewport:=true;
  end;
  
  Application.AddOnIdleHandler(@OnAppIdle);
end;

procedure TForm1.OpenGLControl1Paint(Sender: TObject);
var
  Speed: Double;
begin
  glClearColor(1.0, 1.0, 1.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(45.0, double(width) / height, 0.1, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glTranslatef(0.0, 0.0,-6.0);
  glRotatef(cube_rotationx, cube_rotationy, cube_rotationz, 0.0);

  glBegin(GL_QUADS);
          glColor3f(0.0,1.0,0.0);                              // Set The Color To Green
          glVertex3f( 1.0, 1.0,-1.0);                  // Top Right Of The Quad (Top)
          glVertex3f(-1.0, 1.0,-1.0);                  // Top Left Of The Quad (Top)
          glVertex3f(-1.0, 1.0, 1.0);                  // Bottom Left Of The Quad (Top)
          glVertex3f( 1.0, 1.0, 1.0);                  // Bottom Right Of The Quad (Top)
  glEnd();
  glBegin(GL_QUADS);
          glColor3f(1.0,0.5,0.0);                              // Set The Color To Orange
          glVertex3f( 1.0,-1.0, 1.0);                  // Top Right Of The Quad (Bottom)
          glVertex3f(-1.0,-1.0, 1.0);                  // Top Left Of The Quad (Bottom)
          glVertex3f(-1.0,-1.0,-1.0);                  // Bottom Left Of The Quad (Bottom)
          glVertex3f( 1.0,-1.0,-1.0);                  // Bottom Right Of The Quad (Bottom)
  glEnd();
  glBegin(GL_QUADS);
          glColor3f(1.0,0.0,0.0);                              // Set The Color To Red
          glVertex3f( 1.0, 1.0, 1.0);                  // Top Right Of The Quad (Front)
          glVertex3f(-1.0, 1.0, 1.0);                  // Top Left Of The Quad (Front)
          glVertex3f(-1.0,-1.0, 1.0);                  // Bottom Left Of The Quad (Front)
          glVertex3f( 1.0,-1.0, 1.0);                  // Bottom Right Of The Quad (Front)
  glEnd();
  glBegin(GL_QUADS);
          glColor3f(1.0,1.0,0.0);                              // Set The Color To Yellow
          glVertex3f( 1.0,-1.0,-1.0);                  // Bottom Left Of The Quad (Back)
          glVertex3f(-1.0,-1.0,-1.0);                  // Bottom Right Of The Quad (Back)
          glVertex3f(-1.0, 1.0,-1.0);                  // Top Right Of The Quad (Back)
          glVertex3f( 1.0, 1.0,-1.0);                  // Top Left Of The Quad (Back)
  glEnd();
  glBegin(GL_QUADS);
          glColor3f(0.0,0.0,1.0);                              // Set The Color To Blue
          glVertex3f(-1.0, 1.0, 1.0);                  // Top Right Of The Quad (Left)
          glVertex3f(-1.0, 1.0,-1.0);                  // Top Left Of The Quad (Left)
          glVertex3f(-1.0,-1.0,-1.0);                  // Bottom Left Of The Quad (Left)
          glVertex3f(-1.0,-1.0, 1.0);                  // Bottom Right Of The Quad (Left)
  glEnd();
  glBegin(GL_QUADS);
          glColor3f(1.0,0.0,1.0);                              // Set The Color To Violet
          glVertex3f( 1.0, 1.0,-1.0);                  // Top Right Of The Quad (Right)
          glVertex3f( 1.0, 1.0, 1.0);                  // Top Left Of The Quad (Right)
          glVertex3f( 1.0,-1.0, 1.0);                  // Bottom Left Of The Quad (Right)
          glVertex3f( 1.0,-1.0,-1.0);                  // Bottom Right Of The Quad (Right)
  glEnd();

  Speed := double(OpenGLControl1.FrameDiffTimeInMSecs)/10;

  cube_rotationx += 5.15 * Speed;
  cube_rotationy += 5.15 * Speed;
  cube_rotationz += 20.0 * Speed;

  OpenGLControl1.SwapBuffers;
end;

procedure TForm1.OpenGLControl1Resize(Sender: TObject);
begin
  if OpenGLControl1.Height <= 0 then exit;
  // the viewport is automatically resized by the TOpenGLControl
  // you can disable it (OpenGLControl1.AutoResizeViewport:=false)
  // and do something yourself here
end;

procedure TForm1.OnAppIdle(Sender: TObject; var Done: Boolean);
begin
  Done:=false;
  //DebugLn(['TForm1.OnAppIdle ']);
  OpenGLControl1.Invalidate;
end;

end.

