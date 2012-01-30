unit fpv3d_mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, EditBtn, StdCtrls, fpvectorial, gl, glu;

type

  { TformFPV3D }

  TformFPV3D = class(TForm)
    Button1: TButton;
    FileNameEdit1: TFileNameEdit;
    glControl: TOpenGLControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure glControlPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    VecDoc: TvVectorialDocument;
  end; 

var
  formFPV3D: TformFPV3D;

implementation

{$R *.lfm}

{ TformFPV3D }

procedure TformFPV3D.glControlPaint(Sender: TObject);
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
  glRotatef(100, 220, 330, 0.0);

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

//  Speed := double(OpenGLControl1.FrameDiffTimeInMSecs)/10;

//  cube_rotationx += 5.15 * Speed;
//  cube_rotationy += 5.15 * Speed;
//  cube_rotationz += 20.0 * Speed;

  glControl.SwapBuffers;
end;

procedure TformFPV3D.FormCreate(Sender: TObject);
begin
  VecDoc := TvVectorialDocument.Create;
end;

procedure TformFPV3D.FormDestroy(Sender: TObject);
begin
  VecDoc.Free;
end;

end.

