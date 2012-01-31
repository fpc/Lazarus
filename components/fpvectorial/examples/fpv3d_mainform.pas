unit fpv3d_mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, EditBtn, StdCtrls, fpvectorial, gl, glu, lasvectorialreader;

type

  { TformFPV3D }

  TformFPV3D = class(TForm)
    Button1: TButton;
    buttonLoad: TButton;
    editFileName: TFileNameEdit;
    glControl: TOpenGLControl;
    procedure Button1Click(Sender: TObject);
    procedure buttonLoadClick(Sender: TObject);
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
var
  VecPage: TvVectorialPage;
  i: Integer;
  lPoint1, lPoint2, lPoint3: TvPoint;
  lEntity: TvEntity;
  lPos1, lPos2, lPos3: T3DPoint;
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
  //glRotatef(100, 220, 330, 0.0);

{  glBegin(GL_QUADS);
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
  glEnd();}

  //s==============================

  VecPage := VecDoc.GetCurrentPage();
  if VecPage = nil then Exit;
  for i := 0 to VecPage.GetEntitiesCount() - 3 do
  begin
    lEntity := VecPage.GetEntity(i);
    if not (lEntity is TvPoint) then Continue;
    lPoint1 := lEntity as TvPoint;

    lEntity := VecPage.GetEntity(i+1);
    if not (lEntity is TvPoint) then Continue;
    lPoint2 := lEntity as TvPoint;

    lEntity := VecPage.GetEntity(i+2);
    if not (lEntity is TvPoint) then Continue;
    lPoint3 := lEntity as TvPoint;

    glBegin(GL_TRIANGLES);		// Drawing Using Triangles
      lPos1 := lPoint1.GetNormalizedPos(VecPage);
      lPos2 := lPoint2.GetNormalizedPos(VecPage);
      lPos3 := lPoint3.GetNormalizedPos(VecPage);
      glColor3f(0.0,0.0,1.0);                           // Set The Color To Blue
      glVertex3f(lPos1.X, lPos1.Y, lPos1.Z);
      glVertex3f(lPos2.X, lPos2.Y, lPos2.Z);
      glVertex3f(lPos3.X, lPos3.Y, lPos3.Z);
    glEnd();					// Finished Drawing
  end;

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

procedure TformFPV3D.buttonLoadClick(Sender: TObject);
begin
  VecDoc.ReadFromFile(editFileName.FileName);
  glControl.Invalidate;
end;

procedure TformFPV3D.Button1Click(Sender: TObject);
begin
  glControl.Invalidate;
end;

procedure TformFPV3D.FormDestroy(Sender: TObject);
begin
  VecDoc.Free;
end;

end.

