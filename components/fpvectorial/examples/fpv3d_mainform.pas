unit fpv3d_mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, EditBtn, StdCtrls, fpvectorial, gl, glu, FPimage, lasvectorialreader;

type

  { TformFPV3D }

  TformFPV3D = class(TForm)
    Button1: TButton;
    buttonRotZ: TButton;
    buttonZoomIn: TButton;

    buttonZoomOut: TButton;  buttonLoad: TButton;
    editFileName: TFileNameEdit;
    glControl: TOpenGLControl;
    procedure Button1Click(Sender: TObject);
    procedure buttonLoadClick(Sender: TObject);
    procedure buttonRotZClick(Sender: TObject);
    procedure buttonZoomInClick(Sender: TObject);
    procedure buttonZoomOutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure glControlPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    VecDoc: TvVectorialDocument;
    glAltitude: Integer;
    glRotateAngle, glRotateX, glRotateY, glRotateZ: Double;
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
  lColor: TFPColor;
begin
  glClearColor(1.0, 1.0, 1.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(45.0, double(width) / height, 0.1, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glTranslatef(0.0, 0.0,-glAltitude);

  if glRotateAngle <> 0 then
    glRotatef(glRotateAngle, glRotateX, glRotateY, glRotateZ);

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
      lPos1 := lPoint1.GetNormalizedPos(VecPage, -1, 1);
      lPos2 := lPoint2.GetNormalizedPos(VecPage, -1, 1);
      lPos3 := lPoint3.GetNormalizedPos(VecPage, -1, 1);
      lColor := lPoint1.Pen.Color;
      glColor3f(lColor.Red / $FFFF, lColor.Green / $FFFF, lColor.Blue / $FFFF);
      glVertex3f(lPos1.X, lPos1.Y, lPos1.Z);
      glVertex3f(lPos2.X, lPos2.Y, lPos2.Z);
      glVertex3f(lPos3.X, lPos3.Y, lPos3.Z);
    glEnd();					// Finished Drawing
  end;

  glControl.SwapBuffers;
end;

procedure TformFPV3D.FormCreate(Sender: TObject);
begin
  VecDoc := TvVectorialDocument.Create;
  glAltitude := 3;
end;

procedure TformFPV3D.buttonLoadClick(Sender: TObject);
begin
  VecDoc.ReadFromFile(editFileName.FileName);
  glControl.Invalidate;
end;

procedure TformFPV3D.buttonRotZClick(Sender: TObject);
begin
  glRotateAngle := glRotateAngle + 10;
  glRotateX := 0.0;
  glRotateY := 0.0;
  glRotateZ := 1.0;
  glControl.Invalidate;
end;

procedure TformFPV3D.buttonZoomInClick(Sender: TObject);
begin
  Dec(glAltitude);
  if glAltitude < 1 then glAltitude := 1;
  glControl.Invalidate;
end;

procedure TformFPV3D.buttonZoomOutClick(Sender: TObject);
begin
  Inc(glAltitude);
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

