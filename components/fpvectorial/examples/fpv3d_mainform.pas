unit fpv3d_mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, EditBtn, StdCtrls, fpvectorial, gl, glu, FPimage,
  Math, lasvectorialreader;

type

  { TformFPV3D }

  TformFPV3D = class(TForm)
    Button1: TButton;
    btnConvert3DPointArrayToHeightMap: TButton;
    buttonCutFile: TButton;
    buttonRotZ: TButton;
    buttonZoomIn: TButton;

    buttonZoomOut: TButton;  buttonLoad: TButton;
    editFileName: TFileNameEdit;
    glControl: TOpenGLControl;
    procedure btnConvert3DPointArrayToHeightMapClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure buttonCutFileClick(Sender: TObject);
    procedure buttonLoadClick(Sender: TObject);
    procedure buttonRotZClick(Sender: TObject);
    procedure buttonZoomInClick(Sender: TObject);
    procedure buttonZoomOutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure glControlPaint(Sender: TObject);
  private
    { private declarations }
    procedure Render3DPointsArrayAlternative1();
    //
    function GetMapHeight(X, Y: Integer): Byte;
    procedure SetVertexColor(bRenderPolygons: Boolean; x, y: Integer);
    procedure RenderHeightMapV1Helper(bRenderPolygons: Boolean);
    procedure RenderHeightMapV1;
  public
    { public declarations }
    VecDoc: TvVectorialDocument;
    glAltitude: Integer;
    glRotateAngle, glRotateX, glRotateY, glRotateZ: Double;
    HeightMap: TvRasterImage;
  end; 

const
  STEP_SIZE    = 16;		     // Width And Height Of Each Quad (NEW)
  HEIGHT_RATIO = 1.5;		     // Ratio That The Y Is Scaled According To The X And Z (NEW)

var
  formFPV3D: TformFPV3D;

implementation

{$R *.lfm}

{ TformFPV3D }

procedure TformFPV3D.glControlPaint(Sender: TObject);
begin
  glControl.SwapBuffers;

  //Render3DPointsArrayAlternative1;

  RenderHeightMapV1();
end;

procedure TformFPV3D.Render3DPointsArrayAlternative1;
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
end;

function TformFPV3D.GetMapHeight(X, Y: Integer): Byte;
var
  lPos: TPoint;
begin
  lPos.X := Min(X, HeightMap.RasterImage.Width-1);
  lPos.Y := Min(Y, HeightMap.RasterImage.Height-1);
  Result := Byte(HeightMap.RasterImage.Colors[lPos.X, lPos.Y].Red div $FF);
end;

{-----------------------------------------------------------------------------}
{  Sets The Color Value For A Particular Index, Depending On The Height Index }
{-----------------------------------------------------------------------------}
procedure TformFPV3D.SetVertexColor(bRenderPolygons: Boolean; x, y : Integer);
var fColor : glFloat;
begin
  fColor :=-0.2 + GetMapHeight(X, Y) / $FF;

  // Assign This Blue Shade To The Current Vertex
  if bRenderPolygons then
    glColor3f((220-104*fColor)/256, (220-110*abs(fColor-0.4))/256, (220-200*abs(fColor-0.6))/256)
  else
    glColor3i(0, 0, 0);
end;

procedure TformFPV3D.RenderHeightMapV1Helper(bRenderPolygons: Boolean);
var
  X, Y : Integer;
  x2, y2, z2 : Integer;
begin
  if HeightMap = nil then Exit;
  if HeightMap.RasterImage = nil then Exit;

  if (bRenderPolygons) then                        // What We Want To Render
    glBegin( GL_QUADS )                    // Render Polygons
  else
    glBegin( GL_LINES );                   // Render Lines Instead

  X :=0;
  while X < HeightMap.RasterImage.Width-1 do
  begin
    Y :=0;
    while Y < HeightMap.RasterImage.Height-1 do
    begin
      // Get The (X, Y, Z) Value For The Bottom Left Vertex
      x2 := X;
      y2 := GetMapHeight(X, Y);
      z2 := Y;

      // Set The Color Value Of The Current Vertex
      SetVertexColor(bRenderPolygons, x2, z2);

      // Send This Vertex To OpenGL To Be Rendered (Integer Points Are Faster)
      glVertex3i(x2, y2, z2);

      // Get The (X, Y, Z) Value For The Top Left Vertex
      x2 := X;
      y2 := GetMapHeight(X, Y + STEP_SIZE);
      z2 := Y + STEP_SIZE ;

      // Set The Color Value Of The Current Vertex
      SetVertexColor(bRenderPolygons, x2, z2);

      // Send This Vertex To OpenGL To Be Rendered
      glVertex3i(x2, y2, z2);

      // Get The (X, Y, Z) Value For The Top Right Vertex
      x2 := X + STEP_SIZE;
      y2 := GetMapHeight(X + STEP_SIZE, Y + STEP_SIZE);
      z2 := Y + STEP_SIZE ;

      // Set The Color Value Of The Current Vertex
      SetVertexColor(bRenderPolygons, x2, z2);

      // Send This Vertex To OpenGL To Be Rendered
      glVertex3i(x2, y2, z2);

      // Get The (X, Y, Z) Value For The Bottom Right Vertex
      x2 := X + STEP_SIZE;
      y2 := GetMapHeight(X + STEP_SIZE, Y );
      z2 := Y;

      // Set The Color Value Of The Current Vertex
      SetVertexColor(bRenderPolygons, x2, z2);

      // Send This Vertex To OpenGL To Be Rendered
      glVertex3i(x2, y2, z2);

      Y :=Y + STEP_SIZE
    end;
    X := X + STEP_SIZE
  end;
  glEnd();
  glColor4f(1.0, 1.0, 1.0, 1.0);             // Reset The Color
end;

procedure TformFPV3D.RenderHeightMapV1();
var
  ScaleValue: Double;
begin
  // Init
  glClearColor(0.0, 0.0, 0.0, 0.5); 	   // Black Background
  glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
  glClearDepth(1.0);                       // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
  glDepthFunc(GL_LEQUAL);	           // The Type Of Depth Test To Do
  glDisable(GL_TEXTURE_2D);                // Disable Texture Mapping
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations

  // Resize
  glViewport(0, 0, Width, Height);    // Set the viewport for the OpenGL window
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity();                   // Reset View
  gluPerspective(45.0, glControl.Width/glControl.Height, 1.0, 500.0);  // Do the perspective calculations. Last value = max clipping depth
  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glLoadIdentity();                   // Reset View

  //bRender :=TRUE;
  ScaleValue := 0.18 - glAltitude * 0.01;

  // Paint repetition

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity();                                       // Reset The View
  //           Position        View      Up Vector
  gluLookAt(212, 60, 194,  186, 55, 171,  0, 1, 0);	  // This Determines Where The Camera's Position And View Is
  glScalef(scaleValue, scaleValue * HEIGHT_RATIO, scaleValue);

  RenderHeightMapV1Helper(True);
  RenderHeightMapV1Helper(False);
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

procedure TformFPV3D.btnConvert3DPointArrayToHeightMapClick(Sender: TObject);
var
  lRasterImage: TvRasterImage;
  lPage: TvVectorialPage;
  lFile: TFileStream;
  x, y: Integer;
  lRed: Word;
begin
  lRasterImage := TvRasterImage.Create;
  HeightMap := lRasterImage;
  lPage := VecDoc.GetPage(0);
  lPage.AddEntity(lRasterImage);
  lRasterImage.InitializeWithConvertionOf3DPointsToHeightMap(lPage, 1024, 1024);

  lFile := TFileStream.Create('Terrain.raw', fmCreate);
  try
    for x := 0 to 1023 do
      for y := 0 to 1023 do
      begin
        lRed := lRasterImage.RasterImage.Colors[x, y].Red;
        lFile.WriteByte(Byte(lRed div $FF));
      end;
  finally
    lFile.Free;
  end;

  glControl.Invalidate;
end;

procedure TformFPV3D.buttonCutFileClick(Sender: TObject);
var
  lPage: TvVectorialPage;
begin
  VecDoc.ReadFromFile(editFileName.FileName);
  //lPage := VecDoc.GetPage(0);
  //while lPage.DeleteEntity(20000) do ;
  VecDoc.WriteToFile(editFileName.FileName + 'smaller.las');
end;

procedure TformFPV3D.FormDestroy(Sender: TObject);
begin
  VecDoc.Free;
end;

end.

