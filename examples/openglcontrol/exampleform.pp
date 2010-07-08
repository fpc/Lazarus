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

written 2001 by Satan

}
unit ExampleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LCLProc, Forms, LResources, Buttons,
  StdCtrls, Dialogs, GL, OpenGLContext;

const
  GL_CLAMP_TO_EDGE = $812F;

type
  TglTexture = class
  public
    Width,Height: longint;
    Data        : pointer;
    destructor Destroy; override;
  end;
  
type

  { TExampleForm }

  TExampleForm = class(TForm)
    OpenGLControl1: TOpenGLControl;
    ExitButton1: TButton;
    LightingButton1: TButton;
    BlendButton1: TButton;
    MoveCubeButton1: TButton;
    MoveBackgroundButton1: TButton;
    RotateZButton1: TButton;
    RotateZButton2: TButton;
    HintLabel1: TLabel;
    procedure IdleFunc(Sender: TObject; var Done: Boolean);
    procedure FormResize(Sender: TObject);
    procedure ExitButton1Click(Sender: TObject);
    procedure LightingButton1Click(Sender: TObject);
    procedure BlendButton1Click(Sender: TObject);
    procedure MoveCubeButton1Click(Sender: TObject);
    procedure MoveBackgroundButton1Click(Sender: TObject);
    procedure RotateZButton1Click(Sender: TObject);
    procedure RotateZButton2Click(Sender: TObject);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  private
    AreaInitialized: boolean;
    FrameCount: integer;
    LastFrameTicks: integer;
  end;

  TParticle = class
    x, y, z: GLfloat;
    vx, vy, vz: GLfloat;
    life: single;
  end;

  TParticleEngine = class
    xspawn: GLfloat;
    Particle: array [1..2001] of TParticle;
    procedure MoveParticles;
    procedure DrawParticles;
    procedure Start;
  public
    constructor Create;
    destructor Destroy; override;
  private
    procedure RespawnParticle(i: integer);
  end;
  
var AnExampleForm: TExampleForm;
    front, left1: GLuint;
    rx, ry, rz, rrx, rry, rrz: single;
    LightAmbient : array [0..3] of GLfloat;
    checked, blended, lighted, ParticleBlended, MoveCube, MoveBackground: boolean;
    textures       : array [0..2] of GLuint;    // Storage For 3 Textures
    MyglTextures   : array [0..2] of TglTexture;
    lightamb, lightdif, lightpos, light2pos, light2dif,
    light3pos, light3dif, light4pos, light4dif, fogcolor: array [0..3] of GLfloat;
    ParticleEngine: TParticleEngine;
    ParticleList, CubeList, BackList: GLuint;

var direction: boolean;
    timer: single;
    LastMsecs: integer;

implementation


function LoadFileToMemStream(const Filename: string): TMemoryStream;
var FileStream: TFileStream;
begin
  Result:=TMemoryStream.Create;
  try
    FileStream:=TFileStream.Create(UTF8ToSys(Filename), fmOpenRead);
    try
      Result.CopyFrom(FileStream,FileStream.Size);
      Result.Position:=0;
    finally
      FileStream.Free;
    end;
  except
    Result.Free;
    Result:=nil;
  end;
end;

function LoadglTexImage2DFromBitmapFile(Filename:string;
  var Image:TglTexture): boolean;
type
  TBITMAPFILEHEADER = packed record
    bfType: Word;
    bfSize: DWORD;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: DWORD;
  end;

  BITMAPINFOHEADER = packed record
          biSize : DWORD;
          biWidth : Longint;
          biHeight : Longint;
          biPlanes : WORD;
          biBitCount : WORD;
          biCompression : DWORD;
          biSizeImage : DWORD;
          biXPelsPerMeter : Longint;
          biYPelsPerMeter : Longint;
          biClrUsed : DWORD;
          biClrImportant : DWORD;
       end;

  RGBQUAD = packed record
          rgbBlue : BYTE;
          rgbGreen : BYTE;
          rgbRed : BYTE;
       //   rgbReserved : BYTE;
       end;

  BITMAPINFO = packed record
          bmiHeader : BITMAPINFOHEADER;
          bmiColors : array[0..0] of RGBQUAD;
       end;

  PBITMAPINFO = ^BITMAPINFO;

  TRawImage = packed record
     p:array[0..0] of byte;
   end;
  PRawImage = ^TRawImage;

const
  BI_RGB = 0;

var
  MemStream: TMemoryStream;
  BmpHead: TBitmapFileHeader;
  BmpInfo:PBitmapInfo;
  ImgSize:longint;
  InfoSize, PixelCount, i:integer;
  BitsPerPixel:integer;
  AnRGBQuad: RGBQUAD;
begin
  Result:=false;
  MemStream:=LoadFileToMemStream(Filename);
  if MemStream=nil then exit;
  try
    if (MemStream.Read(BmpHead, sizeof(BmpHead))<sizeof(BmpHead))
    or (BmpHead.bfType <> $4D42) then begin
      writeln('Invalid windows bitmap (header)');
      exit;
    end;
    InfoSize:=BmpHead.bfOffBits-SizeOf(BmpHead);
    GetMem(BmpInfo,InfoSize);
    try
      if MemStream.Read(BmpInfo^,InfoSize)<>InfoSize then begin
        writeln('Invalid windows bitmap (info)');
        exit;
      end;
      if BmpInfo^.bmiHeader.biSize<>sizeof(BitmapInfoHeader) then begin
        writeln('OS2 bitmaps are not supported yet');
        exit;
      end;
      if BmpInfo^.bmiHeader.biCompression<>bi_RGB then begin
        writeln('RLE compression is not supported yet');
        exit;
      end;
      BitsPerPixel:=BmpInfo^.bmiHeader.biBitCount;
      if BitsPerPixel<>24 then begin
        writeln('Only truecolor bitmaps supported yet');
        exit;
      end;
      ImgSize:=BmpInfo^.bmiHeader.biSizeImage;
      if MemStream.Size-MemStream.Position<ImgSize then begin
        writeln('Invalid windows bitmap (bits)');
        exit;
      end;
      Image.Width:=BmpInfo^.bmiHeader.biWidth;
      Image.Height:=BmpInfo^.bmiHeader.biHeight;
      PixelCount:=Image.Width*Image.Height;
      GetMem(Image.Data,PixelCount * 3);
      try
        for i:=0 to PixelCount-1 do begin
          MemStream.Read(AnRGBQuad,sizeOf(RGBQuad));
          with PRawImage(Image.Data)^ do begin
            p[i*3+0]:=AnRGBQuad.rgbRed;
            p[i*3+1]:=AnRGBQuad.rgbGreen;
            p[i*3+2]:=AnRGBQuad.rgbBlue;
          end;
        end;
      except
        writeln('Error converting bitmap');
        FreeMem(Image.Data);
        Image.Data:=nil;
        exit;
      end;
    finally
      FreeMem(BmpInfo);
    end;
    Result:=true;
  finally
    MemStream.Free;
  end;
  Result:=true;
end;



constructor TExampleForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-800) div 2,(Screen.Height-600) div 2,800,600);
    Caption:='LCL example for the TOpenGLControl';
    
    Application.OnIdle:=@IdleFunc;
    OnResize:=@FormResize;
    blended:=false;
    lighted:=false;
    ParticleEngine:=TParticleEngine.Create;
    
    ExitButton1:=TButton.Create(Self);
    with ExitButton1 do begin
      Name:='ExitButton1';
      Parent:=Self;
      SetBounds(320,10,80,25);
      Caption:='Exit';
      OnClick:=@ExitButton1Click;
    end;

    LightingButton1:=TButton.Create(Self);
    with LightingButton1 do begin
      Name:='LightingButton1';
      Parent:=Self;
      SetBounds(220,0,80,25);
      Caption:='Lighting';
      OnClick:=@LightingButton1Click;
    end;

    BlendButton1:=TButton.Create(Self);
    with BlendButton1 do begin
      Name:='BlendButton1';
      Parent:=Self;
      SetBounds(220,0,80,25);
      Caption:='Blending';
      OnClick:=@BlendButton1Click;
    end;

    MoveCubeButton1:=TButton.Create(Self);
    with MoveCubeButton1 do begin
      Name:='MoveCubeButton1';
      Parent:=Self;
      SetBounds(320,10,80,25);
      Caption:='Move Cube';
      Checked:=false;
      OnClick:=@MoveCubeButton1Click;
    end;

    MoveBackgroundButton1:=TButton.Create(Self);
    with MoveBackgroundButton1 do begin
      Name:='MoveBackgroundButton1';
      Parent:=Self;
      SetBounds(320,10,80,25);
      Caption:='Move Back';
      Checked:=false;
      OnClick:=@MoveBackgroundButton1Click;
    end;
    
    RotateZButton1:=TButton.Create(Self);
    with RotateZButton1 do begin
      Name:='RotateZButton1';
      Parent:=Self;
      SetBounds(320,10,80,25);
      Caption:='P. Respawn';
      Checked:=false;
      OnClick:=@RotateZButton1Click;
    end;

    RotateZButton2:=TButton.Create(Self);
    with RotateZButton2 do begin
      Name:='RotateZButton2';
      Parent:=Self;
      SetBounds(320,10,80,25);
      Caption:='P. Blending';
      Checked:=false;
      OnClick:=@RotateZButton2Click;
    end;

    HintLabel1:=TLabel.Create(Self);
    with HintLabel1 do begin
      Name:='HintLabel1';
      Parent:=Self;
      SetBounds(0,0,280,50);
      Caption:='Demo';
    end;
    
    // resize the components first, because the opengl context needs some time to setup
    FormResize(Self);

    OpenGLControl1:=TOpenGLControl.Create(Self);
    with OpenGLControl1 do begin
      Name:='OpenGLControl1';
      Parent:=Self;
      SetBounds(10,90,380,200);
      OnPaint:=@OpenGLControl1Paint;
      OnResize:=@OpenGLControl1Resize;
    end;

  end;
  // now resize
  FormResize(Self);
end;

destructor TExampleForm.Destroy;
var i: integer;
begin
  for i:=0 to 2 do begin
    Textures[i]:=0;
    FreeAndNil(MyglTextures[i]);
  end;
  FreeAndNil(ParticleEngine);

  inherited Destroy;
end;

// --------------------------------------------------------------------------
//                              Particle Engine
// --------------------------------------------------------------------------

constructor TParticleEngine.Create;
var i: integer; 
begin
  for i:=1 to 2001 do Particle[i]:=TParticle.Create;
  xspawn:=0;
end;

destructor TParticleEngine.Destroy;
var i: integer;
begin
  for i:=1 to 2001 do FreeAndNil(Particle[i]);
  inherited Destroy;
end;

procedure TParticleEngine.DrawParticles;
var i: integer;
begin
  for i:=1 to 2001 do begin
    glPushMatrix;
    glTranslatef(Particle[i].x, Particle[i].y, Particle[i].z);
    glCallList(ParticleList);
    glPopMatrix;
  end;
end;

procedure TParticleEngine.RespawnParticle(i: integer);
begin
  if (xspawn>2) and (direction=true) then direction:=false;
  if (xspawn<-2) and (direction=false) then direction:=true;
  if direction then
    xspawn:=xspawn+0.0002*(timer/10)
  else
    xspawn:=xspawn-0.0002*(timer/10);
  Particle[i].x:=xspawn;
  Particle[i].y:=-0.5;
  Particle[i].z:=0;
  Particle[i].vx:=-0.005+GLFloat(random(2000))/200000;
  Particle[i].vy:=0.035+GLFloat(random(750))/100000;
  Particle[i].vz:=-0.005+GLFloat(random(2000))/200000;
  Particle[i].life:=GLFloat(random(1250))/1000+1;
end;

procedure TParticleEngine.MoveParticles;
var i: integer;
begin
  for i:=1 to 2001 do begin
    if Particle[i].life>0 then begin
      Particle[i].life:=Particle[i].life-0.01*(timer/10);
      Particle[i].x:=Particle[i].x+Particle[i].vx*(timer/10);
      
      Particle[i].vy:=Particle[i].vy-0.00035*(timer/10); // gravity
      Particle[i].y:=Particle[i].y+Particle[i].vy*(timer/10);
      
      Particle[i].z:=Particle[i].z+Particle[i].vz*(timer/10);
    end else begin
      RespawnParticle(i);
    end;
  end;  
end;

procedure TParticleEngine.Start;
var i: integer;
begin
  for i:=1 to 2001 do begin
    RespawnParticle(i);
  end;
end;

// ---------------------------------------------------------------------------
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ---------------------------------------------------------------------------

procedure TExampleForm.IdleFunc(Sender: TObject; var Done: Boolean);
begin
  OpenGLControl1.Invalidate;
  //OpenGLControl1Paint(Self);
  Done:=false; // tell lcl to handle messages and return immediatly
end;

// --------------------------------------------------------------------------
//                                 Buttons
// --------------------------------------------------------------------------

procedure TExampleForm.LightingButton1Click(Sender: TObject);
begin
  if lighted then glDisable(GL_LIGHTING) else glEnable(GL_LIGHTING);
  lighted:=not lighted;
  OpenGLControl1.Invalidate;// not need
end;

procedure TExampleForm.BlendButton1Click(Sender: TObject);
begin
  blended:=not blended;
  OpenGLControl1.Invalidate;
end;

procedure TExampleForm.MoveCubeButton1Click(Sender: TObject);
begin
  MoveCube:=not MoveCube;
  OpenGLControl1.Invalidate;
end;

procedure TExampleForm.MoveBackgroundButton1Click(Sender: TObject);
begin
  MoveBackground:=not MoveBackground;
  OpenGLControl1.Invalidate;
end;

procedure TExampleForm.RotateZButton1Click(Sender: TObject);
begin
  ParticleEngine.Start;
  OpenGLControl1.Invalidate;
end;

procedure TExampleForm.RotateZButton2Click(Sender: TObject);
begin
  ParticleBlended:=not ParticleBlended;
  OpenGLControl1.Invalidate;
end;

// ---------------------------------------------------------------------------
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ---------------------------------------------------------------------------

procedure TExampleForm.FormResize(Sender: TObject);
begin
  if OpenGLControl1<>nil then
    OpenGLControl1.SetBounds(10, 30, Width-120, Height-40);
  ExitButton1.SetBounds(Width-90, 5, 80, 25);
  LightingButton1.SetBounds(Width-90, 180, 80, 25);
  BlendButton1.SetBounds(Width-90, 210, 80, 25);
  MoveCubeButton1.SetBounds(Width-90, 50, 80, 25);
  MoveBackgroundButton1.SetBounds(Width-90, 80, 80, 25);
  RotateZButton1.SetBounds(Width-90, 115, 80, 25);
  RotateZButton2.SetBounds(Width-90, 145, 80, 25);
  HintLabel1.SetBounds(10, 0, 80, 25);
end;

procedure TExampleForm.ExitButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TExampleForm.OpenGLControl1Paint(Sender: TObject);

  procedure myInit;
  begin
    {init lighting variables}
    {ambient color}
    lightamb[0]:=0.5;
    lightamb[1]:=0.5;
    lightamb[2]:=0.5;
    lightamb[3]:=1.0;
    {diffuse color}
    lightdif[0]:=0.8;
    lightdif[1]:=0.0;
    lightdif[2]:=0.0;
    lightdif[3]:=1.0;
    {diffuse position}
    lightpos[0]:=0.0;
    lightpos[1]:=0.0;
    lightpos[2]:=3.0;
    lightpos[3]:=1.0;
    {diffuse 2 color}
    light2dif[0]:=0.0;
    light2dif[1]:=0.8;
    light2dif[2]:=0.0;
    light2dif[3]:=1.0;
    {diffuse 2 position}
    light2pos[0]:=3.0;
    light2pos[1]:=0.0;
    light2pos[2]:=3.0;
    light2pos[3]:=1.0;
    {diffuse 3 color}
    light3dif[0]:=0.0;
    light3dif[1]:=0.0;
    light3dif[2]:=0.8;
    light3dif[3]:=1.0;
    {diffuse 3 position}
    light3pos[0]:=-3.0;
    light3pos[1]:=0.0;
    light3pos[2]:=0.0;
    light3pos[3]:=1.0;
    {fog color}
    
    fogcolor[0]:=0.5;
    fogcolor[1]:=0.5;
    fogcolor[2]:=0.5;
    fogcolor[3]:=1.0;
    
  end;

const GLInitialized: boolean = false;

procedure InitGL;

  procedure LoadglTexture(const Filename:string; var Image:TglTexture);
  begin
    if not LoadglTexImage2DFromBitmapFile(Filename,Image) then begin
      MessageDlg('File not found',
        'Image file not found: '+ExpandFileNameUTF8(Filename),
        mtError,[mbOk],0);
      raise Exception.Create('Image file not found: '+ExpandFileNameUTF8(Filename));
    end;
  end;

var i: integer;
begin
  if GLInitialized then exit;
  GLInitialized:=true;
  {setting lighting conditions}
  glLightfv(GL_LIGHT0,GL_AMBIENT,lightamb);
  glLightfv(GL_LIGHT1,GL_AMBIENT,lightamb);
  glLightfv(GL_LIGHT2,GL_DIFFUSE,lightdif);
  glLightfv(GL_LIGHT2,GL_POSITION,lightpos);
  glLightfv(GL_LIGHT3,GL_DIFFUSE,light2dif);
  glLightfv(GL_LIGHT3,GL_POSITION,light2pos);
  glLightfv(GL_LIGHT4,GL_POSITION,light3pos);
  glLightfv(GL_LIGHT4,GL_DIFFUSE,light3dif);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHT1);
  glEnable(GL_LIGHT2);
  glEnable(GL_LIGHT3);
  glEnable(GL_LIGHT4);
  {}
  for i:=0 to 2 do begin
    Textures[i]:=0;
    MyglTextures[i]:=TglTexture.Create;
  end;
  {loading the texture and setting its parameters}
  
  LoadglTexture('data/particle.bmp',MyglTextures[0]);
  LoadglTexture('data/texture2.bmp',MyglTextures[1]);
  LoadglTexture('data/texture3.bmp',MyglTextures[2]);
  
  glGenTextures(3, @textures[0]);
  for i:=0 to 2 do begin
    glBindTexture(GL_TEXTURE_2D, Textures[i]);
    glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D,0,3,MyglTextures[i].Width,MyglTextures[i].Height,0
        ,GL_RGB,GL_UNSIGNED_BYTE,MyglTextures[i].Data);
  end;
  glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  {instead of GL_MODULATE you can try GL_DECAL or GL_BLEND}
  glEnable(GL_TEXTURE_2D);          // enables 2d textures
  glClearColor(0.0,0.0,0.0,1.0);    // sets background color
  glClearDepth(1.0);
  glDepthFunc(GL_LEQUAL);           // the type of depth test to do
  glEnable(GL_DEPTH_TEST);          // enables depth testing
  glShadeModel(GL_SMOOTH);          // enables smooth color shading
  {blending}
  glColor4f(1.0,1.0,1.0,0.5);       // Full Brightness, 50% Alpha ( NEW )
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
  {}
  glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT,GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);    
  
  // creating display lists
  
  ParticleList:=glGenLists(1);
  glNewList(ParticleList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, textures[0]);
    glBegin(GL_TRIANGLE_STRIP);
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(+0.025, +0.025, 0);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-0.025, +0.025, 0);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(+0.025, -0.025, 0);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-0.025, -0.025, 0);
    glEnd;
  glEndList;
  
  BackList:=ParticleList+1;
  glNewList(BackList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, textures[2]);
    glBegin(GL_QUADS);
      {Front Face}
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 2.5, 2.5, 2.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-2.5, 2.5, 2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-2.5,-2.5, 2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 2.5,-2.5, 2.5);
      {Back Face}
      glNormal3f( 0.0, 0.0,-1.0);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 2.5, 2.5,-2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 2.5,-2.5,-2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-2.5,-2.5,-2.5);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-2.5, 2.5,-2.5);      
      {Left Face}
      glNormal3f(-1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-2.5, 2.5, 2.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-2.5, 2.5,-2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-2.5,-2.5,-2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-2.5,-2.5, 2.5);
      {Right Face}
      glNormal3f( 1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 2.5, 2.5,-2.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 2.5, 2.5, 2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 2.5,-2.5, 2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 2.5,-2.5,-2.5);      
      {Top Face}
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 2.5, 2.5,-2.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-2.5, 2.5,-2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-2.5, 2.5, 2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 2.5, 2.5, 2.5);
      {Bottom Face}
      glNormal3f( 0.0,-1.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-2.5,-2.5,-2.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 2.5,-2.5,-2.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 2.5,-2.5, 2.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-2.5,-2.5, 2.5);
 
    glEnd;
  glEndList;
  
  CubeList:=BackList+1;
  glNewList(CubeList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, textures[1]);
    glBegin(GL_QUADS);
      {Front Face}
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 0.5, 0.5, 0.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-0.5, 0.5, 0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-0.5,-0.5, 0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 0.5,-0.5, 0.5);
      {Back Face}
      glNormal3f( 0.0, 0.0,-1.0);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 0.5, 0.5,-0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 0.5,-0.5,-0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-0.5,-0.5,-0.5);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-0.5, 0.5,-0.5);      
    glEnd;
    glBindTexture(GL_TEXTURE_2D, textures[1]);
    glBegin(GL_QUADS);
      {Left Face}
      glNormal3f(-1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-0.5, 0.5, 0.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-0.5, 0.5,-0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-0.5,-0.5,-0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-0.5,-0.5, 0.5);
      {Right Face}
      glNormal3f( 1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 0.5, 0.5,-0.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 0.5, 0.5, 0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 0.5,-0.5, 0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 0.5,-0.5,-0.5);      
    glEnd;
    glBindTexture(GL_TEXTURE_2D, textures[2]);
    glBegin(GL_QUADS);
      {Top Face}
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 0.5, 0.5,-0.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-0.5, 0.5,-0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(-0.5, 0.5, 0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 0.5, 0.5, 0.5);
      {Bottom Face}
      glNormal3f( 0.0,-1.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(-0.5,-0.5,-0.5);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 0.5,-0.5,-0.5);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 0.5,-0.5, 0.5);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(-0.5,-0.5, 0.5);
    glEnd;
  glEndList;
  
end;

var
  CurTime: TDateTime;
  MSecs: integer;
begin
  inc(FrameCount);
  inc(LastFrameTicks,OpenGLControl1.FrameDiffTimeInMSecs);
  if (LastFrameTicks>=1000) then begin
    DebugLn(['TExampleForm.OpenGLControl1Paint Frames per second: ',FrameCount]);
    dec(LastFrameTicks,1000);
    FrameCount:=0;
  end;

  if OpenGLControl1.MakeCurrent then
  begin
    if not AreaInitialized then begin
      myInit;
      InitGL;
      glMatrixMode (GL_PROJECTION);    { prepare for and then }
      glLoadIdentity ();               { define the projection }
      glFrustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0); { transformation } 
      glMatrixMode (GL_MODELVIEW);  { back to modelview matrix }
      glViewport (0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
                                    { define the viewport }
      AreaInitialized:=true;
    end;

    CurTime:=Now;
    MSecs:=round(CurTime*86400*1000) mod 1000;
    if MSecs<0 then MSecs:=1000+MSecs;
    timer:=msecs-LastMsecs;
    if timer<0 then timer:=1000+timer;
    LastMsecs:=MSecs;
    
    ParticleEngine.MoveParticles;
    
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glLoadIdentity;             { clear the matrix }
    glTranslatef (0.0, 0.0,-3.0);  // -2.5); { viewing transformation }
    {rotate}

    glPushMatrix;

    if MoveBackground then begin
      rrx:=rrx-0.6*(timer/10);
      rry:=rry-0.5*(timer/10);
      rrz:=rrz-0.3*(timer/10);
    end;
    
    glRotatef(rrx,1.0,0.0,0.0);
    glRotatef(rry,0.0,1.0,0.0);
    glRotatef(rrz,0.0,0.0,1.0);

    // draw background
    if blended then begin
      glEnable(GL_BLEND);
      glDisable(GL_DEPTH_TEST);
    end;
    glCallList(BackList);
    
    glPopMatrix;
       
    glPushMatrix;

    if MoveCube then begin
      rx:=rx+0.5*(timer/10);
      ry:=ry+0.25*(timer/10);
      rz:=rz+0.8*(timer/10);
    end;
    
    glRotatef(rx,1.0,0.0,0.0);
    glRotatef(ry,0.0,1.0,0.0);
    glRotatef(rz,0.0,0.0,1.0);
    
    // draw cube
    glCallList(CubeList);
    if blended then begin
      glDisable(GL_BLEND);
      glEnable(GL_DEPTH_TEST);
    end;
    
    glPopMatrix;
    
    if ParticleBlended then glEnable(GL_BLEND);
    ParticleEngine.DrawParticles;
    if ParticleBlended then glDisable(GL_BLEND);
    //glFlush;
    //glFinish;
    // Swap backbuffer to front
    OpenGLControl1.SwapBuffers;
  end;
end;

procedure TExampleForm.OpenGLControl1Resize(Sender: TObject);
begin
  if (AreaInitialized)
  and OpenGLControl1.MakeCurrent then
    glViewport (0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
end;


{ TglTexture }

destructor TglTexture.Destroy;
begin
  if Data<>nil then FreeMem(Data);
  inherited Destroy;
end;

end.
