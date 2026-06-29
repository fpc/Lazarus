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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

written 2001 by Satan
modernized to OpenGL 3.2 core profile (shaders, VBOs, VAOs) so it runs on
backends that only provide a core context, e.g. LCL-GTK3.

}
unit ExampleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, LCLProc, Forms, LResources, Buttons,
  StdCtrls, Dialogs, Graphics, IntfGraphics, GL, GLext, FPimage, OpenGLContext;

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
    procedure LoadTextures;
  private
    AreaInitialized: boolean;
    FrameCount: integer;
    LastFrameTicks: integer;
    procedure InitGLResources;
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
    procedure Start;
  public
    constructor Create;
    destructor Destroy; override;
  private
    procedure RespawnParticle(i: integer);
  end;

var AnExampleForm: TExampleForm;
    rx, ry, rz, rrx, rry, rrz: single;
    blended, lighted, ParticleBlended, MoveCube, MoveBackground: boolean;
    textures       : array [0..2] of GLuint;    // Storage For 3 Textures
    MyglTextures   : array [0..2] of TglTexture;
    ParticleEngine: TParticleEngine;

var direction: boolean;
    timer: single;
    LastMsecs: integer;

function LoadFileToMemStream(const Filename: string): TMemoryStream;
function LoadglTexImage2DFromPNG(PNGFilename:string;
  Image: TglTexture): boolean;

implementation

const
  ParticleCount = 2001;

// --------------------------------------------------------------------------
//  Minimal 4x4 matrix math (column-major, matching OpenGL memory layout)
// --------------------------------------------------------------------------
type
  TMat4 = array[0..15] of GLfloat;

function Mat4Identity: TMat4;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result[0]:=1; Result[5]:=1; Result[10]:=1; Result[15]:=1;
end;

// Returns A*B (so the combined transform applied to a vector v is A*B*v).
function Mat4Mul(const A, B: TMat4): TMat4;
var
  c, r, k: integer;
  s: GLfloat;
begin
  for c:=0 to 3 do
    for r:=0 to 3 do begin
      s:=0;
      for k:=0 to 3 do
        s:=s + A[k*4+r]*B[c*4+k];
      Result[c*4+r]:=s;
    end;
end;

function Mat4Translate(x, y, z: GLfloat): TMat4;
begin
  Result:=Mat4Identity;
  Result[12]:=x; Result[13]:=y; Result[14]:=z;
end;

// Equivalent of glRotatef(AngleDeg, ax,ay,az).
function Mat4RotateDeg(AngleDeg, ax, ay, az: GLfloat): TMat4;
var
  len, c, s, t, a: GLfloat;
begin
  Result:=Mat4Identity;
  len:=Sqrt(ax*ax+ay*ay+az*az);
  if len=0 then exit;
  ax:=ax/len; ay:=ay/len; az:=az/len;
  a:=AngleDeg*Pi/180;
  c:=Cos(a); s:=Sin(a); t:=1-c;
  Result[0]:=t*ax*ax+c;    Result[1]:=t*ax*ay+s*az; Result[2]:=t*ax*az-s*ay;
  Result[4]:=t*ax*ay-s*az; Result[5]:=t*ay*ay+c;    Result[6]:=t*ay*az+s*ax;
  Result[8]:=t*ax*az+s*ay; Result[9]:=t*ay*az-s*ax; Result[10]:=t*az*az+c;
end;

// Equivalent of glFrustum(l,r,b,t,n,f).
function Mat4Frustum(l, r, b, t, n, f: GLfloat): TMat4;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result[0]:=2*n/(r-l);
  Result[5]:=2*n/(t-b);
  Result[8]:=(r+l)/(r-l);
  Result[9]:=(t+b)/(t-b);
  Result[10]:=-(f+n)/(f-n);
  Result[11]:=-1;
  Result[14]:=-2*f*n/(f-n);
end;

// --------------------------------------------------------------------------
//  GLSL shaders (GLSL 1.50 == OpenGL 3.2 core). The three colored point
//  lights and the ambient term replace the old fixed-function lighting.
// --------------------------------------------------------------------------
const
  VertexShaderSrc =
    '#version 150'#10+
    'in vec3 in_position;'#10+
    'in vec3 in_normal;'#10+
    'in vec2 in_texcoord;'#10+
    'uniform mat4 u_mvp;'#10+
    'uniform mat4 u_modelview;'#10+
    'out vec3 v_normal;'#10+
    'out vec3 v_eyepos;'#10+
    'out vec2 v_texcoord;'#10+
    'void main() {'#10+
    '  vec4 eye = u_modelview * vec4(in_position, 1.0);'#10+
    '  v_eyepos = eye.xyz;'#10+
    '  v_normal = mat3(u_modelview) * in_normal;'#10+
    '  v_texcoord = in_texcoord;'#10+
    '  gl_Position = u_mvp * vec4(in_position, 1.0);'#10+
    '}'#10;

  FragmentShaderSrc =
    '#version 150'#10+
    'in vec3 v_normal;'#10+
    'in vec3 v_eyepos;'#10+
    'in vec2 v_texcoord;'#10+
    'uniform sampler2D u_tex;'#10+
    'uniform bool u_lighting;'#10+
    'uniform vec4 u_color;'#10+
    'out vec4 fragColor;'#10+
    'const vec3 ambient = vec3(0.5);'#10+
    'const vec3 lpos0 = vec3( 0.0, 0.0, 3.0);'#10+
    'const vec3 lpos1 = vec3( 3.0, 0.0, 3.0);'#10+
    'const vec3 lpos2 = vec3(-3.0, 0.0, 0.0);'#10+
    'const vec3 lcol0 = vec3(0.8, 0.0, 0.0);'#10+
    'const vec3 lcol1 = vec3(0.0, 0.8, 0.0);'#10+
    'const vec3 lcol2 = vec3(0.0, 0.0, 0.8);'#10+
    'void main() {'#10+
    '  vec4 tex = texture(u_tex, v_texcoord);'#10+
    '  vec3 base = tex.rgb * u_color.rgb;'#10+
    '  vec3 rgb = base;'#10+
    '  if (u_lighting) {'#10+
    '    vec3 N = normalize(v_normal);'#10+
    '    vec3 lit = ambient;'#10+
    '    lit += lcol0 * max(dot(N, normalize(lpos0 - v_eyepos)), 0.0);'#10+
    '    lit += lcol1 * max(dot(N, normalize(lpos1 - v_eyepos)), 0.0);'#10+
    '    lit += lcol2 * max(dot(N, normalize(lpos2 - v_eyepos)), 0.0);'#10+
    '    rgb = base * lit;'#10+
    '  }'#10+
    '  fragColor = vec4(rgb, tex.a * u_color.a);'#10+
    '}'#10;

  GL_CLAMP_TO_EDGE = $812F;

var
  // modern GL resources, created on the first paint
  GLLoaded: boolean = false;
  GLProg: GLuint = 0;
  uMVP, uMV, uTex, uLit, uCol: GLint;
  glVAO: GLuint = 0;
  vboBack, vboSides, vboCaps, vboPart: GLuint;
  nBack, nSides, nCaps, nPart: GLsizei;
  ProjMatrix: TMat4;

// --------------------------------------------------------------------------
//  Shader/program helpers
// --------------------------------------------------------------------------
function CompileShader(AType: GLenum; const Src: string): GLuint;
var
  status, loglen: GLint;
  p: PGLchar;
  logbuf: AnsiString;
begin
  Result := glCreateShader(AType);
  p := PGLchar(Src);
  // FPC's glext declares the source param as PGLchar, so pass the address of
  // the pointer (count=1, length=nil -> null terminated).
  glShaderSource(Result, 1, PGLchar(@p), nil);
  glCompileShader(Result);
  glGetShaderiv(Result, GL_COMPILE_STATUS, @status);
  if status = 0 then begin
    logbuf:='';
    glGetShaderiv(Result, GL_INFO_LOG_LENGTH, @loglen);
    SetLength(logbuf, loglen);
    if loglen > 0 then
      glGetShaderInfoLog(Result, loglen, nil, @logbuf[1]);
    DebugLn(['OpenGL shader compile error: ', logbuf]);
  end;
end;

function BuildProgram: GLuint;
var
  vs, fs: GLuint;
  status, loglen: GLint;
  logbuf: AnsiString;
begin
  vs := CompileShader(GL_VERTEX_SHADER, VertexShaderSrc);
  fs := CompileShader(GL_FRAGMENT_SHADER, FragmentShaderSrc);
  Result := glCreateProgram();
  glAttachShader(Result, vs);
  glAttachShader(Result, fs);
  glBindAttribLocation(Result, 0, 'in_position');
  glBindAttribLocation(Result, 1, 'in_normal');
  glBindAttribLocation(Result, 2, 'in_texcoord');
  glLinkProgram(Result);
  glGetProgramiv(Result, GL_LINK_STATUS, @status);
  if status = 0 then begin
    logbuf:='';
    glGetProgramiv(Result, GL_INFO_LOG_LENGTH, @loglen);
    SetLength(logbuf, loglen);
    if loglen > 0 then
      glGetProgramInfoLog(Result, loglen, nil, @logbuf[1]);
    DebugLn(['OpenGL program link error: ', logbuf]);
  end;
  glDeleteShader(vs);
  glDeleteShader(fs);
end;

// --------------------------------------------------------------------------
//  Mesh builder: interleaved position(3) + normal(3) + texcoord(2)
// --------------------------------------------------------------------------
type
  TMeshBuilder = record
    Data: array of GLfloat;
    Count: integer; // floats used
  end;

procedure MB_Init(out MB: TMeshBuilder);
begin
  SetLength(MB.Data, 0);
  MB.Count := 0;
end;

procedure MB_Vert(var MB: TMeshBuilder; px,py,pz, nx,ny,nz, u,v: GLfloat);
begin
  if MB.Count+8 > Length(MB.Data) then
    SetLength(MB.Data, (MB.Count+8)*2 + 64);
  MB.Data[MB.Count+0]:=px; MB.Data[MB.Count+1]:=py; MB.Data[MB.Count+2]:=pz;
  MB.Data[MB.Count+3]:=nx; MB.Data[MB.Count+4]:=ny; MB.Data[MB.Count+5]:=nz;
  MB.Data[MB.Count+6]:=u;  MB.Data[MB.Count+7]:=v;
  Inc(MB.Count, 8);
end;

// Append a quad (corners a,b,c,d in polygon order) as two triangles.
procedure MB_Quad(var MB: TMeshBuilder;
  ax,ay,az, bx,by,bz, cx,cy,cz, dx,dy,dz, nx,ny,nz,
  au,av, bu,bv, cu,cv, du,dv: GLfloat);
begin
  MB_Vert(MB, ax,ay,az, nx,ny,nz, au,av);
  MB_Vert(MB, bx,by,bz, nx,ny,nz, bu,bv);
  MB_Vert(MB, cx,cy,cz, nx,ny,nz, cu,cv);
  MB_Vert(MB, ax,ay,az, nx,ny,nz, au,av);
  MB_Vert(MB, cx,cy,cz, nx,ny,nz, cu,cv);
  MB_Vert(MB, dx,dy,dz, nx,ny,nz, du,dv);
end;

function UploadMesh(const MB: TMeshBuilder; out vbo: GLuint): GLsizei;
begin
  glGenBuffers(1, @vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, MB.Count*SizeOf(GLfloat), @MB.Data[0], GL_STATIC_DRAW);
  Result := MB.Count div 8;
end;

procedure BuildBackground(out MB: TMeshBuilder);
const s = 2.5;
begin
  MB_Init(MB);
  MB_Quad(MB,  s, s, s,  -s, s, s,  -s,-s, s,   s,-s, s,   0, 0, 1,  1,1, 0,1, 0,0, 1,0); // front
  MB_Quad(MB,  s, s,-s,   s,-s,-s,  -s,-s,-s,  -s, s,-s,   0, 0,-1,  0,1, 0,0, 1,0, 1,1); // back
  MB_Quad(MB, -s, s, s,  -s, s,-s,  -s,-s,-s,  -s,-s, s,  -1, 0, 0,  1,1, 0,1, 0,0, 1,0); // left
  MB_Quad(MB,  s, s,-s,   s, s, s,   s,-s, s,   s,-s,-s,   1, 0, 0,  1,1, 0,1, 0,0, 1,0); // right
  MB_Quad(MB,  s, s,-s,  -s, s,-s,  -s, s, s,   s, s, s,   0, 1, 0,  1,1, 0,1, 0,0, 1,0); // top
  MB_Quad(MB, -s,-s,-s,   s,-s,-s,   s,-s, s,  -s,-s, s,   0,-1, 0,  1,1, 0,1, 0,0, 1,0); // bottom
end;

procedure BuildCubeSides(out MB: TMeshBuilder);
const s = 0.5;
begin
  MB_Init(MB);
  MB_Quad(MB,  s, s, s,  -s, s, s,  -s,-s, s,   s,-s, s,   0, 0, 1,  1,1, 0,1, 0,0, 1,0); // front
  MB_Quad(MB,  s, s,-s,   s,-s,-s,  -s,-s,-s,  -s, s,-s,   0, 0,-1,  0,1, 0,0, 1,0, 1,1); // back
  MB_Quad(MB, -s, s, s,  -s, s,-s,  -s,-s,-s,  -s,-s, s,  -1, 0, 0,  1,1, 0,1, 0,0, 1,0); // left
  MB_Quad(MB,  s, s,-s,   s, s, s,   s,-s, s,   s,-s,-s,   1, 0, 0,  1,1, 0,1, 0,0, 1,0); // right
end;

procedure BuildCubeCaps(out MB: TMeshBuilder);
const s = 0.5;
begin
  MB_Init(MB);
  MB_Quad(MB,  s, s,-s,  -s, s,-s,  -s, s, s,   s, s, s,   0, 1, 0,  1,1, 0,1, 0,0, 1,0); // top
  MB_Quad(MB, -s,-s,-s,   s,-s,-s,   s,-s, s,  -s,-s, s,   0,-1, 0,  1,1, 0,1, 0,0, 1,0); // bottom
end;

procedure BuildParticle(out MB: TMeshBuilder);
const s = 0.025;
begin
  MB_Init(MB);
  MB_Quad(MB,  s, s, 0,  -s, s, 0,  -s,-s, 0,   s,-s, 0,   0, 0, 1,  1,1, 0,1, 0,0, 1,0);
end;

procedure BindMesh(vbo: GLuint);
const
  stride = 8*SizeOf(GLfloat);
begin
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, stride, Pointer(0));
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, stride, Pointer(3*SizeOf(GLfloat)));
  glEnableVertexAttribArray(2);
  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, stride, Pointer(6*SizeOf(GLfloat)));
end;

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

function LoadglTexImage2DFromPNG(PNGFilename: string; Image: TglTexture
  ): boolean;
var
  png: TPortableNetworkGraphic;
  IntfImg: TLazIntfImage;
  y: Integer;
  x: Integer;
  c: TFPColor;
  p: PByte;
begin
  Result:=false;
  png:=TPortableNetworkGraphic.Create;
  IntfImg:=nil;
  try
    png.LoadFromFile(PNGFilename);
    IntfImg:=png.CreateIntfImage;
    Image.Width:=IntfImg.Width;
    Image.Height:=IntfImg.Height;
    GetMem(Image.Data,Image.Width*Image.Height * 3);
    p:=PByte(Image.Data);
    for y:=0 to IntfImg.Height-1 do begin
      for x:=0 to IntfImg.Width-1 do begin
        c:=IntfImg.Colors[x,y];
        p^:=c.red shr 8;
        inc(p);
        p^:=c.green shr 8;
        inc(p);
        p^:=c.blue shr 8;
        inc(p);
      end;
    end;
  finally
    png.Free;
    IntfImg.Free;
  end;
  Result:=true;
end;

{ TExampleForm }

constructor TExampleForm.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);
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
      OnClick:=@MoveCubeButton1Click;
    end;

    MoveBackgroundButton1:=TButton.Create(Self);
    with MoveBackgroundButton1 do begin
      Name:='MoveBackgroundButton1';
      Parent:=Self;
      SetBounds(320,10,80,25);
      Caption:='Move Back';
      OnClick:=@MoveBackgroundButton1Click;
    end;

    RotateZButton1:=TButton.Create(Self);
    with RotateZButton1 do begin
      Name:='RotateZButton1';
      Parent:=Self;
      SetBounds(320,10,80,25);
      Caption:='P. Respawn';
      OnClick:=@RotateZButton1Click;
    end;

    RotateZButton2:=TButton.Create(Self);
    with RotateZButton2 do begin
      Name:='RotateZButton2';
      Parent:=Self;
      SetBounds(320,10,80,25);
      Caption:='P. Blending';
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

  LoadTextures;

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

procedure TExampleForm.LoadTextures;

  procedure LoadglTexture(Filename:string; Image:TglTexture);
  begin
    Filename:=ExpandFileNameUTF8(Filename);
    if not LoadglTexImage2DFromPNG(Filename,Image) then begin
      MessageDlg('File not found',
        'Image file not found: '+Filename,
        mtError,[mbOk],0);
      raise Exception.Create('Image file not found: '+Filename);
    end;
  end;

var
  i: Integer;
begin
  for i:=0 to 2 do begin
    Textures[i]:=0;
    MyglTextures[i]:=TglTexture.Create;
  end;
  {loading the texture and setting its parameters}

  LoadglTexture('data/particle.png',MyglTextures[0]);
  LoadglTexture('data/texture2.png',MyglTextures[1]);
  LoadglTexture('data/texture3.png',MyglTextures[2]);
end;

// --------------------------------------------------------------------------
//                              Particle Engine
// --------------------------------------------------------------------------

constructor TParticleEngine.Create;
var i: integer;
begin
  for i:=1 to ParticleCount do Particle[i]:=TParticle.Create;
  xspawn:=0;
end;

destructor TParticleEngine.Destroy;
var i: integer;
begin
  for i:=1 to ParticleCount do FreeAndNil(Particle[i]);
  inherited Destroy;
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
  for i:=1 to ParticleCount do begin
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
  for i:=1 to ParticleCount do begin
    RespawnParticle(i);
  end;
end;

// ---------------------------------------------------------------------------
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ---------------------------------------------------------------------------

procedure TExampleForm.IdleFunc(Sender: TObject; var Done: Boolean);
begin
  OpenGLControl1.Invalidate;
  Done:=false; // tell lcl to handle messages and return immediatly
end;

// --------------------------------------------------------------------------
//                                 Buttons
// --------------------------------------------------------------------------

procedure TExampleForm.LightingButton1Click(Sender: TObject);
begin
  lighted:=not lighted;
  OpenGLControl1.Invalidate;
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

procedure TExampleForm.InitGLResources;
var
  i: Integer;
  MB: TMeshBuilder;
begin
  if not GLLoaded then begin
    // load the OpenGL 1.5 / 2.0 / 3.0 entry points (buffers, shaders, VAOs)
    Load_GL_version_1_5;
    Load_GL_version_2_0;
    Load_GL_version_3_0;
    GLLoaded:=true;
  end;

  GLProg := BuildProgram;
  uMVP := glGetUniformLocation(GLProg, 'u_mvp');
  uMV  := glGetUniformLocation(GLProg, 'u_modelview');
  uTex := glGetUniformLocation(GLProg, 'u_tex');
  uLit := glGetUniformLocation(GLProg, 'u_lighting');
  uCol := glGetUniformLocation(GLProg, 'u_color');

  // a single VAO holds the vertex layout; attribute pointers are (re)bound
  // per mesh in BindMesh
  glGenVertexArrays(1, @glVAO);
  glBindVertexArray(glVAO);

  // upload the 3 textures
  glGenTextures(3, @textures[0]);
  for i:=0 to 2 do begin
    glBindTexture(GL_TEXTURE_2D, Textures[i]);
    glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGB,MyglTextures[i].Width,MyglTextures[i].Height,0,
        GL_RGB,GL_UNSIGNED_BYTE,MyglTextures[i].Data);
  end;

  // build the geometry once into static vertex buffers
  BuildBackground(MB); nBack  := UploadMesh(MB, vboBack);
  BuildCubeSides(MB);  nSides := UploadMesh(MB, vboSides);
  BuildCubeCaps(MB);   nCaps  := UploadMesh(MB, vboCaps);
  BuildParticle(MB);   nPart  := UploadMesh(MB, vboPart);

  glClearDepth(1.0);
  glDepthFunc(GL_LEQUAL);

  ProjMatrix := Mat4Frustum(-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
end;

procedure TExampleForm.OpenGLControl1Paint(Sender: TObject);
var
  CurTime: TDateTime;
  MSecs: integer;
  Base, MV, MVP: TMat4;
  i: integer;
begin
  inc(FrameCount);
  inc(LastFrameTicks,OpenGLControl1.FrameDiffTimeInMSecs);
  if (LastFrameTicks>=1000) then begin
    DebugLn(['TExampleForm.OpenGLControl1Paint Frames per second: ',FrameCount]);
    dec(LastFrameTicks,1000);
    FrameCount:=0;
  end;

  if not OpenGLControl1.MakeCurrent then exit;

  if not AreaInitialized then begin
    InitGLResources;
    AreaInitialized:=true;
  end;

  glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);

  CurTime:=Now;
  MSecs:=round(CurTime*86400*1000) mod 1000;
  if MSecs<0 then MSecs:=1000+MSecs;
  timer:=msecs-LastMsecs;
  if timer<0 then timer:=1000+timer;
  LastMsecs:=MSecs;

  ParticleEngine.MoveParticles;

  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);

  glUseProgram(GLProg);
  glBindVertexArray(glVAO);
  glActiveTexture(GL_TEXTURE0);
  glUniform1i(uTex, 0);
  glUniform1i(uLit, Ord(lighted));
  // matches the old fixed-function glColor4f(1,1,1,0.5)
  glUniform4f(uCol, 1.0, 1.0, 1.0, 0.5);

  Base := Mat4Translate(0.0, 0.0, -3.0);

  // ------- background -------
  if MoveBackground then begin
    rrx:=rrx-0.6*(timer/10);
    rry:=rry-0.5*(timer/10);
    rrz:=rrz-0.3*(timer/10);
  end;
  MV := Mat4Mul(Base, Mat4Mul(Mat4RotateDeg(rrx,1,0,0),
        Mat4Mul(Mat4RotateDeg(rry,0,1,0), Mat4RotateDeg(rrz,0,0,1))));
  MVP := Mat4Mul(ProjMatrix, MV);
  glUniformMatrix4fv(uMV,  1, GL_FALSE, @MV[0]);
  glUniformMatrix4fv(uMVP, 1, GL_FALSE, @MVP[0]);

  if blended then begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDisable(GL_DEPTH_TEST);
  end;
  glBindTexture(GL_TEXTURE_2D, textures[2]);
  BindMesh(vboBack);
  glDrawArrays(GL_TRIANGLES, 0, nBack);

  // ------- cube -------
  if MoveCube then begin
    rx:=rx+0.5*(timer/10);
    ry:=ry+0.25*(timer/10);
    rz:=rz+0.8*(timer/10);
  end;
  MV := Mat4Mul(Base, Mat4Mul(Mat4RotateDeg(rx,1,0,0),
        Mat4Mul(Mat4RotateDeg(ry,0,1,0), Mat4RotateDeg(rz,0,0,1))));
  MVP := Mat4Mul(ProjMatrix, MV);
  glUniformMatrix4fv(uMV,  1, GL_FALSE, @MV[0]);
  glUniformMatrix4fv(uMVP, 1, GL_FALSE, @MVP[0]);

  glBindTexture(GL_TEXTURE_2D, textures[1]);
  BindMesh(vboSides);
  glDrawArrays(GL_TRIANGLES, 0, nSides);
  glBindTexture(GL_TEXTURE_2D, textures[2]);
  BindMesh(vboCaps);
  glDrawArrays(GL_TRIANGLES, 0, nCaps);

  if blended then begin
    glDisable(GL_BLEND);
    glEnable(GL_DEPTH_TEST);
  end;

  // ------- particles -------
  if ParticleBlended then begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  end;
  glBindTexture(GL_TEXTURE_2D, textures[0]);
  BindMesh(vboPart);
  for i:=1 to ParticleCount do begin
    MV := Mat4Mul(Base, Mat4Translate(ParticleEngine.Particle[i].x,
            ParticleEngine.Particle[i].y, ParticleEngine.Particle[i].z));
    MVP := Mat4Mul(ProjMatrix, MV);
    glUniformMatrix4fv(uMV,  1, GL_FALSE, @MV[0]);
    glUniformMatrix4fv(uMVP, 1, GL_FALSE, @MVP[0]);
    glDrawArrays(GL_TRIANGLES, 0, nPart);
  end;
  if ParticleBlended then glDisable(GL_BLEND);

  glBindVertexArray(0);
  glUseProgram(0);

  // Swap backbuffer to front
  OpenGLControl1.SwapBuffers;
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
