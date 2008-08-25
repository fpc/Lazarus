unit wmimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, HTTPDefs, websession, fpHTTP, fpWeb,fpimage;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure FileRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure PieRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure DoDrawPie(Img: TFPCustomImage;AFontName : String;  Skipped, Failed, Total: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FPWebModule1: TFPWebModule1; 

implementation

// Units needed to draw a pie. Requires freetype font rendering lib
// Make sure the font path is set correctly.

uses ftFont,fpimgcanv,fpWritePng,fpcanvas;

{ Configure this so it makes sense for your system }

Const

  // Directory to send images from.
  ImageDir = '/home/michael/lazarus/images/menu';
  // Directories with TTF files for fonts.
{$ifdef win32}
  FontDirs = 'c:\windows;c:\windows\system32';
{$else}
  FontDirs = '/usr/share/fonts/truetype';
{$endif}
  // Font to be used by default. Must exist in fontdir.
  DefFontName = 'FreeSans';

{ TFPWebModule1 }

{
  When action is pie, a pie diagram simulating a testrun of the FPC
  test suite is generated and sent to the client.
  The pie can be customized with the following request variables
  Runs     : Total number of runs (integer)
  Failed   : Number of failed runs (integer, less than runs)
  Skipped  : Number of skipped runs (integer, less than runs)
  FontName : Name of font to be used for legend. (string)
}


procedure TFPWebModule1.PieRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);

Var
  I : TFPMemoryImage;
  M : TMemoryStream;
  Skipped,Failed,Runs : Integer;
  FontName : String;


begin
  ftFont.InitEngine;
  FontMgr.SearchPath:=FontDirs;
  I:=TFPMemoryImage.Create(320,320);
  try
    // Simulate a testrun. Take values from request variables,
    // Generate default values if needed.
    Runs:=StrToIntDef(ARequest.QueryFields.Values['Runs'],100+Random(100));
    Failed:=StrToIntDef(ARequest.QueryFields.Values['Failed'],Random(Runs div 2));
    Skipped:=StrToIntDef(ARequest.QueryFields.Values['Skipped'],Random(Runs div 10));
    FontName:=ARequest.QueryFields.Values['FontName'];
    If (FontName='') then
      FontName:=DefFontName; // Default. Make sure this exists.
    DoDrawPie(I,FontName,Skipped,Failed,Runs);
    M:=TMemoryStream.Create;
    Try
      With TFPWriterPNG.Create do
        try
          UseAlpha:=True;
          ImageWrite(M,I);
        Finally
          Free;
        end;
      AResponse.ContentType:='image/png';
      M.Position:=0;
      AResponse.ContentStream:=M;
      AResponse.SendContent;
    Finally
      M.Free;
    end;
  Finally
    I.Free;
  end;
end;

{
  When action is file, a png image file is sent to the client.
  The filename is determined from the 'filename' request variable
  and the configured directory. (see above for the config constants)

}

procedure TFPWebModule1.FileRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
  
Var
  OFN, FN : String;
  F : TFileStream;
  
begin
  OFN:=ARequest.QueryFields.Values['FileName'];
  FN:=OFN;
  If (FN='') then
    Raise Exception.Create('This script requires a file name argument');
  // Strip any directory path.
  FN:=ExtractFileName(FN);
  // Make sure it is a PNG file.
  FN:=ChangeFileExt(FN,'.png');
  FN:=IncludeTrailingPathDelimiter(ImageDir)+FN;
  If Not FileExistsUTF8(FN) then
    Raise Exception.Create('The requested file "'+OFN+'" does not exist in the image directory');
  AResponse.ContentType:='image/png';
  F:=TFileStream.Create(UTF8ToSys(FN),fmOpenRead);
  Try
    AResponse.ContentStream:=F;
    AResponse.SendContent;
    Handled:=True;
  Finally
    F.Free;
  end;
end;

{
  This routine is not essential to the webmodule, but demonstrates nicely
  how to draw an image using fpimage.
}

Procedure TFPWebModule1.DoDrawPie(Img : TFPCustomImage; AFontName : String; Skipped,Failed,Total : Integer);

Var
  Cnv : TFPImageCanvas;
  W,H,FH,CR,ra : Integer;
  A1,A2,FR,SR,PR : Double;
  R : TRect;
  F : TFreeTypeFont;

  Procedure AddPie(X,Y,R : Integer; AStart,AStop : Double; Col : TFPColor);

  Var
    DX,Dy : Integer;

  begin
    DX:=Round(R*Cos(A1));
    DY:=Round(R*Sin(A1));
    Cnv.Line(X,Y,X+DX,Y-DY);
    DX:=Round(Ra*Cos(A2));
    DY:=Round(Ra*Sin(A2));
    Cnv.Line(X,Y,X+DX,Y-Dy);
    DX:=Round(R/2*Cos((A1+A2)/2));
    DY:=Round(R/2*Sin((A1+A2)/2));
    Cnv.Brush.FpColor:=Col;
    Cnv.FloodFill(X+DX,Y-DY);
  end;

  Function FractionAngle(F,T : Integer): Double;

  begin
    Result:=(2*Pi*(F/T))
  end;

begin
  F:=TFreeTypeFont.Create;
  With F do
    begin
    Name:=AFontName;
    FontIndex:=0;
    Size:=12;
    FPColor:=colred;
    AntiAliased:=False;
    Resolution:=96;
    end;
  Cnv:=TFPImageCanvas.Create(Img);
  W:=Img.Width;
  H:=Img.Height;
  cnv.Brush.Style:=bsSolid;
  cnv.Brush.FPColor:=colTransparent;
  cnv.Pen.FPColor:=colWhite;
  Cnv.Rectangle(0,0,W,H);
  Cnv.Font:=F;
  FH:=CNV.GetTextHeight('A');
  If FH=0 then
    FH:=14; // 3 * 14;
  Inc(FH,3);
  R.Top:=FH*4;
  R.Left:=0;
  R.Bottom:=H;
  CR:=H-(FH*4);
  If W>CR then
    R.Right:=CR
  else
    R.Right:=W;
  Ra:=CR div 2;
  Cnv.Pen.FPColor:=colBlack;
  cnv.brush.FPColor:=colRed;
  Cnv.Ellipse(R);
  cnv.font.FPColor:=colred;
  Inc(FH,4);
  FR:=Failed/Total;
  SR:=Skipped/Total;
  PR:=1-(FR+SR);
  Cnv.Textout(1,FH,Format('%d Failed (%3.1f%%)',[Failed,Fr*100]));
  cnv.font.FPColor:=colYellow;
  Cnv.Textout(1,FH*2,Format('%d Skipped (%3.1f%%)',[Skipped,SR*100]));
  A1:=(Pi*2*(failed/total));
  A2:=A1+(Pi*2*(Skipped/Total));
  AddPie(Ra,R.Top+Ra,Ra,A1,A2,ColYellow);
  cnv.font.FPColor:=colGreen;
  A1:=A2;
  A2:=A1+(Pi*2*((Total-(Skipped+Failed))/Total));
  Cnv.Textout(1,FH*3,Format('%d Passed (%3.1f%%',[Total-Skipped-Failed,PR*100]));
  AddPie(Ra,R.Top+Ra,Ra,A1,A2,ColGreen);
end;

initialization
  {$I wmimage.lrs}

  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.

