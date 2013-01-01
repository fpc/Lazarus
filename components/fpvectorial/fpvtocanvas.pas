unit fpvtocanvas;

{$mode objfpc}{$H+}

interface

{$ifndef Windows}
{.$define FPVECTORIAL_TOCANVAS_DEBUG}
{$endif}

uses
  Classes, SysUtils, Math,
  fpcanvas,
  fpimage,
  fpvectorial, fpvutils;

procedure DrawFPVectorialToCanvas(ASource: TvVectorialPage;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
procedure DrawFPVPathToCanvas(ASource: TvVectorialPage; CurPath: TPath;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
procedure DrawFPVEntityToCanvas(ASource: TvVectorialPage; CurEntity: TvEntity;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
procedure DrawFPVTextToCanvas(ASource: TvVectorialPage; CurText: TvText;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);

implementation

{@@
  This function draws a FPVectorial vectorial image to a TFPCustomCanvas
  descendent, such as TCanvas from the LCL.

  Be careful that by default this routine does not execute coordinate transformations,
  and that FPVectorial works with a start point in the bottom-left corner, with
  the X growing to the right and the Y growing to the top. This will result in
  an image in TFPCustomCanvas mirrored in the Y axis in relation with the document
  as seen in a PDF viewer, for example. This can be easily changed with the
  provided parameters. To have the standard view of an image viewer one could
  use this function like this:

  DrawFPVectorialToCanvas(ASource, ADest, 0, ASource.Height, 1.0, -1.0);
}
procedure DrawFPVectorialToCanvas(ASource: TvVectorialPage;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
var
  i: Integer;
  CurEntity: TvEntity;
begin
  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn(':>DrawFPVectorialToCanvas');
  {$endif}

  for i := 0 to ASource.GetEntitiesCount - 1 do
  begin
    {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
    Write(Format('[Path] ID=%d', [i]));
    {$endif}

    CurEntity := ASource.GetEntity(i);

    if CurEntity is TPath then DrawFPVPathToCanvas(ASource, TPath(CurEntity), ADest, ADestX, ADestY, AMulX, AMulY)
    else if CurEntity is TvText then DrawFPVTextToCanvas(ASource, TvText(CurEntity), ADest, ADestX, ADestY, AMulX, AMulY)
    else DrawFPVEntityToCanvas(ASource, CurEntity, ADest, ADestX, ADestY, AMulX, AMulY);
  end;

  {$ifdef FPVECTORIAL_TOCANVAS_DEBUG}
  WriteLn(':<DrawFPVectorialToCanvas');
  {$endif}
end;

procedure DrawFPVPathToCanvas(ASource: TvVectorialPage; CurPath: TPath;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
begin
  CurPath.Render(ADest, colWhite, ADestX, ADestY, AMulX, AMulY);
end;

procedure DrawFPVEntityToCanvas(ASource: TvVectorialPage; CurEntity: TvEntity;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
begin
  CurEntity.Render(ADest, colWhite, ADestX, ADestY, AMulX, AMulY);
end;

procedure DrawFPVTextToCanvas(ASource: TvVectorialPage; CurText: TvText;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
begin
  CurText.Render(ADest, colWhite, ADestX, ADestY, AMulX, AMulY);
end;

end.

