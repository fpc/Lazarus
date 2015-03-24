unit fpvtocanvas;

{$mode objfpc}{$H+}

interface

{$ifndef Windows}
{.$define FPVECTORIAL_TOCANVAS_DEBUG}
{$endif}

uses
  fpcanvas, fpvectorial;

procedure DrawFPVectorialToCanvas(ASource: TvVectorialPage;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);

implementation

procedure DrawFPVectorialToCanvas(ASource: TvVectorialPage;
  ADest: TFPCustomCanvas;
  ADestX: Integer = 0; ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0);
begin
  ASource.Render(ADest, ADestX, ADestY, AMulX, AMulY);
end;

end.

