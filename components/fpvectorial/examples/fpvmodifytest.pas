{
Author: Felipe Monteiro de Carvalho

License: Public Domain
}
program fpvmodifytest;

{$mode objfpc}{$H+}

uses
  fpvectorial, svgvectorialwriter, svgvectorialreader, fpvutils, fpvectorialpkg;

const
  cFormat = vfSVG;
  cExtension = '.svg';
var
  VecDoc: TvVectorialDocument;
  Vec: TvVectorialPage;
  Path: TPath;
  i: Integer;
  Segment: TPathSegment;
  _2DSegment: T2DSegment;
  BezSegment: T2DBezierSegment;
  lEntity: TvEntity;
begin
  VecDoc := TvVectorialDocument.Create;
  try
    // Read the file
    VecDoc.ReadFromFile('bezier_1.svg');
    Vec := VecDoc.GetPage(0);

    // Now add 10 to the Y coordinate of all elements
    for i := 0 to Vec.GetEntitiesCount() - 1 do
    begin
      lEntity := Vec.GetEntity(i);
      if not (lEntity is TPath) then Continue;
      Path := lEntity as TPath;
      Path.PrepareForSequentialReading();
      Path.Next();
      while Path.CurPoint <> nil do
      begin
        Segment := Path.CurPoint;

        if Segment is T2DBezierSegment then
        begin
          BezSegment := Segment as T2DBezierSegment;
          BezSegment.Y := BezSegment.Y + 10;
          BezSegment.Y2 := BezSegment.Y2 + 10;
          BezSegment.Y3 := BezSegment.Y3 + 10;
        end
        else if Segment is T2DSegment then
        begin
          _2DSegment := Segment as T2DSegment;
          _2DSegment.Y := _2DSegment.Y + 10;
        end;

        Path.Next();
      end;
    end;

    // Write the changed file to disk
    VecDoc.WriteToFile('bezier_1_mod' + cExtension, cFormat);
  finally
    VecDoc.Free;
  end;
end.

