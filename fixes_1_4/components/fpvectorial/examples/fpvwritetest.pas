{
FPVectorial example application for writing vectorial images
generated in code to disk. This program will generate the following
vectorial images:

single_line_1    One line from (0, 20) to (30, 30)
single_line_2    One line from (20, 30) to (30, 20)
polyline_1       One line from (0, 0) to (10, 10) to (20, 30) to (30, 20)
polyline_2       One line from (10, 10) to (20, 30) to (30, 20) to (40, 40)
bezier_1         One path starting in (0, 0) lining to (10, 10) then bezier to (20, 10) and then line to (30, 0)
bezier_2         One curve from (10, 10) to (20, 20)
text_ascii       One text written at (10, 10)
text_europen     One text testing european languages at (20, 20)
text_asian       One text testing asian languages at (30, 30)

Author: Felipe Monteiro de Carvalho

License: Public Domain
}
program fpvwritetest;

{$mode objfpc}{$H+}

uses
  fpvectorial, svgvectorialwriter, fpvutils, fpvectorialpkg;

const
  cFormat = vfSVG;
  cExtension = '.svg';
var
  Vec: TvVectorialDocument;
  Page: TvVectorialPage;
begin
  Vec := TvVectorialDocument.Create;
  try
    // All documents are 10cm x 10cm
    Vec.Width := 100;
    Vec.Height := 100;
    Page := Vec.AddPage();

    // single_line_1    One line from (0, 20) to (30, 30)
    Page.StartPath(0, 20);
    Page.AddLineToPath(30, 30);
    Page.EndPath();
    Vec.WriteToFile('single_line_1' + cExtension, cFormat);

    // single_line_2    One line from (20, 30) to (30, 20)
    Page.Clear;
    Page.StartPath(20, 30);
    Page.AddLineToPath(30, 20);
    Page.EndPath();
    Vec.WriteToFile('single_line_2' + cExtension, cFormat);

    // single_line_3    One line from (0, 20) to (30, 30) + frame
    Page.Clear;
    Page.StartPath(0, 20);
    Page.AddLineToPath(30, 30);
    Page.EndPath();
    Page.StartPath(0, 0);
    Page.AddLineToPath(100, 0);
    Page.AddLineToPath(100, 100);
    Page.AddLineToPath(0, 100);
    Page.AddLineToPath(0, 0);
    Page.EndPath();
    Vec.WriteToFile('single_line_3' + cExtension, cFormat);

    // polyline_1       One line from (0, 0) to (10, 10) to (20, 30) to (30, 20)
    Page.Clear;
    Page.StartPath(0, 0);
    Page.AddLineToPath(10, 10);
    Page.AddLineToPath(20, 30);
    Page.AddLineToPath(30, 20);
    Page.EndPath();
    Vec.WriteToFile('polyline_1' + cExtension, cFormat);

    //    polyline_2       One line from (10, 10) to (20, 30) to (30, 20) to (40, 40)
    Page.Clear;
    Page.StartPath(10, 10);
    Page.AddLineToPath(20, 30);
    Page.AddLineToPath(30, 20);
    Page.AddLineToPath(40, 40);
    Page.EndPath();
    Vec.WriteToFile('polyline_2' + cExtension, cFormat);

    // bezier_1         One path starting in (0, 0) lining to (10, 10) then bezier to (20, 10) and then line to (30, 0)
    Page.Clear;
    Page.StartPath(0, 0);
    Page.AddLineToPath(10, 10);
    Page.AddBezierToPath(10, 20, 20, 20, 20, 10);
    Page.AddLineToPath(30, 0);
    Page.EndPath();
    Vec.WriteToFile('bezier_1' + cExtension, cFormat);

    // bezier_2         One curve from (10, 10) to (20, 20)
    Page.Clear;
    Page.StartPath(10, 10);
    Page.AddBezierToPath(10, 15, 15, 20, 20, 10);
    Page.EndPath();
    Vec.WriteToFile('bezier_2' + cExtension, cFormat);

    // text_ascii       One text written at (10, 10)
    Page.Clear;
    Page.AddText(10, 10, 0, '10,10 Some text in english.');
    Vec.WriteToFile('text_ascii' + cExtension, cFormat);

    // text_europen     One text testing european languages at (20, 20)
    Page.Clear;
    Page.AddText(20, 20, 0, '20, 20 Mówić, cześć, Włosku, Parabéns, Assunção, Correções.');
    Vec.WriteToFile('text_europen' + cExtension, cFormat);

    // text_asian       One text testing asian languages at (30, 30)
    Page.Clear;
    Page.AddText(30, 30, 0, '30, 30 森林，是一个高密度树木的区域');
    Vec.WriteToFile('text_asian' + cExtension, cFormat);

    // multi_test_1     Combines various elements
    Page.Clear;
    Page.StartPath(0, 20);
    Page.AddLineToPath(30, 30);
    Page.EndPath();
    Page.StartPath(0, 0);
    Page.AddLineToPath(100, 0);
    Page.AddLineToPath(100, 100);
    Page.AddLineToPath(0, 100);
    Page.AddLineToPath(0, 0);
    Page.EndPath();
    Page.StartPath(0, 0);
    Page.AddLineToPath(10, 10);
    Page.AddBezierToPath(10, 20, 20, 20, 20, 10);
    Page.AddLineToPath(30, 0);
    Page.EndPath();
    Page.AddText(10, 10, 0, '10,10 Some text in english.');
    Page.AddText(20, 20, 0, '20, 20 Mówić, cześć, Włosku, Parabéns.');
    Page.AddText(30, 30, 0, '30, 30 森林，是一个高密');
    Vec.WriteToFile('multi_test_1' + cExtension, cFormat);

    // pen_test_1     Tests the properties of the Pen
    Page.Clear;
    Page.StartPath(0, 20);
    Page.AddLineToPath(30, 30);
    Page.SetPenWidth(10);
    Page.EndPath();
    Page.StartPath(0, 0);
    Page.AddLineToPath(100, 0);
    Page.AddLineToPath(100, 100);
    Page.AddLineToPath(0, 100);
    Page.AddLineToPath(0, 0);
    Page.SetPenWidth(10);
    Page.EndPath();
    Page.StartPath(0, 0);
    Page.AddLineToPath(10, 10);
    Page.AddBezierToPath(10, 20, 20, 20, 20, 10);
    Page.AddLineToPath(30, 0);
    Page.SetPenWidth(10);
    Page.EndPath();
    Vec.WriteToFile('pen_test_1' + cExtension, cFormat);

    // pen_test_2     Tests the properties of the Pen
    Page.Clear;
    Page.StartPath(0, 20);
    Page.AddLineToPath(30, 30);
    Page.SetPenWidth(10);
    Page.SetPenColor(RGBToFPColor(255, 0, 0));
    Page.EndPath();
    Page.StartPath(0, 0);
    Page.AddLineToPath(100, 0);
    Page.AddLineToPath(100, 100);
    Page.AddLineToPath(0, 100);
    Page.AddLineToPath(0, 0);
    Page.SetPenWidth(10);
    Page.SetPenColor(RGBToFPColor(0, 255, 0));
    Page.EndPath();
    Page.StartPath(0, 0);
    Page.AddLineToPath(10, 10);
    Page.AddBezierToPath(10, 20, 20, 20, 20, 10);
    Page.AddLineToPath(30, 0);
    Page.SetPenWidth(10);
    Page.SetPenColor(RGBToFPColor(0, 0, 255));
    Page.EndPath();
    Vec.WriteToFile('pen_test_2' + cExtension, cFormat);
  finally
    Vec.Free;
  end;
end.

