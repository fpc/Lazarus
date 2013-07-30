{
FPVectorial example application for writing a text document file to disk.

Author: Felipe Monteiro de Carvalho

License: Public Domain
}
program fpvtextwritetest;

{$mode objfpc}{$H+}

uses
  fpvectorial, odtvectorialwriter, fpvutils, fpvectorialpkg;

var
  Vec: TvVectorialDocument;
  Page: TvTextPageSequence;
begin
  Vec := TvVectorialDocument.Create;
  try
    // A4 -> 210mm x 297mm
    Vec.Width := 210;
    Vec.Height := 297;
    Page := Vec.AddTextPageSequence();

    // First page sequence
    {Page.StartPath(0, 20);
    Page.AddLineToPath(30, 30);
    Page.EndPath();}

    Vec.WriteToFile('text_output.odt', vfODT);
  finally
    Vec.Free;
  end;
end.

