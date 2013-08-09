{
FPVectorial example application for writing a text document file to disk.

Author: Felipe Monteiro de Carvalho

License: Public Domain
}
program fpvtextwritetest;

{$mode objfpc}{$H+}

uses
  fpvectorial, odtvectorialwriter, fpvutils, fpvectorialpkg;

{$R *.res}

var
  Vec: TvVectorialDocument;
  Page: TvTextPageSequence;
  CurParagraph: TvParagraph;
begin
  Vec := TvVectorialDocument.Create;
  try
    // A4 -> 210mm x 297mm
    Vec.Width := 210;
    Vec.Height := 297;
    Vec.AddStandardODTTextDocumentStyles();

    // First page sequence
    Page := Vec.AddTextPageSequence();
    // First paragraph
    CurParagraph := Page.AddParagraph();
    CurParagraph.AddText('');

    CurParagraph := Page.AddParagraph();
    CurParagraph.AddText('Some text 2');

    Vec.WriteToFile('text_output.odt', vfODT);
  finally
    Vec.Free;
  end;
end.

