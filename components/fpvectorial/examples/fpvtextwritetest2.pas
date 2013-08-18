{
FPVectorial example application for writing a text document file to disk.

Author: Felipe Monteiro de Carvalho

License: Public Domain
}
Program fpvtextwritetest2;

{$mode objfpc}{$H+}

Uses
  fpvectorial,
  odtvectorialwriter,
  fpvutils,
  fpvectorialpkg,
  SysUtils;

{$R *.res}

Var
  Vec: TvVectorialDocument;
  Page: TvTextPageSequence;
  CurParagraph: TvParagraph;
  BoldStyle: TvStyle;
  CenterStyle: TvStyle;
Begin
  Vec := TvVectorialDocument.Create;
  Try
    // A4 -> 210mm x 297mm
    Vec.Width := 210;
    Vec.Height := 297;

    Vec.AddStandardODTTextDocumentStyles();

    // Until a Template is available, create the Bold Style ourselves
    BoldStyle := Vec.AddStyle();

    // This implies this style should not be applied to Paragraphs
    BoldStyle.Kind := vskTextSpan;
    BoldStyle.Name := 'Bold';
    BoldStyle.Font.Bold := True;
    BoldStyle.SetElements := BoldStyle.SetElements + [spbfFontBold];

    CenterStyle := Vec.AddStyle();
    CenterStyle.ApplyOver(Vec.StyleTextBody);
    CenterStyle.Name := 'Text Body Centered';
    CenterStyle.Alignment := vsaCenter;
    CenterStyle.SetElements := CenterStyle.SetElements + [spbfAlignment];

    // First page sequence
    Page := Vec.AddTextPageSequence();
    Page.Width := 210;
    Page.Height := 297;

    // Set the Header
    CurParagraph := Page.Header.AddParagraph;
    CurParagraph.Style := CenterStyle;
    CurParagraph.AddText('Introduction to Lazarus and FreePascal').Style := BoldStyle;

    // Set the Footer
    CurParagraph := Page.Footer.AddParagraph;
    CurParagraph.Style := CenterStyle;
    CurParagraph.AddText('Confidential' + #11 + 'Page x of y' + #11 +
      DateTimeToStr(Now)).Style :=
      BoldStyle;

    // Title
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading1;
    CurParagraph.AddText('Lazarus');

    // paragraph
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;
    With CurParagraph Do
    Begin
      AddText('Lazarus ').Style := BoldStyle;
      // Adding the Paragraph as a long string
      AddText('is a free and open source development tool for the ' +
        'Free Pascal compiler, which is also free and open source.');
    End;

    // Empty line
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;

    // Title
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading2;
    CurParagraph.AddText('Overview');

    // paragraph
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;
    With CurParagraph Do
    Begin
      // Adding the Paragraph as a series of TvText's
      // trailing space required
      // Each TvText gets added as it's own text run inside the Word Doc
      AddText('Lazarus ').Style := BoldStyle;
      AddText('is a free cross-platform visual integrated development ');
      AddText('environment (IDE) for rapid application development (RAD) ');
      AddText('using the Free Pascal compiler supported dialects of Object ');
      AddText('Pascal. Developers use ');
      AddText('Lazarus ').Style := BoldStyle;
      AddText('to create native code console ');
      AddText('and graphical user interface (GUI) applications for the desktop ');
      AddText('along with mobile devices, web applications, web services, ');
      AddText('and visual components and function libraries (.so, .dll, etc) ');
      AddText('for use by other programs for any platform the Free Pascal ');
      AddText('compiler supports( Mac, Unix, Linux, Windows, etc). ');
    End;

    // Empty line
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;


    // Second page sequence
    Page := Vec.AddTextPageSequence();
    Page.Height := 210;  // Switched to enforce Landscape
    Page.Width := 297;

    // Set the Header
    CurParagraph := Page.Header.AddParagraph;
    CurParagraph.Style := CenterStyle;
    CurParagraph.AddText('Testing Concepts').Style := BoldStyle;

    // Title
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading2;
    CurParagraph.AddText('Testing Strings');

    // Test for XML tags
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;
    // Adding to the Paragraph by extending the TStringList inside a single TvText
    // Each line will be added inside a new text run inside the Word Doc
    // with a Soft Return inserted at the end of each line
    With CurParagraph.AddText('').Value Do
    Begin
      Add(#11 + '<test>&"This shouldn''t break the resulting document."</test>' + #11);
      Add(#11 + '<test>!@#$%^&*()_+=-~`;:{}[],./|\?</test>' + #11);
    End;

    Vec.WriteToFile('text_output.docx', vfDOCX);
  Finally
    Vec.Free;
  End;
End.
