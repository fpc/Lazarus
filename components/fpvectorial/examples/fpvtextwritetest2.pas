{
FPVectorial example application for writing a text document file to disk.

Author: Mike Thompson
        Felipe Monteiro de Carvalho

License: Public Domain
}
Program fpvtextwritetest2;

{$mode objfpc}{$H+}

Uses
  fpvectorial,
  odtvectorialwriter,
  fpvutils,
  fpvectorialpkg,
  docxvectorialwriter,
  SysUtils, FPImage;

{$R *.res}

Const
    ONE_POINT_IN_MM = 0.35278;

Var
  Vec: TvVectorialDocument;
  Page: TvTextPageSequence;
  CurParagraph: TvParagraph;
  BoldTextStyle: TvStyle;
  CenterParagraphStyle, Center2: TvStyle;
  BulletList : TvBulletList;
  dtTime : TDateTime;
  CurText : TvText;

  CurTable : TvTable;
  CurRow : TvTableRow;
  CurCell : TvTableCell;

  i, j, iMax : Integer;

Begin
  Vec := TvVectorialDocument.Create;
  Try
    // A4 -> 210mm x 297mm
    Vec.Width := 210;
    Vec.Height := 297;

    // Until there is a need, we will stick with supporting ODT styles
    Vec.AddStandardTextDocumentStyles(vfODT);

    Vec.StyleTextBody.MarginRight:=10;
    Vec.StyleTextBody.MarginLeft:=10;
    Vec.StyleTextBody.SetElements:= Vec.StyleTextBody.SetElements + [sseMarginLeft, sseMarginRight];

    // Until a Template is available, create the Bold Style ourselves
    BoldTextStyle := Vec.AddStyle();

    BoldTextStyle.Kind := vskTextSpan; // This implies this style should not be applied to Paragraphs
    BoldTextStyle.Name := 'Bold';
    BoldTextStyle.Font.Bold := True;
    BoldTextStyle.SetElements := BoldTextStyle.SetElements + [spbfFontBold];

    CenterParagraphStyle := Vec.AddStyle();
    CenterParagraphStyle.ApplyOver(Vec.StyleTextBody);
    CenterParagraphStyle.Name := 'Text Body Centered';
    CenterParagraphStyle.Alignment := vsaCenter;
    CenterParagraphStyle.SetElements := CenterParagraphStyle.SetElements + [spbfAlignment];

    // First page sequence
    Page := Vec.AddTextPageSequence();
    Page.Width := 210;
    Page.Height := 297;

    // Set the Header
    CurParagraph := Page.Header.AddParagraph;
    CurParagraph.Style := CenterParagraphStyle;
    CurParagraph.AddText('Introduction to Lazarus and FreePascal').Style := BoldTextStyle;

    // Set the Footer
    CurParagraph := Page.Footer.AddParagraph;
    CurParagraph.Style := CenterParagraphStyle;
    CurParagraph.AddText('Confidential' + #09 + 'Page x of y' + #09 +
      DateTimeToStr(Now)).Style :=
      BoldTextStyle;

    // Title
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading1;
    CurParagraph.AddText('Lazarus');

    // paragraph
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;
    With CurParagraph Do
    Begin
      AddText('Lazarus ').Style := BoldTextStyle;
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
      AddText('Lazarus ').Style := BoldTextStyle;
      AddText('is a free cross-platform visual integrated development ');
      AddText('environment (IDE) for rapid application development (RAD) ');
      AddText('using the Free Pascal compiler supported dialects of Object ');
      AddText('Pascal. Developers use ');
      AddText('Lazarus ').Style := BoldTextStyle;
      AddText('to create native code console ');
      AddText('and graphical user interface (GUI) applications for the desktop ');
      AddText('along with mobile devices, web applications, web services, ');
      AddText('and visual components and function libraries (.so, .dll, etc) ');
      AddText('for use by other programs for any platform the Free Pascal ');
      AddText('compiler supports( Mac, Unix, Linux, Windows, etc). ');
    End;

    BulletList := Page.AddBulletList();
    BulletList.Style := Vec.StyleList;
    BulletList.AddItem(0, 'A What You See Is What You Get (WYSIWYG) visual windows layout designer');
    BulletList.AddItem(1, 'An extensive set of GUI widgets or visual components such as edit boxes, buttons, dialogs, menus, etc.');
    BulletList.AddItem(2, 'An extensive set of non visual components for common behaviors such as persistence of application settings');
    BulletList.AddItem(3, 'A set of data connectivity components for MySQL, PostgresSQL, FireBird, Oracle, SQL Lite, Sybase, and others');
    BulletList.AddItem(4, 'Data aware widget set that allows the developer to see data in visual components in the designer to assist with development');
    BulletList.AddItem(5, 'Interactive code debugger');
    BulletList.AddItem(5, 'Code completion');
    BulletList.AddItem(4, 'Code templates');
    BulletList.AddItem(3, 'Syntax highlighting');
    BulletList.AddItem(2, 'Context sensitive help');
    BulletList.AddItem(1, 'Text resource manager for internationalization');
    BulletList.AddItem(0, 'Automatic code formatting');
    BulletList.AddItem(0, 'The ability to create custom components');

    // Empty line
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;


    // Second page sequence
    Page := Vec.AddTextPageSequence();
    Page.Height := 210;  // Switched to enforce Landscape
    Page.Width := 297;

    // Set the Header
    CurParagraph := Page.Header.AddParagraph;
    CurParagraph.Style := CenterParagraphStyle;
    CurParagraph.AddText('Testing Concepts').Style := BoldTextStyle;

    // Title
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading2;
    CurParagraph.AddText('Testing Strings');


    // Test for XML tags
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;
    CurText := CurParagraph.AddText('');
    // Adding to the Paragraph by extending the TStringList inside a single TvText
    // Each line will be added inside a new text run inside the Word Doc
    // with a Soft Return inserted at the end of each line
    With CurText.Value Do
    Begin
      Add(#09 + '<test>&"This shouldn''t break the resulting document."</test>' + #09);
      Add(#09 + '<test>!@#$%^&*()_+=-~`;:{}[],./|\?</test>' + #09);
    End;

    // Third page sequence
    Page := Vec.AddTextPageSequence();
    Page.Height := 297;  // back to Portrait
    Page.Width := 210;

    // Set the Header
    CurParagraph := Page.Header.AddParagraph;
    CurParagraph.Style := CenterParagraphStyle;
    CurParagraph.AddText('Testing Tables').Style := BoldTextStyle;

    // Title
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading2;
    CurParagraph.AddText('Manual Table');

    CurTable := Page.AddTable;
    CurTable.PreferredWidth := Dimension(100, dimPercent);

    CurTable.CellSpacing    := 0;
    CurTable.Borders.Left.Width := 2 * ONE_POINT_IN_MM;
    CurTable.Borders.Right.Width := 2 * ONE_POINT_IN_MM;
    CurTable.Borders.Top.Width := 2 * ONE_POINT_IN_MM;
    CurTable.Borders.Bottom.Width := 2 * ONE_POINT_IN_MM;
    CurTable.Borders.InsideHoriz.LineType:=tbtSingle;
    CurTable.Borders.InsideVert.LineType :=tbtDashed;

    CurRow := CurTable.AddRow;
    CurRow.BackgroundColor := RGBToFPColor(192, 192, 192);
    CurRow.Header := True;

    CurCell := CurRow.AddCell;
    CurParagraph := CurCell.AddParagraph;
    CurParagraph.AddText('First Cell, First Row');

    CurCell := CurRow.AddCell;
    CurParagraph := CurCell.AddParagraph;
    CurParagraph.AddText('Second Cell, First Row');

    CurCell := CurRow.AddCell;
    CurParagraph := CurCell.AddParagraph;
    CurParagraph.AddText('Third Cell, First Row');

    CurRow := CurTable.AddRow;
    CurRow.CellSpacing := ONE_POINT_IN_MM;
    CurCell := CurRow.AddCell;
    CurParagraph := CurCell.AddParagraph;
    CurParagraph.AddText('First Cell, Second Row');

    CurCell := CurRow.AddCell;
    CurCell.Borders.Left.LineType := tbtDouble;
    CurCell.Borders.Left.Color := RGBToFPColor(255, 0, 0);
    CurCell.Borders.Right.LineType := tbtDouble;
    CurCell.Borders.Right.Color := RGBToFPColor(255, 0, 0);
    CurCell.Borders.Top.LineType := tbtDouble;
    CurCell.Borders.Top.Color := RGBToFPColor(255, 0, 0);
    CurCell.Borders.Bottom.LineType := tbtDouble;
    CurCell.Borders.Bottom.Color := RGBToFPColor(255, 0, 0);

    CurCell.VerticalAlignment:=vaCenter;

    CurParagraph := CurCell.AddParagraph;
    CurParagraph.Style := CenterParagraphStyle;
    CurParagraph.AddText('Second Cell, Second Row'+#13+'This should have a red double border');

    CurCell := CurRow.AddCell;
    CurParagraph := CurCell.AddParagraph;
    CurParagraph.AddText('Third Cell, Second Row');

    CurRow := CurTable.AddRow;
    CurCell := CurRow.AddCell;
    CurParagraph := CurCell.AddParagraph;
    CurParagraph.AddText('First Cell, Third Row');

    CurCell := CurRow.AddCell;
    CurParagraph := CurCell.AddParagraph;
    CurParagraph.AddText('Second Cell, Third Row');

    CurCell := CurRow.AddCell;
    CurParagraph := CurCell.AddParagraph;
    CurParagraph.AddText('Third Cell, Third Row');

    // Style for Subsequent Tables
    Center2 := Vec.AddStyle();
    Center2.Name := 'Table Body Centered';
    Center2.Font.Name := 'Verdana';
    Center2.Font.Size := 8;
    Center2.Alignment := vsaCenter;
    Center2.MarginTop:=2*ONE_POINT_IN_MM;
    Center2.MarginBottom:=2*ONE_POINT_IN_MM;
    Center2.SetElements := [spbfFontSize, spbfFontName, spbfAlignment, sseMarginTop, sseMarginBottom];

    // Title
    CurParagraph := Page.AddParagraph();
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading2;
    CurParagraph.AddText('Coded Table #1');

    // Second Table
    CurTable := Page.AddTable;
    CurTable.PreferredWidth := Dimension(100, dimPercent);

    For i := 0 To 20 do
    Begin
      CurRow := CurTable.AddRow;

      // Header Row
      If i=0 Then
      Begin
        CurRow.BackgroundColor := RGBToFPColor(192, 192, 192);
        CurRow.Header := True;
      end;

      for j := 0 to 4 Do
      begin
        CurCell := CurRow.AddCell;

        CurParagraph := CurCell.AddParagraph;
        CurParagraph.Style := Center2;

        If i=0 Then
          CurParagraph.AddText(Format('Header %d', [j])).Style := BoldTextStyle
        Else
          CurParagraph.AddText(Format('%d x %d', [i, j]));
      end;
    end;

    CurParagraph := Page.AddParagraph();
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading2;
    CurParagraph.AddText('Coded Table #2');

    // Third Table
    CurTable := Page.AddTable;
    CurTable.PreferredWidth := Dimension(100, dimPercent);

    CurTable.ColWidthsUnits:=dimMillimeter;
    CurTable.AddColWidth(15);
    CurTable.AddColWidth(20);
    CurTable.AddColWidth(20);
    CurTable.AddColWidth(20);
    CurTable.AddColWidth(79.5); // For Word (and possibly odt), this only has to be close.
                                // Table.Width=100% means last col is autocalculated
                                // Added a close value for other renderers such as Wordpad that
                                // might not support the autocalculation

    // Header Row
    CurRow := CurTable.AddRow;

    CurRow.BackgroundColor := RGBToFPColor($64, $95, $ED);
    CurRow.Header := True;

    for j := 0 to 4 Do
    begin
      CurCell := CurRow.AddCell;

      CurParagraph := CurCell.AddParagraph;
      CurParagraph.Style := Center2;

      CurParagraph.AddText(Format('Header %d', [j])).Style := BoldTextStyle
    End;

    // Data Rows
    For i := 1 To 20 do
    Begin
      CurRow := CurTable.AddRow;

      if (i Mod 2 = 1) Then
        CurRow.BackgroundColor := RGBToFPColor(224, 224, 224);

      If (i mod 5 <> 1) Then
        iMax := 4
      Else
        //iMax := 4;
        iMax := 3;

      for j := 0 to iMax Do
      begin
        CurCell := CurRow.AddCell;

        CurParagraph := CurCell.AddParagraph;
        CurParagraph.Style := Center2;

        If (iMax=3) And (j=3) Then
        Begin
          CurCell.SpannedCols := 2;
          CurParagraph.AddText(Format('Merged Cells (%d x %d) & (%d x %d)', [i, j, i, j+1]));
        end
        Else
          CurParagraph.AddText(Format('(%d x %d)', [i, j]));
      end;
    end;
(*
    // Fourth page sequence
    Page := Vec.AddTextPageSequence();
    Page.Height := 297;
    Page.Width := 210;

    // Set the Header
    CurParagraph := Page.Header.AddParagraph;
    CurParagraph.Style := CenterParagraphStyle;
    CurParagraph.AddText('Testing Images').Style := BoldTextStyle;

    // Title
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading2;
    CurParagraph.AddText('Image 1');
*)

    dtTime := Now;
    Vec.WriteToFile('text_output.docx', vfDOCX);

    WriteLn('Native docx writer: '+Format('%.1f msec', [24*60*60*1000*(Now-dtTime)]));
    dtTime := Now;

    Vec.WriteToFile('text_output.odt', vfODT);

    WriteLn('Native odt writer: '+Format('%.1f msec', [24*60*60*1000*(Now-dtTime)]));
  Finally
    Vec.Free;
  End;
End.
