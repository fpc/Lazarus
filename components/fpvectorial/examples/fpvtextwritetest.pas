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
  BulletList: TvBulletList;
begin
  Vec := TvVectorialDocument.Create;
  try
    // A4 -> 210mm x 297mm
    Vec.Width := 210;
    Vec.Height := 297;
    Vec.AddStandardTextDocumentStyles(vfODT);

    // First page sequence
    Page := Vec.AddTextPageSequence();
    // Title
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading1;
    CurParagraph.AddText('Lazarus');
    // paragraph
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;
    CurParagraph.AddText('Lazarus is a free and open source development tool '
      + 'for the Free Pascal compiler, which is also free and open source.');
    // Title
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleHeading2;
    CurParagraph.AddText('Overview');
    // paragraph
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;
    CurParagraph.AddText('Lazarus is a free cross-platform visual integrated '
      + 'development environment (IDE) for rapid application development (RAD) '
      + 'using the Free Pascal compiler supported dialects of Object Pascal. '
      + 'Developers use Lazarus to create native code console and graphical user '
      + 'interface (GUI) applications for the desktop along with mobile devices, '
      + 'web applications, web services, and visual components and function '
      + 'libraries (.so, .dll, etc) for use by other programs for any platform '
      + 'the Free Pascal compiler supports( Mac, Unix, Linux, Windows, etc).');
    // Empty line
    CurParagraph := Page.AddParagraph();
    CurParagraph.Style := Vec.StyleTextBody;
    // Lazarus provides a highly visual development environment for the creation of rich user interfaces, application logic, and other supporting code artifacts. Along with the customary project management features, the Lazarus IDE also provides features that includes but are not limited to:

    BulletList := Page.AddBulletList();
    BulletList.AddItem(0, 'A What You See Is What You Get (WYSIWYG) visual windows layout designer');
    BulletList.AddItem(0, 'An extensive set of GUI widgets or visual components such as edit boxes, buttons, dialogs, menus, etc.');
    BulletList.AddItem(0, 'An extensive set of non visual components for common behaviors such as persistence of application settings');
    BulletList.AddItem(0, 'A set of data connectivity components for MySQL, PostgresSQL, FireBird, Oracle, SQL Lite, Sybase, and others');
    BulletList.AddItem(0, 'Data aware widget set that allows the developer to see data in visual components in the designer to assist with development');
    BulletList.AddItem(0, 'Interactive code debugger');
    BulletList.AddItem(0, 'Code completion');
    BulletList.AddItem(0, 'Code templates');
    BulletList.AddItem(0, 'Syntax highlighting');
    BulletList.AddItem(0, 'Context sensitive help');
    BulletList.AddItem(0, 'Text resource manager for internationalization');
    BulletList.AddItem(0, 'Automatic code formatting');
    BulletList.AddItem(0, 'The ability to create custom components');

    Vec.WriteToFile('text_output.odt', vfODT);
  finally
    Vec.Free;
  end;
end.

