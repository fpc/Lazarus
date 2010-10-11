unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SynEdit,
  FileUtil, StdCtrls, Menus, SynHighlighterPosition, SynEditHighlighter;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    SynEdit1: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
  private
  public
    Highlighter: TSynPositionHighlighter;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.OpenMenuItemClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;
  SynEdit1.Lines.LoadFromFile(UTF8ToSys(OpenDialog1.FileName));
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Attr1, Attr2: TtkTokenKind;
begin
  // create highlighter
  Highlighter:=TSynPositionHighlighter.Create(Self);

  // add some attributes
  Attr1:=Highlighter.CreateTokenID('Attr1',clRed,clNone,[]);
  Attr2:=Highlighter.CreateTokenID('Attr2',clBlue,clNone,[fsBold]);

  // define highlighted areas

  // Line 0, from start of line to pos 3 (3 chars) highlighted as Attr1
  Highlighter.AddToken(0,3,Attr1);

  // Line 1, keep the first 3 chars as default
  Highlighter.AddToken(1,3,tkText);
  // Line 1, from pos 3 (end of prev token) to pos 5 (2 chars) highlighted as Attr2
  Highlighter.AddToken(1,5,Attr2);

  // use highlighter
  SynEdit1.Highlighter:=Highlighter;

  // set some example text
  SynEdit1.Lines.Text:=
    'This is some'+LineEnding
    +'example text.'+LineEnding;
end;

initialization
  {$I unit1.lrs}

end.

