unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, XMLPropStorage, IniPropStorage, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    IniPropStorage1: TIniPropStorage;
    Memo1: TMemo;
    XMLPropStorage1: TXMLPropStorage;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Add('Enter some text here.');
  Memo1.Lines.Add('At exit, it will be stored by TIniPropStorage and TXmlPropStorage');
  Memo1.Lines.Add('along with the form''s width and height.');
end;

initialization


end.

