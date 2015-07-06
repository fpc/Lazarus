unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, XMLPropStorage, IniPropStorage;

type

  { TForm1 }

  TForm1 = class(TForm)
    IniPropStorage1: TIniPropStorage;
    Memo1: TMemo;
    XMLPropStorage1: TXMLPropStorage;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

initialization


end.

