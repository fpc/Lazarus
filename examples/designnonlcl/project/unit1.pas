unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MyWidgetSet, LResources;

type

  { TMyForm1 }

  TMyForm1 = class(TMyForm)
    MyButton1: TMyButton;
    MyButton2: TMyButton;
    MyGroupBox1: TMyGroupBox;
    MyGroupBox2: TMyGroupBox;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MyForm1: TMyForm1;

implementation

{$R unit1.lfm}

end.

