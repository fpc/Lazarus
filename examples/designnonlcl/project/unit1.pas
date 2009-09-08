unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MyWidgetSet, LResources;

type
  TMyForm1 = class(TMyForm)
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MyForm1: TMyForm1;

implementation

initialization
  {$I unit1.lrs}

end.

