unit AddEventExample; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls;
  
type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
  public
    MyBitmap: TBitmap;
  end;

  TMyComponent = class(TComponent)
  end;

implementation

end.

