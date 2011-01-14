unit testpreferredsize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, fpcunit,
  testglobals;

type

  { TTestPreferredSize }

  TTestPreferredSize = class(TTestCase)
  published
    procedure TestGroupBoxPreferredSize1;
  end;

implementation

{ TTestPreferredSize }

procedure TTestPreferredSize.TestGroupBoxPreferredSize1;
var
  Form1: TForm;
  GroupBox1: TGroupBox;
  w1: integer;
  h1: integer;
  w2: Integer;
  h2: Integer;
begin
  // create a groupbox on a form
  Form1:=TForm.Create(nil);
  Form1.SetBounds(100,100,300,300);
  GroupBox1:=TGroupBox.Create(Form1);
  GroupBox1.SetBounds(10,10,100,100);
  GroupBox1.Parent:=Form1;
  GroupBox1.Caption:='GroupBox1';
  Form1.Show;
  Application.ProcessMessages;

  // get the preferredsize of the groupbox with a size of 100x100
  w1:=0;
  h1:=0;
  GroupBox1.GetPreferredSize(w1,h1,true,false);
  //writeln('TTestPreferredSize.TestGroupBox1 ',w1,',',h1);

  // get the preferredsize of the groupbox with a size of 10x100
  GroupBox1.Width:=10;
  w2:=0;
  h2:=0;
  GroupBox1.GetPreferredSize(w2,h2,true,false);
  //writeln('TTestPreferredSize.TestGroupBox1 ',w2,',',h2);

  // the preferredsize must be independent of the the current width,height
  AssertEquals('TGroupBox.PreferredSize changed after SetBounds: ',true,(w1=w2) and (h1=h2));

  Form1.Free;
  Application.ProcessMessages;
end;

initialization
  AddToLCLTestSuite(TTestPreferredSize);

end.

