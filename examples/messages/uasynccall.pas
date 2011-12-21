unit uasynccall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

  
type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    RadioGroup1: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure SimpleMethod(Data: PtrInt);
    procedure ComplexMethod(Data: PtrInt);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

type
  TMyData = record
    S: string;
    data: longint;
  end;
  PMyData = ^TMyData;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  MyData: PMyData;
begin
  {
   A direct call, calls the method directly and is executed before the
   Button1Click is finished.

   As result you will see in memo such lines:
     1. Sending message
     2. Got message
     3. Exiting Button.Click()
  }
  Memo1.Lines.Add('--------------------------------');
  Memo1.Lines.Add('Sending message by <Direct call>');
  case RadioGroup1.ItemIndex of
    0 : SimpleMethod(1);
    1 :
      begin
        new(MyData);
        MyData^.data:=1;
        MyData^.S:= 'direct call';
        ComplexMethod(PtrInt(MyData));
      end;
  end;
  Memo1.Lines.Add('Exiting Button.Click()');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  MyData: PMyData;
begin
  {
   QueueAsyncCall queues the method call, so you will get it only
   after other events have been processed.
   You can use QueueAsyncCall to postpone some
   operations, for example until the application is idle.

   As result you will see in memo such lines:
     1. Sending message
     2. Exiting Button.Click()
     3. Got message
  }
  Memo1.Lines.Add('--------------------------------');
  Memo1.Lines.Add('Sending message by QueueAsyncCall');
  case RadioGroup1.ItemIndex of
    0 : Application.QueueAsyncCall(@SimpleMethod, 2);
    1 :
      begin
        new(MyData);
        MyData^.data:=1;
        MyData^.S:= 'QueueAsyncCall call';
        Application.QueueAsyncCall(@ComplexMethod, PtrInt(MyData));
      end;
  end;
  Memo1.Lines.Add('Exiting Button.Click()');
end;

procedure TForm1.SimpleMethod(Data: PtrInt);
var
  S: String;
begin
  case Data of
    1: S := '<DirectCall>';
    2: S := '<QueueAsyncCall>';
  else
    S := '<unknown>'
  end;
  Memo1.Lines.Add('SimpleMethod got called using: ' + S);
end;

procedure TForm1.ComplexMethod(Data: PtrInt);
var
  MyData: TMyData;
begin
  MyData:=PMyData(Data)^;
  Memo1.Lines.Add(
    format('Complex got called using %s and data %d',
      [MyData.S, MyData.Data]));
  Dispose(PMyData(Data));
end;

end.

