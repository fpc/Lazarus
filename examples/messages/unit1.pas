unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LMessages, LCLIntf;

const
  LM_MY_MESSAGE = LM_USER + 1;
  
type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure MyMessageHandler(var Message: TLMessage); message LM_MY_MESSAGE;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  {
   SendMessage sends it directly to control without waiting while other events
   become processed. So SendMessage acts as Control.Perform()

   As result you will see in memo such lines:
     1. Sending message
     2. Got message
     3. Exiting Button.Click()
  }
  Memo1.Lines.Add('--------------------------------');
  Memo1.Lines.Add('Sending message by <SendMessage>');
  SendMessage(Handle, LM_MY_MESSAGE, 1, 0);
  Memo1.Lines.Add('Exiting Button.Click()');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  {
   PostMessage add message at the bottom of message queue, so you will get it only
   after other events become processed. You can use PostMessage to postpone some
   operations.

   As result you will see in memo such lines:
     1. Sending message
     2. Exiting Button.Click()
     3. Got message
  }
  Memo1.Lines.Add('--------------------------------');
  Memo1.Lines.Add('Sending message by <PostMessage>');
  PostMessage(Handle, LM_MY_MESSAGE, 2, 0);
  Memo1.Lines.Add('Exiting Button.Click()');
end;

procedure TForm1.MyMessageHandler(var Message: TLMessage);
var
  S: String;
begin
  {
   Message handler
  }
  case Message.wParam of
    1: S := '<SendMessage>';
    2: S := '<PostMessage>';
  else
    S := '<unknown>'
  end;
  Memo1.Lines.Add('got message from: ' + S);
end;

initialization
  {$I unit1.lrs}

end.

