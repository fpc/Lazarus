unit TestQueueAsyncCall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TQueueAsyncCallForm }

  TQueueAsyncCallForm = class(TForm)
    CallButton: TButton;
    LogListBox: TListBox;
    procedure CallButtonClick(Sender: TObject);
  private
    { private declarations }
    FCounter: PtrInt;
    procedure Async(Data: PtrInt);
  public
    { public declarations }
  end; 

var
  QueueAsyncCallForm: TQueueAsyncCallForm;

implementation

{ TQueueAsyncCallForm }

procedure TQueueAsyncCallForm.CallButtonClick(Sender: TObject);
begin
  LogListBox.Items.Add('Click 1');
  FCounter := FCounter+1;
  Application.QueueAsyncCall(@Async,FCounter);
  LogListBox.Items.Add('Click 2');
end;

procedure TQueueAsyncCallForm.Async(Data: PtrInt);
begin
   LogListBox.Items.Add('Async '+ IntToStr(Data));
end;

initialization
  {$I testqueueasynccall.lrs}

end.

