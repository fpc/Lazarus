unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  popupnotifier, Buttons, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    PopupNotifier1: TPopupNotifier;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PopupNotifier1Close(Sender: TObject; var CloseAction: TCloseAction
      );
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  If PopupNotifier1.Visible then
   Begin
        PopupNotifier1.Hide;
        Button1.Caption := 'Show Popup';
   end else
   begin
        PopupNotifier1.ShowAtPos(100,100);
        Button1.Caption := 'Hide Popup';
   end;
end;

procedure TForm1.Button2Click(Sender: TObject);
Var I : Integer;
begin
  PopupNotifier1.Text := '';
  PopupNotifier1.Title := Edit1.Text;
  If Memo1.Lines.Count > 0 then
  Begin
       PopupNotifier1.Text := Memo1.Lines[0];
         For I := 1 to Memo1.Lines.Count -1 do
         PopupNotifier1.Text := PopupNotifier1.Text+LineEnding+Memo1.Lines[I];
  end;
end;

procedure TForm1.PopupNotifier1Close(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Button1.Caption := 'Show Popup';
end;




initialization
  {$I unit1.lrs}

end.

