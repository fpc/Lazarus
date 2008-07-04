unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
  private
    { private declarations }
  public
    procedure SelectCells(Data: PtrInt);
    procedure MoveToCorner(Data: PtrInt);
    procedure SendKeys(Data: PtrInt);
  end; 

var
  Form1: TForm1;

implementation

uses
  MouseAndKeyInput, LCLType;

{ TForm1 }

procedure TForm1.FormDblClick(Sender: TObject);
begin
  Caption := Caption + ' DblClicked';
end;

procedure TForm1.SelectCells(Data: PtrInt);
begin
  StringGrid1.SetFocus;
  Application.ProcessMessages;
  MouseInput.Down(mbLeft, [], StringGrid1, 10, 10);
  MouseInput.Up(mbLeft, [], StringGrid1, 200, 100);
end;

procedure TForm1.MoveToCorner(Data: PtrInt);
begin
  MouseInput.Move([], Form1, 0, 0, 1000);
  MouseInput.DblClick(mbLeft, []);
end;

procedure TForm1.SendKeys(Data: PtrInt);
begin
  Edit1.SetFocus;
  Application.ProcessMessages;
  KeyInput.Press(VK_H);
  KeyInput.Press(VK_E);
  KeyInput.Press(VK_L);
  KeyInput.Press(VK_L);
  KeyInput.Press(VK_O);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Application.QueueAsyncCall(@SelectCells, 0);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Application.QueueAsyncCall(@MoveToCorner, 0);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Application.QueueAsyncCall(@SendKeys, 0);
end;

initialization
  {$I unit1.lrs}

end.

