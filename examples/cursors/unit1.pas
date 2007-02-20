unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
  public
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Control: TControl;
begin
  if Source is TDragControlObject then
    Control := TDragControlObject(Source).Control else
    Control := TControl(Source);
  Accept := Control = Panel2;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Screen.Cursors[1] := LoadCursorFromLazarusResource('car');
  Button5.Cursor := 1;
end;

procedure TForm1.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  ShowMessage('drag accepted');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if Screen.Cursor = crHourGlass then
  begin
    Screen.Cursor := crDefault;
    Button4.Caption := 'Set Screen.Cursor';
  end else
  begin
    Screen.Cursor := crHourGlass;
    Button4.Caption := 'Unset Screen.Cursor';
  end;
end;

initialization
  {$I unit1.lrs}
  {$I car.lrs}

end.

