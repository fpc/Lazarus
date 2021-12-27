unit ListBoxTestFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LazLogger;

type
  
  { TListBoxTestForm }

  TListBoxTestForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ListBox: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private

  public

  end;

var
  ListBoxTestForm: TListBoxTestForm;

implementation

{$R *.lfm}

{ TListBoxTestForm }

procedure TListBoxTestForm.Button1Click(Sender: TObject);
var
  Index: integer;
begin
  Index := ListBox.ItemIndex;
  if Index = -1 then
    ListBox.Items.Add('Button 1 clicked')
  else 
    ListBox.Items.Insert(Index, 'Button 1 clicked at '+IntToStr(Index));
  for Index := 0 to ListBox.Items.Count - 1 do 
    ListBox.Items.Objects[Index] := TObject(PtrInt(Index));
end;

procedure TListBoxTestForm.Button2Click(Sender: TObject);
var
  Index: integer;
begin
  Index := ListBox.ItemIndex;
  if Index <> -1 then
    ListBox.Items.Delete(Index);
end;

procedure TListBoxTestForm.Button3Click(Sender: TObject);
begin
  ListBox.Items.Clear; 
end;

procedure TListBoxTestForm.Button4Click(Sender: TObject);
var
  X: PtrInt;
begin
  if ListBox.ItemIndex < 0 then Exit;
  X := PtrInt(ListBox.Items.Objects[ListBox.ItemIndex]);
  DebugLn(['TListBoxTestForm.Button4Click ',X]);
end;

procedure TListBoxTestForm.FormResize(Sender: TObject);
begin
  Caption := Format('%dx%d', [ListBox.Width, ListBox.Height]);
end;

end.

