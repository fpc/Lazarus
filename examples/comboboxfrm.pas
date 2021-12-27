unit ComboBoxFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    itmFile: TMenuItem;
    itmFileQuit: TMenuItem;
    mnuMain: TMainMenu;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure ComboOnChange(Sender: TObject);
    procedure ComboOnClick(Sender: TObject);
    procedure itmFileQuitClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Assigned(ComboBox1) and Assigned(Edit1) then
    ComboBox1.Text := Edit1.Text;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ComboBox1.Items.Add(Edit1.Text);
  ComboBox2.Items.Add(Edit1.Text);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Assigned(ComboBox1) and Assigned(Edit1)  then
    Edit1.Text := ComboBox1.Text;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if Assigned(ComboBox1)  then
    ComboBox1.Enabled := not ComboBox1.Enabled;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  i: integer;
begin
  if Assigned(ComboBox1) then
  begin
    i := 0;
    while i < ComboBox1.Items.Count do
    begin
      if Assigned(Memo1) then
        Memo1.Lines.Add(ComboBox1.Items[i]);
      inc (i); 
    end;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  s: string;
begin
  if Assigned(ComboBox1) then
  begin
    s := Format ('%x', [ComboBox1.ItemIndex]);
    if Assigned(Memo1) then
      Memo1.Lines.Add (s);
  end;
end; 

procedure TForm1.Button7Click(Sender: TObject);
begin
  Edit1.SelectAll;
  ComboBox1.SelectAll;  
end; 

procedure TForm1.ComboOnChange(Sender: TObject);
var
  s: string;
begin
  if Sender is TEdit then
    s := TControl(Sender).Name + ':TEdit - '
  else if Sender is TComboBox then
    s := TControl(Sender).Name + ':TComboBox - '
  else
    s := 'UNKNOWN';
  if Assigned(Memo1) then
    Memo1.Lines.Add(s + 'OnChange');
end; 

procedure TForm1.ComboOnClick(Sender: TObject);
begin
  if Assigned(Memo1) then
    Memo1.Lines.Add('OnClick');
end;   

procedure TForm1.itmFileQuitClick(Sender: TObject);
begin
  Close;
end;

end.

