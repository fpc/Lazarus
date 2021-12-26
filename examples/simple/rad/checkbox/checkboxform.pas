unit CheckBoxForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, LazLogger,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Menus;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    Button3: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    itmFileQuit: TMenuItem;
    mnuMain: TMainMenu;
    itmFile: TMenuItem;
    Panel1: TPanel;
    Button11: TSpeedButton;
    Button12: TSpeedButton;
    Button13: TSpeedButton;
    RadioButton: TRadioButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioGroup: TRadioGroup;
    RadioGroup2: TRadioGroup;
    ToggleBox: TToggleBox;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure itmFileQuitClick(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
    procedure RadioGroupClick(Sender: TObject);
    procedure ToggleBoxClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  DebugLn('[TForm1.CheckBoxClick]');
  if Assigned (CheckBox1) and Assigned(Label1) then 
  begin
    DebugLn ('   [checkbox and label assigned]');
    if CheckBox1.Checked then
      Label1.Caption := 'checked'
    else 
      Label1.Caption := 'unchecked';
    
    if CheckBox1.Checked then
      CheckBox1.Caption := 'new caption'
    else 
      CheckBox1.Caption := 'Checkbox 1';
   end;
end;

procedure TForm1.itmFileQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Assigned(CheckBox1) then
    CheckBox1.Checked := not CheckBox1.Checked; 
  Panel1.Caption := 'Changed';
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(RadioGroup) then
  begin
    i := RadioGroup.ItemIndex;
    if i < RadioGroup.Items.Count - 1  then
      RadioGroup.ItemIndex := i + 1
    else 
      RadioGroup.ItemIndex := 0
  end;
  
  if Assigned(RadioGroup2) then
  begin
    i := RadioGroup2.ItemIndex;
    if i < RadioGroup2.Items.Count -1 then
      RadioGroup2.ItemIndex := i + 1
    else 
      RadioGroup2.ItemIndex := 0
  end; 
end;

procedure TForm1.RadioButtonClick(Sender: TObject);
begin
  Debugln('[TForm1.RadioButtonClick]');
  if Assigned(Label2) then
    Label2.Caption := 'active: ' + TRadioButton(Sender).Caption;
end;

procedure TForm1.RadioGroupClick(Sender: TObject);
begin
  Debugln('[TForm1.RadioGroupClick]');
end;

procedure TForm1.ToggleBoxClick(Sender: TObject);
begin
  DebugLn('[TForm1.ToggleBoxClick]');
  if Assigned(ToggleBox) then
  begin
    if ToggleBox.Checked then
      ToggleBox.Caption := 'Togglebox1'
    else 
      ToggleBox.Caption := 'does nothing:-(';
  end;
end;

end.

