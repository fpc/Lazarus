unit ButtonTest;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  customdrawncontrols, customdrawndrawers;

type

  { TFormButtons }

  TFormButtons = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FButton1: TCDButton;
    FButton2: TCDButton;
    FButton3: TCDButton;

    FCheckbox1: TCDCheckbox;
    FCheckbox2: TCDCheckbox;
    FCheckbox3: TCDCheckbox;

    FRadioButton1: TCDRadioButton;
    FRadioButton2: TCDRadioButton;
    FRadioButton3: TCDRadioButton;

  public

  end;

var
  FormButtons: TFormButtons;

implementation

{$R *.lfm}

uses
  MainForm;

{ TFormButtons }

procedure TFormButtons.FormCreate(Sender: TObject);
begin
  FButton1 := TCDButton.Create(self);
  FButton1.Caption := 'Button1';
  FButton1.Parent := self;
  FButton1.Left := 10;
  FButton1.Top := 10;

  FButton2 := TCDButton.Create(self);
  FButton2.Caption := 'Button2';
  FButton2.Parent := self;
  FButton2.Left := FButton1.Left + FButton1.Width + 40;
  FButton2.Top := 10;

  FButton3 := TCDButton.Create(self);
  FButton3.Caption := 'Button3';
  FButton3.Parent := self;
  FButton3.Left := FButton2.Left + FButton2.Width + 40;
  FButton3.Top := 10;
  FButton3.Enabled := false;

  // -------------------

  FCheckbox1 := TCDCheckbox.Create(self);
  FCheckbox1.Caption := 'Checkbox 1';
  FCheckbox1.Parent := Self;
  FCheckbox1.Left := FButton1.Left;
  FCheckbox1.Top := FButton1.Top + FButton1.Height + 20;
  FCheckbox1.Checked;

  FCheckbox2 := TCDCheckbox.Create(self);
  FCheckbox2.Caption := 'Checkbox 1';
  FCheckbox2.Parent := Self;
  FCheckbox2.Left := FButton2.Left;
  FCheckbox2.Top := FCheckbox1.Top;

  FCheckbox3 := TCDCheckbox.Create(self);
  FCheckbox3.Caption := 'Checkbox 3';
  FCheckbox3.Parent := Self;
  FCheckbox3.Left := FButton3.Left;
  FCheckbox3.Top := FCheckbox1.Top;
  FCheckbox3.Enabled := False;

  // ---------------

  FRadioButton1 := TCDRadioButton.Create(self);
  FRadioButton1.Caption := 'RadioButton 1';
  FRadioButton1.Parent := Self;
  FRadioButton1.Left := FButton1.Left;
  FRadioButton1.Top := FCheckbox1.Top + FButton1.Height + 10;
  FRadioButton1.Checked := true;

  FRadioButton2 := TCDRadioButton.Create(self);
  FRadioButton2.Caption := 'RadioButton 2';
  FRadioButton2.Parent := Self;
  FRadioButton2.Left := FButton2.Left;
  FRadioButton2.Top := FRadioButton1.Top;
  FRadioButton1.Checked := false;

  FRadioButton3 := TCDRadioButton.Create(self);
  FRadioButton3.Caption := 'RadioButton 3';
  FRadioButton3.Parent := Self;
  FRadioButton3.Left := FButton3.Left;
  FRadioButton3.Top := FRadioButton1.Top;
  FRadioButton3.Enabled := False;

end;

procedure TFormButtons.FormShow(Sender: TObject);
begin
  FButton1.DrawStyle := TCDDrawStyle(formCDControlsTest.comboDrawer.ItemIndex);
  FButton2.DrawStyle := TCDDrawStyle(formCDControlsTest.comboDrawer.ItemIndex);
  FButton3.DrawStyle := TCDDrawStyle(formCDControlsTest.comboDrawer.ItemIndex);

  FCheckbox1.DrawStyle := TCDDrawStyle(formCDControlsTest.comboDrawer.ItemIndex);
  FCheckbox2.DrawStyle := TCDDrawStyle(formCDControlsTest.comboDrawer.ItemIndex);
  FCheckbox3.DrawStyle := TCDDrawStyle(formCDControlsTest.comboDrawer.ItemIndex);

  FRadioButton1.DrawStyle := TCDDrawStyle(formCDControlsTest.comboDrawer.ItemIndex);
  FRadioButton2.DrawStyle := TCDDrawStyle(formCDControlsTest.comboDrawer.ItemIndex);
  FRadioButton3.DrawStyle := TCDDrawStyle(formCDControlsTest.comboDrawer.ItemIndex);
end;

end.

