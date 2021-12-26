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

{    set the height and width }
//   Height := 450;
//   Width := 700;

   { Create a checkbox }
(*   CheckBox1 := TCheckBox.Create(Self);
   CheckBox1.Parent := self;
   CheckBox1.top := 35;
   CheckBox1.left := 10;
   CheckBox1.Height :=20;
   CheckBox1.Width := 200;
   CheckBox1.OnCLick := @CheckBoxClick;
//   CheckBox1.Show;
   CheckBox1.Caption := 'Checkbox 1';
  
   { Create a label which shows the state checked/unchecked of the checkbox}
   label1 := TLabel.Create(Self);
   label1.Parent := self;
   label1.top := 35;
   label1.left := 220;
   label1.Height :=20;
   label1.Width := 100;
//   label1.Show;
   label1.Caption := 'unchecked';
                
   { Sample panel here }
   Panel1:= TPanel.Create(Self);
   with Panel1 do begin
     Parent := Self;
     Left := 400;
     Top := 60;
     Width := 180;
     Height := 81;
     Alignment:= taRightJustify;
     BevelInner:= bvLowered;
     BevelOuter:= bvRaised;
     BorderWidth:= 4;
     BevelWidth:= 4;
//     Show;
     Caption:= 'Hello world';
   end;     
          
   { Create a button which toggles the checkbox }
   Button2 := TButton.Create(Self);
   Button2.Parent := Self; //Panel1;
   Button2.Left := Panel1.Left; //5;
   Button2.Top := Checkbox1.Top; //45;
   Button2.Width := 180;
   Button2.Height := 20;
//   Button2.Show;
   Button2.Caption := 'Toggle checkbox';
   Button2.OnClick := @Button2Click;
                       
   Button11 := TSpeedButton.Create(Self);
   Button11.GroupIndex:= 1;
   Button11.Layout:= blGlyphTop;
//   Button11.Caption:= 'Option 1';
   Button11.Parent := Self;
   Button11.Left := 5;
   Button11.Top := 5;
   Button11.Width:= 45;
//   Button11.Height:= 55;
   Button11.Flat:= true;
   BtnGlyph := TPortableNetworkGraphic.Create;
   try
     BtnGlyph.LoadFromFile('../../../../images/items/item_form.png');
     Button11.Glyph.Assign(BtnGlyph);
   finally
     BtnGlyph.Free;
   end;
        
//   Button11.Spacing:= -1;
//   Button11.Margin:= 4;
//   Button11.Visible:= true;
   
   Button12 := TSpeedButton.Create(Self);
   Button12.GroupIndex:= 1;
   Button12.Caption:= '2';
   Button12.Parent := Self;
   Button12.Left := 55;
   Button12.Top := 5;
//   Button12.Flat:= true;
//   Button12.Visible:= true;

   Button13 := TSpeedButton.Create(Self);
   Button13.GroupIndex:= 1;
   Button13.Caption:= '3';
   Button13.Parent := Self;
   Button13.Left := 95;
   Button13.Top := 5;
   Button13.Flat:= true;
   Button13.AllowAllUp:= true;
//   Button13.Visible:= true;
                

   { Create a label which shows the caption of the active radiobutton }
   label2 := TLabel.Create(Self);
   label2.Parent := self;
   label2.top := 90;
   label2.left := 220;
   label2.Height :=20;
   label2.Width := 200;
//   label2.Show;
   label2.Caption := 'active: unknown';

{ Create a radio button }
RadioButton := TRadioButton.Create(Self);
RadioButton.Parent := self;
RadioButton.top := 70;
RadioButton.left := 10;
RadioButton.Height :=20;
RadioButton.Width := 200;
RadioButton.OnCLick := @RadioButtonClick;
RadioButton.Checked := false;  
//   RadioButton.Show;
RadioButton.Caption := 'Radio button 1';
             
{ Create a 2nd radiobutton }
RadioButton2 := TRadioButton.Create(Self);
with RadioButton2 do
begin
  Parent := self;
  top := 90;
  left := 10;
  Height :=20;
  Width := 200;
  OnCLick := @RadioButtonClick;
  Checked := true; 
//     Show;
  Caption := 'Radiobutton 2'
end;

{ Create a 3rd radiobutton }
RadioButton3 := TRadioButton.Create(Self);
with RadioButton3 do
begin
  Parent := self;
  top := 110;
  left := 10;
  Height :=20;
  Width := 200;
  OnCLick := @RadioButtonClick;
  Checked := false;  
//     Show;
  Caption := 'Radiobutton 3'
end;
          
{ Create a radiogroup }
RadioGroup := TRadioGroup.Create(Self);
with RadioGroup do
begin
  Parent := self;
  top := 200;
  left := 10;
  Height :=200;
  Width := 150;
  OnCLick := @RadioGroupClick;
  RadioGroup.Items.Add ('No 1');
  RadioGroup.Items.Add ('No 2');
  RadioGroup.Items.Add ('No 3');
  RadioGroup.Items.Add ('No 4');
  RadioGroup.Items.Add ('No 5');
  RadioGroup.Items.Add ('No 6');
  ItemIndex := 3;
//     Show;
  Caption := 'Radiogroup';
end;

{ Create a button which does mystic things with the radiogroup }
Button3 := TButton.Create(Self);
Button3.Parent := Self;
Button3.Left := 220;
Button3.Top := 220;
Button3.Width := 180;
Button3.Height := 30;
//   Button3.Show;
Button3.Caption := 'Mystic Radiogroups';
Button3.OnClick := @Button3Click;
                
{ Create a radiogroup }
RadioGroup2 := TRadioGroup.Create(Self);
with RadioGroup2 do
begin
  Parent := self;
  top     := 300;
  left    := 220;
  Height  := 100;
  Width   := 300;
  Columns := 3;
  Items.Add ('No 1');
  Items.Add ('No 2');
  Items.Add ('No 3');
  Items.Add ('No 4');
  Items.Add ('No 5');
  Items.Add ('No 6');
  ItemIndex := 1;
//     Show;
  Caption := '3 columns';
end;             

{ Create a togglebox }
ToggleBox := TToggleBox.Create(Self);
with ToggleBox do
begin
  Parent := self;
  top := 150;
  left := 10;
  Height :=30;
  Width := 240;
  OnCLick := @ToggleBoxClick;
//     Show;
  Caption := 'ToggleBox 1'
end;          *)

mnuMain := TMainMenu.Create(Self);
Menu := mnuMain;

itmFile := TMenuItem.Create(Self);
itmFile.Caption := 'File';
mnuMain.Items.Add(itmFile);

itmFileQuit := TMenuItem.Create(Self);
itmFileQuit.Caption := 'Quit';
itmFileQuit.OnClick := @mnuQuitClicked;
itmFile.Add(itmFileQuit);

