{

}
{$H+}
unit test_unit;

{$mode objfpc}

interface

uses
  Classes, StdCtrls, Forms, Buttons, Menus, ComCtrls, SysUtils, ExtCtrls,
  ObjectInspector, PropEdits, Graphics;

type
  TMyEnum = (MyEnum1,MyEnum2,MyEnum3);
  TMySet = set of TMyEnum;

	TForm1 = class(TFORM)
	public
	  Label1 : TLabel;
	  Label2 : TLabel;
	  Label3 : TLabel;
	  EditToComboButton: TButton;
	  AddItemButton: TButton;
	  ComboToEditButton: TButton;
	  SwitchEnabledButton: TButton;
	  DumpButton: TButton;
	  IndexButton: TButton;
    OIResizeButton: TButton;
    OIRefreshButton: TButton;
	  Edit1 : TEdit;
	  mnuMain: TMainMenu;
	  itmFileQuit: TMenuItem;
	  itmFile: TMenuItem;
    ComboBox1 : TComboBox;
    ComboBox2 : TComboBox;
    Memo1     : TMemo;
    constructor Create(AOwner: TComponent); override;	
    procedure LoadMainMenu;
	  procedure FormKill(Sender : TObject);
	  procedure FormShow(Sender : TObject);
	  procedure mnuQuitClicked(Sender : TObject);
	protected
	  procedure EditToComboButtonCLick(Sender : TObject);
	  procedure AddItemButtonCLick(Sender : TObject);
	  procedure ComboToEditButtonCLick(Sender : TObject);
	  procedure SwitchEnabledButtonCLick(Sender : TObject);
	  procedure DumpButtonCLick(Sender : TObject);
	  procedure IndexButtonCLick(Sender : TObject);
	  procedure OIResizeButtonCLick(Sender : TObject);
	  procedure OIRefreshButtonCLick(Sender : TObject);
	  procedure ComboOnChange (Sender:TObject);
	  procedure ComboOnClick (Sender:TObject);
  public
    FMyInteger:integer;
    FMyCardinal:Cardinal;
    FMyEnum:TMyEnum;
    FMySet:TMySet;
    FMyFont:TFont;
    FMyString:string;
    FMyBool:boolean;
    FMyBrush:TBrush;
    FMyPen:TPen;
  published
    property MyInteger:integer read FMyInteger write FMyInteger;
    property MyCardinal:cardinal read FMyCardinal write FMyCardinal;
    property MyEnum:TMyEnum read FMyEnum write FMyEnum;
    property MySet:TMySet read FMySet write FMySet;
    property MyFont:TFont read FMyFont write FMyFont;
    property MyString:string read FMyString write FMyString;
    property MyBool:Boolean read FMyBool write FMyBool;
    property MyBrush:TBrush read FMyBrush write FMyBrush;
    property MyPen:TPen read FMyPen write FMyPen;
	end;

var
  Form1 : TForm1;
  OI: TObjectInspector;

implementation


constructor TForm1.Create(AOwner: TComponent);	
begin
  inherited Create(AOwner);
  FMySet:=[MyEnum1];
  FMyEnum:=MyEnum2;
  FMyFont:=TFont.Create;
  FMyBrush:=TBrush.Create;
  FMyPen:=TPen.Create;

  Name:='Form1';
  Caption := 'Test Form';
  OI:=nil;
  OnShow:=@FormShow;
  LoadMainMenu;
  Left:=250;
  Top:=50;
  if OI=nil then begin
    OI:=TObjectInspector.Create(Application);
    OI.Name:='OI';
    OI.SetBounds(7,50,220,700);
    OI.Show;
    OI.RootComponent:=Self;
  end;
end;

procedure TForm1.FormKill(Sender : TObject);
Begin
  FMyBrush.Free;
  FMyPen.Free;
  FMyFont.Free;
  Application.terminate;
End;

procedure TForm1.FormShow(Sender: TObject);
begin
end;

procedure TForm1.OIResizeButtonClick(Sender : TObject);
begin
  OI.DoInnerResize;
end;

procedure TForm1.OIRefreshButtonClick(Sender : TObject);
begin
  OI.RefreshSelections;
end;

procedure TForm1.EditToComboButtonClick(Sender : TObject);
Begin
  if assigned (ComboBox1) and assigned (edit1) then
    ComboBox1.Text := edit1.text;
End;

procedure TForm1.AddItemButtonClick(Sender : TObject);
Begin
   if assigned (ComboBox1) 
      then Combobox1.Items.Add ('item ' + IntToStr (comboBox1.Items.Count));
   if assigned (ComboBox2) 
      then Combobox2.Items.Add ('item ' + IntToStr (comboBox2.Items.Count));
End;

procedure TForm1.ComboToEditButtonClick(Sender : TObject);
Begin
   if assigned (ComboBox1) and assigned (edit1) 
      then edit1.Text := ComboBox1.Text;
End;

procedure TForm1.SwitchEnabledButtonClick(Sender : TObject);
Begin
   if assigned (ComboBox1) 
      then ComboBox1.Enabled := not ComboBox1.Enabled;
End;

procedure TForm1.DumpButtonClick(Sender : TObject);
var
   i : integer;
Begin
   if assigned (ComboBox1) then
   begin
      i := 0;
      while i < ComboBox1.Items.Count do
      begin
         if assigned (Memo1)
            then Memo1.Lines.Add (ComboBox1.Items[i]);
	 inc (i); 
      end;
   end;
End;

procedure TForm1.IndexButtonClick(Sender : TObject);
var
   s : shortstring;
Begin
   if assigned (ComboBox1) then
   begin
      s := Format ('%x', [ComboBox1.ItemIndex]);
      if assigned (Memo1)
         then Memo1.Lines.Add (s);
   end;
End;

procedure TForm1.ComboOnChange (Sender:TObject);
var
   s : shortstring;	
begin
   if sender is TEdit 
      then s := 'TEdit'
   else if sender is TComboBox
      then s := 'TComboBox'
   else
      s := 'UNKNOWN';
   if assigned (Memo1)
      then Memo1.Lines.Add (s + 'ONChange');
end;

procedure TForm1.ComboOnClick (Sender:TObject);
begin
   if assigned (Memo1)
      then Memo1.Lines.Add ('ONClick');
end;

{------------------------------------------------------------------------------}
procedure TForm1.LoadMainMenu;

begin
   OnDestroy := @FormKill;

   { set the height and width }
   Height := 400;
   Width := 700;

   OIResizeButton:=TButton.Create(Self);
   with OIResizeButton do begin
     Name:='OIResizeButton';
     Parent:=Self;
     SetBounds(200,10,100,40);
     Caption:='Resize OI';
     OnClick:=@OIResizeButtonClick;
     Show;
   end;

   OIRefreshButton:=TButton.Create(Self);
   with OIRefreshButton do begin
     Name:='OIRefreshButton';
     Parent:=Self;
     SetBounds(200,60,100,40);
     Caption:='Refresh OI';
     OnClick:=@OIRefreshButtonClick;
     Show;
   end;

   { Create 2 buttons inside the groupbox }
   EditToComboButton := TButton.Create(Self);
   EditToComboButton.Name:='EditToComboButton';
   EditToComboButton.Parent := Self;
   EditToComboButton.Left := 50;
   EditToComboButton.Top := 80;
   EditToComboButton.Width := 120;
   EditToComboButton.Height := 30;
   EditToComboButton.Show;
   EditToComboButton.Caption := 'Edit->Combo';
   EditToComboButton.OnClick := @EditToComboButtonClick;

   AddItemButton := TButton.Create(Self);
   AddItemButton.Name:='AddItemButton';
   AddItemButton.Parent := Self;
   AddItemButton.Left := 50;
   AddItemButton.Top := 40;
   AddItemButton.Width := 120;
   AddItemButton.Height := 30;
   AddItemButton.Show;
   AddItemButton.Caption := 'Add item';
   AddItemButton.OnClick := @AddItemButtonClick;

   { Create 2 more buttons outside the groupbox }
   ComboToEditButton := TButton.Create(Self);
   ComboToEditButton.Name:='ComboToEditButton';
   ComboToEditButton.Parent := Self;
   ComboToEditButton.Left := 50;
   ComboToEditButton.Top := 120;
   ComboToEditButton.Width := 120;
   ComboToEditButton.Height := 30;
   ComboToEditButton.Show;
   ComboToEditButton.Caption := 'Combo->Edit';
   ComboToEditButton.OnClick := @ComboToEditButtonClick;


   SwitchEnabledButton := TButton.Create(Self);
   SwitchEnabledButton.Name:='SwitchEnabledButton';
   SwitchEnabledButton.Parent := Self;
   SwitchEnabledButton.Left := 50;
   SwitchEnabledButton.Top := 160;
   SwitchEnabledButton.Width := 120;
   SwitchEnabledButton.Height := 30;
   SwitchEnabledButton.Show;
   SwitchEnabledButton.Caption := 'Enabled On/Off';
   SwitchEnabledButton.OnClick := @SwitchEnabledButtonClick;

   DumpButton := TButton.Create(Self);
   DumpButton.Name:='DumpButton';
   DumpButton.Parent := Self;
   DumpButton.Left := 50;
   DumpButton.Top := 200;
   DumpButton.Width := 120;
   DumpButton.Height := 30;
   DumpButton.Show;
   DumpButton.Caption := 'Dump';
   DumpButton.OnClick := @DumpButtonClick;

   IndexButton := TButton.Create(Self);
   IndexButton.Name:='IndexButton';
   IndexButton.Parent := Self;
   IndexButton.Left := 50;
   IndexButton.Top := 240;
   IndexButton.Width := 120;
   IndexButton.Height := 30;
   IndexButton.Show;
   IndexButton.Caption := 'Index ?';
   IndexButton.OnClick := @IndexButtonClick;


   { Create a label for the edit field }
   label1 := TLabel.Create(Self);
   Label1.Name:='Label1';
   label1.Parent := self;
   label1.top	 := 50;
   label1.left	 := 320;
   label1.Height := 20;
   label1.Width  := 130;
   label1.Show;
   label1.Caption := 'TEdit :';


   Edit1 := TEdit.Create (self);
   with Edit1 do begin
      Name   := 'Edit1';
      Parent := self;
      Left   := 500;
      Top    := 50;
      Width  := 70;
      Height := 20;
      OnChange := @ComboOnChange;	
      OnClick  := @ComboOnClick;	
      Show;	
   end;

   { Create a label for the 1st combobox }
   label2 := TLabel.Create(Self);
   Label2.Name:='Label2';
   label2.Parent := self;
   label2.top	 := 100;
   label2.left	 := 320;
   label2.Height := 20;
   label2.Width  := 130;
   label2.Enabled:= true;
   label2.Show;
   label2.Caption := 'Combo (unsorted)';
   label2.Enabled:= true;


   { Create the menu now }
   { WARNING: If you do it after creation of the combo, the menu will not 
     appear. Reason is unknown by now!!!!!!}
   mnuMain := TMainMenu.Create(Self);
   mnuMain.Name:='mnuMain';
   Menu := mnuMain;
   itmFile := TMenuItem.Create(Self);
   itmFile.Name:='itmFile';
   itmFile.Caption := '&File';
   mnuMain.Items.Add(itmFile);
   itmFileQuit := TMenuItem.Create(Self);
   itmFileQuit.Name:='itmFileQuit';
   itmFileQuit.Caption := '&Quit';
   itmFileQuit.OnClick := @mnuQuitClicked;
   itmFile.Add(itmFileQuit);

   ComboBox1 := TComboBox.Create (self);
   with ComboBox1 do
   begin
     Name:='ComboBox1';
     Parent := self;
     Left := 500;
     Top	:= 100;
     Width := 170;
     Height := 20;
     Style := csDropDown;
     Items.Add ('wohhh!');
     Items.Add ('22222!');
     ItemIndex := 1;
     Items.Add ('33333!');
     Items.Add ('abcde!');
     OnChange := @ComboOnChange;
     OnClick  := @ComboOnClick;
     Show;
   end;


   { Create a label for the 2nd combobox }
   label3 := TLabel.Create(Self);
   Label3.Name:='Label3';
   label3.Parent := self;
   label3.top	 := 150;
   label3.left	 := 320;
   label3.Height := 20;
   label3.Width  := 130;
   label3.Show;
   label3.Caption := 'Combo (sorted)';


   ComboBox2 := TComboBox.Create (self);
   with ComboBox2 do begin
     Name:='ComboBox2';
     Parent := self;
     Left := 500;
     Top	:= 150;
     Width := 170;
     Height := 20;
     Items.Add ('wohhh!');
     Items.Add ('22222!');
     ItemIndex := 1;
     Items.Add ('33333!');
     Items.Add ('abcde!');
     Sorted := true;
     Show;
   end;

   Memo1 := TMemo.Create(Self);
   with Memo1 do begin
     Memo1.Name:='Memo1';
     Parent := Self;
     Scrollbars := ssBoth;
     Left := 200;
     Top := 200;
     Width := 335;
     Height := 155;
     Show;
   end;
end;

{------------------------------------------------------------------------------}
procedure TForm1.mnuQuitClicked(Sender : TObject);
begin
  Application.Terminate;
end;
{------------------------------------------------------------------------------}


end.
