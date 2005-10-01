{
 /***************************************************************************
                               combobox.pp  
                              -------------
                   Example/test program for combobox usage in lcl


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
program ComboBox;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, StdCtrls, Forms, Buttons, Menus, ComCtrls,
  SysUtils, Extctrls, Controls;

type
  TForm1 = class(TFORM)
  public
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Edit1: TEdit;
    mnuMain: TMainMenu;
    itmFileQuit: TMenuItem;
    itmFile: TMenuItem;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Memo1: TMemo;
    constructor Create(AOwner: TComponent); override;
    procedure LoadMainMenu;
    procedure FormKill(Sender: TObject);
    procedure mnuQuitClicked(Sender: TObject);
  protected
    procedure Button1CLick(Sender: TObject);
    procedure Button2CLick(Sender: TObject);
    procedure Button3CLick(Sender: TObject);
    procedure Button4CLick(Sender: TObject);
    procedure Button5CLick(Sender: TObject);
    procedure Button6CLick(Sender: TObject);
    procedure Button7CLick(Sender: TObject);
    procedure ComboOnChange(Sender: TObject);
    procedure ComboOnClick(Sender: TObject);
  end;

var
  Form1 : TForm1;

constructor TForm1.Create(AOwner: TComponent);	
begin
   inherited Create(AOwner);
   Caption := 'ComboBox Demo v 0.1';
   LoadMainMenu;
end;

procedure TForm1.Button1Click(Sender : TObject);
Begin
   if assigned (ComboBox1) and assigned (edit1)
      then ComboBox1.Text := edit1.text;
End;

procedure TForm1.Button2Click(Sender : TObject);
Begin
{   if assigned (ComboBox1) 
      then Combobox1.Items.Add ('item ' + IntToStr (comboBox1.Items.Count));
   if assigned (ComboBox2) 
      then Combobox2.Items.Add ('item ' + IntToStr (comboBox2.Items.Count));}
  ComboBox1.Items.Add(Edit1.Text);
  ComboBox2.Items.Add(Edit1.Text);
End;

procedure TForm1.Button3Click(Sender : TObject);
Begin
   if assigned (ComboBox1) and assigned (edit1) 
      then edit1.Text := ComboBox1.Text;
End;

procedure TForm1.Button4Click(Sender : TObject);
Begin
   if assigned (ComboBox1) 
      then ComboBox1.Enabled := not ComboBox1.Enabled;
End;

procedure TForm1.Button5Click(Sender : TObject);
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

procedure TForm1.Button6Click(Sender : TObject);
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

procedure TForm1.Button7Click(Sender : TObject);
begin
  Edit1.SelectAll;
  ComboBox1.SelectAll;
end;


procedure TForm1.ComboOnChange (Sender:TObject);
var
   s : shortstring;	
begin
   if sender is TEdit 
      then s := TCOntrol(Sender).name+':TEdit - '
   else if sender is TComboBox
      then s := TControl(Sender).name+':TComboBox - '
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

procedure TForm1.FormKill(Sender : TObject);
Begin

End;

{------------------------------------------------------------------------------}
procedure TForm1.LoadMainMenu;

begin
   OnDestroy := @FormKill;

   { set the height and width }
   Height := 350;
   Width := 700;

   { Create 2 buttons inside the groupbox }
   Button2 := TButton.Create(Self);
   Button2.Parent := Self;
   Button2.Left := 50;
   Button2.Top := 40;
   Button2.Width := 120;
   Button2.Height := 30;
   Button2.Show;
   Button2.Caption := 'Add item';
   Button2.OnClick := @Button2Click;



   Button1 := TButton.Create(Self);
   Button1.Parent := Self;
   Button1.Left := 50;
   Button1.Top := 80;
   Button1.Width := 120;
   Button1.Height := 30;
   Button1.Show;
   Button1.Caption := 'Edit->Combo';
   Button1.OnClick := @Button1Click;


   { Create 2 more buttons outside the groupbox }
   Button3 := TButton.Create(Self);
   Button3.Parent := Self;
   Button3.Left := 50;
   Button3.Top := 120;
   Button3.Width := 120;
   Button3.Height := 30;
   Button3.Show;
   Button3.Caption := 'Combo->Edit';
   Button3.OnClick := @Button3Click;


   Button4 := TButton.Create(Self);
   Button4.Parent := Self;
   Button4.Left := 50;
   Button4.Top := 160;
   Button4.Width := 120;
   Button4.Height := 30;
   Button4.Show;
   Button4.Caption := 'Enabled On/Off';
   Button4.OnClick := @Button4Click;

   Button5 := TButton.Create(Self);
   Button5.Parent := Self;
   Button5.Left := 50;
   Button5.Top := 200;
   Button5.Width := 120;
   Button5.Height := 30;
   Button5.Show;
   Button5.Caption := 'Dump';
   Button5.OnClick := @Button5Click;

   Button6 := TButton.Create(Self);
   Button6.Parent := Self;
   Button6.Left := 50;
   Button6.Top := 240;
   Button6.Width := 120;
   Button6.Height := 30;
   Button6.Show;
   Button6.Caption := 'Index ?';
   Button6.OnClick := @Button6Click;

   Button7 := TButton.Create(Self);
   Button7.Parent := Self;
   Button7.Left := 50;
   Button7.Top := 280;
   Button7.Width := 120;
   Button7.Height := 30;
   Button7.Show;
   Button7.Caption := 'Select All';
   Button7.OnClick := @Button7Click;


   { Create a label for the edit field }
   label1 := TLabel.Create(Self);
   label1.Parent := self;
   label1.top	 := 50;
   label1.left	 := 320;
   label1.Height := 20;
   label1.Width  := 130;
   label1.Show;
   label1.Caption := 'TEdit :';


   Edit1 := TEdit.Create (self);
   with Edit1 do
   begin
      Parent := self;
      Left   := 500;
      Top    := 50;
      Width  := 70;
      Height := 20;
      OnChange := @ComboOnChange;	
      OnClick  := @ComboOnClick;
      Name := 'Edit1';	
      Show;	
   end;


   { Create a label for the 1st combobox }
   label2 := TLabel.Create(Self);
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
   Menu := mnuMain;
   itmFile := TMenuItem.Create(Self);
   itmFile.Caption := '&File';
   mnuMain.Items.Add(itmFile);
   itmFileQuit := TMenuItem.Create(Self);
   itmFileQuit.Caption := '&Quit';
   itmFileQuit.OnClick := @mnuQuitClicked;
   itmFile.Add(itmFileQuit);

   ComboBox1 := TComboBox.Create (self);
   with ComboBox1 do
   begin
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
      Name := 'ComboBox1';	
      Show;	
   end;


   { Create a label for the 2nd combobox }
   label3 := TLabel.Create(Self);
   label3.Parent := self;
   label3.top	 := 150;
   label3.left	 := 320;
   label3.Height := 20;
   label3.Width  := 130;
   label3.Show;
   label3.Caption := 'Combo (sorted)';


   ComboBox2 := TComboBox.Create (self);
   with ComboBox2 do
   begin
      Parent := self;
      Left := 500;
      Top	:= 150;
      Width := 170;
      Height := 20;
      Style := csDropDownList;
      Items.Add ('wohhh!');
      Items.Add ('22222!');
      ItemIndex := 1;
      Items.Add ('33333!');
      Items.Add ('abcde!');
      Name := 'ComboBox2';	
      OnChange := @ComboOnChange;
      OnClick  := @ComboOnClick;
      Sorted := true;
      Show;	
   end;


   Memo1 := TMemo.Create(Self);
   with Memo1 do
   begin
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
  Close;
end;
{------------------------------------------------------------------------------}

begin
   Application.Initialize; { calls InitProcedure which starts up GTK }
   Application.CreateForm(TForm1, Form1);
   Application.Run;
end.

