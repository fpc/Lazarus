{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  LCL Test 3_1

  ComboBox:
    OnSelect - called when user click on an item on the popup menu.
               Not called when ItemIndex is set.
    OnChange - called when user changes text.
               Not called when ItemIndex is set.
               Not called when user clicks on an item on the popup menu.
}
program Test3_1comboboxselect;

{$mode objfpc}{$H+}

uses
  Interfaces, FPCAdds, LCLProc, LCLType, Classes, Controls, Forms, TypInfo,
  LMessages, StdCtrls, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure ComboBox1CloseUp(Sender: TObject);
    procedure ComboBox1DropDown(Sender: TObject);
    procedure ComboBox1EditingDone(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
  public
    ComboBox1: TComboBox;
    Button1: TButton;
    constructor Create(TheOwner: TComponent); override;
  end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  NewItemIndex: Integer;
begin
  DebugLn(['TForm1.Button1Click START ComboBox1.ItemIndex=',ComboBox1.ItemIndex]);
  NewItemIndex:=ComboBox1.ItemIndex+1;
  if NewItemIndex>=ComboBox1.Items.Count then
    NewItemIndex:=0;
  ComboBox1.ItemIndex:=NewItemIndex;
  DebugLn(['TForm1.Button1Click END ComboBox1.ItemIndex=',ComboBox1.ItemIndex]);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  DebugLn(['TForm1.ComboBox1Change ItemIndex=',ComboBox1.ItemIndex]);
end;

procedure TForm1.ComboBox1Click(Sender: TObject);
begin
  DebugLn(['TForm1.ComboBox1Click ItemIndex=',ComboBox1.ItemIndex]);
end;

procedure TForm1.ComboBox1CloseUp(Sender: TObject);
begin
  DebugLn(['TForm1.ComboBox1CloseUp ItemIndex=',ComboBox1.ItemIndex]);
end;

procedure TForm1.ComboBox1DropDown(Sender: TObject);
begin
  DebugLn(['TForm1.ComboBox1DropDown ItemIndex=',ComboBox1.ItemIndex]);
end;

procedure TForm1.ComboBox1EditingDone(Sender: TObject);
begin
  DebugLn(['TForm1.ComboBox1EditingDone ItemIndex=',ComboBox1.ItemIndex]);
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  DebugLn(['TForm1.ComboBox1Select ItemIndex=',ComboBox1.ItemIndex]);
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  
  Name:='Form1';
  Caption:='Title Form1';
  SetBounds(100,90,350,200);
  
  ComboBox1:=TComboBox.Create(Self);
  with ComboBox1 do begin
    Name:='ComboBox1';
    SetBounds(10,10,Width,Height);
    Parent:=Self;
    OnChange:=@ComboBox1Change;
    OnClick:=@ComboBox1Click;
    OnSelect:=@ComboBox1Select;
    OnCloseUp:=@ComboBox1CloseUp;
    OnDropDown:=@ComboBox1DropDown;
    OnEditingDone:=@ComboBox1EditingDone;
    Items.Add('First');
    Items.Add('Second');
    Items.Add('Third');
  end;
  
  Button1:=TButton.Create(Self);
  with Button1 do begin
    Name:='Button1';
    SetBounds(10,40,200,Height);
    Caption:='Change ItemIndex';
    Parent:=Self;
    OnClick:=@Button1Click;
  end;
end;

var
  Form1: TForm1 = nil;
begin
  Application.Title:='test1_1simpleform1';
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  debugln('Form1.Bounds=',dbgs(Form1.BoundsRect));
  Application.Run;
end.

