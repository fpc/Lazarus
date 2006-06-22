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

  LCL Test 4_2

  Test for TPageControl.
}
program test4_2pagecontrol;

{$mode objfpc}{$H+}

uses
  Interfaces, FPCAdds, LCLProc, LCLType, Classes, Controls, Forms, TypInfo,
  LMessages, Buttons, ExtCtrls, ComCtrls, Graphics, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    PagesListBox: TListBox;
    ButtonsGroupBox: TGroupBox;
    InsertPageLeftButton: TButton;
    InsertPageRightButton: TButton;
    DeletePageButton: TButton;
    MovePageLeftButton: TButton;
    MovePageRightButton: TButton;
    PageVisibleCheckBox: TCheckBox;
    procedure DeletePageButtonClick(Sender: TObject);
    procedure Form1Create(Sender: TObject);
    procedure InsertPageLeftButtonClick(Sender: TObject);
    procedure InsertPageRightButtonClick(Sender: TObject);
    procedure MovePageLeftButtonClick(Sender: TObject);
    procedure MovePageRightButtonClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PageVisibleCheckBoxClick(Sender: TObject);
    procedure PagesListBoxClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AddNewPage(Index: integer);
    procedure FillPagesListBox;
  end;

{ TForm1 }

procedure TForm1.Form1Create(Sender: TObject);
begin
  debugln('TForm1.Form1Create ',DbgSName(Sender));
  SetBounds(50,50,500,400);

  PageControl1:=TPageControl.Create(Self);
  with PageControl1 do begin
    Name:='PageControl1';
    Align:=alTop;
    Height:=200;
    Parent:=Self;
    OnChange:=@PageControl1Change;
  end;

  PagesListBox:=TListBox.Create(Self);
  with PagesListBox do begin
    Name:='PagesListBox';
    Align:=alLeft;
    Parent:=Self;
    OnClick:=@PagesListBoxClick;
  end;

  ButtonsGroupBox:=TGroupBox.Create(Self);
  with ButtonsGroupBox do begin
    Name:='ButtonsGroupBox';
    Align:=alClient;
    Parent:=Self;
  end;
  
  InsertPageLeftButton:=TButton.Create(Self);
  with InsertPageLeftButton do begin
    Name:='InsertPageLeftButton';
    Caption:='Insert page left';
    AutoSize:=true;
    Parent:=ButtonsGroupBox;
    OnClick:=@InsertPageLeftButtonClick;
  end;
  
  InsertPageRightButton:=TButton.Create(Self);
  with InsertPageRightButton do begin
    Name:='InsertPageRightButton';
    Caption:='Insert page right';
    AutoSize:=true;
    Parent:=ButtonsGroupBox;
    OnClick:=@InsertPageRightButtonClick;
  end;

  MovePageLeftButton:=TButton.Create(Self);
  with MovePageLeftButton do begin
    Name:='MovePageLeftButton';
    Caption:='move page left';
    AutoSize:=true;
    Parent:=ButtonsGroupBox;
    OnClick:=@MovePageLeftButtonClick;
  end;

  MovePageRightButton:=TButton.Create(Self);
  with MovePageRightButton do begin
    Name:='MovePageRightButton';
    Caption:='move page right';
    AutoSize:=true;
    Parent:=ButtonsGroupBox;
    OnClick:=@MovePageRightButtonClick;
  end;

  DeletePageButton:=TButton.Create(Self);
  with DeletePageButton do begin
    Name:='DeletePageButton';
    Caption:='Delete page';
    AutoSize:=true;
    Parent:=ButtonsGroupBox;
    OnClick:=@DeletePageButtonClick;
  end;

  PageVisibleCheckBox:=TCheckBox.Create(Self);
  with PageVisibleCheckBox do begin
    Name:='PageVisibleCheckBox';
    Caption:='Visible';
    AutoSize:=true;
    Parent:=ButtonsGroupBox;
    OnClick:=@PageVisibleCheckBoxClick;
  end;
  
  ButtonsGroupBox.ChildSizing.ControlsPerLine:=2;
  ButtonsGroupBox.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
end;

procedure TForm1.DeletePageButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=PagesListBox.ItemIndex;
  if (i<0) or (i>=PageControl1.PageCount) then exit;
  PageControl1.Pages[i].Free;
  FillPagesListBox;
end;

procedure TForm1.InsertPageLeftButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=PagesListBox.ItemIndex;
  if (i<0) or (i>=PageControl1.PageCount) then i:=PageControl1.PageIndex;
  AddNewPage(i);
end;

procedure TForm1.InsertPageRightButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  if PageControl1.PageCount=0 then
    AddNewPage(0)
  else begin
    i:=PagesListBox.ItemIndex;
    if (i<0) or (i>=PageControl1.PageCount) then i:=PageControl1.PageIndex;
    AddNewPage(i+1);
  end;
end;

procedure TForm1.MovePageLeftButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=PagesListBox.ItemIndex;
  if (i<0) or (i>=PageControl1.PageCount) then i:=PageControl1.PageIndex;
  if i<=0 then exit;
  PageControl1.Pages[i].PageIndex:=PageControl1.Pages[i].PageIndex-1;
  FillPagesListBox;
  PagesListBox.ItemIndex:=PageControl1.PageIndex;
end;

procedure TForm1.MovePageRightButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=PagesListBox.ItemIndex;
  if (i<0) or (i>=PageControl1.PageCount) then i:=PageControl1.PageIndex;
  if i>=PageControl1.PageCount-1 then exit;
  PageControl1.Pages[i].PageIndex:=PageControl1.Pages[i].PageIndex+1;
  FillPagesListBox;
  PagesListBox.ItemIndex:=PageControl1.PageIndex;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  FillPagesListBox;
  PagesListBox.ItemIndex:=PageControl1.PageIndex;
  DebugLn(['TForm1.PageControl1Change PagesListBox.ItemIndex=',PagesListBox.ItemIndex,' PageControl1.PageIndex=',PageControl1.PageIndex]);
end;

procedure TForm1.PageVisibleCheckBoxClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=PagesListBox.ItemIndex;
  if i<0 then exit;
  PageControl1.Pages[i].TabVisible:=PageVisibleCheckBox.Checked;
end;

procedure TForm1.PagesListBoxClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=PagesListBox.ItemIndex;
  if i<0 then exit;
  PageControl1.PageIndex:=i;
  PageVisibleCheckBox.Checked:=PageControl1.Pages[i].TabVisible;
end;

constructor TForm1.Create(TheOwner: TComponent);
var
  i: Integer;
begin
  OnCreate:=@Form1Create;
  inherited Create(TheOwner);
  // start with 3 pages
  for i:=0 to 2 do AddNewPage(i);
  FillPagesListBox;
end;

procedure TForm1.AddNewPage(Index: integer);
var
  NewPage: TTabSheet;
  NewName: String;
begin
  NewPage:=TTabSheet.Create(Self);
  NewName:='Page1';
  while FindComponent(NewName)<>nil do NewName:=CreateNextIdentifier(NewName);
  NewPage.Name:=NewName;
  NewPage.Caption:=NewName;
  PageControl1.PageList.Insert(Index,NewPage);
  PageControl1.PageIndex:=Index;
  FillPagesListBox;
  PagesListBox.ItemIndex:=PageControl1.PageIndex;
end;

procedure TForm1.FillPagesListBox;
var
  i: Integer;
  OldItemIndex: LongInt;
begin
  PagesListBox.Items.BeginUpdate;
  OldItemIndex:=PagesListBox.ItemIndex;
  for i:=0 to PageControl1.PageCount-1 do begin
    if PagesListBox.Items.Count>i then begin
      PagesListBox.Items[i]:=PageControl1.Pages[i].Name;
    end else begin
      PagesListBox.Items.Add(PageControl1.Pages[i].Name);
    end;
  end;
  while (PagesListBox.Items.Count>PageControl1.PageCount) do
    PagesListBox.Items.Delete(PagesListBox.Items.Count-1);
  PagesListBox.ItemIndex:=OldItemIndex;
  PagesListBox.Items.EndUpdate;
  if PagesListBox.ItemIndex>=0 then
    PageVisibleCheckBox.Checked:=
      PageControl1.Pages[PagesListBox.ItemIndex].TabVisible;
end;

var
  Form1: TForm1 = nil;
begin
  Application.Title:='test4_1synedit';
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Application.Run;
end.

