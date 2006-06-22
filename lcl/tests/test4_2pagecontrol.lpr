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
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AddNewPage(Index: integer);
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
  end;

  PagesListBox:=TListBox.Create(Self);
  with PagesListBox do begin
    Name:='PagesListBox';
    Align:=alLeft;
    Parent:=Self;
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
  end;
  
  ButtonsGroupBox.ChildSizing.ControlsPerLine:=2;
  ButtonsGroupBox.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
end;

procedure TForm1.DeletePageButtonClick(Sender: TObject);
begin
  if PageControl1.PageCount=0 then exit;
  PageControl1.PageList.Delete(PageControl1.PageIndex);
end;

procedure TForm1.InsertPageLeftButtonClick(Sender: TObject);
begin
  AddNewPage(PageControl1.PageIndex);
end;

procedure TForm1.InsertPageRightButtonClick(Sender: TObject);
begin
  if PageControl1.PageCount=0 then
    AddNewPage(0)
  else
    AddNewPage(PageControl1.PageIndex+1);
end;

procedure TForm1.MovePageLeftButtonClick(Sender: TObject);
begin
  if PageControl1.PageIndex=0 then exit;
  PageControl1.PageList.Move(PageControl1.PageIndex,PageControl1.PageIndex-1);
end;

procedure TForm1.MovePageRightButtonClick(Sender: TObject);
begin
  if PageControl1.PageIndex<PageControl1.PageCount-1 then exit;
  PageControl1.PageList.Move(PageControl1.PageIndex,PageControl1.PageIndex+1);
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  OnCreate:=@Form1Create;
  inherited Create(TheOwner);
  AddNewPage(0);
  AddNewPage(1);
  AddNewPage(2);
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
end;

var
  Form1: TForm1 = nil;
begin
  Application.Title:='test4_1synedit';
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Application.Run;
end.

