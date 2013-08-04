{**********************************************************************
 This file is originally part of CodeTyphon Studio (http://www.pilotlogic.com/)
 Copied to Lazarus and modified.
***********************************************************************}

unit CompPagesPopup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  strutils, ExtCtrls, Buttons, MainBar;

type

  { TDlgCompPagesPopup }

  TDlgCompPagesPopup = class(TForm)
    cBtnClose: TSpeedButton;
    ImageList1: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    TreeView1: TTreeView;
    procedure cBtnCloseClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    fPopupItemIndy,
    fPopupItemGLScene,
    fPopupItemCindy,
    fPopupItemExtra,
    fPopupItemBGRA,
    fPopupItemDCP,
    fPopupItemACS,
    fPopupItemASIOVST,
    fPopupItemVirtual,
    fPopupItemSCADA,
    fPopupItemFZControls,
    fPopupItemLuiControls,
    fPopupItemShapes,
    fPopupItemRX     :TTreeNode;
  public
    procedure FixBounds;
    procedure ClearList;
    procedure BuildList;
  end;

var
  DlgCompPagesPopup: TDlgCompPagesPopup;


implementation

{$R *.lfm}

{ TDlgCompPagesPopup }

procedure TDlgCompPagesPopup.FormShow(Sender: TObject);
begin
  BuildList;
end;

procedure TDlgCompPagesPopup.FormDeactivate(Sender: TObject);
begin
  close;
end;

procedure TDlgCompPagesPopup.cBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TDlgCompPagesPopup.FixBounds;
begin
  if (self.Height+100)>screen.Height then
     self.Height:=screen.Height-self.Top-100 else
     self.Height:=Round(2*screen.Height/3) - self.Top;

  if (self.Left+self.Width+50)>screen.Width then
      self.Left:=self.Left-(self.Width div 2)+10;

  if self.Height<400 then self.Height:=400;
end;

//---------------------------------------------------------------
procedure TDlgCompPagesPopup.ClearList;
begin
  TreeView1.Items.Clear;
  fPopupItemIndy:=nil;
  fPopupItemGLScene:=nil;
  fPopupItemCindy:=nil;
  fPopupItemExtra:=nil;
  fPopupItemBGRA:=nil;
  fPopupItemDCP:=nil;
  fPopupItemACS:=nil;
  fPopupItemASIOVST:=nil;
  fPopupItemVirtual:=nil;
  fPopupItemSCADA:=nil;
  fPopupItemRX:=nil;
  fPopupItemFZControls:=nil;
  fPopupItemLuiControls:=nil;
  fPopupItemShapes:=nil;
end;

//---------------------------------------------------------------

procedure TDlgCompPagesPopup.TreeView1Click(Sender: TObject);
var
  id:integer;

  procedure _findPage(const aname:string);
  var
    ix:integer;
  begin
   id:=-1;
   if MainIDEBar.ComponentPageControl=nil then exit;
   if MainIDEBar.ComponentPageControl.PageCount=0 then exit;

   for ix:=0 to MainIDEBar.ComponentPageControl.PageCount-1 do
     if SameText(aname,MainIDEBar.ComponentPageControl.Page[ix].Caption) then
     begin
       id:=ix;
       exit;
     end;
  end;

begin
  if TreeView1.Selected=nil then exit;
  if TreeView1.Selected.ImageIndex=1 then exit;

  _findPage(TreeView1.Selected.Text);
  if id>-1 then
    MainIDEBar.ComponentPageControl.PageIndex:=id;

  Close;
end;

procedure TDlgCompPagesPopup.BuildList;
var
  isSubItem: boolean;

  procedure AddGroup(var aGroupNode: TTreeNode; aGroupTitle, aPageTitle: string);
  var
    PageNode: TTreeNode;
  begin
    if aGroupNode=nil then
    begin
      aGroupNode:=TreeView1.Items.AddChild(nil, aGroupTitle);
      aGroupNode.ImageIndex:=1;
      aGroupNode.SelectedIndex:=1;
    end;
    PageNode:=TreeView1.Items.AddChild(aGroupNode, aPageTitle);
    PageNode.ImageIndex:=0;
    PageNode.SelectedIndex:=0;
    isSubItem:=true;
  end;

var
  i: integer;
  Node: TTreeNode;
  PageCapt: string;
begin
  ClearList;
  TreeView1.BeginUpdate;

  if MainIDEBar.ComponentPageControl=nil then
  begin
    Node:=TreeView1.Items.AddChild(nil,'Sorry, NO Pages');
    Exit;
  end;

  for i:=0 to MainIDEBar.ComponentPageControl.PageCount-1 do
  begin
    isSubItem:=false;
    PageCapt:=MainIDEBar.ComponentPageControl.Page[i].Caption;

 //====================================================================
    if AnsiStartsText('Indy', PageCapt) then
      AddGroup(fPopupItemIndy, 'Indy pages', PageCapt)
    else if AnsiStartsText('GLScene', PageCapt) then
      AddGroup(fPopupItemGLScene, 'GLScene pages', PageCapt)
    else if AnsiStartsText('Cindy', PageCapt) then
      AddGroup(fPopupItemCindy, 'Cindy pages', PageCapt)
    else if AnsiStartsText('Extra', PageCapt) then
      AddGroup(fPopupItemExtra, 'Extra pages', PageCapt)
    else if AnsiStartsText('BGRA', PageCapt) then
      AddGroup(fPopupItemBGRA, 'BGRA pages', PageCapt)
    else if AnsiStartsText('DCP', PageCapt) then
      AddGroup(fPopupItemDCP, 'DCP pages', PageCapt)
    else if AnsiStartsText('RX', PageCapt) then
      AddGroup(fPopupItemRX, 'RX pages', PageCapt)
    else if AnsiStartsText('ACS', PageCapt) then
      AddGroup(fPopupItemACS, 'Audio ACS pages', PageCapt)
    else if AnsiStartsText('ASIO/VST', PageCapt) then
      AddGroup(fPopupItemASIOVST, 'Audio ASIO-VST pages', PageCapt)
    else if AnsiStartsText('Virtual Controls', PageCapt) then
      AddGroup(fPopupItemVirtual, 'Virtual Controls pages', PageCapt)
    else if AnsiStartsText('PascalSCADA', PageCapt) then
      AddGroup(fPopupItemSCADA, 'PascalSCADA pages', PageCapt)
    else if AnsiStartsText('FZControls', PageCapt) then
      AddGroup(fPopupItemFZControls, 'FZControls pages', PageCapt)
    else if AnsiStartsText('LuiControls', PageCapt) then
      AddGroup(fPopupItemLuiControls, 'LuiControls pages', PageCapt)
    else if AnsiStartsText('Shapes', PageCapt) then
      AddGroup(fPopupItemShapes, 'Shapes pages', PageCapt)
    ;
//====================================================================

    if not isSubItem then
    begin
      Node:=TreeView1.Items.AddChild(nil,PageCapt);
      Node.ImageIndex:=0;
      Node.SelectedIndex:=0;
    end;
  end;
  TreeView1.EndUpdate;
  TreeView1.FullExpand;
  //......
  Panel2.Caption:='Total Pages: '+IntToStr(MainIDEBar.ComponentPageControl.PageCount);
end;

end.

