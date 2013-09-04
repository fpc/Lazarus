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
    fGroups: TStringList;   // Objects have group TreeNodes
    procedure FindGroups;
    procedure BuildTreeItem(aPageCapt: string);
    procedure BuildList;
  public
    procedure FixBounds;
  end;

var
  DlgCompPagesPopup: TDlgCompPagesPopup;


implementation

{$R *.lfm}

function FirstWord(aStr: string): string;
var
  spPos: integer;
begin
  spPos := Pos(' ', aStr);
  if spPos > 0 then
    Result := Copy(aStr, 1, spPos-1)
  else
    Result := '';
end;

{ TDlgCompPagesPopup }

procedure TDlgCompPagesPopup.FormShow(Sender: TObject);
begin
  BuildList;
end;

procedure TDlgCompPagesPopup.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TDlgCompPagesPopup.cBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TDlgCompPagesPopup.FixBounds;
begin
  if (self.Height+100)>screen.Height then
    self.Height:=screen.Height-self.Top-100
  else
    self.Height:=Round(2*screen.Height/3) - self.Top;

  if (self.Left+self.Width+50)>screen.Width then
    self.Left:=self.Left-(self.Width div 2)+10;

  if self.Height<400 then
    self.Height:=400;
end;

procedure TDlgCompPagesPopup.TreeView1Click(Sender: TObject);
var
  i: integer;
begin
  if (TreeView1.Selected=nil) or (TreeView1.Selected.ImageIndex=1) then exit;
  with MainIDEBar do
    if Assigned(ComponentPageControl) and (ComponentPageControl.PageCount>0) then
      for i:=0 to ComponentPageControl.PageCount-1 do
        if SameText(TreeView1.Selected.Text, ComponentPageControl.Page[i].Caption) then
        begin
          ComponentPageControl.PageIndex:=i;
          Break;
        end;
  Close;
end;

procedure TDlgCompPagesPopup.FindGroups;
// Find groups. Page names with many words are grouped by the first word.
var
  i, grInd: integer;
  Word1: string;
begin
  for i:=0 to MainIDEBar.ComponentPageControl.PageCount-1 do
  begin
    Word1 := FirstWord(MainIDEBar.ComponentPageControl.Page[i].Caption);
    if (Word1 <> '') and (Word1 <> 'Data') then  // "Data" is an exception
    begin
      grInd := fGroups.IndexOf(Word1);
      if grInd > -1 then // Found, mark as group. TreeNode will be created later.
        fGroups.Objects[grInd] := TObject(0)
      else               // Will be a group only if other members are found.
        fGroups.AddObject(Word1, TObject(1));   // "1" means a single item now.
    end;
  end;
  // Delete single items (marked with "1") from groups list.
  for i := fGroups.Count-1 downto 0 do
    if Assigned(fGroups.Objects[i]) then
      fGroups.Delete(i);
end;

procedure TDlgCompPagesPopup.BuildTreeItem(aPageCapt: string);
// Create items in tree, grouping as needed.
var
  grInd: integer;
  Word1: string;
  GroupNode, ItemNode: TTreeNode;
begin
  GroupNode := Nil;
  Word1 := FirstWord(aPageCapt);
  if Word1 <> '' then
  begin
    grInd := fGroups.IndexOf(Word1);
    if grInd > -1 then    // Group found
    begin
      if Assigned(fGroups.Objects[grInd]) then
        GroupNode := TTreeNode(fGroups.Objects[grInd])
      else begin
        GroupNode := TreeView1.Items.AddChild(nil, Word1+' pages');
        fGroups.Objects[grInd] := GroupNode;
      end;
    end;
  end;
  ItemNode:=TreeView1.Items.AddChild(GroupNode, aPageCapt);
  ItemNode.ImageIndex:=0;
  ItemNode.SelectedIndex:=0;
end;

procedure TDlgCompPagesPopup.BuildList;
var
  i: integer;
  Node: TTreeNode;
begin
  TreeView1.Items.Clear;
  TreeView1.BeginUpdate;
  if MainIDEBar.ComponentPageControl=nil then
  begin
    Node:=TreeView1.Items.AddChild(nil,'Sorry, NO Pages');
    Exit;
  end;
  fGroups := TStringList.Create;
  try
    FindGroups;
    for i:=0 to MainIDEBar.ComponentPageControl.PageCount-1 do
      BuildTreeItem(MainIDEBar.ComponentPageControl.Page[i].Caption);
  finally
    fGroups.Free;
  end;
  TreeView1.EndUpdate;
  TreeView1.FullExpand;
  Panel2.Caption:='Total Pages: '+IntToStr(MainIDEBar.ComponentPageControl.PageCount);
end;

end.

