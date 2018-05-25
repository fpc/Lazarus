{
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
*   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
*                                                                         *
***************************************************************************

  Abstract:
    A drop-down list with all component tab names.
    Allows selection of any tab, including the ones not visible in tab control.
    This list is opened from a button at the right edge of component palette.

    This file is originally part of CodeTyphon Studio (http://www.pilotlogic.com/).
    They improved Lazarus GPL code with this feature, then it was copied
    and backported to Lazarus. Later it was modified with a different button etc.
}
unit CompPagesPopup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, LMessages,
  Dialogs, ComCtrls, ExtCtrls, Buttons, LCLIntf,
  IDEImagesIntf,
  LazarusIDEStrConsts, MainBar, ComponentPalette_Options, MainBase;

type

  { TDlgCompPagesPopup }

  TDlgCompPagesPopup = class(TForm)
    cBtnClose: TSpeedButton;
    Panel1: TPanel;
    Panel2: TPanel;
    TreeView1: TTreeView;
    procedure cBtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    fViewAllNode, fOptionsNode: TTreeNode;
    fGroups: TStringList;   // Objects have group TreeNodes
    fLastCloseUp: QWord;
    fLastCanShowCheck: Boolean;
    procedure FindGroups;
    procedure BuildTreeItem(aPageCapt: string);
    procedure BuildList;
  protected
    procedure WMActivate(var Message : TLMActivate); message LM_ACTIVATE;

    procedure DoCreate; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    procedure FixBounds;
    procedure CanShowCheck;
    property LastCanShowCheck: Boolean read fLastCanShowCheck;
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

procedure TDlgCompPagesPopup.FormCreate(Sender: TObject);
begin
  IDEImages.AssignImage(cBtnClose, 'menu_close');

  TreeView1.Images := IDEImages.Images_16;
  {TIDEImages.AddImageToImageList(ImageList1, 'item_package');
  TIDEImages.AddImageToImageList(ImageList1, 'pkg_open');}
end;

procedure TDlgCompPagesPopup.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);

  if CloseAction = caHide then
    fLastCloseUp := GetTickCount64;
end;

procedure TDlgCompPagesPopup.DoCreate;
begin
  inherited DoCreate;

  fLastCanShowCheck := True;
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
  SelNode: TTreeNode;
begin
  SelNode:=TreeView1.Selected;
  if (SelNode=nil) or (SelNode.ImageIndex=1) then exit;
  if SelNode=fViewAllNode then
    MainIDE.DoShowComponentList
  else if SelNode=fOptionsNode then
    MainIDE.DoOpenIDEOptions(TCompPaletteOptionsFrame, '', [], [])
  else with MainIDEBar do
    if Assigned(ComponentPageControl) and (ComponentPageControl.PageCount>0) then
      for i:=0 to ComponentPageControl.PageCount-1 do
        if SameText(SelNode.Text, ComponentPageControl.Page[i].Caption) then
        begin
          ComponentPageControl.PageIndex:=i;
          ComponentPageControl.OnChange(Self);
          Break;
        end;
  Close;
end;

procedure TDlgCompPagesPopup.WMActivate(var Message: TLMActivate);
begin
  {$IFDEF LCLWin32}
  //activate the mainform to simulate a true popup-window (works only on Windows)
  if Assigned(PopupParent) and PopupParent.HandleAllocated then
    SendMessage(PopupParent.Handle, LM_NCACTIVATE, Ord(Message.Active <> WA_INACTIVE), 0);
  {$ENDIF}

  inherited WMActivate(Message);
end;

procedure TDlgCompPagesPopup.FindGroups;
// Find groups. Page names with many words are grouped by the first word.
var
  i, grpIndex: integer;
  Word1: string;
begin
  for i:=0 to MainIDEBar.ComponentPageControl.PageCount-1 do
  begin
    Word1 := FirstWord(MainIDEBar.ComponentPageControl.Page[i].Caption);
    if (Word1 <> '') and (Word1 <> 'Data') then  // "Data" is an exception
    begin
      grpIndex := fGroups.IndexOf(Word1);
      if grpIndex > -1 then // Found, mark as group. TreeNode will be created later.
        fGroups.Objects[grpIndex] := nil
      else               // Will be a group only if other members are found.
        fGroups.AddObject(Word1, Self);   // <>nil means a single item now.
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
  ItemNode.ImageIndex:=IDEImages.GetImageIndex('item_package');
  ItemNode.SelectedIndex:=0;
end;

procedure TDlgCompPagesPopup.CanShowCheck;
begin
  fLastCanShowCheck := not Visible and (GetTickCount64 > fLastCloseUp + 100);
end;

procedure TDlgCompPagesPopup.BuildList;
var
  i: integer;
begin
  TreeView1.BeginUpdate;
  TreeView1.Items.Clear;
  fViewAllNode:=nil;
  fOptionsNode:=nil;
  if MainIDEBar.ComponentPageControl=nil then
  begin
    TreeView1.Items.AddChild(nil,'Sorry, No Pages');
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

  // add 'View all'
  fViewAllNode:=TreeView1.Items.AddChild(nil, lisCompPalComponentList);
  fViewAllNode.ImageIndex:=IDEImages.GetImageIndex('item_package');
  fViewAllNode.SelectedIndex:=fViewAllNode.ImageIndex;

  // add 'Options'
  fOptionsNode:=TreeView1.Items.AddChild(nil, lisOptions);
  fOptionsNode.ImageIndex:=IDEImages.LoadImage('menu_environment_options');
  fOptionsNode.SelectedIndex:=fOptionsNode.ImageIndex;

  TreeView1.EndUpdate;
  TreeView1.FullExpand;
  Panel2.Caption:=Format(lisTotalPages,
                         [IntToStr(MainIDEBar.ComponentPageControl.PageCount)]);
end;

end.

