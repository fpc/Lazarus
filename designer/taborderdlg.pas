unit TabOrderDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ComCtrls, StdCtrls;

type
  TTabOrderDialog = class(TForm)
    ShowOldValuesCheckbox: TCHECKBOX;
    Imagelist1: TIMAGELIST;
    OkButton: TBUTTON;
    CancelButton: TBUTTON;
    ItemTreeview: TTREEVIEW;
    UpSpeedbutton: TSPEEDBUTTON;
    DownSpeedbutton: TSPEEDBUTTON;
    procedure DownSpeedbuttonCLICK(Sender: TObject);
    procedure OkButtonCLICK(Sender: TObject);
    procedure ShowOldValuesCheckboxCLICK(Sender: TObject);
    procedure TabOrderDialogCLOSE(Sender: TObject; var Action: TCloseAction);
    procedure TabOrderDialogCREATE(Sender: TObject);
    procedure UpSpeedbuttonCLICK(Sender: TObject);
  private
    FLookupRoot: TComponent;
    procedure SetLookupRoot(const AValue: TComponent);
    procedure CommitNodes(ANode: TTreeNode; var TabChanged: boolean);
    procedure CreateNodes(ParentControl: TWinControl; ParentNode: TTreeNode);
  public
    procedure FillTree;
    procedure ClearTree;
  public
    property LookupRoot: TComponent read FLookupRoot write SetLookupRoot;
  end;

function ShowTabOrderDialog(LookupRoot: TComponent): TModalresult;

implementation

function ShowTabOrderDialog(LookupRoot: TComponent): TModalresult;
var
  TabOrderDialog: TTabOrderDialog;
begin
  TabOrderDialog:=TTabOrderDialog.Create(Application);
  TabOrderDialog.LookupRoot:=LookupRoot;
  Result:=TabOrderDialog.ShowModal;
  TabOrderDialog.Free;
end;

{ TTabOrderDialog }

procedure TTabOrderDialog.TabOrderDialogCREATE(Sender: TObject);
begin
  OkButton.Caption:='Ok';
  CancelButton.Caption:='Cancel';
  ShowOldValuesCheckbox.Caption:='Show old tab order';
  UpSpeedbutton.Glyph:=TPixmap.Create;
  UpSpeedbutton.Glyph.LoadFromLazarusResource('uparrow');
  DownSpeedbutton.Glyph:=TPixmap.Create;
  DownSpeedbutton.Glyph.LoadFromLazarusResource('downarrow');
end;

procedure TTabOrderDialog.UpSpeedbuttonCLICK(Sender: TObject);
var
  CurItem: TTreeNode;
begin
  CurItem:=ItemTreeview.Selected;
  if (CurItem=nil) or (CurItem.GetPrevSibling=nil) then exit;
  CurItem.MoveTo(CurItem.GetPrevSibling,naInsert);
  ItemTreeview.Selected:=CurItem;
end;

procedure TTabOrderDialog.TabOrderDialogCLOSE(Sender: TObject;
  var Action: TCloseAction);
begin
  FLookupRoot:=nil;
end;

procedure TTabOrderDialog.ShowOldValuesCheckboxCLICK(Sender: TObject);
begin
  FillTree;
end;

procedure TTabOrderDialog.DownSpeedbuttonCLICK(Sender: TObject);
var
  CurItem: TTreeNode;
begin
  CurItem:=ItemTreeview.Selected;
  if (CurItem=nil) or (CurItem.GetNextSibling=nil) then exit;
  CurItem.MoveTo(CurItem.GetNextSibling,naInsertBehind);
  ItemTreeview.Selected:=CurItem;
end;

procedure TTabOrderDialog.OkButtonCLICK(Sender: TObject);
var
  TabChanged: Boolean;
begin
  TabChanged:=false;
  CommitNodes(ItemTreeview.Items.GetFirstNode,TabChanged);
  if TabChanged then
    ModalResult:=mrOk
  else
    ModalResult:=mrCancel;
end;

procedure TTabOrderDialog.SetLookupRoot(const AValue: TComponent);
begin
  if FLookupRoot=AValue then exit;
  FLookupRoot:=AValue;
  if FLookupRoot<>nil then begin
    Caption:='Tab Order of '+FLookupRoot.Name;
  end;
  FillTree;
end;

procedure TTabOrderDialog.CommitNodes(ANode: TTreeNode;
  var TabChanged: boolean);
var
  AControl: TControl;
  CurTabOrder: Integer;
begin
  CurTabOrder:=0;
  while ANode<>nil do begin
    AControl:=TControl(ANode.Data);
    if AControl.TabStop then begin
      if AControl.TabOrder<>CurTabOrder then
        TabChanged:=true;
      AControl.TabOrder:=CurTabOrder;
      writeln('TTabOrderDialog.CommitNodes A ',AControl.Name,' ',AControl.TabOrder,' ',CurTabOrder);
      inc(CurTabOrder);
    end;
    CommitNodes(ANode.GetFirstChild,TabChanged);
    ANode:=ANode.GetNextSibling;
  end;
end;

procedure TTabOrderDialog.FillTree;
var
  AControl: TWinControl;
begin
  ItemTreeview.BeginUpdate;
  try
    ClearTree;
    if (FLookupRoot=nil) or (not (FLookupRoot is TWinControl)) then exit;
    AControl:=TWinControl(FLookupRoot);
    CreateNodes(AControl,nil);
  finally
    ItemTreeview.EndUpdate;
  end;
end;

procedure TTabOrderDialog.ClearTree;
begin
  ItemTreeview.Items.Clear;
end;

procedure TTabOrderDialog.CreateNodes(ParentControl: TWinControl;
  ParentNode: TTreeNode);
var
  i: Integer;
  AControl: TControl;
  CurTab: Integer;
  FirstSibling: TTreeNode;
  NodeBehind: TTreeNode;
  NewNode: TTreeNode;
  NodeText: String;
begin
  if ParentNode=nil then
    FirstSibling:=nil
  else
    FirstSibling:=ParentNode.GetFirstChild;
  for i:=0 to ParentControl.ControlCount-1 do begin
    AControl:=ParentControl.Controls[i];
    if not (AControl is TWinControl) then continue;
    CurTab:=AControl.TabOrder;
    NodeBehind:=FirstSibling;
    while (NodeBehind<>nil) and (TControl(NodeBehind.Data).TabOrder<=CurTab)
    do
      NodeBehind:=NodeBehind.GetNextSibling;
    NodeText:=AControl.Name;
    if ShowOldValuesCheckbox.Checked then
      NodeText:=NodeText+'   ('+IntToStr(AControl.TabOrder)+')';
    if NodeBehind<>nil then
      NewNode:=ItemTreeview.Items.InsertObject(NodeBehind,NodeText,AControl)
    else
      NewNode:=ItemTreeview.Items.AddChildObject(ParentNode,NodeText,AControl);
    if FirstSibling=nil then
      FirstSibling:=NewNode;
    if AControl is TWinControl then
      CreateNodes(TWinControl(AControl),NewNode);
    NewNode.Expanded:=true;
  end;
end;

initialization
  {$I taborderdlg.lrs}

end.

