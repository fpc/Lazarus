{ TSmallOrderedSetEditDlg

  Copyright (C) 2017 Lazarus team

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.

Abstract:
  Dialog to edit a set of items (string) and able to order them.
}
unit SmallOrderedSetEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, math, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, ComCtrls, Buttons, LCLType, Themes, LazLoggerBase;

type
  TSmOrdSetEditOption = (
    oseoHideUpDown,
    oseoErrorDuplicateItems,     // default: ignore and skip
    oseoErrorDuplicateAvailable, // default: ignore and skip
    oseoErrorItemsContainNotAvailable // default: merge Items into AvailableItems
  );
  TSmOrdSetEditOptions = set of TSmOrdSetEditOption;

  { TSmallOrderedSetEditDlg }

  TSmallOrderedSetEditDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    HeaderLabel: TLabel;
    ImageList1: TImageList;
    ItemsTreeView: TTreeView;
    MoveDownBitBtn: TBitBtn;
    MoveUpBitBtn: TBitBtn;
    procedure ItemsTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; {%H-}State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, {%H-}DefaultDraw: Boolean);
    procedure ItemsTreeViewMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure ItemsTreeViewSelectionChanged(Sender: TObject);
    procedure MoveDownBitBtnClick(Sender: TObject);
    procedure MoveUpBitBtnClick(Sender: TObject);
  private
    FAvailableItems: TStrings;
    FItems: TStrings;
    FOptions: TSmOrdSetEditOptions;
    function GetHeaderCaption: TTranslateString;
    procedure SetAvailableItems(const AValue: TStrings);
    procedure SetHeaderCaption(const AValue: TTranslateString);
    procedure SetItems(const AValue: TStrings);
    function SetList(List, NewList: TStrings; ErrorOnDuplicate: boolean): boolean;
    procedure UpdateButtonState;
  protected
    procedure SetOptions(const AValue: TSmOrdSetEditOptions); virtual;
    function IndexOf(List: TStrings; Value: string): integer; virtual;
    procedure UpdateShowing; override;
    procedure ToggleNode(Node: TTreeNode); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; virtual;
    property Options: TSmOrdSetEditOptions read FOptions write SetOptions;
    property HeaderCaption: TTranslateString read GetHeaderCaption write SetHeaderCaption;
    property Items: TStrings read FItems write SetItems;
    property AvailableItems: TStrings read FAvailableItems write SetAvailableItems;
  end;

function CreateOrderedSetEditor(Items, AvailableItems: TStrings): TSmallOrderedSetEditDlg;
function ShowOrderedSetEditor(aCaption: string; Items, AvailableItems: TStrings): TModalResult;

implementation

function CreateOrderedSetEditor(Items, AvailableItems: TStrings): TSmallOrderedSetEditDlg;
begin
  Result:=TSmallOrderedSetEditDlg.Create(nil);
  Result.Items:=Items;
  Result.AvailableItems:=AvailableItems;
  Result.Init;
end;

function ShowOrderedSetEditor(aCaption: string; Items, AvailableItems: TStrings
  ): TModalResult;
var
  Dlg: TSmallOrderedSetEditDlg;
begin
  Dlg:=CreateOrderedSetEditor(Items,AvailableItems);
  try
    Dlg.Caption:=aCaption;
    Result:=Dlg.ShowModal;
    if Result=mrOK then
      Items.Assign(Dlg.Items);
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TSmallOrderedSetEditDlg }

procedure TSmallOrderedSetEditDlg.ItemsTreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  Selected: Boolean;
  R: TRect;
  Details: TThemedElementDetails;
begin
  Selected:=Node.ImageIndex>0;
  if Stage=cdPrePaint then
    PaintImages:=false
  else if Stage=cdPostPaint then
  begin
    R:=Node.DisplayRect(false);
    R.Left := Node.DisplayIconLeft;
    if Selected then
      Details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal)
    else
      Details := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
    R.Right:=R.Left+ThemeServices.GetDetailSize(Details).cx;
    ThemeServices.DrawElement(ItemsTreeView.Canvas.Handle, Details, R, nil);
  end;
end;

procedure TSmallOrderedSetEditDlg.ItemsTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  Node:=ItemsTreeView.GetNodeAt(X,Y);
  if Node=nil then exit;
  ToggleNode(Node);
end;

procedure TSmallOrderedSetEditDlg.ItemsTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TSmallOrderedSetEditDlg.MoveDownBitBtnClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:=ItemsTreeView.Selected;
  if (SelNode=nil) or (SelNode.Index>=ItemsTreeView.Items.TopLvlCount-1) then exit;
  SelNode.Index:=SelNode.Index+1;
  UpdateButtonState;
end;

procedure TSmallOrderedSetEditDlg.MoveUpBitBtnClick(Sender: TObject);
var
  SelNode: TTreeNode;
begin
  SelNode:=ItemsTreeView.Selected;
  if (SelNode=nil) or (SelNode.Index<1) then exit;
  SelNode.Index:=SelNode.Index-1;
  UpdateButtonState;
end;

function TSmallOrderedSetEditDlg.GetHeaderCaption: TTranslateString;
begin
  Result:=HeaderLabel.Caption;
end;

procedure TSmallOrderedSetEditDlg.SetAvailableItems(const AValue: TStrings);
begin
  SetList(FAvailableItems,AValue,oseoErrorDuplicateAvailable in Options);
end;

procedure TSmallOrderedSetEditDlg.SetHeaderCaption(const AValue: TTranslateString);
begin
  if HeaderCaption=AValue then Exit;
  HeaderLabel.Caption:=AValue;
  HeaderLabel.Visible:=HeaderLabel.Caption<>'';
end;

procedure TSmallOrderedSetEditDlg.SetItems(const AValue: TStrings);
begin
  SetList(FItems,AValue,oseoErrorDuplicateItems in Options);
end;

procedure TSmallOrderedSetEditDlg.UpdateButtonState;
var
  SelNode: TTreeNode;
begin
  SelNode:=ItemsTreeView.Selected;
  MoveUpBitBtn.Enabled:=(SelNode<>nil) and (SelNode.Index>0);
  MoveDownBitBtn.Enabled:=(SelNode<>nil) and (SelNode.Index<ItemsTreeView.Items.TopLvlCount-1);
end;

procedure TSmallOrderedSetEditDlg.SetOptions(const AValue: TSmOrdSetEditOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  MoveUpBitBtn.Visible:=not (oseoHideUpDown in Options);
  MoveDownBitBtn.Visible:=not (oseoHideUpDown in Options);
end;

function TSmallOrderedSetEditDlg.IndexOf(List: TStrings; Value: string): integer;
begin
  Result:=List.IndexOf(Value);
end;

function TSmallOrderedSetEditDlg.SetList(List, NewList: TStrings;
  ErrorOnDuplicate: boolean): boolean;
var
  CleanList: TStringList;
  i: Integer;
  s: String;
begin
  CleanList:=TStringList.Create;
  try
    for i:=0 to NewList.Count-1 do
    begin
      s:=NewList[i];
      if IndexOf(CleanList,s)>=0 then
      begin
        if ErrorOnDuplicate then
          raise EListError.Create(DbgSName(Self)+': duplicate item '+IntToStr(i)+' "'+s+'"');
        continue;
      end;
      CleanList.Add(s);
    end;
    if List.Equals(CleanList) then exit(false);
    Result:=true;
    List.Assign(CleanList);
  finally
    CleanList.Free;
  end;
end;

procedure TSmallOrderedSetEditDlg.UpdateShowing;
var
  CheckedDetails, UnCheckedDetails: TThemedElementDetails;
  CheckedSize, UnCheckedSize: TSize;
  Bmp: TBitmap;
begin
  inherited UpdateShowing;
  if Visible and (ImageList1.Count=0) then begin
    CheckedDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
    UnCheckedDetails := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
    CheckedSize:=ThemeServices.GetDetailSize(CheckedDetails);
    UnCheckedSize:=ThemeServices.GetDetailSize(UnCheckedDetails);
    ImageList1.Width:=Max(CheckedSize.cx,UnCheckedSize.cx);
    ImageList1.Height:=Max(CheckedSize.cy,UnCheckedSize.cy);
    Bmp:=TBitmap.Create;
    Bmp.SetSize(ImageList1.Width,ImageList1.Height);
    ImageList1.Add(Bmp,nil);
    ImageList1.Add(Bmp,nil);
    Bmp.Free;
  end;
end;

procedure TSmallOrderedSetEditDlg.ToggleNode(Node: TTreeNode);
var
  i, j: Integer;
begin
  Node.ImageIndex:=1-Node.ImageIndex;
  Node.SelectedIndex:=Node.ImageIndex;
  if Node.ImageIndex=0 then
  begin
    i:=IndexOf(Items,Node.Text);
    Items.Delete(i);
  end else begin
    j:=0;
    for i:=0 to Node.Index-1 do
    begin
      if ItemsTreeView.Items[i].ImageIndex>0 then
        inc(j);
    end;
    Items.Insert(j,Node.Text);
  end;
end;

constructor TSmallOrderedSetEditDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FItems:=TStringList.Create;
  FAvailableItems:=TStringList.Create;
end;

destructor TSmallOrderedSetEditDlg.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FAvailableItems);
  inherited Destroy;
end;

procedure TSmallOrderedSetEditDlg.Init;
var
  i: Integer;
  s: String;
  Node: TTreeNode;
begin
  for i:=FItems.Count-1 downto 0 do
  begin
    s:=FItems[i];
    if IndexOf(AvailableItems,s)<0 then
    begin
      if oseoErrorItemsContainNotAvailable in Options then
        raise EListError.Create(DbgSName(Self)+': item '+IntToStr(i)+' "'+s+'" is not in AvailableItems');
      FAvailableItems.Insert(0,s);
    end;
  end;

  ItemsTreeView.BeginUpdate;
  ItemsTreeView.Items.Clear;
  for i:=0 to AvailableItems.Count-1 do
  begin
    s:=AvailableItems[i];
    Node:=ItemsTreeView.Items.Add(nil,s);
    if IndexOf(Items,s)>=0 then
      Node.ImageIndex:=1
    else
      Node.ImageIndex:=0;
    Node.SelectedIndex:=Node.ImageIndex;
  end;
  ItemsTreeView.EndUpdate;
  UpdateButtonState;
end;

end.

