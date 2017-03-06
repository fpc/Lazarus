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

 Author: Balázs Székely
}

unit opkman_repositories;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  // OpkMan
  opkman_const, opkman_common, opkman_options, opkman_VirtualTrees;

type

  { TRepositoriesFrm }

  TRepositoriesFrm = class(TForm)
    bEdit: TButton;
    bCancel: TButton;
    bDelete: TButton;
    bAdd: TButton;
    bOk: TButton;
    imTree: TImageList;
    pnBottom: TPanel;
    pnBottom1: TPanel;
    procedure bAddClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FVST: TVirtualStringTree;
    FSortCol: Integer;
    FSortDir: opkman_VirtualTrees.TSortDirection;
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTFocuseChanged(Sender: TBaseVirtualTree; {%H-}Node: PVirtualNode; {%H-}Column: TColumnIndex);
    procedure VSTPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; {%H-}Column: TColumnIndex; {%H-}TextType: TVSTTextType);
    procedure VSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure PopulateTree;
    procedure EnableDisableButtons;
  public
  end;

var
  RepositoriesFrm: TRepositoriesFrm;

implementation

{$R *.lfm}

type
  PData = ^TData;
  TData = record
    FAddress: string;
    FType: Integer;
    FImageIndex: Integer;
  end;

{ TRepositoriesFrm }

procedure TRepositoriesFrm.FormCreate(Sender: TObject);
begin
  Caption := rsRepositories_Caption;
  bAdd.Caption := rsRepositories_bAdd_Caption;
  bEdit.Caption := rsRepositories_bEdit_Caption;
  bDelete.Caption := rsRepositories_bDelete_Caption;
  bOk.Caption := rsRepositories_bOk_Caption;
  bCancel.Caption := rsRepositories_bCancel_Caption;

  FVST := TVirtualStringTree.Create(nil);
  with FVST do
  begin
    Parent := Self;
    Align := alClient;
    Anchors := [akLeft, akTop, akRight];
    Color := clBtnFace;
    DefaultNodeHeight := 22;
    Indent := 0;
    DefaultText := '';
    Colors.BorderColor := clBlack;
    BorderSpacing.Top := 5;
    BorderSpacing.Left := 5;
    BorderSpacing.Right := 0;
    BorderSpacing.Bottom := 0;
    Header.Height := 25;
    Header.Options := [hoAutoResize, hoVisible, hoColumnResize, hoRestrictDrag, hoShowSortGlyphs, hoAutoSpring];
    {$IFDEF LCLCarbon}
    Header.Options := Header.Options - [hoShowSortGlyphs];
    {$ENDIF}
    Header.AutoSizeIndex := 1;
    Header.Height := 25;
    with Header.Columns.Add do
    begin
      Position := 1;
      Width := 250;
      Text := rsRepositories_VST_HeaderColumn;
    end;
    Header.SortColumn := 0;
    TabOrder := 0;
    TreeOptions.MiscOptions := [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
    TreeOptions.PaintOptions := [toHideFocusRect, toPopupMode, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages];
    TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
    TreeOptions.AutoOptions := [toAutoTristateTracking];
    Images := imTree;
    OnGetText := @VSTGetText;
    OnGetImageIndex := @VSTGetImageIndex;
    OnCompareNodes := @VSTCompareNodes;
    OnFocusChanged := @VSTFocuseChanged;
    OnPaintText := @VSTPaintText;
    OnHeaderClick := @VSTHeaderClick;
    OnFreeNode := @VSTFreeNode;
  end;
  FVST.NodeDataSize := SizeOf(TData);
  PopulateTree;
end;

procedure TRepositoriesFrm.bAddClick(Sender: TObject);
var
  PrevNode, Node: PVirtualNode;
  Data: PData;
  Value: String;
begin
  case (Sender as TButton).Tag of
    0: begin
         Value := InputBox(rsRepositories_InputBox_Caption0, rsRepositories_InputBox_Text, '');
         if Value <> '' then
         begin
           Node := FVST.AddChild(nil);
           Data := FVST.GetNodeData(Node);
           Data^.FAddress := Trim(Value);
           Data^.FAddress := FixProtocol(Data^.FAddress);
           if Data^.FAddress[Length(Data^.FAddress)] <> '/' then
             Data^.FAddress := Data^.FAddress + '/';
           Data^.FType := 1;
           Data^.FImageIndex := 0;
           FVST.Selected[Node] := True;
           FVST.FocusedNode := Node;
           FVST.SortTree(0, FSortDir);
         end;
       end;
    1: begin
         Node := FVST.GetFirstSelected;
         if Node <> nil then
         begin
           Data := FVST.GetNodeData(Node);
           Value := InputBox(rsRepositories_InputBox_Caption1, rsRepositories_InputBox_Text, Data^.FAddress);
           if Value <> '' then
           begin
             Data^.FAddress := Trim(Value);
             Data^.FAddress := FixProtocol(Data^.FAddress);
             if Data^.FAddress[Length(Data^.FAddress)] <> '/' then
               Data^.FAddress := Data^.FAddress + '/';
             FVST.ReinitNode(Node, False);
             FVST.RepaintNode(Node);
           end;
           FVST.SortTree(0, FSortDir);
          end;
       end;
    2: begin
         Node := FVST.GetFirstSelected;
         if Node <> nil then
         begin
           Data := FVST.GetNodeData(Node);
           if MessageDlgEx(Format(rsRepositories_Confirmation0, [Data^.FAddress]), mtConfirmation, [mbYes, mbNo], Self) = mrYes then
           begin
             PrevNode := FVST.GetPrevious(Node);
             FVST.DeleteNode(Node);
             if PrevNode <> nil then
             begin
               FVST.Selected[PrevNode] := True;
               FVST.FocusedNode := PrevNode;
             end;
           end;
         end;
       end;
   end;
end;

procedure TRepositoriesFrm.bOkClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PData;
begin
  Options.RemoteRepositoryTmp.Clear;
  FVST.BeginUpdate;
  FVST.SortTree(0, opkman_VirtualTrees.sdAscending);
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    Options.RemoteRepositoryTmp.Add(Data^.FAddress);
    Node := FVST.GetNext(Node);
  end;
end;

procedure TRepositoriesFrm.FormDestroy(Sender: TObject);
begin
  FVST.Clear;
  FVST.Free;
end;

procedure TRepositoriesFrm.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
    CellText := Data^.FAddress;
end;

procedure TRepositoriesFrm.VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var ImageIndex: Integer);
var
  Data: PData;
begin
  if Column <> 0 then
    Exit;
  Data := FVST.GetNodeData(Node);
  ImageIndex := Data^.FImageIndex;
end;

procedure TRepositoriesFrm.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PData;
  Data2: PData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    0: if Data1^.FType > Data2^.FType then
         Result := 1
       else if Data1^.FType < Data2^.FType then
         Result := 0
       else if Data1^.FType = Data2^.FType then
         Result := CompareText(Data1^.FAddress, Data2^.FAddress);
  end;
end;

procedure TRepositoriesFrm.VSTFocuseChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  EnableDisableButtons;
end;

procedure TRepositoriesFrm.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Data^.FType = 0 then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold]
  else
    TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold]
end;

procedure TRepositoriesFrm.VSTHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Column <> 0) and (Column <> 1) and (Column <> 3) then
    Exit;
  if Button = mbLeft then
  begin
    with Sender, Treeview do
    begin
      if (SortColumn = NoColumn) or (SortColumn <> Column) then
      begin
        SortColumn    := Column;
        SortDirection := opkman_VirtualTrees.sdAscending;
      end
      else
      begin
        if SortDirection = opkman_VirtualTrees.sdAscending then
          SortDirection := opkman_VirtualTrees.sdDescending
        else
          SortDirection := opkman_VirtualTrees.sdAscending;
        FSortDir := SortDirection;
      end;
      SortTree(SortColumn, SortDirection, False);
      FSortCol := Sender.SortColumn;
    end;
  end;
end;

procedure TRepositoriesFrm.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TRepositoriesFrm.EnableDisableButtons;
var
  SelNode: PVirtualNode;
  SelData: PData;
begin
  bEdit.Enabled := False;
  bDelete.Enabled := False;
  if FVST.RootNodeCount = 0 then
    Exit;
  SelNode := FVST.GetFirstSelected;
  if SelNode = nil then
    Exit;
  SelData := FVST.GetNodeData(SelNode);
  bEdit.Enabled := SelData^.FType > 0;
  bDelete.Enabled := SelData^.FType > 0;
end;

procedure TRepositoriesFrm.PopulateTree;
var
  I: Integer;
  Node: PVirtualNode;
  Data: PData;
begin
  if Trim(Options.RemoteRepositoryTmp.Text) = '' then
    Options.RemoteRepositoryTmp.Text := Options.RemoteRepository.Text;
  for I := 0 to Options.RemoteRepositoryTmp.Count - 1 do
  begin
    if Trim(Options.RemoteRepositoryTmp.Strings[I]) <> '' then
    begin
      Node := FVST.AddChild(nil);
      Data := FVST.GetNodeData(Node);
      Data^.FAddress := Options.RemoteRepositoryTmp.Strings[I];
      if I = 0 then
        Data^.FType := 0
      else
        Data^.FType := 1;
      Data^.FImageIndex := 0;
    end;
  end;
  FVST.SortTree(0, opkman_VirtualTrees.sdAscending);
  Node := FVST.GetFirst;
  if Node <> nil then
  begin
    FVST.Selected[Node] := True;
    FVST.FocusedNode := Node;
  end;
  EnableDisableButtons;
end;

end.

