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
  Classes, SysUtils, laz.VirtualTrees,
  // LCL
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ButtonPanel,
  // OpkMan
  opkman_const, opkman_common, opkman_options;

type

  { TRepositoriesFrm }

  TRepositoriesFrm = class(TForm)
    bEdit: TButton;
    bDelete: TButton;
    bAdd: TButton;
    ButtonPanel1: TButtonPanel;
    imTree: TImageList;
    pnBottom: TPanel;
    procedure bAddClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FVST: TLazVirtualStringTree;
    FSortCol: Integer;
    FSortDir: laz.VirtualTrees.TSortDirection;
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
    procedure VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure PopulateTree;
    procedure EnableDisableButtons;
    function IsDuplicateRepository(const AAddress: String; const AUniqueID: Integer): Boolean;
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
    FUniqueID: Integer;
  end;

{ TRepositoriesFrm }

procedure TRepositoriesFrm.FormCreate(Sender: TObject);
begin
  if not Options.UseDefaultTheme then
    Self.Color := clBtnFace;
  Caption := rsRepositories_Caption;
  bAdd.Caption := rsRepositories_bAdd_Caption;
  bEdit.Caption := rsRepositories_bEdit_Caption;
  bDelete.Caption := rsRepositories_bDelete_Caption;
  //ButtonPanel1.OKButton.Caption := rsRepositories_bOk_Caption;
  //ButtonPanel1.CancelButton.Caption := rsRepositories_bCancel_Caption;

  FVST := TLazVirtualStringTree.Create(nil);
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
           if Trim(Value[Length(Value)]) <> '/' then
             Value := Trim(Value) + '/';
           if not IsDuplicateRepository(Value, -1) then
           begin
             Node := FVST.AddChild(nil);
             Data := FVST.GetNodeData(Node);
             Data^.FAddress := Trim(Value);
             Data^.FAddress := FixProtocol(Data^.FAddress);
             Data^.FType := 1;
             Data^.FImageIndex := 0;
             FVST.Selected[Node] := True;
             FVST.FocusedNode := Node;
             FVST.SortTree(0, FSortDir);
           end
           else
             MessageDlgEx(Format(rsRepositories_Info1, [value]), mtInformation, [mbOk], Self);
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
             if Trim(Value[Length(Value)]) <> '/' then
               Value := Trim(Value) + '/';
             if not IsDuplicateRepository(Value, Data^.FUniqueID) then
             begin
               Data^.FAddress := Trim(Value);
               Data^.FAddress := FixProtocol(Data^.FAddress);
               if Data^.FAddress[Length(Data^.FAddress)] <> '/' then
                 Data^.FAddress := Data^.FAddress + '/';
               FVST.ReinitNode(Node, False);
               FVST.RepaintNode(Node);
            end
            else
              MessageDlgEx(Format(rsRepositories_Info1, [value]), mtInformation, [mbOk], Self);
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
  FVST.SortTree(0, laz.VirtualTrees.sdAscending);
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

procedure TRepositoriesFrm.VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if (HitInfo.Column <> 0) and (HitInfo.Column <> 1) and (HitInfo.Column <> 3) then
    Exit;
  if HitInfo.Button = mbLeft then
  begin
    with Sender, Treeview do
    begin
      if (SortColumn = NoColumn) or (SortColumn <> HitInfo.Column) then
      begin
        SortColumn    := HitInfo.Column;
        SortDirection := laz.VirtualTrees.sdAscending;
      end
      else
      begin
        if SortDirection = laz.VirtualTrees.sdAscending then
          SortDirection := laz.VirtualTrees.sdDescending
        else
          SortDirection := laz.VirtualTrees.sdAscending;
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

function TRepositoriesFrm.IsDuplicateRepository(const AAddress: String;
  const AUniqueID: Integer): Boolean;
var
  Node: PVirtualNode;
  Data: PData;
begin
  Result := False;
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    if (UpperCase(Data^.FAddress) = UpperCase(AAddress)) and (Data^.FUniqueID <> AUniqueID) then
    begin
      Result := True;
      Break;
    end;
    Node := FVST.GetNext(Node);
  end;
end;

procedure TRepositoriesFrm.PopulateTree;
var
  I: Integer;
  Node: PVirtualNode;
  Data: PData;
  UniqueID: Integer;
begin
  if Trim(Options.RemoteRepositoryTmp.Text) = '' then
    Options.RemoteRepositoryTmp.Text := Options.RemoteRepository.Text;
  UniqueID := 0;
  for I := 0 to Options.RemoteRepositoryTmp.Count - 1 do
  begin
    if Trim(Options.RemoteRepositoryTmp.Strings[I]) <> '' then
    begin
      Inc(UniqueID);
      Node := FVST.AddChild(nil);
      Data := FVST.GetNodeData(Node);
      Data^.FAddress := Options.RemoteRepositoryTmp.Strings[I];
      if I = 0 then
        Data^.FType := 0
      else
        Data^.FType := 1;
      Data^.FImageIndex := 0;
      Data^.FUniqueID := UniqueID;
    end;
  end;
  FVST.SortTree(0, laz.VirtualTrees.sdAscending);
  Node := FVST.GetFirst;
  if Node <> nil then
  begin
    FVST.Selected[Node] := True;
    FVST.FocusedNode := Node;
  end;
  EnableDisableButtons;
end;

end.

