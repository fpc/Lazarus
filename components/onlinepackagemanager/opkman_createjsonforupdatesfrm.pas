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

unit opkman_createjsonforupdatesfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, VirtualTrees,
  // LCL
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLIntf,
  // OpkMan
  opkman_serializablepackages, opkman_const, opkman_common, opkman_updates,
  opkman_options, opkman_maindm;

type

  { TCreateJSONForUpdatesFrm }

  TCreateJSONForUpdatesFrm = class(TForm)
    bClose: TButton;
    bCreate: TButton;
    bHelp: TButton;
    bTest: TButton;
    edLinkToZip: TEdit;
    lbLinkToZip: TLabel;
    pnTop: TPanel;
    pnButtons: TPanel;
    SD: TSaveDialog;
    procedure bCreateClick(Sender: TObject);
    procedure bHelpClick(Sender: TObject);
    procedure bTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FVST: TVirtualStringTree;
    FMetaPackage: TMetaPackage;
    FSortCol: Integer;
    FSortDir: VirtualTrees.TSortDirection;
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
  public
    procedure PopluateTree;
  end;

var
  CreateJSONForUpdatesFrm: TCreateJSONForUpdatesFrm;

implementation

{$R *.lfm}

{ TCreateJSONForUpdatesFrm }

type
  PData = ^TData;
  TData = record
    FName: string;
    FVersion: String;
    FForceNotify: Boolean;
    FInternalVersion: Integer;
    FImageIndex: Integer;
  end;

procedure TCreateJSONForUpdatesFrm.FormCreate(Sender: TObject);
begin
  if not Options.UseDefaultTheme then
    Self.Color := clBtnFace;
  Caption := rsCreateJSONForUpdatesFrm_Caption;
  lbLinkToZip.Caption := rsCreateJSONForUpdatesFrm_lbLinkToZip_Caption;
  bTest.Caption := rsCreateJSONForUpdatesFrm_bTest_Caption;
  bCreate.Caption := rsCreateJSONForUpdatesFrm_bCreate_Caption;
  bHelp.Caption := rsCreateJSONForUpdatesFrm_bHelp_Caption;
  bClose.Caption := rsCreateJSONForUpdatesFrm_bClose_Caption;

  FVST := TVirtualStringTree.Create(nil);
   with FVST do
   begin
     Parent := Self;
     Align := alClient;
     Anchors := [akLeft, akTop, akRight];
     Images := MainDM.Images;
     if not Options.UseDefaultTheme then
       Color := clBtnFace;
     DefaultNodeHeight := Scale96ToForm(25);
     Indent := 0;
     TabOrder := 1;
     DefaultText := '';
     Header.AutoSizeIndex := 0;
     Header.Height := Scale96ToForm(25);
     Colors.BorderColor := clBlack;
     BorderSpacing.Top := Scale96ToForm(15);
     BorderSpacing.Left := Scale96ToForm(15);
     BorderSpacing.Right := Scale96ToForm(15);
     BorderSpacing.Bottom := 0;
     with Header.Columns.Add do
     begin
       Position := 0;
       Width := Scale96ToForm(250);
       Text := rsCreateJSONForUpdatesFrm_Column0_Text;
     end;
     with Header.Columns.Add do
     begin
       Position := 1;
       Width := Scale96ToForm(75);
       Text := rsCreateJSONForUpdatesFrm_Column1_Text;
       Alignment := taCenter;
     end;
     with Header.Columns.Add do
     begin
       Position := 2;
       Width := Scale96ToForm(100);
       Text := rsCreateJSONForUpdatesFrm_Column2_Text;
       Alignment := taCenter;
       Options := Options - [coVisible];
     end;
     with Header.Columns.Add do
     begin
       Position := 3;
       Width := Scale96ToForm(100);
       Text := rsCreateJSONForUpdatesFrm_Column3_Text;
       Alignment := taCenter;
       Options := Options - [coVisible];
     end;
     Header.Options := [hoAutoResize, hoRestrictDrag, hoShowSortGlyphs, hoAutoSpring, hoVisible];
     Header.SortColumn := 0;
     TabOrder := 2;
     TreeOptions.MiscOptions := [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toCheckSupport, toEditable];
     TreeOptions.PaintOptions := [toHideFocusRect, toPopupMode, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages];
     TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
     TreeOptions.AutoOptions := [toAutoTristateTracking];
     OnGetText := @VSTGetText;
     OnGetImageIndex := @VSTGetImageIndex;
     OnCompareNodes := @VSTCompareNodes;
     OnHeaderClick := @VSTHeaderClick;
     OnFreeNode := @VSTFreeNode;
   end;
   FVST.NodeDataSize := SizeOf(TData);
end;

procedure TCreateJSONForUpdatesFrm.FormDestroy(Sender: TObject);
begin
  FVST.Free;
end;

procedure TCreateJSONForUpdatesFrm.bTestClick(Sender: TObject);
begin
  if Trim(edLinkToZip.Text) = '' then
  begin
    MessageDlgEx(rsCreateJSONForUpdatesFrm_Message2, mtInformation, [mbOk], Self);
    edLinkToZip.SetFocus;
    Exit;
  end;
  OpenURL(edLinkToZip.Text);
end;

procedure TCreateJSONForUpdatesFrm.bCreateClick(Sender: TObject);
var
  UpdatePkg: TUpdatePackage;
  UpdateLazPkgs: TUpdateLazPackages;
  JSON: TJSONStringType;
  Ms: TMemoryStream;
  Node: PVirtualNode;
  Data: PData;
  CanClose: Boolean;
  ErrMsg: String;
begin
  if FVST.CheckedCount = 0 then
  begin
    MessageDlgEx(rsCreateJSONForUpdatesFrm_Message3, mtInformation, [mbOk], Self);
    Exit;
  end;
  CanClose := False;
  if FMetaPackage.DisplayName <> '' then
    SD.FileName := 'update_' + FMetaPackage.DisplayName
  else
    SD.FileName := 'update_' + FMetaPackage.Name;
  if SD.Execute then
  begin
    UpdatePkg := TUpdatePackage.Create;
    try
      UpdatePkg.UpdatePackageData.Name := FMetaPackage.Name;
      UpdatePkg.UpdatePackageData.DownloadZipURL := edLinkToZip.Text;

      Node := FVST.GetFirst;
      while Assigned(Node) do
      begin
        if FVST.CheckState[Node] = csCheckedNormal then
        begin
          Data := FVST.GetNodeData(Node);
          UpdateLazPkgs := TUpdateLazPackages(UpdatePkg.UpdateLazPackages.Add);
          UpdateLazPkgs.Name := Data^.FName;
          UpdateLazPkgs.Version := Data^.FVersion;
          UpdateLazPkgs.ForceNotify := Data^.FForceNotify;
          UpdateLazPkgs.InternalVersion := Data^.FInternalVersion;
        end;
        Node := FVST.GetNext(Node);
      end;
      JSON := '';
      if UpdatePkg.SaveToJSON(JSON) then
      begin
        JSON := StringReplace(JSON, '\/', '/', [rfReplaceAll]);
        Ms := TMemoryStream.Create;
        try
          Ms.Write(Pointer(JSON)^, Length(JSON));
          Ms.Position := 0;
          Ms.SaveToFile(SD.FileName);
        finally
          MS.Free;
        end;
        CanClose := True;
        MessageDlgEx(rsCreateJSONForUpdatesFrm_Message4, mtInformation, [mbOk], Self);
      end
      else
      begin
        ErrMsg := StringReplace(UpdatePkg.LastError, '"', '', [rfReplaceAll]);
        MessageDlgEx(rsCreateJSONForUpdatesFrm_Error1 + sLineBreak + '"' + ErrMsg + '"', mtError, [mbOk], Self);
      end;
    finally
      UpdatePkg.Free;
    end;
  end;
  if CanClose then
    Close
end;

procedure TCreateJSONForUpdatesFrm.bHelpClick(Sender: TObject);
begin
  OpenURL(cHelpPage_CreateExternalJSON);
end;

procedure TCreateJSONForUpdatesFrm.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  case Column of
    0: CellText := Data^.FName;
    1: CellText := Data^.FVersion;
    2: CellText := BoolToStr(Data^.FForceNotify, True);
    3: CellText := IntToStr(Data^.FInternalVersion);
  end;
end;

procedure TCreateJSONForUpdatesFrm.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
    ImageIndex := Data^.FImageIndex;
end;

procedure TCreateJSONForUpdatesFrm.VSTCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PData;
  Data2: PData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    0: Result := CompareText(Data1^.FName, Data2^.FName);
    1: Result := CompareText(Data1^.FVersion, Data2^.FVersion);
  end;
end;

procedure TCreateJSONForUpdatesFrm.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  Finalize(Data^)
end;

procedure TCreateJSONForUpdatesFrm.VSTHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Column > 1 then
    Exit;
  if HitInfo.Button = mbLeft then
  begin
    with Sender, Treeview do
    begin
      if (SortColumn = NoColumn) or (SortColumn <> HitInfo.Column) then
      begin
        SortColumn    := HitInfo.Column;
        SortDirection := VirtualTrees.sdAscending;
      end
      else
      begin
        if SortDirection = VirtualTrees.sdAscending then
          SortDirection := VirtualTrees.sdDescending
        else
          SortDirection := VirtualTrees.sdAscending;
        FSortDir := SortDirection;
      end;
      SortTree(SortColumn, SortDirection, False);
      FSortCol := Sender.SortColumn;
    end;
  end;

end;

procedure TCreateJSONForUpdatesFrm.PopluateTree;
var
  I, J: Integer;
  Node: PVirtualNode;
  Data: PData;
  LazarusPkg: TLazarusPackage;
begin
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    FMetaPackage := SerializablePackages.Items[I];
    if FMetaPackage.Checked then
    begin
      Caption := Caption + ' "' + FMetaPackage.DisplayName +'"';
      for J := 0 to FMetaPackage.LazarusPackages.Count - 1 do
      begin
        LazarusPkg := TLazarusPackage(FMetaPackage.LazarusPackages.Items[J]);
        if LazarusPkg.Checked then
        begin
          Node := FVST.AddChild(nil);
          Node^.CheckType := ctTriStateCheckBox;
          FVST.CheckState[Node] := csCheckedNormal;
          Data := FVST.GetNodeData(Node);
          Data^.FName := LazarusPkg.Name;
          Data^.FVersion := LazarusPkg.VersionAsString;
          Data^.FForceNotify := False;
          Data^.FInternalVersion := 1;
          Data^.FImageIndex := 1;
        end;
      end;
      Break;
    end;
  end;
end;


end.

