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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
 Abstract:
   Implementation of the visual tree, which displays the package sructure
   downloaded from the remote repository.
}
unit opkman_visualtree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Menus, Dialogs, Forms, LCLIntf, contnrs,
  PackageIntf, opkman_VirtualTrees, opkman_common, opkman_serializablepackages;


type
  PData = ^TData;
  TData = record
    DataType: Integer;
    Repository: String;
    PackageState: TPackageState;
    PackageName: String;
    PackageDisplayName: String;
    Category: TPackageCategory;
    PackageFileName: String;
    Version: String;
    InstalledVersion: String;
    Description: String;
    Author: String;
    LazCompatibility: String;
    FPCCompatibility: String;
    SupportedWidgetSet: String;
    PackageType: TPackageType;
    Dependencies: String;
    License: String;
    RepositoryFileName: String;
    RepositoryFileSize: Int64;
    RepositoryFileHash: String;
    RepositoryDate: TDate;
    HomePageURL: String;
    DownloadURL: String;
    SVNURL: String;
    IsInstalled: Boolean;
  end;

  TFilterBy = (fbPackageName, fbPackageFileName, fbPackageCategory, fbPackageState,
               fbVersion, fbDescription, fbAuthor, fbLazCompatibility, fbFPCCompatibility,
               fbSupportedWidgetsets, fbPackageType, fbDependecies, fbLicense);

  { TVisualTree }
  TOnChecking = procedure(Sender: TObject; const AIsAllChecked: Boolean) of object;
  TVisualTree = class
  private
    FVST: TVirtualStringTree;
    FHoverNode: PVirtualNode;
    FHoverColumn: Integer;
    FLink: String;
    FLinkClicked: Boolean;
    FSortCol: Integer;
    FSortDir: opkman_VirtualTrees.TSortDirection;
    FCheckingNodes: Boolean;
    FOnChecking: TOnChecking;
    FOnChecked: TNotifyEvent;
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; {%H-}Column: TColumnIndex;
      {%H-}CellPaintMode: TVTCellPaintMode; CellRect: TRect; var {%H-}ContentRect: TRect);
    procedure VSTChecking(Sender: TBaseVirtualTree; {%H-}Node: PVirtualNode;
      var NewState: TCheckState; var {%H-}Allowed: Boolean);
    procedure VSTChecked(Sender: TBaseVirtualTree; {%H-}Node: PVirtualNode);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Kind: TVTImageKind; Column: TColumnIndex; var {%H-}Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure VSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure VSTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; {%H-}Column: TColumnIndex;
      {%H-}TextType: TVSTTextType);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure VSTMouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
    procedure VSTOnDblClick(Sender: TObject);
    function IsAllChecked(const AChecking: PVirtualNode): Boolean;
  public
    constructor Create(const AParent: TWinControl; const AImgList: TImageList;
      APopupMenu: TPopupMenu);
    destructor Destroy; override;
  public
    procedure PopulateTree;
    procedure CheckNodes(const Checked: Boolean);
    procedure FilterTree(const AFilterBy: TFilterBy; const AText: String; const AExtraParam: Integer = -1);
    procedure ResetFilter;
    procedure ExpandEx;
    procedure CollapseEx;
    procedure GetPackageList;
    procedure UpdatePackageStates;
    function ResolveDependencies: TModalResult;
  published
    property OnChecking: TOnChecking read FOnChecking write FOnChecking;
    property OnChecked: TNotifyEvent read FOnChecked write FOnChecked;
    property VST: TVirtualStringTree read FVST;
  end;

var
  VisualTree: TVisualTree = nil;

implementation
uses opkman_const, opkman_options;

{ TVisualTree }

constructor TVisualTree.Create(const AParent: TWinControl; const AImgList: TImageList;
  APopupMenu: TPopupMenu);
begin
  FVST := TVirtualStringTree.Create(nil);
  with FVST do
   begin
     Parent := AParent;
     Align := alClient;
     Anchors := [akLeft, akTop, akRight];
     Images := AImgList;
     PopupMenu := APopupMenu;
     Color := clBtnFace;
     DefaultNodeHeight := 25;
     Indent := 22;
     TabOrder := 1;
     DefaultText := '';
     Header.AutoSizeIndex := 3;
     Header.Height := 25;
     with Header.Columns.Add do begin
       Position := 0;
       Width := 250;
       Text := rsMainFrm_VSTHeaderColumn_PackageName;
     end;
     with Header.Columns.Add do begin
       Position := 1;
       Alignment := taCenter;
       Width := 75;
       Text := rsMainFrm_VSTHeaderColumn_Installed;
     end;
     with Header.Columns.Add do begin
       Position := 2;
       Alignment := taCenter;
       Width := 75;
       Text := rsMainFrm_VSTHeaderColumn_Available;
     end;
     with Header.Columns.Add do begin
        Position := 3;
        Width := 400;
        Text := rsMainFrm_VSTHeaderColumn_Data;
      end;
     Header.Options := [hoAutoResize, hoColumnResize, hoRestrictDrag, hoShowSortGlyphs, hoVisible, hoAutoSpring];
     Header.SortColumn := 0;
     HintMode := hmHint;
     ShowHint := True;
     TabOrder := 2;
     TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
     TreeOptions.PaintOptions := [toHideFocusRect, toAlwaysHideSelection, toPopupMode, toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
     TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
     TreeOptions.AutoOptions := [toAutoTristateTracking];
     OnBeforeCellPaint := @VSTBeforeCellPaint;
     OnChecking := @VSTChecking;
     OnChecked := @VSTChecked;
     OnCompareNodes := @VSTCompareNodes;
     OnGetText := @VSTGetText;
     OnPaintText := @VSTPaintText;
     OnGetImageIndex := @VSTGetImageIndex;
     OnHeaderClick := @VSTHeaderClick;
     OnMouseMove := @VSTMouseMove;
     OnMouseDown := @VSTMouseDown;
     OnDblClick := @VSTOnDblClick;
     OnGetHint := @VSTGetHint;
     OnFreeNode := @VSTFreeNode;
   end;
end;

destructor TVisualTree.Destroy;
begin
  FVST.Free;
  inherited Destroy;
end;

procedure TVisualTree.PopulateTree;
var
  I, J: Integer;
  RootNode, Node, ChildNode, GrandChildNode: PVirtualNode;
  RootData, Data, ChildData, GrandChildData: PData;
  PackageFile: TPackageFile;
begin
  FVST.Clear;
  FVST.NodeDataSize := SizeOf(TData);

  //add repository(DataType = 0)
  RootNode := FVST.AddChild(nil);
  RootData := FVST.GetNodeData(RootNode);
  RootData^.Repository := Options.RemoteRepository;
  RootData^.DataType := 0;
  for I := 0 to SerializablePackages.Count - 1 do
  begin
     //add package(DataType = 1)
     Node := FVST.AddChild(RootNode);
     Node^.CheckType := ctTriStateCheckBox;
     Data := FVST.GetNodeData(Node);
     Data^.PackageName := SerializablePackages.Items[I].Name;
     Data^.PackageDisplayName := SerializablePackages.Items[I].DisplayName;
     Data^.PackageState := SerializablePackages.Items[I].PackageState;
     Data^.DataType := 1;
     for J := 0 to SerializablePackages.Items[I].PackageFiles.Count - 1 do
     begin
       //add packagefiles(DataType = 2)
       PackageFile := TPackageFile(SerializablePackages.Items[I].PackageFiles.Items[J]);
       ChildNode := FVST.AddChild(Node);
       ChildNode^.CheckType := ctTriStateCheckBox;
       ChildData := FVST.GetNodeData(ChildNode);
       ChildData^.PackageFileName := PackageFile.Name;
       ChildData^.InstalledVersion := PackageFile.InstalledFileVersion;
       ChildData^.Version := PackageFile.VersionAsString;
       ChildData^.PackageState := PackageFile.PackageState;
       ChildData^.DataType := 2;
       //add description(DataType = 3)
       GrandChildNode := FVST.AddChild(ChildNode);
       GrandChildData := FVST.GetNodeData(GrandChildNode);
       GrandChildData^.Description := PackageFile.Description;
       GrandChildData^.DataType := 3;
       //add author(DataType = 4)
       GrandChildNode := FVST.AddChild(ChildNode);
       GrandChildData := FVST.GetNodeData(GrandChildNode);
       GrandChildData^.Author := PackageFile.Author;
       GrandChildData^.DataType := 4;
       //add lazcompatibility(DataType = 5)
       GrandChildNode := FVST.AddChild(ChildNode);
       GrandChildData := FVST.GetNodeData(GrandChildNode);
       GrandChildData^.LazCompatibility := PackageFile.LazCompatibility;
       GrandChildData^.DataType := 5;
       //add fpccompatibility(DataType = 6)
       GrandChildNode := FVST.AddChild(ChildNode);
       GrandChildData := FVST.GetNodeData(GrandChildNode);
       GrandChildData^.FPCCompatibility := PackageFile.FPCCompatibility;
       GrandChildData^.DataType := 6;
       //add widgetset(DataType = 7)
       GrandChildNode := FVST.AddChild(ChildNode);
       GrandChildData := FVST.GetNodeData(GrandChildNode);
       GrandChildData^.SupportedWidgetSet := PackageFile.SupportedWidgetSet;
       GrandChildData^.DataType := 7;
       //add packagetype(DataType = 8)
       GrandChildNode := FVST.AddChild(ChildNode);
       GrandChildData := FVST.GetNodeData(GrandChildNode);
       GrandChildData^.PackageType := PackageFile.PackageType;
       GrandChildData^.DataType := 8;
       //add dependencies(DataType = 9)
       GrandChildNode := FVST.AddChild(ChildNode);
       GrandChildData := FVST.GetNodeData(GrandChildNode);
       GrandChildData^.Dependencies := PackageFile.DependenciesAsString;
       GrandChildData^.DataType := 9;
       //add license(DataType = 10)
       GrandChildNode := FVST.AddChild(ChildNode);
       GrandChildData := FVST.GetNodeData(GrandChildNode);
       GrandChildData^.License := PackageFile.License;
       GrandChildData^.DataType := 10;
     end;
     //add miscellaneous(DataType = 11)
     ChildNode := FVST.AddChild(Node);
     ChildData := FVST.GetNodeData(ChildNode);
     ChildData^.DataType := 11;
     //add category(DataType = 12)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.Category := SerializablePackages.Items[I].Category;
     GrandChildData^.DataType := 12;
     //add Repository Filename(DataType = 13)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.RepositoryFileName := SerializablePackages.Items[I].RepositoryFileName;
     GrandChildData^.DataType := 13;
     //add Repository Filesize(DataType = 14)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.RepositoryFileSize := SerializablePackages.Items[I].RepositoryFileSize;
     GrandChildData^.DataType := 14;
     //add Repository Hash(DataType = 15)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.RepositoryFileHash := SerializablePackages.Items[I].RepositoryFileHash;
     GrandChildData^.DataType := 15;
     //add Repository Date(DataType = 16)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.RepositoryDate := SerializablePackages.Items[I].RepositoryDate;
     GrandChildData^.DataType := 16;
     FVST.Expanded[ChildNode] := True;
     //add HomePageURL(DataType = 17)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.HomePageURL := SerializablePackages.Items[I].HomePageURL;
     GrandChildData^.DataType := 17;
     //add DownloadURL(DataType = 18)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.DownloadURL := SerializablePackages.Items[I].DownloadURL;
     GrandChildData^.DataType := 18;
     //add SVNURL(DataType = 19)
     GrandChildNode := FVST.AddChild(ChildNode);
     GrandChildData := FVST.GetNodeData(GrandChildNode);
     GrandChildData^.SVNURL := SerializablePackages.Items[I].SVNURL;
     GrandChildData^.DataType := 19;

  end;
  FVST.SortTree(0, opkman_VirtualTrees.sdAscending);
  FVST.Expanded[RootNode] := True;
end;

function TVisualTree.IsAllChecked(const AChecking: PVirtualNode): Boolean;
var
  Node: PVirtualNode;
begin
  Result := True;
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    if (FVST.CheckState[Node] = csUncheckedNormal) and (Node <> AChecking) then
    begin
      Result := False;
      Break;
    end;
    Node := FVST.GetNext(Node);
  end;
end;

procedure TVisualTree.CheckNodes(const Checked: Boolean);
var
  Node: PVirtualNode;
begin
  FCheckingNodes := True;
  try
    Node := FVST.GetFirst;
    while Assigned(Node) do
    begin
      if Checked then
        FVST.CheckState[Node] := csCheckedNormal
      else
        FVST.CheckState[Node] := csUncheckedNormal;
      Node := FVST.GetNext(Node);
    end;
  finally
    FCheckingNodes := False;
  end;
end;

procedure TVisualTree.FilterTree(const AFilterBy: TFilterBy; const AText:
  String; const AExtraParam: Integer = -1);

  function IsAtLeastOneChildVisible(const ANode: PVirtualNode): Boolean;
  var
    Level: Integer;
    Node: PVirtualNode;
    Data: PData;
  begin
    Result := False;
    Level := FVST.GetNodeLevel(ANode);
    Node := FVST.GetFirstChild(ANode);
    while Assigned(Node) do
    begin
      Data := FVST.GetNodeData(Node);
      case Level of
        0: if (vsVisible in Node^.States) then
           begin
             Result := True;
             Break;
           end;
        1: if (vsVisible in Node^.States) and (Data^.DataType = 2) then
           begin
             Result := True;
             Break;
           end;
      end;
      Node := FVST.GetNextSibling(Node);
    end;
  end;

  procedure HideShowParentNodes(const ANode: PVirtualNode; AShow: Boolean);
  var
    Level: Integer;
    RepositoryNode, PackageNode, PackageFileNode: PVirtualNode;
  begin
    RepositoryNode := nil;
    PackageNode := nil;
    PackageFileNode := nil;
    Level := FVST.GetNodeLevel(ANode);
    case Level of
      1: begin
           RepositoryNode := ANode^.Parent;
           PackageNode := ANode;
         end;
      2: begin
           RepositoryNode := ANode^.Parent^.Parent;
           PackageNode := ANode^.Parent;
           PackageFileNode := ANode;
         end;
      3: begin
           RepositoryNode := ANode^.Parent^.Parent^.Parent;
           PackageNode := ANode^.Parent^.Parent;
           PackageFileNode := ANode^.Parent;
         end;
    end;
    if Level = 1 then
    begin
      if AShow then
        FVST.IsVisible[RepositoryNode] := True
      else
        if not IsAtLeastOneChildVisible(RepositoryNode) then
          FVST.IsVisible[RepositoryNode] := False;
    end
    else if Level = 2 then
    begin
      if AShow then
      begin
        FVST.IsVisible[PackageNode] := True;
        FVST.IsVisible[RepositoryNode] := True;
      end
      else
      begin
        if not IsAtLeastOneChildVisible(PackageNode) then
        begin
          FVST.IsVisible[PackageNode] := False;
          HideShowParentNodes(PackageNode, AShow);
        end;
      end;
    end
    else if Level = 3 then
    begin
      if AShow then
      begin
        FVST.IsVisible[PackageFileNode] := True;
        FVST.IsVisible[PackageNode] := True;
        FVST.IsVisible[RepositoryNode] := True;
      end
      else
      begin
        FVST.IsVisible[PackageFileNode] := False;
        HideShowParentNodes(PackageFileNode, AShow);
      end;
    end;
  end;

  procedure FilterNode(Node: PVirtualNode; ADataText: String);
  var
    P: Integer;
  begin
    P := Pos(UpperCase(AText), UpperCase(ADataText));
    if P > 0 then
      FVST.IsVisible[Node] := True
    else
      FVST.IsVisible[Node] := False;
    if AText = 'PackageCategory' then //special case for categories
    begin
      if (P > 0) then
      begin
        FVST.IsVisible[Node^.Parent^.Parent] := True;
        FVST.IsVisible[Node^.Parent^.Parent^.Parent] := True;
      end
      else
      begin
        FVST.IsVisible[Node^.Parent^.Parent] := False;
        if not IsAtLeastOneChildVisible(Node^.Parent^.Parent^.Parent) then
          FVST.IsVisible[Node^.Parent^.Parent^.Parent] := False
      end;
    end
    else
      HideShowParentNodes(Node, P > 0)
  end;

var
  Node: PVirtualNode;
  Data: PData;
begin
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    case AFilterBy of
      fbPackageName:
        begin
          if (Data^.DataType = 1) then
            FilterNode(Node, Data^.PackageName);
        end;
      fbPackageFileName:
        begin
          if (Data^.DataType = 2) then
            FilterNode(Node, Data^.PackageFileName);
        end;
      fbPackageCategory:
        begin
          if Data^.DataType = 12 then
          begin
            if Data^.Category = TPackageCategory(AExtraParam) then
              FilterNode(Node, 'PackageCategory')
            else
              FilterNode(Node, '')
          end;
        end;
     fbPackageState:
       begin
         if Data^.DataType = 2 then
         begin
           if Data^.PackageState = TPackageState(AExtraParam) then
             FilterNode(Node, 'PackageState')
           else
             FilterNode(Node, '');
         end;
       end;
     fbVersion:
       begin
         if Data^.DataType = 2 then
           FilterNode(Node, Data^.Version);
       end;
     fbDescription:
       begin
         if Data^.DataType = 3 then
           FilterNode(Node, Data^.Description);
       end;
     fbAuthor:
       begin
         if Data^.DataType = 4 then
           FilterNode(Node, Data^.Author);
       end;
     fbLazCompatibility:
       begin
         if Data^.DataType = 5 then
           FilterNode(Node, Data^.LazCompatibility);
       end;
     fbFPCCompatibility:
       begin
         if Data^.DataType = 6 then
           FilterNode(Node, Data^.FPCCompatibility);
       end;
     fbSupportedWidgetsets:
       begin
         if Data^.DataType = 7 then
           FilterNode(Node, Data^.SupportedWidgetSet);
       end;
     fbPackageType:
       begin
         if Data^.DataType = 8 then
         begin
           if Data^.PackageType = TPackageType(AExtraParam) then
             FilterNode(Node, 'PackageType')
           else
             FilterNode(Node, '');
         end;
       end;
     fbDependecies:
       begin
          if Data^.DataType = 9 then
           FilterNode(Node, Data^.Dependencies);
       end;
     fbLicense:
       begin
          if Data^.DataType = 10 then
           FilterNode(Node, Data^.License);
       end;
   end;
   Node := FVST.GetNext(Node);
  end;
end;

procedure TVisualTree.ResetFilter;
var
  Node: PVirtualNode;
begin
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    FVST.IsVisible[Node] := True;
    Node := FVST.GetNext(Node);
  end;
end;

procedure TVisualTree.ExpandEx;
var
  Node: PVirtualNode;
  Data: PData;
begin
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    if (Data^.DataType = 0) or (Data^.DataType = 1) or (Data^.DataType = 11) then
      VST.Expanded[Node] := True;
    Node := FVST.GetNext(Node);
  end;
  Node := FVST.GetFirst;
  if Node <> nil then
    FVST.TopNode := Node;
end;

procedure TVisualTree.CollapseEx;
var
  Node: PVirtualNode;
  Data: PData;
begin
  FVST.FullCollapse;
  Node := FVST.GetFirst(True);
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    if (Data^.DataType = 0) or (Data^.DataType = 11) then
      VST.Expanded[Node] := True;
    Node := FVST.GetNext(Node);
  end;
end;

procedure TVisualTree.GetPackageList;
var
  Node: PVirtualNode;
  Data: PData;
  Package: TPackage;
  PackageFile: TPackageFile;
begin
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    if Data^.DataType = 1 then
    begin
      Package := SerializablePackages.FindPackage(Data^.PackageName, fpbPackageName);
      if Package <> nil then
      begin
        if (FVST.CheckState[Node] = csCheckedNormal) or (FVST.CheckState[Node] = csMixedNormal) then
          Package.Checked := True
        else if FVST.CheckState[Node] = csUncheckedNormal then
          Package.Checked := False
      end;
    end;
    if Data^.DataType = 2 then
    begin
      PackageFile := SerializablePackages.FindPackageFile(Data^.PackageFileName);
      if PackageFile <> nil then
      begin
        if FVST.CheckState[Node] = csCheckedNormal then
          PackageFile.Checked := True
        else if FVST.CheckState[Node] = csUncheckedNormal then
          PackageFile.Checked := False
      end;
    end;
    Node := FVST.GetNext(Node);
  end;
end;

procedure TVisualTree.UpdatePackageStates;
var
  Node: PVirtualNode;
  Data: PData;
  Package: TPackage;
  PackageFile: TPackageFile;
begin
  SerializablePackages.GetPackageStates;
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    Data := FVST.GetNodeData(Node);
    if (Data^.DataType = 1) then
    begin
      Package := SerializablePackages.FindPackage(Data^.PackageName, fpbPackageName);
      if Package <> nil then
      begin
        Data^.PackageState := Package.PackageState;
        FVST.ReinitNode(Node, False);
        FVST.RepaintNode(Node);
      end;
    end;
    if Data^.DataType = 2 then
    begin
      PackageFile := SerializablePackages.FindPackageFile(Data^.PackageFileName);
      if PackageFile <> nil then
      begin
        Data^.InstalledVersion := PackageFile.InstalledFileVersion;
        Data^.PackageState := PackageFile.PackageState;
        FVST.ReinitNode(Node, False);
        FVST.RepaintNode(Node);
      end;
    end;
    Node := FVST.GetNext(Node);
  end;
end;

procedure TVisualTree.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PData;
begin
  Data := Sender.GetNodeData(Node);
  if (Data^.DataType = 0) or (Data^.DataType = 1) then
  begin
    if (Node = Sender.FocusedNode) then
    begin
      if Column = 0 then
      begin
        if Data^.DataType = 0 then
          TargetCanvas.Brush.Color := $00E5E5E5 //00D8D8D8
        else if Data^.DataType = 1 then
          TargetCanvas.Brush.Color := $00E5E5E5;
            TargetCanvas.FillRect(CellRect);
        TargetCanvas.Brush.Color := FVST.Colors.FocusedSelectionColor;
        TargetCanvas.FillRect(ContentRect)
      end
      else
      begin
        TargetCanvas.Brush.Color := FVST.Colors.FocusedSelectionColor;
        TargetCanvas.FillRect(CellRect)
      end;
    end
    else
    begin
      if Data^.DataType = 0 then
        TargetCanvas.Brush.Color := $00E5E5E5 //00D8D8D8
      else if Data^.DataType = 1 then
        TargetCanvas.Brush.Color := $00E5E5E5;
      TargetCanvas.FillRect(CellRect);
    end;
  end
  else
  begin
    if (Node = Sender.FocusedNode) then
    begin
      TargetCanvas.Brush.Color := FVST.Colors.FocusedSelectionColor;
      if Column = 0 then
        TargetCanvas.FillRect(ContentRect)
      else
        TargetCanvas.FillRect(CellRect);
    end
    else
    begin
      TargetCanvas.Brush.Style := bsClear;
      TargetCanvas.FillRect(CellRect);
    end;
  end;
end;

procedure TVisualTree.VSTChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var NewState: TCheckState; var Allowed: Boolean);
begin
  if FCheckingNodes then
    Exit;
  if NewState = csUncheckedNormal then
  begin
    if Assigned(FOnChecking) then
      FOnChecking(Self, False);
  end
  else if NewState = csCheckedNormal then
  begin
    if IsAllChecked(Node) then
      FOnChecking(Self, True);
  end;
end;

procedure TVisualTree.VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if Assigned(FOnChecked) then
    FOnChecked(Self);
end;

function TVisualTree.ResolveDependencies: TModalResult;
var
  Node, NodeSearch: PVirtualNode;
  Data, DataSearch: PData;
  Msg: String;
  PackageList: TObjectList;
  PackageFileName: String;
  DependencyPackage: TPackageFile;
  I: Integer;
begin
  Result := mrNone;
  Node := FVST.GetFirst;
  while Assigned(Node) do
  begin
    if VST.CheckState[Node] = csCheckedNormal then
    begin
      Data := FVST.GetNodeData(Node);
      if Data^.DataType = 2 then
      begin
        PackageList := TObjectList.Create(True);
        try
          SerializablePackages.GetPackageDependencies(Data^.PackageFileName, PackageList, True, True);
          for I := 0 to PackageList.Count - 1 do
          begin
            PackageFileName := TPackageDependency(PackageList.Items[I]).PackageFileName + '.lpk';
            NodeSearch := VST.GetFirst;
            while Assigned(NodeSearch) do
            begin
              if NodeSearch <> Node then
              begin
                DataSearch := FVST.GetNodeData(NodeSearch);
                if DataSearch^.DataType = 2 then
                begin
                  DependencyPackage := SerializablePackages.FindPackageFile(DataSearch^.PackageFileName);
                  if (FVST.CheckState[NodeSearch] <> csCheckedNormal) and
                       (UpperCase(DataSearch^.PackageFileName) = UpperCase(PackageFileName)) and
                         ((SerializablePackages.IsDependencyOk(TPackageDependency(PackageList.Items[I]), DependencyPackage)) and
                           ((not (DependencyPackage.PackageState = psInstalled)) or ((DependencyPackage.PackageState = psInstalled) and (not (SerializablePackages.IsInstalledVersionOk(TPackageDependency(PackageList.Items[I]), DataSearch^.InstalledVersion)))))) then
                  begin
                    if (Result = mrNone) or (Result = mrYes) then
                    begin
                      Msg := rsProgressFrm_lbPackage_Caption + ' "' + Data^.PackageFileName + '" ' + rsMainFrm_rsPackageDependency0 + ' "' + DataSearch^.PackageFileName + '". ' + rsMainFrm_rsPackageDependency1;
                      Result := MessageDlgEx(Msg, mtConfirmation, [mbYes, mbYesToAll, mbNo, mbNoToAll, mbCancel], TForm(FVST.Parent.Parent));
                      if Result in [mrNo, mrNoToAll] then
                        MessageDlgEx(rsMainFrm_rsPackageDependency2, mtInformation, [mbOk], TForm(FVST.Parent.Parent));
                      if (Result = mrNoToAll) or (Result = mrCancel) then
                        Exit;
                    end;
                    if Result in [mrYes, mrYesToAll] then
                    begin
                      FVST.CheckState[NodeSearch] := csCheckedNormal;
                      FVST.ReinitNode(NodeSearch, False);
                      FVST.RepaintNode(NodeSearch);
                    end;
                  end;
                end;
              end;
              NodeSearch := FVST.GetNext(NodeSearch);
            end;
          end;
        finally
          PackageList.Free;
        end;
      end;
    end;
    Node := FVST.GetNext(Node);
  end;
end;

procedure TVisualTree.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1: PData;
  Data2: PData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    0: begin
         if (Data1^.DataType = 1) and (Data1^.DataType = 1) then
           Result := CompareText(Data1^.PackageName, Data2^.PackageName);
         if (Data1^.DataType < Data2^.DataType) then
           Result := 0
         else if (Data1^.DataType > Data2^.DataType) then
           Result := 1
         else if (Data1^.DataType = 2) and (Data1^.DataType = 2) then
           Result := CompareText(Data1^.PackageFileName, Data2^.PackageFileName);
       end;
    3: if (Data1^.DataType = 1) and (Data1^.DataType = 1) then
         Result := Ord(Data1^.PackageState) - Ord(Data2^.PackageState);
  end;
end;

procedure TVisualTree.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
    ImageIndex := Data^.DataType
end;

procedure TVisualTree.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if Column = 0 then
  begin
    case Data^.DataType of
      0: CellText := Data^.Repository;
      1: if Trim(Data^.PackageDisplayName) = '' then
           CellText := Data^.PackageName
         else
           CellText := Data^.PackageDisplayName;
      2: CellText := Data^.PackageFileName;
      3: CellText := rsMainFrm_VSTText_Description;
      4: CellText := rsMainFrm_VSTText_Author;
      5: CellText := rsMainFrm_VSTText_LazCompatibility;
      6: CellText := rsMainFrm_VSTText_FPCCompatibility;
      7: CellText := rsMainFrm_VSTText_SupportedWidgetsets;
      8: CellText := rsMainFrm_VSTText_Packagetype;
      9: CellText := rsMainFrm_VSTText_Dependecies;
     10: CellText := rsMainFrm_VSTText_License;
     11: CellText := rsMainFrm_VSTText_PackageInfo;
     12: CellText := rsMainFrm_VSTText_Category;
     13: CellText := rsMainFrm_VSTText_RepositoryFilename;
     14: CellText := rsMainFrm_VSTText_RepositoryFileSize;
     15: CellText := rsMainFrm_VSTText_RepositoryFileHash;
     16: CellText := rsMainFrm_VSTText_RepositoryFileDate;
     17: CellText := rsMainFrm_VSTText_HomePageURL;
     18: CellText := rsMainFrm_VSTText_DownloadURL;
     19: CellText := rsMainFrm_VSTText_SVNURL;

    end;
  end
  else if Column = 1 then
  begin
    if Data^.InstalledVersion = '' then
      Data^.InstalledVersion := '-';
    if Data^.DataType = 2 then
      CellText := Data^.InstalledVersion
    else
      CellText := '';
  end
  else if Column = 2 then
  begin
    if Data^.DataType = 2 then
      CellText := Data^.Version
    else
      CellText := '';
  end
  else if Column = 3 then
  begin
    case Data^.DataType of
      0: CellText := '';
      1: CellText := '';
      2: case Ord(Data^.PackageState) of
           0: CellText := rsMainFrm_VSTText_PackageState0;
           1: CellText := rsMainFrm_VSTText_PackageState1;
           2: CellText := rsMainFrm_VSTText_PackageState2;
           3: begin
                Data^.IsInstalled := Data^.InstalledVersion >= Data^.Version;
                if Data^.IsInstalled then
                  CellText := rsMainFrm_VSTText_PackageState4
                else
                  CellText := rsMainFrm_VSTText_PackageState3
              end;
         end;
      3: CellText := Data^.Description;
      4: CellText := Data^.Author;
      5: CellText := Data^.LazCompatibility;
      6: CellText := Data^.FPCCompatibility;
      7: CellText := Data^.SupportedWidgetSet;
      8: case Data^.PackageType of
           ptRunAndDesignTime: CellText := rsMainFrm_VSTText_PackageType0;
           ptDesignTime:       CellText := rsMainFrm_VSTText_PackageType1;
           ptRunTime:          CellText := rsMainFrm_VSTText_PackageType2;
           ptRunTimeOnly:      CellText := rsMainFrm_VSTText_PackageType3;
         end;
      9: CellText := Data^.Dependencies;
     10: CellText := Data^.License;
     11: CellText := '';
     12: case Data^.Category of
           pcCryptography:  CellText := rsMainFrm_VSTText_PackageCategory0;
           pcDataControls:  CellText := rsMainFrm_VSTText_PackageCategory1;
           pcGraphics:      CellText := rsMainFrm_VSTText_PackageCategory2;
           pcGUIContainers: CellText := rsMainFrm_VSTText_PackageCategory3;
           pcLazIDEPlugins: CellText := rsMainFrm_VSTText_PackageCategory4;
           pcMultimedia:    CellText := rsMainFrm_VSTText_PackageCategory5;
           pcNetworking:    CellText := rsMainFrm_VSTText_PackageCategory6;
           pcReporting:     CellText := rsMainFrm_VSTText_PackageCategory7;
           pcOther:         CellText := rsMainFrm_VSTText_PackageCategory8
         end;
     13: CellText := Data^.RepositoryFileName;
     14: CellText := FormatSize(Data^.RepositoryFileSize);
     15: CellText := Data^.RepositoryFileHash;
     16: CellText := FormatDateTime('YYYY.MM.DD', Data^.RepositoryDate);
     17: CellText := Data^.HomePageURL;
     18: CellText := Data^.DownloadURL;
     19: CellText := Data^.SVNURL;
    end;
  end;
end;

procedure TVisualTree.VSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Column = 1) or (Column = 2) then
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

procedure TVisualTree.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if (Column = 3) and (FHoverNode = Node) and (FHoverColumn = Column) and ((Data^.DataType = 17) or (Data^.DataType = 18)) then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsUnderline];
    if  Node <> Sender.FocusedNode then
      TargetCanvas.Font.Color := clBlue
    else
      TargetCanvas.Font.Color := clWhite;
  end
  else if (Column = 3) and (Data^.DataType = 2) and (Data^.IsInstalled) then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    if  Node <> Sender.FocusedNode then
      TargetCanvas.Font.Color := clGreen
    else
      TargetCanvas.Font.Color := clWhite;
  end
  else
  begin
    if  Node <> Sender.FocusedNode then
      TargetCanvas.Font.Color := FVST.Font.Color
    else
      TargetCanvas.Font.Color := clWhite;
  end;
end;

procedure TVisualTree.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TVisualTree.VSTMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
 I, L, R: Integer;
begin
  FHoverColumn := -1;
  FHoverNode:= VST.GetNodeAt(X, Y);
  for I := 0 to VST.Header.Columns.Count - 1 do
  begin
    VST.Header.Columns.GetColumnBounds(I, L, R);
    if (X >= L) and (X <= R) then
    begin
      FHoverColumn := I;
      Break;
    end;
  end;
end;

procedure TVisualTree.VSTMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 Node: PVirtualNode;
 Data: PData;
 I, L, R: Integer;
begin
  if Button = mbLeft then
  begin
    Node := FVST.GetNodeAt(X, Y);
    if Node <> nil then
    begin
      Data := FVST.GetNodeData(Node);
      if (Data^.DataType = 17) or (Data^.DataType = 18) then
      begin
        for I := 0 to VST.Header.Columns.Count - 1 do
         begin
           VST.Header.Columns.GetColumnBounds(I, L, R);
           if (X >= L) and (X <= R) and (I = 3) then
           begin
             FLinkClicked := True;
             if (Data^.DataType = 17) and (Trim(Data^.HomePageURL) <> '') then
             begin
               FLink := Data^.HomePageURL;
               Break
             end
             else if (Data^.DataType = 18) and (Trim(Data^.DownloadURL) <> '') then
             begin
               FLink := Data^.DownloadURL;
               Break;
             end;
           end;
         end;
      end;
    end;
  end;
end;

procedure TVisualTree.VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);
var
  Data: PData;
begin
  Data := FVST.GetNodeData(Node);
  if (Column <> 3) then
    Exit;
  LineBreakStyle := hlbForceMultiLine;
  case Data^.DataType of
    3: HintText := Data^.Description;
   10: HintText := Data^.License;
   else
       HintText := '';
  end;
end;

procedure TVisualTree.VSTOnDblClick(Sender: TObject);
begin
  if FLinkClicked then
  begin
    FLinkClicked := False;
    FHoverColumn := -1;
    FHoverNode := nil;
    OpenDocument(FLink);
  end;
end;

end.

