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

  Author: Juha Manninen

  Abstract:
    TProjPackFilePropGui creates and sets layout for controls that are used for
    properties of files and dependencies in a package or in a project.
    Used by the package editor and the project inspector.
}
unit ProjPackFilePropGui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LResources, Forms, Controls, StdCtrls, ComCtrls, Dialogs,
  // LazControls
  TreeFilterEdit,
  // LazUtils
  LazLoggerBase,
  // BuildIntf
  PackageDependencyIntf, PackageLinkIntf, PackageIntf,
  // IdeIntf
  FormEditingIntf, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, ProjPackEditing, PackageLinks, MainIntf;

type

  TMultiBool = (mubNone, mubAllTrue, mubAllFalse, mubMixed);
  TGetPkgDepEvent = function(Immediately: boolean): TPkgDependencyID of object;

  { TProjPackFilePropGui }

  TProjPackFilePropGui = class
  private
    fOwner: TWinControl;
    FNodeDataList: array[TPENodeType] of TPENodeData;
    fOnGetPkgDep: TGetPkgDepEvent;
    procedure MinMaxVersionEditChange(Sender: TObject);
    procedure UseMinVersionCheckBoxChange(Sender: TObject);
    procedure UseMaxVersionCheckBoxChange(Sender: TObject);
  public
    // ImageIndexes
    ImageIndexRequired: integer;
    ImageIndexRemovedRequired: integer;
    ImageIndexConflict: integer;
    ImageIndexAvailableOnline: integer;
    // file properties
    AddToUsesPkgSectionCheckBox: TCheckBox;
    CallRegisterProcCheckBox: TCheckBox;
    RegisteredPluginsGroupBox: TGroupBox;
    RegisteredListBox: TListBox;
    DisableI18NForLFMCheckBox: TCheckBox;  // I18N
    // dependency properties
    UseMinVersionCheckBox: TCheckBox;
    MinVersionEdit: TEdit;
    UseMaxVersionCheckBox: TCheckBox;
    MaxVersionEdit: TEdit;
    ApplyDependencyButton: TButton;
    // Values used when setting controls' visibility and Enabled state.
    ControlVisible, ControlEnabled: Boolean;

    constructor Create(aOwner: TWinControl; aPackageGui: Boolean);
    destructor Destroy; override;
    function CreateNodeData(Typ: TPENodeType; aName: string; aRemoved: boolean): TPENodeData;
    procedure FreeNodeData(Typ: TPENodeType);
    function GetDependencyImageIndex(aDep: TPkgDependencyID): Integer;
    procedure SetAddToUsesCB(State: TMultiBool);
    procedure SetCallRegisterProcCB(State: TMultiBool);
    procedure SetRegisteredPluginsGB(aPlugins: TStringList);
    procedure SetMinMaxVisibility;
    procedure SetMinMaxValues(aDep: TPkgDependencyID);
    procedure SetDisableI18NCB(State: TMultiBool);
    function CheckApplyDependency(aDep: TPkgDependencyID): Boolean;
    procedure UpdateApplyDependencyButton(Immediately: boolean = False);

    property OnGetPkgDep: TGetPkgDepEvent read fOnGetPkgDep write fOnGetPkgDep;
  end;

  function GetNodeData(TVNode: TTreeNode): TPENodeData;
  function NodeTreeIsIn(xIterNode, xParentNode: TTreeNode): Boolean;
  function FindOPLink(const ADependency: TPkgDependencyID): TPackageLink;
  function OPNote(ADep: TPkgDependencyID): string;
  function OpmAddOrOpenDependency(ADep: TPkgDependencyID): Boolean;
  procedure OpmInstallPendingDependencies;


implementation

function GetNodeData(TVNode: TTreeNode): TPENodeData;
var
  o: TObject;
begin
  Result:=nil;
  if TVNode=nil then exit;
  o:=TObject(TVNode.Data);
  if o is TFileNameItem then
    o:=TObject(TFileNameItem(o).Data);
  if o is TPENodeData then
    Result:=TPENodeData(o);
end;

function NodeTreeIsIn(xIterNode, xParentNode: TTreeNode): Boolean;
// Is xIterNode under xParentNode, or the xParentNode itself?
begin
  Result := (xIterNode = xParentNode);
  if not Result and Assigned(xIterNode) then
    Result := NodeTreeIsIn(xIterNode.Parent, xParentNode);
end;

function FindOPLink(const ADependency: TPkgDependencyID): TPackageLink;
var
  PackageLink: TPackageLink;
begin
  Result := nil;
  if OPMInterface = Nil then Exit;
  PackageLink := LazPackageLinks.FindLinkWithPkgName(ADependency.AsString);
  if Assigned(PackageLink) and (PackageLink.Origin = ploOnline)
  and (ADependency.IsCompatible(PackageLink.Version)) then
  begin
    ADependency.LoadPackageResult := lprAvailableOnline;
    Result := PackageLink;
  end;
end;

function OPNote(ADep: TPkgDependencyID): string;
// Returns a note about online package's availability, if there is one.
begin
  if (ADep.LoadPackageResult<>lprSuccess) and (FindOPLink(ADep)<>nil) then
    Result:=' '+lisPckEditAvailableOnline
  else
    Result:='';
end;

// The following 2 functions are a pair.
// The first may add online package names to a list and the second downloads them.
var
  OpmPkgLinks: TList = Nil;

function OpmAddOrOpenDependency(ADep: TPkgDependencyID): Boolean;
var
  PackageLink: TPackageLink;
begin
  Result := True;
  if aDep.DependencyType <> pdtLazarus then Exit;
  case aDep.LoadPackageResult of
    lprSuccess:
      if PackageEditingInterface.DoOpenPackageWithName(aDep.PackageName,[],false)<>mrOk then
        Exit(False);
    lprAvailableOnline:
      begin    //  Install
        PackageLink := FindOPLink(ADep);
        if Assigned(PackageLink) then
        begin
          if OpmPkgLinks = Nil then
            OpmPkgLinks := TList.Create;
          OpmPkgLinks.Add(PackageLink);
        end;
      end;
    else begin   // lprNotFound
      if Assigned(OPMInterface) and not OPMInterface.IsPackageListLoaded then
        OPMInterface.GetPackageList;
      PackageLink := FindOPLink(ADep); // Sets lprAvailableOnline flag if package found.
    end;
  end;
end;

procedure OpmInstallPendingDependencies;
begin
  if Assigned(OpmPkgLinks) then
  try
    if OPMInterface.InstallPackages(OpmPkgLinks) = mrRetry then
      MainIDEInterface.DoBuildLazarus([]); // mrRetry means IDE must be rebuilt.
  finally
    FreeAndNil(OpmPkgLinks);
  end;
end;

{ TProjPackFilePropGui }

constructor TProjPackFilePropGui.Create(aOwner: TWinControl; aPackageGui: Boolean);
begin
  fOwner := aOwner;
  // ImageIndexes to be used later.
  ImageIndexRequired        := IDEImages.LoadImage('pkg_required');
  ImageIndexRemovedRequired := IDEImages.LoadImage('pkg_removedrequired');
  ImageIndexConflict        := IDEImages.LoadImage('pkg_conflict');
  ImageIndexAvailableOnline := IDEImages.LoadImage('pkg_install');

  if aPackageGui then
  begin
    // file properties (not used for project files)
    // ---------------
    CallRegisterProcCheckBox := TCheckBox.Create(fOwner);
    CallRegisterProcCheckBox.Parent := fOwner;
    CallRegisterProcCheckBox.Left := 6;
    CallRegisterProcCheckBox.Top := 0;
    CallRegisterProcCheckBox.Width := 185;
    CallRegisterProcCheckBox.ShowHint := True;
    CallRegisterProcCheckBox.TabOrder := 0;
    CallRegisterProcCheckBox.Caption := lisPckEditRegisterUnit;
    CallRegisterProcCheckBox.Hint := Format(lisPckEditCallRegisterProcedureOfSelectedUnit, ['"', '"']);

    AddToUsesPkgSectionCheckBox := TCheckBox.Create(fOwner);
    AddToUsesPkgSectionCheckBox.Parent := fOwner;
    AddToUsesPkgSectionCheckBox.AnchorSideLeft.Control := CallRegisterProcCheckBox;
    AddToUsesPkgSectionCheckBox.AnchorSideLeft.Side := asrBottom;
    AddToUsesPkgSectionCheckBox.Left := 195;
    AddToUsesPkgSectionCheckBox.Top := 0;
    AddToUsesPkgSectionCheckBox.Width := 222;
    AddToUsesPkgSectionCheckBox.BorderSpacing.Left := 10;
    AddToUsesPkgSectionCheckBox.ShowHint := True;
    AddToUsesPkgSectionCheckBox.TabOrder := 1;
    AddToUsesPkgSectionCheckBox.Caption := lisPkgMangUseUnit;
    AddToUsesPkgSectionCheckBox.Hint := lisPkgMangAddUnitToUsesClause;

    RegisteredPluginsGroupBox := TGroupBox.Create(fOwner);
    RegisteredPluginsGroupBox.Parent := fOwner;
    RegisteredPluginsGroupBox.Left := 3;
    RegisteredPluginsGroupBox.Height := 165;
    RegisteredPluginsGroupBox.Top := 27;
    RegisteredPluginsGroupBox.Width := 452;
    RegisteredPluginsGroupBox.Align := alBottom;
    RegisteredPluginsGroupBox.Anchors := [akTop, akLeft, akRight, akBottom];
    RegisteredPluginsGroupBox.BorderSpacing.Top := 6;
    RegisteredPluginsGroupBox.TabOrder := 7;
    RegisteredPluginsGroupBox.Caption := lisPckEditRegisteredPlugins;

    RegisteredListBox := TListBox.Create(fOwner);
    RegisteredListBox.Parent := RegisteredPluginsGroupBox;
    RegisteredListBox.Align := alClient;
    RegisteredListBox.ScrollWidth := 448;
    RegisteredListBox.Style := lbOwnerDrawFixed;
    RegisteredListBox.TabOrder := 0;
    RegisteredListBox.ItemHeight := ComponentPaletteImageHeight;

    // I18N
    DisableI18NForLFMCheckBox := TCheckBox.Create(fOwner);
    DisableI18NForLFMCheckBox.Parent := fOwner;
    DisableI18NForLFMCheckBox.AnchorSideLeft.Control := AddToUsesPkgSectionCheckBox;
    DisableI18NForLFMCheckBox.AnchorSideLeft.Side := asrBottom;
    DisableI18NForLFMCheckBox.AnchorSideTop.Control := AddToUsesPkgSectionCheckBox;
    DisableI18NForLFMCheckBox.Left := 423;
    DisableI18NForLFMCheckBox.Top := 0;
    DisableI18NForLFMCheckBox.Width := 208;
    DisableI18NForLFMCheckBox.BorderSpacing.Left := 10;
    DisableI18NForLFMCheckBox.ShowHint := True;
    DisableI18NForLFMCheckBox.TabOrder := 8;
    DisableI18NForLFMCheckBox.Caption := lisPckDisableI18NOfLfm;
    DisableI18NForLFMCheckBox.Hint := lisPckWhenTheFormIsSavedTheIDECanStoreAllTTranslateString;
  end;

  // dependency properties
  // ---------------------
  UseMinVersionCheckBox := TCheckBox.Create(fOwner);
  UseMinVersionCheckBox.Parent := fOwner;
  UseMinVersionCheckBox.AnchorSideTop.Control := MinVersionEdit;
  UseMinVersionCheckBox.AnchorSideTop.Side := asrCenter;
  UseMinVersionCheckBox.Left := 6;
  UseMinVersionCheckBox.Top := 6;
  UseMinVersionCheckBox.Width := 179;
  UseMinVersionCheckBox.TabOrder := 2;
  UseMinVersionCheckBox.Caption := lisPckEditMinimumVersion;
  UseMinVersionCheckBox.OnChange := @UseMinVersionCheckBoxChange;

  MinVersionEdit := TEdit.Create(fOwner);
  MinVersionEdit.Parent := fOwner;
  MinVersionEdit.AnchorSideLeft.Control := UseMinVersionCheckBox;
  MinVersionEdit.AnchorSideLeft.Side := asrBottom;
  MinVersionEdit.Left := 201;
  MinVersionEdit.Top := 0;
  MinVersionEdit.Width := 100;
  MinVersionEdit.BorderSpacing.Left := 10;
  MinVersionEdit.TabOrder := 3;
  MinVersionEdit.OnChange := @MinMaxVersionEditChange;

  UseMaxVersionCheckBox := TCheckBox.Create(fOwner);
  UseMaxVersionCheckBox.Parent := fOwner;
  UseMaxVersionCheckBox.AnchorSideTop.Control := MaxVersionEdit;
  UseMaxVersionCheckBox.AnchorSideTop.Side := asrCenter;
  UseMaxVersionCheckBox.Left := 6;
  UseMaxVersionCheckBox.Top := 43;
  UseMaxVersionCheckBox.Width := 182;
  UseMaxVersionCheckBox.TabOrder := 4;
  UseMaxVersionCheckBox.Caption := lisPckEditMaximumVersion;
  UseMaxVersionCheckBox.OnChange := @UseMaxVersionCheckBoxChange;

  MaxVersionEdit := TEdit.Create(fOwner);
  MaxVersionEdit.Parent := fOwner;
  MaxVersionEdit.AnchorSideLeft.Control := UseMaxVersionCheckBox;
  MaxVersionEdit.AnchorSideLeft.Side := asrBottom;
  MaxVersionEdit.AnchorSideTop.Control := MinVersionEdit;
  MaxVersionEdit.AnchorSideTop.Side := asrBottom;
  MaxVersionEdit.Left := 204;
  MaxVersionEdit.Top := 38;
  MaxVersionEdit.Width := 100;
  MaxVersionEdit.BorderSpacing.Left := 10;
  MaxVersionEdit.BorderSpacing.Top := 2;
  MaxVersionEdit.TabOrder := 5;
  MaxVersionEdit.OnChange := @MinMaxVersionEditChange;

  ApplyDependencyButton := TButton.Create(fOwner);
  ApplyDependencyButton.Parent := fOwner;
  ApplyDependencyButton.AnchorSideTop.Control := MaxVersionEdit;
  ApplyDependencyButton.AnchorSideTop.Side := asrBottom;
  ApplyDependencyButton.Left := 6;
  ApplyDependencyButton.Top := 80;
  ApplyDependencyButton.AutoSize := True;
  ApplyDependencyButton.BorderSpacing.Top := 6;
  ApplyDependencyButton.TabOrder := 6;
  ApplyDependencyButton.Caption := lisPckEditApplyChanges;
end;

destructor TProjPackFilePropGui.Destroy;
var
  nt: TPENodeType;
begin
  for nt:=Low(TPENodeType) to High(TPENodeType) do
    FreeNodeData(nt);
  inherited Destroy;
end;

function TProjPackFilePropGui.CreateNodeData(Typ: TPENodeType; aName: string;
  aRemoved: boolean): TPENodeData;
begin
  Result:=TPENodeData.Create(Typ,aName,aRemoved);
  Result.Next:=FNodeDataList[Typ];
  FNodeDataList[Typ]:=Result;
end;

procedure TProjPackFilePropGui.FreeNodeData(Typ: TPENodeType);
var
  NodeData, n: TPENodeData;
begin
  NodeData:=FNodeDataList[Typ];
  while NodeData<>nil do begin
    n:=NodeData;
    NodeData:=NodeData.Next;
    if Assigned(n.Branch) Then
      n.Branch.FreeNodeData(n.Node);
    n.Free;
  end;
  FNodeDataList[Typ]:=nil;
end;

function TProjPackFilePropGui.GetDependencyImageIndex(aDep: TPkgDependencyID): Integer;
begin
  if aDep.Removed then
    Result := ImageIndexRemovedRequired
  else if aDep.LoadPackageResult=lprSuccess then
    Result := ImageIndexRequired
  else if aDep.LoadPackageResult=lprAvailableOnline {Assigned(FindOPLink(aDep))} then
    Result := ImageIndexAvailableOnline
  else
    Result := ImageIndexConflict;
end;

procedure SetCheckBox(Box: TCheckBox; aVisible: boolean; State: TMultiBool);
begin
  Box.Visible:=aVisible;
  case State of
  mubAllTrue:
    begin
      Box.State:=cbChecked;
      Box.AllowGrayed:=false;
    end;
  mubAllFalse:
    begin
      Box.State:=cbUnchecked;
      Box.AllowGrayed:=false;
    end;
  mubMixed:
    begin
      Box.State:=cbGrayed;
      Box.AllowGrayed:=true;
    end;
  end;
end;

procedure TProjPackFilePropGui.SetAddToUsesCB(State: TMultiBool);
begin
  SetCheckBox(AddToUsesPkgSectionCheckBox, ControlVisible, State);
  AddToUsesPkgSectionCheckBox.Enabled := ControlEnabled;
end;

procedure TProjPackFilePropGui.SetCallRegisterProcCB(State: TMultiBool);
begin
  SetCheckBox(CallRegisterProcCheckBox, ControlVisible, State);
  CallRegisterProcCheckBox.Enabled := ControlEnabled;
end;

procedure TProjPackFilePropGui.SetRegisteredPluginsGB(aPlugins: TStringList);
begin
  RegisteredPluginsGroupBox.Visible := ControlVisible;
  RegisteredPluginsGroupBox.Enabled := ControlEnabled;
  if not ControlVisible then
    aPlugins.Clear;
  RegisteredListBox.Items.Assign(aPlugins);
end;

procedure TProjPackFilePropGui.SetMinMaxVisibility;
begin
  UseMinVersionCheckBox.Visible := ControlVisible;
  MinVersionEdit.Visible := ControlVisible;
  UseMaxVersionCheckBox.Visible := ControlVisible;
  MaxVersionEdit.Visible := ControlVisible;
  ApplyDependencyButton.Visible := ControlVisible;
end;

procedure TProjPackFilePropGui.SetMinMaxValues(aDep: TPkgDependencyID);
begin
  UseMinVersionCheckBox.Checked := pdfMinVersion in aDep.Flags;
  MinVersionEdit.Text := aDep.MinVersion.AsString;
  MinVersionEdit.Enabled := pdfMinVersion in aDep.Flags;
  UseMaxVersionCheckBox.Checked := pdfMaxVersion in aDep.Flags;
  MaxVersionEdit.Text := aDep.MaxVersion.AsString;
  MaxVersionEdit.Enabled := pdfMaxVersion in aDep.Flags;
end;

procedure TProjPackFilePropGui.SetDisableI18NCB(State: TMultiBool);
begin
  SetCheckBox(DisableI18NForLFMCheckBox, ControlVisible, State);
  DisableI18NForLFMCheckBox.Enabled := ControlEnabled;
end;

function TProjPackFilePropGui.CheckApplyDependency(aDep: TPkgDependencyID): Boolean;
var
  Flags: TPkgDependencyFlags;
  MinVers, MaxVers: TPkgVersion;
begin
  Result := False;
  MinVers:=TPkgVersion.Create;
  MaxVers:=TPkgVersion.Create;
  try
    // Assign relevant data to temp variables
    Flags:=aDep.Flags;
    MinVers.Assign(aDep.MinVersion);
    MaxVers.Assign(aDep.MinVersion);

    // read minimum version
    if UseMinVersionCheckBox.Checked then begin
      Include(Flags, pdfMinVersion);
      if not MinVers.ReadString(MinVersionEdit.Text) then begin
        MessageDlg(lisPckEditInvalidMinimumVersion,
          Format(lisPckEditTheMinimumVersionIsNotAValidPackageVersion,
                 [MinVersionEdit.Text, LineEnding]),
          mtError,[mbCancel],0);
        exit;
      end;
    end
    else
      Exclude(Flags, pdfMinVersion);

    // read maximum version
    if UseMaxVersionCheckBox.Checked then begin
      Include(Flags, pdfMaxVersion);
      if not MaxVers.ReadString(MaxVersionEdit.Text) then begin
        MessageDlg(lisPckEditInvalidMaximumVersion,
          Format(lisPckEditTheMaximumVersionIsNotAValidPackageVersion,
                 [MaxVersionEdit.Text, LineEnding]),
          mtError,[mbCancel],0);
        exit;
      end;
    end
    else
      Exclude(Flags, pdfMaxVersion);

    // Assign changes back to the dependency
    aDep.Flags := Flags;
    aDep.MinVersion.Assign(MinVers);
    aDep.MaxVersion.Assign(MaxVers);
    Result := True;
  finally
    MaxVers.Free;
    MinVers.Free;
  end;
end;

procedure TProjPackFilePropGui.UpdateApplyDependencyButton(Immediately: boolean);
var
  DependencyChanged: Boolean;
  AVersion: TPkgVersion;
  CurDependency: TPkgDependencyID;
begin
  Assert(Assigned(OnGetPkgDep), 'UpdateApplyDependencyButton: OnPkgDepToUpdate = Nil.');
  CurDependency := OnGetPkgDep(Immediately);
  DependencyChanged := false;
  if Assigned(CurDependency) then
  begin
    // check min version
    if UseMinVersionCheckBox.Checked <> (pdfMinVersion in CurDependency.Flags) then
      DependencyChanged := true;
    if UseMinVersionCheckBox.Checked then begin
      AVersion := TPkgVersion.Create;
      if AVersion.ReadString(MinVersionEdit.Text)
      and (AVersion.Compare(CurDependency.MinVersion)<>0) then
        DependencyChanged := true;
      AVersion.Free;
    end;
    // check max version
    if UseMaxVersionCheckBox.Checked <> (pdfMaxVersion in CurDependency.Flags) then
      DependencyChanged := true;
    if UseMaxVersionCheckBox.Checked then begin
      AVersion := TPkgVersion.Create;
      if AVersion.ReadString(MaxVersionEdit.Text)
      and (AVersion.Compare(CurDependency.MaxVersion)<>0) then
        DependencyChanged := true;
      AVersion.Free;
    end;
  end;
  ApplyDependencyButton.Enabled := DependencyChanged;
end;

procedure TProjPackFilePropGui.MinMaxVersionEditChange(Sender: TObject);
begin
  UpdateApplyDependencyButton;
end;

procedure TProjPackFilePropGui.UseMinVersionCheckBoxChange(Sender: TObject);
begin
  MinVersionEdit.Enabled := UseMinVersionCheckBox.Checked;
  UpdateApplyDependencyButton;
end;

procedure TProjPackFilePropGui.UseMaxVersionCheckBoxChange(Sender: TObject);
begin
  MaxVersionEdit.Enabled := UseMaxVersionCheckBox.Checked;
  UpdateApplyDependencyButton;
end;

end.

