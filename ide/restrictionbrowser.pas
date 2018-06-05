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

  Author: Tomas Gregorovic

  Abstract:
    Browser for widget set restricted properties.
}
unit RestrictionBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs,
  // LCL
  LCLPlatformDef, LCLProc, Forms, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  // LazControls
  TreeFilterEdit,
  // IdeIntf
  IDEImagesIntf,
  // IDE
  CompatibilityRestrictions, IDEOptionDefs, LazarusIDEStrConsts;

type
  { TRestrictionBrowserView }

  TRestrictionBrowserView = class(TForm)
    FilterEdit: TTreeFilterEdit;
    IssueFilterGroupBox: TGroupBox;
    IssueMemo: TMemo;
    IssueTreeView: TTreeView;
    NameLabel: TLabel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure IssueTreeViewSelectionChanged(Sender: TObject);
    procedure NameFilterEditChange(Sender: TObject);
  private
    FIssueList: TRestrictedList;
    FClasses: TClassList;
    procedure SelectFirstVisible(Sender: TObject);
    procedure GetComponentClass(const AClass: TComponentClass);
    procedure UpdateIssueList;
  public
    procedure SetIssueName(const AIssueName: String);
  end;
  
var
  RestrictionBrowserView: TRestrictionBrowserView = nil;

implementation

{$R *.lfm}

{ TRestrictionBrowserView }

procedure TRestrictionBrowserView.FormCreate(Sender: TObject);
var
  P: TLCLPlatform;
  X: Integer;
begin
  FIssueList := GetRestrictedList;
  Name := NonModalIDEWindowNames[nmiwIssueBrowser];
  Caption := lisMenuViewRestrictionBrowser;
  IssueFilterGroupBox.Caption := lisIssues;
  NameLabel.Caption := lisCodeToolsDefsName;
  IssueTreeView.Images := IDEImages.Images_16;
  X := 10;
  // create widget set filter buttons
  for P := Low(TLCLPlatform) to High(TLCLPlatform) do
  begin
    with TSpeedButton.Create(Self) do
    begin
      Name := 'SpeedButton' + LCLPlatformDirNames[P];
      Left := X;
      Top := 4;
      Width := 24;
      Height := 24;
      GroupIndex := Integer(P) + 1;
      Down := True;
      AllowAllUp := True;
      Images := IDEImages.Images_16;
      ImageIndex := IDEImages.LoadImage('issue_'+LCLPlatformDirNames[P]);
      if ImageIndex<0 then
        DebugLn('Restriction Browser: Unable to load image for ' + LCLPlatformDirNames[P] + '!');
      ShowHint := True;
      Hint := LCLPlatformDisplayNames[P];
      OnClick := @NameFilterEditChange;
      Parent := IssueFilterGroupBox;
      Inc(X, Width);
    end;
  end;
  FilterEdit.OnAfterFilter := @SelectFirstVisible;
  UpdateIssueList;
end;

procedure TRestrictionBrowserView.SelectFirstVisible(Sender: TObject);
var
  nd: TTreeNode;
begin
  nd := IssueTreeView.Items.GetFirstVisibleNode;
  if Assigned(nd) then
    IssueTreeView.Selected := nd
  else
    IssueMemo.Clear;
end;

procedure TRestrictionBrowserView.IssueTreeViewSelectionChanged(Sender: TObject);
var
  Issue: TRestriction;
begin
  if Assigned(IssueTreeView.Selected) then
  begin
    Issue := PRestriction(IssueTreeView.Selected.Data)^;
    IssueMemo.Text := Issue.Short + LineEnding + LineEnding + Issue.Description;
  end
  else
    IssueMemo.Clear;
end;

procedure TRestrictionBrowserView.NameFilterEditChange(Sender: TObject);
begin
  UpdateIssueList;
end;

procedure TRestrictionBrowserView.GetComponentClass(const AClass: TComponentClass);
begin
  FClasses.Add(AClass);
end;

procedure TRestrictionBrowserView.UpdateIssueList;
var
  I, ID: PtrInt;
  Issues: TStringList;
  P: TLCLPlatform;
  WidgetSetFilter: TLCLPlatforms;
  Component: TComponent;
begin
  WidgetSetFilter := [];
  for P := Low(TLCLPlatform) to High(TLCLPlatform) do
  begin
    Component := FindComponent('SpeedButton' + LCLPlatformDirNames[P]);
    Assert(Component is TSpeedButton, 'Component '+Component.Name+' is not TSpeedButton');
    if (Component as TSpeedButton).Down then
      Include(WidgetSetFilter, P);
  end;
  Issues := TStringList.Create;
  try
    for I := 0 to High(FIssueList) do
      if FIssueList[I].WidgetSet in WidgetSetFilter then
        Issues.AddObject(FIssueList[I].Name, TObject(I));
    Issues.Sort;
    IssueTreeView.BeginUpdate;
    try
      IssueTreeView.Items.Clear;
      for I := 0 to Issues.Count - 1 do
      begin
        with IssueTreeView.Items.AddChild(nil, Issues[I]) do
        begin
          ID := PtrInt(Issues.Objects[I]);
          ImageIndex := IDEImages.LoadImage(
              'issue_'+LCLPlatformDirNames[FIssueList[ID].WidgetSet]);
          StateIndex := ImageIndex;
          SelectedIndex := ImageIndex;
          Data := @FIssueList[ID];
        end;
      end;
    finally
      IssueTreeView.EndUpdate;
    end;
  finally
    Issues.Free;
  end;
  FilterEdit.InvalidateFilter;
end;

procedure TRestrictionBrowserView.SetIssueName(const AIssueName: String);
var
  P: TLCLPlatform;
  Component: TComponent;
begin
  FilterEdit.Text := AIssueName;
  if AIssueName <> '' then
  begin
    for P := Low(TLCLPlatform) to High(TLCLPlatform) do
    begin
      Component := FindComponent('SpeedButton' + LCLPlatformDirNames[P]);
      Assert(Component is TSpeedButton, 'Component '+Component.Name+' is not TSpeedButton');
      (Component as TSpeedButton).Down := True;
    end;
  end;
  UpdateIssueList;
end;

end.

