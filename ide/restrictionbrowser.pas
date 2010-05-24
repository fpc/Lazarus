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

  Author: Tomas Gregorovic

  Abstract:
    Browser for widget set restricted properties.
}
unit RestrictionBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, InterfaceBase, LCLProc, Contnrs, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ComCtrls, Masks, ExtCtrls, Buttons,
  IDEImagesIntf, ObjectInspector,
  CompatibilityRestrictions, IDEOptionDefs, LazarusIDEStrConsts,
  EnvironmentOpts, ComponentReg, LazConf;

type
  { TRestrictionBrowserView }

  TRestrictionBrowserView = class(TForm)
    NameFilterEdit: TEdit;
    IssueFilterGroupBox: TGroupBox;
    IssueMemo: TMemo;
    NameLabel: TLabel;
    IssueTreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure IssueTreeViewSelectionChanged(Sender: TObject);
    procedure NameFilterEditChange(Sender: TObject);
  private
    FIssueList: TRestrictedList;
    FClasses: TClassList;
    FCanUpdate: Boolean;
    procedure GetComponentClass(const AClass: TComponentClass);
  public
    procedure UpdateIssueList;
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

  IssueFilterGroupBox.Caption := lisFilter;
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
      
      try
        IDEImages.Images_16.GetBitmap(
               IDEImages.LoadImage(16, 'issue_'+LCLPlatformDirNames[P]), Glyph);
      except
        DebugLn('Restriction Browser: Unable to load image for ' + LCLPlatformDirNames[P] + '!');
      end;
      
      ShowHint := True;
      Hint := LCLPlatformDisplayNames[P];
      OnClick := @NameFilterEditChange;
      
      Parent := IssueFilterGroupBox;
      Inc(X, Width);
    end;
  end;
  
  FCanUpdate := True;
  UpdateIssueList;
end;
procedure TRestrictionBrowserView.IssueTreeViewSelectionChanged(Sender: TObject);
var
  Issue: TRestriction;
begin
  if IssueTreeView.Selected = nil then
  begin
    IssueMemo.Clear;
    Exit;
  end;

  Issue := PRestriction(IssueTreeView.Selected.Data)^;
  IssueMemo.Text := Issue.Short + LineEnding + LineEnding + Issue.Description;
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
  IssueClass: String;
  IssueProperty: String;
  IssueMask: TMaskList;
  S, M: String;
  I, ID: PtrInt;
  Issues: TStringList;
  Issue: TRestriction;
  C: TClass;
  AddParentClass: Boolean;
  P: TLCLPlatform;
  WidgetSetFilter: TLCLPlatforms;
  Component: TComponent;
begin
  if not FCanUpdate then Exit;
  S := Trim(NameFilterEdit.Text);
  IssueClass := '';
  IssueProperty := '';
  
  WidgetSetFilter := [];
  for P := Low(TLCLPlatform) to High(TLCLPlatform) do
  begin
    Component := FindComponent('SpeedButton' + LCLPlatformDirNames[P]);
    if Component is TSpeedButton then
      if (Component as TSpeedButton).Down then Include(WidgetSetFilter, P);
  end;
  
  I := Pos('.', S);
  if I = 0 then IssueClass := S
  else
  begin
    IssueClass := Copy(S, 0, I - 1);
    IssueProperty := Copy(S, I + 1, MaxInt);
  end;
  
  if (IssueProperty = '') and (IssueClass = '') then
    M := '*'
  else
  begin
    if IssueClass = '' then
      M := '*.' + IssueProperty + '*'
    else
    begin
      // find parent classes
      M := '';
      FClasses := TClassList.Create;
      try
        IDEComponentPalette.IterateRegisteredClasses(@GetComponentClass);
        
        FClasses.Add(TCustomForm);
        FClasses.Add(TForm);
        FClasses.Add(TDataModule);
        FClasses.Add(TFrame);
        
        for I := 0 to FClasses.Count - 1 do
        begin
          C := FClasses[I];
          AddParentClass := False;
          while C <> nil do
          begin
            if AddParentClass or (Copy(C.ClassName, 0, Length(IssueClass)) = IssueClass) then
            begin
              if M <> '' then M := M + ';';
              M := M + C.ClassName + ';' + C.ClassName + '.' + IssueProperty + '*';
              AddParentClass := True;
            end;
            
            C := C.ClassParent;
          end;
        end;
        
        if FClasses.Count = 0 then
          M := IssueClass + '*;' + IssueClass + '*.' + IssueProperty + '*';
          
        if (Copy('TWidgetSet', 0, Length(IssueClass)) = IssueClass) then
          M := M + ';TWidgetSet';
      finally
        FClasses.Free;
      end;
    end;
  end;
  
  IssueMask := TMaskList.Create(M);
  Issues := TStringList.Create;
  try
    for I := 0 to High(FIssueList) do
    begin
      Issue := FIssueList[I];
      if Issue.WidgetSet in WidgetSetFilter then
        if IssueMask.Matches(Issue.Name)  then
          Issues.AddObject(Issue.Name, TObject(I));
    end;
    
    Issues.Sort;

    IssueTreeView.BeginUpdate;
    try
      IssueTreeView.Items.Clear;
      
      for I := 0 to Issues.Count - 1 do
      begin
        with IssueTreeView.Items.AddChild(nil, Issues[I]) do
        begin
          ID := PtrInt(Issues.Objects[I]);
          
          ImageIndex := IDEImages.LoadImage(16,
              'issue_'+LCLPlatformDirNames[FIssueList[ID].WidgetSet]);
          StateIndex := ImageIndex;
          SelectedIndex := ImageIndex;
          
          Data := @FIssueList[ID];
        end;
        if NameFilterEdit.Text = Issues[I] then
        begin
          IssueTreeView.Selected := IssueTreeView.Items[I];
        end;
      end;
    finally
      IssueTreeView.EndUpdate;
    end;
  finally
    Issues.Free;
    IssueMask.Free;
  end;
  
  if IssueTreeView.Items.Count > 0 then
  begin
    if IssueTreeView.Selected = nil then
      IssueTreeView.Selected := IssueTreeView.Items[0];
  end
  else
    IssueMemo.Clear;
end;

procedure TRestrictionBrowserView.SetIssueName(const AIssueName: String);
var
  P: TLCLPlatform;
  Component: TComponent;
begin
  FCanUpdate := False;
  try
    NameFilterEdit.Text := AIssueName;

    if AIssueName <> '' then
    begin
      for P := Low(TLCLPlatform) to High(TLCLPlatform) do
      begin
        Component := FindComponent('SpeedButton' + LCLPlatformDirNames[P]);
        if Component is TSpeedButton then
          (Component as TSpeedButton).Down := True;
      end;
    end;
  finally
    FCanUpdate := True;
    UpdateIssueList;
  end;
end;

end.

