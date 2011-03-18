{ Copyright (C) 2011,

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

  Original version by Juha Manninen
  Icons added by Marcelo B Paula
}
unit UseProjUnitDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls, Buttons,
  ButtonPanel, Dialogs, LCLProc,
  SrcEditorIntf, LazIDEIntf, IDEImagesIntf, LazarusIDEStrConsts,
  ProjectIntf, Project, CodeCache, CodeToolManager;

type

  { TUseProjUnitDialog }

  TUseProjUnitDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    UnitsListBox: TListBox;
    SectionRadioGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure UnitsListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    UnitImgInd: Integer;
    procedure AddItems(AItems: TStrings);
    procedure SelectFirst;
    function SelectedUnit: string;
    function InterfaceSelected: Boolean;
  public

  end; 

  function ShowUseProjUnitDialog: TModalResult;

implementation

{$R *.lfm}

function ShowUseProjUnitDialog: TModalResult;
var
  UseProjUnitDlg: TUseProjUnitDialog;
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  ProjFile: TUnitInfo;
  MainUsedUnits, ImplUsedUnits: TStrings;
  AvailUnits: TStringList;
  CurrentUnitName, s: String;
  CTRes: Boolean;
begin
  Result:=mrOk;
  if not LazarusIDE.BeginCodeTools then exit;
  // get cursor position
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;
  Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
  if Code=nil then exit;
  CurrentUnitName:='';
  MainUsedUnits:=nil;
  ImplUsedUnits:=nil;
  AvailUnits:=TStringList.Create;
  try
    if not CodeToolBoss.FindUsedUnitNames(Code,MainUsedUnits,ImplUsedUnits) then begin
      DebugLn(['ShowUseProjUnitDialog CodeToolBoss.FindUsedUnitNames failed']);
      LazarusIDE.DoJumpToCodeToolBossError;
      exit(mrCancel);
    end;
    TStringList(MainUsedUnits).CaseSensitive:=False;
    TStringList(ImplUsedUnits).CaseSensitive:=False;
    // Add available unit names to AvailUnits.
    ProjFile:=Project1.FirstPartOfProject;
    while ProjFile<>nil do begin
      s:=ProjFile.Unit_Name;
      // "GetProjectFile.Unit_NameUnit_Name" returns character case matching with
      // "ProjFile.Unit_Name" character case when inside this loop.
      // Earlier it returns a name extracted from file name. Some clever cache feature.
      if s=TUnitInfo(SrcEdit.GetProjectFile).Unit_Name then begin
        CurrentUnitName:=s;
        s:='';             // current unit
      end;
      if (ProjFile <> Project1.MainUnitInfo) and (s <> '') then
        if (MainUsedUnits.IndexOf(s)<0) and (ImplUsedUnits.IndexOf(s)<0) then
          AvailUnits.Add(s);
      ProjFile:=ProjFile.NextPartOfProject;
    end;
    // Show the dialog.
    if AvailUnits.Count>0 then begin
      AvailUnits.Sorted:=True;
      UseProjUnitDlg:=TUseProjUnitDialog.Create(nil);
      try
        UseProjUnitDlg.AddItems(AvailUnits);
        UseProjUnitDlg.SelectFirst;
        if SrcEdit.GetProjectFile = Project1.MainUnitInfo then
        begin
          // there is only main uses section in program/library/package
          UseProjUnitDlg.SectionRadioGroup.ItemIndex := 0;
          UseProjUnitDlg.SectionRadioGroup.Enabled := False;
        end;
        if UseProjUnitDlg.ShowModal=mrOk then begin
          s:=UseProjUnitDlg.SelectedUnit;
          if s<>'' then begin
            if UseProjUnitDlg.InterfaceSelected then
              CTRes:=CodeToolBoss.AddUnitToMainUsesSection(Code, s, '')
            else
              CTRes:=CodeToolBoss.AddUnitToImplementationUsesSection(Code, s, '');
            if not CTRes then begin
              LazarusIDE.DoJumpToCodeToolBossError;
              exit(mrCancel);
            end;
          end;
        end;
      finally
        UseProjUnitDlg.Free;
      end;
    end
    else
      ShowMessage(Format(dlgUnitAlreadyUsesAllOtherUnits,[CurrentUnitName]));
  finally
    CodeToolBoss.SourceCache.ClearAllSourceLogEntries;
    ImplUsedUnits.Free;
    MainUsedUnits.Free;
    AvailUnits.Free;
  end;
end;

{ TUseProjUnitDialog }

procedure TUseProjUnitDialog.FormCreate(Sender: TObject);
begin
  // Internationalization
  Caption := dlgUseUnitCaption;
  SectionRadioGroup.Caption := dlgInsertSection;
  SectionRadioGroup.Items.Clear;
  SectionRadioGroup.Items.Add(dlgInsertInterface);
  SectionRadioGroup.Items.Add(dlgInsertImplementation);
  SectionRadioGroup.ItemIndex:=1;
  ButtonPanel1.OKButton.Caption:=lisOk;
  ButtonPanel1.CancelButton.Caption:=dlgCancel;
  UnitImgInd := IDEImages.LoadImage(16, 'item_unit');
end;

procedure TUseProjUnitDialog.UnitsListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  if Index < 0 then Exit;
  UnitsListBox.Canvas.FillRect(ARect);
  IDEImages.Images_16.Draw(UnitsListBox.Canvas,1,ARect.Top,UnitImgInd);
  UnitsListBox.Canvas.TextRect(ARect, ARect.Left + 20,ARect.Top,
                                UnitsListBox.Items[Index]);
end;

procedure TUseProjUnitDialog.AddItems(AItems: TStrings);
begin
  UnitsListBox.Items.Assign(AItems);
end;

procedure TUseProjUnitDialog.SelectFirst;
begin
  UnitsListBox.Selected[0]:=True;
end;

function TUseProjUnitDialog.SelectedUnit: string;
begin
  Result:=UnitsListBox.Items[UnitsListBox.ItemIndex];
end;

function TUseProjUnitDialog.InterfaceSelected: Boolean;
begin
  Result:=SectionRadioGroup.ItemIndex=0;
end;


end.

