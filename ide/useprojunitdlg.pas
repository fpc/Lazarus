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
  ButtonPanel, Dialogs, LCLProc, FileProcs,
  SourceEditor, LazIDEIntf, IDEImagesIntf, LazarusIDEStrConsts,
  ProjectIntf, Project, CodeCache, CodeToolManager;

type

  { TUseProjUnitDialog }

  TUseProjUnitDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    UnitsListBox: TListBox;
    SectionRadioGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure UnitsListBoxDblClick(Sender: TObject);
    procedure UnitsListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    UnitImgInd: Integer;
    procedure AddItems(AItems: TStrings);
    procedure SelectFirst;
    function SelectedUnit: string;
    function InterfaceSelected: Boolean;
    procedure EnableOnlyInterface;
  public

  end; 

function GetAvailableUnits(SrcEdit: TSourceEditor; out CurrentUnitName: String;
                           IgnoreErrors: boolean): TStringList;
function ShowUseProjUnitDialog: TModalResult;

implementation

{$R *.lfm}

function GetAvailableUnits(SrcEdit: TSourceEditor; out CurrentUnitName: String;
  IgnoreErrors: boolean): TStringList;
var
  MainUsedUnits, ImplUsedUnits: TStrings;
  ProjFile: TUnitInfo;
  s: String;
begin
  MainUsedUnits:=nil;
  ImplUsedUnits:=nil;
  Result:=nil;
  try
    if SrcEdit=nil then exit;
    Assert(Assigned(SrcEdit.CodeBuffer));
    if not CodeToolBoss.FindUsedUnitNames(SrcEdit.CodeBuffer,
                                          MainUsedUnits,ImplUsedUnits)
    then begin
      if not IgnoreErrors then
      begin
        DebugLn(['ShowUseProjUnitDialog CodeToolBoss.FindUsedUnitNames failed']);
        LazarusIDE.DoJumpToCodeToolBossError;
      end;
      exit;
    end;
    Result:=TStringList.Create;  // Result TStringList must be freed by caller.
    TStringList(MainUsedUnits).CaseSensitive:=False;
    TStringList(ImplUsedUnits).CaseSensitive:=False;
    // Debug message will be cleaned soon!!!
    if SrcEdit.GetProjectFile is TUnitInfo then
      CurrentUnitName:=TUnitInfo(SrcEdit.GetProjectFile).Unit_Name
    else
      CurrentUnitName:='';
    //DebugLn('ShowUseProjUnitDialog: CurrentUnitName before loop = '+CurrentUnitName);
    // Add available unit names to Result.
    ProjFile:=Project1.FirstPartOfProject;
    while ProjFile<>nil do begin
      s:=ProjFile.Unit_Name;
      if s=CurrentUnitName then begin      // current unit
        {if SrcEdit.GetProjectFile is TUnitInfo then   // Debug!
          DebugLn('ShowUseProjUnitDialog: CurrentUnitName in loop = ' +
                      TUnitInfo(SrcEdit.GetProjectFile).Unit_Name);}
        s:='';
      end;
      if (ProjFile<>Project1.MainUnitInfo) and (s<>'') then
        if (MainUsedUnits.IndexOf(s)<0) and (ImplUsedUnits.IndexOf(s)<0) then
          Result.Add(s);
      ProjFile:=ProjFile.NextPartOfProject;
    end;
  finally
    ImplUsedUnits.Free;
    MainUsedUnits.Free;
  end;
end;

function ShowUseProjUnitDialog: TModalResult;
var
  UseProjUnitDlg: TUseProjUnitDialog;
  SrcEdit: TSourceEditor;
  AvailUnits: TStringList;
  CurrentUnitName, s: String;
  CTRes: Boolean;
begin
  Result:=mrOk;
  if not LazarusIDE.BeginCodeTools then exit;
  // get cursor position
  SrcEdit:=SourceEditorManager.ActiveEditor;
  try
    AvailUnits:=GetAvailableUnits(SrcEdit, CurrentUnitName, false);
    if AvailUnits=nil then
      exit(mrCancel);
    if AvailUnits.Count>0 then begin
      // Show the dialog.
      AvailUnits.Sorted:=True;
      UseProjUnitDlg:=TUseProjUnitDialog.Create(nil);
      try
        UseProjUnitDlg.AddItems(AvailUnits);
        UseProjUnitDlg.SelectFirst;
        // there is only main uses section in program/library/package
        if SrcEdit.GetProjectFile=Project1.MainUnitInfo then
          UseProjUnitDlg.EnableOnlyInterface;
        if UseProjUnitDlg.ShowModal=mrOk then begin
          s:=UseProjUnitDlg.SelectedUnit;
          if s<>'' then begin
            if UseProjUnitDlg.InterfaceSelected then
              CTRes:=CodeToolBoss.AddUnitToMainUsesSection(SrcEdit.CodeBuffer, s, '')
            else
              CTRes:=CodeToolBoss.AddUnitToImplementationUsesSection(SrcEdit.CodeBuffer, s, '');
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
    else begin
      if CurrentUnitName='' then
        CurrentUnitName:=ExtractFileNameOnly(SrcEdit.CodeBuffer.Filename);
      ShowMessage(Format(dlgAlreadyUsesAllOtherUnits,[CurrentUnitName]));
    end;
  finally
    CodeToolBoss.SourceCache.ClearAllSourceLogEntries;
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

procedure TUseProjUnitDialog.UnitsListBoxDblClick(Sender: TObject);
begin
  if UnitsListBox.ItemIndex >= 0 then ModalResult := mrOK;
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

procedure TUseProjUnitDialog.EnableOnlyInterface;
begin
  SectionRadioGroup.ItemIndex := 0;
  SectionRadioGroup.Enabled := False;
end;


end.

