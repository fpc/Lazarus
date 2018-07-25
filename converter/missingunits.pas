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
    A form asking the user what to do with missing units
    in uses section. Used by ConvertDelphi unit.
}
unit MissingUnits;

{$mode objfpc}{$H+}

interface

uses
  // FCL
  Classes, SysUtils,
  // LCL
  LCLProc, LResources, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  CheckLst, Menus, ExtCtrls,
  // LazUtils
  FileUtil,
  // CodeTools
  DefineTemplates,
  // IdeIntf
  IDEImagesIntf,
   // IDE
  PackageDefs, Project, LazarusIDEStrConsts;

type

  { TMissingUnitsDialog }

  TMissingUnitsDialog = class(TForm)
    AbortButton: TBitBtn;
    ChoicesLabel: TLabel;
    CommentButton: TBitBtn;
    ButtonPanel: TPanel;
    Info1Label: TLabel;
    Info2Label: TLabel;
    Info3Label: TLabel;
    MissingUnitsCheckListBox: TCheckListBox;
    MissingUnitsGroupBox: TGroupBox;
    InfoPanel: TPanel;
    SearchButton: TBitBtn;
    SkipButton: TBitBtn;
    Splitter1: TSplitter;
    UnselectMenuItem: TMenuItem;
    SelectMenuItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveMenuItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SelectMenuItemClick(Sender: TObject);
    procedure UnselectMenuItemClick(Sender: TObject);
  private

  public

  end;

var
  MissingUnitsDialog: TMissingUnitsDialog;

function AskMissingUnits(AMainMissingUnits, AImplMissingUnits: TStrings;
                         AMainUnitName: string; ATargetDelphi: boolean): TModalResult;


implementation

{$R *.lfm}

function AskMissingUnits(AMainMissingUnits, AImplMissingUnits: TStrings;
                         AMainUnitName: string; ATargetDelphi: boolean): TModalResult;
var
  UNFDialog: TMissingUnitsDialog;

  procedure AddUnitsToListBox(AMissingUnits: TStrings);
  var
    i, Ind: Integer;
  begin         // Add missing units to CheckListBox.
    for i:=0 to AMissingUnits.Count-1 do begin
      Ind:=UNFDialog.MissingUnitsCheckListBox.Items.Add(AMissingUnits[i]);
      UNFDialog.MissingUnitsCheckListBox.Checked[Ind]:=true;
    end;
  end;

  function RemoveFromMissing(AUnit: string; AList: TStrings): Boolean;
  var
    i: Integer;
  begin
    i:=AList.IndexOf(AUnit);
    Result:=i<>-1;
    if Result then
      AList.Delete(i);
  end;

var
  UnitsTitle, s: string;
  i: Integer;
  ImplRemoved: Boolean;
begin
  {$IFDEF CommentUnitsAutomatic}
  Result:=mrOK;
  {$ELSE}
  Result:=mrCancel;
  // A title text containing filename.
  if (AMainMissingUnits.Count + AImplMissingUnits.Count) = 1 then
    s:=lisUnitNotFoundInProject
  else
    s:=lisUnitsNotFoundInProject;
  UnitsTitle:=Format(s, [AMainUnitName]);

  UNFDialog:=TMissingUnitsDialog.Create(nil);
  with UNFDialog do begin
    Caption:=UnitsTitle;
    MissingUnitsGroupBox.Caption:=lisTheseUnitsWereNotFound;
    ChoicesLabel.Caption:=lisMissingUnitsChoices;
    SearchButton.Caption:=lisMissingUnitsSearch;
    IDEImages.AssignImage(SearchButton, 'menu_search_find');
    SkipButton.Caption:=lisMissingUnitsSkip;
    IDEImages.AssignImage(SkipButton, 'debugger_current_line_breakpoint');
    IDEImages.AssignImage(CommentButton, 'menu_comment'); // or insertremark
    if ATargetDelphi then begin
      CommentButton.Caption:=lisMissingUnitsForDelphi;
      Info1Label.Caption:=lisMissingUnitsInfo1b;
    end
    else begin
      CommentButton.Caption:=lisMissingUnitsComment;
      Info1Label.Caption:=lisMissingUnitsInfo1;
    end;
    Info2Label.Caption:=lisMissingUnitsInfo2;
    Info3Label.Caption:=lisMissingUnitsInfo3;
    // Add missing units to CheckListBox.
    AddUnitsToListBox(AMainMissingUnits);
    AddUnitsToListBox(AImplMissingUnits);
    // Show dialog and remove the entries that user has unchecked.
    // Missing units will be searched again later.
    Result:=ShowModal;
    if Result in [mrOK, mrIgnore] then begin   // mrIgnore means "skip"
      for i:=0 to MissingUnitsCheckListBox.Count-1 do begin
        // Remove all when Skip was clicked.
        if (Result=mrIgnore) or not MissingUnitsCheckListBox.Checked[i] then begin
          s:=MissingUnitsCheckListBox.Items[i];
          // Remove either from main or implementation sections list.
          if not RemoveFromMissing(s, AMainMissingUnits) then begin
            ImplRemoved:=RemoveFromMissing(s, AImplMissingUnits);
            Assert(ImplRemoved, 'Error with Missing Units in AskMissingUnits.');
          end;
        end;
      end;
    end;
    Free;
  end;
  {$ENDIF}
end;

{ TMissingUnitsDialog }

procedure TMissingUnitsDialog.SelectMenuItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to MissingUnitsCheckListBox.Count-1 do
    MissingUnitsCheckListBox.Checked[i]:=true;
end;

procedure TMissingUnitsDialog.UnselectMenuItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to MissingUnitsCheckListBox.Count-1 do
    MissingUnitsCheckListBox.Checked[i]:=false;
end;

procedure TMissingUnitsDialog.SaveMenuItemClick(Sender: TObject);
var
  fn: String;
begin
  SaveDialog1.FileName:='MissingUnitsList.txt';
  if SaveDialog1.Execute then begin
    fn:=SaveDialog1.FileName;
    MissingUnitsCheckListBox.Items.SaveToFile(fn);
    ShowMessage(Format('Unit list is saved to file %s.', [fn]));
  end;
end;

end.

