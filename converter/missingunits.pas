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

  Author: Juha Manninen
  
  Abstract:
    A form asking the user what to do with missing units
    in uses section. Used by ConvertDelphi unit.
}
unit MissingUnits;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls, FileUtil, CheckLst, Menus,
  // Components
  SynEdit, CodeAtom, CodeCache, CodeToolManager, DefineTemplates,
  // IDEIntf
  LazIDEIntf, IDEMsgIntf,
  // IDE
  CompilerOptions,
  PackageDefs, Project, DialogProcs, IDEProcs, LazarusIDEStrConsts;

type

  { TMissingUnitsDialog }

  TMissingUnitsDialog = class(TForm)
    UnselectMenuItem: TMenuItem;
    SelectMenuItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveMenuItem: TMenuItem;
    MissingUnitsCheckListBox: TCheckListBox;
    CommentButton: TBitBtn;
    ChoicesLabel: TLabel;
    Info1Label: TLabel;
    Info2Label: TLabel;
    Info3Label: TLabel;
    PopupMenu1: TPopupMenu;
    SearchButton: TBitBtn;
    AbortButton: TBitBtn;
    MissingUnitsInfoLabel: TLabel;
    procedure AbortButtonClick(Sender: TObject);
    procedure CommentButtonClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
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
    i: Integer;
  begin         // Add missing units to CheckListBox.
    for i:=0 to AMissingUnits.Count-1 do begin
      UNFDialog.MissingUnitsCheckListBox.Items.Append(AMissingUnits[i]);
      UNFDialog.MissingUnitsCheckListBox.Checked[i]:=true;
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
  Result:=mrCancel;
  // A title text containing filename.
  if (AMainMissingUnits.Count + AImplMissingUnits.Count) = 1 then
    UnitsTitle:=lisUnitNotFound+' '+AMainUnitName
  else
    UnitsTitle:=lisUnitsNotFound2+' '+AMainUnitName;

  UNFDialog:=TMissingUnitsDialog.Create(nil);
  with UNFDialog do begin
    Caption:=UnitsTitle;
    SearchButton.Caption:=lisMissingUnitsSearch;
    MissingUnitsInfoLabel.Caption:=lisTheseUnitsWereNotFound;
    ChoicesLabel.Caption:=lisMissingUnitsChoices;
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
    // Show dialog and delete the entries that user has unchecked.
    Result:=ShowModal;
    if Result=mrOK then begin
      for i:=0 to MissingUnitsCheckListBox.Count-1 do begin
        if not MissingUnitsCheckListBox.Checked[i] then begin
          s:=MissingUnitsCheckListBox.Items[i];
          if not RemoveFromMissing(s, AMainMissingUnits) then begin
            ImplRemoved:=RemoveFromMissing(s, AImplMissingUnits);
            Assert(ImplRemoved, 'Error with Missing Units in AskMissingUnits.');
          end;
        end;
      end;
    end;
    Free;
  end;
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

procedure TMissingUnitsDialog.CommentButtonClick(Sender: TObject);
begin

end;

procedure TMissingUnitsDialog.SearchButtonClick(Sender: TObject);
begin

end;

procedure TMissingUnitsDialog.AbortButtonClick(Sender: TObject);
begin

end;


end.

