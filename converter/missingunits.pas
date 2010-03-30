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
    A form asking what the user about what to do with missing units
    in uses section. Used by ConvertDelphi unit.
}
unit MissingUnits;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls, FileUtil, CheckLst,
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
    MissingUnitsCheckListBox: TCheckListBox;
    CommentButton: TBitBtn;
    ChoicesLabel: TLabel;
    Info1Label: TLabel;
    Info2Label: TLabel;
    Info3Label: TLabel;
    SearchButton: TBitBtn;
    AbortButton: TBitBtn;
    MissingUnitsInfoLabel: TLabel;
    procedure AbortButtonClick(Sender: TObject);
    procedure CommentButtonClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
  private

  public

  end;

var
  MissingUnitsDialog: TMissingUnitsDialog;

function AskMissingUnits(AMissingUnits: TStrings; AMainUnitName: string;
                         ATargetDelphi: boolean): TModalResult;


implementation

{$R *.lfm}

function AskMissingUnits(AMissingUnits: TStrings; AMainUnitName: string;
                         ATargetDelphi: boolean): TModalResult;
var
  UNFDialog: TMissingUnitsDialog;
  UnitsTitle: string;
  i: Integer;
begin
  Result:=mrCancel;

  // A title text containing filename.
  if AMissingUnits.Count=1 then
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
    for i:=0 to AMissingUnits.Count-1 do begin
      MissingUnitsCheckListBox.Items.Append(AMissingUnits[i]);
      MissingUnitsCheckListBox.Checked[i]:=true;
    end;
    // Show dialog and get user action.
    Result:=ShowModal;
    if Result=mrOK then begin
      AMissingUnits.Clear;
      for i := 0 to MissingUnitsCheckListBox.Count-1 do begin
        if MissingUnitsCheckListBox.Checked[i] then begin
          AMissingUnits.Append(MissingUnitsCheckListBox.Items[i]);
        end;
      end;
    end;
    Free;
  end;
end;

{ TMissingUnitsDialog }

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

