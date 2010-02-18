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
unit MissingUnitsUnit;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics,
  Dialogs, Buttons, StdCtrls, FileUtil,
  // Components
  SynEdit, CodeAtom, CodeCache, CodeToolManager, DefineTemplates,
  // IDEIntf
  LazIDEIntf, IDEMsgIntf,
  // IDE
  CompilerOptions,
  PackageDefs, Project, DialogProcs, IDEProcs, LazarusIDEStrConsts;

const
  // Copied from LazarusIDEStrConsts, remove later...
  lisMissingUnitsComment = 'Comment Out';
  lisMissingUnitsSearch = 'Search Unit Path';
  lisTheseUnitsWereNotFound = 'These units were not found:';
  lisMissingUnitsChoices = 'Your choices are:';
  lisMissingUnitsInfo1 = '1) Comment out the missing units (ignore them).';
  lisMissingUnitsInfo2 = '2) Select a unit path which will be added to project settings.';
  lisMissingUnitsInfo3 = '3) Abort now, fix the unit path or install packages and try again.';
  lisUnitNotFound = 'A unit not found in';
  lisUnitsNotFound2 = 'Units not found in';


type

  { TMissingUnitsDialog }

  TMissingUnitsDialog = class(TForm)
    CommentButton: TBitBtn;
    ChoicesLabel: TLabel;
    UnitNamesLabel: TLabel;
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

function AskMissingUnits(AMissingUnits: TStrings; AMainUnitName: string): TModalResult;


implementation

{$R *.lfm}

function AskMissingUnits(AMissingUnits: TStrings; AMainUnitName: string): TModalResult;
var
  UNFDialog: TMissingUnitsDialog;
  UnitsTitle, UnitsCommaList: string;
  i: Integer;
begin
  Result:=mrCancel;

  // A title text containing filename.
  if AMissingUnits.Count=1 then
    UnitsTitle:=lisUnitNotFound+' '+AMainUnitName
  else
    UnitsTitle:=lisUnitsNotFound2+' '+AMainUnitName;

  // A comma separated list of missing units.
  UnitsCommaList:='';
  for i:=0 to AMissingUnits.Count-1 do begin
    if UnitsCommaList<>'' then
      UnitsCommaList:=UnitsCommaList+', ';
    UnitsCommaList:=UnitsCommaList+AMissingUnits[i];
  end;

  UNFDialog:=TMissingUnitsDialog.Create(nil);
  with UNFDialog do begin
    Caption:=UnitsTitle;
    CommentButton.Caption:=lisMissingUnitsComment;
    SearchButton.Caption:=lisMissingUnitsSearch;
    MissingUnitsInfoLabel.Caption:=lisTheseUnitsWereNotFound;
    UnitNamesLabel.Caption:=UnitsCommaList;
    ChoicesLabel.Caption:=lisMissingUnitsChoices;
    Info1Label.Caption:=lisMissingUnitsInfo1;
    Info2Label.Caption:=lisMissingUnitsInfo2;
    Info3Label.Caption:=lisMissingUnitsInfo3;
    Result:=ShowModal;
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

