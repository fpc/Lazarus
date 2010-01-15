{
 /***************************************************************************
                           MsgViewEditor.pas
                           -----------------

 ***************************************************************************/

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
}
unit MsgViewEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, LazarusIDEStrConsts;

type
  { TMessageFilterRule }
  
  TMessageFilterAction = (
    mfaHide
    );
  
  TMessageFilterRule = class
  public
    property Expression: string;
    property SimpleSyntax: boolean;
    property Action: TMessageFilterAction;
  end;
  
  TMessageFilterRules = class(TList)
  private
    function GetItems(Index: integer): TMessageFilterRule;
    procedure SetItems(Index: integer; const AValue: TMessageFilterRule);
  public
    property Items[Index: integer]: TMessageFilterRule read GetItems write SetItems; default;
  end;

  { TMsgViewEditorDlg }

  TMsgViewEditorDlg = class(TForm)
    ActiveFilterSetGroupBox: TGroupBox;
    AddNewSetButton: TBitBtn;
    CancelButton: TBitBtn;
    DeleteSetButton: TBitBtn;
    OkButton: TBitBtn;
    RenameSetButton: TBitBtn;
    RulesListView: TListView;
    FilterSetGroupBox: TGroupBox;
    FilterSetsListBox: TListBox;
    procedure AddNewSetButtonClick(Sender: TObject);
    procedure DeleteSetButtonClick(Sender: TObject);
    procedure MsgViewEditorDlgCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure RenameSetButtonClick(Sender: TObject);
  private
  public
  end;

var
  MsgViewEditorDlg: TMsgViewEditorDlg;

implementation

{$R *.lfm}

{ TMsgViewEditorDlg }

procedure TMsgViewEditorDlg.MsgViewEditorDlgCreate(Sender: TObject);
begin
  Caption:= lisMessagesEditor;
  AddNewSetButton.Caption:=lisAddNewSet;
  ActiveFilterSetGroupBox.Caption:=lisActiveFilter;
  RenameSetButton.Caption:=lisFRIRename;
  DeleteSetButton.Caption:=dlgEdDelete;
  FilterSetGroupBox.Caption:=lisFilterSets;

  AddNewSetButton.LoadGlyphFromLazarusResource('laz_add');
  DeleteSetButton.LoadGlyphFromLazarusResource('laz_delete');
  RenameSetButton.LoadGlyphFromLazarusResource('laz_edit');
end;

procedure TMsgViewEditorDlg.DeleteSetButtonClick(Sender: TObject);
begin

end;

procedure TMsgViewEditorDlg.AddNewSetButtonClick(Sender: TObject);
begin

end;

procedure TMsgViewEditorDlg.OkButtonClick(Sender: TObject);
begin

end;

procedure TMsgViewEditorDlg.RenameSetButtonClick(Sender: TObject);
begin

end;

{ TMessageFilterRules }

function TMessageFilterRules.GetItems(Index: integer): TMessageFilterRule;
begin
  Result:=inherited Items[Index];
end;

procedure TMessageFilterRules.SetItems(Index: integer;
  const AValue: TMessageFilterRule);
begin
  inherited Items[Index]:=AValue;
end;

end.

