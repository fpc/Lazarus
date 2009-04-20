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
}
unit codeexplorer_figures_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms,
  IDEOptionsIntf, LazarusIDEStrConsts, CodeExplOpts, ExtCtrls, Spin, StdCtrls;

type

  { TCodeObserverOptionsFrame }

  TCodeObserverOptionsFrame = class(TAbstractIDEOptionsEditor)
    CodeObsCharConstCheckBox: TCheckBox;
    CodeObsCategoriesCheckGroup: TCheckGroup;
    CodeObsIgnoreConstantsLabel: TLabel;
    COIgnoreConstInFuncsLabel: TLabel;
    Label1: TLabel;
    LongProcLineCountLabel: TLabel;
    LongParamListCountLabel: TLabel;
    CodeObsIgnoreConstantsMemo: TMemo;
    COIgnoreConstInFuncsMemo: TMemo;
    NestedProcCountLabel: TLabel;
    LongProcLineCountSpinEdit: TSpinEdit;
    LongParamListCountSpinEdit: TSpinEdit;
    NestedProcCountSpinEdit: TSpinEdit;
    CodeObsLeftPanel: TPanel;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TCodeObserverOptionsFrame }

function TCodeObserverOptionsFrame.GetTitle: String;
begin
  Result := lisCodeObserver;
end;

procedure TCodeObserverOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
var
  c: TCEObserverCategory;
begin
  CodeObsCategoriesCheckGroup.Caption := lisCEShowCodeObserver;
  for c := Low(TCEObserverCategory) to High(TCEObserverCategory) do
    CodeObsCategoriesCheckGroup.Items.Add(CodeExplorerLocalizedString(c));

  LongProcLineCountLabel.Caption := lisCELongProcLineCount;
  LongParamListCountLabel.Caption := lisCELongParamListCount;
  NestedProcCountLabel.Caption := lisCENestedProcCount;
  CodeObsCharConstCheckBox.Caption := lisCodeObsCharConst;
  CodeObsIgnoreConstantsLabel.Caption := lisCodeObsIgnoreeConstants;
  COIgnoreConstInFuncsLabel.Caption := lisCodeObIgnoreConstInFuncs;
end;

procedure TCodeObserverOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
var
  c: TCEObserverCategory;
  Tmp: TStrings;
begin
  with TCodeExplorerOptions(AOptions) do
  begin
    for c := Low(TCEObserverCategory) to High(TCEObserverCategory) do
      CodeObsCategoriesCheckGroup.Checked[ord(c)] := c in ObserverCategories;

    LongProcLineCountSpinEdit.Value := LongProcLineCount;
    LongParamListCountSpinEdit.Value := LongParamListCount;
    NestedProcCountSpinEdit.Value := NestedProcCount;
    CodeObsCharConstCheckBox.Checked := ObserveCharConst;
    Tmp := CreateListOfCOIgnoreConstants;
    CodeObsIgnoreConstantsMemo.Lines.Assign(Tmp);
    Tmp.Free;
    Tmp := CreateListOfCOIgnoreConstInFuncs;
    COIgnoreConstInFuncsMemo.Lines.Assign(Tmp);
    Tmp.Free;
  end;
end;

procedure TCodeObserverOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
var
  NewCategories: TCEObserverCategories;
  c: TCEObserverCategory;
begin
  NewCategories := [];
  for c := Low(TCEObserverCategory) to high(TCEObserverCategory) do
    if CodeObsCategoriesCheckGroup.Checked[ord(c)] then
      Include(NewCategories, c);
  with TCodeExplorerOptions(AOptions) do
  begin
    ObserverCategories := NewCategories;
    LongProcLineCount := LongProcLineCountSpinEdit.Value;
    LongParamListCount := LongParamListCountSpinEdit.Value;
    NestedProcCount := NestedProcCountSpinEdit.Value;
    ObserveCharConst := CodeObsCharConstCheckBox.Checked;
    SetListOf_COIgnoreConstants(CodeObsIgnoreConstantsMemo.Lines, False);
    SetListOf_COIgnoreConstInFuncs(COIgnoreConstInFuncsMemo.Lines, False);
  end;
end;

class function TCodeObserverOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeExplorerOptions;
end;

initialization
  {$I codeexplorer_figures_options.lrs}
  RegisterIDEOptionsEditor(GroupCodeExplorer, TCodeObserverOptionsFrame, cdeOptionsFigures);

end.

