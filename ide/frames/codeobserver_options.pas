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
}
unit codeobserver_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Graphics, ExtCtrls, Spin, StdCtrls,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  LazarusIDEStrConsts, CodeExplOpts;

type

  { TCodeObserverOptionsFrame }

  TCodeObserverOptionsFrame = class(TAbstractIDEOptionsEditor)
    UnnamedCharConstCheckBox: TCheckBox;
    CodeObsCategoriesCheckGroup: TCheckGroup;
    CodeObsIgnoreConstantsLabel: TLabel;
    COIgnoreConstInFuncsLabel: TLabel;
    LongProcLineCountLabel: TLabel;
    LongParamListCountLabel: TLabel;
    CodeObsIgnoreConstantsMemo: TMemo;
    COIgnoreConstInFuncsMemo: TMemo;
    NestedProcCountLabel: TLabel;
    LongProcLineCountSpinEdit: TSpinEdit;
    LongParamListCountSpinEdit: TSpinEdit;
    NestedProcCountSpinEdit: TSpinEdit;
    CodeObsLeftPanel: TPanel;
    UnnamedCharConstLabel: TLabel;
    procedure CodeObsCategoriesCheckGroupItemClick(Sender: TObject;
      Index: integer);
  private
    FCategoryCheckBoxes: array [TCEObserverCategory] of TCheckBox;
    FGroupCheckBoxes: array [TCEObserverCategoryGroup] of TCheckBox;
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

const
  GroupCategories: array [TCEObserverCategoryGroup] of TCEObserverCategories = (
    [cefcLongProcs, cefcLongParamLists, cefcNestedProcs],
    [cefcEmptyProcs, cefcEmptyBlocks, cefcEmptyClassSections],
    [cefcUnnamedConsts, cefcUnsortedClassVisibility, cefcUnsortedClassMembers],
    [cefcPublishedPropWithoutDefault, cefcWrongIndentation, cefcToDos]);

function GroupName(AGroup: TCEObserverCategoryGroup): String;
begin
  case AGroup of
    ocgComplexity: Result := lisCEComplexityGroup;
    ocgEmpty: Result := lisCEEmptyGroup;
    ocgStyle: Result := lisCEStyleGroup;
    ocgOther: Result := lisCEOtherGroup;
    else Result := '?';
  end;
end;

{ TCodeObserverOptionsFrame }

procedure TCodeObserverOptionsFrame.CodeObsCategoriesCheckGroupItemClick(
  Sender: TObject; Index: integer);
var
  c: TCEObserverCategory;
  g: TCEObserverCategoryGroup;
  hasState: array [Boolean] of Boolean;
begin
  CodeObsCategoriesCheckGroup.OnItemClick := nil;
  try
    for g := Low(g) to High(g) do begin
      hasState[true] := false;
      hasState[false] := false;
      for c := Low(c) to High(c) do
        if c in GroupCategories[g] then
          hasState[FCategoryCheckBoxes[c].Checked] := true;
      if (Sender <> nil)
      and (FGroupCheckBoxes[g] = CodeObsCategoriesCheckGroup.Components[Index])
      then begin
        // Check/uncheck all categories in group
        for c := Low(c) to High(c) do
          if c in GroupCategories[g] then
            FCategoryCheckBoxes[c].Checked := hasState[false];
        FGroupCheckBoxes[g].Checked := hasState[false]
      end
      else if hasState[true] and hasState[false] then
        FGroupCheckBoxes[g].State := cbGrayed
      else
        FGroupCheckBoxes[g].Checked := not hasState[false];
    end;
  finally
    CodeObsCategoriesCheckGroup.OnItemClick :=
      @CodeObsCategoriesCheckGroupItemClick;
  end;
end;

function TCodeObserverOptionsFrame.GetTitle: String;
begin
  Result := lisCodeObserver;
end;

procedure TCodeObserverOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
var
  c: TCEObserverCategory;
  g: TCEObserverCategoryGroup;

  function AddCheckBox(ACaption: String): TCheckBox;
  begin
    with CodeObsCategoriesCheckGroup do
      Result := TCheckBox(Components[Items.Add(ACaption)]);
  end;

begin
  CodeObsCategoriesCheckGroup.Caption := lisCEShowCodeObserver;
  CodeObsCategoriesCheckGroup.OnItemClick := nil;
  try
    for g := Low(g) to High(g) do begin
      FGroupCheckBoxes[g] := AddCheckBox(GroupName(g) + ':');
      with FGroupCheckBoxes[g] do begin
        AllowGrayed := true;
        State := cbGrayed;
        Font.Style := [fsItalic];
      end;
      for c := Low(c) to High(c) do
        if c in GroupCategories[g] then
          FCategoryCheckBoxes[c] :=
            AddCheckBox('   ' + CodeExplorerLocalizedString(c));
    end;
  finally
    CodeObsCategoriesCheckGroup.OnItemClick :=
      @CodeObsCategoriesCheckGroupItemClick;
  end;

  LongProcLineCountLabel.Caption := lisCELongProcLineCount;
  LongParamListCountLabel.Caption := lisCELongParamListCount;
  NestedProcCountLabel.Caption := lisCENestedProcCount;
  UnnamedCharConstLabel.Caption := lisCodeObsCharConst;
  UnnamedCharConstCheckBox.Caption := lisShow;
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
    for c := Low(c) to High(c) do
      FCategoryCheckBoxes[c].Checked := c in ObserverCategories;
    CodeObsCategoriesCheckGroupItemClick(nil, -1);

    LongProcLineCountSpinEdit.Value := LongProcLineCount;
    LongParamListCountSpinEdit.Value := LongParamListCount;
    NestedProcCountSpinEdit.Value := NestedProcCount;
    UnnamedCharConstCheckBox.Checked := ObserveCharConst;
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
  for c := Low(c) to high(c) do
    if FCategoryCheckBoxes[c].Checked then
      Include(NewCategories, c);
  with TCodeExplorerOptions(AOptions) do
  begin
    ObserverCategories := NewCategories;
    LongProcLineCount := LongProcLineCountSpinEdit.Value;
    LongParamListCount := LongParamListCountSpinEdit.Value;
    NestedProcCount := NestedProcCountSpinEdit.Value;
    ObserveCharConst := UnnamedCharConstCheckBox.Checked;
    SetListOf_COIgnoreConstants(CodeObsIgnoreConstantsMemo.Lines, False);
    SetListOf_COIgnoreConstInFuncs(COIgnoreConstInFuncsMemo.Lines, False);
  end;
end;

class function TCodeObserverOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeExplorerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodeExplorer, TCodeObserverOptionsFrame, cdeOptionsFigures);

end.

