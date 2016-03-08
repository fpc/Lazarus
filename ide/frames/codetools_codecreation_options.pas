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
unit codetools_codecreation_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, ExtCtrls, StdCtrls, Dialogs, EditBtn,
  SourceChanger, CodeToolsOptions, LazarusIDEStrConsts, IDEOptionsIntf;

type

  { TCodetoolsCodeCreationOptionsFrame }

  TCodetoolsCodeCreationOptionsFrame = class(TAbstractIDEOptionsEditor)
    ForwardProcsInsertPolicyComboBox: TComboBox;
    TemplateFileEdit: TFileNameEdit;
    UsesInsertPolicyComboBox: TComboBox;
    ForwardProcsKeepOrderCheckBox: TCheckBox;
    ForwardProcsInsertPolicyLabel: TLabel;
    UsesInsertPolicyLabel: TLabel;
    TemplateFileLabel: TLabel;
    UpdateMultiProcSignaturesCheckBox: TCheckBox;
    UpdateOtherProcSignaturesCaseCheckBox: TCheckBox;
    GroupLocalVariablesCheckBox: TCheckBox;
  private
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCodetoolsCodeCreationOptionsFrame }

function TCodetoolsCodeCreationOptionsFrame.GetTitle: String;
begin
  Result := dlgCodeCreation;
end;

procedure TCodetoolsCodeCreationOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  ForwardProcsInsertPolicyLabel.Caption:=dlgForwardProcsInsertPolicy;
  with ForwardProcsInsertPolicyComboBox do begin
    with Items do begin
      BeginUpdate;
      Add(dlgLast);
      Add(dlgInFrontOfMethods);
      Add(dlgBehindMethods);
      EndUpdate;
    end;
  end;

  ForwardProcsKeepOrderCheckBox.Caption:=dlgForwardProcsKeepOrder;

  UsesInsertPolicyLabel.Caption:=lisNewUnitsAreAddedToUsesSections;
  with UsesInsertPolicyComboBox do begin
    with Items do begin
      BeginUpdate;
      Add(lisFirst);
      Add(lisInFrontOfRelated);
      Add(lisBehindRelated);
      Add(dlgCDTLast);
      Add(dlgAlphabetically);
      EndUpdate;
    end;
  end;

  UpdateMultiProcSignaturesCheckBox.Caption:=
    lisCTOUpdateMultipleProcedureSignatures;
  UpdateOtherProcSignaturesCaseCheckBox.Caption:=
    lisUpdateOtherProcedureSignaturesWhenOnlyLetterCaseHa;
  GroupLocalVariablesCheckBox.Caption:=
    lisGroupLocalVariables;

  TemplateFileLabel.Caption:=lisTemplateFile;
  {$IFNDEF EnableCodeCompleteTemplates}
  TemplateFileLabel.Enabled:=false;
  TemplateFileEdit.Enabled:=false;
  {$ENDIF}

  TemplateFileEdit.DialogTitle:=lisChooseAFileWithCodeToolsTemplates;
  TemplateFileEdit.Filter:=dlgFilterCodetoolsTemplateFile+' (*.xml)|*.xml|'+
    dlgFilterAll+'|'+GetAllFilesMask;
end;

procedure TCodetoolsCodeCreationOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodetoolsOptions do
  begin
    case ForwardProcBodyInsertPolicy of
      fpipLast: ForwardProcsInsertPolicyComboBox.ItemIndex:=0;
      fpipInFrontOfMethods: ForwardProcsInsertPolicyComboBox.ItemIndex:=1;
    else
      // fpipBehindMethods
      ForwardProcsInsertPolicyComboBox.ItemIndex:=2;
    end;

    ForwardProcsKeepOrderCheckBox.Checked := KeepForwardProcOrder;

    case UsesInsertPolicy of
    uipFirst:             UsesInsertPolicyComboBox.ItemIndex:=0;
    uipInFrontOfRelated:  UsesInsertPolicyComboBox.ItemIndex:=1;
    uipBehindRelated:     UsesInsertPolicyComboBox.ItemIndex:=2;
    uipLast:              UsesInsertPolicyComboBox.ItemIndex:=3;
    else
      //uipAlphabetically:
                          UsesInsertPolicyComboBox.ItemIndex:=4;
    end;

    UpdateMultiProcSignaturesCheckBox.Checked:=UpdateMultiProcSignatures;
    UpdateOtherProcSignaturesCaseCheckBox.Checked:=UpdateOtherProcSignaturesCase;
    GroupLocalVariablesCheckBox.Checked:=GroupLocalVariables;

    TemplateFileEdit.Text:=CodeCompletionTemplateFileName;
  end;
end;

procedure TCodetoolsCodeCreationOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodetoolsOptions do
  begin
    case ForwardProcsInsertPolicyComboBox.ItemIndex of
      0: ForwardProcBodyInsertPolicy := fpipLast;
      1: ForwardProcBodyInsertPolicy := fpipInFrontOfMethods;
      2: ForwardProcBodyInsertPolicy := fpipBehindMethods;
    end;

    KeepForwardProcOrder := ForwardProcsKeepOrderCheckBox.Checked;

    case UsesInsertPolicyComboBox.ItemIndex of
    0: UsesInsertPolicy:=uipFirst;
    1: UsesInsertPolicy:=uipInFrontOfRelated;
    2: UsesInsertPolicy:=uipBehindRelated;
    3: UsesInsertPolicy:=uipLast;
    else UsesInsertPolicy:=uipAlphabetically;
    end;

    UpdateMultiProcSignatures:=UpdateMultiProcSignaturesCheckBox.Checked;
    UpdateOtherProcSignaturesCase:=UpdateOtherProcSignaturesCaseCheckBox.Checked;
    GroupLocalVariables:=GroupLocalVariablesCheckBox.Checked;

    CodeCompletionTemplateFileName:=TemplateFileEdit.Text;
  end;
end;

class function TCodetoolsCodeCreationOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeToolsOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsCodeCreationOptionsFrame, CdtOptionsCodeCreation);
end.

