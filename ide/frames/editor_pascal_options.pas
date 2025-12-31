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
unit editor_pascal_options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, DividerBevel, Controls, Graphics,
  // SynEdit
  SynEditStrConst, SynHighlighterPas,
  // IDEIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  LazarusIDEStrConsts, EditorOptions, editor_color_options, editor_general_options;

type

  { TEditorPascalOptionsFrame }

  TEditorPascalOptionsFrame = class(TAbstractIDEOptionsEditor)
    CaseLabelLink: TLabel;
    LinkDeclGenParam2: TLabel;
    LinkDeclType: TLabel;
    cbCaseLabelColorForOtherwise: TCheckBox;
    cbDeclaredValueAttrForNumString: TCheckBox;
    chkExtPasKeywords: TCheckBox;
    divKeywords: TDividerBevel;
    divParamTypes: TDividerBevel;
    dropDeclaredTypeAttrMode: TComboBox;
    dropDeclaredValueAttrMode: TComboBox;
    dropGenericParamAttrMode: TComboBox;
    dropPasStringKeywords: TComboBox;
    dropProcNameDeclAttrMode: TComboBox;
    dropProcNameImplAttrMode: TComboBox;
    lblDeclaredTypeAttrMode: TLabel;
    lblDeclaredValueAttrMode: TLabel;
    lblGenericParamAttrMode: TLabel;
    lblPasStringKeywords: TLabel;
    lblProcNameDeclAttrMode: TLabel;
    lblProcNameImplAttrMode: TLabel;
    LinkDeclType2: TLabel;
    LinkDeclType3: TLabel;
    LinkDeclValue: TLabel;
    LinkDeclGenParam: TLabel;
    LinkDeclValue2: TLabel;
    procedure CaseLabelLinkClick(Sender: TObject);
    procedure chkExtPasKeywordsChange(Sender: TObject);
    procedure dropPasStringKeywordsChange(Sender: TObject);
    procedure LinkLabelMouseEnter(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure LinkLabelMouseExit(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;

    function GeneralPage: TEditorGeneralOptionsFrame; inline;
  public
    function GetTitle: String; override;
    procedure UpdatePreviews;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TEditorPascalOptionsFrame }

procedure TEditorPascalOptionsFrame.CaseLabelLinkClick(Sender: TObject);
var
  col: TEditorColorOptionsFrame;
begin
  col := TEditorColorOptionsFrame(FDialog.FindEditor(TEditorColorOptionsFrame));
  if col = nil then exit;

  LinkLabelMouseExit(Sender);
  FDialog.OpenEditor(TEditorColorOptionsFrame);
  if Sender = CaseLabelLink then
    col.SelectNamedColor(SYNS_XML_AttrCaseLabel);

  if Sender = LinkDeclType then
    col.SelectNamedColor(SYNS_XML_AttrDeclarationType);
  if Sender = LinkDeclType2 then
    col.SelectNamedColor(SYNS_XML_AttrProcedureHeaderType);
  if Sender = LinkDeclType3 then
    col.SelectNamedColor(SYNS_XML_AttrProcedureHeaderResult);

  if Sender = LinkDeclValue then
    col.SelectNamedColor(SYNS_XML_AttrDeclarationValue);
  if Sender = LinkDeclValue2 then
    col.SelectNamedColor(SYNS_XML_AttrProcedureHeaderValue);

  if Sender = LinkDeclGenParam then
    col.SelectNamedColor(SYNS_XML_AttrGenericConstraint);
  if Sender = LinkDeclGenParam2 then
    col.SelectNamedColor(SYNS_XML_AttrSpecializeParam);
end;

procedure TEditorPascalOptionsFrame.chkExtPasKeywordsChange(Sender: TObject);
begin
  GeneralPage.PasExtendedKeywordsMode := chkExtPasKeywords.Checked;
  GeneralPage.UpdatePreviewEdits;
end;

procedure TEditorPascalOptionsFrame.dropPasStringKeywordsChange(Sender: TObject);
begin
  GeneralPage.PasStringKeywordMode := TSynPasStringMode(dropPasStringKeywords.ItemIndex);
  GeneralPage.UpdatePreviewEdits;
end;

procedure TEditorPascalOptionsFrame.LinkLabelMouseEnter(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (Sender as TLabel).Font.Underline := True;
  (Sender as TLabel).Font.Color := clRed;
end;

procedure TEditorPascalOptionsFrame.LinkLabelMouseExit(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := False;
  (Sender as TLabel).Font.Color := clBlue;
end;

function TEditorPascalOptionsFrame.GeneralPage: TEditorGeneralOptionsFrame;
begin
  Result := TEditorGeneralOptionsFrame(FDialog.FindEditor(TEditorGeneralOptionsFrame));
end;

function TEditorPascalOptionsFrame.GetTitle: String;
begin
  Result := lisPascalHighlightOpts;
end;

procedure TEditorPascalOptionsFrame.UpdatePreviews;
var
  a: Integer;
  Syn: TSynPasSyn;
begin
  with GeneralPage do
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> nil then
      begin
        if PreviewEdits[a].Highlighter is TSynPasSyn then begin
          Syn := TSynPasSyn(PreviewEdits[a].Highlighter);
          //TSynPasSyn(Syn).ExtendedKeywordsMode := PasExtendedKeywordsMode;
          //TSynPasSyn(Syn).StringKeywordMode := PasStringKeywordMode;
          TSynPasSyn(Syn).CaseLabelAttriMatchesElseOtherwise    := cbCaseLabelColorForOtherwise.Checked;
          TSynPasSyn(Syn).DeclaredTypeAttributeMode             := TSynPasTypeAttributeMode(dropDeclaredTypeAttrMode.ItemIndex);
          TSynPasSyn(Syn).DeclaredValueAttributeMode            := TSynPasTypeAttributeMode(dropDeclaredValueAttrMode.ItemIndex);
          TSynPasSyn(Syn).DeclaredValueAttributeMachesStringNum := cbDeclaredValueAttrForNumString.Checked;
          TSynPasSyn(Syn).GenericConstraintAttributeMode        := TSynPasTypeAttributeMode(dropGenericParamAttrMode.ItemIndex);
          TSynPasSyn(Syn).SpecializeParamAttributeMode          := TSynPasTypeAttributeMode(dropGenericParamAttrMode.ItemIndex);
          case TProcHeaderNameMode(dropProcNameDeclAttrMode.ItemIndex) of
            pnmGenericOnly:        TSynPasSyn(Syn).ProcNameIntfAttributeMode := [pamDots];
            pnmGenericAndProcName: TSynPasSyn(Syn).ProcNameIntfAttributeMode := [pamDots, pamGenParamKeyword, pamGenParamSym, pamGenParamSeparator];
            pnmProcNameOnly:       TSynPasSyn(Syn).ProcNameIntfAttributeMode := [pamSupressGenParamAttr, pamDots, pamGenParamKeyword, pamGenParamSym, pamGenParamSeparator];
            pnmPlain:              TSynPasSyn(Syn).ProcNameIntfAttributeMode := [pamSupressGenParamAttr, pamDots];
          end;
          case TProcHeaderNameMode(dropProcNameImplAttrMode.ItemIndex) of
            pnmGenericOnly:        TSynPasSyn(Syn).ProcNameImplAttributeMode := [pamDots];
            pnmGenericAndProcName: TSynPasSyn(Syn).ProcNameImplAttributeMode := [pamDots, pamGenParamKeyword, pamGenParamSym, pamGenParamSeparator];
            pnmProcNameOnly:       TSynPasSyn(Syn).ProcNameImplAttributeMode := [pamSupressGenParamAttr, pamDots, pamGenParamKeyword, pamGenParamSym, pamGenParamSeparator];
            pnmPlain:              TSynPasSyn(Syn).ProcNameImplAttributeMode := [pamSupressGenParamAttr, pamDots];
          end;
        end;
      end;
end;

procedure TEditorPascalOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FDialog := ADialog;

  divKeywords.Caption := dlgPasExtHighlightGroup;
  divParamTypes.Caption := dlgPasParamTypes;
  chkExtPasKeywords.Caption := dlgPasExtKeywords;

  lblPasStringKeywords.Caption := dlgPasStringKeywords;
  dropPasStringKeywords.Items.Add(dlgPasStringKeywordsOptDefault);
  dropPasStringKeywords.Items.Add(dlgPasStringKeywordsOptString);
  cbCaseLabelColorForOtherwise.Caption := dlgPasCaseLabelForOtherwise;

  CaseLabelLink.Caption := dlgColorLink;
  LinkDeclType.Caption := dlgColorLink;
  LinkDeclType2.Caption := dlgColorLink2;
  LinkDeclType3.Caption := dlgColorLink2;
  LinkDeclValue.Caption := dlgColorLink;
  LinkDeclValue2.Caption := dlgColorLink2;
  LinkDeclGenParam.Caption := dlgColorLink;
  LinkDeclGenParam2.Caption := dlgColorLink2;
  dropPasStringKeywords.Items.Add(dlgPasStringKeywordsOptNone);

  lblDeclaredTypeAttrMode.Caption := dlgPasDeclaredTypeAttrMode;
  dropDeclaredTypeAttrMode.Items.Clear;
  dropDeclaredTypeAttrMode.Items.add(dlgPasDeclaredTypeAttrModeIdent);
  dropDeclaredTypeAttrMode.Items.add(dlgPasDeclaredTypeAttrModeNames);
  dropDeclaredTypeAttrMode.Items.add(dlgPasDeclaredTypeAttrModeKeywords);
  dropDeclaredTypeAttrMode.Items.add(dlgPasDeclaredTypeAttrModeKeyAndSym);

  lblDeclaredValueAttrMode.Caption := dlgPasDeclaredTypeValueMode;
  dropDeclaredValueAttrMode.Items.Clear;
  dropDeclaredValueAttrMode.Items.add(dlgPasDeclaredTypeAttrModeIdent);
  dropDeclaredValueAttrMode.Items.add(dlgPasDeclaredTypeAttrModeNames);
  dropDeclaredValueAttrMode.Items.add(dlgPasDeclaredTypeAttrModeKeywords);
  dropDeclaredValueAttrMode.Items.add(dlgPasDeclaredTypeAttrModeKeyAndSym);

  lblGenericParamAttrMode.Caption := dlgPasGenericParamAttrMode;
  dropGenericParamAttrMode.Items.Clear;
  dropGenericParamAttrMode.Items.add(dlgPasDeclaredTypeAttrModeIdent);
  dropGenericParamAttrMode.Items.add(dlgPasDeclaredTypeAttrModeNames);
  dropGenericParamAttrMode.Items.add(dlgPasDeclaredTypeAttrModeKeywords);
  dropGenericParamAttrMode.Items.add(dlgPasDeclaredTypeAttrModeKeyAndSym);

  lblProcNameDeclAttrMode.Caption := dlgPasProcNameDeclAttrMode;
  dropProcNameDeclAttrMode.Items.Clear;
  dropProcNameDeclAttrMode.Items.add(dlgPasProcNameAttrModeGenOnly);
  dropProcNameDeclAttrMode.Items.add(dlgPasProcNameAttrModeGenAndProc);
  dropProcNameDeclAttrMode.Items.add(dlgPasProcNameAttrModeProcOnly);
  dropProcNameDeclAttrMode.Items.add(dlgPasProcNameAttrModeNone);

  lblProcNameImplAttrMode.Caption := dlgPasProcNameImplAttrMode;
  dropProcNameImplAttrMode.Items.Clear;
  dropProcNameImplAttrMode.Items.add(dlgPasProcNameAttrModeGenOnly);
  dropProcNameImplAttrMode.Items.add(dlgPasProcNameAttrModeGenAndProc);
  dropProcNameImplAttrMode.Items.add(dlgPasProcNameAttrModeProcOnly);
  dropProcNameImplAttrMode.Items.add(dlgPasProcNameAttrModeNone);

  cbDeclaredValueAttrForNumString.Caption := dlgPasDeclaredTypeValueModeLiteral;

end;

procedure TEditorPascalOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    chkExtPasKeywords.Checked := PasExtendedKeywordsMode;
    dropPasStringKeywords.ItemIndex          := ord(PasStringKeywordMode);
    cbCaseLabelColorForOtherwise.Checked     := CaseLabelAttriMatchesElseOtherwise;
    dropDeclaredTypeAttrMode.ItemIndex       := ord(DeclaredTypeAttributeMode);
    dropDeclaredValueAttrMode.ItemIndex      := ord(DeclaredValueAttributeMode);
    dropGenericParamAttrMode.ItemIndex       := ord(GenericParamAttrMode);
    dropProcNameDeclAttrMode.ItemIndex       := ord(ProcHeaderNameDeclMode);
    dropProcNameImplAttrMode.ItemIndex       := ord(ProcHeaderNameImplMode);
    cbDeclaredValueAttrForNumString.Checked  := DeclaredValueAttributeMachesStringNum;
  end;
end;

procedure TEditorPascalOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    PasExtendedKeywordsMode               := chkExtPasKeywords.Checked;
    PasStringKeywordMode                  := TSynPasStringMode(dropPasStringKeywords.ItemIndex);
    CaseLabelAttriMatchesElseOtherwise    := cbCaseLabelColorForOtherwise.Checked;
    DeclaredTypeAttributeMode             := TSynPasTypeAttributeMode(dropDeclaredTypeAttrMode.ItemIndex);
    DeclaredValueAttributeMode            := TSynPasTypeAttributeMode(dropDeclaredValueAttrMode.ItemIndex);
    GenericParamAttrMode                  := TSynPasTypeAttributeMode(dropGenericParamAttrMode.ItemIndex);
    ProcHeaderNameDeclMode                := TProcHeaderNameMode(dropProcNameDeclAttrMode.ItemIndex);
    ProcHeaderNameImplMode                := TProcHeaderNameMode(dropProcNameImplAttrMode.ItemIndex);
    DeclaredValueAttributeMachesStringNum := cbDeclaredValueAttrForNumString.Checked;
  end;
end;

class function TEditorPascalOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorPascalOptionsFrame,
    EdtOptionsPascal, EdtOptionsDisplay);
end.

