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
unit codetools_general_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  Forms, StdCtrls, Dialogs, Graphics, EditBtn, Spin,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  CodeToolsOptions, LazarusIDEStrConsts;

type

  { TCodetoolsGeneralOptionsFrame }

  TCodetoolsGeneralOptionsFrame = class(TAbstractIDEOptionsEditor)
    AdjustTopLineDueToCommentCheckBox: TCheckBox;
    AvoidUnnecessaryJumpsCheckBox: TCheckBox;
    GeneralAutoIndent: TLabel;
    IndentOnPasteCheckBox: TCheckBox;
    IndentOnLineBreakCheckBox: TCheckBox;
    IndentContextSensitiveCheckBox: TCheckBox;
    CursorBeyondEOLCheckBox: TCheckBox;
    IndentFileEdit: TFileNameEdit;
    IndentationGroupBox: TGroupBox;
    JumpingGroupBox: TGroupBox;
    IndentFileLabel: TLabel;
    JumpToMethodBodyCheckBox: TCheckBox;
    SkipForwardDeclarationsCheckBox: TCheckBox;
    JumpSingleLinePosLabel: TLabel;
    JumpSingleLinePosEdit: TSpinEdit;
    JumpCodeBlockPosEdit: TSpinEdit;
    JumpCodeBlockPosLabel: TLabel;
    procedure GeneralAutoIndentClick(Sender: TObject);
    procedure GeneralAutoIndentMouseEnter(Sender: TObject);
    procedure GeneralAutoIndentMouseLeave(Sender: TObject);
    procedure IndentOnLineBreakCheckBoxChange(Sender: TObject);
    procedure IndentOnPasteCheckBoxChange(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    procedure VisualizeIndentEnabled;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCodetoolsGeneralOptionsFrame }

procedure TCodetoolsGeneralOptionsFrame.IndentOnPasteCheckBoxChange(Sender: TObject);
begin
  VisualizeIndentEnabled;
end;

procedure TCodetoolsGeneralOptionsFrame.VisualizeIndentEnabled;
var
  e: Boolean;
begin
  e:=IndentOnLineBreakCheckBox.Checked or IndentOnPasteCheckBox.Checked;
  IndentFileLabel.Enabled:=e;
  IndentFileEdit.Enabled:=e;
  IndentContextSensitiveCheckBox.Enabled:=e;
end;

procedure TCodetoolsGeneralOptionsFrame.IndentOnLineBreakCheckBoxChange(Sender: TObject);
begin
  VisualizeIndentEnabled;
end;

procedure TCodetoolsGeneralOptionsFrame.GeneralAutoIndentClick(Sender: TObject);
begin
  FDialog.OpenEditor(GroupEditor,EdtOptionsIndent);
end;

procedure TCodetoolsGeneralOptionsFrame.GeneralAutoIndentMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := True;
  (Sender as TLabel).Font.Color := clRed;
end;

procedure TCodetoolsGeneralOptionsFrame.GeneralAutoIndentMouseLeave(
  Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := False;
  (Sender as TLabel).Font.Color := clBlue;
end;

function TCodetoolsGeneralOptionsFrame.GetTitle: String;
begin
  Result := lisGeneral;
end;

procedure TCodetoolsGeneralOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FDialog:=ADialog;

  JumpingGroupBox.Caption:=dlgJumpingETC;
  AdjustTopLineDueToCommentCheckBox.Caption:=dlgAdjustTopLine;
  JumpSingleLinePosLabel.Caption:=dlgJumpSingleLinePos;
  JumpCodeBlockPosLabel.Caption:=dlgJumpCodeBlockPos;
  AvoidUnnecessaryJumpsCheckBox.Caption:=dlgAvoidUnnecessaryJumps;
  CursorBeyondEOLCheckBox.Caption:=dlgcursorbeyondeol;
  SkipForwardDeclarationsCheckBox.Caption:=dlgSkipForwardClassDeclarations;
  JumpToMethodBodyCheckBox.Caption := dlgJumpToMethodBody;

  IndentationGroupBox.Caption:=lisIndentationForPascalSources;
  GeneralAutoIndent.Caption:=lisSetupDefaultIndentation;
  IndentOnLineBreakCheckBox.Caption:=lisOnBreakLineIEReturnOrEnterKey;
  IndentOnPasteCheckBox.Caption:=lisOnPasteFromClipboard;
  IndentFileLabel.Caption:=lisExampleFile;
  IndentContextSensitiveCheckBox.Caption:=lisContextSensitive;
  IndentContextSensitiveCheckBox.ShowHint:=true;
  IndentContextSensitiveCheckBox.Hint:=
    lisImitateIndentationOfCurrentUnitProjectOrPackage;
  IndentFileEdit.DialogTitle:=lisChooseAPascalFileForIndentationExamples;
  IndentFileEdit.Filter:=dlgFilterPascalFile+'|*.pas;*.pp;*.inc';
end;

procedure TCodetoolsGeneralOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodeToolsOptions do
  begin
    AdjustTopLineDueToCommentCheckBox.Checked := AdjustTopLineDueToComment;
    JumpCodeBlockPosEdit.Value := JumpCodeBlockPos;
    JumpSingleLinePosEdit.Value := JumpSingleLinePos;
    AvoidUnnecessaryJumpsCheckBox.Checked := AvoidUnnecessaryJumps;
    CursorBeyondEOLCheckBox.Checked := CursorBeyondEOL;
    SkipForwardDeclarationsCheckBox.Checked := SkipForwardDeclarations;
    JumpToMethodBodyCheckBox.Checked := JumpToMethodBody;
    IndentOnLineBreakCheckBox.Checked:=IndentOnLineBreak;
    IndentOnPasteCheckBox.Checked:=IndentOnPaste;
    IndentFileEdit.Text:=IndentationFileName;
    IndentContextSensitiveCheckBox.Checked:=IndentContextSensitive;
  end;
  VisualizeIndentEnabled;
end;

procedure TCodetoolsGeneralOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodeToolsOptions do
  begin
    AdjustTopLineDueToComment := AdjustTopLineDueToCommentCheckBox.Checked;
    JumpCodeBlockPos := JumpCodeBlockPosEdit.Value;
    JumpSingleLinePos := JumpSingleLinePosEdit.Value;
    AvoidUnnecessaryJumps := AvoidUnnecessaryJumpsCheckBox.Checked;
    CursorBeyondEOL := CursorBeyondEOLCheckBox.Checked;
    SkipForwardDeclarations := SkipForwardDeclarationsCheckBox.Checked;
    JumpToMethodBody:=JumpToMethodBodyCheckBox.Checked;
    IndentOnLineBreak:=IndentOnLineBreakCheckBox.Checked;
    IndentOnPaste:=IndentOnPasteCheckBox.Checked;
    IndentationFileName:=IndentFileEdit.Text;
    IndentContextSensitive:=IndentContextSensitiveCheckBox.Checked;
  end;
end;

class function TCodetoolsGeneralOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeToolsOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsGeneralOptionsFrame, CdtOptionsGeneral);
end.

