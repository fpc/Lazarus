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
unit codetools_general_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, Buttons, Dialogs,
  IDEDialogs,
  CodeToolsOptions, LazarusIDEStrConsts, IDEOptionsIntf;

type

  { TCodetoolsGeneralOptionsFrame }

  TCodetoolsGeneralOptionsFrame = class(TAbstractIDEOptionsEditor)
    AdjustTopLineDueToCommentCheckBox: TCheckBox;
    IndentFileButton: TButton;
    CursorBeyondEOLCheckBox: TCheckBox;
    IndentFileEdit: TEdit;
    IndentationGroupBox: TGroupBox;
    JumpCenteredCheckBox: TCheckBox;
    JumpingGroupBox: TGroupBox;
    IndentFileLabel: TLabel;
    SkipForwardDeclarationsCheckBox: TCheckBox;
    SrcPathEdit: TEdit;
    SrcPathGroupBox: TGroupBox;
    procedure IndentFileButtonClick(Sender: TObject);
  private
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TCodetoolsGeneralOptionsFrame }

procedure TCodetoolsGeneralOptionsFrame.IndentFileButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Title:=lisChooseAPascalFileForIndentationExamples;
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
    if OpenDialog.Execute then
      IndentFileEdit.Text:=OpenDialog.FileName;
  finally
    OpenDialog.Free;
  end;
end;

function TCodetoolsGeneralOptionsFrame.GetTitle: String;
begin
  Result := lisMenuInsertGeneral;
end;

procedure TCodetoolsGeneralOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  with SrcPathGroupBox do
    Caption:=dlgAdditionalSrcPath;

  with JumpingGroupBox do
    Caption:=dlgJumpingETC;

  with AdjustTopLineDueToCommentCheckBox do
    Caption:=dlgAdjustTopLine;

  with JumpCenteredCheckBox do
    Caption:=dlgcentercursorline;

  with CursorBeyondEOLCheckBox do
    Caption:=dlgcursorbeyondeol;

  SkipForwardDeclarationsCheckBox.Caption:=dlgSkipForwardDeclarations;

  IndentationGroupBox.Caption:=lisIndentation;
  IndentFileLabel.Caption:=lisExampleFile;
  IndentFileButton.Caption:=lisPathEditBrowse;
end;

procedure TCodetoolsGeneralOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodeToolsOptions do
  begin
    SrcPathEdit.Text := SrcPath;
    AdjustTopLineDueToCommentCheckBox.Checked := AdjustTopLineDueToComment;
    JumpCenteredCheckBox.Checked := JumpCentered;
    CursorBeyondEOLCheckBox.Checked := CursorBeyondEOL;
    SkipForwardDeclarationsCheckBox.Checked := SkipForwardDeclarations;
    IndentFileEdit.Text:=IndentationFileName;
  end;
end;

procedure TCodetoolsGeneralOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodeToolsOptions do
  begin
    SrcPath := SrcPathEdit.Text;
    AdjustTopLineDueToComment := AdjustTopLineDueToCommentCheckBox.Checked;
    JumpCentered := JumpCenteredCheckBox.Checked;
    CursorBeyondEOL := CursorBeyondEOLCheckBox.Checked;
    SkipForwardDeclarations := SkipForwardDeclarationsCheckBox.Checked;
    IndentationFileName:=IndentFileEdit.Text;
  end;
end;

class function TCodetoolsGeneralOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeToolsOptions;
end;

initialization
  {$I codetools_general_options.lrs}
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsGeneralOptionsFrame, CdtOptionsGeneral);
end.

