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
unit codetools_space_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  StdCtrls,
  // CodeTools
  SourceChanger,
  // SynEdit
  SynEdit,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  EditorOptions, atom_checkboxes_options;

type

  { TCodetoolsSpaceOptionsFrame }

  TCodetoolsSpaceOptionsFrame = class(TCodetoolsAtomCheckboxesOptionsFrame)
    DoInsertSpaceAfterGroupBox: TGroupBox;
    DoInsertSpaceInFrontGroupBox: TGroupBox;
    SpacePreviewLabel: TLabel;
    SpacePreviewSynEdit: TSynEdit;
    procedure UpdateExample(Sender: TObject);
  private
    BeautifyCodeOptions: TBeautifyCodeOptions;
    FHighlighter: TPreviewPasSyn;
    procedure UpdateSpaceExample;
    procedure UpdatePreviewSettings;
    procedure WriteBeautifyCodeOptions(Options: TBeautifyCodeOptions);
    function GetHighlighter(Options: TEditorOptions): TPreviewPasSyn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

uses
  CodeToolsOptions, LazarusIDEStrConsts;

{ TCodetoolsSpaceOptionsFrame }

procedure TCodetoolsSpaceOptionsFrame.UpdateExample(Sender: TObject);
begin
  UpdateSpaceExample;
  UpdatePreviewSettings;
end;

procedure TCodetoolsSpaceOptionsFrame.UpdateSpaceExample;
const
  SpaceExampleText =
    'function F(Sender:TObject;const Val1,Val2,Val3:char;' +
    'var Var1,Var2:array of const):integer;'#13 +
    'const i=1+2+3;'#13 +
    'begin'#13 +
    '  A:=@B.C;D:=3;E:=X[5];'#13 +
    '  PRec^.F:=PArr^[5];'#13 +
    '  {$I unit1.lrs}'#13 +
    '  {$R-}{$R+}'#13 +
    '  // ąčęęėįšųūž'#13+ //do not change this, Unicode characters are threated as symbols by CodeTools
    'end;';
begin
  if BeautifyCodeOptions = nil then
    Exit;
  WriteBeautifyCodeOptions(BeautifyCodeOptions);
  BeautifyCodeOptions.LineLength := 40;
  SpacePreviewSynEdit.Text := BeautifyCodeOptions.BeautifyStatement(
    SpaceExampleText, 0);
end;

procedure TCodetoolsSpaceOptionsFrame.UpdatePreviewSettings;
var
  Options: TEditorOptions;
begin
  Options := EditorOpts;
  SpacePreviewSynEdit.Highlighter := GetHighlighter(Options);
  Options.GetSynEditPreviewSettings(SpacePreviewSynEdit);
  SpacePreviewSynEdit.Gutter.Visible := False;
  SpacePreviewSynEdit.Options := SpacePreviewSynEdit.Options + [eoNoCaret, eoNoSelection];
  SpacePreviewSynEdit.ReadOnly := True;
end;

procedure TCodetoolsSpaceOptionsFrame.WriteBeautifyCodeOptions(
  Options: TBeautifyCodeOptions);
begin
  with Options do
  begin
    DoInsertSpaceInFront := ReadAtomCheckBoxes(DoInsertSpaceInFrontGroupBox);
    DoInsertSpaceAfter := ReadAtomCheckBoxes(DoInsertSpaceAfterGroupBox);
  end;
end;

function TCodetoolsSpaceOptionsFrame.GetHighlighter(Options: TEditorOptions): TPreviewPasSyn;
begin
  if FHighlighter = nil then
  begin
    FHighlighter := TPreviewPasSyn.Create(Self);
    Options.ReadHighlighterSettings(FHighlighter, '');
  end;
  Result := FHighlighter;
end;

constructor TCodetoolsSpaceOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeautifyCodeOptions := TBeautifyCodeOptions.Create;
  UpdateExample(nil);
end;

destructor TCodetoolsSpaceOptionsFrame.Destroy;
begin
  BeautifyCodeOptions.Free;
  inherited Destroy;
end;

function TCodetoolsSpaceOptionsFrame.GetTitle: String;
begin
  Result := dlgSpaceNotCosmos;
end;

procedure TCodetoolsSpaceOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
const
  DoInsertSpaceAtoms = [
    atKeyword, atIdentifier, atColon, atSemicolon, atComma,
    atPoint, atAt, atNumber, atStringConstant, atSymbol, atBracket, atCaret];
begin
  with DoInsertSpaceInFrontGroupBox do begin
    Caption:=dlgInsSpaceFront;
    CreateAtomCheckBoxes(
      DoInsertSpaceInFrontGroupBox, DoInsertSpaceAtoms, 2, @UpdateExample);
  end;

  with DoInsertSpaceAfterGroupBox do begin
    Caption:=dlgInsSpaceAfter;
    CreateAtomCheckBoxes(
      DoInsertSpaceAfterGroupBox, DoInsertSpaceAtoms, 2, @UpdateExample);
  end;

  with SpacePreviewLabel do
    Caption:=dlgWRDPreview;
end;

procedure TCodetoolsSpaceOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodetoolsOptions do
  begin
    SetAtomCheckBoxes(DoInsertSpaceInFront, DoInsertSpaceInFrontGroupBox);
    SetAtomCheckBoxes(DoInsertSpaceAfter, DoInsertSpaceAfterGroupBox);
  end;
end;

procedure TCodetoolsSpaceOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodetoolsOptions do
  begin
    DoInsertSpaceInFront := ReadAtomCheckBoxes(DoInsertSpaceInFrontGroupBox);
    DoInsertSpaceAfter := ReadAtomCheckBoxes(DoInsertSpaceAfterGroupBox);
  end;
end;

class function TCodetoolsSpaceOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodetoolsOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsSpaceOptionsFrame, CdtOptionsSpace);
end.

