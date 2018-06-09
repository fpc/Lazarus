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
unit codetools_linesplitting_options;

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
  { TCodetoolsLineSplittingOptionsFrame }

  TCodetoolsLineSplittingOptionsFrame = class(TCodetoolsAtomCheckboxesOptionsFrame)
    DoNotSplitLineAfterGroupBox: TGroupBox;
    DoNotSplitLineInFrontGroupBox: TGroupBox;
    LineLengthEdit: TEdit;
    LineLengthLabel: TLabel;
    SplitPreviewLabel: TLabel;
    SplitPreviewSynEdit: TSynEdit;
    procedure UpdateExample(Sender: TObject);
  private
    BeautifyCodeOptions: TBeautifyCodeOptions;
    FHighlighter: TPreviewPasSyn;
    procedure UpdateSplitLineExample;
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

{ TCodetoolsLineSplittingOptionsFrame }

procedure TCodetoolsLineSplittingOptionsFrame.UpdateExample(Sender: TObject);
begin
  UpdateSplitLineExample;
  UpdatePreviewSettings;
end;

procedure TCodetoolsLineSplittingOptionsFrame.UpdateSplitLineExample;
const
  LineSplitExampleText =
    'function F(Sender: TObject; const Val1, Val2, Val3:char; ' +
    'var Var1, Var2: array of const): integer; { comment // }'#13 +
    'const i=1+2+3;';
begin
  if BeautifyCodeOptions = nil then
    Exit;
  WriteBeautifyCodeOptions(BeautifyCodeOptions);
  BeautifyCodeOptions.LineLength := 1;
  SplitPreviewSynEdit.Text :=
    BeautifyCodeOptions.BeautifyStatement(LineSplitExampleText, 0);
end;

procedure TCodetoolsLineSplittingOptionsFrame.UpdatePreviewSettings;
var
  Options: TEditorOptions;
begin
  Options := EditorOpts;
  SplitPreviewSynEdit.Highlighter := GetHighlighter(Options);
  Options.GetSynEditPreviewSettings(SplitPreviewSynEdit);
  SplitPreviewSynEdit.Gutter.Visible := False;
  SplitPreviewSynEdit.Options := SplitPreviewSynEdit.Options + [eoNoCaret, eoNoSelection];
  SplitPreviewSynEdit.ReadOnly := True;
end;

procedure TCodetoolsLineSplittingOptionsFrame.WriteBeautifyCodeOptions(
  Options: TBeautifyCodeOptions);
var
  ACodeToolsOptions: TCodeToolsOptions;
begin
  ACodeToolsOptions := TCodeToolsOptions.Create;
  try
    WriteSettings(ACodeToolsOptions);
    Options.Assign(ACodeToolsOptions);
  finally
    ACodeToolsOptions.Free;
  end;
end;

function TCodetoolsLineSplittingOptionsFrame.GetHighlighter(
  Options: TEditorOptions): TPreviewPasSyn;
begin
  if FHighlighter = nil then
  begin
    FHighlighter := TPreviewPasSyn.Create(Self);
    Options.ReadHighlighterSettings(FHighlighter, '');
  end;
  Result := FHighlighter;
end;

constructor TCodetoolsLineSplittingOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeautifyCodeOptions := TBeautifyCodeOptions.Create;
  UpdateExample(nil);
end;

destructor TCodetoolsLineSplittingOptionsFrame.Destroy;
begin
  BeautifyCodeOptions.Free;
  inherited Destroy;
end;

function TCodetoolsLineSplittingOptionsFrame.GetTitle: String;
begin
  Result := dlgLineSplitting;
end;

procedure TCodetoolsLineSplittingOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
const
  DoNotSplitAtoms = [
    atKeyword, atIdentifier, atColon, atSemicolon, atComma,
    atPoint, atAt, atNumber, atStringConstant, atSpace, atSymbol, atBracket];
begin
  with LineLengthLabel do
    Caption:=dlgMaxLineLength;

  with DoNotSplitLineInFrontGroupBox do begin
    Caption:=dlgNotSplitLineFront;
    CreateAtomCheckBoxes(
      DoNotSplitLineInFrontGroupBox, DoNotSplitAtoms, 2, @UpdateExample);
  end;

  with DoNotSplitLineAfterGroupBox do begin
    Caption:=dlgNotSplitLineAfter;
    CreateAtomCheckBoxes(
      DoNotSplitLineAfterGroupBox, DoNotSplitAtoms, 2, @UpdateExample);
  end;

  with SplitPreviewLabel do
    Caption:=dlgCDTPreview;
end;

procedure TCodetoolsLineSplittingOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodetoolsOptions do
  begin
    LineLengthEdit.Text := IntToStr(LineLength);
    SetAtomCheckBoxes(DoNotSplitLineInFront, DoNotSplitLineInFrontGroupBox);
    SetAtomCheckBoxes(DoNotSplitLineAfter, DoNotSplitLineAfterGroupBox);
  end;
end;

procedure TCodetoolsLineSplittingOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodetoolsOptions do
  begin
    LineLength := StrToIntDef(LineLengthEdit.Text, 80);
    if LineLength < 5 then
      LineLength:=5;
    DoNotSplitLineInFront := ReadAtomCheckBoxes(DoNotSplitLineInFrontGroupBox);
    DoNotSplitLineAfter := ReadAtomCheckBoxes(DoNotSplitLineAfterGroupBox);
  end;
end;

class function TCodetoolsLineSplittingOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodetoolsOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsLineSplittingOptionsFrame, CdtOptionsLineSplitting);
end.

