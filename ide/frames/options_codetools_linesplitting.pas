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
unit options_codetools_linesplitting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, SynEdit,
  SourceChanger, CodeToolsOptions, LazarusIDEStrConsts, IDEOptionsIntf,
  EditorOptions;

type
  { TCodetoolsLineSplittingOptionsFrame }

  TCodetoolsLineSplittingOptionsFrame = class(TAbstractIDEOptionsEditor)
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
    procedure CreateAtomCheckBoxes(ParentGroupBox: TGroupBox;
                                   AtomTypes: TAtomTypes; Columns: integer);
    procedure SetAtomCheckBoxes(AtomTypes: TAtomTypes; ParentGroupBox: TGroupBox);
    function ReadAtomCheckBoxes(ParentGroupBox: TGroupBox): TAtomTypes;
    procedure UpdateSplitLineExample;
    procedure UpdatePreviewSettings;
    procedure WriteBeautifyCodeOptions(Options: TBeautifyCodeOptions);
    function GetHighlighter(Options: TEditorOptions): TPreviewPasSyn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TCodetoolsLineSplittingOptionsFrame }

procedure TCodetoolsLineSplittingOptionsFrame.UpdateExample(Sender: TObject);
begin
  UpdateSplitLineExample;
  UpdatePreviewSettings;
end;

procedure TCodetoolsLineSplittingOptionsFrame.CreateAtomCheckBoxes(
  ParentGroupBox: TGroupBox; AtomTypes: TAtomTypes; Columns: integer);
var
  Count, i, yi, MaxYCount: integer;
  a: TAtomType;
  X, Y, CurX, CurY, XStep, YStep: integer;
  NewCheckBox: TCheckBox;
begin
  if Columns < 1 then
    Columns := 1;
  Count := 0;
  for a := Low(TAtomTypes) to High(TAtomTypes) do
    if a in AtomTypes then
      inc(Count);
  if Count = 0 then
    Exit;

  MaxYCount := ((Count+Columns-1) div Columns);
  X:=6;
  Y:=1;
  XStep:=((ParentGroupBox.ClientWidth-10) div Columns);
  YStep:=((ParentGroupBox.ClientHeight-20) div MaxYCount);
  CurX:=X;
  CurY:=Y;
  i:=0;
  yi:=0;

  for a := Low(TAtomTypes) to High(TAtomTypes) do
  begin
    if a in AtomTypes then
    begin
      inc(i);
      inc(yi);
      NewCheckBox:=TCheckBox.Create(ParentGroupBox);
      with NewCheckBox do
      begin
        Name:=ParentGroupBox.Name+'CheckBox'+IntToStr(i+1);
        Parent:=ParentGroupBox;
        SetBounds(CurX,CurY,XStep-10,Height);
        Caption:=GetTranslatedAtomTypes(a);
        OnClick:=@UpdateExample;
        Visible:=true;
      end;
      if yi>=MaxYCount then
      begin
        inc(X,XStep);
        CurX:=X;
        CurY:=Y;
        yi:=0;
      end
      else
        inc(CurY,YStep);
    end;
  end;
end;

procedure TCodetoolsLineSplittingOptionsFrame.SetAtomCheckBoxes(
  AtomTypes: TAtomTypes; ParentGroupBox: TGroupBox);
var
  i: integer;
  ACheckBox: TCheckBox;
  a: TAtomType;
begin
  for i := 0 to ParentGroupBox.ComponentCount - 1 do
  begin
    if (ParentGroupBox.Components[i] is TCheckBox) then
    begin
      ACheckBox:=TCheckBox(ParentGroupBox.Components[i]);
      a := TranslatedAtomToType(ACheckBox.Caption);
      ACheckBox.Checked := (a <> atNone) and (a in AtomTypes);
    end;
  end;
end;

function TCodetoolsLineSplittingOptionsFrame.ReadAtomCheckBoxes(
  ParentGroupBox: TGroupBox): TAtomTypes;
var
  i: integer;
  ACheckBox: TCheckBox;
  a: TAtomType;
begin
  Result := [];
  for i := 0 to ParentGroupBox.ComponentCount - 1 do
  begin
    if (ParentGroupBox.Components[i] is TCheckBox) then
    begin
      ACheckBox := TCheckBox(ParentGroupBox.Components[i]);
      a := TranslatedAtomToType(ACheckBox.Caption);
      if (a <> atNone) and (ACheckBox.Checked) then
        Include(Result, a);
    end;
  end;
end;

procedure TCodetoolsLineSplittingOptionsFrame.UpdateSplitLineExample;
const
  LineSplitExampleText =
       'function(Sender: TObject; const Val1, Val2, Val3:char; '
      +'var Var1, Var2: array of const): integer;'#13
      +'const i=1+2+3;';
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
  Options := TEditorOptions.Create;
  try
    if Assigned(OnSaveIDEOptions) then
      OnSaveIDEOptions(Self, Options);
    SplitPreviewSynEdit.Highlighter := GetHighlighter(Options);
    Options.GetSynEditPreviewSettings(SplitPreviewSynEdit);
    SplitPreviewSynEdit.Gutter.Visible := False;
    SplitPreviewSynEdit.Options := SplitPreviewSynEdit.Options + [eoNoCaret, eoNoSelection];
    SplitPreviewSynEdit.ReadOnly := True;
  finally
    Options.Free;
  end;
end;

procedure TCodetoolsLineSplittingOptionsFrame.WriteBeautifyCodeOptions(Options: TBeautifyCodeOptions);
var
  ACodeToolsOptions: TCodeToolsOptions;
begin
  ACodeToolsOptions := TCodeToolsOptions.Create;
  try
    if Assigned(OnSaveIDEOptions) then
      OnSaveIDEOptions(Self, ACodeToolsOptions);
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
    Options.AddSpecialHilightAttribsToHighlighter(FHighlighter);
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

procedure TCodetoolsLineSplittingOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
const
  DoNotSplitAtoms = [
    atKeyword, atIdentifier, atColon, atSemicolon, atComma,
    atPoint, atAt, atNumber, atStringConstant, atSpace, atSymbol, atBracket];
begin
  with LineLengthLabel do
    Caption:=dlgMaxLineLength;

  with DoNotSplitLineInFrontGroupBox do begin
    Caption:=dlgNotSplitLineFront ;
    CreateAtomCheckBoxes(DoNotSplitLineInFrontGroupBox,DoNotSplitAtoms,2);
  end;

  with DoNotSplitLineAfterGroupBox do begin
    Caption:=dlgNotSplitLineAfter ;
    CreateAtomCheckBoxes(DoNotSplitLineAfterGroupBox,DoNotSplitAtoms,2);
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
  {$I options_codetools_linesplitting.lrs}
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsLineSplittingOptionsFrame, CdtOptionsLineSplitting);
end.

