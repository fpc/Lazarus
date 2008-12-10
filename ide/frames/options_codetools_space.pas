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
unit options_codetools_space;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, SynEdit,
  SourceChanger, CodeToolsOptions, LazarusIDEStrConsts, IDEOptionsIntf,
  EditorOptions;

type

  { TCodetoolsSpaceOptionsFrame }

  TCodetoolsSpaceOptionsFrame = class(TAbstractIDEOptionsEditor)
    DoInsertSpaceAfterGroupBox: TGroupBox;
    DoInsertSpaceInFrontGroupBox: TGroupBox;
    SpacePreviewLabel: TLabel;
    SpacePreviewSynEdit: TSynEdit;
    procedure UpdateExample(Sender: TObject);
  private
    BeautifyCodeOptions: TBeautifyCodeOptions;
    FHighlighter: TPreviewPasSyn;
    procedure CreateAtomCheckBoxes(ParentGroupBox: TGroupBox;
                                   AtomTypes: TAtomTypes; Columns: integer);
    procedure SetAtomCheckBoxes(AtomTypes: TAtomTypes; ParentGroupBox: TGroupBox);
    function ReadAtomCheckBoxes(ParentGroupBox: TGroupBox): TAtomTypes;
    procedure UpdateSpaceExample;
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

{ TCodetoolsSpaceOptionsFrame }

procedure TCodetoolsSpaceOptionsFrame.UpdateExample(Sender: TObject);
begin
  UpdateSpaceExample;
  UpdatePreviewSettings;
end;

procedure TCodetoolsSpaceOptionsFrame.CreateAtomCheckBoxes(
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

procedure TCodetoolsSpaceOptionsFrame.SetAtomCheckBoxes(AtomTypes: TAtomTypes;
  ParentGroupBox: TGroupBox);
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

function TCodetoolsSpaceOptionsFrame.ReadAtomCheckBoxes(
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

procedure TCodetoolsSpaceOptionsFrame.UpdateSpaceExample;
const
  SpaceExampleText =
       'function(Sender:TObject;const Val1,Val2,Val3:char;'
      +'var Var1,Var2:array of const):integer;'#13
      +'const i=1+2+3;'#13
      +'begin'#13
      +'  A:=@B.C;D:=3;E:=X[5];'#13
      +'  {$I unit1.lrs}'#13
      +'  {$R-}{$R+}'#13
      +'end;';
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
  Options := TEditorOptions.Create;
  try
    if Assigned(OnSaveIDEOptions) then
      OnSaveIDEOptions(Self, Options);
    SpacePreviewSynEdit.Highlighter := GetHighlighter(Options);
    Options.GetSynEditPreviewSettings(SpacePreviewSynEdit);
    SpacePreviewSynEdit.Gutter.Visible := False;
    SpacePreviewSynEdit.Options := SpacePreviewSynEdit.Options + [eoNoCaret, eoNoSelection];
    SpacePreviewSynEdit.ReadOnly := True;
  finally
    Options.Free;
  end;
end;

procedure TCodetoolsSpaceOptionsFrame.WriteBeautifyCodeOptions(
  Options: TBeautifyCodeOptions);
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

function TCodetoolsSpaceOptionsFrame.GetHighlighter(Options: TEditorOptions): TPreviewPasSyn;
begin
  if FHighlighter = nil then
  begin
    FHighlighter := TPreviewPasSyn.Create(Self);
    Options.AddSpecialHilightAttribsToHighlighter(FHighlighter);
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
    atPoint, atAt, atNumber, atStringConstant, atSymbol, atBracket];
begin
  with DoInsertSpaceInFrontGroupBox do begin
    Caption:=dlgInsSpaceFront;
    CreateAtomCheckBoxes(DoInsertSpaceInFrontGroupBox,DoInsertSpaceAtoms,2);
  end;

  with DoInsertSpaceAfterGroupBox do begin
    Caption:=dlgInsSpaceAfter;
    CreateAtomCheckBoxes(DoInsertSpaceAfterGroupBox,DoInsertSpaceAtoms,2);
  end;

  with SpacePreviewLabel do
    Caption:=dlgWRDPreview;
end;

procedure TCodetoolsSpaceOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions
  );
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
  {$I options_codetools_space.lrs}
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsSpaceOptionsFrame, CdtOptionsSpace);
end.

