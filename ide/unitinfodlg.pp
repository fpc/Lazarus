{
  Author: Jens Arm

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
unit UnitInfoDlg;

{$mode objfpc}{$H+}

interface

uses
  // LCL + FCL
  SysUtils, LCLProc, Controls, StdCtrls, Forms, Buttons, ComCtrls,
  // CodeTools
  CodeToolsDefPreview, CodeToolManager, FileProcs,
  // LazUtils
  LazFileUtils,
  // IDE
  LazarusIDEStrConsts;

type

  { TUnitInfoDialog }

  TUnitInfoDialog = class(TForm)
    GotoIncludeDirectiveButton: TButton;
    CodeToolsDefsButton: TButton;
    ListValues: TListView;
    OkButton: TBitBtn;
    ClearIncludedBy: TButton;
    UnitPathMemo: TMemo;
    IncludePathMemo: TMemo;
    SrcPathMemo: TMemo;
    Notebook: TPageControl;
    GeneralPage: TTabSheet;
    UnitPathsPage: TTabSheet;
    IncludePathsPage: TTabSheet;
    CompleteUnitPathsPage: TTabSheet;
    PathsGroupBox: TGroupBox;
    procedure CodeToolsDefsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure clearIncludedByClick(Sender: TObject);
  private
    FFilePath: string;
    function getIncludedBy: string;
  end;

function ShowUnitInfoDlg(const AnUnitName, AType: string;
  IsPartOfProject: boolean; SizeInBytes, UnitSizeWithIncludeFiles, UnitSizeParsed,
  LineCount, UnitLineCountWithIncludes, UnitLineCountParsed: integer;
  const FilePath: string; const IncludedBy: string; out ClearIncludedBy: boolean;
  const UnitPath, IncludePath, SrcPath: string): TModalResult;

implementation

{$R *.lfm}

function ShowUnitInfoDlg(const AnUnitName, AType: string;
  IsPartOfProject: boolean; SizeInBytes, UnitSizeWithIncludeFiles,
  UnitSizeParsed, LineCount, UnitLineCountWithIncludes,
  UnitLineCountParsed: integer; const FilePath: string;
  const IncludedBy: string; out ClearIncludedBy: boolean; const UnitPath,
  IncludePath, SrcPath: string): TModalResult;
var Dlg: TUnitInfoDialog;
  s: String;
begin
  ClearIncludedBy:=false;

  Dlg:=TUnitInfoDialog.Create(nil);
  with Dlg do begin
    Caption:=Format(lisInformationAboutUnit, [AnUnitName]);

    FFilePath:=FilePath;

    ListValues.Items[0].SubItems[0]:=AnUnitName;
    ListValues.Items[1].SubItems[0]:=AType;

    if IsPartOfProject then s:=lisUIDyes else s:=lisUIDno;
    ListValues.Items[2].SubItems[0]:=s;

    s:=Format(lisUIDbytes, [IntToStr(SizeInBytes)]);
    if UnitSizeWithIncludeFiles<>SizeInBytes then
      s:=s+lisWithIncludes2+IntToStr(UnitSizeWithIncludeFiles);
    if UnitSizeParsed<>UnitSizeWithIncludeFiles then
      s:=s+lisParsed+IntToStr(UnitSizeParsed);
    ListValues.Items[3].SubItems[0]:=s;

    s:=IntToStr(LineCount);
    if UnitLineCountWithIncludes<>LineCount then
      s:=s+lisWithIncludes2+IntToStr(UnitLineCountWithIncludes);
    if UnitLineCountParsed<>LineCount then
      s:=s+lisParsed+IntToStr(UnitLineCountParsed);
    ListValues.Items[4].SubItems[0]:=s;

    ListValues.Items[5].SubItems[0]:=FilePath;
    ListValues.Items[6].SubItems[0]:=IncludedBy;

    UnitPathMemo.Lines.Delimiter := ';';
    UnitPathMemo.Lines.StrictDelimiter := true;
    UnitPathMemo.Lines.DelimitedText := MinimizeSearchPath(UnitPath);

    IncludePathMemo.Lines.Delimiter := ';';
    IncludePathMemo.Lines.StrictDelimiter := true;
    IncludePathMemo.Lines.DelimitedText := MinimizeSearchPath(IncludePath);

    SrcPathMemo.Lines.Delimiter := ';';
    SrcPathMemo.Lines.StrictDelimiter := true;
    SrcPathMemo.Lines.DelimitedText := MinimizeSearchPath(SrcPath);

    GotoIncludeDirectiveButton.Visible:=IncludedBy<>'';
  end;

  Result:=Dlg.ShowModal;
  ClearIncludedBy:=(Result in [mrOk,mrYes]) and (IncludedBy<>'')
                   and (Dlg.getIncludedBy='');
  Dlg.Free;
end;

{ TUnitInfoDialog }

procedure TUnitInfoDialog.CodeToolsDefsButtonClick(Sender: TObject);
begin
  DebugLn(['TUnitInfoDialog.CodeToolsDefsButtonClick FFilePath=',FFilePath]);
  ShowCodeToolsDefinesValuesDialog(CodeToolBoss.DefineTree, ExtractFilePath(FFilePath));
end;

procedure TUnitInfoDialog.FormCreate(Sender: TObject);
begin
  Notebook.Page[0].Caption := lisGeneral;
  Notebook.Page[1].Caption := lisUnitPaths;
  Notebook.Page[2].Caption := lisIncludePaths;
  Notebook.Page[3].Caption := lisSourcePaths;
  Notebook.PageIndex := 0;

  with ListValues do
  begin
    with Items.Add do begin Caption:= lisUIDName; SubItems.Add(''); end;
    with Items.Add do begin Caption:= lisUIDType; SubItems.Add(''); end;
    with Items.Add do begin Caption:= lisUIDinProject; SubItems.Add(''); end;
    with Items.Add do begin Caption:= lisUIDSize; SubItems.Add(''); end;
    with Items.Add do begin Caption:= lisUIDLines; SubItems.Add(''); end;
    with Items.Add do begin Caption:= lisToFPCPath; SubItems.Add(''); end;
    with Items.Add do begin Caption:= lisUIDIncludedBy; SubItems.Add(''); end;
  end;

  ClearIncludedBy.Caption:=lisUIClearIncludedByReference;
  CodeToolsDefsButton.Caption:=lisUIShowCodeToolsValues;
  GotoIncludeDirectiveButton.Caption:=lisMenuGotoIncludeDirective;
end;

procedure TUnitInfoDialog.clearIncludedByClick(Sender: TObject);
begin
  ListValues.Items[6].SubItems[0]:='';
end;

function TUnitInfoDialog.getIncludedBy: string;
begin
  Result:=ListValues.Items[6].SubItems[0];
end;

end.
