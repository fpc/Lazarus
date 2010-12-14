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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit UnitInfoDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, Controls, StdCtrls, Forms, Buttons,
  FileProcs, ExtCtrls, ComCtrls,
  CodeToolsDefPreview, CodeToolManager,
  LazarusIDEStrConsts;

type

  { TUnitInfoDialog }

  TUnitInfoDialog = class(TForm)
    GotoIncludeDirectiveButton: TButton;
    CodeToolsDefsButton: TButton;
    OkButton: TBitBtn;
    ClearIncludedBy: TButton;
    UnitPathMemo: TMemo;
    IncludePathMemo: TMemo;
    SrcPathMemo: TMemo;
    Notebook: TPageControl;
    OutIncludedBy: TLabel;
    OutInProject: TLabel;
    OutLines: TLabel;
    OutName: TLabel;
    OutPath: TLabel;
    OutSize: TLabel;
    OutType: TLabel;
    GeneralPage: TTabSheet;
    UnitPathsPage: TTabSheet;
    IncludePathsPage: TTabSheet;
    CompleteUnitPathsPage: TTabSheet;
    PathsGroupBox: TGroupBox;
    UIncludedBy: TLabel;
    UInProject: TLabel;
    ULines: TLabel;
    UName: TLabel;
    UPath: TLabel;
    USize: TLabel;
    UType: TLabel;
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
  const FilePath: string; const IncludedBy: string; var ClearIncludedBy: boolean;
  const UnitPath, IncludePath, SrcPath: string): TModalResult;

implementation

{$R *.lfm}

function ShowUnitInfoDlg(const AnUnitName, AType: string;
  IsPartOfProject: boolean;
  SizeInBytes, UnitSizeWithIncludeFiles, UnitSizeParsed,
  LineCount, UnitLineCountWithIncludes, UnitLineCountParsed: integer;
  const FilePath: string; const IncludedBy: string; var ClearIncludedBy: boolean;
  const UnitPath, IncludePath, SrcPath: string): TModalResult;
var Dlg: TUnitInfoDialog;
  s: String;
begin
  Dlg:=TUnitInfoDialog.Create(nil);
  with Dlg do begin
    Caption:=Format(lisInformationAboutUnit, [AnUnitName]);

    FFilePath:=FilePath;

    OutName.Caption:=AnUnitName;
    OutType.Caption:=AType;

    if IsPartOfProject then
      OutInProject.Caption:=lisUIDyes
    else
      OutInProject.Caption:=lisUIDno;

    s:=Format(lisUIDbytes, [IntToStr(SizeInBytes)]);
    if UnitSizeWithIncludeFiles<>SizeInBytes then
      s:=s+lisWithIncludes2+IntToStr(UnitSizeWithIncludeFiles);
    if UnitSizeParsed<>UnitSizeWithIncludeFiles then
      s:=s+lisParsed+IntToStr(UnitSizeParsed);
    OutSize.Caption:=s;

    s:=IntToStr(LineCount);
    if UnitLineCountWithIncludes<>LineCount then
      s:=s+lisWithIncludes2+IntToStr(UnitLineCountWithIncludes);
    if UnitLineCountParsed<>LineCount then
      s:=s+lisParsed+IntToStr(UnitLineCountParsed);
    OutLines.Caption:=s;

    OutPath.Caption:=FilePath;
    OutIncludedBy.Caption:=IncludedBy;

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
  Notebook.Page[0].Caption := lisMenuInsertGeneral;
  Notebook.Page[1].Caption := lisUnitPaths;
  Notebook.Page[2].Caption := lisIncludePaths;
  Notebook.Page[3].Caption := lisSourcePaths;
  Notebook.PageIndex := 0;

  UName.Caption:=lisUIDName;
  UType.Caption:=lisUIDType;
  UInProject.Caption:=lisUIDinProject;
  USize.Caption:=lisUIDSize;
  ULines.Caption:=lisUIDLines;
  UPath.Caption:=lisToFPCPath;
  UIncludedBy.Caption:=lisUIDIncludedBy;
  ClearIncludedBy.Caption:=lisUIClearIncludedByReference;
  CodeToolsDefsButton.Caption:=lisUIShowCodeToolsValues;
  GotoIncludeDirectiveButton.Caption:=lisMenuGotoIncludeDirective;
end;

procedure TUnitInfoDialog.clearIncludedByClick(Sender: TObject);
begin
  OutIncludedBy.Caption:='';
end;

function TUnitInfoDialog.getIncludedBy: string;
begin
  Result:=OutIncludedBy.Caption;
end;

end.
