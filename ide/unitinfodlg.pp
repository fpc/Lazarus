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
  Classes, SysUtils, LCLType, Controls, StdCtrls, Forms, Buttons, FileProcs,
  LazarusIDEStrConsts, CodeToolsDefPreview, CodeToolManager, ExtCtrls;

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
    Notebook: TNotebook;
    OutIncludedBy: TLabel;
    OutInProject: TLabel;
    OutLines: TLabel;
    OutName: TLabel;
    OutPath: TLabel;
    OutSize: TLabel;
    OutType: TLabel;
    GeneralPage: TPage;
    UnitPathsPage: TPage;
    IncludePathsPage: TPage;
    CompleteUnitPathsPage: TPage;
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
    procedure GotoIncludeDirectiveButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure UnitInfoDlgResize(Sender: TObject);
    procedure clearIncludedByClick(Sender: TObject);
  private
    FFilePath: string;
    function getIncludedBy: string;
  end;

function ShowUnitInfoDlg(const AnUnitName, AType: string;
  IsPartOfProject: boolean; SizeInBytes, LineCount: integer;
  const FilePath: string; const IncludedBy: string; var ClearIncludedBy: boolean;
  const UnitPath, IncludePath, SrcPath: string): TModalResult;

implementation

uses LResources;

function ShowUnitInfoDlg(const AnUnitName, AType: string;
  IsPartOfProject: boolean; SizeInBytes, LineCount: integer;
  const FilePath: string; const IncludedBy: string; var ClearIncludedBy: boolean;
  const UnitPath, IncludePath, SrcPath: string): TModalResult;
var Dlg: TUnitInfoDialog;
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

    OutSize.Caption:=Format(lisUIDbytes, [IntToStr(SizeInBytes)]);

    OutLines.Caption:=IntToStr(LineCount);
    OutPath.Caption:=FilePath;
    OutIncludedBy.Caption:=IncludedBy;

    UnitPathMemo.Lines.Delimiter := ';';
    UnitPathMemo.Lines.DelimitedText := MinimizeSearchPath(UnitPath);

    IncludePathMemo.Lines.Delimiter := ';';
    IncludePathMemo.Lines.DelimitedText := MinimizeSearchPath(IncludePath);

    SrcPathMemo.Lines.Delimiter := ';';
    SrcPathMemo.Lines.DelimitedText := MinimizeSearchPath(SrcPath);

    GotoIncludeDirectiveButton.Visible:=IncludedBy<>'';
  end;

  Result:=Dlg.ShowModal;
  ClearIncludedBy:=(Result in [mrOk,mrYes]) and (IncludedBy<>'')
                   and (Dlg.getIncludedBy='');
  Dlg.Free;
end;

{ TUnitInfoDialog }

procedure TUnitInfoDialog.UnitInfoDlgResize(Sender: TObject);
var MaxLength: integer;
begin
  MaxLength := UName.Width;

  if UType.Width > MaxLength then
    MaxLength := UType.Width;

  if UInProject.Width > MaxLength then
    MaxLength := UInProject.Width;

  if USize.Width > MaxLength then
    MaxLength := USize.Width;

  if ULines.Width > MaxLength then
    MaxLength := ULines.Width;

  if UPath.Width > MaxLength then
    MaxLength := UPath.Width;

  if UIncludedBy.Width > MaxLength then
    MaxLength := UIncludedBy.Width;

  UName.Width := MaxLength;
  UType.Width := MaxLength;
  UInProject.Width := MaxLength;
  USize.Width := MaxLength;
  ULines.Width := MaxLength;
  UPath.Width := MaxLength;
  UIncludedBy.Width := MaxLength;
end;

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
  ClearIncludedBy.Caption    := 'Clear included by reference';
  CodeToolsDefsButton.Caption:=lisUIShowCodeToolsValues;
end;

procedure TUnitInfoDialog.GotoIncludeDirectiveButtonClick(Sender: TObject);
begin

end;

procedure TUnitInfoDialog.OkButtonClick(Sender: TObject);
begin

end;

procedure TUnitInfoDialog.clearIncludedByClick(Sender: TObject);
begin
  OutIncludedBy.Caption:='';
end;

function TUnitInfoDialog.getIncludedBy: string;
begin
  Result:=OutIncludedBy.Caption;
end;

initialization
  {$I unitinfodlg.lrs}
  
end.
