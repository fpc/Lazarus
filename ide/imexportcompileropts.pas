{ /***************************************************************************
                      imexportcompilerOpts.pas
                      ------------------------

 ***************************************************************************/

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

  Author: Mattias Gaertner
}
unit ImExportCompilerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  IDEProcs, FileUtil, Laz2_XMLCfg, LazFileCache, LCLType, EditBtn, Menus,
  ExtCtrls, ButtonPanel, MainIntf, LazarusIDEStrConsts, InputHistory, Project,
  CompilerOptions;

type
  { TImExportCompOptsDlg }

  TImExportCompOptsDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    ExportRadioGroup: TRadioGroup;
    FileLabel: TLabel;
    FileNameEdit: TFileNameEdit;
    HistoryLabel: TLabel;
    MenuItem1: TMenuItem;
    HistoryButton: TButton;
    RecentPopupMenu: TPopupMenu;
    procedure ImExportCompOptsDlgCLOSE(Sender: TObject; var CloseAction: TCloseAction);
    procedure ImExportCompOptsDlgCREATE(Sender: TObject);
    procedure OpenButtonCLICK(Sender: TObject);
    procedure PopupClick(Sender: TObject);
    procedure HistoryButtonClick(Sender: TObject);
    procedure RecentSaveButton1Click(Sender: TObject);
    procedure SaveButtonCLICK(Sender: TObject);
  private
    FFilename: string;
    procedure InitExport;
    procedure InitImport;
    procedure LoadRecentList;
    //procedure SaveRecentList;
    procedure SetFilename(const AValue: string);
    procedure DoOpenFile(const AFilename: string);
    procedure DoSaveFile(const AFilename: string);
  public
    property Filename: string read FFilename write SetFilename;
  end;

function ShowImportCompilerOptionsDialog: TModalResult;
function ShowExportCompilerOptionsDialog: TModalResult;

function DoImportCompilerOptions(const Filename: string): TModalResult;
function DoExportCompilerOptions(const Filename: string): TModalResult;

implementation

{$R *.lfm}

const
  DefaultCompilerOptPath = 'CompilerOptions/';

function ShowImportCompilerOptionsDialog: TModalResult;
var
  ImExportCompOptsDlg: TImExportCompOptsDlg;
begin
  ImExportCompOptsDlg := TImExportCompOptsDlg.Create(nil);
  try
    ImExportCompOptsDlg.InitImport;
    Result := ImExportCompOptsDlg.ShowModal;
    if Result = mrOk then
      Result := DoImportCompilerOptions(ImExportCompOptsDlg.Filename);
  finally
    ImExportCompOptsDlg.Free;
  end;
end;

function ShowExportCompilerOptionsDialog: TModalResult;
var
  ImExportCompOptsDlg: TImExportCompOptsDlg;
begin
  ImExportCompOptsDlg := TImExportCompOptsDlg.Create(nil);
  try
    ImExportCompOptsDlg.InitExport;
    Result := ImExportCompOptsDlg.ShowModal;
    if Result = mrOk then
      Result := DoExportCompilerOptions(ImExportCompOptsDlg.Filename);
  finally
    ImExportCompOptsDlg.Free;
  end;
end;

function ReadIntFromXMLConfig(const Filename, Path: string;
  DefaultValue, ValueForReadError: integer): integer;
var
  XMLConfig: TXMLConfig;
begin
  Result:=ValueForReadError;
  if FileExistsUTF8(Filename) then
    try
      XMLConfig:=TXMLConfig.Create(Filename);
      Result:=XMLConfig.GetValue(Path,DefaultValue);
    except
      Result:=ValueForReadError;
    end;
end;

function GetXMLPathForCompilerOptions(XMLConfig: TXMLConfig): string;
const
  OptPathSuffix = 'SearchPaths/CompilerPath/Value';
  PkgCompilerOptPath = 'Package/CompilerOptions/';
  PkgVersionPath = 'Package/Version';
var
  FileVersion: Integer;
begin
  if XMLConfig.GetValue(OptPathSuffix,'')<>'' then
    // old lpi file
    Result:=''
  else if XMLConfig.GetValue(DefaultCompilerOptPath+OptPathSuffix,'')<>'' then
    // current lpi file
    Result:=DefaultCompilerOptPath
  else if XMLConfig.GetValue(PkgCompilerOptPath+OptPathSuffix,'')<>'' then
    // current lpk file
    Result:=PkgCompilerOptPath
  else begin
    // default: depending on file type
    Result:=DefaultCompilerOptPath;
    if CompareFileExt(XMLConfig.Filename,'.lpk',false)=0 then begin
      FileVersion:=ReadIntFromXMLConfig(XMLConfig.Filename,PkgVersionPath,0,2);
      if FileVersion>=2 then
        Result:=PkgCompilerOptPath;   // current lpk file
    end;
  end;
end;

function DoImportCompilerOptions(const Filename: string): TModalResult;
var
  XMLConfig: TXMLConfig;
  Path: String;
begin
  Result := mrOk;
  try
    XMLConfig:=TXMLConfig.Create(Filename);
  except
    on E: Exception do
    begin
      Result:=MessageDlg(lisIECOErrorLoadingXml,
        Format(lisIECOErrorLoadingXmlFile, [Filename, LineEnding, E.Message]),
        mtError, [mbCancel], 0);
    end;
  end;
  try
    Path:=GetXMLPathForCompilerOptions(XMLConfig);
    Project1.CompilerOptions.LoadFromXMLConfig(XMLConfig,Path);
  finally
    XMLConfig.Free;
  end;
end;

function DoExportCompilerOptions(const Filename: string): TModalResult;
var
  XMLConfig: TXMLConfig;
  Path: String;
begin
  Result:=mrOk;
  try
    InvalidateFileStateCache;
    XMLConfig:=TXMLConfig.Create(Filename);
    try
      Path:=DefaultCompilerOptPath;
      Project1.CompilerOptions.SaveToXMLConfig(XMLConfig,Path);
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do
    begin
      Result:=MessageDlg(lisIECOErrorAccessingXml,
        Format(lisIECOErrorAccessingXmlFile, [Filename, LineEnding, E.Message]),
        mtError, [mbCancel], 0);
    end;
  end;
end;

{ TImExportCompOptsDlg }

procedure TImExportCompOptsDlg.InitImport;
begin
  Caption:=lisIECOImportCompilerOptions;
  FileNameEdit.Filter:='XML file (*.xml)|*.xml|'
                      +'Project file (*.lpi)|*.lpi|'
                      +'Package file (*.lpk)|*.lpk|'
                      //+'Session file (*.lps)|*.lps|'
                      +'All files (*)|*';
  FileNameEdit.DialogOptions:=FileNameEdit.DialogOptions+[ofFileMustExist];
  ExportRadioGroup.Visible:=False;
  with ButtonPanel1 do begin
    OKButton.Caption:=lisIECOLoadFromFile;
    OKButton.LoadGlyphFromStock(idButtonOpen);
    if OKButton.Glyph.Empty then
      OKButton.LoadGlyphFromResourceName(HInstance, 'laz_open');
    OKButton.OnClick:=@OpenButtonCLICK;
  end;
end;

procedure TImExportCompOptsDlg.InitExport;
begin
  Caption:=lisIECOExportCompilerOptions;
  FileNameEdit.Filter:='XML file (*.xml)|*.xml|All files (*)|*';
  FileNameEdit.DialogKind:=dkSave;
  with ButtonPanel1 do begin
    OKButton.Caption:=lisIECOSaveToFile;
    OKButton.LoadGlyphFromStock(idButtonSave);
    if OKButton.Glyph.Empty then
      OKButton.LoadGlyphFromResourceName(HInstance, 'laz_save');
    OKButton.OnClick:=@SaveButtonCLICK;
  end;
end;

procedure TImExportCompOptsDlg.ImExportCompOptsDlgCREATE(Sender: TObject);
begin
  HistoryLabel.Caption:=lisIECORecentFiles;
  HistoryLabel.Hint:=lisIECORecentFiles;
  FileLabel.Caption:=lisFile;
  LoadRecentList;
end;

procedure TImExportCompOptsDlg.OpenButtonCLICK(Sender: TObject);
begin
  DoOpenFile(CleanAndExpandFilename(FileNameEdit.FileName));
end;

procedure TImExportCompOptsDlg.PopupClick(Sender: TObject);
begin
  FileNameEdit.Text := (Sender as TMenuItem).Caption;
end;

procedure TImExportCompOptsDlg.HistoryButtonClick(Sender: TObject);
begin
  RecentPopupMenu.PopUp;
end;

procedure TImExportCompOptsDlg.RecentSaveButton1Click(Sender: TObject);
begin
  RecentPopupMenu.PopUp;
end;

procedure TImExportCompOptsDlg.SaveButtonCLICK(Sender: TObject);
begin
  DoSaveFile(CleanAndExpandFilename(FileNameEdit.FileName));
end;

procedure TImExportCompOptsDlg.ImExportCompOptsDlgCLOSE(Sender: TObject; var CloseAction: TCloseAction);
begin
  //SaveRecentList;
end;

procedure TImExportCompOptsDlg.LoadRecentList;
var
  sl: TStringList;
  mi: TMenuItem;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    RecentPopupMenu.Items.Clear;
    sl.Assign(InputHistories.HistoryLists.GetList(hlCompilerOptsImExport,true,rltFile));
    for i := 0 to sl.Count-1 do begin
      mi := TMenuItem.Create(RecentPopupMenu);
      mi.Caption:=sl[i];
      mi.OnClick:=@PopupClick;
      RecentPopupMenu.Items.Add(mi);
    end;
  finally
    sl.Free;
  end;
end;
{
procedure TImExportCompOptsDlg.SaveRecentList;
begin
  InputHistories.HistoryLists.GetList(hlCompilerOptsImExport,true,rltFile).Assign(
    RecentListbox.Items);
  InputHistories.Save;
end;
}
procedure TImExportCompOptsDlg.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
  InputHistories.HistoryLists.GetList(hlCompilerOptsImExport,true,rltFile).AppendEntry(FFilename);
  LoadRecentList;
end;

procedure TImExportCompOptsDlg.DoOpenFile(const AFilename: string);
begin
  if DirPathExists(AFilename) then exit;
  Filename := AFilename;
  ModalResult := mrOk;
end;

procedure TImExportCompOptsDlg.DoSaveFile(const AFilename: string);
var
  MsgResult: TModalResult;
begin
  if DirPathExists(AFilename) then exit;
  if ExtractFileExt(AFilename) = '' then
    Filename:=AFilename + '.xml'
  else
    Filename:=AFilename;
  if FileExistsUTF8(Filename) then begin
    MsgResult:=MessageDlg(lisIECOExportFileExists,
      Format(lisIECOExportFileExistsOpenFileAndReplaceOnlyCompilerOpti,
             [Filename, LineEnding, LineEnding]),
      mtConfirmation,[mbYes,mbCancel],0);
    if MsgResult<>mrYes then exit;
  end;
  ModalResult := mrOk;
end;

end.

