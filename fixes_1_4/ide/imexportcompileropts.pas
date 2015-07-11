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

  Author: Mattias Gaertner, Juha Manninen
}
unit ImExportCompilerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Menus, ExtCtrls, EditBtn, ButtonPanel,
  IDEProcs, FileUtil, Laz2_XMLCfg, LazFileCache, LCLType, LazarusIDEStrConsts,
  MainIntf, IDEOptionsIntf, InputHistory, Project, CompilerOptions;

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
    procedure FileNameEditChangeImport(Sender: TObject);
    procedure FileNameEditChangeExport(Sender: TObject);
    procedure ImExportCompOptsDlgCLOSE(Sender: TObject; var CloseAction: TCloseAction);
    procedure ImExportCompOptsDlgCREATE(Sender: TObject);
    procedure OpenButtonCLICK(Sender: TObject);
    procedure PopupClick(Sender: TObject);
    procedure HistoryButtonClick(Sender: TObject);
    //procedure RecentSaveButton1Click(Sender: TObject);
    procedure SaveButtonCLICK(Sender: TObject);
  private
    FFilename: string;
    FParentDialog: TAbstractOptionsEditorDialog;
    procedure HideRadioButtons;
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

  { TOptsImExport }

  TOptsImExport = class
  private
    fOwner: TImExportCompOptsDlg;
    fXMLConfig: TXMLConfig;
    function GetXMLPathForCompilerOptions: string;
    function OpenXML(const Filename: string): TModalResult;
  public
    function DoImport(const Filename: string): TModalResult;
    function DoExportOptions(const Filename: string): TModalResult;
    function DoExportBuildModes(const Filename: string): TModalResult;
    constructor Create(aOwner: TImExportCompOptsDlg);
    destructor Destroy; override;
  end;

function ShowImportCompilerOptionsDialog(aDialog: TAbstractOptionsEditorDialog): TModalResult;
function ShowExportCompilerOptionsDialog(aDialog: TAbstractOptionsEditorDialog): TModalResult;


implementation

{$R *.lfm}

const
  DefaultCompilerOptPath = 'CompilerOptions/';

function ShowImportCompilerOptionsDialog(aDialog: TAbstractOptionsEditorDialog): TModalResult;
var
  ImExportCompOptsDlg: TImExportCompOptsDlg;
  Importer: TOptsImExport;
begin
  ImExportCompOptsDlg := TImExportCompOptsDlg.Create(nil);
  try
    ImExportCompOptsDlg.FParentDialog := aDialog;
    ImExportCompOptsDlg.InitImport;
    Result := ImExportCompOptsDlg.ShowModal;
    if Result <> mrOk then Exit;
    Importer := TOptsImExport.Create(ImExportCompOptsDlg);
    try
      Result := Importer.DoImport(ImExportCompOptsDlg.Filename);
    finally
      Importer.Free;
    end;
  finally
    ImExportCompOptsDlg.Free;
  end;
end;

function ShowExportCompilerOptionsDialog(aDialog: TAbstractOptionsEditorDialog): TModalResult;
var
  ImExportCompOptsDlg: TImExportCompOptsDlg;
  Exporter: TOptsImExport;
begin
  ImExportCompOptsDlg := TImExportCompOptsDlg.Create(nil);
  try
    ImExportCompOptsDlg.FParentDialog := aDialog;
    ImExportCompOptsDlg.InitExport;
    Result := ImExportCompOptsDlg.ShowModal;
    if Result <> mrOk then Exit;
    Exporter := TOptsImExport.Create(ImExportCompOptsDlg);
    try
      case ImExportCompOptsDlg.ExportRadioGroup.ItemIndex of
        0: Result := Exporter.DoExportOptions(ImExportCompOptsDlg.Filename);
        1: Result := Exporter.DoExportBuildModes(ImExportCompOptsDlg.Filename);
      end;
    finally
      Exporter.Free;
    end;
  finally
    ImExportCompOptsDlg.Free;
  end;
end;

{ TOptsImExport }

constructor TOptsImExport.Create(aOwner: TImExportCompOptsDlg);
begin
  inherited Create;
  fOwner := aOwner;
end;

destructor TOptsImExport.Destroy;
begin
  fXMLConfig.Free;
  inherited Destroy;
end;

function TOptsImExport.OpenXML(const Filename: string): TModalResult;
begin
  Result := mrOk;
  try
    fXMLConfig:=TXMLConfig.Create(Filename);
  except
    on E: Exception do
    begin
      Result:=MessageDlg(lisIECOErrorOpeningXml,
        Format(lisIECOErrorOpeningXmlFile, [Filename, LineEnding, E.Message]),
        mtError, [mbCancel], 0);
    end;
  end;
end;

function TOptsImExport.GetXMLPathForCompilerOptions: string;
const
  OptPathSuffix = 'SearchPaths/CompilerPath/Value';
  PkgCompilerOptPath = 'Package/CompilerOptions/';
  PkgVersionPath = 'Package/Version';
var
  FileVersion: Integer;
begin
  if fXMLConfig.GetValue(OptPathSuffix,'')<>'' then
    // old lpi file
    Result:=''
  else if fXMLConfig.GetValue(DefaultCompilerOptPath+OptPathSuffix,'')<>'' then
    // current lpi file
    Result:=DefaultCompilerOptPath
  else if fXMLConfig.GetValue(PkgCompilerOptPath+OptPathSuffix,'')<>'' then
    // current lpk file
    Result:=PkgCompilerOptPath
  else begin
    // default: depending on file type
    Result:=DefaultCompilerOptPath;
    if CompareFileExt(fXMLConfig.Filename,'.lpk',false)=0 then
    begin
      try
        FileVersion:=fXMLConfig.GetValue(PkgVersionPath,0);
      except
        FileVersion:=2;               // On error assume version 2.
      end;
      if FileVersion>=2 then
        Result:=PkgCompilerOptPath;   // current lpk file
    end;
  end;
end;

function TOptsImExport.DoImport(const Filename: string): TModalResult;
var
  Path: String;
begin
  Result := OpenXML(Filename);
  if Result <> mrOK then Exit;
  Path := GetXMLPathForCompilerOptions;
  if Assigned(fXMLConfig.FindNode('BuildModes',False)) then begin
    Project1.BuildModes.LoadProjOptsFromXMLConfig(fXMLConfig, '');
    fOwner.FParentDialog.UpdateBuildModeGUI;
    ShowMessageFmt(lisSuccessfullyImportedBuildModes, [Project1.BuildModes.Count, Filename]);
  end
  else if Assigned(fXMLConfig.FindNode(Path,False)) then begin
    Project1.CompilerOptions.LoadFromXMLConfig(fXMLConfig, Path);
    ShowMessageFmt(lisSuccessfullyImportedCompilerOptions, [Filename]);
  end
  else
    ShowMessageFmt(lisIECONoCompilerOptionsInFile, [Filename]);
end;

function TOptsImExport.DoExportOptions(const Filename: string): TModalResult;
begin
  Result := OpenXML(Filename);
  if Result <> mrOK then Exit;
  Project1.CompilerOptions.SaveToXMLConfig(fXMLConfig, DefaultCompilerOptPath);
  fXMLConfig.Flush;
  ShowMessageFmt(lisSuccessfullyExportedCompilerOptions, [Filename]);
end;

function TOptsImExport.DoExportBuildModes(const Filename: string): TModalResult;
begin
  Result := OpenXML(Filename);
  if Result <> mrOK then Exit;
  Project1.BuildModes.SaveProjOptsToXMLConfig(fXMLConfig, '', False);
  fXMLConfig.Flush;
  ShowMessageFmt(lisSuccessfullyExportedBuildModes, [Project1.BuildModes.Count, Filename]);
end;

{ TImExportCompOptsDlg }

procedure TImExportCompOptsDlg.HideRadioButtons;
begin
  Width:=FileNameEdit.Left + FileNameEdit.Width + 70; // Room for localized "File"
  ExportRadioGroup.Visible:=False;
end;

procedure TImExportCompOptsDlg.InitImport;
begin
  Caption:=lisIECOImportCompilerOptions;
  HideRadioButtons;
  FileNameEdit.Filter:='XML file (*.xml)|*.xml|'
                      +'Project file (*.lpi)|*.lpi|'
                      +'Package file (*.lpk)|*.lpk|'
                      +'Session file (*.lps)|*.lps|'
                      +'All files (*)|*';
  FileNameEdit.DialogOptions:=FileNameEdit.DialogOptions+[ofFileMustExist];
  FileNameEdit.OnChange:=@FileNameEditChangeImport;
  with ButtonPanel1 do begin
    OKButton.Caption:=lisIECOLoadFromFile;
    OKButton.LoadGlyphFromStock(idButtonOpen);
    if OKButton.Glyph.Empty then
      OKButton.LoadGlyphFromResourceName(HInstance, 'laz_open');
    OKButton.Enabled:=False;
    OKButton.OnClick:=@OpenButtonCLICK;
  end;
end;

procedure TImExportCompOptsDlg.InitExport;
begin
  Caption:=lisIECOExportCompilerOptions;
  FileNameEdit.Filter:='XML file (*.xml)|*.xml|All files (*)|*';
  FileNameEdit.DialogKind:=dkSave;
  FileNameEdit.OnChange:=@FileNameEditChangeExport;
  if Project1.BuildModes.Count <= 1 then
    HideRadioButtons;
  with ButtonPanel1 do begin
    OKButton.Caption:=lisIECOSaveToFile;
    OKButton.LoadGlyphFromStock(idButtonSave);
    if OKButton.Glyph.Empty then
      OKButton.LoadGlyphFromResourceName(HInstance, 'laz_save');
    OKButton.Enabled:=False;
    OKButton.OnClick:=@SaveButtonCLICK;
  end;
end;

procedure TImExportCompOptsDlg.ImExportCompOptsDlgCREATE(Sender: TObject);
begin
  HistoryLabel.Caption:=lisIECORecentFiles;
  HistoryLabel.Hint:=lisIECORecentFiles;
  FileLabel.Caption:=lisFile;
  ExportRadioGroup.Caption:=lisIECOCompilerOptionsOf;
  ExportRadioGroup.Items.Strings[0]:=lisIECOCurrentBuildMode;
  ExportRadioGroup.Items.Strings[1]:=lisIECOAllBuildModes;
  LoadRecentList;
end;

procedure TImExportCompOptsDlg.FileNameEditChangeImport(Sender: TObject);
begin
  ButtonPanel1.OKButton.Enabled := FileExistsUTF8((Sender as TFileNameEdit).FileName);
end;

procedure TImExportCompOptsDlg.FileNameEditChangeExport(Sender: TObject);
begin
  ButtonPanel1.OKButton.Enabled := (Sender as TFileNameEdit).FileName <> '';
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
{
procedure TImExportCompOptsDlg.RecentSaveButton1Click(Sender: TObject);
begin
  RecentPopupMenu.PopUp;
end;
}
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
    HistoryLabel.Visible := sl.Count > 0;
    HistoryButton.Visible := HistoryLabel.Visible;
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

