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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner, Juha Manninen
}
unit ImExportCompilerOpts;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  LCLType, Forms, Controls, Dialogs, StdCtrls, Buttons, ExtCtrls, ButtonPanel,
  // LazUtils
  FileUtil, LazFileUtils, Laz2_XMLCfg,
  // IdeIntf
  IDEOptEditorIntf, IDEImagesIntf,
  // IDE
  IDEProcs, LazarusIDEStrConsts, InputHistory, Project, CompilerOptions;

type
  { TImExportCompOptsDlg }

  TImExportCompOptsDlg = class(TForm)
    ButtonBrowse: TButton;
    ButtonPanel1: TButtonPanel;
    ExportRadioGroup: TRadioGroup;
    FileLabel: TLabel;
    FileNameEdit: TComboBox;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    procedure ButtonBrowseClick(Sender: TObject);
    procedure FileNameEditChange(Sender: TObject);
    procedure ImExportCompOptsDlgCLOSE(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure ImExportCompOptsDlgCREATE(Sender: TObject);
    procedure OpenButtonCLICK(Sender: TObject);
    procedure SaveButtonCLICK(Sender: TObject);
  private
    FFilename: string;
    FParentDialog: TAbstractOptionsEditorDialog;
    FExport: boolean;
    procedure InitExport(AOptions: TBaseCompilerOptions);
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
    fOptions: TBaseCompilerOptions; // TProjectCompilerOptions or TPkgCompilerOptions
    fImExportDlg: TImExportCompOptsDlg;
    fXMLConfig: TXMLConfig;
    function GetXMLPathForCompilerOptions: string;
    function OpenXML(const Filename: string): TModalResult;
  public
    function DoImport(const Filename: string): TModalResult;
    function DoExportOptions(const Filename: string): TModalResult;
    function DoExportBuildModes(const Filename: string): TModalResult;
    constructor Create(AOptions: TBaseCompilerOptions; aImExportDlg: TImExportCompOptsDlg);
    destructor Destroy; override;
  end;

function ShowImportCompilerOptionsDialog(AOptions: TBaseCompilerOptions;
  aDialog: TAbstractOptionsEditorDialog): TModalResult;
function ShowExportCompilerOptionsDialog(AOptions: TBaseCompilerOptions;
  aDialog: TAbstractOptionsEditorDialog): TModalResult;


implementation

{$R *.lfm}

const
  DefaultCompilerOptPath = 'CompilerOptions/';

function ShowImportCompilerOptionsDialog(AOptions: TBaseCompilerOptions;
  aDialog: TAbstractOptionsEditorDialog): TModalResult;
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
    Importer := TOptsImExport.Create(AOptions, ImExportCompOptsDlg);
    try
      Result := Importer.DoImport(ImExportCompOptsDlg.Filename);
    finally
      Importer.Free;
    end;
  finally
    ImExportCompOptsDlg.Free;
  end;
end;

function ShowExportCompilerOptionsDialog(AOptions: TBaseCompilerOptions;
  aDialog: TAbstractOptionsEditorDialog): TModalResult;
var
  ImExportCompOptsDlg: TImExportCompOptsDlg;
  Exporter: TOptsImExport;
begin
  ImExportCompOptsDlg := TImExportCompOptsDlg.Create(nil);
  try
    ImExportCompOptsDlg.FParentDialog := aDialog;
    ImExportCompOptsDlg.InitExport(AOptions);
    Result := ImExportCompOptsDlg.ShowModal;
    if Result <> mrOk then Exit;
    Exporter := TOptsImExport.Create(AOptions, ImExportCompOptsDlg);
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

constructor TOptsImExport.Create(AOptions: TBaseCompilerOptions;
  aImExportDlg: TImExportCompOptsDlg);
begin
  inherited Create;
  fOptions := AOptions;
  fImExportDlg := aImExportDlg;
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
        FileVersion:=fXMLConfig.GetValue(PkgVersionPath,2);
      except
        FileVersion:=2;               // On error assume version 2.
      end;
      if FileVersion>=2 then
        Result:=PkgCompilerOptPath;   // current lpk file
    end;
  end;
end;

function TOptsImExport.DoImport(const Filename: string): TModalResult;
// Import options to project or package. Only projects support BuildModes.
var
  CompOptPath: String;
begin
  Result := OpenXML(Filename);
  if Result <> mrOK then Exit;
  CompOptPath := GetXMLPathForCompilerOptions;
  if Assigned(fXMLConfig.FindNode('BuildModes',False)) then
  begin                                        // The import file has BuildModes.
    if fOptions is TProjectCompilerOptions then
    begin                                      // Project options
      Project1.BuildModes.LoadProjOptsFromXMLConfig(fXMLConfig, '');
      fImExportDlg.FParentDialog.UpdateBuildModeGUI;
      ShowMessageFmt(lisSuccessfullyImportedBuildModes, [Project1.BuildModes.Count, Filename]);
    end
    else begin                                 // Package options
      ShowMessage(lisImportingBuildModesNotSupported);
      Result := mrAbort;
    end;
  end
  else if Assigned(fXMLConfig.FindNode(CompOptPath,False)) then
  begin
    fOptions.LoadFromXMLConfig(fXMLConfig, CompOptPath);
    ShowMessageFmt(lisSuccessfullyImportedCompilerOptions, [Filename]);
  end
  else
    ShowMessageFmt(lisIECONoCompilerOptionsInFile, [Filename]);
end;

function TOptsImExport.DoExportOptions(const Filename: string): TModalResult;
// Export options of project or package.
begin
  Result := OpenXML(Filename);
  if Result <> mrOK then Exit;
  fOptions.SaveToXMLConfig(fXMLConfig, DefaultCompilerOptPath);
  fXMLConfig.Flush;
  ShowMessageFmt(lisSuccessfullyExportedCompilerOptions, [Filename]);
end;

function TOptsImExport.DoExportBuildModes(const Filename: string): TModalResult;
// Export options of project's BuildModes. Packages don't have BuildModes.
begin
  Result := OpenXML(Filename);
  if Result <> mrOK then Exit;
  Project1.BuildModes.SaveProjOptsToXMLConfig(fXMLConfig, '', False, True);
  fXMLConfig.Flush;
  ShowMessageFmt(lisSuccessfullyExportedBuildModes, [Project1.BuildModes.Count, Filename]);
end;

{ TImExportCompOptsDlg }

procedure TImExportCompOptsDlg.InitImport;
begin
  FExport:=false;
  Caption:=lisIECOImportCompilerOptions;
  // ToDo: Later when the XML file is selected, ExportRadioGroup can be made
  //  Visible but disabled. Then ItemIndex must be set according to file contents.
  //  If file contains one mode -> ItemIndex:=0. If more modes -> ItemIndex:=1.
  ExportRadioGroup.Visible:=False;

  OpenDlg.Filter:=Format('%s|*.xml|'
                        +'%s|*.lpi|'
                        +'%s|*.lpk|'
                        +'%s|*.lps|'
                        +'%s|%s',
                      [dlgFilterXML,
                       dlgFilterLazarusProject,
                       dlgFilterLazarusPackage,
                       dlgFilterLazarusSession,
                       dlgFilterAll, GetAllFilesMask]);

  with ButtonPanel1 do begin
    OKButton.Caption:=lisIECOLoadFromFile;
    OKButton.LoadGlyphFromStock(idButtonOpen);
    if OKButton.Glyph.Empty then
      IDEImages.AssignImage(OKButton, 'laz_open');
    OKButton.Enabled:=False;
    OKButton.OnClick:=@OpenButtonCLICK;
  end;
end;

procedure TImExportCompOptsDlg.InitExport(AOptions: TBaseCompilerOptions);
begin
  FExport:=true;
  Caption:=lisIECOExportCompilerOptions;
  SaveDlg.Filter:=Format('%s|*.xml|%s|%s',
                     [dlgFilterXML, dlgFilterAll, GetAllFilesMask]);

  if not (AOptions is TProjectCompilerOptions) then
    ExportRadioGroup.Visible:=False
  else if Project1.BuildModes.Count <= 1 then
    ExportRadioGroup.Enabled:=False;
  with ButtonPanel1 do begin
    OKButton.Caption:=lisIECOSaveToFile;
    OKButton.LoadGlyphFromStock(idButtonSave);
    if OKButton.Glyph.Empty then
      IDEImages.AssignImage(OKButton, 'laz_save');
    OKButton.Enabled:=False;
    OKButton.OnClick:=@SaveButtonCLICK;
  end;
end;

procedure TImExportCompOptsDlg.ImExportCompOptsDlgCREATE(Sender: TObject);
begin
  FileLabel.Caption:=lisFile;
  ExportRadioGroup.Caption:=lisIECOCompilerOptionsOf;
  ExportRadioGroup.Items.Strings[0]:=lisIECOCurrentBuildMode;
  ExportRadioGroup.Items.Strings[1]:=lisIECOAllBuildModes;
  LoadRecentList;
end;

procedure TImExportCompOptsDlg.ButtonBrowseClick(Sender: TObject);
var
  Dlg: TFileDialog;
begin
  if FExport then Dlg:= SaveDlg
    else Dlg:= OpenDlg;

  with Dlg do
  begin
    FileName := '';
    if Execute then
    begin
      FileNameEdit.Text := FileName;
      FileNameEditChange(nil);
    end;
  end;
end;

procedure TImExportCompOptsDlg.FileNameEditChange(Sender: TObject);
begin
  if FExport then
    ButtonPanel1.OKButton.Enabled := FileNameEdit.Text <> ''
  else
    ButtonPanel1.OKButton.Enabled := FileExistsUTF8(FileNameEdit.Text);
end;

procedure TImExportCompOptsDlg.OpenButtonCLICK(Sender: TObject);
begin
  DoOpenFile(CleanAndExpandFilename(FileNameEdit.Text));
end;

procedure TImExportCompOptsDlg.SaveButtonCLICK(Sender: TObject);
begin
  DoSaveFile(CleanAndExpandFilename(FileNameEdit.Text));
end;

procedure TImExportCompOptsDlg.ImExportCompOptsDlgCLOSE(Sender: TObject; var CloseAction: TCloseAction);
begin
  //SaveRecentList;
end;

procedure TImExportCompOptsDlg.LoadRecentList;
begin
  FileNameEdit.Items.Assign(
    InputHistories.HistoryLists.GetList(hlCompilerOptsImExport,true,rltFile));
end;
{
procedure TImExportCompOptsDlg.SaveRecentList;
begin
  InputHistories.HistoryLists.GetList(hlCompilerOptsImExport,true,rltFile).Assign(
    FileNameEdit.Items);
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

