{
/***************************************************************************
                             inputhistory.pas
                             ----------------

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
}
unit InputHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEProcs, Laz_XMLCfg, LazConf, Dialogs;

type
  TFileDialogSettings = record
    InitialDir: string;
    Width: integer;
    Height: integer;
    HistoryList: TStringList;
    MaxHistory: integer;
  end;

  TInputHistories = class
  private
    FFileDialogSettings: TFileDialogSettings;
    FFilename: string;
  
    // Find- and replace-history
    FFindHistory: TStringList;
    FReplaceHistory: TStringList;
    FMaxFindHistory: Integer;
    
    // Unit dependencies
    FUnitDependenciesHistory: TStringList;
    FMaxUnitDependeciesHistory: integer;
    
    // FPC unitlinks
    FLastFPCUnitLinks: string;
    FLastFPCPath: string;
    FLastFPCSearchPath: string;
    FLastFPCAge: longint;
    
    procedure SetFilename(const AValue: string);
    procedure SetLastFPCPath(const AValue: string);
  public
    constructor Create;
    destructor Destroy;  override;
    procedure Clear;
    procedure Load;
    procedure Save;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SetLazarusDefaultFilename;

    // Find- and replace-history
    function AddToFindHistory(const AFindStr: string): boolean;
    function AddToReplaceHistory(const AReplaceStr: String): boolean;
    
    // Unit dependencies
    function AddToUnitDependenciesHistory(const ARootFilename: String): boolean;

    function LastFPCUnitLinksValid: boolean;
    function LastFPCUnitLinksNeedsUpdate(const SearchPath: string): boolean;
    procedure SetLastFPCUnitLinks(const FPCPath, SearchPath, UnitLinks: string);
    
    // filedialog
    procedure ApplyFileDialogSettings(DestDialog: TFileDialog);
    procedure StoreFileDialogSettings(SourceDialog: TFileDialog);
  public
    // Find- and replace-history
    property FindHistory: TStringList read FFindHistory write FFindHistory;
    property ReplaceHistory: TStringList read FReplaceHistory write FReplaceHistory;
    property MaxFindHistory: Integer read FMaxFindHistory write FMaxFindHistory;
    property Filename: string read FFilename write SetFilename;

    // Unit dependencies
    property UnitDependenciesHistory: TStringList read FUnitDependenciesHistory;
    property MaxUnitDependeciesHistory: integer
      read FMaxUnitDependeciesHistory write FMaxUnitDependeciesHistory;

    // FPC unitlinks
    property LastFPCUnitLinks: string read FLastFPCUnitLinks;
    property LastFPCPath: string read FLastFPCPath write SetLastFPCPath;
    property LastFPCSearchPath: string read FLastFPCSearchPath;
    property LastFPCAge: longint read FLastFPCAge;
    
    // filedialogs
    property FileDialogSettings: TFileDialogSettings
      read FFileDialogSettings write FFileDialogSettings;
  end;

const
  InputHistories: TInputHistories = nil;


implementation


const
  DefaultHistoryFile = 'inputhistory.xml';
  InputHistoryVersion = 1;

{ TInputHistories }

procedure TInputHistories.SetFilename(const AValue: string);
begin
  FFilename:=AValue;
end;

procedure TInputHistories.SetLastFPCPath(const AValue: string);
begin
  if FLastFPCPath=AValue then exit;
  FLastFPCPath:=AValue;
  FLastFPCAge:=-1;
  FLastFPCUnitLinks:='';
end;

constructor TInputHistories.Create;
begin
  inherited Create;
  // Find- and replace-history
  FFindHistory:=TStringList.Create;
  FReplaceHistory:=TStringList.Create;
  FMaxFindHistory:=20;
  
  FUnitDependenciesHistory:=TStringList.Create;
  FMaxUnitDependeciesHistory:=20;
  
  FFileDialogSettings.HistoryList:=TStringList.Create;
  FFileDialogSettings.MaxHistory:=20;
  
  FFilename:='';
  Clear;
end;

destructor TInputHistories.Destroy;
begin
  FFileDialogSettings.HistoryList.Free;
  FUnitDependenciesHistory.Free;
  FFindHistory.Free;
  FReplaceHistory.Free;
  inherited Destroy;
end;

procedure TInputHistories.Clear;
begin
  FFindHistory.Clear;
  FReplaceHistory.Clear;
  with FFileDialogSettings do begin
    HistoryList.Clear;
    Width:=0;
    Height:=0;
    InitialDir:='';
  end;
  FLastFPCPath:='';
end;

procedure TInputHistories.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  // Find- and replace-history
  fMaxFindHistory:=XMLConfig.GetValue(Path+'Find/History/Max',FMaxFindHistory);
  LoadRecentList(XMLConfig,FFindHistory,Path+'Find/History/Find/');
  LoadRecentList(XMLConfig,FReplaceHistory,Path+'Find/History/Replace/');
  LoadRecentList(XMLConfig,FUnitDependenciesHistory,Path+'UnitDependencies/History/');
  FLastFPCAge:=XMLConfig.GetValue(Path+'FPCUnitLinks/FPCAge',-1);
  FLastFPCPath:=XMLConfig.GetValue(Path+'FPCUnitLinks/FPCPath','');
  FLastFPCSearchPath:=XMLConfig.GetValue(Path+'FPCUnitLinks/FPCSearchPath','');
  FLastFPCUnitLinks:=XMLConfig.GetValue(Path+'FPCUnitLinks/UnitLinks','');
  with FFileDialogSettings do begin
    Width:=XMLConfig.GetValue(Path+'FileDialog/Width',0);
    Height:=XMLConfig.GetValue(Path+'FileDialog/Height',0);
    InitialDir:=XMLConfig.GetValue(Path+'FileDialog/InitialDir','');
    MaxHistory:=XMLConfig.GetValue(Path+'FileDialog/MaxHistory',20);
    LoadRecentList(XMLConfig,HistoryList,Path+'FileDialog/HistoryList/');
  end;
end;

procedure TInputHistories.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  // Find- and replace-history
  XMLConfig.SetValue(Path+'Find/History/Max',FMaxFindHistory);
  SaveRecentList(XMLConfig,FFindHistory,Path+'Find/History/Find/');
  SaveRecentList(XMLConfig,FReplaceHistory,Path+'Find/History/Replace/');
  SaveRecentList(XMLConfig,FUnitDependenciesHistory,Path+'UnitDependencies/History/');
  XMLConfig.SetValue(Path+'FPCUnitLinks/FPCAge',FLastFPCAge);
  XMLConfig.SetValue(Path+'FPCUnitLinks/FPCPath',FLastFPCPath);
  XMLConfig.SetValue(Path+'FPCUnitLinks/FPCSearchPath',FLastFPCSearchPath);
  XMLConfig.SetValue(Path+'FPCUnitLinks/UnitLinks',FLastFPCUnitLinks);
  with FFileDialogSettings do begin
    XMLConfig.SetValue(Path+'FileDialog/Width',Width);
    XMLConfig.SetValue(Path+'FileDialog/Height',Height);
    XMLConfig.SetValue(Path+'FileDialog/InitialDir',InitialDir);
    XMLConfig.SetValue(Path+'FileDialog/MaxHistory',MaxHistory);
    SaveRecentList(XMLConfig,HistoryList,Path+'FileDialog/HistoryList/');
  end;
end;

procedure TInputHistories.SetLazarusDefaultFilename;
var
  ConfFileName: string;
begin
  ConfFileName:=SetDirSeparators(
                             GetPrimaryConfigPath+'/'+DefaultHistoryFile);
  CopySecondaryConfigFile(DefaultHistoryFile);
  FFilename:=ConfFilename;
end;

procedure TInputHistories.Load;
var
  XMLConfig: TXMLConfig;
  //FileVersion: integer;
begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    //FileVersion:=XMLConfig.GetValue('InputHistory/Version/Value',0);
    LoadFromXMLConfig(XMLConfig,'InputHistory/');
    XMLConfig.Free;
  except
    // ToDo
    writeln('[TCodeToolsOptions.Load]  error reading "',FFilename,'"');
  end;
end;

procedure TInputHistories.Save;
var
  XMLConfig: TXMLConfig;
begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    XMLConfig.SetValue('InputHistory/Version/Value',
      InputHistoryVersion);
    SaveToXMLConfig(XMLConfig,'InputHistory/');
    XMLConfig.Flush;
    XMLConfig.Free;
  except
    // ToDo
    writeln('[TEnvironmentOptions.Save]  error writing "',FFilename,'"');
  end;
end;

function TInputHistories.AddToFindHistory(const AFindStr: string): boolean;
begin
  Result:=AddToRecentList(AFindStr,FFindHistory,FMaxFindHistory);
end;

function TInputHistories.AddToReplaceHistory(const AReplaceStr: String): boolean;
begin
  Result:=AddToRecentList(AReplaceStr,FReplaceHistory,FMaxFindHistory);
end;

function TInputHistories.AddToUnitDependenciesHistory(
  const ARootFilename: String): boolean;
begin
  Result:=AddToRecentList(ARootFilename,FUnitDependenciesHistory,
                  FMaxUnitDependeciesHistory);
end;

function TInputHistories.LastFPCUnitLinksValid: boolean;
begin
  Result:=(LastFPCPath<>'') and (FLastFPCAge>=0);
end;

function TInputHistories.LastFPCUnitLinksNeedsUpdate(
  const SearchPath: string): boolean;
begin
  Result:=(not LastFPCUnitLinksValid)
           or (SearchPath<>LastFPCSearchPath)
           or (FileAge(LastFPCPath)<>LastFPCAge);
end;

procedure TInputHistories.SetLastFPCUnitLinks(const FPCPath, SearchPath,
  UnitLinks: string);
begin
  FLastFPCPath:=FPCPath;
  FLastFPCUnitLinks:=UnitLinks;
  FLastFPCSearchPath:=SearchPath;
  FLastFPCAge:=FileAge(FPCPath);
end;

procedure TInputHistories.ApplyFileDialogSettings(DestDialog: TFileDialog);
begin
  DestDialog.InitialDir:=FFileDialogSettings.InitialDir;
  DestDialog.Width:=FFileDialogSettings.Width;
  DestDialog.Height:=FFileDialogSettings.Height;
  
  DestDialog.HistoryList:=FFileDialogSettings.HistoryList;
end;

procedure TInputHistories.StoreFileDialogSettings(SourceDialog: TFileDialog);
var s: string;
begin
  FFileDialogSettings.InitialDir:=SourceDialog.InitialDir;
  FFileDialogSettings.Width:=SourceDialog.Width;
  FFileDialogSettings.Height:=SourceDialog.Height;
  s:=ExtractFilePath(FFileDialogSettings.InitialDir);
  if s<>'' then
    AddToRecentList(s,FFileDialogSettings.HistoryList,
                    FFileDialogSettings.MaxHistory);
end;

end.

