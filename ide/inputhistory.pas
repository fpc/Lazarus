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

  Author: Mattias Gaertner
  

}
unit InputHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DiffPatch, IDEProcs, Laz_XMLCfg, LazConf, Dialogs;

const
  // these are the names of the various history lists in the IDE:
  hlPublishProjectDestDirs = 'PublishProjectDestinationDirectories';
  hlPublishProjectCommandsAfter = 'PublishProjectCommmandsAfter';
  hlPublishProjectIncludeFileFilter = 'PublishProjectIncludeFileFilter';
  hlPublishProjectExcludeFileFilter = 'PublishProjectExcludeFileFilter';
  hlMakeResourceStringSections = 'MakeResourceStringSections';
  hlMakeResourceStringPrefixes = 'MakeResourceStringPrefixes';
  hlMakeResourceStringLengths = 'MakeResourceStringLengths';

type
  TFileDialogSettings = record
    InitialDir: string;
    Width: integer;
    Height: integer;
    HistoryList: TStringList;
    MaxHistory: integer;
  end;
  
  THistoryList = class(TStringList)
  private
    FMaxCount: integer;
    FName: string;
    procedure SetMaxCount(const AValue: integer);
    procedure SetName(const AValue: string);
  public
    constructor Create;
    destructor Destroy;  override;
    function Push(const Entry: string): integer;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  public
    property Name: string read FName write SetName;
    property MaxCount: integer read FMaxCount write SetMaxCount;
  end;
  
  THistoryLists = class
  private
    FItems: TList;
    function GetItems(Index: integer): THistoryList;
    function GetXMLListPath(const Path: string; i: integer): string;
  public
    constructor Create;
    destructor Destroy;  override;
    procedure Clear;
    function Count: integer;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    function IndexOfName(const Name: string): integer;
    function GetList(const Name: string;
      CreateIfNotExists: boolean): THistoryList;
    procedure Add(const ListName, Entry: string);
    property Items[Index: integer]: THistoryList read GetItems;
  end;

  TInputHistories = class
  private
    FDiffFlags: TTextDiffFlags;
    FDiffText2: string;
    FDiffText2OnlySelection: boolean;
    FFileDialogSettings: TFileDialogSettings;
    FFilename: string;
  
    // Find- and replace-history
    FFindHistory: TStringList;
    FReplaceHistory: TStringList;
    FMaxFindHistory: Integer;
    
    // Unit dependencies
    FUnitDependenciesHistory: TStringList;
    FMaxUnitDependenciesHistory: integer;
    
    // FPC unitlinks
    FLastFPCUnitLinks: string;
    FLastFPCPath: string;
    FLastFPCSearchPath: string;
    FLastFPCAge: longint;
    
    // various history lists
    FHistoryLists: THistoryLists;
    
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
    property MaxUnitDependenciesHistory: integer
      read FMaxUnitDependenciesHistory write FMaxUnitDependenciesHistory;

    // FPC unitlinks
    property LastFPCUnitLinks: string read FLastFPCUnitLinks;
    property LastFPCPath: string read FLastFPCPath write SetLastFPCPath;
    property LastFPCSearchPath: string read FLastFPCSearchPath;
    property LastFPCAge: longint read FLastFPCAge;
    
    // filedialogs
    property FileDialogSettings: TFileDialogSettings
      read FFileDialogSettings write FFileDialogSettings;
      
    // various history lists
    property HistoryLists: THistoryLists read FHistoryLists;
    
    // diff dialog
    property DiffFlags: TTextDiffFlags read FDiffFlags write FDiffFlags;
    property DiffText2: string read FDiffText2 write FDiffText2;
    property DiffText2OnlySelection: boolean read FDiffText2OnlySelection
                                             write FDiffText2OnlySelection;
  end;

var
  InputHistories: TInputHistories;


implementation


const
  DefaultHistoryFile = 'inputhistory.xml';
  InputHistoryVersion = 1;
  DefaultDiffFlags = [tdfIgnoreCase,tdfIgnoreEmptyLineChanges,
                      tdfIgnoreLineEnds,tdfIgnoreTrailingSpaces];

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
  
  // unit dependencies
  FUnitDependenciesHistory:=TStringList.Create;
  FMaxUnitDependenciesHistory:=20;
  
  // file dialog
  FFileDialogSettings.HistoryList:=TStringList.Create;
  FFileDialogSettings.MaxHistory:=20;
  
  // various history lists
  FHistoryLists:=THistoryLists.Create;
  
  FFilename:='';
  Clear;
end;

destructor TInputHistories.Destroy;
begin
  FHistoryLists.Free;
  FFileDialogSettings.HistoryList.Free;
  FUnitDependenciesHistory.Free;
  FFindHistory.Free;
  FReplaceHistory.Free;
  inherited Destroy;
end;

procedure TInputHistories.Clear;
begin
  FHistoryLists.Clear;
  FFindHistory.Clear;
  FReplaceHistory.Clear;
  with FFileDialogSettings do begin
    HistoryList.Clear;
    Width:=0;
    Height:=0;
    InitialDir:='';
  end;
  FLastFPCPath:='';
  FDiffFlags:=DefaultDiffFlags;
  FDiffText2:='';
  FDiffText2OnlySelection:=false;
end;

procedure TInputHistories.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  DiffFlag: TTextDiffFlag;
begin
  // Find- and replace-history
  fMaxFindHistory:=XMLConfig.GetValue(Path+'Find/History/Max',FMaxFindHistory);
  LoadRecentList(XMLConfig,FFindHistory,Path+'Find/History/Find/');
  LoadRecentList(XMLConfig,FReplaceHistory,Path+'Find/History/Replace/');
  // unit dependencies
  LoadRecentList(XMLConfig,FUnitDependenciesHistory,Path+'UnitDependencies/History/');
  // fpc
  FLastFPCAge:=XMLConfig.GetValue(Path+'FPCUnitLinks/FPCAge',-1);
  FLastFPCPath:=XMLConfig.GetValue(Path+'FPCUnitLinks/FPCPath','');
  FLastFPCSearchPath:=XMLConfig.GetValue(Path+'FPCUnitLinks/FPCSearchPath','');
  FLastFPCUnitLinks:=XMLConfig.GetValue(Path+'FPCUnitLinks/UnitLinks','');
  // file dialog
  with FFileDialogSettings do begin
    Width:=XMLConfig.GetValue(Path+'FileDialog/Width',0);
    Height:=XMLConfig.GetValue(Path+'FileDialog/Height',0);
    InitialDir:=XMLConfig.GetValue(Path+'FileDialog/InitialDir','');
    MaxHistory:=XMLConfig.GetValue(Path+'FileDialog/MaxHistory',20);
    LoadRecentList(XMLConfig,HistoryList,Path+'FileDialog/HistoryList/');
  end;
  // history lists
  FHistoryLists.LoadFromXMLConfig(XMLConfig,Path+'HistoryLists/');
  // diff dialog
  FDiffFlags:=[];
  for DiffFlag:=Low(TTextDiffFlag) to High(TTextDiffFlag) do begin
    if XMLConfig.GetValue(
      Path+'DiffDialog/Options/'+TextDiffFlagNames[DiffFlag],
      DiffFlag in DefaultDiffFlags)
    then
      Include(FDiffFlags,DiffFlag);
  end;
  FDiffText2:=XMLConfig.GetValue(Path+'DiffDialog/Text2/Name','');
  FDiffText2OnlySelection:=
    XMLConfig.GetValue(Path+'DiffDialog/Text2/OnlySelection',false);
end;

procedure TInputHistories.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  DiffFlag: TTextDiffFlag;
begin
  // Find- and replace-history
  XMLConfig.SetDeleteValue(Path+'Find/History/Max',FMaxFindHistory,20);
  SaveRecentList(XMLConfig,FFindHistory,Path+'Find/History/Find/');
  SaveRecentList(XMLConfig,FReplaceHistory,Path+'Find/History/Replace/');
  // unit dependencies
  SaveRecentList(XMLConfig,FUnitDependenciesHistory,Path+'UnitDependencies/History/');
  // fpc
  XMLConfig.SetDeleteValue(Path+'FPCUnitLinks/FPCAge',FLastFPCAge,0);
  XMLConfig.SetDeleteValue(Path+'FPCUnitLinks/FPCPath',FLastFPCPath,'');
  XMLConfig.SetDeleteValue(Path+'FPCUnitLinks/FPCSearchPath',FLastFPCSearchPath,'');
  XMLConfig.SetDeleteValue(Path+'FPCUnitLinks/UnitLinks',FLastFPCUnitLinks,'');
  // file dialog
  with FFileDialogSettings do begin
    XMLConfig.SetDeleteValue(Path+'FileDialog/Width',Width,0);
    XMLConfig.SetDeleteValue(Path+'FileDialog/Height',Height,0);
    XMLConfig.SetDeleteValue(Path+'FileDialog/InitialDir',InitialDir,'');
    XMLConfig.SetDeleteValue(Path+'FileDialog/MaxHistory',MaxHistory,20);
    SaveRecentList(XMLConfig,HistoryList,Path+'FileDialog/HistoryList/');
  end;
  // history lists
  FHistoryLists.SaveToXMLConfig(XMLConfig,Path+'HistoryLists/');
  // diff dialog
  for DiffFlag:=Low(TTextDiffFlag) to High(TTextDiffFlag) do begin
    XMLConfig.SetDeleteValue(
      Path+'DiffDialog/Options/'+TextDiffFlagNames[DiffFlag],
      DiffFlag in DiffFlags,DiffFlag in DefaultDiffFlags);
  end;
  XMLConfig.SetDeleteValue(Path+'DiffDialog/Text2/Name',FDiffText2,'');
  XMLConfig.SetDeleteValue(Path+'DiffDialog/Text2/OnlySelection',
                           FDiffText2OnlySelection,false);
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
    XMLConfig:=TXMLConfig.CreateClean(FFileName);
    XMLConfig.SetDeleteValue('InputHistory/Version/Value',
      InputHistoryVersion,0);
    SaveToXMLConfig(XMLConfig,'InputHistory/');
    XMLConfig.Flush;
    XMLConfig.Free;
  except
    on E: Exception do begin
      writeln('[TEnvironmentOptions.Save]  error writing "',FFilename,'" ',E.Message);
    end;
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
                  FMaxUnitDependenciesHistory);
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

{ THistoryList }

procedure THistoryList.SetMaxCount(const AValue: integer);
begin
  if FMaxCount=AValue then exit;
  FMaxCount:=AValue;
end;

procedure THistoryList.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

constructor THistoryList.Create;
begin
  FMaxCount:=20;
end;

destructor THistoryList.Destroy;
begin
  inherited Destroy;
end;

function THistoryList.Push(const Entry: string): integer;
begin
  AddToRecentList(Entry,Self,MaxCount);
  Result:=-1;
end;

procedure THistoryList.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  if FName='' then
    FName:=XMLConfig.GetValue(Path+'Name','');
  FMaxCount:=XMLConfig.GetValue(Path+'MaxCount',MaxCount);
  LoadRecentList(XMLConfig,Self,Path);
end;

procedure THistoryList.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string
  );
begin
  XMLConfig.SetDeleteValue(Path+'Name',Name,'');
  XMLConfig.SetDeleteValue(Path+'MaxCount',MaxCount,20);
  SaveRecentList(XMLConfig,Self,Path);
end;

{ THistoryLists }

function THistoryLists.GetItems(Index: integer): THistoryList;
begin
  Result:=THistoryList(FItems[Index]);
end;

function THistoryLists.GetXMLListPath(const Path: string; i: integer): string;
begin
  Result:=Path+'List'+IntToStr(i)+'/';
end;

constructor THistoryLists.Create;
begin
  FItems:=TList.Create;
end;

destructor THistoryLists.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure THistoryLists.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].Free;
  FItems.Clear;
end;

function THistoryLists.Count: integer;
begin
  Result:=FItems.Count;
end;

procedure THistoryLists.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  MergeCount, i: integer;
  CurList: THistoryList;
  ListName, ListPath: string;
begin
  MergeCount:=XMLConfig.GetValue(Path+'Count',0);
  for i:=0 to MergeCount-1 do begin
    ListPath:=GetXMLListPath(Path,i);
    ListName:=XMLConfig.GetValue(ListPath+'Name','');
    if ListName='' then continue;
    CurList:=GetList(ListName,true);
    CurList.LoadFromXMLConfig(XMLConfig,ListPath);
  end;
end;

procedure THistoryLists.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  i, CurID: integer;
begin
  XMLConfig.SetDeleteValue(Path+'Count',Count,0);
  CurID:=0;
  for i:=0 to Count-1 do begin
    if Items[i].Count>0 then begin
      Items[i].SaveToXMLConfig(XMLConfig,GetXMLListPath(Path,CurID));
      inc(CurID);
    end;
  end;
end;

function THistoryLists.IndexOfName(const Name: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(Items[Result].Name,Name)<>0) do
    dec(Result);
end;

function THistoryLists.GetList(const Name: string;
  CreateIfNotExists: boolean): THistoryList;
var
  i: integer;
begin
  i:=IndexOfName(Name);
  if i>=0 then
    Result:=Items[i]
  else begin
    Result:=THistoryList.Create;
    Result.Name:=Name;
    FItems.Add(Result);
  end;
end;

procedure THistoryLists.Add(const ListName, Entry: string);
begin
  GetList(ListName,true).Push(Entry);
end;

initialization
  InputHistories:= nil;


end.

