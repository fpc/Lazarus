{
/***************************************************************************
                             inputhistory.pas
                             ----------------

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit InputHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEProcs, XMLCfg, LazConf;

type
  TInputHistories = class
  private
    FFilename: string;
  
    // Find- and replace-history
    FFindHistory: TStringList;
    FReplaceHistory: TStringList;
    FMaxFindHistory: Integer;
    
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
    procedure AddToFindHistory(const AFindStr: string);
    procedure AddToReplaceHistory(const AReplaceStr: String);
    
    function LastFPCUnitLinksValid: boolean;
    function LastFPCUnitLinksNeedsUpdate(const SearchPath: string): boolean;
    procedure SetLastFPCUnitLinks(const FPCPath, SearchPath, UnitLinks: string);
  public
    // Find- and replace-history
    property FindHistory: TStringList read FFindHistory write FFindHistory;
    property ReplaceHistory: TStringList read FReplaceHistory write FReplaceHistory;
    property MaxFindHistory: Integer read FMaxFindHistory write FMaxFindHistory;
    property Filename: string read FFilename write SetFilename;
    
    // FPC unitlinks
    property LastFPCUnitLinks: string read FLastFPCUnitLinks;
    property LastFPCPath: string read FLastFPCPath write SetLastFPCPath;
    property LastFPCSearchPath: string read FLastFPCSearchPath;
    property LastFPCAge: longint read FLastFPCAge;
  end;

var InputHistories: TInputHistories;


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
  
  FFilename:='';
  Clear;
end;

destructor TInputHistories.Destroy;
begin
  FFindHistory.Free;
  FReplaceHistory.Free;
  inherited Destroy;
end;

procedure TInputHistories.Clear;
begin
  FFindHistory.Clear;
  FReplaceHistory.Clear;
  FLastFPCPath:='';
end;

procedure TInputHistories.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  // Find- and replace-history
  fMaxFindHistory:=XMLConfig.GetValue(Path+'Find/History/Max',FMaxFindHistory);
  LoadRecentList(XMLConfig,FFindHistory,Path+'Find/History/Find/');
  LoadRecentList(XMLConfig,FReplaceHistory,Path+'Find/History/Replace/');
  FLastFPCAge:=XMLConfig.GetValue(Path+'FPCUnitLinks/FPCAge',-1);
  FLastFPCPath:=XMLConfig.GetValue(Path+'FPCUnitLinks/FPCPath','');
  FLastFPCSearchPath:=XMLConfig.GetValue(Path+'FPCUnitLinks/FPCSearchPath','');
  FLastFPCUnitLinks:=XMLConfig.GetValue(Path+'FPCUnitLinks/UnitLinks','');
end;

procedure TInputHistories.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  // Find- and replace-history
  XMLConfig.SetValue(Path+'Find/History/Max',FMaxFindHistory);
  SaveRecentList(XMLConfig,FFindHistory,Path+'Find/History/Find/');
  SaveRecentList(XMLConfig,FReplaceHistory,Path+'Find/History/Replace/');
  XMLConfig.SetValue(Path+'FPCUnitLinks/FPCAge',FLastFPCAge);
  XMLConfig.SetValue(Path+'FPCUnitLinks/FPCPath',FLastFPCPath);
  XMLConfig.SetValue(Path+'FPCUnitLinks/FPCSearchPath',FLastFPCSearchPath);
  XMLConfig.SetValue(Path+'FPCUnitLinks/UnitLinks',FLastFPCUnitLinks);
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

procedure TInputHistories.AddToFindHistory(const AFindStr: string);
begin
  AddToRecentList(AFindStr,FFindHistory,FMaxFindHistory);
end;

procedure TInputHistories.AddToReplaceHistory(const AReplaceStr: String);
begin
  AddToRecentList(AReplaceStr,FReplaceHistory,FMaxFindHistory);
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

end.

