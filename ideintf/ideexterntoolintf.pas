{ Copyright (C) 2006

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  Author: Mattias Gaertner
}
unit IDEExternToolIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LazConfigStorage, Forms, Controls, BaseIDEIntf;

{ The xml format version:
    When the format changes (new values, changed formats) we can distinguish old
    files and are able to convert them.
}
const
  ExternalToolOptionsVersion = '2';

type
  TIDEExternalToolOptions = class;

  { TIDEScanMessageLine }

  TIDEScanMessageLine = class(TPersistent)
  private
    FCaller: TObject;
    FLine: string;
    FLineNumber: integer;
    FTool: TIDEExternalToolOptions;
    FWorkingDirectory: string;
    procedure SetLine(const AValue: string);
    procedure SetWorkingDirectory(const AValue: string);
  protected
    procedure SetTool(const AValue: TIDEExternalToolOptions);
    procedure SetLineNumber(const NewLineNumber: integer);
    procedure LineChanged(const OldValue: string); virtual; abstract;
    procedure WorkingDirectoryChanged(const OldValue: string); virtual; abstract;
  public
    constructor Create(TheCaller: TObject = nil; TheTool: TIDEExternalToolOptions = nil);
    property Caller: TObject read FCaller;
    property Line: string read FLine write SetLine;
    property WorkingDirectory: string read FWorkingDirectory write SetWorkingDirectory;
    property LineNumber: integer read FLineNumber;
    property Tool: TIDEExternalToolOptions read FTool;
  end;


  TOnIDEExtToolParseLine = procedure(Sender: TObject;
                                     Line: TIDEScanMessageLine) of object;

  {
    TIDEExternalToolOptions - the storage object for a single external tool
  }
  TIDEExternalToolOptions = class(TPersistent)
  private
    fCmdLineParams: string;
    FEnvironmentOverrides: TStringList;
    fFilename: string;
    FHideMainForm: boolean;
    FOnParseLine: TOnIDEExtToolParseLine;
    FScanners: TStrings;
    FScanOutput: boolean;
    fScanOutputForFPCMessages: boolean;
    fScanOutputForMakeMessages: boolean;
    FShowAllOutput: boolean;
    fTitle: string;
    fWorkingDirectory: string;
    procedure SetScanners(const AValue: TStrings);
    procedure SetScanOutput(const AValue: boolean);
    procedure SetShowAllOutput(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    function NeedsOutputFilter: boolean;
    function Load(Config: TConfigStorage): TModalResult; virtual;
    function Save(Config: TConfigStorage): TModalResult; virtual;
    function ShortDescription: string;
    procedure AssignEnvironmentTo(Strings: TStrings);
    procedure ParseLine(Sender: TObject; Line: TIDEScanMessageLine); virtual;

    property CmdLineParams: string read fCmdLineParams write fCmdLineParams;
    property Filename: string read fFilename write fFilename;
    property Title: string read fTitle write fTitle;
    property WorkingDirectory: string
      read fWorkingDirectory write fWorkingDirectory;
    property EnvironmentOverrides: TStringList read FEnvironmentOverrides;
    property ScanOutputForFPCMessages: boolean
      read fScanOutputForFPCMessages write fScanOutputForFPCMessages;
    property ScanOutputForMakeMessages: boolean
      read fScanOutputForMakeMessages write fScanOutputForMakeMessages;
    property ScanOutput: boolean read FScanOutput write SetScanOutput;
    property ShowAllOutput: boolean read FShowAllOutput write SetShowAllOutput;
    property OnParseLine: TOnIDEExtToolParseLine read FOnParseLine write FOnParseLine;
    property Scanners: TStrings read FScanners write SetScanners;
    property HideMainForm: boolean read FHideMainForm write FHideMainForm default true;
  end;
  
type
  TRunExternalTool = function (Tool: TIDEExternalToolOptions): TModalResult of object;

var
  RunExternalTool: TRunExternalTool = nil;// set by the IDE

implementation

{ TIDEExternalToolOptions }

procedure TIDEExternalToolOptions.SetScanOutput(const AValue: boolean);
begin
  if FScanOutput=AValue then exit;
  FScanOutput:=AValue;
end;

procedure TIDEExternalToolOptions.SetScanners(const AValue: TStrings);
begin
  if FScanners=AValue then exit;
  FScanners.Assign(AValue);
end;

procedure TIDEExternalToolOptions.SetShowAllOutput(const AValue: boolean);
begin
  if FShowAllOutput=AValue then exit;
  FShowAllOutput:=AValue;
end;

procedure TIDEExternalToolOptions.Assign(Source: TPersistent);
var
  Src: TIDEExternalToolOptions;
begin
  if Source=Self then exit;
  if Source is TIDEExternalToolOptions then begin
    Src:=TIDEExternalToolOptions(Source);
    fTitle:=Src.fTitle;
    fFilename:=Src.fFilename;
    fCmdLineParams:=Src.fCmdLineParams;
    fWorkingDirectory:=Src.fWorkingDirectory;
    fScanOutputForFPCMessages:=Src.fScanOutputForFPCMessages;
    fScanOutputForMakeMessages:=Src.fScanOutputForMakeMessages;
    FScanOutput:=Src.FScanOutput;
    FShowAllOutput:=Src.FShowAllOutput;
    FScanners.Assign(Src.FScanners);
    FHideMainForm:=Src.HideMainForm;
  end else
    inherited Assign(Source);
end;

constructor TIDEExternalToolOptions.Create;
begin
  inherited Create;
  FEnvironmentOverrides:=TStringList.Create;
  FScanners:=TStringList.Create;
  FHideMainForm:=true;
  Clear;
end;

destructor TIDEExternalToolOptions.Destroy;
begin
  FreeAndNil(FEnvironmentOverrides);
  FreeAndNil(FScanners);
  inherited Destroy;
end;

procedure TIDEExternalToolOptions.Clear;
begin
  fTitle:='';
  fFilename:='';
  fCmdLineParams:='';
  fWorkingDirectory:='';
  fScanOutputForFPCMessages:=false;
  fScanOutputForMakeMessages:=false;
  FScanOutput:=false;
  FShowAllOutput:=false;
  FHideMainForm:=true;
  FEnvironmentOverrides.Clear;
  FScanners.Clear;
end;

function TIDEExternalToolOptions.Load(Config: TConfigStorage): TModalResult;
begin
  Clear;
  fTitle:=Config.GetValue('Title/Value','');
  fFilename:=Config.GetValue('Filename/Value','');
  fCmdLineParams:=Config.GetValue('CmdLineParams/Value','');
  fWorkingDirectory:=Config.GetValue('WorkingDirectory/Value','');
  fScanOutputForFPCMessages:=Config.GetValue(
                                        'ScanOutputForFPCMessages/Value',false);
  fScanOutputForMakeMessages:=Config.GetValue(
                                  'ScanOutputForMakeMessages/Value',false);
  FShowAllOutput:=Config.GetValue('ShowAllOutput/Value',false);
  FHideMainForm:=Config.GetValue('HideMainForm/Value',true);
  Config.GetValue('EnvironmentOverrides/',FEnvironmentOverrides);
  Config.GetValue('Scanners/',FScanners);
  Result:=mrOk;
end;

function TIDEExternalToolOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Config.SetValue('Format/Version',ExternalToolOptionsVersion);
  Config.SetDeleteValue('Title/Value',fTitle,'');
  Config.SetDeleteValue('Filename/Value',fFilename,'');
  Config.SetDeleteValue('CmdLineParams/Value',fCmdLineParams,'');
  Config.SetDeleteValue('WorkingDirectory/Value',fWorkingDirectory,'');
  Config.SetDeleteValue(
               'ScanOutputForFPCMessages/Value',fScanOutputForFPCMessages,
               false);
  Config.SetDeleteValue(
             'ScanOutputForMakeMessages/Value',fScanOutputForMakeMessages,
             false);
  Config.SetDeleteValue('ShowAllOutput/Value',FShowAllOutput,false);
  Config.SetDeleteValue('HideMainForm/Value',FHideMainForm,true);
  Config.SetValue('EnvironmentOverrides/',FEnvironmentOverrides);
  Config.SetValue('Scanners/',FScanners);
  Result:=mrOk;
end;

function TIDEExternalToolOptions.ShortDescription: string;
begin
  Result:=Title;
end;

procedure TIDEExternalToolOptions.AssignEnvironmentTo(Strings: TStrings);
begin
  BaseIDEIntf.AssignEnvironmentTo(Strings,EnvironmentOverrides);
end;

procedure TIDEExternalToolOptions.ParseLine(Sender: TObject;
  Line: TIDEScanMessageLine);
begin
  if Assigned(OnParseLine) then
    OnParseLine(Sender,Line);
end;

function TIDEExternalToolOptions.NeedsOutputFilter: boolean;
begin
  Result:=ScanOutput
       or ScanOutputForFPCMessages or ScanOutputForMakeMessages
       or ShowAllOutput
       or ((FScanners<>nil) and (FScanners.Count>0));
end;

{ TIDEScanMessageLine }

procedure TIDEScanMessageLine.SetLine(const AValue: string);
var
  OldLine: String;
begin
  if FLine=AValue then exit;
  OldLine:=FLine;
  FLine:=AValue;
  LineChanged(OldLine);
end;

procedure TIDEScanMessageLine.SetWorkingDirectory(const AValue: string);
var
  OldDir: String;
begin
  if FWorkingDirectory=AValue then exit;
  OldDir:=FWorkingDirectory;
  FWorkingDirectory:=AValue;
  WorkingDirectoryChanged(OldDir);
end;

procedure TIDEScanMessageLine.SetTool(const AValue: TIDEExternalToolOptions);
begin
  FTool:=AValue;
end;

procedure TIDEScanMessageLine.SetLineNumber(const NewLineNumber: integer);
begin
  FLineNumber:=NewLineNumber;
end;

constructor TIDEScanMessageLine.Create(TheCaller: TObject;
  TheTool: TIDEExternalToolOptions);
begin
  FCaller:=TheCaller;
  FTool:=TheTool;
end;

end.

