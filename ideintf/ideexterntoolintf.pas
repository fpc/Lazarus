{ Copyright (C) 2006

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  Classes, SysUtils, LCLType, LazConfigStorage, Forms, Controls,
  BaseIDEIntf;

{ The xml format version:
    When the format changes (new values, changed formats) we can distinguish old
    files and are able to convert them.
}
const ExternalToolOptionsFormat = '1.0';

type
  {
    TExternalToolOptions - the storage object for a single external tool
  }
  TExternalToolOptions = class
  private
    fCmdLineParams: string;
    FEnvironmentOverrides: TStringList;
    fFilename: string;
    fKey: word;
    FScanOutput: boolean;
    fScanOutputForFPCMessages: boolean;
    fScanOutputForMakeMessages: boolean;
    fShift: TShiftState;
    FShowAllOutput: boolean;
    fTitle: string;
    fWorkingDirectory: string;
    procedure SetScanOutput(const AValue: boolean);
    procedure SetShowAllOutput(const AValue: boolean);
  public
    procedure Assign(Source: TExternalToolOptions);
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function NeedsOutputFilter: boolean;
    function Load(Config: TConfigStorage): TModalResult;
    function Save(Config: TConfigStorage): TModalResult;
    function ShortDescription: string;
    procedure AssignEnvironmentTo(Strings: TStrings);

    property CmdLineParams: string read fCmdLineParams write fCmdLineParams;
    property Filename: string read fFilename write fFilename;
    property Key: word read fKey write fKey;
    property Title: string read fTitle write fTitle;
    property ScanOutputForFPCMessages: boolean
      read fScanOutputForFPCMessages write fScanOutputForFPCMessages;
    property ScanOutputForMakeMessages: boolean
      read fScanOutputForMakeMessages write fScanOutputForMakeMessages;
    property Shift: TShiftState read fShift write fShift;
    property WorkingDirectory: string
      read fWorkingDirectory write fWorkingDirectory;
    property EnvironmentOverrides: TStringList read FEnvironmentOverrides;
    property ScanOutput: boolean read FScanOutput write SetScanOutput;
    property ShowAllOutput: boolean read FShowAllOutput write SetShowAllOutput;
  end;

implementation

{ TExternalToolOptions }

procedure TExternalToolOptions.SetScanOutput(const AValue: boolean);
begin
  if FScanOutput=AValue then exit;
  FScanOutput:=AValue;
end;

procedure TExternalToolOptions.SetShowAllOutput(const AValue: boolean);
begin
  if FShowAllOutput=AValue then exit;
  FShowAllOutput:=AValue;
end;

procedure TExternalToolOptions.Assign(Source: TExternalToolOptions);
begin
  if Source=Self then exit;
  if Source=nil then
    Clear
  else begin
    fTitle:=Source.fTitle;
    fFilename:=Source.fFilename;
    fCmdLineParams:=Source.fCmdLineParams;
    fWorkingDirectory:=Source.fWorkingDirectory;
    fKey:=Source.fKey;
    fShift:=Source.fShift;
    fScanOutputForFPCMessages:=Source.fScanOutputForFPCMessages;
    fScanOutputForMakeMessages:=Source.fScanOutputForMakeMessages;
    FScanOutput:=Source.FScanOutput;
    FShowAllOutput:=Source.FShowAllOutput;
  end;
end;

constructor TExternalToolOptions.Create;
begin
  inherited Create;
  FEnvironmentOverrides:=TStringList.Create;
  Clear;
end;

destructor TExternalToolOptions.Destroy;
begin
  FEnvironmentOverrides.Free;
  inherited Destroy;
end;

procedure TExternalToolOptions.Clear;
begin
  fTitle:='';
  fFilename:='';
  fCmdLineParams:='';
  fWorkingDirectory:='';
  fKey:=VK_UNKNOWN;
  fShift:=[];
  fScanOutputForFPCMessages:=false;
  fScanOutputForMakeMessages:=false;
  FScanOutput:=false;
  FShowAllOutput:=false;
end;

function TExternalToolOptions.Load(Config: TConfigStorage): TModalResult;
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
  Config.GetValue('EnvironmentOverrides/',FEnvironmentOverrides);
  // key and shift are loaded with the keymapping in the editoroptions
  Result:=mrOk;
end;

function TExternalToolOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Config.SetValue('Format/Version',ExternalToolOptionsFormat);
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
  Config.SetValue('EnvironmentOverrides/',FEnvironmentOverrides);
  // key and shift are saved with the keymapping in the editoroptions
  Result:=mrOk;
end;

function TExternalToolOptions.ShortDescription: string;
begin
  Result:=Title;
end;

procedure TExternalToolOptions.AssignEnvironmentTo(Strings: TStrings);
begin
  BaseIDEIntf.AssignEnvironmentTo(Strings,EnvironmentOverrides);
end;

function TExternalToolOptions.NeedsOutputFilter: boolean;
begin
  Result:=ScanOutput or ScanOutputForFPCMessages or ScanOutputForMakeMessages
                     or ShowAllOutput;
end;

end.

