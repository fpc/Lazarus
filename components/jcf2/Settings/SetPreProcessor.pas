unit SetPreProcessor;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetPreprocessor, released 2003
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2003-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ settings for preprocessor
}

uses
  Classes, SysUtils,
  { local }
  JcfSetBase, SettingsStream;

type
  TSetPreProcessor = class(TSetBase)
  private
    fbEnabled: boolean;
    fcDefinedSymbols: TStringList;
    fcDefinedOptions: TStringList;

    procedure AddDefaultSymbols;
    procedure AddDefaultOptions;
  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property Enabled: boolean Read fbEnabled Write fbEnabled;

    function OptionIsDefined(const psOption: string): boolean;
    function SymbolIsDefined(const psSymbol: string): boolean;

    property DefinedSymbols: TStringList Read fcDefinedSymbols Write fcDefinedSymbols;
    property DefinedOptions: TStringList Read fcDefinedOptions Write fcDefinedOptions;

  end;

implementation

const
  REG_ENABLED = 'Enabled';
  REG_DEFINED_SYMBOLS = 'DefinedSymbols';
  REG_DEFINED_OPTIONS = 'DefinedOptions';

{ TSetPreProcessor }

constructor TSetPreProcessor.Create;
begin
  inherited;
  SetSection('PreProcessor');

  fcDefinedSymbols := TStringList.Create;
  //fcDefinedSymbols.Sorted := True;
  fcDefinedSymbols.Duplicates := dupIgnore;

  fcDefinedOptions := TStringList.Create;
  //fcDefinedOptions.Sorted := True;
  fcDefinedOptions.Duplicates := dupIgnore;
end;

destructor TSetPreProcessor.Destroy;
begin
  FreeAndNil(fcDefinedSymbols);
  FreeAndNil(fcDefinedOptions);
  inherited;
end;

procedure TSetPreProcessor.AddDefaultSymbols;
begin
  fcDefinedSymbols.Add('MSWINDOWS');
  fcDefinedSymbols.Add('WIN32');
  fcDefinedSymbols.Add('DELPHI5_UP');
end;

procedure TSetPreProcessor.AddDefaultOptions;
begin

end;

procedure TSetPreProcessor.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  fbEnabled := pcStream.Read(REG_ENABLED, True);

  fcDefinedSymbols.Sorted := False;
  if not pcStream.Read(REG_DEFINED_SYMBOLS, fcDefinedSymbols) then
    AddDefaultSymbols;

  fcDefinedSymbols.Sort;
  fcDefinedSymbols.Sorted := True;


  fcDefinedOptions.Sorted := False;
  if not pcStream.Read(REG_DEFINED_OPTIONS, fcDefinedOptions) then
    AddDefaultOptions;

  fcDefinedOptions.Sort;
  fcDefinedOptions.Sorted := True;
end;

procedure TSetPreProcessor.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_ENABLED, fbEnabled);
  pcOut.Write(REG_DEFINED_SYMBOLS, fcDefinedSymbols);
  pcOut.Write(REG_DEFINED_OPTIONS, fcDefinedOptions);

end;

function TSetPreProcessor.OptionIsDefined(const psOption: string): boolean;
begin
  Result := fcDefinedOptions.IndexOf(psOption) >= 0;
end;

function TSetPreProcessor.SymbolIsDefined(const psSymbol: string): boolean;
begin
  Result := fcDefinedSymbols.IndexOf(psSymbol) >= 0;
end;

end.
