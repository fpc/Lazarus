{
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

  Abstract:
    SynEdit (actually TSynSearchOptions) dependent parts separated from
    InputHistory unit. The idea is to reduce dependencies.
}
unit InputhistoryWithSearchOpt;

{$mode objfpc}{$H+}

interface

uses
  // LazUtils
  Laz2_XMLCfg,
  // SynEdit
  SynEditTypes,
  // IDE
  InputHistory;

type

  { TInputHistoriesWithSearchOpt }

  TInputHistoriesWithSearchOpt = class(TInputHistories)
  private
    FFindOptions: TSynSearchOptions;
  protected
    procedure LoadSearchOptions(XMLConfig: TXMLConfig; const Path: string); override;
    procedure SaveSearchOptions(XMLConfig: TXMLConfig; const Path: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    property FindOptions: TSynSearchOptions read FFindOptions write FFindOptions;
  end;

const
  LazFindSearchOptionNames: array[TSynSearchOption] of string = (
    'MatchCase',
    'WholeWord',
    'Backwards',
    'EntireScope',
    'SelectedOnly',
    'Replace',
    'ReplaceAll',
    'Prompt',
    'SearchInReplacement',
    'RegExpr',
    'RegExprMultiLine',
    'ssoFindContinue'
    );

var
  InputHistoriesSO: TInputHistoriesWithSearchOpt = nil;

implementation

{ TInputHistoriesWithSearchOpt }

constructor TInputHistoriesWithSearchOpt.Create;
begin
  inherited Create;
  FFindOptions:=LazFindSearchOptionsDefault;
end;

destructor TInputHistoriesWithSearchOpt.Destroy;
begin
  inherited Destroy;
end;

procedure TInputHistoriesWithSearchOpt.LoadSearchOptions(XMLConfig: TXMLConfig; const Path: string);
var
  FindOption: TSynSearchOption;
begin
  FFindOptions:=[];
  for FindOption:=Low(FFindOptions) to High(FFindOptions) do
  begin
    if XMLConfig.GetValue(Path+'Find/Options/'+LazFindSearchOptionNames[FindOption],
                          FindOption in LazFindSearchOptionsDefault)
    then
      Include(FFindOptions,FindOption);
  end;
end;

procedure TInputHistoriesWithSearchOpt.SaveSearchOptions(XMLConfig: TXMLConfig; const Path: string);
var
  FindOption: TSynSearchOption;
begin
  for FindOption:=Low(FFindOptions) to High(FFindOptions) do begin
    XMLConfig.SetDeleteValue(
      Path+'Find/Options/'+LazFindSearchOptionNames[FindOption],
      FindOption in FindOptions,
      FindOption in LazFindSearchOptionsDefault);
  end;
end;

end.

