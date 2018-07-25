{
 /***************************************************************************
                            publishmodule.pas
                            -----------------


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

  Author: Mattias Gaertner

}
unit PublishModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr,
  // LazUtils
  LazFileUtils, Laz2_XMLCfg, LazLoggerBase, LazTracer, LazStringUtils;

type
  { TPublishModuleOptions }

  TPublishModuleOptions = class
  private
    FCommandAfter: string;
    FDestinationDirectory: string;
    FExcludeFileFilter: string;
    FExcludeFilterRegExpr: TRegExpr;
    FExcludeFilterSimpleSyntax: boolean;
    FExcludeFilterValid: boolean;
    FIgnoreBinaries: boolean;
    FIncludeFileFilter: string;
    FIncludeFilterRegExpr: TRegExpr;
    FIncludeFilterSimpleSyntax: boolean;
    FIncludeFilterValid: boolean;
    FModified: boolean;
    FModifiedLock: integer;
    FOwner: TObject;
    FUseExcludeFileFilter: boolean;
    FUseIncludeFileFilter: boolean;
    procedure SetCommandAfter(const AValue: string);
    procedure SetDestinationDirectory(const AValue: string);
    procedure SetExcludeFileFilter(const AValue: string);
    procedure SetExcludeFilterSimpleSyntax(const AValue: boolean);
    procedure SetIgnoreBinaries(const AValue: boolean);
    procedure SetIncludeFileFilter(const AValue: string);
    procedure SetIncludeFilterSimpleSyntax(const AValue: boolean);
    procedure SetModified(const AValue: boolean);
    procedure SetUseExcludeFileFilter(const AValue: boolean);
    procedure SetUseIncludeFileFilter(const AValue: boolean);
    procedure UpdateIncludeFilter;
    procedure UpdateExcludeFilter;
  protected
    procedure DoOnModifyChange; virtual;
  public
    constructor Create(TheOwner: TObject);
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure LoadDefaults; virtual;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const APath: string;
                                AdjustPathDelims: boolean); virtual;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const APath: string;
                              UsePathDelim: TPathDelimSwitch); virtual;
    function FileCanBePublished(const AFilename: string): boolean; virtual;
    procedure LockModified;
    procedure UnlockModified;
    function GetDefaultDestinationDir: string; virtual;
  public
    property Owner: TObject read FOwner;
    property Modified: boolean read FModified write SetModified;
  
    // destination
    property DestinationDirectory: string
                read FDestinationDirectory write SetDestinationDirectory;
    property CommandAfter: string read FCommandAfter write SetCommandAfter;

    // file filter
    property IgnoreBinaries: boolean read FIgnoreBinaries write SetIgnoreBinaries;
    property UseIncludeFileFilter: boolean
                read FUseIncludeFileFilter write SetUseIncludeFileFilter;
    property IncludeFilterSimpleSyntax: boolean
                read FIncludeFilterSimpleSyntax write SetIncludeFilterSimpleSyntax;
    property IncludeFileFilter: string
                read FIncludeFileFilter write SetIncludeFileFilter;
    property IncludeFilterValid: boolean read FIncludeFilterValid;
    property UseExcludeFileFilter: boolean
                read FUseExcludeFileFilter write SetUseExcludeFileFilter;
    property ExcludeFilterSimpleSyntax: boolean
                read FExcludeFilterSimpleSyntax write SetExcludeFilterSimpleSyntax;
    property ExcludeFileFilter: string
                read FExcludeFileFilter write SetExcludeFileFilter;
    property ExcludeFilterValid: boolean read FExcludeFilterValid;
  end;
  
const
  PublishModulOptsVersion = 2;

  DefPublModIncFilter = '*.(pas|pp|inc|lpr|lfm|lrs|lpi|lpk|xml|sh)';
  DefPublModExcFilter = '*.(bak|ppu|ppl|a|o|so);*~;backup';
  DefPublishDirectory = '$(TestDir)/publishedproject/';

implementation

{ TPublishModuleOptions }

procedure TPublishModuleOptions.SetCommandAfter(const AValue: string);
begin
  if FCommandAfter=AValue then exit;
  FCommandAfter:=AValue;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetDestinationDirectory(const AValue: string);
begin
  if FDestinationDirectory=AValue then exit;
  FDestinationDirectory:=AValue;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetExcludeFileFilter(const AValue: string);
begin
  if FExcludeFileFilter=AValue then exit;
  FExcludeFileFilter:=AValue;
  UpdateExcludeFilter;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetExcludeFilterSimpleSyntax(
  const AValue: boolean);
begin
  if FExcludeFilterSimpleSyntax=AValue then exit;
  FExcludeFilterSimpleSyntax:=AValue;
  UpdateExcludeFilter;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetIgnoreBinaries(const AValue: boolean);
begin
  if FIgnoreBinaries=AValue then exit;
  FIgnoreBinaries:=AValue;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetIncludeFileFilter(const AValue: string);
begin
  if FIncludeFileFilter=AValue then exit;
  FIncludeFileFilter:=AValue;
  UpdateIncludeFilter;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetIncludeFilterSimpleSyntax(
  const AValue: boolean);
begin
  if FIncludeFilterSimpleSyntax=AValue then exit;
  FIncludeFilterSimpleSyntax:=AValue;
  UpdateIncludeFilter;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetModified(const AValue: boolean);
begin
  if AValue and (FModifiedLock>0) then exit;
  if FModified=AValue then exit;
  FModified:=AValue;
  DoOnModifyChange;
end;

procedure TPublishModuleOptions.SetUseExcludeFileFilter(const AValue: boolean);
begin
  if FUseExcludeFileFilter=AValue then exit;
  FUseExcludeFileFilter:=AValue;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetUseIncludeFileFilter(const AValue: boolean);
begin
  if FUseIncludeFileFilter=AValue then exit;
  FUseIncludeFileFilter:=AValue;
  Modified:=true;
end;

procedure TPublishModuleOptions.UpdateIncludeFilter;
var
  Expr: string;
begin
  if FIncludeFilterRegExpr=nil then
    FIncludeFilterRegExpr:=TRegExpr.Create;
  if IncludeFilterSimpleSyntax then
    Expr:=SimpleSyntaxToRegExpr(FIncludeFileFilter)
  else
    Expr:=FIncludeFileFilter;
  try
    FIncludeFilterRegExpr.Expression:=Expr;
    FIncludeFilterValid:=true;
  except
    on E: Exception do begin
      DebugLn('Invalid Include File Expression ',Expr,' ',E.Message);
      FIncludeFilterValid:=false;
    end;
  end;
end;

procedure TPublishModuleOptions.UpdateExcludeFilter;
var
  Expr: string;
begin
  if FExcludeFilterRegExpr=nil then
    FExcludeFilterRegExpr:=TRegExpr.Create;
  if ExcludeFilterSimpleSyntax then
    Expr:=SimpleSyntaxToRegExpr(FExcludeFileFilter)
  else
    Expr:=FExcludeFileFilter;
  try
    FExcludeFilterRegExpr.Expression:=Expr;
    FExcludeFilterValid:=true;
  except
    on E: Exception do begin
      DebugLn('Invalid Exclude File Expression ',Expr,' ',E.Message);
      FExcludeFilterValid:=false;
    end;
  end;
end;

procedure TPublishModuleOptions.DoOnModifyChange;
begin

end;

constructor TPublishModuleOptions.Create(TheOwner: TObject);
begin
  FOwner:=TheOwner;
  LoadDefaults;
end;

destructor TPublishModuleOptions.Destroy;
begin
  Clear;
  FIncludeFilterRegExpr.Free;
  FExcludeFilterRegExpr.Free;
  inherited Destroy;
end;

procedure TPublishModuleOptions.Clear;
begin
  LoadDefaults;
end;

procedure TPublishModuleOptions.LoadDefaults;
begin
  DestinationDirectory:=GetDefaultDestinationDir;
  CommandAfter:='';
  IgnoreBinaries:=true;
  UseIncludeFileFilter:=true;
  IncludeFilterSimpleSyntax:=true;
  IncludeFileFilter:=DefPublModIncFilter;
  UseExcludeFileFilter:=false;
  ExcludeFilterSimpleSyntax:=true;
  ExcludeFileFilter:=DefPublModExcFilter;
end;

procedure TPublishModuleOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const APath: string; AdjustPathDelims: boolean);

  function f(const Filename: string): string;
  begin
    Result:=SwitchPathDelims(Filename,AdjustPathDelims);
  end;

var
  XMLVersion: integer;
begin
  XMLVersion:=XMLConfig.GetValue(APath+'Version/Value',0);
  FDestinationDirectory:=f(XMLConfig.GetValue(APath+'DestinationDirectory/Value',
                                            GetDefaultDestinationDir));
  FCommandAfter:=f(XMLConfig.GetValue(APath+'CommandAfter/Value',''));
  IgnoreBinaries:=XMLConfig.GetValue(APath+'IgnoreBinaries/Value',true);
  UseIncludeFileFilter:=XMLConfig.GetValue(APath+'UseIncludeFileFilter/Value',
                                            true);
  IncludeFilterSimpleSyntax:=
    XMLConfig.GetValue(APath+'IncludeFilterSimpleSyntax/Value',true);
  if XMLVersion>=2 then
    IncludeFileFilter:=XMLConfig.GetValue(APath+'IncludeFileFilter/Value',
                                           DefPublModIncFilter);
  UseExcludeFileFilter:=XMLConfig.GetValue(APath+'UseExcludeFileFilter/Value',
                                           false);
  ExcludeFilterSimpleSyntax:=
    XMLConfig.GetValue(APath+'ExcludeFilterSimpleSyntax/Value',
                       true);
  if XMLVersion>=2 then
    ExcludeFileFilter:=XMLConfig.GetValue(APath+'ExcludeFileFilter/Value',
                                           DefPublModExcFilter);
end;

procedure TPublishModuleOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const APath: string; UsePathDelim: TPathDelimSwitch);

  function f(const AFilename: string): string;
  begin
    Result:=SwitchPathDelims(AFilename,UsePathDelim);
  end;

begin
  XMLConfig.SetValue(APath+'Version/Value',PublishModulOptsVersion);
  XMLConfig.SetDeleteValue(APath+'DestinationDirectory/Value',
                           f(DestinationDirectory),
                           f(GetDefaultDestinationDir));
  XMLConfig.SetDeleteValue(APath+'CommandAfter/Value',f(CommandAfter),'');
  XMLConfig.SetDeleteValue(APath+'IgnoreBinaries/Value',IgnoreBinaries,true);
  XMLConfig.SetDeleteValue(APath+'UseIncludeFileFilter/Value',
    UseIncludeFileFilter,true);
  XMLConfig.SetDeleteValue(APath+'IncludeFilterSimpleSyntax/Value',
    IncludeFilterSimpleSyntax,true);
  XMLConfig.SetDeleteValue(APath+'IncludeFileFilter/Value',
    IncludeFileFilter,DefPublModIncFilter);
  XMLConfig.SetDeleteValue(APath+'UseExcludeFileFilter/Value',
    UseExcludeFileFilter,false);
  XMLConfig.SetDeleteValue(APath+'ExcludeFilterSimpleSyntax/Value',
    ExcludeFilterSimpleSyntax,true);
  XMLConfig.SetDeleteValue(APath+'ExcludeFileFilter/Value',
    ExcludeFileFilter,DefPublModExcFilter);
end;

function TPublishModuleOptions.FileCanBePublished(
  const AFilename: string): boolean;
begin
  Result:=false;

  // check include filter
  if UseIncludeFileFilter
  and (FIncludeFilterRegExpr<>nil)
  and (not FIncludeFilterRegExpr.Exec(ExtractFilename(AFilename))) then
    exit;

  // check exclude filter
  if UseExcludeFileFilter
  and (FExcludeFilterRegExpr<>nil)
  and (FExcludeFilterRegExpr.Exec(ExtractFilename(AFilename))) then
    exit;

  // check binaries
  if IgnoreBinaries and (not DirPathExists(AFilename))
  and (not FileIsText(AFilename)) then exit;

  Result:=true;
end;

procedure TPublishModuleOptions.LockModified;
begin
  inc(FModifiedLock);
end;

procedure TPublishModuleOptions.UnlockModified;
begin
  if FModifiedLock<=0 then
    RaiseGDBException('TPublishModuleOptions.UnlockModified');
  dec(FModifiedLock);
end;

function TPublishModuleOptions.GetDefaultDestinationDir: string;
begin
  Result:=DefPublishDirectory;
end;

end.

