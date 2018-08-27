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

  Author: Mattias Gaertner, Juha Manninen

}
unit PublishModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr,
  // LazUtils
  LazFileUtils, LazLoggerBase, LazTracer, LazStringUtils, Laz2_XMLCfg, UITypes,
  // IdeIntf
  MacroIntf, IDEDialogs,
  // IDE
  IDEProcs, LazarusIDEStrConsts;

type
  { TPublishModuleOptions }

  TPublishModuleOptions = class
  private
    FCompressFinally: boolean;
    FDestinationDirectory: string;
    FExcludeFileFilter: string;
    FExcludeFilterRegExpr: TRegExpr;
    FExcludeFilterSimpleSyntax: boolean;
    FExcludeFilterValid: boolean;
    FIncludeFileFilter: string;
    FIncludeFilterRegExpr: TRegExpr;
    FIncludeFilterSimpleSyntax: boolean;
    FIncludeFilterValid: boolean;
    FModified: boolean;
    FModifiedLock: integer;
    FOwner: TObject;
    FUseFileFilters: boolean;
    procedure SetCompressFinally(const AValue: boolean);
    procedure SetDestinationDirectory(const AValue: string);
    procedure SetExcludeFileFilter(const AValue: string);
    procedure SetExcludeFilterSimpleSyntax(const AValue: boolean);
    procedure SetIncludeFileFilter(const AValue: string);
    procedure SetIncludeFilterSimpleSyntax(const AValue: boolean);
    procedure SetModified(const AValue: boolean);
    procedure SetUseFileFilters(const AValue: boolean);
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
    function GetDefaultDestinationDir: string; virtual; abstract;
  public
    property Owner: TObject read FOwner;
    property Modified: boolean read FModified write SetModified;
  
    // destination
    property DestinationDirectory: string
                read FDestinationDirectory write SetDestinationDirectory;
    property CompressFinally: boolean read FCompressFinally write SetCompressFinally;
    property UseFileFilters: boolean read FUseFileFilters write SetUseFileFilters;
    // Include Filter
    property IncludeFilterSimpleSyntax: boolean
                read FIncludeFilterSimpleSyntax write SetIncludeFilterSimpleSyntax;
    property IncludeFileFilter: string
                read FIncludeFileFilter write SetIncludeFileFilter;
    property IncludeFilterValid: boolean read FIncludeFilterValid;
    // Exclude Filter
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

function RealPublishDir(AOptions: TPublishModuleOptions): string;


implementation

function RealPublishDir(AOptions: TPublishModuleOptions): string;
begin
  Result:=AOptions.DestinationDirectory;
  if IDEMacros.SubstituteMacros(Result) then begin
    if FilenameIsAbsolute(Result) then begin
      Result:=AppendPathDelim(TrimFilename(Result));
    end else begin
      Result:='';
    end;
  end else begin
    Result:='';
  end;
end;

{ TPublishModuleOptions }

procedure TPublishModuleOptions.SetCompressFinally(const AValue: boolean);
begin
  if FCompressFinally=AValue then exit;
  FCompressFinally:=AValue;
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

procedure TPublishModuleOptions.SetExcludeFilterSimpleSyntax(const AValue: boolean);
begin
  if FExcludeFilterSimpleSyntax=AValue then exit;
  FExcludeFilterSimpleSyntax:=AValue;
  UpdateExcludeFilter;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetIncludeFileFilter(const AValue: string);
begin
  if FIncludeFileFilter=AValue then exit;
  FIncludeFileFilter:=AValue;
  UpdateIncludeFilter;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetIncludeFilterSimpleSyntax(const AValue: boolean);
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

procedure TPublishModuleOptions.SetUseFileFilters(const AValue: boolean);
begin
  if FUseFileFilters=AValue then exit;
  FUseFileFilters:=AValue;
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
  CompressFinally:=true;
  UseFileFilters:=true;
  IncludeFilterSimpleSyntax:=true;
  IncludeFileFilter:=DefPublModIncFilter;
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
  CompressFinally:=XMLConfig.GetValue(APath+'CompressFinally/Value',true);
  UseFileFilters:=XMLConfig.GetValue(APath+'UseFileFilters/Value',false);
  IncludeFilterSimpleSyntax:=XMLConfig.GetValue(APath+'IncludeFilterSimpleSyntax/Value',true);
  ExcludeFilterSimpleSyntax:=XMLConfig.GetValue(APath+'ExcludeFilterSimpleSyntax/Value',true);
  if XMLVersion>=2 then begin
    IncludeFileFilter:=XMLConfig.GetValue(APath+'IncludeFileFilter/Value',
                                           DefPublModIncFilter);
    ExcludeFileFilter:=XMLConfig.GetValue(APath+'ExcludeFileFilter/Value',
                                           DefPublModExcFilter);
  end;
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
  XMLConfig.SetDeleteValue(APath+'CompressFinally/Value',CompressFinally,true);
  XMLConfig.SetDeleteValue(APath+'UseFileFilters/Value',UseFileFilters,false);
  XMLConfig.SetDeleteValue(APath+'IncludeFilterSimpleSyntax/Value',
                           IncludeFilterSimpleSyntax,true);
  XMLConfig.SetDeleteValue(APath+'IncludeFileFilter/Value',IncludeFileFilter,
                           DefPublModIncFilter);
  XMLConfig.SetDeleteValue(APath+'ExcludeFilterSimpleSyntax/Value',
                           ExcludeFilterSimpleSyntax,true);
  XMLConfig.SetDeleteValue(APath+'ExcludeFileFilter/Value',ExcludeFileFilter,
                           DefPublModExcFilter);
end;

function TPublishModuleOptions.FileCanBePublished(const AFilename: string): boolean;
begin
  Result:=false;
  // check include filter
  if (FIncludeFilterRegExpr<>nil)
  and (not FIncludeFilterRegExpr.Exec(ExtractFilename(AFilename))) then
    exit;
  // check exclude filter
  if (FExcludeFilterRegExpr<>nil)
  and (FExcludeFilterRegExpr.Exec(ExtractFilename(AFilename))) then
    exit;
  // check binaries
  //if IgnoreBinaries and (not DirPathExists(AFilename))
  //and (not FileIsText(AFilename)) then exit;
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
{
procedure TPublishModuleOptions.OnCopyFile(const Filename: string; var Copy: boolean; Data: TObject);
begin
  //if Data=nil then exit;
  Assert(Data is TPublishModuleOptions, 'TPublishModuleOptions.OnCopyFile: Data type is wrong.');
  Copy:=TPublishModuleOptions(Data).FileCanBePublished(Filename);
  //debugln('TLazSourceFileManager.OnCopyFile "',Filename,'" ',Copy);
end;

procedure TPublishModuleOptions.OnCopyError(const ErrorData: TCopyErrorData;
  var Handled: boolean; Data: TObject);
begin
  case ErrorData.Error of
    ceSrcDirDoesNotExists:
      IDEMessageDialog(lisCopyError2,
        Format(lisSourceDirectoryDoesNotExist, [ErrorData.Param1]),
        mtError,[mbCancel]);
    ceCreatingDirectory:
      IDEMessageDialog(lisCopyError2,
        Format(lisUnableToCreateDirectory, [ErrorData.Param1]),
        mtError,[mbCancel]);
    ceCopyFileError:
      IDEMessageDialog(lisCopyError2,
        Format(lisUnableToCopyFileTo, [ErrorData.Param1, LineEnding, ErrorData.Param1]),
        mtError,[mbCancel]);
  end;
end;
}
end.

