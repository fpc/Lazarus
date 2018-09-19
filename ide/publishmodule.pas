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
  LazFileUtils, LazLoggerBase, LazTracer, LazStringUtils, Laz2_XMLCfg,
  // IdeIntf
  MacroIntf,
  // IDE
  IDEProcs;

type
  { TPublishModuleOptions }

  TPublishModuleOptions = class
  private
    FCompressFinally: boolean;
    FOpenInFileMan: boolean;
    FDestinationDirectory: string;
    FFileFilter: string;
    FFilterRegExpr: TRegExpr;
    FFilterSimpleSyntax: boolean;
    FFilterValid: boolean;
    FModified: boolean;
    FModifiedLock: integer;
    FOwner: TObject;
    FUseFileFilters: boolean;
    procedure SetCompressFinally(const AValue: boolean);
    procedure SetOpenInFileMan(const AValue: boolean);
    procedure SetDestinationDirectory(const AValue: string);
    procedure SetFileFilter(const AValue: string);
    procedure SetFilterSimpleSyntax(const AValue: boolean);
    procedure SetModified(const AValue: boolean);
    procedure SetUseFileFilters(const AValue: boolean);
    procedure UpdateFilter;
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
    property OpenInFileMan: boolean read FOpenInFileMan write SetOpenInFileMan;
    property UseFileFilters: boolean read FUseFileFilters write SetUseFileFilters;
    // Filter
    property FilterSimpleSyntax: boolean read FFilterSimpleSyntax write SetFilterSimpleSyntax;
    property FileFilter: string read FFileFilter write SetFileFilter;
    property FilterValid: boolean read FFilterValid;
  end;
  
const
  PublishModulOptsVersion = 2;

  DefPublModIncFilter = '*.(pas|pp|inc|lpr|lfm|lrs|lpi|lpk|xml|sh)';
  //DefPublModExcFilter = '*.(bak|ppu|ppl|a|o|so);*~;backup';

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

procedure TPublishModuleOptions.SetOpenInFileMan(const AValue: boolean);
begin
  if FOpenInFileMan=AValue then exit;
  FOpenInFileMan:=AValue;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetDestinationDirectory(const AValue: string);
begin
  if FDestinationDirectory=AValue then exit;
  FDestinationDirectory:=AValue;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetFileFilter(const AValue: string);
begin
  if FFileFilter=AValue then exit;
  FFileFilter:=AValue;
  UpdateFilter;
  Modified:=true;
end;

procedure TPublishModuleOptions.SetFilterSimpleSyntax(const AValue: boolean);
begin
  if FFilterSimpleSyntax=AValue then exit;
  FFilterSimpleSyntax:=AValue;
  UpdateFilter;
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

procedure TPublishModuleOptions.UpdateFilter;
var
  Expr: string;
begin
  if FFilterRegExpr=nil then
    FFilterRegExpr:=TRegExpr.Create;
  if FilterSimpleSyntax then
    Expr:=SimpleSyntaxToRegExpr(FFileFilter)
  else
    Expr:=FFileFilter;
  try
    FFilterRegExpr.Expression:=Expr;
    FFilterValid:=true;
  except
    on E: Exception do begin
      DebugLn('Invalid File Expression ',Expr,' ',E.Message);
      FFilterValid:=false;
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
  FFilterRegExpr.Free;
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
  OpenInFileMan:=false;
  UseFileFilters:=true;
  FilterSimpleSyntax:=true;
  FileFilter:=DefPublModIncFilter;
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
  OpenInFileMan:=XMLConfig.GetValue(APath+'OpenInFileMan/Value',false);
  UseFileFilters:=XMLConfig.GetValue(APath+'UseFileFilters/Value',false);
  FilterSimpleSyntax:=XMLConfig.GetValue(APath+'FilterSimpleSyntax/Value',true);
  if XMLVersion>=2 then
    FileFilter:=XMLConfig.GetValue(APath+'FileFilter/Value',DefPublModIncFilter);
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
  XMLConfig.SetDeleteValue(APath+'OpenInFileMan/Value',OpenInFileMan,false);
  XMLConfig.SetDeleteValue(APath+'UseFileFilters/Value',UseFileFilters,false);
  XMLConfig.SetDeleteValue(APath+'FilterSimpleSyntax/Value',FilterSimpleSyntax,true);
  XMLConfig.SetDeleteValue(APath+'FileFilter/Value',FileFilter,DefPublModIncFilter);
end;

function TPublishModuleOptions.FileCanBePublished(const AFilename: string): boolean;
begin
  // check file filter
  Result := (FFilterRegExpr=nil) or FFilterRegExpr.Exec(ExtractFilename(AFilename));
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

