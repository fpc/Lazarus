{
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

  Abstract:
    Methods and types for simple HTML help.
}
unit HelpHTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Process, FileCtrl, ConfigStorage,
  PropEdits, ObjInspStrConsts, HelpIntf;
  
type
  { THTMLHelpDatabase }

  THTMLHelpDatabase = class(THelpDatabase)
  private
    FBaseURL: string;
    procedure SetBaseURL(const AValue: string);
  public
    constructor Create(TheID: THelpDatabaseID); override;
    function ShowURL(const URL, Title: string;
                     var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelp(Query: THelpQuery; BaseNode, NewNode: THelpNode;
                      var ErrMsg: string): TShowHelpResult; override;
    function GetEffectiveBaseURL: string;
  public
    property BaseURL: string read FBaseURL write SetBaseURL;
  end;
  
  
  { THTMLBrowserHelpViewer }
  
  THTMLBrowserHelpViewer = class(THelpViewer)
  private
    FBrowserParams: string;
    FBrowserPath: string;
    procedure SetBrowserParams(const AValue: string);
    procedure SetBrowserPath(const AValue: string);
  public
    constructor Create;
    function ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult; override;
    function FindDefaultBrowser: string; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Load(Storage: TConfigStorage); override;
    procedure Save(Storage: TConfigStorage); override;
    function GetLocalizedName: string; override;
  published
    property BrowserPath: string read FBrowserPath write SetBrowserPath;
    property BrowserParams: string read FBrowserParams write SetBrowserParams;
  end;
  
  
implementation

{ THTMLHelpDatabase }

procedure THTMLHelpDatabase.SetBaseURL(const AValue: string);
begin
  if FBaseURL=AValue then exit;
  FBaseURL:=AValue;
end;

constructor THTMLHelpDatabase.Create(TheID: THelpDatabaseID);
begin
  inherited Create(TheID);
  AddSupportedMimeType('text/html');
end;

function THTMLHelpDatabase.ShowURL(const URL, Title: string; var ErrMsg: string
  ): TShowHelpResult;
var
  URLType, URLPath, URLParams: string;
  BaseURLType, BaseURLPath, BaseURLParams: string;
  Viewer: THelpViewer;
  EffBaseURL: String;
  Node: THelpNode;
  FullURL: String;
begin
  //DebugLn('THTMLHelpDatabase.ShowURL A URL="',URL,'" Title="',Title,'"');

  // find HTML viewer
  Result:=FindViewer('text/html',ErrMsg,Viewer);
  if Result<>shrSuccess then exit;

  // make URL absolute
  SplitURL(URL,URLType,URLPath,URLParams);
  debugln('THTMLHelpDatabase.ShowHelp A NewNode.URL=',URL,' URLType=',URLType,' URLPath=',URLPath,' URLParams=',URLParams);

  if URLType='file' then begin
    if not URLFilenameIsAbsolute(URLPath) then begin
      EffBaseURL:=GetEffectiveBaseURL;
      SplitURL(EffBaseURL,BaseURLType,BaseURLPath,BaseURLParams);
      if (BaseURLType='file') and (BaseURLPath<>'') then
        URLPath:=BaseURLPath+URLPath;
    end;
    if (not FileExists(URLPath)) then begin
      Result:=shrContextNotFound;
      ErrMsg:=Format(oisHelpTheHelpDatabaseWasUnableToFindFile, ['"', ID,
        '"', '"', URLPath, '"']);
      exit;
    end;
  end;
  FullURL:=CombineURL(URLType,URLPath,URLParams);
  debugln('THTMLHelpDatabase.ShowHelp B URL=',URL,' URLType=',URLType,' URLPath=',URLPath,' URLParams=',URLParams);

  // call viewer
  Node:=nil;
  try
    Node:=THelpNode.CreateURL(Self,Title,FullURL);
    Result:=Viewer.ShowNode(Node,ErrMsg);
  finally
    Node.Free;
  end;
end;

function THTMLHelpDatabase.ShowHelp(Query: THelpQuery;
  BaseNode, NewNode: THelpNode; var ErrMsg: string): TShowHelpResult;
begin
  ErrMsg:='';
  Result:=shrContextNotFound;
  if NewNode.URLValid then begin
    Result:=ShowURL(NewNode.URL,NewNode.Title,ErrMsg);
  end else begin
    Result:=shrContextNotFound;
    ErrMsg:='THTMLHelpDatabase.ShowHelp Node.URLValid=false';
  end;
end;

function THTMLHelpDatabase.GetEffectiveBaseURL: string;
begin
  Result:='';
  if BaseURL<>'' then begin
    Result:=BaseURL;
    if (HelpDatabases<>nil) then
      HelpDatabases.SubstituteMacros(Result);
  end else if (BasePathObject<>nil) and (Databases<>nil) then
    Result:=Databases.GetBaseURLForBasePathObject(BasePathObject);
  if (Result<>'') and (Result[length(Result)]<>'/') then
    Result:=Result+'/';
end;

{ THTMLBrowserHelpViewer }

procedure THTMLBrowserHelpViewer.SetBrowserParams(const AValue: string);
begin
  if FBrowserParams=AValue then exit;
  FBrowserParams:=AValue;
end;

procedure THTMLBrowserHelpViewer.SetBrowserPath(const AValue: string);
begin
  if FBrowserPath=AValue then exit;
  FBrowserPath:=AValue;
end;

constructor THTMLBrowserHelpViewer.Create;
begin
  inherited Create;
  AddSupportedMimeType('text/html');
  FBrowserParams:='%s';
  ParameterHelp:=oisHelpTheMacroSInBrowserParamsWillBeReplacedByTheURL;
end;

function THTMLBrowserHelpViewer.ShowNode(Node: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
var
  Params: String;
  URLMacroPos: LongInt;
  BrowserProcess: TProcess;
  CommandLine: String;
begin
  Result:=shrViewerError;
  ErrMsg:='';
  if (not Node.URLValid) then begin
    ErrMsg:='THTMLBrowserHelpViewer.ShowNode Node.URLValid=false';
    exit;
  end;
  if (Node.URL='') then begin
    ErrMsg:='THTMLBrowserHelpViewer.ShowNode Node.URL empty';
    exit;
  end;

  // check browser path
  CommandLine:=BrowserPath;
  if CommandLine='' then
    CommandLine:=FindDefaultBrowser;
  if CommandLine='' then begin
    ErrMsg:=Format(oisHelpNoHTMLBrowserFoundPleaseDefineOneInHelpConfigureHe, [
      #13]);
    exit;
  end;
  if (not FileExists(CommandLine)) then begin
    ErrMsg:=Format(oisHelpBrowserNotFound, ['"', CommandLine, '"']);
    exit;
  end;
  if (not FileIsExecutable(CommandLine)) then begin
    ErrMsg:=Format(oisHelpBrowserNotExecutable, ['"', CommandLine, '"']);
    exit;
  end;
  
  //debugln('THTMLBrowserHelpViewer.ShowNode Node.URL=',Node.URL);
  
  // create params and replace %s for URL
  Params:=BrowserParams;
  URLMacroPos:=Pos('%s',Params);
  if URLMacroPos>=1 then
    Params:=copy(Params,1,URLMacroPos-1)+Node.URL
           +copy(Params,URLMacroPos+2,length(Params)-URLMacroPos-1)
  else begin
    if Params<>'' then
      Params:=Params+' ';
    Params:=Params+Node.URL;
  end;
  CommandLine:=CommandLine+' '+Params;
  
  // run
  try
    BrowserProcess:=TProcess.Create(nil);
    try
      BrowserProcess.CommandLine:=CommandLine;
      BrowserProcess.Execute;
    finally
      BrowserProcess.Free;
    end;
    Result:=shrSuccess;
  except
    on E: Exception do begin
      ErrMsg:=Format(oisHelpErrorWhileExecuting, ['"', CommandLine, '"', #13,
        E.Message]);
    end;
  end;
end;

function THTMLBrowserHelpViewer.FindDefaultBrowser: string;

  function Find(const ShortFilename: string; var Filename: string): boolean;
  begin
    Filename:=SearchFileInPath(ShortFilename{$IFDEF win32}+'.exe'{$ENDIF},'',
                   Application.EnvironmentVariable['PATH'],':',[]);
    Result:=Filename<>'';
  end;

begin
  Result:='';
  // prefer open source ;)
  if Find('mozilla',Result) then exit;
  if Find('galeon',Result) then exit;
  if Find('konqueror',Result) then exit;
  if Find('safari',Result) then exit;
  if Find('netscape',Result) then exit;
  if Find('opera',Result) then exit;
  if Find('iexplorer',Result) then exit;
  Result:='';
end;

procedure THTMLBrowserHelpViewer.Assign(Source: TPersistent);
var
  Viewer: THTMLBrowserHelpViewer;
begin
  if Source is THTMLBrowserHelpViewer then begin
    Viewer:=THTMLBrowserHelpViewer(Source);
    BrowserPath:=Viewer.BrowserPath;
    BrowserParams:=Viewer.BrowserParams;
  end;
  inherited Assign(Source);
end;

procedure THTMLBrowserHelpViewer.Load(Storage: TConfigStorage);
begin
  BrowserPath:=Storage.GetValue('Browser/Path','');
  BrowserParams:=Storage.GetValue('Browser/Params','%s');
end;

procedure THTMLBrowserHelpViewer.Save(Storage: TConfigStorage);
begin
  Storage.SetDeleteValue('Browser/Path',BrowserPath,'');
  Storage.SetDeleteValue('Browser/Params',BrowserParams,'%s');
end;

function THTMLBrowserHelpViewer.GetLocalizedName: string;
begin
  Result:='HTML Browser';
end;

initialization
  RegisterPropertyEditor(TypeInfo(AnsiString),
    THTMLBrowserHelpViewer,'BrowserPath',TFileNamePropertyEditor);

end.

