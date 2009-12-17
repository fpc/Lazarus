{
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

  Abstract:
    Methods and types for simple HTML help.
}
unit LazHelpHTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLIntf, Forms, Process, FileUtil, UTF8Process,
  LazConfigStorage, LCLStrConsts, HelpIntfs, LazHelpIntf;
  
type
  { THTMLHelpDatabase

    KeywordPrefix: if set, then the database will handle all Keywords
      beginning with this value. And when the path is created by replacing
      the prefix with the BaseURL.
      For example:
        Put a THTMLHelpDatabase on a form.
        Set AutoRegister to true.
        Set KeywordPrefix to 'MyHelp/'
        Set BaseURL to 'file://'
        
        Put a THTMLBrowserHelpViewer on the form.
        Set AutoRegister to true.
        Set BrowserPath to '/usr/bin/mozilla'

        Put a TEdit on a form.
        Set HelpType to htKeyword
        Set HelpKeyword to 'MyHelp/page.html'
        
        Run the program.
        Focus the edit field and press F1. The page 'page.html' will be shown.
        }

  THTMLHelpDatabase = class(THelpDatabase)
  private
    FBaseURL: string;
    FDefaultBaseURL: string;
    FKeywordPrefix: string;
    FKeywordPrefixNode: THelpNode;
    function IsBaseURLStored: boolean;
    procedure SetBaseURL(const AValue: string);
    procedure SetDefaultBaseURL(const AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function ShowURL(const URL, Title: string;
                     var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelp(Query: THelpQuery; BaseNode, NewNode: THelpNode;
                      QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; override;
    function GetNodesForKeyword(const HelpKeyword: string;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; override;
    function GetEffectiveBaseURL: string;
    procedure Load(Storage: TConfigStorage); override;
    procedure Save(Storage: TConfigStorage); override;
    property DefaultBaseURL: string read FDefaultBaseURL write SetDefaultBaseURL;// used, if BaseURL is empty
  published
    property BaseURL: string read FBaseURL write SetBaseURL stored IsBaseURLStored;
    property AutoRegister;
    property KeywordPrefix: string read FKeywordPrefix write FKeywordPrefix;// see above
  end;
  
  
  { THTMLBrowserHelpViewer

    If no browser is specified it searches for a common browser. }
  
  TOnFindDefaultBrowser = procedure(var DefaultBrowser, Params: string) of object;

  THTMLBrowserHelpViewer = class(THelpViewer)
  private
    FBrowserParams: string;
    FBrowserPath: string;
    FDefaultBrowser: string;
    FDefaultBrowserParams: string;
    FOnFindDefaultBrowser: TOnFindDefaultBrowser;
    procedure SetBrowserParams(const AValue: string);
    procedure SetBrowserPath(const AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    function ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult; override;
    procedure FindDefaultBrowser(out Browser, Params: string); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Load(Storage: TConfigStorage); override;
    procedure Save(Storage: TConfigStorage); override;
    function GetLocalizedName: string; override;
    property OnFindDefaultBrowser: TOnFindDefaultBrowser
               read FOnFindDefaultBrowser write FOnFindDefaultBrowser;
  published
    property BrowserPath: string read FBrowserPath write SetBrowserPath;
    property BrowserParams: string read FBrowserParams write SetBrowserParams;
    property AutoRegister;
  end;
  

procedure Register;
  
implementation

procedure Register;
begin
  RegisterComponents('System',[THTMLHelpDatabase,THTMLBrowserHelpViewer]);
end;

{ THTMLHelpDatabase }

procedure THTMLHelpDatabase.SetBaseURL(const AValue: string);
begin
  if FBaseURL=AValue then exit;
  //debugln('THTMLHelpDatabase.SetBaseURL ',dbgsName(Self),' ',AValue);
  if AValue=DefaultBaseURL then
    FBaseURL:=''
  else
    FBaseURL:=AValue;
end;

procedure THTMLHelpDatabase.SetDefaultBaseURL(const AValue: string);
begin
  if FDefaultBaseURL=AValue then exit;
  if (FBaseURL='') or (FBaseURL=FDefaultBaseURL) then
    FBaseURL:=FDefaultBaseURL;
  FDefaultBaseURL:=AValue;
end;

function THTMLHelpDatabase.IsBaseURLStored: boolean;
begin
  Result:=FBaseURL<>DefaultBaseURL;
end;

constructor THTMLHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AddSupportedMimeType('text/html');
end;

destructor THTMLHelpDatabase.Destroy;
begin
  FreeAndNil(FKeywordPrefixNode);
  inherited Destroy;
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
  //debugln('THTMLHelpDatabase.ShowURL A NewNode.URL=',URL,' URLType=',URLType,' URLPath=',URLPath,' URLParams=',URLParams);

  if URLType='file' then begin
    if not URLFilenameIsAbsolute(URLPath) then begin
      EffBaseURL:=GetEffectiveBaseURL;
      //DebugLn('THTMLHelpDatabase.ShowURL file relative, making absolute ... EffBaseURL="',EffBaseURL,'"');
      if EffBaseURL<>'' then begin
        SplitURL(EffBaseURL,BaseURLType,BaseURLPath,BaseURLParams);
        if (BaseURLPath<>'') then
          URLPath:=BaseURLPath+URLPath;
        URLType:=BaseURLType;
      end;
    end;
    if (URLType='file') and (not URLFilenameIsAbsolute(URLPath)) then
      URLPath:=FilenameToURLPath(TrimFilename(GetCurrentDirUTF8+PathDelim))+URLPath;
    
    if (URLType='file') and (not FileExistsUTF8(URLPath)) then begin
      Result:=shrContextNotFound;
      ErrMsg:=Format(hhsHelpTheHelpDatabaseWasUnableToFindFile, ['"', ID,
        '"', '"', URLPath, '"']);
      exit;
    end;
  end;
  FullURL:=CombineURL(URLType,URLPath,URLParams);
  {$IFNDEF DisableChecks}
  debugln('THTMLHelpDatabase.ShowURL B URL=',URL,' URLType=',URLType,' URLPath=',URLPath,' URLParams=',URLParams);
  {$ENDIF}

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
  BaseNode, NewNode: THelpNode; QueryItem: THelpQueryItem;
  var ErrMsg: string): TShowHelpResult;
begin
  ErrMsg:='';
  Result:=shrContextNotFound;
  if NewNode.URLValid then begin
    Result:=ShowURL(NewNode.URL,NewNode.Title,ErrMsg);
  end else begin
    Result:=shrContextNotFound;
    ErrMsg:='THTMLHelpDatabase.ShowHelp Node.URLValid=false Node.URL="'+NewNode.URL+'"';
  end;
end;

function THTMLHelpDatabase.GetNodesForKeyword(const HelpKeyword: string;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
var
  Path: String;
begin
  Result:=inherited GetNodesForKeyword(HelpKeyword, ListOfNodes, ErrMsg);
  if Result<>shrSuccess then exit;

  if not (csDesigning in ComponentState)
  and (KeywordPrefix<>'')
  and (LeftStr(HelpKeyword,length(KeywordPrefix))=KeywordPrefix) then begin
    // HelpKeyword starts with KeywordPrefix -> add default node
    if FKeywordPrefixNode=nil then
      FKeywordPrefixNode:=THelpNode.CreateURL(Self,'','');
    Path:=copy(HelpKeyword,length(KeywordPrefix)+1,length(HelpKeyword));
    FKeywordPrefixNode.Title:='Show page '+Path;
    FKeywordPrefixNode.URL:='file://'+Path;
    CreateNodeQueryListAndAdd(FKeywordPrefixNode,nil,ListOfNodes,true);
  end;
end;

function THTMLHelpDatabase.GetEffectiveBaseURL: string;
begin
  Result:='';
  if BaseURL<>'' then begin
    Result:=BaseURL;
    if (Databases<>nil) then begin
      Databases.SubstituteMacros(Result);
      Result:=FilenameToURLPath(Result);
    end;
    //debugln('THTMLHelpDatabase.GetEffectiveBaseURL using BaseURL="',Result,'"');
  end else if (BasePathObject<>nil) and (Databases<>nil) then begin
    Result:=Databases.GetBaseURLForBasePathObject(BasePathObject);
    //debugln('THTMLHelpDatabase.GetEffectiveBaseURL using BasePathObject="',Result,'"');
  end;
  if (Result='') and (DefaultBaseURL<>'') then begin
    Result:=DefaultBaseURL;
    if (Databases<>nil) then begin
      Databases.SubstituteMacros(Result);
      Result:=FilenameToURLPath(Result);
    end;
    //debugln('THTMLHelpDatabase.GetEffectiveBaseURL using DefaultBaseURL="',Result,'"');
  end;
  Result:=AppendURLPathDelim(Result);
end;

procedure THTMLHelpDatabase.Load(Storage: TConfigStorage);
begin
  inherited Load(Storage);
  BaseURL:=Storage.GetValue('BaseURL/Value',DefaultBaseURL);
end;

procedure THTMLHelpDatabase.Save(Storage: TConfigStorage);
begin
  inherited Save(Storage);
  Storage.SetDeleteValue('BaseURL/Value',BaseURL,DefaultBaseURL);
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

constructor THTMLBrowserHelpViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AddSupportedMimeType('text/html');
  FBrowserParams:='%s';
  ParameterHelp:=hhsHelpTheMacroSInBrowserParamsWillBeReplacedByTheURL;
end;

function THTMLBrowserHelpViewer.ShowNode(Node: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
var
  Params: String;
  URLMacroPos: LongInt;
  BrowserProcess: TProcessUTF8;
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
  Params:=BrowserParams;
  if CommandLine='' then
    FindDefaultBrowser(CommandLine, Params);
  if CommandLine='' then begin
    if (HelpDatabases<>nil)
    and (CompareText(HelpDatabases.ClassName,'TIDEHelpDatabases')=0) then
      ErrMsg:=Format(hhsHelpNoHTMLBrowserFoundPleaseDefineOneInHelpConfigureHe,
        [ #13])
    else
      ErrMsg:=hhsHelpNoHTMLBrowserFound;
    exit;
  end;
  if (not FileExistsUTF8(CommandLine)) then begin
    ErrMsg:=Format(hhsHelpBrowserNotFound, ['"', CommandLine, '"']);
    exit;
  end;
  if (not FileIsExecutable(CommandLine)) then begin
    ErrMsg:=Format(hhsHelpBrowserNotExecutable, ['"', CommandLine, '"']);
    exit;
  end;
  
  //debugln('THTMLBrowserHelpViewer.ShowNode Node.URL=',Node.URL);
  
  // create params and replace %s for URL
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
  
  {$IFNDEF DisableChecks}
  debugln('THTMLBrowserHelpViewer.ShowNode CommandLine=',CommandLine);
  {$ENDIF}

  // run
  try
    BrowserProcess:=TProcessUTF8.Create(nil);
    try
      BrowserProcess.CommandLine:=CommandLine;
      BrowserProcess.Execute;
    finally
      BrowserProcess.Free;
    end;
    Result:=shrSuccess;
  except
    on E: Exception do begin
      ErrMsg:=Format(hhsHelpErrorWhileExecuting, ['"', CommandLine, '"', #13,
        E.Message]);
    end;
  end;
end;

procedure THTMLBrowserHelpViewer.FindDefaultBrowser(out Browser, Params: string);
begin
  if FDefaultBrowser='' then
  begin
    if Assigned(OnFindDefaultBrowser) then
      OnFindDefaultBrowser(FDefaultBrowser, FDefaultBrowserParams);
  end;
  if FDefaultBrowser = '' then
    LCLIntf.FindDefaultBrowser(FDefaultBrowser, FDefaultBrowserParams);

  Browser := FDefaultBrowser;
  Params := FDefaultBrowserParams;

  //DebugLn('THTMLBrowserHelpViewer.FindDefaultBrowser Browser=',Browser,' Params=',Params);
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

end.

