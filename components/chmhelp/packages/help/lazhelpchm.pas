{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Methods and types for CHM help using chm viewer "lhelp".
}
unit LazHelpCHM;

{$mode objfpc}{$H+}

{$IFDEF VerboseLCLHelp}
{$DEFINE VerboseChmHelp}
{$ENDIF}

interface

uses
  Classes, SysUtils, LazHelpIntf, LazConfigStorage, HelpIntfs,
  Dialogs, Forms, LazLoggerBase, FileUtil, LazFileUtils, LHelpControl, LResources;

const
  CHMMimeType = 'application/chm';
  CHMPathParam = 'path';

type
  { TCHMHelpDatabase

    KeywordPrefix: if set, then the database will handle all Keywords
      beginning with this value. And when the path is created by replacing
      the prefix with the BaseURL.
      For example:
        Create a chm. For example build and run chmmaker in lazarus/tools/chmmaker
        to create the example.chm (lazarus/tools/chmmaker/example.chm).

        Put a TCHMHelpDatabase on a form.
        Set AutoRegister to true.
        Set KeywordPrefix to 'example'
        Set CHM file to '../../../tools/chmmaker/example.chm'

        Put a TLHelpRemoteViewer on the form.
        Set AutoRegister to true.
        Set LHelpPath to the path of lhelp. E.g. '../../lhelp/lhelp'

        Put a TEdit on a form.
        Set HelpType to htKeyword
        Set HelpKeyword to 'example/MainPage.html'

        Run the program.
        Focus the edit field and press F1. The page '/MainPage.html' will be shown.
        Note: lhelp requires the leading slash.
        }
  TCHMHelpDatabase = class(THelpDatabase)
  private
    FFilename: string;
    FHelpNode: THelpNode;
    FKeywordPrefix: string;
    procedure SetFilename(AValue: string);
    procedure SetKeywordPrefix(AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function ShowHelp({%H-}Query: THelpQuery; {%H-}BaseNode, NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; override;
    function ShowURL(const URL, Title: string;
                     var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForKeyword(const HelpKeyword: string;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; override;
    procedure Load(Storage: TConfigStorage); override;
    procedure Save(Storage: TConfigStorage); override;
  published
    property AutoRegister;
    property Filename: string read FFilename write SetFilename;
    property KeywordPrefix: string read FKeywordPrefix write SetKeywordPrefix;
  end;

type
  TOnFindLHelp = procedure(var Path: string) of object;

  { TLHelpConnector }

  TLHelpConnector = class(THelpViewer)
  private
    FConnection: TLHelpConnection;
    FLHelpPath: string;
    FOnFindLHelp: TOnFindLHelp;
    procedure SetLHelpPath(AValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    function ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult; override;
    procedure Assign(Source: TPersistent); override;
    procedure Load(Storage: TConfigStorage); override;
    procedure Save(Storage: TConfigStorage); override;
    function GetLocalizedName: string; override;
    property OnFindLHelp: TOnFindLHelp read FOnFindLHelp write FOnFindLHelp;
    property Connection: TLHelpConnection read FConnection;
  published
    property LHelpPath: string read FLHelpPath write SetLHelpPath;
    property AutoRegister;
  end;

procedure Register;

implementation

{$R lazhelpchm.res}

procedure Register;
begin
  RegisterComponents('System',[TCHMHelpDatabase,TLHelpConnector]);
end;

{ TLHelpConnector }

procedure TLHelpConnector.SetLHelpPath(AValue: string);
begin
  if FLHelpPath=AValue then Exit;
  FLHelpPath:=AValue;
end;

constructor TLHelpConnector.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AddSupportedMimeType(CHMMimeType);
end;

function TLHelpConnector.ShowNode(Node: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
var
  Path: String;
  IPCFile: String;
  URLScheme: string;
  URLPath: string;
  URLParams: string;
  CHMFilename: String;
  SubPath: String;
  Response: TLHelpResponse;
  s: String;
begin
  {$IFDEF VerboseChmHelp}
  debugln(['TLHelpConnector.ShowNode START URL="',Node.URL,'"']);
  {$ENDIF}

  Result:=shrViewerError;
  ErrMsg:='';
  if (not Node.URLValid) then
  begin
    ErrMsg:='TLHelpConnector.ShowNode Node.URLValid=false';
    exit;
  end;
  if (Node.URL='') then
  begin
    ErrMsg:='TLHelpConnector.ShowNode Node.URL empty';
    exit;
  end;

  SplitURL(Node.URL,URLScheme,URLPath,URLParams);
  CHMFilename:=CleanAndExpandFilename(URLPath);
  if not FileExistsUTF8(CHMFilename) then
  begin
    ErrMsg:='chm file "'+CHMFilename+'" not found';
    exit;
  end;
  if DirPathExists(CHMFilename) then
  begin
    ErrMsg:='invalid chm file "'+CHMFilename+'"';
    exit;
  end;

  SubPath:='';
  if (URLParams<>'') and (URLParams[1]='?') then
    Delete(URLParams,1,1);
  if LeftStr(URLParams,length(CHMPathParam)+1)=CHMPathParam+'=' then
  begin
    SubPath:=URLParams;
    Delete(SubPath,1,length(CHMPathParam)+1);
  end;

  if Connection=nil then
  begin
    // create a connection to lhelp:
    FConnection := TLHelpConnection.Create;
    Connection.ProcessWhileWaiting := @Application.ProcessMessages;
  end;

  if Connection.ServerRunning = false then
  begin
    // Use '_lhlpctl_' in case application developer uses SimpleIPC
    // and also uses the exe name followed by the process ID.
    // See help protocol specs defined in
    // http://wiki.lazarus.freepascal.org/Help_protocol
    // Use process id in order to avoid conflicts when multiple entries are running
    IPCFile:=LowerCase(ExtractFileName(Application.ExeName))+
      '_lhlpctl_'+
      copy(inttostr(GetProcessID)+'00000',1,5);
    {$IFDEF Unix}
    if FileExistsUTF8('/tmp/'+IPCFile) then
      DeleteFileUTF8('/tmp/'+IPCFile);
    {$ENDIF}

    // get lhelp path
    Path:=LHelpPath;
    if Assigned(OnFindLHelp) then
      OnFindLHelp(Path);

    // append exe extension
    if (ExtractFileExt(Path)='') and (GetExeExt<>'') then
      Path:=Path+GetExeExt;

    // search in Path
    if (Path<>'') and (ExtractFilePath(Path)='') then
    begin
      s:=FindDefaultExecutablePath(Path);
      if s<>'' then Path:=s;
    end;

    if not FileExistsUTF8(Path) then
    begin
      ErrMsg:='The chm viewer program lhelp was not found at "'+Path+'"';
      exit;
    end;

    Connection.StartHelpServer(IPCFile,Path);
  end;

  {$IFDEF VerboseChmHelp}
  debugln(['TLHelpConnector.ShowNode CHMFilename="',CHMFilename,'" SubPath="',SubPath,'"']);
  {$ENDIF}
  Response:=Connection.OpenURL(CHMFilename,SubPath);
  case Response of
  srSuccess: exit(shrSuccess);
  srNoAnswer: ErrMsg:='lhelp does not respond';
  srInvalidFile: ErrMsg:='lhelp can not open the file "'+CHMFilename+'"';
  srInvalidURL,srInvalidContext: ErrMsg:='lhelp can not find the help entry "'+SubPath+'"';
  else
    ErrMsg:='Something is wrong with lhelp';
  end;
  debugln(['TLHelpConnector.ShowNode error: ',ErrMsg]);
end;

procedure TLHelpConnector.Assign(Source: TPersistent);
var
  Src: TLHelpConnector;
begin
  if Source is TLHelpConnector then
  begin
    Src:=TLHelpConnector(Source);
    LHelpPath:=Src.LHelpPath;
  end;
  inherited Assign(Source);
end;

procedure TLHelpConnector.Load(Storage: TConfigStorage);
begin
  inherited Load(Storage);
  LHelpPath:=Storage.GetValue('LHelp/Path','');
end;

procedure TLHelpConnector.Save(Storage: TConfigStorage);
begin
  inherited Save(Storage);
  Storage.SetDeleteValue('LHelp/Path',LHelpPath,'');
end;

function TLHelpConnector.GetLocalizedName: string;
begin
  Result:='LHelp Connector';
end;

{ TCHMHelpDatabase }

procedure TCHMHelpDatabase.SetFilename(AValue: string);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
end;

procedure TCHMHelpDatabase.SetKeywordPrefix(AValue: string);
begin
  if FKeywordPrefix=AValue then Exit;
  FKeywordPrefix:=AValue;
end;

constructor TCHMHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AddSupportedMimeType(CHMMimeType);
end;

destructor TCHMHelpDatabase.Destroy;
begin
  FreeAndNil(FHelpNode);
  inherited Destroy;
end;

function TCHMHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
begin
  ErrMsg:='';
  Result:=shrContextNotFound;
  if NewNode.URLValid then
  begin
    Result:=ShowURL(NewNode.URL,NewNode.Title,ErrMsg);
  end
  else
  begin
    Result:=shrContextNotFound;
    ErrMsg:='TCHMHelpDatabase.ShowHelp Node.URLValid=false Node.URL="'+NewNode.URL+'"';
  end;
end;

function TCHMHelpDatabase.ShowURL(const URL, Title: string; var ErrMsg: string
  ): TShowHelpResult;
var
  Viewer: THelpViewer;
  Node: THelpNode;
begin
  //DebugLn('TCHMHelpDatabase.ShowURL A URL="',URL,'" Title="',Title,'"');

  if not FileExistsUTF8(Filename) then
  begin
    ErrMsg:='chm help file "'+Filename+'" not found';
    exit(shrDatabaseNotFound);
  end;

  // find HTML viewer
  Result:=FindViewer(CHMMimeType,ErrMsg,Viewer);
  if Result<>shrSuccess then exit;

  // call viewer
  Node:=nil;
  try
    Node:=THelpNode.CreateURL(Self,Title,URL);
    Result:=Viewer.ShowNode(Node,ErrMsg);
  finally
    Node.Free;
  end;
end;

function TCHMHelpDatabase.GetNodesForKeyword(const HelpKeyword: string;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
var
  Path: String;
begin
  Result:=inherited GetNodesForKeyword(HelpKeyword, ListOfNodes, ErrMsg);
  if Result<>shrSuccess then exit;

  if not (csDesigning in ComponentState) and
    (KeywordPrefix<>'') and
    (LeftStr(HelpKeyword,length(KeywordPrefix))=KeywordPrefix) then
  begin
    // HelpKeyword starts with KeywordPrefix -> add default node
    if FHelpNode=nil then
      FHelpNode:=THelpNode.CreateURL(Self,'','');
    Path:=copy(HelpKeyword,length(KeywordPrefix)+1,length(HelpKeyword));
    FHelpNode.Title:='Show page '+Path+' of '+ExtractFileName(Filename);
    FHelpNode.URL:='chmfile://'+FilenameToURLPath(Filename)+'?'+CHMPathParam+'='+Path;
    CreateNodeQueryListAndAdd(FHelpNode,nil,ListOfNodes,true);
  end;
end;

procedure TCHMHelpDatabase.Load(Storage: TConfigStorage);
begin
  inherited Load(Storage);
  KeywordPrefix:=Storage.GetValue('KeywordPrefix','');
  Filename:=Storage.GetValue('Filename','');
end;

procedure TCHMHelpDatabase.Save(Storage: TConfigStorage);
begin
  inherited Save(Storage);
  Storage.SetDeleteValue('KeywordPrefix',KeywordPrefix,'');
  Storage.SetDeleteValue('Filename',Filename, '');
end;

end.

