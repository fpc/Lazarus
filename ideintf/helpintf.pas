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
    This unit defines various base classes for the Help System used by the IDE.
}
unit HelpIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, FileCtrl, ConfigStorage, ObjInspStrConsts;

type
  // All help-specific error messages should be thrown as this type.
  EHelpSystemException = class(Exception);

  TShowHelpResult = (
    shrNone,
    shrSuccess,
    shrDatabaseNotFound,
    shrContextNotFound,
    shrViewerNotFound,
    shrHelpNotFound,
    shrViewerError,
    shrSelectorError
    );
  TShowHelpResults = set of TShowHelpResult;

  
  { TPascalHelpContextList }

  TPascalHelpContextType = (
    pihcFilename,
    pihcSourceName,  // unit name, library name, ..
    pihcProperty,
    pihcProcedure,
    pihcParameterList,
    pihcVariable,
    pihcType,
    pihcConst,
    pihcIdentifier
    );
  TPascalHelpContext = record
    Descriptor: TPascalHelpContextType;
    Context: string;
  end;
  TPascalHelpContextPtr = ^TPascalHelpContext;
  
  TPascalHelpContextList = class
  private
    FCount: integer;
    fItems: TPascalHelpContextPtr;
    function GetItems(Index: integer): TPascalHelpContext;
  public
    procedure Add(const Context: TPascalHelpContext);
    procedure Insert(Index: integer; const Context: TPascalHelpContext);
    procedure Clear;
    destructor Destroy; override;
    property Count: integer read FCount;
    property Items[Index: integer]: TPascalHelpContext read GetItems;
    property List: TPascalHelpContextPtr read fItems;
  end;
  
  
  THelpDatabase = class;

  { THelpNode
    A help node is a position/place in a help database.
    For example it points to Help file or to a Link on a HTML file. }
  
  THelpNodeType = (
    hntURLIDContext, // URL, ID and Context valid
    hntURL,          // URL valid, ignore ID and Context
    hntURLID,        // URL and ID valid, ignore Context
    hntID,           // ID valid, ignore URL and Context
    hntContext,      // Context valid, ignore URL and ID
    hntURLContext    // URL and Context valid, ignore ID
    );

  THelpNode = class(TPersistent)
  private
    FContext: THelpContext;
    FURL: string;
    FHelpType: THelpNodeType;
    fID: string;
    FOwner: THelpDatabase;
    FTitle: string;
  public
    constructor Create(TheOwner: THelpDatabase; Node: THelpNode);
    constructor Create(TheOwner: THelpDatabase;
                       const TheTitle, TheURL, TheID: string;
                       TheContext: THelpContext);
    constructor CreateURL(TheOwner: THelpDatabase;
                          const TheTitle, TheURL: string);
    constructor CreateID(TheOwner: THelpDatabase; const TheTitle, TheID: string);
    constructor CreateURLID(TheOwner: THelpDatabase; const TheTitle,
                            TheURL, TheID: string);
    constructor CreateContext(TheOwner: THelpDatabase; const TheTitle: string;
                              TheContext: THelpContext);
    constructor CreateURLContext(TheOwner: THelpDatabase;
                                 const TheTitle, TheURL: string;
                                 TheContext: THelpContext);
  public
    property Owner: THelpDatabase read FOwner write FOwner;
    function URLValid: boolean;
    function IDValid: boolean;
    function ContextValid: boolean;
    procedure Assign(Source: TPersistent); override;
  published
    property Title: string read FTitle write FTitle;
    property HelpType: THelpNodeType read FHelpType write FHelpType;
    property URL: string read FURL write FURL;
    property ID: string read fID write fID;
    property Context: THelpContext read FContext write FContext;
  end;


  { THelpDBSearchItem
    Base class for registration search items associated with a THelpDatabase.
    See THelpDBSISourceDirectory for an example.
    Node is optional, pointing to a help page about the help item. }

  THelpDBSearchItem = class(TPersistent)
  private
    FNode: THelpNode;
  public
    constructor Create(TheNode: THelpNode);
    destructor Destroy; override;
  published
    property Node: THelpNode read FNode write FNode;
  end;
  
  
  { THelpDBSISourceFile
    Help registration item for a single source file.
    If Filename is relative, the BasePathObject is used to get a base directory.
    
    For example: If BasePathObject is a TLazPackage the Filename is relative to
    the directory of the .lpk file }

  THelpDBSISourceFile = class(THelpDBSearchItem)
  private
    FBasePathObject: TObject;
    FFilename: string;
    procedure SetFilename(const AValue: string);
  public
    function FileMatches(const AFilename: string): boolean; virtual;
    function GetFullFilename: string; virtual;
    function GetBasePath: string; virtual;
  published
    property BasePathObject: TObject read FBasePathObject write FBasePathObject;
    property Filename: string read FFilename write SetFilename;
  end;
  

  { THelpDBSISourceDirectory
    Help registration item for a source directory.
    As THelpDBSISourceFile, except that Filename is a directory and
    the item is valid for all source files fitting the FileMask.
    FileMask can be for example '*.pp;*.pas;*.inc'

    For example: A package providing help for all its source files registers
    a THelpDBSISourceDirectory. Node points to the fpdoc main page.
    }

  THelpDBSISourceDirectory = class(THelpDBSISourceFile)
  private
    FFileMask: string;
    FWithSubDirectories: boolean;
  public
    function FileMatches(const AFilename: string): boolean; override;
  published
    property FileMask: string read FFileMask write FFileMask;
    property WithSubDirectories: boolean read FWithSubDirectories
                                         write FWithSubDirectories;
  end;
  

  { THelpDBSIClass
    Help registration item for a class.
    Used by the IDE to search for help for a class without source.
    For example for a registered component class in the component palette, that
    comes without source. If the component comes with source use the
    THelpDBSISourceDirectory or THelpDBSISourceFile instead. }

  THelpDBSIClass = class(THelpDBSearchItem)
  private
    FTheClass: TClass;
  public
    property TheClass: TClass read FTheClass write FTheClass;
  end;


  { THelpDatabase
    Base class for a collection of help files or entries.
    BasePathObject: THelpDatabase can be created by packages.
                    The IDE will set BasePathObject accordingly. }

  THelpDatabaseID = string;
  THelpDatabases = class;
  THelpViewer = class;

  THelpDatabase = class(TPersistent)
  private
    FBasePathObject: TObject;
    FID: THelpDatabaseID;
    FDatabases: THelpDatabases;
    FRefCount: integer;
    FSearchItems: TList;
    FSupportedMimeTypes: TStrings;
    FTOCNode: THelpNode;
    procedure SetID(const AValue: THelpDatabaseID);
    procedure SetDatabases(const AValue: THelpDatabases);
  protected
    procedure SetSupportedMimeTypes(List: TStrings); virtual;
    procedure AddSupportedMimeType(const AMimeType: string); virtual;
  public
    constructor Create(TheID: THelpDatabaseID); virtual;
    destructor Destroy; override;
    procedure Reference;
    procedure RegisterSelf;
    procedure Release;
    procedure UnregisterSelf;
    function Registered: boolean;
    function CanShowTableOfContents: boolean; virtual;
    procedure ShowTableOfContents; virtual;
    procedure ShowError(ShowResult: TShowHelpResult; const ErrMsg: string); virtual;
    function ShowHelp(BaseNode, NewNode: THelpNode;
                      var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpFile(BaseNode: THelpNode; const Title, Filename: string;
                          var ErrMsg: string): TShowHelpResult; virtual;
    function SupportsMimeType(const AMimeType: string): boolean; virtual;
    function GetNodesForKeyword(const HelpKeyword: string;
                                var ListOfNodes: TList; var ErrMsg: string
                                ): TShowHelpResult; virtual;
    function GetNodesForContext(HelpContext: THelpContext;
                                var ListOfNodes: TList; var ErrMsg: string
                                ): TShowHelpResult; virtual;
    function GetNodesForPascalContexts(ListOfPascalHelpContextList: TList;
                                       var ListOfNodes: TList;
                                       var ErrMsg: string): TShowHelpResult; virtual;
    function FindViewer(const MimeType: string; var ErrMsg: string;
                        var Viewer: THelpViewer): TShowHelpResult; virtual;
  public
    // registration
    procedure RegisterItem(NewItem: THelpDBSearchItem);
    procedure RegisterItemWithNode(Node: THelpNode);
    procedure UnregisterItem(AnItem: THelpDBSearchItem);
    function RegisteredItemCount: integer;
    function GetRegisteredItem(Index: integer): THelpDBSearchItem;
  public
    property Databases: THelpDatabases read FDatabases write SetDatabases;
    property ID: THelpDatabaseID read FID write SetID;
    property SupportedMimeTypes: TStrings read FSupportedMimeTypes;
    property BasePathObject: TObject read FBasePathObject write FBasePathObject;
    property TOCNode: THelpNode read FTOCNode write FTOCNode;
  end;

  THelpDatabaseClass = class of THelpDatabase;
  

  { THelpDatabases
    Class for storing all registered THelpDatabase }
  THelpDatabases = class
  private
    FItems: TList;
    FHelpDBClasses: TList;
    function GetItems(Index: integer): THelpDatabase;
    procedure DoRegisterDatabase(ADatabase: THelpDatabase);
    procedure DoUnregisterDatabase(ADatabase: THelpDatabase);
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    property Items[Index: integer]: THelpDatabase read GetItems; default;
  public
    function FindDatabase(ID: THelpDatabaseID): THelpDatabase;
    function GetDatabase(ID: THelpDatabaseID; HelpDB: THelpDatabase;
                         var HelpResult: TShowHelpResult; var ErrMsg: string): boolean;
    function IndexOf(ID: THelpDatabaseID): integer;
    function CreateUniqueDatabaseID(const WishID: string): THelpDatabaseID;
    function CreateHelpDatabase(const WishID: string;
                                HelpDataBaseClass: THelpDatabaseClass;
                                AutoRegister: boolean): THelpDatabase;
    function ShowTableOfContents(var ErrMsg: string): TShowHelpResult;
    procedure ShowError(ShowResult: TShowHelpResult; const ErrMsg: string); virtual; abstract;
    function GetBaseURLForBasePathObject(BasePathObject: TObject): string; virtual;
    function GetBaseDirectoryForBasePathObject(BasePathObject: TObject): string; virtual;
  public
    // show help for ...
    function ShowHelpForNodes(Nodes: TList; var ErrMsg: string): TShowHelpResult;
    function ShowHelpForContext(HelpDatabaseID: THelpDatabaseID;
                                HelpContext: THelpContext;
                                var ErrMsg: string): TShowHelpResult;
    function ShowHelpForKeyword(HelpDatabaseID: THelpDatabaseID;
                                const HelpKeyword: string;
                                var ErrMsg: string): TShowHelpResult;
    function ShowHelpForPascalContexts(ListOfPascalHelpContextList: TList;
                                       var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpForSourcePosition(const Filename: string;
                                       const CodePos: TPoint;
                                       var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpForMessageLine(const MessageLine: string;
                                    var ErrMsg: string): TShowHelpResult;
    function ShowHelpForClass(const AClass: TClass;
                              var ErrMsg: string): TShowHelpResult;
    function GetNodesForKeyword(const HelpKeyword: string;
                                var ListOfNodes: TList; var ErrMsg: string
                                ): TShowHelpResult; virtual;
    function GetNodesForContext(HelpContext: THelpContext;
                                var ListOfNodes: TList; var ErrMsg: string
                                ): TShowHelpResult; virtual;
    function GetNodesForPascalContexts(ListOfPascalHelpContextList: TList;
                                       var ListOfNodes: TList;
                                       var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpSelector(Nodes: TList; var ErrMsg: string;
                              var Selection: THelpNode): TShowHelpResult; virtual;
  public
    // registration of THelpDatabaseClass
    procedure RegisterHelpDatabaseClass(NewHelpDB: THelpDatabaseClass);
    procedure UnregisterHelpDatabaseClass(AHelpDB: THelpDatabaseClass);
    function HelpDatabaseClassCount: integer;
    function GetHelpDatabaseClass(Index: integer): THelpDatabaseClass;
  end;
  
  
  { THelpViewer
    base class for all Help viewers }
  
  THelpViewer = class(TPersistent)
  private
    FParameterHelp: string;
    FStorageName: string;
    FSupportedMimeTypes: TStrings;
  protected
    procedure SetSupportedMimeTypes(List: TStrings); virtual;
    procedure AddSupportedMimeType(const AMimeType: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function SupportsTableOfContents: boolean; virtual;
    procedure ShowTableOfContents(Node: THelpNode); virtual;
    function SupportsMimeType(const AMimeType: string): boolean; virtual;
    function ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult; virtual;
    procedure Hide; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Load(Storage: TConfigStorage); virtual;
    procedure Save(Storage: TConfigStorage); virtual;
    function GetLocalizedName: string; virtual;
  public
    property SupportedMimeTypes: TStrings read FSupportedMimeTypes;
    property ParameterHelp: string read FParameterHelp write FParameterHelp;
    property StorageName: string read FStorageName write FStorageName;
  end;

  THelpViewerClass = class of THelpViewer;
  
  
  { THelpViewers }
  
  THelpViewers = class
  private
    FItems: TList;
    function GetItems(Index: integer): THelpViewer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function GetViewersSupportingMimeType(const MimeType: string): TList;
    procedure RegisterViewer(AHelpViewer: THelpViewer);
    procedure UnregisterViewer(AHelpViewer: THelpViewer);
    procedure Load(Storage: TConfigStorage); virtual;
    procedure Save(Storage: TConfigStorage); virtual;
  public
    property Items[Index: integer]: THelpViewer read GetItems; default;
  end;
  
  
var
  HelpDatabases: THelpDatabases; // initialized by the IDE
  HelpViewers: THelpViewers; // initialized by the IDE

//==============================================================================
{ Showing help (how it works):

  - starts the help system, if not already started
  - search all appropriate help Databases for the given context
    If multiple contexts fit, a help selector is shown and the user chooses one.
  - calls the help Database to show the context
    The help Database will search an appropriate help viewer and starts it.
}

// table of contents
function ShowTableOfContents: TShowHelpResult;
function ShowTableOfContents(var ErrMsg: string): TShowHelpResult;

// help by ID
function ShowHelpOrErrorForContext(HelpDatabaseID: THelpDatabaseID;
  HelpContext: THelpContext): TShowHelpResult;
function ShowHelpForContext(HelpDatabaseID: THelpDatabaseID;
  HelpContext: THelpContext; var ErrMsg: string): TShowHelpResult;
function ShowHelpForContext(HelpContext: THelpContext; var ErrMsg: string
  ): TShowHelpResult;

// help by keyword
function ShowHelpOrErrorForKeyword(HelpDatabaseID: THelpDatabaseID;
  const HelpKeyword: string): TShowHelpResult;
function ShowHelpForKeyword(HelpDatabaseID: THelpDatabaseID;
  const HelpKeyword: string; var ErrMsg: string): TShowHelpResult;
function ShowHelpForKeyword(const HelpKeyword: string; var ErrMsg: string
  ): TShowHelpResult;

// help for pascal identifier
function ShowHelpForPascalContexts(ListOfPascalHelpContextList: TList;
  var ErrMsg: string): TShowHelpResult;
function ShowHelpForPascalContext(ContextList: TPascalHelpContextList;
  var ErrMsg: string): TShowHelpResult;
function ShowHelpOrErrorForSourcePosition(const Filename: string;
  const CodePos: TPoint): TShowHelpResult;

// help for messages (compiler messages, codetools messages, make messages, ...)
function ShowHelpForMessageLine(const MessageLine: string;
  var ErrMsg: string): TShowHelpResult;

// URL functions
function FilenameToURL(const Filename: string): string;
procedure SplitURL(const URL: string; var URLType, URLPath, URLParams: string);
function CombineURL(const URLType, URLPath, URLParams: string): string;
function URLFilenameIsAbsolute(const Filename: string): boolean;

implementation

function ShowTableOfContents: TShowHelpResult;
var
  ErrMsg: String;
begin
  ErrMsg:='';
  Result:=ShowTableOfContents(ErrMsg);
  HelpDatabases.ShowError(Result,ErrMsg);
end;

function ShowTableOfContents(var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpDatabases.ShowTableOfContents(ErrMsg);
end;

function ShowHelpOrErrorForContext(HelpDatabaseID: THelpDatabaseID;
  HelpContext: THelpContext): TShowHelpResult;
var
  ErrMsg: String;
begin
  ErrMsg:='';
  Result:=ShowHelpForContext(HelpDatabaseID,HelpContext,ErrMsg);
  HelpDatabases.ShowError(Result,ErrMsg);
end;

function ShowHelpForContext(HelpDatabaseID: THelpDatabaseID;
  HelpContext: THelpContext; var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpDatabases.ShowHelpForContext(HelpDatabaseID,HelpContext,ErrMsg);
end;

function ShowHelpForContext(HelpContext: THelpContext; var ErrMsg: string
  ): TShowHelpResult;
begin
  Result:=ShowHelpForContext('',HelpContext,ErrMsg);
end;

function ShowHelpOrErrorForKeyword(HelpDatabaseID: THelpDatabaseID;
  const HelpKeyword: string): TShowHelpResult;
var
  ErrMsg: String;
begin
  ErrMsg:='';
  Result:=ShowHelpForKeyword(HelpDatabaseID,HelpKeyword,ErrMsg);
  HelpDatabases.ShowError(Result,ErrMsg);
end;

function ShowHelpForKeyword(HelpDatabaseID: THelpDatabaseID;
  const HelpKeyword: string; var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpDatabases.ShowHelpForKeyword(HelpDatabaseID,HelpKeyword,ErrMsg);
end;

function ShowHelpForKeyword(const HelpKeyword: string; var ErrMsg: string
  ): TShowHelpResult;
begin
  Result:=ShowHelpForKeyword('',HelpKeyword,ErrMsg);
end;

function ShowHelpForPascalContexts(ListOfPascalHelpContextList: TList;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpDatabases.ShowHelpForPascalContexts(ListOfPascalHelpContextList,
                                                  ErrMsg);
end;

function ShowHelpForPascalContext(ContextList: TPascalHelpContextList;
  var ErrMsg: string): TShowHelpResult;
var
  List: TList;
begin
  List:=TList.Create;
  try
    List.Add(ContextList);
    Result:=ShowHelpForPascalContexts(List,ErrMsg);
  finally
    List.Free;
  end;
end;

function ShowHelpOrErrorForSourcePosition(const Filename: string;
  const CodePos: TPoint): TShowHelpResult;
var
  ErrMsg: String;
begin
  ErrMsg:='';
  Result:=HelpDatabases.ShowHelpForSourcePosition(Filename,CodePos,ErrMsg);
  HelpDatabases.ShowError(Result,ErrMsg);
end;

function ShowHelpForMessageLine(const MessageLine: string;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpDatabases.ShowHelpForMessageLine(MessageLine,ErrMsg);
end;

function FilenameToURL(const Filename: string): string;
var
  i: Integer;
begin
  Result:=Filename;
  if PathDelim<>'/' then
    for i:=1 to length(Result) do
      if Result[i]=PathDelim then
        Result[i]:='/';
  if Result<>'' then
    Result:='file://'+Result;
end;

procedure SplitURL(const URL: string; var URLType, URLPath, URLParams: string);
var
  Len: Integer;
  ColonPos: Integer;
  ParamStartPos: integer;
  URLStartPos: Integer;
begin
  URLType:='';
  URLPath:='';
  URLParams:='';
  Len:=length(URL);
  // search colon
  ColonPos:=1;
  while (ColonPos<=len) and (URL[ColonPos]<>':') do
    inc(ColonPos);
  if ColonPos=len then exit;
  // get URLType
  URLType:=copy(URL,1,ColonPos-1);
  URLStartPos:=ColonPos+1;
  // skip the '//' after the colon
  if (URLStartPos<=len) and (URL[URLStartPos]='/') then inc(URLStartPos);
  if (URLStartPos<=len) and (URL[URLStartPos]='/') then inc(URLStartPos);
  // search param delimiter ?
  ParamStartPos:=ColonPos+1;
  while (ParamStartPos<=len) and (URL[ParamStartPos]<>'?') do
    inc(ParamStartPos);
  // get URLPath and URLParams
  URLPath:=copy(URL,URLStartPos,ParamStartPos-URLStartPos);
  URLParams:=copy(URL,ParamStartPos+1,len-ParamStartPos);
end;

function CombineURL(const URLType, URLPath, URLParams: string): string;
begin
  Result:=URLType+'://'+URLPath;
  if URLParams<>'' then
    Result:=Result+'?'+URLParams;
end;

function URLFilenameIsAbsolute(const Filename: string): boolean;
begin
  if PathDelim='/' then
    Result:=FilenameIsAbsolute(Filename)
  else
    Result:=FilenameIsAbsolute(SetDirSeparators(Filename));
end;

procedure CreateListAndAdd(const AnObject: TObject; var List: TList);
begin
  if List=nil then List:=TList.Create;
  List.Add(AnObject);
end;


{ THelpDatabase }

procedure THelpDatabase.SetID(const AValue: THelpDatabaseID);
var
  OldRegistered: Boolean;
begin
  if FID=AValue then exit;
  OldRegistered:=Registered;
  if OldRegistered then UnregisterSelf;
  FID:=AValue;
  if OldRegistered then RegisterSelf;
end;

procedure THelpDatabase.SetDatabases(const AValue: THelpDatabases);
begin
  if AValue=Databases then exit;
  Reference;
  if FDatabases<>nil then FDatabases.DoUnregisterDatabase(Self);
  FDatabases:=AValue;
  if FDatabases<>nil then FDatabases.DoRegisterDatabase(Self);
  Release;
end;

procedure THelpDatabase.SetSupportedMimeTypes(List: TStrings);
begin
  FSupportedMimeTypes.Free;
  FSupportedMimeTypes:=List;
end;

procedure THelpDatabase.AddSupportedMimeType(const AMimeType: string);
begin
  if FSupportedMimeTypes=nil then SetSupportedMimeTypes(TStringList.Create);
  FSupportedMimeTypes.Add(AMimeType);
end;

constructor THelpDatabase.Create(TheID: THelpDatabaseID);
begin
  FID:=TheID;
end;

destructor THelpDatabase.Destroy;
var
  i: Integer;
begin
  Reference; // reference to not call Free again
  if Databases<>nil then UnregisterSelf;
  FSupportedMimeTypes.Free;
  if FSearchItems<>nil then begin
    for i:=FSearchItems.Count-1 downto 0 do
      THelpNode(FSearchItems[i]).Free;
    FSearchItems.Free;
  end;
  FTOCNode.Free;
  inherited Destroy;
end;

procedure THelpDatabase.RegisterSelf;
begin
  if Databases<>nil then
    raise EHelpSystemException.Create(Format(oisHelpAlreadyRegistered, [ID]));
  Databases:=HelpDatabases;
end;

procedure THelpDatabase.UnregisterSelf;
begin
  if Databases=nil then
    raise EHelpSystemException.Create(Format(oisHelpNotRegistered, [ID]));
  Databases:=nil;
end;

function THelpDatabase.Registered: boolean;
begin
  Result:=Databases<>nil;
end;

function THelpDatabase.CanShowTableOfContents: boolean;
begin
  Result:=TOCNode<>nil;
end;

procedure THelpDatabase.ShowTableOfContents;
var
  ErrMsg: string;
  ShowResult: TShowHelpResult;
begin
  if TOCNode=nil then exit;
  ErrMsg:='';
  ShowResult:=ShowHelp(nil,TOCNode,ErrMsg);
  ShowError(ShowResult,ErrMsg);
end;

procedure THelpDatabase.ShowError(ShowResult: TShowHelpResult;
  const ErrMsg: string);
begin
  if ShowResult=shrSuccess then exit;
  if Databases<>nil then
    Databases.ShowError(ShowResult,ErrMsg)
  else
    raise EHelpSystemException.Create(ErrMsg);
end;

function THelpDatabase.ShowHelp(BaseNode, NewNode: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
begin
  ErrMsg:='';
  Result:=shrContextNotFound;
end;

function THelpDatabase.ShowHelpFile(BaseNode: THelpNode;
  const Title, Filename: string; var ErrMsg: string): TShowHelpResult;
var
  FileNode: THelpNode;
begin
  FileNode:=THelpNode.CreateURL(Self,Title,FilenameToURL(Filename));
  try
    Result:=ShowHelp(BaseNode,FileNode,ErrMsg);
  finally
    FileNode.Free;
  end;
end;

function THelpDatabase.SupportsMimeType(const AMimeType: string): boolean;
begin
  Result:=false;
  if FSupportedMimeTypes<>nil then
    Result:=(FSupportedMimeTypes.IndexOf(AMimeType)>=0);
end;

function THelpDatabase.GetNodesForKeyword(const HelpKeyword: string;
  var ListOfNodes: TList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
  Node: THelpNode;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  // add the registered nodes
  if FSearchItems<>nil then begin
    for i:=0 to FSearchItems.Count-1 do begin
      Node:=THelpDBSearchItem(FSearchItems[i]).Node;
      if (Node=nil) or (not Node.IDValid) then continue;
      if AnsiCompareText(Node.ID,HelpKeyword)<>0 then continue;
      CreateListAndAdd(Node,ListOfNodes);
    end;
  end;
end;

function THelpDatabase.GetNodesForContext(HelpContext: THelpContext;
  var ListOfNodes: TList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
  Node: THelpNode;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  // add the registered nodes
  if FSearchItems<>nil then begin
    for i:=0 to FSearchItems.Count-1 do begin
      Node:=THelpDBSearchItem(FSearchItems[i]).Node;
      if (Node=nil) or (not Node.ContextValid) then continue;
      if Node.Context<>HelpContext then continue;
      CreateListAndAdd(Node,ListOfNodes);
    end;
  end;
end;

function THelpDatabase.GetNodesForPascalContexts(
  ListOfPascalHelpContextList: TList; var ListOfNodes: TList; var ErrMsg: string
  ): TShowHelpResult;
// if ListOfNodes<>nil new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
  j: Integer;
  SearchItem: THelpDBSearchItem;
  PascalContext: TPascalHelpContextList;
  FileItem: THelpDBSISourceFile;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  if (ListOfPascalHelpContextList=nil)
  or (ListOfPascalHelpContextList.Count=0) then exit;
  // add the registered nodes
  if FSearchItems<>nil then begin
    for i:=0 to FSearchItems.Count-1 do begin
      SearchItem:=THelpDBSearchItem(FSearchItems[i]);
      if not (SearchItem is THelpDBSISourceFile) then continue;
      FileItem:=THelpDBSISourceFile(SearchItem);
      // check every pascal context
      for j:=0 to ListOfPascalHelpContextList.Count-1 do begin
        PascalContext:=TPascalHelpContextList(ListOfPascalHelpContextList[j]);
        if (PascalContext.List[0].Descriptor=pihcFilename)
        and (FileItem.FileMatches(PascalContext.List[0].Context)) then
          CreateListAndAdd(FileItem.Node,ListOfNodes);
      end;
    end;
  end;
end;

function THelpDatabase.FindViewer(const MimeType: string; var ErrMsg: string;
  var Viewer: THelpViewer): TShowHelpResult;
var
  Viewers: TList;
begin
  Viewer:=nil;
  Viewers:=HelpViewers.GetViewersSupportingMimeType(MimeType);
  try
    if (Viewers=nil) or (Viewers.Count=0) then begin
      ErrMsg:=Format(oisHelpHelpDatabaseDidNotFoundAViewerForAHelpPageOfType, [
        '"', ID, '"', MimeType]);
      Result:=shrViewerNotFound;
    end else begin
      Viewer:=THelpViewer(Viewers[0]);
      Result:=shrSuccess;
    end;
  finally
    Viewers.Free;
  end;
end;

procedure THelpDatabase.RegisterItem(NewItem: THelpDBSearchItem);
begin
  if NewItem=nil then
    raise EHelpSystemException.Create('THelpDatabase.RegisterItem NewItem=nil');
  if FSearchItems=nil then FSearchItems:=TList.Create;
  if FSearchItems.IndexOf(NewItem)<0 then
    FSearchItems.Add(NewItem)
  else
    NewItem.Free;
end;

procedure THelpDatabase.RegisterItemWithNode(Node: THelpNode);
begin
  if Node=nil then
    raise EHelpSystemException.Create('THelpDatabase.RegisterItemWithNode Node=nil');
  RegisterItem(THelpDBSearchItem.Create(Node));
end;

procedure THelpDatabase.UnregisterItem(AnItem: THelpDBSearchItem);
begin
  if FSearchItems=nil then exit;
  FSearchItems.Remove(AnItem);
end;

function THelpDatabase.RegisteredItemCount: integer;
begin
  if FSearchItems=nil then
    Result:=0
  else
    Result:=FSearchItems.Count;
end;

function THelpDatabase.GetRegisteredItem(Index: integer): THelpDBSearchItem;
begin
  Result:=THelpDBSearchItem(FSearchItems[Index]);
end;

procedure THelpDatabase.Reference;
begin
  inc(FRefCount);
end;

procedure THelpDatabase.Release;
begin
  if FRefCount=0 then
    raise EHelpSystemException.Create('THelpDatabase.Release');
  dec(FRefCount);
  if FRefCount=0 then Free;
end;

{ THelpDatabases }

function THelpDatabases.GetItems(Index: integer): THelpDatabase;
begin
  Result:=THelpDatabase(FItems[Index]);
end;

procedure THelpDatabases.DoRegisterDatabase(ADatabase: THelpDatabase);
begin
  ADatabase.Reference;
  if FItems=nil then FItems:=TList.Create;
  FItems.Add(ADatabase);
end;

procedure THelpDatabases.DoUnregisterDatabase(ADatabase: THelpDatabase);
begin
  if FItems<>nil then
    FItems.Remove(ADatabase);
  ADatabase.Release;
end;

constructor THelpDatabases.Create;
begin

end;

destructor THelpDatabases.Destroy;
begin
  while (Count>0) do Items[Count-1].UnregisterSelf;
  FItems.Free;
  FHelpDBClasses.Free;
  inherited Destroy;
end;

function THelpDatabases.Count: integer;
begin
  if FItems=nil then
    Result:=0
  else
    Result:=FItems.Count;
end;

function THelpDatabases.FindDatabase(ID: THelpDatabaseID): THelpDatabase;
var
  Index: LongInt;
begin
  Index:=IndexOf(ID);
  if Index>=0 then
    Result:=Items[Index]
  else
    Result:=nil;
end;

function THelpDatabases.GetDatabase(ID: THelpDatabaseID; HelpDB: THelpDatabase;
  var HelpResult: TShowHelpResult; var ErrMsg: string): boolean;
begin
  HelpDB:=FindDatabase(ID);
  if HelpDB=nil then begin
    Result:=false;
    HelpResult:=shrDatabaseNotFound;
    ErrMsg:=Format(oisHelpHelpDatabaseNotFound, ['"', ID, '"']);
  end else begin
    HelpResult:=shrSuccess;
    Result:=true;
    ErrMsg:='';
  end;
end;

function THelpDatabases.IndexOf(ID: THelpDatabaseID): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(ID,Items[Result].ID)<>0) do
    dec(Result);
end;

function THelpDatabases.CreateUniqueDatabaseID(
  const WishID: string): THelpDatabaseID;
var
  i: Integer;
begin
  if (WishID<>'') and (FindDatabase(WishID)=nil) then begin
    Result:=WishID;
  end else begin
    i:=1;
    repeat
      Result:=WishID+IntToStr(i);
      if FindDatabase(Result)=nil then exit;
      inc(i);
    until false;
  end;
end;

function THelpDatabases.CreateHelpDatabase(const WishID: string;
  HelpDataBaseClass: THelpDatabaseClass; AutoRegister: boolean): THelpDatabase;
begin
  Result:=HelpDataBaseClass.Create(CreateUniqueDatabaseID(WishID));
  if AutoRegister then Result.RegisterSelf;
end;

function THelpDatabases.ShowTableOfContents(var ErrMsg: string
  ): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='THelpDatabases.ShowTableOfContents not implemented';
  // ToDo
end;

function THelpDatabases.GetBaseURLForBasePathObject(BasePathObject: TObject
  ): string;
begin
  Result:=GetBaseDirectoryForBasePathObject(BasePathObject);
  if Result='' then exit;
  Result:=FilenameToURL(Result);
end;

function THelpDatabases.GetBaseDirectoryForBasePathObject(BasePathObject: TObject
  ): string;
begin
  Result:='';
end;

function THelpDatabases.ShowHelpForNodes(Nodes: TList; var ErrMsg: string
  ): TShowHelpResult;
var
  Node: THelpNode;
begin
  // check if several nodes found
  if (Nodes.Count>1) then begin
    Node:=nil;
    Result:=ShowHelpSelector(Nodes,ErrMsg,Node);
    if Result<>shrSuccess then exit;
    if Node=nil then exit;
  end else begin
    Node:=THelpNode(Nodes[0]);
  end;

  // show node
  if Node.Owner=nil then begin
    Result:=shrDatabaseNotFound;
    ErrMsg:=Format(oisHelpHelpNodeHasNoHelpDatabase, ['"', Node.Title, '"']);
    exit;
  end;
  Result:=Node.Owner.ShowHelp(nil,Node,ErrMsg);
end;

function THelpDatabases.ShowHelpForContext(HelpDatabaseID: THelpDatabaseID;
  HelpContext: THelpContext; var ErrMsg: string): TShowHelpResult;
var
  Nodes: TList;
  HelpDB: THelpDatabase;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  
  // search node
  Nodes:=nil;
  try
    if HelpDatabaseID<>'' then begin
      HelpDB:=nil;
      if not GetDatabase(HelpDatabaseID,HelpDB,Result,ErrMsg) then exit;
      Result:=HelpDB.GetNodesForContext(HelpContext,Nodes,ErrMsg);
      if Result<>shrSuccess then exit;
    end else begin
      Result:=GetNodesForContext(HelpContext,Nodes,ErrMsg);
      if Result<>shrSuccess then exit;
    end;

    // check if at least one node found
    if (Nodes<>nil) then Nodes.Pack;
    if (Nodes=nil) or (Nodes.Count=0) then begin
      Result:=shrContextNotFound;
      if HelpDatabaseID<>'' then
        ErrMsg:=Format(oisHelpHelpContextNotFoundInDatabase, [IntToStr(
          HelpContext), '"', HelpDatabaseID, '"'])
      else
        ErrMsg:=Format(oisHelpHelpContextNotFound, [IntToStr(HelpContext)]);
      exit;
    end;

    Result:=ShowHelpForNodes(Nodes,ErrMsg);
  finally
    Nodes.Free;
  end;
end;

function THelpDatabases.ShowHelpForKeyword(HelpDatabaseID: THelpDatabaseID;
  const HelpKeyword: string; var ErrMsg: string): TShowHelpResult;
var
  Nodes: TList;
  HelpDB: THelpDatabase;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;

  // search node
  Nodes:=nil;
  try
    if HelpDatabaseID<>'' then begin
      HelpDB:=nil;
      if not GetDatabase(HelpDatabaseID,HelpDB,Result,ErrMsg) then exit;
      Result:=HelpDB.GetNodesForKeyword(HelpKeyword,Nodes,ErrMsg);
      if Result<>shrSuccess then exit;
    end else begin
      Result:=GetNodesForKeyword(HelpKeyword,Nodes,ErrMsg);
      if Result<>shrSuccess then exit;
    end;

    // check if at least one node found
    if (Nodes<>nil) then Nodes.Pack;
    if (Nodes=nil) or (Nodes.Count=0) then begin
      Result:=shrContextNotFound;
      if HelpDatabaseID<>'' then
        ErrMsg:=Format(oisHelpHelpKeywordNotFoundInDatabase, ['"', HelpKeyword,
          '"', '"', HelpDatabaseID, '"'])
      else
        ErrMsg:=Format(oisHelpHelpKeywordNotFound, ['"', HelpKeyword, '"']);
      exit;
    end;

    Result:=ShowHelpForNodes(Nodes,ErrMsg);
  finally
    Nodes.Free;
  end;
end;

function THelpDatabases.ShowHelpForPascalContexts(
  ListOfPascalHelpContextList: TList; var ErrMsg: string): TShowHelpResult;
var
  Nodes: TList;
begin
  ErrMsg:='';
  Result:=shrSuccess;

  // search node
  Nodes:=nil;
  try
    Result:=GetNodesForPascalContexts(ListOfPascalHelpContextList,Nodes,ErrMsg);
    if Result<>shrSuccess then exit;

    // check if at least one node found
    if (Nodes<>nil) then Nodes.Pack;
    if (Nodes=nil) or (Nodes.Count=0) then begin
      // no node found for the source is not a bug
      Result:=shrSuccess;
      ErrMsg:='';
      exit;
    end;

    Result:=ShowHelpForNodes(Nodes,ErrMsg);
  finally
    Nodes.Free;
  end;
end;

function THelpDatabases.ShowHelpForSourcePosition(const Filename: string;
  const CodePos: TPoint; var ErrMsg: string): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='THelpDatabases.ShowHelpForPascalSource not implemented';
end;

function THelpDatabases.ShowHelpForMessageLine(const MessageLine: string;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='THelpDatabases.ShowHelpForMessageLine not implemented';
end;

function THelpDatabases.ShowHelpForClass(const AClass: TClass;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='THelpDatabases.ShowHelpForClass not implemented';
end;

function THelpDatabases.GetNodesForKeyword(const HelpKeyword: string;
  var ListOfNodes: TList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil then new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  for i:=Count-1 downto 0 do begin
    Result:=Items[i].GetNodesForKeyword(HelpKeyword,ListOfNodes,ErrMsg);
    if Result<>shrSuccess then exit;
  end;
end;

function THelpDatabases.GetNodesForContext(HelpContext: THelpContext;
  var ListOfNodes: TList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil then new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  for i:=Count-1 downto 0 do begin
    Result:=Items[i].GetNodesForContext(HelpContext,ListOfNodes,ErrMsg);
    if Result<>shrSuccess then exit;
  end;
end;

function THelpDatabases.GetNodesForPascalContexts(
  ListOfPascalHelpContextList: TList; var ListOfNodes: TList;
  var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil then new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  for i:=Count-1 downto 0 do begin
    Result:=Items[i].GetNodesForPascalContexts(ListOfPascalHelpContextList,
                                               ListOfNodes,ErrMsg);
    if Result<>shrSuccess then exit;
  end;
end;

function THelpDatabases.ShowHelpSelector(Nodes: TList; var ErrMsg: string;
  var Selection: THelpNode): TShowHelpResult;
// Nodes is a list of THelpNode
begin
  Result:=shrSelectorError;
  // TODO
  ErrMsg:='THelpDatabases.ShowHelpSelector not implemented';
end;

procedure THelpDatabases.RegisterHelpDatabaseClass(NewHelpDB: THelpDatabaseClass
  );
begin
  if FHelpDBClasses=nil then FHelpDBClasses:=TList.Create;
  if FHelpDBClasses.IndexOf(NewHelpDB)<0 then
    FHelpDBClasses.Add(NewHelpDB);
end;

procedure THelpDatabases.UnregisterHelpDatabaseClass(
  AHelpDB: THelpDatabaseClass);
begin
  if FHelpDBClasses=nil then exit;
  FHelpDBClasses.Remove(AHelpDB);
end;

function THelpDatabases.HelpDatabaseClassCount: integer;
begin
  if FHelpDBClasses=nil then
    Result:=0
  else
    Result:=FHelpDBClasses.Count;
end;

function THelpDatabases.GetHelpDatabaseClass(Index: integer
  ): THelpDatabaseClass;
begin
  Result:=THelpDatabaseClass(FHelpDBClasses[Index]);
end;

{ THelpViewers }

function THelpViewers.GetItems(Index: integer): THelpViewer;
begin
  Result:=THelpViewer(FItems[Index]);
end;

constructor THelpViewers.Create;
begin
  FItems:=TList.Create;
end;

destructor THelpViewers.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure THelpViewers.Clear;
var
  i: Integer;
begin
  for i:=0 to Count-1 do Items[Count-1].Free;
  FItems.Clear;
end;

function THelpViewers.Count: integer;
begin
  Result:=FItems.Count;
end;

function THelpViewers.GetViewersSupportingMimeType(
  const MimeType: string): TList;
var
  i: Integer;
begin
  Result:=nil;
  // LIFO: last registered, first shown
  for i:=Count-1 downto 0 do
    if Items[i].SupportsMimeType(MimeType) then begin
      if Result=nil then Result:=TList.Create;
      Result.Add(Items[i]);
    end;
end;

procedure THelpViewers.RegisterViewer(AHelpViewer: THelpViewer);
begin
  FItems.Add(AHelpViewer);
end;

procedure THelpViewers.UnregisterViewer(AHelpViewer: THelpViewer);
begin
  FItems.Remove(AHelpViewer);
end;

procedure THelpViewers.Load(Storage: TConfigStorage);
var
  i: Integer;
  Viewer: THelpViewer;
begin
  for i:=0 to Count-1 do begin
    Viewer:=Items[i];
    Storage.AppendBasePath(Viewer.StorageName);
    try
      Viewer.Load(Storage);
    finally
      Storage.UndoAppendBasePath;
    end;
  end;
end;

procedure THelpViewers.Save(Storage: TConfigStorage);
var
  i: Integer;
  Viewer: THelpViewer;
begin
  for i:=0 to Count-1 do begin
    Viewer:=Items[i];
    Storage.AppendBasePath(Viewer.StorageName);
    try
      Viewer.Save(Storage);
    finally
      Storage.UndoAppendBasePath;
    end;
  end;
end;

{ THelpViewer }

procedure THelpViewer.SetSupportedMimeTypes(List: TStrings);
begin
  if FSupportedMimeTypes<>nil then FSupportedMimeTypes.Free;
  FSupportedMimeTypes:=nil;
end;

procedure THelpViewer.AddSupportedMimeType(const AMimeType: string);
begin
  if FSupportedMimeTypes=nil then FSupportedMimeTypes:=TStringList.Create;
  FSupportedMimeTypes.Add(AMimeType);
end;

constructor THelpViewer.Create;
begin
  FStorageName:=ClassName;
end;

destructor THelpViewer.Destroy;
begin
  FSupportedMimeTypes.Free;
  inherited Destroy;
end;

function THelpViewer.SupportsTableOfContents: boolean;
begin
  Result:=false;
end;

procedure THelpViewer.ShowTableOfContents(Node: THelpNode);
begin
  raise EHelpSystemException.Create('THelpViewer.ShowTableOfContents not implemented');
end;

function THelpViewer.SupportsMimeType(const AMimeType: string): boolean;
begin
  Result:=false;
  if FSupportedMimeTypes<>nil then
    Result:=(FSupportedMimeTypes.IndexOf(AMimeType)>=0);
end;

function THelpViewer.ShowNode(Node: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
begin
  // for descendents to override
  Result:=shrViewerError;
  ErrMsg:='THelpViewer.ShowNode not implemented for this help type';
end;

procedure THelpViewer.Hide;
begin
  // override this
end;

procedure THelpViewer.Assign(Source: TPersistent);
begin
  if Source is THelpViewer then begin

  end;
  inherited Assign(Source);
end;

procedure THelpViewer.Load(Storage: TConfigStorage);
begin

end;

procedure THelpViewer.Save(Storage: TConfigStorage);
begin

end;

function THelpViewer.GetLocalizedName: string;
begin
  Result:=StorageName;
end;

{ THelpNode }

constructor THelpNode.Create(TheOwner: THelpDatabase; Node: THelpNode);
begin
  FOwner:=TheOwner;
  Assign(Node);
end;

constructor THelpNode.Create(TheOwner: THelpDatabase; const TheTitle,
  TheURL, TheID: string; TheContext: THelpContext);
begin
  FOwner:=TheOwner;
  FHelpType:=hntURLIDContext;
  FTitle:=TheTitle;
  FURL:=TheURL;
  FID:=TheID;
  FContext:=TheContext;
end;

constructor THelpNode.CreateURL(TheOwner: THelpDatabase; const TheTitle,
  TheURL: string);
begin
  FOwner:=TheOwner;
  FHelpType:=hntURL;
  FTitle:=TheTitle;
  FURL:=TheURL;
end;

constructor THelpNode.CreateID(TheOwner: THelpDatabase;
  const TheTitle, TheID: string);
begin
  FOwner:=TheOwner;
  FHelpType:=hntID;
  FTitle:=TheTitle;
  FID:=TheID;
end;

constructor THelpNode.CreateURLID(TheOwner: THelpDatabase;
  const TheTitle, TheURL, TheID: string);
begin
  FOwner:=TheOwner;
  FHelpType:=hntURLID;
  FTitle:=TheTitle;
  FURL:=TheURL;
  FID:=TheID;
end;

constructor THelpNode.CreateContext(TheOwner: THelpDatabase;
  const TheTitle: string; TheContext: THelpContext);
begin
  FOwner:=TheOwner;
  FHelpType:=hntContext;
  FTitle:=TheTitle;
  FContext:=TheContext;
end;

constructor THelpNode.CreateURLContext(TheOwner: THelpDatabase; const TheTitle,
  TheURL: string; TheContext: THelpContext);
begin
  FOwner:=TheOwner;
  FHelpType:=hntURLContext;
  FTitle:=TheTitle;
  FURL:=TheURL;
  FContext:=TheContext;
end;

function THelpNode.URLValid: boolean;
begin
  Result:=FHelpType in [hntURLIDContext,hntURLID,hntURLContext];
end;

function THelpNode.IDValid: boolean;
begin
  Result:=FHelpType in [hntURLIDContext,hntURLID,hntID];
end;

function THelpNode.ContextValid: boolean;
begin
  Result:=FHelpType in [hntURLIDContext,hntURLContext,hntContext];
end;

procedure THelpNode.Assign(Source: TPersistent);
var
  Node: THelpNode;
begin
  if Source is THelpNode then begin
    Node:=THelpNode(Source);
    FHelpType:=Node.HelpType;
    FTitle:=Node.Title;
    FURL:=Node.URL;
    FContext:=Node.Context;
  end else
    inherited Assign(Source);
end;

{ THelpDBSearchItem }

constructor THelpDBSearchItem.Create(TheNode: THelpNode);
begin
  Node:=TheNode
end;

destructor THelpDBSearchItem.Destroy;
begin
  Node.Free;
  inherited Destroy;
end;

{ TPascalHelpContextList }

function TPascalHelpContextList.GetItems(Index: integer): TPascalHelpContext;
begin
  Result:=fItems[Index];
end;

procedure TPascalHelpContextList.Add(const Context: TPascalHelpContext);
begin
  inc(FCount);
  ReAllocMem(fItems,SizeOf(TPascalHelpContext)*FCount);
  fItems[FCount-1]:=Context;
end;

procedure TPascalHelpContextList.Insert(Index: integer;
  const Context: TPascalHelpContext);
begin
  inc(FCount);
  ReAllocMem(fItems,SizeOf(TPascalHelpContext)*FCount);
  if Index<FCount-1 then
    System.Move(fItems[Index],fItems[Index+1],
                SizeOf(TPascalHelpContext)*(FCount-Index-1));
  fItems[Index]:=Context;
end;

procedure TPascalHelpContextList.Clear;
begin
  ReAllocMem(fItems,0);
end;

destructor TPascalHelpContextList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ THelpDBSISourceFile }

procedure THelpDBSISourceFile.SetFilename(const AValue: string);
begin
  FFilename:=TrimFilename(AValue);
end;

function THelpDBSISourceFile.FileMatches(const AFilename: string): boolean;
begin
  if (FFilename='') or (AFilename='') then
    Result:=false
  else
    Result:=CompareFilenames(GetFullFilename,AFilename)=0;
end;

function THelpDBSISourceFile.GetFullFilename: string;
var
  BaseDir: String;
begin
  if FilenameIsAbsolute(FFilename) then
    Result:=FFilename
  else begin
    BaseDir:=GetBasePath;
    Result:=BaseDir+FFilename;
  end;
end;

function THelpDBSISourceFile.GetBasePath: string;
begin
  if BasePathObject=nil then
    Result:=''
  else
    Result:=AppendPathDelim(
               HelpDatabases.GetBaseDirectoryForBasePathObject(BasePathObject));
end;

{ THelpDBSISourceDirectory }

function THelpDBSISourceDirectory.FileMatches(const AFilename: string
  ): boolean;
var
  TheDirectory: String;
begin
  Result:=false;
  if (FFilename='') or (AFilename='') then exit;
  TheDirectory:=GetFullFilename;
  if WithSubDirectories then begin
    if not FileIsInPath(AFilename,TheDirectory) then exit;
  end else begin
    if not FileIsInDirectory(AFilename,TheDirectory) then exit;
  end;
  if (FileMask<>'')
  and (not FileInFilenameMasks(ExtractFilename(AFilename),FileMask)) then exit;
  Result:=true;
end;

initialization
  HelpDatabases:=nil;

end.

