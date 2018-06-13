{  $Id$  }
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    This unit defines various base classes for the LCL Help System.
    
  ToDo:
    - fix TCHMHelpViewer
    - Make THelpDatabase and THelpViewer components usable in the designer.
    - localization support.
    - Add Help Editing functions
}
unit LazHelpIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  FileUtil, LazFileUtils, LazUtilities, LazUTF8, LazConfigStorage, Masks,
  // LCL
  LCLProc, LCLStrConsts, Dialogs, HelpIntfs;

type
  { THelpQueryItem }

  THelpQueryItem = class
  public
    function AsString: string; virtual; abstract;
    function IsEqual(QueryItem: THelpQueryItem): boolean; virtual;
  end;

  { TPascalHelpContextList }

  TPascalHelpContextType = (
    pihcFilename,
    pihcSourceName,  // unit name, library name, ..
    pihcProperty,
    pihcProcedure,
    pihcParameterList,
    pihcVariable,
    pihcType,
    pihcConst
    );
  TPascalHelpContext = record
    Descriptor: TPascalHelpContextType;
    Context: string;
  end;
  TPascalHelpContextPtr = ^TPascalHelpContext;
  
  TPascalHelpContextList = class(THelpQueryItem)
  private
    FCount: integer;
    fItems: TPascalHelpContextPtr;
    function GetItems(Index: integer): TPascalHelpContext;
  public
    procedure Add(const Context: TPascalHelpContext);
    procedure Add(Descriptor: TPascalHelpContextType; const Context: string);
    procedure Insert(Index: integer; const Context: TPascalHelpContext);
    procedure Clear;
    destructor Destroy; override;
    function IsEqual(QueryItem: THelpQueryItem): boolean; override;
    function CompareList(AList: TPascalHelpContextList): integer;
    function AsString: string; override;
  public
    property Count: integer read FCount;
    property Items[Index: integer]: TPascalHelpContext read GetItems;
    property List: TPascalHelpContextPtr read fItems;
  end;
  
  
  THelpDatabase = class;

  { THelpNode
    A help node is a position/place in a help database.
    For example it points to a Help file or to a Link on a HTML file. }
  
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
    function AsString: string;
    procedure Assign(Source: TPersistent); override;
  published
    property Title: string read FTitle write FTitle;
    property HelpType: THelpNodeType read FHelpType write FHelpType;
    property URL: string read FURL write FURL;
    property ID: string read fID write fID;
    property Context: THelpContext read FContext write FContext;
  end;
  
  
  { THelpNodeQuery }

  THelpNodeQuery = class
  private
    FNode: THelpNode;
    FQueryItem: THelpQueryItem;
  public
    constructor Create;
    constructor Create(TheNode: THelpNode; TheQueryItem: THelpQueryItem);
    function IsEqual(TheNode: THelpNode; TheQueryItem: THelpQueryItem): boolean;
    function IsEqual(NodeQuery: THelpNodeQuery): boolean;
    function AsString: string;
    property Node: THelpNode read FNode write FNode;
    property QueryItem: THelpQueryItem read FQueryItem write FQueryItem;
  end;
  
  
  { THelpNodeQueryList }

  THelpNodeQueryList = class
  private
    fItems: TFPList;
    function GetItems(Index: integer): THelpNodeQuery;
    procedure SetItems(Index: integer; const AValue: THelpNodeQuery);
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function Add(NodeQuery: THelpNodeQuery): integer;
    function Add(Node: THelpNode; QueryItem: THelpQueryItem): integer;
    procedure Delete(Index: integer);
    function IndexOf(NodeQuery: THelpNodeQuery): integer;
    function IndexOf(Node: THelpNode; QueryItem: THelpQueryItem): integer;
    procedure Clear;
    property Items[Index: integer]: THelpNodeQuery read GetItems write SetItems; default;
  end;


  { THelpDBItem
    Base class for registration items associated with a THelpDatabase.
    See THelpDBSISourceDirectory for an example.
    Node is optional, pointing to a help page about the help item. }

  THelpDBItem = class(TPersistent)
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

  THelpDBISourceFile = class(THelpDBItem)
  private
    FBasePathObject: TObject;
    FFilename: string;
    procedure SetFilename(const AValue: string);
  public
    constructor Create(TheNode: THelpNode; const TheFilename: string);
    function FileMatches(const AFilename: string): boolean; virtual;
    function GetFullFilename: string; virtual;
    function GetBasePath: string; virtual;
  published
    property BasePathObject: TObject read FBasePathObject write FBasePathObject;
    property Filename: string read FFilename write SetFilename;
  end;
  

  { THelpDBISourceDirectory
    Help registration item for source directory.
    As THelpDBISourceFile, except that Filename is a directory and
    the item is valid for all source files fitting the FileMask.
    FileMask can be for example '*.pp;*.pas;*.inc'

    For example: A package providing help for all its source files registers
    a THelpDBISourceDirectory. Node points to the fpdoc main page.
    }

  THelpDBISourceDirectory = class(THelpDBISourceFile)
  private
    FFileMask: string;
    FWithSubDirectories: boolean;
  public
    constructor Create(TheNode: THelpNode; const Directory,
                       TheFileMask: string; Recursive: boolean);
    function FileMatches(const AFilename: string): boolean; override;
  published
    property FileMask: string read FFileMask write FFileMask;
    property WithSubDirectories: boolean read FWithSubDirectories
                                         write FWithSubDirectories;
  end;


  { THelpDBISourceDirectories
    Help registration item for source directories.
    As THelpDBISourceDirectory, except that Filename are directories separated
    by semicolon and the item is valid for all source files fitting the FileMask.
    FileMask can be for example '*.pp;*.pas;*.inc'

    For example: A package providing help for all its source files registers
    a THelpDBISourceDirectory. Node points to the fpdoc main page.
    }

  THelpDBISourceDirectories = class(THelpDBISourceDirectory)
  private
    FBaseDirectory: string;
  public
    constructor Create(TheNode: THelpNode; const BaseDir, Directories,
                       TheFileMask: string; Recursive: boolean);
    function FileMatches(const AFilename: string): boolean; override;
    function GetFullFilename: string; override;
    function GetBasePath: string; override;
  published
    property BaseDirectory: string read FBaseDirectory write FBaseDirectory;
  end;

  { THelpDBIClass
    Help registration item for a class.
    Used by the IDE to search for help for a class without source.
    For example for a registered component class in the component palette, that
    comes without source. If the component comes with source use the
    THelpDBISourceDirectory or THelpDBISourceFile instead. }

  THelpDBIClass = class(THelpDBItem)
  private
    FTheClass: TClass;
  public
    property TheClass: TClass read FTheClass write FTheClass;
  end;
  
  
  { THelpDBIMessage
    Help registration item for a message (e.g. an FPC warning).
    Used by the IDE to search for help for one message (typically a line).
    For example a line like
      "/usr/share/lazarus/components/synedit/syneditkeycmds.pp(532,10) Warning: Function result does not seem to be set"
    }

  THelpDBIMessage = class(THelpDBItem)
  public
    function MessageMatches(const TheMessage: string; MessageParts: TStrings
                            ): boolean; virtual; abstract;
  end;


  { THelpQueryNode }

  THelpQueryNode = class(THelpQuery)
  private
    FNode: THelpNode;
  public
    constructor Create(const TheHelpDatabaseID: THelpDatabaseID;
                       const TheNode: THelpNode);
    property Node: THelpNode read FNode write FNode;
  end;


  { THelpDatabase
    Base class for a collection of help files or entries.
    BasePathObject: THelpDatabase can be created by packages.
                    The IDE will set BasePathObject accordingly. }

  THelpDatabases = class;
  THelpViewer = class;

  TOnHelpDBFindViewer =
    function(HelpDB: THelpDatabase; const MimeType: string;
        var ErrMsg: string; out Viewer: THelpViewer): TShowHelpResult of object;

  THelpDatabase = class(TComponent)
  private
    FAutoRegister: boolean;
    FBasePathObject: TObject;
    FID: THelpDatabaseID;
    FDatabases: THelpDatabases;
    FOnFindViewer: TOnHelpDBFindViewer;
    FRefCount: integer;
    FSearchItems: TFPList;
    FSupportedMimeTypes: TStrings;
    FTOCNode: THelpNode;
    procedure SetAutoRegister(const AValue: boolean);
    procedure SetID(const AValue: THelpDatabaseID);
    procedure SetDatabases(const AValue: THelpDatabases);
  protected
    procedure SetSupportedMimeTypes(List: TStrings); virtual;
    procedure AddSupportedMimeType(const AMimeType: string); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reference;
    procedure RegisterSelf;
    procedure Release;
    procedure UnregisterSelf;
    function Registered: boolean;
    function CanShowTableOfContents: boolean; virtual;
    procedure ShowTableOfContents; virtual;
    procedure ShowError(ShowResult: TShowHelpResult; const ErrMsg: string); virtual;
    function ShowHelp(Query: THelpQuery; BaseNode, NewNode: THelpNode;
                      QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpFile(Query: THelpQuery; BaseNode: THelpNode;
                          const Title, Filename: string;
                          var ErrMsg: string): TShowHelpResult; virtual;
    function SupportsMimeType(const AMimeType: string): boolean; virtual;
    function GetNodesForKeyword(const HelpKeyword: string;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForDirective(const HelpDirective: string;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForContext(HelpContext: THelpContext;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForPascalContexts(ListOfPascalHelpContextList: TList;
                                       var ListOfNodes: THelpNodeQueryList;
                                       var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForClass(AClass: TClass;
                              var ListOfNodes: THelpNodeQueryList;
                              var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForMessage(const AMessage: string; MessageParts: TStrings;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; virtual;
    function FindViewer(const MimeType: string; var ErrMsg: string;
                        out Viewer: THelpViewer): TShowHelpResult; virtual;
  public
    // registration
    procedure RegisterItem(NewItem: THelpDBItem);
    procedure RegisterItemWithNode(Node: THelpNode);
    procedure RegisterFileItemWithNode(const Filename: string; Node: THelpNode);
    procedure UnregisterItem(AnItem: THelpDBItem);
    procedure UnregisterAllItems;
    function RegisteredItemCount: integer;
    function GetRegisteredItem(Index: integer): THelpDBItem;
    procedure Load(Storage: TConfigStorage); virtual;
    procedure Save(Storage: TConfigStorage); virtual;
    function GetLocalizedName: string; virtual;
  public
    property Databases: THelpDatabases read FDatabases write SetDatabases;
    property ID: THelpDatabaseID read FID write SetID;
    property SupportedMimeTypes: TStrings read FSupportedMimeTypes;
    property BasePathObject: TObject read FBasePathObject write FBasePathObject;
    property TOCNode: THelpNode read FTOCNode write FTOCNode;
    property AutoRegister: boolean read FAutoRegister write SetAutoRegister;
    property OnFindViewer: TOnHelpDBFindViewer read FOnFindViewer write FOnFindViewer;
  end;

  THelpDatabaseClass = class of THelpDatabase;
  

  { THelpDatabases
    Class for storing all registered THelpDatabase(s) }

  THelpDatabases = class(THelpManager)
  private
    FItems: TFPList;
    FHelpDBClasses: TFPList;
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
    function GetDatabase(ID: THelpDatabaseID; var HelpDB: THelpDatabase;
                         var HelpResult: TShowHelpResult; var ErrMsg: string): boolean;
    function IndexOf(ID: THelpDatabaseID): integer;
    function CreateUniqueDatabaseID(const WishID: string): THelpDatabaseID;
    function CreateHelpDatabase(const WishID: string;
                                HelpDataBaseClass: THelpDatabaseClass;
                                AutoRegister: boolean): THelpDatabase;
    function ShowTableOfContents(var ErrMsg: string): TShowHelpResult; override;
    procedure ShowError(ShowResult: TShowHelpResult; const ErrMsg: string); override;
    function GetBaseURLForBasePathObject(BasePathObject: TObject): string; virtual;
    function GetBaseDirectoryForBasePathObject(BasePathObject: TObject): string; virtual;
    function FindViewer(const MimeType: string; var ErrMsg: string;
                        var Viewer: THelpViewer): TShowHelpResult; virtual;
    function SubstituteMacros(var s: string): boolean; virtual;
  public
    // show help for ...
    function ShowHelpForNodes(Query: THelpQuery; Nodes: THelpNodeQueryList;
                              var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpForQuery(Query: THelpQuery; AutoFreeQuery: boolean;
                              var ErrMsg: string): TShowHelpResult; override;
    function ShowHelpForContext(Query: THelpQueryContext;
                                var ErrMsg: string): TShowHelpResult; override;
    function ShowHelpForKeyword(Query: THelpQueryKeyword;
                                var ErrMsg: string): TShowHelpResult; override;
    function ShowHelpForDirective(Query: THelpQueryDirective;
                                var ErrMsg: string): TShowHelpResult; override;
    function ShowHelpForPascalContexts(Query: THelpQueryPascalContexts;
                                       var ErrMsg: string): TShowHelpResult; override;
    function ShowHelpForSourcePosition(Query: THelpQuerySourcePosition;
                                       var ErrMsg: string): TShowHelpResult; override;
    function ShowHelpForMessageLine(Query: THelpQueryMessage;
                                    var ErrMsg: string): TShowHelpResult; override;
    function ShowHelpForClass(Query: THelpQueryClass;
                              var ErrMsg: string): TShowHelpResult; override;
    function ShowHelpFile(const Filename, Title, MimeType: string;
                      var ErrMsg: string): TShowHelpResult; override;
    function ShowHelp(const URL, Title, MimeType: string;
                      var ErrMsg: string): TShowHelpResult; override;
    // search registered items in all databases
    function GetNodesForKeyword(const HelpKeyword: string;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForDirective(const HelpDirective: string;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForContext(HelpContext: THelpContext;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForPascalContexts(ListOfPascalHelpContextList: TList;
                                       var ListOfNodes: THelpNodeQueryList;
                                       var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForClass(AClass: TClass;
                              var ListOfNodes: THelpNodeQueryList;
                              var ErrMsg: string): TShowHelpResult; virtual;
    function GetNodesForMessage(const AMessage: string; MessageParts: TStrings;
                                var ListOfNodes: THelpNodeQueryList;
                                var ErrMsg: string): TShowHelpResult; virtual;
    // Show the help selector
    function ShowHelpSelector(Query: THelpQuery; Nodes: THelpNodeQueryList;
                              var ErrMsg: string;
                              var Selection: THelpNodeQuery
                              ): TShowHelpResult; virtual;
  public
    // registration of THelpDatabaseClass
    procedure RegisterHelpDatabaseClass(NewHelpDB: THelpDatabaseClass);
    procedure UnregisterHelpDatabaseClass(AHelpDB: THelpDatabaseClass);
    function HelpDatabaseClassCount: integer;
    function GetHelpDatabaseClass(Index: integer): THelpDatabaseClass;
    procedure Load(Storage: TConfigStorage); virtual;
    procedure Save(Storage: TConfigStorage); virtual;
  end;
  
  
  { THelpViewer
    base class for all Help viewers }
  
  THelpViewer = class(TComponent)
  private
    FAutoRegister: boolean;
    FParameterHelp: string;
    FStorageName: string;
    FSupportedMimeTypes: TStrings;
    procedure SetAutoRegister(const AValue: boolean);
  protected
    procedure SetSupportedMimeTypes(List: TStrings); virtual;
    procedure AddSupportedMimeType(const AMimeType: string); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
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
    procedure RegisterSelf; virtual;
    procedure UnregisterSelf; virtual;
  public
    property SupportedMimeTypes: TStrings read FSupportedMimeTypes;
    property ParameterHelp: string read FParameterHelp write FParameterHelp;
    property StorageName: string read FStorageName write FStorageName;
    property AutoRegister: boolean read FAutoRegister write SetAutoRegister;
  end;

  THelpViewerClass = class of THelpViewer;
  
  
  { THelpViewers }
  
  THelpViewers = class
  private
    FItems: TFPList;
    FDestroying: boolean;
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
    function IndexOf(AHelpViewer: THelpViewer): integer;
  public
    property Items[Index: integer]: THelpViewer read GetItems; default;
  end;
  
  
  { THelpBasePathObject
    Simple class to store a base file path for help databases. }
  
  THelpBasePathObject = class(TPersistent)
  private
    FBasePath: string;
  protected
    procedure SetBasePath(const AValue: string); virtual;
  public
    constructor Create;
    constructor Create(const TheBasePath: string);
    property BasePath: string read FBasePath write SetBasePath;
  end;
  
  { THelpBaseURLObject
    Simple class to store a base URL path for help databases. }

  THelpBaseURLObject = class(TPersistent)
  private
    FBaseURL: string;
  protected
    procedure SetBaseURL(const AValue: string);
  public
    constructor Create;
    constructor Create(const TheBaseURL: string);
    property BaseURL: string read FBaseURL write SetBaseURL;
  end;

var
  HelpDatabases: THelpDatabases = nil; // initialized by the IDE
  HelpViewers: THelpViewers = nil; // initialized by the IDE

procedure CreateLCLHelpSystem;
procedure FreeLCLHelpSystem;
procedure FreeUnusedLCLHelpSystem;

// URL functions
// used names:
//  URL: Type + Path + Params e.g. http://www.freepascal.org?param
//  URLType: e.g. file or http
//  URLPath: URL without type and without parameters (always / as path delimiter)
//  URLParams: parameters appended by ? or #
function FilenameToURL(const Filename: string): string;
function FilenameToURLPath(const Filename: string): string;
function URLPathToFilename(const URLPath: string): string;
procedure SplitURL(const URL: string; out URLScheme, URLPath, URLParams: string);
function CombineURL(const URLScheme, URLPath, URLParams: string): string;
function URLFilenameIsAbsolute(const URLPath: string): boolean;
function FindURLPathStart(const URL: string): integer;
function FindURLPathEnd(const URL: string): integer;
function ChompURLParams(const URL: string): string;
function ExtractURLPath(const URL: string): string;
function ExtractURLDirectory(const URL: string): string;
function TrimUrl(const URL: string): string;
function TrimURLPath(const URLPath: string): string;
function IsFileURL(const URL: string): boolean;
function AppendURLPathDelim(const URLPath: string): string;

procedure CreateListAndAdd(const AnObject: TObject; var List: TList;
  OnlyIfNotExists: boolean);
procedure CreateNodeQueryListAndAdd(const ANode: THelpNode;
  const QueryItem: THelpQueryItem;
  var List: THelpNodeQueryList; OnlyIfNotExists: boolean);


implementation


procedure CreateLCLHelpSystem;
begin
  if (HelpDatabases<>nil) or (HelpManager<>nil) then exit;
  HelpDatabases:=THelpDatabases.Create;
  HelpManager:=HelpDatabases;
  HelpViewers:=THelpViewers.Create;
end;

procedure FreeLCLHelpSystem;
begin
  FreeThenNil(HelpDatabases);
  FreeThenNil(HelpViewers);
  HelpManager:=nil;
end;

procedure FreeUnusedLCLHelpSystem;
begin
  if (HelpViewers<>nil) and (HelpViewers.Count>0) then exit;
  if (HelpDatabases<>nil) and (HelpDatabases.Count>0) then exit;
  FreeLCLHelpSystem;
end;

function FilenameToURL(const Filename: string): string;
begin
  Result:=FilenameToURLPath(Filename);
  if Result<>'' then
    Result:='file://'+Result;
end;

function FilenameToURLPath(const Filename: string): string;
var
  i: Integer;
begin
  Result:=Filename;
  {$warnings off}
  if PathDelim<>'/' then
    for i:=1 to length(Result) do
      if Result[i]=PathDelim then
        Result[i]:='/';
  {$warnings on}
end;

function URLPathToFilename(const URLPath: string): string;
var
  i: Integer;
begin
  Result:=URLPath;
  {$warnings off}
  if PathDelim<>'/' then
    for i:=1 to length(Result) do
      if Result[i]='/' then
        Result[i]:=PathDelim;
  {$warnings on}
end;

procedure SplitURL(const URL: string; out URLScheme, URLPath, URLParams: string);
var
  Len: Integer;
  ColonPos: Integer;
  ParamStartPos: integer;
  URLStartPos: Integer;
begin
  URLScheme:='';
  URLPath:='';
  URLParams:='';
  Len:=length(URL);
  // search colon
  ColonPos:=1;
  while (ColonPos<=len) and (URL[ColonPos]<>':') do
    inc(ColonPos);
  if ColonPos>len then exit;
  // get URLScheme
  URLScheme:=copy(URL,1,ColonPos-1);
  URLStartPos:=ColonPos+1;
  // skip the '//' after the colon
  if (URLStartPos<=len) and (URL[URLStartPos]='/') then inc(URLStartPos);
  if (URLStartPos<=len) and (URL[URLStartPos]='/') then inc(URLStartPos);
  // search for param delimiter (?) or anchor delimiter (#)
  ParamStartPos:=ColonPos+1;
  while (ParamStartPos<=len) and not (URL[ParamStartPos]in ['?', '#']) do
    inc(ParamStartPos);
  // get URLPath and URLParams
  URLPath:=copy(URL,URLStartPos,ParamStartPos-URLStartPos);
  URLParams:=copy(URL,ParamStartPos,len-ParamStartPos+1);
end;

function CombineURL(const URLScheme, URLPath, URLParams: string): string;
begin
  Result:=URLScheme+'://'+URLPath;
  if URLParams<>'' then
    Result:=Result+URLParams;
end;

function URLFilenameIsAbsolute(const URLPath: string): boolean;
begin
  Result:=FilenameIsAbsolute(URLPathToFilename(URLPath));
end;

function FindURLPathStart(const URL: string): integer;
var
  Len: Integer;
  ColonPos: Integer;
  URLStartPos: Integer;
begin
  Result:=-1;
  Len:=length(URL);
  // search colon
  ColonPos:=1;
  while (ColonPos<=len) and (URL[ColonPos]<>':') do
    inc(ColonPos);
  if ColonPos=Len then exit;
  URLStartPos:=ColonPos+1;
  // skip the '//' after the colon
  if (URLStartPos<=Len) and (URL[URLStartPos]='/') then inc(URLStartPos);
  if (URLStartPos<=Len) and (URL[URLStartPos]='/') then inc(URLStartPos);
  Result:=URLStartPos;
end;

function FindURLPathEnd(const URL: string): integer;
var
  Len: Integer;
begin
  Result:=1;
  Len:=length(URL);
  while (Result<=Len) and not (URL[Result] in ['?','#']) do inc(Result);
end;

function ChompURLParams(const URL: string): string;
begin
  Result:=copy(URL,1,FindURLPathEnd(URL)-1);
end;

function ExtractURLDirectory(const URL: string): string;
var
  p: Integer;
  PathStart: LongInt;
begin
  Result:='';
  PathStart:=FindURLPathStart(URL);
  if PathStart<1 then exit;
  p:=FindURLPathEnd(URL);
  repeat
    dec(p);
  until (p<=0) or (URL[p]='/');
  if p<=PathStart then exit;
  Result:=copy(URL,1,p);
end;

function TrimUrl(const URL: string): string;
var
  URLType, URLPath, URLParams: string;
begin
  SplitURL(URL,URLType,URLPath,URLParams);
  Result:=CombineURL(URLType,TrimURLPath(URLPath),URLParams);
end;

function TrimURLPath(const URLPath: string): string;
begin
  Result:=FilenameToURLPath(TrimFilename(URLPathToFilename(URLPath)));
end;

function IsFileURL(const URL: string): boolean;
begin
  Result:=(length(URL)>=7)
          and (CompareByte(URL[1],'file://',7)=0);
end;

function AppendURLPathDelim(const URLPath: string): string;
begin
  if (URLPath<>'') and (URLPath[length(URLPath)]<>'/') then
    Result:=URLPath+'/'
  else
    Result:=URLPath;
end;

function ExtractURLPath(const URL: string): string;
var
  URLType, URLPath, URLParams: string;
begin
  SplitURL(URL,URLType,URLPath,URLParams);
  Result:=URLPath;
end;

procedure CreateListAndAdd(const AnObject: TObject; var List: TList;
  OnlyIfNotExists: boolean);
begin
  if List=nil then
    List:=TList.Create
  else if OnlyIfNotExists and (List.IndexOf(AnObject)>=0) then
    exit;
  List.Add(AnObject);
end;

procedure CreateNodeQueryListAndAdd(const ANode: THelpNode;
  const QueryItem: THelpQueryItem;
  var List: THelpNodeQueryList; OnlyIfNotExists: boolean);
begin
  if List=nil then
    List:=THelpNodeQueryList.Create
  else if OnlyIfNotExists and (List.IndexOf(ANode,QueryItem)>=0) then
    exit;
  List.Add(ANode,QueryItem);
end;

{ THelpDBISourceDirectories }

constructor THelpDBISourceDirectories.Create(TheNode: THelpNode; const BaseDir,
  Directories, TheFileMask: string; Recursive: boolean);
begin
  inherited Create(TheNode,Directories,TheFileMask,Recursive);
  FBaseDirectory:=BaseDir;
end;

function THelpDBISourceDirectories.FileMatches(const AFilename: string
  ): boolean;
var
  SearchPath: String;
  EndPos: Integer;
  StartPos: Integer;
  Dir: String;
begin
  Result:=false;
  //debugln('THelpDBISourceDirectories.FileMatches AFilename="',AFilename,'" FFilename="',FFilename,'"');
  if (FFilename='') or (AFilename='') then exit;
  SearchPath:=GetFullFilename;
  if SearchPath='' then begin
    {$IFNDEF DisableChecks}
    DebugLn(['WARNING: THelpDBISourceDirectory.FileMatches ',DbgSName(Self),' Filename="',Filename,'" -> ""']);
    {$ENDIF}
    exit;
  end;
  EndPos:=1;
  while EndPos<=length(SearchPath) do begin
    StartPos:=EndPos;
    while (EndPos<=length(SearchPath)) and (SearchPath[EndPos]<>';') do
      inc(EndPos);
    Dir:=copy(SearchPath,StartPos,EndPos-StartPos);
    inc(EndPos);
    //debugln(['THelpDBISourceDirectories.FileMatches TheDirectory="',Dir,'" WithSubDirectories=',WithSubDirectories]);
    if WithSubDirectories then begin
      if not FileIsInPath(AFilename,Dir) then continue;
    end else begin
      if not FileIsInDirectory(AFilename,Dir) then continue;
    end;
    //debugln('THelpDBISourceDirectories.FileMatches FileMask="',FileMask,'"');
    if (FileMask='')
    or MatchesMaskList(ExtractFilename(AFilename),FileMask) then
      exit(true);
  end;
end;

function THelpDBISourceDirectories.GetFullFilename: string;
var
  ExpFilename: String;
  EndPos: Integer;
  StartPos: Integer;
  Dir: String;
  BaseDir: String;
begin
  ExpFilename:=FFilename;
  //DebugLn(['THelpDBISourceDirectories.GetFullFilename ExpFilename="',ExpFilename,'" HelpDatabases=',DbgSName(HelpDatabases)]);
  if (HelpDatabases<>nil) then
    HelpDatabases.SubstituteMacros(ExpFilename);
  //DebugLn(['THelpDBISourceFile.GetFullFilename substituted ',ExpFilename]);
  EndPos:=1;
  Result:='';
  BaseDir:='';
  while EndPos<=length(ExpFilename) do begin
    StartPos:=EndPos;
    while (EndPos<=length(ExpFilename)) and (ExpFilename[EndPos]<>';') do
      inc(EndPos);
    Dir:=TrimFilename(GetForcedPathDelims(copy(ExpFilename,StartPos,EndPos-StartPos)));
    if Dir<>'' then begin
      if not FilenameIsAbsolute(Dir) then begin
        if BaseDir='' then
          BaseDir:=AppendPathDelim(GetBasePath);
        Dir:=BaseDir+Dir;
      end;
      if Result<>'' then Result:=Result+';';
      Result:=Result+Dir;
    end;
    inc(EndPos);
  end;
end;

function THelpDBISourceDirectories.GetBasePath: string;
begin
  if BaseDirectory='' then
    Result:=inherited GetBasePath
  else begin
    Result:=BaseDirectory;
    if (HelpDatabases<>nil) then
      HelpDatabases.SubstituteMacros(Result);
  end;
  Result:=TrimFilename(GetForcedPathDelims(Result));
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

procedure THelpDatabase.SetAutoRegister(const AValue: boolean);
begin
  if FAutoRegister=AValue then exit;
  FAutoRegister:=AValue;
  if not (csDesigning in ComponentState) then begin
    if FAutoRegister then begin
      if FID='' then
        FID:=Name;
      if Databases=nil then RegisterSelf;
    end else begin
      if Databases<>nil then UnregisterSelf;
    end;
  end;
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

constructor THelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
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
    raise EHelpSystemException.Create(Format(rsHelpAlreadyRegistered, [ID]));
  if HelpDatabases=nil then CreateLCLHelpSystem;
  Databases:=HelpDatabases;
end;

procedure THelpDatabase.UnregisterSelf;
begin
  if Databases=nil then
    raise EHelpSystemException.Create(Format(rsHelpNotRegistered, [ID]));
  Databases:=nil;
  FreeUnusedLCLHelpSystem;
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
  Query: THelpQueryTOC;
begin
  if TOCNode=nil then exit;
  ErrMsg:='';
  Query:=THelpQueryTOC.Create(ID);
  try
    ShowResult:=ShowHelp(Query,nil,TOCNode,nil,ErrMsg);
  finally
    Query.Free;
  end;
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

function THelpDatabase.ShowHelp(Query: THelpQuery; BaseNode, NewNode: THelpNode;
  QueryItem: THelpQueryItem; var ErrMsg: string): TShowHelpResult;
begin
  ErrMsg:='';
  Result:=shrContextNotFound;
end;

function THelpDatabase.ShowHelpFile(Query: THelpQuery; BaseNode: THelpNode;
  const Title, Filename: string; var ErrMsg: string): TShowHelpResult;
var
  FileNode: THelpNode;
begin
  FileNode:=THelpNode.CreateURL(Self,Title,FilenameToURL(Filename));
  try
    Result:=ShowHelp(Query,BaseNode,FileNode,nil,ErrMsg);
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
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
  Node: THelpNode;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  if csDesigning in ComponentState then exit;
  // add the registered nodes
  if FSearchItems<>nil then begin
    for i:=0 to FSearchItems.Count-1 do begin
      Node:=THelpDBItem(FSearchItems[i]).Node;
      if (Node=nil) or (not Node.IDValid) then continue;
      if UTF8CompareText(Node.ID,HelpKeyword)<>0 then continue;
      CreateNodeQueryListAndAdd(Node,nil,ListOfNodes,true);
    end;
  end;
end;

function THelpDatabase.GetNodesForDirective(const HelpDirective: string;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
  Node: THelpNode;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  if csDesigning in ComponentState then exit;
  // add the registered nodes
  if FSearchItems<>nil then begin
    for i:=0 to FSearchItems.Count-1 do begin
      Node:=THelpDBItem(FSearchItems[i]).Node;
      if (Node=nil) or (not Node.IDValid) then continue;
      if UTF8CompareText(Node.ID,HelpDirective)<>0 then continue;
      CreateNodeQueryListAndAdd(Node,nil,ListOfNodes,true);
    end;
  end;
end;

function THelpDatabase.GetNodesForContext(HelpContext: THelpContext;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
  Node: THelpNode;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  if csDesigning in ComponentState then exit;
  // add the registered nodes
  if FSearchItems<>nil then begin
    for i:=0 to FSearchItems.Count-1 do begin
      Node:=THelpDBItem(FSearchItems[i]).Node;
      if (Node=nil) or (not Node.ContextValid) then continue;
      if Node.Context<>HelpContext then continue;
      CreateNodeQueryListAndAdd(Node,nil,ListOfNodes,true);
    end;
  end;
end;

function THelpDatabase.GetNodesForPascalContexts(
  ListOfPascalHelpContextList: TList; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
  j: Integer;
  SearchItem: THelpDBItem;
  PascalContext: TPascalHelpContextList;
  FileItem: THelpDBISourceFile;
  Filename: String;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  if csDesigning in ComponentState then exit;
  if (ListOfPascalHelpContextList=nil) or
    (ListOfPascalHelpContextList.Count=0) then exit;
  // add the registered nodes
  //debugln('THelpDatabase.GetNodesForPascalContexts A ID="',ID,'" ListOfPascalHelpContextList.Count=',dbgs(ListOfPascalHelpContextList.Count));
  if FSearchItems<>nil then begin
    // check every Pascal context
    for j:=0 to ListOfPascalHelpContextList.Count-1 do begin
      PascalContext:=TPascalHelpContextList(ListOfPascalHelpContextList[j]);
      //debugln('THelpDatabase.GetNodesForPascalContexts A ID="',ID,'" PascalContext.Count=',dbgs(PascalContext.Count));
      if (PascalContext.Count>0) and
        (PascalContext.List[0].Descriptor=pihcFilename) then begin
        Filename:=PascalContext.List[0].Context;
        // search file item
        for i:=0 to FSearchItems.Count-1 do begin
          SearchItem:=THelpDBItem(FSearchItems[i]);
          if not (SearchItem is THelpDBISourceFile) then continue;
          FileItem:=THelpDBISourceFile(SearchItem);
          //debugln('THelpDatabase.GetNodesForPascalContexts B FileItem.ClassName=',FileItem.ClassName,' Filename=',Filename,' FileItem.GetFullFilename="',FileItem.GetFullFilename,'"');
          if (FileItem.FileMatches(Filename)) then begin
            CreateNodeQueryListAndAdd(FileItem.Node,PascalContext,ListOfNodes,true);
            {$IFNDEF DisableChecks}
            debugln(['THelpDatabase.GetNodesForPascalContexts C ID="',ID,'" ',i+1,'/',ListOfPascalHelpContextList.Count,' FileItem.ClassName=',FileItem.ClassName,' Filename=',Filename,' ',ListOfNodes.Count]);
            {$ENDIF}
          end;
        end;
      end;
    end;
  end;
end;

function THelpDatabase.GetNodesForClass(AClass: TClass;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
  SearchItem: THelpDBItem;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  if csDesigning in ComponentState then exit;
  // add the registered nodes
  if FSearchItems<>nil then begin
    for i:=0 to FSearchItems.Count-1 do begin
      SearchItem:=THelpDBItem(FSearchItems[i]);
      if not (SearchItem is THelpDBIClass) then continue;
      if THelpDBIClass(SearchItem).TheClass<>AClass then continue;
      CreateNodeQueryListAndAdd(SearchItem.Node,nil,ListOfNodes,true);
    end;
  end;
end;

function THelpDatabase.GetNodesForMessage(const AMessage: string;
  MessageParts: TStrings; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
  SearchItem: THelpDBItem;
begin
  Result:=shrSuccess;
  ErrMsg:='';
  if csDesigning in ComponentState then exit;
  // add the registered nodes
  if FSearchItems<>nil then begin
    for i:=0 to FSearchItems.Count-1 do begin
      SearchItem:=THelpDBItem(FSearchItems[i]);
      if not (SearchItem is THelpDBIMessage) then continue;
      if not THelpDBIMessage(SearchItem).MessageMatches(AMessage,MessageParts)
      then continue;
      CreateNodeQueryListAndAdd(SearchItem.Node,nil,ListOfNodes,true);
    end;
  end;
end;

function THelpDatabase.FindViewer(const MimeType: string; var ErrMsg: string;
  out Viewer: THelpViewer): TShowHelpResult;
var
  Viewers: TList;
begin
  Viewer:=nil;
  if Assigned(OnFindViewer) then begin
    Result:=OnFindViewer(Self,MimeType,ErrMsg,Viewer);
    exit;
  end;

  Viewers:=HelpViewers.GetViewersSupportingMimeType(MimeType);
  try
    if (Viewers=nil) or (Viewers.Count=0) then begin
      ErrMsg:=Format(rsHelpHelpDatabaseDidNotFoundAViewerForAHelpPageOfType,
                     [ID, MimeType]);
      Result:=shrViewerNotFound;
    end else begin
      Viewer:=THelpViewer(Viewers[0]);
      Result:=shrSuccess;
    end;
  finally
    Viewers.Free;
  end;
end;

procedure THelpDatabase.RegisterItem(NewItem: THelpDBItem);
begin
  if NewItem=nil then
    raise EHelpSystemException.Create('THelpDatabase.RegisterItem NewItem=nil');
  if FSearchItems=nil then FSearchItems:=TFPList.Create;
  if FSearchItems.IndexOf(NewItem)<0 then
    FSearchItems.Add(NewItem)
  else
    NewItem.Free;
end;

procedure THelpDatabase.RegisterItemWithNode(Node: THelpNode);
begin
  if Node=nil then
    raise EHelpSystemException.Create('THelpDatabase.RegisterItemWithNode Node=nil');
  RegisterItem(THelpDBItem.Create(Node));
end;

procedure THelpDatabase.RegisterFileItemWithNode(const Filename: string;
  Node: THelpNode);
begin
  RegisterItem(THelpDBISourceFile.Create(Node,Filename));
end;

procedure THelpDatabase.UnregisterItem(AnItem: THelpDBItem);
begin
  if FSearchItems=nil then exit;
  FSearchItems.Remove(AnItem);
end;

procedure THelpDatabase.UnregisterAllItems;
var
  i: Integer;
begin
  if FSearchItems=nil then exit;
  for i:=0 to FSearchItems.Count-1 do
    TObject(FSearchItems[i]).Free;
  FSearchItems.Clear;
end;

function THelpDatabase.RegisteredItemCount: integer;
begin
  if FSearchItems=nil then
    Result:=0
  else
    Result:=FSearchItems.Count;
end;

function THelpDatabase.GetRegisteredItem(Index: integer): THelpDBItem;
begin
  Result:=THelpDBItem(FSearchItems[Index]);
end;

procedure THelpDatabase.Load(Storage: TConfigStorage);
begin

end;

procedure THelpDatabase.Save(Storage: TConfigStorage);
begin

end;

function THelpDatabase.GetLocalizedName: string;
begin
  Result:=ID;
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
  if FItems=nil then FItems:=TFPList.Create;
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

function THelpDatabases.GetDatabase(ID: THelpDatabaseID; var HelpDB: THelpDatabase;
  var HelpResult: TShowHelpResult; var ErrMsg: string): boolean;
begin
  HelpDB:=FindDatabase(ID);
  if HelpDB=nil then begin
    Result:=false;
    HelpResult:=shrDatabaseNotFound;
    ErrMsg:=Format(rsHelpHelpDatabaseNotFound, [ID]);
  end else begin
    HelpResult:=shrSuccess;
    Result:=true;
    ErrMsg:='';
  end;
end;

function THelpDatabases.IndexOf(ID: THelpDatabaseID): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (UTF8CompareText(ID,Items[Result].ID)<>0) do
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
  Result:=HelpDataBaseClass.Create(nil);
  Result.FID:=CreateUniqueDatabaseID(WishID);
  if AutoRegister then Result.RegisterSelf;
end;

function THelpDatabases.ShowTableOfContents(var ErrMsg: string
  ): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='THelpDatabases.ShowTableOfContents not implemented';
  // ToDo
end;

procedure THelpDatabases.ShowError(ShowResult: TShowHelpResult;
  const ErrMsg: string);
var
  ErrorCaption: String;
begin
  case ShowResult of
  shrNone: ErrorCaption:=rsHelpError;
  shrSuccess: exit;
  shrCancel: exit;
  shrDatabaseNotFound: ErrorCaption:=rsHelpDatabaseNotFound;
  shrContextNotFound: ErrorCaption:=rsHelpContextNotFound;
  shrViewerNotFound: ErrorCaption:=rsHelpViewerNotFound;
  shrHelpNotFound: ErrorCaption:=rsHelpNotFound;
  shrViewerError: ErrorCaption:=rsHelpViewerError;
  shrSelectorError: ErrorCaption:=rsHelpSelectorError;
  else ErrorCaption:=rsUnknownErrorPleaseReportThisBug;
  end;
  MessageDlg(ErrorCaption,ErrMsg,mtError,[mbCancel],0);
end;

function THelpDatabases.GetBaseURLForBasePathObject(BasePathObject: TObject
  ): string;
begin
  // this method will be overriden by the IDE
  // provide some useful defaults:
  if (BasePathObject is THelpBaseURLObject) then begin
    Result:=THelpBaseURLObject(BasePathObject).BaseURL;
  end else begin
    // otherwise fetch a filename
    Result:=GetBaseDirectoryForBasePathObject(BasePathObject);
    if Result='' then exit;
    Result:=FilenameToURL(Result);
  end;
  Result:=AppendURLPathDelim(Result);
end;

function THelpDatabases.GetBaseDirectoryForBasePathObject(
  BasePathObject: TObject): string;
// returns the base file directory of the BasePathObject
begin
  if (BasePathObject is THelpBaseURLObject) then begin
    Result:=THelpBaseURLObject(BasePathObject).BaseURL;
    if Result='' then exit;
    if not IsFileURL(Result) then begin
      Result:='';
      exit;
    end;
    Result:=ExtractURLPath(Result);
  end else if (BasePathObject is THelpBasePathObject) then
    Result:=THelpBasePathObject(BasePathObject).BasePath
  else
    Result:='';
  Result:=AppendURLPathDelim(Result);
end;

function THelpDatabases.FindViewer(const MimeType: string; var ErrMsg: string;
  var Viewer: THelpViewer): TShowHelpResult;
var
  Viewers: TList;
begin
  Viewer:=nil;
  Viewers:=HelpViewers.GetViewersSupportingMimeType(MimeType);
  try
    if (Viewers=nil) or (Viewers.Count=0) then begin
      ErrMsg:=Format(rsHelpThereIsNoViewerForHelpType, [MimeType]);
      Result:=shrViewerNotFound;
    end else begin
      Viewer:=THelpViewer(Viewers[0]);
      Result:=shrSuccess;
    end;
  finally
    Viewers.Free;
  end;
end;

function THelpDatabases.SubstituteMacros(var s: string): boolean;
begin
  Result:=true;
end;

function THelpDatabases.ShowHelpForNodes(Query: THelpQuery;
  Nodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
var
  NodeQuery: THelpNodeQuery;
  Node: THelpNode;
begin
  // check if several nodes found
  //debugln('THelpDatabases.ShowHelpForNodes A Nodes.Count=',dbgs(Nodes.Count));
  NodeQuery:=nil;
  if (Nodes.Count>1) then begin
    Result:=ShowHelpSelector(Query,Nodes,ErrMsg,NodeQuery);
    if Result<>shrSuccess then exit;
    if NodeQuery=nil then exit;
  end else begin
    NodeQuery:=Nodes[0];
  end;

  // show node
  Node:=NodeQuery.Node;
  if Node.Owner=nil then begin
    Result:=shrDatabaseNotFound;
    ErrMsg:=Format(rsHelpHelpNodeHasNoHelpDatabase, [Node.Title]);
    exit;
  end;
  {$IFDEF VerboseLCLHelp}
  debugln(['THelpDatabases.ShowHelpForNodes Node.Owner=',DbgSName(Node.Owner),' UnitName=',Node.Owner.UnitName]);
  {$ENDIF}
  Result:=Node.Owner.ShowHelp(Query,nil,Node,NodeQuery.QueryItem,ErrMsg);
end;

function THelpDatabases.ShowHelpForQuery(Query: THelpQuery;
  AutoFreeQuery: boolean; var ErrMsg: string): TShowHelpResult;
begin
  try
    // descendants first
    if Query is THelpQueryPascalContexts then
      Result:=ShowHelpForPascalContexts(THelpQueryPascalContexts(Query),ErrMsg)
    else if Query is THelpQueryTOC then
      Result:=ShowTableOfContents(ErrMsg)
    else if Query is THelpQueryContext then
      Result:=ShowHelpForContext(THelpQueryContext(Query),ErrMsg)
    else if Query is THelpQueryKeyword then
      Result:=ShowHelpForKeyword(THelpQueryKeyword(Query),ErrMsg)
    else if Query is THelpQueryDirective then
      Result:=ShowHelpForDirective(THelpQueryDirective(Query),ErrMsg)
    else if Query is THelpQuerySourcePosition then
      Result:=ShowHelpForSourcePosition(THelpQuerySourcePosition(Query),ErrMsg)
    else if Query is THelpQueryMessage then
      Result:=ShowHelpForMessageLine(THelpQueryMessage(Query),ErrMsg)
    else if Query is THelpQueryClass then
      Result:=ShowHelpForClass(THelpQueryClass(Query),ErrMsg)
    else
      Result:=shrContextNotFound;
  finally
    if AutoFreeQuery then Query.Free;
  end;
end;

function THelpDatabases.ShowHelpForContext(Query: THelpQueryContext;
  var ErrMsg: string): TShowHelpResult;
var
  Nodes: THelpNodeQueryList;
  HelpDB: THelpDatabase;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  
  // search node
  Nodes:=nil;
  try
    if Query.HelpDatabaseID<>'' then begin
      HelpDB:=nil;
      if not GetDatabase(Query.HelpDatabaseID,HelpDB,Result,ErrMsg) then exit;
      Result:=HelpDB.GetNodesForContext(Query.Context,Nodes,ErrMsg);
      if Result<>shrSuccess then exit;
    end else begin
      Result:=GetNodesForContext(Query.Context,Nodes,ErrMsg);
      if Result<>shrSuccess then exit;
    end;

    // check if at least one node found
    if (Nodes=nil) or (Nodes.Count=0) then begin
      Result:=shrContextNotFound;
      if Query.HelpDatabaseID<>'' then
        ErrMsg:=Format(rsHelpHelpContextNotFoundInDatabase,
                       [IntToStr(Query.Context), Query.HelpDatabaseID])
      else
        ErrMsg:=Format(rsHelpHelpContextNotFound, [IntToStr(Query.Context)]);
      exit;
    end;

    Result:=ShowHelpForNodes(Query,Nodes,ErrMsg);
  finally
    Nodes.Free;
  end;
end;

function THelpDatabases.ShowHelpForKeyword(Query: THelpQueryKeyword;
  var ErrMsg: string): TShowHelpResult;
var
  Nodes: THelpNodeQueryList;
  HelpDB: THelpDatabase;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;

  // search node
  Nodes:=nil;
  try
    if Query.HelpDatabaseID<>'' then begin
      HelpDB:=nil;
      if not GetDatabase(Query.HelpDatabaseID,HelpDB,Result,ErrMsg) then exit;
      Result:=HelpDB.GetNodesForKeyword(Query.Keyword,Nodes,ErrMsg);
      if Result<>shrSuccess then exit;
    end else begin
      Result:=GetNodesForKeyword(Query.Keyword,Nodes,ErrMsg);
      if Result<>shrSuccess then exit;
    end;

    // check if at least one node found
    if (Nodes=nil) or (Nodes.Count=0) then begin
      Result:=shrContextNotFound;
      if Query.HelpDatabaseID<>'' then
        ErrMsg:=Format(rsHelpHelpKeywordNotFoundInDatabase, [Query.Keyword, Query.HelpDatabaseID])
      else
        ErrMsg:=Format(rsHelpHelpKeywordNotFound, [Query.Keyword]);
      exit;
    end;

    Result:=ShowHelpForNodes(Query,Nodes,ErrMsg);
  finally
    Nodes.Free;
  end;
end;

function THelpDatabases.ShowHelpForDirective(Query: THelpQueryDirective;
  var ErrMsg: string): TShowHelpResult;
var
  Nodes: THelpNodeQueryList;
  HelpDB: THelpDatabase;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;

  // search node
  Nodes:=nil;
  try
    if Query.HelpDatabaseID<>'' then begin
      HelpDB:=nil;
      if not GetDatabase(Query.HelpDatabaseID,HelpDB,Result,ErrMsg) then exit;
      Result:=HelpDB.GetNodesForKeyword(Query.Directive,Nodes,ErrMsg);
      if Result<>shrSuccess then exit;
    end else begin
      Result:=GetNodesForDirective(Query.Directive,Nodes,ErrMsg);
      if Result<>shrSuccess then exit;
    end;

    // check if at least one node found
    if (Nodes=nil) or (Nodes.Count=0) then begin
      Result:=shrContextNotFound;
      if Query.HelpDatabaseID<>'' then
        ErrMsg:=Format(rsHelpHelpForDirectiveNotFoundInDatabase, [Query.Directive, Query.HelpDatabaseID])
      else
        ErrMsg:=Format(rsHelpHelpForDirectiveNotFound, [Query.Directive]);
      exit;
    end;

    Result:=ShowHelpForNodes(Query,Nodes,ErrMsg);
  finally
    Nodes.Free;
  end;
end;

function THelpDatabases.ShowHelpForPascalContexts(
  Query: THelpQueryPascalContexts; var ErrMsg: string): TShowHelpResult;
var
  Nodes: THelpNodeQueryList;
begin
  ErrMsg:='';
  Result:=shrSuccess;

  {$IFDEF VerboseLCLHelp}
  debugln('THelpDatabases.ShowHelpForPascalContexts A Count=',dbgs(Query.ListOfPascalHelpContextList.Count));
  {$ENDIF}
  // search node
  Nodes:=nil;
  try
    Result:=GetNodesForPascalContexts(Query.ListOfPascalHelpContextList,Nodes,
                                      ErrMsg);
    if Result<>shrSuccess then exit;

    // check if at least one node found
    if (Nodes=nil) or (Nodes.Count=0) then begin
      Result:=shrHelpNotFound;
      ErrMsg:=format(rsHelpNoHelpFoundForSource,
        [Query.SourcePosition.y, Query.SourcePosition.x, Query.Filename]);
      exit;
    end;
    {$IFDEF VerboseLCLHelp}
    debugln('THelpDatabases.ShowHelpForPascalContexts B Nodes.Count=',dbgs(Nodes.Count));
    {$ENDIF}

    Result:=ShowHelpForNodes(Query,Nodes,ErrMsg);
  finally
    Nodes.Free;
  end;
end;

function THelpDatabases.ShowHelpForSourcePosition(
  Query: THelpQuerySourcePosition; var ErrMsg: string): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='THelpDatabases.ShowHelpForPascalSource not implemented';
end;

function THelpDatabases.ShowHelpForMessageLine(Query: THelpQueryMessage;
  var ErrMsg: string): TShowHelpResult;
var
  Nodes: THelpNodeQueryList;
begin
  ErrMsg:='';
  Result:=shrSuccess;

  {$IFDEF VerboseLCLHelp}
  debugln('THelpDatabases.ShowHelpForMessageLine A Msg="',Query.WholeMessage,'"');
  {$ENDIF}
  // search node
  Nodes:=nil;
  try
    Result:=GetNodesForMessage(Query.WholeMessage,Query.MessageParts,Nodes,
                               ErrMsg);
    if Result<>shrSuccess then exit;

    // check if at least one node found
    if (Nodes=nil) or (Nodes.Count=0) then begin
      Result:=shrHelpNotFound;
      ErrMsg:='No help found for "'+Query.WholeMessage+'"';
      exit;
    end;

    Result:=ShowHelpForNodes(Query,Nodes,ErrMsg);
  finally
    Nodes.Free;
  end;
end;

function THelpDatabases.ShowHelpForClass(Query: THelpQueryClass;
  var ErrMsg: string): TShowHelpResult;
var
  Nodes: THelpNodeQueryList;
begin
  ErrMsg:='';
  Result:=shrSuccess;

  {$IFDEF VerboseLCLHelp}
  debugln('THelpDatabases.ShowHelpForClass A ',Query.TheClass.ClassName);
  {$ENDIF}
  // search node
  Nodes:=nil;
  try
    Result:=GetNodesForClass(Query.TheClass,Nodes,ErrMsg);
    if Result<>shrSuccess then exit;

    // check if at least one node found
    if (Nodes=nil) or (Nodes.Count=0) then begin
      // no node found for the class is not a bug
      Result:=shrSuccess;
      ErrMsg:='';
      exit;
    end;

    Result:=ShowHelpForNodes(Query,Nodes,ErrMsg);
  finally
    Nodes.Free;
  end;
end;

function THelpDatabases.ShowHelpFile(const Filename, Title, MimeType: string;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=ShowHelp(FilenameToURL(Filename),Title,MimeType,ErrMsg);
end;

function THelpDatabases.ShowHelp(const URL, Title, MimeType: string;
  var ErrMsg: string): TShowHelpResult;
var
  Viewer: THelpViewer;
  Node: THelpNode;
begin
  ErrMsg:='';
  // get a viewer for this file
  Result:=FindViewer(MimeType,ErrMsg,Viewer);
  if Result<>shrSuccess then exit;

  // call viewer
  Node:=nil;
  try
    Node:=THelpNode.CreateURL(nil,Title,URL);
    Result:=Viewer.ShowNode(Node,ErrMsg);
  finally
    Node.Free;
  end;
end;

function THelpDatabases.GetNodesForKeyword(const HelpKeyword: string;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil then new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
begin
  ErrMsg:='';
  for i:=Count-1 downto 0 do begin
    Result:=Items[i].GetNodesForKeyword(HelpKeyword,ListOfNodes,ErrMsg);
    if Result=shrCancel then exit;
  end;
  Result:=shrSuccess;
end;

function THelpDatabases.GetNodesForDirective(const HelpDirective: string;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil then new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
begin
  ErrMsg:='';
  for i:=Count-1 downto 0 do begin
    Result:=Items[i].GetNodesForDirective(HelpDirective,ListOfNodes,ErrMsg);
    if Result=shrCancel then exit;
  end;
  Result:=shrSuccess;
end;

function THelpDatabases.GetNodesForContext(HelpContext: THelpContext;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil then new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
begin
  ErrMsg:='';
  for i:=Count-1 downto 0 do begin
    Result:=Items[i].GetNodesForContext(HelpContext,ListOfNodes,ErrMsg);
    if Result=shrCancel then exit;
  end;
  Result:=shrSuccess;
end;

function THelpDatabases.GetNodesForPascalContexts(
  ListOfPascalHelpContextList: TList; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil then new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
begin
  ErrMsg:='';
  for i:=Count-1 downto 0 do begin
    Result:=Items[i].GetNodesForPascalContexts(ListOfPascalHelpContextList,
                                               ListOfNodes,ErrMsg);
    if Result=shrCancel then exit;
  end;
  Result:=shrSuccess;
end;

function THelpDatabases.GetNodesForClass(AClass: TClass;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil then new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
begin
  ErrMsg:='';
  for i:=Count-1 downto 0 do begin
    Result:=Items[i].GetNodesForClass(AClass,ListOfNodes,ErrMsg);
    if Result=shrCancel then exit;
  end;
  Result:=shrSuccess;
end;

function THelpDatabases.GetNodesForMessage(const AMessage: string;
  MessageParts: TStrings; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil then new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
begin
  ErrMsg:='';
  for i:=Count-1 downto 0 do begin
    Result:=Items[i].GetNodesForMessage(AMessage,MessageParts,ListOfNodes,
                                        ErrMsg);
    if Result=shrCancel then exit;
  end;
  Result:=shrSuccess;
end;

function THelpDatabases.ShowHelpSelector(Query: THelpQuery;
  Nodes: THelpNodeQueryList; var ErrMsg: string;
  var Selection: THelpNodeQuery): TShowHelpResult;
// to override
// default is to always take the first node
begin
  if (Nodes=nil) or (Nodes.Count=0) then begin
    Result:=shrSelectorError;
    Selection:=nil;
    ErrMsg:=rsHelpNoHelpNodesAvailable;
  end else begin
    Selection:=THelpNodeQuery(Nodes[0]);
    Result:=shrSuccess;
    ErrMsg:='';
  end;
end;

procedure THelpDatabases.RegisterHelpDatabaseClass(NewHelpDB: THelpDatabaseClass);
begin
  if FHelpDBClasses=nil then FHelpDBClasses:=TFPList.Create;
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

procedure THelpDatabases.Load(Storage: TConfigStorage);
var
  i: Integer;
  HelpDB: THelpDatabase;
  Path: String;
begin
  for i:=0 to Count-1 do begin
    HelpDB:=Items[i];
    Path:=HelpDB.ID;
    if not IsValidIdent(Path) then continue;
    Storage.AppendBasePath(Path);
    try
      HelpDB.Load(Storage);
    finally
      Storage.UndoAppendBasePath;
    end;
  end;
end;

procedure THelpDatabases.Save(Storage: TConfigStorage);
var
  i: Integer;
  HelpDB: THelpDatabase;
  Path: String;
begin
  for i:=0 to Count-1 do begin
    HelpDB:=Items[i];
    Path:=HelpDB.ID;
    if not IsValidIdent(Path) then continue;
    Storage.AppendBasePath(Path);
    try
      HelpDB.Save(Storage);
    finally
      Storage.UndoAppendBasePath;
    end;
  end;
end;

{ THelpViewers }

function THelpViewers.GetItems(Index: integer): THelpViewer;
begin
  Result:=THelpViewer(FItems[Index]);
end;

constructor THelpViewers.Create;
begin
  FItems:=TFPList.Create;
end;

destructor THelpViewers.Destroy;
begin
  FDestroying:=true;
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure THelpViewers.Clear;
var
  i: Integer;
begin
  i:=Count-1;
  while (i>=0) do begin
    if i<Count then begin
      if Items[i].Owner=nil then begin
        Items[i].Free;
        if fItems=nil then exit;
      end;
      if i<Count then
        FItems[i]:=nil;
    end;
    dec(i);
  end;
  FItems.Clear;
end;

function THelpViewers.Count: integer;
begin
  if fItems<>nil then
    Result:=FItems.Count
  else
    Result:=0;
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
  if FDestroying then exit;
  FItems.Remove(AHelpViewer);
end;

procedure THelpViewers.Load(Storage: TConfigStorage);
var
  i: Integer;
  Viewer: THelpViewer;
  Path: String;
begin
  for i:=0 to Count-1 do begin
    Viewer:=Items[i];
    Path:=Viewer.StorageName;
    if not IsValidIdent(Path) then continue;
    Storage.AppendBasePath(Path);
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
  Path: String;
begin
  for i:=0 to Count-1 do begin
    Viewer:=Items[i];
    Path:=Viewer.StorageName;
    if not IsValidIdent(Path) then continue;
    Storage.AppendBasePath(Path);
    try
      Viewer.Save(Storage);
    finally
      Storage.UndoAppendBasePath;
    end;
  end;
end;

function THelpViewers.IndexOf(AHelpViewer: THelpViewer): integer;
begin
  Result:=FItems.IndexOf(AHelpViewer);
end;

{ THelpViewer }

procedure THelpViewer.SetAutoRegister(const AValue: boolean);
begin
  if FAutoRegister=AValue then exit;
  FAutoRegister:=AValue;
  if not (csDesigning in ComponentState) then begin
    if FAutoRegister then begin
      RegisterSelf;
    end else begin
      UnregisterSelf;
    end;
  end;
end;

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

constructor THelpViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FStorageName:=ClassName;
end;

destructor THelpViewer.Destroy;
begin
  UnregisterSelf;
  FreeAndNil(FSupportedMimeTypes);
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

procedure THelpViewer.RegisterSelf;
begin
  if (HelpViewers<>nil) and (HelpViewers.IndexOf(Self)>=0) then
    raise EHelpSystemException.Create('help viewer is already registered');
  CreateLCLHelpSystem;
  HelpViewers.RegisterViewer(Self);
end;

procedure THelpViewer.UnregisterSelf;
begin
  if (HelpViewers=nil) or (HelpViewers.IndexOf(Self)<0) then exit;
  HelpViewers.UnregisterViewer(Self);
  FreeUnusedLCLHelpSystem;
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
  Result:=FHelpType in [hntURL,hntURLIDContext,hntURLID,hntURLContext];
end;

function THelpNode.IDValid: boolean;
begin
  Result:=FHelpType in [hntURLIDContext,hntURLID,hntID];
end;

function THelpNode.ContextValid: boolean;
begin
  Result:=FHelpType in [hntURLIDContext,hntURLContext,hntContext];
end;

function THelpNode.AsString: string;
begin
  Result:=Title;
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
    FID:=Node.ID;
    FContext:=Node.Context;
  end else
    inherited Assign(Source);
end;

{ THelpDBItem }

constructor THelpDBItem.Create(TheNode: THelpNode);
begin
  Node:=TheNode
end;

destructor THelpDBItem.Destroy;
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
  // to prevent freeing uninitialized strings, initialize the new strings to nil
  FillChar(fItems[FCount-1], SizeOf(TPascalHelpContext), 0);
  fItems[FCount-1]:=Context;
end;

procedure TPascalHelpContextList.Add(Descriptor: TPascalHelpContextType;
  const Context: string);
var
  CurContext: TPascalHelpContext;
begin
  CurContext.Descriptor:=Descriptor;
  CurContext.Context:=Context;
  Add(CurContext);
end;

procedure TPascalHelpContextList.Insert(Index: integer;
  const Context: TPascalHelpContext);
begin
  inc(FCount);
  ReAllocMem(fItems,SizeOf(TPascalHelpContext)*FCount);
  if Index<FCount-1 then
    System.Move(fItems[Index],fItems[Index+1],
                SizeOf(TPascalHelpContext)*(FCount-Index-1));
  // to prevent freeing uninitialized strings, initialize the new strings to nil
  FillChar(fItems[Index], SizeOf(TPascalHelpContext), 0);
  fItems[Index]:=Context;
end;

procedure TPascalHelpContextList.Clear;
var
  Index: Integer;
begin
  // Set all item strings to '', so fpc will finalize them.
  for Index := 0 to FCount-1 do
    fItems[Index].Context := '';
  ReAllocMem(fItems,0);
end;

destructor TPascalHelpContextList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TPascalHelpContextList.IsEqual(QueryItem: THelpQueryItem): boolean;
begin
  Result:=(QueryItem is TPascalHelpContextList)
          and (CompareList(TPascalHelpContextList(QueryItem))=0);
end;

function TPascalHelpContextList.CompareList(AList: TPascalHelpContextList
  ): integer;
var
  i: Integer;
begin
  i:=0;
  while (i<Count) and (i<AList.Count) do begin
    if fItems[i].Descriptor<AList.fItems[i].Descriptor then begin
      Result:=1;
      exit;
    end else if fItems[i].Descriptor>AList.fItems[i].Descriptor then begin
      Result:=-1;
      exit;
    end else begin
      Result:=CompareText(fItems[i].Context,AList.fItems[i].Context);
      if Result<>0 then exit;
    end;
    inc(i);
  end;
  if Count>i then
    Result:=-1
  else
    Result:=1;
end;

function TPascalHelpContextList.AsString: string;
var
  i: Integer;
  Item: TPascalHelpContext;
  Filename: String;
begin
  Result:='';
  i:=0;
  while (i<Count) and (Items[i].Descriptor=pihcFilename) do begin
    Filename:=Items[i].Context;
    inc(i);
  end;
  while i<Count do begin
    Item:=Items[i];
    case Item.Descriptor of
    pihcFilename: Result:=Result+Item.Context;
    pihcSourceName: ;
    pihcProperty: Result:=Result+' property '+Item.Context;
    pihcProcedure: Result:=Result+' procedure/function '+Item.Context;
    pihcParameterList: Result:=Result+Item.Context;
    pihcVariable: Result:=Result+' var '+Item.Context;
    pihcType: Result:=Result+' type '+Item.Context;
    pihcConst: Result:=Result+' const '+Item.Context;
    end;
    //DebugLn(['TPascalHelpContextList.AsString ',i,' ',Item.Descriptor,' ',Result]);
    inc(i);
  end;
  if Filename<>'' then
    Result:=Result+' in '+Filename;
end;

{ THelpDBISourceFile }

procedure THelpDBISourceFile.SetFilename(const AValue: string);
begin
  FFilename:=AValue;
end;

constructor THelpDBISourceFile.Create(TheNode: THelpNode;
  const TheFilename: string);
begin
  inherited Create(TheNode);
  FFilename:=TrimFilename(GetForcedPathDelims(TheFilename));
end;

function THelpDBISourceFile.FileMatches(const AFilename: string): boolean;
begin
  if (FFilename<>'') and (AFilename<>'') then
    Result:=CompareFilenames(GetFullFilename,AFilename)=0
  else
    Result:=false;
end;

function THelpDBISourceFile.GetFullFilename: string;
var
  BaseDir: String;
  ExpFilename: String;
begin
  ExpFilename:=FFilename;
  //DebugLn(['THelpDBISourceFile.GetFullFilename ExpFilename="',ExpFilename,'" HelpDatabases=',DbgSName(HelpDatabases)]);
  if (HelpDatabases<>nil) then
    HelpDatabases.SubstituteMacros(ExpFilename);
  //DebugLn(['THelpDBISourceFile.GetFullFilename substituted ',ExpFilename]);
  ExpFilename:=TrimFilename(GetForcedPathDelims(ExpFilename));
  if FilenameIsAbsolute(ExpFilename) then
    Result:=ExpFilename
  else begin
    BaseDir:=GetBasePath;
    Result:=AppendPathDelim(BaseDir)+ExpFilename;
  end;
end;

function THelpDBISourceFile.GetBasePath: string;
begin
  if BasePathObject=nil then
    Result:=''
  else
    Result:=AppendPathDelim(TrimFilename(GetForcedPathDelims(
             HelpDatabases.GetBaseDirectoryForBasePathObject(BasePathObject))));
end;

{ THelpDBISourceDirectory }

constructor THelpDBISourceDirectory.Create(TheNode: THelpNode;
  const Directory, TheFileMask: string; Recursive: boolean);
begin
  inherited Create(TheNode,Directory);
  FFileMask:=GetForcedPathDelims(TheFileMask);
  WithSubDirectories:=Recursive;
end;

function THelpDBISourceDirectory.FileMatches(const AFilename: string
  ): boolean;
var
  TheDirectory: String;
begin
  Result:=false;
  //debugln('THelpDBISourceDirectory.FileMatches AFilename="',AFilename,'" FFilename="',FFilename,'"');
  if (FFilename='') or (AFilename='') then exit;
  TheDirectory:=GetFullFilename;
  if TheDirectory='' then begin
    {$IFNDEF DisableChecks}
    DebugLn(['WARNING: THelpDBISourceDirectory.FileMatches ',DbgSName(Self),' Filename="',Filename,'" -> ""']);
    {$ENDIF}
    exit;
  end;
  //debugln('THelpDBISourceDirectory.FileMatches TheDirectory="',TheDirectory,'" WithSubDirectories=',dbgs(WithSubDirectories));
  if WithSubDirectories then begin
    if not FileIsInPath(AFilename,TheDirectory) then exit;
  end else begin
    if not FileIsInDirectory(AFilename,TheDirectory) then exit;
  end;
  //debugln('THelpDBISourceDirectory.FileMatches FileMask="',FileMask,'"');
  if (FileMask<>'')
  and (not MatchesMaskList(ExtractFilename(AFilename),FileMask)) then exit;
  //debugln('THelpDBISourceDirectory.FileMatches Success');
  Result:=true;
end;

{ THelpQueryNode }

constructor THelpQueryNode.Create(const TheHelpDatabaseID: THelpDatabaseID;
  const TheNode: THelpNode);
begin
  inherited Create(TheHelpDatabaseID);
  FNode:=TheNode;
end;

{ THelpBasePathObject }

procedure THelpBasePathObject.SetBasePath(const AValue: string);
begin
  if FBasePath=AValue then exit;
  FBasePath:=AValue;
end;

constructor THelpBasePathObject.Create;
begin

end;

constructor THelpBasePathObject.Create(const TheBasePath: string);
begin
  BasePath:=TheBasePath;
end;

{ THelpNodeQuery }

constructor THelpNodeQuery.Create;
begin

end;

constructor THelpNodeQuery.Create(TheNode: THelpNode;
  TheQueryItem: THelpQueryItem);
begin
  Create;
  FNode:=TheNode;
  FQueryItem:=TheQueryItem;
end;

function THelpNodeQuery.IsEqual(TheNode: THelpNode; TheQueryItem: THelpQueryItem
  ): boolean;
begin
  Result:=(Node=TheNode) and (QueryItem.IsEqual(TheQueryItem))
end;

function THelpNodeQuery.IsEqual(NodeQuery: THelpNodeQuery): boolean;
begin
  Result:=IsEqual(NodeQuery.Node,NodeQuery.QueryItem)
end;

function THelpNodeQuery.AsString: string;
begin
  Result:=Node.AsString;
  if QueryItem<>nil then
    Result:=Result+' ('+QueryItem.AsString+')';
end;

{ THelpNodeQueryList }

function THelpNodeQueryList.GetItems(Index: integer): THelpNodeQuery;
begin
  Result:=THelpNodeQuery(fItems[Index]);
end;

procedure THelpNodeQueryList.SetItems(Index: integer;
  const AValue: THelpNodeQuery);
begin
  fItems[Index]:=AValue;
end;

constructor THelpNodeQueryList.Create;
begin
  fItems:=TFPList.Create;
end;

destructor THelpNodeQueryList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

function THelpNodeQueryList.Count: integer;
begin
  Result:=fItems.Count;
end;

function THelpNodeQueryList.Add(NodeQuery: THelpNodeQuery): integer;
begin
  Result:=fItems.Add(NodeQuery);
end;

function THelpNodeQueryList.Add(Node: THelpNode; QueryItem: THelpQueryItem
  ): integer;
begin
  Result:=Add(THelpNodeQuery.Create(Node,QueryItem));
end;

procedure THelpNodeQueryList.Delete(Index: integer);
begin
  TObject(fItems[Index]).Free;
  fItems.Delete(Index);
end;

function THelpNodeQueryList.IndexOf(NodeQuery: THelpNodeQuery): integer;
begin
  Result:=Count;
  while (Result>=0) and (not Items[Result].IsEqual(NodeQuery)) do
    dec(Result);
end;

function THelpNodeQueryList.IndexOf(Node: THelpNode; QueryItem: THelpQueryItem
  ): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (not Items[Result].IsEqual(Node,QueryItem)) do
    dec(Result);
end;

procedure THelpNodeQueryList.Clear;
var
  i: Integer;
begin
  for i:=0 to Count-1 do TObject(fItems[i]).Free;
  fItems.Clear;
end;

{ THelpQueryItem }

function THelpQueryItem.IsEqual(QueryItem: THelpQueryItem): boolean;
begin
  Result:=AsString=QueryItem.AsString;
end;

{ THelpBaseURLObject }

procedure THelpBaseURLObject.SetBaseURL(const AValue: string);
begin
  if FBaseURL=AValue then exit;
  FBaseURL:=AValue;
end;

constructor THelpBaseURLObject.Create;
begin

end;

constructor THelpBaseURLObject.Create(const TheBaseURL: string);
begin
  BaseURL:=TheBaseURL;
end;

end.

