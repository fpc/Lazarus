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
  Classes, SysUtils, Controls;

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
  
  TPascalHelpContextType = (
    pihcFilename,
    pihcSourceName,  // unit name, library name, ..
    pihcClass,
    pihcObject,
    pihcInterface,
    pihcRecord,
    pihcEnumeration,
    pihcProcedure,
    pihcParameterList,
    pihcIdentifier
    );
  TPascalHelpContext = record
    Descriptor: TPascalHelpContextType;
    Context: string;
  end;
  TPascalHelpContextPtr = ^TPascalHelpContext;


  THelpDatabase = class;

  { THelpNode
    A help node is a position/place in a help database.
    For example it points to Help file or to a Link on a HTML file. }
  
  THelpNodeType = (
    hntFile,     // Filename valid, ignore Link and ID
    hntFileLink, // Filename and Link valid, ignore ID
    hntFileID    // Filename and ID valid, ignore Link
    );

  THelpNode = class(TPersistent)
  private
    FFilename: string;
    FHelpType: THelpNodeType;
    fID: integer;
    FLink: string;
    FOwner: THelpDatabase;
    FTitle: string;
  public
    constructor Create(TheOwner: THelpDatabase;
                       const TheTitle, TheFilename: string);
    constructor Create(TheOwner: THelpDatabase; const TheTitle, TheFilename,
                       TheLink: string);
    constructor Create(TheOwner: THelpDatabase; const TheTitle,
                       TheFilename: string; TheID: integer);
  public
    property Owner: THelpDatabase read FOwner write FOwner;
  published
    property Title: string read FTitle write FTitle;
    property HelpType: THelpNodeType read FHelpType write FHelpType;
    property Filename: string read FFilename write FFilename;
    property ID: integer read fID write fID;
    property Link: string read FLink write FLink;
  end;


  { THelpDBSearchItem
    Base class for registered search items associated with a THelpDatabase.
    See THelpDBSISourceDirectory for an example.
    Node is optional, pointing to a help page about the help item. }

  THelpDBSearchItem = class(TPersistent)
  private
    FNode: THelpNode;
  published
    property Node: THelpNode read FNode write FNode;
  end;
  
  
  { THelpDBSISourceFile
    Used by the IDE to search for help for a sourcefile
    If Filename is relative, the BasePathObject is used to get a base directory.
    
    For example: If BasePathObject is a TLazPackage the Filename is relative to
    the directory of the .lpk file }

  THelpDBSISourceFile = class(THelpDBSearchItem)
  private
    FBasePathObject: TObject;
    FFilename: string;
  published
    property BasePathObject: TObject read FBasePathObject write FBasePathObject;
    property Filename: string read FFilename write FFilename;
  end;
  

  { THelpDBSISourceDirectory
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
  published
    property FileMask: string read FFileMask write FFileMask;
    property WithSubDirectories: boolean read FWithSubDirectories
                                         write FWithSubDirectories;
  end;
  

  { THelpDBSIClass
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

  THelpDatabase = class(TPersistent)
  private
    FBasePathObject: TObject;
    FID: THelpDatabaseID;
    FDatabases: THelpDatabases;
    FRefCount: integer;
    FSearchItems: TList;
    FSupportedMimeTypes: TStrings;
    procedure SetID(const AValue: THelpDatabaseID);
    procedure SetDatabases(const AValue: THelpDatabases);
  public
    constructor Create(TheID: THelpDatabaseID);
    destructor Destroy; override;
    procedure Reference;
    procedure RegisterSelf;
    procedure Release;
    procedure UnregisterSelf;
    function Registered: boolean;
    function CanShowTableOfContents: boolean; virtual;
    procedure ShowTableOfContents; virtual;
    function ShowHelp(BaseNode, NewNode: THelpNode;
                      var ErrMsg: string): TShowHelpResult;
    function SupportsMimeType(const AMimeType: string): boolean; virtual;
    function GetNodesForKeyword(const HelpKeyword: string;
                                var ListOfNodes: TList; var ErrMsg: string
                                ): TShowHelpResult; virtual;
    function GetNodesForContext(HelpContext: THelpContext;
                                var ListOfNodes: TList; var ErrMsg: string
                                ): TShowHelpResult; virtual;
  public
    // registration
    procedure RegisterItem(NewItem: THelpDBSearchItem);
    procedure UnregisterItem(AnItem: THelpDBSearchItem);
    function RegisteredItemCount: integer;
    function GetRegisteredItem(Index: integer): THelpDBSearchItem;
  public
    property Databases: THelpDatabases read FDatabases write SetDatabases;
    property ID: THelpDatabaseID read FID write SetID;
    property SupportedMimeTypes: TStrings read FSupportedMimeTypes;
    property BasePathObject: TObject read FBasePathObject write FBasePathObject;
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
    // find databases
    function FindDatabase(ID: THelpDatabaseID): THelpDatabase;
    function IndexOf(ID: THelpDatabaseID): integer;
  public
    // table of content
    function ShowTableOfContents(var ErrMsg: string): TShowHelpResult;
  public
    // show help for ...
    function ShowHelpForNodes(Nodes: TList; var ErrMsg: string): TShowHelpResult;
    function ShowHelpForContext(HelpDatabaseID: THelpDatabaseID;
                                HelpContext: THelpContext;
                                var ErrMsg: string): TShowHelpResult;
    function ShowHelpForKeyword(HelpDatabaseID: THelpDatabaseID;
                                const HelpKeyword: string;
                                var ErrMsg: string): TShowHelpResult;
    function ShowHelpForPascalSource(ContextList: TPascalHelpContextPtr;
                                     var ErrMsg: string): TShowHelpResult;
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
    FPreferredLanguage: string;
    procedure SetPreferredLanguage(const AValue: string);
  protected
    FSupportedMimeTypes: TStrings;
  public
    destructor Destroy; override;
    function SupportsTableOfContents: boolean; virtual;
    procedure ShowTableOfContents(Node: THelpNode); virtual;
    function SupportsMimeType(const AMimeType: string): boolean; virtual;
    procedure ShowNode(Node: THelpNode); virtual;
    procedure Hide; virtual;
  public
    property SupportedMimeTypes: TStrings read FSupportedMimeTypes;
    property PreferredLanguage: string read FPreferredLanguage
                                       write SetPreferredLanguage;
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
    function GetHelpViewers(const MimeType: string): TList;
    procedure RegisterViewer(AHelpViewer: THelpViewer);
    procedure UnregisterViewer(AHelpViewer: THelpViewer);
  public
    property Items[Index: integer]: THelpViewer read GetItems;
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
function ShowTableOfContents(var ErrMsg: string): TShowHelpResult;

// help by ID
function ShowHelpForContext(HelpDatabaseID: THelpDatabaseID;
  HelpContext: THelpContext; var ErrMsg: string): TShowHelpResult;
function ShowHelpForContext(HelpContext: THelpContext; var ErrMsg: string
  ): TShowHelpResult;

// help by keyword
function ShowHelpForKeyword(HelpDatabaseID: THelpDatabaseID;
  const HelpKeyword: string; var ErrMsg: string): TShowHelpResult;
function ShowHelpForKeyword(const HelpKeyword: string; var ErrMsg: string
  ): TShowHelpResult;

// help for pascal identifier
function ShowHelpForPascalSource(ContextList: TPascalHelpContextPtr;
  var ErrMsg: string): TShowHelpResult;

// help for messages (compiler messages, codetools messages, make messages, ...)
function ShowHelpForMessageLine(const MessageLine: string;
  var ErrMsg: string): TShowHelpResult;


implementation


function ShowTableOfContents(var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpDatabases.ShowTableOfContents(ErrMsg);
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

function ShowHelpForPascalSource(ContextList: TPascalHelpContextPtr;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpDatabases.ShowHelpForPascalSource(ContextList,ErrMsg);
end;

function ShowHelpForMessageLine(const MessageLine: string;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpDatabases.ShowHelpForMessageLine(MessageLine,ErrMsg);
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
  if FDatabases<>nil then FDatabases.DoUnregisterDatabase(Self);
  FDatabases:=AValue;
  if FDatabases<>nil then FDatabases.DoRegisterDatabase(Self);
end;

constructor THelpDatabase.Create(TheID: THelpDatabaseID);
begin
  FID:=TheID;
end;

destructor THelpDatabase.Destroy;
begin
  if Databases<>nil then UnregisterSelf;
  FSupportedMimeTypes.Free;
  FSearchItems.Free;
  inherited Destroy;
end;

procedure THelpDatabase.RegisterSelf;
begin
  if Databases<>nil then
    raise EHelpSystemException.Create(ID+': Already registered');
  Databases:=HelpDatabases;
end;

procedure THelpDatabase.UnregisterSelf;
begin
  if Databases=nil then
    raise EHelpSystemException.Create(ID+': Not registered');
  Databases:=nil;
end;

function THelpDatabase.Registered: boolean;
begin
  Result:=Databases<>nil;
end;

function THelpDatabase.CanShowTableOfContents: boolean;
begin
  Result:=false;
end;

procedure THelpDatabase.ShowTableOfContents;
begin
  // for descendents to override
end;

function THelpDatabase.ShowHelp(BaseNode, NewNode: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
begin
  ErrMsg:='';
  Result:=shrContextNotFound;
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
begin
  Result:=shrSuccess;
  ErrMsg:='';
end;

function THelpDatabase.GetNodesForContext(HelpContext: THelpContext;
  var ListOfNodes: TList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
begin
  Result:=shrSuccess;
  ErrMsg:='';
end;

procedure THelpDatabase.RegisterItem(NewItem: THelpDBSearchItem);
begin
  if FSearchItems=nil then FSearchItems:=TList.Create;
  if FSearchItems.IndexOf(NewItem)<0 then
    FSearchItems.Add(NewItem);
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

function THelpDatabases.IndexOf(ID: THelpDatabaseID): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(ID,Items[Result].ID)<>0) do
    dec(Result);
end;

function THelpDatabases.ShowTableOfContents(var ErrMsg: string
  ): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='THelpDatabases.ShowTableOfContents not implemented';
  // ToDo
end;

function THelpDatabases.ShowHelpForNodes(Nodes: TList; var ErrMsg: string
  ): TShowHelpResult;
var
  Node: THelpNode;
begin
  // check if several nodes found
  if (Nodes.Count>1) then begin
    Result:=ShowHelpSelector(Nodes,ErrMsg,Node);
    if Result<>shrSuccess then exit;
    if Node=nil then exit;
  end else begin
    Node:=THelpNode(Nodes[0]);
  end;

  // show node
  if Node.Owner=nil then begin
    Result:=shrDatabaseNotFound;
    ErrMsg:='Help node not found';
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
  Result:=shrHelpNotFound;
  
  // search node
  Nodes:=nil;
  try
    if HelpDatabaseID<>'' then begin
      HelpDB:=FindDatabase(HelpDatabaseID);
      if HelpDB=nil then begin
        Result:=shrDatabaseNotFound;
        exit;
      end;
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
      ErrMsg:='Help context '+IntToStr(HelpContext)+' not found.';
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
  Result:=shrHelpNotFound;

  // search node
  Nodes:=nil;
  try
    if HelpDatabaseID<>'' then begin
      HelpDB:=FindDatabase(HelpDatabaseID);
      if HelpDB=nil then begin
        Result:=shrDatabaseNotFound;
        exit;
      end;
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
      ErrMsg:='Help keyword '+HelpKeyword+' not found.';
      exit;
    end;

    Result:=ShowHelpForNodes(Nodes,ErrMsg);
  finally
    Nodes.Free;
  end;
end;

function THelpDatabases.ShowHelpForPascalSource(
  ContextList: TPascalHelpContextPtr; var ErrMsg: string): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='THelpDatabases.ShowHelpForPascalSource not implemented yet';
  // ToDo
end;

function THelpDatabases.ShowHelpForMessageLine(const MessageLine: string;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='THelpDatabases.ShowHelpForMessageLine not implemented yet';
  // ToDo
end;

function THelpDatabases.ShowHelpForClass(const AClass: TClass;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='THelpDatabases.ShowHelpForClass not implemented yet';
  // ToDo
end;

function THelpDatabases.GetNodesForKeyword(const HelpKeyword: string;
  var ListOfNodes: TList; var ErrMsg: string): TShowHelpResult;
// if ListOfNodes<>nil then new nodes will be appended
// if ListOfNodes=nil and nodes exists a new list will be created
var
  i: Integer;
begin
  Result:=shrSuccess;
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
  for i:=Count-1 downto 0 do begin
    Result:=Items[i].GetNodesForContext(HelpContext,ListOfNodes,ErrMsg);
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
begin
  while (Count>0) do Items[Count-1].Free;
  FItems.Clear;
end;

function THelpViewers.Count: integer;
begin
  Result:=FItems.Count;
end;

function THelpViewers.GetHelpViewers(const MimeType: string): TList;
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

{ THelpViewer }

procedure THelpViewer.SetPreferredLanguage(const AValue: string);
begin
  if FPreferredLanguage=AValue then exit;
  FPreferredLanguage:=AValue;
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
  // ToDo
end;

function THelpViewer.SupportsMimeType(const AMimeType: string): boolean;
begin
  Result:=false;
  if FSupportedMimeTypes<>nil then
    Result:=(FSupportedMimeTypes.IndexOf(AMimeType)>=0);
end;

procedure THelpViewer.ShowNode(Node: THelpNode);
begin
  // ToDo
end;

procedure THelpViewer.Hide;
begin
  // override this
end;

{ THelpNode }

constructor THelpNode.Create(TheOwner: THelpDatabase; const TheTitle,
  TheFilename: string);
begin
  FHelpType:=hntFile;
  FTitle:=TheTitle;
  FFilename:=TheFilename;
end;

constructor THelpNode.Create(TheOwner: THelpDatabase; const TheTitle,
  TheFilename, TheLink: string);
begin
  FHelpType:=hntFileLink;
  FTitle:=TheTitle;
  FFilename:=TheFilename;
  FLink:=TheLink;
end;

constructor THelpNode.Create(TheOwner: THelpDatabase;
  const TheTitle, TheFilename: string; TheID: integer);
begin
  FHelpType:=hntFileID;
  FTitle:=TheTitle;
  FFilename:=TheFilename;
  FID:=TheID;
end;

initialization
  HelpDatabases:=nil;

end.

