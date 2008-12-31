{
 /***************************************************************************
                                 helpintfs.pas
                                 -------------
                             Component Library Code


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
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
    Interfaces to define the abstract HelpSystem.
    You can create your own HelpSystem based on these interfaces
    or use the LCL help system in lazhelpintf.pas.
    The THTMLHelpDatabase and THTMLBrowserHelpViewer in lazhelphtml.pas use the
    LCL help system.
    
    To create your own help system, implement a descendant of THelpManager.
}
unit HelpIntfs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc;
  
type
  // All help-specific errors should be thrown as this type.
  EHelpSystemException = class(Exception);

  TShowHelpResult = (
    shrNone,
    shrSuccess,
    shrCancel,
    shrDatabaseNotFound,
    shrContextNotFound,
    shrViewerNotFound,
    shrHelpNotFound,
    shrViewerError,
    shrSelectorError
    );
  TShowHelpResults = set of TShowHelpResult;

  THelpDatabaseID = string;

  { THelpQuery }

  THelpQuery = class(TPersistent)
  private
    FHelpDatabaseID: THelpDatabaseID;
  public
    constructor Create(const TheHelpDatabaseID: THelpDatabaseID);
    property HelpDatabaseID: THelpDatabaseID read FHelpDatabaseID
                                             write FHelpDatabaseID;
  end;


  { THelpQueryTOC }

  THelpQueryTOC = class(THelpQuery)
  end;


  { THelpQueryContext }

  THelpQueryContext = class(THelpQuery)
  private
    FContext: THelpContext;
  public
    constructor Create(const TheHelpDatabaseID: THelpDatabaseID;
                       const TheContext: THelpContext);
    property Context: THelpContext read FContext write FContext;
  end;


  { THelpQueryKeyword }

  THelpQueryKeyword = class(THelpQuery)
  private
    FKeyword: string;
  public
    constructor Create(const TheHelpDatabaseID: THelpDatabaseID;
                       const TheKeyWord: string);
    property Keyword: string read FKeyword write FKeyword;
  end;


  { THelpQuerySourcePosition }

  THelpQuerySourcePosition = class(THelpQuery)
  private
    FFilename: string;
    FSourcePosition: TPoint;
  public
    constructor Create(const TheHelpDatabaseID: THelpDatabaseID;
                       const TheFilename: string; const SrcPos: TPoint);
    property Filename: string read FFilename write FFilename;
    property SourcePosition: TPoint read FSourcePosition write FSourcePosition;
  end;


  { THelpQueryPascalContexts }

  THelpQueryPascalContexts = class(THelpQuerySourcePosition)
  private
    FContextLists: TList;
  public
    constructor Create(const TheHelpDatabaseID: THelpDatabaseID;
                       const TheFilename: string; const SrcPos: TPoint;
                       ContextLists: TList);
    property ListOfPascalHelpContextList: TList read FContextLists
                                                write FContextLists;
  end;


  { THelpQueryMessage
    A query for messages, like the compiler warnings and errors.

    'WholeMessage' is the complete line as string.

    'MessageParts' can be a list of Name=Value pairs, that has been extracted
    by the IDE. Common names and values are:
      Name    | Value
      --------|-----------------------------------------------------------------
      Stage    Indicates what part of the build process the message
               belongs to. Common values are 'FPC', 'Linker' or 'make'
      Type     For FPC: 'Hint', 'Note', 'Warning', 'Error', 'Fatal', 'Panic',
               'Compiling', 'Assembling'
               For make:
               For Linker:
      Line     An integer for the linenumber as given by FPC in brackets.
      Column   An integer for the column as given by FPC in brackets.
      Message  The message text without other parsed items.


    Example:
      Message written by FPC:
        unit1.pas(21,3) Warning: unit buttons not used

      Results in
        Stage=FPC
        Type=Warning
        Line=21
        Column=3
        Message=unit buttons not used

    }

  THelpQueryMessage = class(THelpQuery)
  private
    FMessageParts: TStrings;
    FWholeMessage: string;
  public
    constructor Create(const TheHelpDatabaseID: THelpDatabaseID;
                       const TheMessage: string;
                       TheMessageParts: TStrings);
    destructor Destroy; override;
    property WholeMessage: string read FWholeMessage write FWholeMessage;
    property MessageParts: TStrings read FMessageParts write FMessageParts;
  end;


  { THelpQueryClass }

  THelpQueryClass = class(THelpQuery)
  private
    FTheClass: TClass;
  public
    constructor Create(const TheHelpDatabaseID: THelpDatabaseID;
                       const AClass: TClass);
    property TheClass: TClass read FTheClass write FTheClass;
  end;


  { THelpManager }

  THelpManager = class(TObject)
  public
    function DoHelpNotFound(var ErrMsg: string): TShowHelpResult;
    function ShowTableOfContents(var ErrMsg: string): TShowHelpResult; virtual;
    procedure ShowError(ShowResult: TShowHelpResult; const ErrMsg: string); virtual; abstract;
    // show help for ...
    function ShowHelpForQuery(Query: THelpQuery; AutoFreeQuery: boolean;
                              var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpForContext(Query: THelpQueryContext;
                                var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpForKeyword(Query: THelpQueryKeyword;
                                var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpForPascalContexts(Query: THelpQueryPascalContexts;
                                       var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpForSourcePosition(Query: THelpQuerySourcePosition;
                                       var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpForMessageLine(Query: THelpQueryMessage;
                                    var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpForClass(Query: THelpQueryClass;
                              var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelpFile(const Filename, Title, MimeType: string;
                      var ErrMsg: string): TShowHelpResult; virtual;
    function ShowHelp(const URL, Title, MimeType: string;
                      var ErrMsg: string): TShowHelpResult; virtual;
  end;

var
  HelpManager: THelpManager = nil;// set by the IDE

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

// help for pascal sources
function ShowHelpForPascalContexts(const Filename: string;
  const SourcePosition: TPoint; ListOfPascalHelpContextList: TList;
  var ErrMsg: string): TShowHelpResult;
function ShowHelpOrErrorForSourcePosition(const Filename: string;
  const SourcePosition: TPoint): TShowHelpResult;

// help for messages (compiler messages, codetools messages, make messages, ...)
function ShowHelpForMessageLine(const MessageLine: string;
  MessageParts: TStrings; var ErrMsg: string): TShowHelpResult;
function ShowHelpOrErrorForMessageLine(const MessageLine: string;
  MessageParts: TStrings): TShowHelpResult;

// view help
function ShowHelpFile(const Filename, Title, MimeType: string;
  var ErrMsg: string): TShowHelpResult;
function ShowHelpFileOrError(const Filename, Title, MimeType: string
  ): TShowHelpResult;
function ShowHelp(const URL, Title, MimeType: string;
  var ErrMsg: string): TShowHelpResult;
function ShowHelpOrError(const URL, Title, MimeType: string
  ): TShowHelpResult;


implementation


function ShowTableOfContents: TShowHelpResult;
var
  ErrMsg: String;
begin
  ErrMsg:='';
  Result:=ShowTableOfContents(ErrMsg);
  HelpManager.ShowError(Result,ErrMsg);
end;

function ShowTableOfContents(var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpManager.ShowTableOfContents(ErrMsg);
end;

function ShowHelpOrErrorForContext(HelpDatabaseID: THelpDatabaseID;
  HelpContext: THelpContext): TShowHelpResult;
var
  ErrMsg: String;
begin
  ErrMsg:='';
  Result:=ShowHelpForContext(HelpDatabaseID,HelpContext,ErrMsg);
  HelpManager.ShowError(Result,ErrMsg);
end;

function ShowHelpForContext(HelpDatabaseID: THelpDatabaseID;
  HelpContext: THelpContext; var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpManager.ShowHelpForQuery(
            THelpQueryContext.Create(HelpDatabaseID,HelpContext),
            true,ErrMsg);
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
  HelpManager.ShowError(Result,ErrMsg);
end;

function ShowHelpForKeyword(HelpDatabaseID: THelpDatabaseID;
  const HelpKeyword: string; var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpManager.ShowHelpForQuery(
            THelpQueryKeyword.Create(HelpDatabaseID,HelpKeyword),
            true,ErrMsg);
end;

function ShowHelpForKeyword(const HelpKeyword: string; var ErrMsg: string
  ): TShowHelpResult;
begin
  Result:=ShowHelpForKeyword('',HelpKeyword,ErrMsg);
end;

function ShowHelpForPascalContexts(const Filename: string;
  const SourcePosition: TPoint; ListOfPascalHelpContextList: TList;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpManager.ShowHelpForQuery(
            THelpQueryPascalContexts.Create('',Filename,
                                    SourcePosition,ListOfPascalHelpContextList),
            true,ErrMsg);
end;

function ShowHelpOrErrorForSourcePosition(const Filename: string;
  const SourcePosition: TPoint): TShowHelpResult;
var
  ErrMsg: String;
begin
  ErrMsg:='';
  Result:=HelpManager.ShowHelpForQuery(
            THelpQuerySourcePosition.Create('',Filename,
                                            SourcePosition),
            true,ErrMsg);
  HelpManager.ShowError(Result,ErrMsg);
end;

function ShowHelpForMessageLine(const MessageLine: string;
  MessageParts: TStrings; var ErrMsg: string): TShowHelpResult;
// MessageParts will be freed
begin
  Result:=HelpManager.ShowHelpForQuery(
            THelpQueryMessage.Create('',MessageLine,MessageParts),
            true,ErrMsg);
end;

function ShowHelpOrErrorForMessageLine(const MessageLine: string;
  MessageParts: TStrings): TShowHelpResult;
var
  ErrMsg: String;
begin
  ErrMsg:='';
  Result:=ShowHelpForMessageLine(MessageLine,MessageParts,ErrMsg);
  //debugln(['ShowHelpOrErrorForMessageLine Result=',ord(Result),' ErrMsg=',ErrMsg,' ',dbgsName(HelpManager)]);
  HelpManager.ShowError(Result,ErrMsg);
end;

function ShowHelpFile(const Filename, Title, MimeType: string;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpManager.ShowHelpFile(Filename,Title,MimeType,ErrMsg);
end;

function ShowHelpFileOrError(const Filename, Title, MimeType: string
  ): TShowHelpResult;
var
  ErrMsg: String;
begin
  ErrMsg:='';
  Result:=ShowHelpFile(Filename,Title,MimeType,ErrMsg);
  HelpManager.ShowError(Result,ErrMsg);
end;

function ShowHelp(const URL, Title, MimeType: string; var ErrMsg: string
  ): TShowHelpResult;
begin
  Result:=HelpManager.ShowHelp(URL,Title,MimeType,ErrMsg);
end;

function ShowHelpOrError(const URL, Title, MimeType: string): TShowHelpResult;
var
  ErrMsg: String;
begin
  ErrMsg:='';
  Result:=ShowHelp(URL,Title,MimeType,ErrMsg);
  HelpManager.ShowError(Result,ErrMsg);
end;

{ THelpQuery }

constructor THelpQuery.Create(const TheHelpDatabaseID: THelpDatabaseID);
begin
  FHelpDatabaseID:=TheHelpDatabaseID;
end;

{ THelpQueryContext }

constructor THelpQueryContext.Create(const TheHelpDatabaseID: THelpDatabaseID;
  const TheContext: THelpContext);
begin
  inherited Create(TheHelpDatabaseID);
  FContext:=TheContext;
end;

{ THelpQueryKeyword }

constructor THelpQueryKeyword.Create(const TheHelpDatabaseID: THelpDatabaseID;
  const TheKeyWord: string);
begin
  inherited Create(TheHelpDatabaseID);
  FKeyword:=TheKeyWord;
end;

{ THelpQuerySourcePosition }

constructor THelpQuerySourcePosition.Create(
  const TheHelpDatabaseID: THelpDatabaseID; const TheFilename: string;
  const SrcPos: TPoint);
begin
  inherited Create(TheHelpDatabaseID);
  FFilename:=TheFilename;
  FSourcePosition:=SrcPos;
end;

{ THelpQueryPascalContext }

constructor THelpQueryPascalContexts.Create(
  const TheHelpDatabaseID: THelpDatabaseID; const TheFilename: string;
  const SrcPos: TPoint; ContextLists: TList);
begin
  inherited Create(TheHelpDatabaseID,TheFilename,SrcPos);
  FContextLists:=ContextLists;
end;

{ THelpQueryMessage }

constructor THelpQueryMessage.Create(const TheHelpDatabaseID: THelpDatabaseID;
  const TheMessage: string; TheMessageParts: TStrings);
begin
  inherited Create(TheHelpDatabaseID);
  FWholeMessage:=TheMessage;
  FMessageParts:=TheMessageParts;
end;

destructor THelpQueryMessage.Destroy;
begin
  FMessageParts.Free;
  inherited Destroy;
end;

{ THelpQueryClass }

constructor THelpQueryClass.Create(const TheHelpDatabaseID: THelpDatabaseID;
  const AClass: TClass);
begin
  inherited Create(TheHelpDatabaseID);
  FTheClass:=AClass;
end;

{ THelpManager }

function THelpManager.DoHelpNotFound(var ErrMsg: string): TShowHelpResult;
begin
  Result:=shrHelpNotFound;
  ErrMsg:='Help not found';
end;

function THelpManager.ShowTableOfContents(var ErrMsg: string): TShowHelpResult;
begin
  Result:=DoHelpNotFound(ErrMsg);
end;

function THelpManager.ShowHelpForQuery(Query: THelpQuery;
  AutoFreeQuery: boolean; var ErrMsg: string): TShowHelpResult;
begin
  Result:=DoHelpNotFound(ErrMsg);
end;

function THelpManager.ShowHelpForContext(Query: THelpQueryContext;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=DoHelpNotFound(ErrMsg);
end;

function THelpManager.ShowHelpForKeyword(Query: THelpQueryKeyword;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=DoHelpNotFound(ErrMsg);
end;

function THelpManager.ShowHelpForPascalContexts(
  Query: THelpQueryPascalContexts; var ErrMsg: string): TShowHelpResult;
begin
  Result:=DoHelpNotFound(ErrMsg);
end;

function THelpManager.ShowHelpForSourcePosition(
  Query: THelpQuerySourcePosition; var ErrMsg: string): TShowHelpResult;
begin
  Result:=DoHelpNotFound(ErrMsg);
end;

function THelpManager.ShowHelpForMessageLine(Query: THelpQueryMessage;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=DoHelpNotFound(ErrMsg);
end;

function THelpManager.ShowHelpForClass(Query: THelpQueryClass;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=DoHelpNotFound(ErrMsg);
end;

function THelpManager.ShowHelpFile(const Filename, Title, MimeType: string;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=DoHelpNotFound(ErrMsg);
end;

function THelpManager.ShowHelp(const URL, Title, MimeType: string;
  var ErrMsg: string): TShowHelpResult;
begin
  Result:=DoHelpNotFound(ErrMsg);
end;

end.

