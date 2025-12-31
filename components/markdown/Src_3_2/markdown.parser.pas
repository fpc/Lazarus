{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown block structure parser.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit MarkDown.Parser;

{$mode objfpc}
{$h+}
interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CodePages.UnicodeData, System.SysUtils, System.Classes, System.Contnrs,
{$ELSE}
  UnicodeData, SysUtils, Classes, Contnrs,
{$ENDIF}  
  MarkDown.Elements,
  MarkDown.Utils,
  MarkDown.Scanner,
  MarkDown.Line,
  MarkDown.InlineText,
  MarkDown.HtmlEntities;

type
  EMarkDown = class(Exception);
  // Forward definition
  TMarkDownParser = class;

  // Options
  TMarkDownOption = (mdoGithubFlavoured);
  TMarkDownOptions = set of TMarkDownOption;

  // Parent block context
  TMarkDownBlockProcessingContext = (bpGeneral, bpCodeBlock, bpFencedCodeBlock);

  { TMarkDownBlockProcessor }
  TMarkDownBlockProcessor = class abstract (TObject)
  private
    FParser : TMarkdownParser;
    function GetParentProcessor: TMarkDownBlockProcessor;
  protected
    function inListOrQuote : boolean; virtual;
    // Access to parser methods
    function PeekLine : TMarkDownLine;
    function NextLine : TMarkDownLine;
    function Done : Boolean;
    procedure RedoLine(aResetLine: Boolean);
    function InList(aBlock : TMarkDownBlock; ordered : boolean; marker : String; indent : integer; grace : integer; out list : TMarkDownListBlock) : boolean;
    function IsBlock(aBlock : TMarkDownBlock; blocks : TMarkDownBlockList; const aLine : String; wsLen : integer = 3) : boolean;
    function CurrentLine : TMarkDownLine;
    procedure Parse(aParent: TMarkDownContainerBlock; aPArentProcessor: TMarkDownBlockProcessor); overload;
    // Our parser
    Property parser : TMarkDownParser read FParser;
    // Parent processor
    property ParentProcessor : TMarkDownBlockProcessor Read GetParentProcessor;

  public
    // One instance is created for each block type.
    constructor Create(aParser : TMarkdownParser); virtual;
    // Register for given block type
    class procedure register(const aBlockType : String);
    // This is called to see whether aLine closes the current block.
    function LineEndsBlock(aBlock : TMarkDownContainerBlock; aLine: TMarkDownLine): Boolean; virtual;
    // Return true if this processor handles the current line. Needs to handle state.
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; virtual; abstract;
    // When HandlesLine returned true, ProcessLine is called.
    // If ProcessLine returned true, the next line is started.
    // If it is false, the line is given to another processor.
    // processline is where a new block is created and attached to the parent block.
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; context : TMarkDownBlockProcessingContext) : boolean; virtual; abstract;
  end;
  TMarkDownBlockProcessorArray = array of TMarkDownBlockProcessor;
  TMarkDownBlockProcessorClass = class of TMarkDownBlockProcessor;
  TMarkDownBlockProcessorClassArray = array of TMarkDownBlockProcessorClass;

  { TMarkDownDocumentProcessor }

  // We always need this one. It is not registered, but created hardcoded
  TMarkDownDocumentProcessor = class (TMarkDownBlockProcessor)
  public
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : Boolean; override;
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
  end;


  { TMarkDownParser }

  TMarkDownParser = class(TComponent)
  private
    FLazy: Boolean;
    FLines : TMarkDownLineList;
    FCurrentLine : integer;
    FBuilder : TStringBuilder;
    FEntities : TFPStringHashTable;
    FOptions : TMarkDownOptions;
    FProcessors : TMarkDownBlockProcessorArray;
    FProcessorStack : TStack;
    function CreateScanner(const aText: String; aStartLine: Integer): TMarkDownTextScanner;
  Protected
    // Collect all entitues
    procedure CollectEntities(aList: TFPStringHashTable);
    // aLine operations
    // Convert strings in lines to TMarkDownLine instances
    procedure ConvertLines(aLines : TStrings);
    // Get the current line (can be nil);
    function CurrentLine : TMarkDownLine;
    // Get the next line (can be nil), but do not move the current line pointer
    function PeekLine : TMarkDownLine;
    // Get the next line (can be nil) and move the current line pointer
    function NextLine : TMarkDownLine;
    // Should we re-process the current line ? Moves line pointer one back, optionally resets the line.
    procedure RedoLine(aResetLine : Boolean = True);
    // is there a line available for redo ?
    function CanRedo : boolean;
    // Have we reached the last line ?
    function Done : boolean;
    // status
    // Is the last block a list with the given properties ?
    // if yes, return the list
    function InList(aBlock: TMarkDownBlock; aOrdered: boolean; const aMarker: String; aIndent: integer; aGrace: integer; out
      aList: TMarkDownListBlock): boolean;
    // Does aLine start a new block (true) or can it be a continuation (false) ?
    function IsBlock(aParent: TMarkDownBlock; aBlocks: TMarkDownBlockList; const aLine: String; aWhiteSpaceLen: integer): boolean;
    // block parsing loop
    procedure Parse(aParent: TMarkDownContainerBlock; aPArentProcessor: TMarkDownBlockProcessor); overload;
    // Parent processor of current processor
    function ParentProcessor : TMarkDownBlockProcessor;
    // process a text line
    function ProcessText(const aText: String; wsMode: TWhitespaceMode; aStartLine: integer): TMarkDownTextNodeList;
    // Recursively process inline text
    procedure ProcessInlines(aBlock : TMarkDownBlock; wsMode : TWhitespaceMode);
    // Initialize processors
    Procedure InitProcessors;
    // Done with processors
    Procedure DoneProcessors;
    // To customize the Inline Text processor class, override this.
    function GetInlineTextProcessorClass : TInlineTextProcessorClass; virtual;
    // To customize top-level document class, override this.
    function CreateDocument(aLine: integer): TMarkDownDocument; virtual;
    // To customize top-level document bloc k parser, override this.
    function CreateDocumentProcessor: TMarkDownDocumentProcessor; virtual;
  public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Utility function to handle parsing of inline text.
    procedure ParseInline(aParent : TMarkDownContainerBlock; const aLine : String);
    // Parse the markDown in strings
    function Parse(aSource: TStrings): TMarkDownDocument; overload;
    // Utility function: Parse the markDown in file aFileName.
    function ParseFile(const aFilename : string): TMarkDownDocument;
    // Helper : is the last block a plain paragraph ?
    class function InPara(blocks : TMarkDownBlockList; canBeQuote : boolean) : boolean;
    // Helper to quickly parse a stringlist into a markdown document
    class function FastParse(aSource: TStrings; aOptions: TMarkDownOptions): TMarkDownDocument;
    // Helper to quickly parse a stringlist into a markdown document
    class function FastParseFile(const aFileName : string; aOptions: TMarkDownOptions = []): TMarkDownDocument;
    // State control in lazy continuation .
    property Lazy : Boolean Read FLazy Write FLazy;
    // HTML entities to convert
    Property Entities : TFPStringHashTable read FEntities;
  published
    // Options
    Property Options : TMarkDownOptions Read FOptions Write FOptions;
  end;

  { TMarkDownProcessorFactory }

  TMarkDownProcessorFactory = class(TObject)
  Private
    class var _instance : TMarkDownProcessorFactory;
  private
    Type
      TRegisteredProcessor = class(TObject)
        Name : string;
        Processor : TMarkDownBlockProcessorClass;
        constructor create(const aName : String; aProcessor : TMarkDownBlockProcessorClass);
      end;
      TRegisteredProcessorList = class(Specialize TGFPObjectList<TRegisteredProcessor>);
  Private
    FList : TRegisteredProcessorList;
    function Find(const aBlockType : String; aAllowCreate : Boolean) : TRegisteredProcessor;
  public
    constructor Create;
    destructor Destroy; override;
    class constructor init;
    class destructor done;
    // Return an array with all known processors.
    function All : TMarkDownBlockProcessorClassArray;
    // Find a processor for a block of type aBlockType
    function FindProcessor(const aBlockType : string) : TMarkDownBlockProcessorClass;
    // Register a processor for block type aBlockType. Existing processor will be overwritten
    Procedure RegisterProcessor(const aBlockType : String; aProcessor : TMarkDownBlockProcessorClass);
    // Singleton instance
    class property Instance : TMarkDownProcessorFactory Read _instance;
  end;


implementation

{ TMarkDownBlockProcessor }

constructor TMarkDownBlockProcessor.Create(aParser: TMarkdownParser);

begin
  inherited Create;
  FParser:=aParser;
end;


class procedure TMarkDownBlockProcessor.register(const aBlockType: String);

begin
  TMarkDownProcessorFactory.Instance.RegisterProcessor(aBlockType,Self);
end;


function TMarkDownBlockProcessor.LineEndsBlock(aBlock: TMarkDownContainerBlock; aLine: TMarkDownLine): Boolean;

begin
  Result:=(aLine=Nil) or (aBlock=Nil);
end;


function TMarkDownBlockProcessor.inListOrQuote: boolean;

begin
  // Todo
  Result:=false;
end;


function TMarkDownBlockProcessor.PeekLine: TMarkDownLine;

begin
  Result:=FParser.PeekLine;
end;


function TMarkDownBlockProcessor.NextLine: TMarkDownLine;

begin
  Result:=FParser.NextLine;
end;


function TMarkDownBlockProcessor.Done: Boolean;

begin
  Result:=FParser.Done;
end;


procedure TMarkDownBlockProcessor.RedoLine(aResetLine: Boolean);

begin
  FParser.RedoLine(aResetLine);
end;


function TMarkDownBlockProcessor.InList(aBlock: TMarkDownBlock; ordered: boolean; marker: String; indent: integer; grace: integer;
  out list: TMarkDownListBlock): boolean;

begin
  Result:=FParser.InList(aBlock,ordered,marker,indent,grace,list);
end;


function TMarkDownBlockProcessor.IsBlock(aBlock: TMarkDownBlock; blocks: TMarkDownBlockList; const aLine: String; wsLen: integer
  ): boolean;

begin
  Result:=FParser.IsBlock(aBlock,Blocks,aLine,wsLen);
end;


function TMarkDownBlockProcessor.CurrentLine: TMarkDownLine;

begin
  Result:=FParser.CurrentLine;
end;


procedure TMarkDownBlockProcessor.Parse(aParent: TMarkDownContainerBlock; aPArentProcessor: TMarkDownBlockProcessor);

begin
  FParser.Parse(aParent,aParentProcessor);
end;


function TMarkDownBlockProcessor.GetParentProcessor: TMarkDownBlockProcessor;

begin
  Result:=FParser.ParentProcessor;
end;

{ TMarkDownDocumentProcessor }

function TMarkDownDocumentProcessor.processLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine; aContext : TMarkDownBlockProcessingContext): Boolean;

begin
  Result:=False;
end;


function TMarkDownDocumentProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;

begin
  Result:=False;
end;


{ TMarkDownParser }

class function TMarkDownParser.FastParse(aSource: TStrings; aOptions: TMarkDownOptions): TMarkDownDocument;

var
  lParser : TMarkDownParser;
  lDone : Boolean;

begin
  Result:=Nil;
  lDone:=false;
  lParser:=TMarkDownParser.Create(Nil);
  try
    lParser.Options:=aOptions;
    Result:=LParser.Parse(aSource);
    lDone:=true;
  finally
    if not lDone then
      Result.Free;
    lParser.free;
  end;
end;

class function TMarkDownParser.FastParseFile(const aFileName: string; aOptions: TMarkDownOptions): TMarkDownDocument;
var
  lFile : TStrings;
begin
  lFile:=TStringList.Create;
  try
    lFile.LoadFromFile(aFileName,TEncoding.UTF8);
    Result:=FastParse(lFile,aOptions);
  finally
    lFile.Free;
  end;
end;


procedure TMarkDownParser.CollectEntities(aList :TFPStringHashTable);

var
  Ent : THTMLEntityDef;

begin
  for ent in EntityDefList do
    aList.Add(ent.e,Utf8Encode(ent.u));
end;


constructor TMarkDownParser.Create(aOwner : TComponent);

begin
  inherited ;
  FOptions:=[];
  FBuilder:=TStringBuilder.Create;
  FProcessorStack:=TStack.Create;
  FLines:=TMarkDownLineList.create(true);
  FEntities:=TFPStringHashTable.create;
  CollectEntities(FEntities);
end;


destructor TMarkDownParser.Destroy;

begin
  FreeAndNil(FEntities);
  FreeAndNil(FProcessorStack);
  FreeAndNil(FBuilder);
  FreeAndNil(FLines);
  inherited;
end;


function TMarkDownParser.CreateDocument(aLine : integer) : TMarkDownDocument;

begin
  Result:=TMarkDownDocument.Create(Nil,aLine);
end;


function TMarkDownParser.CreateDocumentProcessor : TMarkDownDocumentProcessor;

begin
  Result:=TMarkDownDocumentProcessor.Create(Self);
end;

function TMarkDownParser.Parse(aSource: TStrings): TMarkDownDocument;

var
  lProc : TMarkDownDocumentProcessor;
  lDone : Boolean;

begin
  Result:=Nil;
  lProc:=Nil;
  lDone:=false;
  try
    InitProcessors;
    ConvertLines(aSource);
    lProc:=CreateDocumentProcessor;
    Result:=CreateDocument(1);
    parse(Result,lProc);
    processInlines(Result, wsTrim);
    lDone:=True;
    Result.closed:=True;
  finally
    DoneProcessors;
    if not lDone then
      Result.Free;
    lProc.Free;
  end;
end;

function TMarkDownParser.ParseFile(const aFilename: string): TMarkDownDocument;
var
  lFile : TStrings;
begin
  lFile:=TStringList.Create;
  try
    lFile.LoadFromFile(aFileName,TEncoding.UTF8);
    Result:=Parse(lFile);
  finally
    lFile.Free;
  end;
end;


function TMarkDownParser.NextLine: TMarkDownLine;

begin
  Result:=Nil;
  inc(FCurrentLine);
  if FCurrentLine<FLines.Count then
    Result:=FLines[FCurrentLine];
end;


function TMarkDownParser.PeekLine: TMarkDownLine;

begin
  Result:=nil;
  if (FCurrentLine<FLines.Count-1) then
    Result:=FLines[FCurrentLine+1];
end;


procedure TMarkDownParser.RedoLine(aResetLine: Boolean);

begin
  if aResetLine then
    FLines[FCurrentLine].reset;
  dec(FCurrentLine);
end;


function TMarkDownParser.CanRedo: boolean;

begin
  Result:=(FCurrentLine<FLines.Count);
end;


function TMarkDownParser.Done: boolean;
begin
  Result:=(FCurrentLine>=FLines.Count-1)
end;


function TMarkDownParser.InList(aBlock: TMarkDownBlock; aOrdered: boolean; const aMarker: String; aIndent: integer;
  aGrace: integer; out aList: TMarkDownListBlock): boolean;
var
  lBlock: TMarkDownBlock;
  lList : TMarkDownListBlock absolute lBlock;
begin
  Result:=False;
  lBlock:=aBlock;
  While (Not Result) and Assigned(lBlock) do
    begin
    Result:=lBlock is TMarkDownListBlock;
    // Check for exact match: same type, marker, and base indentation level
    if Result then
      Result:=(lList.ordered=aOrdered)
              and (lList.Marker=aMarker)
              and (aIndent-aGrace <= lList.BaseIndent)
              and not lList.closed;
    if Result then
      aList:=lList;
    lBlock:=lBlock.Parent;
    end;
end;


class function TMarkDownParser.InPara(blocks: TMarkDownBlockList; canBeQuote: boolean): boolean;

begin
  Result:=(blocks.Count > 0)
            and (blocks.Last is TMarkDownParagraphBlock)
            and not (blocks.Last as TMarkDownParagraphBlock).closed
            and ((blocks.Last as TMarkDownParagraphBlock).header = 0);
  if Result and not canBeQuote and not (blocks.Last as TMarkDownParagraphBlock).isPlainPara then
    Result:=false;
end;


function TMarkDownParser.IsBlock(aParent: TMarkDownBlock; aBlocks: TMarkDownBlockList; const aLine: String; aWhiteSpaceLen: integer): boolean;

  function inOrderedList : boolean;
  begin
    Result:=(aParent is TMarkDownListBlock) and (aParent as TMarkDownListBlock).Ordered;
  end;

  function IsOpenPara : boolean;
  begin
    Result:=(aBlocks.count > 0) and (aBlocks.lastblock is TMarkDownParagraphBlock) and not (aBlocks.LastBlock.Closed)
  end;

var
  lSkip : integer;
  lLine,lMarker : String;

begin
  // Known blocks
  Result:=True;
  if StartsWithWhitespace(aLine, ['*','-','+','#','`','~','>'],lSkip,aWhiteSpaceLen) then
    Exit;
  // Thematic break
  if StartsWithWhitespace(aLine, '___',lSkip) then
    Exit;
  // Code block
  if (LeadingWhitespace(aLine) >= 4) and not InPara(aBlocks,False) then
    Exit;
  // open para ?
  Result:=Not IsOpenPara;
  if Result then
    exit;
  // Remove whitespace
  lSkip:=LeadingWhitespace(aLine);
  lLine:=RemoveLeadingWhiteSpace(aLine,lSkip);
  // Check ordered List item.
  lMarker:=CopyMatching(lLine, ['0'..'9']);
  if (lMarker='') then
    Exit;
  // 1. is always a new block. In an ordered list we must check.
  if (lMarker='1') or inOrderedList then
    begin
    Delete(lLine,1,Length(lMarker));
    if (lLine<>'') then
      begin
      Result:=(lLine[1] in [')','.']);
      if Result then
        Result:=(length(lLine)>1) and (lLine[2]=' ');
      end;
    end;
end;


procedure TMarkDownParser.ParseInline(aParent: TMarkDownContainerBlock; const aLine: String);

var
  lBlock : TMarkDownTextBlock;

begin
  if (aParent.blocks.Count > 0) and (aParent.blocks.Last is TMarkDownTextBlock) then
    lBlock:=aParent.blocks.Last as TMarkDownTextBlock
  else
    lBlock:=TMarkDownTextBlock.create(aParent,FCurrentLine,'');
  if lBlock.Text<>'' then
    lBlock.Text:=lBlock.Text+#10;
  lBlock.Text:=lBlock.Text+aLine;
end;


procedure TMarkDownParser.Parse(aParent: TMarkDownContainerBlock; aPArentProcessor: TMarkDownBlockProcessor);

var
  lLine : TMarkDownLine;
  lprocessor : TMarkDownBlockProcessor;
  i,lProcCount : Integer;
  lProcessed : boolean;

begin
  FProcessorStack.Push(aParentProcessor);
  try
    Lazy:=False;
    while not done do
      begin
      if aParentProcessor.LineEndsBlock(aParent,PeekLine) then
        exit;
      lLine:=NextLine;
      lProcessed:=False;
      I:=0;
      lProcCount:=Length(FProcessors);
      While (not lProcessed) and (I<lProcCount) do
        begin
        lprocessor:=FProcessors[i];
        lProcessed:=lprocessor.HandlesLine(aParent,lLine);
        if lProcessed then
           lProcessed:=lprocessor.processLine(aParent,lLine,bpGeneral);
        // The last processor is normally the paragraph block...
        inc(I);
        end;
      if not lProcessed then
        Raise EMarkDown.CreateFmt('Line %s not processed',[lLine.LineNo]);
      end;
    if aParent.ChildCount>0 then
      aParent[aParent.ChildCount-1].closed:=True;
  finally
    FProcessorStack.Pop;
  end;
end;


procedure TMarkDownParser.ConvertLines(aLines: TStrings);

var
  i : integer;

begin
  FLines.Clear;
  For I:=0 to aLines.Count-1 do
    FLines.Add(TMarkDownLine.Create(aLines[i],I+1));
  FCurrentLine:=-1;
end;

function TMarkDownParser.CurrentLine: TMarkDownLine;

begin
  Result:=Nil;
  if (FCurrentLine>=0) and (FCurrentLine<FLines.Count) then
    Result:=FLines[FCurrentLine];
end;


function TMarkDownParser.ParentProcessor: TMarkDownBlockProcessor;

begin
  Result:=TMarkDownBlockProcessor(FProcessorStack.Peek);
end;

function TMarkDownParser.CreateScanner(const aText : String; aStartLine : Integer) : TMarkDownTextScanner;
begin
  Result:=TMarkDownTextScanner.Create(aText,aStartLine);
end;

function TMarkDownParser.ProcessText(const aText: String; wsMode: TWhitespaceMode; aStartLine: integer): TMarkDownTextNodeList;

var
  Scanner : TMarkDownTextScanner;
  Processor : TInlineTextProcessor;
  lClass : TInlineTextProcessorClass;
begin
  Result:=TMarkDownTextNodeList.Create;
  Scanner:=CreateScanner(aText,aStartLine);
  try
    lClass:=GetInlineTextProcessorClass;
    Processor:= LClass.Create(Scanner,Result,FEntities,wsMode);
    Processor.GFMExtensions:=mdoGithubFlavoured in Options;
    Processor.process(true);
  finally
    Scanner.Free;
    Processor.Free;
  end;
end;


procedure TMarkDownParser.ProcessInlines(aBlock: TMarkDownBlock; wsMode: TWhitespaceMode);

var
  I : Integer;
  lTextBlock : TMarkDownTextBlock absolute aBlock;

begin
  if aBlock is TMarkDownTextBlock then
    lTextBlock.Nodes:=processText(lTextBlock.Text,wsMode,aBlock.Line);
  for I:=0 to aBlock.ChildCount-1 do
    processInlines(aBlock.Children[i],aBlock.WhiteSpaceMode);
end;


procedure TMarkDownParser.InitProcessors;

var
  lClasses : TMarkDownBlockProcessorClassArray;
  lPar : TMarkDownBlockProcessorClass;
  lCount,I : Integer;

begin
  DoneProcessors;
  lClasses:=TMarkDownProcessorFactory.Instance.All;
  if Length(lClasses)=0 then
    Raise EMarkDown.Create('No markdown processors registered');
  lPar:=TMarkDownProcessorFactory.Instance.findprocessor('paragraph');
  SetLength(FProcessors,Length(lClasses));
  lCount:=0;
  For I:=0 to Length(lClasses)-1 do
    begin
    if lClasses[i]<>lPar then
      begin
      FProcessors[lCount]:=lClasses[i].Create(Self);
      inc(lCount);
      end;
    end;
  FProcessors[lCount]:=LPar.Create(Self);
end;


procedure TMarkDownParser.DoneProcessors;

var
  I : Integer;

begin
  For I:=0 to Length(FProcessors)-1 do
    FreeAndNil(FProcessors[i]);
end;


function TMarkDownParser.GetInlineTextProcessorClass: TInlineTextProcessorClass;

begin
  Result:=TInlineTextProcessor;
end;


{ TMarkDownProcessorFactory }

function TMarkDownProcessorFactory.Find(const aBlockType: String; aAllowCreate: Boolean): TRegisteredProcessor;

var
  Idx : integer;

begin
  Result:=Nil;
  Idx:=FList.Count-1;
  While (Result=Nil) and (Idx>=0) do
    begin
    Result:=FList[Idx];
    if not SameText(Result.Name,aBlockType) then
      Result:=Nil;
    Dec(Idx);
    end;
  if (Result=Nil) and aAllowCreate then
    begin
    Result:=TRegisteredProcessor.Create(aBlockType,Nil);
    FList.Add(Result);
    end;
end;


constructor TMarkDownProcessorFactory.Create;

begin
  FList:=TRegisteredProcessorList.Create(True);
end;


destructor TMarkDownProcessorFactory.Destroy;

begin
  FreeAndNil(FList);
  inherited destroy;
end;


class constructor TMarkDownProcessorFactory.init;

begin
  _instance:=TMarkDownProcessorFactory.Create;
end;


class destructor TMarkDownProcessorFactory.done;

begin
  FreeAndNil(_instance);
end;


function TMarkDownProcessorFactory.All: TMarkDownBlockProcessorClassArray;

var
  i : integer;

begin
  Result:=[];
  SetLength(Result,FList.Count);
  For I:=0 to FList.Count-1 do
    Result[I]:=FList[i].Processor;
end;


function TMarkDownProcessorFactory.FindProcessor(const aBlockType: string): TMarkDownBlockProcessorClass;

var
  lReg : TRegisteredProcessor;

begin
  Result:=Nil;
  LReg:=Find(aBlockType,False);
  if Assigned(lReg) then
    Result:=lReg.Processor;
end;


procedure TMarkDownProcessorFactory.RegisterProcessor(const aBlockType: String; aProcessor: TMarkDownBlockProcessorClass);

var
  lReg : TRegisteredProcessor;

begin
  LReg:=Find(LowerCase(aBlockType),True);
  lReg.Processor:=aProcessor;
end;


{ TMarkDownProcessorFactory.TRegisteredProcessor }

constructor TMarkDownProcessorFactory.TRegisteredProcessor.create(const aName: String; aProcessor: TMarkDownBlockProcessorClass);

begin
  Name:=aName;
  Processor:=aProcessor;
end;


end.
