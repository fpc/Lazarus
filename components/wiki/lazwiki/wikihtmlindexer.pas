{ Copyright (C) <2008> <Andrew Haines> htmlindexer.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
unit wikihtmlindexer;
{$MODE OBJFPC}{$H+}
interface
uses Classes, SysUtils, FastHTMLParser,{$ifdef userb}fos_redblacktree_gen{$else}avl_tree{$endif};

Type

  { TIndexDocument }
  TIndexDocument = class(TObject)
  private
    FDocumentIndex: Integer;
    FLastEntry : Integer;
    WordIndex: array of Integer;
    function getindexentries:integer;
  public
    function GetWordIndex(i:integer):integer; inline;
    procedure AddWordIndex(AIndex: Integer);
    constructor Create(ADocumentIndex: Integer);
    property DocumentIndex: Integer read FDocumentIndex;
    property IndexEntry[i:integer] : Integer read GetWordIndex;
    property NumberofIndexEntries : integer read getindexentries;
  end;

  { TIndexedWord }
  TIndexedWord = class(TObject)
  private
    FIsTitle: Boolean;
    FTheWord: string;
    FCachedTopic: TIndexDocument;
    FDocuments: Array of TIndexDocument;
    function GetDocument ( TopicIndexNum: Integer ) : TIndexDocument;
    function GetDocumentCount: Integer;
  public
    constructor Create(AWord: String; AIsTitle: Boolean);
    destructor Destroy; override;
    function GetLogicalDocument(AIndex: Integer): TIndexDocument;
    property TheWord: string read FTheWord write ftheword; // Always lowercase
    property DocumentTopic[TopicIndexNum: Integer]: TIndexDocument read GetDocument;
    property DocumentCount: Integer read GetDocumentCount;
    property IsTitle: Boolean read FIsTitle write fistitle;
  end;

  { TIndexedWordList }

  {$ifdef userb}
  TRBIndexTree = specialize TGFOS_RBTree<String,TIndexedWord>;
  {$endif}

  TForEachMethod = procedure (AWord:TIndexedWord) of object;
  TForEachProcedure = Procedure (AWord:TIndexedWord;state:pointer);
  TIndexedWordList = class(TObject)
  private
    FIndexTitlesOnly: Boolean;
    FIndexedFileCount: DWord;
    //vars while processing page
    FInTitle,
    FInBody: Boolean;
    FWordCount: Integer; // only words in body
    FDocTitle: String;
    FTopicIndex: Integer;
    //end vars
    FTotalDifferentWordLength: DWord;
    FTotalDIfferentWords: DWord;
    FTotalWordCount: DWord;
    FTotalWordLength: DWord;
    FLongestWord: DWord;
    FParser: THTMLParser;
    {$ifdef userb}
    FAVLTree : TRBIndexTree;
    {$else}
    FAVLTree : TAVLTree;
    Spare :TIndexedWord;
    {$endif}

    function AddGetWord(AWord: String; IsTitle: Boolean): TIndexedWord;
    // callbacks
    procedure CBFoundTag(NoCaseTag, ActualTag: string);
    procedure CBFountText(Text: string);

    procedure EatWords(Words: String; IsTitle: Boolean);
  public
    constructor Create;
    destructor  Destroy; override;
    function  IndexFile(AStream: TStream; ATOPICIndex: Integer; AIndexOnlyTitles: Boolean): String; // returns the documents <Title>
    procedure Clear;
    procedure AddWord(const AWord: TIndexedWord);
    procedure ForEach(Proc:TForEachMethod);
    procedure ForEach(Proc:TForEachProcedure;state:pointer);
    property IndexedFileCount: DWord read FIndexedFileCount;
    property LongestWord: DWord read FLongestWord;
    property TotalWordCount: DWord read FTotalWordCount;
    property TotalDIfferentWords: DWord read FTotalDIfferentWords;
    property TotalWordLength: DWord read FTotalWordLength;
    property TotalDifferentWordLength: DWord read FTotalDifferentWordLength;
    property Words[AWord: String; IsTitle: Boolean] : TIndexedWord read AddGetWord;
  end;

implementation

Const GrowSpeed = 10;

function Max(ANumber, BNumber: DWord): DWord;
begin
  if ANumber > BNumber then
    Result := ANumber
  else
    Result := BNumber;
end;

const titlexlat : array [boolean] of char = ('0','1');

function  makekey( n : string;istitle:boolean):string; inline;

begin
   result:=n+'___'+titlexlat[istitle];
end;

Function CompareProcObj(Node1, Node2: Pointer): integer;
var n1,n2 : TIndexedWord; 
begin
  n1:=TIndexedWord(Node1); n2:=TIndexedWord(Node2);
  Result := CompareText(n1.theword, n2.theword);
  if Result = 0 then
  begin
    Result := ord(n2.IsTitle)-ord(n1.IsTitle);
  end;
  if Result < 0 then Result := -1
  else if Result > 0 then Result := 1;
end;

{ TIndexedWordList }
function TIndexedWordList.AddGetWord(AWord: String; IsTitle: Boolean): TIndexedWord;
var 
{$ifdef userb}
   key : string;
{$else}
   n : TAVLTreeNode;
{$endif}   
begin
  Result := nil;
  AWord := LowerCase(AWord);
 {$ifdef userb}
   key:=makekey(aword,istitle);
   if not favltree.Find(key,result) then result:=nil;;
  {$else}
  if not assigned(spare) then
    spare:=TIndexedWord.Create(AWord,IsTitle)
  else
    begin
      spare.TheWord:=aword;
      spare.IsTitle:=IsTitle;
    end;

  n:=favltree.FindKey(Spare,@CompareProcObj);
  if assigned(n) then
   result:=TIndexedWord(n.Data);
  {$endif}
  
  if Result = nil then
  begin
    Inc(FTotalDifferentWordLength, Length(AWord));
    Inc(FTotalDIfferentWords);
    {$ifdef  userb}
      result:=TIndexedWord.Create(AWord,IsTitle);
      favltree.add(key,result);
    {$else}
    Result := spare; // TIndexedWord.Create(AWord,IsTitle);
    spare:=nil;
    AddWord(Result);
    {$endif}

    //  if IsTitle then
    //WriteLn('Creating word: ', AWord);
    FLongestWord := Max(FLongestWord, Length(AWord));
  end;
  Inc(FTotalWordLength, Length(AWord));
  Inc(FTotalWordCount);
end;

procedure TIndexedWordList.CBFoundTag(NoCaseTag, ActualTag: string);
begin
  if FInBody then begin
    if NoCaseTag = '</BODY>' then FInBody := False;
  end
  else begin
    //WriteLn('"',NoCaseTag,'"');
    if NoCaseTag      = '<TITLE>' then FInTitle := True
    else if NoCaseTag = '</TITLE>' then FInTitle := False
    else if NoCaseTag = '<BODY>' then FInBody := True
    else
  end;
  if FInBody and FIndexTitlesOnly then FParser.Done := True;
end;

procedure TIndexedWordList.CBFountText(Text: string);
begin
  if Length(Text) < 1 then
    Exit;
  EatWords(Text, FInTitle and not FInBody);
end;

procedure TIndexedWordList.EatWords ( Words: String; IsTitle: Boolean ) ;
var
  WordPtr: PChar;
  WordStart: PChar;
  InWord: Boolean;
  IsNumberWord: Boolean;
  function IsEndOfWord: Boolean;
  begin
    Result := not (WordPtr^ in ['a'..'z', '0'..'9', #01, #$DE, #$FE]);
    if  Result and IsNumberWord then
      Result :=  Result and (WordPtr[0] <> '.');
    if Result and InWord then
      Result := Result and (WordPtr[0] <> '''');
  ;
  end;
  var
    WordIndex: TIndexedWord;
    WordName: String;
    FPos: Integer;
begin
  if IsTitle then
    FDocTitle := Words;
  Words := LowerCase(Words);
  WordStart := PChar(Words);
  WordPtr := WordStart;
  IsNumberWord := False;
  InWord := False;
  repeat
    if InWord and IsEndOfWord then
    begin
      WordName := Copy(WordStart, 0, (WordPtr-WordStart));
      FPos := Pos('''', WordName);
      while FPos > 0 do
      begin
        Delete(WordName, FPos, 1);
        FPos := Pos('''', WordName);
      end;
      WordIndex := addgetword(wordname,istitle);
      InWord := False;
      IsNumberWord := False;
      WordIndex.DocumentTopic[FTopicIndex].AddWordIndex(FWordCount);
      //if not IsTitle then
        Inc(FWordCount);

    end
    else if not InWord and not IsEndOfWord then
    begin
      InWord := True;
      WordStart := WordPtr;
      IsNumberWord := WordPtr^ in ['0'..'9'];
    end;
    Inc(WordPtr);
  until WordPtr^ = #0;

  if InWord then
  begin
    WordName := Copy(WordStart, 0, (WordPtr-WordStart));
    try
    WordIndex := addgetword(wordname,istitle); // Self.Words[WordName, IsTitle];
    except on e:exception do writeln(wordname); end;
    WordIndex.DocumentTopic[FTopicIndex].AddWordIndex(FWordCount);
    InWord := False;
    //if IsNumberWord then WriteLn('Following is NUMBER WORD: "', (WordStart[0]),'"'); ;
    IsNumberWord := False;
    //WriteLn(FWordCount, ' "', WordName,'"');
    if not IsTitle then
      Inc(FWordCount);
  end;
end;

function defaultindexedword : TIndexedWord;

begin
  result:=Tindexedword.create('',false);
end;

constructor TIndexedWordList.Create;
begin
  inherited;
  {$ifdef userb}
  FAVLTree :=TRBIndexTree.create(@default_rb_string_compare,
                                 @defaultindexedword,
                                 @default_rb_string_undef );
  {$else}
  favltree:=TAVLTree.Create(@CompareProcObj);
  spare:=nil;
  {$endif}
end;

procedure FreeObject(const Obj:TIndexedWord);
begin
 obj.free;
end;
 

destructor TIndexedWordList.Destroy;
begin
  clear;
  {$ifndef userb}
  if assigned(spare) then spare.free;
  {$endif}
  favltree.free;
  inherited Destroy;
end;

function TIndexedWordList.IndexFile(AStream: TStream; ATOPICIndex: Integer; AIndexOnlyTitles: Boolean): String;
var
  TheFile: String;
begin
  FInBody := False;
  FInTitle:= False;
  FIndexTitlesOnly := AIndexOnlyTitles;
  FWordCount := 0;
  FTopicIndex := ATOPICIndex;
  FIndexedFileCount := FIndexedFileCount +1;

  SetLength(TheFile, AStream.Size+1);
  AStream.Position := 0;
  AStream.Read(TheFile[1], AStream.Size);
  TheFile[Length(TheFile)] := #0;

  FParser := THTMLParser.Create(@TheFile[1]);
  FParser.OnFoundTag := @CBFoundTag;
  FParser.OnFoundText := @CBFountText;
  FParser.Exec;
  FParser.Free;

  Result := FDocTitle;
  FDocTitle := '';
  FInBody := False;
  FInTitle:= False;
  FWordCount := 0;
  FTopicIndex := -1;

  AStream.Position := 0;
end;

procedure TIndexedWordList.Clear;
begin
  {$ifdef userb}
   fAvlTree.ClearN(@FreeObject);
  {$else}
  fAvlTree.FreeAndClear;
  {$endif}
end;

procedure TIndexedWordList.AddWord(const AWord: TIndexedWord);
begin
 {$ifdef userb}
  favltree.add(makekey(aword.theword,aword.istitle),AWord);
 {$else}
  favltree.add(aword);
 {$endif}
end;

procedure TIndexedWordList.ForEach(Proc:TForEachMethod);
{$ifdef userb}
var key : string;
    val:TIndexedWord;
{$else}
var   
    AVLNode   : TAVLTreeNode;
{$endif}
begin
 {$ifdef userb}
    if favltree.FirstNode(key,val) then 
      begin  // Scan it forward
        repeat
          proc(val);
        until not favltree.FindNext(key,val);
      end;         
 {$else}
   AVLNode:=fAVLTree.FindLowest;
   while (AVLNode<>nil) do
      begin
        Proc(TIndexedWord(AVLNode.Data));
        AVLNode:=FAVLTree.FindSuccessor(AVLNode)
      end;
 {$endif}
end; 

procedure TIndexedWordList.ForEach(Proc:TForEachProcedure;state:pointer); 

{$ifdef userb}
var key : string;
    val:TIndexedWord;
{$else}
var   
    AVLNode   : TAVLTreeNode;
{$endif}
begin
 {$ifdef userb}
    if favltree.FirstNode(key,val) then 
      begin  // Scan it forward
        repeat
          proc(val,state);
        until not favltree.FindNext(key,val);
      end;         
 {$else}
   AVLNode:=fAVLTree.FindLowest;
   while (AVLNode<>nil) do
      begin
        Proc(TIndexedWord(AVLNode.Data),State);
        AVLNode:=FAVLTree.FindSuccessor(AVLNode)
      end;
  {$endif}
end; 

{ TIndexedWord }
function TIndexedWord.GetDocument ( TopicIndexNum: Integer ) : TIndexDocument;
var
  i: Integer;
begin
  Result := nil;
  if (FCachedTopic <> nil) and (FCachedTopic.FDocumentIndex = TopicIndexNum) then
    Exit(FCachedTopic);

  for i := 0 to High(FDocuments) do
    if FDocuments[i].FDocumentIndex = TopicIndexNum then
      Exit(FDocuments[i]);
  if Result = nil then
  begin
    Result := TIndexDocument.Create(TopicIndexNum);
    SetLength(FDocuments, Length(FDocuments)+1);
    FDocuments[High(FDocuments)] := Result;
  end;
  FCachedTopic := Result;
end;

function TIndexedWord.GetDocumentCount: Integer;
begin
  Result := Length(FDocuments);
end;

constructor TIndexedWord.Create(AWord: String; AIsTitle: Boolean);
begin
  FTheWord := AWord;
  FIsTitle := AIsTitle;
end;

destructor TIndexedWord.Destroy;
var
  i: Integer;
begin
  // here the word removed itself from the linked list. But it can't
  // touch the AVL tree here.
  for i := 0 to High(FDocuments) do
    FreeAndNil(FDocuments[i]);
  inherited Destroy;
end;

function TIndexedWord.GetLogicalDocument ( AIndex: Integer ) : TIndexDocument;
begin
  Result := FDocuments[AIndex];;
end;

{ TIndexDocument }
procedure TIndexDocument.AddWordIndex ( AIndex: Integer ) ;
begin
  if FLastEntry>=Length(WordIndex) Then
  SetLength(WordIndex, Length(WordIndex)+GrowSpeed);
  WordIndex[FLastEntry] := AIndex;
  Inc(FLastEntry); 
end;

constructor TIndexDocument.Create ( ADocumentIndex: Integer ) ;
begin
  FDocumentIndex := ADocumentIndex;
  flastentry:=0;
end;

function TIndexDocument.GetWordIndex(i:integer):integer;
begin
  result:=WordIndex[i];  
end;

function TIndexDocument.getindexentries:integer;
begin
 result:=flastentry-1; 
end;

end.
