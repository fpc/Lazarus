{
  Author: Mattias Gaertner
  
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Abstract:
    A wordcompletion stores words and can createse a list of words gathered
    from the recently added words and provided source texts.
 
}
unit WordCompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, SynEdit;

type
  TWordCompletionGetSource =
    procedure(var Source:TStrings; var TopLine, BottomLine: Integer;
      SourceIndex:integer) of object;

  TWordCompletion = class
  private
    FWordBuffer:TStringList;// the recent used words list. the newest are at the end
    FWordBufferCapacity:integer;
    FOnGetSource:TWordCompletionGetSource;
    function GetWordBufferCapacity:integer;
    procedure SetWordBufferCapacity(NewCapacity: integer);
    function CaseInsensitiveIndexOf(const AWord: string):integer;
    function CaseSensitiveIndexOf(const AWord: string):integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddWord(const AWord:string);
    property WordBufferCapacity:integer
       read GetWordBufferCapacity write SetWordBufferCapacity;
    procedure GetWordList(AWordList:TStrings; const Prefix:String;
       CaseSensitive:boolean; MaxResults:integer);
    procedure CompletePrefix(const Prefix: string; var CompletedPrefix: string;
       CaseSensitive:boolean);
  public
    property OnGetSource:TWordCompletionGetSource
       read FOnGetSource write FOnGetSource;
  end;

implementation

type
  TCharType = (ctNone,ctWordBegin,ctWord);

var
  CharTable: array[char] of TCharType;

procedure InitCharTable;
var c:char;
begin
  for c:=low(char) to high(char) do
    case c of
      'a'..'z','A'..'Z','_':CharTable[c]:=ctWordBegin;
      '0'..'9':CharTable[c]:=ctWord;
    else CharTable[c]:=ctNone;
    end;
end;


{ TWordCompletion }

procedure TWordCompletion.GetWordList(AWordList: TStrings;
  const Prefix: String; CaseSensitive: boolean; MaxResults: integer);
var i, j, Line, x, PrefixLen, MaxHash, LineLen: integer;
  UpPrefix, LineText, UpLineText, NewWord: string;
  SourceText: TStringList;
  HashList: ^integer;// index list. Every entry points to a word in the AWordList
  SourceTextIndex, SourceTopLine, SourceBottomLine:integer;
  LastCharType:TCharType;
  
  procedure Add(const AWord:string);
  // if AWord is not already in list then add it to AWordList
  var a,Hash,HashTry:integer;
    ALowWord:string;
  begin
    ALowWord:=lowercase(AWord);
    Hash:=0;
    a:=1;
    while (a<=length(ALowWord)) and (a<20) do begin
      inc(Hash,ord(ALowWord[a]) and $7f);
      inc(a);
    end;
    Hash:=(Hash*137) mod MaxHash;
    HashTry:=0;
    while (HashTry<MaxHash) do begin
      a:=HashList[(Hash+HashTry) mod MaxHash];
      if a>=0 then begin
        if (AWordList[a]=AWord) then
          // word already in list -> do not add
          exit;
      end else begin
        // word not in list -> add
        HashList[(Hash+HashTry) mod MaxHash]:=AWordList.Add(AWord);
        exit;
      end;
      inc(HashTry);
    end;
  end;

// TWordCompletion.GetWordList
begin
  AWordList.Clear;
  if MaxResults<1 then MaxResults:=1;
  MaxHash:=MaxResults*3;
  GetMem(HashList,MaxHash*SizeOf(Integer));
  try
    for i:=0 to MaxHash-1 do HashList[i]:=-1;
    PrefixLen:=length(Prefix);
    AWordList.Capacity:=MaxResults;
    UpPrefix:=uppercase(Prefix);
    // first add all recently used words
    i:=FWordBuffer.Count-1;
    while (i>=0) and (AWordList.Count<MaxResults) do begin
      NewWord:=FWordBuffer[i];
      if CaseSensitive then begin
        if copy(NewWord,1,PrefixLen)=Prefix then
          Add(NewWord);
      end else if CompareText(copy(NewWord,1,PrefixLen),UpPrefix)=0 then begin
        if NewWord<>Prefix then
          Add(NewWord)
      end;
      dec(i);
    end;
    if AWordList.Count>=MaxResults then exit;
    // then search in all sources for more words that could fit
    SourceTextIndex:=0;
    if Assigned(FOnGetSource) then begin
      SourceText:=nil;
      SourceTopLine:=0;
      SourceBottomLine:=-1;
      FOnGetSource(SourceText,SourceTopLine,SourceBottomLine,SourceTextIndex);
      repeat
        if SourceText<>nil then begin
          Line:=SourceTopLine;
          if SourceBottomLine<0 then
            SourceBottomLine := SourceText.Count-1;
          UpLineText:='';
          while (Line<=SourceBottomLine) do begin
            LineText:=SourceText[line];
            LineLen:=length(LineText);
            if not CaseSensitive then
              UpLineText:=uppercase(LineText);
            x:=1;
            LastCharType:=ctNone;
            while (x<=LineLen) do begin
              if (LastCharType=ctNone) and (CharTable[LineText[x]]=ctWordBegin)
              then begin
                // word found
                i:=x;
                repeat
                  inc(i);
                until (i>LineLen) or (CharTable[LineText[i]]=ctNone);
                if i-x>=PrefixLen then begin
                  if CaseSensitive then begin
                    j:=1;
                    while (j<=PrefixLen) and (Prefix[j]=LineText[x+j-1]) do
                      inc(j);
                    if (j>PrefixLen) and (copy(LineText,x,i-x)<>Prefix) then
                      Add(copy(LineText,x,i-x));
                  end else begin
                    j:=1;
                    while (j<=PrefixLen) and (UpPrefix[j]=UpLineText[x+j-1]) do
                      inc(j);
                    if (j>PrefixLen) and (copy(LineText,x,i-x)<>Prefix) then
                      Add(copy(LineText,x,i-x))
                  end;
                  if AWordList.Count>=MaxResults then exit;
                end;
                x:=i;
              end else
                inc(x);
              LastCharType:=CharTable[LineText[x-1]];
            end;
            inc(line);
          end;
        end;
        inc(SourceTextIndex);
        SourceText:=nil;
        SourceTopLine:=0;
        SourceBottomLine:=-1;
        FOnGetSource(SourceText,SourceTopLine,SourceBottomLine,SourceTextIndex);
      until SourceText=nil;
    end;
  finally
    FreeMem(HashList);
  end;
end;

procedure TWordCompletion.CompletePrefix(const Prefix: string;
  var CompletedPrefix: string; CaseSensitive:boolean);
var
  WordList: TStringList;
  s: string;
  SamePos: Integer;
  MaxPos: Integer;
  i: Integer;
begin
  CompletedPrefix:=Prefix;
  WordList:=TStringList.Create;
  try
    // fetch all words with Prefix
    GetWordList(WordList,Prefix,CaseSensitive,10000);
    if WordList.Count=0 then exit;
    // find the biggest prefix of all available words
    CompletedPrefix:=WordList[0];
    for i:=1 to WordList.Count-1 do begin
      // stop, when it can't get shorter
      if CompletedPrefix=Prefix then exit;
      s:=WordList[i];
      if length(s)<length(Prefix) then continue;
      // count same
      SamePos:=0;
      MaxPos:=length(s);
      if MaxPos>length(CompletedPrefix) then MaxPos:=length(CompletedPrefix);
      while (SamePos<MaxPos) do begin
        if CaseSensitive then begin
          if s[SamePos+1]<>CompletedPrefix[SamePos+1] then
            break;
        end else begin
          if upcase(s[SamePos+1])<>upcase(CompletedPrefix[SamePos+1]) then
            break;
        end;
        inc(SamePos);
      end;
      if SamePos<length(Prefix) then continue;
      if SamePos<length(CompletedPrefix) then
        CompletedPrefix:=copy(CompletedPrefix,1,SamePos);
    end;
  finally
    WordList.Free;
  end;
end;

constructor TWordCompletion.Create;
begin
  inherited Create;
  FWordBuffer:=TStringList.Create;
  FWordBufferCapacity:=100;
end;

destructor TWordCompletion.Destroy;
begin
  FWordBuffer.Free;
  inherited Destroy;
end;

function TWordCompletion.GetWordBufferCapacity:integer;
begin
  Result:=FWordBufferCapacity;
end;

procedure TWordCompletion.SetWordBufferCapacity(NewCapacity: integer);
var TempWordBuffer:TStringList;
  i:integer;
begin
  if NewCapacity<5 then NewCapacity:=5;
  if NewCapacity<>FWordBufferCapacity then begin
    FWordBufferCapacity:=NewCapacity;
    if FWordBuffer.Count>NewCapacity then begin
      TempWordBuffer:=TStringList.Create;
      TempWordBuffer.Capacity:=NewCapacity;
      i:=FWordBuffer.Count-NewCapacity;
      while i<FWordBuffer.Count do begin
        TempWordBuffer.Add(FWordBuffer[i]);
        inc(i);
      end;
      FWordBuffer.Free;
      FWordBuffer:=TempWordBuffer;
    end;
  end;
end;

procedure TWordCompletion.AddWord(const AWord:string);
var OldIndex:integer;
begin
  OldIndex:=CaseSensitiveIndexOf(AWord);
  if OldIndex>=0 then begin
    // move word to the top
    FWordBuffer.Move(OldIndex,FWordBuffer.Count-1);
  end else begin
    // add new word
    if FWordBuffer.Count=FWordBufferCapacity then
      FWordBuffer.Delete(0);
    FWordBuffer.Add(AWord);
  end;
end;

function TWordCompletion.CaseInsensitiveIndexOf(const AWord:string):integer;
begin
  Result:=FWordBuffer.Count-1;
  while (Result>=0) and (CompareText(FWordBuffer[Result],AWord)<>0) do
    dec(Result);
end;

function TWordCompletion.CaseSensitiveIndexOf(const AWord: string): integer;
begin
  Result:=FWordBuffer.Count-1;
  while (Result>=0) and (FWordBuffer[Result]<>AWord) do
    dec(Result);
end;

initialization
  InitCharTable;

end.

