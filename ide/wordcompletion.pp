{
  Author: Mattias Gaertner
  
  Abstract:
    A wordcompletion stores words and can createe a list of words gathered
    from the recently added words and provided source texts.
 
}
unit WordCompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, SynEdit;

type
  TWordCompletionGetSource =
    procedure(var Source:TStrings; SourceIndex:integer) of object;

  TWordCompletion = class
  private
    FWordBuffer:TStringList;// the recent used words list. the newest are at the end
    FWordBufferCapacity:integer;
    FOnGetSource:TWordCompletionGetSource;
    function GetWordBufferCapacity:integer;
    procedure SetWordBufferCapacity(NewCapacity: integer);
    function CaseInsensitiveIndexOf(const AWord:string):integer;
  public
    procedure AddWord(const AWord:string);
    property WordBufferCapacity:integer
       read GetWordBufferCapacity write SetWordBufferCapacity;
    procedure GetWordList(AWordList:TStrings; const Prefix:String;
       CaseSensitive:boolean; MaxResults:integer);
    property OnGetSource:TWordCompletionGetSource
       read FOnGetSource write FOnGetSource;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Strings, Math;

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

procedure TWordCompletion.GetWordList(AWordList:TStrings; const Prefix:String;
  CaseSensitive:boolean; MaxResults:integer);
var i,line,x,PrefixLen,MaxHash:integer;
  PrefixLow,s:string;
  SourceText:TStringList;
  HashList:^integer;// index list. Every entry points to a word in the AWordList
  SourceTextIndex:integer;
  LastCharType:TCharType;
  
  procedure Add(const AWord:string);
  // if AWord is not already in list then add it to AWordList
  var a,Hash,HashTry:integer;
    ALowWord:string;
  begin
    if CaseSensitive then begin
      if copy(AWord,1,PrefixLen)<>Prefix then exit;
    end else if lowercase(copy(AWord,1,PrefixLen))<>PrefixLow then exit
    else if (AWord=Prefix) then exit;
    ALowWord:=lowercase(AWord);
    Hash:=0;
    a:=0;
    while (a<=length(ALowWord)) and (a<20) do begin
      inc(Hash,ord(ALowWord[a]) and $3f);
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
    PrefixLow:=lowercase(Prefix);
    // first add all recently used words
    i:=FWordBuffer.Count-1;
    while (i>=0) and (AWordList.Count<MaxResults) do begin
      Add(FWordBuffer[i]);
      dec(i);
    end;
    if AWordList.Count>=MaxResults then exit;
    // then search in all sources for more words that could fit
    SourceTextIndex:=0;
    if Assigned(FOnGetSource) then begin
      repeat
        SourceText:=nil;
        FOnGetSource(SourceText,SourceTextIndex);
        if SourceText<>nil then begin
          line:=0;
          while (line<SourceText.Count) do begin
            s:=SourceText[line];
            x:=1;
            LastCharType:=ctNone;
            while (x<=length(s)) do begin
              if (LastCharType=ctNone) and (CharTable[s[x]]=ctWordBegin) then
              begin
                // word found
                i:=x;
                repeat
                  inc(i);
                until (i>length(s)) or (CharTable[s[i]]=ctNone);
                Add(copy(s,x,i-x));
                if AWordList.Count>=MaxResults then exit;
                x:=i;
              end else
                inc(x);
              LastCharType:=CharTable[s[x-1]];
            end;
            inc(line);
          end;
        end;
        inc(SourceTextIndex);
        FOnGetSource(SourceText,SourceTextIndex);
      until SourceText=nil;
    end;
  finally
    FreeMem(HashList);
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
  OldIndex:=FWordBuffer.IndexOf(AWord);
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
var LowWord: string;
begin
  LowWord:=lowercase(AWord);
  Result:=FWordBuffer.Count-1;
  while (Result>=0) and (lowercase(FWordBuffer[Result])<>LowWord) do
    dec(Result);
end;

initialization
  InitCharTable;

end.

