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
      var IgnoreWordPos: TPoint; SourceIndex:integer) of object;

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
    procedure GetWordList(AWordList:TStrings; const Filter: String;
      ContainsFilter, CaseSensitive:boolean; MaxResults:integer);
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

// fast pos functions used in GetWordList

function MyPos(const SubStr, S: string; Offset, LastPos: SizeInt): SizeInt;
var
  i,MaxLen, SubLen : SizeInt;
  SubFirst: Char;
  pc: pchar;
begin
  Result:=0;
  SubLen := Length(SubStr);
  if (SubLen > 0) and (Offset > 0) and (Offset <= LastPos) then
   begin
    MaxLen := LastPos- SubLen;
    SubFirst := SubStr[1];
    i := IndexByte(S[Offset],LastPos - Offset + 1, Byte(SubFirst));
    while (i >= 0) and ((i + sizeint(Offset) - 1) <= MaxLen) do
    begin
      pc := @S[i+SizeInt(Offset)];
      //we know now that pc^ = SubFirst, because indexbyte returned a value > -1
      if (CompareByte(Substr[1],pc^,SubLen) = 0) then
        Exit(i + SizeInt(Offset));
      //point Offset to next char in S
      Offset := sizeint(i) + Offset + 1;
      i := IndexByte(S[Offset],LastPos - Offset + 1, Byte(SubFirst));
    end;
  end;
end;

procedure TWordCompletion.GetWordList(AWordList: TStrings;
  const Filter: String; ContainsFilter, CaseSensitive: boolean;
  MaxResults: integer);
var i, Line, x, FilterLen, MaxHash, LineLen: integer;
  UpFilter, LineText, UpLineText, UpWordBuffer: string;
  SourceText: TStringList;
  HashList: ^integer;// index list. Every entry points to a word in the AWordList
  SourceTextIndex, SourceTopLine, SourceBottomLine:integer;
  LastCharType:TCharType;
  IgnoreWordPos: TPoint;
  
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

  procedure AddIfMatch(const ALine, ALineUp:string; const AFirstPos, ALength: Integer);
  var
    AAdd: Boolean;
  begin
    if FilterLen=0 then
      AAdd := True
    else
    begin
      AAdd := False;
      if CaseSensitive then begin
        if ContainsFilter then
          AAdd := MyPos(Filter, ALine, AFirstPos, AFirstPos+ALength-1)>0
        else
          AAdd := strlcomp(PChar(@ALine[AFirstPos]),PChar(Filter),FilterLen)=0;
      end else
      begin
        if ContainsFilter then
          AAdd := MyPos(UpFilter, ALineUp, AFirstPos, AFirstPos+ALength-1)>0
        else
          AAdd := strlcomp(PChar(@ALineUp[AFirstPos]),PChar(UpFilter),FilterLen)=0;
      end;
    end;
    if AAdd then
      Add(Copy(ALine, AFirstPos, ALength));
  end;

// TWordCompletion.GetWordList
begin
  AWordList.Clear;
  if MaxResults<1 then MaxResults:=1;
  MaxHash:=MaxResults*3;
  GetMem(HashList,MaxHash*SizeOf(Integer));
  try
    for i:=0 to MaxHash-1 do HashList[i]:=-1;
    FilterLen:=length(Filter);
    AWordList.Capacity:=MaxResults;
    UpFilter:=uppercase(Filter);
    // first add all recently used words
    i:=FWordBuffer.Count-1;
    UpWordBuffer:='';
    while (i>=0) and (AWordList.Count<MaxResults) do begin
      if not CaseSensitive then
        UpWordBuffer := UpperCase(FWordBuffer[i]);
      AddIfMatch(FWordBuffer[i], UpWordBuffer, 1, Length(FWordBuffer[i]));
      dec(i);
    end;
    if AWordList.Count>=MaxResults then exit;
    // then search in all sources for more words that could fit
    SourceTextIndex:=0;
    if Assigned(FOnGetSource) then begin
      SourceText:=nil;
      SourceTopLine:=0;
      SourceBottomLine:=-1;
      IgnoreWordPos:=Point(-1,-1);
      FOnGetSource(SourceText,SourceTopLine,SourceBottomLine,IgnoreWordPos,SourceTextIndex);
      UpLineText:='';
      repeat
        if SourceText<>nil then begin
          Line:=SourceTopLine;
          if SourceBottomLine<0 then
            SourceBottomLine := SourceText.Count-1;
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
                if (i-x>=FilterLen) and not ((Line=IgnoreWordPos.Y) and (x<=IgnoreWordPos.X) and (IgnoreWordPos.X<=i)) then begin
                  AddIfMatch(LineText,UpLineText,x,i-x);
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
        IgnoreWordPos:=Point(-1,-1);
        FOnGetSource(SourceText,SourceTopLine,SourceBottomLine,IgnoreWordPos,SourceTextIndex);
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
    GetWordList(WordList,Prefix,False,CaseSensitive,10000);
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

