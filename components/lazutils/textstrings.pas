{
 /***************************************************************************
                                textstrings.pas
                                ---------------
                             Component Library Code

 ***************************************************************************/

 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  TTextStrings is a TStrings descendent that is optimized for handling the
  complete text as whole (instead of as line by line as in TStringList).

  TCustomMemoStrings is a TStrings descendent which works around the behavior
  of TMemo.Lines, which contains the text with wordwrap line endings, in order
  to store the text in the LFM without those wordwrap line endings. See bug 30659
}
unit TextStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazUtf8Classes, LazUtilsStrConsts;

type
  { TTextStrings }

  TTextLineRange = record
    Line: string; // cached line as string
    TheObject: TObject; // user data
    StartPos: integer; // start of line in Text
    EndPos: integer; // end of line in Text (= start of newline character(s))
  end;
  PTextLineRange = ^TTextLineRange;

  TCustomMemoStrings = class(TStrings)
  protected
    procedure DoReadData(Reader: TReader); virtual;
    procedure DoWriteData(Writer: TWriter); virtual;
    procedure DefineProperties(Filer: TFiler); override;
  end;

  TTextStrings = class(TCustomMemoStrings)
  private
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
  protected
    FArraysValid: boolean;
    FLineCount: integer;
    FLineCapacity: integer;
    FLineRanges: PTextLineRange;// array of TTextLineRange
    FText: string;
    FUpdateCount: integer;
    FChangedWhileUpdate: boolean;
    function GetTextStr: string; override;
    procedure SetTextStr(const AValue: string); override;
    procedure BuildArrays; virtual;
    function GetCount: Integer; override;
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    procedure ClearArrays;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AnObject: TObject); override;
    function GetLineLen(Index: integer; IncludeNewLineChars: boolean): integer; inline;
    function GetLineEnd(Index: integer; IncludeNewLineChars: boolean): integer;
    function CountLineEndings(const s: string): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SetText(TheText: PChar); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure MakeTextBufferUnique;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetText: PChar; override;
    function IndexOf(const S: string): Integer; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure AddStrings(TheStrings: TStrings); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  public
    property Text: string read FText write SetTextStr;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

implementation

{ TCustomMemoStrings }

procedure TCustomMemoStrings.DoReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
      Add(Reader.ReadString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TCustomMemoStrings.DoWriteData(Writer: TWriter);
var
  i: Integer;
  lStringsNoWordWrap: TStringList;
begin
  lStringsNoWordWrap := TStringList.Create;
  try
    lStringsNoWordWrap.Text := Text;

    Writer.WriteListBegin;
    for i := 0 to lStringsNoWordWrap.Count - 1 do
      Writer.WriteString(lStringsNoWordWrap.Strings[i]);
    Writer.WriteListEnd;
  finally
    lStringsNoWordWrap.Free;
  end;
end;

procedure TCustomMemoStrings.DefineProperties(Filer: TFiler);
var
  HasData: Boolean;
begin
  HasData := Count > 0;
  Filer.DefineProperty('Strings', @DoReadData, @DoWriteData, HasData);
end;

{ TTextStrings }

function TTextStrings.GetTextStr: string;
begin
  Result:=FText;
end;

procedure TTextStrings.SetTextStr(const AValue: string);
begin
  if FText=AValue then exit;
  FText:=AValue;
  FArraysValid:=false;
end;

procedure TTextStrings.BuildArrays;
var
  p, line: integer;
  l: Integer;
  ArraySize: Integer;
begin
  if FArraysValid then exit;
  ClearArrays;
  FArraysValid:=true;
  // count line ends
  FLineCount:=CountLineEndings(FText);
  l:=length(FText);
  if (FText<>'') and (not (FText[l] in [#10,#13])) then
    inc(FLineCount);
  FLineCapacity:=FLineCount;
  // build line range list
  if FLineCount>0 then begin
    ArraySize:=FLineCount*SizeOf(TTextLineRange);
    GetMem(FLineRanges,ArraySize);
    FillByte(FLineRanges^,ArraySize,0);
    p:=1;
    line:=0;
    FLineRanges[line].StartPos:=1;
    FLineRanges[FLineCount-1].EndPos:=l+1;
    while (p<=l) do begin
      if (not (FText[p] in [#10,#13])) then begin
        inc(p);
      end else begin
        // new line
        FLineRanges[line].EndPos:=p;
        inc(line);
        inc(p);
        if (p<=l) and (FText[p] in [#10,#13])
        and (FText[p]<>FText[p-1]) then
          inc(p);
        if line<FLineCount then
          FLineRanges[line].StartPos:=p;
      end;
    end;
  end;
end;

function TTextStrings.GetCount: Integer;
begin
  if not FArraysValid then BuildArrays;
  Result:=FLineCount;
end;

procedure TTextStrings.Changed;
// called after text changed
begin
  if (FUpdateCount>0) then begin
    FChangedWhileUpdate:=true;
    exit;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TTextStrings.Changing;
begin
  if FUpdateCount=0 then
    if Assigned(FOnChanging) then
      FOnChanging(Self);
end;

function TTextStrings.Get(Index: Integer): string;
var
  Line: PTextLineRange;
begin
  if not FArraysValid then BuildArrays;
  if (Index<0) or (Index>=FLineCount) then
    Error(lrsListIndexExceedsBounds, Index);
  Line:=@FLineRanges[Index];
  if (Line^.Line='')
  and (Line^.StartPos<Line^.EndPos) then begin
    Line^.Line:=copy(FText,Line^.StartPos,Line^.EndPos-Line^.StartPos);
  end;
  Result:=Line^.Line;
end;

procedure TTextStrings.ClearArrays;
var
  i: Integer;
begin
  FArraysValid:=false;
  if FLineRanges<>nil then begin
    for i:=0 to FLineCount-1 do
      FLineRanges[i].Line:='';
    FreeMem(FLineRanges);
    FLineRanges:=nil;
  end;
  FLineCapacity:=0;
end;

function TTextStrings.GetObject(Index: Integer): TObject;
begin
  if FArraysValid then begin
    if (Index<0) or (Index>=FLineCount) then
      Error(lrsListIndexExceedsBounds, Index);
    Result:=FLineRanges[Index].TheObject;
  end else
    Result:=nil;
end;

procedure TTextStrings.Put(Index: Integer; const S: string);
var
  OldLineLen: Integer;
  NewLineLen: Integer;
  Movement: Integer;
  OldStartPos: LongInt;
  OldEndPos: LongInt;
  MoveLen: Integer;
  i: Integer;
  NewEndPos: Integer;
begin
  if not FArraysValid then BuildArrays;
  if (Index<0) or (Index>=FLineCount) then
    Error(lrsListIndexExceedsBounds, Index);
  OldStartPos:=FLineRanges[Index].StartPos;
  OldEndPos:=FLineRanges[Index].EndPos;
  NewLineLen:=length(s);
  OldLineLen:=OldEndPos-OldStartPos;
  Movement:=NewLineLen-OldLineLen;
  NewEndPos:=OldEndPos+Movement;
  // move text behind
  MoveLen := Length(FText) - OldEndPos + 1;
  if (Movement<>0) and (MoveLen>0) then
  begin
    if Movement > 0 then
      SetLength(FText, Length(FText) + Movement);
    System.Move(FText[OldEndPos], FText[NewEndPos], MoveLen);
    if Movement < 0 then
      SetLength(FText, Length(FText) + Movement);

    for i := Index + 1 to FLineCount - 1 do
    begin
      inc(FLineRanges[i].StartPos, Movement);
      inc(FLineRanges[i].EndPos, Movement);
    end;
  end;
  FLineRanges[Index].EndPos:=NewEndPos;
  // copy text
  if NewLineLen>0 then
    System.Move(S[1],FText[OldStartPos],NewLineLen);
  FLineRanges[Index].Line:=S;
  // check if arrays need rebuild
  i:=NewLineLen;
  while (i>0) and (not (S[i] in [#10,#13])) do dec(i);
  if i>0 then begin
    // S contains new line chars => rebuild needed
    FArraysValid:=false;
  end;
end;

procedure TTextStrings.PutObject(Index: Integer; AnObject: TObject);
begin
  if not FArraysValid then BuildArrays;
  if (Index<0) or (Index>=FLineCount) then
    Error(lrsListIndexExceedsBounds, Index);
  FLineRanges[Index].TheObject:=AnObject;
end;

function TTextStrings.GetLineLen(Index: integer; IncludeNewLineChars: boolean
  ): integer;
begin
  Result:=GetLineEnd(Index,IncludeNewLineChars)-FLineRanges[Index].StartPos;
end;

function TTextStrings.GetLineEnd(Index: integer; IncludeNewLineChars: boolean
  ): integer;
begin
  if not FArraysValid then BuildArrays;
  if not IncludeNewLineChars then
    Result:=FLineRanges[Index].EndPos
  else if Index=FLineCount-1 then
    Result:=length(FText)+1
  else
    Result:=FLineRanges[Index+1].StartPos;
end;

function TTextStrings.CountLineEndings(const s: string): integer;
var
  p: Integer;
  l: Integer;
begin
  Result:=0;
  l:=length(s);
  p:=1;
  while p<=l do begin
    if s[p] in [#10,#13] then
    begin
      inc(Result);
      inc(p);
      if (p<=l) and (s[p] in [#10,#13]) and (s[p-1]<>s[p]) then
        inc(p);
    end else begin
      inc(p);
    end;
  end;
end;

constructor TTextStrings.Create;
begin
  inherited Create;
  CheckSpecialChars;
end;

destructor TTextStrings.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTextStrings.Clear;
begin
  ClearArrays;
  FLineCount:=0;
  FText:='';
end;

procedure TTextStrings.SetText(TheText: PChar);
begin
  if FText=TheText then exit;
  FText:=TheText;
  FArraysValid:=false;
end;

procedure TTextStrings.Insert(Index: Integer; const S: string);

  procedure RaiseOutOfBounds;
  begin
    raise EListError.Create('insert index '+IntToStr(Index)+' out of bounds '+IntToStr(FLineCount));
  end;

var
  NewStartPos: Integer;
  NewLineCharCount: Integer;
  NewLineLen: Integer;
  i: Integer;
  SEndsInNewLine: boolean;
  Range: PTextLineRange;
  NewCapacity: Integer;
begin
  if not FArraysValid then BuildArrays;
  NewLineLen:=length(S);
  SEndsInNewLine:=(S<>'') and (S[NewLineLen] in [#10,#13]);
  if Index<FLineCount then
  begin
    if Index<0 then
      RaiseOutOfBounds;
    NewStartPos:=FLineRanges[Index].StartPos;
  end else begin
    if Index>FLineCount then
      RaiseOutOfBounds;
    NewStartPos:=length(FText)+1;
  end;
  NewLineCharCount:=0;
  if SEndsInNewLine then begin
    inc(NewLineCharCount);
    if (NewLineLen>1)
    and (S[NewLineLen-1] in [#10,#13])
    and (S[NewLineLen-1]<>S[NewLineLen]) then
      inc(NewLineCharCount);
    System.Insert(S,FText,NewStartPos);
  end else begin
    // append missing newline char
    System.Insert(S+LineEnding,FText,NewStartPos);
    NewLineCharCount:=length(LineEnding);
    inc(NewLineLen,NewLineCharCount);
  end;
  // adjust arrays
  if FLineCount=FLineCapacity then begin
    if FLineCapacity<8 then
      NewCapacity:=8
    else
      NewCapacity:=FLineCapacity shl 1;
    ReAllocMem(FLineRanges,SizeOf(TTextLineRange)*NewCapacity);
    FillByte(FLineRanges[FLineCapacity],SizeOf(TTextLineRange)*(NewCapacity-FLineCapacity),0);
    FLineCapacity:=NewCapacity;
  end;
  if Index<FLineCount then begin
    System.Move(FLineRanges[Index],FLineRanges[Index+1],
                (FLineCount-Index)*SizeOf(TTextLineRange));
    FillByte(FLineRanges[Index],SizeOf(TTextLineRange),0);
    for i:=Index+1 to FLineCount do begin
      inc(FLineRanges[i].StartPos,NewLineLen);
      inc(FLineRanges[i].EndPos,NewLineLen);
    end;
  end;
  inc(FLineCount);
  Range:=@FLineRanges[Index];
  Range^.Line:=S;
  Range^.StartPos:=NewStartPos;
  Range^.EndPos:=NewStartPos+NewLineLen-NewLineCharCount;
end;

procedure TTextStrings.Delete(Index: Integer);
var
  OldLineLen: Integer;
  OldStartPos: Integer;
  i: Integer;
begin
  if not FArraysValid then BuildArrays;
  if (Index<0) or (Index>=FLineCount) then
    Error(lrsListIndexExceedsBounds, Index);
  // adjust text
  OldLineLen:=GetLineLen(Index,true);
  if OldLineLen>0 then begin
    OldStartPos:=FLineRanges[Index].StartPos;
    System.Delete(FText,OldStartPos,OldLineLen);
  end;
  // adjust arrays
  dec(FLineCount);
  FLineRanges[Index].Line:='';
  if Index<FLineCount then begin
    System.Move(FLineRanges[Index+1],FLineRanges[Index],
         (FLineCount-Index)*SizeOf(TTextLineRange));
    for i:=Index to FLineCount-1 do begin
      dec(FLineRanges[i].StartPos,OldLineLen);
      dec(FLineRanges[i].EndPos,OldLineLen);
    end;
  end;
  // clear last element
  FillByte(FLineRanges[FLineCount],SizeOf(TTextLineRange),0);
end;

procedure TTextStrings.Exchange(Index1, Index2: Integer);
var
  LineLen1: Integer;
  LineLen2: Integer;
  buf: Pointer;
  Dummy: Integer;
  OldBetweenStart: Integer;
  NewBetweenStart: Integer;
  BetweenLength: Integer;
  StartPos1: LongInt;
  StartPos2: LongInt;
  i: Integer;
  Movement: Integer;
  Obj: TObject;
  LineShortLen1: LongInt;
  LineShortLen2: LongInt;
  Line1: PTextLineRange;
  Line2: PTextLineRange;
begin
  // check values
  if Index1=Index2 then exit;
  if Index1<0 then
    Error(lrsListIndexExceedsBounds, Index1);
  if Index2<0 then
    Error(lrsListIndexExceedsBounds, Index2);
  if not FArraysValid then BuildArrays;
  if Index1>=FLineCount then
    Error(lrsListIndexExceedsBounds, Index1);
  if Index2>=FLineCount then
    Error(lrsListIndexExceedsBounds, Index2);

  // make sure Index1<Index2
  if Index1>Index2 then begin
    Dummy:=Index1;
    Index1:=Index2;
    Index2:=Dummy;
  end;

  Line1:=@FLineRanges[Index1];
  Line2:=@FLineRanges[Index2];

  // adjust text
  MakeTextBufferUnique;

  if (Index2=FLineCount-1) and (Line2^.EndPos>length(FText))
  then begin
    // The last line should be exchanged,
    // but Text has no new line character(s) at the end
    // => add LineEnding
    FText:=FText+LineEnding;
  end;

  // get line lengths including new line chars
  LineLen1:=GetLineLen(Index1,true);
  LineLen2:=GetLineLen(Index2,true);
  if (LineLen1<1) and (LineLen2<1) then exit;
  LineShortLen1:=GetLineLen(Index1,false);
  LineShortLen2:=GetLineLen(Index2,false);

  // save the bigger line
  StartPos1:=Line1^.StartPos;
  StartPos2:=Line2^.StartPos;
  if LineLen1>=LineLen2 then begin
    GetMem(buf,LineLen1);
    System.Move(FText[StartPos1],buf^,LineLen1);
  end else begin
    GetMem(buf,LineLen2);
    System.Move(FText[StartPos2],buf^,LineLen2);
  end;

  // move text in between
  OldBetweenStart:=StartPos1+LineLen1;
  BetweenLength:=StartPos2-OldBetweenStart;
  NewBetweenStart:=StartPos1+LineLen2;
  Movement:=NewBetweenStart-OldBetweenStart;
  if (BetweenLength>0) and (Movement<>0) then
    System.Move(FText[OldBetweenStart],FText[NewBetweenStart],BetweenLength);

  // move both lines
  Line1^.Line:='';
  Line2^.Line:='';
  if LineLen1>=LineLen2 then begin
    System.Move(FText[StartPos2],FText[StartPos1],LineLen2);
    System.Move(buf^,FText[StartPos2+Movement],LineLen1);
  end else begin
    System.Move(FText[StartPos1],FText[StartPos2+Movement],LineLen1);
    System.Move(buf^,FText[StartPos1],LineLen2);
  end;

  // adjust line ranges
  if Movement<>0 then
  begin
    Line1^.EndPos:=Line1^.StartPos+LineShortLen2;
    inc(Line2^.StartPos,Movement);
    Line2^.EndPos:=Line2^.StartPos+LineShortLen1;
    for i:=Index1+1 to Index2-1 do begin
      inc(FLineRanges[i].StartPos,Movement);
      inc(FLineRanges[i].EndPos,Movement);
    end;
  end;

  // exchange TheObject
  Obj:=Line1^.TheObject;
  Line1^.TheObject:=Line2^.TheObject;
  Line2^.TheObject:=Obj;

  // clean up
  FreeMem(buf);
end;

procedure TTextStrings.Move(CurIndex, NewIndex: Integer);
var
  SrcPos1: LongInt;
  SrcPos2: LongInt;
  SrcPos3: LongInt;
  LineStr: String;
  LineLen: Integer;
  i: LongInt;
  Obj: TObject;
  LineShortLen: LongInt;
  Line: PTextLineRange;
begin
  // check values
  if CurIndex=NewIndex then exit;
  if CurIndex<0 then
    Error(lrsListIndexExceedsBounds, CurIndex);
  if NewIndex<0 then
    Error(lrsListIndexExceedsBounds, NewIndex);
  if not FArraysValid then BuildArrays;
  if CurIndex>=FLineCount then
    Error(lrsListIndexExceedsBounds, CurIndex);
  if NewIndex>=FLineCount then
    Error(lrsListIndexExceedsBounds, NewIndex);

  // adjust text
  MakeTextBufferUnique;

  if CurIndex<NewIndex then
  begin
    // move to higher index
    if (NewIndex=FLineCount-1) and (FLineRanges[NewIndex].EndPos>length(FText))
    then begin
      // CurIndex should be moved to the end,
      // but Text has no new line character(s) at the end
      // => add LineEnding
      FText:=FText+LineEnding;
    end;
    SrcPos1:=FLineRanges[CurIndex].StartPos;
    SrcPos2:=FLineRanges[CurIndex+1].StartPos;
    SrcPos3:=GetLineEnd(NewIndex,true);
    // store current line with line end
    LineLen:=SrcPos2-SrcPos1;
    LineShortLen:=GetLineLen(CurIndex,false);
    LineStr:=copy(FText,SrcPos1,LineLen);
    Obj:=FLineRanges[CurIndex].TheObject;
    // move lines -1
    System.Move(FText[SrcPos2],FText[SrcPos1],SrcPos3-SrcPos2);
    for i:=CurIndex+1 to NewIndex do begin
      dec(FLineRanges[i].StartPos,LineLen);
      dec(FLineRanges[i].EndPos,LineLen);
    end;
    System.Move(FLineRanges[CurIndex+1],FLineRanges[CurIndex],
                SizeOf(TTextLineRange)*(NewIndex-CurIndex));
    // put current line at new position
    i:=SrcPos3-LineLen;
    System.Move(LineStr[1],FText[i],LineLen);
    Line:=@FLineRanges[NewIndex];
    Line^.StartPos:=i;
    Line^.EndPos:=i+LineShortLen;
    Pointer(Line^.Line):=nil; // this will be updated on demand, see Get
    Line^.TheObject:=Obj;
  end else begin
    // move to lower index
    if (CurIndex=FLineCount-1) and (FLineRanges[CurIndex].EndPos>length(FText))
    then begin
      // CurIndex should be moved from the end,
      // but Text has no new line character(s) at the end
      // => add LineEnding
      FText:=FText+LineEnding;
    end;
    SrcPos1:=FLineRanges[NewIndex].StartPos;
    SrcPos2:=FLineRanges[CurIndex].StartPos;
    SrcPos3:=GetLineEnd(CurIndex,true);
    // store current line with line end
    LineLen:=SrcPos3-SrcPos2;
    LineShortLen:=GetLineLen(CurIndex,false);
    LineStr:=copy(FText,SrcPos2,LineLen);
    Obj:=FLineRanges[CurIndex].TheObject;
    // move lines +1
    System.Move(FText[SrcPos1],FText[SrcPos1+LineLen],SrcPos2-SrcPos1);
    for i:=CurIndex-1 downto NewIndex do begin
      inc(FLineRanges[i].StartPos,LineLen);
      inc(FLineRanges[i].EndPos,LineLen);
    end;
    System.Move(FLineRanges[NewIndex],FLineRanges[NewIndex+1],
                SizeOf(TTextLineRange)*(CurIndex-NewIndex));
    // put current line at new position
    System.Move(LineStr[1],FText[SrcPos1],LineLen);
    Line:=@FLineRanges[NewIndex];
    Line^.StartPos:=SrcPos1;
    Line^.EndPos:=SrcPos1+LineShortLen;
    Pointer(Line^.Line):=nil; // this will be updated on demand, see Get
    Line^.TheObject:=Obj;
  end;
end;

procedure TTextStrings.MakeTextBufferUnique;
begin
  // make string unique (refcount=1) to be able to edit it directly
  UniqueString(FText);
end;

procedure TTextStrings.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TTextStrings.EndUpdate;

  procedure RaiseUpdateCount;
  begin
    raise Exception.Create('TTextStrings.EndUpdate');
  end;

begin
  if FUpdateCount<=0 then RaiseUpdateCount;
  dec(FUpdateCount);
  if FUpdateCount=0 then begin
    if FChangedWhileUpdate then
      Changed;
  end;
end;

function TTextStrings.GetText: PChar;
begin
  Result:=PChar(FText);
end;

function TTextStrings.IndexOf(const S: string): Integer;
begin
  Result:=inherited IndexOf(S);
end;

function TTextStrings.Add(const S: string): Integer;
begin
  Result:=AddObject(S,nil);
end;

function TTextStrings.AddObject(const S: string; AObject: TObject): Integer;
var
  e: String;
  NewLineCount: Integer;
  OldTxtLen: Integer;
  p: Integer;
  l: Integer;
begin
  Result:=Count;
  if (FText<>'') and (not (FText[length(FText)] in [#10,#13])) then
    e:=LineEnding
  else
    e:='';
  OldTxtLen:=length(FText);
  FText:=Text+e+S+LineEnding;
  if AObject<>nil then
    BuildArrays;
  if FArraysValid then
  begin
    // update FLineRanges
    NewLineCount:=FLineCount+CountLineEndings(S)+1;
    if NewLineCount>FLineCapacity then begin
      FLineCapacity:=FLineCapacity*2+10;
      if FLineCapacity<NewLineCount then
        FLineCapacity:=NewLineCount;
      ReAllocMem(FLineRanges,SizeOf(TTextLineRange)*FLineCapacity);
      FillByte(FLineRanges[FLineCount],SizeOf(TTextLineRange)*(FLineCapacity-FLineCount),0);
    end;
    FLineRanges[FLineCount].TheObject:=AObject;
    p:=OldTxtLen+length(e)+1;
    l:=length(FText);
    while FLineCount<NewLineCount do begin
      FLineRanges[FLineCount].StartPos:=p;
      while (p<=l) and (not (FText[p] in [#10,#13])) do
        inc(p);
      FLineRanges[FLineCount].EndPos:=p;
      inc(p);
      if (p<=l) and (FText[p] in [#10,#13]) and (FText[p]<>FText[p-1]) then
        inc(p);
      inc(FLineCount);
    end;
  end;
end;

procedure TTextStrings.AddStrings(TheStrings: TStrings);
var
  s: String;
  i: Integer;
  AddEachLine: Boolean;
  SrcTextStrings: TTextStrings;
  SrcItem: PTextLineRange;
  DstItem: PTextLineRange;
begin
  if TheStrings.Count=0 then exit;
  if FLineCount=0 then begin
    if TheStrings is TTextStrings then begin
      // copy Text, lineranges
      SrcTextStrings:=TTextStrings(TheStrings);
      FText:=SrcTextStrings.Text;
      ClearArrays;
      if not SrcTextStrings.FArraysValid then exit;
      // copy line range list
      FLineCount:=SrcTextStrings.Count;
      FLineCapacity:=FLineCount;
      FLineRanges:=AllocMem(FLineCount*SizeOf(TTextLineRange));
      SrcItem:=SrcTextStrings.FLineRanges;
      DstItem:=FLineRanges;
      for i:=0 to FLineCount-1 do begin
        DstItem^:=SrcItem^;
        inc(SrcItem);
        inc(DstItem);
      end;
      FArraysValid:=true;
      exit;
    end;
  end;
  AddEachLine:=false;
  if FArraysValid then begin
    for i:=0 to FLineCount-1 do
      if FLineRanges[i].TheObject<>nil then begin
        // old objects have to be kept
        AddEachLine:=true;
        break;
      end;
  end;
  if not AddEachLine then begin
    for i:=0 to TheStrings.Count-1 do begin
      if TheStrings.Objects[i]<>nil then begin
        // new objects have to be kept
        AddEachLine:=true;
        break;
      end;
    end;
  end;
  if AddEachLine then begin
    // append line by line, this can be very slow
    for i:=0 to TheStrings.Count-1 do
      AddObject(TheStrings[i],TheStrings.Objects[i]);
  end else begin
    // append the whole text at once
    // Beware: #10,#13 characters in lines are now converted to multiple lines
    if (FText<>'') and (not (FText[length(FText)] in [#10,#13])) then
      s:=LineEnding
    else
      s:='';
    FArraysValid:=false;
    FText:=FText+s+TheStrings.Text;
    BuildArrays;
  end;
end;

procedure TTextStrings.LoadFromFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:=TFileStreamUtf8.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure TTextStrings.SaveToFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:=TFileStreamUtf8.Create(FileName,fmCreate);
  try
    SaveToStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

end.
