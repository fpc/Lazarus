{
 /***************************************************************************
                                textstrings.pas
                                ---------------
                             Component Library Code


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  TTextStrings is a TStrings descendent that is optimized for handling the
  complete text as whole (instead of as line by line as in TStringList).
  
  UNDER CONSTRUCTION by Mattias Gaertner
  
  ToDo:
    - Move
    - IndexOf
}
unit TextStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLStrConsts;
  
type
  { TTextStrings }

  TTextLineRange = record
    StartPos: integer; // start of line in Text
    EndPos: integer; // end of line in Text (= start of newline character(s))
    Line: string; // cached line as string
    TheObject: TObject; // user data
  end;

  TTextStrings = class(TStrings)
  private
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
  protected
    FArraysValid: boolean;
    FLineCount: integer;
    FLineCapacity: integer;
    FLineRanges: ^TTextLineRange;// array of TTextLineRange
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
    function GetLineLen(Index: integer; IncludeNewLineChars: boolean): integer;
  public
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
    procedure AddStrings(TheStrings: TStrings); override;
  public
    property Text: string read FText write SetTextStr;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

implementation

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
  FLineCount:=0;
  l:=length(FText);
  p:=1;
  while (p<=l) do begin
    if (not (FText[p] in [#10,#13])) then begin
      inc(p);
    end else begin
      // new line
      inc(FLineCount);
      inc(p);
      if (p<=l) and (FText[p] in [#10,#13])
      and (FText[p]<>FText[p-1]) then
        inc(p);
    end;
  end;
  if (FText<>'') and (not (FText[l] in [#10,#13])) then
    inc(FLineCount);
  FLineCapacity:=FLineCount;
  // build line range list
  if FLineCount>0 then begin
    ArraySize:=FLineCount*SizeOf(TTextLineRange);
    GetMem(FLineRanges,ArraySize);
    FillChar(FLineRanges^,ArraySize,0);
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
begin
  if not FArraysValid then BuildArrays;
  if (Index<0) or (Index>=FLineCount) then
    Error(rsListIndexExceedsBounds, Index);
  if (FLineRanges[Index].Line='')
  and (FLineRanges[Index].StartPos<FLineRanges[Index].EndPos) then begin
    FLineRanges[Index].Line:=copy(FText,FLineRanges[Index].StartPos,
                         FLineRanges[Index].EndPos-FLineRanges[Index].StartPos);
  end;
  Result:=FLineRanges[Index].Line;
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
  if FArraysValid then
    Result:=FLineRanges[Index].TheObject
  else
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
  OldStartPos:=FLineRanges[Index].StartPos;
  OldEndPos:=FLineRanges[Index].EndPos;
  NewLineLen:=length(s);
  OldLineLen:=OldEndPos-OldStartPos;
  Movement:=NewLineLen-OldLineLen;
  NewEndPos:=OldEndPos+Movement;
  // move text behind
  MoveLen:=length(FText)-OldEndPos;
  if (Movement<>0) and (MoveLen>0) then begin
    SetLength(FText,length(FText)+Movement);
    System.Move(FText[OldEndPos],FText[NewEndPos],MoveLen);
    for i:=Index+1 to FLineCount-1 do begin
      inc(FLineRanges[i].StartPos,Movement);
      inc(FLineRanges[i].EndPos,Movement);
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
  FLineRanges[Index].TheObject:=AnObject;
end;

function TTextStrings.GetLineLen(Index: integer; IncludeNewLineChars: boolean
  ): integer;
var
  LineEndPos: Integer;
begin
  if not FArraysValid then BuildArrays;
  if not IncludeNewLineChars then
    LineEndPos:=FLineRanges[Index].EndPos
  else if Index=FLineCount-1 then
    LineEndPos:=length(FText)
  else
    LineEndPos:=FLineRanges[Index+1].StartPos;
  Result:=LineEndPos-FLineRanges[Index].StartPos;
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
var
  NewStartPos: Integer;
  NewLineCharCount: Integer;
  NewLineLen: Integer;
  i: Integer;
  SEndsInNewLine: boolean;
begin
  if not FArraysValid then BuildArrays;
  NewLineLen:=length(S);
  SEndsInNewLine:=(S<>'') and (S[NewLineLen] in [#10,#13]);
  if Index<FLineCount then
    NewStartPos:=FLineRanges[Index].StartPos
  else
    NewStartPos:=length(FText);
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
      FLineCapacity:=8
    else
      FLineCapacity:=FLineCapacity shl 1;
    ReAllocMem(FLineRanges,SizeOf(TTextLineRange)*FLineCapacity);
  end;
  if Index<FLineCount then begin
    System.Move(FLineRanges[Index],FLineRanges[Index+1],
                (FLineCount-Index)*SizeOf(TTextLineRange));
    for i:=Index+1 to FLineCount do begin
      inc(FLineRanges[i].StartPos,NewLineLen);
      inc(FLineRanges[i].EndPos,NewLineLen);
    end;
  end;
  FLineRanges[Index].Line:=S;
  FLineRanges[Index].EndPos:=NewStartPos+NewLineLen-NewLineCharCount;
  inc(FLineCount);
end;

procedure TTextStrings.Delete(Index: Integer);
var
  OldLineLen: Integer;
  OldStartPos: Integer;
  i: Integer;
begin
  if not FArraysValid then BuildArrays;
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
end;

procedure TTextStrings.Exchange(Index1, Index2: Integer);

  procedure RaiseIndex1Neg;
  begin
    raise Exception.Create('TTextStrings.Exchange Index1<=0');
  end;

  procedure RaiseIndex2Neg;
  begin
    raise Exception.Create('TTextStrings.Exchange Index2<=0');
  end;

  procedure RaiseIndex1Big;
  begin
    raise Exception.Create('TTextStrings.Exchange Index1>=FLineCount');
  end;

  procedure RaiseIndex2Big;
  begin
    raise Exception.Create('TTextStrings.Exchange Index2>=FLineCount');
  end;

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
  DummyRange: TTextLineRange;
  i: Integer;
  Movement: Integer;
begin
  // check values
  if Index1=Index2 then exit;
  if Index1<=0 then RaiseIndex1Neg;
  if Index2<=0 then RaiseIndex2Neg;
  if Index1>=FLineCount then RaiseIndex1Big;
  if Index2>=FLineCount then RaiseIndex2Big;

  // make sure Index1<Index2
  if Index1>Index2 then begin
    Dummy:=Index1;
    Index1:=Index2;
    Index2:=Dummy;
  end;

  // get line lengths including new line chars
  if not FArraysValid then BuildArrays;
  LineLen1:=GetLineLen(Index1,true);
  LineLen2:=GetLineLen(Index2,true);
  if (LineLen1<1) and (LineLen2<1) then exit;

  // adjust text
  MakeTextBufferUnique;

  // save the bigger line
  StartPos1:=FLineRanges[Index1].StartPos;
  StartPos2:=FLineRanges[Index2].StartPos;
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
  if (BetweenLength>0) and (OldBetweenStart<>NewBetweenStart) then
    System.Move(FText[OldBetweenStart],FText[NewBetweenStart],BetweenLength);

  // move both lines
  if LineLen1>=LineLen2 then begin
    System.Move(FText[StartPos2],FText[StartPos1],LineLen2);
    System.Move(buf^,FText[StartPos2],LineLen1);
  end else begin
    System.Move(FText[StartPos1],FText[StartPos2],LineLen1);
    System.Move(buf^,FText[StartPos1],LineLen2);
  end;

  // adjust line ranges
  if LineLen1<>LineLen2 then begin
    System.Move(FLineRanges[Index1],DummyRange,SizeOf(TTextLineRange));
    System.Move(FLineRanges[Index2],FLineRanges[Index1],SizeOf(TTextLineRange));
    System.Move(DummyRange,FLineRanges[Index2],SizeOf(TTextLineRange));
    if (BetweenLength>0) and (OldBetweenStart<>NewBetweenStart) then begin
      Movement:=NewBetweenStart-OldBetweenStart;
      for i:=Index1+1 to Index2-1 do begin
        inc(FLineRanges[i].StartPos,Movement);
        inc(FLineRanges[i].EndPos,Movement);
      end;
    end;
  end;

  // clean up
  FreeMem(buf);
end;

procedure TTextStrings.Move(CurIndex, NewIndex: Integer);
begin
  // TODO
  inherited Move(CurIndex, NewIndex);
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
begin
  if FUpdateCount<=0 then
    raise Exception.Create('TTextStrings.EndUpdate');
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
  // TODO
  Result:=inherited IndexOf(S);
end;

function TTextStrings.Add(const S: string): Integer;
var
  e: String;
begin
  Result:=Count;
  if (FText<>'') and (not (FText[length(FText)] in [#10,#13])) then
    e:=LineEnding
  else
    e:='';
  Text:=Text+e+S;
end;

procedure TTextStrings.AddStrings(TheStrings: TStrings);
var
  s: String;
begin
  if (FText<>'') and (not (FText[length(FText)] in [#10,#13])) then
    s:=LineEnding
  else
    s:='';
  Text:=Text+s+TheStrings.Text;
end;

end.
