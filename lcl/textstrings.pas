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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
    - Exchange
    - Put
    - Sort
    - CustomSort
    - Find
    - Index
    - Add
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
    TheObject: TObject;
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
    function GetTextStr: string; override;
    procedure SetTextStr(const AValue: string); override;
    procedure BuildArrays; virtual;
    function GetCount: Integer; override;
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    procedure ClearArrays;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AnObject: TObject); override;
    function GetLineLen(Index: integer; IncludeNewLineChars: boolean): integer;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure SetText(TheText: PChar); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure MakeTextBufferUnique;
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
begin
  if (FUpdateCount=0) then
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
begin
  if not FArraysValid then BuildArrays;
  NewLineLen:=length(S);
  if Index<FLineCount then
    NewStartPos:=FLineRanges[Index].StartPos
  else
    NewStartPos:=length(FText);
  NewLineCharCount:=0;
  if (NewLineLen>0) and (S[NewLineLen] in [#10,#13]) then begin
    inc(NewLineCharCount);
    if (NewLineLen>1)
    and (S[NewLineLen-1] in [#10,#13])
    and (S[NewLineLen-1]<>S[NewLineLen]) then
      inc(NewLineCharCount);
  end;
  // adjust text
  System.Insert(S,FText,NewStartPos);
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
var
  LineLen1: Integer;
  LineLen2: Integer;
begin
  if Index1=Index2 then exit;
  if not FArraysValid then BuildArrays;
  LineLen1:=GetLineLen(Index1,true);
  LineLen2:=GetLineLen(Index2,true);
  if (LineLen1<1) and (LineLen2<1) then exit;
  // adjust text
  MakeTextBufferUnique;

  
end;

procedure TTextStrings.MakeTextBufferUnique;
begin
  // make string unique (refcount=1) to be able to edit it directly
  UniqueString(FText);
end;

end.

