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
  complete text as whole instead of as line by line as in TStringList.
  
  UNDER CONSTRUCTION by Mattias Gaertner
  
  ToDo:
    - Capacity
    - Add
    - Delete
    - Exchange
    - Insert
    - GetObject
    - Put
    - PutObject
    - Sort
    - CustomSort
    - Find
    - Index
}
unit TextStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLStrConsts;
  
type
  TLineRange = record
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
    FLineRanges: ^TLineRange;// array of TLineRange
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
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure SetText(TheText: PChar); override;
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
  //writeln('[TTextStrings.BuildLineRanges] A Self=',HexStr(Cardinal(Self),8),',LineCount=',FLineCount,' Len=',SourceLength);
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
  // build line range list
  if FLineCount>0 then begin
    ArraySize:=FLineCount*SizeOf(TLineRange);
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
  //writeln('[TTextStrings.BuildLineRanges] END ',FLineCount);
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

end.

