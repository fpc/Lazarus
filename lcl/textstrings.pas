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
}
unit TextStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type
  TLineRange = record
    StartPos, EndPos: integer;
    Line: string;
  end;

  TTextStrings = class(TStrings)
  protected
    FArraysValid: boolean;
    FLineCount: integer;
    FLineRanges: ^TLineRange;// array of TLineRange
    FText: string;
    function GetTextStr: string; override;
    procedure SetTextStr(const Value: string); override;
    procedure BuildArrays; virtual;
    function GetCount: Integer; override;
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    procedure ClearArrays;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure SetText(TheText: PChar); virtual;
  public
    property Text: string read FText write SetTextStr;
  end;

implementation

{ TTextStrings }

function TTextStrings.GetTextStr: string;
begin
  Result:=FText;
end;

procedure TTextStrings.SetTextStr(const Value: string);
begin
  if FText=AValue then exit;
  FText:=AValue;
  FArraysValid:=false;
end;

procedure TTextStrings.BuildArrays;
var
  p, line: integer;
  l: Integer;
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
    GetMem(FLineRanges,FLineCount*SizeOf(TLineRange));
    p:=1;
    line:=0;
    FLineRanges[line].StartPos:=1;
    FLineRanges[FLineCount-1].EndPos:=fSrcLen+1;
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
  if not FArraysValid then BuildLineRanges;
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
  BuildLineRanges;
  if (Index<0) or (Index>=FLineCount) then
    Error(SListIndexError,Index);
  // ToDo
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

