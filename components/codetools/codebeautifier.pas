{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Functions to beautify code.
    Goals:
      - Highly customizable.
      - Beautification of whole sources. For example a unit, or several
        sources.
      - Beautification of parts of sources. For example selections.
      - Beautification of insertion source. For example beautifying code, that
        will be inserted in another source.
      - Working with syntax errors. The beautification will try its best to
        work, even if the source contains errors.
      - Does not ignore comments and directives
      - Contexts: statements, declarations

  Examples for beautification styles:

    if expr then
    begin
      ;
    end;

    if expr then
      ...
    else
      ...;
}
unit CodeBeautifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeCache, BasicCodeTools;
  
type
  TBeautifySplit =(
    bsNone,
    bsInsertSpace, // insert space before
    bsNewLine,     // break line, no indent
    bsEmptyLine,   // insert empty line, no indent
    bsNewLineAndIndent, // break line, indent
    bsEmptyLineAndIndent, // insert empty line, indent
    bsNewLineUnindent,
    bsEmptyLineUnindent,
    bsNoSplit   // do not break line here when line too long
    );
    
  TWordPolicy = (
    wpNone,
    wpLowerCase,
    wpUpperCase,
    wpLowerCaseFirstLetterUp
    );

  TFABContext = (
    cbcNone,
    cbcRepeat,
    cbcUntil
    );

  TOnGetFABExamples = procedure(Sender: TObject; Code: TCodeBuffer;
                                out CodeBuffers: TFPList) of object;

  TFABIndentation = record
    Indent: integer;
    UseTabs: boolean;
    InsertEmptyLines: integer;
  end;

  { TFullyAutomaticBeautifier }

  TFullyAutomaticBeautifier = class
  private
    FOnGetExamples: TOnGetFABExamples;
    FAtomStarts: PInteger;
    FAtomCapacity: integer;
    FAtomCount: integer;
    procedure ParseSource(const Source: string; NewNestedComments: boolean);
    function IndexOfAtomInFront(CleanPos: integer): integer;
  public
    Src: string;
    SrcLen: integer;
    NestedComments: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetIndent(const Source: string; CleanPos: integer;
                       NewNestedComments: boolean;
                       out Indent: TFABIndentation): boolean;
    property OnGetExamples: TOnGetFABExamples read FOnGetExamples write FOnGetExamples;
  end;

implementation

{ TFullyAutomaticBeautifier }

procedure TFullyAutomaticBeautifier.ParseSource(const Source: string;
  NewNestedComments: boolean);
var
  AtomStart: integer;
  MinAtomCapacity: Integer;
  p: Integer;
begin
  Src:=Source;
  SrcLen:=length(Src);
  NestedComments:=NewNestedComments;
  FAtomCount:=0;
  MinAtomCapacity:=SrcLen div 4;
  if MinAtomCapacity<1024 then
    MinAtomCapacity:=1024;
  if FAtomCapacity<MinAtomCapacity then begin
    FAtomCapacity:=MinAtomCapacity;
    ReAllocMem(FAtomStarts,FAtomCapacity*SizeOf(integer));
  end;
  p:=1;
  repeat
    ReadRawNextPascalAtom(Src,p,AtomStart,NestedComments);
    if p>SrcLen then break;
    FAtomStarts[FAtomCount]:=AtomStart;
    inc(FAtomCount);
    if FAtomCount>FAtomCapacity then begin
      FAtomCapacity:=FAtomCapacity*2;
      ReAllocMem(FAtomStarts,FAtomCapacity*SizeOf(integer));
    end;
  until false;
end;

function TFullyAutomaticBeautifier.IndexOfAtomInFront(CleanPos: integer
  ): integer;
// returns index in FAtomStarts of atom in front
// if CleanPos is start of an atom the atom in front is returned
// default: -1
var
  l: Integer;
  r: LongInt;
  m: Integer;
  p: LongInt;
begin
  l:=0;
  r:=FAtomCount-1;
  while l<=r do begin
    m:=(l+r) shr 1;
    p:=FAtomStarts[m];
    if p>CleanPos then
      r:=m-1
    else if p<CleanPos then begin
      if l=r then exit(m);
      l:=m+1;
    end else
      exit(m-1);
  end;
  Result:=-1;
end;

constructor TFullyAutomaticBeautifier.Create;
begin

end;

destructor TFullyAutomaticBeautifier.Destroy;
begin
  Clear;
  ReAllocMem(FAtomStarts,0);
  FAtomCapacity:=0;
  inherited Destroy;
end;

procedure TFullyAutomaticBeautifier.Clear;
begin
  FAtomCount:=0;
end;

function TFullyAutomaticBeautifier.GetIndent(const Source: string;
  CleanPos: integer; NewNestedComments: boolean;
  out Indent: TFABIndentation): boolean;
var
  AtomIndex: LongInt;
begin
  FillByte(Indent,SizeOf(Indent),0);

  // parse source
  ParseSource(Source,NewNestedComments);

  // find context
  AtomIndex:=IndexOfAtomInFront(CleanPos);
  if AtomIndex<0 then begin
    // in comments/space in front of any code
    exit(false);
  end;


end;

end.

