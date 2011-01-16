{ $Id$ }
{                        ----------------------------------------------
                            GDBTypeInfo.pp  -  Debugger helper class 
                         ----------------------------------------------

 @created(Wed Mar 29th WET 2003)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains a helper class for decoding PType output.


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
}
unit GDBTypeInfo;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Debugger, LclProc, DebugUtils;

(*
  ptype = {
     family = "class" | "record" | "enum" | "set" | "procedure" | "function" | "simple" | "pointer"
    [ ancestor = "...", ]
    [ private = "[" ( "{" name = "...", type = ptype "}" )* "}," ]
    [ protected = "[" ( "{" name = "...", type = ptype "}" )* "}," ]
    [ public = "[" ( "{" name = "...", type = ptype "}" )* "},"]
    [ published = "[" ( "{" name = "...", type = ptype "}" )* "}," ]
    [ members = "[" ( "..." )* "]," | "[" ( "{" name = "...", type = "..." "}" )* "]," ]
    [ args = "[" ( "..." )* "]," ]
    [ result = "..." ]
    [ name = "..." ]
    [ type = "..." ]

  Examples: (tested with fpc 2.4.2 and 2.5.1 (Jan 2011) / gdb 7.0, gdb 7.2
  (excluding the '~"type = '  and the '\n"')

  * procedure x(ArgTFoo: TFoo; var VArgTFoo: TFoo); // TFoo = class end;
  * procedure x(ArgPFoo: PFoo; var VArgPFoo: PFoo); // PFoo = ^TFoo;

  "WhatIs" Results:     Normal                          Param-by-ref
                 Stabs    Dwarf        |               Stabs    Dwarf
    ArgTFoo      TFOO     TFOO         |  VArgTFoo     TFOO     &TFOO
    @ArgTFoo     PFOO     ^TFOO        |  @VArgTFoo    PFOO     ^&TFOO     ## whatis @ArgTFoo  may be ^TFoo under Stabs if no named type PFoo exists
    ArgPFoo      PFOO     PFOO         |  VArgPFoo     PFOO     &PFOO
    @ArgPFoo     PPFOO    ^PFOO        |  @VArgPFoo    PPFOO    ^&PFOO     ## whatis @ArgPFoo  may be ^PFoo under Stabs if no named type PPFoo exists

                 Stabs    Dwarf
    TFoo         TFOO     ^TFOO = class
    PFoo         PFOO     ^TFOO

  "PType" Results:
    ptype Arg<YYY>       ~"type = <???> = class : public TOBJECT \n"   ## followed by lines of fields (exlude inherited)
                       Normal                           Param-by-ref
                 Stabs    Dwarf        |               Stabs    Dwarf
    ArgTFoo      ^TFOO    ^TFOO        |  VArgTFoo     ^TFOO    &TFOO
    @ArgTFoo     ^TFOO    ^TFOO        |  @VArgTFoo    ^TFOO    ^&TFOO
    ArgPFoo      ^TFOO    ^TFOO        |  VArgPFoo     ^TFOO    &TFOO
    @ArgPFoo      ^TFOO   ^TFOO        |  @VArgPFoo    ^TFOO    ^&TFOO

                 Stabs    Dwarf
    TFoo         TFOO     ^TFOO
    PFoo         ^TFOO    ^TFOO

    ==> "ptype SomeVariable" does not differ between TFoo and PFoo
    ==> dwarf ptype is the same for TFoo and PFoo (whatis can tell the diff)

  * procedure x(ArgEnum: TEnum); // TEnum = (One, Two, Three);
  * procedure x(ArgEnumSet: TEnumSet; var VArgEnumSet: TEnumSet); // TEnumSet = set of TEnum;
  * procedure x(ArgSet: TSet; var VArgSet: TSet); // TSet = Set of (Alpha, Beta, Gamma);
  * var VarEnumA: (e1,e2,e3); VarEnumSetA: set of TEnum; VarSetA: Set of (s1,s2,s3);

  "WhatIs" Results (| marks a new line / gdb starts a new line with ~"):
                 Stabs                 Dwarf                     Dwarf without -godwarfset
    ArgEnumSet   TENUMSET              TENUMSET                  TENUMSET
    VArgEnumSet  TENUMSET              &TENUMSET                 &TENUMSET
    ArgSet       TSET                  TSET                      TSET
    VArgSet      TSET                  &TSET                     &TSET
    VarEnumSetA  set of TENUM          set of |ONE..THREE        <invalid unnamed pascal type code 8>
    VarSetA      set of  = (...)       set of |S1..S3            <invalid unnamed pascal type code 8>

    TEnumSet     TENUMSET              set of |ONE..THREE        TENUMSET
    TSet         TSET                  set of |ALPHA..GAMMA      TSET

    ArgEnum      TENUM                        ## same for stabs (both)
    VarEnumA      = (...)                     ## same for stabs (both)
    TEnum        TENUN                        ## same for stabs (both)

  "PType" Results:
                 Stabs                            Dwarf                      Dwarf without -godwarfset

    ArgEnumSet   set of TENUM                     set of |ONE..THREE         TENUMSET
    VArgEnumSet  set of TENUM                     &set of |ONE..THREE        &TENUMSET
    ArgSet       set of  = (ALPHA, BETA, GAMMA)   set of |ALPHA..GAMMA       TSET
    VArgSet      set of  = (ALPHA, BETA, GAMMA)   &set of |ALPHA..GAMMA      &TSET
    VarEnumSetA  set of TENUM                     set of |ONE..THREE         <invalid unnamed pascal type code 8>
    VarSetA      set of  = (S1, S2, S3)           set of |S1..S3             <invalid unnamed pascal type code 8>

    TEnumSet     set of TENUM                     set of |ONE..THREE         TENUMSET
    TSet         set of  = (ALPHA, BETA, GAMMA)   set of |ALPHA..GAMMA       TSET

    ArgEnum      TENUM  = (ONE, TWO, THREE)      ## same for stabs (both)
    VarEnumA      = (E1, E2, E3)                 ## same for stabs (both)
    TEnum        TENUM  = (ONE, TWO, THREE)      ## same for stabs (both)

      ## Alternative new lines:  set of ONE|..THREE|      set of S1|..S3|
      ## All results can be prefixed by ^, for unamed pointertypes (^& for var param)



  TODO: functions ? Stabs seem to always add pointer; dwarf does not?

*)
type

  TGDBPTypeResultFlag =
    (ptprfParamByRef,
     ptprfPointer,
     ptprfNoStructure,     // for Class or Record: no full class declaration, type ends after class keyword; DWARF "whatis TFoo"
                         // includes "record {...}"
     ptprfEmpty
    );
  TGDBPTypeResultFlags = set of TGDBPTypeResultFlag;
  TGDBPTypeResultKind =
    (ptprkError, ptprkSimple, ptprkClass, ptprkRecord,
     ptprkEnum, ptprkSet, ptprkArray, ptprkProcedure, ptprkFunction);

  TGDBPTypeResult = record
    GdbDescription: string;
    Flags: TGDBPTypeResultFlags;
    Kind: TGDBPTypeResultKind;
    Name, BaseName: TPCharWithLen; // BaseName is without ^&
    Declaration: TPCharWithLen;
  end;

  PGDBPTypeRequest = ^TGDBPTypeRequest;
  TGDBPTypeRequest = record
    Request: string;
    Result: TGDBPTypeResult;
    Error: string;
    Next: PGDBPTypeRequest;
  end;

  { TGDBTypes }

  TGDBTypes = class(TDBGTypes)
  public
    constructor CreateFromCSV(AValues: String);
  end;

  { TGDBType }

  TGDBTypeCreationFlag = (gtcfClassIsPointer, gtcfFullTypeInfo, gtcfExprIsType);
  TGDBTypeCreationFlags = set of TGDBTypeCreationFlag;

  TGDBTypeProcessState =
    (gtpsInitial,
     gtpsSimplePointer,
     gtpsClass, gtpsClassPointer, gtpsClassAncestor,
     gtpsFinished
    );
  TGDBTypeProcessRequest =
    (gptrPTypeExpr, gptrWhatisExpr, gptrPTypeOfWhatis,
     gptrPTypeExprDeRef, gptrPTypeExprDeDeRef  // "Foo^", "Foo^^"  for Foo=Object, or &Object
    );
  TGDBTypeProcessRequests = set of TGDBTypeProcessRequest;

  TGDBType = class(TDBGType)
  private
    FInternalTypeName: string;
  private
    FEvalError: boolean;
    FEvalRequest: PGDBPTypeRequest;
    FExpression: string;
    FCreationFlags: TGDBTypeCreationFlags;
    FTypeInfoAncestor: TGDBType;

    FProcessState: TGDBTypeProcessState;
    FProccesReuestsMade: TGDBTypeProcessRequests;
    FReqResults: Array [TGDBTypeProcessRequest] of TGDBPTypeRequest;

    procedure AddTypeReq(var AReq :TGDBPTypeRequest; const ACmd: string = '');
    function RequireRequests(ARequired: TGDBTypeProcessRequests): Boolean;
    function IsReqError(AReqType: TGDBTypeProcessRequest; CheckResKind: Boolean = True): Boolean;
  protected
    procedure Init; override;
  public
    constructor CreateForExpression(const AnExpression: string;
                                    const AFlags: TGDBTypeCreationFlags);
    destructor Destroy; override;
    function ProcessExpression: Boolean;
    property EvalRequest: PGDBPTypeRequest read FEvalRequest;
    property EvalError: boolean read FEvalError;
  public
    // InternalTypeName: include ^ for TObject, if needed
    property InternalTypeName: string read FInternalTypeName;
  end;


function CreatePTypeValueList(AResultValues: String): TStringList;
function ParseTypeFromGdb(const ATypeText: string): TGDBPTypeResult;

implementation

(*
function GetPart(const ASkipTo, AnEnd: array of String; var ASource: String): String;
var
  n, i, idx, SkipLen: Integer;
begin
  idx := 0;
  SkipLen := 0;
  if High(ASkipTo) <> -1
  then begin
    for n := Low(ASkipTo) to High(ASkipTo) do
    begin
      if ASkipTo[n] <> ''
      then begin
        i := Pos(ASkipTo[n], ASource);
        if (i > 0) and ((idx = 0) or (i < idx))
        then begin
          idx := i;
          SkipLen := Length(ASkipTo[n]);
        end;
      end;
    end;
    if idx = 0
    then begin
      Result := '';
      Exit;
    end;
    Delete(ASource, 1, idx + SkipLen - 1);
  end;

  idx := 0;
  for n := Low(AnEnd) to High(AnEnd) do
  begin
    if AnEnd[n] <> ''
    then begin
      i := Pos(AnEnd[n], ASource);
      if (i > 0) and ((idx = 0) or (i < idx))
      then idx := i;
    end;
  end;

  if idx = 0
  then begin
    Result := ASource;
    ASource := '';
  end
  else begin
    Result := Copy(ASource, 1, idx - 1);
    Delete(ASource, 1, idx - 1);
  end;
end;
*)

function CreatePTypeValueList(AResultValues: String): TStringList;
var
  S, Line: String;
  Lines: TStringList;

  procedure DoRecord;
  var
    n: Integer;
    S, Members: String;
  begin
    Result.Add('family=record');
    Members := '';

    //concatinate all lines and skip last end
    S := '';
    for n := 0 to Lines.Count - 2 do
      S := S + Lines[n];

    while S <> '' do
    begin
      if Members <> '' then Members := Members + ',';
      Members := Members + '{name=' + GetPart(['    '], [' '], S);
      Members := Members + ',type=' + GetPart([' : '], [';'], S) + '}';
    end;
    Result.Add('members=[' + Members + ']');
  end;

  procedure DoEnum;
  var
    n: Integer;
    S: String;
  begin
    Result.Add('family=enum');

    S := GetPart(['('], [], Line);
    //concatinate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    S := GetPart([], [')'], S);
    Result.Add('members=[' + StringReplace(S, ' ', '', [rfReplaceAll]) + ']');
  end;

  procedure DoProcedure;
  var
    n: Integer;
    S: String;
  begin
    Result.Add('family=procedure');

    S := GetPart(['('], [''], Line);
    //concatinate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    S := GetPart([''], [')'], S);
    Result.Add('args=[' + StringReplace(S, ', ', ',', [rfReplaceAll]) + ']');
  end;

  procedure DoFunction;
  var
    n: Integer;
    S, Args: String;
  begin
    Result.Add('family=function');

    S := GetPart(['('], [], Line);
    //concatinate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    Args := GetPart([], [')'], S);
    S := GetPart([' : '], [], S);
    Result.Add('args=[' + StringReplace(Args, ', ', ',', [rfReplaceAll]) + ']');
    Result.Add('result=' + S);
  end;

  procedure DoClass;
  begin
    Result.Add('family=class');
    Result.Add('ancestor=' + GetPart([': public '], [' '], Line));
  end;

begin
  Result := TStringList.Create;
  if AResultValues = '' then Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := AResultValues;
    if Lines.Count = 0 then Exit;
    Line := Lines[0];
    Lines.Delete(0);

    S := GetPart(['type = '], [' '], Line);
    if S = '' then Exit;
    if Pos(' = class ', Line) > 0
    then DoClass
    else if S[1] = '^'
    then begin
      Result.Add('family=pointer');
      Result.Add('type=' + GetPart(['^'], [' ='], S));
    end
    else if S = 'set'
    then begin
      Result.Add('family=set');
      Result.Add('type=' + Copy(Line, 5, Length(Line)));
    end
    else if S = 'procedure'
    then DoProcedure
    else if S = 'function'
    then DoFunction
    else if Pos(' = (', Line) > 0
    then DoEnum
    else if Pos(' = record', Line) > 0
    then DoRecord
    else begin
      Result.Add('family=simple');
      Result.Add('type=' + S);
    end;

  finally
    Lines.Free;
  end;
end;

function ParseTypeFromGdb(const ATypeText: string): TGDBPTypeResult;
var
  i, StartIdx, EndIdx: Integer;
  CurPtr, HelpPtr, HelpPtr2, EndPtr, DeclPtr: PChar;

  procedure SkipSpaces(var p: PChar); inline;
  begin
    while (p^ = ' ') do inc(p);
  end;

  function CheckKeyword: TGDBPTypeResultKind;
  begin
    Result := ptprkSimple;
    // Might be: "set of ", "class", "record =", "array [", '<invalid unnamed"...,
    case CurPtr^ of
      's', 'S': begin
          if (EndPtr - CurPtr >= 6 )
          and  (UpperCase(copy(CurPtr, 1, 7)) = 'SET OF ')
          then
            Result := ptprkSet;
        end;
      'r', 'R': begin
          if (EndPtr - CurPtr >= 6 )
          and (UpperCase(copy(CurPtr, 1, 6)) = 'RECORD')
          and ((CurPtr+6)^ in [' ', ')', #13])
          then
            Result := ptprkRecord;
        end;
      'c', 'C': begin
          if (EndPtr - CurPtr >= 5 )
          and (UpperCase(copy(CurPtr, 1, 5)) = 'CLASS')
          and ((CurPtr+5)^ in [' ', ')', #13])
          then
            Result := ptprkClass;
        end;
      'a', 'A': begin
          if (EndPtr - CurPtr >= 5 )
          and (UpperCase(copy(CurPtr, 1, 6)) = 'ARRAY ')
          then
            Result := ptprkArray;
        end;
      '<': begin
          if (EndPtr - CurPtr >= 35 )
          and (copy(CurPtr, 1, 36) = '<invalid unnamed pascal type code 8>')
          then
            Result := ptprkSet;
        end;
      'p', 'P': begin
          if (EndPtr - CurPtr >= 9 )
          and (UpperCase(copy(CurPtr, 1, 9)) = 'PROCEDURE')
          and ((CurPtr+9)^ in [' ', '(', ')', #13])
          then
            Result := ptprkProcedure;
        end;
      'f', 'F': begin
          if (EndPtr - CurPtr >= 8 )
          and (UpperCase(copy(CurPtr, 1, 8)) = 'FUNCTION')
          and ((CurPtr+8)^ in [' ', '(', ')', #13])
          then
            Result := ptprkFunction;
        end;

    end;
  end;

  function CheckIsEnum: Integer;
  var
    p: PChar;
  begin
    Result := -1;
    if CurPtr^ <> '(' then exit;

    p := CurPtr;
    while not(p^ in [')', #0]) do inc(p);
    if (p <= EndPtr) and (p^ = ')') then
      Result := p - CurPtr + 1;
  end;

begin
  Result.GdbDescription := ATypeText;
  Result.Flags := [];
  Result.Kind := ptprkError;
  Result.Name.Ptr := nil;
  Result.Name.Len := 0;
  Result.BaseName.Ptr := nil;
  Result.BaseName.Len := 0;
  Result.Declaration.Ptr := nil;
  Result.Declaration.Len := 0;
  If ATypeText = '' then exit;

(*  // Clean the gdb outpu, remove   ~"...."; replace \n by #13
  if (length(ATypeText) >= 2) and (ATypeText[1] = '~') and (ATypeText[2] = '"') then
    UniqueString(Result.GdbDescription);
  CurPtr := @Result.GdbDescription[1];
  EndPtr := CurPtr;
  while (EndPtr^ <> #0) do begin
    if (EndPtr^ = '~') and ((EndPtr+1)^ = '"') then begin
      inc(EndPtr, 2);
      while not (EndPtr^ in [#0..#31]) do begin
        if (EndPtr^ = '\') then begin
          inc(EndPtr);
          if (EndPtr^ = 'n')
          then CurPtr^ := #13 // internal marker only, no need for OS specific
          else CurPtr^ := EndPtr^;
        end
        else
          CurPtr^ := EndPtr^;
        inc(EndPtr);
        inc(CurPtr);
      end;
      dec(CurPtr);
      if CurPtr^ <> '"' then begin
        // something wrong
        debugln('** WARNING: ptype info format error (end-quote): ' + ATypeText);
        Result.GdbDescription := ATypeText;
        CurPtr := @Result.GdbDescription[length(Result.GdbDescription)] + 1;
        break;
      end;
    end
    else begin
      // something wrong
      debugln('** WARNING: ptype info format error (start-quote): ' + ATypeText);
      Result.GdbDescription := ATypeText;
      CurPtr := @Result.GdbDescription[length(Result.GdbDescription)] + 1;
      break;
    end;
    while (EndPtr^ in [#10, #13]) do inc(EndPtr);
  end;
  SetLength(Result.GdbDescription, CurPtr - @Result.GdbDescription[1]);
*)

  StartIdx := pos('type = ', Result.GdbDescription);
  if StartIdx <= 0 then exit;
  inc(StartIdx, 7);
  CurPtr := @Result.GdbDescription[StartIdx];

  EndIdx := pos(LineEnding, Result.GdbDescription); // the first \n, even if not on the first line
  if EndIdx <= 0 then EndIdx := length(Result.GdbDescription)+1;
  EndPtr := @Result.GdbDescription[EndIdx-1];


  // Pointer indicators
  DeclPtr := CurPtr;
  while True do begin
    case CurPtr^ of
      '^': include(Result.Flags, ptprfPointer);
      '&': include(Result.Flags, ptprfParamByRef);
      else break;
    end;
    inc(CurPtr);
  end;
  SkipSpaces(CurPtr); // shouldn'tever happen

  if CurPtr > EndPtr then begin
    include(Result.Flags, ptprfEmpty);
    exit;
  end;


  if CurPtr^ = '=' then begin
    // un-nmaed type
    inc(CurPtr);
    SkipSpaces(CurPtr);

    i := CheckIsEnum;
    if i > 0 then begin
      // un-named enum // type =  = (e1, e2, e3)
      Result.Kind := ptprkEnum;
      Result.Declaration.Ptr := CurPtr;
      Result.Declaration.Len := i;
      exit;
    end;

    // Unexpected, see if we have a keyword
    Result.Kind := CheckKeyword;
    if Result.Kind = ptprkSimple then begin
      Result.Kind := ptprkError;
      debugln('** WARNING: ptype info format error: ' + ATypeText);
      exit;
    end;
  end

  else
  begin
    if CurPtr^ = '(' then begin
      // type in brackets, eg ^(array...)
      inc(CurPtr);
    end;
    SkipSpaces(CurPtr); // shouldn'tever happen

    Result.Kind := CheckKeyword;
    if Result.Kind = ptprkSimple then begin
      // we may have   type = NAME = ....
      HelpPtr := CurPtr;
      while not (HelpPtr^ in [#0..#31, ' ']) do inc(HelpPtr);
      HelpPtr2 := HelpPtr;
      SkipSpaces(HelpPtr2);
      if ((HelpPtr^ = ' ') and ((HelpPtr2)^ = '='))
      or (HelpPtr^ in [#0, #10, #13])
      then begin
        // Type without space, use as name
        Result.Name.Ptr := DeclPtr; //CurPtr;
        Result.Name.Len := HelpPtr - DeclPtr; // CurPtr;
        while DeclPtr^ in ['&', '^'] do inc(DeclPtr);
        Result.BaseName.Ptr := DeclPtr; //CurPtr;
        Result.BaseName.Len := HelpPtr - DeclPtr; // CurPtr;
        if (HelpPtr^ in [#0, #10, #13]) then exit;

        // now there must be a keyword or set
        CurPtr := HelpPtr2 + 1;
        // Todo: in this case the declaration doe not include the pointer, if any => maybe add flag?
        SkipSpaces(CurPtr);
        DeclPtr := CurPtr;
        i := CheckIsEnum;
        if i > 0 then begin
          Result.Kind := ptprkEnum;
          Result.Declaration.Ptr := CurPtr;
          Result.Declaration.Len := i;
          exit;
        end;

        Result.Kind := CheckKeyword;
        if Result.Kind = ptprkSimple then begin
          Result.Kind := ptprkError;
          debugln('** WARNING: ptype info format error: ' + ATypeText);
          exit;
        end;
      end
      else begin
        // Type is a declaration with spaces
        while EndPtr^ = ' ' do dec(EndPtr);
        Result.Declaration.Ptr := CurPtr;
        Result.Declaration.Len := EndPtr - CurPtr + 1;
        exit;
      end;
    end;
  end;

  // now we should be AT a keyword, we may have a name set already // Enum are handled already too
  while EndPtr^ = ' ' do dec(EndPtr);
  case Result.Kind of
    ptprkClass: begin
        HelpPtr := CurPtr + 5;
        SkipSpaces(HelpPtr);
        if HelpPtr^ in [#10, #13] then include(Result.Flags, ptprfNoStructure);
        Result.Declaration.Ptr := DeclPtr;
        Result.Declaration.Len := EndPtr - DeclPtr + 1;
      end;
    ptprkRecord: begin
        HelpPtr := CurPtr + 6;
        SkipSpaces(HelpPtr);
        Result.Declaration.Ptr := DeclPtr;
        if HelpPtr^ in ['{'] then begin
          include(Result.Flags, ptprfNoStructure);
          Result.Declaration.Len := CurPtr + 6 - DeclPtr;
        end
        else
          Result.Declaration.Len := EndPtr - DeclPtr + 1;
      end;
    ptprkSet: begin
        if CurPtr^ <> '<' then begin;
          Result.Declaration.Ptr := DeclPtr;
          Result.Declaration.Len := EndPtr - DeclPtr + 1;
        end;
      end;
    ptprkArray: begin
        Result.Declaration.Ptr := DeclPtr;
        Result.Declaration.Len := EndPtr - DeclPtr + 1;
      end;
    ptprkProcedure, ptprkFunction: begin
        Result.Declaration.Ptr := DeclPtr;
        Result.Declaration.Len := EndPtr - DeclPtr + 1;
      end;
  end;
end;

{ TGDBPType }

procedure TGDBType.AddTypeReq(var AReq: TGDBPTypeRequest; const ACmd: string = '');
begin
  AReq.Result.Kind := ptprkError;
  AReq.Request := ACmd;
  AReq.Error := '';
  AReq.Next := FEvalRequest;
  FEvalRequest := @AReq;
end;

function TGDBType.RequireRequests(ARequired: TGDBTypeProcessRequests): Boolean;

  function ApplyBrackets(e: string): string;
  var
    i: Integer;
    f: Boolean;
  begin
    Result := e;
    if (e='') or ( (e[1] = '(') and (e[length(e)] = ')') ) then exit;
    f := False;
    for i := 1 to length(e) do
      f := f or not(e[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']);
    if f then
      Result := '(' + Result + ')';
  end;

var
  NeededReq: TGDBTypeProcessRequests;
begin
  NeededReq := ARequired - FProccesReuestsMade;
  Result := NeededReq = [];
  if Result then exit;

  if (gptrPTypeOfWhatis in NeededReq) and not (gptrWhatisExpr in FProccesReuestsMade)
  then begin
    Exclude(NeededReq, gptrPTypeOfWhatis);
    Include(NeededReq, gptrWhatisExpr);
  end;

  FProccesReuestsMade := FProccesReuestsMade + NeededReq;

  if gptrPTypeExpr in NeededReq then
    AddTypeReq(FReqResults[gptrPTypeExpr], 'ptype ' + FExpression);

  if gptrWhatisExpr in NeededReq then
    AddTypeReq(FReqResults[gptrWhatisExpr], 'whatis ' + FExpression);

  if gptrPTypeExprDeRef in NeededReq then
    AddTypeReq(FReqResults[gptrPTypeExprDeRef], 'ptype ' + ApplyBrackets(FExpression) + '^');

  if gptrPTypeExprDeDeRef in NeededReq then
    AddTypeReq(FReqResults[gptrPTypeExprDeDeRef], 'ptype ' + ApplyBrackets(FExpression) + '^^');

  if gptrPTypeOfWhatis in NeededReq then
    AddTypeReq(FReqResults[gptrPTypeOfWhatis], 'ptype ' + PCLenToString(FReqResults[gptrWhatisExpr].Result.BaseName));
end;

function TGDBType.IsReqError(AReqType: TGDBTypeProcessRequest; CheckResKind: Boolean = True): Boolean;
begin
  Result := (not (AReqType in FProccesReuestsMade))
         or (FReqResults[AReqType].Error <> '')
         or (CheckResKind and (FReqResults[AReqType].Result.Kind = ptprkError));
end;

procedure TGDBType.Init;
begin
  inherited Init;
  FProcessState := gtpsFinished;
end;

constructor TGDBType.CreateForExpression(const AnExpression: string;
  const AFlags: TGDBTypeCreationFlags);
begin
  Create(skSimple, ''); // initialize
  FInternalTypeName := '';
  FEvalError := False;
  FExpression := AnExpression;
  FCreationFlags := AFlags;
  FEvalRequest := nil;
  FProcessState := gtpsInitial;
end;

destructor TGDBType.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTypeInfoAncestor);
end;

function TGDBType.ProcessExpression: Boolean;
var
  Lines: TStringList;

  function ClearAmpersand(s: string): string;
  var i: Integer;
  begin
    Result := s;
    i := pos('&', Result);
    if i > 0 then delete(Result, i, 1);
  end;

  procedure SetTypNameFromReq(AReqType: TGDBTypeProcessRequest;
    AnUseBaseName: Boolean = False; ADefaultName: String = '');
  begin
    if IsReqError(AReqType) or (FReqResults[AReqType].Result.BaseName.Len = 0)
    then AReqType := gptrPTypeExpr;

    if AnUseBaseName
    then FTypeName := PCLenToString(FReqResults[AReqType].Result.BaseName)
    else FTypeName := ClearAmpersand(PCLenToString(FReqResults[AReqType].Result.Name));

    if FTypeName = ''
    then FTypeName := ADefaultName;
    FInternalTypeName := FTypeName;
  end;

  Procedure InitLinesFrom(AReq: TGDBPTypeRequest);
  begin
    FreeAndNil(Lines);
    Lines := TStringList.Create;
    Lines.Text := AReq.Result.GdbDescription;
  end;

  procedure DoEnum;
  var
    S: String;
  begin
    FKind := skEnum;

    S := PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration);
    S := GetPart(['('], [')'], S);
    if (S = '') or (S = '...') then
      exit;

    FMembers := TStringList.Create;
    FMembers.Text := StringReplace(S, ' ', #13#10, [rfReplaceAll]);
  end;

  procedure DoProcedure;
  var
    S: String;
  begin
    FKind := skProcedure;

    S := PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration);
    S := GetPart(['('], [')'], S);
    if (S = '') then
      exit;

    FArguments := TGDBTypes.CreateFromCSV(S);
  end;

  procedure DoFunction;
  var
    S: String;
  begin
    FKind := skFunction;

    S := PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration);
    S := GetPart(['('], [')'], S);
    if (S = '') then
      exit;

    FArguments := TGDBTypes.CreateFromCSV(S);

    S := PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration);
    FResult := TGDBType.Create(skSimple, GetPart([' : '], [], S));
  end;

  procedure DoSet;
  var
    S: String;
  begin
    FKind := skSet;

    S := PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration);
    S := GetPart(['('], [')'], S);
    if (S = '') or (S = '...') then
      exit;

    FMembers := TStringList.Create;
    FMembers.Text := StringReplace(StringReplace(S, ',', #13#10, [rfReplaceAll]), ' ', '', [rfReplaceAll]);
  end;

  {%region    * Record * }
  procedure DoRecord;
  var
    n: Integer;
    S, S1, S2: String;
    Field: TDBGField;
  begin
    if (FTypeName = 'Variant') or
       (FTypeName = 'VARIANT') then
      FKind := skVariant
    else
    if (FTypeName = 'ShortString') or
       (FTypeName = 'SHORTSTRING') or
       (FTypeName = '&ShortString') then
      FKind := skSimple
    else
      FKind := skRecord;

    FFields := TDBGFields.Create;
    InitLinesFrom(FReqResults[gptrPTypeExpr]);

    //concatenate all lines and skip last end
    for n := 1 to Lines.Count - 2 do begin
      S := Lines[n];
      S1 := Trim(GetPart([' '], [':'], S));
      S2 := Trim(GetPart([':'], [';'], S));
      Field := TDBGField.Create(
        S1,
        TGDBType.Create(skSimple, S2),
        flPublic
      );
      FFields.Add(Field);
    end;
  end;
  {%endregion    * Record * }

  {%region    * Class * }
  procedure DoClass;
  var
    n, i: Integer;
    S, S2: String;

    Name: String;
    DBGType: TDBGType;
    Location: TDBGFieldLocation;
    Flags: TDBGFieldFlags;
  begin
    include(FAttributes, saInternalPointer);
    FKind := skClass;
    InitLinesFrom(FReqResults[gptrPTypeExpr]);
    FFields := TDBGFields.Create;
    if Lines.Count < 1 then exit;
    s := Lines[0];
    FAncestor := GetPart([': public '], [' '], s);

    Location := flPublished;
    n := 0;
    while n <  Lines.Count - 2 do
    begin
      inc(n);
      S := Lines[n];
      if S = '' then Continue;
      if S = 'end' then break;
      if S = '  private' then Location := flPrivate
      else if S = '  protected' then Location := flProtected
      else if S = '  public' then Location := flPublic
      else if S = '  published' then Location := flPublished
      else begin
        Flags := [];
        if Pos('  procedure ', S) > 0
        then begin
          Name := GetPart(['procedure '], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skProcedure,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
        end
        else if Pos('  destructor  ~', S) > 0
        then begin
          Name := GetPart(['destructor  ~'], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skProcedure,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
          Include(Flags, ffDestructor);
        end
        else if Pos('  constructor ', S) > 0
        then begin
          Name := GetPart(['constructor '], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skFunction,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S)),
            TGDBType.Create(skSimple, GetPart([' : '], [';'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
          Include(Flags, ffConstructor);
        end
        else if Pos('  function ', S) > 0
        then begin
          Name := GetPart(['function  '], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skFunction,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S)),
            TGDBType.Create(skSimple, GetPart([' : '], [';'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
        end
        else begin
          Name := GetPart(['    '], [' '], S);
          S2 := GetPart([' : '], [';'], S);
          if (lowercase(copy(S2, 1, 7)) = 'record ') then begin
            i := 1;
            while (n <  Lines.Count - 2) and (i > 0) do
            begin
              inc(n);
              S := Lines[n];
              if S = '' then Continue;
              if pos(': record ', S) > 0 then inc(i);
              if pos(' end;', S) > 0 then dec(i);
              S2 := S2 + ' ' + Trim(S);
            end;
          end;
          DBGType := TGDBType.Create(skSimple, S2);
        end;
        FFields.Add(TDBGField.Create(Name, DBGType, Location, Flags, FTypeName));
      end;
    end;
  end;

  procedure ProcessClassPointer;
  begin
    FProcessState := gtpsClassPointer;
    if not RequireRequests([gptrWhatisExpr]) then
      exit;

      FKind := skPointer;
      SetTypNameFromReq(gptrWhatisExpr);
      Result := True;
      // ====> DONE
  end;

  procedure ProcessClassAncestor;
  var
    r: PGDBPTypeRequest;
    i: Integer;
  begin
    FProcessState := gtpsClassAncestor;

    If FTypeInfoAncestor = nil then begin
      FTypeInfoAncestor := TGDBType.CreateForExpression(FAncestor, FCreationFlags + [gtcfExprIsType]);
    end;

    if FTypeInfoAncestor.ProcessExpression then begin
      // add ancestor
      if FTypeInfoAncestor.FFields <> nil then
        for i := 0 to FTypeInfoAncestor.FFields.Count - 1 do
          FFields.Add(FTypeInfoAncestor.FFields[i]);
      Result := True;
    end
    else begin
      if FTypeInfoAncestor.EvalError then begin
        debugln('TGDBType: EvaleError in ancestor');
        Result := True; // unable to get ancestor
        exit;
      end;
      if (EvalRequest =  nil) then
        FEvalRequest := FTypeInfoAncestor.EvalRequest
      else begin
        r := FEvalRequest;
        while r^.Next <> nil do r := r^.Next;
        r^.Next := FTypeInfoAncestor.EvalRequest;
      end;
    end;
  end;

  procedure ProcessClass;
  var
    t: TGDBTypeProcessRequest;
  begin
    FProcessState := gtpsClass;

    if (gtcfExprIsType in FCreationFlags) then begin
      SetTypNameFromReq(gptrPTypeExpr, True);
      DoClass;
      if (gtcfFullTypeInfo in FCreationFlags) and (FAncestor <> '')
      then ProcessClassAncestor
      else Result := True; // ====> DONE
      exit;
    end;

    if saRefParam in FAttributes
    then t := gptrPTypeExprDeDeRef  // &Class (var param; dwarf)
    else t := gptrPTypeExprDeRef;   // Class

    if not RequireRequests([gptrPTypeExpr, t])
    then exit;

    if IsReqError(t)
    then debugln('Failed "ptype expr^[^]" request for class expression');

    if (not IsReqError(t)) and (ptprfPointer in FReqResults[t].Result.Flags)
    then begin
      ProcessClassPointer;
      exit;
    end
    else begin
      // Handle Error in ptype^ as normal class
      // May need a whatis, if aliased names are needed "type TFooAlias = type TFoo"
      SetTypNameFromReq(gptrWhatisExpr, True);
      DoClass;
      if (gtcfFullTypeInfo in FCreationFlags) and (FAncestor <> '')
      then ProcessClassAncestor
      else Result := True; // ====> DONE
    end;
  end;
  {%endregion    * Class * }

  {%region    * Simple * }
  procedure ProcessSimplePointer;
  begin
    FProcessState := gtpsSimplePointer;
    // there may be multiply levels of pointer, get the name of this pointer
    if not RequireRequests([gptrPTypeExpr, gptrWhatisExpr])
    then exit;

    FKind := skPointer;
    if not IsReqError(gptrWhatisExpr) and (FReqResults[gptrWhatisExpr].Result.Kind = ptprkSimple)
    then begin
      // Whatis result is ok
      if (ptprfParamByRef in FReqResults[gptrWhatisExpr].Result.Flags) then
        include(FAttributes, saRefParam);
      SetTypNameFromReq(gptrWhatisExpr);
    end
    else begin
      // Whatis result failed
      SetTypNameFromReq(gptrPTypeExpr);
    end;
    Result := True;
    // ====> DONE
  end;
  {%endregion    * Simple * }

  procedure ProcessInitial;
  var
    i: Integer;
  begin
    if FReqResults[gptrPTypeExpr].Error <> '' then begin
      FEvalError := True;
      exit;
    end;

    if (ptprfParamByRef in FReqResults[gptrPTypeExpr].Result.Flags) then
      include(FAttributes, saRefParam);

    case FReqResults[gptrPTypeExpr].Result.Kind of
      //ptprkError: ;
      //ptprkSimple: ;
      ptprkClass: begin
          ProcessClass;
          exit;
        end;
      //ptprkRecord: ;
      //ptprkEnum: ;
      //ptprkSet: ;
      //ptprkArray: ;
      //ptprkProcedure: ;
      //ptprkFunction: ;
    end;


    if (ptprfPointer in FReqResults[gptrPTypeExpr].Result.Flags)
    and ( (FReqResults[gptrPTypeExpr].Result.Kind in
           [ptprkSimple, ptprkRecord, ptprkEnum, ptprkSet])
         or ( (gtcfClassIsPointer in FCreationFlags)
              and (FReqResults[gptrPTypeExpr].Result.Kind in [ptprkProcedure, ptprkFunction])  )
        )
    then begin
      ProcessSimplePointer;
      exit;
    end;

    if (ptprfParamByRef in FReqResults[gptrPTypeExpr].Result.Flags)
    and not (FReqResults[gptrPTypeExpr].Result.Kind in [ptprkError])
    then begin
      // could be a pointer // need ptype of whatis
      if not RequireRequests([gptrWhatisExpr])
      then exit;

      if (FReqResults[gptrWhatisExpr].Result.BaseName.Len > 0)
      then begin
        if not RequireRequests([gptrPTypeOfWhatis])
        then exit;

        if (not IsReqError(gptrPTypeOfWhatis, False))
        and (ptprfPointer in FReqResults[gptrPTypeOfWhatis].Result.Flags) then begin
          // pointer
          FKind := skPointer;
          SetTypNameFromReq(gptrWhatisExpr);
          Result := True;
          // ====> DONE
          exit;
        end;
      end;
    end;

    case FReqResults[gptrPTypeExpr].Result.Kind of
      ptprkError: begin
          // could be empty pointer @ArgProcedure
          Result := True; // nothing to be done, keep simple type, no name
        end;
      ptprkSimple: begin
          // may only need whatis, if current name isn't usable?
          if not RequireRequests([gptrWhatisExpr])
          then exit;

          SetTypNameFromReq(gptrWhatisExpr, True);
          FKind := skSimple;
          Result := True;
          // ====> DONE
        end;
      ptprkRecord: begin
          SetTypNameFromReq(gptrWhatisExpr, True);
          DoRecord;
          Result := True;
          // ====> DONE
        end;
      ptprkEnum: begin
          SetTypNameFromReq(gptrWhatisExpr, True);
          FTypeDeclaration := ClearAmpersand(PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration));
          DoEnum;
          Result := True;
          // ====> DONE
        end;
      ptprkSet: begin
          if not RequireRequests([gptrWhatisExpr])
          then exit;

          SetTypNameFromReq(gptrWhatisExpr, True);
          // TODO: resolve enum-name (set of SomeEnum) if mode-full ?
          FTypeDeclaration := ClearAmpersand(PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration));
          i := pos('set of  = ', FTypeDeclaration);
          if  i > 0 then delete(FTypeDeclaration, i+7, 3);
          DoSet;
          Result := True;
          // ====> DONE
        end;
      ptprkArray: begin
          if not RequireRequests([gptrWhatisExpr])
          then exit;

          FKind := skSimple;
          SetTypNameFromReq(gptrWhatisExpr, True);
          FTypeDeclaration := ClearAmpersand(PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration));
          Result := True;
          // ====> DONE
        end;
      ptprkProcedure: begin
          // under stabs, procedure/function are always pointer // pointer to proc/func return empty type
          if (gtcfClassIsPointer in FCreationFlags) // Dwarf
          and (ptprfPointer in FReqResults[gptrPTypeExpr].Result.Flags)
          then begin
            ProcessSimplePointer;
            exit;
          end;

          if not RequireRequests([gptrWhatisExpr])
          then exit;

          SetTypNameFromReq(gptrWhatisExpr, True, 'procedure');
          DoProcedure;
          Result := True;
          // ====> DONE
        end;
      ptprkFunction: begin
          // under stabs, procedure/function are always pointer // pointer to proc/func return empty type
          if (gtcfClassIsPointer in FCreationFlags) // Dwarf
          and (ptprfPointer in FReqResults[gptrPTypeExpr].Result.Flags)
          then begin
            ProcessSimplePointer;
            exit;
          end;

          if not RequireRequests([gptrWhatisExpr])
          then exit;

          SetTypNameFromReq(gptrWhatisExpr, True, 'function');
          DoFunction;
          Result := True;
          // ====> DONE
      end;
    end;
  end;

var
  OldProcessState: TGDBTypeProcessState;
  OldReqMade: TGDBTypeProcessRequests;
  wi: TGDBTypeProcessRequests;
begin
  Result := False;
  FEvalRequest := nil;
  Lines := nil;
  OldProcessState := FProcessState;
  OldReqMade := FProccesReuestsMade;

  if (gtcfFullTypeInfo in FCreationFlags)
  and not (gtcfExprIsType in FCreationFlags)
  then wi := [gptrWhatisExpr]
  else wi := [];

  if not RequireRequests([gptrPTypeExpr]+wi)
  then exit;

  case FProcessState of
    gtpsInitial:         ProcessInitial;
    gtpsSimplePointer:   ProcessSimplePointer;
    gtpsClass:           ProcessClass;
    gtpsClassPointer:    ProcessClassPointer;
    gtpsClassAncestor:   ProcessClassAncestor;
  end;

  FreeAndNil(Lines);
  if Result
  then FProcessState := gtpsFinished;

  if (FProcessState = OldProcessState) and (FProccesReuestsMade = OldReqMade)
  and (not Result) and (FEvalRequest = nil)
  then begin
    debugln('ERROR: detected state loop in ProcessExpression');
    Result := True;
  end;
end;

{ TGDBPTypes }

constructor TGDBTypes.CreateFromCSV(AValues: String);
var
  GDBType: TGDBType;
begin
  Create;
  while AValues <> '' do
  begin
    GDBType := TGDBType.Create(skSimple, GetPart([], [', '], AValues));
    FList.Add(GDBType);
    {if Length(AValues) >= 2 then} Delete(AValues, 1, 2);
  end;
end;

end.
