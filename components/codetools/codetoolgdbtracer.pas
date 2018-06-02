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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Functions to parse GDB stacktraces, dismangle FPC identifiers and
    find the stacktrace identifiers in Pascal sources.

  ToDo:
    - unit names with underscores
    - unit names with points
    - procs starting with underscore
}
unit CodetoolGDBTracer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase, KeywordFuncLists, contnrs;

type
  TCTGDBMangledItemKind = (
    cgmiNone,
    cgmiUnknown,  // '??'
    cgmiProgram,  // program, e.g. P$identifier
    cgmiCompiler, // compiler function, like start, _start, PASCALMAIN
    cgmiUnit,     // unit, e.g. SYSTEM
    cgmiStructure, // class, procedure
    cgmiParameter, // function parameter name
    cgmiParameterType, // function parameter type
    cgmiResultType,    // result type
    cgmiError      // syntax error
    );
  TCTGDBMangledItemTypes = set of TCTGDBMangledItemKind;

  { TCTGDBMangledItem }

  TCTGDBMangledItem = class
  public
    Kind: TCTGDBMangledItemKind;
    Identifier: string;
    function AsString: string;
  end;

  TCTArrayOfGDBMangledItems = array of TCTGDBMangledItem;


  { TCTGDBTraceLine }

  TCTGDBTraceLine = class
  public
    LineNumber: integer; // 1 based
    Source: string;
    Depth: integer;
    MangledIdentifier: string;
    GDBAddress: string; // e.g. '0x007489de'
    GDBFilename: string;
    GDBLine, GDBCol: integer;
    MangledItems: TCTArrayOfGDBMangledItems;
    Error: string;
    ErrorCol: integer;
    constructor Create;
    destructor Destroy; override;
    procedure WriteToStream(Indent: integer; s: TStream);
    procedure Parse;
  end;

  { TCTGDBTracer }

  TCTGDBTracer = class
  private
    fLines: TObjectList; // list of TCTGDBTraceLine
    FTraceText: string;
    fCurP: PChar;
    fLineStart: PChar;
    fLineNumber: integer;
    function GetCurrentLine: string;
    procedure AddUnknownLine;
    function GetLineCount: integer;
    function GetLines(Index: integer): TCTGDBTraceLine;
    procedure SetTraceText(AValue: string);
    procedure SkipLine;
    procedure TraceToLines;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property TraceText: string read FTraceText write SetTraceText;
    property LineCount: integer read GetLineCount;
    property Lines[Index: integer]: TCTGDBTraceLine read GetLines;
    function AsString: string;
    procedure WriteToStream(s: TStream);
    procedure ParseLines;
  end;

procedure DemangleGDBIdentifier(Mangled: string;
  var MangledItems: TCTArrayOfGDBMangledItems);

function dbgs(k: TCTGDBMangledItemKind): string; overload;
function dbgs(Mangles: TCTArrayOfGDBMangledItems): string; overload;

implementation

procedure DemangleGDBIdentifier(Mangled: string;
  var MangledItems: TCTArrayOfGDBMangledItems);
{ examples:
fpc_raiseexception
SYSUTILS_RUNERRORTOEXCEPT$LONGINT$POINTER$POINTER
??
EXTTOOLEDITDLG_TEXTERNALTOOLMENUITEMS_$__LOAD$TCONFIGSTORAGE$$TMODALRESULT
PASCALMAIN
SYSTEM_FPC_SYSTEMMAIN$LONGINT$PPCHAR$PPCHAR

P$TESTSTACKTRACE1_TMAINCLASS_$_TSUBCLASS_$__RAISESOMETHING$ANSISTRING
start
}

  procedure Add(Kind: TCTGDBMangledItemKind; const Identifier: string);
  var
    l: Integer;
    Item: TCTGDBMangledItem;
  begin
    Item:=TCTGDBMangledItem.Create;
    Item.Kind:=Kind;
    Item.Identifier:=Identifier;
    l:=length(MangledItems);
    SetLength(MangledItems,l+1);
    MangledItems[l]:=Item;
  end;

var
  p: PChar;
  StartP: PChar;

  function Extract: string;
  begin
    Result:=copy(Mangled,StartP-PChar(Mangled)+1,p-StartP);
  end;

  procedure ExpectedButFound(const Expected: string);
  begin
    Add(cgmiError,'expected '+Expected+', but found '
      +dbgstr(p^)+' after "'+dbgstr(Mangled,1,p-PChar(Mangled))+'"');
  end;

  function ReadIdentifier: boolean;
  begin
    if not (p^ in ['a'..'z','A'..'Z']) then begin
      ExpectedButFound('identifier');
      exit(false);
    end;
    StartP:=p;
    while p^ in ['a'..'z','A'..'Z'] do inc(p);
    Result:=true;
  end;

var
  Identifier: String;
begin
  SetLength(MangledItems,0);
  if Mangled='' then exit;
  p:=PChar(Mangled);
  if p^='?' then begin
    // for example: ??
    Add(cgmiUnknown,Mangled);
    exit;
  end;
  if (p^='P') and (p^='$') then begin
    // program, for example: P$TESTSTACKTRACE1
    inc(p,2);
    StartP:=p;
    while p^ in ['a'..'z','A'..'Z'] do inc(p);
    Add(cgmiProgram,Extract);
  end else if p^ in ['A'..'Z','a'..'z'] then begin
    StartP:=p;
    while p^ in ['a'..'z','A'..'Z'] do inc(p);
    Identifier:=Extract;
    if p^=#0 then begin
      // Compiler function, e.g. start, _start, PASCALMAIN
      Add(cgmiCompiler,Identifier);
      exit;
    end;
    // unit, for example SYSTEM
    Add(cgmiUnit,Identifier);
  end else begin
    // syntax error
    Add(cgmiError,'unknown format');
    exit;
  end;

  repeat
    // read sub identifiers
    if p^='_' then begin
      inc(p);
      if (p^='$') then begin
        // for example _$_ or _$__
        inc(p);
        if p^<>'_' then begin
          ExpectedButFound('_');
          exit;
        end;
        inc(p);
        if p^='_' then begin
          // _$__
          inc(p);
        end;
      end;
    end else if p^='$' then begin
      // parameters, p1$p2$p3
      if MangledItems[length(MangledItems)-1].Kind<>cgmiStructure then begin
        ExpectedButFound('_');
        exit;
      end;
      repeat
        inc(p);
        // read parameter type
        if not ReadIdentifier then exit;
        Add(cgmiParameterType,Extract);
      until (p^<>'$') or (not IsIdentStartChar[p[1]]);
      if (p^='$') and (p[1]='$') then begin
        // function result type
        inc(p,2);
        if not ReadIdentifier then exit;
        Add(cgmiResultType,Extract);
      end;
      continue;
    end else if p^=#0 then begin
      exit;
    end else begin
      ExpectedButFound('_');
      exit;
    end;

    if not ReadIdentifier then exit;
    Add(cgmiStructure,Extract);
  until false;
end;

function dbgs(k: TCTGDBMangledItemKind): string;
begin
  case k of
  cgmiNone: Result:='None';
  cgmiUnknown: Result:='Unknown';
  cgmiProgram: Result:='Program';
  cgmiCompiler: Result:='Compiler';
  cgmiUnit: Result:='Unit';
  cgmiStructure: Result:='Structure';
  cgmiParameter: Result:='Parameter';
  cgmiParameterType: Result:='ParameterType';
  cgmiResultType: Result:='ResultType';
  cgmiError: Result:='Error';
  else Result:='?';
  end;
end;

function dbgs(Mangles: TCTArrayOfGDBMangledItems): string;
var
  i: Integer;
begin
  Result:='[';
  for i:=0 to length(Mangles)-1 do begin
    if i>0 then Result+=',';
    Result+=Mangles[i].AsString;
  end;
  Result+=']';
end;

{ TCTGDBMangledItem }

function TCTGDBMangledItem.AsString: string;
begin
  Result:=dbgs(Kind)+':'+Identifier;
end;

{ TCTGDBTraceLine }

constructor TCTGDBTraceLine.Create;
begin
  Depth:=-1;
end;

destructor TCTGDBTraceLine.Destroy;
begin
  inherited Destroy;
end;

procedure TCTGDBTraceLine.WriteToStream(Indent: integer; s: TStream);

  procedure w(const h: string);
  begin
    if h='' then exit;
    s.Write(h[1],length(h));
  end;

begin
  // source
  w(Space(Indent));
  w('Source={'+Source+'}');
  w(LineEnding);
  // error
  if Error<>'' then begin
    w(Space(Indent));
    w('Error(');
    w(IntToStr(LineNumber));
    if ErrorCol>0 then begin
      w(',');
      w(IntToStr(ErrorCol));
    end;
    w('): '+Error);
    w(LineEnding);
  end;
  if MangledIdentifier<>'' then begin
    w(Space(Indent));
    w('MangledIdentifier='+MangledIdentifier);
    w(LineEnding);
    if length(MangledItems)>0 then begin
      w(Space(Indent));
      w('Demangled='+dbgs(MangledItems));
      w(LineEnding);
    end;
  end;
  if GDBFilename<>'' then begin
    w(Space(Indent));
    w('GDB Source position='+GDBFilename+'('+IntToStr(GDBLine)+','+IntToStr(GDBCol)+')');
    w(LineEnding);
  end;
end;

procedure TCTGDBTraceLine.Parse;
{ Examples:
#0  0x00020e16 in fpc_raiseexception ()
#2  0x00024e48 in SYSTEM_HANDLEERRORADDRFRAME$LONGINT$POINTER$POINTER ()
#3  0xbffff548 in ?? ()
#9  0x00011124 in PASCALMAIN ()

#0 DOHANDLEMOUSEACTION (this=0x14afae00, ANACTIONLIST=0x14a96af8,ANINFO=...) at synedit.pp:3000
}
var
  p: PChar;
  i: Integer;
  StartP: PChar;
  Level: Integer;

  procedure UnexpectedError(Expected: string);
  begin
    ErrorCol:=p-PChar(Source)+1;
    Error:='expected ' +Expected+', but found '+DbgStr(p^);
  end;

begin
  Depth:=-1;
  MangledIdentifier:='';
  GDBAddress:='';
  GDBFilename:='';
  GDBLine:=0;
  GDBCol:=0;

  if Source='' then begin
    Error:='Empty line';
    exit;
  end;
  p:=PChar(Source);

  // read #
  while p^ in [' ',#9] do inc(p);
  if p^<>'#' then begin
    UnexpectedError('#');
    exit;
  end;
  inc(p);

  // read stack depth number (decimal)
  if not (p^ in ['0'..'9']) then begin
    UnexpectedError('decimal number');
    exit;
  end;
  i:=0;
  while p^ in ['0'..'9'] do begin
    if i>100000 then begin
      UnexpectedError('short decimal number');
      exit;
    end;
    i:=i*10+ord(p^)-ord('0');
    inc(p);
  end;
  Depth:=i;

  // skip space
  while p^ in [' ',#9] do inc(p);

  if (p^='0') and (p[1]='x') then begin
    // format: <hexnumber> in <mangledidentifier> ()
    StartP:=p;
    inc(p,2);
    while IsHexNumberChar[p^] do inc(p);
    GDBAddress:=copy(Source,StartP-PChar(Source)+1,p-StartP);

    // skip space
    while p^ in [' ',#9] do inc(p);

    // read 'in'
    if (p^<>'i') or (p[1]<>'n') then begin
      UnexpectedError('in');
      exit;
    end;
    inc(p,2);

    // skip space
    while p^ in [' ',#9] do inc(p);
  end;

  if (p^ in ['A'..'Z','a'..'z','_','?','$']) then begin
    // format: <gdbidentifier> (<parameter list>) at gdbfilename:gdbline
    // format: <mangledidentifier> ()

    // read identifier
    StartP:=p;
    while p^ in ['a'..'z','A'..'Z','0'..'9','_','?','$'] do inc(p);
    MangledIdentifier:=copy(Source,StartP-PChar(Source)+1,p-StartP);

    // skip space
    while p^ in [' ',#9] do inc(p);

    if p^='(' then begin
      // read parameters
      Level:=0;
      repeat
        case p^ of
        '(':
          begin
            inc(Level);
            inc(p);
          end;
        ')':
          begin
            inc(p);
            dec(Level);
            if Level=0 then break;
          end;
        #0:
          exit;
        else
          inc(p);
        end;
      until false;
      // skip space
      while p^ in [' ',#9] do inc(p);
    end;

    // read 'at'
    if (p^='a') and (p[1]='t') then begin
      inc(p,2);

      // skip space
      while p^ in [' ',#9] do inc(p);

      // read gdbfilename:gdbline
      StartP:=p;
      while not (p^ in [#0,':']) do inc(p);
      GDBFilename:=copy(Source,StartP-PChar(Source)+1,p-StartP);
      if p^=#0 then exit;
      inc(p);
      StartP:=p;
      while p^ in ['0'..'9'] do inc(p);
      GDBLine:=StrToIntDef(copy(Source,StartP-PChar(Source)+1,p-StartP),0);
    end else if (p^='f') and (p[1]='r') and (p[2]='o') and (p[3]='m') then begin
      // from <filename>
    end else if p^=#0 then begin
      // no source position => mangled
      DemangleGDBIdentifier(MangledIdentifier,MangledItems);
    end else begin
      UnexpectedError('at');
      exit;
    end;

  end else begin
    // unknown format
    UnexpectedError('hexnumber');
    exit;
  end;
end;

{ TCTGDBTracer }

procedure TCTGDBTracer.SetTraceText(AValue: string);
begin
  if FTraceText=AValue then Exit;
  Clear;
  FTraceText:=AValue;
  TraceToLines;
end;

function TCTGDBTracer.GetLineCount: integer;
begin
  Result:=fLines.Count;
end;

function TCTGDBTracer.GetLines(Index: integer): TCTGDBTraceLine;
begin
  Result:=TCTGDBTraceLine(fLines[Index]);
end;

procedure TCTGDBTracer.SkipLine;
var
  c: Char;
begin
  while not (fCurP^ in [#0,#10,#13]) do inc(fCurP);
  repeat
    c:=fCurP^;
    if not (c in [#10,#13]) then break;
    inc(fCurP);
    inc(fLineNumber);
    if (fCurP^ in [#10,#13]) and (c<>fCurP^) then
      inc(fCurP);
  until false;
  fLineStart:=fCurP;
end;

function TCTGDBTracer.GetCurrentLine: string;
var
  LineEnd: PChar;
begin
  LineEnd:=fCurP;
  while not (LineEnd^ in [#0,#10,#13]) do inc(LineEnd);
  Result:=copy(FTraceText,fLineStart-PChar(FTraceText)+1,LineEnd-fLineStart);
end;

procedure TCTGDBTracer.AddUnknownLine;
var
  Line: TCTGDBTraceLine;
begin
  Line:=TCTGDBTraceLine.Create;
  Line.LineNumber:=fLineNumber;
  Line.Source:=GetCurrentLine;
  Line.Error:='invalid format';
  fLines.Add(Line);
  SkipLine;
end;

procedure TCTGDBTracer.TraceToLines;
{ Example:

#0  0x00020e16 in fpc_raiseexception ()
#1  0x0004cb37 in SYSUTILS_RUNERRORTOEXCEPT$LONGINT$POINTER$POINTER ()
#3  0xbffff548 in ?? ()
#4  0x007489de in EXTTOOLEDITDLG_TEXTERNALTOOLMENUITEMS_$__LOAD$TCONFIGSTORAGE$$TMODALRESULT ()
#7  0x0007e620 in MAIN_TMAINIDE_$__LOADGLOBALOPTIONS ()
#9  0x00011124 in PASCALMAIN ()
#10 0x0002f416 in SYSTEM_FPC_SYSTEMMAIN$LONGINT$PPCHAR$PPCHAR ()

#0  0x0001136d in P$TESTSTACKTRACE1_TMAINCLASS_$_TSUBCLASS_$__RAISESOMETHING$ANSISTRING ()
#5  0x0001114a in start ()

~"#0 DOHANDLEMOUSEACTION (this=0x14afae00, ANACTIONLIST=0x14a96af8,
ANINFO=...) at synedit.pp:3000\n"
~"#1 0x00aea3e9 in FINDANDHANDLEMOUSEACTION (this=0x14afae00,
ABUTTON=MBLEFT, ASHIFT=..., X=233, Y=241, ACCOUNT=CCSINGLE, ADIR=CDDOWN,
ANAC
TIONRESULT=..., AWHEELDELTA=0) at synedit.pp:3307\n"
~"#3 0x005e083b in DOMOUSEDOWN (this=0x14afae00, MESSAGE=...,
BUTTON=MBLEFT, SHIFT=...) at include/control.inc:2135\n"
~"#5 0x0040d096 in DISPATCH (this=0xeebf6d4, MESSAGE=0) at
../inc/objpas.inc:592\n"
~"#8 0x00af3b76 in WNDPROC (this=0x14afae00, MSG=...) at synedit.pp:5740\n"
~"#11 0x7673fd72 in ?? () from C:\\Windows\\system32\\user32.dll\n"
~"#20 0x0040358f in main () at lazarus.pp:128\n"

}

  procedure ReadLine;
  var
    StartP: PChar;
    Line: String;
    CopyStartP: PChar;
    StartLineNumber: Integer;
    NewLine: TCTGDBTraceLine;
    c: Char;

    procedure ConcatLine;
    begin
      if fCurP=CopyStartP then exit;
      Line+=copy(FTraceText,CopyStartP-PChar(FTraceText)+1,fCurP-CopyStartP);
    end;

  begin
    while (fCurP^ in [' ',#9]) do inc(fCurP);
    if fCurP^='~' then begin
      // quoted format ~"<line>"
      inc(fCurP);
      if fCurP^<>'"' then begin
        AddUnknownLine;
        exit;
      end;
      inc(fCurP);
      StartP:=fCurP;
      Line:='';
      CopyStartP:=StartP;
      StartLineNumber:=fLineNumber;
      repeat
        case fCurP^ of
        #0:
          begin
            // missing closing quote
            fCurP:=StartP;
            fLineNumber:=StartLineNumber;
            AddUnknownLine;
            exit;
          end;
        #10,#13:
          begin
            ConcatLine;
            repeat
              c:=fCurP^;
              if not (c in [#10,#13]) then break;
              inc(fLineNumber);
              inc(fCurP);
              if (fCurP^ in [#10,#13]) and (c<>fCurP^) then
                inc(fCurP);
            until false;
            CopyStartP:=fCurP;
          end;
        '\':
          begin
            ConcatLine;
            inc(fCurP);
            case fCurP^ of
            'n': // ignore line breaks
              inc(fCurP);
            #0:
              break;
            else
              Line+=fCurP^;
            end;
            CopyStartP:=fCurP;
          end;
        '"':
          begin
            ConcatLine;
            inc(fCurP);
            break;
          end;
        else
          inc(fCurP);
        end;
      until false;
      NewLine:=TCTGDBTraceLine.Create;
      NewLine.LineNumber:=StartLineNumber;
      NewLine.Source:=Line;
      fLines.Add(NewLine);
      // skip the rest of the line
      SkipLine;
      exit;
    end else if fCurP^='#' then begin
      // non quoted format
      NewLine:=TCTGDBTraceLine.Create;
      NewLine.LineNumber:=fLineNumber;
      NewLine.Source:=GetCurrentLine;
      fLines.Add(NewLine);
      // skip the rest of the line
      SkipLine;
      exit;
    end;
    // unknown format
    AddUnknownLine;
  end;

begin
  fLines.Clear;
  if FTraceText='' then exit;
  fLineNumber:=1;
  fCurP:=PChar(FTraceText);
  fLineStart:=fCurP;
  while fCurP^<>#0 do
    ReadLine;
end;

procedure TCTGDBTracer.ParseLines;
var
  i: Integer;
  Line: TCTGDBTraceLine;
begin
  for i:=0 to LineCount-1 do begin
    Line:=Lines[i];
    if Line.Error<>'' then continue;
    Line.Parse;
  end;
end;

constructor TCTGDBTracer.Create;
begin
  fLines:=TObjectList.Create(true);
end;

destructor TCTGDBTracer.Destroy;
begin
  Clear;
  FreeAndNil(fLines);
  inherited Destroy;
end;

procedure TCTGDBTracer.Clear;
begin
  fLines.Clear;
end;

function TCTGDBTracer.AsString: string;
var
  ms: TMemoryStream;
begin
  ms:=TMemoryStream.Create;
  try
    WriteToStream(ms);
    ms.Position:=0;
    SetLength(Result,ms.Size);
    if Result<>'' then
      ms.Read(Result[1],length(Result));
  finally
    ms.Free;
  end;
end;

procedure TCTGDBTracer.WriteToStream(s: TStream);

  procedure w(const h: string);
  begin
    if h='' then exit;
    s.Write(h[1],length(h));
  end;

var
  i: Integer;
  Line: TCTGDBTraceLine;
begin
  for i:=0 to LineCount-1 do begin
    Line:=Lines[i];
    w(IntToStr(i+1)+'/'+IntToStr(LineCount)+': ');
    Line.WriteToStream(2,s);
    w(LineEnding);
  end;
end;

end.

