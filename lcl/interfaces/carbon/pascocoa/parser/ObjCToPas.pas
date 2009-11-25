unit ObjCToPas;
{ * This file is part of ObjCParser tool
  * Copyright (C) 2008-2009 by Dmitry Boyarintsev under the GNU LGPL
  * license version 2.0 or 2.1.  You should have received a copy of the
  * LGPL license along with at http://www.gnu.org/
}
// the unit contains (should contain) lobjc to Pascal convertion utility routines
// todo: move all ObjCParserUtils functions here.

interface

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

uses
  SysUtils, ObjCParserTypes;

const
  ObjCDefaultParamDelim = '_';


type
  TCProcessor = class(TObject)
  public
    procedure ProcessTree(Root: TEntity); virtual; abstract;
  end;

function ObjCToPasMethodName(mtd: TClassMethodDef; CutLastDelims: Boolean = false; ParamDelim: AnsiChar = ObjCDefaultParamDelim): AnsiString;
function IsPascalReserved(const s: AnsiString): Boolean;

implementation

function ObjCToPasMethodName(mtd: TClassMethodDef; CutLastDelims: Boolean; ParamDelim: AnsiChar): AnsiString;
var
  i   : Integer;
  obj : TObject;
begin
  Result := mtd._Name;
  for i := 0 to mtd.Items.Count - 1 do begin
    obj := mtd.Items[i];
    if not Assigned(obj) then Continue;
    if obj is TParamDescr then begin
      Result := Result + TParamDescr(obj)._Descr;
    end else if obj is TObjCParameterDef then
      Result := Result + ParamDelim;
  end;

  if CutLastDelims then begin
    i := length(Result);
    while (i > 0) and (Result[i] = ParamDelim) do dec(i);
    Result := Copy(Result, 1, i);
  end;
end;

// 'result' is considered reserved word!
function IsPascalReserved(const s: AnsiString): Boolean;
var
  ls  : AnsiString;
begin
  //todo: a hash table should be used!
  Result := false;
  if s = '' then Exit;
  ls := AnsiLowerCase(s);
  case ls[1] of
    'a': Result := (ls = 'absolute') or (ls = 'abstract') or (ls = 'and') or (ls = 'array') or (ls = 'as') or (ls= 'asm') or (ls = 'assembler');
    'b': Result := (ls = 'begin') or (ls = 'break');
    'c': Result := (ls = 'cdecl') or (ls = 'class') or (ls = 'const') or (ls = 'constructor') or (ls = 'continue') or (ls = 'cppclass');
    'd': Result := (ls = 'deprecated') or (ls = 'destructor') or (ls = 'div') or (ls = 'do') or (ls = 'downto');
    'e': Result := (ls = 'else') or (ls = 'end') or (ls = 'except') or (ls = 'exit') or (ls = 'export') or (ls = 'exports') or (ls = 'external');
    'f': Result := (ls = 'fail') or (ls = 'false') or (ls = 'far') or (ls = 'file') or (ls = 'finally') or (ls = 'for') or (ls = 'forward') or (ls = 'function');
    'g': Result := (ls = 'goto');
    'i':
      Result := (ls = 'if') or (ls = 'implementation') or (ls = 'in') or (ls = 'index') or (ls = 'inherited') or (ls = 'initialization') or (ls = 'inline')
        or  (ls = 'interface') or (ls = 'interrupt') or (ls = 'is');
    'l': Result := (ls = 'label') or (ls = 'library');
    'm': Result := (ls = 'mod');
    'n': Result := {(ls = 'name') or} (ls = 'near') or (ls = 'nil') or (ls = 'not');
    'o': Result := (ls = 'object') or (ls = 'of') or (ls = 'on') or (ls = 'operator') or (ls = 'or') or (ls = 'otherwise');
    'p':
      Result := (ls = 'packed') or (ls = 'popstack') or (ls = 'private') or (ls = 'procedure') or (ls = 'program') or (ls = 'property')
        or (ls = 'protected') or (ls = 'public');
    'r': Result := (ls = 'raise') or (ls = 'record') or (ls = 'reintroduce') or (ls = 'repeat') or (ls = 'result');
    's': Result := (ls = 'self') or (ls = 'set') or (ls = 'shl') or (ls = 'shr') or (ls = 'stdcall') or (ls = 'string');
    't': Result := (ls = 'then') or (ls = 'to') or (ls = 'true') or (ls = 'try') or (ls = 'type');
    'u': Result := (ls = 'unimplemented') or (ls = 'unit') or (ls = 'until') or (ls = 'uses');
    'v': Result := (ls = 'var') or (ls = 'virtual');
    'w': Result := (ls = 'while') or (ls = 'with');
    'x': Result := (ls = 'xor');
  end;
end;





end.
