{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterSQL.pas, released 2000-04-21.
The Original Code is based on the wmSQLSyn.pas and wmSybaseSyn.pas files from
the mwEdit component suite by Martin Waldenburg and other developers, the
Initial Author of these files is Willo van der Merwe. Initial Author of
SynHighlighterSQL.pas is Michael Hieke.
Portions created by Willo van der Merwe are Copyright 1999 Willo van der Merwe.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

2000-06: Thanks to Daniel Parnell for adding the new Oracle dialect.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(SQL highlighter SynEdit with support for different dialects.)
@author(Michael Hieke)
@created(2000-04-21)
@lastmod(2000-06-20)
The SynHighlighterSQL implements a highlighter for SQL for the SynEdit projects.
Different SQL dialects can be selected via the Dialect property.
}
unit SynHighlighterSQL;

{$I SynEdit.inc}

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, Graphics, Registry,
  SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries;

type
  TtkTokenKind = (tkComment, tkDatatype, tkException, tkFunction, tkIdentifier, // DJLP 2000-06-15
    tkKey, tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown,
    tkVariable);

  TRangeState = (rsUnknown, rsComment, rsString);                               // DJLP 2000-06-15

  TProcTableProc = procedure of object;

  TSQLDialect = (sqlStandard, sqlSybase, sqlOracle);                            // DJLP 2000-06-15

type
  TSynSQLSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fKeywords: TSynHashEntryList;
    fDialect: TSQLDialect;
    fCommentAttri: TSynHighlighterAttributes;
{begin}                                                                         // DJLP 2000-06-15
    fDataTypeAttri: TSynHighlighterAttributes;
    fExceptionAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
{end}                                                                           // DJLP 2000-06-15
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure SymbolAssignProc;
    procedure VariableProc;
    procedure UnknownProc;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AnsiCProc;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    procedure SetDialect(Value: TSQLDialect);
  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}                                         //mh 2000-07-14
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEOL: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
{begin}                                                                         // DJLP 2000-06-15
    property DataTypeAttri: TSynHighlighterAttributes read fDataTypeAttri
      write fDataTypeAttri;
    property ExceptionAttri: TSynHighlighterAttributes read fExceptionAttri
      write fExceptionAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
{end}                                                                           // DJLP 2000-06-15
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
    property SQLDialect: TSQLDialect read fDialect write SetDialect;
  end;

implementation

uses
  SynEditStrConst;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

const
  // keywords for standard SQL
  StandardKW: string =
    'active,after,all,alter,and,any,as,asc,ascending,at,auto,' +
    'base_name,before,begin,between,by,cache,cast,check,column,commit,' +
    'committed,computed,conditional,constraint,containing,count,create,' +
    'current,cursor,database,debug,declare,default,delete,desc,descending,' +
    'distinct,do,domain,drop,else,end,entry_point,escape,exception,execute,' +
    'exists,exit,external,extract,filter,for,foreign,from,full,function,' +
    'generator,grant,group,having,if,in,inactive,index,inner,insert,into,is,' +
    'isolation,join,key,left,level,like,merge,names,no,not,null,of,on,only,' +
    'or,order,outer,parameter,password,plan,position,primary,privileges,' +
    'procedure,protected,read,retain,returns,revoke,right,rollback,schema,' +
    'select,set,shadow,shared,snapshot,some,suspend,table,then,to,' +
    'transaction,trigger,uncommitted,union,unique,update,user,using,view,' +
    'wait,when,where,while,with,work';

  // Sybase SQL keywords
  SybaseKW: string =
    'absolute,action,add,after,alias,all,allocate,alter,and,any,are,' +
    'arith_overflow,as,asc,assertion,async,at,authorization,avg,before,begin,' +
    'between,bit,bit_length,boolean,both,breadth,break,browse,bulk,by,call,' +
    'cascade,cascaded,case,cast,catalog,char,char_convert,char_length,' +
    'character,character_length,check,checkpoint,close,clustered,coalesce,' +
    'collate,collation,column,commit,completion,compute,confirm,' +
    'connect,connection,constraint,constraints,continue,controlrow,convert,' +
    'corresponding,count,create,cross,current,current_date,current_time,' +
    'current_timestamp,current_user,cursor,cycle,data,database,date,day,dbcc,' +
    'deallocate,dec,decimal,declare,default,deferrable,deferred,delete,depth,' +
    'desc,describe,descriptor,diagnostics,dictionary,dis,disconnect,distinct,' +
    'domain,double,drop,dummy,dump,each,else,elseif,en,end,endtran,equals,' +
    'errlvl,errordata,errorexit,escape,except,exception,exclusive,exec,' +
    'execute,exists,exit,exp_row_size,external,extract,false,fetch,' +
    'fillfactor,first,float,for,foreign,found,from,full,general,get,global,' +
    'go,goto,grant,group,having,holdlock,hour,identity,identity_gap,' +
    'identity_insert,identity_start,if,ignore,immediate,in,index,indicator,' +
    'initially,inner,input,insensitive,insert,install,int,integer,intersect,' +
    'interval,into,is,isolation,jar,join,key,kill,language,last,leading,' +
    'leave,left,less,level,like,limit,lineno,load,local,lock,loop,lower,' +
    'match,max,max_rows_per_page,min,minute,mirror,mirrorexit,modify,module,' +
    'month,names,national,natural,nchar,new,next,no,noholdlock,nonclustered,' +
    'none,not,null,nullif,numeric,numeric_truncation,object,' +
    'octet_length,of,off,offsets,oid,old,on,once,online,only,open,operation,' +
    'operators,option,or,order,others,outer,output,over,overlaps,pad,' +
    'parameters,partial,partition,pendant,perm,permanent,plan,position,' +
    'precision,preorder,prepare,preserve,primary,print,prior,private,' +
    'privileges,proc,procedure,processexit,protected,proxy_table,public,' +
    'quiesce,raiserror,read,readpast,readtext,real,reconfigure,recursive,' +
    'ref,reference,referencing,relative,remove,reorg,replace,replication,' +
    'reservepagegap,resignal,restrict,return,returns,revoke,right,role,' +
    'rollback,routine,row,rowcount,rows,rule,save,savepoint,schema,scroll,' +
    'search,second,section,select,sensitive,sequence,session_user,set,' +
    'setuser,shared,shutdown,signal,similar,size,smallint,some,space,sql,' +
    'sqlcode,sqlerror,sqlexception,sqlstate,statistics,stripe,structure,' +
    'substring,sum,syb_identity,syb_restree,system_user,table,temp,temporary,' +
    'test,textsize,then,there,time,timestamp,timezone_hour,timezone_minute,' +
    'to,trailing,tran,transaction,translate,translation,trigger,trim,true,' +
    'truncate,tsequal,type,under,union,unique,unknown,unpartition,update,' +
    'upper,usage,use,user,user_option,using,value,values,varchar,variable,' +
    'varying,view,virtual,visible,wait,waitfor,when,whenever,where,while,' +
    'with,without,work,write,writetext,year,zone';

{begin}                                                                         // DJLP 2000-06-15
  // Oracle SQL keywords                                                        // JJV  2000-07-07
  OracleKW: string = 'ACCESS,ADD,ADMIN,AFTER,ALL,ALLOCATE,ALTER,ANALYZE,AND,' +
    'ANY,ARCHIVE,ARCHIVELOG,AS,ASC,AUDIT,BACKUP,BECOME,BEFORE,BETWEEN,BLOCK,' +
    'BY,CACHE,CANCEL,CASCADE,CHANGE,CHARACTER,CHECK,CHECKPOINT,CLUSTER,COBOL,' +
    'COLUMN,COMMENT,COMPILE,COMPRESS,CONNECT,CONSTRAINT,CONSTRAINTS,CONTENTS,' +
    'CONTINUE,CONTROLFILE,CREATE,CURRENT,CYCLE,DATAFILE,DEC,DEFAULT,DELETE,' +
    'DESC,DISABLE,DISMOUNT,DISTINCT,DROP,EACH,ELSE,ENABLE,ESCAPE,EVENTS,' +
    'EXCEPTIONS,EXCLUSIVE,EXEC,EXECUTE,EXISTS,EXPLAIN,EXTENT,EXTERNALLY,FILE,' +
    'FLUSH,FOR,FORCE,FOREIGN,FORTRAN,FOUND,FREELISTS,FROM,GO,GRANT,GROUP,' +
    'GROUPS,HAVING,IDENTIFIED,IMMEDIATE,IN,INCLUDING,INCREMENT,INDEX,INFILE,' +
    'INITIAL,INITRANS,INSERT,INSTANCE,INT,INTERSECT,INTO,IS,KEY,LANGUAGE,' +
    'LAYER,LEVEL,LIKE,LINK,LISTS,LOCK,LOGFILE,MANAGE,MANUAL,MAXDATAFILES,' +
    'MAXEXTENTS,MAXINSTANCES,MAXLOGFILES,MAXLOGHISTORY,MAXLOGMEMBERS,' +
    'MAXTRANS,MAXVALUE,MINEXTENTS,MINUS,MINVALUE,MODE,MODIFY,MODULE,MOUNT,' +
    'NEXT,NO,NOARCHIVELOG,NOAUDIT,NOCOMPRESS,NOCYCLE,NOMAXVALUE,NOMINVALUE,' +
    'NONE,NOORDER,NORESETLOGS,NORMAL,NOSORT,NOT,NOWAIT,NULL,OF,OFFLINE,OLD,' +
    'ON,ONLINE,ONLY,OPTION,OR,ORDER,OWN,PARALLEL,PASCAL,PCTFREE,PCTINCREASE,' +
    'PCTUSED,PLAN,PLI,PRECISION,PRIMARY,PRIOR,PRIVILEGES,PROFILE,PUBLIC,' +
    'QUOTA,READ,RECOVER,REFERENCES,REFERENCING,RENAME,RESETLOGS,RESOURCE,' +
    'RESTRICTED,REUSE,REVOKE,ROLE,ROLES,ROW,ROWLABEL,ROWNUM,ROWS,SCN,SECTION,' +
    'SEGMENT,SELECT,SEQUENCE,SESSION,SET,SHARE,SHARED,SIZE,SOME,SORT,' +
    'SPECIFIED,START,STATISTICS,STOP,STORAGE,SUCCESSFUL,SWITCH,SYNONYM,' +
    'SYSTEM,TABLE,TABLESPACE,TEMPORARY,THEN,THREAD,TIME,TO,TRACING,' +
    'TRANSACTION,TRIGGER,TRUNCATE,UNDER,UNION,UNIQUE,UNLIMITED,UNTIL,UPDATE,' +
    'VALIDATE,VALUES,VARGRAPHIC,VIEW,WHENEVER,WHERE,WITH,' +

    // PLSQL keywords start here
    'ABORT,ACCEPT,ARRAY,ARRAYLEN,ASSERT,ASSIGN,AT,AUTHORIZATION,BASE_TABLE,' +
    'BEGIN,BODY,CASE,CHAR_BASE,CLOSE,CLUSTERS,COLAUTH,COLUMNS,COMMIT,' +
    'CONSTANT,CRASH,CURRVAL,CURSOR,DATA_BASE,DATABASE,DBA,DEBUGOFF,DEBUGON,' +
    'DECLARE,DEFINITION,DELAY,DELTA,DIGITS,DISPOSE,DO,ELSIF,END,ENTRY,' +
    'EXCEPTION,EXCEPTION_INIT,EXIT,FALSE,FETCH,FORM,FUNCTION,GENERIC,GOTO,IF,' +
    'INDEXES,INDICATOR,INTERFACE,LIMITED,LOOP,NATURAL,NATURALN,NEW,NEXTVAL,' +
    'NUMBER_BASE,OPEN,OUT,PACKAGE,PARTITION,POSITIVE,POSITIVEN,PRAGMA,' +
    'PRIVATE,PROCEDURE,RAISE,RANGE,RECORD,REF,RELEASE,REM,REMR,' +
    'RESTRICT_REFERENCES,RETURN,REVERSE,ROLLBACK,ROWTYPE,RUN,SAVEPOINT,' +
    'SCHEMA,SEPARATE,SPACE,SQL,SQLERROR,STATEMENT,SUBTYPE,TABAUTH,TABLES,' +
    'TASK,TERMINATE,TRUE,TYPE,USE,VIEWS,WHEN,WHILE,WORK,WRITE,XOR';

  // Oracle data types
  OracleTypes: string = 'BFILE,BINARY_INTEGER,BLOB,BOOLEAN,CHAR,CLOB,DATE,' +
    'DECIMAL,FLOAT,INTEGER,LONG,MLSLABEL,NCHAR,NCLOB,NUMBER,NVARCHAR2,' +
    'PLS_INTEGER,RAW,REAL,ROWID,SMALLINT,VARCHAR,VARCHAR2';

  // Oracle built in exceptions
  OracleExceptions : string = 'ACCESS_INTO_NULL,COLLECTION_IS_NULL,' +
    'CURSOR_ALREADY_OPEN,DUP_VAL_ON_INDEX,INVALID_CURSOR,INVALID_NUMBER,' +
    'LOGIN_DENIED,NO_DATA_FOUND,NOT_LOGGED_ON,OTHERS,PROGRAM_ERROR,' +
    'STORAGE_ERROR,SUBSCRIPT_BEYOND_COUNT,SUBSCRIPT_OUTSIDE_LIMIT,' +
    'TIMEOUT_ON_RESOURCE,TOO_MANY_ROWS,VALUE_ERROR,ZERO_DIVIDE';

  // Oracle built in functions
  OracleFunctions: string = 'ABS,ACOS,ADD_MONTHS,ASCII,ASIN,ATAN,ATAN2,' +
    'AVG,BFILENAME,CEIL,CHARTOROWID,CHR,CONCAT,CONVERT,COS,COSH,COUNT,DECODE,' +
    'DEREF,DUMP,EMPTY_BLOB,EMPTY_CLOB,EXP,FLOOR,GLB,GREATEST,GREATEST_LB,' +
    'HEXTORAW,INITCAP,INSTR,INSTRB,LAST_DAY,LEAST,LEAST_LB,LENGTH,LENGTHB,LN,' +
    'LOG,LOWER,LPAD,LTRIM,LUB,MAKE_REF,MAX,MIN,MOD,MONTHS_BETWEEN,NEW_TIME,' +
    'NEXT_DAY,NLS_CHARSET_DECL_LEN,NLS_CHARSET_ID,NLS_CHARSET_NAME,' +
    'NLS_INITCAP,NLS_LOWER,NLS_UPPER,NLSSORT,NVL,POWER,RAWTOHEX,REFTOHEX,' +
    'REPLACE,ROUND,ROWIDTOCHAR,RPAD,RTRIM,SIGN,SIN,SINH,SOUNDEX,SQLCODE,' +
    'SQLERRM,SQRT,STDDEV,SUBSTR,SUBSTRB,SUM,SYSDATE,TAN,TANH,TO_CHAR,TO_DATE,' +
    'TO_LABEL,TO_MULTI_BYTE,TO_NUMBER,TO_SINGLE_BYTE,TRANSLATE,TRUNC,' +
    'UID,UPPER,USER,USERENV,USING,VARIANCE,VSIZE';
{end}                                                                           // DJLP 2000-06-15 + JJV 2000-07-07

procedure MakeIdentTable;
var
  c: char;
begin
  FillChar(Identifiers, SizeOf(Identifiers), 0);
  for c := 'a' to 'z' do
    Identifiers[c] := TRUE;
  for c := 'A' to 'Z' do
    Identifiers[c] := TRUE;
  for c := '0' to '9' do
    Identifiers[c] := TRUE;
  Identifiers['_'] := TRUE;

  FillChar(mHashTable, SizeOf(mHashTable), 0);
  mHashTable['_'] := 1;
  for c := 'a' to 'z' do
    mHashTable[c] := 2 + Ord(c) - Ord('a');
  for c := 'A' to 'Z' do
    mHashTable[c] := 2 + Ord(c) - Ord('A');
end;

function TSynSQLSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while Identifiers[ToHash^] do begin
{$IFOPT Q-}
    Result := 2 * Result + mHashTable[ToHash^];
{$ELSE}
    Result := (2 * Result + mHashTable[ToHash^]) and $FFFFFF;                   // DJLP 2000-06-08
{$ENDIF}
    inc(ToHash);
  end;
  Result := Result and $FF; // 255
  fStringLen := ToHash - fToIdent;
end;

function TSynSQLSyn.KeyComp(const aKey: string): Boolean;
var
  i: integer;
  pKey1, pKey2: PChar;
begin
  pKey1 := fToIdent;
  // Note: fStringLen is always > 0 !
  pKey2 := pointer(aKey);
  for i := 1 to fStringLen do
  begin
    if mHashTable[pKey1^] <> mHashTable[pKey2^] then
    begin
      Result := FALSE;
      exit;
    end;
    Inc(pKey1);
    Inc(pKey2);
  end;
  Result := TRUE;
end;

function TSynSQLSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if KeyComp(Entry.Keyword) then begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TSynSQLSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
       #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      #39: fProcTable[I] := AsciiCharProc;
      '=': fProcTable[I] := EqualProc;
      '>': fProcTable[I] := GreaterProc;
      '<': fProcTable[I] := LowerProc;
      '-': fProcTable[I] := MinusProc;
      '|': fProcTable[I] := OrSymbolProc;
      '+': fProcTable[I] := PlusProc;
      '/': fProcTable[I] := SlashProc;
      '&': fProcTable[I] := AndSymbolProc;
      #34: fProcTable[I] := StringProc;

      ':', '@':                                                                 // DJLP 2000-06-15
        fProcTable[I] := VariableProc;

      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := IdentProc;
      '0'..'9':
        fProcTable[I] := NumberProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      '^', '%', '*', '!':
        fProcTable[I] := SymbolAssignProc;
      '{', '}', '.', ',', ';', '?', '(', ')', '[', ']', '~':
        fProcTable[I] := SymbolProc;
      else
        fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynSQLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeywords := TSynHashEntryList.Create;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
{begin}                                                                         // DJLP 2000-06-15
  fDataTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType);
  fDataTypeAttri.Style := [fsBold];
  AddAttribute(fDataTypeAttri);
  fExceptionAttri := TSynHighlighterAttributes.Create(SYNS_AttrException);
  fExceptionAttri.Style := [fsItalic];
  AddAttribute(fExceptionAttri);
  fFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction);
  fFunctionAttri.Style := [fsBold];
  AddAttribute(fFunctionAttri);
{end}                                                                           // DJLP 2000-06-15
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_Attrstring);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterSQL;
  fRange := rsUnknown;
  fDialect := sqlSybase;
  EnumerateKeywords(Ord(tkKey), SybaseKW, IdentChars, DoAddKeyword);
end;

destructor TSynSQLSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynSQLSyn.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynSQLSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '&'] then Inc(Run);
end;

procedure TSynSQLSyn.AsciiCharProc;
begin
{begin}                                                                         // DJLP 2000-06-29
  // Oracle SQL allows strings to go over multiple lines
  if fLine[Run] = #0 then
    NullProc
  else begin
    fTokenID := tkString;
    // else it's end of multiline string
    if (Run > 0) or (fRange <> rsString) or (fLine[Run] <> #39) then begin
      fRange := rsString;
      repeat
        Inc(Run);
      until fLine[Run] in [#0, #10, #13, #39];
    end;
    if fLine[Run] = #39 then begin
      Inc(Run);
      fRange := rsUnknown;
    end;
  end;
{end}                                                                           // DJLP 2000-06-29
end;

procedure TSynSQLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynSQLSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TSynSQLSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TSynSQLSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynSQLSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSQLSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  case fLine[Run] of
    '=': Inc(Run);
    '<': begin
           Inc(Run);
           if fLine[Run] = '=' then Inc(Run);
         end;
  end;
end;

procedure TSynSQLSyn.MinusProc;
begin
  Inc(Run);
  if fLine[Run] = '-' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end else
    fTokenID := tkSymbol;
end;

procedure TSynSQLSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynSQLSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', '-'] do begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynSQLSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '|'] then Inc(Run);
end;

procedure TSynSQLSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '+'] then Inc(Run);
end;

procedure TSynSQLSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        fRange := rsComment;
        fTokenID := tkComment;
        repeat
          Inc(Run);
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
            fRange := rsUnknown;
            Inc(Run, 2);
            break;
          end;
        until fLine[Run] in [#0, #10, #13];
      end;
    '=':
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynSQLSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynSQLSyn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSQLSyn.SymbolAssignProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSynSQLSyn.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not (fLine[Run] in [#0, #10, #13]) do
    case fLine[Run] of
      '\': if fLine[Run + 1] = #34 then
             Inc(Run, 2);
      #34: if fLine[Run + 1] = #34 then
             Inc(Run, 2)
           else begin
             Inc(Run);
             break;
           end;
      else
        Inc(Run);
    end;
end;

procedure TSynSQLSyn.VariableProc;
var
  i: integer;
begin
{begin}                                                                         // DJLP 2000-06-15
  // Oracle uses the ':' character to indicate bind variables
  if (SQLDialect = sqlOracle) and (fLine[Run] = '@') then
    SymbolProc
  else if (SQLDialect <> sqlOracle) and (fLine[Run] = ':') then
    SymbolProc
  else begin
    fTokenID := tkVariable;
    i := Run;
    repeat
      Inc(i);
    until not (Identifiers[fLine[i]]);
    Run := i;
  end;
{end}                                                                           // DJLP 2000-06-15
end;

procedure TSynSQLSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSQLSyn.AnsiCProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
          fRange := rsUnknown;
          Inc(Run, 2);
          break;
        end;
        Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynSQLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment:
      AnsiCProc;
    rsString:
      AsciiCharProc;                                                            // DJLP 2000-05-15
    else
      fProcTable[fLine[Run]];
  end;
end;

function TSynSQLSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynSQLSyn.GetEOL: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynSQLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynSQLSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  Setstring(Result, (FLine + fTokenPos), Len);
end;

function TSynSQLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynSQLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
{begin}                                                                         // DJLP 2000-06-15
    tkDatatype: Result := fDataTypeAttri;
    tkException: Result := fExceptionAttri;
    tkFunction: Result := fFunctionAttri;
{end}                                                                           // DJLP 2000-06-15
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynSQLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynSQLSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynSQLSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynSQLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynSQLSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}                                             //mh 2000-07-14
function TSynSQLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

procedure TSynSQLSyn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

procedure TSynSQLSyn.SetDialect(Value: TSQLDialect);
begin
  if Value <> fDialect then begin
    fDialect := Value;
    fKeywords.Clear;
    case fDialect of
      sqlStandard:
        EnumerateKeywords(Ord(tkKey), StandardKW, IdentChars, DoAddKeyword);
      sqlSybase:
        EnumerateKeywords(Ord(tkKey), SybaseKW, IdentChars, DoAddKeyword);
{begin}                                                                         // DJLP 2000-06-15
      sqlOracle:
        begin
          EnumerateKeywords(Ord(tkKey), OracleKW, IdentChars, DoAddKeyword);
          EnumerateKeywords(Ord(tkDatatype), OracleTypes, IdentChars,
            DoAddKeyword);
          EnumerateKeywords(Ord(tkException), OracleExceptions, IdentChars,
            DoAddKeyword);
          EnumerateKeywords(Ord(tkFunction), OracleFunctions, IdentChars,
            DoAddKeyword);
        end;
{end}                                                                           // DJLP 2000-06-15
    end;
    DefHighlightChange(Self);
  end;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}                                                            //mh 2000-07-14
  RegisterPlaceableHighlighter(TSynSQLSyn);
{$ENDIF}
end.

