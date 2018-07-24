{
 /***************************************************************************
                       idemacros.pp  -  macros for tools
                       ---------------------------------

 ***************************************************************************/

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    This unit defines the classes TTransferMacro and TTransferMacroList. These
    classes store and substitute macros in strings. Transfer macros are an
    easy way to transfer some ide variables to programs like the compiler,
    the debugger and all the other tools.
    Transfer macros have the form $(macro_name). It is also possible to define
    macro functions, which have the form $macro_func_name(parameter).
    The default macro functions are:
      $Ext(filename) - equal to ExtractFileExt
      $Path(filename) - equal to ExtractFilePath
      $Name(filename) - equal to ExtractFileName
      $NameOnly(filename) - equal to ExtractFileName but without extension.
      $MakeDir(filename) - append path delimiter
      $MakeFile(filename) - chomp path delimiter
      $Trim(filename) - equal to TrimFilename

  ToDo:
    sort items to accelerate find

}
unit TransferMacros;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazUTF8,
  // CodeTools
  FileProcs,
  // IdeIntf
  MacroIntf, MacroDefIntf,
  // IDE
  LazarusIDEStrConsts;

type

  { TTransferMacroList }

  TTransferMacroList = class
  private
    fItems: TFPList;  // list of TTransferMacro
    FMarkUnhandledMacros: boolean;
    FMaxUsePerMacro: integer;
    fOnSubstitution: TOnSubstitution;
    fBusy: TStringList; // current working Macros, used for circle detection
    function GetItems(Index: integer): TTransferMacro;
    procedure SetItems(Index: integer; NewMacro: TTransferMacro);
    procedure SetMarkUnhandledMacros(const AValue: boolean);
  protected
    function MF_Ext(const Filename:string; const {%H-}Data: PtrInt; var {%H-}Abort: boolean):string; virtual;
    function MF_Path(const Filename:string; const {%H-}Data: PtrInt; var {%H-}Abort: boolean):string; virtual;
    function MF_Name(const Filename:string; const {%H-}Data: PtrInt; var {%H-}Abort: boolean):string; virtual;
    function MF_NameOnly(const Filename:string; const {%H-}Data: PtrInt; var {%H-}Abort: boolean):string; virtual;
    function MF_MakeDir(const Filename:string; const {%H-}Data: PtrInt; var {%H-}Abort: boolean):string; virtual;
    function MF_MakeFile(const Filename:string; const {%H-}Data: PtrInt; var {%H-}Abort: boolean):string; virtual;
    function MF_Trim(const Filename:string; const {%H-}Data: PtrInt; var {%H-}Abort: boolean):string; virtual;
    procedure DoSubstitution(TheMacro: TTransferMacro; const MacroName: string;
      var s:string; const Data: PtrInt; var Handled, Abort: boolean;
      Depth: integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index: integer]: TTransferMacro
       read GetItems write SetItems; default;
    procedure SetValue(const MacroName, NewValue: string);
    function Count: integer;
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Add(NewMacro: TTransferMacro);
    function FindByName(const MacroName: string): TTransferMacro; virtual;
    function SubstituteStr(var s: string; const Data: PtrInt = 0;
      Depth: integer = 0): boolean; virtual;
    procedure ExecuteMacro(const MacroName: string;
      var MacroParam: string; const Data: PtrInt; out Handled, Abort: boolean;
      Depth: integer);
    class function StrHasMacros(const s: string): boolean;
    property OnSubstitution: TOnSubstitution
       read fOnSubstitution write fOnSubstitution;
    // error handling and loop detection
    property MarkUnhandledMacros: boolean read FMarkUnhandledMacros
                                          write SetMarkUnhandledMacros default true;
    property MaxUsePerMacro: integer read FMaxUsePerMacro write FMaxUsePerMacro default 3;
  end;

{ TLazIDEMacros }

type
  TLazIDEMacros = class(TIDEMacros)
  public
    function StrHasMacros(const s: string): boolean; override;
    function SubstituteMacros(var s: string): boolean; override;
    function IsMacro(const Name: string): boolean; override;
    procedure Add(NewMacro: TTransferMacro);override;
  end;
  
var
  GlobalMacroList: TTransferMacroList = nil;

type
  TCompilerParseStampIncreasedEvent = procedure of object;
var
  CompilerParseStamp: integer = 0; // TimeStamp of base value for macros
  CompilerParseStampIncreased: TCompilerParseStampIncreasedEvent = nil;
  BuildMacroChangeStamp: integer = 0; // TimeStamp of base value for build macros

procedure IncreaseCompilerParseStamp;
// Called when a package dependency changes or when project build macro values change.
//  Automatically calls IncreaseCompilerParseStamp
procedure IncreaseBuildMacroChangeStamp;

implementation

var
  IsIdentChar: array[char] of boolean;

procedure IncreaseCompilerParseStamp;
begin
  CTIncreaseChangeStamp(CompilerParseStamp);
  if Assigned(CompilerParseStampIncreased) then
    CompilerParseStampIncreased();
end;

procedure IncreaseBuildMacroChangeStamp;
begin
  IncreaseCompilerParseStamp;
  CTIncreaseChangeStamp(BuildMacroChangeStamp);
end;

{ TTransferMacroList }

constructor TTransferMacroList.Create;
begin
  inherited Create;
  fItems:=TFPList.Create;
  FMarkUnhandledMacros:=true;
  FMaxUsePerMacro:=3;
  Add(TTransferMacro.Create('Ext', '', lisTMFunctionExtractFileExtension, @MF_Ext, []));
  Add(TTransferMacro.Create('Path', '', lisTMFunctionExtractFilePath, @MF_Path, []));
  Add(TTransferMacro.Create('Name', '', lisTMFunctionExtractFileNameExtension, @MF_Name,[]));
  Add(TTransferMacro.Create('NameOnly', '', lisTMFunctionExtractFileNameOnly, @MF_NameOnly,[]));
  Add(TTransferMacro.Create('MakeDir', '', lisTMFunctionAppendPathDelimiter, @MF_MakeDir,[]));
  Add(TTransferMacro.Create('MakeFile', '', lisTMFunctionChompPathDelimiter, @MF_MakeFile,[]));
end;

destructor TTransferMacroList.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  FreeAndNil(fBusy);
  inherited Destroy;
end;

function TTransferMacroList.GetItems(Index: integer): TTransferMacro;
begin
  Result:=TTransferMacro(fItems[Index]);
end;

procedure TTransferMacroList.SetItems(Index: integer; NewMacro: TTransferMacro);
begin
  fItems[Index]:=NewMacro;
end;

procedure TTransferMacroList.SetMarkUnhandledMacros(const AValue: boolean);
begin
  if FMarkUnhandledMacros=AValue then exit;
  FMarkUnhandledMacros:=AValue;
end;

procedure TTransferMacroList.SetValue(const MacroName, NewValue: string);
var AMacro:TTransferMacro;
begin
  AMacro:=FindByName(MacroName);
  if AMacro<>nil then AMacro.Value:=NewValue;
end;

function TTransferMacroList.Count: integer;
begin
  Result:=fItems.Count;
end;

procedure TTransferMacroList.Clear;
var i:integer;
begin
  for i:=0 to fItems.Count-1 do Items[i].Free;
  fItems.Clear;
end;

procedure TTransferMacroList.Delete(Index: integer);
begin
  Items[Index].Free;
  fItems.Delete(Index);
end;

procedure TTransferMacroList.Add(NewMacro: TTransferMacro);
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
begin
  l:=0;
  r:=fItems.Count-1;
  m:=0;
  while l<=r do begin
    m:=(l+r) shr 1;
    cmp:=UTF8CompareText(NewMacro.Name,Items[m].Name);
    if cmp<0 then
      r:=m-1
    else if cmp>0 then
      l:=m+1
    else
      break;
  end;
  if (m<fItems.Count) and (UTF8CompareText(NewMacro.Name,Items[m].Name)>0) then
    inc(m);
  fItems.Insert(m,NewMacro);
  //if NewMacro.MacroFunction<>nil then
  //  debugln('TTransferMacroList.Add A ',NewMacro.Name);
end;

function TTransferMacroList.SubstituteStr(var s:string; const Data: PtrInt;
  Depth: integer): boolean;

  function SearchBracketClose(Position: integer): integer;
  var BracketClose:char;
  begin
    if s[Position]='(' then BracketClose:=')'
    else BracketClose:='}';
    inc(Position);
    while (Position<=length(s)) and (s[Position]<>BracketClose) do begin
      if (s[Position] in ['(','{']) then
        Position:=SearchBracketClose(Position);
      inc(Position);
    end;
    Result:=Position;
  end;

var
  MacroStart,MacroEnd: integer;
  MacroName, MacroStr, MacroParam: string;
  Handled, Abort: boolean;
  OldMacroLen: Integer;
  sLen: Integer;
  InUse: Integer;
  i: Integer;
  LoopDepth: Integer;
  LoopPos: Integer;
begin
  if Depth>10 then begin
    Result:=false;
    s:='(macro loop detected)'+s;
    exit;
  end;
  Result:=true;
  sLen:=length(s);
  MacroStart:=1;
  LoopDepth:=1;
  LoopPos:=1;
  repeat
    while (MacroStart<sLen) do begin
      if (s[MacroStart]<>'$') then
        inc(MacroStart)
      else if (s[MacroStart+1]='$') then // skip $$
        inc(MacroStart,2)
      else
        break;
    end;
    if MacroStart>=sLen then break;
    
    MacroEnd:=MacroStart+1;
    while (MacroEnd<=sLen) and (IsIdentChar[s[MacroEnd]]) do
      inc(MacroEnd);

    if (MacroEnd<sLen) and (s[MacroEnd] in ['(','{']) then begin
      MacroName:=copy(s,MacroStart+1,MacroEnd-MacroStart-1);
      //debugln(['TTransferMacroList.SubstituteStr FUNC ',MacroName]);
      MacroEnd:=SearchBracketClose(MacroEnd)+1;
      if MacroEnd>sLen+1 then
        break; // missing closing bracket
      OldMacroLen:=MacroEnd-MacroStart;
      MacroStr:=copy(s,MacroStart,OldMacroLen);
      // Macro found
      Handled:=false;
      Abort:=false;
      if MacroName='' then begin
        // Macro variable
        MacroName:=copy(s,MacroStart+2,OldMacroLen-3);
        MacroParam:='';
      end else begin
        // Macro function -> substitute macro parameter first
        //if MacroName='LCLWidgetSet' then DebugLn(['TTransferMacroList.SubstituteStr MacroStr="',MacroStr,'"']);
        MacroParam:=copy(MacroStr,length(MacroName)+3,
                                  length(MacroStr)-length(MacroName)-3);
      end;
      //if MacroName='PATH' then
      //  debugln(['TTransferMacroList.SubstituteStr START MacroName=',MacroName,' Param="',MacroParam,'"']);
      // check for endless loop
      InUse:=0;
      if fBusy<>nil then begin
        for i:=0 to fBusy.Count-1 do begin
          if SysUtils.CompareText(fBusy[i],MacroName)=0 then begin
            inc(InUse);
            if InUse>MaxUsePerMacro then begin
              // cycle detected
              Handled:=true;
              MacroStr:='<MACRO-CYCLE:'+MacroName+'>';
            end;
          end;
        end;
      end;
      if not Handled then begin
        if fBusy=nil then fBusy:=TStringList.Create;
        try
          fBusy.Add(MacroName);
          if MacroParam<>'' then begin
            // substitute param
            if not SubstituteStr(MacroParam,Data,Depth+1) then begin
              Result:=false;
              exit;
            end;
          end;
          // find macro and get value
          ExecuteMacro(MacroName,MacroParam,Data,Handled,Abort,Depth+1);
          if Abort then begin
            Result:=false;
            exit;
          end;
        finally
          fBusy.Delete(fBusy.Count-1);
        end;
        MacroStr:=MacroParam;
      end;
      // mark unhandled macros
      if not Handled and MarkUnhandledMacros then begin
        MacroStr:=Format(lisTMunknownMacro, [MacroStr]);
        Handled:=true;
      end;
      // replace macro with new value
      if Handled then begin
        if MacroStart>LoopPos then
          LoopDepth:=1
        else begin
          inc(LoopDepth);
          //DebugLn(['TTransferMacroList.SubstituteStr double macro: ',s,' Depth=',LoopDepth,' Pos=',LoopPos]);
        end;
        LoopPos:=MacroStart;
        s:=copy(s,1,MacroStart-1)+MacroStr+copy(s,MacroEnd,length(s));
        sLen:=length(s);
        // continue at replacement, because a macrovalue can contain macros
        MacroEnd:=MacroStart;
      end;
    end;
    MacroStart:=MacroEnd;
  until false;
  
  // convert $$ chars
  MacroStart:=2;
  while (MacroStart<sLen) do begin
    if (s[MacroStart]='$') and (s[MacroStart+1]='$') then begin
      System.Delete(s,MacroStart,1);
      dec(sLen);
    end;
    inc(MacroStart);
  end;
end;

procedure TTransferMacroList.ExecuteMacro(const MacroName: string;
  var MacroParam: string; const Data: PtrInt; out Handled, Abort: boolean;
  Depth: integer);
var
  Macro: TTransferMacro;
begin
  Handled:=false;
  Abort:=false;
  Macro:=FindByName(MacroName);
  DoSubstitution(Macro,MacroName,MacroParam,Data,Handled,Abort,Depth);
  if Abort or Handled then exit;
  if Macro=nil then exit;
  if Assigned(Macro.MacroFunction) then begin
    MacroParam:=Macro.MacroFunction(MacroParam,Data,Abort);
    if Abort then exit;
  end else begin
    MacroParam:=Macro.Value;
  end;
  Handled:=true;
end;

class function TTransferMacroList.StrHasMacros(const s: string): boolean;
// search for $( or $xxx(
var
  p: Integer;
  Len: Integer;
begin
  Result:=false;
  p:=1;
  Len:=length(s);
  while (p<Len) do begin
    if s[p]='$' then begin
      inc(p);
      if (p<Len) and (s[p]<>'$') then begin
        // skip macro function name
        while (p<Len) and (s[p]<>'(') do inc(p);
        if (p<Len) then begin
          Result:=true;
          exit;
        end;
      end else begin
        // $$ is not a macro
        inc(p);
      end;
    end else
      inc(p);
  end;
end;

function TTransferMacroList.FindByName(const MacroName: string): TTransferMacro;
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
begin
  l:=0;
  r:=fItems.Count-1;
  m:=0;
  while l<=r do begin
    m:=(l+r) shr 1;
    Result:=Items[m];
    cmp:=UTF8CompareText(MacroName,Result.Name);
    if cmp<0 then
      r:=m-1
    else if cmp>0 then
      l:=m+1
    else begin
      exit;
    end;
  end;
  Result:=nil;
end;

function TTransferMacroList.MF_Ext(const Filename:string;
  const Data: PtrInt; var Abort: boolean):string;
begin
  Result:=ExtractFileExt(Filename);
end;

function TTransferMacroList.MF_Path(const Filename:string; 
  const Data: PtrInt; var Abort: boolean):string;
begin
  Result:=TrimFilename(ExtractFilePath(Filename));
  //debugln(['TTransferMacroList.MF_Path ',Filename,' Result=',Result]);
end;

function TTransferMacroList.MF_Name(const Filename:string; 
  const Data: PtrInt; var Abort: boolean):string;
begin
  Result:=ExtractFilename(Filename);
end;

function TTransferMacroList.MF_NameOnly(const Filename:string;
  const Data: PtrInt; var Abort: boolean):string;
var Ext:string;
begin
  Result:=ExtractFileName(Filename);
  Ext:=ExtractFileExt(Result);
  Result:=copy(Result,1,length(Result)-length(Ext));
end;

function TTransferMacroList.MF_MakeDir(const Filename: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=Filename;
  if (Result<>'') and (Result[length(Result)]<>PathDelim) then
    Result:=Result+PathDelim;
  Result:=TrimFilename(Result);
end;

function TTransferMacroList.MF_MakeFile(const Filename: string;
  const Data: PtrInt; var Abort: boolean): string;
var
  ChompLen: integer;
begin
  Result:=Filename;
  ChompLen:=0;
  while (length(Filename)>ChompLen)
  and (Filename[length(Filename)-ChompLen]=PathDelim) do
    inc(ChompLen);
  if ChompLen>0 then
    Result:=LeftStr(Result,length(Filename)-ChompLen);
  Result:=TrimFilename(Result);
end;

function TTransferMacroList.MF_Trim(const Filename: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Result:=TrimFilename(Filename);
end;

procedure TTransferMacroList.DoSubstitution(TheMacro: TTransferMacro;
  const MacroName: string; var s: string; const Data: PtrInt; var Handled,
  Abort: boolean; Depth: integer);
begin
  if Assigned(OnSubstitution) then
    OnSubstitution(TheMacro,MacroName,s,Data,Handled,Abort,Depth);
end;

{ TLazIDEMacros }

function TLazIDEMacros.StrHasMacros(const s: string): boolean;
begin
  Result:=GlobalMacroList.StrHasMacros(s);
end;

function TLazIDEMacros.SubstituteMacros(var s: string): boolean;
begin
  Result:=GlobalMacroList.SubstituteStr(s);
end;

function TLazIDEMacros.IsMacro(const Name: string): boolean;
begin
  Result:=GlobalMacroList.FindByName(Name)<>nil;
end;

procedure TLazIDEMacros.Add(NewMacro: TTransferMacro);
Begin
  GlobalMacroList.Add(NewMacro);
end;

procedure InternalInit;
var
  c: char;
begin
  for c:=Low(char) to High(char) do begin
    IsIdentChar[c]:=c in ['a'..'z','A'..'Z','0'..'9','_'];
  end;
end;

initialization
  InternalInit;

end.
