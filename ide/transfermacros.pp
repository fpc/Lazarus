{  $Id$  }
{
 /***************************************************************************
                       idemacros.pp  -  macros for tools
                       ---------------------------------


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    This unit defines the classes TTransferMacro and TTransferMacroList. These
    classes stores and substitutes macros in strings. Transfer macros are an
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

uses Classes, SysUtils, FileCtrl;

type
  TTransferMacro = class;

  TOnSubstitution = procedure(TheMacro: TTransferMacro; var s:string;
    var Handled, Abort: boolean) of object;

  TMacroFunction = function(const s:string; var Abort: boolean):string of object;
  
  TTransferMacroFlag = (tmfInteractive);
  TTransferMacroFlags = set of TTransferMacroFlag;

  TTransferMacro = class
  public
    Name: string;
    Value: string;
    Description: string;
    MacroFunction: TMacroFunction;
    Flags: TTransferMacroFlags;
    constructor Create(AName, AValue, ADescription:string;
      AMacroFunction: TMacroFunction; TheFlags: TTransferMacroFlags);
  end;

  TTransferMacroList = class
  private
    fItems: TList;  // list of TTransferMacro
    FMarkUnhandledMacros: boolean;
    fOnSubstitution: TOnSubstitution;
    function GetItems(Index: integer): TTransferMacro;
    procedure SetItems(Index: integer; NewMacro: TTransferMacro);
    procedure SetMarkUnhandledMacros(const AValue: boolean);
  protected
    function MF_Ext(const Filename:string; var Abort: boolean):string; virtual;
    function MF_Path(const Filename:string; var Abort: boolean):string; virtual;
    function MF_Name(const Filename:string; var Abort: boolean):string; virtual;
    function MF_NameOnly(const Filename:string; var Abort: boolean):string; virtual;
    function MF_MakeDir(const Filename:string; var Abort: boolean):string; virtual;
    function MF_MakeFile(const Filename:string; var Abort: boolean):string; virtual;
    function MF_Trim(const Filename:string; var Abort: boolean):string; virtual;
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
    function SubstituteStr(var s:string): boolean; virtual;
    property OnSubstitution: TOnSubstitution
       read fOnSubstitution write fOnSubstitution;
    function FindByName(const MacroName: string): TTransferMacro; virtual;
    property MarkUnhandledMacros: boolean read FMarkUnhandledMacros write SetMarkUnhandledMacros;
  end;


implementation

var
  IsIdentChar: array[char] of boolean;

{ TTransferMacro }

constructor TTransferMacro.Create(AName, AValue, ADescription:string;
  AMacroFunction: TMacroFunction; TheFlags: TTransferMacroFlags);
begin
  Name:=AName;
  Value:=AValue;
  Description:=ADescription;
  MacroFunction:=AMacroFunction;
  Flags:=TheFlags;
end;

{ TTransferMacroList }

constructor TTransferMacroList.Create;
begin
  inherited Create;
  fItems:=TList.Create;
  FMarkUnhandledMacros:=true;
  Add(TTransferMacro.Create('Ext','','Function: extract file extension',@MF_Ext,[]));
  Add(TTransferMacro.Create('Path','','Function: extract file path',@MF_Path,[]));
  Add(TTransferMacro.Create('Name','','Function: extract file name+extension',
                                    @MF_Name,[]));
  Add(TTransferMacro.Create('NameOnly','','Function: extract file name only',
                                    @MF_NameOnly,[]));
  Add(TTransferMacro.Create('MakeDir','','Function: append path delimiter',
                                    @MF_MakeDir,[]));
  Add(TTransferMacro.Create('MakeFile','','Function: chomp path delimiter',
                                    @MF_MakeFile,[]));
end;

destructor TTransferMacroList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

function TTransferMacroList.GetItems(Index: integer): TTransferMacro;
begin
  Result:=TTransferMacro(fItems[Index]);
end;

procedure TTransferMacroList.SetItems(Index: integer;
  NewMacro: TTransferMacro);
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
begin
  fItems.Add(NewMacro);
end;

function TTransferMacroList.SubstituteStr(var s:string): boolean;
var MacroStart,MacroEnd: integer;
  MacroName, MacroStr, MacroParam: string;
  AMacro: TTransferMacro;
  Handled, Abort: boolean;
  OldMacroLen: Integer;
  NewMacroEnd: Integer;
  NewMacroLen: Integer;
  BehindMacroLen: Integer;
  NewString: String;
  InFrontOfMacroLen: Integer;
  NewStringLen: Integer;
  NewStringPos: Integer;
  sLen: Integer;

  function SearchBracketClose(Position:integer): integer;
  var BracketClose:char;
  begin
    if s[Position]='(' then BracketClose:=')'
    else BracketClose:='}';
    inc(Position);
    while (Position<=length(s)) and (s[Position]<>BracketClose) do begin
      if s[Position]='\' then
        inc(Position)
      else if (s[Position] in ['(','{']) then
        Position:=SearchBracketClose(Position);
      inc(Position);
    end;
    Result:=Position;
  end;

begin
  Result:=true;
  sLen:=length(s);
  MacroStart:=1;
  repeat
    while (MacroStart<sLen) do begin
      if (s[MacroStart]='$') and ((MacroStart=1) or (s[MacroStart-1]<>'\')) then
        break
      else
        inc(MacroStart);
    end;
    if MacroStart>=sLen then break;
    
    MacroEnd:=MacroStart+1;
    while (MacroEnd<=sLen) and (IsIdentChar[s[MacroEnd]]) do
      inc(MacroEnd);

    if (MacroEnd<sLen) and (s[MacroEnd] in ['(','{']) then begin
      MacroName:=copy(s,MacroStart+1,MacroEnd-MacroStart-1);
      MacroEnd:=SearchBracketClose(MacroEnd)+1;
      if MacroEnd>sLen+1 then break;
      OldMacroLen:=MacroEnd-MacroStart;
      MacroStr:=copy(s,MacroStart,OldMacroLen);
      // Macro found
      Handled:=false;
      Abort:=false;
      if MacroName<>'' then begin
        // Macro function -> substitute macro parameter first
        MacroParam:=copy(MacroStr,length(MacroName)+3,
                                  length(MacroStr)-length(MacroName)-3);
        if not SubstituteStr(MacroParam) then begin
          Result:=false;
          exit;
        end;
        MacroStr:=MacroParam;
        AMacro:=FindByName(MacroName);
        if Assigned(fOnSubstitution) then
          fOnSubstitution(AMacro,MacroStr,Handled,Abort);
        if Abort then begin
          Result:=false;
          exit;
        end;
        if (not Handled) and (AMacro<>nil) and (Assigned(AMacro.MacroFunction)) then
        begin
          MacroStr:=AMacro.MacroFunction(MacroStr,Abort);
          if Abort then begin
            Result:=false;
            exit;
          end;
          Handled:=true;
        end;  
      end else begin
        // Macro variable
        MacroStr:=copy(s,MacroStart+2,OldMacroLen-3);
        AMacro:=FindByName(MacroStr);
        if Assigned(fOnSubstitution) then
          fOnSubstitution(AMacro,MacroStr,Handled,Abort);
        if Abort then begin
          Result:=false;
          exit;
        end;
        if (not Handled) and (AMacro<>nil) then begin
          // standard macro
          MacroStr:=AMacro.Value;
          Handled:=true;
        end;
      end;
      // mark unhandled macros
      if not Handled and MarkUnhandledMacros then begin
        MacroStr:='(unknown macro: '+MacroStr+')';
        Handled:=true;
      end;
      // replace macro with new value
      if Handled then begin
        NewMacroLen:=length(MacroStr);
        NewMacroEnd:=MacroStart+NewMacroLen;
        InFrontOfMacroLen:=MacroStart-1;
        BehindMacroLen:=sLen-MacroEnd+1;
        NewString:='';
        NewStringLen:=InFrontOfMacroLen+NewMacroLen+BehindMacroLen;
        if NewStringLen>0 then begin
          SetLength(NewString,NewStringLen);
          NewStringPos:=1;
          if InFrontOfMacroLen>0 then begin
            Move(s[1],NewString[NewStringPos],InFrontOfMacroLen);
            inc(NewStringPos,InFrontOfMacroLen);
          end;
          if NewMacroLen>0 then begin
            Move(MacroStr[1],NewString[NewStringPos],NewMacroLen);
            inc(NewStringPos,NewMacroLen);
          end;
          if BehindMacroLen>0 then begin
            Move(s[MacroEnd],NewString[NewStringPos],BehindMacroLen);
            inc(NewStringPos,BehindMacroLen);
          end;
        end;
        s:=NewString;
        sLen:=length(s);
        // continue after the replacement
        MacroEnd:=NewMacroEnd;
      end;
    end;
    MacroStart:=MacroEnd;
  until false;
  
  // convert \$ chars
  MacroStart:=2;
  while (MacroStart<=length(s)) do begin
    if (s[MacroStart]='$') and (s[MacroStart-1]='\') then begin
      System.Delete(s,MacroStart-1,1);
    end else
      inc(MacroStart);
  end;
end;

function TTransferMacroList.FindByName(const MacroName: string): TTransferMacro;
var
  i:integer;
  Cnt: Integer;
begin
  Cnt:=Count;
  for i:=0 to Cnt-1 do
    if AnsiCompareText(MacroName,Items[i].Name)=0 then begin
      Result:=Items[i];
      exit;
    end;
  Result:=nil;
end;

function TTransferMacroList.MF_Ext(const Filename:string;
  var Abort: boolean):string;
begin
  Result:=ExtractFileExt(Filename);
end;

function TTransferMacroList.MF_Path(const Filename:string; 
 var Abort: boolean):string;
begin
  Result:=ExtractFilePath(Filename);
end;

function TTransferMacroList.MF_Name(const Filename:string; 
  var Abort: boolean):string;
begin
  Result:=ExtractFilename(Filename);
end;

function TTransferMacroList.MF_NameOnly(const Filename:string;
  var Abort: boolean):string;
var Ext:string;
begin
  Result:=ExtractFileName(Filename);
  Ext:=ExtractFileExt(Result);
  Result:=copy(Result,1,length(Result)-length(Ext));
end;

function TTransferMacroList.MF_MakeDir(const Filename: string;
  var Abort: boolean): string;
begin
  Result:=Filename;
  if (Result<>'') and (Result[length(Result)]<>PathDelim) then
    Result:=Result+PathDelim;
end;

function TTransferMacroList.MF_MakeFile(const Filename: string;
  var Abort: boolean): string;
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
end;

function TTransferMacroList.MF_Trim(const Filename: string; var Abort: boolean
  ): string;
begin
  Result:=TrimFilename(Filename);
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
