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
    TCodeToolManager gathers all tools in one single Object and makes it easy
    to use the code tools in a program.

  ToDo:

}
unit CodeToolManager;

{$ifdef fpc}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeCompletionTool, CodeTree, CodeAtom, SourceChanger,
  DefineTemplates, CodeCache, ExprEval, LinkScanner, KeywordFuncLists, TypInfo;

type
  TCodeToolManager = class;

  TGetStringProc = procedure(const s: string) of object;
  TOnBeforeApplyChanges = procedure(Manager: TCodeToolManager;
                                    var Abort: boolean) of object;
  TOnAfterApplyChanges = procedure(Manager: TCodeToolManager) of object;

  TCodeToolManager = class
  private
    FCatchExceptions: boolean;
    FCheckFilesOnDisk: boolean;
    FCodeTool: TCodeCompletionCodeTool;
    FCursorBeyondEOL: boolean;
    FErrorCode: TCodeBuffer;
    FErrorColumn: integer;
    FErrorLine: integer;
    FErrorMsg: string;
    FErrorTopLine: integer;
    FIndentSize: integer;
    FJumpCentered: boolean;
    FOnAfterApplyChanges: TOnAfterApplyChanges;
    FOnBeforeApplyChanges: TOnBeforeApplyChanges;
    FSourceExtensions: string; // default is '.pp;.pas;.lpr;.dpr;.dpk'
    FVisibleEditorLines: integer;
    FWriteExceptions: boolean;
    function OnScannerGetInitValues(Code: Pointer): TExpressionEvaluator;
    procedure OnDefineTreeReadValue(Sender: TObject; const VariableName: string;
                                    var Value: string);
    procedure OnGlobalValuesChanged;
    function GetMainCode(Code: TCodeBuffer): TCodeBuffer;
    function InitCodeTool(Code: TCodeBuffer): boolean;
    procedure SetCheckFilesOnDisk(NewValue: boolean);
    procedure SetIndentSize(NewValue: integer);
    procedure SetVisibleEditorLines(NewValue: integer);
    procedure SetJumpCentered(NewValue: boolean);
    procedure SetCursorBeyondEOL(NewValue: boolean);
    procedure BeforeApplyingChanges(var Abort: boolean);
    procedure AfterApplyingChanges;
    function HandleException(AnException: Exception): boolean;
  public
    DefinePool: TDefinePool; // definition templates (rules)
    DefineTree: TDefineTree; // cache for defines (e.g. initial compiler values)
    SourceCache: TCodeCache; // cache for source (units, include files, ...)
    SourceChangeCache: TSourceChangeCache; // cache for write accesses
    GlobalValues: TExpressionEvaluator;
    
    // Write Lock
    procedure BeginUpdate;
    procedure EndUpdate;

    // file handling
    property SourceExtensions: string
          read FSourceExtensions write FSourceExtensions;
    function FindFile(const ExpandedFilename: string): TCodeBuffer;
    function LoadFile(const ExpandedFilename: string;
          UpdateFromDisk, Revert: boolean): TCodeBuffer;
    function CreateFile(const AFilename: string): TCodeBuffer;
    function SaveBufferAs(OldBuffer: TCodeBuffer;const ExpandedFilename: string; 
          var NewBuffer: TCodeBuffer): boolean;
    function FilenameHasSourceExt(const ExpandedFilename: string): boolean;
    
    // exception handling
    property CatchExceptions: boolean
          read FCatchExceptions write FCatchExceptions;
    property WriteExceptions: boolean
          read FWriteExceptions write FWriteExceptions;
    property ErrorCode: TCodeBuffer read fErrorCode;
    property ErrorColumn: integer read fErrorColumn;
    property ErrorLine: integer read fErrorLine;
    property ErrorMessage: string read fErrorMsg;
    property ErrorTopLine: integer read fErrorTopLine;

    // tool settings
    property CheckFilesOnDisk: boolean
          read FCheckFilesOnDisk write SetCheckFilesOnDisk;
    property IndentSize: integer read FIndentSize write SetIndentSize;
    property VisibleEditorLines: integer
          read FVisibleEditorLines write SetVisibleEditorLines;
    property JumpCentered: boolean read FJumpCentered write SetJumpCentered;
    property CursorBeyondEOL: boolean
          read FCursorBeyondEOL write SetCursorBeyondEOL;

    // events
    property OnBeforeApplyChanges: TOnBeforeApplyChanges
          read FOnBeforeApplyChanges write FOnBeforeApplyChanges;
    property OnAfterApplyChanges: TOnAfterApplyChanges
          read FOnAfterApplyChanges write FOnAfterApplyChanges;

    // syntax checking  (true on syntax is ok)
    function CheckSyntax(Code: TCodeBuffer; var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer; var ErrorMsg: string): boolean;

    // method jumping
    function JumpToMethod(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;

    // find declaration
    function FindDeclaration(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;

    // functions for events in the object inspector
    procedure GetCompatibleMethods(Code: TCodeBuffer; const AClassName: string;
          TypeData: PTypeData; Proc: TGetStringProc);
    function MethodExists(Code:TCodeBuffer; const AClassName,
          AMethodName: string; TypeData: PTypeData): boolean;
    function JumpToMethodBody(Code: TCodeBuffer;
          const AClassName, AMethodName: string;  TypeData: PTypeData;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    function RenameMethod(Code: TCodeBuffer; const AClassName, OldMethodName,
          NewMethodName: string; TypeData: PTypeData): boolean;
    function CreateMethod(Code: TCodeBuffer; const AClassName,
          NewMethodName: string; TypeData: PTypeData): boolean;
          
    // code completion = auto class completion, auto forward proc completion
    function CompleteCode(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;

    // source name  e.g. 'unit UnitName;'
    function GetSourceName(Code: TCodeBuffer): string;
    function RenameSource(Code: TCodeBuffer; const NewName: string): boolean;
    function GetSourceType(Code: TCodeBuffer): string;

    // uses sections
    function FindUnitInAllUsesSections(Code: TCodeBuffer;
          const AnUnitName: string;
          var NamePos, InPos: integer): boolean;
    function RenameUsedUnit(Code: TCodeBuffer;
          const OldUnitName, NewUnitName, NewUnitInFile: string): boolean;
    function AddUnitToMainUsesSection(Code: TCodeBuffer;
          const NewUnitName, NewUnitInFile: string): boolean;
    function RemoveUnitFromAllUsesSections(Code: TCodeBuffer;
          const AnUnitName: string): boolean;

    // resources
    function FindLFMFileName(Code: TCodeBuffer): string;
    function FindNextResourceFile(Code: TCodeBuffer;
          var LinkIndex: integer): TCodeBuffer;
    function FindLazarusResource(Code: TCodeBuffer;
          const ResourceName: string): TAtomPosition;
    function AddLazarusResource(Code: TCodeBuffer;
          const ResourceName, ResourceData: string): boolean;
    function RemoveLazarusResource(Code: TCodeBuffer;
          const ResourceName: string): boolean;
    function RenameMainInclude(Code: TCodeBuffer; const NewFilename: string;
          KeepPath: boolean): boolean;

    // Application.Createform(ClassName,VarName) statements in program source
    function FindCreateFormStatement(Code: TCodeBuffer; StartPos: integer;
          const AClassName, AVarName: string;
          var Position: integer): integer; // 0=found, -1=not found, 1=found, but wrong classname
    function AddCreateFormStatement(Code: TCodeBuffer;
          const AClassName, AVarName: string): boolean;
    function RemoveCreateFormStatement(Code: TCodeBuffer;
          const AVarName: string): boolean;
    function ListAllCreateFormStatements(Code: TCodeBuffer): TStrings;
    function SetAllCreateFromStatements(Code: TCodeBuffer; 
          List: TStrings): boolean;

    // form components
    function PublishedVariableExists(Code: TCodeBuffer;
          const AClassName, AVarName: string): boolean;
    function AddPublishedVariable(Code: TCodeBuffer;
          const AClassName,VarName, VarType: string): boolean;
    function RemovePublishedVariable(Code: TCodeBuffer;
          const AClassName, AVarName: string): boolean;


    function ApplyChanges: boolean;

    constructor Create;
    destructor Destroy; override;
    
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport(WriteTool,
          WriteDefPool, WriteDefTree, WriteCache, WriteGlobalValues: boolean);
  end;


var CodeToolBoss: TCodeToolManager;



implementation


{ TCodeToolManager }

constructor TCodeToolManager.Create;
begin
  inherited Create;
  FCheckFilesOnDisk:=true;
  DefineTree:=TDefineTree.Create;
  DefineTree.OnReadValue:=@OnDefineTreeReadValue;
  DefinePool:=TDefinePool.Create;
  SourceCache:=TCodeCache.Create;
  SourceChangeCache:=TSourceChangeCache.Create;
  SourceChangeCache.OnBeforeApplyChanges:=@BeforeApplyingChanges;
  SourceChangeCache.OnAfterApplyChanges:=@AfterApplyingChanges;
  GlobalValues:=TExpressionEvaluator.Create;
  FSourceExtensions:='.pp;.pas;.lpr;.dpr;.dpk';
  FCatchExceptions:=true;
  FWriteExceptions:=true;
  FIndentSize:=2;
  FVisibleEditorLines:=20;
  FJumpCentered:=true;
  FCursorBeyondEOL:=true;
end;

destructor TCodeToolManager.Destroy;
begin
{$IFDEF CTDEBUG}
writeln('[TCodeToolManager.Destroy] A');
{$ENDIF}
  GlobalValues.Free;
{$IFDEF CTDEBUG}
writeln('[TCodeToolManager.Destroy] B');
{$ENDIF}
  FCodeTool.Free;
{$IFDEF CTDEBUG}
writeln('[TCodeToolManager.Destroy] C');
{$ENDIF}
  DefineTree.Free;
  DefinePool.Free;
{$IFDEF CTDEBUG}
writeln('[TCodeToolManager.Destroy] D');
{$ENDIF}
  SourceChangeCache.Free;
{$IFDEF CTDEBUG}
writeln('[TCodeToolManager.Destroy] E');
{$ENDIF}
  SourceCache.Free;
{$IFDEF CTDEBUG}
writeln('[TCodeToolManager.Destroy] F');
{$ENDIF}
  inherited Destroy;
{$IFDEF CTDEBUG}
writeln('[TCodeToolManager.Destroy] END');
{$ENDIF}
{$IFDEF MEM_CHECK}
CheckHeap('TCodeToolManager.Destroy END');
{$ENDIF}
end;

procedure TCodeToolManager.BeginUpdate;
begin
  SourceChangeCache.BeginUpdate;
end;

procedure TCodeToolManager.EndUpdate;
begin
  SourceChangeCache.EndUpdate;
end;

function TCodeToolManager.FindFile(const ExpandedFilename: string): TCodeBuffer;
begin
  Result:=SourceCache.FindFile(ExpandedFilename);
end;

function TCodeToolManager.LoadFile(const ExpandedFilename: string;
  UpdateFromDisk, Revert: boolean): TCodeBuffer;
begin
{$IFDEF CTDEBUG}
writeln('>>>>>> [TCodeToolManager.LoadFile] ',ExpandedFilename,' Update=',UpdateFromDisk,' Revert=',Revert);
{$ENDIF}
  Result:=SourceCache.LoadFile(ExpandedFilename);
  if Result<>nil then begin
    if Revert then
      Result.Revert
    else if UpdateFromDisk then
      Result.Reload;
  end;
end;

function TCodeToolManager.CreateFile(const AFilename: string): TCodeBuffer;
begin
  Result:=SourceCache.CreateFile(AFilename);
{$IFDEF CTDEBUG}
writeln('****** TCodeToolManager.CreateFile "',AFilename,'" ',Result<>nil);
{$ENDIF}
end;

function TCodeToolManager.SaveBufferAs(OldBuffer: TCodeBuffer;
  const ExpandedFilename: string; var NewBuffer: TCodeBuffer): boolean;
begin
  Result:=SourceCache.SaveBufferAs(OldBuffer,ExpandedFilename,NewBuffer);
end;

function TCodeToolManager.FilenameHasSourceExt(
  const ExpandedFilename: string): boolean;
var i, CurExtStart, CurExtEnd, ExtStart, ExtLen: integer;
begin
  ExtStart:=length(ExpandedFilename);
  while (ExtStart>0) and (ExpandedFilename[ExtStart]<>'.')
  and (ExpandedFilename[ExtStart]<>PathDelim) do
    dec(ExtStart);
  if (ExtStart<1) or (ExpandedFilename[ExtStart]<>'.') then begin
    Result:=false;
    exit;
  end;
  ExtLen:=length(ExpandedFilename)-ExtStart+1;
  CurExtStart:=1;
  CurExtEnd:=CurExtStart;
  while CurExtEnd<=length(FSourceExtensions)+1 do begin
    if (CurExtEnd>length(FSourceExtensions))
    or (FSourceExtensions[CurExtEnd] in [':',';']) then begin
      // compare current extension with filename-extension
      if ExtLen=CurExtEnd-CurExtStart then begin
        i:=0;
        while (i<ExtLen) 
        and (UpChars[ExpandedFilename[i+ExtStart]]
            =UpChars[FSourceExtensions[CurExtStart+i]]) do
          inc(i);
        if i=ExtLen then begin
          Result:=true;
          exit;
        end;
      end;
      inc(CurExtEnd);
      CurExtStart:=CurExtEnd;
    end else
      inc(CurExtEnd);
  end;
  Result:=false;
end;

function TCodeToolManager.GetMainCode(Code: TCodeBuffer): TCodeBuffer;
begin
  // find MainCode (= the start source, e.g. a unit/program/package source)
  Result:=Code;
  if Result=nil then exit;
  while (not FilenameHasSourceExt(Result.Filename)) do begin
    // source is no begin of unit/program/package
    // perhaps it is included by another source
    if Result.LastIncludedByFile<>'' then begin
      // source is included
      Result:=SourceCache.LoadFile(Result.LastIncludedByFile);
      if Result=nil then exit;
    end else begin
      // source was never parsed
      exit;
    end;
  end;
  if FilenameHasSourceExt(Result.Filename) and (Result.Scanner=nil) then begin
    // create a scanner for the unit/program
    Result.Scanner:=TLinkScanner.Create;
    Result.Scanner.OnGetInitValues:=@OnScannerGetInitValues;
  end;
end;

function TCodeToolManager.ApplyChanges: boolean;
begin
  Result:=SourceChangeCache.Apply;
end;

function TCodeToolManager.InitCodeTool(Code: TCodeBuffer): boolean;
var MainCode: TCodeBuffer;
begin
  Result:=false;
  fErrorMsg:='';
  fErrorCode:=nil;
  fErrorLine:=-1;
  MainCode:=GetMainCode(Code);
  if MainCode=nil then begin
    fErrorMsg:='TCodeToolManager.InitCodeTool MainCode=nil';
    exit;
  end;
  if FCodeTool=nil then begin
    FCodeTool:=TCodeCompletionCodeTool.Create;
    FCodeTool.CheckFilesOnDisk:=FCheckFilesOnDisk;
    FCodeTool.IndentSize:=FIndentSize;
    FCodeTool.VisibleEditorLines:=FVisibleEditorLines;
    FCodeTool.JumpCentered:=FJumpCentered;
    FCodeTool.CursorBeyondEOL:=FCursorBeyondEOL;
  end;
  FCodeTool.ErrorPosition.Code:=nil;
  FCodeTool.Scanner:=MainCode.Scanner;
{$IFDEF CTDEBUG}
writeln('[TCodeToolManager.InitCodeTool] ',Code.Filename,' ',Code.SourceLength);
{$ENDIF}
  Result:=(FCodeTool.Scanner<>nil);
  if not Result then begin
    fErrorCode:=MainCode;
    fErrorMsg:='No scanner available';
  end;
end;

function TCodeToolManager.HandleException(AnException: Exception): boolean;
begin
  fErrorMsg:=AnException.Message;
  if FCodeTool<>nil then begin
    fErrorCode:=FCodeTool.ErrorPosition.Code;
    fErrorColumn:=FCodeTool.ErrorPosition.X;
    fErrorLine:=FCodeTool.ErrorPosition.Y;
    fErrorTopLine:=fErrorLine;
    if JumpCentered then begin
      dec(fErrorTopLine,VisibleEditorLines div 2);
      if fErrorTopLine<1 then fErrorTopLine:=1;
    end;
  end;
  if (AnException is ELinkScannerError)
  and (FCodeTool<>nil) and (FCodeTool.Scanner<>nil)
  and (FCodeTool.Scanner.Code<>nil)
  and (FCodeTool.Scanner.LinkCount>0) then begin
    fErrorCode:=TCodeBuffer(FCodeTool.Scanner.Code);
    if fErrorCode<>nil then
      fErrorCode.AbsoluteToLineCol(
        FCodeTool.Scanner.SrcPos,fErrorLine,fErrorColumn);
  end;
  if FWriteExceptions then begin
{$IFDEF CTDEBUG}
WriteDebugReport(true,false,false,false,false);
{$ENDIF}
    write('### TCodeToolManager.HandleException: "'+ErrorMessage+'"');
    if ErrorLine>0 then write(' at Line=',ErrorLine);
    if ErrorColumn>0 then write(' Col=',ErrorColumn);
    if ErrorCode<>nil then write(' in "',ErrorCode.Filename,'"');
    writeln('');
  end;
  if not FCatchExceptions then raise AnException;
  Result:=false;
end;

function TCodeToolManager.CheckSyntax(Code: TCodeBuffer;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer;
  var ErrorMsg: string): boolean;
// returns true on syntax correct
begin
  Result:=false;
  try
    if InitCodeTool(Code) then begin
      FCodeTool.BuildTree(false);
      Result:=true;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
  NewCode:=ErrorCode;
  NewX:=ErrorColumn;
  NewY:=ErrorLine;
  NewTopLine:=ErrorTopLine;
  ErrorMsg:=ErrorMessage;
end;

function TCodeToolManager.JumpToMethod(Code: TCodeBuffer; X,Y: integer;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.JumpToMethod A ',Code.Filename,' x=',x,' y=',y);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.JumpToMethod B ',FCodeTool.Scanner<>nil);
{$ENDIF}
  try
    Result:=FCodeTool.FindJumpPoint(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.JumpToMethod END ');
{$ENDIF}
end;

function TCodeToolManager.FindDeclaration(Code: TCodeBuffer; X,Y: integer;
  var NewCode: TCodeBuffer;
  var NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.FindDeclaration A ',Code.Filename,' x=',x,' y=',y);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.FindDeclaration B ',FCodeTool.Scanner<>nil);
{$ENDIF}
  try
    Result:=FCodeTool.FindDeclaration(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.FindDeclaration END ');
{$ENDIF}
end;

procedure TCodeToolManager.GetCompatibleMethods(Code: TCodeBuffer;
  const AClassName: string; TypeData: PTypeData; Proc: TGetStringProc);
begin
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.GetCompatibleMethods A ',Code.Filename,' Classname=',AClassname);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    FCodeTool.GetCompatiblePublishedMethods(UpperCaseStr(AClassName),
       TypeData,Proc);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.MethodExists(Code:TCodeBuffer;
  const AClassName, AMethodName: string;  TypeData: PTypeData): boolean;
begin
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.MethodExists A ',Code.Filename,' ',AClassName,':',AMethodName);
{$ENDIF}
  Result:=InitCodeTool(Code);
  if not Result then exit;
  try
    Result:=FCodeTool.PublishedMethodExists(UpperCaseStr(AClassName),
              UpperCaseStr(AMethodName),TypeData);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.JumpToMethodBody(Code: TCodeBuffer;
  const AClassName, AMethodName: string;  TypeData: PTypeData;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer): boolean;
var NewPos: TCodeXYPosition;
begin
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.JumpToMethodBody A ',Code.Filename,' ',AClassName,':',AMethodName);
{$ENDIF}
  Result:=InitCodeTool(Code);
  if not Result then exit;
  try
    Result:=FCodeTool.JumpToPublishedMethodBody(UpperCaseStr(AClassName),
              UpperCaseStr(AMethodName),TypeData,NewPos,NewTopLine);
    if Result then begin
      NewCode:=NewPos.Code;
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenameMethod(Code: TCodeBuffer; const AClassName,
  OldMethodName, NewMethodName: string; TypeData: PTypeData): boolean;
begin
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.RenameMethod A');
{$ENDIF}
  Result:=InitCodeTool(Code);
  if not Result then exit;
  try
    SourceChangeCache.Clear;
    Result:=FCodeTool.RenamePublishedMethod(UpperCaseStr(AClassName),
              UpperCaseStr(OldMethodName),NewMethodName,TypeData,
              SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.CreateMethod(Code: TCodeBuffer; const AClassName,
  NewMethodName: string; TypeData: PTypeData): boolean;
begin
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.CreateMethod A');
{$ENDIF}
  Result:=InitCodeTool(Code);
  if not Result then exit;
  try
    SourceChangeCache.Clear;
    Result:=FCodeTool.CreatePublishedMethod(UpperCaseStr(AClassName),
              NewMethodName,TypeData,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.CompleteCode(Code: TCodeBuffer; X,Y: integer;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.CompleteCode A ',Code.Filename,' x=',x,' y=',y);
{$ENDIF}
  Result:=false;
  if not InitCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCodeTool.CompleteCode(CursorPos,NewPos,NewTopLine,SourceChangeCache);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.GetSourceName(Code: TCodeBuffer): string;
begin
  Result:='';
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.GetSourceName A ',Code.Filename,' ',Code.SourceLength);
{$ENDIF}
{$IFDEF MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.GetSourceName;
  except
    on e: Exception do HandleException(e);
  end;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.GetSourceName B ',Code.Filename,' ',Code.SourceLength);
{$IFDEF MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
writeln('SourceName=',Result);
{$ENDIF}
end;

function TCodeToolManager.GetSourceType(Code: TCodeBuffer): string;
begin
  Result:='';
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.GetSourceType A ',Code.Filename,' ',Code.SourceLength);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    // GetSourceType does not parse the code -> parse it with GetSourceName
    FCodeTool.GetSourceName;
    case FCodeTool.GetSourceType of
      ctnProgram: Result:='PROGRAM';
      ctnPackage: Result:='PACKAGE';
      ctnLibrary: Result:='LIBRARY';
      ctnUnit: Result:='UNIT';
    else
      Result:='';
    end;
  except
    on e: Exception do HandleException(e);
  end;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.GetSourceType END ',Code.Filename,',',Code.SourceLength);
{$IFDEF MEM_CHECK}
CheckHeap(IntToStr(GetMem_Cnt));
{$ENDIF}
writeln('SourceType=',Result);
{$ENDIF}
end;

function TCodeToolManager.RenameSource(Code: TCodeBuffer;
  const NewName: string): boolean;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.RenameSource A ',Code.Filename,' NewName=',NewName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.RenameSource(NewName,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindUnitInAllUsesSections(Code: TCodeBuffer;
  const AnUnitName: string;
  var NamePos, InPos: integer): boolean;
var NameAtomPos, InAtomPos: TAtomPosition;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.FindUnitInAllUsesSections A ',Code.Filename,' UnitName=',AnUnitName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.FindUnitInAllUsesSections B ',Code.Filename,' UnitName=',AnUnitName);
{$ENDIF}
  try
    Result:=FCodeTool.FindUnitInAllUsesSections(UpperCaseStr(AnUnitName),
                NameAtomPos, InAtomPos);
    if Result then begin
      NamePos:=NameAtomPos.StartPos;
      InPos:=InAtomPos.StartPos;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenameUsedUnit(Code: TCodeBuffer;
  const OldUnitName, NewUnitName, NewUnitInFile: string): boolean;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.RenameUsedUnit A, ',Code.Filename,' Old=',OldUnitName,' New=',NewUnitName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.RenameUsedUnit(UpperCaseStr(OldUnitName),NewUnitName,
                  NewUnitInFile,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.AddUnitToMainUsesSection(Code: TCodeBuffer;
  const NewUnitName, NewUnitInFile: string): boolean;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.AddUnitToMainUsesSection A ',Code.Filename,' NewUnitName=',NewUnitName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.AddUnitToMainUsesSection(NewUnitName, NewUnitInFile,
                    SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemoveUnitFromAllUsesSections(Code: TCodeBuffer;
  const AnUnitName: string): boolean;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.RemoveUnitFromAllUsesSections A ',Code.Filename,' UnitName=',AnUnitName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.RemoveUnitFromAllUsesSections(UpperCaseStr(AnUnitName),
                SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindLFMFileName(Code: TCodeBuffer): string;
var LinkIndex: integer;
  CurCode: TCodeBuffer;
  Ext: string;
begin
  Result:='';
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.FindLFMFileName A ',Code.Filename);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    LinkIndex:=-1;
    CurCode:=FCodeTool.FindNextIncludeInInitialization(LinkIndex);
    while (CurCode<>nil) do begin
      if UpperCaseStr(ExtractFileExt(CurCode.Filename))='.LRS' then begin
        Result:=CurCode.Filename;
        Ext:=ExtractFileExt(Result);
        Result:=copy(Result,1,length(Result)-length(Ext))+'.lfm';
        exit;
      end;
      CurCode:=FCodeTool.FindNextIncludeInInitialization(LinkIndex);
    end;
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.FindNextResourceFile(Code: TCodeBuffer;
  var LinkIndex: integer): TCodeBuffer;
begin
  Result:=nil;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.FindNextResourceFile A ',Code.Filename);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.FindNextIncludeInInitialization(LinkIndex);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.FindLazarusResource(Code: TCodeBuffer;
  const ResourceName: string): TAtomPosition;
begin
  Result.StartPos:=-1;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.FindLazarusResource A ',Code.Filename,' ResourceName=',ResourceName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.FindLazarusResource(ResourceName);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.AddLazarusResource(Code: TCodeBuffer;
  const ResourceName, ResourceData: string): boolean;
var ResCode: TCodeBuffer;
  LinkIndex: integer;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.AddLazarusResource A ',Code.Filename,' ResourceName=',ResourceName,' ',length(ResourceData));
{$ENDIF}
  if not InitCodeTool(Code) then exit;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.AddLazarusResource B ');
{$ENDIF}
  try
    LinkIndex:=-1;
    ResCode:=FCodeTool.FindNextIncludeInInitialization(LinkIndex);
    if ResCode=nil then exit;
    Result:=FCodeTool.AddLazarusResource(Rescode,ResourceName,ResourceData,
                  SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemoveLazarusResource(Code: TCodeBuffer;
  const ResourceName: string): boolean;
var ResCode: TCodeBuffer;
  LinkIndex: integer;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.RemoveLazarusResource A ',Code.Filename,' ResourceName=',ResourceName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    LinkIndex:=-1;
    ResCode:=FCodeTool.FindNextIncludeInInitialization(LinkIndex);
    if ResCode=nil then exit;
    Result:=FCodeTool.RemoveLazarusResource(ResCode,ResourceName,
                       SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenameMainInclude(Code: TCodeBuffer;
  const NewFilename: string; KeepPath: boolean): boolean;
var LinkIndex: integer;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.RenameMainInclude A ',Code.Filename,' NewFilename=',NewFilename,' KeepPath=',KeepPath);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    LinkIndex:=-1;
    if FCodeTool.FindNextIncludeInInitialization(LinkIndex)=nil then exit;
    Result:=FCodeTool.RenameInclude(LinkIndex,NewFilename,KeepPath,
                       SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindCreateFormStatement(Code: TCodeBuffer;
  StartPos: integer;
  const AClassName, AVarName: string;
  var Position: integer): integer;
// 0=found, -1=not found, 1=found, but wrong classname
var PosAtom: TAtomPosition;
begin
  Result:=-1;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.FindCreateFormStatement A ',Code.Filename,' StartPos=',StartPos,' ',AClassName,':',AVarName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.FindCreateFormStatement(StartPos,UpperCaseStr(AClassName),
                 UpperCaseStr(AVarName),PosAtom);
    if Result<>-1 then
      Position:=PosAtom.StartPos;
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.AddCreateFormStatement(Code: TCodeBuffer;
  const AClassName, AVarName: string): boolean;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.AddCreateFormStatement A ',Code.Filename,' ',AClassName,':',AVarName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.AddCreateFormStatement(AClassName,AVarName,
                    SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemoveCreateFormStatement(Code: TCodeBuffer;
  const AVarName: string): boolean;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.RemoveCreateFormStatement A ',Code.Filename,' ',AVarName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.RemoveCreateFormStatement(UpperCaseStr(AVarName),
                    SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ListAllCreateFormStatements(
  Code: TCodeBuffer): TStrings;
begin
  Result:=nil;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.ListAllCreateFormStatements A ',Code.Filename);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.ListAllCreateFormStatements;
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.SetAllCreateFromStatements(Code: TCodeBuffer; 
  List: TStrings): boolean;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.SetAllCreateFromStatements A ',Code.Filename);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.SetAllCreateFromStatements(List,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.PublishedVariableExists(Code: TCodeBuffer;
  const AClassName, AVarName: string): boolean;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.PublishedVariableExists A ',Code.Filename,' ',AClassName,':',AVarName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.FindPublishedVariable(UpperCaseStr(AClassName),
                 UpperCaseStr(AVarName))<>nil;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.AddPublishedVariable(Code: TCodeBuffer;
  const AClassName, VarName, VarType: string): boolean;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.AddPublishedVariable A ',Code.Filename,' ',AClassName,':',VarName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.AddPublishedVariable(UpperCaseStr(AClassName),
                      VarName,VarType,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemovePublishedVariable(Code: TCodeBuffer;
  const AClassName, AVarName: string): boolean;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('TCodeToolManager.RemovePublishedVariable A ',Code.Filename,' ',AClassName,':',AVarName);
{$ENDIF}
  if not InitCodeTool(Code) then exit;
  try
    Result:=FCodeTool.RemovePublishedVariable(UpperCaseStr(AClassName),
               UpperCaseStr(AVarName),SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.OnScannerGetInitValues(
  Code: Pointer): TExpressionEvaluator;
begin
  Result:=nil;
  if Code=nil then exit;
//DefineTree.WriteDebugReport;
  if not TCodeBuffer(Code).IsVirtual then
    Result:=DefineTree.GetDefinesForDirectory(
      ExtractFilePath(TCodeBuffer(Code).Filename))
  else
    Result:=DefineTree.GetDefinesForVirtualDirectory;
end;

procedure TCodeToolManager.OnDefineTreeReadValue(Sender: TObject;
  const VariableName: string; var Value: string);
begin
  Value:=GlobalValues[VariableName];
//writeln('[TCodeToolManager.OnDefineTreeReadValue] Name="',VariableName,'" = "',Value,'"');
end;

procedure TCodeToolManager.OnGlobalValuesChanged;
begin
  DefineTree.ClearCache;
end;

procedure TCodeToolManager.SetCheckFilesOnDisk(NewValue: boolean);
begin
  if NewValue=FCheckFilesOnDisk then exit;
  FCheckFilesOnDisk:=NewValue;
  if FCodeTool<>nil then
    FCodeTool.CheckFilesOnDisk:=NewValue;
end;

procedure TCodeToolManager.SetIndentSize(NewValue: integer);
begin
  if NewValue=FIndentSize then exit;
  FIndentSize:=NewValue;
  if FCodeTool<>nil then
    FCodeTool.IndentSize:=NewValue;
end;

procedure TCodeToolManager.SetVisibleEditorLines(NewValue: integer);
begin
  if NewValue=FVisibleEditorLines then exit;
  FVisibleEditorLines:=NewValue;
  if FCodeTool<>nil then
    FCodeTool.VisibleEditorLines:=NewValue;
end;

procedure TCodeToolManager.SetJumpCentered(NewValue: boolean);
begin
  if NewValue=FJumpCentered then exit;
  FJumpCentered:=NewValue;
  if FCodeTool<>nil then
    FCodeTool.JumpCentered:=NewValue;
end;

procedure TCodeToolManager.SetCursorBeyondEOL(NewValue: boolean);
begin
  if NewValue=FCursorBeyondEOL then exit;
  FCursorBeyondEOL:=NewValue;
  if FCodeTool<>nil then
    FCodeTool.CursorBeyondEOL:=NewValue;
end;

procedure TCodeToolManager.BeforeApplyingChanges(var Abort: boolean);
begin
  if Assigned(FOnBeforeApplyChanges) then
    FOnBeforeApplyChanges(Self,Abort);
end;

procedure TCodeToolManager.AfterApplyingChanges;
begin
  if Assigned(FOnAfterApplyChanges) then
    FOnAfterApplyChanges(Self);
end;

function TCodeToolManager.ConsistencyCheck: integer;
// 0 = ok
begin
  try
    Result:=0;
    if FCodeTool<>nil then begin
      Result:=FCodeTool.ConsistencyCheck;
      if Result<>0 then begin
        dec(Result,1000);  exit;
      end;
    end;
    Result:=DefinePool.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,2000);  exit;
    end;
    Result:=DefineTree.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,3000);  exit;
    end;
    Result:=SourceCache.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,4000);  exit;
    end;
    Result:=GlobalValues.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,5000);  exit;
    end;
    Result:=SourceChangeCache.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,6000);  exit;
    end;
  finally
    if (Result<>0) and (FCatchExceptions=false) then
      raise Exception.Create(
                        'TCodeToolManager.ConsistencyCheck='+IntToStr(Result));
  end;
  Result:=0;
end;

procedure TCodeToolManager.WriteDebugReport(WriteTool,
  WriteDefPool, WriteDefTree, WriteCache, WriteGlobalValues: boolean);
begin
  writeln('[TCodeToolManager.WriteDebugReport] Consistency=',ConsistencyCheck);
  if FCodeTool<>nil then begin
    if WriteTool then
      FCodeTool.WriteDebugTreeReport
    else
      writeln('  FCodeTool.ConsistencyCheck=',FCodeTool.ConsistencyCheck);
  end;
  if WriteDefPool then
    DefinePool.WriteDebugReport
  else
    writeln('  DefinePool.ConsistencyCheck=',DefinePool.ConsistencyCheck);
  if WriteDefTree then
    DefineTree.WriteDebugReport
  else
    writeln('  DefineTree.ConsistencyCheck=',DefineTree.ConsistencyCheck);
  if WriteCache then
    SourceCache.WriteDebugReport
  else
    writeln('  SourceCache.ConsistencyCheck=',SourceCache.ConsistencyCheck);
  if WriteGlobalValues then
    GlobalValues.WriteDebugReport
  else
    writeln('  GlobalValues.ConsistencyCheck=',GlobalValues.ConsistencyCheck);
end;

//-----------------------------------------------------------------------------

initialization
  CodeToolBoss:=TCodeToolManager.Create;


finalization
{$IFDEF CTDEBUG}
writeln('codetoolmanager.pas - finalization');
{$ENDIF}
  CodeToolBoss.Free;
  CodeToolBoss:=nil;
{$IFDEF CTDEBUG}
writeln('codetoolmanager.pas - finalization finished');
{$ENDIF}

end.

