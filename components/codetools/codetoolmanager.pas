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

}
unit CodeToolManager;

{$ifdef fpc}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{ $DEFINE CTDEBUG}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeToolsStrConsts, EventCodeTool, CodeTree, CodeAtom,
  SourceChanger, DefineTemplates, CodeCache, ExprEval, LinkScanner,
  KeywordFuncLists, TypInfo, AVL_Tree, CustomCodeTool, FindDeclarationTool,
  IdentCompletionTool, ResourceCodeTool;

type
  TCodeToolManager = class;

  TGetStringProc = procedure(const s: string) of object;
  TOnBeforeApplyChanges = procedure(Manager: TCodeToolManager;
                                    var Abort: boolean) of object;
  TOnAfterApplyChanges = procedure(Manager: TCodeToolManager) of object;
  
  TCodeTool = TEventsCodeTool;
  
  TOnSearchUsedUnit = function(const SrcFilename: string;
    const TheUnitName, TheUnitInFilename: string): TCodeBuffer of object;
  TOnCodeToolCheckAbort = function: boolean of object;

  TCodeToolManager = class
  private
    FAbortable: boolean;
    FAddInheritedCodeToOverrideMethod: boolean;
    FAdjustTopLineDueToComment: boolean;
    FCatchExceptions: boolean;
    FCheckFilesOnDisk: boolean;
    FCompleteProperties: boolean;
    FCurCodeTool: TCodeTool; // current codetool
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
    FOnCheckAbort: TOnCodeToolCheckAbort;
    FOnSearchUsedUnit: TOnSearchUsedUnit;
    FResourceTool: TResourceCodeTool;
    FSetPropertyVariablename: string;
    FSourceExtensions: string; // default is '.pp;.pas;.lpr;.dpr;.dpk'
    FSourceTools: TAVLTree; // tree of TCustomCodeTool
    FVisibleEditorLines: integer;
    FWriteExceptions: boolean;
    FWriteLockCount: integer;// Set/Unset counter
    FWriteLockStep: integer; // current write lock ID
    function OnScannerGetInitValues(Code: Pointer;
      var AChangeStep: integer): TExpressionEvaluator;
    procedure OnDefineTreeReadValue(Sender: TObject; const VariableName: string;
                                    var Value: string);
    procedure OnGlobalValuesChanged;
    function DoOnFindUsedUnit(SrcTool: TFindDeclarationTool; const TheUnitName,
          TheUnitInFilename: string): TCodeBuffer;
    function GetMainCode(Code: TCodeBuffer): TCodeBuffer;
    procedure CreateScanner(Code: TCodeBuffer);
    function InitCurCodeTool(Code: TCodeBuffer): boolean;
    function InitResourceTool: boolean;
    function FindCodeToolForSource(Code: TCodeBuffer): TCustomCodeTool;
    function GetCodeToolForSource(Code: TCodeBuffer;
      ExceptionOnError: boolean): TCustomCodeTool;
    procedure SetAbortable(const AValue: boolean);
    procedure SetAddInheritedCodeToOverrideMethod(const AValue: boolean);
    procedure SetCheckFilesOnDisk(NewValue: boolean);
    procedure SetCompleteProperties(const AValue: boolean);
    procedure SetIndentSize(NewValue: integer);
    procedure SetVisibleEditorLines(NewValue: integer);
    procedure SetJumpCentered(NewValue: boolean);
    procedure SetCursorBeyondEOL(NewValue: boolean);
    procedure BeforeApplyingChanges(var Abort: boolean);
    procedure AfterApplyingChanges;
    function HandleException(AnException: Exception): boolean;
    function OnGetCodeToolForBuffer(Sender: TObject;
      Code: TCodeBuffer): TFindDeclarationTool;
    procedure OnToolSetWriteLock(Lock: boolean);
    procedure OnToolGetWriteLockInfo(var WriteLockIsSet: boolean;
      var WriteLockStep: integer);
    function OnParserProgress(Tool: TCustomCodeTool): boolean;
    function OnScannerProgress(Sender: TLinkScanner): boolean;
    function GetResourceTool: TResourceCodeTool;
    function GetOwnerForCodeTreeNode(ANode: TCodeTreeNode): TObject;
  public
    DefinePool: TDefinePool; // definition templates (rules)
    DefineTree: TDefineTree; // cache for defines (e.g. initial compiler values)
    SourceCache: TCodeCache; // cache for source (units, include files, ...)
    SourceChangeCache: TSourceChangeCache; // cache for write accesses
    GlobalValues: TExpressionEvaluator;
    IdentifierList: TIdentifierList;
    
    procedure ActivateWriteLock;
    procedure DeactivateWriteLock;

    // file handling
    property SourceExtensions: string
          read FSourceExtensions write FSourceExtensions;
    function FindFile(const ExpandedFilename: string): TCodeBuffer;
    function LoadFile(const ExpandedFilename: string;
          UpdateFromDisk, Revert: boolean): TCodeBuffer;
    function CreateFile(const AFilename: string): TCodeBuffer;
    function SaveBufferAs(OldBuffer: TCodeBuffer;const ExpandedFilename: string; 
          var NewBuffer: TCodeBuffer): boolean;
    function FilenameHasSourceExt(const AFilename: string): boolean;
    property OnSearchUsedUnit: TOnSearchUsedUnit
          read FOnSearchUsedUnit write FOnSearchUsedUnit;
    
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
    property Abortable: boolean read FAbortable write SetAbortable;
    property OnCheckAbort: TOnCodeToolCheckAbort
          read FOnCheckAbort write FOnCheckAbort;

    // tool settings
    property AdjustTopLineDueToComment: boolean
        read FAdjustTopLineDueToComment write FAdjustTopLineDueToComment;
    property CheckFilesOnDisk: boolean
          read FCheckFilesOnDisk write SetCheckFilesOnDisk;
    property CursorBeyondEOL: boolean
          read FCursorBeyondEOL write SetCursorBeyondEOL;
    property IndentSize: integer read FIndentSize write SetIndentSize;
    property JumpCentered: boolean read FJumpCentered write SetJumpCentered;
    property SetPropertyVariablename: string
      read FSetPropertyVariablename write FSetPropertyVariablename;
    property VisibleEditorLines: integer
          read FVisibleEditorLines write SetVisibleEditorLines;
    property CompleteProperties: boolean
      read FCompleteProperties write SetCompleteProperties;
    property AddInheritedCodeToOverrideMethod: boolean
      read FAddInheritedCodeToOverrideMethod write SetAddInheritedCodeToOverrideMethod;

    // source changing
    procedure BeginUpdate;
    procedure EndUpdate;
    function ApplyChanges: boolean;
    property OnBeforeApplyChanges: TOnBeforeApplyChanges
          read FOnBeforeApplyChanges write FOnBeforeApplyChanges;
    property OnAfterApplyChanges: TOnAfterApplyChanges
          read FOnAfterApplyChanges write FOnAfterApplyChanges;
          
    // defines
    function GetIncludePathForDirectory(const Directory: string): string;
    function GetSrcPathForDirectory(const Directory: string): string;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    // syntax checking  (true on syntax is ok)
    function CheckSyntax(Code: TCodeBuffer; var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer; var ErrorMsg: string): boolean;
          
    // compiler directives
    function GuessMisplacedIfdefEndif(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    // find include directive of include file at position X,Y
    function FindEnclosingIncludeDirective(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
          
    // keywords
    function IsKeyword(Code: TCodeBuffer; const KeyWord: string): boolean;

    // blocks (e.g. begin..end, case..end, try..finally..end, repeat..until)
    function FindBlockCounterPart(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    function FindBlockStart(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    function GuessUnclosedBlock(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;

    // method jumping
    function JumpToMethod(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer;
          var RevertableJump: boolean): boolean;

    // find declaration
    function FindDeclaration(Code: TCodeBuffer; X,Y: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    function FindSmartHint(Code: TCodeBuffer; X,Y: integer): string;
    
    // gather identifiers
    function GatherIdentifiers(Code: TCodeBuffer; X,Y: integer): boolean;

    // functions for events in the object inspector
    function GetCompatiblePublishedMethods(Code: TCodeBuffer;
          const AClassName: string; TypeData: PTypeData;
          Proc: TGetStringProc): boolean;
    function PublishedMethodExists(Code:TCodeBuffer; const AClassName,
          AMethodName: string; TypeData: PTypeData;
          var MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean
          ): boolean;
    function JumpToPublishedMethodBody(Code: TCodeBuffer;
          const AClassName, AMethodName: string;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;
    function RenamePublishedMethod(Code: TCodeBuffer;
          const AClassName, OldMethodName,
          NewMethodName: string): boolean;
    function CreatePublishedMethod(Code: TCodeBuffer; const AClassName,
          NewMethodName: string; ATypeInfo: PTypeInfo): boolean;
          
    // code completion = auto class completion, auto forward proc completion
    function CompleteCode(Code: TCodeBuffer; X,Y,TopLine: integer;
          var NewCode: TCodeBuffer;
          var NewX, NewY, NewTopLine: integer): boolean;

    // source name  e.g. 'unit UnitName;'
    function GetSourceName(Code: TCodeBuffer; SearchMainCode: boolean): string;
    function GetCachedSourceName(Code: TCodeBuffer): string;
    function RenameSource(Code: TCodeBuffer; const NewName: string): boolean;
    function GetSourceType(Code: TCodeBuffer; SearchMainCode: boolean): string;

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
    function FindUsedUnits(Code: TCodeBuffer; var MainUsesSection,
          ImplementationUsesSection: TStrings): boolean;

    // resources
    function FindLFMFileName(Code: TCodeBuffer): string;
    function FindNextResourceFile(Code: TCodeBuffer;
          var LinkIndex: integer): TCodeBuffer;
    function AddLazarusResourceHeaderComment(Code: TCodeBuffer;
          const CommentText: string): boolean;
    function FindLazarusResource(Code: TCodeBuffer;
          const ResourceName: string): TAtomPosition;
    function AddLazarusResource(Code: TCodeBuffer;
          const ResourceName, ResourceData: string): boolean;
    function RemoveLazarusResource(Code: TCodeBuffer;
          const ResourceName: string): boolean;
    function RenameMainInclude(Code: TCodeBuffer; const NewFilename: string;
          KeepPath: boolean): boolean;
    function RenameIncludeDirective(Code: TCodeBuffer; LinkIndex: integer;
          const NewFilename: string; KeepPath: boolean): boolean;

    // Application.Createform(ClassName,VarName) statements in program source
    function FindCreateFormStatement(Code: TCodeBuffer; StartPos: integer;
          const AClassName, AVarName: string;
          var Position: integer): integer; // 0=found, -1=not found, 1=found, but wrong classname
    function AddCreateFormStatement(Code: TCodeBuffer;
          const AClassName, AVarName: string): boolean;
    function RemoveCreateFormStatement(Code: TCodeBuffer;
          const AVarName: string): boolean;
    function ChangeCreateFormStatement(Code: TCodeBuffer;
          const OldClassName, OldVarName: string;
          const NewClassName, NewVarName: string;
          OnlyIfExists: boolean): boolean;
    function ListAllCreateFormStatements(Code: TCodeBuffer): TStrings;
    function SetAllCreateFromStatements(Code: TCodeBuffer; 
          List: TStrings): boolean;

    // forms
    function RenameForm(Code: TCodeBuffer;
      const OldFormName, OldFormClassName: string;
      const NewFormName, NewFormClassName: string): boolean;

    // form components
    function PublishedVariableExists(Code: TCodeBuffer;
          const AClassName, AVarName: string): boolean;
    function AddPublishedVariable(Code: TCodeBuffer;
          const AClassName,VarName, VarType: string): boolean;
    function RemovePublishedVariable(Code: TCodeBuffer;
          const AClassName, AVarName: string): boolean;
    function RenamePublishedVariable(Code: TCodeBuffer;
          const AClassName, OldVariableName, NewVarName,
          VarType: shortstring): boolean;
          

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    constructor Create;
    destructor Destroy; override;
    
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport(WriteTool,
          WriteDefPool, WriteDefTree, WriteCache, WriteGlobalValues: boolean);
  end;


var CodeToolBoss: TCodeToolManager;


implementation


function CompareCodeToolMainSources(Data1, Data2: Pointer): integer;
var
  Src1, Src2: integer;
begin
  Src1:=Integer(TCustomCodeTool(Data1).Scanner.MainCode);
  Src2:=Integer(TCustomCodeTool(Data2).Scanner.MainCode);
  if Src1<Src2 then
    Result:=-1
  else if Src1>Src2 then
    Result:=+1
  else
    Result:=0;
end;

function GetOwnerForCodeTreeNode(ANode: TCodeTreeNode): TObject;
begin
  Result:=CodeToolBoss.GetOwnerForCodeTreeNode(ANode);
end;


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
  FAddInheritedCodeToOverrideMethod:=true;
  FAdjustTopLineDueToComment:=true;
  FCatchExceptions:=true;
  FCompleteProperties:=true;
  FCursorBeyondEOL:=true;
  FIndentSize:=2;
  FJumpCentered:=true;
  FSourceExtensions:='.pp;.pas;.lpr;.dpr;.dpk';
  FVisibleEditorLines:=20;
  FWriteExceptions:=true;
  FSourceTools:=TAVLTree.Create(@CompareCodeToolMainSources);
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
  IdentifierList.Free;
  FSourceTools.FreeAndClear;
  FSourceTools.Free;
  FResourceTool.Free;
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
  const AFilename: string): boolean;
var i, CurExtStart, CurExtEnd, ExtStart, ExtLen: integer;
begin
  ExtStart:=length(AFilename);
  while (ExtStart>0) and (AFilename[ExtStart]<>'.')
  and (AFilename[ExtStart]<>PathDelim) do
    dec(ExtStart);
  if (ExtStart<1) or (AFilename[ExtStart]<>'.') then begin
    Result:=false;
    exit;
  end;
  ExtLen:=length(AFilename)-ExtStart+1;
  CurExtStart:=1;
  CurExtEnd:=CurExtStart;
  while CurExtEnd<=length(FSourceExtensions)+1 do begin
    if (CurExtEnd>length(FSourceExtensions))
    or (FSourceExtensions[CurExtEnd] in [':',';']) then begin
      // compare current extension with filename-extension
      if ExtLen=CurExtEnd-CurExtStart then begin
        i:=0;
        while (i<ExtLen) 
        and (UpChars[AFilename[i+ExtStart]]
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
  // if this is an include file, find the top level source
  while (Result.LastIncludedByFile<>'') do begin
    Result:=SourceCache.LoadFile(Result.LastIncludedByFile);
    if Result=nil then exit;
  end;
  CreateScanner(Result);
end;

procedure TCodeToolManager.CreateScanner(Code: TCodeBuffer);
begin
  if FilenameHasSourceExt(Code.Filename) and (Code.Scanner=nil) then begin
    // create a scanner for the unit/program
    Code.Scanner:=TLinkScanner.Create;
    Code.Scanner.OnGetInitValues:=@OnScannerGetInitValues;
    Code.Scanner.OnSetGlobalWriteLock:=@OnToolSetWriteLock;
    Code.Scanner.OnGetGlobalWriteLockInfo:=@OnToolGetWriteLockInfo;
    Code.Scanner.OnProgress:=@OnScannerProgress;
  end;
end;

function TCodeToolManager.ApplyChanges: boolean;
begin
  Result:=SourceChangeCache.Apply;
end;

function TCodeToolManager.GetIncludePathForDirectory(const Directory: string
  ): string;
begin
  Result:=DefineTree.GetIncludePathForDirectory(Directory);
end;

function TCodeToolManager.GetSrcPathForDirectory(const Directory: string
  ): string;
begin
  Result:=DefineTree.GetSrcPathForDirectory(Directory);
end;

function TCodeToolManager.InitCurCodeTool(Code: TCodeBuffer): boolean;
var MainCode: TCodeBuffer;
begin
  Result:=false;
  fErrorMsg:='';
  fErrorCode:=nil;
  fErrorLine:=-1;
  MainCode:=GetMainCode(Code);
  if MainCode=nil then begin
    fErrorMsg:='TCodeToolManager.InitCurCodeTool MainCode=nil';
    exit;
  end;
  if MainCode.Scanner=nil then begin
    FErrorMsg:=Format(ctsNoScannerFound,[MainCode.Filename]);
    exit;
  end;
  FCurCodeTool:=TCodeTool(GetCodeToolForSource(MainCode,true));
  FCurCodeTool.ErrorPosition.Code:=nil;
  {$IFDEF CTDEBUG}
  writeln('[TCodeToolManager.InitCurCodeTool] ',Code.Filename,' ',Code.SourceLength);
  {$ENDIF}
  Result:=(FCurCodeTool.Scanner<>nil);
  if not Result then begin
    fErrorCode:=MainCode;
    fErrorMsg:=ctsNoScannerAvailable;
  end;
end;

function TCodeToolManager.InitResourceTool: boolean;
begin
  fErrorMsg:='';
  fErrorCode:=nil;
  fErrorLine:=-1;
  Result:=true;
end;

function TCodeToolManager.HandleException(AnException: Exception): boolean;
var ErrorSrcTool: TCustomCodeTool;
begin
  fErrorMsg:=AnException.Message;
  fErrorTopLine:=0;
  if (AnException is ELinkScannerError) then begin
    // linker error
    fErrorCode:=TCodeBuffer(ELinkScannerError(AnException).Sender.Code);
    if fErrorCode<>nil then begin
      fErrorCode.AbsoluteToLineCol(
        ELinkScannerError(AnException).Sender.SrcPos,fErrorLine,fErrorColumn);
    end;
  end else if (AnException is ECodeToolError) then begin
    // codetool error
    ErrorSrcTool:=ECodeToolError(AnException).Sender;
    fErrorCode:=ErrorSrcTool.ErrorPosition.Code;
    fErrorColumn:=ErrorSrcTool.ErrorPosition.X;
    fErrorLine:=ErrorSrcTool.ErrorPosition.Y;
  end else begin
    // unknown exception
    FErrorMsg:=AnException.ClassName+': '+FErrorMsg;
    if FCurCodeTool<>nil then begin
      fErrorCode:=FCurCodeTool.ErrorPosition.Code;
      fErrorColumn:=FCurCodeTool.ErrorPosition.X;
      fErrorLine:=FCurCodeTool.ErrorPosition.Y;
    end;
  end;
  // adjust error topline
  if (fErrorCode<>nil) and (fErrorTopLine<1) then begin
    fErrorTopLine:=fErrorLine;
    if (fErrorTopLine>0) and JumpCentered then begin
      dec(fErrorTopLine,VisibleEditorLines div 2);
      if fErrorTopLine<1 then fErrorTopLine:=1;
    end;
  end;
  // write error
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
  // raise or catch
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
    if InitCurCodeTool(Code) then begin
      FCurCodeTool.BuildTree(false);
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
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer;
  var RevertableJump: boolean): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.JumpToMethod A ',Code.Filename,' x=',x,' y=',y);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.JumpToMethod B ',FCurCodeTool.Scanner<>nil);
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindJumpPoint(CursorPos,NewPos,NewTopLine,
                                       RevertableJump);
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
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindDeclaration B ',FCurCodeTool.Scanner<>nil);
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindDeclaration(CursorPos,NewPos,NewTopLine);
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

function TCodeToolManager.FindSmartHint(Code: TCodeBuffer; X, Y: integer
  ): string;
var
  CursorPos: TCodeXYPosition;
begin
  Result:='';
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindSmartHint A ',Code.Filename,' x=',x,' y=',y);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindSmartHint B ',FCurCodeTool.Scanner<>nil);
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindSmartHint(CursorPos);
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindSmartHint END ');
  {$ENDIF}
end;

function TCodeToolManager.GatherIdentifiers(Code: TCodeBuffer; X, Y: integer
  ): boolean;
var
  CursorPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GatherIdentifiers A ',Code.Filename,' x=',x,' y=',y);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GatherIdentifiers B ',FCurCodeTool.Scanner<>nil);
  {$ENDIF}
  try
    Result:=FCurCodeTool.GatherIdentifiers(CursorPos,IdentifierList);
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GatherIdentifiers END ');
  {$ENDIF}
end;

function TCodeToolManager.GuessMisplacedIfdefEndif(Code: TCodeBuffer; X,Y: integer;
  var NewCode: TCodeBuffer;
  var NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GuessMisplacedIfdefEndif A ',Code.Filename,' x=',x,' y=',y);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.GuessMisplacedIfdefEndif(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindEnclosingIncludeDirective(Code: TCodeBuffer; X,
  Y: integer; var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer
  ): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindEnclosingIncludeDirective A ',Code.Filename,' x=',x,' y=',y);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.FindEnclosingIncludeDirective(CursorPos,
                                                       NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.IsKeyword(Code: TCodeBuffer; const KeyWord: string
  ): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.IsKeyword A ',Code.Filename,' Keyword=',KeyWord);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.StringIsKeyWord(KeyWord);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindBlockCounterPart(Code: TCodeBuffer;
  X, Y: integer; var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer
  ): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindBlockCounterPart A ',Code.Filename,' x=',x,' y=',y);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindBlockCounterPart B ',FCurCodeTool.Scanner<>nil);
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindBlockCounterPart(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindBlockCounterPart END ');
  {$ENDIF}
end;

function TCodeToolManager.FindBlockStart(Code: TCodeBuffer;
  X, Y: integer; var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer
  ): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindBlockStart A ',Code.Filename,' x=',x,' y=',y);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindBlockStart B ',FCurCodeTool.Scanner<>nil);
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindBlockStart(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindBlockStart END ');
  {$ENDIF}
end;

function TCodeToolManager.GuessUnclosedBlock(Code: TCodeBuffer; X, Y: integer;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GuessUnclosedBlock A ',Code.Filename,' x=',x,' y=',y);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GuessUnclosedBlock B ',FCurCodeTool.Scanner<>nil);
  {$ENDIF}
  try
    Result:=FCurCodeTool.GuessUnclosedBlock(CursorPos,NewPos,NewTopLine);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GuessUnclosedBlock END ');
  {$ENDIF}
end;

function TCodeToolManager.GetCompatiblePublishedMethods(Code: TCodeBuffer;
  const AClassName: string; TypeData: PTypeData; Proc: TGetStringProc): boolean;
begin
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GetCompatiblePublishedMethods A ',Code.Filename,' Classname=',AClassname);
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.GetCompatiblePublishedMethods(UpperCaseStr(AClassName),
       TypeData,Proc);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.PublishedMethodExists(Code:TCodeBuffer;
  const AClassName, AMethodName: string; TypeData: PTypeData;
  var MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean): boolean;
begin
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.PublishedMethodExists A ',Code.Filename,' ',AClassName,':',AMethodName);
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not Result then exit;
  try
    Result:=FCurCodeTool.PublishedMethodExists(UpperCaseStr(AClassName),
              UpperCaseStr(AMethodName),TypeData,
              MethodIsCompatible,MethodIsPublished,IdentIsMethod);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.JumpToPublishedMethodBody(Code: TCodeBuffer;
  const AClassName, AMethodName: string;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer): boolean;
var NewPos: TCodeXYPosition;
begin
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.JumpToPublishedMethodBody A ',Code.Filename,' ',AClassName,':',AMethodName);
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not Result then exit;
  try
    Result:=FCurCodeTool.JumpToPublishedMethodBody(UpperCaseStr(AClassName),
              UpperCaseStr(AMethodName),NewPos,NewTopLine);
    if Result then begin
      NewCode:=NewPos.Code;
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenamePublishedMethod(Code: TCodeBuffer;
  const AClassName, OldMethodName, NewMethodName: string): boolean;
begin
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.RenamePublishedMethod A');
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not Result then exit;
  try
    SourceChangeCache.Clear;
    Result:=FCurCodeTool.RenamePublishedMethod(UpperCaseStr(AClassName),
              UpperCaseStr(OldMethodName),NewMethodName,
              SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.CreatePublishedMethod(Code: TCodeBuffer;
  const AClassName, NewMethodName: string; ATypeInfo: PTypeInfo): boolean;
begin
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.CreatePublishedMethod A');
  {$ENDIF}
  Result:=InitCurCodeTool(Code);
  if not Result then exit;
  try
    SourceChangeCache.Clear;
    Result:=FCurCodeTool.CreatePublishedMethod(UpperCaseStr(AClassName),
              NewMethodName,ATypeInfo,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.CompleteCode(Code: TCodeBuffer; X,Y,TopLine: integer;
  var NewCode: TCodeBuffer; var NewX, NewY, NewTopLine: integer): boolean;
var
  CursorPos: TCodeXYPosition;
  NewPos: TCodeXYPosition;
begin
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.CompleteCode A ',Code.Filename,' x=',x,' y=',y);
  {$ENDIF}
  Result:=false;
  if not InitCurCodeTool(Code) then exit;
  CursorPos.X:=X;
  CursorPos.Y:=Y;
  CursorPos.Code:=Code;
  try
    Result:=FCurCodeTool.CompleteCode(CursorPos,TopLine,
                                           NewPos,NewTopLine,SourceChangeCache);
    if Result then begin
      NewX:=NewPos.X;
      NewY:=NewPos.Y;
      NewCode:=NewPos.Code;
    end;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.GetSourceName(Code: TCodeBuffer;
  SearchMainCode: boolean): string;
begin
  Result:='';
  if (Code=nil)
  or ((not SearchMainCode) and (Code.LastIncludedByFile<>'')) then exit;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GetSourceName A ',Code.Filename,' ',Code.SourceLength);
  {$ENDIF}
  {$IFDEF MEM_CHECK}
  CheckHeap(IntToStr(GetMem_Cnt));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.GetSourceName;
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

function TCodeToolManager.GetCachedSourceName(Code: TCodeBuffer): string;
begin
  Result:='';
  if (Code=nil)
  or (Code.LastIncludedByFile<>'') then exit;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GetCachedSourceName A ',Code.Filename,' ',Code.SourceLength);
  {$ENDIF}
  {$IFDEF MEM_CHECK}
  CheckHeap(IntToStr(GetMem_Cnt));
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.GetCachedSourceName;
  except
    on e: Exception do HandleException(e);
  end;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GetCachedSourceName B ',Code.Filename,' ',Code.SourceLength);
  {$IFDEF MEM_CHECK}
  CheckHeap(IntToStr(GetMem_Cnt));
  {$ENDIF}
  writeln('SourceName=',Result);
  {$ENDIF}
end;

function TCodeToolManager.GetSourceType(Code: TCodeBuffer;
  SearchMainCode: boolean): string;
begin
  Result:='';
  if (Code=nil)
  or ((not SearchMainCode) and (Code.LastIncludedByFile<>'')) then exit;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.GetSourceType A ',Code.Filename,' ',Code.SourceLength);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    // GetSourceType does not parse the code -> parse it with GetSourceName
    FCurCodeTool.GetSourceName;
    case FCurCodeTool.GetSourceType of
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RenameSource(NewName,SourceChangeCache);
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
  if not InitCurCodeTool(Code) then exit;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindUnitInAllUsesSections B ',Code.Filename,' UnitName=',AnUnitName);
  {$ENDIF}
  try
    Result:=FCurCodeTool.FindUnitInAllUsesSections(UpperCaseStr(AnUnitName),
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RenameUsedUnit(UpperCaseStr(OldUnitName),NewUnitName,
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.AddUnitToMainUsesSection(NewUnitName, NewUnitInFile,
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RemoveUnitFromAllUsesSections(UpperCaseStr(AnUnitName),
                SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.FindUsedUnits(Code: TCodeBuffer; var MainUsesSection,
  ImplementationUsesSection: TStrings): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindUsedUnits A ',Code.Filename);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindUsedUnits(MainUsesSection,
                                       ImplementationUsesSection);
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
  if not InitCurCodeTool(Code) then exit;
  try
    LinkIndex:=-1;
    CurCode:=FCurCodeTool.FindNextIncludeInInitialization(LinkIndex);
    while (CurCode<>nil) do begin
      if UpperCaseStr(ExtractFileExt(CurCode.Filename))='.LRS' then begin
        Result:=CurCode.Filename;
        Ext:=ExtractFileExt(Result);
        Result:=copy(Result,1,length(Result)-length(Ext))+'.lfm';
        exit;
      end;
      CurCode:=FCurCodeTool.FindNextIncludeInInitialization(LinkIndex);
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindNextIncludeInInitialization(LinkIndex);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.AddLazarusResourceHeaderComment(Code: TCodeBuffer;
  const CommentText: string): boolean;
begin
  Result:=false;
  if not InitResourceTool then exit;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.AddLazarusResourceHeaderComment A ',Code.Filename,' CommentText=',CommentText);
  {$ENDIF}
  try
    Result:=GetResourceTool.AddLazarusResourceHeaderComment(Code,
      '{ '+CommentText+' }'+SourceChangeCache.BeautifyCodeOptions.LineEnd
      +SourceChangeCache.BeautifyCodeOptions.LineEnd);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.FindLazarusResource(Code: TCodeBuffer;
  const ResourceName: string): TAtomPosition;
begin
  Result.StartPos:=-1;
  if not InitResourceTool then exit;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.FindLazarusResource A ',Code.Filename,' ResourceName=',ResourceName);
  {$ENDIF}
  try
    Result:=GetResourceTool.FindLazarusResource(Code,ResourceName,-1);
  except
    on e: Exception do HandleException(e);
  end;
end;

function TCodeToolManager.AddLazarusResource(Code: TCodeBuffer;
  const ResourceName, ResourceData: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.AddLazarusResource A ',Code.Filename,' ResourceName=',ResourceName,' ',length(ResourceData));
  {$ENDIF}
  if not InitResourceTool then exit;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.AddLazarusResource B ');
  {$ENDIF}
  try
    Result:=GetResourceTool.AddLazarusResource(Code,ResourceName,ResourceData);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RemoveLazarusResource(Code: TCodeBuffer;
  const ResourceName: string): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.RemoveLazarusResource A ',Code.Filename,' ResourceName=',ResourceName);
  {$ENDIF}
  if not InitResourceTool then exit;
  try
    Result:=GetResourceTool.RemoveLazarusResource(Code,ResourceName);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenameMainInclude(Code: TCodeBuffer;
  const NewFilename: string; KeepPath: boolean): boolean;
var
  LinkIndex: integer;
  OldIgnoreMissingIncludeFiles: boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.RenameMainInclude A ',Code.Filename,' NewFilename=',NewFilename,' KeepPath=',KeepPath);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    OldIgnoreMissingIncludeFiles:=
      FCurCodeTool.Scanner.IgnoreMissingIncludeFiles;
    FCurCodeTool.Scanner.IgnoreMissingIncludeFiles:=true;
    LinkIndex:=-1;
    if FCurCodeTool.FindNextIncludeInInitialization(LinkIndex)=nil then exit;
    Result:=FCurCodeTool.RenameInclude(LinkIndex,NewFilename,KeepPath,
                       SourceChangeCache);
    FCurCodeTool.Scanner.IgnoreMissingIncludeFiles:=
      OldIgnoreMissingIncludeFiles;
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenameIncludeDirective(Code: TCodeBuffer;
  LinkIndex: integer; const NewFilename: string; KeepPath: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.RenameIncludeDirective A ',Code.Filename,' NewFilename=',NewFilename,' KeepPath=',KeepPath);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RenameInclude(LinkIndex,NewFilename,KeepPath,
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindCreateFormStatement(StartPos,UpperCaseStr(AClassName),
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.AddCreateFormStatement(AClassName,AVarName,
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RemoveCreateFormStatement(UpperCaseStr(AVarName),
                    SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.ChangeCreateFormStatement(Code: TCodeBuffer;
  const OldClassName, OldVarName: string; const NewClassName,
  NewVarName: string; OnlyIfExists: boolean): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.ChangeCreateFormStatement A ',Code.Filename,
    ' ',OldVarName.':',OldClassName,' -> ',NewVarName.':',NewClassName,
    ' OnlyIfExists=',OnlyIfExists);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.ChangeCreateFormStatement(-1,OldClassName,OldVarName,
                    NewClassName,NewVarName,true,
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.ListAllCreateFormStatements;
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.SetAllCreateFromStatements(List,SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenameForm(Code: TCodeBuffer; const OldFormName,
  OldFormClassName: string; const NewFormName, NewFormClassName: string
  ): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.RenameForm A ',Code.Filename,
    ' OldFormName=',OldFormName,' OldFormClassName=',OldFormClassName,
    ' NewFormName=',NewFormName,' NewFormClassName=',NewFormClassName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RenameForm(OldFormName,OldFormClassName,
                                NewFormName,NewFormClassName,SourceChangeCache);
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.FindPublishedVariable(UpperCaseStr(AClassName),
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.AddPublishedVariable(UpperCaseStr(AClassName),
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
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RemovePublishedVariable(UpperCaseStr(AClassName),
               UpperCaseStr(AVarName),SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.RenamePublishedVariable(Code: TCodeBuffer;
  const AClassName, OldVariableName, NewVarName, VarType: shortstring): boolean;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  writeln('TCodeToolManager.RenamePublishedVariable A ',Code.Filename,' ',AClassName,' OldVar=',OldVarName,' NewVar=',NewVarName);
  {$ENDIF}
  if not InitCurCodeTool(Code) then exit;
  try
    Result:=FCurCodeTool.RenamePublishedVariable(UpperCaseStr(AClassName),
               UpperCaseStr(OldVariableName),NewVarName,VarType,
               SourceChangeCache);
  except
    on e: Exception do Result:=HandleException(e);
  end;
end;

function TCodeToolManager.DoOnFindUsedUnit(SrcTool: TFindDeclarationTool;
  const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
begin
  if Assigned(OnSearchUsedUnit) then
    Result:=OnSearchUsedUnit(SrcTool.MainFilename,
                             TheUnitName,TheUnitInFilename)
  else
    Result:=nil;
end;

function TCodeToolManager.OnParserProgress(Tool: TCustomCodeTool): boolean;
begin
  Result:=true;
  if not FAbortable then exit;
  if not Assigned(OnCheckAbort) then exit;
  Result:=not OnCheckAbort();
end;

function TCodeToolManager.OnScannerProgress(Sender: TLinkScanner): boolean;
begin
  Result:=true;
  if not FAbortable then exit;
  if not Assigned(OnCheckAbort) then exit;
  Result:=not OnCheckAbort();
end;

function TCodeToolManager.OnScannerGetInitValues(Code: Pointer;
  var AChangeStep: integer): TExpressionEvaluator;
begin
  Result:=nil;
  AChangeStep:=DefineTree.ChangeStep;
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
  if FCurCodeTool<>nil then
    FCurCodeTool.CheckFilesOnDisk:=NewValue;
end;

procedure TCodeToolManager.SetCompleteProperties(const AValue: boolean);
begin
  if CompleteProperties=AValue then exit;
  FCompleteProperties:=AValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.CompleteProperties:=AValue;
end;

procedure TCodeToolManager.SetIndentSize(NewValue: integer);
begin
  if NewValue=FIndentSize then exit;
  FIndentSize:=NewValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.IndentSize:=NewValue;
end;

procedure TCodeToolManager.SetVisibleEditorLines(NewValue: integer);
begin
  if NewValue=FVisibleEditorLines then exit;
  FVisibleEditorLines:=NewValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.VisibleEditorLines:=NewValue;
end;

procedure TCodeToolManager.SetJumpCentered(NewValue: boolean);
begin
  if NewValue=FJumpCentered then exit;
  FJumpCentered:=NewValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.JumpCentered:=NewValue;
end;

procedure TCodeToolManager.SetCursorBeyondEOL(NewValue: boolean);
begin
  if NewValue=FCursorBeyondEOL then exit;
  FCursorBeyondEOL:=NewValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.CursorBeyondEOL:=NewValue;
end;

procedure TCodeToolManager.BeforeApplyingChanges(var Abort: boolean);
begin
  if Assigned(FOnBeforeApplyChanges) then
    FOnBeforeApplyChanges(Self,Abort);
end;

procedure TCodeToolManager.AfterApplyingChanges;
begin
  // clear all codetrees of changed buffers
  if FCurCodeTool<>nil then
    FCurCodeTool.Clear;
    
  // user callback
  if Assigned(FOnAfterApplyChanges) then
    FOnAfterApplyChanges(Self);
end;

function TCodeToolManager.FindCodeToolForSource(Code: TCodeBuffer
  ): TCustomCodeTool;
var ANode: TAVLTreeNode;
  CurSrc, SearchedSrc: integer;
begin
  ANode:=FSourceTools.Root;
  SearchedSrc:=integer(Code);
  while (ANode<>nil) do begin
    CurSrc:=integer(TCustomCodeTool(ANode.Data).Scanner.MainCode);
    if CurSrc>SearchedSrc then
      ANode:=ANode.Left
    else if CurSrc<SearchedSrc then
      ANode:=ANode.Right
    else begin
      Result:=TCustomCodeTool(ANode.Data);
      exit;
    end;
  end;
  Result:=nil;
end;

function TCodeToolManager.GetCodeToolForSource(Code: TCodeBuffer;
  ExceptionOnError: boolean): TCustomCodeTool;
// return a codetool for the source
begin
  Result:=nil;
  if Code=nil then begin
    if ExceptionOnError then
      raise Exception.Create('TCodeToolManager.GetCodeToolForSource '
        +'internal error: Code=nil');
    exit;
  end;
  Result:=FindCodeToolForSource(Code);
  if Result=nil then begin
    CreateScanner(Code);
    if Code.Scanner=nil then begin
      if ExceptionOnError then
        raise Exception.CreateFmt(ctsNoScannerFound,[Code.Filename]);
      exit;
    end;
    Result:=TCodeTool.Create;
    Result.Scanner:=Code.Scanner;
    FSourceTools.Add(Result);
  end;
  with TCodeTool(Result) do begin
    AdjustTopLineDueToComment:=Self.AdjustTopLineDueToComment;
    AddInheritedCodeToOverrideMethod:=Self.AddInheritedCodeToOverrideMethod;
    CompleteProperties:=Self.CompleteProperties;
  end;
  Result.CheckFilesOnDisk:=FCheckFilesOnDisk;
  Result.IndentSize:=FIndentSize;
  Result.VisibleEditorLines:=FVisibleEditorLines;
  Result.JumpCentered:=FJumpCentered;
  Result.CursorBeyondEOL:=FCursorBeyondEOL;
  TFindDeclarationTool(Result).OnGetCodeToolForBuffer:=@OnGetCodeToolForBuffer;
  TFindDeclarationTool(Result).OnFindUsedUnit:=@DoOnFindUsedUnit;
  Result.OnSetGlobalWriteLock:=@OnToolSetWriteLock;
  Result.OnGetGlobalWriteLockInfo:=@OnToolGetWriteLockInfo;
  TFindDeclarationTool(Result).OnParserProgress:=@OnParserProgress;
end;

procedure TCodeToolManager.SetAbortable(const AValue: boolean);
begin
  if FAbortable=AValue then exit;
  FAbortable:=AValue;
end;

procedure TCodeToolManager.SetAddInheritedCodeToOverrideMethod(
  const AValue: boolean);
begin
  if FAddInheritedCodeToOverrideMethod=AValue then exit;
  FAddInheritedCodeToOverrideMethod:=AValue;
  if FCurCodeTool<>nil then
    FCurCodeTool.AddInheritedCodeToOverrideMethod:=AValue;
end;

function TCodeToolManager.OnGetCodeToolForBuffer(Sender: TObject;
  Code: TCodeBuffer): TFindDeclarationTool;
begin
  {$IFDEF CTDEBUG}
  writeln('[TCodeToolManager.OnGetCodeToolForBuffer]'
    ,' Sender=',TCustomCodeTool(Sender).MainFilename
    ,' Code=',Code.Filename);
  {$ENDIF}
  Result:=TFindDeclarationTool(GetCodeToolForSource(Code,true));
end;

procedure TCodeToolManager.ActivateWriteLock;
begin
  if FWriteLockCount=0 then begin
    // start a new write lock
    if FWriteLockStep<>$7fffffff then
      inc(FWriteLockStep)
    else
      FWriteLockStep:=-$7fffffff;
    SourceCache.GlobalWriteLockIsSet:=true;
    SourceCache.GlobalWriteLockStep:=FWriteLockStep;
  end;
  inc(FWriteLockCount);
  {$IFDEF CTDEBUG}
  writeln('[TCodeToolManager.ActivateWriteLock] FWriteLockCount=',FWriteLockCount,' FWriteLockStep=',FWriteLockStep);
  {$ENDIF}
end;

procedure TCodeToolManager.DeactivateWriteLock;
begin
  if FWriteLockCount>0 then begin
    dec(FWriteLockCount);
    if FWriteLockCount=0 then begin
      // end the write lock
      SourceCache.GlobalWriteLockIsSet:=false;
    end;
  end;
  {$IFDEF CTDEBUG}
  writeln('[TCodeToolManager.DeactivateWriteLock] FWriteLockCount=',FWriteLockCount,' FWriteLockStep=',FWriteLockStep);
  {$ENDIF}
end;

procedure TCodeToolManager.OnToolGetWriteLockInfo(var WriteLockIsSet: boolean;
  var WriteLockStep: integer);
begin
  WriteLockIsSet:=FWriteLockCount>0;
  WriteLockStep:=FWriteLockStep;
//writeln(' FWriteLockCount=',FWriteLockCount,' FWriteLockStep=',FWriteLockStep);
end;

function TCodeToolManager.GetResourceTool: TResourceCodeTool;
begin
  if FResourceTool=nil then FResourceTool:=TResourceCodeTool.Create;
  Result:=FResourceTool;
end;

function TCodeToolManager.GetOwnerForCodeTreeNode(ANode: TCodeTreeNode
  ): TObject;
var
  AToolNode: TAVLTreeNode;
  CurTool: TCustomCodeTool;
  RootCodeTreeNode: TCodeTreeNode;
begin
  Result:=nil;
  if ANode=nil then exit;
  RootCodeTreeNode:=ANode.GetRoot;
  AToolNode:=FSourceTools.FindLowest;
  while (AToolNode<>nil) do begin
    CurTool:=TCustomCodeTool(AToolNode.Data);
    if CurTool.Tree.Root=RootCodeTreeNode then begin
      Result:=CurTool;
      exit;
    end;
    AToolNode:=FSourceTools.FindSuccessor(AToolNode);
  end;
end;

procedure TCodeToolManager.OnToolSetWriteLock(Lock: boolean);
begin
  if Lock then ActivateWriteLock else DeactivateWriteLock;
end;

function TCodeToolManager.ConsistencyCheck: integer;
// 0 = ok
begin
  try
    Result:=0;
    if FCurCodeTool<>nil then begin
      Result:=FCurCodeTool.ConsistencyCheck;
      if Result<>0 then begin
        dec(Result,10000);  exit;
      end;
    end;
    Result:=DefinePool.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,20000);  exit;
    end;
    Result:=DefineTree.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,30000);  exit;
    end;
    Result:=SourceCache.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,40000);  exit;
    end;
    Result:=GlobalValues.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,50000);  exit;
    end;
    Result:=SourceChangeCache.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,60000);  exit;
    end;
    Result:=FSourceTools.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,70000);  exit;
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
  if FCurCodeTool<>nil then begin
    if WriteTool then
      FCurCodeTool.WriteDebugTreeReport
    else
      writeln('  FCurCodeTool.ConsistencyCheck=',FCurCodeTool.ConsistencyCheck);
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
  OnFindOwnerOfCodeTreeNode:=@GetOwnerForCodeTreeNode;


finalization
  {$IFDEF CTDEBUG}
  writeln('codetoolmanager.pas - finalization');
  {$ENDIF}
  OnFindOwnerOfCodeTreeNode:=nil;
  CodeToolBoss.Free;
  CodeToolBoss:=nil;
  {$IFDEF CTDEBUG}
  writeln('codetoolmanager.pas - finalization finished');
  {$ENDIF}

end.

