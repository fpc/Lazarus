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
    This unit is a support unit for the code tools. It manages compilation
    information which is not stored in the source, like Makefile information and
    compiler command line options. This information is needed to successfully
    find the right units, include files, predefined variables, etc..
    
    The information is stored in a TDefineTree, which contains nodes of type
    TDefineTemplate. Each TDefineTemplate is a tree of defines, undefines,
    definealls, ifdefs, ifndefs, elses, elseifs and directories.
    
    Simply give a TDefineTree a directory and it will return all predefined
    variables for that directory. These values can be used to parse a unit in
    the directory.
    
    TDefineTree can be saved to and loaded from a XML file.
    
    The TDefinePool contains a list of TDefineTemplate trees, and can generate
    some default templates for Lazarus and FPC sources.
    
  ToDo:
    Error handling for DefinePool
}
unit DefineTemplates;

{$ifdef FPC} {$mode objfpc} {$endif}{$H+}

interface

uses
  Classes, SysUtils, ExprEval{$ifdef FPC}, XMLCfg{$endif}, AVL_Tree, Process,
  KeywordFuncLists;

const
  ExternalMacroStart: char = '#';
  {$ifdef win32}
  SpecialChar: char = '/';
  {$else}
  SpecialChar: char = '\';
  {$endif}
  {$ifdef win32}
  {$define CaseInsensitiveFilenames}
  {$endif}
  
  // Standard Template Names
  StdDefTemplFPC = 'Free Pascal Compiler';
  StdDefTemplFPCSrc = 'Free Pascal Sources';
  StdDefTemplLazarusSources = 'Lazarus Sources';
  StdDefTemplLCLProject = 'LCL Project';

  // FPC operating systems and processor types
  FPCOperatingSystemNames: array[1..11] of shortstring =(
      'linux', 'freebsd', 'win32', 'go32v1', 'go32v2', 'beos', 'os2', 'amiga',
      'atari', 'sunos', 'palmos'
    );
  FPCOperatingSystemAlternativeNames: array[1..1] of shortstring =(
      'unix'
    );
  FPCProcessorNames: array[1..3] of shortstring =(
      'i386', 'powerpc', 'm68k'
    );

type
  //---------------------------------------------------------------------------
  // TDefineTemplate is a list of TDefineEntry
  // TDefineEntry stores a define action, the variablename and the value
  TDefineAction = (da_None, da_Block, da_Define, da_Undefine, da_DefineAll,
     da_If, da_IfDef, da_IfNDef, da_ElseIf, da_Else, da_Directory);

  TDefineTemplate = class
  private
    FChildCount: integer;
    FParent: TDefineTemplate;
    FNext: TDefineTemplate;
    FPrior: TDefineTemplate;
    FFirstChild: TDefineTemplate;
    FLastChild: TDefineTemplate;
  public
    Name: string;
    Description: string;
    Variable: string;
    Value: string;
    Action: TDefineAction;
    property ChildCount: integer read FChildCount;
    property Parent: TDefineTemplate read FParent;
    property Next: TDefineTemplate read FNext;
    property Prior: TDefineTemplate read FPrior;
    property FirstChild: TDefineTemplate read FFirstChild;
    property LastChild: TDefineTemplate read FLastChild;
    procedure AddChild(ADefineTemplate: TDefineTemplate);
    procedure InsertAfter(APrior: TDefineTemplate);
    procedure Assign(ADefineTemplate: TDefineTemplate); virtual;
    function LoadFromXMLConfig(XMLConfig: TXMLConfig;
        const Path: string): boolean;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    function CreateCopy: TDefineTemplate;
    procedure Clear;
    constructor Create;
    constructor Create(const AName, ADescription, AVariable, AValue: string;
        AnAction: TDefineAction);
    destructor Destroy; override;
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport;
  end;

  //---------------------------------------------------------------------------
  //
  TDirectoryDefines = class
  public
    Path: string;
    Values: TExpressionEvaluator;
    constructor Create;
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // TDefineTree caches the define values for directories
  TOnReadValue = procedure(Sender: TObject; const VariableName: string;
                          var Value: string) of object;
  
  TDefineTree = class
  private
    FFirstDefineTemplate: TDefineTemplate;
    FCache: TAVLTree; // tree of TDirectoryDefines
    FOnReadValue: TOnReadValue;
    FErrorTemplate: TDefineTemplate;
    FErrorDescription: string;
    function FindDirectoryInCache(const Path: string): TDirectoryDefines;
    function Calculate(DirDef: TDirectoryDefines): boolean;
  public
    function GetDefinesForDirectory(const Path: string): TExpressionEvaluator;
    property RootTemplate: TDefineTemplate
        read FFirstDefineTemplate write FFirstDefineTemplate;
    procedure Add(ADefineTemplate: TDefineTemplate);
    property OnReadValue: TOnReadValue read FOnReadValue write FOnReadValue;
    property ErrorTemplate: TDefineTemplate read FErrorTemplate;
    property ErrorDescription: string read FErrorDescription;
    function LoadFromXMLConfig(XMLConfig: TXMLConfig;
        const Path: string): boolean;
    function SaveToXMLConfig(XMLConfig: TXMLConfig;
        const Path: string): boolean;
    procedure ClearCache;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport;
  end;

  //---------------------------------------------------------------------------
  TDefinePool = class
  private
    FItems: TList; // list of TDefineTemplate;
    function GetItems(Index: integer): TDefineTemplate;
  public
    property Items[Index: integer]: TDefineTemplate read GetItems; default;
    function Count: integer;
    procedure Add(ADefineTemplate: TDefineTemplate);
    procedure Insert(Index: integer; ADefineTemplate: TDefineTemplate);
    procedure Delete(Index: integer);
    procedure Move(SrcIndex, DestIndex: integer);
    function CreateFPCTemplate(const PPC386Path: string;
        var UnitSearchPath: string): TDefineTemplate;
    function CreateFPCSrcTemplate(const FPCSrcDir,
        UnitSearchPath: string): TDefineTemplate;
    function CreateLCLProjectTemplate(const LazarusSrcDir, WidgetType,
        ProjectDir: string): TDefineTemplate;
    function CreateLazarusSrcTemplate(
        const LazarusSrcDir, WidgetType: string): TDefineTemplate;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function ConsistencyCheck: integer; // 0 = ok
    procedure WriteDebugReport;
  end;
  

implementation


type
  TUnitNameLink = class
  public
    UnitName: string;
    Filename: string;
  end;


// some useful functions

function CompareFilenames(const FileName1, Filename2: string): integer;
begin
  {$ifdef CaseInsensitiveFilenames}
  Result:=AnsiCompareText(FileName1,Filename2);
  {$else}
  Result:=AnsiCompareStr(FileName1,Filename2);
  {$endif}
end;

function CompareUnitLinkNodes(NodeData1, NodeData2: pointer): integer;
var Link1, Link2: TUnitNameLink;
begin
  Link1:=TUnitNameLink(NodeData1);
  Link2:=TUnitNameLink(NodeData2);
  Result:=AnsiCompareText(Link1.UnitName,Link2.UnitName);
end;

function CompareDirectoryDefines(NodeData1, NodeData2: pointer): integer;
var DirDef1, DirDef2: TDirectoryDefines;
begin
  DirDef1:=TDirectoryDefines(NodeData1);
  DirDef2:=TDirectoryDefines(NodeData2);
  Result:=CompareFilenames(DirDef1.Path,DirDef2.Path);
end;

function FilenameIsMatching(const Mask, Filename: string;
  MatchExactly: boolean): boolean;
{
  check if Filename matches Mask
  Filename matches exactly or is a file/directory in a subdirectory of mask
  Mask can contain the wildcards * and ?
  The wildcards will _not_ match OSDirSeparator
  If you need the asterisk, the question mark or the OSDirSeparator as character
  just put the SpecialChar character in front of it.

  Examples:
    /abc          matches /abc, /abc/p, /abc/xyz/filename
                  but not /abcd
    /abc/x?z/www  matches /abc/xyz/www, /abc/xaz/www
                  but not /abc/x/z/www
    /abc/x*z/www  matches /abc/xz/www, /abc/xyz/www, /abc/xAAAz/www
                  but not /abc/x/z/www
    /abc/x\*z/www matches /abc/x*z/www, /abc/x*z/www/ttt
}
var DirStartMask, DirEndMask, DirStartFile, DirEndFile, AsteriskPos: integer;
begin
//writeln('[FilenameIsMatching] Mask="',Mask,'" Filename="',Filename,'" MatchExactly=',MatchExactly);
  Result:=false;
  if (Filename='') then exit;
  if (Mask='') then begin
    Result:=true;  exit;
  end;
  // test every directory
  DirStartMask:=1;
  DirStartFile:=1;
  repeat
    // find start of directories
    while (DirStartMask<=length(Mask))
    and (Mask[DirStartMask]=OSDirSeparator) do
      inc(DirStartMask);
    while (DirStartFile<=length(Filename))
    and (Filename[DirStartFile]=OSDirSeparator) do
      inc(DirStartFile);
    // find ends of directories
    DirEndMask:=DirStartMask;
    DirEndFile:=DirStartFile;
    while (DirEndMask<=length(Mask)) do begin
      if Mask[DirEndMask]=SpecialChar then
        inc(DirEndMask,2)
      else if (Mask[DirEndMask]=OSDirSeparator) then
        break
      else
        inc(DirEndMask);
    end;
    while (DirEndFile<=length(Filename)) do begin
      if Filename[DirEndFile]=SpecialChar then
        inc(DirEndFile,2)
      else if (Filename[DirEndFile]=OSDirSeparator) then
        break
      else
        inc(DirEndFile);
    end;
// writeln('  Compare "',copy(Mask,DirStartMask,DirEndMask-DirStartMask),'"',
//   ' "',copy(Filename,DirStartFile,DirEndFile-DirStartFile),'"');
    // compare directories
    AsteriskPos:=0;
    while (DirStartMask<DirEndMask) and (DirStartFile<DirEndFile) do begin
      case Mask[DirStartMask] of
      '?':
        begin
          inc(DirStartMask);
          inc(DirStartFile);
        end;
      '*':
        begin
          inc(DirStartMask);
          AsteriskPos:=DirStartMask;
        end;
      else
        begin
          if Mask[DirStartMask]=SpecialChar then begin
            inc(DirStartMask);
            if (DirStartMask>length(Mask)) then exit;
          end;
          {$ifdef CaseInsensitiveFilenames}
          if (UpChars[Mask[DirStartMask]]<>UpChars[Filename[DirStartFile]]) then
          {$else}
          if (Mask[DirStartMask]<>Filename[DirStartFile]) then
          {$endif}
          begin
            if AsteriskPos=0 then exit;
            DirStartMask:=AsteriskPos;
          end else begin
            inc(DirStartMask);
            inc(DirStartFile);
          end;
        end;
      end;
    end;
    if (DirStartMask<DirEndmask) or (DirStartFile<DirEndFile) then exit;
    // find starts of next directorys
    DirStartMask:=DirEndMask+1;
    DirStartFile:=DirEndFile+1;
  until (DirStartFile>length(Filename)) or (DirStartMask>length(Mask));
  while (DirStartMask<=length(Mask))
  and (Mask[DirStartMask]=OSDirSeparator) do
    inc(DirStartMask);
  Result:=(DirStartMask>length(Mask));
  if MatchExactly then begin
    while (DirStartFile<=length(Filename))
    and (Filename[DirStartFile]=OSDirSeparator) do
      inc(DirStartFile);
    Result:=(Result and (DirStartFile>length(Filename)));
  end;
//writeln('  [FilenameIsMatching] Result=',Result,' ',DirStartMask,',',length(Mask),'  ',DirStartFile,',',length(Filename));
end;


{ TDefineTemplate }

procedure TDefineTemplate.AddChild(ADefineTemplate: TDefineTemplate);
// add as last child
begin
  if ADefineTemplate=nil then exit;
  ADefineTemplate.FPrior:=FLastChild;
  FLastChild:=ADefineTemplate;
  if FFirstChild=nil then FFirstChild:=ADefineTemplate;
  if ADefineTemplate.FPrior<>nil then
    ADefineTemplate.FPrior.FNext:=ADefineTemplate;
  while ADefineTemplate<>nil do begin
    ADefineTemplate.FParent:=Self;
    inc(FChildCount);
    ADefineTemplate:=ADefineTemplate.FNext;
  end;
end;

procedure TDefineTemplate.InsertAfter(APrior: TDefineTemplate);
begin
  if APrior=nil then exit;
  FPrior:=APrior;
  FNext:=APrior.Next;
  APrior.FNext:=Self;
  if FNext<>nil then FNext.FPrior:=Self;
  FParent:=APrior.FParent;
end;

procedure TDefineTemplate.Assign(ADefineTemplate: TDefineTemplate);
var ChildTemplate, CopyTemplate: TDefineTemplate;
begin
  Clear;
  if ADefineTemplate=nil then exit;
  ChildTemplate:=ADefineTemplate.FirstChild;
  while ChildTemplate<>nil do begin
    CopyTemplate:=ChildTemplate.CreateCopy;
    AddChild(CopyTemplate);
    ChildTemplate:=ChildTemplate.Next;
  end;
  Name:=ADefineTemplate.Name;
  Description:=ADefineTemplate.Description;
  Variable:=ADefineTemplate.Variable;
  Value:=ADefineTemplate.Value;
  Action:=ADefineTemplate.Action;
end;

procedure TDefineTemplate.Clear;
begin
  while FFirstChild<>nil do FFirstChild.Free;
  while FNext<>nil do FNext.Free;
  Name:='';
  Description:='';
  Value:='';
  Variable:='';
end;

constructor TDefineTemplate.Create;
begin
  inherited Create;
end;

constructor TDefineTemplate.Create(const AName, ADescription, AVariable,
  AValue: string; AnAction: TDefineAction);
begin
  inherited Create;
  Name:=AName;
  Description:=ADescription;
  Variable:=AVariable;
  Value:=AValue;
  Action:=AnAction;
end;

function TDefineTemplate.CreateCopy: TDefineTemplate;
begin
  Result:=TDefineTemplate.Create;
  Result.Assign(Self);
end;

destructor TDefineTemplate.Destroy;
begin
  Clear;
  if FPrior<>nil then FPrior.FNext:=FNext;
  if FNext<>nil then FNext.FPrior:=FPrior;
  if FParent<>nil then begin
    if FParent.FFirstChild=Self then FParent.FFirstChild:=FNext;
    if FParent.FLastChild=Self then FParent.FLastChild:=FPrior;
    dec(FParent.FChildCount);
  end;
  FNext:=nil;
  FPrior:=nil;
  inherited Destroy;
end;

function TDefineTemplate.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string): boolean;
var ActionStr, IndexedPath: string;
  i, LvlCount: integer;
  DefTempl, LastDefTempl: TDefineTemplate;
begin
  Clear;
  LvlCount:=XMLConfig.GetValue(Path+'/Count/Value',0);
  DefTempl:=nil;
  for i:=0 to LvlCount-1 do begin
    if i=0 then begin
      LastDefTempl:=nil;
      DefTempl:=Self
    end else begin
      LastDefTempl:=DefTempl;
      DefTempl:=TDefineTemplate.Create;
      DefTempl.FPrior:=LastDefTempl;
      DefTempl.FParent:=LastDefTempl.Parent;
      if DefTempl.FParent<>nil then begin
        DefTempl.FParent.FLastChild:=DefTempl;
        inc(DefTempl.FParent.FChildCount);
      end;
    end;
    IndexedPath:=Path+'/'+IntToStr(i);
    DefTempl.Name:=XMLConfig.GetValue(IndexedPath+'/Name/Value','no name');
    DefTempl.Description:=XMLConfig.GetValue(IndexedPath+'/Description/Value','');
    DefTempl.Value:=XMLConfig.GetValue(IndexedPath+'/Value/Value','');
    DefTempl.Variable:=XMLConfig.GetValue(IndexedPath+'/Variable/Value','');
    ActionStr:=UpperCaseStr(XMLConfig.GetValue(IndexedPath+'/Action/Value',''));
    if ActionStr='BLOCK' then
      Action:=da_Block
    else if ActionStr='DEFINE' then
      Action:=da_Define
    else if ActionStr='UNDEFINE' then
      Action:=da_Undefine
    else if ActionStr='DEFINEALL' then
      Action:=da_DefineAll
    else if ActionStr='IF' then
      Action:=da_If
    else if ActionStr='IFDEF' then
      Action:=da_IfDef
    else if ActionStr='IFNDEF' then
      Action:=da_IfNDef
    else if ActionStr='ELSEIF' then
      Action:=da_ElseIf
    else if ActionStr='ELSE' then
      Action:=da_Else
    else if ActionStr='DIRECTORY' then
      Action:=da_Directory
    else
      Action:=da_None;
    // load childs
    if XMLConfig.GetValue(IndexedPath+'/Count/Value',0)>0 then begin
      FFirstChild:=TDefineTemplate.Create;
      if not FFirstChild.LoadFromXMLConfig(XMLConfig,IndexedPath) then begin
        Result:=false;  exit;
      end;
    end;
  end;
  Result:=true;
end;

procedure TDefineTemplate.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var ActionStr, IndexedPath: string;
  Index, LvlCount: integer;
  DefTempl: TDefineTemplate;
begin
  DefTempl:=Self;
  LvlCount:=0;
  while DefTempl<>nil do begin
    inc(LvlCount);
    DefTempl:=DefTempl.Next;
  end;
  XMLConfig.SetValue(Path+'/Count/Value',LvlCount);
  DefTempl:=Self;
  Index:=1;
  repeat
    IndexedPath:=Path+'/'+IntToStr(Index);
    XMLConfig.SetValue(IndexedPath+'/Name/Value',DefTempl.Name);
    XMLConfig.SetValue(IndexedPath+'/Description/Value',DefTempl.Description);
    XMLConfig.SetValue(IndexedPath+'/Value/Value',DefTempl.Value);
    XMLConfig.SetValue(IndexedPath+'/Variable/Value',DefTempl.Variable);
    case DefTempl.Action of
      da_Block     : ActionStr:='Block';
      da_Define    : ActionStr:='Define';
      da_Undefine  : ActionStr:='Undefine';
      da_DefineAll : ActionStr:='DefineAll';
      da_If        : ActionStr:='If';
      da_IfDef     : ActionStr:='IfDef';
      da_IfNDef    : ActionStr:='IfNDef';
      da_ElseIf    : ActionStr:='ElseIf';
      da_Else      : ActionStr:='Else';
      da_Directory : ActionStr:='Directory';
    else
      ActionStr:='None';
    end;
    XMLConfig.SetValue(IndexedPath+'/Action/Value',ActionStr);
    // save childs
    if FFirstChild<>nil then
      FirstChild.SaveToXMLConfig(XMLConfig,IndexedPath);
    inc(Index);
    DefTempl:=DefTempl.Next;
  until DefTempl=nil;
end;

function TDefineTemplate.ConsistencyCheck: integer;
var RealChildCount: integer;
  DefTempl: TDefineTemplate;
begin
  RealChildCount:=0;
  DefTempl:=FFirstChild;
  if DefTempl<>nil then begin
    if DefTempl.Prior<>nil then begin
      // not first child
      Result:=-2;  exit;
    end;
    while DefTempl<>nil do begin
      if DefTempl.Parent<>Self then begin
      writeln('  C: ',Name,',',DefTempl.Name);
        Result:=-3;  exit;
      end;
      if (DefTempl.Next<>nil) and (DefTempl.Next.Prior<>DefTempl) then begin
        Result:=-4;  exit;
      end;
      if (DefTempl.Prior<>nil) and (DefTempl.Prior.Next<>DefTempl) then begin
        Result:=-5;  exit;
      end;
      Result:=DefTempl.ConsistencyCheck;
      if Result<>0 then begin
        dec(Result,100);  exit;
      end;
      DefTempl:=DefTempl.Next;
      inc(RealChildCount);
    end;
  end;
  if RealChildCount<>FChildCount then begin
    Result:=-1;  exit;
  end;
  Result:=0;
end;

procedure TDefineTemplate.WriteDebugReport;

  procedure WriteNode(ANode: TDefineTemplate; const Prefix: string);
  var ActionStr: string;
  begin
    if ANode=nil then exit;
    case ANode.Action of
      da_Block     : ActionStr:='Block';
      da_Define    : ActionStr:='Define';
      da_Undefine  : ActionStr:='Undefine';
      da_DefineAll : ActionStr:='DefineAll';
      da_If        : ActionStr:='If';
      da_IfDef     : ActionStr:='IfDef';
      da_IfNDef    : ActionStr:='IfNDef';
      da_ElseIf    : ActionStr:='ElseIf';
      da_Else      : ActionStr:='Else';
      da_Directory : ActionStr:='Directory';
    else
      ActionStr:='None';
    end;
    writeln(Prefix,'Self=',HexStr(Cardinal(ANode),8),
      ' Consistency=',ANode.ConsistencyCheck,
      ' Next=',HexStr(Cardinal(ANode.Next),8),
      ' Prior=',HexStr(Cardinal(ANode.Prior),8),
      ' Action=',ActionStr,
      ' Name="',ANode.Name,'"');
    writeln(Prefix+'   + Description="',ANode.Description,'"');
    writeln(Prefix+'   + Variable="',ANode.Variable,'"');
    writeln(Prefix+'   + Value="',ANode.Value,'"');
    WriteNode(ANode.FFirstChild,Prefix+'  ');
    WriteNode(ANode.Next,Prefix);
  end;

begin
  WriteNode(Self,'  ');
end;


{ TDirectoryDefines }

constructor TDirectoryDefines.Create;
begin
  inherited Create;
  Values:=TExpressionEvaluator.Create;
  Path:='';
end;

destructor TDirectoryDefines.Destroy;
begin
  Values.Free;
  inherited Destroy;
end;


{ TDefineTree }

procedure TDefineTree.Clear;
begin
  FFirstDefineTemplate.Free;
  FFirstDefineTemplate:=nil;
  ClearCache;
end;

procedure TDefineTree.ClearCache;
begin
  FCache.FreeAndClear;
end;

constructor TDefineTree.Create;
begin
  inherited Create;
  FFirstDefineTemplate:=nil;
  FCache:=TAVLTree.Create(@CompareDirectoryDefines);
end;

destructor TDefineTree.Destroy;
begin
  Clear;
  FCache.Free;
  inherited Destroy;
end;

function TDefineTree.FindDirectoryInCache(
  const Path: string): TDirectoryDefines;
var cmp: integer;
  ANode: TAVLTreeNode;
begin
  ANode:=FCache.Root;
  while (ANode<>nil) do begin
    cmp:=CompareFilenames(Path,TDirectoryDefines(ANode.Data).Path);
    if cmp<0 then
      ANode:=ANode.Left
    else if cmp>0 then
      ANode:=ANode.Right
    else
      break;
  end;
  if ANode<>nil then
    Result:=TDirectoryDefines(ANode.Data)
  else
    Result:=nil;
end;

function TDefineTree.GetDefinesForDirectory(
  const Path: string): TExpressionEvaluator;
var ExpPath: string;
  DirDef: TDirectoryDefines;
begin
//writeln('[TDefineTree.GetDefinesForDirectory] "',Path,'"');
  ExpPath:=Path;
  if (ExpPath<>'') and (ExpPath[length(ExpPath)]<>OSDirSeparator) then
    ExpPath:=ExpPath+OSDirSeparator;
  DirDef:=FindDirectoryInCache(ExpPath);
  if DirDef<>nil then begin
    Result:=DirDef.Values;
  end else begin
    DirDef:=TDirectoryDefines.Create;
    DirDef.Path:=ExpPath;
//writeln('[TDefineTree.GetDefinesForDirectory] B ',ExpPath,' ');
    if Calculate(DirDef) then begin
      FCache.Add(DirDef);
      Result:=DirDef.Values;
    end else begin
      DirDef.Free;
      Result:=nil;
    end;
  end;
end;

function TDefineTree.Calculate(DirDef: TDirectoryDefines): boolean;
// calculates the values for a single directory
// returns false on error
var
  ExpandedDirectory, EvalResult: string;

  function ReadValue(const PreValue: string): string;
  // replace variables of the form $() and functions of the form $name()
  // replace backslash characters

    function SearchBracketClose(const s: string; Position:integer): integer;
    var BracketClose:char;
    begin
      if s[Position]='(' then BracketClose:=')'
      else BracketClose:='{';
      inc(Position);
      while (Position<=length(s)) and (s[Position]<>BracketClose) do begin
        if s[Position]='\' then
          inc(Position)
        else if (s[Position] in ['(','{']) then
          Position:=SearchBracketClose(s,Position);
        inc(Position);
      end;
      Result:=Position;
    end;
    
    function ExecuteMacroFunction(const FuncName, Params: string): string;
    var UpFuncName, Ext: string;
    begin
      UpFuncName:=UpperCaseStr(FuncName);
      if UpFuncName='EXT' then begin
        Result:=ExtractFileExt(Params);
      end else if UpFuncName='PATH' then begin
        Result:=ExtractFilePath(Params);
      end else if UpFuncName='NAME' then begin
        Result:=ExtractFileName(Params);
      end else if UpFuncName='NAMEONLY' then begin
        Result:=ExtractFileName(Params);
        Ext:=ExtractFileExt(Result);
        Result:=copy(Result,1,length(Result)-length(Ext));
      end else
        Result:='<Unknown function '+FuncName+'>';
    end;

  // function ReadValue(const PreValue: string): string;
  var MacroStart,MacroEnd: integer;
    MacroFuncName, MacroStr, MacroParam: string;
  begin
//  writeln('    [ReadValue] A   "',PreValue,'"');
    Result:=PreValue;
    MacroStart:=1;
    while MacroStart<=length(Result) do begin
      // search for macro
      while (MacroStart<=length(Result)) and (Result[MacroStart]<>'$') do begin
        if (Result[MacroStart]=SpecialChar) then inc(MacroStart);
        inc(MacroStart);
      end;
      if MacroStart>length(Result) then break;
      // read macro function name
      MacroEnd:=MacroStart+1;
      while (MacroEnd<=length(Result))
      and (Result[MacroEnd] in ['a'..'z','A'..'Z','0'..'9','_']) do
        inc(MacroEnd);
      MacroFuncName:=copy(Result,MacroStart+1,MacroEnd-MacroStart-1);
      // read macro name / parameters
      if (MacroEnd<length(Result)) and (Result[MacroEnd] in ['(','{']) then
      begin
        MacroEnd:=SearchBracketClose(Result,MacroEnd)+1;
        if MacroEnd>length(Result)+1 then break;
        MacroStr:=copy(Result,MacroStart,MacroEnd-MacroStart);
        // Macro found
        if MacroFuncName<>'' then begin
          // Macro function -> substitute macro parameter first
          MacroParam:=ReadValue(copy(MacroStr,length(MacroFuncName)+3
              ,length(MacroStr)-length(MacroFuncName)-3));
          // execute the macro function
          MacroStr:=ExecuteMacroFunction(MacroFuncName,MacroParam);
        end else begin
          // Macro variable
          MacroStr:=copy(Result,MacroStart+2,MacroEnd-MacroStart-3);
          if DirDef.Values.IsDefined(MacroStr) then
            MacroStr:=DirDef.Values.Variables[MacroStr]
          else if Assigned(FOnReadValue) then begin
            MacroParam:=MacroStr;
            MacroStr:='';
            FOnReadValue(Self,MacroParam,MacroStr);
          end else
            MacroStr:='';
        end;
        Result:=copy(Result,1,MacroStart-1)+MacroStr
               +copy(Result,MacroEnd,length(Result)-MacroEnd+1);
        MacroEnd:=MacroStart+length(MacroStr);
      end;
      MacroStart:=MacroEnd;
    end;
  //writeln('    [ReadValue] END "',Result,'"');
  end;

  procedure CalculateTemplate(DefTempl: TDefineTemplate; const CurPath: string);
  
    procedure CalculateIfChilds;
    begin
      // execute childs
      CalculateTemplate(DefTempl.FirstChild,CurPath);
      // jump to end of else templates
      DefTempl:=DefTempl.Next;
      while (DefTempl<>nil) and (DefTempl.Action in [da_Else,da_ElseIf])
      do
        DefTempl:=DefTempl.Next;
      if DefTempl=nil then exit;
    end;

  // procedure CalculateTemplate(DefTempl: TDefineTemplate; const CurPath: string);
  var SubPath: string;
  begin
    while DefTempl<>nil do begin
  //writeln('  [CalculateTemplate] CurPath="',CurPath,'" DefTempl.Name="',DefTempl.Name,'"');
      case DefTempl.Action of
      da_Block:
        // calculate children
        begin
          CalculateTemplate(DefTempl.FirstChild,CurPath);
        end;
      da_Define:
        // Define for a single Directory (not SubDirs)
        begin
          if FilenameIsMatching(CurPath,ExpandedDirectory,true) then begin
            DirDef.Values.Variables[DefTempl.Variable]:=
              ReadValue(DefTempl.Value);
          end;
        end;
      da_Undefine:
        // Undefine for a single Directory (not SubDirs)
        begin
          if FilenameIsMatching(CurPath,ExpandedDirectory,true) then begin
            DirDef.Values.Undefine(DefTempl.Variable);
          end;
        end;
      da_DefineAll:
        begin
          // Define for current and sub directories
          DirDef.Values.Variables[DefTempl.Variable]:=
            ReadValue(DefTempl.Value);
        end;
      da_If, da_ElseIf:
        begin
          // test expression in value
          EvalResult:=DirDef.Values.Eval(ReadValue(DefTempl.Value));
          if EvalResult='1' then
            CalculateIfChilds
          else if EvalResult='0' then begin
            FErrorDescription:=
               'Syntax Error in expression "'+ReadValue(DefTempl.Value)+'"';
            FErrorTemplate:=DefTempl;
            exit;
          end;
        end;
      da_IfDef:
        begin
          // test if variable is defined
          if DirDef.Values.IsDefined(DefTempl.Variable) then
            CalculateIfChilds;
        end;
      da_IfNDef:
        begin
          // test if variable is not defined
          if not DirDef.Values.IsDefined(DefTempl.Variable) then
            CalculateIfChilds;
        end;
      da_Else:
        begin
          // execute childs
          CalculateTemplate(DefTempl.FirstChild,CurPath);
        end;
      da_Directory:
        begin
          // template for a sub directory
          {$ifdef win32}
          if CurPath='' then
            SubPath:=ReadValue(DefTempl.Value)
          else
            SubPath:=CurPath+OSDirSeparator+ReadValue(DefTempl.Value);
          {$else}
          SubPath:=CurPath+OSDirSeparator+ReadValue(DefTempl.Value);
          {$endif}
          // test if ExpandedDirectory is part of SubPath
          if FilenameIsMatching(SubPath,ExpandedDirectory,false) then
            CalculateTemplate(DefTempl.FirstChild,SubPath);
        end;
      end;
      if ErrorTemplate<>nil then exit;
      DefTempl:=DefTempl.Next;
    end;
  end;

// function TDefineTree.Calculate(DirDef: TDirectoryDefines): boolean;
begin
//writeln('[TDefineTree.Calculate] "',DirDef.Path,'"');
  Result:=true;
  FErrorTemplate:=nil;
  ExpandedDirectory:=ReadValue(DirDef.Path);
  DirDef.Values.Clear;
  // compute the result of all matching DefineTemplates
  CalculateTemplate(FFirstDefineTemplate,'');
  Result:=(ErrorTemplate=nil);
end;

function TDefineTree.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string): boolean;
begin
  FFirstDefineTemplate.Free;
  if XMLConfig.GetValue(Path+'/Count/Value',0)>0 then begin
    FFirstDefineTemplate:=TDefineTemplate.Create;
    Result:=FFirstDefineTemplate.LoadFromXMLConfig(XMLConfig,Path);
  end else begin
    FFirstDefineTemplate:=nil;
    Result:=true;
  end;
end;

function TDefineTree.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string): boolean;
begin
  FFirstDefineTemplate.SaveToXMLConfig(XMLConfig,Path);
  Result:=true;
end;

procedure TDefineTree.Add(ADefineTemplate: TDefineTemplate);
// add as last
var LastDefTempl: TDefineTemplate;
begin
  if ADefineTemplate=nil then exit;
  if RootTemplate=nil then
    RootTemplate:=ADefineTemplate
  else begin
    // add as last
    LastDefTempl:=RootTemplate;
    while LastDefTempl.Next<>nil do
      LastDefTempl:=LastDefTempl.Next;
    ADefineTemplate.InsertAfter(LastDefTempl);
  end;
end;

function TDefineTree.ConsistencyCheck: integer;
begin
  if FFirstDefineTemplate<>nil then begin
    Result:=FFirstDefineTemplate.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,1000);  exit;
    end;
  end;
  Result:=FCache.ConsistencyCheck;
  if Result<>0 then begin
    dec(Result,2000);  exit;
  end;
  Result:=0;
end;

procedure TDefineTree.WriteDebugReport;
begin
  writeln('TDefineTree.WriteDebugReport  Consistency=',ConsistencyCheck);
  if FFirstDefineTemplate<>nil then
    FFirstDefineTemplate.WriteDebugReport;
  writeln(FCache.ReportAsString);
  writeln('');
end;

    
{ TDefinePool }

constructor TDefinePool.Create;
begin
  inherited Create;
  FItems:=TList.Create;
end;

destructor TDefinePool.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TDefinePool.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  FItems.Clear;
end;

function TDefinePool.GetItems(Index: integer): TDefineTemplate;
begin
  Result:=TDefineTemplate(FItems[Index]);
end;

procedure TDefinePool.Add(ADefineTemplate: TDefineTemplate);
begin
  if ADefineTemplate<>nil then
    FItems.Add(ADefineTemplate);
end;

procedure TDefinePool.Insert(Index: integer; ADefineTemplate: TDefineTemplate);
begin
  FItems.Insert(Index,ADefineTemplate);
end;

procedure TDefinePool.Delete(Index: integer);
begin
  Items[Index].Free;
  FItems.Delete(Index);
end;

procedure TDefinePool.Move(SrcIndex, DestIndex: integer);
begin
  FItems.Move(SrcIndex,DestIndex);
end;

function TDefinePool.Count: integer;
begin
  Result:=FItems.Count;
end;

function TDefinePool.CreateFPCTemplate(
  const PPC386Path: string; var UnitSearchPath: string): TDefineTemplate;
// create makro definitions for the freepascal compiler
// To get reliable values the compiler itself is asked for

  procedure ProcessOutputLine(var LastDefTempl: TDefineTemplate; Line: string);
  var NewDefTempl: TDefineTemplate;
    MacroName, MacroValue, UpLine: string;
    i: integer;
  begin
    NewDefTempl:=nil;
    UpLine:=UpperCaseStr(Line);
    if copy(UpLine,1,15)='MACRO DEFINED: ' then begin
      MacroName:=copy(UpLine,16,length(Line)-15);
      NewDefTempl:=TDefineTemplate.Create('Define '+MacroName,
           'Default ppc386 macro',MacroName,'',da_DefineAll);
    end else if copy(UpLine,1,6)='MACRO ' then begin
      Line:=copy(Line,7,length(Line)-6);
      i:=1;
      while (i<=length(Line)) and (Line[i]<>' ') do inc(i);
      MacroName:=copy(UpLine,1,i-1);
      inc(i);
      Line:=copy(Line,i,length(Line)-i+1);
      if copy(Line,1,7)='set to ' then begin
        MacroValue:=copy(Line,8,length(Line)-7);
        NewDefTempl:=TDefineTemplate.Create('Define '+MacroName,
             'Default ppc386 macro',MacroName,MacroValue,da_DefineAll);
      end;
    end else if copy(UpLine,1,17)='USING UNIT PATH: ' then begin
      UnitSearchPath:=UnitSearchPath+copy(Line,18,length(Line)-17)+#13;
    end;
    if NewDefTempl<>nil then begin
      if LastDefTempl<>nil then
        NewDefTempl.InsertAfter(LastDefTempl);
      LastDefTempl:=NewDefTempl;
    end;
  end;
  
// function TDefinePool.CreateFPCTemplate(
//  const PPC386Path: string): TDefineTemplate;
var CmdLine, BogusFilename: string;
  i, OutLen, LineStart: integer;
  TheProcess : TProcess;
  OutputLine, Buf, TargetOS, SrcOS, TargetProcessor: String;
  DefTempl, NewDefTempl: TDefineTemplate;
begin
  Result:=nil;
  UnitSearchPath:='';
  if PPC386Path='' then exit;
  DefTempl:=nil;
  // find all initial compiler macros and all unit paths
  // -> ask compiler with the -va switch
  SetLength(Buf,1024);
  try
    CmdLine:=PPC386Path+' -va ';
    // give compiler a not existing file, so that it will return quickly
    BogusFilename:='bogus';
    i:=1;
    while FileExists(BogusFilename+IntToStr(i)) do inc(i);
    CmdLine:=CmdLine+BogusFilename;
    TheProcess:=TProcess.Create(CmdLine,[poUsePipes,poNoConsole
         ,poStdErrToOutput]);
    try
      TheProcess.Execute;
      OutputLine:='';
      repeat
        if TheProcess.Output<>nil then
          OutLen:=TheProcess.Output.Read(Buf[1],length(Buf))
        else
          OutLen:=0;
        LineStart:=1;
        i:=1;
        while i<=OutLen do begin
          if Buf[i] in [#10,#13] then begin
            OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
            ProcessOutputLine(DefTempl,OutputLine);
            OutputLine:='';
            if (i<OutLen) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
            then
              inc(i);
            LineStart:=i+1;
          end;
          inc(i);
        end;
        OutputLine:=copy(Buf,LineStart,OutLen-LineStart+1);
      until OutLen=0;
    finally
      TheProcess.Free;
    end;

    // ask for target operating system -> ask compiler with switch -iTO
    CmdLine:=PPC386Path+' -iTO';
    TheProcess:=TProcess.Create(CmdLine,[poUsePipes,poNoConsole
         ,poStdErrToOutput]);
    try
      TheProcess.Execute;
      if TheProcess.Output<>nil then
        OutLen:=TheProcess.Output.Read(Buf[1],length(Buf))
      else
        OutLen:=0;
      i:=1;
      while i<=OutLen do begin
        if Buf[i] in [#10,#13] then begin
          TargetOS:=copy(Buf,1,i-1);
          NewDefTempl:=TDefineTemplate.Create('Define TargetOS',
            'Default ppc386 target Operating System',
            ExternalMacroStart+'TargetOS',TargetOS,da_DefineAll);
          if DefTempl<>nil then
            NewDefTempl.InsertAfter(DefTempl);
          DefTempl:=NewDefTempl;
          if TargetOS='linux' then
            SrcOS:='unix'
          else
            SrcOS:=TargetOS;
          NewDefTempl:=TDefineTemplate.Create('Define SrcOS',
            'Default ppc386 source Operating System',
            ExternalMacroStart+'SrcOS',SrcOS,da_DefineAll);
          if DefTempl<>nil then
            NewDefTempl.InsertAfter(DefTempl);
          DefTempl:=NewDefTempl;
          break;
        end;
        inc(i);
      end;
    finally
      TheProcess.Free;
    end;
    
    // ask for target processor -> ask compiler with switch -iTP
    TheProcess:=TProcess.Create(PPC386Path+' -iTP',[poUsePipes,poNoConsole
         ,poStdErrToOutput]);
    try
      TheProcess.Execute;
      if TheProcess.Output<>nil then
        OutLen:=TheProcess.Output.Read(Buf[1],length(Buf))
      else
        OutLen:=0;
      i:=1;
      while i<=OutLen do begin
        if Buf[i] in [#10,#13] then begin
          TargetProcessor:=copy(Buf,1,i-1);
          NewDefTempl:=TDefineTemplate.Create('Define TargetProcessor',
            'Default ppc386 target Operating System',
            ExternalMacroStart+'TargetProcessor',TargetProcessor,da_DefineAll);
          if DefTempl<>nil then
            NewDefTempl.InsertAfter(DefTempl);
          DefTempl:=NewDefTempl;
          break;
        end;
        inc(i);
      end;
    finally
      TheProcess.Free;
    end;

    // add
    if (DefTempl<>nil) then begin
      while (DefTempl.Prior<>nil) do DefTempl:=DefTempl.Prior;
      Result:=TDefineTemplate.Create('Free Pascal Compiler',
        'Free Pascal Compiler initial makros','','',da_Block);
      Result.AddChild(DefTempl);
    end;
  except
  end;
end;

function TDefinePool.CreateFPCSrcTemplate(
  const FPCSrcDir, UnitSearchPath: string): TDefineTemplate;
var DefTempl, MainDir,
  FCLDir, RTLDir, PackagesDir, CompilerDir: TDefineTemplate;
  Dir, TargetOS, SrcOS, TargetProcessor, UnitLinks, UnitLinkList: string;
  DS: char;
  UnitTree: TAVLTree; // tree of TUnitNameLink

  procedure GatherUnits; forward;

  function FindUnitLink(const AnUnitName: string): TUnitNameLink;
  var ANode: TAVLTreeNode;
    cmp: integer;
  begin
    if UnitTree=nil then GatherUnits;
    ANode:=UnitTree.Root;
    while ANode<>nil do begin
      Result:=TUnitNameLink(ANode.Data);
      cmp:=AnsiCompareText(AnUnitName,Result.UnitName);
      if cmp<0 then
        ANode:=ANode.Left
      else if cmp>0 then
        ANode:=ANode.Right
      else
        exit;
    end;
    Result:=nil;
  end;

  procedure GatherUnits;
  
    function FileNameMacroCount(const AFilename: string): integer;
    // count number of macros in filename
    // a macro looks like this '$(name)' without a SpecialChar in front
    // macronames can contain macros themselves
    var i: integer;
    begin
      Result:=0;
      i:=1;
      while (i<=length(AFilename)) do begin
        if (AFilename[i]=SpecialChar) then
          inc(i,2)
        else if (AFilename[i]='$') then begin
          inc(i);
          if (i<=length(AFilename)) and (AFilename[i]='(') then
            inc(Result);
        end else
          inc(i);
      end;
    end;
    
    function BuildMacroFilename(const AFilename: string;
      var SrcOSMakroUsed: boolean): string;
    // replace Operating System and Processor Type with macros
    var DirStart, DirEnd, i: integer;
      DirName: string;
    begin
      SrcOSMakroUsed:=false;
      Result:=copy(AFilename,length(FPCSrcDir)+1,
                   length(AFilename)-length(FPCSrcDir));
      DirStart:=1;
      while (DirStart<=length(Result)) do begin
        while (DirStart<=length(Result)) and (Result[DirStart]=OSDirSeparator)
        do
          inc(DirStart);
        DirEnd:=DirStart;
        while (DirEnd<=length(Result)) and (Result[DirEnd]<>OSDirSeparator) do
          inc(DirEnd);
        if DirEnd>length(Result) then break;
        if DirEnd>DirStart then begin
          DirName:=copy(Result,DirStart,DirEnd-DirStart);
          // replace operating system
          for i:=Low(FPCOperatingSystemNames) to High(FPCOperatingSystemNames)
          do
            if FPCOperatingSystemNames[i]=DirName then begin
              Result:=copy(Result,1,DirStart-1)+TargetOS+
                      copy(Result,DirEnd,length(Result)-DirEnd+1);
              inc(DirEnd,length(TargetOS)-length(DirName));
              DirName:=TargetOS;
              break;
            end;
          // replace operating system class
          for i:=Low(FPCOperatingSystemAlternativeNames)
              to High(FPCOperatingSystemAlternativeNames)
          do
            if FPCOperatingSystemAlternativeNames[i]=DirName then begin
              Result:=copy(Result,1,DirStart-1)+SrcOS+
                      copy(Result,DirEnd,length(Result)-DirEnd+1);
              inc(DirEnd,length(SrcOS)-length(DirName));
              DirName:=SrcOS;
              SrcOSMakroUsed:=true;
              break;
            end;
          // replace processor type
          for i:=Low(FPCProcessorNames) to High(FPCProcessorNames) do
            if FPCProcessorNames[i]=DirName then begin
              Result:=copy(Result,1,DirStart-1)+TargetProcessor+
                      copy(Result,DirEnd,length(Result)-DirEnd+1);
              inc(DirEnd,length(TargetProcessor)-length(DirName));
              DirName:=TargetProcessor;
              break;
            end;
        end;
        DirStart:=DirEnd;
      end;
      Result:=FPCSrcDir+Result;
    end;
    
    procedure BrowseDirectory(ADirPath: string);
    const
      IgnoreDirs: array[1..12] of shortstring =(
          '.', '..', 'CVS', 'examples', 'example', 'tests', 'fake', 'ide',
          'demo', 'docs', 'template', 'fakertl'
        );
    
    var
      AFilename, Ext, UnitName, MakroFileName: string;
      FileInfo: TSearchRec;
      NewUnitLink, OldUnitLink: TUnitNameLink;
      SrcOSMakroUsed: boolean;
      i: integer;
    begin
      if ADirPath='' then exit;
      if not (ADirPath[length(ADirPath)]=OSDirSeparator) then
        ADirPath:=ADirPath+OSDirSeparator;
      if FindFirst(ADirPath+'*.*',faAnyFile,FileInfo)=0 then begin
        repeat
          AFilename:=FileInfo.Name;
          i:=High(IgnoreDirs);
          while (i>=Low(IgnoreDirs)) and (AFilename<>IgnoreDirs[i]) do dec(i);
          if i>=Low(IgnoreDirs) then continue;
          AFilename:=ADirPath+AFilename;
          if (FileInfo.Attr and faDirectory)>0 then begin
            // ToDo: prevent cycling in links
            BrowseDirectory(AFilename);
          end else begin
            Ext:=UpperCaseStr(ExtractFileExt(AFilename));
            if (Ext='.PP') or (Ext='.PAS') then begin
              // pascal unit found
              UnitName:=FileInfo.Name;
              UnitName:=copy(UnitName,1,length(UnitName)-length(Ext));
              OldUnitLink:=FindUnitLink(UnitName);
              MakroFileName:=BuildMacroFileName(AFilename,SrcOSMakroUsed);
              if OldUnitLink=nil then begin
                // first unit with this name
                if UnitName<>'' then begin
                  NewUnitLink:=TUnitNameLink.Create;
                  NewUnitLink.UnitName:=UnitName;
                  NewUnitLink.FileName:=MakroFileName;
                  UnitTree.Add(NewUnitLink);
                end;
              end else begin
                { there is another unit with this name
                
                  the decision which filename is the right one is based on a
                  simple heuristic:
                    FPC stores a unit many times, if there is different version
                    for each Operating System or Processor Type. And sometimes
                    units are stored in a combined OS (e.g. 'unix').
                    Therefore every occurence of such values is replaced by a
                    macro. And filenames without macros are always deleted if
                    there is a filename with a macro.
                    For example:
                     classes.pp can be found in several places
                      <FPCSrcDir>/fcl/os2/classes.pp
                      <FPCSrcDir>/fcl/linux/classes.pp
                      <FPCSrcDir>/fcl/win32/classes.pp
                      <FPCSrcDir>/fcl/go32v2/classes.pp
                      <FPCSrcDir>/fcl/freebsd/classes.pp
                      <FPCSrcDir>/fcl/template/classes.pp
                      
                     This will result in a single filename:
                      $(#FPCSrcDir)/fcl/$(#TargetOS)/classes.pp
                }
                if (FileNameMacroCount(OldUnitLink.Filename)=0)
                or (SrcOSMakroUsed) then begin
                  // old filename has no macros -> build a macro filename
                  OldUnitLink.Filename:=MakroFileName;
                end;
              end;
            end;
          end;
        until FindNext(FileInfo)<>0;
      end;
    end;
  
  begin
    if UnitTree=nil then
      UnitTree:=TAVLTree.Create(@CompareUnitLinkNodes)
    else
      UnitTree.FreeAndClear;
    BrowseDirectory(FPCSrcDir);
  end;
  

  procedure AddFPCSourceLinkForUnit(const AnUnitName: string);
  var UnitLink: TUnitNameLink;
    s: string;
  begin
    // search
    if AnUnitName='' then exit;
    UnitLink:=FindUnitLink(AnUnitName);
    if UnitLink=nil then exit;
    s:=AnUnitName+' '+UnitLink.Filename+#13;
    UnitLinkList:=UnitLinkList+s;
  end;

  procedure FindStandardPPUSources;
  var PathStart, PathEnd: integer;
    ADirPath, UnitName: string;
    FileInfo: TSearchRec;
  begin
    // try every ppu file in every reachable directory (CompUnitPath)
    UnitLinkList:='';
    PathStart:=1;
    while PathStart<=length(UnitSearchPath) do begin
      while (PathStart<=length(UnitSearchPath))
      and (UnitSearchPath[PathStart]=#13) do
        inc(PathStart);
      PathEnd:=PathStart;
      // extract single path from unit search path
      while (PathEnd<=length(UnitSearchPath))
      and (UnitSearchPath[PathEnd]<>#13) do
        inc(PathEnd);
      if PathEnd>PathStart then begin
        ADirPath:=copy(UnitSearchPath,PathStart,PathEnd-PathStart);
        // search all ppu files in this directory
        if FindFirst(ADirPath+'*.ppu',faAnyFile,FileInfo)=0 then begin
          repeat
            UnitName:=ExtractFileName(FileInfo.Name);
            UnitName:=copy(UnitName,1,length(UnitName)-4);
            AddFPCSourceLinkForUnit(UnitName);
          until FindNext(FileInfo)<>0;
        end;
      end;
      PathStart:=PathEnd;
    end;
  end;

// function TDefinePool.CreateFPCSrcTemplate(
//  const FPCSrcDir: string): TDefineTemplate;
begin
  Result:=nil;
  if FPCSrcDir='' then exit;
  DS:=OSDirSeparator;
  Dir:=FPCSrcDir;
  if Dir[length(Dir)]<>DS then Dir:=Dir+DS;
  TargetOS:='$('+ExternalMacroStart+'TargetOS)';
  SrcOS:='$('+ExternalMacroStart+'SrcOS)';
  TargetProcessor:='$('+ExternalMacroStart+'TargetProcessor)';
  UnitLinks:='$('+ExternalMacroStart+'UnitLinks)';
  UnitTree:=nil;

  Result:=TDefineTemplate.Create(StdDefTemplFPCSrc,
     'Free Pascal Sources, RTL, FCL, Packages, Compiler','','',da_Block);
     
  // try to find for every reachable ppu file the unit file in the FPC sources
  FindStandardPPUSources;
  DefTempl:=TDefineTemplate.Create('FPC Unit Links',
    'Source filenames for the standard fpc units',
    UnitLinks,UnitLinkList,da_DefineAll);
  Result.AddChild(DefTempl);

  // The free pascal sources build a world of their own,
  // reset source search path
  MainDir:=TDefineTemplate.Create('Free Pascal Source Directory',
    'Free Pascal Source Directory',
    '',FPCSrcDir,da_Directory);
  Result.AddChild(MainDir);
  DefTempl:=TDefineTemplate.Create('Reset SrcPath',
    'SrcPath Init',
    ExternalMacroStart+'SrcPath','',da_DefineAll);
  MainDir.AddChild(DefTempl);

  // compiler
  CompilerDir:=TDefineTemplate.Create('Compiler','Compiler','','compiler',
     da_Directory);
  MainDir.AddChild(CompilerDir);

  // fcl
  FCLDir:=TDefineTemplate.Create('FCL','Free Pascal Component Library','','fcl',
      da_Directory);
  MainDir.AddChild(FCLDir);

  // rtl
  RTLDir:=TDefineTemplate.Create('RTL','Runtime library','','rtl',da_Directory);
  MainDir.AddChild(RTLDir);

  // packages
  PackagesDir:=TDefineTemplate.Create('Packages','Package directories','',
     'packages',da_Directory);
  MainDir.AddChild(PackagesDir);
end;

function TDefinePool.CreateLazarusSrcTemplate(
  const LazarusSrcDir, WidgetType: string): TDefineTemplate;
const ds: char = OSDirSeparator;
var MainDir, DirTempl, SubDirTempl: TDefineTemplate;
  TargetOS, SrcPath: string;
begin
  Result:=nil;
  if (LazarusSrcDir='') or (WidgetType='') then exit;
  TargetOS:='$('+ExternalMacroStart+'TargetOS)';
  SrcPath:='$('+ExternalMacroStart+'SrcPath)';

  // <LazarusSrcDir>
  MainDir:=TDefineTemplate.Create('Lazarus Source Directory',
    'Definitions for the Lazarus Sources','',LazarusSrcDir,da_Directory);
  MainDir.AddChild(TDefineTemplate.Create('LCL path addition',
    'adds lcl to SrcPath',ExternalMacroStart+'SrcPath',
    'lcl:lcl'+ds+'interfaces'+ds+WidgetType+':'+SrcPath
    ,da_Define));
  MainDir.AddChild(TDefineTemplate.Create('Component path addition',
    'adds designer and synedit to SrcPath',ExternalMacroStart+'SrcPath',
    'components'+ds+'synedit:designer:'+SrcPath
    ,da_Define));
  MainDir.AddChild(TDefineTemplate.Create('includepath addition',
    'adds include to IncPath',ExternalMacroStart+'IncPath',
    'include'+ds+TargetOS,
    da_Define));
    
  // examples
  DirTempl:=TDefineTemplate.Create('Examples','Examples Directory',
    '','examples',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    'adds lcl to SrcPath',
    ExternalMacroStart+'SrcPath',
    '..'+ds+'lcl:..'+ds+'lcl/interfaces/'+WidgetType+':'+SrcPath
    ,da_Define));
  MainDir.AddChild(DirTempl);
  
  // lcl
  DirTempl:=TDefineTemplate.Create('LCL','LCL Directory',
    '','lcl',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('WidgetPath',
     'adds widget path to SrcPath'
    ,ExternalMacroStart+'SrcPath',
    'interfaces'+ds+WidgetType+':'+SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('IncludePath',
     'adds include to IncPaty',ExternalMacroStart+'IncPath',
    +'include',da_Define));
  MainDir.AddChild(DirTempl);

  // Widget Directory
  SubDirTempl:=TDefineTemplate.Create('Widget Directory','Widget Directory',
    '','interfaces'+ds+WidgetType,da_Directory);
  SubDirTempl.AddChild(TDefineTemplate.Create('LCL Path',
    'adds lcl to SrcPath','SrcPath',
    '..'+ds+'..:'+SrcPath,da_Define));
  DirTempl.AddChild(SubDirTempl);
  
  // components
  DirTempl:=TDefineTemplate.Create('Components','Components Dircetory',
    '','components',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL Path','adds lcl to SrcPath',
    'SrcPath',
    LazarusSrcDir+ds+'lcl'
    +':'+LazarusSrcDir+ds+'lcl'+ds+'interfaces'+ds+WidgetType
    +':'+SrcPath
    ,da_DefineAll));
  MainDir.AddChild(DirTempl);

  // tools
  
  // include
  
  // designer
  DirTempl:=TDefineTemplate.Create('Designer','Designer Directory',
    '','designer',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    'adds lcl to SrcPath',
    ExternalMacroStart+'SrcPath',
    '..'+ds+'lcl'
      +':..'+ds+'lcl'+ds+'interfaces'+ds+WidgetType
      +':'+SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('main path addition',
    'adds lazarus source directory to SrcPath',
    ExternalMacroStart+'SrcPath',
    '..:'+SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('synedit path addition',
    'adds synedit directory to SrcPath',
    ExternalMacroStart+'SrcPath',
    '../components/synedit:'+SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('includepath addition',
    'adds include to IncPath',ExternalMacroStart+'IncPath',
    'include'+ds+TargetOS,
    da_Define));
  MainDir.AddChild(DirTempl);
  
  // images
  
  // debugger
  
  if MainDir<>nil then begin
    Result:=TDefineTemplate.Create(StdDefTemplLazarusSources,
       'Lazarus Sources, LCL, IDE, Components, Examples, Tools','','',da_Block);
    Result.AddChild(MainDir);
  end;
end;

function TDefinePool.CreateLCLProjectTemplate(
  const LazarusSrcDir, WidgetType, ProjectDir: string): TDefineTemplate;
var DirTempl: TDefineTemplate;
begin
  Result:=nil;
  if (LazarusSrcDir='') or (WidgetType='') or (ProjectDir='') then exit;
  DirTempl:=TDefineTemplate.Create('ProjectDir','an LCL project',
    '',ProjectDir,da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL','adds lcl to SrcPath',
    ExternalMacroStart+'SrcPath',
    LazarusSrcDir+OSDirSeparator+'lcl:'
     +LazarusSrcDir+OSDirSeparator+'lcl'+OSDirSeparator+'interfaces'
     +OSDirSeparator+WidgetType
     +':$('+ExternalMacroStart+'SrcPath)'
    ,da_DefineAll));
  Result:=TDefineTemplate.Create(StdDefTemplLCLProject,
       'LCL Project','','',da_Block);
  Result.AddChild(DirTempl);
end;

function TDefinePool.ConsistencyCheck: integer;
var i: integer;
begin
  for i:=0 to Count-1 do begin
    Result:=Items[i].ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,100);  exit;
    end;
  end;
  Result:=0;
end;

procedure TDefinePool.WriteDebugReport;
var i: integer;
begin
  writeln('TDefinePool.WriteDebugReport Consistency=',ConsistencyCheck);
  for i:=0 to Count-1 do begin
    Items[i].WriteDebugReport;
  end;
end;


end.

