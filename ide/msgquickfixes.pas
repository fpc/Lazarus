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
}
(*
  Author: Mattias Gaertner

  Abstract:
    Defines the standard message Quick Fix menu items.

  ToDo:
    - There is no method in an ancestor class to be overriden:
      1. option: if the ancestor has a function with the same name: update the parameter list
      2. option: remove the method
      3. option: add a virtual method to the ancestor
    - complete function implementations with missing parameters
    - private variable not used => remove
    - Hint: Local variable "Path" does not seem to be initialized
         auto add begin+end
         Pointer:=nil
         integer:=0
         string:=''
         record: FillByte(p{%H-},SizeOf(p),0)
         set:=[]
         enum:=low(enum);

*)
unit MsgQuickFixes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls, Dialogs, FileUtil, KeywordFuncLists,
  BasicCodeTools, CodeTree, CodeAtom, CodeCache, CodeToolManager,
  DirectoryCacher, FileProcs, IDEMsgIntf, TextTools, ProjectIntf, LazIDEIntf,
  PackageIntf, IDEDialogs, AbstractsMethodsDlg, LazarusIDEStrConsts,
  EnvironmentOpts;
  
type

  { TQuickFixIdentifierNotFoundAddLocal }

  TQuickFixIdentifierNotFoundAddLocal = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

  { TQuickFixUnitNotFoundPosition - improve message }

  TQuickFixUnitNotFoundPosition = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;
  
  { TQuickFixUnitNotFound_Remove - add menu item to remove unit from uses section }

  TQuickFixUnitNotFound_Remove = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

  { TQuickFixErrorWhileCompilingResources_Hint - improve message }

  TQuickFixErrorWhileCompilingResources_Hint = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

  { TQuickFixJumpToLinkerUndefinedReference }

  TQuickFixJumpToLinkerUndefinedReference = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

  { TQuickFixClassWithAbstractMethods
    Quick fix for example:
    Warning: Constructing a class "TClassA" with abstract methods }

  TQuickFixClassWithAbstractMethods = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

  { TQuickFixLocalVariableNotUsed_Remove }

  TQuickFixLocalVariableNotUsed_Remove = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

  { TQuickFixHint_Hide }

  TQuickFixHint_Hide = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

procedure QuickFixParameterNotUsed(Sender: TObject; Step: TIMQuickFixStep;
                                   Msg: TIDEMessageLine);
procedure QuickFixUnitNotUsed(Sender: TObject; Step: TIMQuickFixStep;
                              Msg: TIDEMessageLine);

function GetMsgLineFile(Msg: TIDEMessageLine;
                        out CodeBuf: TCodeBuffer; Quiet: boolean): boolean;
function IsFileInIDESrcDir(Filename: string): boolean;

procedure InitStandardIDEQuickFixItems;
procedure FreeStandardIDEQuickFixItems;

implementation

procedure ShowError(Msg: string);
begin
  IDEMessageDialog('QuickFix error',Msg,mtError,[mbCancel]);
end;

function IsIdentifierInCode(Code: TCodeBuffer; X,Y: integer;
  Identifier, ErrorMsg: string): boolean;
var
  p: integer;
  IdentStart: integer;
  IdentEnd: integer;
begin
  Result:=false;
  if Code=nil then begin
    ShowError(ErrorMsg+' (Code=nil)');
    exit;
  end;
  Code.LineColToPosition(Y,X,p);
  if p<1 then begin
    ShowError(ErrorMsg+' (position outside of source');
    exit;
  end;
  GetIdentStartEndAtPosition(Code.Source,p,IdentStart,IdentEnd);
  if SysUtils.CompareText(Identifier,copy(Code.Source,IdentStart,IdentEnd-IdentStart))<>0
  then begin
    ShowError(ErrorMsg);
    exit;
  end;
  Result:=true;
end;

procedure QuickFixParameterNotUsed(Sender: TObject; Step: TIMQuickFixStep;
  Msg: TIDEMessageLine);
begin
  DebugLn('QuickFixParameterNotUsed ');
end;

procedure QuickFixUnitNotUsed(Sender: TObject; Step: TIMQuickFixStep;
  Msg: TIDEMessageLine);
var
  CodeBuf: TCodeBuffer;
  UnneededUnitname: String;
  OldChange: Boolean;
begin
  if Step<>imqfoMenuItem then exit;
  if not GetMsgLineFile(Msg,CodeBuf,false) then exit;
  
  if not REMatches(Msg.Msg,'Unit "([a-z_0-9]+)" not used','I') then begin
    DebugLn('QuickFixUnitNotUsed invalid message ',Msg.Msg);
    ShowError('QuickFix: UnitNotUsed invalid message '+Msg.Msg);
    exit;
  end;
  UnneededUnitname:=REVar(1);

  // remove unit
  if not LazarusIDE.BeginCodeTools then exit;
  OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
  LazarusIDE.OpenEditorsOnCodeToolChange:=true;
  try
    LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
    if not CodeToolBoss.RemoveUnitFromAllUsesSections(CodeBuf,UnneededUnitname)
    then begin
      LazarusIDE.DoJumpToCodeToolBossError;
      exit;
    end;
  finally
    LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
  end;

  // message fixed
  Msg.Msg:='';
end;

function GetMsgLineFile(Msg: TIDEMessageLine; out CodeBuf: TCodeBuffer;
  Quiet: boolean): boolean;
var
  Filename: String;
  TestDir: String;
begin
  Result:=false;
  CodeBuf:=nil;
  if Msg.Parts=nil then begin
    DebugLn('GetMsgLineFilename Msg.Parts=nil');
    if not Quiet then begin
      IDEMessageDialog(lisCCOErrorCaption,
        Format(lisMessageContainsNoFilePositionInformation, [LineEnding, Msg.Msg]),
          mtError, [mbCancel]);
    end;
    exit;
  end;

  Filename:=Msg.Parts.Values['Filename'];
  TestDir:=LazarusIDE.GetTestBuildDirectory;
  if (TestDir<>'') and (FileIsInDirectory(Filename,TestDir)) then
    Filename:=ExtractFileName(Filename)
  else if not FilenameIsAbsolute(Filename) then
    Filename:=AppendPathDelim(Msg.Directory)+Filename;
  //DebugLn('GetMsgLineFilename Filename=',Filename,' ',Msg.Parts.Text);

  CodeBuf:=CodeToolBoss.LoadFile(Filename,false,false);
  if CodeBuf=nil then begin
    DebugLn('GetMsgLineFilename Filename "',Filename,'" not found.');
    if not Quiet then begin
      IDEMessageDialog(lisCCOErrorCaption,
        Format(lisUnableToLoadFile, [LineEnding, Filename]), mtError, [mbCancel]);
    end;
    exit;
  end;
  Result:=true;
end;

function IsFileInIDESrcDir(Filename: string): boolean;
var
  LazDir: String;
begin
  Filename:=TrimFilename(Filename);
  if not FilenameIsAbsolute(Filename) then exit(false);
  LazDir:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory);
  Result:=FileIsInPath(Filename,LazDir+'ide')
       or FileIsInPath(Filename,LazDir+'debugger')
       or FileIsInPath(Filename,LazDir+'packager')
       or FileIsInPath(Filename,LazDir+'converter')
       or FileIsInPath(Filename,LazDir+'designer');
end;

procedure InitStandardIDEQuickFixItems;
begin
  IDEMsgQuickFixes:=TIDEMsgQuickFixItems.Create;
  
  //RegisterIDEMsgQuickFix('Parameter xxx not used','Quick fix: Add dummy line',
  //  'Parameter "[a-z_0-9]+" not used',nil,@QuickFixParameterNotUsed);
  RegisterIDEMsgQuickFix('Unit xxx not used in yyy', lisQuickFixRemoveUnit,
    'Unit "[a-z_0-9]+" not used in [a-z_0-9]+',[imqfoMenuItem],
    nil,@QuickFixUnitNotUsed);
    
  RegisterIDEMsgQuickFix(TQuickFixUnitNotFoundPosition.Create);
  RegisterIDEMsgQuickFix(TQuickFixUnitNotFound_Remove.Create);
  RegisterIDEMsgQuickFix(TQuickFixJumpToLinkerUndefinedReference.Create);
  RegisterIDEMsgQuickFix(TQuickFixErrorWhileCompilingResources_Hint.Create);
  RegisterIDEMsgQuickFix(TQuickFixClassWithAbstractMethods.Create);
  RegisterIDEMsgQuickFix(TQuickFixIdentifierNotFoundAddLocal.Create);
  RegisterIDEMsgQuickFix(TQuickFixLocalVariableNotUsed_Remove.Create);
  RegisterIDEMsgQuickFix(TQuickFixHint_Hide.Create);
end;

procedure FreeStandardIDEQuickFixItems;
begin
  FreeThenNil(IDEMsgQuickFixes);
end;

{ TQuickFixErrorWhileCompilingResources_Hint }

constructor TQuickFixErrorWhileCompilingResources_Hint.Create;
begin
  Name:='Improve error message: Error while compiling resources';
  Steps:=[imqfoImproveMessage];
end;

function TQuickFixErrorWhileCompilingResources_Hint.IsApplicable(
  Line: TIDEMessageLine): boolean;
const
  SearchStr = 'Error while compiling resources';
var
  Msg: String;
  p: integer;
begin
  Result:=false;
  if (Line.Parts=nil) then exit;
  Msg:=Line.Msg;
  p:=System.Pos(SearchStr,Msg);
  if p<1 then exit;
  Result:=true;
end;

procedure TQuickFixErrorWhileCompilingResources_Hint.Execute(
  const Msg: TIDEMessageLine; Step: TIMQuickFixStep);
var
  s: String;
begin
  if Step<>imqfoImproveMessage then exit;
  s:=Msg.Msg;
  if s[length(s)]<>'.' then s+='.';
  s+=' Compile with -vd for more details. Check for duplicates.';
  Msg.Msg:=s;
end;

{ TQuickFixUnitNotFoundPosition }

constructor TQuickFixUnitNotFoundPosition.Create;
begin
  Name:='Improve error position of: Fatal: Can''t find unit xxx';
  Steps:=[imqfoImproveMessage];
end;

function TQuickFixUnitNotFoundPosition.IsApplicable(Line: TIDEMessageLine
  ): boolean;
var
  Msg: String;
begin
  if Line.Parts=nil then exit(false);
  Msg:=Line.Msg;
  Result:=(System.Pos(') Fatal: Can''t find unit ',Msg)>0)
       or (System.Pos(') Fatal: Can not find unit ',Msg)>0)
       or (System.Pos('Fatal: Can''t find unit ',Msg)=1)
       or (System.Pos('Fatal: Can not find unit ',Msg)=1);
end;

procedure TQuickFixUnitNotFoundPosition.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
// for example:
// Fatal: Can't find unit Unit12 used by testunit1
// /home/user/laz/main.pp(1,1) Fatal: Can't find unit lazreport used by lazarus

  procedure FixSourcePos(CodeBuf: TCodeBuffer; MissingUnitname: string;
    var NewFilename: string; var Dir: string);
  var
    Caret: TCodeXYPosition;
    Tool: TCodeTool;
    InPos: Integer;
    NamePos: Integer;
  begin
    {$IFDEF VerboseQuickFixUnitNotFoundPosition}
    debugln(['TQuickFixUnitNotFoundPosition.Execute File=',CodeBuf.Filename]);
    {$ENDIF}
    LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
    if not CodeToolBoss.FindUnitInAllUsesSections(CodeBuf,MissingUnitname,NamePos,InPos)
    then begin
      DebugLn('QuickFixUnitNotFoundPosition failed due to syntax errors or '+MissingUnitname+' is not used in '+CodeBuf.Filename);
      //LazarusIDE.DoJumpToCodeToolBossError;
      exit;
    end;
    Tool:=CodeToolBoss.CurCodeTool;
    if Tool=nil then exit;
    if not Tool.CleanPosToCaret(NamePos,Caret) then exit;
    if (Caret.X>0) and (Caret.Y>0) then begin
      //DebugLn('QuickFixUnitNotFoundPosition Line=',dbgs(Line),' Col=',dbgs(Col));
      NewFilename:=Caret.Code.Filename;
      if (Msg.Directory<>'') and (FilenameIsAbsolute(Msg.Directory)) then
        NewFilename:=CreateRelativePath(NewFilename,Msg.Directory);
      Msg.SetSourcePosition(NewFilename,Caret.Y,Caret.X);
      Dir:=AppendPathDelim(TrimFilename(Msg.Directory));
    end;
  end;

  procedure FindPPUInInstalledPkgs(MissingUnitname: string;
    var PPUFilename, PkgName: string);
  var
    i: Integer;
    Pkg: TIDEPackage;
    DirCache: TCTDirectoryCache;
    UnitOutDir: String;
  begin
    // search ppu in installed packages
    for i:=0 to PackageEditingInterface.GetPackageCount-1 do begin
      Pkg:=PackageEditingInterface.GetPackages(i);
      if Pkg.AutoInstall=pitNope then continue;
      UnitOutDir:=Pkg.LazCompilerOptions.GetUnitOutputDirectory(false);
      //debugln(['TQuickFixUnitNotFoundPosition.Execute ',Pkg.Name,' UnitOutDir=',UnitOutDir]);
      if FilenameIsAbsolute(UnitOutDir) then begin
        DirCache:=CodeToolBoss.DirectoryCachePool.GetCache(UnitOutDir,true,false);
        PPUFilename:=DirCache.FindFile(MissingUnitname+'.ppu',ctsfcLoUpCase);
        //debugln(['TQuickFixUnitNotFoundPosition.Execute ShortPPU=',PPUFilename]);
        if PPUFilename<>'' then begin
          PkgName:=Pkg.Name;
          PPUFilename:=AppendPathDelim(DirCache.Directory)+PPUFilename;
          break;
        end;
      end;
    end;
  end;

  procedure FindPackage(MissingUnitname: string; var PkgName: string;
    OnlyInstalled: boolean);
  var
    i: Integer;
    Pkg: TIDEPackage;
    j: Integer;
    PkgFile: TLazPackageFile;
  begin
    if PkgName='' then begin
      // search unit in installed packages
      for i:=0 to PackageEditingInterface.GetPackageCount-1 do begin
        Pkg:=PackageEditingInterface.GetPackages(i);
        if OnlyInstalled and (Pkg.AutoInstall=pitNope) then continue;
        if CompareTextCT(Pkg.Name,MissingUnitname)=0 then begin
          PkgName:=Pkg.Name;
          break;
        end;
        for j:=0 to Pkg.FileCount-1 do begin
          PkgFile:=Pkg.Files[j];
          if not FilenameIsPascalUnit(PkgFile.Filename) then continue;
          if CompareTextCT(ExtractFileNameOnly(PkgFile.Filename),MissingUnitname)<>0
          then continue;
          PkgName:=Pkg.Name;
          break;
        end;
      end;
    end;
  end;

var
  CodeBuf: TCodeBuffer;
  MissingUnitname: String;
  UsedByUnit: String;
  NewFilename: String; // referencing unit
  Dir: String;
  PPUFilename: String;
  s: String;
  Filename: string;
  Line: integer;
  Col: integer;
  PkgName: String;
  OnlyInstalled: Boolean;
  Owners: TFPList;
  UsedByOwner: TObject;
begin
  if Step<>imqfoImproveMessage then exit;
  {$IFDEF VerboseQuickFixUnitNotFoundPosition}
  DebugLn('QuickFixUnitNotFoundPosition START');
  {$ENDIF}

  if not REMatches(Msg.Msg,'Can(''t| not) find unit ([a-z_.0-9]+) used by ','I')
  then begin
    {$IFDEF VerboseQuickFixUnitNotFoundPosition}
    DebugLn('QuickFixUnitNotFoundPosition invalid message ',Msg.Msg);
    {$ENDIF}
    exit;
  end;
  Dir:=AppendPathDelim(TrimFilename(Msg.Directory));
  if Dir='' then exit;

  Msg.GetSourcePosition(Filename,Line,Col);
  Filename:=TrimFilename(Filename);
  MissingUnitname:=REVar(2);
  UsedByUnit:='';
  if REMatches(Msg.Msg,'Can(''t| not) find unit ([a-z_.0-9]+) used by ([a-z_.0-9]+)','I')
  then begin
    UsedByUnit:=REVar(3);
    {$IFDEF VerboseQuickFixUnitNotFoundPosition}
    debugln(['TQuickFixUnitNotFoundPosition.Execute Missing="',MissingUnitname,'" used by "',UsedByUnit,'"']);
    {$ENDIF}

    if (CompareFilenames(ExtractFileName(Filename),'staticpackages.inc')=0)
    and IsFileInIDESrcDir(Dir+'test') then begin
      // common case: when building the IDE a package unit is missing
      // staticpackages.inc(1,1) Fatal: Can't find unit sqldblaz used by Lazarus
      // change to lazarus.pp(1,1)
      NewFilename:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)+'ide'+PathDelim;
      Msg.SetSourcePosition(NewFilename,1,1);
      Msg.Msg:='lazarus.pp(1,1) Fatal: Can''t find a valid '+MissingUnitname+'.ppu';
      Dir:=AppendPathDelim(TrimFilename(Msg.Directory));
    end else if SysUtils.CompareText(ExtractFileNameOnly(Filename),UsedByUnit)<>0
    then begin
      // the message belongs to another unit
      NewFilename:='';
      if FilenameIsAbsolute(Dir) then
      begin
        // For example: /path/laz/main.pp(1,1) Fatal: Can't find unit lazreport used by lazarus
        // => search source lazarus in directory
        NewFilename:=CodeToolBoss.DirectoryCachePool.FindUnitInDirectory(
                                                 Dir,UsedByUnit,true);
      end;
      if NewFilename='' then begin
        NewFilename:=LazarusIDE.FindUnitFile(UsedByUnit);
        if NewFilename='' then begin
          {$IFDEF VerboseQuickFixUnitNotFoundPosition}
          DebugLn('QuickFixUnitNotFoundPosition unit not found: ',UsedByUnit);
          {$ENDIF}
          //ShowError('QuickFix: UnitNotFoundPosition unit not found: '+UsedByUnit);
        end;
      end;
    end;
  end;

  // load source
  CodeBuf:=nil;
  if (NewFilename='') and (FilenameIsAbsolute(Filename)) then
    NewFilename:=Filename;
  if NewFilename<>'' then begin
    CodeBuf:=CodeToolBoss.LoadFile(NewFilename,false,false);
    if CodeBuf=nil then begin
      {$IFDEF VerboseQuickFixUnitNotFoundPosition}
      DebugLn('QuickFixUnitNotFoundPosition unable to load unit: ',NewFilename);
      {$ENDIF}
      //ShowError('QuickFix: UnitNotFoundPosition unable to load unit: '+NewFilename);
    end;
  end else begin
    {$IFDEF VerboseQuickFixUnitNotFoundPosition}
    DebugLn('QuickFixUnitNotFoundPosition unable to locate UsedByUnit: ',UsedByUnit);
    {$ENDIF}
  end;

  // fix line and column
  Owners:=nil;
  UsedByOwner:=nil;
  try
    if CodeBuf<>nil then begin
      FixSourcePos(CodeBuf,MissingUnitname,NewFilename,Dir);
      Owners:=PackageEditingInterface.GetOwnersOfUnit(NewFilename);
      if (Owners<>nil) and (Owners.Count>0) then
        UsedByOwner:=TObject(Owners[0]);
    end;

    // if the ppu is there then improve the message
    {$IFDEF VerboseQuickFixUnitNotFoundPosition}
    debugln(['TQuickFixUnitNotFoundPosition.Execute Dir=',Dir]);
    {$ENDIF}
    if FilenameIsAbsolute(Dir) then begin
      PPUFilename:=CodeToolBoss.DirectoryCachePool.FindCompiledUnitInCompletePath(
                                                             Dir,MissingUnitname);
      {$IFDEF VerboseQuickFixUnitNotFoundPosition}
      debugln(['TQuickFixUnitNotFoundPosition.Execute PPUFilename=',PPUFilename,' IsFileInIDESrcDir=',IsFileInIDESrcDir(Dir+'test')]);
      {$ENDIF}
      PkgName:='';
      OnlyInstalled:=IsFileInIDESrcDir(Dir+'test');
      if OnlyInstalled and (PPUFilename='') then begin
        FindPPUInInstalledPkgs(MissingUnitname,PPUFilename,PkgName);
      end;

      FindPackage(MissingUnitname,PkgName,OnlyInstalled);
      if PPUFilename<>'' then begin
        // there is a ppu file in the unit path
        if PPUFilename<>'' then begin
          // there is a ppu file, but the compiler didn't like it
          // => change message
          s:='Can not find '+MissingUnitname;
          if UsedByUnit<>'' then
            s+=' used by '+UsedByUnit;
          s+=', ppu='+CreateRelativePath(PPUFilename,Dir);
          if PkgName<>'' then
            s+=', package '+PkgName;
        end else if PkgName<>'' then begin
          // ppu is missing, but the package is known
          // => change message
          s:='Can''t find ppu of unit '+MissingUnitname;
          if UsedByUnit<>'' then
            s+=' used by '+UsedByUnit;
          s+='. Maybe package '+PkgName+' needs a clean rebuild.';
        end;
      end else begin
        // there is no ppu file in the unit path
        s:='Can not find unit '+MissingUnitname;
        if UsedByUnit<>'' then
          s+=' used by '+UsedByUnit;
        if (UsedByOwner is TIDEPackage)
        and (CompareTextCT(TIDEPackage(UsedByOwner).Name,PkgName)=0) then
        begin
          // unit of package is not found by a unit of package
          s+='. Check search path of package '+TIDEPackage(UsedByOwner).Name
            +' and unit cycles.';
        end else begin
          if PkgName<>'' then
            s+='. Check if package '+PkgName+' is in the dependencies';
          if UsedByOwner is TLazProject then
            s+=' of the project inspector'
          else if UsedByOwner is TIDEPackage then
            s+=' of package '+TIDEPackage(UsedByOwner).Name;
        end;
        s+='.';
      end;
      Msg.GetSourcePosition(Filename,Line,Col);
      Msg.Msg:=CreateRelativePath(Filename,Msg.Directory)
          +'('+IntToStr(Line)+','+IntToStr(Col)+') Fatal: '+s;
      {$IFDEF VerboseQuickFixUnitNotFoundPosition}
      debugln(['TQuickFixUnitNotFoundPosition.Execute Msg.Msg="',Msg.Msg,'"']);
      {$ENDIF}
    end;
  finally
    Owners.Free;
  end;
end;

{ TQuickFixJumpToLinkerUndefinedReference }

constructor TQuickFixJumpToLinkerUndefinedReference.Create;
var
  re: String;
begin
  Name:='Linker: undefined reference to';
  Steps:=[imqfoJump];
  re:='((.*:[0-9]+)?: .* `(.*)'')'; // e.g. some.inc:37: undefined reference to `DoesNotExist'
  re+='|((.*)\(\.text.*?\): .* `([A-Z0-9_$]+)'':)'; // e.g. unit1.o(.text+0x1a): In function `SubProc':
  re+='|((.*)\.o: .* `([A-Z0-9_$]+)'':)'; // e.g. unit1.o: In function `SubProc':
  re:='^'+re+'$';
  RegExpression:=re;
end;

procedure TQuickFixJumpToLinkerUndefinedReference.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
{ Examples:
  /usr/lib/fpc/2.1.1/units/i386-linux/gtk2/gtk2.o(.text+0xbba1): In function `GTK2_GTK_TYPE_CELL_RENDERER_COMBO$$LONGWORD':
  : undefined reference to `gtk_cell_renderer_combo_get_type'

  unit1.o(.text+0x1a): In function `SubProc':
  some.inc:37: undefined reference to `DoesNotExist'

  unit1.o(.text+0x3a):some.inc:48: undefined reference to `DoesNotExist'

  unit1.o: In function `SubProc':
  some.inc:37: undefined reference to `DoesNotExist'
}
const
  REFileIdentifier = '^(.*\.o)(\(\.text.*?\))?: .* `([a-zA-Z0-9_$]+)'':$';
  REOptFileLineIdentifier = '^(.*:[0-9]+)?: .* `(.*)''$';
  REFileLineIdentifier = '^(.*):([0-9]+): .* `(.*)''$';

  procedure JumpTo(Line1, Line2: TIDEMessageLine);
  var
    Identifier: String;
    Filename: String;
    MangledFunction: String;
    CurProject: TLazProject;
    CodeBuf: TCodeBuffer;
    NewCode: TCodeBuffer;
    NewX, NewY, NewTopLine: integer;
    AnUnitName: String;
    SourceFilename: String;
    SourceLine: Integer;
  begin
    DebugLn('TQuickFixLinkerUndefinedReference.JumpTo START');
    Debugln('  Line1=',Line1.Msg);
    if Line2<>nil then
      Debugln('  Line2=',Line2.Msg);
    Filename:='';
    MangledFunction:='';
    Identifier:='';
    SourceFilename:='';
    SourceLine:=0;
    if REMatches(Line1.Msg,REFileIdentifier) then
    begin
      // example: unit1.o(.text+0x1a): In function `SubProc':
      Filename:=REVar(1);
      MangledFunction:=REVar(3);
      if (Line2<>nil) and REMatches(Line2.Msg,'^: .* `(.*)''$') then begin
        // example: ": undefined reference to `gtk_cell_renderer_combo_get_type'"
        Identifier:=REVar(1);
      end else if (Line2<>nil)
      and REMatches(Line2.Msg,REFileLineIdentifier) then begin
        // example: unit1.pas:37: undefined reference to `DoesNotExist'
        SourceFilename:=REVar(1);
        SourceLine:=StrToIntDef(REVar(2),0);
        Identifier:=REVar(3);
      end else begin
        DebugLn('TQuickFixLinkerUndefinedReference.JumpTo Line2 does not match: "',Line2.Msg,'"');
        exit;
      end;
    end
    else if REMatches(Line1.Msg,'^(.*)\(\.text.*?\):(.*):([0-9]*): .* `([a-zA-Z0-9_$]+)'':$')
    then begin
      // example: unit1.o(.text+0x3a):unit1.pas:48: undefined reference to `DoesNotExist'
      Filename:=REVar(1);
      SourceFilename:=REVar(2);
      SourceLine:=StrToIntDef(REVar(3),0);
      Identifier:=REVar(4);
    end
    else if REMatches(Line1.Msg,REFileLineIdentifier) then
    begin
      // example: unit1.pas:48: undefined reference to `DoesNotExist'
      Filename:=REVar(1);
      SourceFilename:=Filename;
      SourceLine:=StrToIntDef(REVar(2),0);
      Identifier:=REVar(3);
    end else begin
      DebugLn('JumpTo Line1 does not match: "',Line1.Msg,'"');
      exit;
    end;
    Filename:=TrimFilename(Filename); // for example uni1.o
    SourceFilename:=TrimFilename(SourceFilename); // for example wsimagelist.pp
    // Beware: SourceFilename might be an include file
    // Linker errors are emitted when compiling the project
    // => search the unit of the .o file
    DebugLn(['TQuickFixLinkerUndefinedReference.JumpTo Filename="',Filename,'" MangledFunction="',MangledFunction,'" Identifier="',Identifier,'" SourceFilename="',SourceFilename,'" SourceLine=',SourceLine]);
    CurProject:=LazarusIDE.ActiveProject;
    if CurProject=nil then begin
      ShowError('QuickFix: LinkerUndefinedReference no project');
      exit;
    end;
    if (CurProject.MainFile=nil) then begin
      ShowError('QuickFix: LinkerUndefinedReference no main file in project');
      exit;
    end;
    CodeBuf:=CodeToolBoss.LoadFile(CurProject.MainFile.Filename,true,false);
    if (CodeBuf=nil) then begin
      ShowError('QuickFix: LinkerUndefinedReference project main file has no source');
      exit;
    end;
    AnUnitName:=ExtractFilenameOnly(Filename);
    CodeBuf:=CodeToolBoss.FindUnitSource(CodeBuf,AnUnitName,'');
    if (CodeBuf=nil) then begin
      ShowError('QuickFix: LinkerUndefinedReference unit not found: '+AnUnitName);
      exit;
    end;
    if not CodeToolBoss.JumpToLinkerIdentifier(CodeBuf,
      SourceFilename,SourceLine,MangledFunction,Identifier,
      NewCode,NewX,NewY,NewTopLine)
    then begin
      if CodeToolBoss.ErrorCode<>nil then
        LazarusIDE.DoJumpToCodeToolBossError
      else
        ShowError('QuickFix: LinkerUndefinedReference function not found: '+MangledFunction+' Identifier='+Identifier);
      exit;
    end;
    LazarusIDE.DoOpenFileAndJumpToPos(NewCode.Filename,Point(NewX,NewY),
                                      NewTopLine,-1,-1,[]);
  end;
  
var
  MsgBefore: TIDEMessageLine;
  MsgAfter: TIDEMessageLine;
begin
  inherited Execute(Msg, Step);
  if Step=imqfoJump then begin
    DebugLn(['TQuickFixLinkerUndefinedReference.Execute ',Msg.Msg]);
    MsgBefore:=nil;
    if Msg.Position>0 then
      MsgBefore:=IDEMessagesWindow[Msg.Position-1];
    MsgAfter:=nil;
    if Msg.Position+1<IDEMessagesWindow.LinesCount then
      MsgAfter:=IDEMessagesWindow[Msg.Position+1];
    if REMatches(Msg.Msg,'^(.*)\(\.text.*?\):.*:([0-9]+): .* `([a-zA-Z0-9_$]+)''$')
    then begin
      // example:
      //   unit1.o(.text+0x3a):unit1.pas:48: undefined reference to `DoesNotExist'
      JumpTo(Msg,nil)
    end
    else if REMatches(Msg.Msg,REFileIdentifier) then
    begin
      // example:
      // -> unit1.o(.text+0x1a): In function `SubProc':
      //    some.inc:37: undefined reference to `DoesNotExist'
      JumpTo(Msg,MsgAfter);
    end
    else if (MsgBefore<>nil)
    and REMatches(MsgBefore.Msg,REFileIdentifier)
    and REMatches(Msg.Msg,REOptFileLineIdentifier) then begin
      // example:
      //    unit1.o(.text+0x1a): In function `SubProc':
      // -> some.inc:37: undefined reference to `DoesNotExist'
      JumpTo(MsgBefore,Msg)
    end
    else if REMatches(Msg.Msg,REFileLineIdentifier) then begin
      // example:
      //   some.inc:37: undefined reference to `DoesNotExist'
      JumpTo(Msg,nil);
    end;
  end;
end;

{ TQuickFixClassWithAbstractMethods }

constructor TQuickFixClassWithAbstractMethods.Create;
begin
  Name:='Show abstract methods';
  Caption:=srkmecShowAbstractMethods;
  Steps:=[imqfoMenuItem];
end;

function TQuickFixClassWithAbstractMethods.IsApplicable(Line: TIDEMessageLine
  ): boolean;
begin
  Result:=(Line.Parts<>nil)
          and (System.Pos(') Warning: Constructing a class "',Line.Msg)>0)
          and (System.Pos('" with abstract methods',Line.Msg)>0);
end;

procedure TQuickFixClassWithAbstractMethods.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
var
  CodeBuf: TCodeBuffer;
  Caret: TPoint;
  Filename: string;
  NewCode: TCodeBuffer;
  NewX,NewY,NewTopLine: Integer;
  Tool: TCodeTool;
  CurClassName: String;
begin
  if Step=imqfoMenuItem then begin
    DebugLn(['TQuickFixClassWithAbstractMethods.Execute ']);
    // get source position
    // (FPC reports position right after the constructor call
    //  for example right after TStrings.Create)
    if not GetMsgLineFile(Msg,CodeBuf,false) then exit;
    Msg.GetSourcePosition(Filename,Caret.Y,Caret.X);
    if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFixClassWithAbstractMethods.Execute failed because IDE busy']);
      exit;
    end;

    // get class name
    if not REMatches(Msg.Msg,'Warning: Constructing a class "([a-z_0-9]+)"','I') then begin
      DebugLn('QuickFixClassWithAbstractMethods invalid message ',Msg.Msg);
      ShowError('QuickFix: ClassWithAbstractMethods invalid message '+Msg.Msg);
      exit;
    end;
    CurClassName:=REVar(1);
    //DebugLn(['TQuickFixClassWithAbstractMethods.Execute Class=',CurClassName]);

    // find the class

    // build the tree
    CodeToolBoss.Explore(CodeBuf,Tool,false,true);
    if Tool=nil then begin
      DebugLn(['TQuickFixClassWithAbstractMethods.Execute no tool for ',CodeBuf.Filename]);
      ShowError('QuickFix: ClassWithAbstractMethods no tool for '+CodeBuf.Filename);
      exit;
    end;

    if not CodeToolBoss.FindDeclarationOfIdentifier(CodeBuf,Caret.X,Caret.Y,
      @CurClassName[1],NewCode,NewX,NewY,NewTopLine)
    then begin
      if CodeToolBoss.ErrorMessage<>'' then begin
        LazarusIDE.DoJumpToCodeToolBossError
      end else begin
        IDEMessageDialog('Class not found',
          'Class '+CurClassName+' not found at '
          +CodeBuf.Filename+'('+IntToStr(Caret.Y)+','+IntToStr(Caret.X)+')',
          mtError,[mbCancel]);
      end;
      exit;
    end;
    //DebugLn(['TQuickFixClassWithAbstractMethods.Execute Declaration at ',NewCode.Filename,' ',NewX,',',NewY]);

    if LazarusIDE.DoOpenFileAndJumpToPos(NewCode.Filename,
      Point(NewX,NewY),NewTopLine,-1,-1,[])<>mrOk
    then begin
      DebugLn(['TQuickFixClassWithAbstractMethods.Execute failed opening ',NewCode.Filename]);
      ShowError('QuickFix: ClassWithAbstractMethods failed opening '+NewCode.Filename);
      exit;
    end;

    ShowAbstractMethodsDialog;
  end;
end;

{ TQuickFixIdentifierNotFoundAddLocal }

constructor TQuickFixIdentifierNotFoundAddLocal.Create;
begin
  Name:='Create local variable: Error: Identifier not found "identifier"';
  Caption:=lisQuickFixCreateLocalVariable;
  Steps:=[imqfoMenuItem];
end;

function TQuickFixIdentifierNotFoundAddLocal.IsApplicable(Line: TIDEMessageLine
  ): boolean;
// FPC gives position of end of identifier
const
  SearchStr = ') Error: Identifier not found "';
var
  Filename: string;
  Caret: TPoint;
  Code: TCodeBuffer;
  Tool: TCodeTool;
  CleanPos: integer;
  p: LongInt;
  Msg: String;
  Identifier: String;
  Node: TCodeTreeNode;
begin
  Result:=false;
  if (Line.Parts=nil) then exit;
  Msg:=Line.Msg;
  p:=System.Pos(SearchStr,Msg);
  if p<1 then exit;
  inc(p,length(SearchStr));
  Line.GetSourcePosition(Filename,Caret.Y,Caret.X);
  if (Filename='') or (Caret.X<1) or (Caret.Y<1) then exit;
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then exit;
  if not CodeToolBoss.Explore(Code,Tool,false) then exit;
  if Tool.CaretToCleanPos(CodeXYPosition(Caret.X,Caret.Y,Code),CleanPos)<>0 then exit;
  Node:=Tool.FindDeepestNodeAtPos(CleanPos,false);
  if Node=nil then exit;
  if not (Node.Desc in AllPascalStatements) then exit;
  Tool.MoveCursorToCleanPos(CleanPos);
  Tool.ReadPriorAtom;
  Identifier:=GetIdentifier(@Msg[p]);
  if not Tool.AtomIs(Identifier) then exit;
  Tool.ReadPriorAtom;
  if (Tool.CurPos.Flag in [cafPoint,cafRoundBracketClose,cafEdgedBracketClose,
                           cafEnd])
  then exit;
  Result:=true;
end;

procedure TQuickFixIdentifierNotFoundAddLocal.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
var
  Identifier: String;
  CodeBuf: TCodeBuffer;
  Filename: string;
  Caret: TPoint;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  Tool: TCodeTool;
  CleanPos: integer;
  CodeXY: TCodeXYPosition;
begin
  if Step=imqfoMenuItem then begin
    DebugLn(['TQuickFixIdentifierNotFoundAddLocal.Execute Dir=',Msg.Directory,' Msg=',Msg.Msg]);
    // get source position
    // (FPC reports position right after the unknown identifier
    //  for example right after FilenameIsAbsolute)
    if not GetMsgLineFile(Msg,CodeBuf,false) then exit;
    Msg.GetSourcePosition(Filename,Caret.Y,Caret.X);

    if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFixIdentifierNotFoundAddLocal.Execute failed because IDE busy']);
      exit;
    end;

    if (Filename='') or (Caret.X<1) or (Caret.Y<1) then exit;
    CodeBuf:=CodeToolBoss.LoadFile(Filename,true,false);
    if CodeBuf=nil then exit;
    if not CodeToolBoss.Explore(CodeBuf,Tool,false) then exit;
    if Tool.CaretToCleanPos(CodeXYPosition(Caret.X,Caret.Y,CodeBuf),CleanPos)<>0 then exit;
    if CleanPos>Tool.SrcLen then CleanPos:=Tool.SrcLen;
    if (CleanPos>1) and (not IsIdentChar[Tool.Src[CleanPos]]) then dec(CleanPos);
    Tool.MoveCursorToCleanPos(CleanPos);
    Tool.ReadPriorAtom;
    CleanPos:=Tool.CurPos.StartPos;
    if CleanPos<1 then exit;
    if not Tool.CleanPosToCaret(CleanPos,CodeXY) then exit;

    // get identifier
    if not REMatches(Msg.Msg,'Error: Identifier not found "([a-z_0-9]+)"','I') then begin
      DebugLn('TQuickFixIdentifierNotFoundAddLocal invalid message ',Msg.Msg);
      ShowError('QuickFix: IdentifierNotFoundAddLocal invalid message '+Msg.Msg);
      exit;
    end;
    Identifier:=REVar(1);
    DebugLn(['TQuickFixIdentifierNotFoundAddLocal.Execute Identifier=',Identifier,' ',Dbgs(CodeXY)]);

    if not IsIdentifierInCode(CodeXY.Code,CodeXY.X,CodeXY.Y,Identifier,
      Identifier+' not found in '+CodeBuf.Filename
         +' at line '+IntToStr(Caret.Y)+', column '+IntToStr(Caret.X)+'.'
         +LineEnding+'Maybe the message is outdated.')
    then exit;

    if not CodeToolBoss.CreateVariableForIdentifier(CodeXY.Code,CodeXY.X,CodeXY.Y,-1,
               NewCode,NewX,NewY,NewTopLine)
    then begin
      LazarusIDE.DoJumpToCodeToolBossError;
      exit;
    end;

    // message fixed -> clean
    Msg.Msg:='';
  end;
end;

{ TQuickFixUnitNotFound_Remove }

constructor TQuickFixUnitNotFound_Remove.Create;
begin
  Name:='Search unit: Error: Can''t find unit Name';
  Caption:=lisRemoveUnitFromUsesSection;
  Steps:=[imqfoMenuItem];
end;

function TQuickFixUnitNotFound_Remove.IsApplicable(Line: TIDEMessageLine
  ): boolean;
var
  Msg: String;
begin
  if Line.Parts=nil then exit(false);
  Msg:=Line.Msg;
  Result:=(System.Pos(') Fatal: Can''t find unit ',Msg)>0)
       or (System.Pos(') Fatal: Can not find unit ',Msg)>0);
end;

procedure TQuickFixUnitNotFound_Remove.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
var
  CodeBuf: TCodeBuffer;
  Filename: string;
  Caret: TPoint;
  AnUnitName: String;
begin
  if Step=imqfoMenuItem then begin
    DebugLn(['TQuickFixUnitNotFound_Remove.Execute ']);
    // get source position
    if not GetMsgLineFile(Msg,CodeBuf,false) then exit;
    Msg.GetSourcePosition(Filename,Caret.Y,Caret.X);
    if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFixUnitNotFound_Remove.Execute failed because IDE busy']);
      exit;
    end;

    // get unitname
    if not REMatches(Msg.Msg,'Fatal: Can(''t| not) find unit ([a-z_0-9]+) ','I') then begin
      DebugLn('TQuickFixUnitNotFound_Remove invalid message ',Msg.Msg);
      ShowError('QuickFix: UnitNotFound_Remove invalid message '+Msg.Msg);
      exit;
    end;
    AnUnitName:=REVar(2);
    DebugLn(['TQuickFixUnitNotFound_Remove.Execute Unit=',AnUnitName]);

    if (AnUnitName='') or (not IsValidIdent(AnUnitName)) then begin
      DebugLn(['TQuickFixUnitNotFound_Remove.Execute not an identifier "',dbgstr(AnUnitName),'"']);
      ShowError('QuickFix: UnitNotFound_Remove not an identifier "'+dbgstr(AnUnitName)+'"');
      exit;
    end;

    if not CodeToolBoss.RemoveUnitFromAllUsesSections(CodeBuf,AnUnitName) then
    begin
      DebugLn(['TQuickFixUnitNotFound_Remove.Execute RemoveUnitFromAllUsesSections failed']);
      LazarusIDE.DoJumpToCodeToolBossError;
      exit;
    end;

    // message fixed -> clean
    Msg.Msg:='';
  end;
end;

{ TQuickFixLocalVariableNotUsed_Remove }

constructor TQuickFixLocalVariableNotUsed_Remove.Create;
begin
  Name:='Remove local variable: Note: Local variable "x" not used';
  Caption:=lisRemoveLocalVariable2;
  Steps:=[imqfoMenuItem];
end;

function TQuickFixLocalVariableNotUsed_Remove.IsApplicable(Line: TIDEMessageLine
  ): boolean;
const
  SearchStr1 = ') Note: Local variable "';
  SearchStr2 = '" not used';
var
  Msg: String;
  StartPos: integer;
  p: LongInt;
  Variable: String;
begin
  Result:=false;
  if (Line.Parts=nil) then exit;
  Msg:=Line.Msg;
  //DebugLn(['TQuickFixLocalVariableNotUsed_Remove.IsApplicable Msg="',Msg,'" ',System.Pos(SearchStr1,Msg),' ',System.Pos(SearchStr2,Msg)]);
  StartPos:=System.Pos(SearchStr1,Msg);
  if StartPos<1 then exit;
  inc(StartPos,length(SearchStr1));
  p:=StartPos;
  while (p<=length(Msg)) and (Msg[p]<>'"') do inc(p);
  if copy(Msg,p,length(SearchStr2))<>SearchStr2 then exit;
  Variable:=copy(Msg,StartPos,p-StartPos);
  if (Variable='') or not IsValidIdent(Variable) then exit;
  Caption:=Format(lisRemoveLocalVariable, [Variable]);
  Result:=true;
end;

procedure TQuickFixLocalVariableNotUsed_Remove.Execute(
  const Msg: TIDEMessageLine; Step: TIMQuickFixStep);
var
  CodeBuf: TCodeBuffer;
  Filename: string;
  Caret: TPoint;
  Variable: String;
begin
  if Step=imqfoMenuItem then begin
    DebugLn(['TQuickFixLocalVariableNotUsed_Remove.Execute ']);
    // get source position
    if not GetMsgLineFile(Msg,CodeBuf,false) then exit;
    Msg.GetSourcePosition(Filename,Caret.Y,Caret.X);
    if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFixLocalVariableNotUsed_Remove.Execute failed because IDE busy']);
      exit;
    end;

    // get variables name
    if not REMatches(Msg.Msg,'Note: Local variable "([a-z_0-9]+)" not used','I')
    then begin
      DebugLn('TQuickFixLocalVariableNotUsed_Remove invalid message ',Msg.Msg);
      ShowError('QuickFix: LocalVariableNotUsed_Remove invalid message '+Msg.Msg);
      exit;
    end;
    Variable:=REVar(1);
    //DebugLn(['TQuickFixLocalVariableNotUsed_Remove.Execute Variable=',Variable]);

    // check if the variable is at that position
    if not IsIdentifierInCode(CodeBuf,Caret.X,Caret.Y,Variable,
      Variable+' not found in '+CodeBuf.Filename
         +' at line '+IntToStr(Caret.Y)+', column '+IntToStr(Caret.X)+'.'+LineEnding
         +'Maybe the message is outdated.')
    then exit;

    if not CodeToolBoss.RemoveIdentifierDefinition(CodeBuf,Caret.X,Caret.Y) then
    begin
      DebugLn(['TQuickFixLocalVariableNotUsed_Remove.Execute remove failed']);
      LazarusIDE.DoJumpToCodeToolBossError;
      exit;
    end;

    // message fixed -> clean
    Msg.Msg:='';
  end;
end;

{ TQuickFixHint_Hide }

constructor TQuickFixHint_Hide.Create;
begin
  Name:='Hide hint, note or warning';
  Caption:=lisHideMessageViaDirective;
  Steps:=[imqfoMenuItem];
end;

function TQuickFixHint_Hide.IsApplicable(Line: TIDEMessageLine): boolean;
var
  MsgType: string;
  Filename: string;
  LineNumber, Column: integer;
begin
  Result:=false;
  if (Line.Parts=nil) then exit;
  MsgType:=Line.Parts.Values['Type'];
  if (MsgType<>'Hint') and (MsgType<>'Note') and (MsgType<>'Warning') then exit;
  Line.GetSourcePosition(Filename,LineNumber,Column);
  Result:=FilenameIsAbsolute(Filename) and (LineNumber>=1) and (Column>=1);
end;

procedure TQuickFixHint_Hide.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
var
  CodeBuf: TCodeBuffer;
  Filename: string;
  Caret: TPoint;
  p: integer;
begin
  if Step=imqfoMenuItem then begin
    DebugLn(['TQuickFixHint_Hide.Execute ']);
    // get source position
    if not GetMsgLineFile(Msg,CodeBuf,false) then exit;
    Msg.GetSourcePosition(Filename,Caret.Y,Caret.X);
    if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFixHint_Hide.Execute failed because IDE busy']);
      exit;
    end;

    CodeBuf.LineColToPosition(Caret.Y,Caret.X,p);
    if p<1 then begin
      DebugLn(['TQuickFixHint_Hide.Execute failed because invalid line, column']);
      IDEMessageDialog(lisCCOErrorCaption,
        Format(lisInvalidLineColumnInMessage, [LineEnding, Msg.Msg]),
        mtError, [mbCancel]);
      exit;
    end;

    CodeBuf.Insert(p,'{%H-}');
  end;
end;

end.

