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
    Defines the standard message Quick Fix menu items.

  ToDo:
    - There is no method in an ancestor class to be overriden:
      1. option: if the ancestor has a function with the same name: update the parameter list
      2. option: remove the method
      3. option: add a virtual method to the ancestor
    - complete function implementations with missing parameters

}
unit MsgQuickFixes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls, Dialogs, FileUtil,
  BasicCodeTools, CodeTree, CodeAtom, CodeCache, CodeToolManager,
  IDEMsgIntf, TextTools, ProjectIntf, LazIDEIntf,
  AbstractsMethodsDlg, LazarusIDEStrConsts;
  
type

  { TQuickFixIdentifierNotFoundAddLocal }

  TQuickFixIdentifierNotFoundAddLocal = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

  { TQuickFixUnitNotFoundPosition }

  TQuickFixUnitNotFoundPosition = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;
  
  { TQuickFixUnitNotFound_Remove }

  TQuickFixUnitNotFound_Remove = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
  end;

  { TQuickFixLinkerUndefinedReference }

  TQuickFixLinkerUndefinedReference = class(TIDEMsgQuickFixItem)
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

function GetMsgLineFilename(Msg: TIDEMessageLine;
                            out CodeBuf: TCodeBuffer; Quiet: boolean): boolean;

procedure InitStandardIDEQuickFixItems;
procedure FreeStandardIDEQuickFixItems;

implementation

procedure ShowError(Msg: string);
begin
  MessageDlg('QuickFix error',Msg,mtError,[mbCancel],0);
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
  if not GetMsgLineFilename(Msg,CodeBuf,false) then exit;
  
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

function GetMsgLineFilename(Msg: TIDEMessageLine; out CodeBuf: TCodeBuffer;
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
      MessageDlg(lisCCOErrorCaption,
        Format(lisMessageContainsNoFilePositionInformation, [#13, Msg.Msg]),
          mtError, [mbCancel], 0);
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
      MessageDlg(lisCCOErrorCaption,
        Format(lisUnableToLoadFile, [#13, Filename]), mtError, [mbCancel], 0);
    end;
    exit;
  end;
  Result:=true;
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
  RegisterIDEMsgQuickFix(TQuickFixLinkerUndefinedReference.Create);
  RegisterIDEMsgQuickFix(TQuickFixClassWithAbstractMethods.Create);
  RegisterIDEMsgQuickFix(TQuickFixIdentifierNotFoundAddLocal.Create);
  RegisterIDEMsgQuickFix(TQuickFixLocalVariableNotUsed_Remove.Create);
  RegisterIDEMsgQuickFix(TQuickFixHint_Hide.Create);
end;

procedure FreeStandardIDEQuickFixItems;
begin
  FreeThenNil(IDEMsgQuickFixes);
end;

{ TQuickFixUnitNotFoundPosition }

constructor TQuickFixUnitNotFoundPosition.Create;
begin
  Name:='Improve error position of: Fatal: Can''t find unit xxx';
  Steps:=[imqfoImproveMessage];
end;

function TQuickFixUnitNotFoundPosition.IsApplicable(Line: TIDEMessageLine
  ): boolean;
begin
  Result:=(Line.Parts<>nil)
          and (System.Pos(') Fatal: Can''t find unit ',Line.Msg)>0);
end;

procedure TQuickFixUnitNotFoundPosition.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
var
  CodeBuf: TCodeBuffer;
  MissingUnitname: String;
  NamePos, InPos: Integer;
  Line, Col: Integer;
  UsedByUnit: String;
  NewFilename: String;
begin
  if Step<>imqfoImproveMessage then exit;
  //DebugLn('QuickFixUnitNotFoundPosition ');

  if not REMatches(Msg.Msg,'Can''t find unit ([a-z_0-9]+)','I') then begin
    DebugLn('QuickFixUnitNotFoundPosition invalid message ',Msg.Msg);
    exit;
  end;
  MissingUnitname:=REVar(1);
  if REMatches(Msg.Msg,'Can''t find unit ([a-z_0-9]+) used by ([a-z_0-9]+)','I')
  then begin
    UsedByUnit:=REVar(2);
    if SysUtils.CompareText(UsedByUnit,MissingUnitname)<>0 then
    begin
      // the message belongs to another unit
      NewFilename:=LazarusIDE.FindUnitFile(UsedByUnit);
      if NewFilename='' then begin
        DebugLn('QuickFixUnitNotFoundPosition unit not found: ',UsedByUnit);
        //ShowError('QuickFix: UnitNotFoundPosition unit not found: '+UsedByUnit);
        exit;
      end;
      CodeBuf:=CodeToolBoss.LoadFile(NewFilename,false,false);
      if CodeBuf=nil then begin
        DebugLn('QuickFixUnitNotFoundPosition unable to load unit: ',NewFilename);
        //ShowError('QuickFix: UnitNotFoundPosition unable to load unit: '+NewFilename);
        exit;
      end;
    end;
  end;
  if CodeBuf=nil then exit;
  LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
  if not CodeToolBoss.FindUnitInAllUsesSections(CodeBuf,MissingUnitname,
    NamePos,InPos)
  then begin
    DebugLn('QuickFixUnitNotFoundPosition failed due to syntax errors or '+MissingUnitname+' is not used in '+CodeBuf.Filename);
    //LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;
  if InPos=0 then ;
  CodeBuf.AbsoluteToLineCol(NamePos,Line,Col);
  if (Line>0) and (Col>0) then begin
    //DebugLn('QuickFixUnitNotFoundPosition Line=',dbgs(Line),' Col=',dbgs(Col));
    NewFilename:=CodeBuf.Filename;
    if (Msg.Directory<>'') and (FilenameIsAbsolute(Msg.Directory)) then
      NewFilename:=CreateRelativePath(NewFilename,Msg.Directory);
    Msg.SetSourcePosition(NewFilename,Line,Col);
  end;
end;

{ TQuickFixLinkerUndefinedReference }

constructor TQuickFixLinkerUndefinedReference.Create;
begin
  Name:='Linker: undefined reference to';
  Steps:=[imqfoJump];
  RegExpression:='^((.*:[0-9]+)?: .* `(.*)'')|((.*)\(\.text.*?\): .* `([A-Z0-9_$]+)'':)$';
end;

procedure TQuickFixLinkerUndefinedReference.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
{ Examples:
  /usr/lib/fpc/2.1.1/units/i386-linux/gtk2/gtk2.o(.text+0xbba1): In function `GTK2_GTK_TYPE_CELL_RENDERER_COMBO$$LONGWORD':
  : undefined reference to `gtk_cell_renderer_combo_get_type'

  unit1.o(.text+0x1a): In function `SubProc':
  unit1.pas:37: undefined reference to `DoesNotExist'
  unit1.o(.text+0x3a):unit1.pas:48: undefined reference to `DoesNotExist'
}

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
    DebugLn(['TQuickFixLinkerUndefinedReference.JumpTo START ',Line1.Msg]);
    Filename:='';
    MangledFunction:='';
    Identifier:='';
    SourceFilename:='';
    SourceLine:=0;
    if REMatches(Line1.Msg,'^(.*)\(\.text.*?\): .* `([a-zA-Z0-9_$]+)'':$') then
    begin
      // example: unit1.o(.text+0x1a): In function `SubProc':
      Filename:=REVar(1);
      MangledFunction:=REVar(2);
      if (Line2<>nil) and REMatches(Line2.Msg,'^: .* `(.*)''$') then begin
        // example: ": undefined reference to `gtk_cell_renderer_combo_get_type'"
        Identifier:=REVar(1);
      end else if (Line2<>nil)
      and REMatches(Line2.Msg,'^(.*):([0-9]+): .* `(.*)''$') then begin
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
    else if REMatches(Line1.Msg,'^(.*):([0-9]+): .* `([a-zA-Z0-9_$]+)''$') then begin
      // example: unit1.pas:48: undefined reference to `DoesNotExist'
      Filename:=REVar(1);
      SourceFilename:=Filename;
      SourceLine:=StrToIntDef(REVar(2),0);
      Identifier:=REVar(3);
    end else begin
      DebugLn('JumpTo Line1 does not match: "',Line1.Msg,'"');
      exit;
    end;
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
  
begin
  inherited Execute(Msg, Step);
  if Step=imqfoJump then begin
    //DebugLn(['TQuickFixLinkerUndefinedReference.Execute ',Msg.Msg]);
    if REMatches(Msg.Msg,'^(.*)\(\.text.*?\):.*:([0-9]+): .* `([a-zA-Z0-9_$]+)''$') then
      // example: unit1.o(.text+0x3a):unit1.pas:48: undefined reference to `DoesNotExist'
      JumpTo(Msg,nil)
    else if (Msg.Position>0) and REMatches(Msg.Msg,'^(.*:[0-9]+)?: .* `(.*)''$') then
      // example: unit1.pas:37: undefined reference to `DoesNotExist'
      JumpTo(IDEMessagesWindow[Msg.Position-1],Msg)
    else if (Msg.Position<IDEMessagesWindow.LinesCount-1)
    and REMatches(Msg.Msg,'^(.*)\(\.text.*?\): .* `([a-zA-Z0-9_$]+)'':$') then
      // example: unit1.o(.text+0x1a): In function `SubProc':
      JumpTo(Msg,IDEMessagesWindow[Msg.Position+1]);
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
    if not GetMsgLineFilename(Msg,CodeBuf,false) then exit;
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
        MessageDlg('Class not found',
          'Class '+CurClassName+' not found at '
          +CodeBuf.Filename+'('+IntToStr(Caret.Y)+','+IntToStr(Caret.X)+')',
          mtError,[mbCancel],0);
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
begin
  if Step=imqfoMenuItem then begin
    DebugLn(['TQuickFixIdentifierNotFoundAddLocal.Execute Dir=',Msg.Directory,' Msg=',Msg.Msg]);
    // get source position
    // (FPC reports position right after the unknown identifier
    //  for example right after FilenameIsAbsolute)
    if not GetMsgLineFilename(Msg,CodeBuf,false) then exit;
    Msg.GetSourcePosition(Filename,Caret.Y,Caret.X);

    if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFixIdentifierNotFoundAddLocal.Execute failed because IDE busy']);
      exit;
    end;

    // get identifier
    if not REMatches(Msg.Msg,'Error: Identifier not found "([a-z_0-9]+)"','I') then begin
      DebugLn('TQuickFixIdentifierNotFoundAddLocal invalid message ',Msg.Msg);
      ShowError('QuickFix: IdentifierNotFoundAddLocal invalid message '+Msg.Msg);
      exit;
    end;
    Identifier:=REVar(1);
    //DebugLn(['TQuickFixIdentifierNotFoundAddLocal.Execute Identifier=',Identifier]);

    if not IsIdentifierInCode(CodeBuf,Caret.X,Caret.Y,Identifier,
      Identifier+' not found in '+CodeBuf.Filename
         +' at line '+IntToStr(Caret.Y)+', column '+IntToStr(Caret.X)+'.'#13
         +'Maybe the message is outdated.')
    then exit;

    if not CodeToolBoss.CreateVariableForIdentifier(CodeBuf,Caret.X,Caret.Y,-1,
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
const
  SearchStr = ') Fatal: Can''t find unit ';
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
    if not GetMsgLineFilename(Msg,CodeBuf,false) then exit;
    Msg.GetSourcePosition(Filename,Caret.Y,Caret.X);
    if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFixUnitNotFound_Remove.Execute failed because IDE busy']);
      exit;
    end;

    // get unitname
    if not REMatches(Msg.Msg,'Fatal: Can''t find unit ([a-z_0-9]+) ','I') then begin
      DebugLn('TQuickFixUnitNotFound_Remove invalid message ',Msg.Msg);
      ShowError('QuickFix: UnitNotFound_Remove invalid message '+Msg.Msg);
      exit;
    end;
    AnUnitName:=REVar(1);
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
    if not GetMsgLineFilename(Msg,CodeBuf,false) then exit;
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
         +' at line '+IntToStr(Caret.Y)+', column '+IntToStr(Caret.X)+'.'#13
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
    if not GetMsgLineFilename(Msg,CodeBuf,false) then exit;
    Msg.GetSourcePosition(Filename,Caret.Y,Caret.X);
    if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFixHint_Hide.Execute failed because IDE busy']);
      exit;
    end;

    CodeBuf.LineColToPosition(Caret.Y,Caret.X,p);
    if p<1 then begin
      DebugLn(['TQuickFixHint_Hide.Execute failed because invalid line, column']);
      MessageDlg(lisCCOErrorCaption,
        Format(lisInvalidLineColumnInMessage, [#13, Msg.Msg]), mtError, [
          mbCancel], 0);
      exit;
    end;

    CodeBuf.Insert(p,'{%H-}');
  end;
end;

end.

