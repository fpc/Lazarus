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
}
unit MsgQuickFixes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, FileUtil, LCLProc, IDEMsgIntf, TextTools,
  LazarusIDEStrConsts, ProjectIntf, LazIDEIntf, CodeCache, CodeToolManager;
  
type

  { TQuickFixUnitNotFoundPosition }

  TQuickFixUnitNotFoundPosition = class(TIDEMsgQuickFixItem)
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

procedure QuickFixParameterNotUsed(Sender: TObject; Step: TIMQuickFixStep;
                                   Msg: TIDEMessageLine);
procedure QuickFixUnitNotUsed(Sender: TObject; Step: TIMQuickFixStep;
                              Msg: TIDEMessageLine);

function GetMsgLineFilename(Msg: TIDEMessageLine;
                            out CodeBuf: TCodeBuffer): boolean;

procedure InitStandardIDEQuickFixItems;
procedure FreeStandardIDEQuickFixItems;

implementation

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
begin
  if Step<>imqfoMenuItem then exit;
  if not GetMsgLineFilename(Msg,CodeBuf) then exit;
  
  if not REMatches(Msg.Msg,'Unit "([a-z_0-9]+)" not used','I') then begin
    DebugLn('QuickFixUnitNotUsed invalid message ',Msg.Msg);
    exit;
  end;
  UnneededUnitname:=REVar(1);

  // remove unit
  LazarusIDE.SaveSourceEditorChangesToCodeCache(-1);
  if not CodeToolBoss.RemoveUnitFromAllUsesSections(CodeBuf,UnneededUnitname)
  then begin
    LazarusIDE.DoJumpToCodeToolBossError;
  end;

  // message fixed
  Msg.Msg:='';
end;

function GetMsgLineFilename(Msg: TIDEMessageLine; out CodeBuf: TCodeBuffer
  ): boolean;
var
  Filename: String;
begin
  Result:=false;
  CodeBuf:=nil;
  if Msg.Parts=nil then begin
    DebugLn('GetMsgLineFilename Msg.Parts=nil');
    exit;
  end;

  Filename:=Msg.Parts.Values['Filename'];
  //DebugLn('GetMsgLineFilename Filename=',Filename,' ',Msg.Parts.Text);
  CodeBuf:=CodeToolBoss.LoadFile(Filename,false,false);
  if CodeBuf=nil then begin
    DebugLn('GetMsgLineFilename Filename "',Filename,'" not found.');
    exit;
  end;
  Result:=true;
end;

procedure InitStandardIDEQuickFixItems;
begin
  IDEMsgQuickFixes:=TIDEMsgQuickFixItems.Create;
  
  //RegisterIDEMsgQuickFix('Parameter xxx not used','Quick fix: Add dummy line',
  //  'Parameter "[a-z_0-9]+" not used',nil,@QuickFixParameterNotUsed);
  RegisterIDEMsgQuickFix('Unit xxx not used in yyy','Quick fix: Remove unit',
    'Unit "[a-z_0-9]+" not used in [a-z_0-9]+',[imqfoMenuItem],
    nil,@QuickFixUnitNotUsed);
  RegisterIDEMsgQuickFix(TQuickFixUnitNotFoundPosition.Create);
  RegisterIDEMsgQuickFix(TQuickFixLinkerUndefinedReference.Create);
end;

procedure FreeStandardIDEQuickFixItems;
begin
  FreeThenNil(IDEMsgQuickFixes);
end;

{ TQuickFixUnitNotFoundPosition }

constructor TQuickFixUnitNotFoundPosition.Create;
begin
  Name:='Fatal: Can''t find unit xxx';
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
begin
  if Step<>imqfoImproveMessage then exit;
  //DebugLn('QuickFixUnitNotFoundPosition ');
  if not GetMsgLineFilename(Msg,CodeBuf) then exit;

  if not REMatches(Msg.Msg,'Can''t find unit ([a-z_0-9]+)','I') then begin
    DebugLn('QuickFixUnitNotFoundPosition invalid message ',Msg.Msg);
    exit;
  end;
  MissingUnitname:=REVar(1);
  LazarusIDE.SaveSourceEditorChangesToCodeCache(-1);
  if not CodeToolBoss.FindUnitInAllUsesSections(CodeBuf,MissingUnitname,
    NamePos,InPos)
  then begin
    DebugLn('QuickFixUnitNotFoundPosition failed due to syntax errors');
    exit;
  end;
  if InPos=0 then ;
  CodeBuf.AbsoluteToLineCol(NamePos,Line,Col);
  if (Line>0) and (Col>0) then begin
    //DebugLn('QuickFixUnitNotFoundPosition Line=',dbgs(Line),' Col=',dbgs(Col));
    Msg.SetSourcePosition('',Line,Col);
  end;
end;

{ TQuickFixLinkerUndefinedReference }

constructor TQuickFixLinkerUndefinedReference.Create;
begin
  Name:='Linker: undefined reference to';
  Steps:=[imqfoJump];
  RegExpression:='^(: .* `(.*)'')|((.*)\(\.text.*?\): .* `([A-Z0-9_$]+)'':)$';
end;

procedure TQuickFixLinkerUndefinedReference.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
{ Example:
 /usr/lib/fpc/2.1.1/units/i386-linux/gtk2/gtk2.o(.text+0xbba1): In function `GTK2_GTK_TYPE_CELL_RENDERER_COMBO$$LONGWORD':
 undefined reference to `gtk_cell_renderer_combo_get_type'
}

  procedure Error(const Msg: string);
  begin
    DebugLn('TQuickFixLinkerUndefinedReference.Execute ',Msg);
    MessageDlg('TQuickFixLinkerUndefinedReference.Execute',
               Msg,mtError,[mbCancel],0);
  end;

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
  begin
    DebugLn(['JumpTo START ',Line1.Msg]);
    if not REMatches(Line1.Msg,'^(.*)\(\.text.*?\): .* `([A-Z0-9_$]+)'':$')
    then
      exit;
    Filename:=REVar(1);
    MangledFunction:=REVar(2);
    if not REMatches(Line2.Msg,'^: .* `(.*)''$') then exit;
    Identifier:=REVar(1);
    DebugLn(['TQuickFixLinkerUndefinedReference.JumpTo Filename="',Filename,'" MangledFunction="',MangledFunction,'" Identifier="',Identifier,'"']);
    CurProject:=LazarusIDE.ActiveProject;
    if CurProject=nil then begin
      Error('no project');
      exit;
    end;
    if (CurProject.MainFile=nil) then begin
      Error('no main file in project');
      exit;
    end;
    if (CurProject.MainFile=nil) then begin
      Error('no main file in project');
      exit;
    end;
    CodeBuf:=CodeToolBoss.FindFile(CurProject.MainFile.Filename);
    if (CodeBuf=nil) then begin
      Error('project main file has no source');
      exit;
    end;
    AnUnitName:=ExtractFilenameOnly(Filename);
    CodeBuf:=CodeToolBoss.FindUnitSource(CodeBuf,AnUnitName,'');
    if (CodeBuf=nil) then begin
      Error('unit not found: '+AnUnitName);
      exit;
    end;
    if not CodeToolBoss.JumpToLinkerIdentifier(CodeBuf,
      MangledFunction,Identifier,NewCode,NewX,NewY,NewTopLine)
    then begin
      Error('function not found: '+MangledFunction+' Identifier='+Identifier);
      exit;
    end;
    LazarusIDE.DoOpenFileAndJumpToPos(NewCode.Filename,Point(NewX,NewY),
                                      NewTopLine,-1,[]);
  end;
  
begin
  inherited Execute(Msg, Step);
  if Step=imqfoJump then begin
    DebugLn(['TQuickFixLinkerUndefinedReference.Execute ',Msg.Msg,' ',REMatches(Msg.Msg,'^(.*)\(\.text.*?\): .* `([A-Z0-9_$]+)'':$')]);
    if (Msg.Position>0) and REMatches(Msg.Msg,'^: .* `(.*)''$') then
      JumpTo(IDEMessagesWindow[Msg.Position-1],Msg)
    else if (Msg.Position<IDEMessagesWindow.LinesCount-1)
    and REMatches(Msg.Msg,'^(.*)\(\.text.*?\): .* `([A-Z0-9_$]+)'':$') then
      JumpTo(Msg,IDEMessagesWindow[Msg.Position+1]);
  end;
end;

end.

