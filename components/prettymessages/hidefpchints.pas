{ Extends Lazarus to hide FPC messages.
  For example:
    testunit1.pas(20,27) Hint: Parameter "X" not used


  Copyright (C) 2007 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit HideFPCHints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls,
  // IDEIntf
  IDEMsgIntf, LazIDEIntf, SrcEditorIntf,
  // CodeTools
  BasicCodeTools, SourceLog, CodeCache, CodeToolManager,
  // our stuff
  PrettyMsgOptions;
  
type

  { THideFPCHintWorker }

  THideFPCHintWorker = class(TIDEMsgQuickFixItem)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
  end;
  
procedure QuickFixHideFPCHint(Sender: TObject; Step: TIMQuickFixStep;
                              Msg: TIDEMessageLine);

procedure Register;

implementation

procedure QuickFixHideFPCHint(Sender: TObject; Step: TIMQuickFixStep;
  Msg: TIDEMessageLine);
begin

end;

procedure Register;
var
  Item: TIDEMsgQuickFixItem;
begin
  Item:=THideFPCHintWorker.Create;
  RegisterIDEMsgQuickFix(Item);
end;

{ THideFPCHintWorker }

constructor THideFPCHintWorker.Create;
begin
  inherited Create;
  Name:='Hide FPC hints';
  Caption:='Hide this FPC hint';
  Steps:=[imqfoImproveMessage,imqfoMenuItem];
end;

destructor THideFPCHintWorker.Destroy;
begin
  inherited Destroy;
end;

procedure THideFPCHintWorker.Execute(const Msg: TIDEMessageLine;
  Step: TIMQuickFixStep);
var
  Filename: string;
  Line: integer;
  Column: integer;
  CodeBuf: TCodeBuffer;
  LineRange: TLineRange;
  DirectivePos: LongInt;
  Src: String;
  EndPos: LongInt;
  p: LongInt;
  MsgType: string;
  Directive: String;
  SrcEdit: TSourceEditorInterface;
  InsertPos: Integer;
  HasDirective: Boolean;
begin
  inherited Execute(Msg, Step);
  
  // get filename and line number and load file
  if Msg.Parts=nil then exit;
  if Msg.Parts.Values['Stage']<>'FPC' then exit;
  MsgType:=Msg.Parts.Values['Type'];
  if MsgType='Hint' then
    Directive:='h-'
  else if MsgType='Note' then
    Directive:='n-'
  else if MsgType='Warning' then
    Directive:='w-'
  else
    exit;

  Msg.GetSourcePosition(Filename,Line,Column);
  //DebugLn(['THideFPCHintWorker.Execute ',Filename,' Line=',Line]);
  CodeBuf:=CodeToolBoss.LoadFile(Filename,false,false);
  if CodeBuf=nil then exit;
  if (Line<1) or (Line>CodeBuf.LineCount) then exit;
  Src:=CodeBuf.Source;
  CodeBuf.GetLineRange(Line-1,LineRange);

  // check if the code contains an IDE directive to hide the hint
  HasDirective:=false;
  p:=LineRange.StartPos;
  EndPos:=LineRange.EndPos;
  repeat
    DirectivePos:=FindNextIDEDirective(Src,p,true,EndPos);
    if (DirectivePos<1) then break;
    if CompareSubStrings(Directive,Src,1,DirectivePos+2,2,false)=0 then begin
      //DebugLn(['THideFPCHintWorker.Execute Ignoring message: ',Msg.Msg]);
      HasDirective:=true;
    end;
    p:=DirectivePos;
    while (p<EndPos) and (Src[p]<>'}') do inc(p);
  until p>=EndPos;

  if Step=imqfoMenuItem then begin
    // user clicked on the menu item to add an IDE directive
    if HasDirective then exit;
    
    // open the file in the source editor
    if LazarusIDE.DoOpenFileAndJumpToPos(Filename,Point(1,Line),-1,-1,-1,
      [ofOnlyIfExists,ofRegularFile,ofUseCache,ofDoNotLoadResource])<>mrOk then
    begin
      DebugLn(['THideFPCHintWorker.Execute open failed: ',Filename]);
      exit;
    end;
    // find the source editor page
    SrcEdit:=SourceEditorManagerIntf.SourceEditorIntfWithFilename(Filename);
    if SrcEdit=nil then begin
      DebugLn(['THideFPCHintWorker.Execute unable to find the file in the source editor of ',Filename]);
      exit;
    end;
    // find the source editor line
    if SrcEdit.LineCount<Line then begin
      DebugLn(['THideFPCHintWorker.Execute unable to find the line ',Line,' of ',Filename]);
      exit;
    end;
    
    // add at end of line
    InsertPos:=length(SrcEdit.Lines[Line-1])+1;
    SrcEdit.ReplaceText(Point(InsertPos,Line),Point(InsertPos,Line),'{%'+Directive+'}');
    
  end else if Step=imqfoImproveMessage then begin
    // the parser found the message -> hide the message
    if HasDirective then
      Msg.Visible:=false;
  end;
end;

function THideFPCHintWorker.IsApplicable(Line: TIDEMessageLine): boolean;
var
  MsgType: string;
begin
  Result:=false;
  if Line.Parts=nil then exit;
  if Line.Parts.Values['Stage']<>'FPC' then exit;
  MsgType:=Line.Parts.Values['Type'];
  if MsgType='Hint' then
    Result:=true
  else if MsgType='Note' then
    Result:=true
  else if MsgType='Warning' then
    Result:=true;
end;

end.

