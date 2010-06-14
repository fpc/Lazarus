{
  Copyright (C) 2007 Graeme Geldenhuys (graemeg@gmail.com)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit jumpto_impl;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,CodeToolManager
  ,CodeTree
  ;


type
  TJumpType =
    ( jmpIntf, jmpIntfUses, jmpImpl, jmpImplUses, jmpInit );


const
  cJumpNames: array[TJumpType] of string =
      ('Interface', 'Interface uses', 'Implementation', 'Implementation uses',
       'Initialization');

type
  TJumpHandler = class(TComponent)
  private
    function    JumpToNode(Tool: TCodeTool; Node: TCodeTreeNode): Boolean;
  public
    procedure   DoJump(Sender: TObject);
    procedure   DoJumpToImplementation(Sender: TObject);
  end;



implementation

uses
  CodeAtom
  ,SrcEditorIntf
  ,LazIDEIntf
  ,Controls
  ,CodeCache
  ,CustomCodeTool
  ,Dialogs
  ,SysUtils
  ,LResources
  ,Forms
  ,ComCtrls
  ,Menus
  ,editortoolbar_str
  ;

{ TJumpHandler }

function TJumpHandler.JumpToNode(Tool: TCodeTool; Node: TCodeTreeNode): Boolean;
var
  NewTopLine: Integer;
  NewCodePos: TCodeXYPosition;
  SrcEditor: TSourceEditorInterface;
begin
  NewTopLine := 0;
  NewCodePos := CleanCodeXYPosition;
  Result := Tool.CleanPosToCaretAndTopLine(Node.StartPos, NewCodePos,NewTopLine);
  if Result then
    Result := LazarusIDE.DoOpenFileAndJumpToPos(NewCodePos.Code.Filename
        ,Point(NewCodePos.X,NewCodePos.Y), NewTopLine, -1,-1
        ,[ofRegularFile,ofUseCache]) = mrOk;
  if Result then
  begin
    SrcEditor := SourceEditorManagerIntf.ActiveEditor;
    if Assigned(SrcEditor) then
      SrcEditor.EditorControl.SetFocus;
  end;
end;

procedure TJumpHandler.DoJump(Sender: TObject);
var
  SrcEditor: TSourceEditorInterface;
  CodeBuffer: TCodeBuffer;
  Node: TCodeTreeNode;
  Tool: TCodeTool;
  T: TJumpType;
begin
  If (Sender <> nil) and (Sender is TComponent) then
    T := TJumpType(TComponent(Sender).Tag);

  if not LazarusIDE.BeginCodeTools then
    Exit; //==>

  SrcEditor := SourceEditorManagerIntf.ActiveEditor;
  if not Assigned(SrcEditor) then
    Exit; //==>

  CodeBuffer := SrcEditor.CodeToolsBuffer as TCodeBuffer;
  if CodeToolBoss.Explore(CodeBuffer,Tool,false,false) then
  begin
    case T of
      jmpIntf     : Node := Tool.FindInterfaceNode;
      jmpIntfUses : Node := Tool.FindMainUsesSection;
      jmpImpl     : Node := Tool.FindImplementationNode;
      jmpImplUses : Node := Tool.FindImplementationUsesSection;
      jmpInit     : Node := Tool.FindInitializationNode;
    end;
    if (Node <> nil) then
      JumpToNode(Tool, Node)
    else
      ShowMessage(Format(SErrCouldNotFind, [cJumpNames[T]]));
  end
  else
    LazarusIDE.DoJumpToCodeToolBossError;
end;

procedure TJumpHandler.DoJumpToImplementation(Sender: TObject);
begin
  if (Sender <> nil) and (Sender is TComponent) then
    TComponent(Sender).Tag := Ord(jmpImpl);
  DoJump(Sender);
end;


end.

