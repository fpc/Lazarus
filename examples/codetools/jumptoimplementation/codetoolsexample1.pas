{ Copyright (C) 2005 Mattias Gaertner

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
  
  Abstract:
    Demonstrates how to add a new menu item to the IDE:
    Search -> Jump to Implementation
}
unit CodeToolsExample1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MenuIntf, LazIDEIntf, Controls, SrcEditorIntf,
  CodeToolManager, CodeTree, CodeCache, CodeAtom, CustomCodeTool;
  
procedure JumpIDEToImplementationKeyword(Sender: TObject);

procedure Register;

implementation

procedure JumpIDEToImplementationKeyword(Sender: TObject);
var
  SrcEditor: TSourceEditorInterface;
  CodeBuffer: TCodeBuffer;
  CurCodeTool: TCustomCodeTool;
  Node: TCodeTreeNode;
  Tool: TCodeTool;
  NewCodePos: TCodeXYPosition;
  NewTopLine: Integer;
  Ok: Boolean;
begin
  if Sender=nil then ;
  // commit editor changes to codetools
  if not LazarusIDE.BeginCodeTools then exit;
  
  // get active source editor
  SrcEditor:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEditor=nil then exit;
  CodeBuffer:=SrcEditor.CodeToolsBuffer as TCodeBuffer;
  
  Ok:=false;
  try
    // init codetool for the source
    if CodeToolBoss.InitCurCodeTool(CodeBuffer) then begin
      CurCodeTool:=CodeToolBoss.CurCodeTool;
      if CurCodeTool is TCodeTool then begin
        // search imlementation node
        Tool:=TCodeTool(CurCodeTool);
        Node:=Tool.FindImplementationNode;
        if Node<>nil then begin
          // convert text position to editor postion
          NewTopLine:=0;
          NewCodePos:=CleanCodeXYPosition;
          if Tool.CleanPosToCaretAndTopLine(Node.StartPos,
                                            NewCodePos,NewTopLine)
          then begin
            // jump
            if LazarusIDE.DoOpenFileAndJumpToPos(NewCodePos.Code.Filename,
                      Point(NewCodePos.X,NewCodePos.Y),NewTopLine,-1,-1,
                      [ofRegularFile,ofUseCache])=mrOk
            then
              Ok:=true;
          end;
        end;
      end;
    end;
  except
    on E: Exception do begin
      CodeToolBoss.HandleException(E);
    end;
  end;
  if not Ok then
    LazarusIDE.DoJumpToCodeToolBossError;
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmCodeToolSearches,'JumpToImplementation',
    'Jump to implementation keyword',nil,@JumpIDEToImplementationKeyword);
end;

end.

