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
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
  
  Abstract:
    Demonstrates how to add a new menu item to the IDE:
    Search -> Jump to Implementation
}
unit CodeToolsExample1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, Controls, SrcEditorIntf,
  CodeToolManager, CodeTree, CodeCache, CustomCodeTool, IDECommands,
  ToolBarIntf, LCLType, Menus, ComCtrls;

type
  TJumpToSectionDemoToolButton = class(TIDEToolButton)
  private
    procedure JumpToInterface(Sender: TObject);
    procedure JumpToImplementation(Sender: TObject);
  public
    procedure DoOnAdded; override;
  end;

  TKeyWordType = (kwInterface, kwImplementation);

  
procedure JumpIDEToKeyword(KeywordType: TKeyWordType);
procedure JumpIDEToImplementationKeyword(Sender: TObject);
procedure JumpIDEToInterfaceKeyword(Sender: TObject);

procedure Register;

implementation

procedure JumpIDEToKeyword(KeywordType: TKeyWordType);
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
        case KeywordType of
          kwInterface: Node:=Tool.FindInterfaceNode;
          kwImplementation: Node:=Tool.FindImplementationNode;
        end;
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

procedure JumpIDEToImplementationKeyword(Sender: TObject);
begin
  JumpIDEToKeyword(kwImplementation);
end;

procedure JumpIDEToInterfaceKeyword(Sender: TObject);
begin
  JumpIDEToKeyword(kwInterface);
end;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  xCmd: TIDECommand;
  xBtnCmd: TIDEButtonCommand;
begin
  // register IDE shortcut and tool button
  Key := IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Cat:=IDECommandList.FindCategoryByName(CommandCategoryCodeTools);
  xCmd := RegisterIDECommand(Cat,'JumpToImplementationDemo', 'Jump to implementation keyword (demo)', Key, nil, @JumpIDEToImplementationKeyword);

  xBtnCmd := RegisterIDEButtonCommand(xCmd);
  xBtnCmd.ToolButtonClass := TJumpToSectionDemoToolButton;
end;

{ TJumpToSectionDemoToolButton }

procedure TJumpToSectionDemoToolButton.DoOnAdded;
var
  xItem: TMenuItem;
begin
  inherited DoOnAdded;

  if DropdownMenu = nil then
  begin
    DropdownMenu := TPopupMenu.Create(Self);
    Style := tbsDropDown;
  end;

  xItem := TMenuItem.Create(DropdownMenu);
  DropdownMenu.Items.Add(xItem);
  xItem.Caption := 'Jump to interface keyword';
  xItem.OnClick := @JumpToInterface;

  xItem := TMenuItem.Create(DropdownMenu);
  DropdownMenu.Items.Add(xItem);
  xItem.Caption := 'Jump to implementation keyword';
  xItem.OnClick := @JumpToImplementation;
end;

procedure TJumpToSectionDemoToolButton.JumpToImplementation(Sender: TObject);
begin
  JumpIDEToImplementationKeyword(Sender);
end;

procedure TJumpToSectionDemoToolButton.JumpToInterface(Sender: TObject);
begin
  JumpIDEToInterfaceKeyword(Sender);
end;

end.

