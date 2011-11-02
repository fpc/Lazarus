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
    An IDE dialog to show various internals about a TCodeTreeNode at cursor.
}
unit CodyNodeInfoDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, StdCtrls,
  // IDEIntf
  SrcEditorIntf,
  // CodeTools
  CodeToolManager, CodeTree, FindDeclarationCache, PascalParserTool,
  LinkScanner, CodeCache, BasicCodeTools, FindDeclarationTool,
  CodyUtils, CodyStrConsts;

type

  { TCodyNodeInfoDialog }

  TCodyNodeInfoDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    PageControl1: TPageControl;
    ReportMemo: TMemo;
    ReportTabSheet: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    procedure AddReportAboutBaseTypeCache(Report: TStringList;
      Tool: TFindDeclarationTool; Node: TCodeTreeNode);
  public
    procedure UpdateReport;
  end;

procedure ShowCodeNodeInfoDialog(Sender: TObject);

implementation

procedure ShowCodeNodeInfoDialog(Sender: TObject);
var
  CodyNodeInfoDialog: TCodyNodeInfoDialog;
begin
  CodyNodeInfoDialog:=TCodyNodeInfoDialog.Create(nil);
  try
    CodyNodeInfoDialog.ShowModal;
  finally
    CodyNodeInfoDialog.Free;
  end;
end;

{ TCodyNodeInfoDialog }

procedure TCodyNodeInfoDialog.FormCreate(Sender: TObject);
begin
  Caption:=crsCodeNodeInformation;
  ReportTabSheet.Caption:=crsReport;
  ButtonPanel1.CloseButton.Caption:=crsClose;

  UpdateReport;
end;

procedure TCodyNodeInfoDialog.AddReportAboutBaseTypeCache(Report: TStringList;
  Tool: TFindDeclarationTool; Node: TCodeTreeNode);

  procedure ReportNode(Prefix: string; ATool: TFindDeclarationTool;
    ANode: TCodeTreeNode);
  var
    s: String;
  begin
    if (ATool=nil) or (ANode=nil) then begin
      Report.Add(Prefix+'Tool='+DbgSName(ATool)+' Node='+DbgSName(ANode));
      exit;
    end;
    Report.Add(Prefix+'Node.Desc='+ANode.DescAsString+' Start='+ATool.CleanPosToStr(ANode.StartPos,true));
    s:=dbgstr(ATool.Src,ANode.StartPos,ANode.EndPos-ANode.StartPos);
    if length(s)>100 then
      s:=copy(s,1,48)+'...'+copy(s,length(s)-47,48);
    Report.Add(GetIndentStr(length(Prefix)+2)+'Src="'+s+'"');
  end;

var
  Cache: TBaseTypeCache;
  BaseContext: TFindContext;
  LastBaseContext: TFindContext;
  i: Integer;
  Visited: TFPList;
  NextTool: TFindDeclarationTool;
  NextNode: TCodeTreeNode;
begin
  LastBaseContext:=CleanFindContext;
  i:=0;
  Visited:=TFPList.Create;
  try
    while Node<>nil do begin
      inc(i);
      ReportNode(dbgs(i)+': ',Tool,Node);
      if not (Node.Cache is TBaseTypeCache) then begin
        Report.Add('Node.Cache='+DbgSName(Node.Cache));
        exit;
      end;
      if Visited.IndexOf(Node)>=0 then begin
        Report.Add('ERROR: CIRCLE');
        exit;
      end;
      Visited.Add(Node);
      Cache:=TBaseTypeCache(Node.Cache);
      BaseContext:=CreateFindContext(TFindDeclarationTool(Cache.BaseTool),Cache.BaseNode);
      if CompareFindContexts(@LastBaseContext,@BaseContext)<>0 then begin
        ReportNode('  Base: ',BaseContext.Tool,BaseContext.Node);
        LastBaseContext:=BaseContext;
      end;

      NextTool:=TFindDeclarationTool(Cache.NextTool);
      NextNode:=Cache.NextNode;
      if (NextNode<>nil) and (NextTool=nil) then begin
        Report.Add('Error: node without tool: Cache.NextTool=nil, Cache.NextNode='+NextNode.DescAsString);
        exit;
      end;
      if NextNode=Node then begin
        // base node reached
        exit;
      end;
      Node:=NextNode;
      Tool:=NextTool;
    end;
  finally
    Visited.Free;
  end;
end;

procedure TCodyNodeInfoDialog.UpdateReport;
var
  SrcEdit: TSourceEditorInterface;
  CursorPos: TCodeXYPosition;
  sl: TStringList;
  Tool: TCodeTool;
  CleanPos: integer;
  Node: TCodeTreeNode;
  s: String;
begin
  sl:=TStringList.Create;
  try
    SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
    if SrcEdit=nil then begin
      sl.Add('no source editor');
      exit;
    end;
    sl.Add('File='+SrcEdit.FileName);
    CursorPos.X:=SrcEdit.CursorTextXY.X;
    CursorPos.Y:=SrcEdit.CursorTextXY.Y;
    sl.Add('Line='+dbgs(CursorPos.Y));
    sl.Add('Column='+dbgs(CursorPos.X));
    CursorPos.Code:=SrcEdit.CodeToolsBuffer as TCodeBuffer;
    if not CodeToolBoss.InitCurCodeTool(CursorPos.Code) then begin
      sl.Add('CodeToolBoss.InitCurCodeTool failed. Maybe unit of include file not found.');
      exit;
    end;
    try
      Tool:=CodeToolBoss.CurCodeTool;
      if CompareFilenames(Tool.MainFilename,SrcEdit.FileName)<>0 then
        sl.Add('Unit='+Tool.MainFilename);
      Tool.BuildTreeAndGetCleanPos(trTillCursor,lsrEnd,CursorPos,CleanPos,
                                   [btSetIgnoreErrorPos]);
      Node:=Tool.FindDeepestNodeAtPos(CleanPos,false);
      if Node=nil then begin
        sl.Add('Error: no node at cursor');
        exit;
      end;
      sl.Add('Node.Desc='+Node.DescAsString);
      sl.Add('Node.SubDesc=%'+binStr(Node.SubDesc,16));
      sl.Add('Node.StartPos='+dbgs(Node.StartPos)+'='+Tool.CleanPosToStr(Node.StartPos));
      sl.Add('Node.EndPos='+dbgs(Node.EndPos)+'='+Tool.CleanPosToStr(Node.EndPos));
      s:=dbgstr(Tool.Src,Node.StartPos,Node.EndPos-Node.StartPos);
      if length(s)>100 then
        s:=copy(s,1,48)+'...'+copy(s,length(s)-47,48);
      sl.Add('Node Src="'+s+'"');

      AddReportAboutBaseTypeCache(sl,Tool,Node);
    except
      on e: Exception do CodeToolBoss.HandleException(e);
    end;
  finally
    if CodeToolBoss.ErrorMessage<>'' then begin
      sl.Add('Error: '+CodeToolBoss.ErrorMessage);
      if CodeToolBoss.ErrorCode<>nil then begin
        sl.Add('Error: '+CodeToolBoss.ErrorCode.Filename);
        if CodeToolBoss.ErrorLine>0 then
          sl.Add('Error line='+dbgs(CodeToolBoss.ErrorLine)+' column='+dbgs(CodeToolBoss.ErrorColumn));
      end;
    end;
    ReportMemo.Lines.Assign(sl);
    sl.Free;
  end;
end;

{$R *.lfm}

end.

