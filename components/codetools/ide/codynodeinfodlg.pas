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
  Classes, SysUtils, AVL_Tree, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, ComCtrls, StdCtrls,
  // IDEIntf
  SrcEditorIntf,
  // CodeTools
  CodeToolManager, CodeTree, FindDeclarationCache, PascalParserTool,
  LinkScanner, CodeCache, BasicCodeTools, FindDeclarationTool, SourceLog,
  CodyStrConsts, FileProcs, LazFileUtils;

type

  { TCodyNodeInfoDialog }

  TCodyNodeInfoDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    CodeBufferMemo: TMemo;
    CodeBuffersComboBox: TComboBox;
    LinksMemo: TMemo;
    PageControl1: TPageControl;
    ReportMemo: TMemo;
    ReportTabSheet: TTabSheet;
    CodeBuffersTabSheet: TTabSheet;
    LinksTabSheet: TTabSheet;
    procedure CodeBuffersComboBoxSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ReportBaseTypeCache(Report: TStringList;
      Tool: TFindDeclarationTool; Node: TCodeTreeNode);
    procedure ReportAllNodes(Report: TStringList;
      Tool: TFindDeclarationTool);
    procedure ShowCodeBuffer(Filename: string);
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
  CodeBuffersTabSheet.Caption:=crsCodeBuffers;
  LinksTabSheet.Caption:=crsLinks;
  ButtonPanel1.CloseButton.Caption:=crsClose;
  PageControl1.PageIndex:=0;
  UpdateReport;
end;

procedure TCodyNodeInfoDialog.CodeBuffersComboBoxSelect(Sender: TObject);
begin
  ShowCodeBuffer(CodeBuffersComboBox.Text);
end;

procedure TCodyNodeInfoDialog.ReportBaseTypeCache(Report: TStringList;
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

procedure TCodyNodeInfoDialog.ReportAllNodes(Report: TStringList;
  Tool: TFindDeclarationTool);
var
  Node: TCodeTreeNode;
  s: String;
begin
  Report.Add('');
  Report.Add('Nodes:');
  if Tool=nil then exit;
  if Tool.Tree=nil then exit;
  Node:=Tool.Tree.Root;
  while Node<>nil do begin
    s:=GetIndentStr(Node.GetLevel*2);
    s:=s+Node.DescAsString;
    s:=s+',Start='+IntToStr(Node.StartPos)+'='+Tool.CleanPosToStr(Node.StartPos);
    s:=s+',End='+IntToStr(Node.EndPos)+'='+Tool.CleanPosToStr(Node.EndPos);
    Report.Add(s);
    Node:=Node.Next;
  end;
end;

procedure TCodyNodeInfoDialog.ShowCodeBuffer(Filename: string);
var
  Code: TCodeBuffer;
  i: Integer;
  sl: TStringList;
begin
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  sl:=TStringList.Create;
  if Code<>nil then begin
    for i:=0 to Code.LineCount-1 do begin
      sl.Add('Line='+dbgs(i+1)+' Start='+dbgs(Code.GetLineStart(i))+' ="'+dbgstr(Code.GetLine(i,true))+'"');
    end;
  end;
  CodeBufferMemo.Lines.Assign(sl);
  sl.Free;
end;

procedure TCodyNodeInfoDialog.UpdateReport;
var
  SrcEdit: TSourceEditorInterface;
  CursorPos: TCodeXYPosition;
  sl: TStringList;
  Tool: TCodeTool;
  CleanPos: integer;
  Node: TCodeTreeNode;
  SrcCode: TSourceLog;
  i: Integer;
  UsedCodeBuffers: TAVLTree;
  AVLNode: TAVLTreeNode;
  Filenames: TStringList;
  CodeBuf: TCodeBuffer;
  Scanner: TLinkScanner;
  Link: TSourceLink;
  LinkSize: Integer;
  s: String;
begin
  Tool:=nil;
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

      // nodes
      sl.Add('Scanner.ScannedRange='+dbgs(Tool.Scanner.ScannedRange));
      sl.Add('Tool.ScannedRange='+dbgs(Tool.ScannedRange));
      Node:=Tool.FindDeepestNodeAtPos(CleanPos,false);
      if Node=nil then begin
        try
          Tool.RaiseCursorOutsideCode(CursorPos);
        except
          on E: Exception do begin
            sl.Add('Error: '+E.Message);
          end;
        end;
      end else begin
        sl.Add('Node.Desc='+Node.DescAsString);
        sl.Add('Node.SubDesc=%'+binStr(Node.SubDesc,16));
        sl.Add('Node.StartPos='+dbgs(Node.StartPos)+'='+Tool.CleanPosToStr(Node.StartPos));
        sl.Add('Node.EndPos='+dbgs(Node.EndPos)+'='+Tool.CleanPosToStr(Node.EndPos));
        sl.Add('Node Src>>>>>>>>>>>>>>>>>>');
        SrcCode:=TSourceLog.Create(copy(Tool.Src,Node.StartPos,Node.EndPos-Node.StartPos));
        for i:=0 to SrcCode.LineCount-1 do begin
          sl.Add('Line='+dbgs(i)+',CleanPos='+dbgs(Node.StartPos+SrcCode.GetLineStart(i)-1)+'="'+dbgstr(SrcCode.GetLine(i,true))+'"');
        end;
        SrcCode.Free;
        sl.Add('Node Src<<<<<<<<<<<<<<<<<<');

        ReportBaseTypeCache(sl,Tool,Node);
        ReportAllNodes(sl,Tool);
      end;

      // codebuffers
      Filenames:=TStringList.Create;
      if Tool.Scanner<>nil then begin
        UsedCodeBuffers:=Tool.Scanner.CreateTreeOfSourceCodes;
        AVLNode:=UsedCodeBuffers.FindLowest;
        while AVLNode<>nil do begin
          CodeBuf:=TCodeBuffer(AVLNode.Data);
          Filenames.Add(CodeBuf.Filename);
          AVLNode:=UsedCodeBuffers.FindSuccessor(AVLNode);
        end;
        UsedCodeBuffers.Free;
      end;
      CodeBuffersComboBox.Items.Assign(Filenames);
      Filenames.Free;
      if CodeBuffersComboBox.Items.Count>0 then begin
        CodeBuffersComboBox.Text:=CodeBuffersComboBox.Items[0];
        ShowCodeBuffer(CodeBuffersComboBox.Text);
      end else begin
        CodeBuffersComboBox.Text:='';
      end;

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
  // links
  sl:=TStringList.Create;
  try
    sl.Clear;
    if Tool.Scanner<>nil then begin
      Scanner:=Tool.Scanner;
      sl.Add('MainFilename:'+Scanner.MainFilename);
      sl.Add('ScannedRange='+dbgs(Scanner.ScannedRange));
      sl.Add('===================================');
      sl.Add('InitialValues:');
      sl.Add(Scanner.InitialValues.AsString);
      sl.Add('===================================');
      sl.Add('Values:');
      sl.Add(Scanner.Values.AsString);
      sl.Add('===================================');
      sl.Add('IsUnit='+dbgs(Scanner.IsUnit));
      sl.Add('SourceName='+Scanner.SourceName);
      sl.Add('===================================');
      sl.Add('Links:');
      for i:=0 to Scanner.LinkCount-1 do begin
        Link:=Scanner.Links[i];
        s:=dbgs(i)+'/'+dbgs(Scanner.LinkCount)+': Kind='+dbgs(Link.Kind);
        if Link.Code<>nil then
          s+=',File='+ExtractFileName(TCodeBuffer(Link.Code).Filename);
        s+=',CleanedPos='+dbgs(Link.CleanedPos)+',SrcPos='+dbgs(Link.SrcPos);
        LinkSize:=Scanner.LinkSize(i);
        s+=',Size='+dbgs(LinkSize);
        if LinkSize>60 then
          s+=',Code="'+dbgstr(Scanner.CleanedSrc,Link.CleanedPos,30)+'...'+dbgstr(Scanner.CleanedSrc,Link.CleanedPos+LinkSize-30,30)+'"'
        else
          s+=',Code="'+dbgstr(Scanner.CleanedSrc,Link.CleanedPos,LinkSize)+'"';
        sl.Add(s);
      end;
      sl.Add('===================================');
    end;
    LinksMemo.Lines.Assign(sl);
  finally
    sl.Free;
  end;
end;

{$R *.lfm}

end.

