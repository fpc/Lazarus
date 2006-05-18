{
/***************************************************************************
                             CodeContextForm.pas
                             -------------------

 ***************************************************************************/

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
    The popup tooltip window for the source editor.
}
unit CodeContextForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  LCLType, LCLIntf,
  SynEdit, CodeCache, FindDeclarationTool, IdentCompletionTool, CodeTree,
  CodeAtom, PascalParserTool, CodeToolManager,
  SrcEditorIntf;

type

  { TCodeContextFrm }

  TCodeContextFrm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FHints: TStrings;
    FProcNameCodeXYPos: TCodeXYPosition;
    procedure CreateHints(const CodeContexts: TCodeContextInfo);
    procedure CalculateHintsBounds(const CodeContexts: TCodeContextInfo);
    procedure DrawHints(var MaxWidth, MaxHeight: Integer; Draw: boolean);
  public
    procedure SetCodeContexts(const CodeContexts: TCodeContextInfo);
    property ProcNameCodeXYPos: TCodeXYPosition read FProcNameCodeXYPos;
  end;

var
  CodeContextFrm: TCodeContextFrm = nil;
  
function ShowCodeContext(Code: TCodeBuffer; Editor: TSynEdit): boolean;

implementation

function ShowCodeContext(Code: TCodeBuffer; Editor: TSynEdit): boolean;
var
  LogCaretXY: TPoint;
  CodeContexts: TCodeContextInfo;
begin
  Result:=false;
  LogCaretXY:=Editor.LogicalCaretXY;
  CodeContexts:=nil;
  try
    if not CodeToolBoss.FindCodeContext(Code,LogCaretXY.X,LogCaretXY.Y,
      CodeContexts)
    then
      exit;
    DebugLn('ShowCodeContext show');
    {$IFNDEF EnableCodeContext}
    exit;
    {$ENDIF}
    if CodeContextFrm=nil then
      CodeContextFrm:=TCodeContextFrm.Create(nil);
    CodeContextFrm.SetCodeContexts(CodeContexts);
    CodeContextFrm.Visible:=true;
  finally
    CodeContexts.Free;
  end;
end;

{ TCodeContextFrm }

procedure TCodeContextFrm.FormCreate(Sender: TObject);
begin
  FHints:=TStringList.Create;
end;

procedure TCodeContextFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHints);
end;

procedure TCodeContextFrm.FormPaint(Sender: TObject);
var
  DrawWidth: LongInt;
  DrawHeight: LongInt;
begin
  DrawWidth:=Self.ClientWidth;
  DrawHeight:=Self.ClientHeight;
  DrawHints(DrawWidth,DrawHeight,true);
end;

procedure TCodeContextFrm.SetCodeContexts(const CodeContexts: TCodeContextInfo);
begin
  FillChar(FProcNameCodeXYPos,SizeOf(FProcNameCodeXYPos),0);

  if CodeContexts<>nil then begin
    if (CodeContexts.ProcNameAtom.StartPos>0) then
      CodeContexts.Tool.CleanPosToCaret(CodeContexts.ProcNameAtom.StartPos,
                                        FProcNameCodeXYPos);
  end;
  
  CreateHints(CodeContexts);
  CalculateHintsBounds(CodeContexts);
end;

procedure TCodeContextFrm.CreateHints(const CodeContexts: TCodeContextInfo);
var
  i: Integer;
  CurExprType: TExpressionType;
  CodeNode: TCodeTreeNode;
  CodeTool: TFindDeclarationTool;
  s: String;
  p: Integer;
begin
  FHints.Clear;
  if (CodeContexts=nil) or (CodeContexts.Count=0) then exit;
  for i:=0 to CodeContexts.Count-1 do begin
    CurExprType:=CodeContexts[i];
    s:=ExpressionTypeDescNames[CurExprType.Desc];
    if CurExprType.Context.Node<>nil then begin
      CodeNode:=CurExprType.Context.Node;
      CodeTool:=CurExprType.Context.Tool;
      case CodeNode.Desc of
      ctnProcedure:
        begin
          s:=CodeTool.ExtractProcHead(CodeNode,
              [phpWithVarModifiers,phpWithParameterNames,phpWithDefaultValues,
               phpWithResultType,phpWithOfObject]);
        end;
      end;
    end;
    // insert spaces
    for p:=length(s)-1 downto 1 do begin
      if (s[p] in [',',';',':']) and (s[p+1]<>' ') then
        System.Insert(' ',s,p+1);
    end;
    FHints.Add(Trim(s));
  end;
  DebugLn('TCodeContextFrm.UpdateHints ',FHints.Text);
end;

procedure TCodeContextFrm.CalculateHintsBounds(const
  CodeContexts: TCodeContextInfo);
var
  DrawWidth: LongInt;
  SrcEdit: TSourceEditorInterface;
  NewBounds: TRect;
  CursorTextXY: TPoint;
  ScreenTextXY: TPoint;
  ClientXY: TPoint;
  DrawHeight: LongInt;
  ScreenXY: TPoint;
begin
  SrcEdit:=SourceEditorWindow.ActiveEditor;
  if SrcEdit=nil then exit;

  // calculate the position of the context in the source editor
  CursorTextXY:=SrcEdit.CursorTextXY;
  if ProcNameCodeXYPos.Code<>nil then begin
    if (ProcNameCodeXYPos.Code=SrcEdit.CodeToolsBuffer)
    and (ProcNameCodeXYPos.Y<=CursorTextXY.Y) then begin
      CursorTextXY:=Point(ProcNameCodeXYPos.X,ProcNameCodeXYPos.Y);
    end;
  end;
  // calculate screen position
  ScreenTextXY:=SrcEdit.TextToScreenPosition(CursorTextXY);
  ClientXY:=SrcEdit.ScreenToPixelPosition(ScreenTextXY);

  // calculate size of hints
  DrawWidth:=SourceEditorWindow.ClientWidth;
  DrawHeight:=ClientXY.Y;
  DrawHints(DrawWidth,DrawHeight,false);
  if DrawWidth<20 then DrawWidth:=20;
  if DrawHeight<5 then DrawHeight:=5;

  // calculate position of hints in editor client area
  if ClientXY.X+DrawWidth>SrcEdit.EditorControl.ClientWidth then
    ClientXY.X:=SrcEdit.EditorControl.ClientWidth-DrawWidth;
  if ClientXY.X<0 then
    ClientXY.X:=0;
  dec(ClientXY.Y,DrawHeight);

  // calculate screen position
  ScreenXY:=SrcEdit.EditorControl.ClientToScreen(ClientXY);
  dec(ScreenXY.Y,4);
  NewBounds:=Bounds(ScreenXY.X,ScreenXY.Y,DrawWidth,DrawHeight);

  // move form
  BoundsRect:=NewBounds;
end;

procedure TCodeContextFrm.DrawHints(var MaxWidth, MaxHeight: Integer;
  Draw: boolean);
var
  BackgroundColor, TextColor: TColor;
  i: Integer;
  NewMaxHeight: Integer;
  Flags: Cardinal;
  CurRect: TRect;
  s: string;
  CurTextRect: TRect;
  HorizontalSpace: Integer;
  VerticalSpace: Integer;
  NewMaxWidth: Integer;
begin
  //DebugLn('TCodeContextFrm.DrawHints DrawWidth=',dbgs(MaxWidth),' DrawHeight=',dbgs(MaxHeight),' Draw=',dbgs(Draw));
  if Draw then begin
    // TODO: make colors configurable and theme dependent
    BackgroundColor:=clWhite;
    TextColor:=clBlack;
  end;
  HorizontalSpace:=2;
  VerticalSpace:=2;

  if Draw then begin
    Canvas.Brush.Color:=BackgroundColor;
    Canvas.Font.Color:=TextColor;
  end;
  NewMaxWidth:=0;
  NewMaxHeight:=0;
  for i:=0 to FHints.Count-1 do begin
    if Draw and (NewMaxHeight>=MaxHeight) then break;
    s:=FHints[i];
    Flags:=DT_WordBreak;
    CurTextRect:=Rect(0,NewMaxHeight,MaxWidth,MaxHeight);
    OffsetRect(CurTextRect,HorizontalSpace,VerticalSpace);
    // calculate height
    DrawText(Canvas.Handle,PChar(s),Length(s),CurTextRect,Flags+DT_CalcRect);
    if Draw then
      CurRect:=Rect(0,NewMaxHeight,MaxWidth,CurTextRect.Bottom+VerticalSpace)
    else
      CurRect:=Rect(0,NewMaxHeight,
                    CurTextRect.Right+HorizontalSpace,
                    CurTextRect.Bottom+VerticalSpace);
    //DebugLn('TCodeContextFrm.DrawHints i=',dbgs(i),' CurTextRect=',dbgs(CurTextRect),' CurRect=',dbgs(CurRect),' s="',s,'"');
    if CurRect.Right>NewMaxWidth then
      NewMaxWidth:=CurRect.Right;
    if Draw then begin
      // draw text and background
      Canvas.FillRect(CurRect);
      DrawText(Canvas.Handle, PChar(s), Length(s), CurTextRect, Flags);
    end;
    NewMaxHeight:=CurRect.Bottom;
  end;
  if Draw then begin
    // draw frame around window
    Canvas.Pen.Color:=TextColor;
    Canvas.Frame(Rect(0,0,MaxWidth-1,MaxHeight-1));
  end;
  if not Draw then begin
    if NewMaxWidth<MaxWidth then
      MaxWidth:=NewMaxWidth;
    if NewMaxHeight<MaxHeight then
      MaxHeight:=NewMaxHeight;
  end;
end;

initialization
  {$I codecontextform.lrs}

end.

