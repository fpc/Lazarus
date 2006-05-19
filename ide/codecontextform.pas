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
  Classes, SysUtils, Types, LCLProc, LResources, Forms, Controls, Graphics,
  Dialogs, LCLType, LCLIntf,
  BasicCodeTools, LinkScanner, CodeCache, FindDeclarationTool,
  IdentCompletionTool, CodeTree, CodeAtom, PascalParserTool, CodeToolManager,
  SrcEditorIntf;

type

  { TCodeContextFrm }

  TCodeContextFrm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
  private
    FHints: TStrings;
    FProcNameCodeXYPos: TCodeXYPosition;
    procedure CreateHints(const CodeContexts: TCodeContextInfo);
    procedure ClearMarksInHints;
    procedure MarkCurrentParameterInHints(ParameterIndex: integer); // 0 based
    procedure CalculateHintsBounds(const CodeContexts: TCodeContextInfo);
    procedure DrawHints(var MaxWidth, MaxHeight: Integer; Draw: boolean);
  public
    procedure SetCodeContexts(const CodeContexts: TCodeContextInfo);
    property ProcNameCodeXYPos: TCodeXYPosition read FProcNameCodeXYPos;
  end;

var
  CodeContextFrm: TCodeContextFrm = nil;
  
function ShowCodeContext(Code: TCodeBuffer): boolean;

implementation

function ShowCodeContext(Code: TCodeBuffer): boolean;
var
  LogCaretXY: TPoint;
  CodeContexts: TCodeContextInfo;
begin
  Result:=false;
  LogCaretXY:=SourceEditorWindow.ActiveEditor.CursorTextXY;
  CodeContexts:=nil;
  try
    if (not CodeToolBoss.FindCodeContext(Code,LogCaretXY.X,LogCaretXY.Y,
      CodeContexts))
    or (CodeContexts=nil) or (CodeContexts.Count=0) then
      exit;
    {$IFNDEF EnableCodeContext}
    exit;
    {$ENDIF}
    if CodeContextFrm=nil then
      CodeContextFrm:=TCodeContextFrm.Create(nil);
    CodeContextFrm.SetCodeContexts(CodeContexts);
    CodeContextFrm.Visible:=true;
    Result:=true;
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

procedure TCodeContextFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then Close;
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
    // mark the mark characters
    for p:=length(s) downto 1 do
      if s[p]='\' then
        System.Insert('\',s,p+1);
    FHints.Add(Trim(s));
  end;
  MarkCurrentParameterInHints(CodeContexts.ParameterIndex-1);
  DebugLn('TCodeContextFrm.UpdateHints ',FHints.Text);
end;

procedure TCodeContextFrm.ClearMarksInHints;
// remove all marks except the \\ marks
var
  i: Integer;
  s: string;
  p: Integer;
begin
  for i:=0 to FHints.Count-1 do begin
    s:=FHints[i];
    p:=1;
    while p<length(s) do begin
      if s[p]<>'\' then
        inc(p)  // normal character
      else if s[p+1]='\' then
        inc(p,2) // '\\'
      else begin
        System.Delete(s,p,2); // remove mark
      end;
    end;
    FHints[i]:=s;
  end;
end;

procedure TCodeContextFrm.MarkCurrentParameterInHints(ParameterIndex: integer);

  function MarkCurrentParameterInHint(const s: string): string;
  var
    p: Integer;
    CurrentMark: Char;

    procedure Mark(NewMark: char; Position: integer);
    begin
      if p=Position then
        CurrentMark:=NewMark;
      System.Insert('\'+NewMark,Result,Position);
      if Position<=p then
        inc(p,2);
      //DebugLn('Mark Position=',dbgs(Position),' p=',dbgs(p),' CurrentMark="',CurrentMark,'" ',copy(Result,1,Position+2));
    end;
  
  var
    BracketLevel: Integer;
    CurParameterIndex: Integer;
    WordStart: LongInt;
    WordEnd: LongInt;
    ModifierStart: LongInt;
    ModifierEnd: LongInt;
    SearchingType: Boolean;
    ReadingType: Boolean;
  begin
    Result:=s;
    BracketLevel:=0;
    CurParameterIndex:=0;
    CurrentMark:='*';
    ReadingType:=false;
    SearchingType:=false;
    ModifierStart:=-1;
    ModifierEnd:=-1;
    p:=1;
    while (p<=length(Result)) do begin
      //DebugLn('MarkCurrentParameterInHint p=',dbgs(p),' "',Result[p],'" BracketLevel=',dbgs(BracketLevel),' CurParameterIndex=',dbgs(CurParameterIndex),' ReadingType=',dbgs(ReadingType),' SearchingType=',dbgs(SearchingType));
      case Result[p] of
      '(','{':
        inc(BracketLevel);
      ')','}':
        begin
          if (BracketLevel=1) then begin
            if CurrentMark<>'*' then
              Mark('*',p);
            exit;
          end;
          dec(BracketLevel);
        end;
      ',':
        if BracketLevel=1 then begin
          inc(CurParameterIndex);
        end;
      ':':
        if BracketLevel=1 then begin
          // names ended, type started
          if SearchingType then
            Mark('b',p);
          ReadingType:=true;
          SearchingType:=false;
        end;
      ';':
        if BracketLevel=1 then begin
          // type ended, next parameter started
          if CurrentMark<>'*' then
            Mark('*',p);
          SearchingType:=false;
          ReadingType:=false;
          ModifierStart:=-1;
          inc(CurParameterIndex);
        end;
      '''':
        repeat
          inc(p);
        until (p>=length(Result)) or (Result[p]='''');
      'a'..'z','A'..'Z','_':
        if (BracketLevel=1) and (not ReadingType) then begin
          WordStart:=p;
          while (p<=length(Result)) and (IsIdentChar[Result[p]]) do
            inc(p);
          WordEnd:=p;
          //DebugLn('MarkCurrentParameterInHint Word=',copy(Result,WordStart,WordEnd-WordStart));
          if (CompareIdentifiers('const',@Result[WordStart])=0)
          or (CompareIdentifiers('out',@Result[WordStart])=0)
          or (CompareIdentifiers('var',@Result[WordStart])=0) then begin
            // modifier
            ModifierStart:=WordStart;
            ModifierEnd:=WordEnd;
          end else begin
            // parameter name
            if ParameterIndex=CurParameterIndex then begin
              // mark parameter
              Mark('*',WordEnd); // mark WordEnd before WordStart !
              Mark('b',WordStart);
              // mark modifier
              if ModifierStart>0 then begin
                Mark('*',ModifierEnd); // mark ModifierEnd before ModifierStart !
                Mark('b',ModifierStart);
              end;
              // search type
              SearchingType:=true;
            end;
          end;
          dec(p);
        end;
      end;
      inc(p);
    end;
  end;
  
var
  i: Integer;
begin
  ClearMarksInHints;
  for i:=0 to FHints.Count-1 do
    FHints[i]:=MarkCurrentParameterInHint(FHints[i]);
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
  HorizontalSpace: Integer;
  VerticalSpace: Integer;
  BackgroundColor, TextColor, TextBColor: TColor;

  procedure DrawHint(const Line: string; var AHintRect: TRect);
  var
    ATextRect: TRect;
    TokenStart: Integer;
    TokenRect: TRect;
    TokenSize: TPoint;
    TokenPos: TPoint;
    TokenEnd: LongInt;
    UsedWidth: Integer; // maximum right token position
    LineHeight: Integer; // current line height
    LastTokenEnd: LongInt;
  begin
    ATextRect:=Rect(AHintRect.Left+HorizontalSpace,
                    AHintRect.Top+VerticalSpace,
                    AHintRect.Right-HorizontalSpace,
                    AHintRect.Bottom-VerticalSpace);
    UsedWidth:=0;
    LineHeight:=0;
    TokenPos:=Point(ATextRect.Left,ATextRect.Top);
    TokenEnd:=1;
    while (TokenEnd<=length(Line)) do begin
      LastTokenEnd:=TokenEnd;
      ReadRawNextPascalAtom(Line,TokenEnd,TokenStart);
      if TokenEnd<=LastTokenEnd then break;
      if Line[TokenStart]='\' then begin
        // mark found
        if TokenStart>LastTokenEnd then begin
          // there is a gap between last token and this token -> draw that first
          TokenEnd:=TokenStart;
        end else begin
          inc(TokenStart);
          if TokenStart>length(Line) then break;
          TokenEnd:=TokenStart+1;
          // the token is a mark
          case Line[TokenStart] of
          
          '*':
            begin
              // switch to normal font
              Canvas.Font.Color:=TextColor;
              //DebugLn('DrawHint normal');
              continue;
            end;
            
          'b':
            begin
              // switch to bold font
              Canvas.Font.Color:=TextBColor;
              //DebugLn('DrawHint blue');
              continue;
            end;
            
          else
            // the token is a normal character -> paint it
          end;
        end;
      end;
      //DebugLn('DrawHint Token="',copy(Line,TokenStart,TokenEnd-TokenStart),'"');
      
      // calculate token size
      TokenRect:=Bounds(0,0,12345,1234);
      DrawText(Canvas.Handle,@Line[LastTokenEnd],TokenEnd-LastTokenEnd,TokenRect,
               DT_SINGLELINE+DT_CALCRECT+DT_NOCLIP);
      TokenSize:=Point(TokenRect.Right,TokenRect.Bottom);

      if (LineHeight>0) and (TokenPos.X+TokenRect.Right>ATextRect.Right) then
      begin
        // token does not fit into line -> break line
        // fill end of line
        if Draw and (TokenPos.X<AHintRect.Right) then begin
          Canvas.FillRect(Rect(TokenPos.X,TokenPos.Y-VerticalSpace,
                          AHintRect.Right,TokenPos.Y+LineHeight+VerticalSpace));
        end;
        TokenPos:=Point(ATextRect.Left,TokenPos.y+LineHeight+VerticalSpace);
        LineHeight:=0;
      end;

      // token fits into line
      // => draw token
      OffsetRect(TokenRect,TokenPos.x,TokenPos.y);
      if Draw then begin
        Canvas.FillRect(Rect(TokenRect.Left,TokenRect.Top-VerticalSpace,
                             TokenRect.Right,TokenRect.Bottom+VerticalSpace));
        DrawText(Canvas.Handle,@Line[LastTokenEnd],TokenEnd-LastTokenEnd,TokenRect,
                 DT_SINGLELINE+DT_NOCLIP);
      end;
      // update LineHeight and UsedWidth
      if LineHeight<TokenSize.y then
        LineHeight:=TokenSize.y;
      inc(TokenPos.X,TokenSize.x);
      if UsedWidth<TokenPos.X then
        UsedWidth:=TokenPos.X;
    end;
    // fill end of line
    if Draw and (TokenPos.X<AHintRect.Right) and (LineHeight>0) then begin
      Canvas.FillRect(Rect(TokenPos.X,TokenPos.Y-VerticalSpace,
                      AHintRect.Right,TokenPos.Y+LineHeight+VerticalSpace));
    end;

    if (not Draw) and (UsedWidth>0) then
      AHintRect.Right:=UsedWidth+HorizontalSpace;
    AHintRect.Bottom:=TokenPos.Y+LineHeight+VerticalSpace;
  end;
  
var
  i: Integer;
  NewMaxHeight: Integer;
  NewMaxWidth: Integer;
  CurHintRect: TRect;
begin
  //DebugLn('TCodeContextFrm.DrawHints DrawWidth=',dbgs(MaxWidth),' DrawHeight=',dbgs(MaxHeight),' Draw=',dbgs(Draw));
  if Draw then begin
    // TODO: make colors configurable and theme dependent
    BackgroundColor:=clWhite;
    TextColor:=clDkGray;
    TextBColor:=clBlack;
  end;
  HorizontalSpace:=2;
  VerticalSpace:=2;

  if Draw then begin
    Canvas.Brush.Color:=BackgroundColor;
    Canvas.Font.Color:=TextColor;
    Canvas.Pen.Color:=clBlack;
  end;
  NewMaxWidth:=0;
  NewMaxHeight:=0;
  for i:=0 to FHints.Count-1 do begin
    if Draw and (NewMaxHeight>=MaxHeight) then break;
    CurHintRect:=Rect(0,NewMaxHeight,MaxWidth,MaxHeight);
    DrawHint(FHints[i],CurHintRect);
    //DebugLn('TCodeContextFrm.DrawHints i=',dbgs(i),' CurTextRect=',dbgs(CurTextRect),' CurRect=',dbgs(CurRect),' s="',s,'"');
    if CurHintRect.Right>NewMaxWidth then
      NewMaxWidth:=CurHintRect.Right;
    NewMaxHeight:=CurHintRect.Bottom;
  end;
  if Draw then begin
    // fill rest of form
    if NewMaxHeight<MaxHeight then
      Canvas.FillRect(Rect(0,NewMaxHeight,MaxWidth,MaxHeight));
    // draw frame around window
    Canvas.Frame(Rect(0,0,MaxWidth-1,MaxHeight-1));
  end;
  if not Draw then begin
    // adjust max width and height
    if NewMaxWidth<MaxWidth then
      MaxWidth:=NewMaxWidth;
    if NewMaxHeight<MaxHeight then
      MaxHeight:=NewMaxHeight;
  end;
end;

initialization
  {$I codecontextform.lrs}

end.

