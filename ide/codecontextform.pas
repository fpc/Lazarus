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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    The popup tooltip window for the source editor.
    For example for the parameter hints.
}
unit CodeContextForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  // LCL
  LCLProc, LCLType, LCLIntf, LResources, LMessages, Forms, Controls,
  Graphics, Dialogs, Themes, Buttons,
  // SynEdit
  SynEdit, SynEditKeyCmds,
  // CodeTools
  BasicCodeTools, KeywordFuncLists, LinkScanner, CodeCache, FindDeclarationTool,
  IdentCompletionTool, CodeTree, CodeAtom, PascalParserTool, CodeToolManager,
  // IdeIntf
  SrcEditorIntf, LazIDEIntf, IDEImagesIntf,
  // IDE
  IDEProcs, LazarusIDEStrConsts;

type

  { TCodeContextItem }

  TCodeContextItem = class
  public
    Code: string;
    Hint: string;
    NewBounds: TRect;
    CopyAllButton: TSpeedButton;
    destructor Destroy; override;
  end;

  { TCodeContextFrm }

  TCodeContextFrm = class(THintWindow)
    procedure ApplicationIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure CopyAllBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure OnSrcEditStatusChange(Sender: TObject);
  private
    FHints: TFPList; // list of TCodeContextItem
    FIdleConnected: boolean;
    FLastParameterIndex: integer;
    FParamListBracketOpenCodeXYPos: TCodeXYPosition;
    FProcNameCodeXYPos: TCodeXYPosition;
    FSourceEditorTopIndex: integer;
    FBtnWidth: integer;
    fDestroying: boolean;
    procedure CreateHints(const CodeContexts: TCodeContextInfo);
    procedure ClearMarksInHints;
    function GetHints(Index: integer): TCodeContextItem;
    procedure MarkCurrentParameterInHints(ParameterIndex: integer); // 0 based
    procedure CalculateHintsBounds;
    procedure DrawHints(var MaxWidth, MaxHeight: Integer; Draw: boolean);
    procedure CompleteParameters(DeclCode: string);
    procedure ClearHints;
    procedure SetIdleConnected(AValue: boolean);
    procedure SetCodeContexts(const CodeContexts: TCodeContextInfo);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure WMNCHitTest(var Message: TLMessage); message LM_NCHITTEST;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateHints;
    procedure Paint; override;
    property ProcNameCodeXYPos: TCodeXYPosition read FProcNameCodeXYPos;
    property ParamListBracketOpenCodeXYPos: TCodeXYPosition
                                            read FParamListBracketOpenCodeXYPos;
    property SourceEditorTopIndex: integer read FSourceEditorTopIndex;
    property LastParameterIndex: integer read FLastParameterIndex;
    property Hints[Index: integer]: TCodeContextItem read GetHints;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

var
  CodeContextFrm: TCodeContextFrm = nil;

function ShowCodeContext(Code: TCodeBuffer): boolean;

implementation

type
  TWinControlAccess = class(TWinControl);

function ShowCodeContext(Code: TCodeBuffer): boolean;
var
  LogCaretXY: TPoint;
  CodeContexts: TCodeContextInfo;
begin
  Result := False;
  LogCaretXY := SourceEditorManagerIntf.ActiveEditor.CursorTextXY;
  CodeContexts := nil;
  try
    if not CodeToolBoss.FindCodeContext(Code, LogCaretXY.X, LogCaretXY.Y, CodeContexts) or
      (CodeContexts = nil) or (CodeContexts.Count = 0) then
      Exit;
    if CodeContextFrm = nil then
      CodeContextFrm := TCodeContextFrm.Create(LazarusIDE.OwningComponent);
    CodeContextFrm.DisableAlign;
    try
      CodeContextFrm.SetCodeContexts(CodeContexts);
      CodeContextFrm.Visible := True;
    finally
      CodeContextFrm.EnableAlign;
    end;
    Result := True;
  finally
    CodeContexts.Free;
  end;
end;

{ TCodeContextItem }

destructor TCodeContextItem.Destroy;
begin
  FreeAndNil(CopyAllButton);
  inherited Destroy;
end;

{ TCodeContextFrm }

procedure TCodeContextFrm.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  if not Visible then exit;
  UpdateHints;
  IdleConnected:=false;
end;

procedure TCodeContextFrm.CopyAllBtnClick(Sender: TObject);
var
  i: LongInt;
  Item: TCodeContextItem;
begin
  i:=FHints.Count-1;
  while (i>=0) do begin
    Item:=Hints[i];
    if Item.CopyAllButton=Sender then begin
      //debugln(['TCodeContextFrm.CopyAllBtnClick Hint="',Item.Code,'"']);
      CompleteParameters(Item.Code);
      exit;
    end;
    dec(i);
  end;
end;

procedure TCodeContextFrm.FormCreate(Sender: TObject);
begin
  FBtnWidth:=16;
  FHints:=TFPList.Create;
  IdleConnected:=true;
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorStatus,@OnSrcEditStatusChange);
end;

procedure TCodeContextFrm.FormDestroy(Sender: TObject);
begin
  if SourceEditorManagerIntf<>nil then
    SourceEditorManagerIntf.UnregisterChangeEvent(semEditorStatus,@OnSrcEditStatusChange);
  IdleConnected:=false;
  ClearHints;
  FreeAndNil(FHints);
end;

procedure TCodeContextFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  SrcEdit: TSourceEditorInterface;
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
    Hide
  else if SourceEditorManagerIntf<>nil then begin
    SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
    if SrcEdit=nil then
      Hide
    else begin
      // redirect keys
      TWinControlAccess(SrcEdit.EditorControl).KeyDown(Key,Shift);
      SetActiveWindow(SourceEditorManagerIntf.ActiveSourceWindow.Handle);
    end;
  end;
end;

procedure TCodeContextFrm.FormPaint(Sender: TObject);
var
  DrawWidth: LongInt;
  DrawHeight: LongInt;
begin
  DrawWidth:=ClientWidth;
  DrawHeight:=ClientHeight;
  DrawHints(DrawWidth,DrawHeight,true);
end;

procedure TCodeContextFrm.FormUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
var
  SrcEdit: TSourceEditorInterface;
  ASynEdit: TCustomSynEdit;
begin
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then begin
    Hide;
  end else begin
    ASynEdit:=(SrcEdit.EditorControl as TCustomSynEdit);
    ASynEdit.CommandProcessor(ecChar,UTF8Key,nil);
  end;
end;

procedure TCodeContextFrm.OnSrcEditStatusChange(Sender: TObject);
begin
  IdleConnected:=true;
end;

procedure TCodeContextFrm.SetCodeContexts(const CodeContexts: TCodeContextInfo);
begin
  FillChar(FProcNameCodeXYPos,SizeOf(FProcNameCodeXYPos),0);
  FillChar(FParamListBracketOpenCodeXYPos,SizeOf(FParamListBracketOpenCodeXYPos),0);

  if CodeContexts<>nil then begin
    if (CodeContexts.ProcNameAtom.StartPos>0) then begin
      CodeContexts.Tool.MoveCursorToCleanPos(CodeContexts.ProcNameAtom.StartPos);
      CodeContexts.Tool.CleanPosToCaret(CodeContexts.Tool.CurPos.StartPos,
                                        FProcNameCodeXYPos);
      CodeContexts.Tool.ReadNextAtom;// read proc name
      CodeContexts.Tool.ReadNextAtom;// read bracket open
      if CodeContexts.Tool.CurPos.Flag
        in [cafRoundBracketOpen,cafEdgedBracketOpen]
      then begin
        CodeContexts.Tool.CleanPosToCaret(CodeContexts.Tool.CurPos.StartPos,
                                          FParamListBracketOpenCodeXYPos);
      end;
    end;
  end;
  
  CreateHints(CodeContexts);
  CalculateHintsBounds;
end;

procedure TCodeContextFrm.UpdateHints;
var
  SrcEdit: TSourceEditorInterface;
  CurTextXY: TPoint;
  ASynEdit: TSynEdit;
  NewParameterIndex: Integer;
  BracketPos: TPoint;
  Line: string;
  Code: String;
  TokenEnd: LongInt;
  TokenStart: LongInt;
  KeepOpen: Boolean;
  BracketLevel: Integer;
  i: Integer;
  Item: TCodeContextItem;
begin
  if not Visible then exit;
  
  KeepOpen:=false;
  NewParameterIndex:=-1;
  try
    if not Application.Active then exit;

    // check Source Editor
    if SourceEditorManagerIntf=nil then exit;
    SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
    if (SrcEdit=nil) or (SrcEdit.CodeToolsBuffer<>ProcNameCodeXYPos.Code) then
      exit;
    if SrcEdit.TopLine<>FSourceEditorTopIndex then exit;

    CurTextXY:=SrcEdit.CursorTextXY;
    BracketPos:=Point(ParamListBracketOpenCodeXYPos.X,
                      ParamListBracketOpenCodeXYPos.Y);
    if ComparePoints(CurTextXY,BracketPos)<=0 then begin
      // cursor moved in front of parameter list
      exit;
    end;

    // find out, if cursor is in procedure call and where
    ASynEdit:=SrcEdit.EditorControl as TSynEdit;

    Line:=ASynEdit.Lines[BracketPos.Y-1];
    if (length(Line)<BracketPos.X) or (not (Line[BracketPos.X] in ['(','[']))
    then begin
      // bracket lost -> something changed -> hints became invalid
      exit;
    end;
    
    // collect the lines from bracket open to cursor
    Code:=StringListPartToText(ASynEdit.Lines,BracketPos.Y-1,CurTextXY.Y-1,#10);
    if CurTextXY.Y<=ASynEdit.Lines.Count then begin
      Line:=ASynEdit.Lines[CurTextXY.Y-1];
      if length(Line)>=CurTextXY.X then
        SetLength(Code,length(Code)-length(Line)+CurTextXY.X-1);
    end;
    //DebugLn('TCodeContextFrm.UpdateHints Code="',DbgStr(Code),'"');

    // parse the code
    TokenEnd:=BracketPos.X;
    BracketLevel:=0;
    repeat
      ReadRawNextPascalAtom(Code,TokenEnd,TokenStart);
      if TokenEnd=TokenStart then break;
      case Code[TokenStart] of
      '(','[':
        begin
          inc(BracketLevel);
          if BracketLevel=1 then
            NewParameterIndex:=0;
        end;
      ')',']':
        begin
          if BracketLevel=1 then begin
            if Code[TokenStart]=']' then begin
              ReadRawNextPascalAtom(Code,TokenEnd,TokenStart);
              if TokenEnd=TokenStart then exit;
              if Code[TokenStart]='[' then begin
                inc(NewParameterIndex);
                continue; // [][] is full version of [,]
              end
            end else
              exit;// cursor behind procedure call
          end;
          dec(BracketLevel);
        end;
      ',':
        if BracketLevel=1 then inc(NewParameterIndex);
      else
        if IsIdentStartChar[Code[TokenStart]] then begin
          if CompareIdentifiers(@Code[TokenStart],'end')=0 then
            break;// cursor behind procedure call
        end;
      end;
    until false;
    KeepOpen:=true;

    // show buttons
    for i:=0 to FHints.Count-1 do begin
      Item:=TCodeContextItem(FHints[i]);
      if Item.CopyAllButton <> nil then begin
        Item.CopyAllButton.BoundsRect:=Item.NewBounds;
        Item.CopyAllButton.Visible:=Item.NewBounds.Right>0;
      end;
    end;
  finally
    if not KeepOpen then
      Hide
    else if NewParameterIndex<>LastParameterIndex then
      MarkCurrentParameterInHints(NewParameterIndex);
  end;
end;

procedure TCodeContextFrm.WMNCHitTest(var Message: TLMessage);
begin
  Message.Result := HTCLIENT;
end;

procedure TCodeContextFrm.CreateHints(const CodeContexts: TCodeContextInfo);

  function FindBaseType(Tool: TFindDeclarationTool; Node: TCodeTreeNode;
    var s: string): boolean;
  var
    Expr: TExpressionType;
    Params: TFindDeclarationParams;
    ExprTool: TFindDeclarationTool;
    ExprNode: TCodeTreeNode;
  begin
    Result:=false;
    Params:=TFindDeclarationParams.Create(Tool, Node);
    try
      try
        Expr:=Tool.ConvertNodeToExpressionType(Node,Params);
        if (Expr.Desc=xtContext) and (Expr.Context.Node<>nil) then begin
          ExprTool:=Expr.Context.Tool;
          ExprNode:=Expr.Context.Node;
          if ExprNode.Desc=ctnReferenceTo then begin
            ExprNode:=ExprNode.FirstChild;
            if ExprNode=nil then exit;
          end;
          case ExprNode.Desc of
          ctnProcedureType:
            begin
              s:=s+ExprTool.ExtractProcHead(ExprNode,
                 [phpWithVarModifiers,phpWithParameterNames,phpWithDefaultValues,
                 phpWithResultType]);
              Result:=true;
            end;
          ctnOpenArrayType,ctnRangedArrayType:
            begin
              s:=s+ExprTool.ExtractArrayRanges(ExprNode,[]);
              Result:=true;
            end;
          end;
        end else if Expr.Desc in (xtAllStringTypes+xtAllWideStringTypes-[xtShortString])
        then begin
          s:=s+'[Index: 1..high(PtrUInt)]';
          Result:=true;
        end else if Expr.Desc=xtShortString then begin
          s:=s+'[Index: 0..255]';
          Result:=true;
        end;
        if not Result then
          debugln(['TCodeContextFrm.CreateHints.FindBaseType: not yet supported: ',ExprTypeToString(Expr)]);
      except
      end;
    finally
      Params.Free;
    end;
  end;

var
  i: Integer;
  CurExprType: TExpressionType;
  CodeNode: TCodeTreeNode;
  CodeTool: TFindDeclarationTool;
  s: String;
  p: Integer;
  CurContext: TCodeContextInfoItem;
  Btn: TSpeedButton;
  j: Integer;
  Code: String;
  Item: TCodeContextItem;
begin
  ClearHints;
  if (CodeContexts=nil) or (CodeContexts.Count=0) then exit;
  for i:=0 to CodeContexts.Count-1 do begin
    CurContext:=CodeContexts[i];
    CurExprType:=CurContext.Expr;
    Code:=ExpressionTypeDescNames[CurExprType.Desc];
    if CurExprType.Context.Node<>nil then begin
      CodeNode:=CurExprType.Context.Node;
      CodeTool:=CurExprType.Context.Tool;
      case CodeNode.Desc of
      ctnProcedure:
        begin
          Code:=CodeTool.ExtractProcHead(CodeNode,
              [phpWithVarModifiers,phpWithParameterNames,phpWithDefaultValues,
               phpWithResultType]);
        end;
      ctnProperty:
        begin
          if CodeTool.PropertyNodeHasParamList(CodeNode) then begin
            Code:=CodeTool.ExtractProperty(CodeNode,
                [phpWithVarModifiers,phpWithParameterNames,phpWithDefaultValues,
                 phpWithResultType]);
          end else if not CodeTool.PropNodeIsTypeLess(CodeNode) then begin
            Code:=CodeTool.ExtractPropName(CodeNode,false);
            if not FindBaseType(CodeTool,CodeNode,Code) then
              continue;
          end else begin
            // ignore properties without type
            continue;
          end;
        end;
      ctnVarDefinition:
        begin
          Code:=CodeTool.ExtractDefinitionName(CodeNode);
          if not FindBaseType(CodeTool,CodeNode,Code) then
            continue; // ignore normal variables
        end;
      end;
    end else if CurContext.Params<>nil then begin
      // compiler function
      Code:=CurContext.ProcName+'('+CurContext.Params.DelimitedText+')';
      if CurContext.ResultType<>'' then
        Code:=Code+':'+CurContext.ResultType;
    end;
    // insert spaces
    for p:=length(Code)-1 downto 1 do begin
      if (Code[p] in [',',';',':']) and (Code[p+1]<>' ') then
        System.Insert(' ',Code,p+1);
    end;
    Code:=Trim(Code);
    s:=Code;
    // mark the mark characters
    for p:=length(s) downto 1 do
      if s[p]='\' then
        System.Insert('\',s,p+1);
    // add hint if not already exists
    j:=FHints.Count-1;
    while (j>=0) and (CompareText(Hints[j].Code,Code)<>0) do
      dec(j);
    if j<0 then begin
      Item:=TCodeContextItem.Create;
      Item.Code:=Code;
      Item.Hint:=s;
      Btn:=TSpeedButton.Create(Self);
      Item.CopyAllButton:=Btn;
      Btn.Name:='CopyAllSpeedButton'+IntToStr(i+1);
      Btn.OnClick:=@CopyAllBtnClick;
      Btn.Visible:=false;
      IDEImages.AssignImage(Btn, 'laz_copy');
      Btn.Flat:=true;
      Btn.Parent:=Self;
      FHints.Add(Item);
    end;
  end;
  if FHints.Count=0 then begin
    Item:=TCodeContextItem.Create;
    Item.Code:='';
    Item.Hint:=lisNoHints;
    FHints.Add(Item);
  end;
  MarkCurrentParameterInHints(CodeContexts.ParameterIndex-1);
end;

procedure TCodeContextFrm.ClearMarksInHints;
// remove all marks except the \\ marks
var
  i: Integer;
  s: string;
  p: Integer;
  Item: TCodeContextItem;
begin
  for i:=0 to FHints.Count-1 do begin
    Item:=Hints[i];
    s:=Item.Hint;
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
    Item.Hint:=s;
  end;
end;

function TCodeContextFrm.GetHints(Index: integer): TCodeContextItem;
begin
  Result:=TCodeContextItem(FHints[Index]);
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
      '(','{','[':
        inc(BracketLevel);
      ')','}',']':
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
      'a'..'z','A'..'Z','_','0'..'9':
        if (BracketLevel=1) and (not ReadingType) then begin
          WordStart:=p;
          while (p<=length(Result)) and IsDottedIdentChar[Result[p]] do
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
  Item: TCodeContextItem;
begin
  //DebugLn('TCodeContextFrm.MarkCurrentParameterInHints FLastParameterIndex=',dbgs(FLastParameterIndex),' ParameterIndex=',dbgs(ParameterIndex));
  ClearMarksInHints;
  for i:=0 to FHints.Count-1 do begin
    Item:=Hints[i];
    Item.Hint:=MarkCurrentParameterInHint(Item.Hint);
  end;
  FLastParameterIndex:=ParameterIndex;
  Invalidate;
end;

procedure TCodeContextFrm.CalculateHintsBounds;
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
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
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
  ScreenXY:=SrcEdit.EditorControl.ClientToScreen(ClientXY);
  FSourceEditorTopIndex:=SrcEdit.TopLine;

  // calculate size of hints
  DrawWidth:=SourceEditorManagerIntf.ActiveSourceWindow.ClientWidth;
  DrawHeight:=ScreenXY.Y-GetParentForm(SrcEdit.EditorControl).Monitor.WorkareaRect.Top-10;
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
  NewBounds:=Bounds(ScreenXY.X,ScreenXY.Y-4,DrawWidth,DrawHeight);

  // move form
  BoundsRect:=NewBounds;
end;

procedure TCodeContextFrm.DrawHints(var MaxWidth, MaxHeight: Integer;
  Draw: boolean);
var
  LeftSpace, RightSpace: Integer;
  VerticalSpace: Integer;
  BackgroundColor, TextGrayColor, TextColor, PenColor: TColor;
  TextGrayStyle, TextStyle: TFontStyles;

  procedure DrawHint(Index: integer; var AHintRect: TRect);
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
    Line: string;
    Item: TCodeContextItem;
    y: LongInt;
    r: TRect;
  begin
    Item:=Hints[Index];
    Line:=Item.Hint;
    ATextRect:=Rect(AHintRect.Left+LeftSpace,
                    AHintRect.Top+VerticalSpace,
                    AHintRect.Right-RightSpace,
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
              if Draw then begin
                Canvas.Font.Color:=TextGrayColor;
                Canvas.Font.Style:=TextGrayStyle;
              end;
              //DebugLn('DrawHint gray');
              continue;
            end;
            
          'b':
            begin
              // switch to normal font
              if Draw then begin
                Canvas.Font.Color:=TextColor;
                Canvas.Font.Style:=TextStyle;
              end;
              //DebugLn('DrawHint normal');
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
      {$IFDEF EnableCCFFontMin}
      // workaround for bug 22190
      if TokenSize.y<14 then TokenSize.y:=14;
      {$ENDIF}
      //DebugLn(['DrawHint Draw="',Draw,'" Token="',copy(Line,TokenStart,TokenEnd-TokenStart),'" TokenSize=',dbgs(TokenSize)]);

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
        DrawText(Canvas.Handle,@Line[LastTokenEnd],TokenEnd-LastTokenEnd,
                 TokenRect,DT_SINGLELINE+DT_NOCLIP);
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
      AHintRect.Right:=UsedWidth+RightSpace;
    AHintRect.Bottom:=TokenPos.Y+LineHeight+VerticalSpace;

    if Draw and (Item.CopyAllButton<>nil) then begin
      // move button at end of first line
      y:=ATextRect.Top;
      if LineHeight>FBtnWidth then
        inc(y,(LineHeight-FBtnWidth) div 2);
      Item.NewBounds:=Bounds(AHintRect.Right-RightSpace-1,y,FBtnWidth,FBtnWidth);
      r:=Item.CopyAllButton.BoundsRect;
      if not CompareRect(@r,@Item.NewBounds) then
        IdleConnected:=true;
    end;
    //debugln(['DrawHint ',y,' Line="',dbgstr(Line),'" LineHeight=',LineHeight,' ']);
  end;
  
var
  i: Integer;
  NewMaxHeight: Integer;
  NewMaxWidth: Integer;
  CurHintRect: TRect;
  Details: TThemedElementDetails;
begin
  //DebugLn('TCodeContextFrm.DrawHints DrawWidth=',dbgs(MaxWidth),' DrawHeight=',dbgs(MaxHeight),' Draw=',dbgs(Draw));
  if Draw then begin
    // make colors theme dependent
    BackgroundColor:=clInfoBk;
    TextGrayColor:=clInfoText;
    TextGrayStyle:=[];
    TextColor:=clInfoText;
    TextStyle:=[fsBold];
    PenColor:=clBlack;
  end;
  LeftSpace:=2;
  RightSpace:=2+FBtnWidth;
  VerticalSpace:=2;

  if Draw then begin
    Canvas.Brush.Color:=BackgroundColor;
    Canvas.Font.Color:=TextGrayColor;
    Canvas.Font.Style:=TextGrayStyle;
    Canvas.Pen.Color:=PenColor;
    Details := ThemeServices.GetElementDetails(tttStandardLink);
    ThemeServices.DrawElement(Canvas.Handle, Details, ClientRect);
  end else begin
    Canvas.Font.Style:=[fsBold];
  end;
  NewMaxWidth:=0;
  NewMaxHeight:=0;
  for i:=0 to FHints.Count-1 do begin
    if Draw and (NewMaxHeight>=MaxHeight) then break;
    CurHintRect:=Rect(0,NewMaxHeight,MaxWidth,MaxHeight);
    DrawHint(i,CurHintRect);
    if CurHintRect.Right>NewMaxWidth then
      NewMaxWidth:=CurHintRect.Right;
    NewMaxHeight:=CurHintRect.Bottom;
  end;
  // for fractionals add some space
  inc(NewMaxWidth,2);
  inc(NewMaxHeight,2);
  // add space for the copy all button
  inc(NewMaxWidth,16);

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

procedure TCodeContextFrm.CompleteParameters(DeclCode: string);
// add the parameter names in the source editor

  function ReadNextAtom(ASynEdit: TSynEdit; var TokenLine, TokenEnd: integer;
    out TokenStart: integer): string;
  var
    Line: string;
  begin
    while TokenLine<=ASynEdit.Lines.Count do begin
      Line:=ASynEdit.Lines[TokenLine-1];
      ReadRawNextPascalAtom(Line,TokenEnd,TokenStart);
      if TokenStart<TokenEnd then begin
        Result:=copy(Line,TokenStart,TokenEnd-TokenStart);
        exit;
      end;
      inc(TokenLine);
      TokenEnd:=1;
    end;
    TokenStart:=TokenEnd;
    Result:='';
  end;

  procedure AddParameters(ASynEdit: TSynEdit; Y, X: integer;
    AddComma, AddCLoseBracket: boolean;
    StartIndex: integer);
  var
    NewCode: String;
    TokenStart: Integer;
    BracketLevel: Integer;
    ParameterIndex: Integer;
    TokenEnd: integer;
    LastToken: String;
    Indent: LongInt;
    XY: TPoint;
  begin
    TokenEnd:=1;
    BracketLevel:=0;
    ParameterIndex:=-1;
    NewCode:='';
    LastToken:='';
    repeat
      ReadRawNextPascalAtom(DeclCode,TokenEnd,TokenStart);
      if TokenEnd=TokenStart then break;
      case DeclCode[TokenStart] of
      '(','[':
        begin
          inc(BracketLevel);
          if BracketLevel=1 then
            ParameterIndex:=0;
        end;
      ')',']':
        begin
          dec(BracketLevel);
          if BracketLevel=0 then begin
            // closing bracket found
            break;
          end;
        end;
      ',',':':
        if BracketLevel=1 then begin
          if (LastToken<>'') and (IsIdentStartChar[LastToken[1]])
          and (ParameterIndex>=StartIndex) then begin
            // add parameter
            if AddComma then
              NewCode:=NewCode+',';
            NewCode:=NewCode+LastToken;
            AddComma:=true;
          end;
          if DeclCode[TokenStart]=',' then
            inc(ParameterIndex);
        end;
      ';':
        if BracketLevel=1 then
          inc(ParameterIndex);
      else

      end;
      LastToken:=copy(DeclCode,TokenStart,TokenEnd-TokenStart);
    until false;
    if NewCode='' then exit;
    if AddCLoseBracket then
      NewCode+=')';
    // format insertion
    Indent:=GetLineIndentWithTabs(ASynEdit.Lines[Y-1],X,ASynEdit.TabWidth);
    if Y<>FParamListBracketOpenCodeXYPos.Y then
      dec(Indent,CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.Indent);
    NewCode:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
      NewCode,Indent,[],X);
    delete(NewCode,1,Indent);
    if NewCode='' then begin
      ShowMessage(lisAllParametersOfThisFunctionAreAlreadySetAtThisCall);
      exit;
    end;
    // insert
    ASynEdit.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TCodeContextFrm.CompleteParameters'){$ENDIF};
    try
      XY:=Point(X,Y);
      ASynEdit.BlockBegin:=XY;
      ASynEdit.BlockEnd:=XY;
      ASynEdit.LogicalCaretXY:=XY;
      ASynEdit.SelText:=NewCode;
    finally
      ASynEdit.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TCodeContextFrm.CompleteParameters'){$ENDIF};
    end;
  end;

var
  SrcEdit: TSourceEditorInterface;
  BracketPos: TPoint;
  ASynEdit: TSynEdit;
  Line: string;
  TokenLine, TokenEnd, TokenStart: LongInt;
  LastTokenLine, LastTokenEnd: LongInt;
  BracketLevel: Integer;
  ParameterIndex: Integer;
  Token: String;
  LastToken: String;
  NeedComma: Boolean;
begin
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if (SrcEdit=nil) or (SrcEdit.CodeToolsBuffer<>ProcNameCodeXYPos.Code) then
    exit;
  BracketPos:=Point(ParamListBracketOpenCodeXYPos.X,
                    ParamListBracketOpenCodeXYPos.Y);
  // find out, if cursor is in procedure call and where
  ASynEdit:=SrcEdit.EditorControl as TSynEdit;

  Line:=ASynEdit.Lines[BracketPos.Y-1];
  if (length(Line)<BracketPos.X) or (not (Line[BracketPos.X] in ['(','[']))
  then begin
    // bracket lost -> something changed -> hints became invalid
    exit;
  end;

  // parse the code
  TokenLine:=BracketPos.Y;
  TokenEnd:=BracketPos.X;
  //debugln(['TCodeContextFrm.CompleteParameters START BracketPos=',dbgs(BracketPos)]);
  TokenStart:=TokenEnd;
  BracketLevel:=0;
  ParameterIndex:=-1;
  Token:='';
  repeat
    LastTokenLine:=TokenLine;
    LastTokenEnd:=TokenEnd;
    LastToken:=Token;
    Token:=ReadNextAtom(ASynEdit,TokenLine,TokenEnd,TokenStart);
    //debugln(['TCodeContextFrm.CompleteParameters Token="',Token,'" ParameterIndex=',ParameterIndex]);
    if TokenEnd=TokenStart then break;
    case Token[1] of
    '(','[':
      begin
        inc(BracketLevel);
        if BracketLevel=1 then
          ParameterIndex:=0;
      end;
    ')',']':
      begin
        dec(BracketLevel);
        if BracketLevel=0 then break;
      end;
    ',':
      if BracketLevel=1 then inc(ParameterIndex);
    ';':
      break; // missing close bracket => cursor behind procedure call
    else
      if IsIdentStartChar[Token[1]] then begin
        if CompareIdentifiers(PChar(Token),'end')=0 then
          break;// missing close bracket => cursor behind procedure call
      end;
    end;
  until false;
  NeedComma:=(LastToken<>',') and (LastToken<>'(') and (LastToken<>'[');
  if NeedComma then inc(ParameterIndex);
  //debugln(['TCodeContextFrm.CompleteParameters BracketLevel=',BracketLevel,' NeedComma=',NeedComma,' ParameterIndex=',ParameterIndex]);
  if BracketLevel=0 then begin
    // closing bracket found
    //debugln(['TCodeContextFrm.CompleteParameters y=',LastTokenLine,' x=',LastTokenEnd,' ParameterIndex=',ParameterIndex]);
    AddParameters(ASynEdit,LastTokenLine,LastTokenEnd,NeedComma,false,ParameterIndex);
  end else if BracketLevel=1 then begin
    // missing closing bracket
    AddParameters(ASynEdit,LastTokenLine,LastTokenEnd,NeedComma,true,ParameterIndex);
  end;
end;

procedure TCodeContextFrm.ClearHints;
var
  i: Integer;
begin
  for i:=0 to FHints.Count-1 do
    FreeAndNil(Hints[i].CopyAllButton);
  for i:=0 to FHints.Count-1 do
    TObject(FHints[i]).Free;
  FHints.Clear;
end;

procedure TCodeContextFrm.SetIdleConnected(AValue: boolean);
begin
  if fDestroying then AValue:=false;
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@ApplicationIdle)
  else
    Application.RemoveOnIdleHandler(@ApplicationIdle);
end;

procedure TCodeContextFrm.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
  begin
    if FHints<>nil then
      for i:=0 to FHints.Count-1 do
        if Hints[i].CopyAllButton=AComponent then
          Hints[i].CopyAllButton:=nil;
  end;
end;

procedure TCodeContextFrm.Paint;
begin
  FormPaint(Self);
end;

constructor TCodeContextFrm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnDestroy:=@FormDestroy;
  OnKeyDown:=@FormKeyDown;
  OnUTF8KeyPress:=@FormUTF8KeyPress;
  FormCreate(Self);
end;

destructor TCodeContextFrm.Destroy;
begin
  fDestroying:=true;
  IdleConnected:=false;
  if CodeContextFrm=Self then
    CodeContextFrm:=nil;
  inherited Destroy;
end;

end.

