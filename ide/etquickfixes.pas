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
    Standard Quick Fixes - small tools to fix (compiler) messages.
}
unit etQuickFixes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEExternToolIntf, IDEMsgIntf, LazIDEIntf, IDEDialogs,
  Menus, Dialogs, etFPCMsgParser, CodeToolManager, CodeCache, CodeTree,
  CodeAtom, BasicCodeTools, LazLogger, AvgLvlTree, LazFileUtils;

type

  { TQuickFix_Hide - hide via IDE directive %H- }

  TQuickFix_Hide = class(TMsgQuickFix)
  public
    function IsApplicable(Msg: TMessageLine): boolean;
    procedure CreateMenuItems(Fixes: TMsgQuickFixes); override;
    procedure QuickFix(Fixes: TMsgQuickFixes; Msg: TMessageLine); override;
  end;

  { TQuickFixIdentifierNotFoundAddLocal }

  TQuickFixIdentifierNotFoundAddLocal = class(TMsgQuickFix)
  public
    function IsApplicable(Msg: TMessageLine): boolean;
    procedure CreateMenuItems(Fixes: TMsgQuickFixes); override;
    procedure QuickFix(Fixes: TMsgQuickFixes; Msg: TMessageLine); override;
  end;

  { TIDEQuickFixes }

  TIDEQuickFixes = class(TMsgQuickFixes)
  private
    FParentMenuItem: TMenuItem;
    fMenuItemToInfo: TPointerToPointerTree; // TMenuItem to TMenuItemInfo
    procedure MenuItemClick(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnPopupMenu(aParentMenuItem: TMenuItem);
    procedure SetMsgLines(aMsg: TMessageLine);
    procedure AddMsgLine(aMsg: TMessageLine);
    procedure ClearLines;
    function AddMenuItem(Fix: TMsgQuickFix; Msg: TMessageLine; aCaption: string;
      aTag: PtrInt=0): TMenuItem; override;
    function OpenMsg(Msg: TMessageLine): boolean;
    property ParentMenuItem: TMenuItem read FParentMenuItem write FParentMenuItem;
  end;

var
  IDEQuickFixes: TIDEQuickFixes = nil;

implementation

type
  TMenuItemInfo = class
  public
    MenuItem: TMenuItem;
    Fix: TMsgQuickFix;
    Msg: TMessageLine;
  end;

procedure ShowError(Msg: string);
begin
  IDEMessageDialog('QuickFix error',Msg,mtError,[mbCancel]);
end;

function IsIdentifierInCode(Code: TCodeBuffer; X,Y: integer;
  Identifier, ErrorMsg: string): boolean;
var
  p: integer;
  IdentStart: integer;
  IdentEnd: integer;
begin
  Result:=false;
  if Code=nil then begin
    ShowError(ErrorMsg+' (Code=nil)');
    exit;
  end;
  Code.LineColToPosition(Y,X,p);
  if p<1 then begin
    ShowError(ErrorMsg+' (position outside of source');
    exit;
  end;
  GetIdentStartEndAtPosition(Code.Source,p,IdentStart,IdentEnd);
  if SysUtils.CompareText(Identifier,copy(Code.Source,IdentStart,IdentEnd-IdentStart))<>0
  then begin
    ShowError(ErrorMsg);
    exit;
  end;
  Result:=true;
end;

{ TQuickFixIdentifierNotFoundAddLocal }

function TQuickFixIdentifierNotFoundAddLocal.IsApplicable(Msg: TMessageLine
  ): boolean;
var
  Code: TCodeBuffer;
  Tool: TCodeTool;
  CleanPos: integer;
  Node: TCodeTreeNode;
  Identifier: String;
begin
  Result:=false;
  if (Msg.SubTool<>SubToolFPC)
  or (Msg.MsgID<>5000) // identifier not found "$1"
  or (not Msg.HasSourcePosition)
  then exit;
  Identifier:=TFPCParser.GetFPCMsgValue1(Msg);
  if not IsValidIdent(Identifier) then exit;

  // check if message position is at end of identifier
  // (FPC gives position of end of identifier)
  Code:=CodeToolBoss.LoadFile(Msg.GetFullFilename,true,false);
  if Code=nil then exit;
  if not CodeToolBoss.Explore(Code,Tool,false) then exit;
  if Tool.CaretToCleanPos(CodeXYPosition(Msg.Column,Msg.Line,Code),CleanPos)<>0 then exit;
  Node:=Tool.FindDeepestNodeAtPos(CleanPos,false);
  if Node=nil then exit;
  if not (Node.Desc in AllPascalStatements) then exit;
  Tool.MoveCursorToCleanPos(CleanPos);
  Tool.ReadPriorAtom;
  if not Tool.AtomIs(Identifier) then exit;
  Tool.ReadPriorAtom;
  if (Tool.CurPos.Flag in [cafPoint,cafRoundBracketClose,cafEdgedBracketClose,
                           cafEnd])
  then exit;
  Result:=true;
end;

procedure TQuickFixIdentifierNotFoundAddLocal.CreateMenuItems(
  Fixes: TMsgQuickFixes);
var
  Msg: TMessageLine;
  Identifier: String;
begin
  if Fixes.LineCount<>1 then exit;
  Msg:=Fixes.Lines[0];
  if not IsApplicable(Msg) then exit;
  Identifier:=TFPCParser.GetFPCMsgValue1(Msg);
  if Identifier='' then exit;
  Fixes.AddMenuItem(Self,Msg,'Create local variable "'+Identifier+'"');
  // ToDo: add private/public variable
end;

procedure TQuickFixIdentifierNotFoundAddLocal.QuickFix(Fixes: TMsgQuickFixes;
  Msg: TMessageLine);
var
  Identifier: String;
  Code: TCodeBuffer;
  NewCode: TCodeBuffer;
  NewX: integer;
  NewY: integer;
  NewTopLine: integer;
begin
  if Msg=nil then exit;
  Identifier:=TFPCParser.GetFPCMsgValue1(Msg);
  if Identifier='' then exit;

  if not LazarusIDE.BeginCodeTools then begin
    DebugLn(['TQuickFixIdentifierNotFoundAddLocal.Execute failed because IDE busy']);
    exit;
  end;

  Code:=CodeToolBoss.LoadFile(Msg.GetFullFilename,true,false);
  if Code=nil then exit;

  if not IsIdentifierInCode(Code,Msg.Column,Msg.Line,Identifier,
    Identifier+' not found in '+Code.Filename
       +' at line '+IntToStr(Msg.Line)+', column '+IntToStr(Msg.Column)+'.'
       +LineEnding+'Maybe the message is outdated.')
  then exit;

  if not CodeToolBoss.CreateVariableForIdentifier(Code,Msg.Column,Msg.Line,-1,
             NewCode,NewX,NewY,NewTopLine)
  then begin
    LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;

  // success
  Msg.MarkFixed;
end;

{ TQuickFix_Hide }

procedure TQuickFix_Hide.QuickFix(Fixes: TMsgQuickFixes; Msg: TMessageLine);
var
  Code: TCodeBuffer;

  procedure Fix(CurMsg: TMessageLine);
  var
    p: integer;
    aFilename: String;
  begin
    aFilename:=CurMsg.GetFullFilename;
    if (Code=nil) or (CompareFilenames(aFilename,Code.Filename)<>0) then begin
      Code:=CodeToolBoss.LoadFile(aFilename,true,false);
      if Code=nil then begin
        DebugLn(['TQuickFix_Hide.MenuItemClick ']);
        // ToDo: IDEMessageDialog
        exit;
      end;
    end;
    Code.LineColToPosition(CurMsg.Line,CurMsg.Column,p);
    if p<1 then begin
      DebugLn(['TQuickFix_Hide failed because invalid line, column']);
      {IDEMessageDialog(lisCCOErrorCaption,
        Format(lisInvalidLineColumnInMessage, [LineEnding, Msg.Msg]),
        mtError, [mbCancel]);}
      exit;
    end;

    debugln(['TQuickFix_Hide.MenuItemClick ',Code.Filename,' ',CurMsg.Line,',',CurMsg.Column]);
    Code.Insert(p,'{%H-}');
    CurMsg.Flags:=CurMsg.Flags+[mlfHiddenByIDEDirectiveValid,mlfHiddenByIDEDirective];
    CurMsg.MarkFixed;
  end;

var
  Tree: TAvgLvlTree;
  Node: TAvgLvlTreeNode;
  i: Integer;
begin
  Tree:=TAvgLvlTree.Create(@CompareMsgLinesSrcPos);
  try
    // get all messages to hide and sort them for position
    if Msg=nil then begin
      for i:=0 to Fixes.LineCount-1 do begin
        Msg:=Fixes.Lines[i];
        if not IsApplicable(Msg) then continue;
        Tree.Add(Msg);
      end;
    end else if IsApplicable(Msg) then
      Tree.Add(Msg);
    if Tree.Count=0 then exit;

    {if not LazarusIDE.BeginCodeTools then begin
      DebugLn(['TQuickFix_Hide failed because IDE busy']);
      exit;
    end;}

    // insert marks beginning with the highest line,column
    Code:=nil;
    Node:=Tree.FindHighest;
    while Node<>nil do begin
      Msg:=TMessageLine(Node.Data);
      Fix(Msg);
      Node:=Node.Precessor;
    end;
  finally
    Tree.Free;
  end;
end;

function TQuickFix_Hide.IsApplicable(Msg: TMessageLine): boolean;
begin
  Result:=false;
  if (Msg.Urgency>=mluError)
  or (Msg.SubTool<>SubToolFPC)
  or (not Msg.HasSourcePosition)
  or (mlfHiddenByIDEDirective in Msg.Flags)
  then exit;
  Result:=true;
end;

procedure TQuickFix_Hide.CreateMenuItems(Fixes: TMsgQuickFixes);
var
  Msg: TMessageLine;
  i: Integer;
  List: TFPList;
  aCaption: String;
  aFilename: String;
  MultiFile: Boolean;
begin
  List:=TFPList.Create;
  try
    MultiFile:=false;
    aFilename:='';
    for i:=0 to Fixes.LineCount-1 do begin
      Msg:=Fixes.Lines[i];
      if not IsApplicable(Msg) then continue;
      if aFilename='' then
        aFilename:=Msg.GetFullFilename
      else if CompareFilenames(aFilename,Msg.GetFullFilename)<>0 then
        MultiFile:=true;
      List.Add(Msg);
    end;
    if List.Count=0 then exit;
    if List.Count>1 then
      Fixes.AddMenuItem(Self,nil,'Hide all hints and warnings by inserting IDE directives {%H-}');

    for i:=0 to List.Count-1 do begin
      Msg:=TMessageLine(List[i]);
      if MultiFile then
        aCaption:=Msg.GetShortFilename
      else
        aCaption:='';
      if List.Count>1 then
        aCaption+='('+IntToStr(Msg.Line)+','+IntToStr(Msg.Column)+')';
      if aCaption<>'' then
        aCaption:='Hide message at '+aCaption+' by inserting IDE directive {%H-}'
      else
        aCaption:='Hide message by inserting IDE directive {%H-}';
      Fixes.AddMenuItem(Self,Msg,aCaption);
    end;
  finally
    List.Free;
  end;
end;

{ TIDEQuickFixes }

procedure TIDEQuickFixes.MenuItemClick(Sender: TObject);
var
  i: Integer;
  Info: TMenuItemInfo;
  ListsMsgLines: TFPList;
  MsgLines: TMessageLines;
begin
  Info:=TMenuItemInfo(fMenuItemToInfo[Sender]);
  if Info=nil then exit;
  try
    Info.Fix.QuickFix(Self,Info.Msg);
  finally
    ListsMsgLines:=TFPList.Create;
    try
      for i:=0 to LineCount-1 do begin
        MsgLines:=Lines[i].Lines;
        if ListsMsgLines.IndexOf(MsgLines)>=0 then continue;
        ListsMsgLines.Add(MsgLines);
      end;
      for i:=0 to ListsMsgLines.Count-1 do
        TMessageLines(ListsMsgLines[i]).ApplyFixedMarks;
    finally
      ListsMsgLines.Free;
    end;
  end;
end;

constructor TIDEQuickFixes.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  IDEQuickFixes:=Self;
  MsgQuickFixes:=Self;
  fMenuItemToInfo:=TPointerToPointerTree.Create;

  // init standard quickfixes
  IDEQuickFixes.RegisterQuickFix(TQuickFix_Hide.Create);
end;

destructor TIDEQuickFixes.Destroy;
begin
  FreeAndNil(fMenuItemToInfo);
  MsgQuickFixes:=nil;
  IDEQuickFixes:=nil;
  inherited Destroy;
end;

procedure TIDEQuickFixes.OnPopupMenu(aParentMenuItem: TMenuItem);
var
  i: Integer;
begin
  ParentMenuItem:=aParentMenuItem;
  try
    if LineCount=0 then exit;
    for i:=0 to Count-1 do
      Items[i].CreateMenuItems(Self);
  finally
    ParentMenuItem:=nil;
  end;
end;

procedure TIDEQuickFixes.SetMsgLines(aMsg: TMessageLine);
begin
  ClearLines;
  if aMsg<>nil then
    fMsg.Add(aMsg);
end;

procedure TIDEQuickFixes.AddMsgLine(aMsg: TMessageLine);
begin
  if (aMsg<>nil) and (fMsg.IndexOf(aMsg)<0) then
    fMsg.Add(aMsg);
end;

procedure TIDEQuickFixes.ClearLines;
var
  i: Integer;
begin
  fMenuItemToInfo.Clear;
  for i:=ComponentCount-1 downto 0 do
    if Components[i] is TMenuItem then
      Components[i].Free;
  fMsg.Clear;
end;

function TIDEQuickFixes.AddMenuItem(Fix: TMsgQuickFix; Msg: TMessageLine;
  aCaption: string; aTag: PtrInt): TMenuItem;
var
  Info: TMenuItemInfo;
begin
  if (Fix=nil) then
    raise Exception.Create('missing Fix');
  if (aCaption='') then
    raise Exception.Create('missing Caption');
  if (ParentMenuItem.Count>50) then exit(nil);
  Result:=TMenuItem.Create(Self);
  Info:=TMenuItemInfo.Create;
  Info.MenuItem:=Result;
  Info.Fix:=Fix;
  Info.Msg:=Msg;
  fMenuItemToInfo[Result]:=Info;
  Result.Caption:=aCaption;
  Result.Tag:=aTag;
  Result.OnClick:=@MenuItemClick;
  ParentMenuItem.Add(Result);
end;

function TIDEQuickFixes.OpenMsg(Msg: TMessageLine): boolean;
var
  i: Integer;
begin
  Result:=false;
  if Msg=nil then exit;
  for i:=0 to Count-1 do begin
    Items[i].JumpTo(Msg,Result);
    if Result then exit;
  end;
end;

end.

