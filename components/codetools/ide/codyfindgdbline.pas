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
    An IDE dialog to paste a gdb backtrace from clipboard and find the
    corresponding lines.
}
unit CodyFindGDBLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazLoggerBase, LazLogger, SynEdit, IDEDialogs,
  SrcEditorIntf, LazIDEIntf, ProjectIntf, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ButtonPanel, CodyStrConsts, CodeCache, CodeToolManager,
  CodeTree, KeywordFuncLists, PascalParserTool, LinkScanner;

type

  { TCodyFindGDBLineDialog }

  TCodyFindGDBLineDialog = class(TForm)
    BacktraceMemo: TMemo;
    ButtonPanel1: TButtonPanel;
    FoundLabel: TLabel;
    GDBBacktraceLabel: TLabel;
    procedure BacktraceMemoChange(Sender: TObject);
    procedure BacktraceMemoKeyDown(Sender: TObject; var {%H-}Key: Word;
      {%H-}Shift: TShiftState);
    procedure BacktraceMemoKeyPress(Sender: TObject; var {%H-}Key: char);
    procedure BacktraceMemoMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BacktraceMemoMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ButtonPanel1OKButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
  private
    FErrorMsg: string;
    FIdleConnected: boolean;
    fLastBacktrace: string;
    fLastBacktraceSelStart: integer;
    fLastBacktraceCaret: TPoint;
    FSrcFilename: string;
    FSrcXY: TPoint;
    procedure SetIdleConnected(AValue: boolean);
    procedure Search(Immediately: boolean);
    procedure Jump;
    procedure ParseGDBBacktraceLine(Line: string; out Identifier, TheErrorMsg: string);
    procedure FindGDBIdentifier(GDBIdentifier: string; out TheErrorMsg: string);
    function FindUnit(TheUnitName: string; out aFilename: string): boolean;
    function FindProgram(TheSrcName: string; out aFilename: string): boolean;
  public
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property ErrorMsg: string read FErrorMsg;
    property SrcFilename: string read FSrcFilename;
    property SrcXY: TPoint read FSrcXY;
  end;

procedure ShowFindGDBLineDialog(Sender: TObject);

implementation

procedure ShowFindGDBLineDialog(Sender: TObject);
var
  CodyFindGDBLineDialog: TCodyFindGDBLineDialog;
begin
  CodyFindGDBLineDialog:=TCodyFindGDBLineDialog.Create(nil);
  try
    if CodyFindGDBLineDialog.ShowModal<>mrOk then exit;
    LazarusIDE.DoOpenFileAndJumpToPos(CodyFindGDBLineDialog.SrcFilename,CodyFindGDBLineDialog.SrcXY,-1,-1,-1,[]);
  finally
    CodyFindGDBLineDialog.Free;
  end;
end;

{$R *.lfm}

{ TCodyFindGDBLineDialog }

procedure TCodyFindGDBLineDialog.FormCreate(Sender: TObject);
begin
  Caption:=crsFindSourceOfGDBBacktrace;
  GDBBacktraceLabel.Caption:=crsPasteLinesOfAGdbBacktrace;
  ButtonPanel1.OKButton.Caption:=crsJump;
  ButtonPanel1.OKButton.OnClick:=@ButtonPanel1OKButtonClick;
  BacktraceMemo.Clear;
  Search(false);
end;

procedure TCodyFindGDBLineDialog.OnIdle(Sender: TObject; var Done: Boolean);
begin
  IdleConnected:=false;
  Search(true);
end;

procedure TCodyFindGDBLineDialog.SetIdleConnected(AValue: boolean);
begin
  if csDestroying in ComponentState then
    AValue:=false;
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TCodyFindGDBLineDialog.Search(Immediately: boolean);
var
  y: LongInt;
  s: String;
  Line: String;
  GDBIdentifier: string;
  Code: TCodeBuffer;
  SelStart: Integer;
begin
  if not Immediately then begin
    // update on idle
    IdleConnected:=true;
    exit;
  end;

  // check if something changed
  s:=BacktraceMemo.Lines.Text;
  SelStart:=BacktraceMemo.SelStart;
  if (s=fLastBacktrace)
  and (fLastBacktraceSelStart=BacktraceMemo.SelStart) then
    exit;
  fLastBacktrace:=s;
  fLastBacktraceSelStart:=SelStart;
  Code:=TCodeBuffer.Create;
  try
    Code.Source:=s;
    Code.AbsoluteToLineCol(SelStart+1,fLastBacktraceCaret.Y,fLastBacktraceCaret.X);
    FErrorMsg:='No backtrace.';
    FSrcFilename:='';
    FSrcXY:=Point(0,0);

    // get current line
    y:=fLastBacktraceCaret.Y;
    if (y>0) and (y<=Code.LineCount) then begin
      Line:=Code.GetLine(y-1);
      //debugln(['TCodyFindGDBLineDialog.Search Line="',Line,'"']);
      ParseGDBBacktraceLine(Line,GDBIdentifier,fErrorMsg);
      if FErrorMsg='' then begin
        // find gdb identifier
        FindGDBIdentifier(GDBIdentifier,FErrorMsg);
      end;
    end else begin
      // caret outside
      FErrorMsg:='Please move caret to a line with a backtrace.';
    end;
  finally
    Code.Free;
  end;

  // show found source position
  if ErrorMsg<>'' then
    s:='Error: '+ErrorMsg
  else begin
    s:='';
    if FSrcFilename<>'' then begin
      s:=FSrcFilename;
      if (FSrcXY.Y>0) then
        s+=' ('+dbgs(FSrcXY.Y)+','+dbgs(FSrcXY.X)+')';
    end else begin
      s:='not found';
    end;
  end;
  FoundLabel.Caption:=s;
end;

procedure TCodyFindGDBLineDialog.Jump;
begin
  Search(true);
  if ErrorMsg<>'' then begin
    IDEMessageDialog('Error',ErrorMsg,mtError,[mbCancel]);
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TCodyFindGDBLineDialog.ParseGDBBacktraceLine(Line: string; out
  Identifier, TheErrorMsg: string);
{ For example:
  #0  0x00020e16 in fpc_raiseexception ()
  #1  0x0004cb37 in SYSUTILS_RUNERRORTOEXCEPT$LONGINT$POINTER$POINTER ()
  #2  0x00024e48 in SYSTEM_HANDLEERRORADDRFRAME$LONGINT$POINTER$POINTER ()
  #3  0xbffff548 in ?? ()
  #4  0x007489de in EXTTOOLEDITDLG_TEXTERNALTOOLMENUITEMS_$__LOAD$TCONFIGSTORAGE$$TMODALRESULT ()
  #5  0x00748c44 in EXTTOOLEDITDLG_TEXTERNALTOOLMENUITEMS_$__LOAD$TCONFIGSTORAGE$ANSISTRING$$TMODALRESULT ()
  #6  0x007169a8 in ENVIRONMENTOPTS_TENVIRONMENTOPTIONS_$__LOAD$BOOLEAN ()
  #7  0x0007e620 in MAIN_TMAINIDE_$__LOADGLOBALOPTIONS ()
  #8  0x0007feb1 in MAIN_TMAINIDE_$__CREATE$TCOMPONENT$$TMAINIDE ()
  #9  0x00011124 in PASCALMAIN ()
  #10 0x0002f416 in SYSTEM_FPC_SYSTEMMAIN$LONGINT$PPCHAR$PPCHAR ()
  #11 0x00010eaa in _start ()
  #12 0x00010dd8 in start ()

  ~"#0 DOHANDLEMOUSEACTION (this=0x14afae00, ANACTIONLIST=0x14a96af8,
  ANINFO=...) at synedit.pp:3000\n"
  ~"#1 0x00aea3e9 in FINDANDHANDLEMOUSEACTION (this=0x14afae00,
  ABUTTON=MBLEFT, ASHIFT=..., X=233, Y=241, ACCOUNT=CCSINGLE, ADIR=CDDOWN,
  ANAC
  TIONRESULT=..., AWHEELDELTA=0) at synedit.pp:3307\n"
  ~"#2 0x00aea914 in MOUSEDOWN (this=0x14afae00, BUTTON=MBLEFT,
  SHIFT=..., X=233, Y=241) at synedit.pp:3374\n"
  ~"#3 0x005e083b in DOMOUSEDOWN (this=0x14afae00, MESSAGE=...,
  BUTTON=MBLEFT, SHIFT=...) at include/control.inc:2135\n"
  ~"#4 0x005e0e8f in WMLBUTTONDOWN (this=0x14afae00, MESSAGE=...) at
  include/control.inc:2269\n"
  ~"#5 0x0040d096 in DISPATCH (this=0xeebf6d4, MESSAGE=0) at
  ../inc/objpas.inc:592\n"
  ~"#6 0x005e06e3 in WNDPROC (this=0x14afae00, THEMESSAGE=...) at
  include/control.inc:2099\n"
  ~"#7 0x005d1b88 in WNDPROC (this=0x14afae00, MESSAGE=...) at
  include/wincontrol.inc:5327\n"
  ~"#8 0x00af3b76 in WNDPROC (this=0x14afae00, MSG=...) at synedit.pp:5740\n"
  ~"#9 0x006666a0 in DELIVERMESSAGE (TARGET=0x14afae00, AMESSAGE=0) at
  lclmessageglue.pas:112\n"
  ~"#10 0x0057ad0e in WINDOWPROC (WINDOW=3934144, MSG=513, WPARAM=1,
  LPARAM=15794409) at win32/win32callback.inc:2478\n"
  ~"#11 0x7673fd72 in ?? () from C:\\Windows\\system32\\user32.dll\n"
  ~"#12 0x7673fe4a in ?? () from C:\\Windows\\system32\\user32.dll\n"
  ~"#13 0x7674018d in ?? () from C:\\Windows\\system32\\user32.dll\n"
  ~"#14 0x7674022b in ?? () from C:\\Windows\\system32\\user32.dll\n"
  ~"#15 0x0057e0b8 in APPPROCESSMESSAGES (this=0x183d58) at
  win32/win32object.inc:367\n"
  ~"#16 0x0043d9e1 in HANDLEMESSAGE (this=0x12bf68) at
  include/application.inc:1257\n"
  ~"#17 0x0043df56 in RUNLOOP (this=0x12bf68) at
  include/application.inc:1390\n"
  ~"#18 0x00490481 in APPRUN (this=0x183d58, ALOOP=...) at
  include/interfacebase.inc:54\n"
  ~"#19 0x0043defb in RUN (this=0x12bf68) at include/application.inc:1378\n"
  ~"#20 0x0040358f in main () at lazarus.pp:128\n"

}
var
  p: PChar;
  StartP: PChar;

  procedure ExpectedChar(Expected: string);
  begin
    TheErrorMsg:='Expected '+Expected+' but found '+DbgStr(p^)
      +' at column '+{%H-}IntToStr(PtrUInt(p-PChar(Line))+1);
  end;

  function CheckChar(c: char; Expected: string): boolean;
  begin
    if p^=c then begin
      inc(p);
      Result:=true;
    end else begin
      ExpectedChar(Expected);
      Result:=false;
    end;
  end;

  function CheckWhiteSpace: boolean;
  begin
    if not CheckChar(' ','space') then exit(false);
    while p^=' ' do inc(p);
    Result:=true;
  end;

begin
  //debugln(['TCodyFindGDBLineDialog.ParseGDBBacktraceLine Line="',Line,'"']);
  Identifier:='';
  if Line='' then begin
    TheErrorMsg:='Not a gdb backtrace';
    exit;
  end;
  p:=PChar(Line);

  // read stackframe (#12)
  // read #
  if not CheckChar('#','# (stackframe)') then exit;
  // read number
  if not (p^ in ['0'..'9']) then begin
    ExpectedChar('number');
    exit;
  end;
  while p^ in ['0'..'9'] do inc(p);
  // skip space
  if not CheckWhiteSpace then exit;

  // read address (hex number 0x007489de)
  if not (p^ in ['0'..'9']) then begin
    ExpectedChar('address as hex number');
    exit;
  end;
  inc(p);
  if not CheckChar('x','x (hex number)') then exit;
  while p^ in ['0'..'9','a'..'f','A'..'F'] do inc(p);
  // skip space
  if not CheckWhiteSpace then exit;

  // read 'in'
  if not CheckChar('i','in') then exit;
  if not CheckChar('n','n') then exit;
  // skip space
  if not CheckWhiteSpace then exit;

  // read identifier
  if not (p^ in ['a'..'z','A'..'Z','_','?']) then begin
    ExpectedChar('identifier');
    exit;
  end;
  StartP:=p;
  while p^ in ['a'..'z','A'..'Z','0'..'9','_','$','?'] do inc(p);
  Identifier:=copy(Line,StartP-PChar(Line)+1,p-StartP);
  debugln(['TCodyFindGDBLineDialog.ParseGDBBacktraceLine Identifier="',Identifier,'"']);

  // success
  TheErrorMsg:='';
end;

procedure TCodyFindGDBLineDialog.FindGDBIdentifier(GDBIdentifier: string; out
  TheErrorMsg: string);
{ Examples:
  compiler built-in
    fpc_raiseexception
    ??
    PASCALMAIN
    SYSTEM_FPC_SYSTEMMAIN$LONGINT$PPCHAR$PPCHAR

  unit:
    procedure
      SYSUTILS_RUNERRORTOEXCEPT$LONGINT$POINTER$POINTER
      SYSTEM_HANDLEERRORADDRFRAME$LONGINT$POINTER$POINTER
    method
      EXTTOOLEDITDLG_TEXTERNALTOOLMENUITEMS_$__LOAD$TCONFIGSTORAGE$$TMODALRESULT
      EXTTOOLEDITDLG_TEXTERNALTOOLMENUITEMS_$__LOAD$TCONFIGSTORAGE$ANSISTRING$$TMODALRESULT
      ENVIRONMENTOPTS_TENVIRONMENTOPTIONS_$__LOAD$BOOLEAN
      MAIN_TMAINIDE_$__LOADGLOBALOPTIONS
      MAIN_TMAINIDE_$__CREATE$TCOMPONENT$$TMAINIDE

  program:
    P$TESTPROJECT1_DOTEST
    P$TESTPROJECT1_DOTEST_SUBTEST
    P$TESTPROJECT1_DOTEST$CHAR_SUBTEST$LONGINT
    P$TESTSTACKTRACE1_TMAINCLASS_$_TSUBCLASS_$__RAISESOMETHING$ANSISTRING
}
var
  p: PChar;
  TheSrcName: string;
  Code: TCodeBuffer;
  CurIdentifier: string;
  Tool: TCodeTool;
  Node: TCodeTreeNode;
  CodeXY: TCodeXYPosition;
  SubNode: TCodeTreeNode;
  ClassNode: TCodeTreeNode;
  ProcNode: TCodeTreeNode;
  SectionNode: TCodeTreeNode;

  procedure ReadIdentifier(out Identifier: string);
  var
    StartP: PChar;
  begin
    StartP:=p;
    while p^ in ['A'..'Z','0'..'9'] do inc(p);
    Identifier:=copy(GDBIdentifier,StartP-PChar(GDBIdentifier)+1,p-StartP);
  end;

  procedure ReadParamList;
  begin
    if p^='$' then begin
      // parameter list => skip
      while (p^ in ['$','A'..'Z','0'..'9']) do inc(p);
    end;
  end;

begin
  if GDBIdentifier='' then begin
    TheErrorMsg:='missing identifier';
    exit;
  end;
  p:=PChar(GDBIdentifier);
  if p^ in ['a'..'z'] then begin
    // lower case unit name means compiler built in function
    TheErrorMsg:='compiler built in function "'+GDBIdentifier+'"';
    exit;
  end;
  TheSrcName:='';
  if p^ in ['A'..'Z'] then begin
    ReadIdentifier(TheSrcName);
    debugln(['TCodyFindGDBLineDialog.FindGDBIdentifier first identifier=',TheSrcName,' ...']);
    if (TheSrcName='P') and (p^='$') then begin
      // P$programname
      inc(p);
      if IsIdentStartChar[p^] then
        ReadIdentifier(TheSrcName);
      debugln(['TCodyFindGDBLineDialog.FindGDBIdentifier search source of program "',TheSrcName,'" ...']);
      FindProgram(TheSrcName,FSrcFilename);
      if (SrcFilename='') then begin
        TheErrorMsg:='can''t find program "'+TheSrcName+'"';
        exit;
      end;
    end else if p^='_' then begin
      // a unit name
      // => search unit
      FindUnit(TheSrcName,FSrcFilename);
      if (SrcFilename='') then begin
        TheErrorMsg:='can''t find unit '+TheSrcName;
        exit;
      end;
    end else if p^<>'_' then begin
      // only one uppercase identifier, e.g. PASCALMAIN
      TheErrorMsg:='compiler built in function "'+GDBIdentifier+'"';
      exit;
    end;
    // load unit source
    Code:=CodeToolBoss.LoadFile(SrcFilename,true,false);
    if Code=nil then begin
      TheErrorMsg:='unable to read file "'+SrcFilename+'"';
      exit;
    end;

    inc(p);
    if p^ in ['A'..'Z'] then begin
      ReadIdentifier(CurIdentifier);
      debugln(['TCodyFindGDBLineDialog.FindGDBIdentifier Identifier="',CurIdentifier,'"']);

      if not CodeToolBoss.Explore(Code,Tool,false,true) then begin
        debugln(['TCodyFindGDBLineDialog.FindGDBIdentifier parse error']);
        TheErrorMsg:=CodeToolBoss.ErrorMessage;
        exit;
      end;

      ReadParamList;

      Node:=nil;
      if Tool.GetSourceType=ctnUnit then begin
        // a unit => first search in interface, then in implementation
        SectionNode:=Tool.FindInterfaceNode;
        if SectionNode<>nil then begin
          Node:=Tool.FindSubDeclaration(CurIdentifier,SectionNode);
        end;
        if Node=nil then begin
          // search in implementation
          try
            Node:=Tool.FindDeclarationNodeInImplementation(CurIdentifier,true);
          except
            on E: Exception do begin
              CodeToolBoss.HandleException(E);
              debugln(['TCodyFindGDBLineDialog.FindGDBIdentifier FindDeclarationNodeInImplementation parse error in "',Code.Filename,'": ',E.Message]);
              TheErrorMsg:=CodeToolBoss.ErrorMessage;
              exit;
            end;
          end;
        end;
      end else begin
        // not a unit, e.g. a program
        SectionNode:=Tool.Tree.Root;
        if SectionNode<>nil then begin
          Node:=Tool.FindSubDeclaration(CurIdentifier,SectionNode);
        end;
      end;
      if Node=nil then begin
        // identifier not found => use only SrcFilename
        debugln(['TCodyFindGDBLineDialog.FindGDBIdentifier identifier "',CurIdentifier,'" not found in "',Code.Filename,'"']);
        TheErrorMsg:='identifier "'+CurIdentifier+'" not found in "'+Code.Filename+'"';
        exit;
      end;

      repeat
        if (p^='_') and (p[1]='$') and (p[2]='_') and (p[3]='_') then begin
          // sub identifier is method or member
          inc(p,4);
        end else if (p^='_') and (p[1] in ['A'..'Z']) then begin
          // sub identifier is proc
          inc(p);
        end else
          break;
        if not (p^ in ['A'..'Z']) then begin
          break;
        end;
        // _$__identifier => sub identifier
        ReadIdentifier(CurIdentifier);
        ReadParamList;
        // find sub identifier
        SubNode:=Tool.FindSubDeclaration(CurIdentifier,Node);
        if SubNode=nil then begin
          debugln(['TCodyFindGDBLineDialog.FindGDBIdentifier SubIdentifier="',CurIdentifier,'" not found']);
          break;
        end;
        debugln(['TCodyFindGDBLineDialog.FindGDBIdentifier SubIdentifier="',CurIdentifier,'" found']);
        Node:=SubNode;
      until false;

      if Node.Desc=ctnProcedure then begin
        // proc node => find body
        ClassNode:=Tool.FindClassOrInterfaceNode(Node);
        if ClassNode<>nil then begin
          try
            Tool.BuildTree(lsrInitializationStart);
          except
            on E: Exception do begin
              // ignore
            end;
          end;
          ProcNode:=Tool.FindCorrespondingProcNode(Node,[phpAddClassName]);
          if ProcNode<>nil then
            Node:=ProcNode;
        end;
      end;

      // (part of) identifier found
      Tool.CleanPosToCaret(Node.StartPos,CodeXY);
      fSrcFilename:=CodeXY.Code.Filename;
      FSrcXY.Y:=CodeXY.Y;
      FSrcXY.X:=CodeXY.X;
    end;
    // unknown operator => use only SrcFilename
    debugln(['TCodyFindGDBLineDialog.FindGDBIdentifier operator not yet supported: ',dbgstr(p^)]);
    exit;
  end else begin
    // example: ??
  end;

  TheErrorMsg:='unkown identifier "'+GDBIdentifier+'"';
end;

function TCodyFindGDBLineDialog.FindUnit(TheUnitName: string; out
  aFilename: string): boolean;
var
  i: Integer;
  SrcEdit: TSourceEditorInterface;
  InFilename: string;
begin
  // search in project and all its packages
  InFilename:='';
  aFilename:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(
                             '',TheUnitName,InFilename,true);
  if aFilename<>'' then
    exit(true);
  // search in source editor
  for i:=0 to SourceEditorManagerIntf.SourceEditorCount-1 do begin
    SrcEdit:=SourceEditorManagerIntf.SourceEditors[i];
    aFilename:=SrcEdit.FileName;
    if not FilenameIsPascalUnit(aFileName) then continue;
    if CompareText(ExtractFileNameOnly(aFileName),TheUnitName)<>0 then
      continue;
    exit(true);
  end;
  // not found
  aFilename:='';
  Result:=false;
end;

function TCodyFindGDBLineDialog.FindProgram(TheSrcName: string; out
  aFilename: string): boolean;
var
  aProject: TLazProject;
  i: Integer;
  SrcEdit: TSourceEditorInterface;
begin
  // check active project
  aProject:=LazarusIDE.ActiveProject;
  if (aProject<>nil) and (aProject.MainFile<>nil) then begin
    aFilename:=aProject.MainFile.Filename;
    if FilenameIsAbsolute(aFilename)
    and ((TheSrcName='')
      or (SysUtils.CompareText(ExtractFileNameOnly(aFilename),TheSrcName)=0))
    then
      exit(true);
  end;
  // search in source editor
  for i:=0 to SourceEditorManagerIntf.SourceEditorCount-1 do begin
    SrcEdit:=SourceEditorManagerIntf.SourceEditors[i];
    aFilename:=SrcEdit.FileName;
    if CompareText(ExtractFileNameOnly(aFileName),TheSrcName)<>0 then
      continue;
    exit(true);
  end;
  // not found
  aFilename:='';
  Result:=false;
end;

procedure TCodyFindGDBLineDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IdleConnected:=false;
end;

procedure TCodyFindGDBLineDialog.BacktraceMemoChange(Sender: TObject);
begin
  Search(false);
end;

procedure TCodyFindGDBLineDialog.BacktraceMemoKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  Search(False);
end;

procedure TCodyFindGDBLineDialog.BacktraceMemoKeyPress(Sender: TObject;
  var Key: char);
begin
  Search(false);
end;

procedure TCodyFindGDBLineDialog.BacktraceMemoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssDouble in Shift then
    Jump;
end;

procedure TCodyFindGDBLineDialog.BacktraceMemoMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Search(false);
end;

procedure TCodyFindGDBLineDialog.ButtonPanel1OKButtonClick(Sender: TObject);
begin
  Jump;
end;

end.

