{***************************************************************************
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

  Abstract:
    Frame to edit custom options and conditionals of compiler options
    (project+packages).
}
unit Compiler_Other_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, AVL_Tree, LazLogger, Forms, Controls, Graphics, strutils,
  Dialogs, StdCtrls, LCLProc, ComCtrls, LCLType, ExtCtrls, Buttons, contnrs,
  CodeToolsCfgScript, KeywordFuncLists, SynEdit, SynEditKeyCmds, SynCompletion,
  IDEOptionsIntf, CompOptsIntf, IDECommands, ListFilterEdit, Project,
  CompilerOptions, Compiler, EnvironmentOpts, LazarusIDEStrConsts,
  SourceSynEditor, EditorOptions, PackageDefs, EditBtn;

type

  TIdleAction = (iaScriptEngine, iaOptionsFilter);
  TIdleActions = set of TIdleAction;

  { TCompilerOtherOptionsFrame }

  TCompilerOtherOptionsFrame = class(TAbstractIDEOptionsEditor)
    btnGetAllOptions: TButton;
    edOptionsFilter: TEdit;
    edCustomOptions: TEdit;
    grpAllOptions: TGroupBox;
    lblCustomOptions: TLabel;
    grpConditionals: TGroupBox;
    CondStatusbar: TStatusBar;
    CondSynEdit: TSynEdit;
    CustomSplitter: TSplitter;
    btnResetOptionsFilter: TSpeedButton;
    lblStatus: TLabel;
    sbAllOptions: TScrollBox;
    procedure btnGetAllOptionsClick(Sender: TObject);
    procedure btnResetOptionsFilterClick(Sender: TObject);
    procedure CondSynEditChange(Sender: TObject);
    procedure CondSynEditKeyPress(Sender: TObject; var Key: char);
    procedure CondSynEditProcessUserCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure CondSynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure edOptionsFilterChange(Sender: TObject);
  private
    FCompOptions: TBaseCompilerOptions;
    FOptionsReader: TCompilerOptReader;
    FGeneratedControls: TComponentList;
    FEffectiveFilter: string;
    FIdleConnected: TIdleActions;
    FIsPackage: boolean;
    FCompletionHistory: TStrings;
    FCompletionValues: TStrings;
    FDefaultVariables: TCTCfgScriptVariables;
    FHighlighter: TIDESynFreePasSyn;
    FStatusMessage: string;
    fEngine: TIDECfgScriptEngine;
    fSynCompletion: TSynCompletion;
    procedure SetIdleConnected(AValue: TIdleActions);
    procedure SetStatusMessage(const AValue: string);
    procedure RenderAndFilterOptions;
    procedure StartCompletion;
    procedure UpdateCompletionValues;
    function GetCondCursorWord: string;
    procedure UpdateMessages;
    procedure UpdateStatusBar;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure OnSynCompletionCancel(Sender: TObject);
    procedure OnSynCompletionExecute(Sender: TObject);
    procedure OnSynCompletionKeyCompletePrefix(Sender: TObject);
    procedure OnSynCompletionKeyDelete(Sender: TObject);
    procedure OnSynCompletionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnSynCompletionKeyNextChar(Sender: TObject);
    procedure OnSynCompletionKeyPrevChar(Sender: TObject);
    procedure OnSynCompletionSearchPosition(var Position: integer);
    procedure OnSynCompletionUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure OnSynCompletionValidate(Sender: TObject; KeyChar: TUTF8Char;
      Shift: TShiftState);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Check: Boolean; override;
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    property StatusMessage: string read FStatusMessage write SetStatusMessage;
    property DefaultVariables: TCTCfgScriptVariables read FDefaultVariables;
    property CompletionValues: TStrings read FCompletionValues;
    property CompletionHistory: TStrings read FCompletionHistory;
    property IdleConnected: TIdleActions read FIdleConnected write SetIdleConnected;
    property CompOptions: TBaseCompilerOptions read FCompOptions;
  end;

implementation

{$R *.lfm}

{ TCompilerOtherOptionsFrame }

procedure TCompilerOtherOptionsFrame.RenderAndFilterOptions;
const
  LeftEdit = 120;
  LeftDescrEdit = 230;
  LeftDescrBoolean = 150;
var
  Opt: TCompilerOpt;
  yLoc: Integer;
  Container: TCustomControl;

  function MakeHeaderLabel: TControl;
  begin
    Result := TLabel.Create(Nil); // Container
    Result.Parent := Container;
    Result.Top := yLoc;
    Result.Left := Opt.Indentation*4;
    Result.Caption := Opt.Option+#9#9+Opt.Description;
    FGeneratedControls.Add(Result);
  end;

  function MakeOptionCntrl(aCntrlClass: TControlClass; aTopOffs: integer=0): TControl;
  begin
    Result := aCntrlClass.Create(Nil);
    Result.Parent := Container;
    Result.Top := yLoc+aTopOffs;
    Result.Left := Opt.Indentation*4;
    Result.Caption := Opt.Option;
    FGeneratedControls.Add(Result);
  end;

  function MakeEditCntrl(aLbl: TControl; aCntrlClass: TControlClass): TControl;
  // TEdit or TComboBox
  begin
    Result := aCntrlClass.Create(Nil);
    Result.Parent := Container;
    Result.AnchorSide[akTop].Control := aLbl;
    Result.AnchorSide[akTop].Side := asrCenter;
    Result.Left := LeftEdit;        // Now use Left instead of anchors
    Result.Anchors := [akLeft,akTop];
    FGeneratedControls.Add(Result);
  end;

  procedure MakeDescrLabel(aCntrl: TControl; aLeft: integer);
  // Description label after CheckBox / Edit control
  var
    Lbl: TControl;
  begin
    Lbl := TLabel.Create(Nil);
    Lbl.Parent := Container;
    Lbl.Caption := Opt.Description;
    Lbl.AnchorSide[akTop].Control := aCntrl;
    Lbl.AnchorSide[akTop].Side := asrCenter;
    Lbl.Left := aLeft;              // Now use Left instead of anchors
    Lbl.Anchors := [akLeft,akTop];
    FGeneratedControls.Add(Lbl);
  end;

  procedure AddChoices(aComboBox: TComboBox; aCategory: string);
  // Add selection choices to ComboBox from data originating from "fpc -i".
  var
    i: Integer;
  begin
    with FOptionsReader.SupportedCategories do
      if Find(aCategory, i) then
        aComboBox.Items.Assign(Objects[i] as TStrings)
      else
        raise Exception.CreateFmt('AddChoices: Selection list for "%s" is not found.',
                                  [aCategory]);
  end;

  procedure RenderOneLevel(aParentGroup: TCompilerOptGroup);
  var
    Cntrl, Lbl: TControl;
    cb: TComboBox;
    i, NewLeft: Integer;
  begin
    for i := 0 to aParentGroup.CompilerOpts.Count-1 do begin
      Opt := TCompilerOpt(aParentGroup.CompilerOpts[i]);
      if not Opt.Visible then Continue;         // Maybe filtered out
      case Opt.EditKind of
        oeNone: begin                           // Label
          Cntrl := MakeHeaderLabel;
        end;
        oeBoolean: begin                        // CheckBox
          Cntrl := MakeOptionCntrl(TCheckBox);
          if Length(Opt.Option) > 10 then
            NewLeft := LeftDescrBoolean + (Length(Opt.Option)-10)*8
          else
            NewLeft := LeftDescrBoolean;
          MakeDescrLabel(Cntrl, NewLeft);
        end;
        oeSetElem: begin                        // Sub-item for set, CheckBox
          Cntrl := MakeOptionCntrl(TCheckBox);
        end;
        oeNumber, oeText, oeSetNumber: begin    // Edit
          Lbl := MakeOptionCntrl(TLabel, 3);
          Cntrl := MakeEditCntrl(Lbl, TEdit);
          MakeDescrLabel(Cntrl, LeftDescrEdit);
        end;
        oeList: begin                           // ComboBox
          Lbl := MakeOptionCntrl(TLabel, 3);
          Cntrl := MakeEditCntrl(Lbl, TComboBox);
          cb := TComboBox(Cntrl);
          cb.Style := csDropDownList;
          case Opt.Option of
            '-Ca<x>':     AddChoices(cb, 'ABI targets:');
            '-Cf<x>':     AddChoices(cb, 'FPU instruction sets:');
            '-Cp<x>':     AddChoices(cb, 'CPU instruction sets:');
            '-Oo[NO]<x>': AddChoices(cb, 'Optimizations:');
            '-Op<x>':     AddChoices(cb, 'CPU instruction sets:');
            '-OW<x>':     AddChoices(cb, 'Whole Program Optimizations:');
            '-Ow<x>':     AddChoices(cb, 'Whole Program Optimizations:');
            else
              raise Exception.Create('AddChoices: Unknown option ' + Opt.Option);
          end;
          MakeDescrLabel(Cntrl, LeftDescrEdit);
        end
        else
          raise Exception.Create('TCompilerOptsRenderer.Render: Unknown EditKind.');
      end;
      Inc(yLoc, Cntrl.Height+2);
      if Opt is TCompilerOptGroup then
        RenderOneLevel(TCompilerOptGroup(Opt));  // Show other levels recursively
    end;
  end;

begin
  if FEffectiveFilter = edOptionsFilter.Text then Exit;
  Container := sbAllOptions;
  Container.DisableAutoSizing;
  try
    // First filter and set Visible flag.
    FOptionsReader.FilterOptions(edOptionsFilter.Text);
    // Then create and place new controls in GUI
    FGeneratedControls.Clear;
    yLoc := 0;
    RenderOneLevel(FOptionsReader.RootOptGroup);
    FEffectiveFilter:=edOptionsFilter.Text;
  finally
    Container.EnableAutoSizing;
    Container.Invalidate;
  end;
end;

procedure TCompilerOtherOptionsFrame.btnGetAllOptionsClick(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    lblStatus.Caption := 'Reading Options ...';
    Application.ProcessMessages;
    FOptionsReader.CompilerExecutable := EnvironmentOptions.CompilerFilename;
    if FOptionsReader.ReadAndParseOptions <> mrOK then
      ShowMessage(FOptionsReader.ErrorMsg);
    lblStatus.Caption := 'Rendering GUI ...';
    Application.ProcessMessages;
    sbAllOptions.Anchors := [];
    IdleConnected := IdleConnected + [iaOptionsFilter];
    btnGetAllOptions.Visible := False;
    lblStatus.Visible := False;
    edOptionsFilter.Enabled := True;
    sbAllOptions.Anchors := [akLeft,akTop, akRight, akBottom];
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TCompilerOtherOptionsFrame.btnResetOptionsFilterClick(Sender: TObject);
begin
  edOptionsFilter.Text := '';
  btnResetOptionsFilter.Enabled := False;
end;

procedure TCompilerOtherOptionsFrame.edOptionsFilterChange(Sender: TObject);
begin
  btnResetOptionsFilter.Enabled := edOptionsFilter.Text<>'';
  // ToDo : Filter the list of options
  IdleConnected := IdleConnected + [iaOptionsFilter];
end;

// Events dealing with conditionals SynEdit :

procedure TCompilerOtherOptionsFrame.CondSynEditChange(Sender: TObject);
begin
  UpdateStatusBar;
  IdleConnected := IdleConnected + [iaScriptEngine];
end;

procedure TCompilerOtherOptionsFrame.CondSynEditKeyPress(Sender: TObject; var Key: char);
begin
  //debugln(['TCompilerOtherOptionsFrame.CondSynEditKeyPress ',ord(Key)]);
end;

procedure TCompilerOtherOptionsFrame.CondSynEditProcessUserCommand(
  Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char;
  Data: pointer);
begin
  if (Command=ecWordCompletion) or (Command=ecIdentCompletion) then
    StartCompletion;
end;

procedure TCompilerOtherOptionsFrame.CondSynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if fSynCompletion.TheForm.Visible then
  begin
    //debugln(['TCompilerOtherOptionsFrame.CondSynEditStatusChange ']);
    fSynCompletion.CurrentString := GetCondCursorWord;
  end;
  UpdateStatusBar;
end;

procedure TCompilerOtherOptionsFrame.OnSynCompletionCancel(Sender: TObject);
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompilerOtherOptionsFrame.OnSynCompletionCancel ',fSynCompletion.TheForm.Visible]);
  {$ENDIF}
  if fSynCompletion.TheForm.Visible then
    fSynCompletion.Deactivate;
  fSynCompletion.RemoveEditor(CondSynEdit);
  //fSynCompletion.Editor:=nil;
end;

procedure TCompilerOtherOptionsFrame.OnSynCompletionExecute(Sender: TObject);
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompilerOtherOptionsFrame.OnSynCompletionExecute ']);
  {$ENDIF}
end;

procedure TCompilerOtherOptionsFrame.OnSynCompletionKeyCompletePrefix(
  Sender: TObject);
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompilerOtherOptionsFrame.OnSynCompletionKeyCompletePrefix ToDo']);
  {$ENDIF}
end;

procedure TCompilerOtherOptionsFrame.OnSynCompletionKeyDelete(Sender: TObject);
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompilerOtherOptionsFrame.OnSynCompletionKeyDelete']);
  {$ENDIF}
end;

procedure TCompilerOtherOptionsFrame.OnSynCompletionKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompilerOtherOptionsFrame.OnSynCompletionKeyDown ']);
  {$ENDIF}
  if Key=VK_BACK then
  begin
    Key:=VK_UNKNOWN;
    if fSynCompletion.CurrentString<>'' then
      CondSynEdit.CommandProcessor(ecDeleteLastChar,#0,nil);
  end;
end;

procedure TCompilerOtherOptionsFrame.OnSynCompletionKeyNextChar(Sender: TObject);
var
  XY: TPoint;
  StartX: integer;
  EndX: integer;
  Line: string;
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompilerOtherOptionsFrame.OnSynCompletionKeyNextChar ']);
  {$ENDIF}
  XY:=CondSynEdit.LogicalCaretXY;
  if XY.Y>CondSynEdit.Lines.Count then exit;
  CondSynEdit.GetWordBoundsAtRowCol(XY,StartX,EndX);
  if EndX<=XY.X then exit;
  Line := CondSynEdit.Lines[XY.Y - 1];
  inc(XY.X,UTF8CharacterLength(@Line[XY.X-1]));
  CondSynEdit.LogicalCaretXY:=XY;
end;

procedure TCompilerOtherOptionsFrame.OnSynCompletionKeyPrevChar(Sender: TObject);
var
  XY: TPoint;
  StartX: integer;
  EndX: integer;
  Line: string;
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompilerOtherOptionsFrame.OnSynCompletionKeyPrevChar ']);
  {$ENDIF}
  XY:=CondSynEdit.LogicalCaretXY;
  if XY.Y>CondSynEdit.Lines.Count then exit;
  CondSynEdit.GetWordBoundsAtRowCol(XY,StartX,EndX);
  if StartX>=XY.X then exit;
  Line := CondSynEdit.Lines[XY.Y - 1];
  XY.X:=UTF8FindNearestCharStart(PChar(Line),length(Line),XY.X-2)+1;
  CondSynEdit.LogicalCaretXY:=XY;
end;

procedure TCompilerOtherOptionsFrame.OnSynCompletionSearchPosition(var Position: integer);
var
  sl: TStringList;
  Prefix: String;
  s: string;
  i: Integer;
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompilerOtherOptionsFrame.OnSynCompletionSearchPosition "',fSynCompletion.CurrentString,'"']);
  {$ENDIF}
  Prefix:=fSynCompletion.CurrentString;
  sl:=TStringList.Create;
  try
    Position:=-1;
    for i:=0 to CompletionValues.Count-1 do
    begin
      s:=CompletionValues[i];
      if SysUtils.CompareText(Prefix,copy(s,1,length(Prefix)))<>0 then continue;
      if (Position<0) or (length(Prefix)=length(s)) then
        Position:=sl.Count;
      sl.AddObject(s,TObject({%H-}Pointer(i)));
    end;
    fSynCompletion.ItemList.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TCompilerOtherOptionsFrame.OnSynCompletionUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompilerOtherOptionsFrame.OnSynCompletionUTF8KeyPress ']);
  {$ENDIF}
end;

procedure TCompilerOtherOptionsFrame.OnSynCompletionValidate(Sender: TObject;
  KeyChar: TUTF8Char; Shift: TShiftState);
var
  i: LongInt;
  s: string;
  p: LongInt;
  TxtXY: TPoint;
  TxtStartX: integer;
  TxtEndX: integer;
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompilerOtherOptionsFrame.OnSynCompletionValidate ']);
  {$ENDIF}
  i:=fSynCompletion.Position;
  if (i>=0) and (i<fSynCompletion.ItemList.Count) then begin
    i:=PtrUInt(fSynCompletion.ItemList.Objects[i]);
    if (i>=0) and (i<CompletionValues.Count) then begin
      s:=CompletionValues[i];
      p:=System.Pos(#9,s);
      if p>0 then s:=copy(s,1,p-1);
      TxtXY:=CondSynEdit.LogicalCaretXY;
      CondSynEdit.GetWordBoundsAtRowCol(TxtXY,TxtStartX,TxtEndX);
      CondSynEdit.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TCompilerOtherOptionsFrame.OnSynCompletionValidate'){$ENDIF};
      try
        CondSynEdit.BlockBegin:=Point(TxtStartX,TxtXY.Y);
        CondSynEdit.BlockEnd:=Point(TxtEndX,TxtXY.Y);
        CondSynEdit.SelText:=s;
      finally
        CondSynEdit.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TCompilerOtherOptionsFrame.OnSynCompletionValidate'){$ENDIF};
      end;
      FCompletionHistory.Insert(0,s);
      if FCompletionHistory.Count>100 then
        FCompletionHistory.Delete(FCompletionHistory.Count-1);
    end;
  end;

  fSynCompletion.Deactivate;
end;

procedure TCompilerOtherOptionsFrame.SetStatusMessage(const AValue: string);
begin
  if FStatusMessage=AValue then exit;
  FStatusMessage:=AValue;
  CondStatusbar.Panels[2].Text := FStatusMessage;
end;

procedure TCompilerOtherOptionsFrame.SetIdleConnected(AValue: TIdleActions);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if FIdleConnected <> [] then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TCompilerOtherOptionsFrame.StartCompletion;

  function EditorRowColumnToCompletionXY(ScreenRowCol: TPoint;
    AboveRow: boolean): TPoint;
  begin
    if not AboveRow then
      inc(ScreenRowCol.Y,1);
    Result:=CondSynEdit.RowColumnToPixels(ScreenRowCol);
    Result:=CondSynEdit.ClientToScreen(Result);
    if fSynCompletion.TheForm.Parent<>nil then
      Result:=fSynCompletion.TheForm.Parent.ScreenToClient(Result);
  end;

var
  LogStartX: integer;
  LogEndX: integer;
  LogXY: TPoint;
  ScreenXY: TPoint;
  XY: TPoint;
  Line: String;
begin
  {$IFDEF VerboseCOCondSynCompletion}
  debugln(['TCompOptBuildMacrosFrame.StartCompletion START']);
  {$ENDIF}
  UpdateCompletionValues;
  fSynCompletion.ItemList.Assign(CompletionValues);

  // get row and column of word start at cursor
  LogXY:=CondSynEdit.LogicalCaretXY;
  CondSynEdit.GetWordBoundsAtRowCol(LogXY,LogStartX,LogEndX);
  LogEndX:=Min(LogEndX,LogXY.X);
  // convert text row,column to screen row,column
  ScreenXY:=CondSynEdit.PhysicalToLogicalPos(Point(LogStartX,LogXY.Y));
  // convert screen row,column to coordinates for the completion form
  XY:=EditorRowColumnToCompletionXY(ScreenXY,false);

  if XY.Y+fSynCompletion.TheForm.Height>fSynCompletion.TheForm.Parent.ClientHeight
  then begin
    // place completion above text
    XY:=EditorRowColumnToCompletionXY(ScreenXY,true);
    dec(XY.Y,fSynCompletion.TheForm.Height);
  end;

  // show completion box
  //fSynCompletion.AddEditor(CondSynEdit);
  fSynCompletion.Editor:=CondSynEdit;
  Line:=CondSynEdit.LineText;
  fSynCompletion.Execute(copy(Line,LogStartX,LogEndX-LogStartX),XY.X,XY.Y);
  debugln(['TCompilerOtherOptionsFrame.StartCompletion XY=',dbgs(XY),' fSynCompletion.TheForm.BoundsRect=',dbgs(fSynCompletion.TheForm.BoundsRect)]);
end;

procedure TCompilerOtherOptionsFrame.UpdateCompletionValues;

  function HasWord(const aName: string): Boolean;
  var
    i: Integer;
    s: string;
    p: LongInt;
  begin
    for i:=0 to CompletionValues.Count-1 do begin
      s:=CompletionValues[i];
      p:=System.Pos(#9,s);
      if p>0 then
        s:=copy(s,1,p-1);
      if SysUtils.CompareText(s,aName)=0 then exit(true);
    end;
    Result:=false;
  end;

  procedure AddKeyword(aName: string);
  begin
    CompletionValues.Add(aName);
  end;

  procedure AddWord(aName: string);
  begin
    aName:=dbgstr(aName);
    if aName='' then exit;
    if HasWord(aName) then exit;
    CompletionValues.Add(aName);
  end;

  procedure AddVar(aName, aValue: string);
  var
    s: String;
  begin
    aName:=dbgstr(aName);
    if aName='' then exit;
    if HasWord(aName) then exit;
    s:=dbgstr(aValue);
    if length(s)>50 then s:=copy(s,1,50)+'...';
    s:=aName+#9+aValue;
    CompletionValues.Add(s);
  end;

var
  Node: TAVLTreeNode;
  V: PCTCfgScriptVariable;
  s: String;
  p: PChar;
  AtomStart: PChar;
  pcov: TParsedCompilerOptString;
  pcouv: TParsedCompilerOptString;
  i: Integer;
  j: Integer;
  Macro: TLazBuildMacro;
begin
  CompletionValues.Clear;

  // add default variables with values
  Node:=DefaultVariables.Tree.FindLowest;
  while Node<>nil do begin
    V:=PCTCfgScriptVariable(Node.Data);
    AddVar(V^.Name,GetCTCSVariableAsString(V));
    Node:=DefaultVariables.Tree.FindSuccessor(Node);
  end;

  // add keywords and operands
  AddKeyword('if');
  AddKeyword('then');
  AddKeyword('else');
  AddKeyword('begin');
  AddKeyword('end');
  AddKeyword('not');
  AddKeyword('and');
  AddKeyword('or');
  AddKeyword('xor');
  AddKeyword('undefine');
  AddKeyword('defined');
  AddKeyword('undefined');
  AddKeyword('integer');
  AddKeyword('int64');
  AddKeyword('string');
  AddKeyword('true');
  AddKeyword('false');

  // add IDE functions
  AddWord('GetIDEValue(''OS'')');
  AddWord('GetIDEValue(''CPU'')');
  AddWord('GetIDEValue(''SrcOS'')');
  AddWord('GetIDEValue(''SrcOS2'')');
  AddWord('GetIDEValue(''LCLWidgetType'')');
  AddWord('GetEnv(''USER'')');
  AddWord('GetEnv(''HOME'')');

  // add result variables
  for pcov:=low(ParsedCompilerOptsVars) to high(ParsedCompilerOptsVars) do
    AddWord(ParsedCompilerOptsVars[pcov]);
  if FIsPackage then
    for pcouv:=low(ParsedCompilerOptsUsageVars) to high(ParsedCompilerOptsUsageVars) do
      AddWord(ParsedCompilerOptsUsageVars[pcouv]);

  // add build macros and values
  if CompOptions.BuildMacros<>nil then begin
    for i:=0 to CompOptions.BuildMacros.Count-1 do
    begin
      Macro:=CompOptions.BuildMacros[i];
      AddWord(Macro.Identifier);
      for j:=0 to Macro.Values.Count-1 do
        AddWord(Macro.Values[j]);
    end;
  end;

  // add words in text
  s:=CondSynEdit.Lines.Text;
  if s<>'' then begin
    p:=PChar(s);
    repeat
      AtomStart:=p;
      while (AtomStart^<>#0) and not IsIdentStartChar[AtomStart^] do
        inc(AtomStart);
      if (AtomStart^=#0) then break;
      p:=AtomStart;
      while IsIdentChar[p^] do inc(p);
      AddWord(copy(s,AtomStart-PChar(s)+1,p-AtomStart));
    until false;
  end;

  // sort alphabetically
  TStringList(FCompletionValues).Sort;

  // push recently used words upwards
  for i:=CompletionHistory.Count-1 downto 0 do begin
    j:=CompletionValues.IndexOf(CompletionHistory[i]);
    if j>0 then
      CompletionValues.Move(j,0);
  end;

  // set index
  for i:=0 to CompletionValues.Count-1 do
    CompletionValues.Objects[i]:=TObject({%H-}Pointer(i));

  //debugln(['TCompOptBuildMacrosFrame.UpdateCompletionValues ',CompletionValues.Text]);
end;

function TCompilerOtherOptionsFrame.GetCondCursorWord: string;
var
  XY: TPoint;
  StartX: integer;
  EndX: integer;
  Line: string;
begin
  XY := CondSynEdit.LogicalCaretXY;
  if (XY.Y>=1) and (XY.Y<=CondSynEdit.Lines.Count) then
  begin
    CondSynEdit.GetWordBoundsAtRowCol(XY,StartX,EndX);
    //debugln(['TCompOptBuildMacrosFrame.GetCondCursorWord ',StartX,' ',EndX,' ',XY.X]);
    EndX := Min(EndX,XY.X);
    Line := CondSynEdit.Lines[XY.Y - 1];
    Result := Copy(Line, StartX, EndX - StartX);
  end else
    Result := '';
  //debugln(['TCompOptBuildMacrosFrame.GetCondCursorWord "',Result,'"']);
end;

procedure TCompilerOtherOptionsFrame.UpdateMessages;
begin
  fEngine.Variables.Assign(DefaultVariables);
  fEngine.Execute(CondSynEdit.Lines.Text,1);
  if fEngine.ErrorCount>0 then begin
    StatusMessage:=fEngine.GetErrorStr(0);
  end else begin
    StatusMessage:=lisNoErrors;
  end;
end;

procedure TCompilerOtherOptionsFrame.UpdateStatusBar;
var
  PanelCharMode: String;
  PanelXY: String;
begin
  PanelXY := Format(' %6d:%4d',[CondSynEdit.CaretY,CondSynEdit.CaretX]);
  if CondSynEdit.InsertMode then
    PanelCharMode := uepIns
  else
    PanelCharMode := uepOvr;

  CondStatusbar.Panels[0].Text := PanelXY;
  CondStatusbar.Panels[1].Text := PanelCharMode;
end;

procedure TCompilerOtherOptionsFrame.OnIdle(Sender: TObject; var Done: Boolean);
var
  OldIdleCon: TIdleActions;
begin
  OldIdleCon := IdleConnected;
  IdleConnected := [];
  if iaScriptEngine in OldIdleCon then
    UpdateMessages;
  if iaOptionsFilter in OldIdleCon then
    RenderAndFilterOptions;
end;

constructor TCompilerOtherOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompletionValues:=TStringList.Create;
  FCompletionHistory:=TStringList.Create;
  fDefaultVariables:=TCTCfgScriptVariables.Create;
  fEngine:=TIDECfgScriptEngine.Create;
  FOptionsReader := TCompilerOptReader.Create;
  FGeneratedControls := TComponentList.Create;

  CondSynEdit.OnStatusChange:=@CondSynEditStatusChange;

  fSynCompletion:=TSynCompletion.Create(Self);
  fSynCompletion.ShowSizeDrag:=true;
  fSynCompletion.TheForm.Parent:=Self;
  fSynCompletion.OnExecute:=@OnSynCompletionExecute;
  fSynCompletion.OnCancel:=@OnSynCompletionCancel;
  fSynCompletion.OnValidate:=@OnSynCompletionValidate;
  fSynCompletion.OnSearchPosition:=@OnSynCompletionSearchPosition;
  fSynCompletion.OnKeyCompletePrefix:=@OnSynCompletionKeyCompletePrefix;
  fSynCompletion.OnUTF8KeyPress:=@OnSynCompletionUTF8KeyPress;
  fSynCompletion.OnKeyNextChar:=@OnSynCompletionKeyNextChar;
  fSynCompletion.OnKeyPrevChar:=@OnSynCompletionKeyPrevChar;
  fSynCompletion.OnKeyDelete:=@OnSynCompletionKeyDelete;
  fSynCompletion.OnKeyDown:=@OnSynCompletionKeyDown;
end;

destructor TCompilerOtherOptionsFrame.Destroy;
begin
  FGeneratedControls.Clear;
  FreeAndNil(FGeneratedControls);
  FreeAndNil(FOptionsReader);
  FreeAndNil(FCompletionHistory);
  FreeAndNil(FCompletionValues);
  FreeAndNil(fDefaultVariables);
  FreeAndNil(fEngine);
  inherited Destroy;
end;

function TCompilerOtherOptionsFrame.Check: Boolean;
begin
  Result := True;
end;

function TCompilerOtherOptionsFrame.GetTitle: string;
begin
  Result := dlgCOOther;
end;

procedure TCompilerOtherOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  grpAllOptions.Caption := lisAllOptions;
  lblCustomOptions.Caption := lisCustomOptions2;
  edCustomOptions.Hint := lisCustomOptHint;
  grpConditionals.Caption := lisConditionals;
  edOptionsFilter.Enabled := False;   // Until the options are read.
  edOptionsFilter.Hint := 'Filter the available options list';
  btnResetOptionsFilter.LoadGlyphFromLazarusResource(ResBtnListFilter);
  btnResetOptionsFilter.Enabled := False;
  btnResetOptionsFilter.Hint := 'Clear the filter for options';
  btnGetAllOptions.Caption := 'Get all options';
  btnGetAllOptions.Hint := 'Read available options using "fpc -i" and "fpc -h"';
  lblStatus.Caption := '';
  FEffectiveFilter:=#1; // Set an impossible value first, makes sure options are filtered.
end;

procedure TCompilerOtherOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Vars: TCTCfgScriptVariables;
begin
  FCompOptions := AOptions as TBaseCompilerOptions;
  FIsPackage := CompOptions is TPkgCompilerOptions;
  //debugln(['TCompilerOtherOptionsFrame.ReadSettings ',dbgs(Pointer(FCompOptions)),' ',FCompOptions=Project1.CompilerOptions]);

  edCustomOptions.Text := CompOptions.CustomOptions;

  Vars := GetBuildMacroValues(CompOptions,false);
  if Vars<>nil then
    DefaultVariables.Assign(Vars)
  else
    DefaultVariables.Clear;

  CondSynEdit.Lines.Text := CompOptions.Conditionals;
  if FHighlighter=nil then
  begin
    FHighlighter := TPreviewPasSyn.Create(Self);
    CondSynEdit.Highlighter:=FHighlighter;
  end;
  EditorOpts.ReadHighlighterSettings(FHighlighter, '');
  EditorOpts.GetSynEditSettings(CondSynEdit);
  UpdateStatusBar;
end;

procedure TCompilerOtherOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  CurOptions: TBaseCompilerOptions;
begin
  //debugln(['TCompilerOtherOptionsFrame.WriteSettings ',DbgSName(AOptions)]);
  CurOptions := AOptions as TBaseCompilerOptions;
  with CurOptions do
  begin
    CustomOptions := edCustomOptions.Text;
    Conditionals := CondSynEdit.Lines.Text;
  end;
end;

class function TCompilerOtherOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerOtherOptionsFrame,
    CompilerOptionsOther);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerOtherOptionsFrame,
    CompilerOptionsOther);

end.

