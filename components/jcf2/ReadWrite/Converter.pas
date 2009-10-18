{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is Converter.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

unit Converter;

{
  5 July 2004
  Rewrote as a simpler sting->string converter.
  For file or ide, there will be wrapper classes not subclasses.
  Wrappers will also support the interface IConvert
}

{$I JcfGlobal.inc}

interface

uses
  { delphi } SysUtils,
  { local } ConvertTypes, ParseTreeNode,
  BuildTokenList,
  BuildParseTree, BaseVisitor;

type

  TConverter = class(TObject)
  private
    { the strings for the in and out code }
    fsInputCode, fsOutputCode: WideString;
    fsFileName: String;

    { classes to lex and parse the source }
    fcTokeniser:      TBuildTokenList;
    fcBuildParseTree: TBuildParseTree;

    { used for testing - just run 1 process }
    fcSingleProcess: TTreeNodeVisitorType;

    { state }
    fiTokenCount:     Integer;
    fbConvertError:   Boolean;
    fOnStatusMessage: TStatusMessageProc;

    { false for commandline UI - don't put up a parse fail dialog
      This could be in  batch file on a server }
    fbGuiMessages: Boolean;

    function GetOnStatusMessage: TStatusMessageProc;
    procedure SetOnStatusMessage(const Value: TStatusMessageProc);

    procedure SendExceptionMessage(const pe: Exception);

    { call this to report the current state of the proceedings }
    procedure SendStatusMessage(const psUnit, psMessage: String; const peMessageType: TStatusMessageType; const piY, piX: Integer);

    function GetRoot: TParseTreeNode;

    { this does the reformatting. Virtual method so can be overriden for testing }
    procedure ApplyProcesses;
    procedure ApplySingleProcess;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Convert;
    procedure ConvertPart(const piStartIndex, piEndIndex: Integer);

    procedure ShowParseTree;
    procedure Clear;

    procedure CollectOutput(const pcRoot: TParseTreeNode);

    property InputCode: WideString Read fsInputCode Write fsInputCode;
    property OutputCode: WideString Read fsOutputCode Write fsOutputCode;
    property FileName: String Read fsFileName Write fsFileName;

    property TokenCount: Integer Read fiTokenCount;
    property ConvertError: Boolean Read fbConvertError;
    property GuiMessages: Boolean Read fbGuiMessages Write fbGuiMessages;

    property Root: TParseTreeNode Read GetRoot;

    property OnStatusMessage: TStatusMessageProc Read GetOnStatusMessage Write SetOnStatusMessage;
    property SingleProcess: TTreeNodeVisitorType Read fcSingleProcess Write fcSingleProcess;
  end;

implementation

uses
  { delphi }
  AllProcesses, Controls, Forms,
  { local }
  fShowParseTree, JcfRegistrySettings,
  JcfSettings, JcfStringUtils,
  JcfUnicode,
  ParseError, PreProcessorParseTree,
  SourceToken,
  SourceTokenList, TreeWalker, VisitSetNesting, VisitSetXY;

function StrInsert(const psSub, psMain: String; const piPos: Integer): String;
begin
  Result := StrLeft(psMain, piPos - 1) + psSub + StrRestOf(psMain, piPos);
end;


constructor TConverter.Create;
begin
  inherited;

  { owned objects }
  fcTokeniser      := TBuildTokenList.Create;
  fcTokeniser.FileName := FileName;
  fcBuildParseTree := TBuildParseTree.Create;
  fcSingleProcess  := nil;
  fbGuiMessages    := True; // use Ui to show parse errors by default
end;

destructor TConverter.Destroy;
begin
  FreeAndNil(fcTokeniser);
  FreeAndNil(fcBuildParseTree);

  inherited;
end;

procedure TConverter.Clear;
begin
  fsInputCode     := '';
  fsOutputCode    := '';
  fcSingleProcess := nil;
end;

procedure TConverter.Convert;
var
  lcTokenList: TSourceTokenList;
   {$IFNDEF COMMAND_LINE}
  leOldCursor: TCursor;
   {$ENDIF}
begin
  fbConvertError := False;

  {$IFNDEF COMMAND_LINE}
  leOldCursor := Screen.Cursor;
  try { finally normal cursor }
     
    // this can take a long time for large files
    Screen.Cursor := crHourGlass;
  {$ENDIF}

    // turn text into tokens
    fcTokeniser.SourceCode := InputCode;
    fcTokeniser.FileName   := FileName;

    lcTokenList := fcTokeniser.BuildTokenList;
    try   { finally free the list  }
      try { show exceptions }
        fiTokenCount := lcTokenList.Count;
        lcTokenList.SetXYPositions;

        // remove conditional compilation stuph
        if FormatSettings.PreProcessor.Enabled then
          RemoveConditionalCompilation(lcTokenList);

        // make a parse tree from it
        fcBuildParseTree.TokenList := lcTokenList;
        fcBuildParseTree.BuildParseTree;

      except
        on E: Exception do
        begin
          fbConvertError := True;
          SendExceptionMessage(E);

          if GuiMessages and (GetRegSettings.ShowParseTreeOption = eShowOnError) then
            ShowParseTree;
        end;
      end;

      if fbConvertError then
      begin
        { if there was a parse error, the rest of the unit was not parsed
         there may still be tokens in the list
         Free them or face a small but annoying memory leak. }
        lcTokenList.Clear;
      end;

      // should not be any tokens left
      Assert(lcTokenList.Count = 0, 'Surplus tokens');

    finally
      lcTokenList.Free;
    end;

    try

      if not fbConvertError then
      begin
        if (GetRegSettings.ShowParseTreeOption = eShowAlways) then
          ShowParseTree;

        // do the processes
        if Assigned(fcSingleProcess) then
          ApplySingleProcess
        else
          ApplyProcesses;

        // assemble the output string
        fsOutputCode := '';
        CollectOutput(fcBuildParseTree.Root);
      end;

      fcBuildParseTree.Clear;

    except
      on E: Exception do
      begin
        fbConvertError := True;
        SendExceptionMessage(E);
      end;
    end;

  {$IFNDEF COMMAND_LINE}
  finally
    Screen.Cursor := leOldCursor;
  end;
  {$ENDIF}
end;

{ this is what alters the code (in parse tree form) from source to output }
procedure TConverter.ApplyProcesses;
var
  lcProcess: TAllProcesses;
begin
  lcProcess := TAllProcesses.Create;
  try
    lcProcess.OnMessage := SendStatusMessage;

    lcProcess.Execute(fcBuildParseTree.Root);
  finally
    lcProcess.Free;
  end;
end;

procedure TConverter.ApplySingleProcess;
var
  lcProcess:    TBaseTreeNodeVisitor;
  lcTreeWalker: TTreeWalker;
begin
  lcTreeWalker := TTreeWalker.Create;
  try

    // apply a visit setXY first
    lcProcess := TVisitSetXY.Create;
    try
      lcTreeWalker.Visit(GetRoot, lcProcess);
    finally
      lcProcess.Free;
    end;

    // and set up nesting levels
    lcProcess := TVisitSetNestings.Create;
    try
      lcTreeWalker.Visit(GetRoot, lcProcess);
    finally
      lcProcess.Free;
    end;

    // then apply the process
    lcProcess := SingleProcess.Create;
    try
      lcTreeWalker.Visit(GetRoot, lcProcess);
    finally
      lcProcess.Free;
    end;

  finally
    lcTreeWalker.Free;
  end;
end;


function TConverter.GetRoot: TParseTreeNode;
begin
  Result := fcBuildParseTree.Root;
end;

procedure TConverter.CollectOutput(const pcRoot: TParseTreeNode);
var
  liLoop: Integer;
begin
  Assert(pcRoot <> nil);

  // is it a leaf with source?
  if (pcRoot is TSourceToken) then
  begin
    fsOutputCode := fsOutputCode + TSourceToken(pcRoot).SourceCode;
  end
  else
  begin
    // recurse, write out all child nodes
    for liLoop := 0 to pcRoot.ChildNodeCount - 1 do
    begin
      CollectOutput(pcRoot.ChildNodes[liLoop]);
    end;
  end;
end;

function TConverter.GetOnStatusMessage: TStatusMessageProc;
begin
  Result := fOnStatusMessage;
end;

procedure TConverter.SetOnStatusMessage(const Value: TStatusMessageProc);
begin
  fOnStatusMessage := Value;
end;

procedure TConverter.SendExceptionMessage(const pe: Exception);
var
  lsMessage:     String;
  liX, liY:      Integer;
  leParseError:  TEParseError;
  leMessageType: TStatusMessageType;
begin
  lsMessage := 'Exception ' + pe.ClassName +
    '  ' + pe.Message;

  if pe is TEParseError then
  begin
    leParseError := TEParseError(pe);
    lsMessage := lsMessage + NativeLineBreak + 'Near ' + leParseError.TokenMessage;
    liX := leParseError.XPosition;
    liY := leParseError.YPosition;
    leMessageType := mtParseError;
  end
  else
  begin
    liX := -1;
    liY := -1;
    leMessageType := mtException;
  end;

  SendStatusMessage('', lsMessage, leMessageType, liY, liX);
end;

procedure TConverter.SendStatusMessage(const psUnit, psMessage: String; const peMessageType: TStatusMessageType; const piY, piX: Integer);
begin
  if Assigned(fOnStatusMessage) then
    fOnStatusMessage(psUnit, psMessage, peMessageType, piY, piX);
end;

procedure TConverter.ShowParseTree;
begin
  if fcBuildParseTree.Root <> nil then
    fShowParseTree.ShowParseTree(fcBuildParseTree.Root);
end;

procedure TConverter.ConvertPart(const piStartIndex, piEndIndex: Integer);
const
  FORMAT_START = '{<JCF_!*$>}';
  FORMAT_END   = '{</JCF_!*$>}';
var
  liRealInputStart, liRealInputEnd: Integer;
  liOutputStart, liOutputEnd: Integer;
  lsNewOutput: WideString;
begin
  Assert(piStartIndex >= 0);
  Assert(piEndIndex >= piStartIndex);
  Assert(piEndIndex <= Length(InputCode));

  { round to nearest end of line }
  liRealInputStart := piStartIndex;
  liRealInputEnd   := piEndIndex;

  { get to the start of the line }
  while (liRealInputStart > 1) and (not WideCharIsReturn(InputCode[liRealInputStart - 1])) do
    Dec(liRealInputStart);

  { get to the start of the next line }
  while (liRealInputEnd < Length(InputCode)) and (not WideCharIsReturn(InputCode[liRealInputEnd])) do
    Inc(liRealInputEnd);
  while (liRealInputEnd < Length(InputCode)) and (WideCharIsReturn(InputCode[liRealInputEnd])) do
    Inc(liRealInputEnd);

  { put markers into the input }
  fsInputCode := StrInsert(FORMAT_END, fsInputCode, liRealInputEnd);
  fsInputCode := StrInsert(FORMAT_START, fsInputCode, liRealInputStart);

  Convert;

  { locate the markers in the output,
    and replace before and after }
  liOutputStart := Pos(WideString(FORMAT_START), fsOutputCode) + Length(FORMAT_START);
  liOutputEnd   := Pos(WideString(FORMAT_END), fsOutputCode);


  { splice }
  lsNewOutput := StrLeft(fsInputCode, liRealInputStart - 1);
  lsNewOutput := lsNewOutput + Copy(fsOutputCode, liOutputStart, (liOutputEnd - liOutputStart));
  lsNewOutput := lsNewOutput + StrRestOf(fsInputCode, liRealInputEnd + Length(FORMAT_START) + Length(FORMAT_END));

  fsOutputCode := lsNewOutput;
end;

end.
