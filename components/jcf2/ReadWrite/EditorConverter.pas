unit EditorConverter;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is EditorConverter.pas, released January 2001.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2001 Anthony Steele.
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

{ AFS 12 Jan 2K
  Converter class for the IDE pluggin
}

{$I JcfGlobal.inc}

interface

uses
  Classes, SysUtils, Math,
  // IdeIntf
  SrcEditorIntf,
  { local }
  Converter, ConvertTypes;

type

  TEditorConverter = class(TObject)
  private
    { the string -> string converter }
    fcConverter: TConverter;
    { state }
    fOnStatusMessage: TStatusMessageProc;
    fsCurrentUnitName: string;
    fiConvertCount: integer;

    procedure SendStatusMessage(const psUnit, psMessage: string;
      const peMessageType: TStatusMessageType;
      const piY, piX: integer);

    function GetOnStatusMessage: TStatusMessageProc;
    procedure SetOnStatusMessage(const Value: TStatusMessageProc);

    function ReadFromIDE(const pcUnit: TSourceEditorInterface): string;
    procedure WriteToIDE(const pcUnit: TSourceEditorInterface; const psText: string);

    procedure FinalSummary;
    function OriginalFileName: string;

  protected

  public
    constructor Create;
    destructor Destroy; override;
    procedure Convert(const pciUnit: TSourceEditorInterface);
    procedure Clear;
    function ConvertError: Boolean;
    function TokenCount: integer;

    procedure BeforeConvert;
    procedure AfterConvert;

    property OnStatusMessage: TStatusMessageProc read GetOnStatusMessage write SetOnStatusMessage;
  end;


implementation

uses
  { local }
  JcfLog, JcfRegistrySettings, JcfMiscFunctions;

constructor TEditorConverter.Create;
begin
  inherited;
  
  fcConverter := TConverter.Create;
  fcConverter.OnStatusMessage := SendStatusMessage;
end;

destructor TEditorConverter.Destroy;
begin
  FreeAndNil(fcConverter);
  inherited;
end;

procedure TEditorConverter.Convert(const pciUnit: TSourceEditorInterface);
begin
  Assert(pciUnit <> nil);

  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  { check for read-only  }
  if pciUnit.ReadOnly then
  begin
    SendStatusMessage(pciUnit.FileName, 'Unit is read only. Cannot format ',
      mtInputError, -1, -1);
    exit;
  end;

  fsCurrentUnitName := pciUnit.FileName;
  fcConverter.InputCode := ReadFromIDE(pciUnit);

  // now convert
  fcConverter.Convert;
  fsCurrentUnitName := '';
  if not ConvertError then
  begin
    WriteToIDE(pciUnit, fcConverter.OutputCode);
    SendStatusMessage(pciUnit.FileName, 'Formatted unit', mtProgress, -1, -1);
    Inc(fiConvertCount);
  end;
end;

function TEditorConverter.ReadFromIDE(const pcUnit: TSourceEditorInterface): string;
begin
  Result := pcUnit.Lines.Text;
end;

procedure TEditorConverter.WriteToIDE(const pcUnit: TSourceEditorInterface; const psText: string);
var
  lLogicalCaretXY:TPoint;
  lStart,lEnd:TPoint;
begin
  if pcUnit = nil then
    exit;
  if psText <> fcConverter.InputCode then
  begin
    try
      lLogicalCaretXY:=pcUnit.CursorTextXY;
      pcUnit.BeginUpdate;
      pcUnit.BeginUndoBlock;
      lStart.X:=0;  //select all text.
      lStart.Y:=0;
      lEnd.X:=0;
      if pcUnit.LineCount>0 then
        lEnd.X:=length(pcUnit.Lines[pcUnit.LineCount-1])+1;
      lEnd.Y:=pcUnit.LineCount;
      //pcUnit.Lines.Text := psText;    // removes undo history.
      pcUnit.ReplaceText(lStart,lEnd,psText);
      pcUnit.CursorTextXY:=lLogicalCaretXY;
      pcUnit.Modified := True;
    finally
      pcUnit.EndUndoBlock;  
      pcUnit.EndUpdate;
    end;
  end;
end;

//BUGGY: inserts empty blank lines in "random" position in the editor.
// and if only one line es added or deleted after formatting then doesn't syncronize well.
// i think is better change all text in the editor.
// TODO: delete
{
procedure TEditorConverter.WriteToIDE(const pcUnit: TSourceEditorInterface; const psText: string);
var
  lcSourceLines, lcDestLines: TStrings;
  lcSameStart, lcSameEnd: TStrings;
  lsSourceLine, lsDestLine: string;
  liStart, liIndex, liMaxIndex: integer;
  hasSourceLine: Boolean;
begin
  if pcUnit = nil then
    exit;
  lcSourceLines := TStringList.Create;
  lcSourceLines.Text := fcConverter.InputCode;
  lcDestLines := TStringList.Create;
  lcDestLines.Text := psText;
  lcSameStart := TStringList.Create;
  lcSameEnd := TStringList.Create;

  SplitIntoChangeSections(lcSourceLines, lcDestLines, lcSameStart, lcSameEnd);
  try
    pcUnit.BeginUpdate;
    pcUnit.BeginUndoBlock;

    liStart := lcSameStart.Count;
    liIndex := 0;
    liMaxIndex := Max(lcSourceLines.Count, lcDestLines.Count);
    while (liIndex < liMaxIndex) do
    begin
      hasSourceLine := liIndex < lcSourceLines.Count;
      if hasSourceLine then
        lsSourceLine := lcSourceLines[liIndex]
      else
        lsSourceLine := '';

      if liIndex < lcDestLines.Count then
        lsDestLine := lcDestLines[liIndex]
      else
        lsDestLine := '';

      if not hasSourceLine then
        pcUnit.InsertLine(liStart + liIndex + 1, lsDestLine, True)
      else
      if not AnsiSameStr(lsSourceLine, lsDestLine) then
        // the line is different, replace it
        pcUnit.ReplaceLines(liStart + liIndex + 1, liStart + liIndex + 1, lsDestLine, True);

       inc(liIndex);
     end;
   finally
    pcUnit.EndUndoBlock;
    pcUnit.EndUpdate;
    lcSourceLines.Free;
    lcDestLines.Free;
    lcSameStart.Free;
    lcSameEnd.Free;
   end;
end;
}

procedure TEditorConverter.AfterConvert;
begin
  FinalSummary;
  Log.CloseLog;

  if GetRegSettings.ViewLogAfterRun then
    GetRegSettings.ViewLog;
end;

procedure TEditorConverter.Clear;
begin
  fcConverter.Clear;
end;

function TEditorConverter.ConvertError: Boolean;
begin
  Result := fcConverter.ConvertError;
end;

function TEditorConverter.GetOnStatusMessage: TStatusMessageProc;
begin
  Result := fOnStatusMessage;
end;

function TEditorConverter.OriginalFileName: string;
begin
  if fsCurrentUnitName <> '' then
    Result := fsCurrentUnitName
  else
    Result := 'IDE';
end;

procedure TEditorConverter.SendStatusMessage(const psUnit, psMessage: string;
  const peMessageType: TStatusMessageType;
  const piY, piX: integer);
var
  lsUnit: string;
begin
  lsUnit := psUnit;
  if lsUnit = '' then
    lsUnit := OriginalFileName;

  if Assigned(fOnStatusMessage) then
    fOnStatusMessage(lsUnit, psMessage, peMessageType, piY, piX);
end;

procedure TEditorConverter.SetOnStatusMessage(const Value: TStatusMessageProc);
begin
  fOnStatusMessage := Value;
end;

function TEditorConverter.TokenCount: integer;
begin
  Result := fcConverter.TokenCount;
end;

procedure TEditorConverter.FinalSummary;
var
  lsMessage: string;
begin
  if fiConvertCount = 0 then
  begin
    if ConvertError then
      lsMessage := 'Aborted due to error'
    else
      lsMessage := 'Nothing done';
  end
  {
  else if fbAbort then
    lsMessage := 'Aborted after ' + DescribeFileCount(fiConvertCount)
  }
  else if fiConvertCount > 1 then
    lsMessage := 'Finished processing ' + DescribeFileCount(fiConvertCount)
  else
    lsMessage := '';

  if lsMessage <> '' then
    SendStatusMessage('', lsMessage, mtFinalSummary, -1, -1);

  Log.EmptyLine;
  Log.Write(lsMessage);
end;

procedure TEditorConverter.BeforeConvert;
begin
  fiConvertCount := 0;
end;

end.
