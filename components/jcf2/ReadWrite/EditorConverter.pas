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
  Classes,
  {$ifdef fpc}
    { lazarus design time }
    SrcEditorIntf,
  {$else}
    { delphi design time }
    ToolsAPI,
  {$endif}
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

    {$ifdef fpc}
    function ReadFromIDE(const pcUnit: TSourceEditorInterface): string;
    procedure WriteToIDE(const pcUnit: TSourceEditorInterface; const psText: string);
    {$else}
    function ReadFromIDE(const pcUnit: IOTASourceEditor): string;
    procedure WriteToIDE(const pcUnit: IOTASourceEditor; const psText: string);
    {$endif}

    procedure FinalSummary;
    function OriginalFileName: string;

  protected

  public
    constructor Create;
    destructor Destroy; override;

    {$ifdef fpc}
    procedure Convert(const pciUnit: TSourceEditorInterface);
    {$else}
    procedure Convert(const pciUnit: IOTASourceEditor);
    {$endif}

    procedure Clear;

    function ConvertError: Boolean;
    function TokenCount: integer;

    procedure BeforeConvert;
    procedure AfterConvert;

    property OnStatusMessage: TStatusMessageProc read GetOnStatusMessage write SetOnStatusMessage;
  end;


implementation

uses
  { delphi }
  SysUtils, Math,
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

{$ifdef fpc}

procedure TEditorConverter.Convert(const pciUnit: TSourceEditorInterface);
begin
  Assert(pciUnit <> nil);

  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  { check for read-only  }
  if pciUnit <> nil then
  begin
    if pciUnit.ReadOnly then
    begin
      SendStatusMessage(pciUnit.FileName, 'Unit is read only. Cannot format ',
        mtInputError, -1, -1);
      exit;
    end;
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
begin
  if pcUnit = nil then
    exit;
  pcUnit.ReplaceLines(0, pcUnit.LineCount, psText);
end;

{$else}

procedure TEditorConverter.Convert(const pciUnit: IOTASourceEditor);
var
  lcBuffer: IOTAEditBuffer;
begin
  Assert(pciUnit <> nil);

  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  { check for read-only  }
  pciUnit.QueryInterface(IOTAEditBuffer, lcBuffer);
  if pciUnit <> nil then
  begin
    lcBuffer := pciUnit as IOTAEditBuffer;
    if lcBuffer.IsReadOnly then
    begin
      SendStatusMessage(lcBuffer.FileName, 'Unit is read only. Cannot format ',
        mtInputError, -1, -1);
      exit;
    end;
  end;

  fsCurrentUnitName := lcBuffer.FileName;
  fcConverter.InputCode := ReadFromIDE(pciUnit);

  // now convert
  fcConverter.Convert;

  fsCurrentUnitName := '';

  if not ConvertError then
  begin
    WriteToIDE(pciUnit, fcConverter.OutputCode);
    SendStatusMessage(lcBuffer.FileName, 'Formatted unit', mtProgress, -1, -1);
    Inc(fiConvertCount);
  end;
end;

function TEditorConverter.ReadFromIDE(const pcUnit: IOTASourceEditor): string;
const
  // 10 kb at a time should do it
  BUF_SIZE = 10240;
 //BUF_SIZE = 120; // small for testing
var
  lciEditorReader: IOTAEditReader;
  lsBuf:  AnsiString;
  lpBuf:  PAnsiChar;
  liActualSize, liPos: integer;
  lbDone: boolean;
  //liLoopCount: integer;
begin
  { get a reader from the unit }
  Assert(pcUnit <> nil);
  lciEditorReader := pcUnit.CreateReader;
  Assert(lciEditorReader <> nil);

  Result := '';

  // read it all. Unfortunately the API dictates that we will work in chunks

  liPos := 0;
  //liLoopCount := 0;

  lbDone := False;

  while not lbDone do
  begin
    // clear the buffer
    SetLength(lsBuf, BUF_SIZE);
    lpBuf := PAnsiChar(lsBuf);
    FillChar(lpBuf^, BUF_SIZE, 0);

    // get some text into the buffer
    liActualSize := lciEditorReader.GetText(liPos, lpBuf, BUF_SIZE);

    // store it
    {WP: Do not add the entire lsBuf to fsSource, as in cases where the entire source is less
     than 10Kb in total, there will be junk in the last part of the buffer.
     If this is copied, it shows up as extraneous tokens in the token list
     after the end of the unit proper.
     This then causes an assertion failure in procedure DoConvertUnit in unit Converter.pas,
     When these extra tokens are found that were not consumed by BuildParseTree

     The way is to ensure that you only append as many characters as you've actually read (liActualSize bytes)
     from the buffer into the result. }
    Result := Result + string(Copy(lsBuf, 1, liActualSize));
      //WP: Changed from just adding lsBuf

    // more stuff to read after this?
    liPos  := liPos + liActualSize;
    lbDone := (liActualSize < BUF_SIZE);
    //inc(liLoopCount);
  end;
end;



{ write the text back to the ide
  this is not as simple as you may think
  identical lines of text are skipped over not written
  ( not in all cases, but the simple cases are covered)
  so as to preserve the editor's notion of what has changed and what has not
}
procedure TEditorConverter.WriteToIDE(const pcUnit: IOTASourceEditor; const psText: string);
var
  lciEditorWriter: IOTAEditWriter;
  lsOriginalSource: string;
  liSourcePos{, liDestPos}: integer;
  lcSourceLines, lcDestLines: TStrings;
  lcSameStart, lcSameEnd: TStrings;
  lsSourceLine, lsDestLine: string;
  liIndex, liMaxIndex: integer;
begin
  if pcUnit = nil then
    exit;

  lciEditorWriter := pcUnit.CreateUndoableWriter;
  Assert(lciEditorWriter <> nil);

  liSourcePos := 0;

  lsOriginalSource := fcConverter.InputCode;
  lcSourceLines := SplitIntoLines(lsOriginalSource);
  lcDestLines := SplitIntoLines(psText);
  lcSameStart := TStringList.Create;
  lcSameEnd := TStringList.Create;

  SplitIntoChangeSections(lcSourceLines, lcDestLines, lcSameStart, lcSameEnd);
  try

    // clear off identical text at the start
    for liIndex := 0 to lcSameStart.Count - 1 do
    begin
      liSourcePos := liSourcePos + Length(lcSameStart[liIndex]);
    end;

    lciEditorWriter.CopyTo(liSourcePos);
    //liDestPos := liSourcePos;

   { loop through all lines in in and out
    if they're the same, copy the line
    else overwrite }
    liIndex := 0;
    liMaxIndex := Max(lcSourceLines.Count, lcDestLines.Count);

    while (liIndex < liMaxIndex) do
    begin
      if liIndex < lcSourceLines.Count then
        lsSourceLine := lcSourceLines[liIndex]
      else
        lsSourceLine := '';

      if liIndex < lcDestLines.Count then
        lsDestLine := lcDestLines[liIndex]
      else
        lsDestLine := '';

      liSourcePos := liSourcePos + Length(lsSourceLine);
      //liDestPos := liDestPos + Length(lsDestLine);

      if AnsiSameStr(lsSourceLine, lsDestLine) then
      begin
        // the line is the same, copy it
        lciEditorWriter.CopyTo(liSourcePos);
      end
      else
      begin
        // the line is different, replace it
        lciEditorWriter.DeleteTo(liSourcePos);
        if lsDestLine <> '' then
          lciEditorWriter.Insert(PAnsiChar(AnsiString(lsDestLine)));
      end;

       inc(liIndex);
     end;

    // clear off identical text at the end
    for liIndex := 0 to lcSameEnd.Count - 1 do
    begin
      liSourcePos := liSourcePos + Length(lcSameEnd[liIndex]);
    end;
    lciEditorWriter.CopyTo(liSourcePos);

   finally
    lcSourceLines.Free;
    lcDestLines.Free;
    lcSameStart.Free;
    lcSameEnd.Free;
   end;

end;

{$endif}

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
