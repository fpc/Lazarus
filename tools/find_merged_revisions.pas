#!/usr/bin/instantfpc
// Author: Mattias Gaertner 2016

{$mode objfpc}{$H+}

uses Classes, SysUtils;

procedure WriteUsage;
begin
  writeln('Reads svn logs from trunk and fixes. Writes commits found in both.');
  writeln;
  writeln('Usage:');
  writeln('  ./find_merged_revisions.pas <trunk.log> <fixes.log> <merged.txt>');
  writeln;
  writeln('Create trunk.log and fixes.log from svn with:');
  writeln('  svn log --limit=1000 /path/lazarus/trunk > trunk.log ');
  writeln('  svn log --limit=1000 /path/lazarus/fixes > fixes.log ');
  writeln;
  writeln('merged.txt are the already listed entries on');
  writeln('  http://wiki.lazarus.freepascal.org/Lazarus_1.6_fixes_branch like this:');
  writeln('    r51389 fpvectorial: Fix compilation error with fpc 2.6.4');
  Halt(1);
end;

type
  TLogEntry = class
  public
    Revision: string;
    Message: string;
  end;

function ReadLog(aFilename: string): TFPList; // list of TLogEntry
{ Entries have the format:
------------------------------------------------------------------------
r51652 ...
some text...
}
const
  Separator = '------------------------------';
var
  sl: TStringList;
  i, j: Integer;
  Line: String;
  LogEntry: TLogEntry;
begin
  Result:=TFPList.Create;
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(aFilename);
    i:=0;
    while i<sl.Count do begin
      Line:=sl[i];
      inc(i);
      if LeftStr(Line,length(Separator))=Separator then begin
        if i=sl.Count then break;
        Line:=sl[i];
        inc(i);
        if (length(Line)>=2) and (Line[1]='r') and (Line[2] in ['0'..'9']) then begin
          // read revsion, e.g.  r51234 |...
          j:=2;
          while (j<=length(Line)) and (Line[j] in ['0'..'9']) do inc(j);
          LogEntry:=TLogEntry.Create;
          Result.Add(LogEntry);
          LogEntry.Revision:=LeftStr(Line,j-1);
          LogEntry.Message:='';
          while i<sl.Count do begin
            Line:=sl[i];
            inc(i);
            if LeftStr(Line,length(Separator))=Separator then break;
            Line:=Trim(Line);
            if Line<>'' then begin
              if LogEntry.Message<>'' then
                LogEntry.Message+=' ';
              LogEntry.Message+=Line;
            end;
          end;
          LogEntry.Message:=Trim(LogEntry.Message);
          //writeln('GetLog Rev="',LogEntry.Revision,'" Msg="',LogEntry.Message,'"');
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

function ReadOldMerged(aFilename: string): TFPList;
{ One entry per line like this:
   r51379 Converter: Change global procedures into methods of TConvertSettings.
}
var
  sl: TStringList;
  i, j: Integer;
  Line: String;
  LogEntry: TLogEntry;
begin
  Result:=TFPList.Create;
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(aFilename);
    for i:=0 to sl.Count-1 do begin
      Line:=Trim(sl[i]);
      if (length(Line)>=2) and (Line[1]='r') and (Line[2] in ['0'..'9']) then begin
        j:=2;
        while (j<=length(Line)) and (Line[j] in ['0'..'9']) do inc(j);
        LogEntry:=TLogEntry.Create;
        Result.Add(LogEntry);
        LogEntry.Revision:=LeftStr(Line,j-1);
        LogEntry.Message:=Trim(copy(Line,j,length(Line)));
      end;
    end;
  finally
    sl.Free;
  end;
end;

function FindMsg(aMessage: string; Entries: TFPList): TLogEntry;
var
  i: Integer;
begin
  for i:=0 to Entries.Count-1 do begin
    Result:=TLogEntry(Entries[i]);
    if Result.Message=aMessage then exit;
  end;
  Result:=nil;
end;

function ExpandFile(aFilename: string): string;
begin
  if aFilename='' then
    WriteUsage;
  Result:=ExpandFilename(aFilename);
end;

var
  TrunkLog, FixesLog, OldMerged: TFPList;
  TrunkID: Integer;
  FixesEntry, TrunkEntry: TLogEntry;
begin
  if ParamCount<3 then WriteUsage;
  TrunkLog:=ReadLog(ExpandFile(ParamStr(1)));
  FixesLog:=ReadLog(ExpandFile(ParamStr(2)));
  OldMerged:=ReadOldMerged(ExpandFile(ParamStr(3)));

  TrunkID:=0;
  writeln('Merged revisions:');
  for TrunkID:=TrunkLog.Count-1 downto 0 do begin
    TrunkEntry:=TLogEntry(TrunkLog[TrunkID]);
    FixesEntry:=FindMsg(TrunkEntry.Message,FixesLog);
    if FixesEntry=nil then continue;
    if FindMsg(FixesEntry.Message,OldMerged)<>nil then continue;
    writeln(TrunkEntry.Revision,' (',FixesEntry.Revision,') ',TrunkEntry.Message);
  end;
end.
