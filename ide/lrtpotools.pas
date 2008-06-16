{ Copyright (C) 2004 V.I.Volchenko, Lazarus and FreePascal Developers Teams

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
}
unit LrtPoTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls, Forms, FileUtil, StringHashList,
  DialogProcs;

function AddFiles2Po(Files: TStrings; const POFilename: string): TModalResult;

implementation

function StrToPoStr(const s:string):string;
var
  SrcPos, DestPos: Integer;
  NewLength: Integer;
begin
  NewLength:=length(s);
  for SrcPos:=1 to length(s) do
    if s[SrcPos] in ['"','\'] then inc(NewLength);
  if NewLength=length(s) then begin
    Result:=s;
  end else begin
    SetLength(Result,NewLength);
    DestPos:=1;
    for SrcPos:=1 to length(s) do begin
      case s[SrcPos] of
      '"','\':
        begin
          Result[DestPos]:='\';
          inc(DestPos);
          Result[DestPos]:=s[SrcPos];
          inc(DestPos);
        end;
      else
        Result[DestPos]:=s[SrcPos];
        inc(DestPos);
      end;
    end;
  end;
end;

function AddFiles2Po(Files: TStrings; const POFilename: string): TModalResult;
type
  TFileType = (ftLrt, ftRst);
var
  POValuesHash: TStringHashList;
  POFileChanged: boolean;
  POLines: TStrings;
  Value,Identifier: string;

  procedure AddPoHashEntry;
  begin
    if POValuesHash.Find(Value) = -1 then begin
      DebugLn(['AddFile2PoAux Add ',Identifier,'="',Value,'"']);
      POFileChanged := true;
      POLines.Add('#: '+Identifier);
      POLines.Add('msgid "'+Value+'"');
      POLines.Add('msgstr ""');
      POLines.Add('');
      POValuesHash.Add(Value);
    end;
  end;

  procedure AddFile2PoAux(InputLines: TStringList; FileType: TFileType);
  var
    i,j,n: integer;
    p: LongInt;
    Line,UStr: string;
    Multi: boolean;
  begin
    //for each string in lrt/rst list check if in PO, if not add
    for i:=0 to InputLines.Count-1 do begin
      Line:=InputLines[i];
      n := Length(Line);
      if n=0 then
        continue;
      case FileType of
        ftLrt: begin
          p:=Pos('=',Line);
          Value:=StrToPoStr( copy(Line,p+1,n-p) );//if p=0, that's OK, all the string
          Identifier:=copy(Line,1,p-1);
          AddPoHashEntry;
        end;

        ftRst: begin
          if (Line[1]='#') then begin
            Value := '';
            Identifier := '';
            continue;
          end;

          if Identifier='' then begin
            p:=Pos('=',Line);
            if P=0 then
              continue;
            Identifier := copy(Line,1,p-1);
            inc(p); // points to ' after =
          end else
            p:=1;   // first char in line

          // this will assume rst file is well formed and
          // do similar to rstconv but recognize utf-8 strings
          Multi := Line[n]='+';
          while p<=n do begin
            if Line[p]='''' then begin
              inc(p);
              j:=p;
              while (p<=n)and(Line[p]<>'''') do
                inc(p);
              Value := Value + copy(Line, j, P-j);
              inc(p);
              continue;
            end else
            if Line[p] = '#' then begin
              // collect a string with special chars
              UStr:='';
              repeat
                inc(p);
                j:=p;
                while (p<=n)and(Line[p] in ['0'..'9']) do
                  inc(p);
                UStr := UStr + Chr(StrToInt(copy(Line, j, p-j)));
              until (Line[p]<>'#') or (p>=n);
              // transfer valid UTF-8 segments to result string
              // and re-encode back the rest
              while Ustr<>'' do begin
                j := UTF8CharacterLength(pchar(Ustr));
                if (j=1) and (Ustr[1] in [#0..#31,#128..#255]) then
                  Value := Value + '#'+IntToStr(ord(Ustr[1]))
                else
                  Value := Value + copy(Ustr, 1, j);
                Delete(UStr, 1, j);
              end;
            end else
            if Line[p]='+' then
              break;
          end;
          if not Multi then begin
            Value := StrToPoStr(Value);
            if Value<>'' then
              AddPoHashEntry;
          end;
        end;
      end;
    end;
  end;

var
  InputLines: TStringList;
  i: Integer;
  s: String;
  Filename: string;
begin
  if (Files=nil) or (Files.Count=0) then exit(mrOk);

  POFileChanged := false;
  POLines:=TStringList.Create;
  InputLines:=TStringList.Create;
  POValuesHash := TStringHashList.Create(true);
  try

    //load old po file into a StringList and HashList
    POLines.Clear;
    if FileExists(POFilename) then begin
      Result:=LoadStringListFromFile(POFilename, 'PO File', POLines);
      if Result <> mrOK then Exit;

      for i := 0 to POLines.Count-1 do begin
        s:=POLines[i];
        if LeftStr(s, 7) = 'msgid "' then begin
          s := copy(s, 8,length(s)-8);
          POValuesHash.Add(s);
        end;
      end;
    end else begin
      // To allow poedit to recognize the resulting
      // PO file as valid UTF-8 file, it needs a
      // minimal header (found by trial and error)
      POLines.Add('msgid ""');
      POLines.Add('msgstr ""');
      POLines.Add('"Content-Type: text/plain; charset=UTF-8\n"');
      POLines.Add('');
    end;

    //merge changes of every input file
    // At the moment it only adds new strings and replaces values,
    // but does not delete unused -> ToDo
    for i:=0 to Files.Count-1 do begin
      Filename:=Files[i];
      if (CompareFileExt(Filename,'.lrt')=0)
      or (CompareFileExt(Filename,'.rst')=0) then begin
        //DebugLn(['AddFiles2Po Filename="',Filename,'"']);
        InputLines.Clear;
        Result:=LoadStringListFromFile(Filename, 'Update PO file '+POFilename,
                                       InputLines);
        if Result <> mrOK then Exit;
        if CompareFileExt(Filename,'.lrt')=0 then
          AddFile2PoAux(InputLines, ftLrt)
        else
          AddFile2PoAux(InputLines, ftRst);
      end
    end;

    //if PO file changed save it
    if POFileChanged then
      Result:=SaveStringListToFile(POFilename, 'PO File', POLines)
    else
      Result:=mrOk;
  finally
    POLines.Free;
    InputLines.Free;
    POValuesHash.Free;
  end;
end;

end.

