program UpdateInnoRemovedFilesFromGitDiff;
{$Mode objfpc}{$H+}

(* Call
  UpdateInnoRemovedFilesFromGitDiff.exe c:\lazarus_git_dir lazarus_3_0 lazarus_3_6 lazarus_4_0

  - The git root dir
  - list of recent release tags

*)

uses SysUtils, Classes, LazFileUtils, Laz2_XMLCfg, process;

function FixDelim(p: string): String;
begin
  Result := StringReplace(p, '/', '\', [rfReplaceAll]);
end;

function ReplaceVar(p: string; cpu, os, ws: String): String;
begin
  Result := StringReplace(p,      '$(TargetCPU)',     cpu, [rfReplaceAll]);
  Result := StringReplace(Result, '$(TargetOS)',      os, [rfReplaceAll]);
  Result := StringReplace(Result, '$(LCLWidgetType)', ws, [rfReplaceAll]);
end;

var
  inno: string;

procedure AddToInno(n: string);
begin
  n := FixDelim(n);
  inno := inno
    + 'Type: files; Name: "{app}\' + n + '"'
    + LineEnding;
end;


var
  i: Integer;
  MainDir, s, p, GitDiff, PkgPath, UnitDir, p1, p2, p3, p4, f: String;
  FileLst: TStringList;
  frec: TRawByteSearchRec;
  PkgXml: TXMLConfig;

begin
  MainDir := AppendPathDelim(argv[1]);

  FileLst := TStringList.Create;
  FileLst.Sorted := True;
  FileLst.Duplicates := dupIgnore;

  for i := 2 to argc - 1 do begin
    s := argv[i];
    RunCommand('git.exe', ['diff', '--name-only', '--diff-filter', 'D', s], GitDiff);
    for s in GitDiff.Split([#10,#13], TStringSplitOptions([TStringSplitOptions.ExcludeLastEmpty])) do
      case ExtractFileExt(s) of
        '.pas', '.pp', '.inc', '.lfm': FileLst.Add(s);
      end;
  end;


  inno := '';
  for i := 0 to FileLst.Count - 1 do begin
    s := FileLst[i];
    s := FixDelim(s);
    // Output the file itself
    AddToInno(s);

    p := AppendPathDelim(ExtractFileDir(s));
    PkgPath := '';
    if p = 'ide\' then
      PkgPath := 'ide\lazarus.lpi'
    else
    if FindFirst(MainDir + p+'*.lpk', faAnyFile, frec) = 0 then
      PkgPath := p + frec.Name
    else
    begin
      delete(p, Length(p), 1); // path delim
      p := AppendPathDelim(ExtractFileDir(p));
      if FindFirst(MainDir + p+'*.lpk', faAnyFile, frec) = 0 then
        PkgPath := p + frec.Name;
    end;

    if PkgPath = '' then
      writeln(STDERR, '### no package for ', s);
    if PkgPath <> '' then begin
      PkgXml := TXMLConfig.Create(MainDir + PkgPath);
      UnitDir := PkgXml.GetValue('Package/CompilerOptions/SearchPaths/UnitOutputDirectory/Value', '');
      if UnitDir = '' then
        UnitDir := PkgXml.GetValue('CompilerOptions/SearchPaths/UnitOutputDirectory/Value', '');
      //writeln('## ',PkgPath, ' # ',UnitDir);
      PkgXml.Destroy;

      if UnitDir = '' then continue;

      if strlcomp(PChar(UnitDir), PChar('$(LazarusDir)/'), 14) = 0 then
        delete(UnitDir, 1, 14)
      else
        UnitDir := p + AppendPathDelim(UnitDir);
      p1 := ReplaceVar(UnitDir, 'x86_64', 'win64', 'win32');
      p2 := ReplaceVar(UnitDir, 'i386',   'win32', 'win32');
      p3 := ReplaceVar(UnitDir, 'x86_64', 'win64', 'nogui');
      p4 := ReplaceVar(UnitDir, 'i386',   'win32', 'nogui');
      f := ExtractFileNameWithoutExt(ExtractFileNameOnly(s));
      case ExtractFileExt(s) of
        '.pas', '.pp': begin
          AddToInno(p1+f+'.ppu');
          AddToInno(p1+f+'.o');
          if p3 <> p1 then begin
            AddToInno(p3+f+'.ppu');
            AddToInno(p3+f+'.o');
          end;
          if p2 <> p1 then begin
            AddToInno(p2+f+'.ppu');
            AddToInno(p2+f+'.o');
            if p4 <> p2 then begin
              AddToInno(p4+f+'.ppu');
              AddToInno(p4+f+'.o');
            end;
          end;
        end;
        '.lfm': begin
          AddToInno(p1+f+'.lfm');
          if p3 <> p1 then begin
            AddToInno(p3+f+'.lfm');
          end;
          if p2 <> p1 then begin
            AddToInno(p2+f+'.lfm');
            if p4 <> p2 then begin
              AddToInno(p4+f+'.lfm');
            end;
          end;
        end;
      end;

    end;
  end;

  writeln(inno);
end.

