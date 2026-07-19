{ Tool to update all Makefiles for lazarus packages.

  Copyright (C) 2012 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

program updatemakefiles;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, FileProcs, DefineTemplates, LazFileUtils, Laz2_XMLCfg,
  FileUtil, LazLogger;

var
  LazarusDir: String;
  FPCMake: String;

function FindLPK(Dir, MainSrc: string; out HasConditionals: boolean): string;
var
  FileInfo: TSearchRec;
  LPK: TXMLConfig;
  i: Integer;
  Path, FileType, CurFilename, SubPath: String;
  Fits, LegacyList: Boolean;
  LPKFilename: TFilename;
  FileVersion, Count: Integer;
begin
  Result:='';
  Dir:=AppendPathDelim(Dir);
  Fits:=false;
  HasConditionals:=false;
  if FindFirstUTF8(Dir+'*.lpk',faAnyFile,FileInfo)=0 then begin
    repeat
      LPKFilename:=Dir+FileInfo.Name;
      LPK:=TXMLConfig.Create(LPKFilename);
      try
        Fits:=false;
        if sysutils.CompareText(ExtractFileNameOnly(FileInfo.Name),ExtractFileNameOnly(MainSrc))=0 then
          Fits:=true;
        FileVersion:=LPK.GetValue('Package/Version',integer(0));
        //writeln('FindLPK ',LPKFilename);
        Path:='Package/Files/';
        LegacyList := (FileVersion<=4) or LPK.IsLegacyList(Path);
        Count:=LPK.GetListItemCount(Path, 'Item', LegacyList);
        for i:=0 to Count-1 do begin
          SubPath := Path+LPK.GetListItemXPath('Item', i, LegacyList, True)+'/';
          FileType:=LPK.GetValue(SubPath+'Type/Value','');
          if FileType='' then continue;
          CurFilename:=SetDirSeparators(LPK.GetValue(SubPath+'Filename/Value',''));
          //writeln('FindLPK ',CurFilename,' ',FileType);
          if FileType='Main Unit' then begin
            //writeln('FindLPK MainUnit=',CurFilename);
            if CompareFilenames(CurFilename,MainSrc)=0 then begin
              Fits:=true;
              break;
            end;
          end;
        end;
        if Fits then begin
          Result:=LPKFilename;
          HasConditionals:=LPK.GetValue('Package/CompilerOptions/Conditionals/Value','')<>'';
          break;
        end;
      finally
        LPK.Free;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  if not Fits then
    writeln('NOTE: no lpk found in ',CreateRelativePath(Dir,LazarusDir),' for ',MainSrc,'. Probably this requires a custom Makefile.');
end;

procedure CheckMakefileCompiled(MakefileCompiledFilename: string; LPKFiles, ExtraMakefiles: TStrings);
var
  MakefileCompiled: TXMLConfig;
  MainSrcFilename: String;
  p: Integer;
  LPKFilename, MakeFilename: String;
  HasConditionals: boolean;
begin
  //writeln('CheckMakefileCompiled ',MakefileCompiledFilename);
  MakefileCompiled:=TXMLConfig.Create(MakefileCompiledFilename);
  try
    // extract main source file name from compiler options
    MainSrcFilename:=MakefileCompiled.GetValue('Params/Value','');
    p:=length(MainSrcFilename);
    while (p>1) and (MainSrcFilename[p-1]<>' ') do dec(p);
    MainSrcFilename:=copy(MainSrcFilename,p,length(MainSrcFilename));
    LPKFilename:=FindLPK(ExtractFilePath(MakefileCompiledFilename),MainSrcFilename,HasConditionals);
    //writeln('  MakefileCompiled=',CreateRelativePath(MakefileCompiledFilename,LazarusDir),' MainSrc=',MainSrcFilename,' lpk=',CreateRelativePath(LPKFilename,LazarusDir),' HasConditionals=',HasConditionals);
    if (LPKFilename='') then exit;
    MakeFilename:=ChangeFileExt(MakefileCompiledFilename,'');
    if HasConditionals then
      ExtraMakefiles.Add(MakeFilename)
    else
      LPKFiles.Add(CreateRelativePath(LPKFilename,LazarusDir));
  finally
    MakefileCompiled.Free;
  end;
end;

{ Collect the directories of all "$(MAKE) -C <dir>" lines of the rule Target
  and recursively of its prerequisites. }
procedure CollectMakeDirs(Lines: TStrings; Target: string;
  Dirs, VisitedTargets: TStrings);
var
  i, j, LineLen, StartPos: Integer;
  Line, Prereqs, CurDir: String;
  Prereq: String;
  PrereqList: TStringList;
begin
  if VisitedTargets.IndexOf(Target)>=0 then exit;
  VisitedTargets.Add(Target);

  for i:=0 to Lines.Count-1 do begin
    Line:=Lines[i];
    // a rule starts at column 1 with "Target:"
    if copy(Line,1,length(Target)+1)<>Target+':' then continue;

    // the rest of the line is the list of prerequisites
    Prereqs:=copy(Line,length(Target)+2,length(Line));
    PrereqList:=TStringList.Create;
    try
      PrereqList.Delimiter:=' ';
      PrereqList.StrictDelimiter:=false;
      PrereqList.DelimitedText:=Prereqs;
      for Prereq in PrereqList do
        if Prereq<>'' then
          CollectMakeDirs(Lines,Prereq,Dirs,VisitedTargets);
    finally
      PrereqList.Free;
    end;

    // the following lines starting with a TAB are the recipe
    for j:=i+1 to Lines.Count-1 do begin
      Line:=Lines[j];
      if Line='' then continue;
      if Line[1]<>#9 then break;
      LineLen:=length(Line);
      // search the token "-C" and take the next token as directory
      StartPos:=1;
      while StartPos<LineLen do begin
        if (Line[StartPos]='-') and (Line[StartPos+1]='C')
            and ((StartPos=1) or (Line[StartPos-1]<=' '))
            and ((StartPos+2>LineLen) or (Line[StartPos+2]<=' ')) then
        begin
          inc(StartPos,2);
          while (StartPos<=LineLen) and (Line[StartPos]<=' ') do inc(StartPos);
          CurDir:='';
          while (StartPos<=LineLen) and (Line[StartPos]>' ') do begin
            CurDir:=CurDir+Line[StartPos];
            inc(StartPos);
          end;
          if (CurDir<>'') and (Dirs.IndexOf(CurDir)<0) then
            Dirs.Add(CurDir);
          break;
        end;
        inc(StartPos);
      end;
    end;
    break;
  end;
end;

{ Check the Makefile.compiled of all directories listed in the Makefile.fpc
  rule "lazbuild" (including its prerequisites). }
procedure FindLazbuildMakefiles(LPKFiles, ExtraMakefiles: TStrings);
var
  MakefileFPC, Dir, MakefileCompiled: String;
  Lines, Dirs, VisitedTargets: TStringList;
begin
  MakefileFPC:=LazarusDir+'Makefile.fpc';
  if not FileExistsUTF8(MakefileFPC) then
    raise Exception.Create('missing '+MakefileFPC);

  Lines:=TStringList.Create;
  Dirs:=TStringList.Create;
  VisitedTargets:=TStringList.Create;
  try
    Lines.LoadFromFile(MakefileFPC);
    CollectMakeDirs(Lines,'lazbuild',Dirs,VisitedTargets);
    for Dir in Dirs do begin
      MakefileCompiled:=AppendPathDelim(LazarusDir+SetDirSeparators(Dir))+'Makefile.compiled';
      if not FileExistsUTF8(MakefileCompiled) then continue;
      CheckMakefileCompiled(MakefileCompiled,LPKFiles,ExtraMakefiles);
    end;
  finally
    VisitedTargets.Free;
    Dirs.Free;
    Lines.Free;
  end;
end;

procedure CheckFPCMake;
const
  LastTarget = 'aarch64-darwin';
var
  Lines: TStringList;
begin
  FPCMake:=FindDefaultExecutablePath('fpcmake');
  if FPCMake='' then
    raise Exception.Create('missing fpcmake');
  Lines:=RunTool(FPCMake,'-TAll -v');
  if Pos(' '+LastTarget,Lines.Text)<1 then begin
    writeln(Lines.Text);
    raise Exception.Create('fpcmake does not support target '+LastTarget+'. Did you set PATH to the devel version of fpcmake?');
  end;
  Lines.Free;
end;

procedure UpdateCustomMakefiles(ExtraMakefiles: TStrings);
const
  Dirs: array[1..3] of string = (
    'components',
    'ide',
    'tools'
    );
var
  Dir, Filename: String;
  Lines: TStringList;
begin
  for Dir in Dirs do begin
    writeln(dir);
    Lines:=RunTool(FPCMake,'-TAll',LazarusDir+SetDirSeparators(Dir));
    //writeln(Lines.Text);
    Lines.Free;
  end;
  for Filename in ExtraMakefiles do begin
    writeln(Filename);
    Lines:=RunTool(FPCMake,'-TAll',ExtractFilePath(Filename));
    //writeln(Lines.Text);
    Lines.Free;
  end;
end;

var
  LPKFiles: TStringList;
  LazbuildOut, ExtraMakefiles: TStringList;
  LazbuildExe: String;
begin
  if Paramcount>0 then begin
    writeln('Updates for every lpk in the lazarus directory the Makefile.fpc, Makefile.compiled and Makefile.');
    writeln;
    writeln('Usage: FPCDIR=/path/fpc/src/trunk PATH=/path/to/trunk/fpc/utils/fpcm/bin/cpu-os/:$PATH ./tools/updatemakefiles');
    writeln;
    writeln('IMPORTANT: this needs the fpc trunk version to support all targets');
    exit;
  end;
  CheckFPCMake;

  LazarusDir:=CleanAndExpandDirectory(GetCurrentDirUTF8);
  if ExtractFileName(ChompPathDelim(LazarusDir))='tools' then
    LazarusDir:=ExtractFilePath(ChompPathDelim(LazarusDir));
  LazarusDir:=AppendPathDelim(LazarusDir);

  LPKFiles:=TStringList.Create;
  ExtraMakefiles:=TStringList.Create;

  FindLazbuildMakefiles(LPKFiles,ExtraMakefiles);
  writeln(LPKFiles.Text);
  writeln(ExtraMakefiles.Text);
  LPKFiles.StrictDelimiter:=true;
  LPKFiles.Delimiter:=' ';
  LazbuildExe:=SetDirSeparators(LazarusDir+'lazbuild'+ExeExt);
  if not FileIsExecutable(LazbuildExe) then
  begin
    writeln('Error: missing ',LazbuildExe);
    Halt(1);
  end;

  // run lazbuild
  LazbuildOut:=RunTool(SetDirSeparators(LazarusDir+'lazbuild'+ExeExt),'--lazarusdir="'+LazarusDir+'" --create-makefile '+LPKFiles.DelimitedText,LazarusDir);
  writeln(LazbuildOut.Text);

  // update custom Makefiles

  LazbuildOut.Free;
  LPKFiles.Free;

  UpdateCustomMakefiles(ExtraMakefiles);
  ExtraMakefiles.Free;
end.

