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
  FileUtil;

var
  LazarusDir: String;

function FindLPK(Dir,MainSrc: string; out HasConditionals: boolean): string;
var
  FileInfo: TSearchRec;
  LPK: TXMLConfig;
  i: Integer;
  Path: String;
  FileType: String;
  CurFilename: String;
  Fits: Boolean;
  LPKFilename: TFilename;
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
        //writeln('FindLPK ',LPKFilename);
        for i:=1 to LPK.GetValue('Package/Files/Count',0) do begin
          Path:='Package/Files/Item'+IntToStr(i)+'/';
          FileType:=LPK.GetValue(Path+'Type/Value','');
          if FileType='' then continue;
          CurFilename:=LPK.GetValue(Path+'Filename/Value','');
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

procedure CheckMakefileCompiled(MakefileCompiledFilename: string; LPKFiles: TStrings);
var
  MakefileCompiled: TXMLConfig;
  MainSrcFilename: String;
  p: Integer;
  LPKFilename: String;
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
    if HasConditionals then begin
      writeln('Skipping ',LPKFilename,' due to conditionals');
      exit;
    end;
    LPKFiles.Add(CreateRelativePath(LPKFilename,LazarusDir));
  finally
    MakefileCompiled.Free;
  end;
end;

procedure FindLPKFilesWithMakefiles(Dir: string; LPKFiles: TStrings);
var
  FileInfo: TSearchRec;
begin
  Dir:=AppendPathDelim(Dir);
  if FindFirstUTF8(Dir+AllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if (FileInfo.Name='') or (FileInfo.Name='.') or (FileInfo.Name='..') then
        continue;
      if (FileInfo.Attr and faDirectory)<>0 then
        FindLPKFilesWithMakefiles(Dir+FileInfo.Name,LPKFiles)
      else if FileInfo.Name='Makefile.compiled' then
        CheckMakefileCompiled(Dir+FileInfo.Name,LPKFiles);
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

procedure CheckFPCMake;
const
  LastTarget = 'aarch64-darwin';
var
  FPCMake: String;
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

var
  LPKFiles: TStringList;
  LazbuildOut: TStringList;
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
  LPKFiles:=TStringList.Create;
  FindLPKFilesWithMakefiles(LazarusDir,LPKFiles);
  writeln(LPKFiles.Text);
  LPKFiles.StrictDelimiter:=true;
  LPKFiles.Delimiter:=' ';
  LazbuildExe:=SetDirSeparators(LazarusDir+'/lazbuild'+ExeExt);
  if not FileIsExecutable(LazbuildExe) then
  begin
    writeln('Error: missing ',LazbuildExe);
    Halt(1);
  end;
  LazbuildOut:=RunTool(SetDirSeparators(LazarusDir+'/lazbuild'+ExeExt),'--lazarusdir="'+LazarusDir+'" --create-makefile '+LPKFiles.DelimitedText,LazarusDir);
  writeln(LazbuildOut.Text);
  LPKFiles.Free;
end.

