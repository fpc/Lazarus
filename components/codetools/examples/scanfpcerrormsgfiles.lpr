{
  Author: Mattias Gaertner

  Abstract:
    Scan all FPC error message files (fpc/compiler/msg/error*.msg) and compare
    them with the original file (english, errore.msg).
}
program scanfpcerrormsgfiles;

{$mode objfpc}{$H+}

uses
  sysutils, CodeToolsFPCMsgs, CodeToolsStructs, LazFileUtils;

procedure WriteUsage;
begin
  writeln('Usage:');
  writeln;
  writeln('  ',ParamStr(0),' <fpcsrcdirectory>/compiler/msg [-v]');
  writeln('');
  writeln('  -v  - verbose output, showing exactly what messages are missing and/or mismatched');
end;

type
  THaveParams = array[0..9] of boolean;

procedure GetHaveParams(Pattern: string; out Params: THaveParams);
var
  i: integer;
  p: PChar;
begin
  for i:=0 to 9 do
    Params[i]:=false;
  p:=PChar(Pattern);
  repeat
    case p^ of
    #0: break;
    '$':
      if p[1] in ['0'..'9'] then begin
        inc(p);
        Params[ord(p^)-ord('0')]:=true;
      end;
    end;
    inc(p);
  until false;
end;

procedure ScanDir(Dir: string; ShowVerbose: Boolean = false );
var
  Info: TSearchRec;
  Filename: TFilename;
  aFile: TFPCMsgFile;
  FPCMsgFileList: TFilenameToPointerTree;
  EnglishFile: TFPCMsgFile;
  S2PItem: PStringToPointerTreeItem;
  i: Integer;
  EnglishMsg: TFPCMsgItem;
  TranslatedMsg: TFPCMsgItem;
  GoodCount: Integer;
  MissingCount: Integer;
  MismatchCount: Integer;
  EnglishParams, TranslatedParams: THaveParams;
  k: integer;
  msd: String;
begin
  FPCMsgFileList:=TFilenameToPointerTree.Create(false);
  FPCMsgFileList.FreeValues:=true;
  try
    // search *.msg files
    Dir:=AppendPathDelim(Dir);
    if not FindFirstUTF8(Dir+'*.msg',faAnyFile,Info)=0 then begin
      writeln('Error: no *.msg file found in ',Dir);
      Halt(3);
    end;

    // load all *.msg files
    EnglishFile:=nil;
    repeat
      Filename:=Info.Name;
      if (Filename='') or (Filename='.') or (Filename='..') then continue;
      if faDirectory and Info.Attr>0 then continue;
      //writeln('loading "',Filename,'" ...');
      aFile:=TFPCMsgFile.Create;
      aFile.LoadFromFile(Dir+Filename);
      if Filename='errore.msg' then
        EnglishFile:=aFile
      else
        FPCMsgFileList[Filename]:=aFile;
    until FindNextUTF8(Info)<>0;
    FindCloseUTF8(Info);

    if EnglishFile=nil then begin
      writeln('Error: missing file errore.msg');
      Halt(4);
    end;

    // compare each file with errore
    writeln('errore.msg Count=',EnglishFile.Count);
    for S2PItem in FPCMsgFileList do begin
      Filename:=S2PItem^.Name;
      aFile:=TFPCMsgFile(S2PItem^.Value);
      GoodCount:=0;
      MissingCount:=0;
      MismatchCount:=0; // id is there, but $ parameters don't fit
      msd:='';
      for i:=0 to EnglishFile.Count-1 do begin
        EnglishMsg:=EnglishFile[i];
        TranslatedMsg:=aFile.FindWithID(EnglishMsg.ID);
        if TranslatedMsg=nil then begin
          inc(MissingCount);
          if ShowVerbose then msd:=msd+'    missing: '+IntToStr(EnglishMsg.ID)+' '+EnglishMsg.Pattern+LineEnding;
        end else begin
          GetHaveParams(EnglishMsg.Pattern,EnglishParams);
          GetHaveParams(TranslatedMsg.Pattern,TranslatedParams);
          k:=9;
          while (k>=0) and (EnglishParams[k]=TranslatedParams[k]) do
            dec(k);
          if k<0 then
            inc(GoodCount)
          else begin
            //writeln('Mismatch in ',Filename,' English="',EnglishMsg.Pattern,'" Translated="',TranslatedMsg.Pattern,'"');
            inc(MismatchCount);
            if ShowVerbose then begin
              msd:=msd+'   mismatch: '+LineEnding;
              msd:=msd+'        eng: '+EnglishMsg.Pattern+LineEnding;
              msd:=msd+'        trn: '+TranslatedMsg.Pattern+LineEnding;
            end;
          end;
        end;
      end;
      writeln(Filename,' Count=',aFile.Count,' Good=',GoodCount,' Missing=',MissingCount,' Mismatch=',MismatchCount);
      if ShowVerbose then write(msd);
    end;

  finally
    FPCMsgFileList.Free;
  end;
end;

var
  MsgDir: String;
  ShowVerbose: Boolean = false;
begin
  if ParamCount<1 then begin
    WriteUsage;
    Halt(1);
  end;
  MsgDir:=CleanAndExpandDirectory(ParamStr(1));
  if not DirPathExists(MsgDir) then begin
    writeln('Error: directory not found: ',MsgDir);
    Halt(2);
  end;
  ShowVerbose:=(ParamCount>1) and (LowerCase(ParamStr(2))='-v');
  ScanDir(MsgDir, ShowVerbose);
end.

