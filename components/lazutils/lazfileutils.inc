{%MainUnit lazfileutils.pas}

function ResolveDots(const AFilename: string): string;
//trim double path delims and expand special dirs like .. and .
//on Windows change also '/' to '\' except for filenames starting with '\\?\'

  {$ifdef windows}
  function IsDriveDelim(const Path: string; p: integer): boolean; inline;
  begin
    Result:=(p=2) and (Path[2]=DriveDelim) and (Path[1] in ['a'..'z','A'..'Z']);
  end;
  {$endif}

  function IsPathDelim(const Path: string; p: integer): boolean;
  begin
    if (p<=0) or (Path[p]=PathDelim) then exit(true);
    {$ifdef windows}
    if IsDriveDelim(Path,p) then
      exit(true);
    {$endif}
    Result:=false;
  end;

var SrcPos, DestPos, Len, DirStart: integer;
  c: char;
  MacroPos: LongInt;
begin
  Len:=length(AFilename);
  if Len=0 then exit('');

  Result:=AFilename;

  {$ifdef windows}
  //Special case: everything is literal after this, even dots (this does not apply to '//?/')
  if (length(AFilename)>=4) and (AFilename[1]='\') and (AFilename[2]='\')
  and (AFilename[3]='?') and (AFilename[4]='\') then
    exit;
  {$endif}

  SrcPos:=1;
  DestPos:=1;

  // trim double path delimiters and special dirs . and ..
  while (SrcPos<=Len) do begin
    c:=AFilename[SrcPos];
    {$ifdef windows}
    //change / to \. The WinApi accepts both, but it leads to strange effects in other places
    if (c in AllowDirectorySeparators) then c := PathDelim;
    {$endif}
    // check for duplicate path delims
    if (c=PathDelim) then begin
      inc(SrcPos);
      {$IFDEF Windows}
      if (DestPos>2)
      {$ELSE}
      if (DestPos>1)
      {$ENDIF}
      and (Result[DestPos-1]=PathDelim) then begin
        // skip duplicate PathDelim
        continue;
      end;
      Result[DestPos]:=c;
      inc(DestPos);
      continue;
    end;
    // check for special dirs . and ..
    if (c='.') then begin
      if (SrcPos<Len) then begin
        if (AFilename[SrcPos+1] in AllowDirectorySeparators)
        and IsPathDelim(Result,DestPos-1) then begin
          // special dir ./ or */./
          // -> skip
          inc(SrcPos,2);
          while (SrcPos<=Len) and (AFilename[SrcPos] in AllowDirectorySeparators) do
            inc(SrcPos);
          continue;
        end else if (AFilename[SrcPos+1]='.')
        and ((SrcPos+1=Len) or (AFilename[SrcPos+2] in AllowDirectorySeparators)) then
        begin
          // special dir ..
          //  1. ..      -> copy
          //  2. /..     -> skip .., keep /
          //  3. C:..    -> copy
          //  4. C:\..   -> skip .., keep C:\
          //  5. \\..    -> skip .., keep \\
          //  6. ../..   -> copy because if the first '..' was not resolved, the next can't neither
          //  7. dir/..  -> trim dir and ..
          //  8. dir$macro/..  -> copy
          if DestPos=1 then begin
            //  1. .. or ../  -> copy
          end else if (DestPos=2) and (Result[1]=PathDelim) then begin
            //  2. /..     -> skip .., keep /
            inc(SrcPos,2);
            continue;
          {$IFDEF Windows}
          end else if (DestPos=3) and IsDriveDelim(Result,2) then begin
            //  3. C:..    -> copy
          end else if (DestPos=4) and (Result[3]=PathDelim)
          and IsDriveDelim(Result,2) then begin
            //  4. C:\..   -> skip .., keep C:\
            inc(SrcPos,2);
            continue;
          end else if (DestPos=3) and (Result[1]=PathDelim)
          and (Result[2]=PathDelim) then begin
            //  5. \\..    -> skip .., keep \\
            inc(SrcPos,2);
            continue;
          {$ENDIF}
          end else if (DestPos>1) and (Result[DestPos-1]=PathDelim) then begin
            // */.
            if (DestPos>3)
            and (Result[DestPos-2]='.') and (Result[DestPos-3]='.')
            and IsPathDelim(Result,DestPos-4) then begin
              //  6. ../..   -> copy because if the first '..' was not resolved, the next can't neither
            end else begin
              //  7. xxxdir/..  -> trim dir and skip ..
              DirStart:=DestPos-2;
              while (DirStart>1) and (Result[DirStart-1]<>PathDelim) do
                dec(DirStart);
              {$ifdef windows}
              if (DirStart=1) and IsDriveDelim(Result,2) then
                inc(DirStart,2);
              {$endif}
              MacroPos:=DirStart;
              while MacroPos<DestPos do begin
                if (Result[MacroPos]='$')
                and (Result[MacroPos+1] in ['(','a'..'z','A'..'Z']) then begin
                  // 8. directory contains a macro -> keep
                  break;
                end;
                inc(MacroPos);
              end;
              if MacroPos=DestPos then begin
                // previous directory does not contain a macro -> remove dir/..
                DestPos:=DirStart;
                inc(SrcPos,2);
                //writeln('ResolveDots ',DestPos,' SrcPos=',SrcPos,' File="',AFilename,'" Result="',copy(Result,1,DestPos-1),'"');
                if SrcPos>Len then begin
                  // '..' at end of filename
                  if (DestPos>1) and (Result[DestPos-1]=PathDelim) then begin
                    // foo/dir/.. -> foo
                    dec(DestPos);
                  end else if (DestPos=1) then begin
                    // foo/.. -> .
                    Result[1]:='.';
                    DestPos:=2;
                  end;
                end else if DestPos=1 then begin
                  // e.g. 'foo/../'
                  while (SrcPos<=Len) and (AFilename[SrcPos] in AllowDirectorySeparators) do
                    inc(SrcPos);
                end;
                continue;
              end;
            end;
          end;
        end;
      end else begin
        // special dir . at end of filename
        if DestPos=1 then begin
          Result:='.';
          exit;
        end;
        if (DestPos>2) and (Result[DestPos-1]=PathDelim)
        {$ifdef windows}
        and not IsDriveDelim(Result,DestPos-2)
        {$endif}
        then begin
          // foo/. -> foo
          // C:foo\. -> C:foo
          // C:\. -> C:\
          dec(DestPos);
        end;
        break;
      end;
    end;
    // copy directory
    repeat
      Result[DestPos]:=c;
      inc(DestPos);
      inc(SrcPos);
      if (SrcPos>Len) then break;
      c:=AFilename[SrcPos];
      {$ifdef windows}
      //change / to \. The WinApi accepts both, but it leads to strange effects in other places
      if (c in AllowDirectorySeparators) then c := PathDelim;
      {$endif}
      if c=PathDelim then break;
    until false;
  end;
  // trim result
  if DestPos<=length(AFilename) then
    if (DestPos=1) then
      Result:='.'
    else
      SetLength(Result,DestPos-1);
end;

function FilenameIsWinAbsolute(const TheFilename: string): boolean;
begin
  {$ifdef wince}
  Result := (Length(TheFilename) > 0) and (TheFilename[1] in AllowDirectorySeparators);
  {$else wince}
  Result:=((length(TheFilename)>=3) and
           (TheFilename[1] in ['A'..'Z','a'..'z']) and (TheFilename[2]=':')  and (TheFilename[3]in AllowDirectorySeparators))
      or ((length(TheFilename)>=2) and (TheFilename[1] in AllowDirectorySeparators) and (TheFilename[2] in AllowDirectorySeparators))
      ;
  {$endif wince}
end;

function FilenameIsUnixAbsolute(const TheFilename: string): boolean;
begin
  Result:=(TheFilename<>'') and (TheFilename[1]='/');
end;

function CreateAbsolutePath(const Filename, BaseDirectory: string): string;
begin
  if (Filename='') or FilenameIsAbsolute(Filename) then
    Result:=Filename
  {$IFDEF Windows}
  else if (Filename[1]='\') then
    // only use drive of BaseDirectory
    Result:=ExtractFileDrive(BaseDirectory)+Filename
  {$ENDIF}
  else
    Result:=AppendPathDelim(BaseDirectory)+Filename;
  Result:=TrimFilename(Result);
end;


{
  Returns True if it is possible to create a relative path from Source to Dest
  Function must be thread safe, so no expanding of filenames is done, since this
  is not threadsafe (at least on Windows platform)

  - Dest and Source must either be both absolute filenames, or relative
  - Dest and Source cannot contain '..' since no expanding is done by design
  - Dest and Source must be on same drive or UNC path (Windows)
  - if both Dest and Source are relative they must at least share their base directory
  - Double PathDelims are ignored (unless they are part of the UNC convention)

  - if UsePointDirectory is True and Result is True then if RelPath is Empty string, RelPath becomes '.'
  - if AlwaysRequireSharedBaseFolder is False then Absolute filenames need not share a basefolder

  - if the function succeeds RelPath contains the relative path from Source to Dest,
    no PathDelimiter is appended to the end of RelPath

  Examples:
  - Dest = /foo/bar Source = /foo Result = True RelPath = bar
  - Dest = /foo///bar Source = /foo// Result = True RelPath = bar
  - Dest = /foo Source = /foo/bar Result = True RelPath = ../
  - Dest = /foo/bar Source = /bar Result = True RelPath = ../foo/bar
  - Dest = foo/bar Source = foo/foo Result = True RelPath = ../bar
  - Dest = foo/bar Source = bar/foo Result = False (no shared base directory)
  - Dest = /foo Source = bar Result = False (mixed absolute and relative)
  - Dest = c:foo Source = c:bar Result = False (no expanding)
  - Dest = c:\foo Source = d:\bar Result is False (different drives)
  - Dest = \foo Source = foo (Windows) Result is False (too ambiguous to guess what this should mean)
  - Dest = /foo Source = /bar AlwaysRequireSharedBaseFolder = True Result = False
  - Dest = /foo Source = /bar AlwaysRequireSharedBaseFolder = False Result = True RelPath = ../foo
}
function TryCreateRelativePath(const Dest, Source: String; UsePointDirectory: boolean;
  AlwaysRequireSharedBaseFolder: Boolean; out RelPath: String): Boolean;
Const
  MaxDirs = 129;
Type
  TDirArr =  Array[1..MaxDirs] of String;

  function SplitDirs(Dir: String; out Dirs: TDirArr): Integer;
  var
    Start, Stop, Len: Integer;
    S: String;
  begin
    Result := 0;
    Len := Length(Dir);
    if (Len = 0) then Exit;
    Start := 1;
    Stop := 1;

    While Start <= Len do
    begin
      if (Dir[Start] in AllowDirectorySeparators) then
      begin
        S := Copy(Dir,Stop,Start-Stop);
        //ignore empty strings, they are caused by double PathDelims, which we just ignore
        if (S <> '') then
        begin
          Inc(Result);
          if Result>High(Dirs) then
            raise Exception.Create('too many sub directories');
          Dirs[Result] := S;
        end;
        Stop := Start + 1;
      end;
      Inc(Start);
    end;
    //If (Len > 0) then

    S := Copy(Dir,Stop,Start-Stop);
    if (S <> '') then
    begin
      Inc(Result);
      Dirs[Result] := S;
    end;
  end;


var
  CompareFunc: function(const Item1, Item2: String): PtrInt;
  SourceRoot, DestRoot, CmpDest, CmpSource: String;
  CmpDestLen, CmpSourceLen, DestCount, SourceCount, i,
  SharedFolders, LevelsBack, LevelsUp: Integer;
  SourceDirs, DestDirs: Array[1..MaxDirs] of String;
  IsAbs: Boolean;
begin
  Result := False;
  if (Dest = '') or (Source = '') then Exit;
  if (Pos('..',Dest) > 0) or (Pos('..',Source) > 0) then Exit;
  SourceRoot := ExtractFileRoot(Source);
  DestRoot := ExtractFileRoot(Dest);
  //debugln('TryCreaterelativePath: DestRoot = "',DestRoot,'"');
  //debugln('TryCreaterelativePath: SourceRoot = "',SourceRoot,'"');
  //Root must be same: either both absolute filenames or both relative (and on same drive in Windows)
  if (CompareFileNames(SourceRoot, DestRoot) <> 0) then Exit;
  IsAbs := (DestRoot <> '');
  {$if defined(windows) and not defined(wince)}
  if not IsAbs then  // relative paths
  begin
    //we cannot handle files like c:foo
    if ((Length(Dest) > 1) and (UpCase(Dest[1]) in ['A'..'Z']) and (Dest[2] = ':')) or
       ((Length(Source) > 1) and (UpCase(Source[1]) in ['A'..'Z']) and (Source[2] = ':')) then Exit;
    //we cannot handle combinations like dest=foo source=\bar or the other way around
    if ((Dest[1] in AllowDirectorySeparators) and not (Source[1] in AllowDirectorySeparators)) or
       (not (Dest[1] in AllowDirectorySeparators) and (Source[1] in AllowDirectorySeparators)) then Exit;
  end;
  {$endif}

  {$IFDEF CaseInsensitiveFilenames}
  CompareFunc := @UTF8CompareText;
  {$else CaseInsensitiveFilenames}
  CompareFunc := @Utf8CompareStr;
  {$endif CaseInsensitiveFilenames}

  CmpSource := Source;
  CmpDest := Dest;
  {$IFDEF darwin}
  CmpSource := GetDarwinSystemFilename(CmpSource);
  CmpDest := GetDarwinSystemFilename(CmpDest);
  {$ENDIF}


  CmpDest := ChompPathDelim(Dest);
  CmpSource := ChompPathDelim(Source);
  if IsAbs then
  begin
    System.Delete(CmpSource,1,Length(SourceRoot));
    System.Delete(CmpDest,1,Length(DestRoot));
  end;

  //Get rid of excessive trailing PathDelims now after (!) we stripped Root
  while (Length(CmpDest) > 0) and (CmpDest[Length(CmpDest)] in AllowDirectorySeparators) do System.Delete(CmpDest,Length(CmpDest),1);
  while (Length(CmpSource) > 0) and (CmpSource[Length(CmpSource)] in AllowDirectorySeparators) do System.Delete(CmpSource,Length(CmpSource),1);

  //debugln('TryCreaterelativePath: CmpDest   = "',cmpdest,'"');
  //debugln('TryCreaterelativePath: CmpSource = "',cmpsource,'"');
  CmpDestLen := Length(CmpDest);
  CmpSourceLen := Length(CmpSource);

  DestCount := SplitDirs(CmpDest, DestDirs);
  SourceCount :=  SplitDirs(CmpSource, SourceDirs);

  //debugln('TryCreaterelativePath: DestDirs:');
  //for i := 1 to DestCount do debugln(DbgS(i),' "',DestDirs[i],'"'); debugln;
  //debugln('TryCreaterelativePath:');
  //for i := 1 to SourceCount do debugln(DbgS(i),' "',SourceDirs[i],'"'); debugln;


  i := 1;
  SharedFolders := 0;
  while (i <= DestCount) and (i <= SourceCount) do
  begin
    if (CompareFunc(DestDirs[i], SourceDirs[i]) = 0) then
    begin
      Inc(SharedFolders);
      Inc(i);
    end
    else
    begin
      Break;
    end;
  end;

  //debugln('TryCreaterelativePath: SharedFolders = ',DbgS(SharedFolders));
  if (SharedFolders = 0) and ((not IsAbs) or AlwaysRequireSharedBaseFolder) and not ((CmpDestLen = 0) or (CmpSourceLen = 0)) then
  begin
    //debguln('TryCreaterelativePath: FAIL: IsAbs = ',DbgS(IsAs),' AlwaysRequireSharedBaseFolder = ',DbgS(AlwaysRequireSharedBaseFolder),
    //' SharedFolders = 0, CmpDestLen = ',DbgS(cmpdestlen),' CmpSourceLen = ',DbgS(CmpSourceLen));
    Exit;
  end;
  LevelsBack := SourceCount - SharedFolders;
  LevelsUp := DestCount - SharedFolders;
  //debugln('TryCreaterelativePath: LevelsBack = ',DbgS(Levelsback));
  //debugln('TryCreaterelativePath: LevelsUp   = ',DbgS(LevelsUp));
  if (LevelsBack > 0) then
  begin
    RelPath := '';
    for i := 1 to LevelsBack do RelPath := '..' + PathDelim + Relpath;

    for i := LevelsUp downto 1 do
    begin
      if (RelPath <> '') and not (RelPath[Length(RelPath)] in AllowDirectorySeparators) then RelPath := RelPath + PathDelim;
      RelPath := RelPath + DestDirs[DestCount + 1 - i];
    end;
    RelPath := ChompPathDelim(RelPath);
  end
  else
  begin
    RelPath := '';
    for i := LevelsUp downto 1 do
    begin
      if (RelPath <> '') then RelPath := RelPath + PathDelim;
      RelPath := RelPath + DestDirs[DestCount + 1 - i];
    end;
  end;
  if UsePointDirectory and (RelPath = '') then
    RelPath := '.'; // Dest = Source

  Result := True;
end;

function CreateRelativePath(const Filename, BaseDirectory: string;
  UsePointDirectory: boolean; AlwaysRequireSharedBaseFolder: Boolean): string;
var
  RelPath: String;
begin
  Result:=Filename;
  if TryCreateRelativePath(FileName, Basedirectory, UsePointDirectory, AlwaysRequireSharedBaseFolder, RelPath) then
    Result := RelPath;
end;

procedure FindCloseUTF8(var F: TSearchrec);
begin
  SysUtils.FindClose(F);
end;

function DbgSFileAttr(Attr: LongInt): String;
begin
  if (Attr = -1) then
    Result := ('[Invalid]')
  else
  begin
    Result := '[-------]';
    if (faDirectory and Attr) > 0 then Result[2] := 'D';
    if (faArchive and Attr)   > 0 then Result[3] := 'A';
    if (faSysFile{%H-} and Attr)   > 0 then Result[4] := 'S';
    if (faReadOnly and Attr)  > 0 then Result[5] := 'R';
    if (faHidden{%H-} and Attr)    > 0 then Result[6] := 'H';
    if (faVolumeId{%H-} and Attr)  > 0 then Result[7] := 'V';
    if (faSymLink{%H-} and Attr)   > 0 then Result[8] := 'L';
  end;
end;

