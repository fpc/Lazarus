{%MainUnit lazfileutils.pas}


var
//procedural variables for procedures that are implemented different on Win9x and NT or WinCE platform
//They are intialized in InitLazFileUtils
  //FileAge_         : function (const Filename:string):Longint;
  //FileSize_        : function (const Filename: string): int64;
  //FileSetDate_     : function (const FileName: String; Age: Longint): Longint;
  //FindFirst_       : function (const Path: string; Attr: Longint;
  //                     out Rslt: TSearchRec): Longint;
  //FindNext_        : function (var Rslt: TSearchRec): Longint;
  //FindClose_       : procedure (var F: TSearchrec);
  //FileGetAttr_     : function (const FileName: String): Longint;
  //FileSetAttr_     : function (const Filename: String; Attr: longint): Longint;
  //DeleteFile_      : function (const FileName: String): Boolean;
  //RenameFile_      : function (const OldName, NewName: String): Boolean;
  _GetCurrentDirUtf8   : function : String ;
  _GetDirUtf8          : procedure (DriveNr: Byte; var Dir: String);
  //SetCurrentDir_   : function (const NewDir: String): Boolean;
  //CreateDir_       : function (const NewDir: String): Boolean;
  //RemoveDir_       : function (const Dir: String): Boolean ;

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result:=FilenameIsWinAbsolute(TheFilename);
end;


function ExpandFileNameUtf8(const FileName: string; {const} BaseDir: String = ''): String;
var
  IsAbs, StartsWithRoot, CanUseBaseDir : Boolean;
  {$ifndef WinCE}
  HasDrive: Boolean;
  FnDrive, CurDrive, BaseDirDrive: Char;
  {$endif}
  CurDir, Fn: String;
begin
  //writeln('LazFileUtils.ExpandFileNameUtf8');
  //writeln('FileName = "',FileName,'"');
  //writeln('BaseDir  = "',BaseDir,'"');

  //{$ifndef WinCE}
  //if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then
  //  Result := SysToUtf8(SysUtils.ExpandFileName(Utf8ToSys(FileName)))
  //else
  //{$endif}
  Fn := FileName;
  DoDirSeparators(Fn);
  IsAbs := FileNameIsWinAbsolute(Fn);
  if not IsAbs then
  begin
    StartsWithRoot := (Fn = '\') or
                      ((Length(Fn) > 1) and
                      (Fn[1] = DirectorySeparator) and
                      (Fn[2] <> DirectorySeparator));
    {$ifndef WinCE}
    HasDrive := (Length(Fn) > 1) and
                (Fn[2] = ':') and
                (UpCase(Fn[1]) in ['A'..'Z']);

    if HasDrive then
    begin
      FnDrive := UpCase(Fn[1]);
      _GetDirUtf8(Byte(FnDrive)-64, CurDir);
      CurDrive := UpCase(GetCurrentDirUtf8[1]);
    end
    else
    begin
      CurDir := GetCurrentDirUtf8;
      FnDrive := UpCase(CurDir[1]);
      CurDrive := FnDrive;
    end;

    //writeln('HasDrive = ',HasDrive,' Fn = ',Fn);
    //writeln('CurDir = ',CurDir);
    //writeln('CurDrive = ',CurDrive);
    //writeln('FnDrive  = ',FnDrive);

    if (Length(BaseDir) > 1) and (UpCase(BaseDir[1]) in ['A'..'Z']) and (BaseDir[2] = ':') then
    begin
      BaseDirDrive := BaseDir[1]
    end
    else
    begin
      if HasDrive then
        BaseDirDrive := CurDrive
      else
        BaseDirDrive := #0;
    end;

    //You cannot use BaseDir if both FileName and BaseDir includes a drive and they are not the same
    CanUseBaseDir := ((BaseDirDrive = #0) or
                     (not HasDrive) or
                     (HasDrive and (FnDrive = BaseDirDrive)))
                     and (BaseDir <> '');

    //writeln('CanUseBaseDir = ',CanUseBaseDir);

    if not HasDrive and StartsWithRoot and not CanUseBaseDir then
    begin
      //writeln('HasDrive and StartsWithRoot');
      Fn := Copy(CurDir,1,2) + Fn;
      HasDrive := True;
      IsAbs := True;
    end;
    //FileNames like C:foo, strip Driveletter + colon
    if HasDrive and not IsAbs then Delete(Fn,1,2);

    //writeln('HasDrive = ',Hasdrive,' Fn = ',Fn);
    {$else}
    CanUseBaseDir := True;
    {$endif WinCE}
  end;
  if IsAbs then
  begin
    //writeln('IsAbs = True -> Exit');
    Result := ExpandDots(Fn);
  end
  else
  begin
    if not CanUseBaseDir or (BaseDir = '') then
      Fn := IncludeTrailingPathDelimiter(CurDir) + Fn
    else
    begin
      if (Fn[1] = DirectorySeparator) then Delete(Fn,1,1);
      Fn := IncludeTrailingPathDelimiter(BaseDir) + Fn;
    end;

    Fn := ExpandDots(Fn);
    //if BaseDir is something like 'z:foo\' or '\' then this needs to be expanded as well
    if not FileNameIsAbsolute(Fn) then
      Fn := ExpandFileNameUtf8(Fn, '');
    Result := Fn;
  end;
end;


function GetCurrentDirUTF8: String;
begin
  Result:=_GetCurrentDirUtf8();
end;

procedure GetDirUtf8(DriveNr: Byte; var Dir: String);
begin
  _GetDirUtf8(DriveNr, Dir);
end;


{$ifndef WinCE}
//No ANSII functions on WinCE
function GetCurrentDirAnsi: String;
begin
  Result:=SysToUTF8(SysUtils.GetCurrentDir);
end;

procedure GetDirAnsi(DriveNr: Byte; var Dir: String);
begin
  GetDir(DriveNr, Dir);
  Dir := SysToUtf8(Dir);
end;
{$endif WinCE}

//WideString functions
function GetCurrentDirWide: String;
var
  w   : WideString;
  res : Integer;
begin
  {$ifdef WinCE}
  Result := '\';
  // Previously we sent an exception here, which is correct, but this causes
  // trouble with code which isnt tested for WinCE, so lets just send a dummy result instead
  // Exception.Create('[GetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  res:=GetCurrentDirectoryW(0, nil);
  SetLength(w, res);
  res:=Windows.GetCurrentDirectoryW(res, @w[1]);
  SetLength(w, res);
  Result:=UTF8Encode(w);
  {$endif}
end;

procedure GetDirWide(DriveNr: Byte; var Dir: String);
{This procedure may not be threadsafe, because SetCurrentDirectory isn't}
var
  w, D: WideString;
  SavedDir: WideString;
  res : Integer;
begin
  {$ifdef WinCE}
  Dir := '\';
  // Previously we sent an exception here, which is correct, but this causes
  // trouble with code which isnt tested for WinCE, so lets just send a dummy result instead
  // Exception.Create('[GetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  //writeln('GetDirWide START');
  if not (DriveNr = 0) then
  begin
    res := GetCurrentDirectoryW(0, nil);
    SetLength(SavedDir, res);
    res:=Windows.GetCurrentDirectoryW(res, @SavedDir[1]);
    SetLength(SavedDir,res);

    D := WideChar(64 + DriveNr) + ':';
    if not SetCurrentDirectoryW(@D[1]) then
    begin
      Dir := Char(64 + DriveNr) + ':\';
      SetCurrentDirectoryW(@SavedDir[1]);
      Exit;
    end;
  end;
  res := GetCurrentDirectoryW(0, nil);
  SetLength(w, res);
  res := GetCurrentDirectoryW(res, @w[1]);
  SetLength(w, res);
  Dir:=UTF8Encode(w);
  if not (DriveNr = 0) then SetCurrentDirectoryW(@SavedDir[1]);
  //writeln('GetDirWide END');
  {$endif}
end;





procedure InitLazFileUtils;
begin
  {$ifndef WinCE}
  if Win32MajorVersion <= 4 then
  begin
    //FileAge_ := @FileAgeAnsi;
    //FileSize_ := @FileSizeAnsi;
    //FileSetDate_ := @FileSetDateAnsi;
    //FileGetAttr_ := @FileGetAttrAnsi;
    //FileSetAttr_ := @FileSetAttrAnsi;
    //DeleteFile_ := @DeleteFileAnsi;
    //RenameFile_ := @RenameFileAnsi;
    //SetCurrentDir_ := @SetCurrentDirAnsi;
    _GetCurrentDirUtf8 := @GetCurrentDirAnsi;
    _GetDirUtf8 := @GetDirAnsi;
    //CreateDir_ := @CreateDirAnsi;
    //RemoveDir_ := @RemoveDirAnsi;
    //FindFirst_ := @FindFirstAnsi;
    //FindNext_ := @FindNextAnsi;
    //FindClose_ := @FindCloseAnsi;
  end
  else
  {$endif}
  begin
    //FileAge_ := @FileAgeWide;
    //FileSize_ := @FileSizeWide;
    //FileSetDate_ := @FileSetDateWide;
    //FileGetAttr_ := @FileGetAttrWide;
    //FileSetAttr_ := @FileSetAttrWide;
    //DeleteFile_ := @DeleteFileWide;
    //RenameFile_ := @RenameFileWide;
    //SetCurrentDir_ := @SetCurrentDirWide;
    _GetCurrentDirUtf8 :=@ GetCurrentDirWide;
    _GetDirUtf8 := @GetDirWide;
    //CreateDir_ := @CreateDirWide;
    //RemoveDir_ := @RemoveDirWide;
    //FindFirst_ := @FindFirstWide;
    //FindNext_ := @FindNextWide;
    //FindClose_ := @FindCloseWide;
  end;
end;
