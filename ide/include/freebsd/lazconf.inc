{***************************************************************************
                               lazconf.inc
 ***************************************************************************}

{Should become unit Unix in the future. For both Linux and FreeBSD}
uses
  linux;
  
var
  PrimaryConfigPath,
  SecondaryConfigPath: string;
  
{---------------------------------------------------------------------------
  getPrimaryConfigPath function
 ---------------------------------------------------------------------------}
function GetPrimaryConfigPath: String;
begin
  Result := PrimaryConfigPath;
end;

{---------------------------------------------------------------------------
  getSecondaryConfigPath function
 ---------------------------------------------------------------------------}
function GetSecondaryConfigPath: String;
begin
  Result := SecondaryConfigPath;
end;

{---------------------------------------------------------------------------
  createPrimaryConfigPath function
 ---------------------------------------------------------------------------}
procedure CreatePrimaryConfigPath;
begin
  CreateDir(GetPrimaryConfigPath);
  //MkDir(GetPrimaryConfigPath);
end;

{---------------------------------------------------------------------------
  SetPrimaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetPrimaryConfigPath(const NewValue: String);
begin
  PrimaryConfigPath:=FExpand(NewValue);
end;

{---------------------------------------------------------------------------
  SetSecondaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetSecondaryConfigPath(const NewValue: String);
begin
  SecondaryConfigPath:=FExpand(NewValue);
end;

{---------------------------------------------------------------------------
  CopySecondaryConfigFile procedure
 ---------------------------------------------------------------------------}
procedure CopySecondaryConfigFile(const AFilename: String);
var
  PrimaryFilename, SecondaryFilename: string;
  SrcFS, DestFS: TFileStream;
begin
  PrimaryFilename:=GetPrimaryConfigPath+OSDirSeparator+AFilename;
  SecondaryFilename:=GetSecondaryConfigPath+OSDirSeparator+AFilename;
  if not FileExists(PrimaryFilename) then begin
    try
      SrcFS:=TFileStream.Create(SecondaryFilename,fmOpenRead);
      try
        DestFS:=TFileStream.Create(PrimaryFilename,fmCreate);
        try
          DestFS.CopyFrom(SrcFS,SrcFS.Size);
        finally
          DestFS.Free;
        end;
      finally
        SrcFS.Free;
      end;
    except
    end;
  end;
end;

function FindDefaultCompilerPath: string;
begin
  if FileIsExecutable('/usr/bin/ppc386') then
    Result:='/usr/bin/ppc386'
  else if FileIsExecutable('/opt/fpc/ppc386') then
    Result:='/opt/fpc/ppc386'
  else
    Result:='';
end;


initialization
  PrimaryConfigPath:=FExpand('~/.lazarus');
  SecondaryConfigPath:='/etc/lazarus';


{
  $Log$
  Revision 1.3  2001/12/10 08:44:23  lazarus
  MG: added search for compiler, if not set

  Revision 1.2  2001/05/27 11:52:01  lazarus
  MG: added --primary-config-path=<filename> cmd line option

  Revision 1.1  2001/01/14 20:50:23  marco
   * Initial version. Same as Linux, and probably will be that way.

}

