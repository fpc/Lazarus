{***************************************************************************
                               lazconf.inc
 ***************************************************************************}
{---------------------------------------------------------------------------
  getPrimaryConfigPath function
 ---------------------------------------------------------------------------}
function getPrimaryConfigPath: String;
begin
  { Return the primary config directory  }
  Result := ExtractFilePath(Paramstr(0));
end;

{---------------------------------------------------------------------------
  getSecondaryConfigPath function
 ---------------------------------------------------------------------------}
function getSecondaryConfigPath: String;
begin
  { Return the secondary config directory. This one is checked
    if the primary does not exist }

  { This function might need some work because on WinNT the
    the path is called something different on almost every
    version of Windows. Probably can use the Windows API to
    get the system path. }
  Result := '\windows\system';
end;

{---------------------------------------------------------------------------
  createPrimaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure createPrimaryConfigPath;
begin
  { This function does not need to do anything on Windows
    because the primary config path already exists. It is
    the path that Lazarus is installed to. }
end;

{
  $Log$
  Revision 1.1  2000/07/13 10:28:22  michael
  + Initial import

  Revision 1.1  2000/04/21 05:12:42  lazarus
  Added lazconf.pp file.
  Added include/linux and include/win32 directories.
  Added lazconf.inc files for both linux and win32.


}

