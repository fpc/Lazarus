uses
  linux;

{***************************************************************************
                               lazconf.inc
 ***************************************************************************}
{---------------------------------------------------------------------------
  getPrimaryConfigPath function
 ---------------------------------------------------------------------------}
function GetPrimaryConfigPath: String;
begin
  { Return the primary config directory  }
  Result := FExpand('~/.lazarus');
end;

{---------------------------------------------------------------------------
  getSecondaryConfigPath function
 ---------------------------------------------------------------------------}
function GetSecondaryConfigPath: String;
begin
  { Return the secondary config directory. This one is checked
    if the primary does not exist }
  Result := '/etc';
end;

{---------------------------------------------------------------------------
  createPrimaryConfigPath function
 ---------------------------------------------------------------------------}
procedure CreatePrimaryConfigPath;
begin
  { Create the primary directory }
  CreateDir(GetPrimaryConfigPath);
  //MkDir(GetPrimaryConfigPath);
end;

{
  $Log$
  Revision 1.1  2000/07/13 10:28:22  michael
  + Initial import

  Revision 1.2  2000/04/24 05:06:28  lazarus
  Added code to expand Linux filename when ~ is used.
  Changed CreatePrimaryConfigPath function to use CreateDir function.   CAW

  Revision 1.1  2000/04/21 05:12:42  lazarus
  Added lazconf.pp file.
  Added include/linux and include/win32 directories.
  Added lazconf.inc files for both linux and win32.


}

