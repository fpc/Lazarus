
{Should become unit Unix in the future. For both Linux and FreeBSD}
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
  Revision 1.1  2001/01/14 20:50:23  marco
   * Initial version. Same as Linux, and probably will be that way.

}

