{******************************************************************************
                                  Filectrl
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
 
{------------------------------------------------------------------------------
  DirectoryExists
 ------------------------------------------------------------------------------}
function DirectoryExists(const Name: String): Boolean;
var
  F: Longint;
  dirExist: Boolean;
begin
  //Result := FileExist(Name);
  dirExist := false; 

  F := FileGetAttr(Name);
  if F <> -1 then
    if (F and faDirectory) <> 0 then
      dirExist := true;
  Result := dirExist;
end;

{------------------------------------------------------------------------------
  FileIsWritable
 ------------------------------------------------------------------------------}
function FileIsWritable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=((FileGetAttr(AFilename) and faReadOnly)>0);
  {$ELSE}
  Result:={$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.W_OK);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  GetFileDescription
 ------------------------------------------------------------------------------}
function GetFileDescription(const AFilename: string): string;
{$IFDEF win32}

{$ELSE}
var
  info: stat;
  // permissions
  // user
  // group
  // size
  // date
  // time
{$ENDIF}
begin
  Result:='';
  {$IFDEF win32}

  {$ELSE}
  if not FStat(AFilename,info) then exit;
  
  // permissions
  // file type
  if STAT_IFLNK and info.mode=STAT_IFLNK then
    Result:=Result+'l'
  else
  if STAT_IFDIR and info.mode=STAT_IFDIR then
    Result:=Result+'d'
  else
  if STAT_IFBLK and info.mode=STAT_IFBLK then
    Result:=Result+'b'
  else
  if STAT_IFCHR and info.mode=STAT_IFCHR then
    Result:=Result+'c'
  else
    Result:=Result+'-';
  // other permissions
  if STAT_IROTH and info.mode=STAT_IROTH then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWOTH and info.mode=STAT_IWOTH then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXOTH and info.mode=STAT_IXOTH then
    Result:=Result+'x'
  else
    Result:=Result+'-';
  // group permissions
  if STAT_IRGRP and info.mode=STAT_IRGRP then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWGRP and info.mode=STAT_IWGRP then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXGRP and info.mode=STAT_IXGRP then
    Result:=Result+'x'
  else
    Result:=Result+'-';
  // user permissions
  if STAT_IRUSR and info.mode=STAT_IRUsr then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWUsr and info.mode=STAT_IWUsr then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXUsr and info.mode=STAT_IXUsr then
    Result:=Result+'x'
  else
    Result:=Result+'-';
    

  // user name
  //Result:=Result+' Owner: '+IntToStr(info.uid)+'.'+IntToStr(info.gid);

  // size
  Result:=Result+'  size '+IntToStr(info.size);
  
  {$ENDIF}
  // date + time
  Result:=Result+'  modified '+FormatDateTime('DD/MM/YYYY hh:mm',
                         FileDateToDateTime(FileAge(AFilename)));
end;


{
  $Log$
  Revision 1.3  2002/05/29 21:44:38  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.2  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/24 05:02:43  lazarus
  Added filectrl unit for DirectoryExists function.     CAW


}

