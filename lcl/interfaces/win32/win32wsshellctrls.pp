{
 *****************************************************************************
 *                              WSShellCtrls.pp                              * 
 *                              -------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit win32wsshellctrls;

{$mode objfpc}{$H+}
{$I win32defines.inc}

interface

uses
  SysUtils, Classes, ComCtrls, ShellCtrls, Types,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Graphics, ImgList, Controls, ShellCtrls,
////////////////////////////////////////////////////
  WSShellCtrls;

type

  { TWin32WSCustomShellTreeView }

  TWin32WSCustomShellTreeView = class(TWSCustomShellTreeView)
  published
    class function DrawBuiltInIcon(ATreeView: TCustomShellTreeView; ANode: TTreeNode;
      ARect: TRect): TSize; override;
    class function GetBuiltinIconSize: TSize; override;
  end;

  { TWin32WSCustomShellListView }
  TWin32WSCustomShellListView = class(TWSCustomShellListView)
  published
    class function GetBuiltInImageIndex(AListView: TCustomShellListView;
      const AFileName: String; ALargeImage: Boolean): Integer; override;
  end;

implementation

uses
  windows, shellapi, graphics;

var
  ShellIconSize: TSize = (CX: -1; CY: -1);

function GetShellIcon(const AFileName: WideString): TIcon;
var
  FileInfo: TSHFileInfoW;
  imgHandle: DWORD_PTR;
begin
  imgHandle := SHGetFileInfoW(PWideChar(AFileName), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
  if imgHandle <> 0 then
  begin
    Result := TIcon.Create;
    Result.Handle := FileInfo.hIcon;
  end else
    Result := nil;
end;


{ TWin32WSCustomShellTreeView }

class function TWin32WSCustomShellTreeView.DrawBuiltInIcon(ATreeView: TCustomShellTreeView;
  ANode: TTreeNode; ARect: TRect): TSize;
var
  filename: WideString;
  ico: TIcon;
begin
  fileName := ATreeView.GetPathFromNode(ANode);
  ico := GetShellIcon(fileName);
  try
    ATreeView.Canvas.Draw(ARect.Left, (ARect.Top + ARect.Bottom - ico.Height) div 2, ico);
    Result := Types.Size(ico.Width, ico.Height);
  finally
    ico.Free;
  end;
end;

class function TWin32WSCustomShellTreeView.GetBuiltinIconSize: TSize;
var
  ico: TIcon;
begin
  if (ShellIconSize.CX = -1) and (ShellIconSize.CY = -1) then
  begin
    ico := GetShellIcon(WideString('C:'));
    try
      Result := Types.Size(ico.Width, ico.Height);
      ShellIconSize := Result;
    finally
      ico.Free;
    end;
  end else
    Result := ShellIconSize;
end;


{ TWin32WSCustomShellListView }

class function TWin32WSCustomShellListView.GetBuiltInImageIndex(
  AListView: TCustomShellListView; const AFileName: String;
  ALargeImage: Boolean): Integer;
var
  fullName: WideString;
  info: TSHFILEINFOW;
  sysImageHandle: DWORD_PTR;
  listHandle: HWND;
  flags: DWord;
  lvsil: LongInt;
  attr: LongInt;
begin
  Result := -1;
  fullName := WideString(AFileName);
  attr := FileGetAttr(fullName);
  if ALargeImage then begin
    flags := SHGFI_LARGEICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES;
    lvsil := LVSIL_NORMAL;
  end else
  begin
    flags := SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES;
    lvsil := LVSIL_SMALL;
  end;
  sysImageHandle := SHGetFileInfoW(PWideChar(fullName), attr, info, SizeOf(info), flags);
  if sysImageHandle = 0 then
    Exit;
  listHandle := AListView.Handle;
  if ListView_GetImageList(ListHandle, lvsil) = 0 then
  begin
    SetWindowLong(listHandle, GWL_STYLE,
      GetWindowLong(listHandle, GWL_STYLE) or LVS_SHAREIMAGELISTS);
    ListView_SetImageList(listHandle, sysImageHandle, lvsil);
  end;
  Result := info.iIcon;
end;

end.
