{ /***************************************************************************
                   ApplicationBundle.pas  -  Lazarus IDE unit
                   ------------------------------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Abstract:
    Application Bundle utilities

}
unit ApplicationBundle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs,
  DialogProcs;

type
  EApplicationBundleException = Exception;
  
  { TApplicationPropertyList }

  TApplicationPropertyList = class(TStringList)
  public
    constructor Create(const ExeName: String; Title: String = ''; const Version: String = '0.1');
  end;

function CreateApplicationBundle(const Filename: String; Title: String = ''; Recreate: boolean = false): TModalResult;
function CreateAppBundleSymbolicLink(const Filename: String; Recreate: boolean = false): TModalResult;
  
const
  ApplicationBundleExt = '.app';
  ContentsDirName = 'Contents';
  MacOSDirName = 'MacOS';
  ResourcesDirName = 'Resources';
  PropertyListFileName = 'Info.plist';
  PackageInfoFileName = 'PkgInfo';
  PackageInfoHeader = 'APPL????';
  
implementation

{ TApplicationPropertyList }

constructor TApplicationPropertyList.Create(const ExeName: String; Title: String; const Version: String = '0.1');
begin
  inherited Create;
  
  if Title = '' then Title := ExeName;
  
  Add('<?xml version="1.0" encoding="UTF-8"?>');
  Add('<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">');
  Add('<plist version="1.0">');
  Add('<dict>');
  Add('  <key>CFBundleDevelopmentRegion</key>');
  Add('  <string>English</string>');
  Add('  <key>CFBundleExecutable</key>');
  Add('  <string>' + ExeName + '</string>');
  Add('  <key>CFBundleName</key>');
  Add('  <string>' + Title + '</string>');
  Add('  <key>CFBundleIdentifier</key>');
  Add('  <string>com.company.' + ExeName + '</string>');
  Add('  <key>CFBundleInfoDictionaryVersion</key>');
  Add('  <string>6.0</string>');
  Add('  <key>CFBundlePackageType</key>');
  Add('  <string>APPL</string>');
  Add('  <key>CFBundleSignature</key>');
  Add('  <string>' + Copy(ExeName + '????', 1, 4) + '</string>');
  Add('  <key>CFBundleShortVersionString</key>');
  Add('  <string>' + Version + '</string>');
  Add('  <key>CFBundleVersion</key>');
  Add('  <string>1</string>');
  Add('  <key>CSResourcesFileMapped</key>');
  Add('  <true/>');
  // for accepting files dropped on the dock icon:
  Add('  <key>CFBundleDocumentTypes</key>');
  Add('  <array>');
  Add('    <dict>');
  Add('      <key>CFBundleTypeRole</key>');
  Add('      <string>Viewer</string>');
  Add('      <key>CFBundleTypeExtensions</key>');
  Add('      <array>');
  Add('        <string>*</string>');
  Add('      </array>');
  Add('      <key>CFBundleTypeOSTypes</key>');
  Add('      <array>');
  Add('        <string>fold</string>');
  Add('        <string>disk</string>');
  Add('        <string>****</string>');
  Add('      </array>');
  Add('    </dict>');
  Add('  </array>');
  Add('</dict>');
  Add('</plist>');
end;

function CreateApplicationBundle(const Filename: String; Title: String;
  Recreate: boolean): TModalResult;
var
  AppBundleDir: String;
  ContentsDir: String;
  MacOSDir: String;
  ResourcesDir: String;
  sl: TStrings;
begin
  AppBundleDir := ExtractFileNameWithoutExt(Filename) + ApplicationBundleExt + PathDelim;
  if not Recreate and DirectoryExistsUTF8(AppBundleDir) then exit(mrOk);

  // create 'applicationname.app/Contents/MacOS/' directory
  ContentsDir := AppBundleDir + ContentsDirName + PathDelim;
  MacOSDir := ContentsDir + MacOSDirName + PathDelim;
  Result:=ForceDirectoryInteractive(MacOSDir,[mbIgnore,mbRetry]);
  if Result<>mrOk then exit;

  // create Info.plist file
  sl:=TApplicationPropertyList.Create(ExtractFileNameOnly(Filename), Title);
  Result:=SaveStringListToFile(ContentsDir + PropertyListFileName,'Info.plist part of Application bundle',sl);
  sl.Free;
  if Result<>mrOk then exit;

  // create PkgInfo file
  sl:=TStringList.Create;
  sl.Add(PackageInfoHeader);
  Result:=SaveStringListToFile(ContentsDir+PackageInfoFileName,'PkgInfo part of Application bundle',sl);
  sl.Free;
  if Result<>mrOk then exit;

  // create 'applicationname.app/Contents/Resources/' directory
  ResourcesDir:=ContentsDir + ResourcesDirName + PathDelim;
  Result:=ForceDirectoryInteractive(ResourcesDir,[mbIgnore,mbRetry]);
  if Result<>mrOk then exit;

  Result:=mrOk;
end;

function CreateAppBundleSymbolicLink(const Filename: String;
  Recreate: boolean): TModalResult;
{$IFDEF UNIX}
var
  ShortExeName: String;
  LinkFilename: String;
{$ENDIF}
begin
  {$IFDEF UNIX}
  ShortExeName := ExtractFileNameOnly(Filename);
  LinkFilename := ExtractFileNameWithoutExt(Filename) + ApplicationBundleExt + PathDelim +
    ContentsDirName + PathDelim + MacOSDirName + PathDelim + ShortExeName;
  if (not Recreate) and (FileExistsUTF8(LinkFilename)) then exit(mrOk);
  Result:=CreateSymlinkInteractive(LinkFilename,'..' + PathDelim + '..' + PathDelim + '..' + PathDelim + ShortExeName,[mbIgnore,mbRetry]);
  {$ELSE}
  Result:=mrIgnore;
  {$ENDIF}
end;

end.
