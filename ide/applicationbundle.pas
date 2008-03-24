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
{$IFDEF UNIX}
  BaseUnix,
{$ENDIF}
  Classes, SysUtils, FileUtil;

type
  EApplicationBundleException = Exception;
  
  { TApplicationPropertyList }

  TApplicationPropertyList = class(TStringList)
  public
    constructor Create(const ExeName: String; Title: String = ''; const Version: String = '0.1');
  end;

procedure CreateApplicationBundle(const Filename: String; Title: String = '');
procedure CreateSymbolicLink(const Filename: String);
  
const
  ApplicationBundleExt = '.app';
  ContentsDirName = 'Contents';
  MacOSDirName = 'MacOS';
  ResourcesDirName = 'Resources';
  PropertyListFileName = 'Info.plist';
  PackageInfoFileName = 'PkgInfo';
  PackageInfoHeader = 'APPL????';
  
resourcestring
  rsCreatingDirFailed = 'Creating directory "%s" failed!';
  rsCreatingSymLinkFailed = 'Creating symbolic link "%s" failed!';
  rsCreatingSymLinkNotSupported = 'Creating symbolic link is not supported on this platform!';
  
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

procedure CreateDirectoryInteractive(const Directory: String);
begin
  if not CreateDir(Directory) then
    EApplicationBundleException.CreateFmt(rsCreatingDirFailed, [Directory]);
end;

procedure CreateApplicationBundle(const Filename: String; Title: String);
var
  AppBundleDir: String;
  ContentsDir: String;
  MacOSDir: String;
  ResourcesDir: String;
  
  procedure CreatePackageInfoFile(const Path: String);
  var
    S: TStringList;
  begin
    S := TStringList.Create;
    try
      S.Add(PackageInfoHeader);
      S.SaveToFile(Path + PackageInfoFileName);
    finally
      S.Free;
    end;
  end;
  
begin
  AppBundleDir := ExtractFileNameWithoutExt(Filename) + ApplicationBundleExt + PathDelim;
  // create 'applicationname.app/' directory
  CreateDirectoryInteractive(AppBundleDir);
  begin
    // create 'applicationname.app/Contents/' directory
    ContentsDir := AppBundleDir + ContentsDirName + PathDelim;
    CreateDirectoryInteractive(ContentsDir);
    begin
      // create 'applicationname.app/Contents/MacOS/' directory
      MacOSDir := ContentsDir + MacOSDirName + PathDelim;
      CreateDirectoryInteractive(MacOSDir);

      // create Info.plist file
      with TApplicationPropertyList.Create(ExtractFileNameOnly(Filename), Title) do
        SaveToFile(ContentsDir + PropertyListFileName);

      // create PkgInfo file
      CreatePackageInfoFile(ContentsDir);

      // create 'applicationname.app/Contents/Resources/' directory
      ResourcesDir:=ContentsDir + ResourcesDirName + PathDelim;
      CreateDirectoryInteractive(ResourcesDir);
    end;
  end;
end;

procedure CreateSymbolicLink(const Filename: String);
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
  if FPSymLink(PChar('..' + PathDelim + '..' + PathDelim + '..' + PathDelim + ShortExeName),
    PChar(LinkFilename)) <> 0 then
    raise EApplicationBundleException.CreateFmt(rsCreatingSymLinkFailed, [LinkFilename]);
  {$ELSE}
    raise EApplicationBundleException.Create(rsCreatingSymLinkNotSupported);
  {$ENDIF}
end;



end.
