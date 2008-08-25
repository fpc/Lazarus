{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner
}
unit MacApplicationRes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;
  
type
  EMacResourceException = Exception;
  
procedure CreateMacOSXApplicationResources(const Filename,
  ReadmeTitle, ReadmeDescription: string);
function GetMacOSXExecutableFilename(const BaseDir, ShortExeName: string): string;

procedure CreateDirectoryInteractive(const Directory: string);

implementation

procedure CreateMacOSXApplicationResources(const Filename,
  ReadmeTitle, ReadmeDescription: string);
{
  Filename.app/
    Contents/
      MacOS/
        Executable
      Resources/
        README.rtf
      PkgInfo
      Info.plist
}

  procedure WriteInfoPlistFile(const Directory: string);
  var
    sl: TStringList;
    ExeName: String;
    PLInfoListFilename: String;
  begin
    ExeName:=ExtractFileName(Filename);
    PLInfoListFilename:=Directory+'Info.plist';
    sl:=TStringList.Create;
    try
      sl.Add('<?xml version="1.0" encoding="UTF-8"?>');
      sl.Add('<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">');
      sl.Add('<plist version="1.0">');
      sl.Add('<dict>');
      sl.Add('  <key>CFBundleDevelopmentRegion</key>');
      sl.Add('  <string>English</string>');
      sl.Add('  <key>CFBundleExecutable</key>');
      sl.Add('  <string>'+ExeName+'</string>');
      sl.Add('  <key>CFBundleInfoDictionaryVersion</key>');
      sl.Add('  <string>6.0</string>');
      sl.Add('  <key>CFBundlePackageType</key>');
      sl.Add('  <string>APPL</string>');
      sl.Add('  <key>CFBundleSignature</key>');
      sl.Add('  <string>????</string>');
      sl.Add('  <key>CFBundleVersion</key>');
      sl.Add('  <string>1.0</string>');
      sl.Add('  <key>CSResourcesFileMapped</key>');
      sl.Add('  <true/>');
      // for accepting files dropped on the dock icon:
      sl.Add('  <key>CFBundleDocumentTypes</key>');
      sl.Add('  <array>');
      sl.Add('    <dict>');
      sl.Add('      <key>CFBundleTypeRole</key>');
      sl.Add('      <string>Viewer</string>');
      sl.Add('      <key>CFBundleTypeExtensions</key>');
      sl.Add('      <array>');
      sl.Add('        <string>*</string>');
      sl.Add('      </array>');
      sl.Add('      <key>CFBundleTypeOSTypes</key>');
      sl.Add('      <array>');
      sl.Add('        <string>fold</string>');
      sl.Add('        <string>disk</string>');
      sl.Add('        <string>****</string>');
      sl.Add('      </array>');
      sl.Add('    </dict>');
      sl.Add('  </array>');
      sl.Add('</dict>');
      sl.Add('</plist>');
      sl.SaveToFile(UTF8ToSys(PLInfoListFilename));
    finally
      sl.Free;
    end;
  end;

  procedure WritePkgInfoFile(const Directory: string);
  var
    sl: TStringList;
    PkgInfoFilename: String;
  begin
    PkgInfoFilename:=Directory+'PkgInfo';
    sl:=TStringList.Create;
    try
      sl.Add('APPL????');
      sl.SaveToFile(UTF8ToSys(PkgInfoFilename));
    finally
      sl.Free;
    end;
  end;

  procedure WriteREADMErtfFile(const Directory, Title, Description: string);
  var
    sl: TStringList;
    ReadmeFilename: String;
  begin
    ReadmeFilename:=Directory+'README.rtf';
    sl:=TStringList.Create;
    try
      sl.Add('{\rtf1\mac\ansicpg10000\cocoartf102');
      sl.Add('{\fonttbl\f0\fswiss\fcharset77 Helvetica-Bold;\f1\fswiss\fcharset77 Helvetica;}');
      sl.Add('{\colortbl;\red255\green255\blue255;}');
      sl.Add('\margl1440\margr1440\vieww9000\viewh9000\viewkind0');
      sl.Add('\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\ql\qnatural');
      sl.Add('');
      sl.Add('\f0\b\fs24 \cf0 '+Title);
      sl.Add('\f1\b0 \');
      sl.Add('\');
      sl.Add(Description);
      sl.Add('\');
      sl.Add('');
      sl.Add('}');
      sl.SaveToFile(UTF8ToSys(ReadmeFilename));
    finally
      sl.Free;
    end;
  end;

var
  AppDir: String;
  ContentsDir: String;
  MacOSDir: String;
  ResourcesDir: String;
begin
  // create 'applicationname.app/' directory
  AppDir:=Filename+'.app'+PathDelim;
  CreateDirectoryInteractive(AppDir);
  begin
    // create 'applicationname.app/Contents/' directory
    ContentsDir:=AppDir+'Contents'+PathDelim;
    CreateDirectoryInteractive(ContentsDir);
    begin
      // create 'applicationname.app/Contents/MacOS/' directory
      MacOSDir:=ContentsDir+'MacOS'+PathDelim;
      CreateDirectoryInteractive(MacOSDir);

      // create Info.plist file
      WriteInfoPlistFile(ContentsDir);

      // create PkgInfo file
      WritePkgInfoFile(ContentsDir);

      // create 'applicationname.app/Contents/Resources/' directory
      ResourcesDir:=ContentsDir+'Resources'+PathDelim;
      CreateDirectoryInteractive(ResourcesDir);
      
      // create README.rtf file
      WriteREADMErtfFile(ResourcesDir,ReadmeTitle,ReadmeDescription);
    end;
  end;
end;

function GetMacOSXExecutableFilename(const BaseDir, ShortExeName: string
  ): string;
begin
  Result:=AppendPathDelim(BaseDir)+ShortExeName+'.app'+PathDelim
         +'Contents'+PathDelim+'MacOS'+ShortExeName;
end;

procedure CreateDirectoryInteractive(const Directory: string);
begin
  if not CreateDirUTF8(Directory) then
    raise EMacResourceException.Create('creating directory '+Directory+' failed');
end;

end.
