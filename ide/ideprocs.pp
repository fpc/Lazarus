{
  Simple functions for file access, not yet in fpc.

}
unit IDEProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLCfg;

//
const
  // ToDo: find the constant in the fpc units.
  EndOfLine:shortstring={$IFDEF win32}#13+{$ENDIF}#10;

// files
function FilenameIsAbsolute(TheFilename: string):boolean;
function DirectoryExists(DirectoryName: string): boolean;
function ForceDirectory(DirectoryName: string): boolean;
function ExtractFileNameOnly(const AFilename: string): string;
procedure CheckIfFileIsExecutable(const AFilename: string);
function FileIsExecutable(const AFilename: string): boolean;
function FileIsReadable(const AFilename: string): boolean;
function FileIsWritable(const AFilename: string): boolean;
function FileIsText(const AFilename: string): boolean;
function CompareFilenames(const Filename1, Filename2: string): integer;

// XMLConfig
procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStringList; 
  const Path: string);
procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStringList; 
  const Path: string);
procedure LoadRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);
procedure SaveRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);

// various
procedure AddToRecentList(const s: string; RecentList: TStringList;
  Max: integer);


implementation

// to get more detailed error messages consider the os
 {$IFDEF Linux}
uses
 {$IFDEF Ver1_0}
  Linux
 {$ELSE}
  Unix
 {$ENDIF}
 ;
 {$ENDIF}

procedure AddToRecentList(const s: string; RecentList: TStringList;
  Max: integer);
var i: integer;
begin
  i:=RecentList.Count-1;
  while i>=0 do begin
    if RecentList[i]=s then RecentList.Delete(i);
    dec(i);
  end;
  RecentList.Insert(0,s);
  if Max>0 then
    while RecentList.Count>Max do
      RecentList.Delete(RecentList.Count-1);
end;

procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStringList; 
  const Path: string);
var i: integer;
begin
  XMLConfig.SetValue(Path+'Count',List.Count);
  for i:=0 to List.Count-1 do
    XMLConfig.SetValue(Path+'Item'+IntToStr(i+1)+'/Value',List[i]);
end;

procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStringList; 
  const Path: string);
var i,Count: integer;
  s: string;
begin
  Count:=XMLConfig.GetValue(Path+'Count',0);
  List.Clear;
  for i:=1 to Count do begin
    s:=XMLConfig.GetValue(Path+'Item'+IntToStr(i)+'/Value','');
    if s<>'' then List.Add(s);
  end;
end;

procedure LoadRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);
begin
  ARect.Left:=XMLConfig.GetValue(Path+'Left',ARect.Left);
  ARect.Top:=XMLConfig.GetValue(Path+'Top',ARect.Top);
  ARect.Right:=XMLConfig.GetValue(Path+'Right',ARect.Right);
  ARect.Bottom:=XMLConfig.GetValue(Path+'Bottom',ARect.Bottom);
end;

procedure SaveRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);
begin
  XMLConfig.SetValue(Path+'Left',ARect.Left);
  XMLConfig.SetValue(Path+'Top',ARect.Top);
  XMLConfig.SetValue(Path+'Right',ARect.Right);
  XMLConfig.SetValue(Path+'Bottom',ARect.Bottom);
end;

function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  {$IFDEF WIN32}
  Result:=AnsiCompareText(Filename1, Filename2);
  {$ELSE}
  Result:=AnsiCompareStr(Filename1, Filename2);
  {$ENDIF}
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  try
    CheckIfFileIsExecutable(AFilename);
    Result:=true;
  except
    Result:=false;
  end;
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
var AText: string;
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExists(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" does not exist');
  end;
  {$IFDEF linux}
  if not{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.X_OK) then
  begin
    AText:='"'+AFilename+'"';
    case LinuxError of
    sys_eacces: AText:='execute access denied for '+AText;
    sys_enoent: AText:='a directory component in '+AText
                          +' does not exist or is a dangling symlink';
    sys_enotdir: AText:='a directory component in '+Atext+' is not a directory';
    sys_enomem: AText:='insufficient memory';
    sys_eloop: AText:=AText+' has a circular symbolic link';
    else
      AText:=AText+' is not executable';
    end;
    raise Exception.Create(AText);
  end;
  {$ENDIF linux}
  
  // ToDo: windows and xxxbsd
end;

function ExtractFileNameOnly(const AFilename: string): string;
var ExtLen: integer;
begin
  Result:=ExtractFilename(AFilename);
  ExtLen:=length(ExtractFileExt(Result));
  Result:=copy(Result,1,length(Result)-ExtLen);
end;

function FilenameIsAbsolute(TheFilename: string):boolean;
begin
  DoDirSeparators(TheFilename);
  {$IFDEF win32}
  // windows
  Result:=(copy(TheFilename,1,2)='\\') or ((length(TheFilename)>3) and 
     (upcase(TheFilename[1]) in ['A'..'Z']) and (copy(TheFilename,2,2)=':\'));
  {$ELSE}
  Result:=(TheFilename<>'') and (TheFilename[1]='/');
  {$ENDIF}
end;

function DirectoryExists(DirectoryName: string): boolean;
var sr: TSearchRec;
begin
  if (DirectoryName<>'')
  and (DirectoryName[length(DirectoryName)]=PathDelim) then
    DirectoryName:=copy(DirectoryName,1,length(DirectoryName)-1);
  if FindFirst(DirectoryName,faAnyFile,sr)=0 then
    Result:=((sr.Attr and faDirectory)>0)
  else
    Result:=false;
  FindClose(sr);
end;

function ForceDirectory(DirectoryName: string): boolean;
var i: integer;
  Dir: string;
begin
  DoDirSeparators(DirectoryName);
  i:=1;
  while i<=length(DirectoryName) do begin
    if DirectoryName[i]=PathDelim then begin
      Dir:=copy(DirectoryName,1,i-1);
      if not DirectoryExists(Dir) then begin
        Result:=CreateDir(Dir);
        if not Result then exit;
      end;
    end;
  end;
  Result:=true;
end;

function FileIsReadable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=true;
  {$ELSE}
  Result:={$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.R_OK);
  {$ENDIF}
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=((FileGetAttr(Filename) and faReadOnly)>0);
  {$ELSE}
  Result:={$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.W_OK);
  {$ENDIF}
end;

function FileIsText(const AFilename: string): boolean;
var fs: TFileStream;
  Buf: string;
  Len, i: integer;
  NewLine: boolean;
begin
  Result:=false;
  try
    fs:=TFileStream.Create(AFilename,fmOpenRead);
    try
      // read the first 1024 bytes
      Len:=1024;
      if Len>fs.Size then Len:=fs.Size;
      if Len>0 then begin
        SetLength(Buf,Len);
        fs.Read(Buf[1],length(Buf));
        NewLine:=false;
        for i:=1 to length(Buf) do begin
          case Buf[i] of
          #0..#8,#11..#12,#14..#31: exit;
          #10,#13: NewLine:=true;
          end;
        end;
        if NewLine or (Len<1024) then
          Result:=true;
      end else
        Result:=true;
    finally
      fs.Free;
    end;
  except
  end;
end;

end.
