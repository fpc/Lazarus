{

}
unit IDEProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  // ToDo: find the constant in the fpc units.
  EndOfLine:shortstring={$IFDEF win32}#13+{$ENDIF}#10;

function FilenameIsAbsolute(TheFilename: string):boolean;
function DirectoryExists(DirectoryName: string): boolean;
function ForceDirectory(DirectoryName: string): boolean;

implementation

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
    if DirectoryName[i]=OSDirSeparator then begin
      Dir:=copy(DirectoryName,1,i-1);
      if not DirectoryExists(Dir) then begin
        Result:=CreateDir(Dir);
        if not Result then exit;
      end;
    end;
  end;
  Result:=true;
end;

end.
