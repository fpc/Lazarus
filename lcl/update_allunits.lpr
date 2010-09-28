{ Utility to update every alllclintfunits.pas unit of every LCL widget set.
}
program update_allunits;

{$mode objfpc}{$H+}

uses
  // Note: do not use any LCL unit!
  Classes, SysUtils;

procedure FindUnits(SrcDir: string; Units: TStrings);
var
  FileInfo: TSearchRec;
  Ext: String;
  CurUnitName: String;
begin
  if FindFirst(SrcDir+'*.*',faAnyFile,FileInfo)=0 then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      if (FileInfo.Attr and faDirectory)=0 then begin
        Ext:=LowerCase(ExtractFileExt(FileInfo.Name));
        if (Ext='.pp') or (Ext='.pas') or (Ext='.p') then
        begin
          CurUnitName:=copy(FileInfo.Name,1,length(FileInfo.Name)-length(Ext));
          if CompareText(CurUnitName,'AllLCLIntfUnits')=0 then continue;
          Units.Add(CurUnitName);
        end;
      end;
    until FindNext(FileInfo)<>0;
  end;
  FindClose(FileInfo);
end;

procedure Run(WidgetSet, UnitPaths: string);
var
  Units: TStringList;
  Paths: TStringList;
  BaseDir, Path: String;
  i: Integer;
  Source: TStringList;
  Filename: String;
  s: string;
begin
  Units:=TStringList.Create;
  Paths:=TStringList.Create;
  Source:=TStringList.Create;
  try
    BaseDir:='interfaces/'+WidgetSet+'/';
    FindUnits(BaseDir,Units);
    Paths.Delimiter:=';';
    Paths.StrictDelimiter:=true;
    Paths.DelimitedText:=UnitPaths;
    for i:=0 to Paths.Count-1 do begin
      Path:=Trim(Paths[i]);
      if Path='' then continue;
      FindUnits(BaseDir+Path,Units);
    end;
    Source.Add('{ This unit was automatically created by update_allunits }');
    Source.Add('unit AllLCLIntfUnits;');
    Source.Add('interface');
    Source.Add('uses');
    for i:=0 to Units.Count-1 do begin
      s:='  '+Units[i];
      if i=Units.Count-1 then
        s:=s+';'
      else
        s:=s+',';
      Source.Add(s);
    end;
    Source.Add('implementation');
    Source.Add('end.');
    Filename:=BaseDir+'alllclintfunits.pas';
    writeln('writing ',Filename,' ...');
    Source.SaveToFile(Filename);
  finally
    Source.Free;
    Paths.Free;
    Units.Free;
  end;
end;

begin
  Run('gtk','');
  Run('gtk2','');
  Run('qt','');
  Run('win32','');
  Run('nogui','');
  Run('cocoa','');
  Run('carbon','objc;pascocoa/appkit;pascocoa/foundation');
  // fpgui needs manual additions for corelib/x11 and corelib/gdi
  Run('fpgui','gui;corelib');
end.

