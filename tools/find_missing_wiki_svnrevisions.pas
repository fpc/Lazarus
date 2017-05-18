{ Finds missing svn revisions in wiki list format,
  used for updating the wiki pages fixes branch.

  Usage:
    ./findmissingsvnrevisionwiki -h
    ./findmissingsvnrevisionwiki -s <svnlogfile> -w <wiki-list-file>

  Copyright (C) 2017 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
program find_missing_wiki_svnrevisions;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, LazFileUtils;

type

  { TMissingWikiSVNRevisions }

  TMissingWikiSVNRevisions = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure ParamError(Msg: string);
    procedure ExtractSVN(Lines: TStrings; UserFilter: String);
    procedure ExtractWikiRevs(Lines: TStrings);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMissingWikiSVNRevisions }

procedure TMissingWikiSVNRevisions.DoRun;
var
  ErrorMsg, SVNFile, WikiFile, Line, Rev, UserFilter: String;
  SVNLines, WIKILines: TStringList;
  i: Integer;
  p: SizeInt;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hs:w:u:', 'help svnlog wikilist user');
  if ErrorMsg<>'' then
    ParamError(ErrorMsg);

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Halt;
  end;

  if not HasOption('s','svnlog') then begin
    WriteHelp;
    Halt;
  end;
  if not HasOption('w','wikilist') then begin
    WriteHelp;
    Halt;
  end;

  SVNFile:=ExpandFileNameUTF8(GetOptionValue('s','svnlog'));
  if not FileExists(SVNFile) then
    ParamError('svn file not found "'+SVNFile+'"');

  WikiFile:=ExpandFileNameUTF8(GetOptionValue('w','wikilist'));
  if not FileExists(WikiFile) then
    ParamError('wiki list file not found "'+WikiFile+'"');

  UserFilter:=GetOptionValue('u','user');

  SVNLines:=TStringList.Create;
  SVNLines.LoadFromFile(SVNFile);
  WIKILines:=TStringList.Create;
  WIKILines.LoadFromFile(WikiFile);

  ExtractSVN(SVNLines,UserFilter);
  ExtractWikiRevs(WIKILines);
  //writeln('SVN: ',SVNLines.Text);
  //writeln('WIKI: ',WIKILines.Text);

  // write missing svn revisions
  writeln('Missing svn revisions:');
  for i:=0 to SVNLines.Count-1 do begin
    Line:=SVNLines[i];
    p:=Pos(' ',Line);
    Rev:=LeftStr(Line,p-1);
    if WIKILines.IndexOf(Rev)>=0 then continue;
    writeln('*',Line);
  end;

  // stop program loop
  Terminate;
end;

procedure TMissingWikiSVNRevisions.ParamError(Msg: string);
begin
  writeln('Invalid param: ',Msg);
  writeln;
  writeln('Usage: ',ExeName,' -h');
  Halt;
end;

procedure TMissingWikiSVNRevisions.ExtractSVN(Lines: TStrings;
  UserFilter: String);
{ Remove empty and separator lines.
  Combine each log entry into a single line "*r12345 message"

------------------------------------------------------------------------
r54919 | martin | 2017-05-14 12:59:15 +0200 (So, 14 Mai 2017) | 1 line

Fixed compile ifdef

->

*r54919 Fixed compile ifdef
}
const MaxCol=80;
var
  i: Integer;
  Line, Revision, UserName: String;
  p, StartP: PChar;
begin
  i:=0;
  UserName:='';
  while i<Lines.Count do begin
    Line:=Lines[i];
    p:=PChar(Line);
    if (LeftStr(Line,10)='----------') or (Trim(Line)='') then begin
      Lines.Delete(i);
    end else if (p^='r') and (p[1] in ['0'..'9']) then begin
      // revision
      inc(p,1);
      while p^ in ['0'..'9'] do inc(p);
      Revision:=LeftStr(Line,p-PChar(Line)); // 'r12345'
      while p^ in [' ','|'] do inc(p);
      StartP:=p;
      while not (p^ in [' ','|',#0]) do inc(p);
      UserName:=copy(Line,StartP-PChar(Line)+1,p-StartP);
      if (UserFilter<>'') and (UserName<>UserFilter) then
        Lines.Delete(i)
      else begin
        Lines[i]:=Revision;
        inc(i);
      end;
    end else begin
      // comment
      Line:=Trim(Line);
      if (UserFilter='') or (UserName=UserFilter) then begin
        if (i>0) and (length(Lines[i-1])<MaxCol) then begin
          Line:=Lines[i-1]+' '+Line;
          if length(Line)>MaxCol then
            Line:=LeftStr(Line,MaxCol)+'...';
          Lines[i-1]:=Line;
        end;
      end;
      Lines.Delete(i);
    end;
  end;
end;

procedure TMissingWikiSVNRevisions.ExtractWikiRevs(Lines: TStrings);
var
  i: Integer;
  Line: String;
  p: PChar;
begin
  i:=0;
  while i<Lines.Count do begin
    Line:=Lines[i];
    if Trim(Line)='' then begin
      Lines.Delete(i);
    end else begin
      p:=PChar(Line);
      if (p[0]='*') and (p[1]='r') and (p[2] in ['0'..'9']) then begin
        inc(p,2);
        while p^ in ['0'..'9'] do inc(p);
        Lines[i]:=copy(Line,2,p-PChar(Line)-1); // 'r12345'
        inc(i);
      end else begin
        writeln('WARNING: invalid wiki line: "',Line,'"');
        Lines.Delete(i);
      end;
    end;
  end;
end;

constructor TMissingWikiSVNRevisions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMissingWikiSVNRevisions.Destroy;
begin
  inherited Destroy;
end;

procedure TMissingWikiSVNRevisions.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -s svn.log -w wikilist.txt [-u username]');
  writeln;
  writeln('Shows svn log entries not listed in the wikilist.');
  writeln;
  writeln('-h, --help');
  writeln('        show this help');
  writeln('-s <file>, --svnlog=<file>');
  writeln('        SVN log, e.g. "svn log --limit=1000 > log.txt"');
  writeln('-w <file>, --wikilist=<file>');
  writeln('        List in wiki format: Every line "*r12345 text"');
  writeln('-u <user>, --user=<user>');
  writeln('        only user ');
end;

var
  Application: TMissingWikiSVNRevisions;
begin
  Application:=TMissingWikiSVNRevisions.Create(nil);
  Application.Title:='Find missing svn revisions in wiki';
  Application.Run;
  Application.Free;
end.

