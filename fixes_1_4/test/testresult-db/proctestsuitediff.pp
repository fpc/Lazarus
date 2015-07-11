program proctestsuitediff;
{$mode objfpc}{$h+}

uses
  sysutils, classes, strutils;

const
  runhour = 8;      { cut-off hour that distinguishes yesterday and today }
  urlprefix = 'http://fpcfos64.freepascal.org/laztestsuite/cgi-bin/testsuite.cgi?';

function getdate(line: string): string;
begin
  result := copy(line, posex('|', line, Pos('|', line)+1)+2, 10);
end;

function getfail(line: string): string;
var
  i, j: integer;
begin
  i := 2;
  while (i < length(line)) and (line[i] = ' ') do
    inc(i);
  j := i+1;
  while (j < length(line)) and (line[j] in ['0'..'9']) do
    inc(j);
  result := copy(line, i, j-i);
end;

type
  toutputline = class(tobject)
  public
    data, url: string;
  end;

var
  header: array[0..2] of string;
  footer: string;
  lenfailstr: integer;
  urllist: tstrings;

procedure printtable(list: tstringlist; heading: string);
var
  outputline: toutputline;
  str, urlref: string;
  i: integer;
begin
  if list.count = 0 then
    exit;
  writeln(heading);
  for i := 0 to 2 do
    writeln(header[i]);
  for i := 0 to list.count - 1 do
  begin
    str := list.strings[i];
    outputline := toutputline(list.objects[i]);
    urlref := inttostr(urllist.count+1);
    writeln('| ' + stringofchar(' ', 3 - length(urlref)) + urlref + ' | ' + str + stringofchar(' ', lenfailstr - length(str)) + ' ' + outputline.data);
    urllist.add(outputline.url);
  end;
  writeln(footer);
  writeln;
end;

procedure printurllist;
var
  i: integer;
begin
  for i := 0 to urllist.count-1 do
    writeln('[' + inttostr(i+1) + ']: ' + urllist.strings[i]);
  writeln;
end;

procedure addlist(list: tstrings; const failstr, data, url: string);
var
  outputline: toutputline;
begin
  outputline := toutputline.create;
  outputline.data := data;
  outputline.url := url;
  list.addobject(failstr, outputline);
  if length(failstr) > lenfailstr then
    lenfailstr := length(failstr);
end;

type
  ttestrun = record
    line, date, fail, data, runid, dbfail, failset: string;
    hour: integer;
  end;

function construct_results_url(const runid: string): string;
begin
  result := urlprefix+'action=1&failedonly=1&run1id='+runid;
end;

function construct_compare_url(const run1id, run2id: string): string;
begin
  result := urlprefix+'action=1&run1id='+run1id+'&run2id='+run2id+'&noskipped=1';
end;

function checkchange(var prev, curr: ttestrun; const prevdate, currdate: string;
  changelist, nochangelist: tstrings): boolean;
var
  failstr: string;
begin
  result := (prev.date = prevdate) and (prev.data = curr.data) and (curr.date = currdate);
  if result then
  begin
    if (length(curr.line) <> 0) and (length(prev.line) <> 0) then
    begin
      if prev.dbfail = curr.dbfail then
      begin
        failstr := curr.fail;
        addlist(nochangelist, failstr, curr.data, construct_results_url(curr.runid));
      end else begin
        failstr := prev.fail + ' -> ' + curr.fail;
        addlist(changelist, failstr, curr.data, construct_compare_url(prev.runid, curr.runid));
      end;
    end;
    { both these lines have been processed }
    curr.line := '';
    prev.line := '';
  end;
end;

function findseparator(aoffset, aindex: integer): integer;
var
  I: integer;
begin
  for I := 1 to aindex do
  begin
    inc(aoffset);
    while (aoffset<length(header[1])) and (header[1][aoffset] <> '|') do
      inc(aoffset);
  end;
  result := aoffset;
end;

const
  { cut fails and date (first two fields, '| FAILS | DATE       ', 21 characters) }
  datastart = 22;

var
  twodaysago, yesterday, today: string;
  curr, prev, old: ttestrun;
  list, prevnochangelist, prevchangelist, prevdisappearlist: tstringlist;
  prevnewlist, disappearlist, nochangelist, changelist, newlist: tstringlist;
  blinkerchangelist, blinkernochangelist: tstringlist;
  todaydate: TDateTime;
  dataend, datalen, houroffset: integer;
  runidoffset, runidend, runidlen: integer;
  dbfailsep, failoffset, failend: integer;
begin
  blinkernochangelist := tstringlist.create;
  blinkerchangelist := tstringlist.create;
  prevdisappearlist := tstringlist.create;
  prevnochangelist := tstringlist.create;
  prevchangelist := tstringlist.create;
  disappearlist := tstringlist.create;
  nochangelist := tstringlist.create;
  prevnewlist := tstringlist.create;
  changelist := tstringlist.create;
  newlist := tstringlist.create;
  urllist := tstringlist.create;
  footer := '';
  old.data := '';
  readln;
  repeat
    if eof then
      halt(1);
    readln(header[0]);
  until (length(header[0]) > 0) and (header[0][1] = '+');
  readln(header[1]);
  readln(header[2]);
  if ParamCount >= 3 then
    todaydate := EncodeDate(StrToInt(ParamStr(1)), StrToInt(ParamStr(2)),
      StrToInt(ParamStr(3)))
  else
    todaydate := Now;
  twodaysago := FormatDateTime('YYYY-mm-dd', todaydate-2);
  yesterday := FormatDateTime('YYYY-mm-dd', todaydate-1);
  today := FormatDateTime('YYYY-mm-dd', todaydate);
  lenfailstr := 5;  { Length('FAILS') = column header }
  dataend := findseparator(datastart, 8);
  datalen := dataend - datastart + 1;
  { cut time (last 2 fields, ' HH:MM:SS |  XXXX |') }
  houroffset := dataend + 2;
  runidoffset := houroffset + 11;
  runidend := findseparator(runidoffset, 1);
  runidlen := runidend - 1 - runidoffset;
  failoffset := runidend + 2;
  failend := findseparator(failoffset, 1);
  fillchar(curr,sizeof(curr),0);
  repeat
    if eof then
      break;
    if (length(curr.line) = 0) or (curr.line[1] <> '+') then
    begin
      readln(curr.line);
      curr.fail := getfail(curr.line);
      curr.date := getdate(curr.line);
      curr.data := copy(curr.line, datastart, datalen);
      curr.hour := strtointdef(copy(curr.line, houroffset, 2), 0);
      curr.runid := trim(copy(curr.line, runidoffset, runidlen));
      dbfailsep := posex('|', curr.line, failoffset);
      curr.dbfail := copy(curr.line, failoffset, dbfailsep-failoffset);
      curr.failset := trim(copy(curr.line, dbfailsep+1, failend-2-dbfailsep));
      //if curr.dbfail <> curr.fail then
        //curr.fail := curr.fail + ' (' + curr.dbfail + ')';
    end else
    if length(footer) = 0 then
      footer := curr.line;
    { 'same' testrun yesterday and today, changelist or nochangelist modified }
    if checkchange(prev, curr, yesterday, today, changelist, nochangelist)
        and (old.data = prev.data) then
      old.line := '';
    { 'same' testrun two days ago and today, a "blinker" }
    if checkchange(prev, curr, twodaysago, today, blinkerchangelist, blinkernochangelist)
        and (old.data = prev.data) then
      old.line := '';
    { 'same' testrun two days ago and yesterday, prevchangelist or prevnochangelist modified }
    { only detect equal testruns yesterday if submitted late for diff mail yesterday }
    if prev.hour >= runhour then
      checkchange(old, prev, twodaysago, yesterday, prevchangelist, prevnochangelist);
    { still some unprocessed line? }
    if length(old.line) > 0 then
    begin
      list := nil;
      if old.date = twodaysago then
      begin
        if old.hour >= runhour then
          list := prevdisappearlist
        { else we already had it disappear yesterday }
      end else if old.date = yesterday then
        if old.hour < runhour then
          list := disappearlist
        else
          list := prevnewlist
      else if old.date = today then
        list := newlist;
      if list <> nil then
        addlist(list, old.fail, old.data, construct_results_url(old.runid));
    end;
    old := prev;
    prev := curr;
  until (length(old.line) > 0) and (old.line[1] = '+');

  header[0] := '+-----' + copy(header[0], 1, 1) + stringofchar('-', lenfailstr+2) +
    copy(header[0], datastart, datalen);
  header[1] := '| URL ' + copy(header[1], 1, 7) + stringofchar(' ', lenfailstr-4) +
    copy(header[1], datastart, datalen);
  header[2] := '+-----' + copy(header[2], 1, 1) + stringofchar('-', lenfailstr+2) +
    copy(header[2], datastart, datalen);
  footer    := '+-----' + copy(footer,    1, 1) + stringofchar('-', lenfailstr+2) +
    copy(footer,    datastart, datalen);

  printtable(disappearlist, 'DISAPPEARED:');
  printtable(prevdisappearlist, 'DISAPPEARED YESTERDAY:');
  printtable(changelist, 'CHANGED:');
  printtable(prevchangelist, 'CHANGED YESTERDAY:');
  printtable(blinkerchangelist, 'CHANGED BLINKER:');
  printtable(newlist, 'NEW:');
  printtable(prevnewlist, 'NEW YESTERDAY:');
  printtable(nochangelist, 'UNCHANGED:');
  printtable(prevnochangelist, 'UNCHANGED YESTERDAY:');
  printtable(blinkernochangelist, 'UNCHANGED BLINKER:');

  printurllist;

  newlist.free;
  changelist.free;
  prevnewlist.free;
  nochangelist.free;
  disappearlist.free;
  prevchangelist.free;
  prevnochangelist.free;
  prevdisappearlist.free;
  blinkerchangelist.free;
  blinkernochangelist.free;
end.
