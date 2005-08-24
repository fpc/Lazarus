unit LrtPoTools;
{ Copyright (C) 2004 V.I.Volchenko, Lazarus and FreePascal Developers Teams

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
}
//TODO: Make more quick mechanism for .po updating
//This source uses a portion of GPL'ed code from rstconv.pp from fpc/utils
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;
type
  TPOStyle=(postStandard,postPropName,postFull);
procedure Lrt2Po(const LRTFile:string;POStyle:TPOStyle);
procedure CombinePoFiles(SL:TStrings;const FName:string);

implementation

function OKStr(const s:string):string;
var
   i: Integer;
begin
  Result:='';
  for i:=1 to Length(s) do
   case s[i] of
   '"':Result:=Result+'\"';
   '%':Result:=Result+'\%';
   '\':Result:=Result+'\\';
   '#':Result:=Result+'\#';
   //TODO: check if that's all
   else Result:=Result+s[i];
   end;
end;

procedure Lrt2Po(const LRTFile: string;POStyle:TPoStyle);
var
  SL1: TStringList;
  SL2: TStringList;
  i: Integer;
  p: LongInt;
  k: LongInt;
  s: string;
  DotPos: LongInt;
  EqPos: LongInt;
  s1: String;
  j: Integer;
  found: Boolean;
  m: LongInt;
begin
  SL1:=TStringList.Create;
  SL2:=TStringList.Create;
  if FileExists(LRTFile) then SL1.LoadFromFile(LRTFile);
// Trying to process RST file in addition

  if FileExists(ChangeFileExt(LRTFile,'.rst')) then
  begin
    SL2.LoadFromFile(ChangeFileExt(LRTFile,'.rst'));
    // Some code from rstconv used here
    k:=0;
    while k<SL2.Count do
    begin
      s:=SL2[k];
      If (Length(S)=0) or (S[1]='#') then begin inc(k);continue;end;
      DotPos := Pos('.', s);
      EqPos := Pos('=', s);
      if DotPos > EqPos then // paranoia checking.
        DotPos := 0;

      s1 := '';
      i := EqPos + 1;
      while i <= Length(s) do
      begin
        if s[i] = '''' then
        begin
          Inc(i);
          j := i;
          while (i <= Length(s)) and (s[i] <> '''') do
            Inc(i);
          s1 := s1 + Copy(s, j, i - j);
          Inc(i);
        end else if s[i] = '#' then
        begin
          Inc(i);
          j := i;
          while (i <= Length(s)) and (s[i] in ['0'..'9']) do
            Inc(i);
          s1 := s1 + Chr(StrToInt(Copy(s, j, i - j)));
        end else if s[i] = '+' then
        begin
          if k<SL2.Count-1 then begin s:=SL2[k+1];inc(k);end;
          i := 1;
        end else
          Inc(i);
      end;
      SL1.Add(s1);
      inc(k);
    end;
  end;
  SL2.Clear;
  for i:=0 to SL1.Count-1 do
  begin
    found:=false;
    for j:=0 to SL2.Count-1 do
      if SL2[j]=SL1[i] then begin found:=true;break;end;
    if not found then SL2.Add(SL1[i]);
  end;
  SL1.Clear;
  SL1.AddStrings(SL2);
  SL2.Clear;
  for i:=0 to SL1.Count-1 do
  begin
   case POStyle of
    postStandard:
      begin
        p:=Pos('=',SL1[i]);
        s:=copy(SL1[i],p+1,Length(SL1[i])-p);//if p=0, that's OK, all the string
      end;
    postPropName:
      begin
        p:=Pos('.',SL1[i]);
        s:=copy(SL1[i],p+1,Length(SL1[i])-p);
      end;
    postFull:s:=SL1[i];
   end;
   s:=OKStr(s);
   p:=pos(SL1[i],'=');
   SL2.Add('#'+copy(SL1[i],1,p-1));
   SL2.Add('msgid  "'+s+'"');
   SL2.Add('msgstr ""');
   SL2.Add('');
  end;
  s:=ChangeFileExt(LRTFile,'.po');
  if not FileExists(s) then
  try
    SL2.SaveToFile(s);
  finally
    SL1.Free;SL2.Free;
  end
  else try
    SL1.Clear;
    SL1.LoadFromFile(s);
    for i:=0 to SL2.Count-1 do
    begin
      for j:=0 to SL1.Count-1 do
      begin
        if (Length(SL1[j])>6) and
         (Length(SL2[i])>6) and
         (LeftStr(SL1[j],5)='msgid') and
         (LeftStr(SL2[i],5)='msgid')and
         (Trim(copy(SL1[j],6,length(SL1[j])-5))=Trim(copy(SL2[i],5,length(SL2[i])-4)))
        then //found!
        begin
          //Ignore any comment etc
          k:=j;
          while (k<SL1.Count) and
          ((length(SL1[k])<7) or
           (LeftStr(SL1[k],5)<>'msgstr')) do inc(k);
          if (k<SL1.Count) then
          begin
            m:=i;
            while (m<SL2.Count) and
            ((length(SL2[m])<7) or
             (LeftStr(SL2[m],5)<>'msgstr')) do inc(m);
            if (m<SL2.Count)then
             SL2[m]:=SL1[k];
          end;
        end;
      end;
    end;
    if FileExists(s) then DeleteFile(s);
    SL2.SaveToFile(s);
  finally
    SL1.Free;SL2.Free;
  end;
end;

procedure CombinePoFiles(SL:TStrings;const FName:string);
var
  SL1,SL2: TStringList;
  i: Integer;
  k: Integer;
  function ComparePoStrings(const s1,s2:string):boolean;
  var
    S3: String;
    s4: String;
  begin
    S3:=Trim(s1);s4:=Trim(S2);
    Result:=false;
    if length(s3)<6 then exit;
    if length(s4)<6 then exit;
    if LeftStr(S3,5)<>'msgid' then exit;
    if LeftStr(S4,5)<>'msgid' then exit;
    if Trim(copy(s3,6,length(s3)-5))<>Trim(copy(s4,6,length(s4)-5)) then exit;
    Result:=true;
  end;
begin
  SL1:=TStringList.Create;
  SL2:=TStringList.Create;
  for i:=0 to SL.Count-1 do
  begin
    SL2.Clear;
    SL2.LoadFromFile(SL[i]);
    SL1.AddStrings(SL2);
  end;
  //Removing dublicates. Seems that I have just overprogrammed the code.
  //This code needs to be tested.
  SL2.Clear;//SL2 will be IDList
  k:=0;
  while k<SL1.Count do
  begin
    if k<SL1.Count-1 then for i:=k+1 to SL1.Count-1 do
      if ComparePoStrings(SL1[k],SL1[i]) then
      begin
        inc(k);
        while (k<SL1.Count)and(LeftStr(Trim(SL1[k]),6)<>'msgstr') do
        begin
          SL2.Add(SL1[k]);
          inc(k);
        end;
        if k<SL1.Count then inc(k);
        break;
      end;
    if k<SL1.Count then SL2.Add(SL1[k]);
    inc(k);
  end;
  try
    if FileExists(FName) then DeleteFile(FName);
    SL2.SaveToFile(FName);
  finally
    SL1.Free;SL2.Free;
  end;
end;

end.

