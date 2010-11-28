{
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

  Author: Mattias Gaertner

  Abstract:
    function for testing if a xml fragment is correct for fpdoc
    and if not to automatically repair it
}
unit CTXMLFixFragment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, contnrs, BasicCodeTools;

procedure FixFPDocFragment(var Fragment: string; AllowTags, Fix: boolean;
  out ErrorList: TObjectList; Verbose: boolean = false);

implementation


type
  TFPDocFragmentError = class
  public
    ErrorPos: integer;
    Msg: string;
  end;

procedure FixFPDocFragment(var Fragment: string; AllowTags, Fix: boolean;
  out ErrorList: TObjectList; Verbose: boolean);
{ - Fix all tags to lowercase to reduce svn commits
  - auto close comments
  - remove #0 from comments
  - convert special characters to &x;
  - fix &name; lower case
  - fix unclosed attribute values
  - auto close unclosed tags
}
type
  TStackItemTyp = (
    sitTag,
    sitComment
    );
  TStackItem = record
    Typ: TStackItemTyp;
    StartPos: integer; // position of <
    EndPos: integer; // position behind >
    CloseStartPos: integer; // position of <
  end;
  PStackItem = ^TStackItem;
var
  Stack: PStackItem;
  Capacity: integer;
  Top: integer;
  TopItem: PStackItem;
  p: PChar;

  function Rel(pt: PChar): integer;
  begin
    Result:=pt-PChar(Fragment)+1;
  end;

  function LineCol(pt: integer): string;
  var
    Line: Integer;
    Col: Integer;
  begin
    SrcPosToLineCol(Fragment,pt,Line,Col);
    Result:=IntToStr(Line)+','+IntToStr(Col);
  end;

  procedure Error(ErrorPos: PChar; ErrorMsg: string);
  var
    NewError: TFPDocFragmentError;
    LineStart,LineEnd: integer;
  begin
    if Verbose then begin
      debugln(['Error at ',LineCol(Rel(ErrorPos)),': ',ErrorMsg]);
      GetLineStartEndAtPosition(Fragment,Rel(ErrorPos),LineStart,LineEnd);
      debugln(['  Line: ',copy(Fragment,LineStart,Rel(ErrorPos)-LineStart),'|',
                          copy(Fragment,Rel(ErrorPos),LineEnd-Rel(ErrorPos)+1)]);
    end;
    if not Fix then exit;
    if ErrorList=nil then
      ErrorList:=TObjectList.Create(true);
    NewError:=TFPDocFragmentError.Create;
    NewError.ErrorPos:=Rel(ErrorPos);
    NewError.Msg:=ErrorMsg;
    ErrorList.Add(NewError);
  end;

  procedure Replace(StartPos, Len: integer; const NewTxt: string);
  var
    i: Integer;
    Item: PStackItem;
    Diff: Integer;
    OldP: integer;
  begin
    OldP:=Rel(p);
    Fragment:=copy(Fragment,1,StartPos-1)+NewTxt
                                  +copy(Fragment,StartPos+Len,length(Fragment));
    Diff:=length(NewTxt)-Len;
    if Diff<>0 then begin
      // adjust positions
      if OldP>StartPos then
        inc(OldP,Diff);
      for i:=0 to Top do begin
        Item:=@Stack[i];
        if Item^.StartPos>StartPos then inc(Item^.StartPos,Diff);
        if Item^.EndPos>StartPos then inc(Item^.EndPos,Diff);
        if Item^.CloseStartPos>StartPos then inc(Item^.CloseStartPos,Diff);
      end;
    end;
    p:=PChar(Fragment)+OldP-1;
    //debugln(['Replace ',dbgstr(copy(Fragment,1,Rel(p)-1)),'|',dbgstr(copy(Fragment,Rel(p),length(Fragment)))]);
  end;

  procedure HandleSpecialChar;
  var
    c: char;
  begin
    c:=p^;
    Error(p,'invalid character '+dbgstr(c));
    if Fix then begin
      case p^ of
      #0..#31,#127:
        // delete
        Replace(Rel(p),1,'');
      '<': Replace(Rel(p),1,'&lt;');
      '>': Replace(Rel(p),1,'&gt;');
      '&': Replace(Rel(p),1,'&amp;');
      '''': Replace(Rel(p),1,'&apos;');
      '"': Replace(Rel(p),1,'&quot;');
      else
        // convert
        Replace(Rel(p),1,'&'+IntToStr(ord(c))+';');
      end;
    end
    else begin
      // skip
      inc(p);
    end;
  end;

  procedure Push(Typ: TStackItemTyp);
  begin
    inc(Top);
    if Top=Capacity then begin
      inc(Capacity);
      ReAllocMem(Stack,SizeOf(TStackItem)*Capacity);
    end;
    TopItem:=@Stack[Top];
    FillByte(TopItem^,SizeOf(TStackItem),0);
    TopItem^.Typ:=Typ;
    TopItem^.StartPos:=p-PChar(Fragment);
  end;

  procedure Pop;
  begin
    if Top<0 then raise Exception.Create('bug');
    dec(Top);
    if Top>=0 then
      TopItem:=@Stack[Top]
    else
      TopItem:=nil;
  end;

  procedure ParseComment;
  begin
    // comment
    Push(sitComment);
    inc(p,4);
    // parse comment
    repeat
      if p^ in [#0..#8,#11,#12,#14..#31,#127] then begin
        // invalid character in comment => delete
        if (p^=#0) and (p-PChar(Fragment)=length(Fragment)) then
        begin
          // reached end of fragment => close comment
          Error(p,'comment end not found, start at '+LineCol(TopItem^.StartPos));
          if Fix then begin
            Replace(Rel(p),0,'-->');
            inc(p,3);
          end;
          break;
        end else begin
          // invalid #0 character in comment => delete
          Error(p,'invalid character');
          if Fix then
            Replace(Rel(p),1,'')
          else
            inc(p);
        end;
      end else if (p^='-') and (p[1]='-') and (p[2]='>') then
      begin
        // comment end found
        inc(p,3);
        break;
      end else
        inc(p);
    until false;
    Pop;
  end;

  procedure ParseAmpersand;
  var
    AmpPos: PChar;
    i: Integer;
    NeedLowercase: PChar;
    len: integer;
  begin
    AmpPos:=p;
    // &amp; or &name; or &decimal;
    case p[1] of
    '0'..'9':
      begin
        // decimal number
        inc(p);
        i:=ord(p^)-ord('0');
        while p^ in ['0'..'9'] do
        begin
          i:=i+10+ord(p^)-ord('0');
          if i>$10FFFF then
            break;
          inc(p);
        end;
        if p^=';' then
        begin
          // ok
          inc(p);
          exit;
        end;
      end;
    'a'..'z','A'..'Z':
      begin
        // name
        inc(p);
        NeedLowercase:=nil;
        while p^ in ['a'..'z','A'..'Z'] do begin
          if (NeedLowercase=nil) and (p^ in ['A'..'Z']) then
            NeedLowercase:=p;
          inc(p);
        end;
        if p^=';' then begin
          if NeedLowercase<>nil then begin
            Error(NeedLowercase,'character name must be lower case');
            if Fix then begin
              len:=(p-AmpPos)-1;
              Replace(Rel(AmpPos)+1,len,lowercase(copy(Fragment,Rel(AmpPos)+1,len)));
            end else
              inc(p);
          end else begin
            // ok
            inc(p);
          end;
          exit;
        end;
      end;
    end;
    p:=AmpPos;
    // invalid character => convert or skip
    HandleSpecialChar;
  end;

  procedure LowerCaseName;
  var
    Start: PChar;
    NeedLowercase: PChar;
  begin
    Start:=p;
    NeedLowercase:=nil;
    repeat
      case p^ of
      'a'..'z': inc(p);
      'A'..'Z':
        begin
          if NeedLowercase=nil then
            NeedLowercase:=p;
          inc(p);
        end;
      else break;
      end;
    until false;
    if NeedLowercase<>nil then begin
      Error(NeedLowercase,'names must be lower case');
      if Fix then
        Replace(Rel(Start),p-Start,lowercase(copy(Fragment,Rel(Start),p-Start)));
    end;
  end;

  procedure ParseAttribute;
  var
    Quot: Char;
  begin
    // read attribute name
    LowerCaseName;
    while p^ in [' ',#9,#10,#13] do inc(p); // skip space
    if p^<>'=' then begin
      // missing value
      Error(p,'expected =');
      if Fix then
        Replace(Rel(p),0,'=');
    end;
    if p^='=' then begin
      // read equal
      inc(p);
      while p^ in [' ',#9,#10,#13] do inc(p); // skip space
      if not (p^ in ['"','''']) then begin
        // missing quote
        Error(p,'expected "');
        if Fix then
          Replace(Rel(p),0,'"');
      end;
    end;
    if p^ in ['"',''''] then begin
      Quot:=p^;
      // read value
      inc(p);
      repeat
        case p^ of
        '>',#0..#8,#11,#12,#14..#31,#127:
          begin
            // the > is not allowed in the quotes, probably the ending quot is missing
            Error(p,'expected ending '+Quot);
            if Fix then
              Replace(Rel(p),0,Quot)
            else
              break;
          end;
        '&':
          ParseAmpersand;
        else
          if p^=Quot then
          begin
            inc(p);
            break;
          end;
          inc(p);
        end;
      until false;
    end;
  end;

  procedure ParseCloseTag;
  var
    i: LongInt;
  begin
    if (p^<>'<') or (p[1]<>'/') or (not (p[2] in ['a'..'z','A'..'Z'])) then
    begin
      HandleSpecialChar;
      exit;
    end;
    // p stands on < of </tagname>
    i:=Top;
    while (i>=0)
    and (CompareIdentifiers(p+2,@Fragment[Stack[i].StartPos+1])<>0) do
      dec(i);
    if i<0 then begin
      // no corresponding open tag
      Error(p,'tag was never opened');
      HandleSpecialChar;
      exit;
    end;
    TopItem^.CloseStartPos:=Rel(p);
    inc(p,2); // skip </
    // read name
    LowerCaseName;
    // close unclosed sub tag
    i:=Top;
    while CompareIdentifiers(@Fragment[Stack[i].StartPos+1],
                             @Fragment[TopItem^.CloseStartPos+2])<>0
    do begin
      Error(@Fragment[TopItem^.StartPos],'tag must be closed');
      if Fix then
        Replace(TopItem^.EndPos-1,0,'/');
      Pop;
      dec(i);
    end;
    // skip space
    while (p^ in [' ',#9,#10,#13]) do inc(p);
    if p^='/' then begin
      // uneeded close /
      Error(p,'invalid close /');
      if Fix then
        Replace(Rel(p),1,'')
      else
        inc(p);
    end;
    if p^<>'>' then begin
      Error(p,'missing >');
      if Fix then
        Replace(Rel(p),0,'>');
    end;
    if p^='>' then inc(p);
    // close tag
    Pop;
  end;

  procedure ParseOpenTag;
  begin
    Push(sitTag);
    TopItem^.StartPos:=Rel(p);
    inc(p);
    LowerCaseName;
    repeat
      while p^ in [' ',#9,#10,#13] do inc(p); // skip space
      case p^ of
      'a'..'z','A'..'Z':
        ParseAttribute;
      '/':
        begin
          // end and close tag
          inc(p);
          if p^<>'>' then begin
            Error(p,'expected >');
            if Fix then begin
              Replace(Rel(p),0,'>');
              inc(p);
            end;
          end;
          Pop;
          exit;
        end;
      '>':
        begin
          // end tag
          inc(p);
          TopItem^.EndPos:=Rel(p);
          exit;
        end;
      else
        // invalid character => end tag
        Error(p,'expected >');
        if Fix then begin
          Replace(Rel(p),0,'>');
          inc(p);
        end;
        exit;
      end;
    until false;
  end;

  procedure ParseLowerThan;
  begin
    // comment, tag or 'lower than'
    if not AllowTags then begin
      // invalid character => convert or skip
      HandleSpecialChar;
    end else if (p[1]='!') and (p[2]='-') and (p[3]='-') then begin
      // comment
      ParseComment;
    end else if p[1] in ['a'..'z','A'..'Z'] then begin
      // start tag
      ParseOpenTag;
    end else if p[1]='/' then begin
      // close tag
      ParseCloseTag;
    end else
      // invalid character => convert or skip
      HandleSpecialChar;
  end;

begin
  ErrorList:=nil;
  if Fragment='' then exit;
  Top:=-1;
  TopItem:=nil;
  Capacity:=0;
  Stack:=nil;
  try
    p:=PChar(Fragment);
    repeat
      case p^ of
      #0..#8,#11,#12,#14..#31,#127:
        begin
          if (p^=#0) and (p-PChar(Fragment)=length(Fragment)) then
          begin
            // reached end of fragment
            break;
          end else begin
            // invalid character => convert or skip
            HandleSpecialChar;
          end;
        end;
      '<':
        ParseLowerThan;
      '>':
        // invalid character => convert or skip
        HandleSpecialChar;
      '&':
        ParseAmpersand;
      else
        inc(p);
      end;
    until false;
    while Top>=0 do begin
      // fix unclosed tags
      Error(@Fragment[TopItem^.StartPos],'tag must be closed');
      if Fix then
        Replace(TopItem^.EndPos-1,0,'/');
      Pop;
    end;
  finally
    ReAllocMem(Stack,0);
  end;
end;

end.

