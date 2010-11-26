unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLProc,
  contnrs, XMLRead;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure TestComment;
    procedure TestInvalidCharacters;
    function Test(Title, Fragment, FixedFragment: string): boolean;
  end;

var
  Form1: TForm1; 

procedure FixFPDocFragment(var Fragment: string; Fix: boolean;
  out ErrorList: TObjectList);

implementation

{$R *.lfm}

type
  TFPDocFragmentError = class
  public
    ErrorPos: integer;
    Msg: string;
  end;

procedure FixFPDocFragment(var Fragment: string; Fix: boolean;
  out ErrorList: TObjectList);
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
    StartPos: integer;
    NameStartPos: integer;
    NameEndPos: integer;
    EndPos: integer;
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
    i: Integer;
  begin
    Line:=1;
    Col:=1;
    if pt>length(Fragment) then pt:=length(Fragment)+1;
    i:=1;
    while i<pt do begin
      case Fragment[i] of
      #10,#13:
        begin
          inc(Line);
          inc(i);
          if (i<=length(Fragment)) and (Fragment[i] in [#10,#13])
            and (Fragment[i-1]<>Fragment[i])
          then
            inc(i);
          Col:=1;
        end;
      else
        inc(Col);
      end;
      inc(i);
    end;
    Result:=IntToStr(Line)+','+IntToStr(Col);
  end;

  procedure Error(ErrorPos: PChar; ErrorMsg: string);
  var
    NewError: TFPDocFragmentError;
  begin
    debugln(['Error ',ErrorMsg]);
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
        if Item^.NameStartPos>StartPos then inc(Item^.NameStartPos,Diff);
        if Item^.NameEndPos>StartPos then inc(Item^.NameEndPos,Diff);
      end;
    end;
    p:=PChar(Fragment)+OldP-1;
    debugln(['Replace ',dbgstr(copy(Fragment,1,Rel(p)-1)),'|',dbgstr(copy(Fragment,Rel(p),length(Fragment)))]);
  end;

  procedure HandleSpecialChar;
  var
    c: Integer;
  begin
    c:=ord(p^);
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
        Replace(Rel(p),1,'&'+IntToStr(c)+';');
      end;
    end
    else begin
      // skip
      Error(p,'invalid character #'+IntToStr(c));
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
          if Fix then begin
            Replace(Rel(p),0,'-->');
            inc(p,3);
          end else
            Error(p,'comment end not found, start at '+LineCol(TopItem^.StartPos));
          break;
        end else begin
          // invalid #0 character in comment => delete
          if Fix then
            Replace(Rel(p),1,'')
          else begin
            Error(p,'invalid character');
            inc(p);
          end;
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
            if Fix then begin
              len:=(p-AmpPos)-1;
              Replace(Rel(AmpPos)+1,len,lowercase(copy(Fragment,Rel(AmpPos)+1,len)));
            end else begin
              Error(NeedLowercase,'special character name is not lower case');
              inc(p);
            end;
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
          inc(p);
          if NeedLowercase=nil then
            NeedLowercase:=p;
        end;
      else break;
      end;
    until false;
    if NeedLowercase<>nil then begin
      if Fix then begin
        Replace(Rel(Start),p-Start,lowercase(copy(Fragment,Rel(Start),p-Start)));
      end else begin
        // all current tags must be lower case
        Error(NeedLowercase,'tags must be lower case');
      end;
    end;
  end;

  procedure ParseAttribute;
  begin
    // attribute name
    LowerCaseName;
    while p^ in [' ',#9,#10,#13] do inc(p); // skip space
    if p^<>'=' then begin
      // missing value
      if Fix then begin
        Replace(Rel(p),0,'=');
      end else begin
        Error(p,'expected =');
      end;
    end;
    if p^='=' then begin
      inc(p);
      while p^ in [' ',#9,#10,#13] do inc(p); // skip space
      if p^<>'"' then begin
        // missing quotes
        if Fix then begin
          Replace(Rel(p),0,'"');
        end else begin
          Error(p,'expected "');
        end;
      end;
    end;
    if p^='"' then begin
      // read value
      inc(p);
      repeat
        case p^ of
        '>',#0..#8,#11,#12,#14..#31,#127:
          // the > is not allowed in the quotes, probably the ending quot is missing
          if Fix then begin
            Replace(Rel(p),0,'"');
          end else begin
            Error(p,'expected ending "');
            break;
          end;
        '&':
          ParseAmpersand;
        '"':
          begin
            inc(p);
            break;
          end
        else
          inc(p);
        end;
      until false;
    end;
  end;

  procedure ParseLowerThan;
  begin
    // comment, tag or 'lower than'
    if (p[1]='!') and (p[2]='-') and (p[3]='-') then begin
      // comment
      ParseComment;
      exit;
    end;
    if p[1] in ['a'..'z','A'..'Z'] then begin
      // open tag
      Push(sitTag);
      TopItem^.StartPos:=Rel(p);
      inc(p);
      TopItem^.NameStartPos:=Rel(p);
      LowerCaseName;
      TopItem^.NameEndPos:=Rel(p);
      while p^ in [' ',#9,#10,#13] do inc(p); // skip space
      case p^ of
      'a'..'z','A'..'Z':
        ParseAttribute;
      '/':
        begin
          // ToDo: close tag
          RaiseGDBException('ToDo');
        end;
      '>':
        begin
          // ToDo:
        end;
      end;
    end else if p[1]='/' then begin
      // ToDo: close tag
      RaiseGDBException('ToDo');
    end;
    // invalid character => convert or skip
    HandleSpecialChar;
  end;

begin
  ErrorList:=nil;
  if Fragment='' then exit;
  Top:=-1;
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
    if Top>=0 then begin
      // ToDo: fix unclosed tags
      debugln(['FixFPDocFragment ToDo: fix unclosed tags']);
    end;
  finally
    ReAllocMem(Stack,0);
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //TestComment;
  //TestInvalidCharacters;
end;

procedure TForm1.TestComment;
begin
  Test('close comment','<!--','<!---->');
  Test('close comment and delete invalid char','<!--null'#0#1#2'comment','<!--nullcomment-->');
end;

procedure TForm1.TestInvalidCharacters;
begin
  Test('delete special characters','A'#0'B'#1#127,'AB');
  Test('replace tag characters','LT<GT>AMP&','LT&lt;GT&gt;AMP&amp;');
  Test('lower case special characters','&LT;','&lt;');
end;

function TForm1.Test(Title, Fragment, FixedFragment: string): boolean;
var
  s: String;
  ErrorList: TObjectList;
begin
  Result:=true;
  try
    s:=Fragment;
    FixFPDocFragment(s,true,ErrorList);
    if s<>FixedFragment then begin
      Result:=false;
      debugln(['failed: ',Title]);
      debugln(['  fragment: '+DbgStr(Fragment)]);
      debugln(['  should:   '+DbgStr(FixedFragment)]);
      debugln(['  result:   '+DbgStr(s)]);
    end;
  finally
    ErrorList.Free;
  end;
end;

end.

