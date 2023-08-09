unit lazddsqlutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TQuoteOption = (qoAddLineBreak,qoAddConst,qoTStringsAdd);
  TQuoteOptions = Set of TQuoteOption;


procedure UnquoteSQL(Src,Dest : TStrings);
procedure QuoteSQL(Src,Dest : TStrings; Options : TQuoteOptions; Const aConstName : string = '');


implementation

procedure TStringsAddQuoteSQL(Src,Dest : TStrings; Const aConstName : string = '');

Var
  ind : String;
  i : Integer;
  S : String;

begin
  Ind:='';
  if aConstName<>'' then
    begin
    Dest.Add('with %s do',[aConstName]);
    Dest.Add('  begin');
    Ind:='  ';
    end;
  For I:=0 to Src.Count-1 do
    begin
    S:=Src[I];
    S:='Add('''+StringReplace(S,'''','''''',[rfReplaceAll])+''');';
    Dest.Add(Ind+S);
    end;
  if aConstName<>'' then
    Dest.Add('  end;');
end;

procedure QuoteSQL(Src,Dest : TStrings; Options : TQuoteOptions; Const aConstName : string = '');

Var
  ind : String;
  i : Integer;
  S : String;

begin
  if (qoTStringsAdd in Options) then
    begin
    TStringsAddQuoteSQL(Src,Dest,aConstName);
    exit;
    end;
  ind:='  ';
  Dest.Clear;
  if (qoAddConst in Options) then
    begin
    Dest.Add('  SQLSelect = ');
    ind:=ind+'  ';
    end;
  For I:=0 to Src.Count-1 do
    begin
    S:=Src[I];
    S:=''''+StringReplace(S,'''','''''',[rfReplaceAll]);
    if I=Src.Count-1 then
      begin
      S:=S+'''';
      if (qoAddConst in Options) then
        S:=S+';';
      end
    else
      begin
      if qoAddLineBreak in Options then
        S:=S+''' + sLineBreak+'
      else
        S:=S+' '' + ';
      end;
    Dest.Add(Ind+S);
    end;
end;

procedure UnquoteSQL(Src,Dest : TStrings);

Const
  Q = '''';

Var

  S,U : String;
  I,L,FirstQ,LastQ : Integer;
  C : Char;
  InQ : Boolean;

begin
  Dest.Clear;
  For S in Src do
    begin
    U:='';
    I:=1;
    L:=Length(S);
    InQ:=False;
    LastQ:=0;
    FirstQ:=0;
    While (I<=L) do
      begin
      C:=S[I];
      if C<>Q then
        U:=U+C
      else
        if InQ then
          begin
          if (I<L) and (S[I+1]=Q) then
            begin
            U:=U+C;
            Inc(i);
            end
          else
            begin
            InQ:=False;
            LastQ:=Length(U);
            end
          end
        else
          begin
          InQ:=True;
          if FirstQ=0 then
            FirstQ:=Length(U);
          end;
      Inc(I);
      end;
    // Strip all after last quote
    If LastQ>0 then
      U:=Copy(U,1,LastQ);
    // Strip all before first quote
    if Trim(Copy(U,1,FirstQ))='' then
      Delete(U,1,FirstQ);
    Dest.Add(U);
    end;
end;



end.

