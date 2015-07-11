unit pdfvrsintatico;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pdfvrlexico;

type
  AnSintaticoPage = class
  public
    Estado: Int64;
    obj1,obj2 : String;
    pageFound: Boolean;
    constructor Create;
    procedure automata(t: Token);
  end;

  AnSintaticoPageContents = class
  public
    Estado: Int64;
    obj1,obj2 : String;
    len_obj1,len_obj2: String;
    contentsFound: Boolean;
    h: PageHeader;
    constructor Create;
    procedure automata(t: Token; Input: TStream);
  end;

  AnSintaticoCommand = class
  public
    Estado: Int64;
    Codigo: Boolean;
    c: Command;
    constructor Create;
    function automata(t: Token):Command;
  end;

  AnSintaticoLength = class
  public
    Estado: Int64;
    len_obj1,len_obj2: String;
    page_length : Int64;
    lenghtFound: Boolean;
    constructor Create;
    procedure automata(t: Token);
  end;

implementation

procedure AnSintaticoPage.automata(t: Token);
begin
  case Estado of
  1:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPage.automata Estado 1');
    {$endif}
             if(t.token_string = 'Type') then
                   begin
                       Estado := 2;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  2:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPage.automata Estado 2');
    {$endif}
             if(t.token_string = 'Page') then
                   begin
                        Estado := 3;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  3:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPage.automata Estado 3');
    {$endif}
             if(t.token_string = 'Contents') then
                   begin
                        Estado := 4;
                   end
             else
                   begin
                        Estado := 3;
                   end;
  end;
  4:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPage.automata Estado 4');
    {$endif}
             if(t.tipo = 1) then // numbers 1
                   begin
                        obj1:=t.token_string;
                        Estado := 5;
                   end
             else
                   begin
                        raise Exception.Create('ERROR: Arquivo corrompido.');
                        Halt(1);
                   end;
  end;
  5:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPage.automata Estado 5');
    {$endif}
             if(t.tipo = 1) then // numbers 2
                   begin
                        obj2:=t.token_string;
                        Estado := 6; // symbolic state
                        pageFound := true;
                   end
             else
                   begin
                        raise Exception.Create('ERROR: Arquivo corrompido.');
                        Halt(1);
                   end;
  end;
  else
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPage.automata Estado ELSE');
    {$endif}
    Estado := 1;
  end;
end;

procedure AnSintaticoPageContents.automata(t: Token; Input: TStream);
var
  myAnLexicoLength: AnLexico;
  myAnSintaticoLength: AnSintaticoLength;
  mytokenLength: Token;
begin
  case Estado of
  1:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPageContents.automata Estado 1');
    {$endif}
             if(t.token_string = obj1) then
                   begin
                        Estado := 2;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  2:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPageContents.automata Estado 2');
    {$endif}
             if(t.token_string = obj2) then
                   begin
                        Estado := 3;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  3:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPageContents.automata Estado 3');
    {$endif}
             if(t.token_string = 'obj') then
                   begin
                        Estado := 4;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  4:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPageContents.automata Estado 4');
    {$endif}
             if(t.token_string = 'Length') then
                   begin
                        Estado := 5;
                   end
             else if (t.token_string = 'Filter') then
                  begin
                       Estado := 7;
                  end
             else
                   begin
                        Estado := 4;
                   end;
  end;
  5:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPageContents.automata Estado 5');
    {$endif}
             if(t.tipo = 1) then
                   begin
                        h.page_length := StrToInt(t.token_string);
                        len_obj1:=t.token_string;
                        Estado := 6;
                   end
             else
                   begin
                        raise Exception.Create('ERROR: Arquivo corrompido.');
                        Halt(1);
                   end;
  end;
  6:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPageContents.automata Estado 6');
    {$endif}
             if(t.token_string = 'Filter') then
                   begin
                        Estado := 7;
                   end
             else if (t.token_string = 'stream') then
                  begin
                       contentsFound := true;
                       Estado := 9; // symbolic state
                  end
             else if (t.tipo = 1) then
                  begin
                      len_obj2:=t.token_string;
                      myAnLexicoLength := AnLexico.Create;
                      myAnLexicoLength.Doc := Input;
                      myAnLexicoLength.bytesRemaining:= myAnLexicoLength.Doc.Size;
                      myAnSintaticoLength := AnSintaticoLength.Create;

                      myAnSintaticoLength.len_obj1:=len_obj1;
                      myAnSintaticoLength.len_obj2:=len_obj2;

                      while ((myAnSintaticoLength.lenghtFound <> true) and
                            (myAnLexicoLength.bytesRemaining > 0)) do
                      begin
                           mytokenLength := myAnLexicoLength.getToken();
                           myAnSintaticoLength.automata(mytokenLength);
                      end;

                      if (myAnSintaticoLength.lenghtFound = false) then
                      begin
                           raise Exception.Create('ERROR: Arquivo corrompido.');
                           Halt(1);
                      end;

                      h.page_length:=myAnSintaticoLength.page_length;
                      myAnLexicoLength.Doc.Destroy;
                      Estado := 6;
                  end
             else
                   begin
                        Estado := 6;
                   end;
  end;
  7:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPageContents.automata Estado 7');
    {$endif}
             if(t.token_string = 'FlateDecode') then
                   begin
                        h.flate_decode := true;
                        Estado := 8;
                   end
             else
                   begin
                        raise Exception.Create('ERROR: Encodificacao nao suportada.');
                        Halt(1);
                   end;
  end;
  8:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPageContents.automata Estado 8');
    {$endif}
             if(t.token_string = 'stream') then
                   begin
                        contentsFound := true;
                        Estado := 9; // symbolic state
                   end
             else if (t.token_string = 'Length') then
                  begin
                       Estado := 5;
                  end
             else
                   begin
                        Estado := 8;
                   end;
  end;
  else
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoPageContents.automata Estado ELSE');
    {$endif}
    Estado := 1;
  end;
end;

procedure AnSintaticoLength.automata(t: Token);
begin
  case Estado of
  1:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoLength.automata Estado 1');
    {$endif}
             if(t.token_string = len_obj1) then
                   begin
                        Estado := 2;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  2:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoLength.automata Estado 2');
    {$endif}
             if(t.token_string = len_obj2) then
                   begin
                        Estado := 3;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  3:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoLength.automata Estado 3');
    {$endif}
             if(t.token_string = 'obj') then
                   begin
                        Estado := 4;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  4:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoLength.automata Estado 4 Length: ', StrToInt(t.token_string));
    {$endif}
             if(t.tipo = 1) then
                   begin
                        page_length:=StrToInt(t.token_string);
                        lenghtFound:=true;
                        Estado := 5; // symbolic state
                   end
             else
                   begin
                        raise Exception.Create('ERROR: Arquivo corrompido.');
                        Halt(1);
                   end;
  end;
  else
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoLength.automata Estado ELSE');
    {$endif}
    Estado := 1;
  end;
end;

function AnSintaticoCommand.automata(t: Token):Command;
begin
  c.cord_x3 := c.cord_y3;
  c.cord_y3 := c.cord_x2;
  c.cord_x2 := c.cord_y2;
  c.cord_y2 := c.cord_x;
  c.cord_x := c.cord_y;
  c.cord_y := c.my_operator;
  c.my_operator := t.token_string;
  c.code := cc_NONE;

  Codigo := false;

  case Estado of
  1:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoCommand.automata Estado 1');
    {$endif}
             if(t.tipo = 1) then // numbers 1
                   begin
                       Estado := 2;
                   end
             else if( t.token_string = 'h' ) then  // command h
                  begin
                        Estado := 9; // symbolic state
                        Estado := 1;
                        Codigo := true;
                        c.code:=cc_H_CLOSE_PATH;
                        Result:=c;
                  end
             else if( t.token_string = 's' ) then // command s
                  begin
                        Estado := 10; // symbolic state
                        Estado := 1;
                        Codigo := true;
                        c.code:=cc_hS_CLOSE_AND_END_PATH;
                        Result:=c;
                  end
             else if( t.token_string = 'S' ) then // command S
                  begin
                        Estado := 11; // symbolic state
                        Estado := 1;
                        Codigo := true;
                        c.code:=cc_S_END_PATH;
                        Result:=c;
                  end
             else if( t.token_string = 'Q' ) then // command Q
                  begin
                       Estado := 21; // symbolic state
                       Estado := 1;
                       Codigo := true;
                       c.code:=cc_RESTORE_MATRIX;
                       Result:=c;
                  end
             else if ((t.token_string = 'f') or (t.token_string = 'F')
                  or (t.token_string = 'f*') or (t.token_string = 'B')
                  or (t.token_string = 'B*') or (t.token_string = 'b')
                  or (t.token_string = 'b*') or (t.token_string = 'n')) then
                  begin
                      Estado := 12; // symbolic state
                      Estado := 1;
                      Codigo := true;
                      c.code:=cc_hS_CLOSE_AND_END_PATH; // ignore painting..
                      Result:=c;
                      //raise Exception.Create('ERROR: Prenchimento nao eh suportado.');
                      //Halt(1);
                  end
             else if ((t.token_string = 'W') or (t.token_string = 'W*')) then
                  begin
                      Estado := 13; // symbolic state
                      raise Exception.Create('ERROR: Clipping nao eh suportado.');
                      Halt(1);
                  end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  2:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoCommand.automata Estado 2');
    {$endif}
             if(t.tipo = 1) then // numbers 2
                   begin
                        Estado := 3;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  3:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoCommand.automata Estado 3');
    {$endif}
             if(t.tipo = 1) then // numbers 3
                  begin
                       Estado := 5;
                  end
             else if(t.token_string = 'l') then // command l
                  begin
                       Estado := 14; // symbolic state
                       Estado := 1;
                       c.code:=cc_l_ADD_LINE_TO_PATH;
                       Codigo := true;
                       Result:=c;
                  end
             else if(t.token_string = 'm') then  // command m
                  begin
                       Estado := 15; // symbolic state
                       Estado := 1;
                       c.code:=cc_m_START_PATH;
                       Codigo := true;
                       Result:=c;
                  end
             else
                  begin
                       Estado := 1;
                  end;
  end;
  5:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoCommand.automata Estado 5');
    {$endif}
             if(t.tipo = 1) then // numbers 4
                   begin
                       Estado := 6;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  6:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoCommand.automata Estado 6');
    {$endif}
             if(t.tipo = 1) then // numbers 5
                   begin
                       Estado := 7;
                   end
             else if( t.token_string = 'v' ) then // command v
                  begin
                       Estado := 16; // symbolic state
                       raise Exception.Create('ERROR: Curva de bezier nao eh suportada.');
                       Halt(1);
                  end
             else if( t.token_string = 'y' ) then // command y
                  begin
                       Estado := 17; // symbolic state
                       raise Exception.Create('ERROR: Curva de bezier nao eh suportada.');
                       Halt(1);
                  end
             else if( t.token_string = 're' ) then // command re
                  begin
                       Estado := 18; // symbolic state
                       raise Exception.Create('ERROR: Comando nao suportado.');
                       Halt(1);
                  end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  7:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoCommand.automata Estado 7');
    {$endif}
             if(t.tipo = 1) then // numbers 6
                   begin
                       Estado := 8;
                   end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  8:
  begin
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoCommand.automata Estado 8');
    {$endif}
             if(t.token_string = 'c') then // commmand c
                   begin
                       Estado := 19; // symbolic state
                       Estado := 1;
                       c.code:=cc_c_BEZIER_TO_X_Y_USING_X2_Y2_AND_X3_Y3;
                       Codigo := true;
                       Result:=c;
                   end
             else if( t.token_string = 'cm' ) then // command cm
                  begin
                       Estado := 20; // symbolic state
                       Estado := 1;
                       c.code:=cc_CONCATENATE_MATRIX;
                       Codigo := true;
                       Result:=c;
                  end
             else
                   begin
                        Estado := 1;
                   end;
  end;
  else
    {$ifdef FPVECTORIALDEBUG}
    WriteLn(':> AnSintaticoCommand.automata Estado ELSE');
    {$endif}
    Estado := 1;
  end;
end;

constructor AnSintaticoCommand.Create;
begin
  inherited Create;
  Estado := 1;
end;

constructor AnSintaticoPage.Create;
begin
  inherited Create;
  Estado := 1;
  pageFound := false;
end;

constructor AnSintaticoPageContents.Create;
begin
  inherited Create;
  Estado := 1;
  contentsFound := false;
  h.flate_decode := false;
end;

constructor AnSintaticoLength.Create;
begin
  inherited Create;
  Estado := 1;
  lenghtFound := false;
end;

end.

