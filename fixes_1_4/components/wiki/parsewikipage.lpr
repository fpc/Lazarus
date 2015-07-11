program parsewikipage;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, WikiParser;

type

  { TWikiParse }

  TWikiParse = class
    procedure OnToken(Token: TWPToken);
  public
    procedure Parse(Filename: string);
  end;

{ TWikiParse }

procedure TWikiParse.OnToken(Token: TWPToken);
var
  TextToken: TWPTextToken;
begin
  exit;
  write('TWikiParse.OnToken ',dbgs(Token.Token));
  if Token is TWPTextToken then begin
    TextToken:=TWPTextToken(Token);
    write(': "',copy(TextToken.Page.Src,TextToken.StartPos,TextToken.EndPos-TextToken.StartPos),'"');
  end;
  writeln;
end;

procedure TWikiParse.Parse(Filename: string);
var
  Page: TWikiPage;
begin
  Page:=TWikiPage.Create;
  try
    Page.LoadFromFile(Filename);
    writeln(ExtractFileName(Filename),' ID="',Page.ID,'" Title="',Page.Title,'" Revision="',Page.Revision,'" timestamp="',Page.TimeStamp,'"');
    Page.Parse(@OnToken);
  finally
    Page.Free;
  end;
end;

var
  i: Integer;
  Parser: TWikiParse;
begin
  Parser:=TWikiParse.Create;
  for i:=1 to Paramcount do begin
    Parser.Parse(ParamStr(i));
  end;
end.

