unit wmdump;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, cgiModules; 

type

  { TDemoModule }

  TDemoModule = class(TCGIDatamodule)
    procedure DemoModuleCGIRequest(Sender: TObject);
  private
    procedure EmitRequestVariables;
    procedure EmitServerVariables;
    { private declarations }
  public
    { public declarations }
    procedure EmitVariable(Const VarName,VarValue : String);
  end; 

var
  DemoModule: TDemoModule;

implementation

uses cgiapp;

{ TDemoModule }

{
  The OnCGIRequest handler is called to handle the request
}
procedure TDemoModule.DemoModuleCGIRequest(Sender: TObject);

begin
  // Emit content type (See ContentType property of the module)
  EmitContentType;
  // AddResponseLn sends one line of text to the web client.
  AddResponseLn('<HTML>');
  AddResponseLn('<TITLE>CGI Server environment</TITLE>');
  AddResponseLn('<BODY>');
  EmitServerVariables;
  AddResponseLn('<HR/>');
  EmitRequestVariables;
end;

procedure TDemoModule.EmitServerVariables;

begin
  AddResponseLn('<H1>CGI Server environment:</H1>');
  AddResponseLn('<TABLE>');
  AddResponseLn('<TR><TH>Variable</TH><TH>Value</TH></TR>');
  // Server environment is accessible as properties of the Application class.
  // The same list can be retrieved as a name=value stringlist with the
  // GetCGIVarList call.
  EmitVariable('AuthType',Application.AuthType);
  EmitVariable('ContentLength',IntToStr(Application.ContentLength));
  EmitVariable('ContentType',Application.ContentType);
  EmitVariable('GatewayInterface',Application.GatewayInterface);
  EmitVariable('PathInfo',Application.PathInfo);
  EmitVariable('PathTranslated',Application.PathTranslated);
  EmitVariable('QueryString',Application.QueryString);
  EmitVariable('RemoteAddress',Application.RemoteAddress);
  EmitVariable('RemoteHost',Application.RemoteHost);
  EmitVariable('RemoteIdent',Application.RemoteIdent);
  EmitVariable('RemoteUser',Application.RemoteUser);
  EmitVariable('RequestMethod',Application.RequestMethod);
  EmitVariable('ScriptName',Application.ScriptName);
  EmitVariable('ServerName',Application.ServerName);
  EmitVariable('ServerPort',IntToStr(Application.ServerPort));
  EmitVariable('ServerProtocol',Application.ServerProtocol);
  EmitVariable('ServerSoftware',Application.ServerSoftware);
  EmitVariable('HTTPAccept',Application.HTTPAccept);
  EmitVariable('HTTPAcceptCharset',Application.HTTPAcceptCharset);
  EmitVariable('HTTPAcceptEncoding',Application.HTTPAcceptEncoding);
  EmitVariable('HTTPIfModifiedSince',Application.HTTPIfModifiedSince);
  EmitVariable('HTTPReferer',Application.HTTPReferer);
  EmitVariable('HTTPUserAgent',Application.HTTPUserAgent);
  EmitVariable('Email',Application.Email);
  EmitVariable('Administrator',Application.Administrator);

//  EmitVariable('',Application.);
end;

procedure TDemoModule.EmitRequestVariables;

Var
  L : TStringList;
  I : Integer;
  N,V : String;
  
begin
  AddResponseLn('</TABLE>');
  AddResponseLn('<H1>Query variables:</H1>');
  AddResponseLn('<TR><TH>Variable</TH><TH>Value</TH></TR>');
  L:=TStringList.Create;
  try
    // Retrieve parsed list of variables as name=value pairs
    Application.GetRequestVarList(L,False);
    For I:=0 to L.Count-1 do
      begin
      L.GetNameValue(I,N,V);
      EmitVariable(N,V);
      end;
    // Alternatively,
    // Application.RequestVariables[Varname : string] gives named acces to the variables
    // Application.RequestvariableCount returns the number of variables.
  finally
    L.Free;
  end;
  AddResponseLn('</TABLE>');
end;
  
procedure TDemoModule.EmitVariable(const VarName, VarValue: String);
begin
  AddResponseLn(Format('<TR><TD>%s</TD><TD>%s</TD></TR>',[VarName,VarValue]));
end;

initialization
  {$I wmdump.lrs}

end.

