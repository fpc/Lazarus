program GetContext;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, FileProcs, CodeCache, CodeToolManager, DefineTemplates,
  CodeToolsConfig, GetContextExample, IdentCompletionTool, FindDeclarationTool;

const
  ConfigFilename = 'codetools.config';
var
  Code: TCodeBuffer;
  CodeContexts: TCodeContextInfo;
  i: Integer;
  Filename: String;
  Item: TCodeContextInfoItem;
begin
  // setup the Options
  CodeToolBoss.SimpleInit(ConfigFilename);

  // Example: find declaration of 'TObject'

  // Step 1: load the file
  Filename:=CleanAndExpandFilename('scanexamples/getcontextexample.pas');
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // Step 2: find context
  if CodeToolBoss.FindCodeContext(Code,7,14,CodeContexts) then
  begin
    writeln('Contexts found: Count=',CodeContexts.Count);
    for i:=0 to CodeContexts.Count-1 do begin
      Item:=CodeContexts[i];
      write('i=',i,' ',ExprTypeToString(Item.Expr));
      if Item.Expr.Context.Node<>nil then
        write(' ',Item.Expr.Context.Tool.ExtractNode(Item.Expr.Context.Node,[]));
      writeln;
    end;
  end else begin
    writeln('Contexts not found: ',CodeToolBoss.ErrorMessage);
  end;
  CodeContexts.Free;
end.

