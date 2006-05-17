program GetContext;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, DefineTemplates,
  CodeToolsConfig, GetContextExample, IdentCompletionTool, FindDeclarationTool;

const
  ConfigFilename = 'codetools.config';
var
  Options: TCodeToolsOptions;
  Code: TCodeBuffer;
  CodeContexts: TCodeContextInfo;
  i: Integer;
  ExprType: TExpressionType;
begin
  // setup the Options
  Options:=TCodeToolsOptions.Create;

  // To not parse the FPC sources every time, the options are saved to a file.
  if FileExists(ConfigFilename) then
    Options.LoadFromFile(ConfigFilename);

  // setup your paths
  Options.FPCPath:='/usr/bin/ppc386';
  Options.FPCSrcDir:=ExpandFileName('~/freepascal/fpc');
  Options.LazarusSrcDir:=ExpandFileName('~/pascal/lazarus');

  // optional: ProjectDir and TestPascalFile exists only to easily test some
  // things.
  Options.ProjectDir:=GetCurrentDir+'/scanexamples/';
  Options.TestPascalFile:=Options.ProjectDir+'getcontextexample.pas';

  // init the codetools
  if not Options.UnitLinkListValid then
    writeln('Scanning FPC sources may take a while ...');
  CodeToolBoss.Init(Options);

  // save the options and the FPC unit links results.
  Options.SaveToFile(ConfigFilename);

  // Example: find declaration of 'TObject'

  // Step 1: load the file
  Code:=CodeToolBoss.LoadFile(Options.TestPascalFile,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Options.TestPascalFile);

  // Step 2: find context
  if CodeToolBoss.FindCodeContext(Code,7,14,CodeContexts) then
  begin
    writeln('Contexts found: Count=',CodeContexts.Count);
    for i:=0 to CodeContexts.Count-1 do begin
      ExprType:=CodeContexts[i];
      write('i=',i,' ',ExprTypeToString(ExprType));
      if ExprType.Context.Node<>nil then
        write(' ',ExprType.Context.Tool.ExtractNode(ExprType.Context.Node,[]));
      writeln;
    end;
  end else begin
    writeln('Contexts not found: ',CodeToolBoss.ErrorMessage);
  end;
  CodeContexts.Free;

  Options.Free;
end.

