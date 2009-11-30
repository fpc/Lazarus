{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit CodeTools1; 

interface

uses
    BasicCodeTools, CacheCodeTools, CCodeParserTool, CodeAtom, CodeBeautifier, 
  CodeCache, CodeCompletionTool, CodeGraph, CodeTemplatesTool, 
  CodeToolManager, CodeToolMemManager, CodeToolsConfig, CodeToolsStrConsts, 
  CodeToolsStructs, CodeTree, CustomCodeTool, DefineTemplates, DirectivesTree, 
  DirectoryCacher, EventCodeTool, ExprEval, ExtractProcTool, FileProcs, 
  FindDeclarationCache, FindDeclarationTool, H2PasTool, IdentCompletionTool, 
  KeywordFuncLists, Laz_DOM, Laz_XMLCfg, Laz_XMLRead, Laz_XMLStreaming, 
  Laz_XMLWrite, LFMTrees, LinkScanner, MethodJumpTool, MultiKeyWordListTool, 
  NonPascalCodeTools, PascalParserTool, PascalReaderTool, PPUCodeTools, 
  PPUGraph, PPUParser, ResourceCodeTool, SourceChanger, SourceLog, 
  StdCodeTools, AllCodeToolUnits, CodeIndex, FindOverloads, PascalExpr, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('CodeTools1', @Register); 
end.
