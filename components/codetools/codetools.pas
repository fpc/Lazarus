{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CodeTools; 

interface

uses
  BasicCodeTools, CacheCodeTools, CCodeParserTool, CodeAtom, CodeBeautifier, 
  CodeCache, CodeCompletionTool, CodeGraph, CodeIndex, CodeTemplatesTool, 
  CodeToolManager, CodeToolMemManager, CodeToolsConfig, CodeToolsStrConsts, 
  CodeToolsStructs, CodeTree, CustomCodeTool, DefineTemplates, DirectivesTree, 
  DirectoryCacher, EventCodeTool, ExprEval, ExtractProcTool, FileProcs, 
  FindDeclarationCache, FindDeclarationTool, FindOverloads, H2PasTool, 
  IdentCompletionTool, KeywordFuncLists, LFMTrees, LinkScanner, 
  MethodJumpTool, MultiKeyWordListTool, NonPascalCodeTools, PascalParserTool, 
  PascalReaderTool, PPUCodeTools, PPUGraph, PPUParser, ResourceCodeTool, 
  SourceChanger, SourceLog, StdCodeTools, OtherIdentifierTree, 
  CodeToolsCfgScript, CTXMLFixFragment, CTUnitGraph, ChangeDeclarationTool, 
  CodeToolsFPCMsgs, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('CodeTools', @Register); 
end.
