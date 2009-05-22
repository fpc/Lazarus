{  $Id$  }
{
 /***************************************************************************
                            allcodetoolunits.pp

                      dummy unit to compile all units 

 /***************************************************************************
}
unit AllCodeToolUnits;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF VER2_3}
  MemCheck,
  {$ENDIF}
  CodeToolManager, CustomCodeTool, PascalParserTool, PascalReaderTool,
  FindDeclarationTool, StdCodeTools, MethodJumpTool, EventCodeTool,
  CodeCompletionTool, LinkScanner, FindDeclarationCache, BasicCodeTools,
  NonPascalCodeTools, CodeTree, CodeAtom, SourceChanger, CodeToolMemManager,
  CodeCache, KeywordFuncLists, SourceLog, ExprEval, DefineTemplates, FileProcs,
  CodeToolsStrConsts, DirectoryCacher, CCodeParserTool, H2PasTool,
  MultiKeyWordListTool, ResourceCodeTool, CodeToolsStructs, CacheCodeTools,
  CodeBeautifier, PPUParser, PPUGraph, CodeIndex, FindOverloads,
  // fast xml units, changes not merged in current fpc
  Laz_DOM, Laz_XMLCfg, Laz_XMLRead, Laz_XMLWrite, Laz_XMLStreaming;

implementation

end.

