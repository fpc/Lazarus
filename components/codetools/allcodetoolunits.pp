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
  MemCheck,
  CodeToolManager, CustomCodeTool, PascalParserTool, FindDeclarationTool,
  StdCodeTools, MethodJumpTool, EventCodeTool, CodeCompletionTool, LinkScanner,
  FindDeclarationCache, BasicCodeTools, CodeTree, CodeAtom, SourceChanger,
  CodeToolMemManager, CodeCache, KeywordFuncLists, SourceLog, ExprEval,
  DefineTemplates, FileProcs, AVL_Tree, CodeToolsStrConsts,
  MultiKeyWordListTool, ResourceCodeTool;


implementation

end.

{ =============================================================================

  $Log$
  Revision 1.12  2002/12/24 12:52:53  mattias
  fixed ReAllocmem of memcheck and added memcheck for fpc 1.1

  Revision 1.11  2002/10/20 21:54:02  lazarus
  MG: fixes for 1.1

  Revision 1.10  2002/04/28 14:10:30  lazarus
  MG: fixes for saving resource files

  Revision 1.9  2002/03/28 20:31:01  lazarus
  MG: added inputhistory

  Revision 1.8  2002/01/31 16:52:24  lazarus
  MG: added base class for mem managers and started node cache

  Revision 1.7  2002/01/28 12:14:56  lazarus
  MG: fixed Makefile

  Revision 1.6  2002/01/23 22:12:54  lazarus
  MG: external tool output parsing for fpc and make messages

  Revision 1.5  2001/12/15 22:57:19  lazarus
  MG: added find declaration (not useful yet)

  Revision 1.4  2001/12/12 21:11:29  lazarus
  MG: started finddeclarationtool

  Revision 1.3  2001/11/15 09:08:35  lazarus
  MG: splitted codetools.pp, it was a monster

  Revision 1.2  2001/10/24 00:35:53  lazarus
  MG: fixes for fpc 1.1: range check errors

  Revision 1.1  2001/10/09 10:04:43  lazarus
  MG: added allcodetoolunits.pp


}
