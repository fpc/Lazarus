{  $Id$  }
{
 /***************************************************************************
                            allcodetoolunits.pp

                      dummy unit to compile all units 

 /***************************************************************************
}
unit allcodetoolunits;

{$mode objfpc}{$H+}

interface

uses
  MemCheck,
  CodeToolManager, CustomCodeTool, PascalParserTool, FindDeclarationTool,
  StdCodeTools, MethodJumpTool, EventCodeTool, CodeCompletionTool, LinkScanner,
  BasicCodeTools, CodeTree, CodeAtom, SourceChanger, CodeCache,
  KeywordFuncLists, SourceLog, ExprEval, DefineTemplates, FileProcs, AVL_Tree;


implementation

end.

{ =============================================================================

  $Log$
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
