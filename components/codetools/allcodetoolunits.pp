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
  CodeToolManager, CodeTools, LinkScanner, BasicCodeTools, SourceChanger,
  CodeCache, KeywordFuncLists, SourceLog, ExprEval, AVL_Tree, DefineTemplates;


implementation

end.

{ =============================================================================

  $Log$
  Revision 1.2  2001/10/24 00:35:53  lazarus
  MG: fixes for fpc 1.1: range check errors

  Revision 1.1  2001/10/09 10:04:43  lazarus
  MG: added allcodetoolunits.pp


}
