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
  CodeToolManager, AVL_Tree, KeywordFuncLists, ExprEval, LinkScanner, SourceLog,
  BasicCodeTools, CodeCache, SourceChanger, CodeTools, DefineTemplates;


implementation

end.

{ =============================================================================

  $Log$
  Revision 1.1  2001/10/09 10:04:43  lazarus
  MG: added allcodetoolunits.pp


}
