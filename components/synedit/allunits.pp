{  $Id$  }
{
 /***************************************************************************
                               allunits.pp

                      dummy unit to compile all units 

 /***************************************************************************
}
unit allunits;
{$mode objfpc}
interface
uses
  synedit, syneditautocomplete, synedithighlighter, syneditkeycmds,
  syneditmiscclasses, syneditmiscprocs, syneditsearch, syneditstrconst,
  synedittextbuffer, synedittypes, synhighlighterpas, syntextdrawer;
	
implementation
end.

{ =============================================================================

  $Log$
  Revision 1.2  2001/01/30 22:55:00  lazarus
  MWE:
    + Added $mode objfpc directive

  Revision 1.1  2001/01/28 16:16:11  lazarus
  MWE:
    + Added synedit to the components


}
