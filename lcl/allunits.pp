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
  interfacebase, interfaces,
	buttons,    extctrls,        registry,     vclglobals, 
	clipbrd,    filectrl,        forms,        lcllinux,   spin, 
	comctrls,   graphics,        lmessages,    stdctrls, 
	controls,   imglist,         menus,        toolwin, 
	dialogs,    messages,        utrace;

implementation
end.

{ =============================================================================

  $Log$
  Revision 1.4  2001/01/30 22:56:54  lazarus
  MWE:
    + added $mode objfpc directive

  Revision 1.3  2001/01/10 23:54:59  lazarus
  MWE:
    * Fixed make clean
    + moved allunits from exe to unit, skipping link stage

}
