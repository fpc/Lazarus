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
	dialogs,    messages,        utrace,       dynhasharray,
	clistbox;

implementation
end.

{ =============================================================================

  $Log$
  Revision 1.6  2001/03/27 20:55:23  lazarus
  MWE:
    * changed makefiles so that the LCL is build separate fron interfaces

  Revision 1.5  2001/03/19 18:51:57  lazarus
  MG: added dynhasharray and renamed tsynautocompletion

  Revision 1.4  2001/01/30 22:56:54  lazarus
  MWE:
    + added $mode objfpc directive

  Revision 1.3  2001/01/10 23:54:59  lazarus
  MWE:
    * Fixed make clean
    + moved allunits from exe to unit, skipping link stage

}
