{  $Id$  }
{
 /***************************************************************************
                               allunits.pp

                      dummy unit to compile all units 

 /***************************************************************************
}
unit allunits;
{$mode objfpc}{$H+}
interface
uses
  interfacebase, interfaces,
	buttons,    extctrls,        registry,     vclglobals,   calendar,
	clipbrd,    filectrl,        forms,        lcllinux,     spin, 
	comctrls,   graphics,        lmessages,    stdctrls, 
	controls,   imglist,         menus,        toolwin, 
	dialogs,    messages,        utrace,       dynhasharray,
	clistbox,   lazqueue;

implementation
end.

{ =============================================================================

  $Log$
  Revision 1.9  2001/12/05 18:19:11  lazarus
  MG: added calendar to allunits and removed unused vars

  Revision 1.8  2001/06/16 09:14:38  lazarus
  MG: added lazqueue and used it for the messagequeue

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
