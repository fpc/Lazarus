{
 /***************************************************************************
                    CarbonInt.pas  -  CarbonInterface Object
                    ----------------------------------------

                 Initial Revision  : Mon August 6th CST 2004


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 }

unit CarbonInt;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

{off $Define Critical_Sections_Support}

uses
  // rtl+ftl
  Classes, SysUtils, FPCAdds,
  // interfacebase
  InterfaceBase,
  // carbon bindings

  { ToDo }

  // LCL
  Controls, Forms, Dialogs, LCLStrConsts, LMessages, LCLProc, LCLIntf, LCLType,
  GraphType, GraphMath, Graphics, Menus;


type
  TCarbonWidgetSet = class(TWidgetSet)
  protected
    procedure PassCmdLineOptions; override;
  public
    constructor Create;
    destructor Destroy; override;
    function  IntSendMessage3(LM_Message : Integer; Sender : TObject;
                              data : pointer) : integer; override;
    procedure HandleEvents; override;
    procedure WaitMessage; override;
    procedure SendCachedLCLMessages; override;
    procedure AppTerminate; override;
    procedure AppInit; override;
    procedure AppMinimize; override;

    // create and destroy
    function CreateComponent(Sender : TObject): THandle; override;
    function CreateTimer(Interval: integer; TimerFunc: TFNTimerProc) : integer; override;
    function DestroyTimer(TimerHandle: integer) : boolean; override;

    // the winapi compatibility methods
    {$I carbonwinapih.inc}
    // the extra LCL interface methods
    {$I carbonlclintfh.inc}

  public
  end;

var
  CarbonWidgetSet: TCarbonWidgetSet;

implementation

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as litle as posible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// CarbonWSActnList,
// CarbonWSArrow,
// CarbonWSButtons,
// CarbonWSCalendar,
// CarbonWSCheckLst,
// CarbonWSCListBox,
// CarbonWSComCtrls,
// CarbonWSControls,
// CarbonWSDbCtrls,
// CarbonWSDBGrids,
// CarbonWSDialogs,
// CarbonWSDirSel,
// CarbonWSEditBtn,
// CarbonWSExtCtrls,
// CarbonWSExtDlgs,
// CarbonWSFileCtrl,
// CarbonWSForms,
// CarbonWSGrids,
// CarbonWSImgList,
// CarbonWSMaskEdit,
// CarbonWSMenus,
// CarbonWSPairSplitter,
// CarbonWSSpin,
// CarbonWSStdCtrls,
// CarbonWSToolwin,
////////////////////////////////////////////////////
  Math, Buttons, StdCtrls, PairSplitter, ComCtrls, CListBox, Calendar, Arrow,
  Spin, CommCtrl, ExtCtrls, FileCtrl, LResources;
  
// the implementation of the utility methods
{$I carbonobject.inc}
// the implementation of the winapi compatibility methods
{$I carbonwinapi.inc}
// the implementation of the extra LCL interface methods
{$I carbonlclintf.inc}


procedure InternalInit;
begin
end;

procedure InternalFinal;
begin
end;


initialization
  {$I carbonimages.lrs}
  InternalInit;

finalization
  InternalFinal;

end.

{
  $Log$
  Revision 1.1  2004/09/06 22:24:52  mattias
  started the carbon LCL interface

}
