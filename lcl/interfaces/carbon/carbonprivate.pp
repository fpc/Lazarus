{ $Id: $
                  ----------------------------------------
                  carbonprivate.pp  -  Carbon internal classes
                  ----------------------------------------

 @created(Wed Aug 26st WET 2005)
 @lastmod($Date: 2005-09-10 18:07:05 +0200 (Sat, 10 Sep 2005) $)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains the private classhierarchy for the carbon implemetations
 This hierarchy reflects (more or less) the carbon widget hierarchy

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit CarbonPrivate;
{$mode objfpc}{$H+}

interface

uses
  // libs
  FPCMacOSAll, CarbonUtils, 
  // LCL
  LCLType, LMessages, LCLProc, Controls, Classes, SysUtils, Forms,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  CarbonDef, CarbonProc;


type
  { TCarbonPrivate }
  { Generic base class, don't know if it is needed }

  TCarbonPrivate = class(TWSPrivate)
  private
  protected
  public
  end;

  { TCarbonPrivateHandle }
  { Generic base class for handle based widgets }

  TCarbonPrivateHandle = class(TCarbonPrivate)
  private
  protected
  public
    class procedure RegisterEvents(AInfo: PWidgetInfo); virtual;
    class procedure UnregisterEvents; virtual;
  end;
  TCarbonPrivateHandleClass = class of TCarbonPrivateHandle;

  { TCarbonPrivateHiObject }
  { Private class for HIObject based widgets }
  
  TCarbonPrivateHiObject = class(TCarbonPrivateHandle)
  private
  protected
  public
  end;

  { TCarbonPrivateHiView }
  { Private class for HIView based widgets }
  
  TCarbonPrivateHiView = class(TCarbonPrivateHiObject)
  private
  protected
//    class procedure InstallControlHandler(AInfo: PWidgetInfo; AClass: UInt32; AKind: UInt32; AHandler: Pointer; var AUPP: EventHandlerUPP);
  public
    class procedure RegisterEvents(AInfo: PWidgetInfo); override;
    class procedure UnregisterEvents; override;
  end;
  TCarbonPrivateHiViewClass = class of TCarbonPrivateHiView;
  
  { TCarbonPrivateWindow }
  { Private class for window based widgets }
  
  TCarbonPrivateWindow = class(TCarbonPrivateHandle)
  private
  protected
  public
    class procedure RegisterEvents(AInfo: PWidgetInfo); override;
    class procedure UnregisterEvents; override;
  end;
  TCarbonPrivateWindowClass = class of TCarbonPrivateWindow;
  
  TCarbonPrivateCheckBox = class(TCarbonPrivateHiView)
  private
  protected
  public
    class procedure RegisterEvents(AInfo: PWidgetInfo); override;
    class procedure UnregisterEvents; override;
  end;
  TCarbonPrivateCheckBoxClass = class of TCarbonPrivateCheckBox;

  

implementation  

{$I carbonprivatehiview.inc}
{$I carbonprivatewindow.inc}
{$I carbonprivatecheckbox.inc}

// move to inc
class procedure TCarbonPrivateHandle.RegisterEvents(AInfo: PWidgetInfo);
begin
end;

class procedure TCarbonPrivateHandle.UnregisterEvents;
begin
end;


end.
