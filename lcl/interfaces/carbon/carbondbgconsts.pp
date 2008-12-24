{
 /***************************************************************************
                    carbondbgconsts.pp  -  Carbon string constants
 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 }

unit CarbonDbgConsts;

{$mode objfpc}{$H+}

interface

const
  SCarbonWSPrefix = 'TCarbonWidgetSet.';
   
  SCreateWidget = 'CreateWidget';
  SDestroyWidget = 'DestroyWidget';
  SInvalidate = 'Invalidate';
  SEnable = 'Enable';
  SSetFocus = 'SetFocus';
  SSetBounds = 'SetBounds';
  SSetColor = 'SetColor';
  SGetText = 'GetText';
  SSetText = 'SetText';
  
  SShowModal = 'ShowModal';
  
  SCreate = 'Create';
  SDestroy = 'Destroy';
  
  SGetCurrentProc = 'GetCurrentProcess';
  SShowHideProc = 'ShowHideProcess';
  
  SGetKeyboardFocus = 'GetKeyboardFocus';
  SSetUserFocusWindow = 'SetUserFocusWindow';
  
  SSetControlProp = 'SetControlProperty';

  SSetFontStyle = 'SetControlFontStyle';
  
  SCreateBevelButton = 'SCreateBevelButtonControl';

  SActivateWindow = 'ActivateWindow';
  SGetWindowBounds = 'GetWindowBounds';
  
  SViewForMouse = 'HIViewGetViewForMouseEvent';
  SViewVisible  = 'HIViewSetVisible';
  SViewConvert = 'HIViewConvertPoint';
  SViewRender  = 'HIViewRender';
  SViewFrame = 'HIViewSetFrame';
  SViewNeedsDisplay = 'HiViewSetNeedsDisplay';
  SViewNeedsDisplayRect = 'HiViewSetNeedsDisplayInRect';
  SViewAddView = 'HIViewAddSubview';
  SViewSetScrollBarAutoHide = 'HIScrollViewSetScrollBarAutoHide';
  
  SSetTXNControls = 'TXNSetTXNObjectControls';
  
  SEnableControl = 'EnableControl';
  SDisableControl = 'DisableControl';
  
  SChangeMenuItemAttrs = 'ChangeMenuItemAttributes';
  SChangeMenuAttrs = 'ChangeMenuAttributes';
  
  SSetMenuTitle = 'SetMenuTitleWithCFString';
  
  SChangeWindowAttrs = 'ChangeWindowAttributes';
  SSetModality = 'SetWindowModality';
  
  SGetData = 'GetControlData';
  SSetData = 'SetControlData';
  
  SGetEvent = 'GetEventParameter';
  SSetEvent = 'SetEventParameter';
  SInstallEvent = 'InstallEventHandler';
  
  SControlPart = 'kEventParamControlPart';
  SKeyModifiers = 'kEventParamKeyModifiers';
  
  SControlFont = 'kControlFontStyleTag';
  
  SGetUnjustifiedBounds = 'ATSUGetUnjustifiedBounds';
  SCreateStyle = 'ATSUCreateStyle';
  SDisposeStyle = 'ATSUDisposeStyle';


implementation

end.
