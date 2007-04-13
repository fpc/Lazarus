{
 /***************************************************************************
                    carbonconsts.pp  -  Carbon string constants
 ***************************************************************************/

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

unit CarbonConsts;

{$mode objfpc}{$H+}

interface

const
  SCreateWidget = 'CreateWidget';
  SDestroyWidget = 'DestroyWidget';
  SInvalidate = 'Invalidate';
  SEnable = 'Enable';
  SSetColor = 'SetColor';
  SSetText = 'SetText';
  
  SCreate = 'Create';
  SDestroy = 'Destroy';
  
  SSetControlProp = 'SetControlProperty';

  SSetFontStyle = 'SetControlFontStyle';
  
  SCreateBevelButton = 'SCreateBevelButtonControl';

  SGetWindowBounds = 'GetWindowBounds';
  SViewForMouse = 'HIViewGetViewForMouseEvent';
  SViewVisible  = 'HIViewSetVisible';
  SViewConvert = 'HIViewConvertPoint';
  SViewRender  = 'HIViewRenderle';
  SViewNeedsDisplay = 'HiViewSetNeedsDisplay';
  SViewNeedsDisplayRect = 'HiViewSetNeedsDisplayInRect';
  SViewAddView = 'HIViewAddSubview';
  
  SEnableControl = 'EnableControl';
  SDisableControl = 'DisableControl';
  
  SChangeWindowAttrs = 'ChangeWindowAttributes';
  
  SGetData = 'GetControlData';
  SSetData = 'GetControlData';
  
  SGetEvent = 'GetEventParameter';
  SSetEvent = 'SetEventParameter';
  SInstallEvent = 'InstallEventHandler';
  
  SControlPart = 'kEventParamControlPart';
  SKeyModifiers = 'kEventParamKeyModifiers';
  
  SControlFont = 'kControlFontStyleTag';
  
  SCreateStyle = 'ATSUCreateStyle';
  SDisposeStyle = 'ATSUDisposeStyle';

implementation

end.
