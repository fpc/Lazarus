{**********************************************************************
          Copyright (c) PilotLogic Software House
                   All rights reserved

 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************}

unit embedded_designer_basics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LMessages, Forms,
  FormEditingIntf, IDEWindowIntf;

type
  TEmbedParentType = (ctptParent, ctptParentWindow, ctptDock, ctptNone);
  TEmbedMessageHandlerEvent = procedure(Sender: TObject; SenderCtrl: TControl; Msg: TLMessage) of object;

const
 CN_PREVIEW_FACTOR: Single = 7;

{$if defined(LCLCocoa)}
 CN_BORDER_PX=6;
{$elseif defined(LCLCarbon)}
 CN_BORDER_PX=6;
{$else}
 CN_BORDER_PX=6;
{$endif}

 CN_CAPTION_HEIGHT=18;

//=== Parent Type  ===========================

{$IF DEFINED(WINDOWS)}    //------------ Win --------------------------------

   {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)} 
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow; 
   {$ELSEIF DEFINED(LCLGTK2)}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$ELSEIF DEFINED(LCLGTK3)}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$elseif DEFINED(LCLQT)}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParent;
   {$elseif DEFINED(LCLQT5)}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParent;
   {$ELSE}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$ENDIF}

{$ELSEIF DEFINED(DARWIN)}  //------------ MacOS ------------------------------
 
   {$IF DEFINED(LCLCocoa)}            
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParent;
   {$ELSEIF DEFINED(LCLGTK2)}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$ELSEIF DEFINED(LCLGTK3)}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$elseif DEFINED(LCLQT)}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParent;
   {$elseif DEFINED(LCLQT5)}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParent;
   {$ELSEIF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$ELSE}
     CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$ENDIF}
   
{$ELSE}                  //------------- Unix --------------------------------
 
   {$IF DEFINED(LCLGTK2)}
    CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$ELSEIF DEFINED(LCLGTK3)}
    CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParent;
   {$ELSEIF DEFINED(LCLQT)}
    CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$ELSEIF DEFINED(LCLQT5)}
    CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$ELSEIF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
    CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$ELSE}
    CN_FORM_DESIGNER_PARENT_TYPE  : TEmbedParentType = ctptParentWindow;
   {$ENDIF}

{$ENDIF}

//=== OTHER ==================================

{$IF DEFINED(MSWINDOWS)} //------------ Win --------------------------------

  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
    CN_REMOVE_FORM_BORDER          = True;    //Windows Has Form Border
    CN_MUST_FIND_MAINMENU_HEIGHT   = True;    //On Windows MainMenu Height is NOT part of Total Form Height
    CN_USE_FAKE_MAINMENU           = False;
  {$ELSE}
    CN_REMOVE_FORM_BORDER          = False;   //QT and GTK2 NOT has Form Border
    CN_MUST_FIND_MAINMENU_HEIGHT   = False;   //On QT MainMenu Height is part of Total Form Height

     {$IF DEFINED(LCLGTK2) or DEFINED(GTK2) or DEFINED(LCLGTK) or DEFINED(GTK)}
       CN_USE_FAKE_MAINMENU         = True;   //On GTK Must Use Fake Menu
     {$ELSE}
       CN_USE_FAKE_MAINMENU         = False;
     {$ENDIF}

  {$ENDIF}
  
{$ELSEIF DEFINED(DARWIN)} //------------ MacOS ------------------------------

  {$IF DEFINED(LCLGTK2) or DEFINED(GTK2) or DEFINED(LCLGTK) or DEFINED(GTK)}
    CN_REMOVE_FORM_BORDER        = False;   
    CN_MUST_FIND_MAINMENU_HEIGHT = False;   
    CN_USE_FAKE_MAINMENU         = True;   //On GTK Must Use Fake Menu
  {$elseif defined(LCLCocoa)}
    CN_REMOVE_FORM_BORDER        = False;
    CN_MUST_FIND_MAINMENU_HEIGHT = True;
    CN_USE_FAKE_MAINMENU         = True;
  {$elseif defined(LCLCarbon)}
    CN_REMOVE_FORM_BORDER        = False;
    CN_MUST_FIND_MAINMENU_HEIGHT = True;
    CN_USE_FAKE_MAINMENU         = True;
  {$ELSE}
    CN_REMOVE_FORM_BORDER        = False;   //QT and GTK2 NOT has Form Border
    CN_MUST_FIND_MAINMENU_HEIGHT = False;   //On QT MainMenu Height is part of Total Form Height
    CN_USE_FAKE_MAINMENU         = False;
  {$ENDIF}

{$ELSE}                 //------------- Unix --------------------------------
  CN_REMOVE_FORM_BORDER          = False;   //QT and GTK2 NOT has Form Border
  CN_MUST_FIND_MAINMENU_HEIGHT   = False;   //On QT MainMenu Height is part of Total Form Height

  {$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2) or DEFINED(LCLGTK3)}
    CN_USE_FAKE_MAINMENU         = True;   //On GTK Must Use Fake Menu
  {$ELSE}
    CN_USE_FAKE_MAINMENU         = False;
  {$ENDIF}
  
{$ENDIF}

var
 Embedded_Updating : boolean = false;

// Functions

function Embed_IsDesignForm(fm: TCustomForm): boolean;

implementation

function Embed_IsDesignForm(fm: TCustomForm): boolean;
begin
  result:=false;
  if fm = nil then Exit;
  result:=IsFormDesign(fm);

  //....
  if result=true then Exit;
  Result := ( (csDesignInstance in fm.ComponentState) or
              (csDesigning      in fm.ComponentState) ) and
              (fm.InheritsFrom(TCustomForm) );
end;

end.
