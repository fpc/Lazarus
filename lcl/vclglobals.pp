{/***************************************************************************
                          vclglobals.pp
                          -------------
    begin                : Tue Apr 6 1999

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

  This unit will be removed in future. The types will be moved and/or vanish.
}
unit VCLGlobals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
//(*
// Part of LCLType
  {These should be moved to the correct area eventually}
  WS_EX_CLIENTEDGE = $200;
  WS_HSCroll = $100000;
  WS_VSCroll = $200000;
  WS_BORDER = $800000;
  WS_CLIPCHILDREN = $2000000;
  {}
//*)

  csNone = 0;
  csAlignment = 1;
//  csBox = 2;
  csButton = 3;
  csComboBox = 4;
  csCheckbox = 5;
  csEdit = 6;
  csForm= 7;
  csLabel = 8;
//  csgtkTable = 9;
  csScrollBar = 10;
  csListView = 11;
//  csMainForm = 12;
  csMemo = 13;
  csMainMenu = 14;
  csMenuBar = 15;
  csMenuItem = 16;
  csNotebook = 17;
  csFileDialog = 18;
  csRadioButton = 19;
  csScrolledWindow= 20;
  csSpinedit = 21;
  csStatusBar = 22;
//  csTable = 23;
  csToggleBox = 24;
  //csVScrollBar = 25;
//  csFrame = 26;
//  csButtonBox = 27;
//  csCanvas = 28;
  csGroupBox = 29;

//  csFont = 30;
//  csPen = 31;
//  csBrush = 32;
  //csTimer = 33;
  csPage = 34;

  csColorDialog = 35;
  csListBox = 36;
  csFontDialog = 37;
  csProgressBar = 38;
  csTrackBar = 39;
  csWinControl = 40;
  csFixed = csWinControl; //TODO remove
  csImage = 41;
  csToolbar = 42;
  csToolButton = 43;
  csBitBtn = 44;
  csCListBox = 45;
  csSpeedButton = 46;
  csPopupMenu = 47;
  csHintWindow = 48;

  csCalendar = 49;

  csArrow = 50;
  csPanel = 51;
  csScrollBox = 52;
  
  csCheckListBox = 53;
  csPairSplitter = 54;
  csPairSplitterSide = 55;

  csOpenFileDialog = 56;
  csSaveFileDialog = 57;
  csSelectDirectoryDialog = 58;
  csPreviewFileControl = 59;
  csPreviewFileDialog = 60;

  csNonLCL = 61; // for non LCL controls, that create their own handles


const
  {Mouse message key states}
  MK_LBUTTON  = 1;
  MK_RBUTTON = 2;
  MK_SHIFT = 4;
  MK_CONTROL = 8;
  MK_MBUTTON = $10;


Function CS_To_String(CompStyle: Integer): String;


implementation

{------------------------------------------------------------------------------
  Function: CS_To_String
  Params: CompStyle - Component Style
  Returns: The component style name

  Converts a component style identIfier into the correct component style name
 ------------------------------------------------------------------------------}
Function CS_To_String(CompStyle: Integer): String;
Begin
  Case CompStyle of
    csNone:
      Result := 'csNone';
    csAlignment:
      Result := 'csAlignment';
//    csBox:
//      Result := 'csBox';
    csButton:
      Result := 'csButton';
    csComboBox:
      Result := 'csComboBox';
    csCheckbox:
      Result := 'csCheckbox';
    csEdit:
      Result := 'csEdit';
    csForm:
      Result := 'csForm';
    csLabel:
      Result := 'csLabel';
//    csGTKTable:
//      Result := 'csGTKTable';
    csScrollBar:
      Result := 'csScrollBar';
    csListView:
      Result := 'csListView';
//    csMainForm:
//      Result := 'csMainForm';
    csMemo:
      Result := 'csMemo';
    csMainMenu:
      Result := 'csMainMenu';
    csMenuBar:
      Result := 'csMenuBar';
    csMenuItem:
      Result := 'csMenuItem';
    csNotebook:
      Result := 'csNotebook';
    csFileDialog:
      Result := 'csFileDialog';
    csOpenFileDialog:
      Result := 'csOpenFileDialog';
    csSaveFileDialog:
      Result := 'csSaveFileDialog';
    csSelectDirectoryDialog:
      Result := 'csSelectDirectoryDialog';
    csRadioButton:
      Result := 'csRadioButton';
    csScrolledWinDow:
      Result := 'csScrolledWinDow';
    csSpinEdit:
      Result := 'csSpinEdit';
    csStatusBar:
      Result := 'csStatusBar';
//    csTable:
//      Result := 'csTable';
    csToggleBox:
      Result := 'csToggleBox';
//    25: //csVScrollBar
//      Result := 'csVScrollBar';
//    csFrame:
//      Result := 'csFrame';
//    csButtonBox:
//      Result := 'csButtonBox';
//    csCanvas:
//      Result := 'csCanvas';
    csGroupBox:
      Result := 'csGroupBox';
//    csFont:
//      Result := 'csFont';
//    csPen:
//      Result := 'csPen';
//    csBrush:
//      Result := 'csBrush';
//    33: //csTimer
//      Result := 'csTimer';
    csPage:
      Result := 'csPage';
    csColorDialog:
      Result := 'csColorDialog';
    csListBox:
      Result := 'csListBox';
    csFontDialog:
      Result := 'csFontDialog';
    csProgressBar:
      Result := 'csProgressBar';
    csTrackBar:
      Result := 'csTrackBar';
    csFixed:
      Result := 'csFixed';
    csImage:
      Result := 'csImage';
    csToolbar:
      Result := 'csToolbar';
    csToolButton:
      Result := 'csToolButton';
    csBitBtn:
      Result := 'csBitBtn';
    csCListBox:
      Result := 'csCListBox';
    csSpeedButton:
      Result := 'csSpeedButton';
    csPopupMenu:
      Result := 'csPopupMenu';
    csHintWinDow:
      Result := 'csHintWinDow';
    csCalendar:
      Result := 'csCalendar';
    csArrow:
      Result := 'csArrow';
    csPanel:
      Result := 'csPanel';
    csScrollBox:
      Result := 'csScrollBox';
    csCheckListBox:
      Result := 'csCheckListBox';
    csPairSplitter:
      Result := 'csPairSplitter';
    csPairSplitterSide:
      Result := 'csPairSplitterSide';
    csPreviewFileControl:
      Result := 'csPreviewFileControl';
    csPreviewFileDialog:
      Result := 'csPreviewFileDialog';
    csNonLCL:
      Result := 'csNonLCL';
    Else
      Result := Format('Unknown component style %D', [CompStyle]);
  End; {Case}
End;

end.

