{ $Id$}
{
 *****************************************************************************
 *                               WSExtCtrls.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit WSExtCtrls;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  LCLProc, Controls, ExtCtrls, Classes, ImgList, Graphics,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSStdCtrls, WSFactory;

type
  { TWSCustomPage }

  TWSCustomPageClass = class of TWSCustomPage;
  TWSCustomPage = class(TWSWinControl)
  published
    class procedure UpdateProperties(const ACustomPage: TCustomPage); virtual;
  end;

  { TWSCustomNotebook }

  TWSCustomNotebook = class(TWSWinControl)
  published
    class procedure AddPage(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AIndex: integer); virtual;
    class procedure MovePage(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const NewIndex: integer); virtual;
    class procedure RemovePage(const ANotebook: TCustomNotebook; const AIndex: integer); virtual;

    class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; virtual;
    class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; virtual;
    class function GetPageRealIndex(const ANotebook: TCustomNotebook; AIndex: Integer): Integer; virtual;
    class function GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer; virtual;
    class function GetTabRect(const ANotebook: TCustomNotebook; const AIndex: Integer): TRect; virtual;
    class function GetCapabilities: TNoteBookCapabilities; virtual;
    class procedure SetImageList(const ANotebook: TCustomNotebook; const AImageList: TCustomImageList); virtual;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); virtual;
    class procedure SetTabCaption(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AText: string); virtual;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); virtual;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); virtual;
    class procedure UpdateProperties(const ANotebook: TCustomNotebook); virtual;
  end;
  TWSCustomNotebookClass = class of TWSCustomNotebook;

  { TWSPage }

  TWSPage = class(TWSCustomPage)
  published
  end;

  { TWSNotebook }

  TWSNotebook = class(TWSCustomNotebook)
  published
  end;

  { TWSShape }

  TWSShape = class(TWSGraphicControl)
  published
  end;

  { TWSCustomSplitter }

  TWSCustomSplitter = class(TWSCustomControl)
  published
  end;

  { TWSSplitter }

  TWSSplitter = class(TWSCustomSplitter)
  published
  end;

  { TWSPaintBox }

  TWSPaintBox = class(TWSGraphicControl)
  published
  end;

  { TWSCustomImage }

  TWSCustomImage = class(TWSGraphicControl)
  published
  end;

  { TWSImage }

  TWSImage = class(TWSCustomImage)
  published
  end;

  { TWSBevel }

  TWSBevel = class(TWSGraphicControl)
  published
  end;

  { TWSCustomRadioGroup }

  TWSCustomRadioGroup = class(TWSCustomGroupBox)
  published
  end;

  { TWSRadioGroup }

  TWSRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TWSCustomCheckGroup }

  TWSCustomCheckGroup = class(TWSCustomGroupBox)
  published
  end;

  { TWSCheckGroup }

  TWSCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TWSCustomLabeledEdit }

  TWSCustomLabeledEdit = class(TWSCustomEdit)
  published
  end;

  { TWSLabeledEdit }

  TWSLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TWSCustomPanel }

  TWSCustomPanel = class(TWSCustomControl)
  published
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSPanel }

  TWSPanel = class(TWSCustomPanel)
  end;

  { TWSCustomTrayIcon }

  TWSCustomTrayIcon = class(TWSLCLComponent)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; virtual;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; virtual;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); virtual;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; virtual;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; virtual;
    class function GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas; virtual;
  end;
  TWSCustomTrayIconClass = class of TWSCustomTrayIcon;

  { WidgetSetRegistration }

  procedure RegisterCustomPage;
  procedure RegisterCustomNotebook;
  procedure RegisterShape;
  procedure RegisterCustomSplitter;
  procedure RegisterPaintBox;
  procedure RegisterCustomImage;
  procedure RegisterBevel;
  procedure RegisterCustomRadioGroup;
  procedure RegisterCustomCheckGroup;
  procedure RegisterCustomLabeledEdit;
  procedure RegisterCustomPanel;
  procedure RegisterCustomTrayIcon;

implementation

{ TWSCustomPage }

class procedure TWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
begin
end;

{ TWSCustomNotebook }

{ -----------------------------------------------------------------------------
  Method: TWSCustomNotebook.AddPage
  Params: ANotebook - A notebook control
          AChild - Page to insert
          AIndex  - The position in the notebook to insert the page
  Returns: Nothing

  Adds a new page to a notebook
 ------------------------------------------------------------------------------}
class procedure TWSCustomNotebook.AddPage(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AIndex: integer);
begin
end;

{------------------------------------------------------------------------------
  Method: TWSCustomNotebook.MovePage
  Params: ANotebook - The notebook control
          AChild    - The page to move
          NewIndex  - The new index of the page
  Returns: Nothing

  Moves a page in a notebook control
 ------------------------------------------------------------------------------}
class procedure TWSCustomNotebook.MovePage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const NewIndex: integer);
begin
end;

{------------------------------------------------------------------------------
  Method: TWSCustomNotebook.RemovePage
  Params: ANotebook - The notebook control
          AIndex    - The index of the page to delete
  Returns: Nothing

  Removes a page from a notebook control
 ------------------------------------------------------------------------------}
class procedure TWSCustomNotebook.RemovePage(const ANotebook: TCustomNotebook; const AIndex: integer);
begin
end;

{-------------------------------------------------------------------------------
  function TWSCustomNotebook.GetNotebookMinTabHeight(
    const AWinControl: TWinControl): integer;

  Returns the minimum height of the horizontal tabs of a notebook. That is the
  Notebook with TabPosition in [tpTop,tpBottom] without the client panel.
-------------------------------------------------------------------------------}
class function  TWSCustomNotebook.GetNotebookMinTabHeight(
  const AWinControl: TWinControl): integer;
begin
  Result:=30;
end;

{-------------------------------------------------------------------------------
  function TWSCustomNotebook.GetNotebookMinTabWidth(
    const AWinControl: TWinControl): integer;

  Returns the minimum width of the vertical tabs of a notebook. That is the
  Notebook with TabPosition in [tpLeft,tpRight] without the client panel.
-------------------------------------------------------------------------------}
class function TWSCustomNotebook.GetNotebookMinTabWidth(const AWinControl: TWinControl
  ): integer;
begin
  Result:=60;
end;

class function TWSCustomNotebook.GetPageRealIndex(const ANotebook: TCustomNotebook;
  AIndex: Integer): Integer;
begin
  Result := AIndex;
end;

class function TWSCustomNotebook.GetTabIndexAtPos(const ANotebook: TCustomNotebook;
  const AClientPos: TPoint): integer;
begin
  Result := -1;
end;

class function TWSCustomNotebook.GetTabRect(const ANotebook: TCustomNotebook;
  const AIndex: Integer): TRect;
begin
  Result := Rect(-1,-1,-1,-1);
end;

class function TWSCustomNotebook.GetCapabilities: TNoteBookCapabilities;
begin
  Result:=[];
end;

class procedure TWSCustomNotebook.SetImageList(
  const ANotebook: TCustomNotebook; const AImageList: TCustomImageList);
begin
end;

class procedure TWSCustomNotebook.SetPageIndex(const ANotebook: TCustomNotebook;
  const AIndex: integer);
begin
end;

class procedure TWSCustomNotebook.SetTabCaption(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const AText: string);
begin
end;

class procedure TWSCustomNotebook.SetTabPosition(const ANotebook: TCustomNotebook;
  const ATabPosition: TTabPosition);
begin
end;

class procedure TWSCustomNotebook.ShowTabs(const ANotebook: TCustomNotebook;
  AShowTabs: boolean);
begin
end;

class procedure TWSCustomNotebook.UpdateProperties(
  const ANotebook: TCustomNotebook);
begin

end;

{ TWSCustomTrayIcon }

class function TWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

class function TWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

class procedure TWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
begin

end;

{*******************************************************************
*  TWSCustomTrayIcon.ShowBalloonHint ()
*
*  RETURNS:        False if we should use the popupnotifier to implement this method
*                  True if a platform-specific baloon is implemented
*
*******************************************************************}
class function TWSCustomTrayIcon.ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

class function TWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
end;

class function TWSCustomTrayIcon.GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas;
begin
  Result := ATrayIcon.Icon.Canvas;
end;

{ WidgetSetRegistration }

procedure RegisterCustomPage;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomPage;
//  if not WSRegisterCustomPage then
//    RegisterWSComponent(TCustomPage, TWSCustomPage);
  Done := True;
end;

procedure RegisterCustomNotebook;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomNotebook then
    RegisterWSComponent(TCustomNotebook, TWSCustomNotebook);
  Done := True;
end;

procedure RegisterShape;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterShape;
//  if not WSRegisterShape then
//    RegisterWSComponent(TShape, TWSShape);
  Done := True;
end;

procedure RegisterCustomSplitter;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomSplitter;
//  if not WSRegisterCustomSplitter then
//    RegisterWSComponent(TCustomSplitter, TWSCustomSplitter);
  Done := True;
end;

procedure RegisterPaintBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPaintBox;
//  if not WSRegisterPaintBox then
//    RegisterWSComponent(TPaintBox, TWSPaintBox);
  Done := True;
end;

procedure RegisterCustomImage;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomImage;
//  if not WSRegisterCustomImage then
//    RegisterWSComponent(TCustomImage, TWSCustomImage);
  Done := True;
end;

procedure RegisterBevel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterBevel;
//  if not WSRegisterBevel then
//    RegisterWSComponent(TBevel, TWSBevel);
  Done := True;
end;

procedure RegisterCustomRadioGroup;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomRadioGroup;
//  if not WSRegisterCustomRadioGroup then
//    RegisterWSComponent(TCustomRadioGroup, TWSCustomRadioGroup);
  Done := True;
end;

procedure RegisterCustomCheckGroup;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCheckGroup;
//  if not WSRegisterCustomCheckGroup then
//    RegisterWSComponent(TCustomCheckGroup, TWSCustomCheckGroup);
  Done := True;
end;

procedure RegisterCustomLabeledEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomLabeledEdit;
//  if not WSRegisterCustomLabeledEdit then
//    RegisterWSComponent(TCustomLabeledEdit, TWSCustomLabeledEdit);
  Done := True;
end;

procedure RegisterCustomPanel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomPanel;
//  if not WSRegisterCustomPanel then
//    RegisterWSComponent(TCustomPanel, TWSCustomPanel);
  Done := True;
end;

procedure RegisterCustomTrayIcon;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomTrayIcon then
    RegisterWSComponent(TCustomTrayIcon, TWSCustomTrayIcon);
  Done := True;
end;

{ TWSCustomPanel }

class function TWSCustomPanel.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clBtnFace,
 { dctFont  } clBtnText
  );
begin
  Result := DefColors[ADefaultColorType];
end;

end.
