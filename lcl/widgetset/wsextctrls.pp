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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  WSLCLClasses, WSControls, WSStdCtrls;

type
  { TWSCustomPage }

  TWSCustomPageClass = class of TWSCustomPage;
  TWSCustomPage = class(TWSWinControl)
    class procedure UpdateProperties(const ACustomPage: TCustomPage); virtual;
  end;

  { TWSCustomNotebook }

  TWSCustomNotebook = class(TWSWinControl)
    class procedure AddPage(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AIndex: integer); virtual;
    class procedure MovePage(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const NewIndex: integer); virtual;
    class procedure RemovePage(const ANotebook: TCustomNotebook; const AIndex: integer); virtual;

    class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; virtual;
    class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; virtual;
    class function GetPageRealIndex(const ANotebook: TCustomNotebook; AIndex: Integer): Integer; virtual;
    class function GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer; virtual;
    class procedure SetImageList(const ANotebook: TCustomNotebook; const AImageList: TCustomImageList); virtual;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); virtual;
    class procedure SetTabCaption(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AText: string); virtual;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); virtual;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); virtual;
  end;
  TWSCustomNotebookClass = class of TWSCustomNotebook;

  { TWSPage }

  TWSPage = class(TWSCustomPage)
  end;

  { TWSNotebook }

  TWSNotebook = class(TWSCustomNotebook)
  end;

  { TWSShape }

  TWSShape = class(TWSGraphicControl)
  end;

  { TWSCustomSplitter }

  TWSCustomSplitter = class(TWSCustomControl)
  public
  end;

  { TWSSplitter }

  TWSSplitter = class(TWSCustomSplitter)
  end;

  { TWSPaintBox }

  TWSPaintBox = class(TWSGraphicControl)
  end;

  { TWSCustomImage }

  TWSCustomImage = class(TWSGraphicControl)
  end;

  { TWSImage }

  TWSImage = class(TWSCustomImage)
  end;

  { TWSBevel }

  TWSBevel = class(TWSGraphicControl)
  end;

  { TWSCustomRadioGroup }

  TWSCustomRadioGroup = class(TWSCustomGroupBox)
  end;

  { TWSRadioGroup }

  TWSRadioGroup = class(TWSCustomRadioGroup)
  end;

  { TWSCustomCheckGroup }

  TWSCustomCheckGroup = class(TWSCustomGroupBox)
  end;

  { TWSCheckGroup }

  TWSCheckGroup = class(TWSCustomCheckGroup)
  end;

  { TWSCustomLabeledEdit }

  TWSCustomLabeledEdit = class(TWSCustomEdit)
  end;

  { TWSLabeledEdit }

  TWSLabeledEdit = class(TWSCustomLabeledEdit)
  end;

  { TWSCustomPanel }

  TWSCustomPanel = class(TWSCustomControl)
  end;

  { TWSPanel }

  TWSPanel = class(TWSCustomPanel)
  end;

  { TWSCustomTrayIcon }

  TWSCustomTrayIcon = class(TWSLCLComponent)
  public
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; virtual;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; virtual;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); virtual;
    class procedure ShowBalloonHint(const ATrayIcon: TCustomTrayIcon); virtual;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; virtual;
    class function GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas; virtual;
  end;
  TWSCustomTrayIconClass = class of TWSCustomTrayIcon;


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

class procedure TWSCustomTrayIcon.ShowBalloonHint(const ATrayIcon: TCustomTrayIcon);
begin

end;

class function TWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
end;

class function TWSCustomTrayIcon.GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas;
begin
  Result := ATrayIcon.Icon.Canvas;
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomPage, TWSCustomPage);
  RegisterWSComponent(TCustomNotebook, TWSCustomNotebook);
//  RegisterWSComponent(TPage, TWSPage);
//  RegisterWSComponent(TNotebook, TWSNotebook);
//  RegisterWSComponent(TShape, TWSShape);
//  RegisterWSComponent(TCustomSplitter, TWSCustomSplitter);
//  RegisterWSComponent(TCustomSplitter, TWSCustomSplitter);
//  RegisterWSComponent(TPaintBox, TWSPaintBox);
//  RegisterWSComponent(TCustomImage, TWSCustomImage);
//  RegisterWSComponent(TImage, TWSImage);
//  RegisterWSComponent(TBevel, TWSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TWSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TWSCheckGroup);
//  RegisterWSComponent(TCustomLabeledEdit, TWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TWSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TWSCustomPanel);
//  RegisterWSComponent(TPanel, TWSPanel);
  RegisterWSComponent(TCustomTrayIcon, TWSCustomTrayIcon);
////////////////////////////////////////////////////
end.
