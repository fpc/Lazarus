{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSExtCtrls.pp                          *
 *                              -------------------                          *
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
unit CarbonWSExtCtrls;

{$mode delphi}

interface

// defines
{$I carbondefines.inc}

uses
  // libs
  MacOSAll,
  // Cocoa
  {$ifdef CarbonUseCocoa}
  appkit, foundation, lobjc,
  {$endif CarbonUseCocoa}
  // LCL
  Classes, Controls, ExtCtrls, LCLType, LCLProc, Graphics, Math, SysUtils,
  Menus,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSControls, WSProc,
  // LCL Carbon
  carbongdiobjects, CarbonWSControls;

type

  { TCarbonWSCustomPage }

  TCarbonWSCustomPage = class(TWSCustomPage)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
  end;

  { TCarbonWSCustomNotebook }

  TCarbonWSCustomNotebook = class(TWSCustomNotebook)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure AddPage(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ANotebook: TCustomNotebook; const AIndex: integer); override;

    //class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    //class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    //class function GetPageRealIndex(const ANotebook: TCustomNotebook; AIndex: Integer): Integer; override;
    class function GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer; override;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); override;
  end;

  { TCarbonWSPage }

  TCarbonWSPage = class(TWSPage)
  published
  end;

  { TCarbonWSNotebook }

  TCarbonWSNotebook = class(TWSNotebook)
  published
  end;

  { TCarbonWSShape }

  TCarbonWSShape = class(TWSShape)
  published
  end;

  { TCarbonWSCustomSplitter }

  TCarbonWSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TCarbonWSSplitter }

  TCarbonWSSplitter = class(TWSSplitter)
  published
  end;

  { TCarbonWSPaintBox }

  TCarbonWSPaintBox = class(TWSPaintBox)
  published
  end;

  { TCarbonWSCustomImage }

  TCarbonWSCustomImage = class(TWSCustomImage)
  published
  end;

  { TCarbonWSImage }

  TCarbonWSImage = class(TWSImage)
  published
  end;

  { TCarbonWSBevel }

  TCarbonWSBevel = class(TWSBevel)
  published
  end;

  { TCarbonWSCustomRadioGroup }

  TCarbonWSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TCarbonWSRadioGroup }

  TCarbonWSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TCarbonWSCustomCheckGroup }

  TCarbonWSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TCarbonWSCheckGroup }

  TCarbonWSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TCarbonWSCustomLabeledEdit }

  TCarbonWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TCarbonWSLabeledEdit }

  TCarbonWSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TCarbonWSCustomPanel }

  TCarbonWSCustomPanel = class(TWSCustomPanel)
  published
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TCarbonWSPanel }

  TCarbonWSPanel = class(TWSPanel)
  published
  end;

  { TCarbonWSCustomTrayIcon }

  TCarbonWSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    {$ifdef CarbonUseCocoa}
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
    class function IsTrayIconMenuVisible(const ATrayIcon: TCustomTrayIcon): Boolean;
    {$endif CarbonUseCocoa}
  end;

implementation

uses
  CarbonProc, CarbonDef, CarbonTabs, CarbonCanvas;

{ TCarbonWSCustomPage }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomPage.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new custom page in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonTab.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomPage.UpdateProperties
  Params:  ACustomPage - LCL custom page

  Update properties of the specified custom page in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
begin
  if not CheckHandle(ACustomPage, Self, 'UpdateProperties') then Exit;
  
  TCarbonTab(ACustomPage.Handle).UpdateTab;
end;

{ TCarbonWSCustomNotebook }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new custom notebook in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomNotebook.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonTabsControl.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.AddPage
  Params:  ANotebook - LCL custom notebook
           AChild    - New tab
           AIndex    - New tab index

  Adds tab with the specified index in notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.AddPage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const AIndex: integer);
begin
  if not CheckHandle(ANotebook, Self, 'AddPage') then Exit;
  if AChild.HandleAllocated and not CheckHandle(AChild, Self, 'AddPage AChild') then Exit;
  
  // create child handle
  AChild.HandleNeeded;
  // add page
  TCarbonTabsControl(ANotebook.Handle).Add(TCarbonTab(AChild.Handle), AIndex);
  // sync PageIndex with LCL
  TCarbonTabsControl(ANotebook.Handle).SetPageIndex(ANotebook.PageIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.MovePage
  Params:  ANotebook - LCL custom notebook
           AChild    - Moved tab
           AIndex    - New tab index

  Moves tab to the specified index in notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.MovePage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const NewIndex: integer);
begin
  if not CheckHandle(ANotebook, Self, 'MovePage') then Exit;
  if not CheckHandle(AChild, Self, 'MovePage AChild') then Exit;

  TCarbonTabsControl(ANotebook.Handle).Remove(AChild.PageIndex);
  TCarbonTabsControl(ANotebook.Handle).Add(TCarbonTab(AChild.Handle), NewIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.RemovePage
  Params:  ANotebook - LCL custom notebook
           AIndex    - Removed tab index

  Removes tab with the specified index from notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.RemovePage(const ANotebook: TCustomNotebook;
  const AIndex: integer);
begin
  if not CheckHandle(ANotebook, Self, 'RemovePage') then Exit;
  
  TCarbonTabsControl(ANotebook.Handle).Remove(AIndex);
  // sync PageIndex with LCL
  TCarbonTabsControl(ANotebook.Handle).SetPageIndex(ANotebook.PageIndex);
end;


class function TCarbonWSCustomNotebook.GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer;
var
  p : TPoint;
begin
  if not CheckHandle(ANotebook, Self, 'GetTabIndexAtPos') then Exit;
  p := AClientPos;
  inc(p.y, 35); // todo: find out why AClientPos incorrect for TNotebook
  Result := TCarbonTabsControl(ANotebook.Handle).GetPageIndexAtCursor(p);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.SetPageIndex
  Params:  ANotebook - LCL custom notebook
           AIndex    - New tab index

  Selects tab with the specified index in notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.SetPageIndex(const ANotebook: TCustomNotebook;
  const AIndex: integer);
begin
  if not CheckHandle(ANotebook, Self, 'SetPageIndex') then Exit;

  TCarbonTabsControl(ANotebook.Handle).SetPageIndex(AIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.SetTabPosition
  Params:  ANotebook    - LCL custom notebook
           ATabPosition - New position of tabs

  Changes position of the tabs of notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.SetTabPosition(const ANotebook: TCustomNotebook;
  const ATabPosition: TTabPosition);
begin
  if not CheckHandle(ANotebook, Self, 'SetTabPosition') then Exit;
  
  TCarbonTabsControl(ANotebook.Handle).SetTabPosition(ATabPosition);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.ShowTabs
  Params:  ANotebook - LCL custom notebook
           AShowTabs - Tabs visibility

  Changes visibility of all tabs of notebook in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomNotebook.ShowTabs(const ANotebook: TCustomNotebook;
  AShowTabs: boolean);
begin
  if not CheckHandle(ANotebook, Self, 'ShowTabs') then Exit;

  TCarbonTabsControl(ANotebook.Handle).ShowTabs(AShowTabs);
end;

{ TCarbonWSCustomPanel }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.ShowTabs
  Params:  ANotebook - LCL custom notebook
           AShowTabs - Tabs visibility

  TCustomPanel should return preferred size (0,0)
  bugs #16337, and Mattias in comment 0036957 of #16323
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomPanel.GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:=0;
end;


{$include carbontrayicon.inc}

end.

