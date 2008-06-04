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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonWSExtCtrls;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
  // libs
  {$ifdef ver2_2_0}
  FPCMacOSAll,
  {$else}
  MacOSAll,
  {$endif}
  // Cocoa
  {$ifdef CarbonUseCocoa}
  appkit, foundation,
  {$endif CarbonUseCocoa}
  // LCL
  Classes, Controls, ExtCtrls, LCLType, LCLProc, Graphics, Math,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSControls, WSProc,
  // LCL Carbon
  carbongdiobjects, CarbonWSControls;

type

  { TCarbonWSCustomPage }

  TCarbonWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
  end;

  { TCarbonWSCustomNotebook }

  TCarbonWSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure AddPage(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ANotebook: TCustomNotebook; const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ANotebook: TCustomNotebook; const AIndex: integer); override;

    //class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    //class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    //class function GetPageRealIndex(const ANotebook: TCustomNotebook; AIndex: Integer): Integer; override;
    //class function GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer; override;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); override;
  end;

  { TCarbonWSPage }

  TCarbonWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TCarbonWSNotebook }

  TCarbonWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TCarbonWSShape }

  TCarbonWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TCarbonWSCustomSplitter }

  TCarbonWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TCarbonWSSplitter }

  TCarbonWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TCarbonWSPaintBox }

  TCarbonWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomImage }

  TCarbonWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TCarbonWSImage }

  TCarbonWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TCarbonWSBevel }

  TCarbonWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TCarbonWSCustomRadioGroup }

  TCarbonWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TCarbonWSRadioGroup }

  TCarbonWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TCarbonWSCustomCheckGroup }

  TCarbonWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TCarbonWSCheckGroup }

  TCarbonWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TCarbonWSCustomLabeledEdit }

  TCarbonWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TCarbonWSLabeledEdit }

  TCarbonWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TCarbonWSCustomPanel }

  TCarbonWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TCarbonWSPanel }

  TCarbonWSPanel = class(TWSPanel)
  private
  protected
  public
  end;

  { TCarbonWSCustomTrayIcon }

  TCarbonWSCustomTrayIcon = class(TWSCustomTrayIcon)
  public
    {$ifdef CarbonUseCocoa}
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
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
  if not CheckHandle(AChild, Self, 'AddPage AChild') then Exit;
  
  TCarbonTabsControl(ANotebook.Handle).Add(TCarbonTab(AChild.Handle), AIndex);
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

{$include carbontrayicon.inc}

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomPage, TCarbonWSCustomPage);
  RegisterWSComponent(TCustomNotebook, TCarbonWSCustomNotebook);
//  RegisterWSComponent(TPage, TCarbonWSPage);
//  RegisterWSComponent(TNotebook, TCarbonWSNotebook);
//  RegisterWSComponent(TShape, TCarbonWSShape);
//  RegisterWSComponent(TCustomSplitter, TCarbonWSCustomSplitter);
//  RegisterWSComponent(TSplitter, TCarbonWSSplitter);
//  RegisterWSComponent(TPaintBox, TCarbonWSPaintBox);
//  RegisterWSComponent(TCustomImage, TCarbonWSCustomImage);
//  RegisterWSComponent(TImage, TCarbonWSImage);
//  RegisterWSComponent(TBevel, TCarbonWSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TCarbonWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TCarbonWSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TCarbonWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TCarbonWSCheckGroup);
//  RegisterWSComponent(TBoundLabel, TCarbonWSBoundLabel);
//  RegisterWSComponent(TCustomLabeledEdit, TCarbonWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TCarbonWSLabeledEdit);
//  RegisterWSComponent(TCustomPanel, TCarbonWSCustomPanel);
//  RegisterWSComponent(TPanel, TCarbonWSPanel);
  RegisterWSComponent(TCustomTrayIcon, TCarbonWSCustomTrayIcon);
////////////////////////////////////////////////////
end.

