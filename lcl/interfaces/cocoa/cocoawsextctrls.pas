{ $Id: cocoawsextctrls.pp 15459 2008-06-18 20:23:05Z sekelsenmat $}
{
 *****************************************************************************
 *                              CocoaWSExtCtrls.pp                           *
 *                              -------------------                          *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaWSExtCtrls;

{$mode delphi}
{$modeswitch objectivec1}

interface

uses
  // libs
  MacOSAll, CocoaAll,
  // LCL
  Classes, Controls, ExtCtrls, LCLType, LCLProc, Graphics, Math, SysUtils,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSControls, WSProc,
  // LCL Cocoa
  CocoaPrivate, CocoaWSMenus, CocoaWSCommon, CocoaGDIObjects, CocoaScrollers;

type

  { TCocoaWSPage }

  TCocoaWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TCocoaWSNotebook }

  TCocoaWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TCocoaWSShape }

  TCocoaWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TCocoaWSCustomSplitter }

  TCocoaWSCustomSplitter = class(TWSCustomSplitter)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCocoaWSSplitter }

  TCocoaWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TCocoaWSPaintBox }

  TCocoaWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TCocoaWSCustomImage }

  TCocoaWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TCocoaWSImage }

  TCocoaWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TCocoaWSBevel }

  TCocoaWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TCocoaWSCustomRadioGroup }

  TCocoaWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TCocoaWSRadioGroup }

  TCocoaWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TCocoaWSCustomCheckGroup }

  TCocoaWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TCocoaWSCheckGroup }

  TCocoaWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TCocoaWSCustomLabeledEdit }

  TCocoaWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TCocoaWSLabeledEdit }

  TCocoaWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TCocoaWSCustomPanel }

  TCocoaWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TCocoaWSPanel }

  TCocoaWSPanel = class(TWSPanel)
  private
  protected
  public
  end;

  { TCocoaWSCustomTrayIcon }

  TCocoaWSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

implementation

{$include cocoatrayicon.inc}

{ TCocoaWSCustomSplitter }

type

  { TCocoaSplitterOwnerControl }

  // todo: this should be removed with theme drawing
  TCocoaSplitterOwnerControl = objcclass(TCocoaCustomControl)
    splitter: NSSplitView;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure dealloc; override;
  end;

{ TCocoaSplitterOwnerControl }

procedure TCocoaSplitterOwnerControl.drawRect(dirtyRect: NSRect);
var
  r : NSRect;
begin
  splitter.setVertical(frame.size.height>frame.size.width);
  r:=frame;
  r.origin.x:=0;
  r.origin.y:=0;
  splitter.drawDividerInRect(r);
  inherited drawRect(dirtyRect);
end;

procedure TCocoaSplitterOwnerControl.dealloc;
begin
  splitter.release;
  inherited dealloc;
end;

class function TCocoaWSCustomSplitter.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  ctrl : TCocoaSplitterOwnerControl;
  sl   : TCocoaManualScrollView;
  lcl  : TLCLCommonCallback;
begin
  ctrl := TCocoaSplitterOwnerControl(TCocoaSplitterOwnerControl.alloc.lclInitWithCreateParams(AParams));

  ctrl.splitter:=NSSplitView.alloc.initWithFrame( ctrl.frame );
  // The "pane" decoration doesn't always look good
  //ctrl.splitter.setDividerStyle(NSSplitViewDividerStylePaneSplitter);

  lcl := TLCLCommonCallback.Create(ctrl, AWinControl);
  lcl.BlockCocoaUpDown := true;
  ctrl.callback := lcl;

  sl := EmbedInManualScrollView(ctrl);
  sl.callback := ctrl.callback;
  lcl.frame:=sl;

  Result := TLCLIntfHandle(sl);
end;

end.

