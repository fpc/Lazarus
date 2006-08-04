{ $Id$
                  ----------------------------------------
                  carbonproc.pp  -  Carbon interface procs
                  ----------------------------------------

 @created(Wed Aug 26st WET 2005)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains procedures/functions needed for the Carbon <-> LCL interface
 Common carbon untilities (usable by other projects) go to CarbonUtils

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

unit CarbonProc;
{$mode objfpc}{$H+}

interface

uses
  FPCMacOSAll, Classes,
  LCLProc, LCLClasses, Controls, LMessages, Forms, Avl_Tree, SysUtils,
  CarbonDef;

function CreateWidgetInfo(AWidget: Pointer; AObject: TLCLComponent;
  TheType: TCarbonWidgetType): PWidgetInfo;
procedure FreeWidgetInfo(AInfo: PWidgetInfo);
function GetWidgetInfo(AWidget: Pointer): PWidgetInfo;

function DeliverMessage(ATarget: TObject; var AMessage): Integer;

function RegisterEventHandler(AHandler: TCarbonWSEventHandlerProc): EventHandlerUPP;
procedure UnRegisterEventHandler(AHandler: TCarbonWSEventHandlerProc);

function GetCarbonRect(Left, Top, Width, Height: Integer): FPCMacOSAll.Rect;
function GetCarbonRect(const ARect: TRect): FPCMacOSAll.Rect;

function CreateDefaultDevContext: TDeviceContext;
function CreateDevContextForInfo(AnInfo: PWidgetInfo): TDeviceContext;
procedure FreeDevContext(DC: TDeviceContext);

function Dbgs(const ARect: FPCMacOSAll.Rect): string; overload;

implementation

function CreateWidgetInfo(AWidget: Pointer; AObject: TLCLComponent;
  TheType: TCarbonWidgetType): PWidgetInfo;
begin
  New(Result);
  FillChar(Result^, SizeOf(Result^), 0);
  Result^.LCLObject := AObject;
  Result^.Widget := AWidget;
  Result^.WSClass := AObject.WidgetSetClass;
  Result^.widgetType := TheType;

  if IsValidControlHandle(AWidget)
  then begin
    SetControlProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC,
                       SizeOf(Result), @Result);
  end
  else begin
    // there is no (cheap) check for windows so assume a window
    // when it is not a control.
    SetWindowProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC,
                      SizeOf(Result), @Result);
  end;
end;

procedure FreeWidgetInfo(AInfo: PWidgetInfo);
begin
  if AInfo = nil then Exit;

  if (AInfo^.UserData <> nil) and (AInfo^.DataOwner)
  then begin
    System.FreeMem(AInfo^.UserData);
    AInfo^.UserData := nil;
  end;

  if IsValidControlHandle(AInfo^.Widget)
  then begin
    RemoveControlProperty(AInfo^.Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC);
  end
  else begin
    // there is no (cheap) check for windows so assume a window
    // when it is not a control.
    RemoveWindowProperty(AInfo^.Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC);
  end;

  Dispose(AInfo);
end;

function GetWidgetInfo(AWidget: Pointer): PWidgetInfo;
var
  m: LongWord;
begin
  Result := nil;
  if IsValidControlHandle(AWidget)
  then begin
    GetControlProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Result), @m, @Result);
  end
  else begin
    // there is no (cheap) check for windows so assume a window
    // when it is not a control.
    GetWindowProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Result), @m, @Result);
  end;
end;

{------------------------------------------------------------------------------
  Procedure: DeliverMessage
  Params:    Message: the message to process
  Returns:   True if handled

  Generic function which calls the WindowProc if defined, otherwise the
  dispatcher
 ------------------------------------------------------------------------------}
function DeliverMessage(ATarget: TObject; var AMessage): Integer;
begin
  if ATarget = nil
  then begin
    DebugLn('[DeliverMessage] Target = nil');
    Result := 0;
    Exit;
  end;

  try
    if TObject(ATarget) is TControl
    then TControl(ATarget).WindowProc(TLMessage(AMessage))
    else TObject(ATarget).Dispatch(TLMessage(AMessage));
  except
    Application.HandleException(nil);
  end;

  Result := TLMessage(AMessage).Result;
end;

//=====================================================
// UPP mamanger
//=====================================================
type
  TUPPAVLTreeNode = class(TAVLTreeNode)
  public
    UPP: EventHandlerUPP;
    RefCount: Integer;
    procedure Clear; reintroduce; // not overridable, so reintroduce since we only will call this clear
    destructor Destroy; override;
  end;

var
  UPPTree: TAVLTree = nil;

procedure TUPPAVLTreeNode.Clear;
begin
  if UPP <> nil
  then begin
    DisposeEventHandlerUPP(UPP);
    UPP := nil;
  end;
  inherited Clear;
end;

destructor TUPPAVLTreeNode.Destroy;
begin
  if UPP <> nil
  then begin
    DisposeEventHandlerUPP(UPP);
    UPP := nil;
  end;
  inherited Destroy;
end;


function RegisterEventHandler(AHandler: TCarbonWSEventHandlerProc): EventHandlerUPP;
var
  node: TUPPAVLTreeNode;
begin
  if UPPTree = nil then UPPTree := TAVLTree.Create;
  node := TUPPAVLTreeNode(UPPTree.Find(AHandler));
  if node = nil
  then begin
    node := TUPPAVLTreeNode.Create;
    node.Data := AHandler;
    node.UPP := NewEventHandlerUPP(EventHandlerProcPtr(AHandler));
    UPPTree.Add(node);
  end;
  Inc(node.Refcount);
  Result := node.UPP;
end;

procedure UnRegisterEventHandler(AHandler: TCarbonWSEventHandlerProc);
var
  node: TUPPAVLTreeNode;
begin
  if UPPTree = nil then Exit; //???
  node := TUPPAVLTreeNode(UPPTree.Find(AHandler));
  if node = nil then Exit; //???
  if node.Refcount <= 0
  then begin
    DebugLn('[UnRegisterEventHandler] UPPInconsistency, node.refcount <= 0');
    Exit;
  end;

  Dec(node.Refcount);
  if node.Refcount > 0 then Exit;

  // Sigh !
  // there doesn't exist a light version of the avltree without buildin memmanager
  // So, just free it and "pollute" the memmanager with our classes;
  // Freeing our node is also not an option, since that would
  // corrupt the tree (no handling for that).
  // Tweaking the memmanager is also not possible since only the class is public
  // and not the manager itself.

  node.Clear;
  UPPTree.Delete(node);
end;

function GetCarbonRect(Left, Top, Width, Height: Integer): FPCMacOSAll.Rect;
begin
  Result.left:=Left;
  Result.top:=Top;
  Result.right:=Left+Width;
  Result.bottom:=Top+Height;
end;

function GetCarbonRect(const ARect: TRect): FPCMacOSAll.Rect;
begin
  Result.left:=ARect.Left;
  Result.top:=ARect.Top;
  Result.right:=ARect.Right;
  Result.bottom:=ARect.Bottom;
end;

function CreateDefaultDevContext: TDeviceContext;
begin
  Result:=TDeviceContext.Create;
end;

function CreateDevContextForInfo(AnInfo: PWidgetInfo): TDeviceContext;
begin
  Result:=TDeviceContext.Create;
  Result.Info:=AnInfo;
end;

procedure FreeDevContext(DC: TDeviceContext);
begin
  DC.Free;
end;

function Dbgs(const ARect: FPCMacOSAll.Rect): string;
begin
  Result:=IntToStr(ARect.left)+','+IntToStr(ARect.top)
          +','+IntToStr(ARect.right)+','+IntToStr(ARect.bottom);
end;

finalization
  if UPPTree <> nil
  then FreeAndNil(UPPTree);

end.
