{ $Id: $
                  ----------------------------------------
                  carbonproc.pp  -  Carbon interface procs
                  ----------------------------------------
 
 @created(Wed Aug 26st WET 2005)
 @lastmod($Date: $)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains procedures/functions needed for the Carbon <-> LCL interface 
 Common carbon untilities (usable by other projects) go to CarbonUtils
 
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
}

unit CarbonProc;
{$mode objfpc}{$H+}

interface       

uses
  Carbon,
  LCLProc, Controls, LMessages, Forms,
  CarbonDef;
  
function CreateWidgetInfo(AWidget: Pointer; AObject: TObject): PWidgetInfo;
procedure FreeWidgetInfo(AInfo: PWidgetInfo);
function GetWidgetInfo(AWidget: Pointer): PWidgetInfo;

function DeliverMessage(ATarget: TObject; var AMessage): Integer;

implementation


function CreateWidgetInfo(AWidget: Pointer; AObject: TObject): PWidgetInfo;
begin
  New(Result);
  FillChar(Result^, SizeOf(Result^), 0);
  Result^.LCLObject := AObject;
  Result^.Widget := AWidget;

  if IsValidControlHandle(AWidget) 
  then begin
    SetControlProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Result), @Result);
  end
  else begin
    // there is no (cheap) check for windows so assume a window 
    // when it is not a control.
    SetWindowProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Result), @Result);
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
    GetControlProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Result), m, @Result);
  end
  else begin   
    // there is no (cheap) check for windows so assume a window 
    // when it is not a control.
    GetWindowProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Result), m, @Result);
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


end.