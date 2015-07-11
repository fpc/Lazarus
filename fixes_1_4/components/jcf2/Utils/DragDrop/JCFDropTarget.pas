unit JCFDropTarget;

{ AFS 16 May 2K
  Got this unit as freeware from www.undu.com October 1998 page
  code by Thorsten Engler - Thorsten.Engler@gmx.net
  Renamed to JCFDropTarget to avoid name conflicts (peter3)
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JCFDropTarget, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  Windows,
  Classes,
  ActiveX,
  Controls;

type
  TTeDropTarget    = class;
  TTeDropInterface = class;

  TTeDropTargetLifeState = (lsStart, lsExists, lsLocked, lsRegd);

  TTeDragOperation = (doNothing, doCopy, doMove, doLink);

  TTeComDragObject = class(TDragObject)
  private
    FDropInterface: TTeDropInterface;
    function GetDataObject: IDataObject;
    function GetDragOperation: TTeDragOperation;
    procedure SetDragOperation(Value: TTeDragOperation);
    function GetShiftState: TShiftState;
  public
    constructor Create(ADropInterface: TTeDropInterface); virtual;
    property DataObject: IDataObject Read GetDataObject;
    property DragOperation: TTeDragOperation Read GetDragOperation
      Write SetDragOperation;
    property ShiftState: TShiftState Read GetShiftState;
  end;

  TComDragObjectClass = class of TTeComDragObject;

  TTeDropInterface = class
  private
    function DoDragOver(DragMsg: TDragMessage): boolean;
    function DragTo(const Pos: TPoint): boolean;
    function DragFindTarget(const Pos: TPoint; var Handle: HWND): Pointer;
  protected
    FDropTarget: TTeDropTarget;
    FWinControl: TWinControl;
    FDataObject: IDataObject;
    FDragOperation: TTeDragOperation;
    FShiftState: TShiftState;
    FDragObject: TTeComDragObject;
  public
    property CFDropTarget: TTeDropTarget Read FDropTarget;

    constructor Create(AWinControl: TWinControl); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function DropTarget_Create: HResult;
    function DropTarget_Destroy: HResult;
    function DropTarget_Exists: boolean;
  protected
    procedure DropTarget_Forget;
  public
    function DropTarget_LifeState: TTeDropTargetLifeState;

    function DragEnter(const dataObj: IDataObject; grfKeyState: longint;
      pt: TPoint; var dwEffect: longint): HResult; virtual;
    function DragOver(grfKeyState: longint; pt: TPoint;
      var dwEffect: longint): HResult; virtual;
    function DragLeave: HResult; virtual;
    function Drop(const dataObj: IDataObject; grfKeyState: longint;
      pt: TPoint; var dwEffect: longint): HResult; virtual;

    property DataObject: IDataObject Read FDataObject;
    property DragOperation: TTeDragOperation Read FDragOperation Write FDragOperation;
    property ShiftState: TShiftState Read FShiftState;
  end;

  TTeDropTarget = class(TInterfacedObject, IDropTarget)
  private
    FDropHWND: HWND;
    FDropWinControl: TWinControl;

    FDropInterface: TTeDropInterface;
    FLifeState: TTeDropTargetLifeState;

    procedure SetLifeState(Value: TTeDropTargetLifeState);
  public
    property DropHWND: HWND Read FDropHWnd;
    property DropWinControl: TWinControl Read FDropWinControl;
    property LifeState: TTeDropTargetLifeState Read FLifeState Write SetLifeState;

    constructor Create(AWinControl: TWinControl;
      ADropInterface: TTeDropInterface); virtual;
    procedure BeforeDestruction; override;

    function ToState_Exists: HResult;
    function ToState_Locked: HResult;
    function ToState_Regd: HResult;
  public
    { IDropTarget }
    function DragEnter(const dataObj: IDataObject; grfKeyState: longint;
      pt: TPoint; var dwEffect: longint): HResult; stdcall;
    function DragOver(grfKeyState: longint; pt: TPoint;
      var dwEffect: longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: longint;
      pt: TPoint; var dwEffect: longint): HResult; stdcall;
  end;

var
  ComDragObjectClass: TComDragObjectClass;

const
  Effects: array[TTeDragOperation] of integer =
    (DROPEFFECT_NONE, DROPEFFECT_COPY, DROPEFFECT_MOVE, DROPEFFECT_LINK);

implementation

function DragMessage(Handle: HWND; Msg: TDragMessage; Source: TDragObject;
  Target: Pointer; const Pos: TPoint): longint;
var
  DragRec: TDragRec;
begin
  Result := 0;
  if Handle <> 0 then
  begin
    DragRec.Pos := Pos;
    DragRec.Target := Target;
    DragRec.Source := Source;
    DragRec.Docking := False;
    Result := SendMessage(Handle, CM_DRAG, longint(Msg), longint( @DragRec));
  end;
end;

function DragFindWindow(const Pos: TPoint): HWND;
begin
  Result := WindowFromPoint(Pos);
  while Result <> 0 do
    if not Assigned(FindControl(Result)) then
      Result := GetParent(Result)
    else
      Exit;
end;

function TTeDropInterface.DragFindTarget(const Pos: TPoint; var Handle: HWND): Pointer;
begin
  Handle := DragFindWindow(Pos);
  Result := Pointer(DragMessage(Handle, dmFindTarget, FDragObject, nil, Pos));
end;

function TTeDropInterface.DoDragOver(DragMsg: TDragMessage): boolean;
begin
  Result := False;
  if FDragObject.DragTarget <> nil then
    Result := longbool(DragMessage(FDragObject.DragHandle, DragMsg, FDragObject,
      FDragObject.DragTarget, FDragObject.DragPos));
end;

function TTeDropInterface.DragTo(const Pos: TPoint): boolean;
var
  Target: TControl;
  TargetHandle: HWND;
begin
  Target := DragFindTarget(Pos, TargetHandle);
  if Target <> FDragObject.DragTarget then
  begin
    DoDragOver(dmDragLeave);
    FDragObject.DragTarget := Target;
    FDragObject.DragHandle := TargetHandle;
    FDragObject.DragPos    := Pos;
    DoDragOver(dmDragEnter);
  end;
  FDragObject.DragPos := Pos;
  if FDragObject.DragTarget <> nil then
    FDragObject.DragTargetPos := TControl(FDragObject.DragTarget).ScreenToClient(Pos);
  Result := DoDragOver(dmDragMove);
end;

constructor TTeDropInterface.Create(AWinControl: TWinControl);
begin
  inherited Create;
  FWinControl := AWinControl;
  FDropTarget := nil;
  FDragObject := ComDragObjectClass.Create(Self);
end;

procedure TTeDropInterface.BeforeDestruction;
begin
  inherited;
  if Assigned(FDragObject) then
    FDragObject.FDropInterface := nil;
  if Assigned(FDropTarget) then
    FDropTarget.Free;
end;

function TTeDropInterface.DropTarget_Create: HResult;
begin
  Result := E_UNEXPECTED;
  try
    if not Assigned(FDropTarget) then
      FDropTarget := TTeDropTarget.Create(FWinControl, Self);
    if Assigned(FDropTarget) then
      Result := CFDropTarget.ToState_Regd;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTeDropInterface.DropTarget_Destroy: HResult;
begin
  Result := S_OK;
  try
    if Assigned(FDropTarget) then
      Result := CFDropTarget.ToState_Locked;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTeDropInterface.DropTarget_Exists: boolean;
begin
  Result := Assigned(FDropTarget);
end;

procedure TTeDropInterface.DropTarget_Forget;
begin
  FDropTarget := nil;
end;

function TTeDropInterface.DropTarget_LifeState: TTeDropTargetLifeState;
begin
  if DropTarget_Exists then
    Result := CFDropTarget.LifeState
  else
    Result := lsStart;
end;

function CreateShiftState(grfKeyState: longint): TShiftState;
begin
  Result := [];
  if (grfKeyState and MK_CONTROL) = MK_CONTROL then
    Include(Result, ssCtrl);
  if (grfKeyState and MK_SHIFT) = MK_SHIFT then
    Include(Result, ssShift);
//  if (grfKeyState and MK_ALT)     = MK_ALT     then Include (Result, ssAlt);
  if (grfKeyState and MK_LBUTTON) = MK_LBUTTON then
    Include(Result, ssLeft);
  if (grfKeyState and MK_MBUTTON) = MK_MBUTTON then
    Include(Result, ssMiddle);
  if (grfKeyState and MK_RBUTTON) = MK_RBUTTON then
    Include(Result, ssRight);
end;

function CreateDragOperation(ShiftState: TShiftState): TTeDragOperation;
begin
  Result := doMove; // muss noch geändert werden;
  if ssCtrl in ShiftState then
    Result := doCopy;
  if ssShift in ShiftState then
    Result := doMove;
  if (ssCtrl in ShiftState) and (ssShift in ShiftState) then
    Result := doLink;
end;

function TTeDropInterface.DragEnter(const dataObj: IDataObject;
  grfKeyState: longint; pt: TPoint; var dwEffect: longint): HResult;
begin
  Result   := S_OK;
  dwEffect := DROPEFFECT_NONE;
  if not Assigned(FWinControl) then
    exit;
  if not Assigned(FDragObject) then
    exit;
  try
    FShiftState    := CreateShiftState(grfKeyState);
    FDragOperation := CreateDragOperation(FShiftState);
    FDataObject    := dataObj;
    if not DragTo(pt) then
      FDragOperation := doNothing;
    dwEffect := Effects[FDragOperation];
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTeDropInterface.DragOver(grfKeyState: longint; pt: TPoint;
  var dwEffect: longint): HResult;
begin
  Result   := S_OK;
  dwEffect := DROPEFFECT_NONE;
  if not Assigned(FWinControl) then
    exit;
  if not Assigned(FDragObject) then
    exit;
  try
    FShiftState    := CreateShiftState(grfKeyState);
    FDragOperation := CreateDragOperation(FShiftState);
    if not DragTo(pt) then
      FDragOperation := doNothing;
    dwEffect := Effects[FDragOperation];
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTeDropInterface.DragLeave: HResult;
begin
  Result := S_OK;
  if not Assigned(FWinControl) then
    exit;
  if not Assigned(FDragObject) then
    exit;
  try
    DoDragOver(dmDragLeave);
    FDragObject.DragTarget := nil;
    FDragObject.DragHandle := 0;
    FDataObject := nil;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TTeDropInterface.Drop(const dataObj: IDataObject; grfKeyState: longint;
  pt: TPoint; var dwEffect: longint): HResult;
begin
  Result   := S_OK;
  dwEffect := DROPEFFECT_NONE;
  if not Assigned(FWinControl) then
    exit;
  if not Assigned(FDragObject) then
    exit;
  try
    FDataObject := dataObj;
    try
      FShiftState    := CreateShiftState(grfKeyState);
      FDragOperation := CreateDragOperation(FShiftState);
      if not DragTo(pt) then
        FDragOperation := doNothing;
      dwEffect := Effects[FDragOperation];
      if FDragOperation <> doNothing then
        DoDragOver(dmDragDrop);
    finally
      FDataObject := nil;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

constructor TTeDropTarget.Create(AWinControl: TWinControl;
  ADropInterface: TTeDropInterface);
begin
  inherited Create;
  FDropWinControl := AWinControl;
  FDropInterface  := ADropInterface;
  FLifeState      := lsExists;
end;

procedure TTeDropTarget.BeforeDestruction;
begin
  if Assigned(FDropInterface) then
    FDropInterface.DropTarget_Forget;

  if FLifeState > lsLocked then
  begin
    while RefCount < 2 do
      _AddRef;
    ActiveX.RevokeDragDrop(FDropHWND);
    FDropHWND  := 0;
    FLifeState := lsLocked;
  end;

  if FLifeState > lsExists then
  begin
    while RefCount < 2 do
      _AddRef;
    ActiveX.CoLockObjectExternal(Self as IDropTarget, False, False);
    FLifeState := lsExists;
  end;
end;

function TTeDropTarget.ToState_Exists: HResult;
begin
  Result := S_OK;
  if LifeState = lsRegd then
    Result := ToState_Locked;

  if LifeState = lsLocked then
  begin
    LifeState := lsExists;
    Result    := ActiveX.CoLockObjectExternal(Self as IDropTarget, False, True);
  end;
end;

function TTeDropTarget.ToState_Locked: HResult;
begin
  Result := S_OK;

  if LifeState = lsExists then
  begin
    Result := ActiveX.CoLockObjectExternal(Self as IDropTarget, True, False);
    if Result = S_OK then
      LifeState := lsLocked;
  end;

  if LifeState = lsRegd then
  begin
    while RefCount < 2 do
      _AddRef;
    Result    := ActiveX.RevokeDragDrop(FDropHWND);
    FDropHWND := 0;
    if Result = S_OK then
      LifeState := lsLocked;
  end;
end;

function TTeDropTarget.ToState_Regd: HResult;
begin
  Result := S_OK;
  if LifeState = lsExists then
    Result := ToState_Locked;
  if LifeState = lsLocked then
  begin
    FDropHWND := FDropWinControl.Handle;
    Result    := ActiveX.RegisterDragDrop(FDropHWND, Self as IDropTarget);
    if Result = S_OK then
      LifeState := lsRegd;
  end;
end;

procedure TTeDropTarget.SetLifeState(Value: TTeDropTargetLifeState);
begin
  FLifeState := Value;
end;

function TTeDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: longint; pt: TPoint; var dwEffect: longint): HResult;
begin
  if Assigned(FDropInterface) then
    Result := FDropInterface.DragEnter(dataObj, grfKeyState, pt, dwEffect)
  else
    Result := E_UNEXPECTED;
end;

function TTeDropTarget.DragOver(grfKeyState: longint; pt: TPoint;
  var dwEffect: longint): HResult;
begin
  if Assigned(FDropInterface) then
    Result := FDropInterface.DragOver(grfKeyState, pt, dwEffect)
  else
    Result := E_UNEXPECTED;
end;

function TTeDropTarget.DragLeave: HResult;
begin
  if Assigned(FDropInterface) then
    Result := FDropInterface.DragLeave
  else
    Result := E_UNEXPECTED;
end;

function TTeDropTarget.Drop(const dataObj: IDataObject; grfKeyState: longint;
  pt: TPoint; var dwEffect: longint): HResult;
begin
  if Assigned(FDropInterface) then
    Result := FDropInterface.Drop(dataObj, grfKeyState, pt, dwEffect)
  else
    Result := E_UNEXPECTED;
end;

{ TTeComDragObject }

constructor TTeComDragObject.Create(ADropInterface: TTeDropInterface);
begin
  inherited Create;
  FDropInterface := ADropInterface;
end;

function TTeComDragObject.GetDataObject: IDataObject;
begin
  if Assigned(FDropInterface) then
    Result := FDropInterface.DataObject
  else
    Result := nil;
end;

function TTeComDragObject.GetDragOperation: TTeDragOperation;
begin
  if Assigned(FDropInterface) then
    Result := FDropInterface.DragOperation
  else
    Result := doNothing;
end;

function TTeComDragObject.GetShiftState: TShiftState;
begin
  if Assigned(FDropInterface) then
    Result := FDropInterface.ShiftState
  else
    Result := [];
end;

procedure TTeComDragObject.SetDragOperation(Value: TTeDragOperation);
begin
  if Assigned(FDropInterface) then
    FDropInterface.DragOperation := Value;
end;

destructor TTeDropInterface.Destroy;
begin
  if Assigned(FDragObject) then
  begin
    FDragObject.Free;
    FDragObject := nil;
  end;
  inherited;
end;

initialization
  ComDragObjectClass := TTeComDragObject;
  OleInitialize(nil);

finalization
  OleUninitialize;
end.
