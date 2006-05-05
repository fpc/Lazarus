{%MainUnit ../dialogs.pp}

{******************************************************************************
                                  TCommonDialog
 ******************************************************************************

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
{------------------------------------------------------------------------------
  Method: TCommonDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCommonDialog.Create (TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

function TCommonDialog.Execute: boolean;
begin
  FUserChoice := mrNone;
  Handle := TWSCommonDialogClass(WidgetSetClass).CreateHandle(Self);
  Result:= DoExecute;
  Close;
end;

procedure TCommonDialog.Close;
begin
  DoClose;
  TWSCommonDialogClass(WidgetSetClass).DestroyHandle(Self);
  FHandle := 0;
end;

procedure TCommonDialog.DoShow;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TCommonDialog.DoClose;
begin
  if Assigned(FOnClose) then FOnClose(Self);
end;

function TCommonDialog.HandleAllocated: boolean;
begin
  Result:=FHandle<>0;
end;

procedure TCommonDialog.SetHandle(const AValue: THandle);
begin
  FHandle:=AValue;
end;

procedure TCommonDialog.SetHeight(const AValue: integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
end;

procedure TCommonDialog.SetWidth(const AValue: integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
end;

function TCommonDialog.DoExecute : boolean;
var CanClose: boolean;
begin
  if Assigned(FOnShow) then FOnShow(Self);
  TWSCommonDialogClass(WidgetSetClass).ShowModal(Self);
  repeat
    if (FUserChoice <> mrNone) and (Handle<>0)
    and (OnCanClose<>nil) then begin
      CanClose:=true;
      OnCanClose(Self,CanClose);
      if not CanClose then FUserChoice:=mrNone;
    end;
    if FUserChoice <> mrNone then
      break;
    { win32 widgetset dialogs use their own message loop,
      so only FUserChoice may have been set already }
    Application.HandleMessage;
  until false;
  Result := (FUserChoice = mrOk);
  if Assigned(FOnClose) then FOnClose(Self);
end;
