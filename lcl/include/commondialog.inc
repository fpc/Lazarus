{%MainUnit ../dialogs.pp}

{******************************************************************************
                                  TCommonDialog
 ******************************************************************************

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
{------------------------------------------------------------------------------
  Method: TCommonDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCommonDialog.Create (TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTitle := DefaultTitle;
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

procedure TCommonDialog.DoCanClose(var CanClose: Boolean);
begin
  FCanCloseCalled := True;
  if Assigned(FOnCanClose) then
    OnCanClose(Self, CanClose);
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

function TCommonDialog.IsTitleStored: boolean;
begin
  result := FTitle<>DefaultTitle;
end;

class procedure TCommonDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCommonDialog;
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
var
  CanClose: boolean;
begin
  FCanCloseCalled := False;
  if Assigned(FOnShow) then
    FOnShow(Self);
  TWSCommonDialogClass(WidgetSetClass).ShowModal(Self);
  // can close was called from widgetset loop
  if not FCanCloseCalled then
  begin
    repeat
      if (FUserChoice <> mrNone) and (Handle<>0) then
      begin
        CanClose := True;
        DoCanClose(CanClose);
        if not CanClose then
          FUserChoice:=mrNone;
      end;
      if FUserChoice <> mrNone then
        break;
      { win32 widgetset dialogs use their own message loop,
        so only FUserChoice may have been set already }
      Application.HandleMessage;
    until false;
  end;
  Result := (FUserChoice = mrOk);
end;

function TCommonDialog.DefaultTitle: string;
begin
  Result := '';
end;
