{%MainUnit ../interfacebase.pp}
{ $Id$
******************************************************************************
                                  TWidgetSet

                                   WinApi stuff


  !! Keep this alphabetical !!
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

constructor TWidgetSet.Create;
begin
  inherited Create;
  FThemeServices := CreateThemeServices;
end;

procedure TWidgetSet.BeforeDestruction;
begin
  FreeAndNil(FThemeServices); // destroy before destructor to release resources
  inherited BeforeDestruction;
end;

function TWidgetSet.GetAppHandle: THandle;
begin
  DebugLn('Warning: AppHandle is not implemented for this widgetset yet');
  Result := 0;
end;

procedure TWidgetSet.SetAppHandle(const AValue: THandle);
begin
  DebugLn('Warning: AppHandle is not implemented for this widgetset yet');
end;

procedure TWidgetSet.PassCmdLineOptions;
begin
end;

function TWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TThemeServices.Create;
end;

procedure TWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  if Assigned(ALoop) then ALoop;
end;

procedure TWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
end;

procedure TWidgetSet.AppSetTitle(const ATitle: string);
begin
end;

procedure TWidgetSet.AppSetVisible(const AVisible: Boolean);
begin
end;

function TWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  Result := False;
end;

function TWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
  Result := False;
end;

procedure TWidgetSet.AppSetMainFormOnTaskBar(const DoSet: Boolean);
begin
end;

function TWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint,
    lcNeedMininimizeAppWithMainForm,
    lcApplicationTitle,
    lcFormIcon,
    lcModalWindow,
    lcReceivesLMClearCutCopyPasteReliably:
      Result := LCL_CAPABILITY_YES;
    lcSendsUTF8KeyPress:
      if IntfSendsUTF8KeyPress() then Result := LCL_CAPABILITY_YES
      else Result := LCL_CAPABILITY_NO;
  else
    Result := LCL_CAPABILITY_NO;
  end;
end;

procedure TWidgetSet.DebugLogLn(const s: string);
begin
  if Assigned(LCLProc.DebugText) then
    WriteLn(LCLProc.DebugText^, s);
end;

procedure TWidgetSet.DebugLog(const s: string);
begin
  if Assigned(LCLProc.DebugText) then
    Write(LCLProc.DebugText^, s);
end;

procedure TWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
begin
end;

