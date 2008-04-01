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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

destructor TWidgetSet.Destroy;
begin
  FreeAndNil(FThemeServices);
  inherited Destroy;
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

procedure TWidgetSet.AppSetTitle(const ATitle: string);
begin
  Debugln('TWidgetSet.AppSetTitle not implemented by ', ClassName);
end;

function TWidgetSet.LCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  Result := 0;
end;

