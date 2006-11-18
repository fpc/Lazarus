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
end;

destructor TWidgetSet.Destroy;
begin
  inherited Destroy;
end;

procedure TWidgetSet.PassCmdLineOptions;
begin
end;

procedure TWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  if Assigned(ALoop) then ALoop;
end;

procedure TWidgetSet.AppSetTitle(const ATitle: string);
begin
  Debugln('TWidgetSet.AppSetTitle not implemented by ', ClassName);
end;

function TWidgetSet.WidgetSetName: string;
begin
  Result := '';
  Debugln('TWidgetSet.WidgetSetName not implemented by ', ClassName);
end;
