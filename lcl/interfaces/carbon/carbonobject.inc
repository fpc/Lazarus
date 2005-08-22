// included by carbonint.pas

{ $Id$ }
{******************************************************************************
  All utility method implementations of the TCarbonWidgetSet class are here.


 ******************************************************************************
 Implementation
 ******************************************************************************

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

{ TCarbonWidgetSet }

procedure TCarbonWidgetSet.PassCmdLineOptions;
begin
  inherited PassCmdLineOptions;
end;

constructor TCarbonWidgetSet.Create;
begin
  inherited Create;
end;

destructor TCarbonWidgetSet.Destroy;
begin
  inherited Destroy;
end;

function TCarbonWidgetSet.IntSendMessage3(LM_Message: Integer; Sender: TObject;
  data: pointer): integer;
begin
  Result:=0;
end;

procedure TCarbonWidgetSet.HandleEvents;
begin

end;

procedure TCarbonWidgetSet.WaitMessage;
begin

end;

procedure TCarbonWidgetSet.SendCachedLCLMessages;
begin
  inherited SendCachedLCLMessages;
end;

procedure TCarbonWidgetSet.AppTerminate;
begin

end;

procedure TCarbonWidgetSet.AppInit;
begin

end;

procedure TCarbonWidgetSet.AppMinimize;
begin

end;

function TCarbonWidgetSet.CreateComponent(Sender: TObject): THandle;
begin
  Result:=0;
end;

function TCarbonWidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc
  ): integer;
begin
  Result:=0;
end;

function TCarbonWidgetSet.DestroyTimer(TimerHandle: integer): boolean;
begin
  Result:=false;
end;


// included by carbonint.pas
{
  $Log$
  Revision 1.1  2004/09/06 22:24:52  mattias
  started the carbon LCL interface

}

