{ $Id$ }
{                        ----------------------------------------  
                          dbgoutputform.pp  -  Shows target output 
                         ---------------------------------------- 
 
 @created(Wed Feb 25st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
} 
unit dbgoutputform;

{$mode objfpc}
{$H+}

interface

uses
  Classes, Graphics, Controls, Forms, Dialogs, LResources,
  Buttons, StdCtrls, Debugger;

type
  TDbgOutputForm = class(TForm)
    txtOutput: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  protected
    procedure Loaded; override;
  public
    procedure AddText(const AText: String);
  end;

implementation

procedure TDbgOutputForm.AddText(const AText: String);
begin
  txtOutput.Lines.Add(AText);
end;

procedure TDbgOutputForm.FormCreate(Sender: TObject);
begin
  txtOutput.Lines.Clear;
end;

procedure TDbgOutputForm.FormDestroy(Sender: TObject);
begin
end;

procedure TDbgOutputForm.Loaded;
begin
  inherited Loaded;
  
  // Not yet through resources
  txtOutput.Scrollbars := ssBoth;
end;

initialization
  {$I dbgoutputform.lrc}

end.
{ =============================================================================
  $Log$
  Revision 1.1  2001/11/05 00:12:51  lazarus
  MWE: First steps of a debugger.

}
