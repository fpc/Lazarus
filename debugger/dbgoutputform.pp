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
  Buttons, StdCtrls, Menus;

type
  TDbgOutputForm = class(TForm)
    txtOutput: TMemo;
    mnuPopup: TPopupMenu;
    popClear: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure popClearClick(Sender: TObject);
  private
  protected
    procedure Loaded; override;
  public
    procedure AddText(const AText: String);
    procedure Clear;
  end;

implementation

procedure TDbgOutputForm.AddText(const AText: String);
begin
  txtOutput.Lines.Add(AText);
end;

procedure TDbgOutputForm.Clear;
begin             
  txtOutput.Lines.Clear; 
end;

procedure TDbgOutputForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
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
  
  // Not yet through resources
  mnuPopUp.Items.Add(popClear);
//    popClear.Caption := '&Clear';
//    popClear.OnClick := @popClearClick; 
WriteLn('Popupcount: ', mnuPopUp.Items.Count);
WriteLn('Itemvisible ', popClear.Visible);
end;

procedure TDbgOutputForm.popClearClick(Sender: TObject);
begin
  Clear;
end;

initialization
  {$I dbgoutputform.lrc}

end.
{ =============================================================================
  $Log$
  Revision 1.3  2002/03/09 02:03:59  lazarus
  MWE:
    * Upgraded gdb debugger to gdb/mi debugger
    * Set default value for autpopoup
    * Added Clear popup to debugger output window

  Revision 1.2  2002/02/20 23:33:24  lazarus
  MWE:
    + Published OnClick for TMenuItem
    + Published PopupMenu property for TEdit and TMemo (Doesn't work yet)
    * Fixed debugger running twice
    + Added Debugger output form
    * Enabled breakpoints

  Revision 1.1  2001/11/05 00:12:51  lazarus
  MWE: First steps of a debugger.

}
