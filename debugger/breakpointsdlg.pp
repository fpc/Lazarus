unit breakpointsdlg;
{ $Id$ }
{               ----------------------------------------------  
                 breakpointsdlg.pp  -  Overview of breeakponts 
                ---------------------------------------------- 
 
 @created(Fri Dec 14st WET 2001)
 @lastmod($Date$)
 @author(Shane Miller)                       
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the Breakpoint dialog.
 
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
} 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, StdCtrls,Buttons,Extctrls,ComCtrls;

type
  TBreakPointsDlg = class(TForm)
    lvBreakPoints: TListView;
  private
    procedure AddBreakPoint(UnitName : String; Line : Integer);
    procedure DeleteBreakPoint(UnitName : String; Line : Integer);
  protected
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;  
  
var
  BREAKPOINTS_DLG: TBreakPointsDlg;  

implementation

constructor TBreakPointsdlg.Create(AOwner : TComponent);
begin
  inherited;
(*
  if LazarusResources.Find(Classname)=nil then
  begin
  lvBreakPoints := TListView.Create(self);
  with lvBreakPoints do
    Begin
      Parent := self;
      Align := alClient;
      Visible := True;
      Name := 'lvBreakPoints';
      Columns.Clear;
      Columns.Updating := TRue;
      Columns.Add('Filename/Address');
      Columns.Add('Line/Length');
      Columns.Add('Condition');
      Columns.Add('Action');
      Columns.Add('Pass Count');
      Columns.Add('Group');
      Columns.Updating := False;
//Example alignment of columns.
//      Columns.Item[1].Alignment := caRight;
      ViewStyle := vsReport;
      Sorted := True;
      OnKeyDown := @lvBreakPointsKeyDown;
      MultiSelect := True;
    end;
//ListView does not accpet keys unless the mouse is held down over it
//so temporarily I do this:
  OnKeyDown := @lvBreakPointsKeyDown;

  Caption := 'Breakpoints';
  Name := 'BreakPointsDlg';
  Width := 350;
  Height := 100;

  end;
*)

end;

destructor TBreakPointsDlg.Destroy;
begin
  inherited;
end;

procedure TBreakPointsDlg.AddBreakPoint(UnitName : String; Line : Integer);
var
  LI : TListItem;
begin
  LI := lvBreakPoints.Items.Add;
  LI.Caption := UnitName;
  LI.SubItems.Add(Inttostr(line));
  LI.SubItems.Add('');
  LI.SubItems.Add('Break');
  LI.SubItems.Add('0');
  LI.SubItems.Add('');
end;


procedure TBreakPointsDlg.DeleteBreakPoint(UnitName : String; Line : Integer);
var
  LI : TListItem;
  I : Integer;
begin
  for I := 0 to lvBreakPoints.Items.Count-1 do
     Begin
       LI := lvBreakPoints.Items[i];
       if LI.Caption <> UnitName then Continue;
       if LI.SubItems.Strings[0] = inttostr(line) then
          begin
            lvBreakPoints.Items.Delete(i);
            Break;
          end;
     
     end;
end;

initialization
  {$I breakpointsdlg.lrc}

end.

{ =============================================================================
  $Log$
  Revision 1.1  2002/03/12 23:55:36  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

}