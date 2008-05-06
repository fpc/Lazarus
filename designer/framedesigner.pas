{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TFrameDesignerForm is a designer form to design TFrame.
}
unit FrameDesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, LCLProc, Graphics, GraphType, Forms, Controls,
  IDEProcs, CustomNonFormDesigner;
  
type

  { TFrameDesignerForm }

  TFrameDesignerForm = class(TCustomNonFormDesignerForm)
  protected
    procedure SetLookupRoot(const AValue: TComponent); override;
  public
    procedure DoLoadBounds; override;
    procedure DoSaveBounds; override;
  end;
  
  
implementation

{ TFrameDesignerForm }

procedure TFrameDesignerForm.SetLookupRoot(const AValue: TComponent);
begin
  if (AValue <> nil) and (AValue is TCustomFrame) then
    TCustomFrame(AValue).Parent := Self;
  inherited;
end;

procedure TFrameDesignerForm.DoLoadBounds;

  procedure SetNewBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
  begin
    if NewWidth<= 0 then NewWidth := Width;
    if NewHeight<= 0 then NewHeight := Height;

    NewWidth := Max(20, Min(NewWidth, Screen.Width - 50));
    NewHeight := Max(20, Min(NewHeight, Screen.Height - 50));
    SetBounds(Left, Top, Max(20, NewWidth), Max(NewHeight, 20));
  end;

var
  CurFrame: TCustomFrame;
  NewWidth: Integer;
  NewHeight: Integer;
begin
  inherited;

  if LookupRoot is TCustomFrame then 
  begin
    CurFrame := TCustomFrame(LookupRoot);
    NewWidth := CurFrame.Width;
    NewHeight := CurFrame.Height;
    
    SetNewBounds(Left, Top, NewWidth, NewHeight);
  end 
  else 
  if LookupRoot <> nil then 
  begin
    // ?
  end;
end;

procedure TFrameDesignerForm.DoSaveBounds;
begin
  if LookupRoot is TFrame then 
    TFrame(LookupRoot).SetBounds(0, 0, Width, Height)
  else 
  if LookupRoot <> nil then
    ;//?
  inherited;  
end;

end.

