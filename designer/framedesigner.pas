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
    constructor Create(AOwner: TComponent); override;
    procedure DoLoadBounds; override;
    procedure DoSaveBounds; override;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
  end;
  
  
implementation

{ TFrameDesignerForm }

constructor TFrameDesignerForm.Create(AOwner: TComponent);
begin
  Position := poDefaultPosOnly; // let it be at default position since frame TopLeft is always (0,0)
  inherited Create(AOwner);
end;

procedure TFrameDesignerForm.SetLookupRoot(const AValue: TComponent);
begin
  if (AValue <> nil) and (AValue is TCustomFrame) then
    TCustomFrame(AValue).Parent := Self;
  inherited;
end;

procedure TFrameDesignerForm.DoLoadBounds;

  procedure SetNewBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
  begin
    if NewWidth <= 0 then NewWidth := Width;
    if NewHeight <= 0 then NewHeight := Height;

    NewWidth := Max(20, Min(NewWidth, Screen.Width - 50));
    NewHeight := Max(20, Min(NewHeight, Screen.Height - 50));
    SetBounds(NewLeft, NewTop, Max(20, NewWidth), Max(NewHeight, 20));
  end;

var
  CurFrame: TCustomFrame;
begin
  inherited;

  if LookupRoot is TCustomFrame then 
  begin
    CurFrame := TCustomFrame(LookupRoot);
    SetNewBounds(Left, Top, CurFrame.Width, CurFrame.Height);
  end 
  else 
  if LookupRoot <> nil then 
    DebugLn(['Unsupported component type in TFrameDesignerForm.DoLoadBounds: ', LookupRoot.ClassName])
end;

procedure TFrameDesignerForm.DoSaveBounds;
begin
  if LookupRoot is TCustomFrame then 
    TFrame(LookupRoot).SetBounds(0, 0, Width, Height)
  else 
  if LookupRoot <> nil then
    DebugLn(['Unsupported component type in TFrameDesignerForm.DoSaveBounds: ', LookupRoot.ClassName]);
  inherited;  
end;

procedure TFrameDesignerForm.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  // auto apply width and height
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
  if (LookupRoot <> nil) and (LookupRoot is TCustomFrame) then
    TCustomFrame(LookupRoot).SetBounds(0, 0, Width, Height);
end;

end.

