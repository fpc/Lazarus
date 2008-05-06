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
  IDEProcs;
  
type

  { TFrameDesignerForm }

  TFrameDesignerForm = class(TForm)
  private
    FFrameWidth: integer;
    FLookupRoot: TComponent;
    FOnLoadBounds: TNotifyEvent;
    FOnSaveBounds: TNotifyEvent;
  protected
    procedure SetFrameWidth(const AValue: integer); virtual;
    procedure SetLookupRoot(const AValue: TComponent); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoLoadBounds; virtual;
    procedure DoSaveBounds; virtual;
  public
    property LookupRoot: TComponent read FLookupRoot write SetLookupRoot;
    property FrameWidth: integer read FFrameWidth write SetFrameWidth;
    property OnLoadBounds: TNotifyEvent read FOnLoadBounds write FOnLoadBounds;
    property OnSaveBounds: TNotifyEvent read FOnSaveBounds write FOnSaveBounds;
  end;
  
  
function CompareFrameDesignerForms(Data1, Data2: Pointer): integer;
function CompareLookupRootAndFrameDesignerForm(Key, Data: Pointer): integer;

implementation


function CompareFrameDesignerForms(Data1, Data2: Pointer): integer;
var
  Form1: TFrameDesignerForm;
  Form2: TFrameDesignerForm;
begin
  Form1 := TFrameDesignerForm(Data1);
  Form2 := TFrameDesignerForm(Data2);
  Result := PtrInt(Form1.LookupRoot) - PtrInt(Form2.LookupRoot);
end;

function CompareLookupRootAndFrameDesignerForm(Key, Data: Pointer): integer;
var
  LookupRoot: TComponent;
  Form: TFrameDesignerForm;
begin
  LookupRoot := TComponent(Key);
  Form := TFrameDesignerForm(Data);
  Result := PtrInt(LookupRoot) - PtrInt(Form.LookupRoot);
end;

{ TFrameDesignerForm }

procedure TFrameDesignerForm.SetLookupRoot(const AValue: TComponent);
begin
  if FLookupRoot = AValue then 
    Exit;
  DoSaveBounds;
  FLookupRoot := AValue;
  if FLookupRoot <> nil then 
  begin
    if FLookupRoot is TCustomFrame then
      TCustomFrame(FLookupRoot).Parent := Self;
    Caption := FLookupRoot.Name;
  end;
  DoLoadBounds;
end;

procedure TFrameDesignerForm.SetFrameWidth(const AValue: integer);
begin
  if FFrameWidth = AValue then 
    Exit;
  FFrameWidth := AValue;
  invalidate;
end;

constructor TFrameDesignerForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FFrameWidth := 1;
end;

destructor TFrameDesignerForm.Destroy;
begin
  inherited Destroy;
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
  if Assigned(OnLoadBounds) then 
    OnLoadBounds(Self);
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
  if Assigned(OnSaveBounds) then OnSaveBounds(Self);
end;

end.

