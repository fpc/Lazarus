// included by controls.pp

{
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

{------------------------------------------------------------------------------}
{  TScrolledWindow Constructor                                                 }
{------------------------------------------------------------------------------}
constructor TScrolledWindow.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  fCompStyle := csScrolledWindow;
  CreateComponent(AOwner);

  Left := 1;
  Top := 1;
  Height := 500;
  Width := 300;

  AddControl;
end;

{------------------------------------------------------------------------------}
{  TScrolledWindow Destructor                                                  }
{------------------------------------------------------------------------------}
destructor TScrolledWindow.Destroy;
begin
  inherited Destroy;
end;

// included by controls.pp

