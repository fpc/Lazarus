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
{       TForm Destructor                                                       }
{------------------------------------------------------------------------------}
destructor TForm.Destroy;
begin
  {$IFDEF VER1_0}
  BeforeDestruction;
  {$ENDIF}
  Assert(False, 'Trace:Destroying signals for TForm');
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{       TForm Constructor                                                      }
{------------------------------------------------------------------------------}
constructor TForm.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;


