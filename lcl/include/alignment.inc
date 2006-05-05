{%MainUnit ../extctrls.pp}
{
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

{------------------------------------------------------------------------------}
{   TAlignment Constructor                                                     }
{------------------------------------------------------------------------------}
constructor TAlignment.Create(AOwner : TComponent);
begin
  //Assert(False, 'Trace:Inside of TAlignment Constructor');
  inherited Create(AOwner);
  //Assert(False, 'Trace:Call of inherited create in TAlignment Constructor Success');
  fCompStyle := csAlignMent;
  //Assert(False, 'Trace:Calling CreateComponent from TAlignment Constructor');

  CreateComponent(AOwner);

  Height := 20;
  Width := 100;
  Top := 100;
  Left := 1;

  AddControl;
  Show;
end;

{------------------------------------------------------------------------------}
{   TAlignment Destructor                                                      }
{------------------------------------------------------------------------------}
destructor TAlignment.Destroy;
begin
  inherited Destroy;
end;