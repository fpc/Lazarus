{%MainUnit ../extctrls.pp}
{******************************************************************************
                                  TNotebook
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

function TNotebook.GetActiveNotebookPageComponent: TPage;
begin
  Result:=TPage{%H-}(GetActivePageComponent);
end;

function TNotebook.GetNoteBookPage(Index: Integer): TPage;
begin
  Result:=TPage{%H-}(GetPage(Index));
end;

procedure TNotebook.SetActiveNotebookPageComponent(const AValue: TPage);
begin
  SetActivePageComponent(AValue);
end;

{------------------------------------------------------------------------------
  TNotebook Constructor
 ------------------------------------------------------------------------------}
constructor TNotebook.Create(TheOwner: TComponent);
begin
  PageClass:=TPage{%H-};
  inherited Create(TheOwner);
end;

