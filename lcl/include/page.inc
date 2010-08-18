{%MainUnit ../extctrls.pp}
{******************************************************************************
                                     TPage
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

{ TPage }

{ TUNBPage }

constructor TUNBPage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  ControlStyle := ControlStyle +
    [csAcceptsControls, csDesignFixedBounds, csNoDesignVisible, csNoFocus];

  // height and width depends on parent, align to client rect
  Align := alClient;
  Caption := '';
  Visible := False;
end;

destructor TUNBPage.Destroy;
begin
  {$ifdef DEBUG_NEW_NOTEBOOK}
  DebugLn('[TUNBPage.Destroy]');
  {$endif}

  if (Parent <> nil) and (Parent is TUntabbedNotebook) then
  begin
    {$ifdef DEBUG_NEW_NOTEBOOK}
    DebugLn('[TUNBPage.Destroy] FPageList.Remove(Self)');
    {$endif}
    TUntabbedNotebook(Parent).FPageList.Remove(Self);
  end;

  inherited Destroy;
end;



// included by extctrls.pp
