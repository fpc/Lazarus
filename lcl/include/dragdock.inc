{%MainUnit ../controls.pp}

{*****************************************************************************
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


{-------------------------------------------------------------------------------
  function HostDockSiteManagerAvailable(HostDockSite: TWinControl): boolean;
-------------------------------------------------------------------------------}
function HostDockSiteManagerAvailable(HostDockSite: TWinControl): boolean;
begin
  Result:=(HostDockSite<>nil) and HostDockSite.UseDockManager
          and (HostDockSite.DockManager<>nil);
end;


// included by controls.pp
