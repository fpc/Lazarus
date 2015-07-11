{%MainUnit ../controls.pp}

{*****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
