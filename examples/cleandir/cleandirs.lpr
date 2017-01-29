{ Directory cleaning service

  Copyright (C) 2007 Michael Van Canneyt michael@freepascal.org

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

Program cleandirs;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}{$ENDIF}
  {$IF FPC_FULLVERSION<20602}
  {$ERROR this example requires at least fpc 2.6.2}
  {$ENDIF}
  daemonapp,
  sysutils
  { add your units here }, svcmap, svccleandirs, dircleaner;

begin
  OnGetApplicationName:=@CleanDirApp;
  Application.Title:='Directory Cleaning service';
  Application.Initialize;
  Application.Run;
end.
