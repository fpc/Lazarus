{ $Id$ }
{
 /***************************************************************************
                                 Lazarus.pp
                             -------------------
                   This is the lazarus editor program.

                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


 ***************************************************************************/

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
}

program Lazarus;

{$mode objfpc}{$H+}

{$I ide.inc}

{$IFDEF WIN32}
  {$R lazarus.res}
{$ENDIF}

{off $DEFINE IDE_MEM_CHECK}

uses
  //cmem,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Interfaces,
  Forms, LCLProc,
  Splash,
  Main,
  AboutFrm,
  // use the custom IDE static packages AFTER 'main'
  {$IFDEF AddStaticPkgs}
  {$I staticpackages.inc}
  {$ENDIF}
  {$IFDEF BigIDE}
    RunTimeTypeInfoControls, Printer4Lazarus, Printers4LazIDE,
    CGILaz, CGILazIDE,
    MemDSLaz, SDFLaz, TurboPowerIPro, JPEGForLazarus,
    FPCUnitTestRunner, FPCUnitIDE, ProjTemplates, TAChartLazarusPkg,
    {$IFDEF windows}
      SQLDBLaz, DBFLaz,
    {$ENDIF}
    {$IFDEF Linux}
      SQLDBLaz, DBFLaz,
    {$ENDIF}
  {$ENDIF}
  MainBase;

{$I revision.inc}

begin
  LazarusRevisionStr:=RevisionStr;
  Application.Initialize;
  TMainIDE.ParseCmdLineOptions;
  if Application.Terminated then exit;

  // Show splashform
  if ShowSplashScreen then begin
    SplashForm := TSplashForm.Create(nil);
    SplashForm.Show;
    Application.ProcessMessages; // process splash paint message
  end;

  MainIDE:=TMainIDE.Create(Application);
  MainIDE.CreateOftenUsedForms;
  MainIDE.StartIDE;
  {$IFDEF IDE_MEM_CHECK}
  CheckHeapWrtMemCnt('lazarus.pp: TMainIDE created');
  {$ENDIF}

  try
    Application.Run;
  except
    debugln('lazarus.pp - unhandled exception');
    Halt;
  end;
  if (SplashForm<>nil) then begin
    SplashForm.Free;
    SplashForm:=nil;
  end;

  debugln('LAZARUS END - cleaning up ...');

  // free the IDE, so everything is freed before the finalization sections
  MainIDE.Free;
  MainIDE:=nil;
end.

