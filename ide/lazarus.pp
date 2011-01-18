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

{off $DEFINE IDE_MEM_CHECK}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  {$IF defined(Unix) and not defined(VER2_2_0) and not defined(VER2_2_2)}
  clocale,
  {$ENDIF}
  SysUtils,
  Interfaces,
  Forms, LCLProc,
  LazConf,
  Splash,
  Main,
  AboutFrm,
  // use the custom IDE static packages AFTER 'main'
  {$IFDEF AddStaticPkgs}
  {$I staticpackages.inc}
  {$ENDIF}
  {$IFDEF BigIDE}
    RunTimeTypeInfoControls, Printer4Lazarus, Printers4LazIDE,
    MemDSLaz, SDFLaz,
    TurboPowerIPro, {$ifdef UseTurbopowerInHelp}TurboPowerIProDsgn,{$endif}
    {$ifdef UseJCF}jcfidelazarus,{$endif}
    {$ifdef UseCHMHelp}chmhelppkg,{$endif}
    FPCUnitTestRunner, FPCUnitIDE, ProjTemplates, TAChartLazarusPkg,
    TodoListLaz,
    {$IFDEF windows}
      SQLDBLaz, DBFLaz,
    {$ENDIF}
    {$IFDEF Linux}
      SQLDBLaz, DBFLaz,
    {$ENDIF}
  {$ENDIF}
  MainBase;

{$I revision.inc}
{$R lazarus.res}

begin
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('lazarus.pp: begin');{$ENDIF}

  // When quick rebuilding the IDE, FPC rebuilds only the lazarus.pp, so any
  // flag that should work with quick build must be set here.
  KeepInstalledPackages:={$IFDEF BigIDE}True{$ELSE}False{$ENDIF};

  // end of build flags
  
  LazarusRevisionStr:=RevisionStr;
  Application.Title:='Lazarus';
  OnGetApplicationName:=@GetLazarusApplicationName;
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
  try
    MainIDE.StartIDE;
  except
    Application.HandleException(MainIDE);
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('lazarus.pp: TMainIDE created');{$ENDIF}

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

