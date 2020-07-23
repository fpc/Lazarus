{
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}

// !!! if you change any of these flags, you have to make cleanide !!!
{$inline on}

{ $DEFINE IDE_VERBOSE}

{ $DEFINE IDE_MEM_CHECK}

{ $DEFINE IDE_DEBUG}

{ $DEFINE VerboseIDEMultiForm}

{off $apptype console}

// debug form load/save
{ $DEFINE VerboseJITForms}
{ $DEFINE VerboseFormEditor}

// TODO: Test on all platforms
{$IFNDEF DisableAsyncProcess}
  {$IFDEF Linux}
    {$IFDEF CPUI386}
      {off $DEFINE UseAsyncProcess}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF windows}
    {$DEFINE UseAsyncProcess}
  {$ENDIF}
{$ENDIF}

{$IF FPC_FULLVERSION<20700}
// fpc 2.6.2 and 2.6.3 have a bug with -O2 and for..in
// see http://bugs.freepascal.org/view.php?id=25588
  {$OPTIMIZATION NOSTACKFRAME}
{$ENDIF}

{$IF defined(Windows) and defined(HEAPTRC_WINDOW) and (FPC_FULLVERSION>=20701)}
  {$DEFINE EnableRedirectStdErr}
{$ENDIF}

{$IF FPC_FULLVERSION>30100}
  {$warn 6058 off}
{$ENDIF}

// end.

