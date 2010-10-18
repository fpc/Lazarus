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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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

{off $define UseTurbopowerInHelp}
{$define UseCHMHelp}
{$define UseJCF}

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

// end.

