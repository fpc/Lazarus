{******************************************************************************
                                 win32listslh.inc
                 TWin32ListStringList and TWin32CListStringList

 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{$IFOPT H+} 
  {$DEFINE H_PLUS} 
{$ELSE}
  {$H+}
{$ENDIF} 

Type
  TWin32ListStringList = Class(TStrings)
  Private
    FWin32List: HWND;
    FSorted: Boolean;
    FSender: TControl;
    FOrigHeight: Integer;
    //Win32 Flags
    FFlagSort: Cardinal;
    FFlagGetText:Cardinal;
    FFlagGetTextLen:Cardinal;
    FFlagGetCount:Cardinal;
    FFlagResetContent:Cardinal;
    FFlagDeleteString:Cardinal;
    FFlagInsertString:Cardinal;
  Protected
    Function Get(Index: Integer): String; Override;
    Function GetCount: Integer; Override;
    Procedure SetSorted(Val: Boolean); Virtual;
  Public
    Constructor Create(List: HWND);
    Procedure Assign(Source: TPersistent); Override;
    Procedure Clear; Override;
    Procedure Delete(Index: Integer); Override;
    Procedure Insert(Index: Integer; Const S: String); Override;
    Procedure Sort; Virtual;
    Property Sorted: Boolean Read FSorted Write SetSorted;
  End;

  TWin32CListStringList = Class(TStrings)
  Private
    FWin32CList: HWND;
    FSorted: Boolean;
  Protected
    Function Get(Index: Integer): String; Override;
    Function GetCount: Integer; Override;
    Function GetObject(Index: Integer): TObject; Override;
    Procedure PutObject(Index: Integer; AObject: TObject); Override;
    Procedure SetSorted(Val: Boolean); Virtual;
  Public
    Constructor Create(List: HWND);
    Procedure Assign(Source: TPersistent); Override;
    Procedure Clear; Override;
    Procedure Delete(Index: Integer); Override;
    Procedure Insert(Index: Integer; Const S: String); Override;
    Procedure Sort; Virtual;
    Property Sorted: Boolean Read FSorted Write SetSorted;
  End;

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.4  2002/11/15 23:43:54  mattias
  applied patch from Karl Brandt

  Revision 1.3  2002/05/10 07:43:48  lazarus
  MG: updated licenses

  Revision 1.2  2002/02/04 10:54:33  lazarus
  Keith:
    * Fixes for Win32
    * Added new listviewtest.pp example

  Revision 1.1  2002/01/06 23:09:53  lazarus
  MG: added missing files


}
