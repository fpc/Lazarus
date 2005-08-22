// included by win32int.pp

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
    FSender: TWinControl;
    FlastInsertedIndex: Integer;
  protected
    //Win32 Flags
    FFlagSort: Cardinal;
    FFlagGetText: Cardinal;
    FFlagGetTextLen: Cardinal;
    FFlagGetCount: Cardinal;
    FFlagResetContent: Cardinal;
    FFlagDeleteString: Cardinal;
    FFlagInsertString: Cardinal;
    FFlagAddString: Cardinal;
    FFlagGetItemData: Cardinal;
    FFlagSetItemData: Cardinal;
    FFlagGetItemIndex: cardinal;
    FFlagSetItemIndex: cardinal;
    FFlagGetSelected: cardinal;
    FFlagsetSelected: cardinal;
  Protected
    function Add(const S: string): Integer; override;
    Function Get(Index: Integer): String; Override;
    Function GetCount: Integer; Override;
    Function GetObject(Index: Integer): TObject; Override;
    procedure Put(Index: integer; const S: string); override;
    Procedure PutObject(Index: Integer; AObject: TObject); Override;

    procedure InitFlags; virtual;
    Procedure SetSorted(Val: Boolean); Virtual;
  Public
    Constructor Create(List : HWND; TheOwner: TWinControl);
    Procedure Assign(Source: TPersistent); Override;
    Procedure Clear; Override;
    Procedure Delete(Index: Integer); Override;
    Procedure Insert(Index: Integer; Const S: String); Override;
    Procedure Sort; Virtual;
    Property Sorted: Boolean Read FSorted Write SetSorted;
  End;

  TWin32ComboBoxStringList = class(TWin32ListStringList)
  private
    FEditHeight: Integer;
    FItemHeight: Integer;
    FDropDownCount: Integer;
  protected
    function  GetComboHeight: integer;
    procedure InitFlags; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure Insert(Index: integer; const S: string); override;

    property ComboHeight: integer read GetComboHeight;
  end;
    
  PWin32CheckListBoxItemRecord = ^TWin32CheckListBoxItemRecord;
  TWin32CheckListBoxItemRecord = record
    TheObject: TObject;
    Checked: Boolean;
  end;

  TWin32CheckListBoxStrings = class(TWin32ListStringList)
  private
    FDefaultItem: TWin32CheckListBoxItemRecord;
    function GetChecked(const Index: Integer): Boolean;
    procedure SetChecked(const Index: Integer; const AValue: Boolean);
    function GetItemRecord(const Index: Integer; const CreateNew: boolean): PWin32CheckListBoxItemRecord;
    procedure SetItemRecord(const Index: Integer; ItemRecord: PWin32CheckListBoxItemRecord);
  protected
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
  public
    constructor Create(List : HWND; TheOwner: TWinControl);
    class procedure DeleteItemRecords(const List: HWND);
    class procedure DeleteItemRecord(const List: HWND; const Index: integer);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    property Checked[const Index: Integer]: Boolean read GetChecked write SetChecked;
  end;

  TWin32CListStringList = Class(TStrings)
  Private
    FWin32CList: HWND;
    FSender: TWinControl; // Needed to recreate the window
    FSorted: Boolean;
  Protected
    Function Get(Index: Integer): String; Override;
    Function GetCount: Integer; Override;
    Function GetObject(Index: Integer): TObject; Override;
    Procedure PutObject(Index: Integer; AObject: TObject); Override;
    Procedure SetSorted(Val: Boolean); Virtual;
  Public
    Constructor Create(List : HWND; TheOwner: TWinControl);
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

{
  $Log$
  Revision 1.19  2005/06/10 18:57:12  vincents
  fixed objects with a sorted listbox   from Jesus

  Revision 1.18  2005/04/11 12:35:17  micha
  fixed: listbox loses selection when updating selected item (fixes 780)

  Revision 1.17  2004/11/04 15:12:35  micha
  remove usage of fcompstyle in twin32liststringlist by using descendent class for combobox specific things

  Revision 1.16  2004/05/08 12:32:32  micha
  fix combobox height; whatever the lcl passes as height for combobox, always calculate our own

  Revision 1.15  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.14  2003/12/26 09:28:05  micha
  fix itemdata (string+object) for combobox (from vincent)

  Revision 1.13  2003/12/21 11:50:39  micha
  fix AV when retrieving object (from vincent)

  Revision 1.12  2003/12/15 21:57:16  micha
  checklistbox, implement object+checked; from vincent

  Revision 1.11  2003/11/18 07:20:40  micha
  added "included by" notice at top of file

  Revision 1.10  2003/10/02 11:18:09  mattias
  clean ups from Karl

  Revision 1.9  2003/08/26 08:12:33  mattias
  applied listbox/combobox patch from Karl

  Revision 1.8  2003/08/13 21:23:10  mattias
  fixed log

  Revision 1.7  2003/08/13 16:26:07  mattias
  fixed combobox height from Karl

  Revision 1.6  2003/07/28 06:42:42  mattias
  removed debuggging SetName, Patch from Karl Brandt

  Revision 1.5  2003/01/01 10:46:59  mattias
  fixes for win32 listbox/combobox from Karl Brandt

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
