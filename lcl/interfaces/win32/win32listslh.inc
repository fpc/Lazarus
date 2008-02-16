// included by win32int.pp

{******************************************************************************
                                 win32listslh.inc
                 TWin32ListStringList and TWin32CListStringList

 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

  { TWin32ListStringList }

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
    FFlagInitStorage: cardinal;
  Protected
    function Add(const S: string): Integer; override;
    Function Get(Index: Integer): String; Override;
    Function GetCount: Integer; Override;
    Function GetObject(Index: Integer): TObject; Override;
    procedure Put(Index: integer; const S: string); override;
    Procedure PutObject(Index: Integer; AObject: TObject); Override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;

    procedure InitFlags; virtual;
    Procedure SetSorted(Val: Boolean); Virtual;
  Public
    Constructor Create(List : HWND; TheOwner: TWinControl);
    procedure AddStrings(TheStrings: TStrings); override;
    Procedure Clear; Override;
    Procedure Delete(Index: Integer); Override;
    Procedure Insert(Index: Integer; Const S: String); Override;
    Procedure Sort; Virtual;
    Property Sorted: Boolean Read FSorted Write SetSorted;
  End;

  { TWin32ComboBoxStringList }

  TWin32ComboBoxStringList = class(TWin32ListStringList)
  private
    FEditHeight: Integer;
    FItemHeight: Integer;
    FDropDownCount: Integer;
  protected
    function GetComboHeight: integer;
    procedure InitFlags; override;
    procedure UpdateComboHeight;
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
    State: TCheckBoxState;
  end;

  { TWin32CheckListBoxStrings }

  TWin32CheckListBoxStrings = class(TWin32ListStringList)
  private
    FDefaultItem: TWin32CheckListBoxItemRecord;
    function GetState(Index: Integer): TCheckBoxState;
    function GetItemRecord(const Index: Integer; const CreateNew: boolean): PWin32CheckListBoxItemRecord;
    procedure SetItemRecord(const Index: Integer; ItemRecord: PWin32CheckListBoxItemRecord);
    procedure SetState(Index: Integer; const AValue: TCheckBoxState);
  protected
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
  public
    constructor Create(List : HWND; TheOwner: TWinControl);
    class procedure DeleteItemRecords(const List: HWND);
    class procedure DeleteItemRecord(const List: HWND; const Index: integer);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
  end;

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}
