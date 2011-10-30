// included by win32int.pp

{******************************************************************************
                                 win32listslh.inc
  TWin32ListStringList, TWin32ComboBoxStringList and TWin32CheckListBoxStrings

 ******************************************************************************

 *****************************************************************************
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

{$IFOPT H+}
  {$DEFINE H_PLUS}
{$ELSE}
  {$H+}
{$ENDIF}

Type

  { TWin32ListStringList }

  TWin32ListStringList = Class(TStrings)
  private
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
    FFlagGetCaretIndex: cardinal;
    FFlagSetCaretIndex: cardinal;
    FFlagGetSelected: cardinal;
    FFlagsetSelected: cardinal;
    FFlagInitStorage: cardinal;
  protected
    function Get(Index: Integer): String; Override;
    function GetCount: Integer; Override;
    function GetObject(Index: Integer): TObject; Override;
    procedure Put(Index: integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); Override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;

    procedure InitFlags; virtual;
    procedure SetSorted(Val: Boolean); Virtual;
    function SaveData(AIndex: Integer): Pointer; virtual;
    procedure RestoreData(AIndex: Integer; AData: Pointer); virtual;
  public
    constructor Create(List : HWND; TheOwner: TWinControl);
    function Add(const S: string): Integer; override;
    procedure AddStrings(TheStrings: TStrings); override;
    procedure Clear; Override;
    procedure Delete(Index: Integer); Override;
    procedure Insert(Index: Integer; Const S: String); Override;
    procedure Sort; Virtual;
    Property Sorted: Boolean Read FSorted Write SetSorted;
  end;

  { TWin32ComboBoxStringList }

  TWin32ComboBoxStringList = class(TWin32ListStringList)
  private
    FDropDownCount: Integer;
    procedure SetDropDownCount(const AValue: integer);
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
    property DropDownCount: integer read FDropDownCount write SetDropDownCount;
  end;
    
  PWin32CheckListBoxItemRecord = ^TWin32CheckListBoxItemRecord;
  TWin32CheckListBoxItemRecord = record
    TheObject: TObject;
    State: TCheckBoxState;
    Enabled: Boolean;
  end;

  { TWin32CheckListBoxStrings }

  TWin32CheckListBoxStrings = class(TWin32ListStringList)
  private
    FDefaultItem: TWin32CheckListBoxItemRecord;
    function GetEnabled(Index: Integer): Boolean;
    function GetState(Index: Integer): TCheckBoxState;
    function GetItemRecord(const Index: Integer; const CreateNew: boolean): PWin32CheckListBoxItemRecord;
    procedure SetEnabled(Index: Integer; const AValue: Boolean);
    procedure SetItemRecord(const Index: Integer; ItemRecord: PWin32CheckListBoxItemRecord);
    procedure SetState(Index: Integer; const AValue: TCheckBoxState);
  protected
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    function SaveData(AIndex: Integer): Pointer; override;
    procedure RestoreData(AIndex: Integer; AData: Pointer); override;
  public
    constructor Create(List: HWND; TheOwner: TWinControl);
    class procedure DeleteItemRecords(const List: HWND);
    class procedure DeleteItemRecord(const List: HWND; const Index: integer);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    property Enabled[Index: Integer]: Boolean read GetEnabled write SetEnabled;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
  end;

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}
