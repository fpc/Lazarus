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
  TWinCEListStringList = Class(TStrings)
  Private
    FWinCEList: HWND;
    FSorted: Boolean;
    FSender: TWinControl;
    FlastInsertedIndex: Integer;
  protected
    //WinCE Flags
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

  TWinCEComboBoxStringList = class(TWinCEListStringList)
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
    
  PWinCECheckListBoxItemRecord = ^TWinCECheckListBoxItemRecord;
  TWinCECheckListBoxItemRecord = record
    TheObject: TObject;
    State: TCheckBoxState;
  end;

  { TWinCECheckListBoxStrings }

  TWinCECheckListBoxStrings = class(TWinCEListStringList)
  private
    FDefaultItem: TWinCECheckListBoxItemRecord;
    function GetState(AIndex: Integer): TCheckBoxState;
    function GetItemRecord(const Index: Integer; const CreateNew: boolean): PWinCECheckListBoxItemRecord;
    procedure SetItemRecord(const Index: Integer; ItemRecord: PWinCECheckListBoxItemRecord);
    procedure SetState(AIndex: Integer; const AValue: TCheckBoxState);
  protected
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
  public
    constructor Create(List : HWND; TheOwner: TWinControl);
    class procedure DeleteItemRecords(const List: HWND);
    class procedure DeleteItemRecord(const List: HWND; const Index: integer);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    property State[AIndex: Integer]: TCheckBoxState read GetState write SetState;
  end;

  TWinCECListStringList = Class(TStrings)
  Private
    FWinCECList: HWND;
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

