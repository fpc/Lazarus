{%MainUnit winceint.pp}

{******************************************************************************
                                 win32listslh.inc
  TWinCEListStringList, TWinCEComboBoxStringList and TWinCECheckListBoxStrings

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

type
  TWinCEListStringList = class(TStrings)
  private
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
  protected
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;

    procedure InitFlags; virtual;
    procedure SetSorted(Val: Boolean); Virtual;
  public
    Constructor Create(List : HWND; TheOwner: TWinControl);
    procedure Assign(Source: TPersistent); Override;
    function Add(const S: string): Integer; override;
    procedure Clear; Override;
    procedure Delete(Index: Integer); Override;
    procedure Insert(Index: Integer; Const S: String); Override;
    procedure Sort; Virtual;
    property Sorted: Boolean Read FSorted Write SetSorted;
  end;

  { TWinCEComboBoxStringList }

  TWinCEComboBoxStringList = class(TWinCEListStringList)
  private
    FEditHeight: Integer;
    FItemHeight: Integer;
    FDropDownCount: Integer;
  protected
    function  GetComboHeight: integer;
    procedure InitFlags; override;
    procedure UpdateComboHeight;
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
    procedure Move(CurIndex, NewIndex: Integer); override;
    property State[AIndex: Integer]: TCheckBoxState read GetState write SetState;
  end;

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}

