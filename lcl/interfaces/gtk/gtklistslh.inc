{******************************************************************************
                                 gtklistslh.inc
                 TGtkListStringList and TGtkCListStringList
                 
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

type
  { TGtkListStringList }

  TGtkListStringsState = (glsItemCacheNeedsUpdate,glsCountNeedsUpdate);
  TGtkListStringsStates = set of TGtkListStringsState;
  
  PPGtkListItem = ^PGtkListItem;

  TGtkListStringList = class(TStrings)
  private
    FGtkList : PGtkList;
    FOwner: TWinControl;
    FSorted : boolean;
    FStates: TGtkListStringsStates;
    FCachedCount: integer;
    FCachedCapacity: integer;
    FCachedItems: PPGtkListItem;
    FUpdateCount: integer;
    FWithCheckBox: Boolean;
  protected
    function GetListItem(Index: integer): PGtkListItem;
    function GetLabel(Index: integer): PGtkLabel;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AnObject: TObject); override;
    procedure SetSorted(Val : boolean); virtual;
    procedure CheckForInvalidFocus;
    procedure ConnectItemCallbacks(Index: integer);
    procedure ConnectItemCallbacks(Li: PGtkListItem); virtual;
    procedure ConnectAllCallbacks; virtual;
    procedure RemoveItemCallbacks(Index: integer);
    procedure RemoveItemCallbacks(AItem: PGtkListItem); virtual;
    procedure RemoveAllCallbacks; virtual;
    procedure UpdateItemCache;
    function CacheValid: boolean;
  public
    constructor Create(List : PGtkList; TheOwner: TWinControl;
                       const AWithCheckBox: Boolean);
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Assign(Source : TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index : integer; const S: string); override;
    function GetInsertPosition(const S: string): integer;
    procedure Move(FromIndex, ToIndex: Integer); override;
    procedure Sort; virtual;
    function IsEqual(List: TStrings; CompareObjects: boolean): boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ConsistencyCheck;
  public
    property Sorted: boolean read FSorted write SetSorted;
    property Owner: TWinControl read FOwner;
  end;

