{%mainunit gtkwsstdctrls.pp}
{ $Id$}
{
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

  PGtkTextInsertEvent = ^TGtkTextInsertEvent;
  TGtkTextInsertEvent = procedure (NewText: pgchar; NewTextLength: gint; position: pgint) of object;
  
  PGtkTextDeleteEvent = ^TGtkTextDeleteEvent;
  TGtkTextDeleteEvent = procedure(StartPos, EndPos: gint) of object;
  { TGtkMemoStrings }

  TGtkMemoStrings = class(TStrings)
  private
    FGtkText : PGtkText;
    FFreezed: boolean;
    FOwner: TWinControl;
    fTextInsertEvent: TGtkTextInsertEvent;
    fTextDeleteEvent: TGtkTextDeleteEvent;
    FCachedCount: Integer;
    FLineStartCapacity: Integer;
    FLineStartPos: array of Integer;
  protected
    fModified: Boolean;
    fDeleting: Boolean;
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    procedure AdjustIndices(FromIndex, ToIndex: Integer; ACount: Integer; Back: Boolean);
    procedure InsertIndices(StartIndex: Integer; AValue: array of Integer; ACount: Integer);
    procedure DeleteIndices(StartIndex, ACount: Integer);
    procedure SetCachedCount(AValue: Integer);
    procedure TextInserted(NewText: pgchar; NewTextLength: gint; position: pgint);
    procedure TextDeleted(StartPos, EndPos: gint);
    function PositionToLine(Position: Integer; StartLine: Integer = 0): Integer;
    procedure SetUpdateState(Updating: Boolean); override;
    //procedure SetSorted(Val : boolean); virtual;
  public
    constructor Create(GtkText : PGtkText; TheOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure SetText(TheText: PChar); override;
    //procedure Sort; virtual;
  public
    //property Sorted: boolean read FSorted write SetSorted;
    property Owner: TWinControl read FOwner;
  end;
