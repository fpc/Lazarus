(******************************************************************************
                                 gtklistslh.inc
                 TGtkListStringList and TGtkCListStringList
                 
 ******************************************************************************)

{$IFOPT H+} 
  {$DEFINE H_PLUS} 
{$ELSE}
  {$H+}
{$ENDIF} 
type

  TGtkListStringList = class(TStrings)
  private
    FGtkList : PGtkList;
    FSorted : boolean;
  protected
    function Get(Index : Integer) : string; override;
    function GetCount : integer; override;
	  procedure SetSorted(Val : boolean); virtual;
    public
    constructor Create(List : PGtkList);
    procedure Assign(Source : TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
  	procedure Sort; virtual;
  	property Sorted : boolean read FSorted write SetSorted;
  end;

  TGtkCListStringList = class(TStrings)
  private
    FGtkCList : PGtkCList;
    FSorted : boolean;
  protected
    function Get(Index : Integer) : string; override;
    function GetCount : integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetSorted(Val : boolean); virtual;
  public
    constructor Create(List : PGtkCList);
    procedure Assign(Source : TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure Sort; virtual;
    property Sorted : boolean read FSorted write SetSorted;
  end;

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

  Revision 1.1  2000/03/30 22:51:42  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.3  2000/03/04 00:05:21  lazarus
  MWE: added changes from Hans (HJO)

  Revision 1.2  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

}
