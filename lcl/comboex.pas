{ Visual components TComboBoxEx and TCheckComboBox

  Copyright (C) 2014 Vojtěch Čihák, e-mail: cihakvjtch@seznam.cz

  This library is free software; you can redistribute it and/or modify it under the terms of the
  GNU Library General Public License as published by the Free Software Foundation; either version
  2 of the License, or (at your option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this
  library with independent modules to produce an executable, regardless of the license terms of
  these independent modules,and to copy and distribute the resulting executable under terms of
  your choice, provided that you also meet, for each linked independent module, the terms and
  conditions of the license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this exception to your
  version of the library, but you are not obligated to do so. If you do not wish to do so, delete
  this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
  the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this
  library; if not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth
  Floor, Boston, MA 02110-1335, USA.
}

unit ComboEx;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types,
  LCLIntf, LCLType, LMessages, LResources, LazLoggerBase,
  ImgList, Controls, StdCtrls, ComCtrls, ExtCtrls, Graphics, GraphUtil,
  Themes, Forms;

type
  {$PACKENUM 2}
  TAutoCompleteOption = (acoAutoSuggest, acoAutoAppend, acoSearch, acoFilterPrefixes,
                         acoUseTab, acoUpDownKeyDropsList, acoRtlReading);
  TAutoCompleteOptions = set of TAutoCompleteOption;
  TComboBoxExStyle = (csExDropDown, csExSimple, csExDropDownList);
  TComboBoxExStyleEx = (csExCaseSensitive, csExNoEditImage, csExNoEditImageIndent,
                        csExNoSizeLimit, csExPathWordBreak);
  TComboBoxExStyles = set of TComboBoxExStyleEx;
  TCustomData = Pointer;
  TListControlItems = class;  { forward }
  TListItemsCompare = function (AList: TListControlItems; AItem1, AItem2: Integer): Integer;
  TListItemsSortType = TSortType;
  { Events }
  TCheckItemChange = procedure(Sender: TObject; AIndex: Integer) of object;
  TListControlItem = class;  { forward }
  TListCompareEvent = function(AList: TListControlItems; AItem1, AItem2: TListControlItem): Integer of object;

  { TListControlItem }
  TListControlItem = class(TCollectionItem)
  private
    FCaption: TTranslateString;
    FData: TCustomData;
    FImageIndex: SmallInt;
    procedure SetCaption(const AValue: TTranslateString);
    procedure SetImageIndex(AValue: SmallInt);
  public
    property Data: TCustomData read FData write FData;
    constructor Create(ACollection: TCollection); override;
  published
    property Caption: TTranslateString read FCaption write SetCaption;
    property ImageIndex: SmallInt read FImageIndex write SetImageIndex default -1;
  end;

  { TComboExItem }
  TComboExItem = class(TListControlItem)
  private
    FIndent: SmallInt;
    FOverlayImageIndex: SmallInt;
    FSelectedImageIndex: SmallInt;
    procedure SetIndent(AValue: SmallInt);
    procedure SetOverlayImageIndex(AValue: SmallInt);
    procedure SetSelectedImageIndex(AValue: SmallInt);
  protected const
    cDefCaption = 'ItemEx';
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Indent: SmallInt read FIndent write SetIndent default -1;
    property OverlayImageIndex: SmallInt read FOverlayImageIndex write SetOverlayImageIndex default -1;
    property SelectedImageIndex: SmallInt read FSelectedImageIndex write SetSelectedImageIndex default -1;
  end;

  { TListControlItems }
  TListControlItems = class(TOwnedCollection)
  private
    FCaseSensitive: Boolean;
    FSortType: TListItemsSortType;
    FOnCompare: TListCompareEvent;
    FCompare: TListItemsCompare;
    function GetItems(AIndex: Integer): TListControlItem;
    procedure SetCaseSensitive(AValue: Boolean);
    procedure SetSortType(AValue: TListItemsSortType);
  protected
    function CompareItems(AItem1, AItem2: TListControlItem): Integer; virtual;
    function DoCustomSort(AItem1, AItem2: TListControlItem): Integer;
    function DoOnCompare(AItem1, AItem2: TListControlItem): Integer;
    procedure Update(AItem: TCollectionItem); override;
  public
    function Add: TListControlItem;
    procedure CustomSort(ACompare: TListItemsCompare);
    procedure Sort;
    property Items[AIndex: Integer]: TListControlItem read GetItems; default;
  published
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive default False;
    property SortType: TListItemsSortType read FSortType write SetSortType default stNone;
    property OnCompare: TListCompareEvent read FOnCompare write FOnCompare;
  end;

  { TComboExItems }
  TComboExItems = class(TListControlItems)
  private
    function GetComboItems(AIndex: Integer): TComboExItem;
  protected
    FAddingOrDeletingItem: Boolean;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TComboExItem;
    function AddItem(const ACaption: string; AImageIndex: SmallInt = -1;
                 AOverlayImageIndex: SmallInt = -1; ASelectedImageIndex: SmallInt = -1;
                 AIndent: SmallInt = -1; AData: TCustomData = nil): TComboExItem;
    function Insert(AIndex: Integer): TComboExItem;
    property ComboItems[AIndex: Integer]: TComboExItem read GetComboItems; default;
  end;

  { TCustomComboBoxEx }
  TCustomComboBoxEx = class(TCustomComboBox)
  private
    FAutoCompleteOptions: TAutoCompleteOptions;
    FImages: TCustomImageList;
    FItemsEx: TComboExItems;
    FStyle: TComboBoxExStyle;
    FStyleEx: TComboBoxExStyles;
    FImagesWidth: Integer;
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImagesWidth(const aImagesWidth: Integer);
    procedure SetStyle(AValue: TComboBoxExStyle); reintroduce;
    procedure SetStyleEx(AValue: TComboBoxExStyles);
  protected const
    cDefAutoCompOpts = [acoAutoAppend];
    cDefStyle = csExDropDown;
  protected
    FNeedMeasure: Boolean;
    FRightToLeft: Boolean;
    FTextHeight: SmallInt;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    procedure FontChanged(Sender: TObject); override;
    procedure InitializeWnd; override;
    procedure SetItemHeight(const AValue: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Add: Integer; overload;
    procedure Add(const ACaption: string; AIndent: SmallInt = -1;
                  AImgIdx: SmallInt = -1; AOverlayImgIdx: SmallInt = -1;
                  ASelectedImgIdx: SmallInt = -1); overload;
    procedure AddItem(const Item: String; AnObject: TObject); override;
    procedure AssignItemsEx(AItems: TStrings); overload;
    procedure AssignItemsEx(AItemsEx: TComboExItems); overload;
    procedure Clear; override;
    procedure Delete(AIndex: Integer);
    procedure DeleteSelected;
    procedure Insert(AIndex: Integer; const ACaption: string; AIndent: SmallInt = -1;
                     AImgIdx: SmallInt = -1; AOverlayImgIdx: SmallInt = -1;
                     ASelectedImgIdx: SmallInt = -1);
    property AutoCompleteOptions: TAutoCompleteOptions read FAutoCompleteOptions
             write FAutoCompleteOptions default cDefAutoCompOpts;
    property Images: TCustomImageList read FImages write SetImages;
    property ImagesWidth: Integer read FImagesWidth write SetImagesWidth default 0;
    property ItemsEx: TComboExItems read FItemsEx write FItemsEx;
    property Style: TComboBoxExStyle read FStyle write SetStyle default cDefStyle;
    property StyleEx: TComboBoxExStyles read FStyleEx write SetStyleEx default [];
  end;

  { TComboBoxEx }
  TComboBoxEx = class(TCustomComboBoxEx)
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteOptions;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property Images;
    property ImagesWidth;
    property ItemHeight;
    property ItemsEx;  { do not change order; ItemsEx must be before ItemIndex }
    property ItemIndex;
    property ItemWidth;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Style;
    property StyleEx;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  end;

  { TCheckComboItemState }
  TCheckComboItemState = class
  public
    State: TCheckBoxState;
    Enabled: Boolean;
    Data: TObject;
  end;

  { TCustomCheckCombo }
  TCustomCheckCombo = class(TCustomComboBox)
  private
    FAllowGrayed: Boolean;
    FOnItemChange: TCheckItemChange;
    procedure AsyncCheckItemStates(Data: PtrInt);
    function GetChecked(AIndex: Integer): Boolean;
    function GetCount: Integer;
    function GetItemEnabled(AIndex: Integer): Boolean;
    function GetObject(AIndex: Integer): TObject;
    function GetState(AIndex: Integer): TCheckBoxState;
    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    procedure SetItemEnabled(AIndex: Integer; AValue: Boolean);
    procedure SetObject(AIndex: Integer; AValue: TObject);
    procedure SetState(AIndex: Integer; AValue: TCheckBoxState);
  protected
    FCheckHighlight: Boolean;
    FCheckSize: TSize;
    FDropped: Boolean;
    FHilightedIndex: Integer;
    FHiLiteLeft: Integer;
    FHiLiteRight: Integer;
    FNeedMeasure: Boolean;
    FRejectDropDown: Boolean;
    FRejectToggleOnSelect: Boolean;
    FRightToLeft: Boolean;
    FTextHeight: SmallInt;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure ClearItemStates;
    procedure CloseUp; override;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    procedure DropDown; override;
    procedure FontChanged(Sender: TObject); override;
    procedure InitializeWnd; override;
    procedure InitItemStates;
    procedure CheckItemStates;
    procedure QueueCheckItemStates;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetItemHeight(const AValue: Integer); override;
    procedure SetItems(const Value: TStrings); override;
    procedure Select; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(const AItem: string; AState: TCheckBoxState; AEnabled: Boolean = True); reintroduce;
    procedure AssignItems(AItems: TStrings);
    procedure Clear; override;
    procedure DeleteItem(AIndex: Integer);
    procedure CheckAll(AState: TCheckBoxState; AAllowGrayed: Boolean = True; AAllowDisabled: Boolean = True);
    procedure Toggle(AIndex: Integer);
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Count: Integer read GetCount;
    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[AIndex: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property Objects[AIndex: Integer]: TObject read GetObject write SetObject;
    property State[AIndex: Integer]: TCheckBoxState read GetState write SetState;
    property OnItemChange: TCheckItemChange read FOnItemChange write FOnItemChange;
  end;

  { TCheckComboBox }
  TCheckComboBox = class(TCustomCheckCombo)
  published
    property Align;
    property AllowGrayed;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoDropDown;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Constraints;
    property Count;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ItemWidth;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnItemChange;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  end;

procedure Register;

implementation

{$include comboex.inc}

procedure Register;
begin
  RegisterComponents('Misc', [TComboBoxEx, TCheckComboBox]);
end;

end.


