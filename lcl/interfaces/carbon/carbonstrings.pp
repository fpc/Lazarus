{ $Id$
                  -----------------------------------
                  carbonstrings.pp  -  Carbon strings
                  -----------------------------------

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
unit CarbonStrings;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
 // carbon bindings
  FPCMacOSAll,
 // rtl+ftl
  Types, Classes, SysUtils, Math,
 // LCL
  LCLProc, LCLType, Graphics, Controls, StdCtrls,
 // LCL Carbon
  CarbonDef, CarbonEdits, CarbonLists;

type
  { TCarbonComboBoxStrings }

  TCarbonComboBoxStrings = class(TStringList)
  private
    FOwner: TCarbonComboBox;  // Carbon combo box control owning strings
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AOwner: TCarbonComboBox);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Sort; override;
  public
    property Owner: TCarbonComboBox read FOwner;
  end;

  TCarbonListBoxItem = record
    str : CFStringRef;
    obj : TObject;
    checked : boolean; //for checklistbox
  end;
  PCarbonListBoxItem = ^TCarbonListBoxItem;

  { TCarbonListBoxStrings }

  TCarbonListBoxStrings = class(TStrings)
  private
    FOwner: TCarbonListBox;   // Carbon list box control owning strings
    FList : TList;
    FSorted : boolean;
    function FindSortedPosition(aStr : CFStringRef) : integer;
    function InternalInsert(Index : integer; const S : string; o : TObject; c : boolean) : integer;
    procedure SetSorted(AValue : boolean);
    procedure QuickSort(L, R: Integer);
  protected
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AOwner: TCarbonListBox);
    property Owner: TCarbonListBox read FOwner;
    property Sorted : boolean read FSorted write SetSorted;
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure AddStrings(TheStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function GetCFString(Index : integer) : CFStringRef;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Sort;
  end;

  TCarbonCheckListBoxStrings = class(TCarbonListBoxStrings)
  private
    function GetChecked(Index : integer) : boolean;
    procedure SetChecked(Index : integer; aValue : boolean);
  protected
  public
    property Checked[Index : integer] : boolean read GetChecked write SetChecked;
  end;


  { TCarbonMemoStrings }

  TCarbonMemoStrings = class(TStrings)
  private
    FStringList: TStringList; // internal string list
    FOwner: TCarbonMemo;      // Carbon memo control owning strings
    FExternalChanged: Boolean;// Carbon strings object has changed
    procedure InternalUpdate;
    procedure ExternalUpdate;
  protected
    function GetTextStr: string; override;
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
  public
    constructor Create(AOwner: TCarbonMemo);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure SetText(TheText: PChar); override;

    procedure ExternalChanged; dynamic;
  public
    property Owner: TCarbonMemo read FOwner;
  end;


implementation

uses
  CarbonProc;

{ TCarbonComboBoxStrings }

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.Put
  Params:  Index - Index of string to change
           S     - New text

  Changes the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);

  FOwner.Remove(Index);
  FOwner.Insert(Index, S);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.InsertItem
  Params:  Index - Line index
           S     - Text to insert

  Inserts the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.InsertItem(Index: Integer; const S: string);
begin
  inherited InsertItem(Index, S);

  FOwner.Insert(Index, S);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.InsertItem
  Params:  Index - Line index
           S     - Text to insert
           O     - Object to insert

  Inserts the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.InsertItem(Index: Integer; const S: string;
  O: TObject);
begin
  inherited InsertItem(Index, S, O);

  FOwner.Insert(Index, S);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.Create
  Params:  AOwner - Carbon combo box owner of strings

  Creates new strings for Carbon combo box items
 ------------------------------------------------------------------------------}
constructor TCarbonComboBoxStrings.Create(AOwner: TCarbonComboBox);
begin
  inherited Create;
  FOwner := AOwner;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.Clear

  Clears strings
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.Clear;
var
  I: Integer;
  C: Integer;
begin
  C := Count;
  
  inherited Clear;
  
  for I := C - 1 downto 0 do FOwner.Remove(I);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.Delete
  Params:  Index - Line index

  Deletes line with the specified index from strings
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);
  
  FOwner.Remove(Index);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.Sort

  Sorts the strings
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.Sort;
var
  I: Integer;
begin
  inherited Sort;
  
  for I := 0 to Count - 1 do
  begin
    FOwner.Remove(I);
    FOwner.Insert(I, Strings[I]);
  end;
end;

{ TCarbonListBoxStrings }

function TCarbonListBoxStrings.FindSortedPosition(aStr : CFStringRef) : integer;
var i,l,r : Longint;
    res : CFComparisonResult;
    data : PCarbonListBoxItem;
begin
  l:=0;
  r:=FList.Count-1;
  while l<=r do
  begin
    i:=(l+r) div 2;
    data:=fList.Items[i];
    res:=CFStringCompare(data^.str,aStr,kCFCompareCaseInsensitive);
    if res<0 then l:=i+1
    else if res>0 then r:=i-1
    else
    begin
      Result:=i;
      exit;
    end;
  end;
  Result:=l;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.InternalInsert
  Params:  Index - Line index
           S     - string
           o     - object
           c     - checked value (for checklistbox)

  Inserts a new line with specified index.
  Raises an exception if Index is out of bounds.
 ------------------------------------------------------------------------------}
function TCarbonListBoxStrings.InternalInsert(Index : integer; const S : string;
         o : TObject; c : boolean) : integer;
var data : PCarbonListBoxItem;
begin
  GetMem(data,sizeof(TCarbonListBoxItem));
  CreateCFString(s,data^.str);
  data^.obj:=o;
  data^.checked:=c;
  try
    if FSorted then Index:=FindSortedPosition(data^.str);
    FList.Insert(Index,data);
    Result:=Index;
  except
    System.FreeMem(data);
    raise;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Get
  Params:  Index - Index of string to read
  
  Returns: The line at Index position
 ------------------------------------------------------------------------------}
function TCarbonListBoxStrings.Get(Index: Integer): string;
var data : PCarbonListBoxItem;
begin
  data:=PCarbonListBoxItem(fList[Index]);
  Result:=CFStringToStr(data^.str);
end;

function TCarbonListBoxStrings.GetCapacity: Integer;
begin
  Result:=FList.Capacity;
end;

function TCarbonListBoxStrings.GetCount: Integer;
begin
  Result:=fList.Count;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.GetObject
  Params:  Index - Index of the object to read

  Returns: The object at Index position
 ------------------------------------------------------------------------------}
function TCarbonListBoxStrings.GetObject(Index: Integer): TObject;
var data : PCarbonListBoxItem;
begin
  data:=PCarbonListBoxItem(fList[Index]);
  Result:=data^.obj;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Put
  Params:  Index - Index of string to change
           S     - New text

  Changes the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Put(Index: Integer; const S: string);
var data : PCarbonListBoxItem;
    id : DataBrowserItemID;
begin
  data:=PCarbonListBoxItem(fList[Index]);
  FreeCFString(data^.str);
  CreateCFString(s,data^.str);
  if FSorted then
  begin
    BeginUpdate;
    try
      FList.Delete(Index);
      Index:=FindSortedPosition(data^.str);
      FList.Insert(Index,data);
    finally
      EndUpdate;
    end;
  end
  //no need to do a full update, just update this item.
  else
  if UpdateCount=0 then
  begin
    id:=Index+1;
    OSError(
      UpdateDataBrowserItems(FOwner.widget,kDataBrowserNoItem,1,@id,
      kDataBrowserItemNoProperty,ListBoxColumnTextPropID),
      Self,'Put','UpdateDataBrowserItems');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.PutObject
  Params:  Index - Index of string to change
           o     - New Object

  Changes the object on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.PutObject(Index: Integer; AObject: TObject);
var data : PCarbonListBoxItem;
begin
  data:=PCarbonListBoxItem(fList[Index]);
  data^.obj:=AObject;
end;

procedure TCarbonListBoxStrings.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity>fList.Capacity then
    fList.Capacity:=NewCapacity;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.SetUpdateState
  Params:  Updating - tells if we are updating or not

  If we finished updating, updates the control
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  //if we finished updating, update the control
  //note: items=nil empties the DataBrowser and fills it again,
  //assigning all item ids again starting from 1
  if Updating=false then
    if fOwner.Widget<>nil then
    begin
      FOwner.CheckNeedsScrollbar;
      //better forcing to empty every time, sometimes db doesn't update as it should
      OSError(
        RemoveDataBrowserItems(FOwner.Widget,kDataBrowserNoItem,0,
        nil,kDataBrowserItemNoProperty),
        Self,'SetUpdateState','RemoveDataBrowserItems');
      if Count<>0 then
      OSError(
        AddDataBrowserItems(FOwner.Widget,kDataBrowserNoItem,Count,
        nil,kDataBrowserItemNoProperty),
        Self,'SetUpdateState','AddDataBrowserItems');
    end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Create
  Params:  AOwner - Carbon list box owner of strings

  Creates new strings for Carbon list box items
 ------------------------------------------------------------------------------}
constructor TCarbonListBoxStrings.Create(AOwner: TCarbonListBox);
begin
  inherited Create;
  
  FOwner := AOwner;
  FList:=TList.Create;
  FSorted:=false;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Destroy
  Params:

  Destroys strings for Carbon list box items
 ------------------------------------------------------------------------------}
destructor TCarbonListBoxStrings.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Add
  Params:  S     - Line to add

  Adds a line at the end of the list
 ------------------------------------------------------------------------------}
function TCarbonListBoxStrings.Add(const S: string): Integer;
begin
  Result:=AddObject(s,nil);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.AddObject
  Params:  S       - Line to add
           AObject - Associated object

  Adds a line and the associated object at the end of the list,
 ------------------------------------------------------------------------------}
function TCarbonListBoxStrings.AddObject(const S: string; AObject: TObject): Integer;
var id : DataBrowserItemID;
begin
  //We don't call BeginUpdate/EndUpdate intentionally if it's an unsorted list
  if fSorted then BeginUpdate;
  try
  Result:=InternalInsert(Count,s,AObject,false);
  finally
    if fSorted then EndUpdate;
  end;
  if (UpdateCount=0) and (not FSorted) then
  begin
    FOwner.CheckNeedsScrollbar;
    id:=Count;
    OSError(
      AddDataBrowserItems(FOwner.Widget,kDataBrowserNoItem,1,
      @id,kDataBrowserItemNoProperty),
      Self,'AddObject','AddDataBrowserItems');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.AddStrings
  Params:  TheStrings - Strings to add

  Adds lines and associated objects to the list
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.AddStrings(TheStrings: TStrings);
var i : integer;
    ids : DataBrowserItemIDPtr;
    oldcount : integer;
begin
  //We don't call BeginUpdate/EndUpdate intentionally if it's an unsorted list
  oldcount:=Count;
  if fSorted then BeginUpdate;
  try
    for i:=0 to TheStrings.Count-1 do
      InternalInsert(oldcount+i,TheStrings[i],TheStrings.Objects[i],false);
  finally
    if fSorted then EndUpdate;
  end;
  if (UpdateCount=0) and (TheStrings.Count<>0) and (not Sorted) then
  begin
    FOwner.CheckNeedsScrollbar;
    GetMem(ids,sizeof(DataBrowserItemID)*TheStrings.Count);
    try
      for i:=1 to TheStrings.Count do
        ids[i]:=oldcount+i;
      OSError(
        AddDataBrowserItems(FOwner.Widget,kDataBrowserNoItem,TheStrings.Count,
        ids,kDataBrowserItemNoProperty),
        Self,'AddStrings','AddDataBrowserItems');
    finally
      System.FreeMem(ids);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Clear

  Clears strings
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Clear;
var i : integer;
begin
  BeginUpdate;
  try
    for i:=fList.Count-1 downto 0 do
      Delete(i);
  finally
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Delete
  Params:  Index - Line index

  Deletes line with the specified index from list
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Delete(Index: Integer);
var data : PCarbonListBoxItem;
begin
  BeginUpdate;
  try
    data:=PCarbonListBoxItem(fList[Index]);
    fList.Delete(Index);
    FreeCFString(data^.str);
    System.FreeMem(data);
  finally
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Exchange
  Params:  Index1 - First line index
           Index2 - Second line index
           
  Exchanges positions of two lines
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Exchange(Index1, Index2: Integer);
var data1,data2 : PCarbonListBoxItem;
begin
  if FSorted then exit;
  BeginUpdate;
  try
    data1:=PCarbonListBoxItem(fList[Index1]);
    data2:=PCarbonListBoxItem(fList[Index2]);
    fList[Index2]:=data1;
    fList[Index1]:=data2;
  finally
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Insert
  Params:  Index - Line index
           S     - Text to insert

  Inserts the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Insert(Index: Integer; const S: string);
begin
  if Index=Count then Add(s)
  else
  begin
    BeginUpdate;
    try
      InternalInsert(Index,S,nil,false);
    finally
      EndUpdate;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Move
  Params:  CurIndex - Index of string to move
           NewIndex - Index to move the string to

  Removes string from position CurIndex and inserts it in position NewIndex
  Note: NewIndex is processed AFTER the removal of the string at CurIndex
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Move(CurIndex, NewIndex: Integer);
var data : PCarbonListBoxItem;
begin
  if FSorted then exit;
  BeginUpdate;
  try
    data:=PCarbonListBoxItem(fList[CurIndex]);
    fList.Delete(CurIndex);
    try
      fList.Insert(NewIndex,data);
    except
    //if there was an error, undo the changes
      fList.Insert(CurIndex,data);
      raise;
    end;
  finally
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.GetCFString
  Params:  Index - Index of CFStringRef to get

  Returns: The CFStringRef at Index position, or nil if not found
 ------------------------------------------------------------------------------}
function TCarbonListBoxStrings.GetCFString(Index : integer) : CFStringRef;
var data : PCarbonListBoxItem;
begin
  if (Index<1) or (Index>Count) then Result:=nil
  else
  begin
    data:=PCarbonListBoxItem(fList[Index-1]);
    Result:=data^.str;
  end;
end;

procedure TCarbonListBoxStrings.SetSorted(AValue : boolean);
begin
  if AValue=FSorted then exit;
  fSorted:=Avalue;
  if fSorted then Sort;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Sort
  Params:  -

  Sorts the list using quicksort
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Sort;
begin
  if FList.Count>1 then
  begin
    BeginUpdate;
    QuickSort(0,FList.Count-1);
    fSorted:=true;
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.QuickSort
  Params:  l - left index
           r - right index

  Sorts the list. This method is copied from fpc's TStringList.QuickSort,
  with small modifications.
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.QuickSort(l, r: Integer);
var i,j, pivot : Longint;
    data1,data2 : PCarbonListBoxItem;
begin
  repeat
    i:=l;
    j:=r;
    pivot:=(l+r) div 2;
    repeat
      data1:=PCarbonListBoxItem(fList[i]);
      data2:=PCarbonListBoxItem(fList[pivot]);
      while CFStringCompare(data1^.str,data2^.str,kCFCompareCaseInsensitive) <0 do
      begin
        inc(i);
        data1:=PCarbonListBoxItem(fList[i]);
      end;
      data1:=PCarbonListBoxItem(fList[j]);
      while CFStringCompare(data1^.str,data2^.str,kCFCompareCaseInsensitive) >0 do
      begin
        dec(j);
        data1:=PCarbonListBoxItem(fList[j]);
      end;
      if i<=j then
      begin
        fList[j]:=fList[i];
        fList[i]:=data1;
        if pivot=i then
          pivot:=j
        else if pivot=j then
          pivot := i;
        inc(i);
        dec(j);
      end;
    until i>j;
    if l<j then QuickSort(l,j);
    l:=i;
  until i>=r;
end;


{ TCarbonCheckListBoxStrings }

{------------------------------------------------------------------------------
  Method:  TCarbonCheckListBoxStrings.GetChecked
  Params:  Index - Index of the item

  Returns: the checked state of the item
 ------------------------------------------------------------------------------}
function TCarbonCheckListBoxStrings.GetChecked(Index : integer) : boolean;
var data : PCarbonListBoxItem;
begin
  data:=PCarbonListBoxItem(fList[Index]);
  Result:=data^.checked;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCheckListBoxStrings.SetChecked
  Params:  Index  - Index of the item
           aValue - New checked value

  Change the checked state of the item
 ------------------------------------------------------------------------------}
procedure TCarbonCheckListBoxStrings.SetChecked(Index : integer; aValue : boolean);
var data : PCarbonListBoxItem;
    id : DataBrowserItemID;
begin
  data:=PCarbonListBoxItem(fList[Index]);
  if data^.checked=aValue then exit;
  data^.checked:=aValue;
  //no need to do a full update, just update this item.
  if UpdateCount=0 then
  begin
    id:=Index+1;
    OSError(
      UpdateDataBrowserItems(FOwner.widget,kDataBrowserNoItem,1,@id,
      kDataBrowserItemNoProperty,ListBoxColumnCheckPropID),
      Self,'SetChecked','UpdateDataBrowserItems');
  end;
end;

{ TCarbonMemoStrings }

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.InternalUpdate

  Updates the internal strings from Carbon interface
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.InternalUpdate;
var
  S: String;
begin
  S := '';
  //DebugLn('TCarbonMemoStrings.InternalUpdate');
  if FOwner.GetText(S) then
    FStringList.Text := S;

  FExternalChanged := False;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.ExternalUpdate

  Updates the strings in Carbon interface from internal
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.ExternalUpdate;
begin
  //DebugLn('TCarbonMemoStrings.ExternalUpdate Text: ' + FStringList.Text);
  FOwner.SetText(FStringList.Text);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.GetTextStr
  Returns: Text of Carbon strings
 ------------------------------------------------------------------------------}
function TCarbonMemoStrings.GetTextStr: string;
begin
  if FExternalChanged then InternalUpdate;
  Result := FStringList.Text;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.GetCount
  Returns: Number of lines
 ------------------------------------------------------------------------------}
function TCarbonMemoStrings.GetCount: Integer;
begin
  if FExternalChanged then InternalUpdate;
  Result := FStringList.Count;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Get
  Params:  Index - Line index
  Returns: Text on line with the specified index
 ------------------------------------------------------------------------------}
function TCarbonMemoStrings.Get(Index: Integer): string;
begin
  if FExternalChanged then InternalUpdate;
  Result := FStringList[Index];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Create
  Params:  AOwner - Carbon memo owner of strings

  Creates new strings for Carbon memo strings
 ------------------------------------------------------------------------------}
constructor TCarbonMemoStrings.Create(AOwner: TCarbonMemo);
begin
  FOwner := AOwner;
  FStringList := TStringList.Create;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Destroy

  Releases strings from Carbon memo strings
 ------------------------------------------------------------------------------}
destructor TCarbonMemoStrings.Destroy;
begin
  FStringList.Free;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Assign
  Params:  Source - Object to assing

  Assings strings object
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Assign(Source: TPersistent);
begin
  if (Source = Self) or (Source = nil) then Exit;
  if Source is TStrings then
  begin
    FStringList.Clear;
    FStringList.Text := TStrings(Source).Text;
    ExternalUpdate;
  end
  else
    inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Clear

  Clears strings
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Clear;
begin
  FStringList.Clear;
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Delete
  Params:  Index - Line index

  Deletes line with the specified index from strings
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Delete(Index: Integer);
begin
  FStringList.Delete(Index);
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Insert
  Params:  Index - Line index
           S     - Text to insert

  Inserts the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Insert(Index: Integer; const S: string);
begin
  FStringList.Insert(Index, S);
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.SetText
  Params:  TheText - Text to set

  Sets the text of strings
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.SetText(TheText: PChar);
begin
  FStringList.Text := TheText;
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.ExternalChanged

  Notifies that strings object in Carbon interface has changed
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.ExternalChanged;
begin
  FExternalChanged := True;
end;


end.
