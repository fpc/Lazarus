{%MainUnit ../comctrls.pp}
{ $Id$

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
                          
{==============================================================================}
(*
 * TListItemSubItems is a helper class to notify only changed subitems to the IF
 *)
{==============================================================================}
type        
  TSubItemUpdate = (siuText, siuImage);
  
  { TListItemSubItems }

  TListItemSubItems = class(TStringList)
  private
    FChangeIndex: Integer;
    FOwner: TListItem;
    FUpdate: set of TSubItemUpdate;
    function GetImageIndex(const AIndex: Integer): Integer;
    procedure SetImageIndex(const AIndex, AValue: Integer);
  protected
    procedure Changed; override;
    function  GetObject(AIndex: Integer): TObject; override;
    procedure Put(AIndex: Integer; const S: string); override;
    procedure PutObject(AIndex: Integer; AObject: TObject); override;
  public
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    constructor Create(const AOwner: TListItem);
    destructor Destroy; override;
    procedure Delete(AIndex: Integer); override;
    procedure Insert(AIndex: Integer; const S: string); override;
    property ImageIndex[const AIndex: Integer]: Integer read GetImageIndex write SetImageIndex;
  end;  
  
  PListItemImageObject = ^ TListItemImageObject;
  TListItemImageObject = record
    FImage: Integer;
    FObject: TObject;
  end;
                          
{ TListItemSubItems }

function TListItemSubItems.Add(const S: string): Integer;
begin
  FChangeIndex := Count;
  FUpdate := [siuText];
  try
    Result := inherited Add(S);
  finally
    FUpdate := [];
    FChangeIndex := -1;
  end;
end;

procedure TListItemSubItems.Changed;
var
  LV: TCustomListView;
  n, Idx, Cnt, ColCnt: Integer;
  WSC: TWSCustomListViewClass;
begin
  if (FOwner <> nil)
  and FOwner.WSUpdateAllowed
  and ((FChangeIndex = -1) or (FUpdate <> []))
  then begin                  
    LV := FOwner.FOwner.FOwner;
    WSC := TWSCustomListViewClass(LV.WidgetSetClass);
    idx := FOwner.GetIndex;
    
    if FChangeIndex = -1 
    then begin
      // We don't know what changed, so update all
      ColCnt := LV.Columns.Count - 1; // skip main column
      if ColCnt > 0
      then begin
        Cnt := Count;
        if Cnt > ColCnt then Cnt := ColCnt;
                    
        // set text            
        for n := 0 to Cnt - 1 do                                            
          WSC.ItemSetText(LV, idx, FOwner, n + 1, Get(n));      
        // whipe others
        for n := Cnt to ColCnt - 1 do                                            
          WSC.ItemSetText(LV, idx, FOwner, n + 1, '');      
        // set image
        for n := 0 to Cnt - 1 do                                            
          WSC.ItemSetImage(LV, idx, FOwner, n + 1, GetImageIndex(n));      
        // whipe others
        for n := Cnt to ColCnt - 1 do                                            
          WSC.ItemSetImage(LV, idx, FOwner, n + 1, -1);      
      end;
    end
    else begin
      if siuText in FUpdate
      then begin
        if (FChangeIndex >= 0) and (FChangeIndex < Count) 
        then WSC.ItemSetText(LV, idx, FOwner, FChangeIndex + 1, Get(FChangeIndex))
        else WSC.ItemSetText(LV, idx, FOwner, FChangeIndex + 1, '');
      end;

      if siuImage in FUpdate
      then begin
        if (FChangeIndex >= 0) and (FChangeIndex < Count) 
        then WSC.ItemSetImage(LV, idx, FOwner, FChangeIndex + 1, GetImageIndex(FChangeIndex))
        else WSC.ItemSetImage(LV, idx, FOwner, FChangeIndex + 1, -1);
      end 
    end;
  end;
  inherited;
end;

procedure TListItemSubItems.Clear;
var 
  n: Integer;
begin
  for n := 0 to Count - 1 do
    Dispose(PListItemImageObject(pointer(inherited GetObject(n))));
  inherited;
end;

constructor TListItemSubItems.Create(const AOwner: TListItem);
begin
  inherited Create;
  FOwner := AOwner;
  FChangeIndex := -1;
  FUpdate := [];
end;

destructor TListItemSubItems.Destroy;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    Dispose(PListItemImageObject(pointer(inherited GetObject(n))));
  inherited Destroy;
end;

procedure TListItemSubItems.Delete(AIndex: Integer);
begin
  if AIndex = Count
  then FChangeIndex := AIndex
  else FChangeIndex := -1;
  FUpdate := [siuText, siuImage];
  try
    Dispose(PListItemImageObject(inherited GetObject(AIndex)));
    inherited;
  finally
    FUpdate := [];
    FChangeIndex := -1;
  end;
end;

function TListItemSubItems.GetImageIndex(const AIndex: Integer): Integer;
var 
  io: PListItemImageObject;
begin
  io := PListItemImageObject(inherited GetObject(AIndex));
  if io = nil
  then Result := -1
  else Result := io^.FImage;
end;

function TListItemSubItems.GetObject(AIndex: Integer): TObject; 
var 
  io: PListItemImageObject;
begin
  io := PListItemImageObject(inherited GetObject(AIndex));
  if io = nil
  then Result := nil
  else Result := io^.FObject;
end;

procedure TListItemSubItems.Insert(AIndex: Integer; const S: string);
begin
  if AIndex = Count
  then FChangeIndex := AIndex
  else FChangeIndex := -1;
  FUpdate := [siuText];
  try
    inherited;
  finally
    FUpdate := [];
    FChangeIndex := -1;
  end;
end;

procedure TListItemSubItems.Put(AIndex: Integer; const S: string);
begin
  FChangeIndex := AIndex;
  FUpdate := [siuText];
  try
    inherited;
  finally
    FUpdate := [];
    FChangeIndex := -1;
  end;
end;

procedure TListItemSubItems.PutObject(AIndex: Integer; AObject: TObject);
var 
  io: PListItemImageObject;
begin
  io := PListItemImageObject(inherited GetObject(AIndex));
  if (io = nil) and (AObject <> nil)
  then begin
    New(io);
    io^.FImage := -1;
  end;
  
  if io <> nil 
  then begin
    if (AObject = nil) and (io^.FImage = -1)
    then begin
      Dispose(io);
      io := nil;
    end
    else io^.FObject := AObject;
  end;

  FChangeIndex := AIndex;
  FUpdate := [];
  try
    inherited PutObject(AIndex, TObject(io));
  finally
    FChangeIndex := -1;
  end;
end;

procedure TListItemSubItems.SetImageIndex(const AIndex, AValue: Integer);
var 
  io: PListItemImageObject;
begin
  io := PListItemImageObject(inherited GetObject(AIndex));
  if (io = nil) and (AValue >= 0)
  then begin
    New(io);
    io^.FObject := nil;
  end;
  
  if io <> nil 
  then begin
    if (AValue < 0) and (io^.FObject = nil)
    then begin
      Dispose(io);
      io := nil;
    end
    else begin
      if AValue < 0
      then io^.FImage := -1
      else io^.FImage := AValue;
    end;
  end;

  FChangeIndex := AIndex;
  FUpdate := [siuImage];
  try
    inherited PutObject(AIndex, TObject(io));
  finally
    FUpdate := [];
    FChangeIndex := -1;
  end;
end;

{==============================================================================}
{==============================================================================}
                          
{------------------------------------------------------------------------------}
{   TListItem.Assign                                                           }
{------------------------------------------------------------------------------}
procedure TListItem.Assign(ASource: TPersistent);

begin

  if ASource is TListItem 
  then begin
    Caption := TListItem(ASource).Caption;

    Data := TListItem(ASource).Data;

    ImageIndex := TListItem(ASource).ImageIndex;

    SubItems := TListItem(ASource).SubItems;

  end

  else inherited Assign(ASource);

end;


{------------------------------------------------------------------------------}
{   TListItem Constructor                                                      }
{------------------------------------------------------------------------------}
constructor TListItem.Create(AOwner : TListItems);
begin
  inherited Create;
  FOwner := AOwner;
  FFlags := [];
  FStates := [];
  FImageIndex := -1;
  FSubItems := nil;
  
  // We can't create ourself in the interface since we don't know out index here
  // So interface creation is done in TListItems
end;

{------------------------------------------------------------------------------}
{   TListItem GetIndex                                                         }
{------------------------------------------------------------------------------}
function TListItem.GetIndex : Integer;
begin
  if Assigned(FOwner) 
  then Result := FOwner.IndexOf(self)
  else Result := -1;
end;

{------------------------------------------------------------------------------}
{   TListItem Delete                                                           }
{------------------------------------------------------------------------------}
procedure TListItem.Delete;
begin
  if not (lifDestroying in FFlags) then Free;
end;

{------------------------------------------------------------------------------}
{   TListItem Destructor                                                       }
{------------------------------------------------------------------------------}
destructor TListItem.Destroy;
begin
  Include(FFlags, lifDestroying);
  if FOwner <> nil
  then FOwner.ItemDestroying(Self);

  FreeAndNil(FSubItems);
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{   TListItem DisplayRect                                                      }
{------------------------------------------------------------------------------}
function TListItem.DisplayRect(Code: TDisplayCode): TRect;
var
  LV: TCustomListView;
begin
  LV := FOwner.FOwner;
  Result := TWSCustomListViewClass(LV.WidgetSetClass).ItemDisplayRect(
                                                         LV, GetIndex, 0, code);
end;

{------------------------------------------------------------------------------}
{   TListItem DisplayRectSubItem                                               }
{   The same as DisplayRect, except that it works for the sub items in the     }
{   tecord view                                                                }
{   If subItem = 0 then it will should return DisplayRect(code)                }
{------------------------------------------------------------------------------}
function TListItem.DisplayRectSubItem(subItem: integer; Code: TDisplayCode): TRect;
var
  LV: TCustomListView;
begin
  LV := FOwner.FOwner;
  Result := TWSCustomListViewClass(LV.WidgetSetClass).ItemDisplayRect(
                                                   LV, GetIndex, subItem, code);
end;

{------------------------------------------------------------------------------}
{   TListItem IntfUpdateText                                                   }
{------------------------------------------------------------------------------}
procedure TListItem.WSUpdateText;
var
  LV: TCustomListView;
  n, Idx, Cnt, ColCnt: Integer;
  WSC: TWSCustomListViewClass;
begin
  LV := FOwner.FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  idx := GetIndex;
  
  WSC.ItemSetText(LV, idx, Self, 0, FCaption);      
  
  ColCnt := LV.Columns.Count - 1; // skip main column
  if ColCnt > 0
  then begin
    if FSubItems = nil
    then Cnt := 0
    else Cnt := FSubItems.Count;
    if Cnt > ColCnt then Cnt := ColCnt;
                
    // set text            
    for n := 0 to Cnt - 1 do                                            
      WSC.ItemSetText(LV, idx, Self, n + 1, FSubItems[n]);      
    // whipe others
    for n := Cnt to ColCnt - 1 do                                            
      WSC.ItemSetText(LV, idx, Self, n + 1, '');      
  end;
end;

{------------------------------------------------------------------------------}
{   TListItem IntfUpdateImages                                                 }
{------------------------------------------------------------------------------}
procedure TListItem.WSUpdateImages;
var
  LV: TCustomListView;
  n, Idx, Cnt, ColCnt: Integer;
  WSC: TWSCustomListViewClass;
begin
  LV := FOwner.FOwner;
  WSC := TWSCustomListViewClass(LV.WidgetSetClass);
  idx := GetIndex;
  
  WSC.ItemSetImage(LV, idx, Self, 0, FImageIndex);      
  
  ColCnt := LV.Columns.Count - 1; // skip main column
  if ColCnt > 0
  then begin
    if FSubItems = nil
    then Cnt := 0
    else Cnt := FSubItems.Count;
    if Cnt > ColCnt then Cnt := ColCnt;
                
    // set image
    for n := 0 to Cnt - 1 do                                            
      WSC.ItemSetImage(LV, idx, Self, n + 1, TListItemSubItems(FSubItems).ImageIndex[n]);      
    // whipe others
    for n := Cnt to ColCnt - 1 do                                            
      WSC.ItemSetImage(LV, idx, Self, n + 1, -1);      
  end;
end;

procedure TListItem.WSUpdateChecked;
var
  LV: TCustomListView;
  Idx: Integer;
begin
  LV := FOwner.FOwner;
  idx := GetIndex;
  TWSCustomListViewClass(LV.WidgetSetClass).ItemSetChecked(LV, idx, Self, FChecked);
end;

procedure TListItem.WSUpdateState;
var
  AState: TListItemState;
  AIsSet: Boolean;
  LV: TCustomListView;
  idx: Integer;
begin
  LV := FOwner.FOwner;
  idx := GetIndex;
  FStates := [];
  for AState := Low(TListItemState) to High(TListItemState) do
  begin
    if TWSCustomListViewClass(LV.WidgetSetClass).ItemGetState(LV, idx, Self, AState, AIsSet) then
    begin
      if AIsSet then
        include(FStates, AState);
    end;
  end;
end;

procedure TListItem.WSSetState;
var
  AState: TListItemState;
  LV: TCustomListView;
  idx: Integer;
begin
  LV := FOwner.FOwner;
  idx := GetIndex;
  for AState := Low(TListItemState) to High(TListItemState) do
    TWSCustomListViewClass(LV.WidgetSetClass).ItemSetState(LV, idx, Self, AState, AState in FStates);
end;

{------------------------------------------------------------------------------}
{   TListItem IntfUpdateAllowed                                                }
{------------------------------------------------------------------------------}
function TListItem.WSUpdateAllowed: Boolean;
begin
  Result := (FFlags * [lifDestroying, lifCreated] = [lifCreated])
    and (FOwner <> nil)
    and FOwner.WSUpdateAllowed;
end;

{------------------------------------------------------------------------------}
{   TListItem IsEqual                                                          }
{------------------------------------------------------------------------------}
function TListItem.IsEqual(const AItem: TListItem): Boolean;
begin
  Result := (Caption = AItem.Caption) 
        and (Data = AItem.Data) 
        and (FStates = AItem.FStates);
end;

{------------------------------------------------------------------------------}
{   TListItem GetCheckedInternal                                               }
{------------------------------------------------------------------------------}
function TListItem.GetCheckedInternal: Boolean;
begin
  Result := FChecked;
end;

function TListItem.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{------------------------------------------------------------------------------}
{   TListItem GetChecked                                                       }
{------------------------------------------------------------------------------}
function TListItem.GetChecked: Boolean;
var
  LV: TCustomListView;
begin
  LV := FOwner.FOwner;
  Result := LV.Checkboxes and WSUpdateAllowed;
  if Result then
    Result := TWSCustomListViewClass(LV.WidgetSetClass).ItemGetChecked(LV, GetIndex, Self)
  else
    Result := FChecked;
end;

{------------------------------------------------------------------------------}
{   TListItem GetState                                                         }
{------------------------------------------------------------------------------}
function TListItem.GetState(const ALisOrd: Integer): Boolean;
var
  AState: TListItemState;
  LV: TCustomListView;
begin
  AState := TListItemState(ALisOrd);
  LV := FOwner.FOwner;

  if WSUpdateAllowed
  and TWSCustomListViewClass(LV.WidgetSetClass).ItemGetState(LV, GetIndex, Self, AState, Result)
  then begin
    // update FStates
    if Result
    then Include(FStates, AState)
    else Exclude(FStates, AState);
  end
  else Result := AState in FStates;
end;

function TListItem.GetLeft: Integer;
begin
  Result := Position.X;
end;

{------------------------------------------------------------------------------}
{   The ListView this ListItem belongs to                                      }
{------------------------------------------------------------------------------}
function TListItem.GetListView: TCustomListView;
begin
  Result := Owner.Owner;
end;

function TListItem.GetPosition: TPoint;
var
  LV: TCustomListView;
begin
  LV := FOwner.FOwner;
  Result := TWSCustomListViewClass(LV.WidgetSetClass).ItemGetPosition(LV, GetIndex);
end;

{------------------------------------------------------------------------------}
{   TListItem GetSubItemImages                                                 }
{------------------------------------------------------------------------------}
function TListItem.GetSubItemImages(const AIndex: Integer): Integer;
begin
  Result := TListItemSubItems(SubItems).ImageIndex[AIndex];
end;

{------------------------------------------------------------------------------}
{   TListItem GetSubItems                                                      }
{------------------------------------------------------------------------------}
function TListItem.GetSubItems: TStrings;
begin
  if FSubItems = nil
  then FSubItems := TListItemSubItems.Create(Self);
  Result := FSubItems;
end;

function TListItem.GetTop: Integer;
begin
  Result := Position.Y;
end;

procedure TListItem.SetPosition(const AValue: TPoint);
var
  LV: TCustomListView;
  CurPos: TPoint;
begin
  LV := FOwner.FOwner;
  CurPos := Position;
  if (CurPos.X <> AValue.X) or (CurPos.Y <> AValue.Y) then
    TWSCustomListViewClass(LV.WidgetSetClass).ItemSetPosition(LV, GetIndex, AValue);
end;

{------------------------------------------------------------------------------}
{   TListItem MakeVisible                                                      }
{------------------------------------------------------------------------------}
procedure TListItem.MakeVisible(PartialOK: Boolean);
begin
  if (FOwner <> nil)
  and (FOwner.Fowner <> nil) 
  then FOwner.FOwner.SetItemVisible(Self, PartialOK);
end;

{------------------------------------------------------------------------------}
{   TListItem SetCaption                                                       }
{------------------------------------------------------------------------------}
procedure TListItem.SetCaption(const AValue : String);
var
  LV: TCustomListView;
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  LV := FOwner.FOwner;
  if WSUpdateAllowed
  then TWSCustomListViewClass(LV.WidgetSetClass).ItemSetText(LV, GetIndex, Self, 0, FCaption);
end;

{------------------------------------------------------------------------------}
{   TListItem GetCaption                                                       }
{------------------------------------------------------------------------------}
function TListItem.GetCaption: String; 
begin
  Result := FCaption;
end;

{------------------------------------------------------------------------------}
{   TListItem SetChecked                                                       }
{------------------------------------------------------------------------------}
procedure TListItem.SetChecked(AValue: Boolean);
var
  LV: TCustomListView;
begin
  LV := FOwner.FOwner;
  FChecked := AValue;
  if LV.Checkboxes and WSUpdateAllowed then
    TWSCustomListViewClass(LV.WidgetSetClass).ItemSetChecked(LV, GetIndex,
      Self, AValue);
end;

{------------------------------------------------------------------------------}
{   TListItem SetData                                                          }
{------------------------------------------------------------------------------}
procedure TListItem.SetData(const AValue: Pointer);
begin
  FData := AValue;
  // TODO: Sort
end;

{------------------------------------------------------------------------------}
{   TListItem SetImageIndex                                                    }
{------------------------------------------------------------------------------}
procedure TListItem.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue = FImageIndex then Exit;

  FImageIndex := AValue;

  if WSUpdateAllowed
  then TWSCustomListViewClass(FOwner.FOwner.WidgetSetClass).ItemSetImage(FOwner.FOwner, GetIndex, Self, 0, FImageIndex);
end;

{------------------------------------------------------------------------------}
{   TListItem GetImageIndex                                                    }
{------------------------------------------------------------------------------}
function TListItem.GetImageIndex: TImageIndex;
begin
  Result := FImageIndex;
end;

procedure TListItem.SetLeft(Value: Integer);
begin
  Position := Point(Value, Top);
end;

{------------------------------------------------------------------------------}
{   TListItem SetStateIndex                                                    }
{------------------------------------------------------------------------------}
procedure TListItem.SetStateIndex(const AValue: TImageIndex);
begin
  if AValue = FStateIndex then Exit;

  FStateIndex := AValue;

  if WSUpdateAllowed then
    TWSCustomListViewClass(FOwner.FOwner.WidgetSetClass).ItemSetStateImage(FOwner.FOwner, GetIndex, Self, 0, FStateIndex);

end;

{------------------------------------------------------------------------------}
{   TListItem GetStateIndex                                                    }
{------------------------------------------------------------------------------}
function TListItem.GetStateIndex: TImageIndex;
begin
  Result := FStateIndex;
end;

{------------------------------------------------------------------------------}
{   TListItem SetState                                                         }
{------------------------------------------------------------------------------}
procedure TListItem.SetState(const ALisOrd: Integer; const AIsSet: Boolean);
var
  AState: TListItemState;
  LV: TCustomListView;
begin
  AState := TListItemState(ALisOrd);

  if (AState in FStates) = AIsSet then Exit;

  if AIsSet
  then Include(FStates, AState)
  else Exclude(FStates, AState);

  if not WSUpdateAllowed then Exit;
  LV := FOwner.FOwner;
  TWSCustomListViewClass(LV.WidgetSetClass).ItemSetState(LV, GetIndex, Self, AState, AIsSet);
end;

{------------------------------------------------------------------------------}
{   TListItem SetSubItemImages                                                 }
{------------------------------------------------------------------------------}
procedure TListItem.SetSubItemImages(const AIndex, AValue: Integer);
begin
  TListItemSubItems(SubItems).ImageIndex[AIndex] := AValue;
end;

{------------------------------------------------------------------------------}
{   TListItem SetSubItems                                                      }
{------------------------------------------------------------------------------}
procedure TListItem.SetSubItems(const AValue: TStrings);
begin
  if (AValue = nil) and (FSubItems = nil) then Exit;
  SubItems.Assign(AValue);
end;

procedure TListItem.SetTop(Value: Integer);
begin
  Position := Point(Left, Value);
end;

{ TOwnerDataListItem }

{------------------------------------------------------------------------------}
{   TOwnerDataListItem GetIndex                                                }
{------------------------------------------------------------------------------}

function TOwnerDataListItem.GetIndex: Integer;  
begin
  Result:=FDataIndex;
end;

{------------------------------------------------------------------------------}
{   TOwnerDataListItem GetIndex                                                }
{------------------------------------------------------------------------------}

procedure TOwnerDataListItem.SetCaption(const AValue: String);  
begin
  FCaption:=AValue;       
end;

function TOwnerDataListItem.GetCaption: String; 
begin
  if not FCached then DoCacheItem;
  Result := inherited GetCaption;
end;

function TOwnerDataListItem.GetImageIndex: TImageIndex;
begin
  if not FCached then DoCacheItem;
  Result := inherited GetImageIndex;
end;

procedure TOwnerDataListItem.SetImageIndex(const AValue: TImageIndex);
begin
  FImageIndex := AValue;
end;


{------------------------------------------------------------------------------}
{   TOwnerDataListItem SetDataIndex                                            }
{------------------------------------------------------------------------------}

procedure TOwnerDataListItem.SetDataIndex(ADataIndex: Integer);
begin
  if FDataIndex <> ADataIndex then 
  begin
    FDataIndex := ADataIndex;
    FCached := False;
    if Assigned(FSubItems) then FSubItems.Clear;   
    if FDataIndex>=0 then begin 
      WSUpdateState;
      DoCacheItem;
    end;
  end;
end;

function TOwnerDataListItem.GetSubItems: TStrings; 
begin
  if not FCached then DoCacheItem;
  Result := inherited GetSubItems;
end;

procedure TOwnerDataListItem.DoCacheItem;
begin
  FCached := True;
  FOwner.FOwner.DoGetOwnerData(Self);
end;

