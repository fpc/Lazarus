{%MainUnit ../comctrls.pp}
{ $Id$

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  and FOwner.IntfUpdateAllowed
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
{$ifdef ver1_0}
// fpc 1.0.x needs a var parameter for dispose.
  io: PListItemImageObject;
{$endif}
begin
  for n := 0 to Count - 1 do
  {$ifdef ver1_0}
  begin
    io := PListItemImageObject(inherited GetObject(n));
    Dispose(io);
  end;
  {$else}
    Dispose(PListItemImageObject(pointer(inherited GetObject(n))));
  {$endif}
  inherited;
end;

constructor TListItemSubItems.Create(const AOwner: TListItem);
begin
  inherited Create;
  FOwner := AOwner;
  FChangeIndex := -1;
  FUpdate := [];
end;

procedure TListItemSubItems.Delete(AIndex: Integer);
{$ifdef ver1_0}
// fpc 1.0.x needs a var parameter for dispose.
var
  io: PListItemImageObject;
{$endif}
begin
  if AIndex = Count
  then FChangeIndex := AIndex
  else FChangeIndex := -1;
  FUpdate := [siuText, siuImage];
  try
    {$ifdef ver1_0}
    io := PListItemImageObject(inherited GetObject(AIndex));
    Dispose(io);
    {$else}
    Dispose(PListItemImageObject(inherited GetObject(AIndex)));
    {$endif}
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
{   TListItem IntfUpdateText                                                   }
{------------------------------------------------------------------------------}
procedure TListItem.IntfUpdateText;
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
procedure TListItem.IntfUpdateImages;
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

{------------------------------------------------------------------------------}
{   TListItem IntfUpdateAllowed                                                }
{------------------------------------------------------------------------------}
function TListItem.IntfUpdateAllowed: Boolean;
begin
  Result := (FFlags * [lifDestroying, lifCreated] = [lifCreated])
    and (FOwner <> nil)
    and FOwner.IntfUpdateAllowed;
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
{   TListItem GetState                                                         }
{------------------------------------------------------------------------------}
function TListItem.GetState(const ALisOrd: Integer): Boolean;
var
  AState: TListItemState;
begin
  AState := TListItemState(ALisOrd);

  if IntfUpdateAllowed 
  and TWSCustomListViewClass(FOwner.FOwner.WidgetSetClass)
    .ItemGetState(FOwner.FOwner, GetIndex, Self, AState, Result)
  then begin
    // update FStates
    if Result 
    then Include(FStates, AState)
    else Exclude(FStates, AState); 
  end
  else Result := AState in FStates;
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
begin
  if FCaption = AValue then Exit;
  FCaption := AValue;
  if IntfUpdateAllowed
  then TWSCustomListViewClass(FOwner.FOwner.WidgetSetClass).ItemSetText(FOwner.FOwner, GetIndex, Self, 0, FCaption);
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
procedure TListItem.SetImageIndex(const AValue: Integer);
begin
  if AValue = FImageIndex then Exit;

  FImageIndex := AValue;

  if IntfUpdateAllowed
  then TWSCustomListViewClass(FOwner.FOwner.WidgetSetClass).ItemSetImage(FOwner.FOwner, GetIndex, Self, 0, FImageIndex);
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

  if not IntfUpdateAllowed then Exit;
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

{ =============================================================================

  $Log$
  Revision 1.22  2005/07/01 18:20:41  mattias
  added ldocktree.pas

  Revision 1.21  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.20  2004/09/08 23:05:35  mattias
  improved TListView.SetItemVisible  from Andrew Haines

  Revision 1.19  2004/07/14 15:57:53  mattias
  fixed 1.0.10 compilation  from Vincent

  Revision 1.18  2004/07/11 17:20:47  marc
  * Implemented most of TListColoum/Item in the Ws for gtk and win32

  Revision 1.17  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.16  2004/04/04 17:10:05  marc
  Patch from Andrew Haines

  Revision 1.15  2003/03/15 13:26:07  mattias
  fixes for fpc 1.1

  Revision 1.14  2003/02/24 19:00:42  mattias
  added TlistView.SubItem improvements from Olivier Guilbaud

  Revision 1.13  2003/02/18 23:22:56  mattias
  added listview items property editor

  Revision 1.12  2002/11/25 11:37:18  mattias
  applied patch from Vasily

  Revision 1.11  2002/11/18 13:38:44  mattias
  fixed buffer overrun and added several checks

  Revision 1.10  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.9  2002/03/27 00:33:54  lazarus
  MWE:
    * Cleanup in lmessages
    * Added Listview selection and notification events
    + introduced commctrl

  Revision 1.8  2002/03/24 16:38:01  lazarus
  MWE:
    * Fixed bug on ListItems.Delete

  Revision 1.7  2002/03/23 15:49:22  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

}
