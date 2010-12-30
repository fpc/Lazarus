{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TCustomHeaderControl
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
{ TCustomHeaderControl }

const HeaderBorderSize = 2;
      DragStartDistance = 5;

procedure TCustomHeaderControl.SetImages(const AValue: TCustomImageList);
begin
  if FImages = AValue then
    Exit;
  if FImages <> nil then
    FImages.RemoveFreeNotification(Self);
  FImages := AValue;
  if FImages <> nil then
    FImages.FreeNotification(Self);
end;

function TCustomHeaderControl.GetSectionFromOriginalIndex(OriginalIndex: Integer): THeaderSection;
var
  i: Longint;
begin
  Result := nil;
  for i := 0 to FSections.Count - 1 do
    if FSections[i].OriginalIndex = OriginalIndex then
      Exit(FSections[i]);
end;

procedure TCustomHeaderControl.SetSections(const AValue: THeaderSections);
begin
  FSections := AValue;
end;

procedure TCustomHeaderControl.UpdateSection(Index: Integer);
begin
  // repaint item
  Repaint;
end;

procedure TCustomHeaderControl.UpdateSections;
{var
  i: integer;}
begin
{  for i := 0 to Sections.Count - 1 do
    UpdateSection(i);
    }
  Repaint;
end;

function TCustomHeaderControl.CreateSection: THeaderSection;
var
  HeaderSectionClass: THeaderSectionClass;
begin
  HeaderSectionClass := THeaderSection;
  if Assigned(FOnCreateSectionClass) then
    FOnCreateSectionClass(Self, HeaderSectionClass);
  Result := HeaderSectionClass.Create(Sections);
end;

function TCustomHeaderControl.CreateSections: THeaderSections;
begin
  Result := THeaderSections.Create(Self);
end;

constructor TCustomHeaderControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSections := CreateSections;
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csNoFocus, csOpaque] -
    [csSetCaption];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TCustomHeaderControl.Destroy;
begin
  FSections.Free;
  inherited Destroy;
end;

procedure TCustomHeaderControl.Click;
var
  Index: Integer;
begin
  if FDown and not FDragging then
  begin
    inherited Click;
    Index := GetSectionAt(ScreenToClient(Mouse.CursorPos));
    if Index <> -1 then
      SectionClick(Sections[Index]);
  end;
end;

procedure TCustomHeaderControl.DblClick;
begin
  inherited DblClick;
  if FTracking then
  begin
    SectionSeparatorDblClick(Sections[FSelectedSection]);
  end;
end;

function TCustomHeaderControl.GetSectionAt(P: TPoint): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Sections.Count - 1 do
    if Sections[i].Visible and (Sections[i].Left <= P.X) and (Sections[i].Right >= P.X) then
    begin
      Result := i;
      break;
    end;
end;

procedure TCustomHeaderControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TCustomHeaderControl.SectionClick(Section: THeaderSection);
begin
  if Assigned(FOnSectionClick) then
    OnSectionClick(Self, Section);
end;

procedure TCustomHeaderControl.SectionResize(Section: THeaderSection);
begin
  if Assigned(FOnSectionResize) then
    FOnSectionResize(Self, Section);
end;

procedure TCustomHeaderControl.SectionTrack(Section: THeaderSection;
  State: TSectionTrackState);
begin
  if Assigned(FOnSectionTrack) then
    FOnSectionTrack(Self, Section, Section.FWidth, State);
end;

procedure TCustomHeaderControl.SectionSeparatorDblClick(Section: THeaderSection);
begin
  if Assigned(FOnSectionSeparatorDblClick) then
    FOnSectionSeparatorDblClick(Self, Section);
end;

procedure TCustomHeaderControl.SectionEndDrag();
begin
  if Assigned(FOnSectionEndDrag) then
    FOnSectionEndDrag(self);
end;

function TCustomHeaderControl.SectionDrag(FromSection,
  ToSection: THeaderSection): Boolean;
begin
  Result:=DragReorder;
  if Result and Assigned(FOnSectionDrag) then
    FOnSectionDrag(self,FromSection,ToSection,Result);
end;

procedure TCustomHeaderControl.MouseEnter;
begin
  inherited MouseEnter;
  if not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    UpdateState;
  end;
end;

procedure TCustomHeaderControl.MouseLeave;
begin
  inherited MouseLeave;
  if not (csDesigning in ComponentState) then
  begin
    FMouseInControl := False;
    FDown := False;
    UpdateState;
  end;
end;

procedure TCustomHeaderControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not (csDesigning in ComponentState) then
  begin
    FDown := True;
    FDownPoint := Point(X, Y);
    if Button = mbLeft then
      if (X > HeaderBorderSize ) and
         (GetSectionAt(Point(X - HeaderBorderSize, Y))<>GetSectionAt(Point(X + HeaderBorderSize, Y))) then
      begin
        FTracking:=true;
        FSelectedSection:=GetSectionAt(Point(X - HeaderBorderSize, Y));
        if FSelectedSection = -1 then
          FTracking:=false
        else
          Cursor:=crSizeE;
        if FTracking then
        begin
          FDown := False;
          SectionTrack(Sections[FSelectedSection], tsTrackBegin);
        end;
      end else
        FSelectedSection:=GetSectionAt(Point(X, Y));
    UpdateState;
  end;
end;

procedure TCustomHeaderControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var CurrentSectionIndex: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if not (csDesigning in ComponentState) then
  begin
    if FTracking and (ssLeft in shift) then
    begin
      if x>=FSections[FSelectedSection].Left then
      begin
        FSections[FSelectedSection].Width := X - FSections[FSelectedSection].Left;
        SectionTrack(Sections[FSelectedSection], tsTrackMove);
      end;
    end
    else if FDragging and (ssLeft in shift) then
    begin
      CurrentSectionIndex:=GetSectionAt(Point(x,y));
      if CurrentSectionIndex>-1 then
      begin
        if (Sections[CurrentSectionIndex].GetLeft + Sections[CurrentSectionIndex].Width div 2 < X) then
          FEndDragSectionIndex:=CurrentSectionIndex+1
         else
          FEndDragSectionIndex:=CurrentSectionIndex;
        if FEndDragSectionIndex < Sections.Count - 1 then
          FDragging:=SectionDrag(Sections[FSelectedSection],Sections[FEndDragSectionIndex])
         else
          FDragging:=SectionDrag(Sections[FSelectedSection],Sections[Sections.Count - 1]);
        RePaint;
      end;
    end
    else if FDown then
    begin
      if DragReorder and (abs(X-FDownPoint.X) >= DragStartDistance) then
      begin
       FDragging:=true;
       FEndDragSectionIndex:=FSelectedSection;
      end else
       if GetSectionAt(Point(X, Y)) <> GetSectionAt(FDownPoint) then
         FDown := False;
    end;
    if shift = [] then
      if (X > HeaderBorderSize) and
         (GetSectionAt(Point(X - HeaderBorderSize, Y))<>GetSectionAt(Point(X + HeaderBorderSize, Y))) then
        Cursor:=crSizeE
       else
        Cursor:=crDefault;
    UpdateState;
  end;
end;

procedure TCustomHeaderControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FTracking then
  begin
    SectionTrack(Sections[FSelectedSection],tsTrackEnd);
    SectionResize(Sections[FSelectedSection]);
  end;
  if FDragging then begin
    if FSelectedSection<FEndDragSectionIndex then
      Sections[FSelectedSection].Index:=FEndDragSectionIndex - 1
     else if FSelectedSection>FEndDragSectionIndex then
      Sections[FSelectedSection].Index:=FEndDragSectionIndex;
    SectionEndDrag();
  end;
  FDown := False;
  FTracking:=false;
  FDragging:=false;
  UpdateState;
end;

procedure TCustomHeaderControl.UpdateState;
var
  i, Index: Integer;
  MaxState: THeaderSectionState;
  P: TPoint;
begin
  MaxState := hsNormal;
  if Enabled then
    if FDown then
    begin
      MaxState := hsPressed;
      Index := FSelectedSection;
    end else if FMouseInControl then
    begin
      MaxState := hsHot;
      P := ScreenToClient(Mouse.CursorPos);
      Index := GetSectionAt(P);
    end;

  for i := 0 to Sections.Count - 1 do
    if (i <> Index) then
      Sections[i].State := hsNormal
    else
      Sections[i].State := MaxState;
end;

class function TCustomHeaderControl.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 170;
  Result.CY := 30;
end;

procedure TCustomHeaderControl.Paint;
var
  Details: TThemedElementDetails;
  i: integer;
begin
  inherited Paint;
  FPaintRect := Rect(0, 0, Width, Height);

  for i := 0 to Sections.Count - 1 do
    PaintSection(i);

  if Sections.Count > 0 then
    FPaintRect.Left := Sections[Sections.Count - 1].Right;
  if FPaintRect.Left < FPaintRect.Right then
  begin
    Details := ThemeServices.GetElementDetails(thHeaderItemRightNormal);
    ThemeServices.DrawElement(Canvas.Handle, Details, FPaintRect);
  end;

  if FDragging then
  begin
    Canvas.Pen.Width:=2;
    Canvas.Pen.Color:=clHotLight;
    if FEndDragSectionIndex < Sections.Count then
      Canvas.MoveTo(Sections[FEndDragSectionIndex].Left,0)
     else
      Canvas.MoveTo(Sections[Sections.Count - 1].Right,0);
    Canvas.LineTo(canvas.PenPos.x,ClientHeight);
  end;
end;

procedure TCustomHeaderControl.PaintSection(Index: Integer);
const
  AlignmentMap: array[TAlignment] of Cardinal =
  (
    DT_LEFT,
    DT_RIGHT,
    DT_CENTER
  );
  HeaderStateMap: array[THeaderSectionState] of TThemedHeader =
  (
    thHeaderItemNormal,
    thHeaderItemHot,
    thHeaderItemPressed
  );
var
  ARect, ContentRect: TRect;
  Details: TThemedElementDetails;
  Section: THeaderSection;
begin
  Section := Sections[Index];
  if not Section.Visible then Exit;
  ARect := FPaintRect;
  ARect.Left := FPaintRect.Left + Section.Left;
  ARect.Right := FPaintRect.Left + Section.Right;
  if ARect.Right <= ARect.Left then
    exit;
  Details := ThemeServices.GetElementDetails(HeaderStateMap[Section.State]);
    
  ThemeServices.DrawElement(Canvas.Handle, Details, ARect);
  ContentRect := ThemeServices.ContentRect(Canvas.Handle, Details, ARect);
  if CompareMem(@ContentRect, @ARect, SizeOf(ARect)) then
    InflateRect(ContentRect, -3, -3);
  
  if (Images <> nil) and (Section.ImageIndex <> -1) then
  begin
    inc(ContentRect.Left);
    ThemeServices.DrawIcon(Canvas, Details,
      Point(ContentRect.Left, (ContentRect.Top + ContentRect.Bottom - Images.Height) div 2),
      Images, Section.ImageIndex);
    inc(ContentRect.Left, Images.Width + 2);
  end;
  
  if Section.Text <> '' then
    ThemeServices.DrawText(Canvas, Details, Section.Text, ContentRect, AlignmentMap[Section.Alignment] or DT_VCENTER or DT_SINGLELINE, 0);
end;

{ THeaderSections }

function THeaderSections.GetItem(Index: Integer): THeaderSection;
begin
  Result := THeaderSection(inherited GetItem(Index));
end;

procedure THeaderSections.SetItem(Index: Integer; Value: THeaderSection);
begin
  inherited SetItem(Index, Value);
end;

function THeaderSections.GetOwner: TPersistent;
begin
  Result := FHeaderControl;
end;

procedure THeaderSections.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    FHeaderControl.UpdateSection(Item.Index)
  else
    FHeaderControl.UpdateSections;
end;

constructor THeaderSections.Create(HeaderControl: TCustomHeaderControl);
begin
  inherited Create(THeaderSection);
  FHeaderControl := HeaderControl;
end;

function THeaderSections.Add: THeaderSection;
begin
  Result := AddItem(nil, Count);
end;

function THeaderSections.AddItem(Item: THeaderSection; Index: Integer): THeaderSection;
var
  i: longint;
begin
  if Item = nil then
    Result := FHeaderControl.CreateSection
  else
    Result := Item;

  Result.Collection := Self;
  if Index > Count then
    Index := Count - 1;
  Result.Index := Index;
  //updates OriginalIndex so that it has the value Index would have if there
  //never was a move
  for i := 0 to Count - 1 do
    if Items[i].FOriginalIndex >= Index then
      Items[i].FOriginalIndex := Items[i].FOriginalIndex + 1;
  Result.FOriginalIndex := Index;
end;

function THeaderSections.Insert(Index: Integer): THeaderSection;
begin
  Result := AddItem(nil, Index);
end;

procedure THeaderSections.Delete(Index: Integer);
var i:longint;
begin
  inherited Delete(Index);
  //updates OriginalIndex so that it has the value Index would have if there
  //never was a move
  for i:=0 to Count - 1 do
    if items[i].FOriginalIndex > Index then
      items[i].FOriginalIndex := items[i].FOriginalIndex - 1;
end;

{ THeaderSection }

function THeaderSection.GetWidth: Integer;
begin
  if Visible then
    Result := FWidth
  else
    Result := 0;
end;

function THeaderSection.GetLeft: Integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Index - 1 do
    Inc(Result, THeaderSections(Collection).Items[i].Width);
end;

function THeaderSection.GetRight: Integer;
begin
  Result := GetLeft + Width;
end;

procedure THeaderSection.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    Changed(False);
  end;
end;

procedure THeaderSection.SetMaxWidth(AValue: Integer);
begin
  if AValue > 10000 then
    AValue := 10000;
  if AValue < FMinWidth then
    AValue := FMinWidth;

  if FMaxWidth <> AValue then
  begin
    FMaxWidth := AValue;
    CheckConstraints;
    Changed(False);
  end;
end;

procedure THeaderSection.SetMinWidth(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if AValue > FMaxWidth then
    AValue := FMaxWidth;

  if FMinWidth <> AValue then
  begin
    FMinWidth := AValue;
    CheckConstraints;
    Changed(False);
  end;
end;

procedure THeaderSection.SetState(const AValue: THeaderSectionState);
begin
  if FState <> AValue then
  begin
    FState := AValue;
    Changed(False);
  end;
end;

procedure THeaderSection.SetText(const Value: TCaption);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

procedure THeaderSection.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed(False);
  end;
end;

procedure THeaderSection.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    CheckConstraints;
    Changed(False);
  end;
end;

procedure THeaderSection.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure THeaderSection.CheckConstraints;
begin
  if FWidth < FMinWidth then
    FWidth := FMinWidth;
  if FWidth > FMaxWidth then
    FWidth := FMaxWidth;
end;

function THeaderSection.GetDisplayName: string;
begin
  if Length(Text) = 0 then
    Result := inherited GetDisplayName
  else
    Result := Text;
end;

constructor THeaderSection.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FWidth := 30;
  FImageIndex := -1;
  FText := '';
  FAlignment := taLeftJustify;
  FState := hsNormal;
  FVisible := True;
  FMinWidth := 0;
  FMaxWidth := 10000;
  FOriginalIndex:=ACollection.Count-1;
end;

procedure THeaderSection.Assign(Source: TPersistent);
var
  SourceSection: THeaderSection absolute Source;
begin
  if Source is THeaderSection then
  begin
    FImageIndex := SourceSection.ImageIndex;
    FText := SourceSection.Text;
    FWidth := SourceSection.Width;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;
