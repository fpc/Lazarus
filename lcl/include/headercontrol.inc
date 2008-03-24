{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TCustomHeaderControl
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
{ TCustomHeaderControl }

const HeaderBorderSize = 2;

procedure TCustomHeaderControl.SetImages(const AValue: TCustomImageList);
begin
  FImages := AValue;
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
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
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
  if not FTracking then
  begin
    inherited Click;
    Index := GetSectionAt(ScreenToClient(Mouse.CursorPos));
    if Index <> -1 then
      SectionClick(Sections[Index]);
  end;
end;

function TCustomHeaderControl.GetSectionAt(P: TPoint): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Sections.Count - 1 do
    if (Sections[i].Left <= P.X) and (Sections[i].Right >= P.X) then
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
      if GetSectionAt(Point(X - HeaderBorderSize, Y))<>GetSectionAt(Point(X + HeaderBorderSize, Y)) then
      begin
        FTracking:=true;
        FSelectedSelection:=GetSectionAt(Point(X - HeaderBorderSize, Y));
        if FSelectedSelection = -1 then
          FTracking:=false
        else
          Cursor:=crSizeE;
        if FTracking then
        begin
          FDown := False;
          SectionTrack(Sections[FSelectedSelection], tsTrackBegin);
        end;
      end;
    UpdateState;
  end;
end;

procedure TCustomHeaderControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not (csDesigning in ComponentState) then
  begin
    if FTracking and (ssLeft in shift) then
    begin
      if x>=FSections[FSelectedSelection].Left then
      begin
        FSections[FSelectedSelection].Width := X - FSections[FSelectedSelection].Left;
        SectionTrack(Sections[FSelectedSelection], tsTrackMove);
      end;
    end
    else
    if FDown then
    begin
      if GetSectionAt(Point(X, Y)) <> GetSectionAt(FDownPoint) then
        FDown := False;
    end;
    if shift = [] then
      if GetSectionAt(Point(X - HeaderBorderSize, Y))<>GetSectionAt(Point(X + HeaderBorderSize, Y)) then
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
    SectionTrack(Sections[FSelectedSelection],tsTrackEnd);
    SectionResize(Sections[FSelectedSelection]);
  end;
  FDown := False;
  FTracking:=false;
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
      MaxState := hsPressed
    else
    if FMouseInControl then
      MaxState := hsHot;

  P := ScreenToClient(Mouse.CursorPos);
  Index := GetSectionAt(P);
  for i := 0 to Sections.Count - 1 do
    if (i <> Index) then
      Sections[i].State := hsNormal
    else
      Sections[i].State := MaxState;
end;

class function TCustomHeaderControl.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=170;
  Result.Y:=30;
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
  Details := ThemeServices.GetElementDetails(thHeaderItemRightNormal);
  ThemeServices.DrawElement(Canvas.Handle, Details, FPaintRect);
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
  ARect: TRect;
  Details: TThemedElementDetails;
  Section: THeaderSection;
begin
  Section := Sections[Index];
  ARect := FPaintRect;
  ARect.Left := FPaintRect.Left + Section.Left;
  ARect.Right := FPaintRect.Left + Section.Right;

  Details := ThemeServices.GetElementDetails(HeaderStateMap[Section.State]);
    
  ThemeServices.DrawElement(Canvas.Handle, Details, ARect);
  ARect := ThemeServices.ContentRect(Canvas.Handle, Details, ARect);
  
  if (Images <> nil) and (Section.ImageIndex <> -1) then
  begin
    inc(ARect.Left);
    ThemeServices.DrawIcon(Canvas, Details,
      Point(ARect.Left, (ARect.Top + ARect.Bottom - Images.Height) div 2),
      Images, Section.ImageIndex);
    inc(ARect.Left, Images.Width + 2);
  end;
  
  if Section.Text <> '' then
    ThemeServices.DrawText(Canvas, Details, Section.Text, Arect, AlignmentMap[Section.Alignment] or DT_VCENTER or DT_SINGLELINE, 0);
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
  Result := AddItem(nil, -1);
end;

function THeaderSections.AddItem(Item: THeaderSection; Index: Integer): THeaderSection;
begin
  if Item = nil then
    Result := FHeaderControl.CreateSection;

  Result.Collection := Self;
  if Index < Count then
    Index := Count - 1;
  Result.Index := Index;
end;

function THeaderSections.Insert(Index: Integer): THeaderSection;
begin
  Result := AddItem(nil, Index);
end;

{ THeaderSection }

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

procedure THeaderSection.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
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

procedure THeaderSection.CheckConstraints; inline;
begin
  if FWidth < FMinWidth then
    FWidth := FMinWidth;
  if FWidth > FMaxWidth then
    FWidth := FMaxWidth;
end;

constructor THeaderSection.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FWidth := 30;
  FImageIndex := -1;
  FText := '';
  FAlignment := taLeftJustify;
  FState := hsNormal;
  FMinWidth := 0;
  FMaxWidth := 10000;
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
