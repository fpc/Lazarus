{%MainUnit ../controls.pp}

{******************************************************************************
                                  TWinControl
 ******************************************************************************

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

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{ $DEFINE CHECK_POSITION}
{ $DEFINE VerboseMouseBugfix}

{------------------------------------------------------------------------------
  TWinControl AdjustSize
  
  Calls DoAutoSize smart.
  During loading and handle creation the calls are delayed.
  
  This method do the same as TWinControl.DoAutoSize at the beginning.
  But since DoAutoSize is commonly overriden by existing Delphi components,
  they do not all tests, which can result in too much overhead. To reduce this
  the LCL calls AdjustSize instead.
------------------------------------------------------------------------------}
procedure TWinControl.AdjustSize;
begin
  If not AutoSizeCanStart then exit;
  if AutoSizeDelayed then begin
    //debugln('TWinControl.AdjustSize AutoSizeDelayed ',DbgSName(Self));
    Include(FWinControlFlags,wcfAutoSizeNeeded);
    exit;
  end;
  //debugln('TWinControl.AdjustSize DoAutoSize ',DbgSName(Self));
  DoAutoSize;
end;

{------------------------------------------------------------------------------
  function TWinControl.AutoSizeDelayed: boolean;
------------------------------------------------------------------------------}
function TWinControl.AutoSizeDelayed: boolean;
begin
  Result:=(wcfCreatingChildHandles in FWinControlFlags)
          or (inherited AutoSizeDelayed);
  //if Result then debugln('TWinControl.AutoSizeDelayed A ',DbgSName(Self),' wcfCreatingChildHandles=',dbgs(wcfCreatingChildHandles in FWinControlFlags),' csLoading=',dbgs(csLoading in ComponentState));
end;

{------------------------------------------------------------------------------
  TWinControl AdjustClientRect
------------------------------------------------------------------------------}
Procedure TWinControl.AdjustClientRect(var ARect: TRect);
Begin
  //Not used.  It's a virtual procedure that should be overriden.
end;

{------------------------------------------------------------------------------
  TWinControl AlignControls

  Align child controls

------------------------------------------------------------------------------}
procedure TWinControl.AlignControls(AControl: TControl;
  var RemainingClientRect: TRect);
var
  AlignList: TList;
  RemainingBorderSpace: TRect; // borderspace around RemainingClientRect

  function AlignWork: Boolean;
  var
    I: Integer;
    CurControl: TControl;
  begin
    Result := True;

    for I := ControlCount - 1 downto 0 do
    begin
      CurControl:=Controls[I];
      if (CurControl.Align <> alNone)
      or (CurControl.Anchors <> [akLeft, akTop])
      or (CurControl.AnchorSide[akLeft].Control<>nil)
      or (CurControl.AnchorSide[akTop].Control<>nil)
      then Exit;
    end;
    Result := False;
  end;

  function Anchored(Align: TAlign; Anchors: TAnchors): Boolean;
  begin
    case Align of
      alLeft: Result := akLeft in Anchors;
      alTop: Result := akTop in Anchors;
      alRight: Result := akRight in Anchors;
      alBottom: Result := akBottom in Anchors;
      alClient: Result := Anchors = [akLeft, akTop, akRight, akBottom];
    else
      Result := False;
    end;
  end;
  
  procedure DoPosition(Control: TControl; AAlign: TAlign);
  var
    NewLeft, NewTop, NewWidth, NewHeight: Integer;
    ParentBaseClientSize: TPoint;
    CurBaseBounds: TRect;
    NewRight: Integer;
    NewBottom: Integer;
  var
    MinWidth: Integer;
    MaxWidth: Integer;
    MinHeight: Integer;
    MaxHeight: Integer;
    CurRemainingClientRect: TRect;
    CurRemainingBorderSpace: TRect;
    ChildAroundSpace: TRect;
    AnchorSideCacheValid: array[TAnchorKind] of boolean;
    AnchorSideCache: array[TAnchorKind] of integer;

    function ConstraintWidth(NewWidth: integer): integer;
    begin
      Result:=NewWidth;
      if (MaxWidth>=MinWidth) and (Result>MaxWidth) and (MaxWidth>0) then
        Result:=MaxWidth;
      if Result<MinWidth then Result:=MinWidth;
    end;

    function ConstraintHeight(NewHeight: integer): integer;
    begin
      Result:=NewHeight;
      if (MaxHeight>=MinHeight) and (Result>MaxHeight) and (MaxHeight>0) then
        Result:=MaxHeight;
      if Result<MinHeight then Result:=MinHeight;
    end;
    
    procedure InitAnchorSideCache;
    var
      a: TAnchorKind;
    begin
      for a:=Low(TAnchorKind) to High(TAnchorKind) do
        AnchorSideCacheValid[a]:=false;
    end;
    
    function GetAnchorSidePosition(Kind: TAnchorKind;
      DefaultPosition: Integer): integer;
    var
      CurAnchorSide: TAnchorSide;
      ReferenceControl: TControl;
      ReferenceSide: TAnchorSideReference;
      Position: Integer;
    begin
      if AnchorSideCacheValid[Kind] then begin
        Result:=AnchorSideCache[Kind];
        exit;
      end;
      Result:=DefaultPosition;
      CurAnchorSide:=Control.AnchorSide[Kind];
      //debugln('GetAnchorSidePosition A Self=',DbgSName(Self),' Control=',DbgSName(Control),' ',DbgSName(CurAnchorSide.Control));
      CurAnchorSide.GetSidePosition(ReferenceControl,ReferenceSide,Position);
      if ReferenceControl<>nil then
        Result:=Position;
      //debugln('GetAnchorSidePosition B Self=',DbgSName(Self),' Control=',DbgSName(Control),' ',dbgs(Result));
      AnchorSideCacheValid[Kind]:=true;
      AnchorSideCache[Kind]:=Result;
    end;

  begin
    {$IFDEF CHECK_POSITION}
    if AnsiCompareText(Control.ClassName,'TScrollBar')=0 then
    with Control do
      DebugLn('[TWinControl.AlignControls.DoPosition] A Control=',Name,':',ClassName,' ',dbgs(Left),',',dbgs(Top),',',dbgs(Width),',',dbgs(Height),' recalculate the anchors=',dbgs(Control.Anchors <> AnchorAlign[AAlign]),' Align=',AlignNames[AAlign]);
    {$ENDIF}


    with Control do begin
      // get constraints
      MinWidth:=Constraints.EffectiveMinWidth;
      if MinWidth<0 then MinWidth:=0;
      MaxWidth:=Constraints.EffectiveMaxWidth;
      MinHeight:=Constraints.EffectiveMinHeight;
      if MinHeight<0 then MinHeight:=0;
      MaxHeight:=Constraints.EffectiveMaxHeight;
      
      // get default bounds
      NewLeft:=Left;
      NewTop:=Top;
      NewWidth:=ConstraintWidth(Width);
      NewHeight:=ConstraintHeight(Height);
    end;
    
    InitAnchorSideCache;
    
    { Recalculate the anchors

      Use Anchors to ensure that a control maintains its current position
      relative to an edge of its parent or another sibling.
      This is controlled with the AnchorSide properties.

      1. If AnchorSide[].Control is not set, the distance is kept relative to
      the edges of the client are of its parent.
      When its parent is resized, the control holds its position relative to the
      edges to which it is anchored.
      If a control is anchored to opposite edges of its parent, the control
      stretches when its parent is resized. For example, if a control has its
      Anchors property set to [akLeft,akRight], the control stretches when the
      width of its parent changes.
      Anchors is enforced only when the parent is resized. Thus, for example,
      if a control is anchored to opposite edges of a form at design time and
      the form is created in a maximized state, the control is not stretched
      because the form is not resized after the control is created.
      
      2. If AnchorSide[].Control is set, the BorderSpace properties defines the
      distance to another sibling (i.e. AnchorSide[].Control).
    }
    if (AAlign = alNone) or (Control.Anchors <> AnchorAlign[AAlign]) then
    begin
      // Get the base bounds. The base bounds are the user defined bounds
      // without automatic aligning and/or anchoring

      // get base size of parents client area
      ParentBaseClientSize:=Control.FBaseParentClientSize;
      if (ParentBaseClientSize.X=0)
      and (ParentBaseClientSize.Y=0) then
        ParentBaseClientSize:=Point(Control.Parent.ClientWidth,
                                    Control.Parent.ClientHeight);

      // get base bounds of Control
      CurBaseBounds:=Control.FBaseBounds;
      if (CurBaseBounds.Right=CurBaseBounds.Left)
      and (CurBaseBounds.Bottom=CurBaseBounds.Top) then
        CurBaseBounds:=Control.BoundsRect;

      {$IFDEF CHECK_POSITION}
      //if csDesigning in ComponentState then
      if AnsiCompareText(Control.ClassName,'TScrollBar')=0 then
      DebugLn('[TWinControl.AlignControls.DoPosition] Before Anchoring ',
        ' ',Name,':',ClassName,
        ' CurBaseBounds='+dbgs(CurBaseBounds.Left)+','+dbgs(CurBaseBounds.Top)+','+dbgs(CurBaseBounds.Right-CurBaseBounds.Left)+','+dbgs(CurBaseBounds.Bottom-CurBaseBounds.Top),
        ' ParBaseClient='+dbgs(ParentBaseClientSize.X)+','+dbgs(ParentBaseClientSize.Y),
        ' ParClient='+dbgs(Control.Parent.ClientWidth)+','+dbgs(Control.Parent.ClientHeight),
        ' NewBounds='+dbgs(NewLeft)+','+dbgs(NewTop)+','+dbgs(NewWidth)+','+dbgs(NewHeight),
        '');
      {$ENDIF}

      if akLeft in Control.Anchors then begin
        // keep distance to left side of parent or another sibling
        NewLeft:=GetAnchorSidePosition(akLeft,CurBaseBounds.Left);
        if akRight in Control.Anchors then begin
          // keep distance to right side of parent or another sibling
          // -> change the width
          NewRight:=Control.Parent.ClientWidth
                    -(ParentBaseClientSize.X-CurBaseBounds.Right);
          NewRight:=GetAnchorSidePosition(akRight,NewRight);
          NewWidth:=ConstraintWidth(NewRight-NewLeft);
        end else begin
          // do not anchor to the right
          // -> keep new width
        end;
      end else begin
        // do not anchor to the left
        if akRight in Control.Anchors then begin
          // keep distance to right side of parent
          // and keep new width
          NewRight:=Control.Parent.ClientWidth
                    -(ParentBaseClientSize.X-CurBaseBounds.Right);
          NewRight:=GetAnchorSidePosition(akRight,NewRight);
          NewLeft:=NewRight-NewWidth;
        end else begin
          // do not anchor to the right
          // -> keep new width and center horizontally
          NewLeft:=(Control.Parent.ClientWidth-NewWidth) div 2;
        end;
      end;

      if akTop in Control.Anchors then begin
        // keep distance to top side of parent
        NewTop:=GetAnchorSidePosition(akTop,CurBaseBounds.Top);
        if akBottom in Control.Anchors then begin
          // keep distance to bottom side of parent
          // -> change the height
          NewBottom:=Control.Parent.ClientHeight
                    -(ParentBaseClientSize.Y-CurBaseBounds.Bottom);
          NewBottom:=GetAnchorSidePosition(akBottom,NewBottom);
          NewHeight:=ConstraintHeight(NewBottom-NewTop);
        end else begin
          // do not anchor to the bottom
          // -> keep new height
        end;
      end else begin
        // do not anchor to the top
        if akBottom in Control.Anchors then begin
          // keep distance to bottom side of parent
          // and keep new height
          NewBottom:=Control.Parent.ClientHeight
                    -(ParentBaseClientSize.Y-CurBaseBounds.Bottom);
          NewBottom:=GetAnchorSidePosition(akBottom,NewBottom);
          NewTop:=NewBottom-NewHeight;
        end else begin
          // do not anchor to the bottom
          // -> keep new height and center vertically
          NewTop:=(Control.Parent.ClientHeight-NewHeight) div 2;
        end;
      end;
      {$IFDEF CHECK_POSITION}
      //if csDesigning in ComponentState then
      if AnsiCompareText(Control.ClassName,'TScrollBar')=0 then
      with Control do
        DebugLn('[TWinControl.AlignControls.DoPosition] After Anchoring',
          ' ',Name,':',ClassName,
          ' Align=',AlignNames[AAlign],
          ' Control=',Name,':',ClassName,
          ' Old=',Left,',',Top,',',Width,',',Height,
          ' New=',NewLeft,',',NewTop,',',NewWidth,',',NewHeight,
          '');
      {$ENDIF}
    end;

    // set min size to stop circling (this should not be needed. But if someone
    // plays/fixes the above code, new bugs can enter and there are far too many
    // combinations to test, and so the LCL can loop for some applications.
    // Prevent this, so users can at least report a bug.)
    if NewWidth<0 then NewWidth:=0;
    if NewHeight<0 then NewHeight:=0;

    if AAlign in [alLeft,alTop,alRight,alBottom,alClient] then begin
      { Realign

        Use Align to align a control to the top, bottom, left, or right of a
        form or panel and have it remain there even if the size of the form,
        panel, or component that contains the control changes. When the parent
        is resized, an aligned control also resizes so that it continues to span
        the top, bottom, left, or right edge of the parent (more exact:
        span the remaining client area of its parent).
      }
      NewRight:=NewLeft+NewWidth;
      NewBottom:=NewTop+NewHeight;

      // calculate current RemainingClientRect for the current Control
      CurRemainingClientRect:=RemainingClientRect;
      CurRemainingBorderSpace:=RemainingBorderSpace;
      Control.BorderSpacing.GetSpaceAround(ChildAroundSpace);
      AdjustBorderSpace(CurRemainingClientRect,CurRemainingBorderSpace,
                        ChildAroundSpace);

      {$IFDEF CHECK_POSITION}
      if AnsiCompareText(Control.ClassName,'TScrollBar')=0 then
        DebugLn(' Before aligning  akRight in AnchorAlign[AAlign]=',akRight in AnchorAlign[AAlign],
                ' akLeft in Control.Anchors=',akLeft in Control.Anchors,
                ' ARect=',ARect.Left,',',ARect.Top,',',ARect.Right,',',ARect.Bottom,
                ' New=',NewLeft,',',NewTop,',',NewRight,',',NewBottom);
      {$ENDIF}
      if akLeft in AnchorAlign[AAlign] then begin
        if (akRight in (Control.Anchors+AnchorAlign[AAlign])) then begin
          // left align and keep right border
          NewLeft:=CurRemainingClientRect.Left;
          NewRight:=NewLeft+ConstraintWidth(NewRight-NewLeft);
        end else begin
          // left align and right border free to move (-> keep width)
          dec(NewRight,NewLeft-RemainingClientRect.Left);
          NewLeft:=CurRemainingClientRect.Left;
        end;
      end;
      if akTop in AnchorAlign[AAlign] then begin
        if (akBottom in (Control.Anchors+AnchorAlign[AAlign])) then begin
          // top align and keep bottom border
          NewTop:=CurRemainingClientRect.Top;
          NewBottom:=NewTop+ConstraintHeight(NewBottom-NewTop);
        end else begin
          // top align and bottom border is free to move (-> keep height)
          dec(NewBottom,NewTop-RemainingClientRect.Top);
          NewTop:=CurRemainingClientRect.Top;
        end;
      end;
      if akRight in AnchorAlign[AAlign] then begin
        if (akLeft in (Control.Anchors+AnchorAlign[AAlign])) then begin
          // right align and keep left border
          NewWidth:=ConstraintWidth(CurRemainingClientRect.Right-NewLeft);
          if AAlign=alRight then begin
            // align to right (this overrides the keeping of left border)
            NewRight:=CurRemainingClientRect.Right;
            NewLeft:=NewRight-NewWidth;
          end else begin
            // keep left border overrides keeping right border
            NewRight:=NewLeft+NewWidth;
          end;
        end else begin
          // right align and left border free to move (-> keep width)
          inc(NewLeft,CurRemainingClientRect.Right-NewRight);
          NewRight:=CurRemainingClientRect.Right;
        end;
      end;
      if akBottom in AnchorAlign[AAlign] then begin
        if (akTop in (Control.Anchors+AnchorAlign[AAlign])) then begin
          // bottom align and keep top border
          NewHeight:=ConstraintHeight(CurRemainingClientRect.Bottom-NewTop);
          if AAlign=alBottom then begin
            // align to bottom (this overrides the keeping of top border)
            NewBottom:=CurRemainingClientRect.Bottom;
            NewTop:=NewBottom-NewHeight;
          end else begin
            // keeping top border overrides keeping bottom border
            NewBottom:=NewTop+NewHeight;
          end;
        end else begin
          // bottom align and top border free to move (-> keep height)
          inc(NewTop,CurRemainingClientRect.Bottom-NewBottom);
          NewBottom:=CurRemainingClientRect.Bottom;
        end;
      end;
      NewWidth:=Max(0,NewRight-NewLeft);
      NewHeight:=Max(0,NewBottom-NewTop);
      
      {$IFDEF CHECK_POSITION}
      //if csDesigning in Control.ComponentState then
      if AnsiCompareText(Control.ClassName,'TScrollBar')=0 then
      with Control do
        DebugLn('[TWinControl.AlignControls.DoPosition] After Aligning',
          ' ',Name,':',ClassName,
          ' Align=',AlignNames[AAlign],
          ' Control=',Name,':',ClassName,
          ' Old=',Left,',',Top,',',Width,',',Height,
          ' New=',NewLeft,',',NewTop,',',NewWidth,',',NewHeight,
          ' ARect=',ARect.Left,',',ARect.Top,',',ARect.Right-ARect.Left,',',ARect.Bottom-ARect.Top,
          '');
      {$ENDIF}
    end;

    // set the new bounds
    if (Control.Left <> NewLeft) or (Control.Top <> NewTop)
    or (Control.Width <> NewWidth) or (Control.Height <> NewHeight) then begin
      {$IFDEF CHECK_POSITION}
      //if csDesigning in Control.ComponentState then
      if AnsiCompareText(Control.ClassName,'TScrollBar')=0 then
      with Control do
        DebugLn('[TWinControl.AlignControls.DoPosition] NEW BOUNDS Control=',Name,':',ClassName,' NewBounds=',NewLeft,',',NewTop,',',NewWidth,',',NewHeight,' Align=',AlignNames[AAlign]);
      {$ENDIF}
      // lock the base bounds, so that the new automatic bounds do not override
      // the user settings
      Control.SetAlignedBounds(NewLeft, NewTop, NewWidth, NewHeight);
      // Sometimes SetBounds change the bounds. For example due to constraints.
      // update the new bounds
      with Control do
      begin
        NewLeft:=Left;
        NewTop:=Top;
        NewWidth:=Width;
        NewHeight:=Height;
      end;
      {$IFDEF CHECK_POSITION}
      //if csDesigning in Control.ComponentState then
      if AnsiCompareText(Control.ClassName,'TScrollBar')=0 then
      with Control do
        DebugLn('[TWinControl.AlignControls.DoPosition] AFTER SETBOUND Control=',Name,':',ClassName,' Bounds=',Left,',',Top,',',Width,',',Height);
      {$ENDIF}
    end;

    // adjust the remaining client area
    case AAlign of
      alTop:
        begin
          RemainingClientRect.Top:=NewTop+NewHeight;
          RemainingBorderSpace.Top:=0;
          AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
            0,Max(ChildSizing.VerticalSpacing,ChildAroundSpace.Bottom),0,0);
        end;
      alBottom:
        begin
          RemainingClientRect.Bottom:=NewTop;
          RemainingBorderSpace.Bottom:=0;
          AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
            0,0,0,Max(ChildSizing.VerticalSpacing,ChildAroundSpace.Top));
        end;
      alLeft:
        begin
          RemainingClientRect.Left:=NewLeft+NewWidth;
          RemainingBorderSpace.Left:=0;
          AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
            Max(ChildSizing.HorizontalSpacing,ChildAroundSpace.Right),0,0,0);
        end;
      alRight:
        begin
          RemainingClientRect.Right:=NewLeft;
          RemainingBorderSpace.Right:=0;
          AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
            0,0,Max(ChildSizing.HorizontalSpacing,ChildAroundSpace.Left),0);
        end;
      alClient:
        begin
          // VCL is tricking here.
          // For alClients with Constraints do the same as for alLeft
          {debugln('TWinControl.AlignControls.DoPosition A Self=',Name,' Control=',DbgSName(Control),' ',dbgs(NewLeft),' ',dbgs(NewWidth));
            RemainingClientRect.Left:=NewLeft+NewWidth;
            RemainingBorderSpace.Left:=0;
            AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
              Max(ChildSizing.HorizontalSpacing,ChildAroundSpace.Right),0,0,0);}
        end;
    end;

    {$IFDEF CHECK_POSITION}
    if AnsiCompareText(Control.ClassName,'TScrollBar')=0 then
    with Control do
      DebugLn('[TWinControl.AlignControls.DoPosition] END Control=',
        Name,':',ClassName,
        ' ',Left,',',Top,',',Width,',',Height,
        ' Align=',AlignNames[AAlign],
        ' ARect=',ARect.Left,',',ARect.Top,',',ARect.Right-ARect.Left,',',ARect.Bottom-ARect.Top,
        '');
    {$ENDIF}
  end;

  function InsertBefore(Control1, Control2: TControl;
    AAlign: TAlign): Boolean;
  begin
    Result := False;
    case AAlign of
      alTop: Result := Control1.Top < Control2.Top;
      alBottom: Result := (Control1.Top + Control1.Height)
                          >= (Control2.Top + Control2.Height);
      alLeft: Result := Control1.Left < Control2.Left;
      alRight: Result := (Control1.Left + Control1.Width)
                         >= (Control2.Left + Control2.Width);
    end;
  end;

  procedure DoAlign(AAlign: TAlign);
  var
    I, X: Integer;
    Control: TControl;
  begin
    AlignList.Clear;
    
    // first add the current control
    if (AControl <> nil)
      and (AControl.Align = AAlign)
      and ((AAlign = alNone)
        or AControl.Visible
        or ((csDesigning in AControl.ComponentState)
        and not (csNoDesignVisible in AControl.ControlStyle)))
    then
      AlignList.Add(AControl);

    // then add all other
    for I := 0 to ControlCount - 1 do
    begin
      Control := Controls[I];

      if (Control.Align = AAlign)
        and ((AAlign = alNone)
          or (Control.Visible
          or (Control.ControlStyle * [csAcceptsControls, csNoDesignVisible] =
               [csAcceptsControls, csNoDesignVisible]))
          or ((csDesigning in Control.ComponentState)
              and not (csNoDesignVisible in Control.ControlStyle))) then
      begin
        if Control = AControl then Continue;

        X := 0;
        while (X < AlignList.Count)
        and not InsertBefore(Control, TControl(AlignList[X]), AAlign) do
          Inc(X);
        AlignList.Insert(X, Control);
      end;
    end;
    
    if not DoAlignChildControls(AAlign,AControl,AlignList,RemainingClientRect) then
      for I := 0 to AlignList.Count - 1 do
        DoPosition(TControl(AlignList[I]), AAlign);
  end;

var
  i: Integer;
  ChildControl: TControl;
begin
  if wcfAligningControls in FWinControlFlags then exit;
  Include(FWinControlFlags,wcfAligningControls);

  // unset all align needed flags
  Exclude(FWinControlFlags,wcfReAlignNeeded);
  for i:=ControlCount-1 downto 0 do begin
    ChildControl:=Controls[i];
    Exclude(ChildControl.FControlFlags,cfRequestAlignNeeded);
  end;

  try
    //if csDesigning in ComponentState then begin
      //DebugLn('[TWinControl.AlignControls] ',Name,':',Classname,' ',Left,',',Top,',',Width,',',Height,' AlignWork=',AlignWork,' ControlCount=',ControlCount);
      //if AControl<>nil then DebugLn('  AControl=',AControl.Name,':',AControl.ClassName);
    //end;
    if AlignWork then
    begin
      AdjustClientRect(RemainingClientRect);
      RemainingBorderSpace:=Rect(0,0,0,0);
      // adjust RemainingClientRect by ChildSizing properties
      AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
        ChildSizing.LeftRightSpacing,ChildSizing.TopBottomSpacing,
        ChildSizing.LeftRightSpacing,ChildSizing.TopBottomSpacing);
      //DebugLn('[TWinControl.AlignControls] ',Name,':',Classname,' ',Left,',',Top,',',Width,',',Height,' ClientRect=',RemainingClientRect.Left,',',RemainingClientRect.Top,',',RemainingClientRect.Right,',',RemainingClientRect.Bottom);
      AlignList := TList.Create;
      try
        DoAlign(alTop);
        DoAlign(alBottom);
        DoAlign(alLeft);
        DoAlign(alRight);
        DoAlign(alClient);
        DoAlign(alCustom);
        DoAlign(alNone);
      finally
        AlignList.Free;
      end;
    end;
    ControlsAligned;
  finally
    Exclude(FWinControlFlags,wcfAligningControls);
  end;

  if Showing then AdjustSize;
end;

function TWinControl.DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
  AControlList: TList; var ARect: TRect): Boolean;
begin
  Result:=false;
end;

procedure TWinControl.DoChildSizingChange(Sender: TObject);
begin
  InvalidatePreferredSize;
  AdjustSize;
end;

Procedure TWinControl.DoAutoSize;
var
  I : Integer;
  AControl: TControl;
  PreferredWidth: LongInt;
  PreferredHeight: LongInt;
  ChildBounds: TRect;
begin
  //debugln('TWinControl.DoAutoSize ',DbgSName(Self));
  If not AutoSizeCanStart then exit;
  if AutoSizeDelayed then begin
    Include(FWinControlFlags,wcfAutoSizeNeeded);
    exit;
  end;

  AutoSizing := True;
  try
    // autosize control to preferred size
    GetPreferredSize(PreferredWidth,PreferredHeight,false);

    // move childs tight to left and top
    If ControlCount > 0 then begin
      GetChildBounds(ChildBounds,true);
      if (ChildBounds.Left<>0) or (ChildBounds.Top<>0) then begin
        For I := 0 to ControlCount - 1 do begin
          AControl:=Controls[I];
          If AControl.Visible then begin
            AControl.SetBoundsKeepBase(AControl.Left - ChildBounds.Left,
                                       AControl.Top - ChildBounds.Top,
                                       AControl.Width,AControl.Height,true);
          end;
        end;
      end;
    end;

    // set new size
    {$IFDEF VerboseAutoSize}
    debugln('DoAutoSize A ',DbgSName(Self),' Cur=',dbgs(Width),'x',dbgs(Height),' Prefer=',dbgs(PreferredWidth),'x',dbgs(PreferredHeight),' WidgetClass=',WidgetSetClass.ClassName);
    {$ENDIF}
    if (PreferredWidth<>Width) or (PreferredHeight<>Height) then begin
      //debugln('DoAutoSize Resize ',DbgSName(Self),' W=',dbgs(PreferredWidth),' H=',dbgs(PreferredHeight));
      {$IFDEF EnablePreferredSize}
      SetBoundsKeepBase(Left,Top,PreferredWidth,PreferredHeight,true);
      {$ENDIF}
    end;
  finally
    AutoSizing := False;
  end;
  Exclude(FWinControlFlags,wcfAutoSizeNeeded);
end;

{------------------------------------------------------------------------------}
{  TWinControl BroadCast                                                       }
{------------------------------------------------------------------------------}
Procedure TWinControl.BroadCast(var ToAllMessage);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    Controls[I].WindowProc(TLMessage(ToAllMessage));
    if TLMessage(ToAllMessage).Result <> 0 then Exit;
  end;
end;

procedure TWinControl.NotifyControls(Msg: Word);
var
  ToAllMessage: TLMessage;
begin
  ToAllMessage.Msg := Msg;
  ToAllMessage.WParam := 0;
  ToAllMessage.LParam := 0;
  ToAllMessage.Result := 0;
  Broadcast(ToAllMessage);
end;

procedure TWinControl.DefaultHandler(var AMessage);
begin
  CallDefaultWndHandler(Self,AMessage);
end;


{------------------------------------------------------------------------------}
{  TWinControl CanFocus                                                        }
{------------------------------------------------------------------------------}
Function TWinControl.CanFocus : Boolean;
var
  Control: TWinControl;
  Form: TCustomForm;
begin
  Result := False;
  //Verify that every parent is enabled and visible before returning true.
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    Control := Self;
    repeat
      if not (Control.FVisible and Control.Enabled) then Exit;
      if Control = Form then break;
      Control := Control.Parent;
    until false;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl CMDrag
------------------------------------------------------------------------------}
Procedure TWinControl.CMDrag(var Message: TCMDrag);
Begin
  {$IFDEF VerboseDrag}
  DebugLn('TWinControl.CMDrag ',Name,':',ClassName,' ',ord(Message.DragMessage));
  {$ENDIF}
  DoDragMsg(Message);
end;

{------------------------------------------------------------------------------
  TWinControl CreateSubClass
------------------------------------------------------------------------------}
procedure TWinControl.CreateSubClass(var Params: TCreateParams;
  ControlClassName: PChar);
begin
  // TODO: Check if we need this method
end;

{------------------------------------------------------------------------------
  TWinControl DisableAlign
------------------------------------------------------------------------------}
procedure TWinControl.DisableAlign;
begin
  Inc(FAlignLevel);
End;

{-------------------------------------------------------------------------------
  TWinControl DoAdjustClientRectChange

  Asks the interface if clientrect has changed since last AlignControl
  and calls AlignControl(nil) on change.
-------------------------------------------------------------------------------}
procedure TWinControl.DoAdjustClientRectChange;
var r: TRect;
begin
  r:=GetClientRect;
  AdjustClientRect(r);
  //DebugLn(' TWinControl.DoAdjustClientRectChange ',Name,':',ClassName,' ',dbgs(r.Right),',',dbgs(r.Bottom));
  if not CompareRect(@r,@FAdjustClientRectRealized) then begin
    // client rect changed since last AlignControl
    {$IFDEF VerboseClientRectBugFix}
    DebugLn('UUU TWinControl.DoAdjustClientRectChange ClientRect changed ',Name,':',ClassName,
      ' Old=',FAdjustClientRectRealized.Right,'x',FAdjustClientRectRealized.Bottom,
      ' New=',r.RIght,'x',r.Bottom);
    {$ENDIF}
    FAdjustClientRectRealized:=r;
    ReAlign;
    Resize;
  end;
end;

{-------------------------------------------------------------------------------
  TWinControl DoConstraintsChange
  Params: Sender : TObject

  Call inherited, then send the constraints to the interface
-------------------------------------------------------------------------------}
procedure TWinControl.DoConstraintsChange(Sender : TObject);
begin
  inherited DoConstraintsChange(Sender);
  //debugln('TWinControl.DoConstraintsChange ',DbgSName(Self),' HandleAllocated=',dbgs(HandleAllocated));
  if HandleAllocated then
    TWSWinControlClass(WidgetSetClass).ConstraintsChange(Self);
end;

{-------------------------------------------------------------------------------
  TWinControl InvalidateClientRectCache(WithChildControls: boolean)

  The clientrect is cached. Call this procedure to invalidate the cache, so that
  next time the clientrect is fetched from the interface.
-------------------------------------------------------------------------------}
procedure TWinControl.InvalidateClientRectCache(WithChildControls: boolean);
var
  I: Integer;
begin
  {$IFDEF VerboseClientRectBugFix}
  DebugLn('[TWinControl.InvalidateClientRectCache] ',Name,':',ClassName);
  {$ENDIF}
  Include(FWinControlFlags,wcfClientRectNeedsUpdate);

  if WithChildControls then begin
    // invalidate clients too
    if Assigned(FWinControls) then
      for I := 0 to FWinControls.Count - 1 do
        if Assigned(FWinControls.Items[I]) then
          TWinControl(FWinControls.Items[I]).InvalidateClientRectCache(true);
  end;
end;

{-------------------------------------------------------------------------------
  TWinControl InvalidateClientRectCache

  The clientrect is cached. Check if cache is valid.
-------------------------------------------------------------------------------}
function TWinControl.ClientRectNeedsInterfaceUpdate: boolean;
var
  IntfClientRect: TRect;
begin
  Result:=false;
  if not HandleAllocated then exit;
  LCLIntf.GetClientRect(Handle,IntfClientRect);
  Result:=(FClientWidth<>IntfClientRect.Right)
       or (FClientHeight<>IntfClientRect.Bottom);
end;

{-------------------------------------------------------------------------------
  TWinControl DoSetBounds
  Params: ALeft, ATop, AWidth, AHeight : integer

  Anticipate the new clientwidth/height and call inherited

  Normally the clientwidth/clientheight is adjusted automatically by the
  interface. But it is up to interface when this will be done. The gtk for
  example just puts resize requests into a queue. The LCL would resize the
  childs just after this procedure due to the clientrect. On complex forms with
  lots of nested controls, this would result in thousands of resizes.
  Changing the clientrect in the LCL to the most probable size reduces
  unneccessary resizes.
-------------------------------------------------------------------------------}
procedure TWinControl.DoSetBounds(ALeft, ATop, AWidth, AHeight : integer);
begin
  if wcfClientRectNeedsUpdate in FWinControlFlags then begin
    GetClientRect;
  end;
  {$IFDEF VerboseClientRectBugFix}
  DbgOut('[TWinControl.DoSetBounds] ',Name,':',ClassName,' OldClient=',FClientWidth,',',FClientHeight,
  ' OldHeight=',FHeight,' NewHeight=',AHeight);
  {$ENDIF}
  inc(FClientWidth,AWidth-FWidth);
  if (FClientWidth<0) then FClientWidth:=0;
  inc(FClientHeight,AHeight-FHeight);
  if (FClientHeight<0) then FClientHeight:=0;
  {$IFDEF VerboseClientRectBugFix}
  DebugLn(' NewClient=',FClientWidth,',',FClientHeight);
  {$ENDIF}

  inherited DoSetBounds(ALeft,ATop,AWidth,AHeight);
end;

{------------------------------------------------------------------------------}
{  TWinControl EnableAlign                                                     }
{------------------------------------------------------------------------------}
procedure TWinControl.EnableAlign;
begin
  Dec(FAlignLevel);
  if FAlignLevel = 0 then begin
    if csAlignmentNeeded in ControlState then ReAlign;
  end;
end;

{------------------------------------------------------------------------------}
{       TWinControl.CanTab
}
{------------------------------------------------------------------------------}
Function TWinControl.CanTab: Boolean;
begin
  Result := CanFocus;
end;

procedure TWinControl.DoDragMsg(var DragMsg: TCMDrag);
var
  TargetControl: TControl;
begin
  case DragMsg.DragMessage of

  dmFindTarget:
    begin
      {$IFDEF VerboseDrag}
      DebugLn('TWinControl.DoDragMsg dmFindTarget ',Name,':',ClassName,' Start DragMsg.DragRec^.Pos=',DragMsg.DragRec^.Pos.X,',',DragMsg.DragRec^.Pos.Y);
      {$ENDIF}
      TargetControl := ControlatPos(ScreentoClient(DragMsg.DragRec^.Pos),False);
      if TargetControl = nil then TargetControl := Self;
      {$IFDEF VerboseDrag}
      DebugLn('TWinControl.DoDragMsg dmFindTarget ',Name,':',ClassName,' End Result=',TargetControl.Name,':',TargetControl.ClassName);
      {$ENDIF}
      DragMsg.Result := LRESULT(TargetControl);
    end;

  else
    inherited DoDragMsg(DragMsg);
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl GetChildren                                                     }
{------------------------------------------------------------------------------}
Procedure TWinControl.GetChildren(Proc: TGetChildProc; Root : TComponent);
var
  I : Integer;
  Control : TControl;
Begin
  for I := 0 to ControlCount-1 do
  Begin
    Control := Controls[i];
    if Control.Owner = Root then Proc(Control);
  end;
End;

{-------------------------------------------------------------------------------
  function TWinControl.ChildClassAllowed(ChildClass: TClass): boolean;

  Allow TControl as child.
-------------------------------------------------------------------------------}
function TWinControl.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result:=(ChildClass<>nil) and ChildClass.InheritsFrom(TControl);
end;

{-------------------------------------------------------------------------------
  TWinControl GetClientOrigin
  Result:  TPoint

  returns the screen coordinate of the topleft coordinate 0,0 of the client area
  Note that this value is the position as stored in the interface and is not
  always in sync with the LCL. When a control is moved, the LCL sets the bounds
  to the wanted position and sends a move message to the interface. It is up to
  the interface to handle moves instantly or queued.
-------------------------------------------------------------------------------}
Function TWinControl.GetClientOrigin: TPoint;
var
  AControl: TWinControl;
Begin
  Result.X := 0;
  Result.Y := 0;
//DebugLn('[TWinControl.GetClientOrigin] ',Name,':',ClassName,'  ',Handle);
  if HandleAllocated then begin
    // get the interface idea where the client area is on the screen
    LCLIntf.ClientToScreen(Handle,Result);
    // adjust the result by all bounds, that are not yet sent to the interface
    AControl:=Self;
    repeat
      inc(Result.X,AControl.Left-AControl.FBoundsRealized.Left);
      inc(Result.Y,AControl.Top-AControl.FBoundsRealized.Top);
      AControl:=AControl.Parent;
    until AControl=nil;
  end else
    Result:=inherited GetClientOrigin;
  Assert(False, Format('Trace:[TWinControl.GetClientOrigin] %s --> (%d, %d)',  [Classname, Result.X, Result.Y]));
end;

{-------------------------------------------------------------------------------
  TWinControl GetClientRect
  Result:  TRect

  returns the client area. Starting at 0,0.
-------------------------------------------------------------------------------}
function TWinControl.GetClientRect: TRect;

  procedure StoreClientRect(NewClientRect: TRect);
  var
    ClientSizeChanged: boolean;
    i: Integer;
  begin
    if wcfClientRectNeedsUpdate in FWinControlFlags then begin
      ClientSizeChanged:=(FClientWidth<>NewClientRect.Right)
                      or (FClientHeight<>NewClientRect.Bottom);
      FClientWidth:=NewClientRect.Right;
      FClientHeight:=NewClientRect.Bottom;
      {$IFDEF VerboseSizeMsg}
      DebugLn('StoreClientRect ',Name,':',ClassName,' ',dbgs(FClientWidth),',',dbgs(FClientHeight));
      {$ENDIF}
      if ClientSizeChanged then begin
        for i:=0 to ControlCount-1 do
          Exclude(Controls[i].FControlFlags,cfLastAlignedBoundsValid);
      end;
      Exclude(FWinControlFlags,wcfClientRectNeedsUpdate);
    end;
  end;

var
  InterfaceWidth, InterfaceHeight: integer;
begin
  if not HandleAllocated then begin
    Result:=inherited GetClientRect;
    StoreClientRect(Result);
  end else if wcfClientRectNeedsUpdate in FWinControlFlags then begin
    // update clientrect from interface
    LCLIntf.GetClientRect(Handle, Result);
    // the LCL is not always in sync with the interface
    // -> adjust client rect based on LCL bounds
    // for example: if the Width in LCL differ from the Width of the Interface
    // object, then adjust the clientwidth accordingly
    LCLIntf.GetWindowSize(Handle, InterfaceWidth, InterfaceHeight);
    Result.Right:=Width-(InterfaceWidth-Result.Right);
    Result.Bottom:=Height-(InterfaceHeight-Result.Bottom);
    StoreClientRect(Result);

    {r:=inherited GetClientRect;
    if (r.Left<>Result.Left)
    or (r.Top<>Result.Top)
    or (r.Right<>Result.Right)
    or (r.Bottom<>Result.Bottom) then begin
      //DebugLn(' TWinControl.GetClientRect ',Name,':',ClassName,
      //  ' Old=',r.Left,',',r.Top,',',r.Right,',',r.Bottom,
      //  ' New=',Result.Left,',',Result.Top,',',Result.Right,',',Result.Bottom
      //  );
    end;}

  end else begin
    Result:=Rect(0,0,FClientWidth,FClientHeight);
  end;
end;

{-------------------------------------------------------------------------------
  TWinControl GetControlOrigin
  Result:  TPoint

  Returns the screen coordinate of the topleft coordinate 0,0 of the control
  area. (The topleft pixel of the control on the screen)
  Note that this value is the position as stored in the interface and is not
  always in sync with the LCL. When a control is moved, the LCL sets the bounds
  to the wanted position and sends a move message to the interface. It is up to
  the interface to handle moves instantly or queued.
-------------------------------------------------------------------------------}
function TWinControl.GetControlOrigin: TPoint;
var
  AControl: TWinControl;
  IntfBounds: TRect;
Begin
  if HandleAllocated then begin
    // get the interface idea where the client area is on the screen
    LCLIntf.GetWindowRect(Handle,IntfBounds);
    Result.X := IntfBounds.Left;
    Result.Y := IntfBounds.Top;
    // adjust the result by all bounds, that are not yet sent to the interface
    AControl:=Self;
    repeat
      inc(Result.X,AControl.Left-AControl.FBoundsRealized.Left);
      inc(Result.Y,AControl.Top-AControl.FBoundsRealized.Top);
      AControl:=AControl.Parent;
    until AControl=nil;
  end else
    Result:=inherited GetControlOrigin;
end;

{------------------------------------------------------------------------------
  function TWinControl.GetChildsRect(Scrolled: boolean): TRect;

  Returns the Client rectangle relative to the controls left, top.
  If Scrolled is true, the rectangle is moved by the current scrolling values
  (for an example see TScrollingWincontrol).
------------------------------------------------------------------------------}
function TWinControl.GetChildsRect(Scrolled: boolean): TRect;
var
  ScrolledOffset: TPoint;
begin
  if HandleAllocated then begin
    LCLIntf.GetClientBounds(Handle,Result);
    if Scrolled then begin
      ScrolledOffset:=GetClientScrollOffset;
      inc(Result.Left,ScrolledOffset.X);
      inc(Result.Top,ScrolledOffset.Y);
      inc(Result.Right,ScrolledOffset.X);
      inc(Result.Bottom,ScrolledOffset.Y);
    end;
  end else
    Result:=inherited GetChildsRect(Scrolled);
end;

{------------------------------------------------------------------------------}
{  TWinControl SetBorderStyle                                                  }
{------------------------------------------------------------------------------}
procedure TWinControl.SetBorderStyle(NewStyle: TBorderStyle);
begin
  FBorderStyle := NewStyle;
  if HandleAllocated then
    TWSWinControlClass(WidgetSetClass).SetBorderStyle(Self, NewStyle);
end;

{------------------------------------------------------------------------------}
{  TWinControl SetBorderWidth                                                  }
{------------------------------------------------------------------------------}
Procedure TWinControl.SetBorderWidth(value : TBorderWidth);
Begin
  //TODO: SETBORDERWIDTH - Not sure if anything more is needed here
  FBorderWidth := Value;
  Invalidate;
  InvalidatePreferredSize;
end;

Procedure TWinControl.SetZOrder(Topmost: Boolean);
const
  WindowPos: array[Boolean] of Word = (HWND_BOTTOM, HWND_TOP);
var
  i: integer;
begin
  if FParent <> nil then begin
    if TopMost then
      i := FParent.FWinControls.Count - 1
    else
      i := 0;
    if FParent.FControls <> nil then
      inc(i, FParent.FControls.Count);
    SetZOrderPosition(i);
  end
  else if HandleAllocated then begin
    SetWindowPos(Handle, WindowPos[TopMost], 0, 0, 0, 0,
      SWP_NOMOVE + SWP_NOSIZE);
  end;
end;

{------------------------------------------------------------------------------
       TControl SetTabOrder
------------------------------------------------------------------------------}
procedure TWinControl.SetTabOrder(NewTabOrder: TTabOrder);
begin
  if csLoading in ComponentState then
    FTabOrder := NewTabOrder
  else
    UpdateTabOrder(NewTabOrder);
end;

procedure TWinControl.SetTabStop(NewTabStop: Boolean);
begin
  if FTabStop = NewTabStop then
    exit;

  FTabStop := NewTabStop;
  UpdateTabOrder(FTabOrder);
end;

{------------------------------------------------------------------------------
  TControl UpdateTabOrder
------------------------------------------------------------------------------}
procedure TWinControl.UpdateTabOrder(NewTabOrder: TTabOrder);
var
  Count: Integer;
begin
  if FParent <> nil then
  begin
    FTabOrder := GetTabOrder;
    Count := ListCount(FParent.FTabList);
    if NewTabOrder < 0 then NewTabOrder := Count;
    if FTabOrder = -1 then Inc(Count);
    if NewTabOrder >= Count then NewTabOrder := Count - 1;
    if NewTabOrder <> FTabOrder then
    begin
      if FTabOrder <> - 1 then
        ListDelete(FParent.FTabList,FTabOrder);
      if NewTabOrder <> -1 then
      begin
        ListInsert(FParent.FTabList,NewTabOrder,Self);
        FTabOrder := NewTabOrder;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  procedure TWinControl.SendMoveSizeMessages(SizeChanged, PosChanged: boolean);
  
  Send Move and Size messages through the LCL message paths. This simulates the
  VCL behaviour and has no real effect.
-------------------------------------------------------------------------------}
procedure TWinControl.SendMoveSizeMessages(SizeChanged, PosChanged: boolean);
var
  SizeMsg : TLMSize;
  MoveMsg : TLMMove;
  //Flags: UINT;
begin
  if (not HandleAllocated)
  or ((not SizeChanged) and (not PosChanged)) then exit;

  Perform(LM_WindowposChanged, 0, 0);

  if SizeChanged then begin
    with SizeMsg do begin
      Msg:= LM_SIZE;
      SizeType:= 6; // force realign
      Width:= FWidth;
      Height:= FHeight;
      {$IFDEF CHECK_POSITION}
      if AnsiCompareText(ClassName,'TScrollBar')=0 then
      DebugLn(' [TControl.SendMoveSizeMessages] ',Name,':',ClassName,' SizeMsg Width=',Width,' Height=',Height);
      {$ENDIF}
    end;
    WindowProc(TLMessage(SizeMsg));
  end;
  
  if PosChanged then begin
    with MoveMsg do begin
      Msg:= LM_MOVE;
      MoveType:= 1;
      XPos:= FLeft;
      YPos:= FTop;
      {$IFDEF CHECK_POSITION}
      if AnsiCompareText(ClassName,'TScrollBar')=0 then
      DebugLn(' [TControl.SendMoveSizeMessages] ',Name,':',ClassName,' MoveMsg XPos=',XPos,' YPos=',YPos);
      {$ENDIF}
    end;
    WindowProc(TLMessage(MoveMsg));
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl UpdateShowing                                                   }
{------------------------------------------------------------------------------}
procedure TWinControl.UpdateShowing;
var
  bShow: Boolean;
  n: Integer;
  ok: boolean;
begin
  bShow := HandleObjectShouldBeVisible;

  if bShow
  then begin
    if not HandleAllocated then CreateHandle;
    if FWinControls <> nil
    then begin
      for n := 0 to FWinControls.Count - 1 do
        TWinControl(FWinControls[n]).UpdateShowing;
    end;
  end;

  if not HandleAllocated then Exit;

  //DebugLn('TWinControl.UpdateShowing A ',Name,':',ClassName,' FShowing=',FShowing,' bShow=',bShow);
  if FShowing = bShow then Exit;
  
  FShowing := bShow;
  ok := false;
  try
    Perform(CM_SHOWINGCHANGED, 0, 0);
    ok := true;
  finally
    if not ok then
      FShowing := not bShow;
  end;
end;

procedure TWinControl.Update;
begin
  if HandleAllocated then UpdateWindow(Handle);
end;

{-------------------------------------------------------------------------------
  procedure TWinControl.SetZOrderPosition(NewPosition: Integer);

-------------------------------------------------------------------------------}
procedure TWinControl.SetZOrderPosition(NewPosition: Integer);
var
  OldPosition, Count: Integer;
  Pos: HWND;
begin
  if FParent <> nil then
  begin
    if FParent.FControls <> nil then
      Dec(NewPosition, FParent.FControls.Count);
    OldPosition := FParent.FWinControls.IndexOf(Self);
    if OldPosition >= 0 then
    begin
      Count := FParent.FWinControls.Count;
      if NewPosition < 0 then NewPosition := 0;
      if NewPosition >= Count then NewPosition := Count - 1;
      if NewPosition = OldPosition then exit;
      FParent.FWinControls.Move(OldPosition,NewPosition);
    end else begin
      // MG: What if OldPosition<0 ?
      debugln('WARNING: TWinControl.SetZOrderPosition OldPosition<0');
    end;
    if HandleAllocated then
    begin
      // In LCL, position 0 is "at the back" and position
      // Count - 1 is "in front", so the order is reversed from win32 api POV.
      // In SetWindowPos, you should give the handle of the window
      // (hWndInsertAfter) of which you want to become just in front of your
      // window (hWnd).
      // Keep in mind, that FWinControls is already reordered.
      if NewPosition <= 0 then
        Pos := HWND_BOTTOM
      else if NewPosition >= FParent.FWinControls.Count - 1 then
        Pos := HWND_TOP
      else
        Pos := TWinControl(FParent.FWinControls[NewPosition + 1]).Handle;
      SetWindowPos(Handle, Pos, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
    end;
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl Focused                                                         }
{------------------------------------------------------------------------------}
Function TWinControl.Focused: Boolean;
Begin
  Result := CanTab and (HandleAllocated and (FindOwnerControl(GetFocus)=Self));
end;

function TWinControl.PerformTab(ForwardTab: boolean): boolean;

  Function GetHighestParent(TopControl : TControl) : TWinControl;
  begin
    Result := nil;
    If TopControl = nil then exit;
    If (TopControl.Parent=nil) then begin
      if TopControl is TWinControl then
        Result := TWinControl(TopControl)
    end else
      Result := GetHighestParent(TopControl.Parent);
  end;

var
  I : Integer;
  List : TList;
  FirstFocus, OldFocus, NewFocus : TWinControl;
  TopLevel : TWinControl;
begin
  NewFocus := nil;
  OldFocus := nil;
  TopLevel := GetHighestParent(Self);
  If TopLevel = nil then
    exit;
  try
    List := TList.Create;
    TopLevel.GetTabOrderList(List);
    FirstFocus := nil;
    For I := 0 to List.Count - 1 do
      If List[I] <> nil then begin
        If I = 0 then
          FirstFocus := TWinControl(List[I]);
        If TWinControl(List[I]).Focused then begin
          OldFocus := TWinControl(List[I]);
          Break;
        end;
      end;
  Finally
    List.Free;
  end;

  if OldFocus<>nil then
    NewFocus := TopLevel.FindNextControl(OldFocus,ForwardTab,True,False);
  //DebugLn('TControl.PerformTab A ',DbgSName(Self),' NewFocus=',DbgSName(NewFocus),' OldFocus=',DbgSName(OldFocus));

  If (NewFocus = nil) then NewFocus:=FirstFocus;
  If NewFocus = OldFocus then begin
    Result := True;
    exit;
  end;
  if NewFocus<>nil then begin
    NewFocus.SetFocus;
    Result := NewFocus.Focused;
  end else
    Result:=true;
end;

{------------------------------------------------------------------------------
  TWinControl FindChildControl
  
  Find next control (Tab control or Child control).
  Like VCL the CurControl parameter is ignored.
------------------------------------------------------------------------------}
procedure TWinControl.SelectNext(CurControl: TWinControl; GoForward,
  CheckTabStop: Boolean);
begin
  CurControl := FindNextControl(CurControl, GoForward,
                                CheckTabStop, not CheckTabStop);
  if CurControl <> nil then CurControl.SetFocus;
end;

{------------------------------------------------------------------------------
  TWinControl FindChildControl
------------------------------------------------------------------------------}
function TWinControl.FindChildControl(const ControlName: string): TControl;
var
  I: Integer;
begin
  Result := nil;
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      if CompareText(TWinControl(FWinControls[I]).Name, ControlName) = 0 then
      begin
        Result := TControl(FWinControls[I]);
        Exit;
      end;
end;

procedure TWinControl.FlipChildren(AllLevels: Boolean);
var
  i: Integer;
  FlipControls: TList;
  CurControl: TControl;
begin
  if ControlCount = 0 then exit;
  FlipControls := TList.Create;
  
  DisableAlign;
  try
    // Collect all controls with Align Right and Left
    for i := 0 to ControlCount - 1 do begin
      CurControl:=Controls[i];
      if CurControl.Align in [alLeft,alRight] then
        FlipControls.Add(CurControl);
    end;
    // flip the rest
    DoFlipChildren;
    // reverse Right and Left alignments
    while FlipControls.Count > 0 do begin
      CurControl:=TControl(FlipControls[FlipControls.Count-1]);
      if CurControl.Align=alLeft then
        CurControl.Align:=alRight
      else if CurControl.Align=alRight then
        CurControl.Align:=alLeft;
      FlipControls.Delete(FlipControls.Count - 1);
    end;
  finally
    FlipControls.Free;
    EnableAlign;
  end;
  
  // flip recursively
  if AllLevels then begin
    for i := 0 to ControlCount - 1 do begin
      CurControl:=Controls[i];
      if CurControl is TWinControl then
        TWinControl(CurControl).FlipChildren(true);
    end;
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl FindNextControl                                                 }
{------------------------------------------------------------------------------}
function TWinControl.FindNextControl(CurrentControl: TWinControl;
  GoForward, CheckTabStop, CheckParent: boolean): TWinControl;
var
  List : TList;
  Next : TWinControl;
  I, J : Longint;
begin
  Try
    Result := nil;
    List := TList.Create;
    GetTabOrderList(List);
    //for i:=0 to List.Count-1 do begin
    //  debugln('TWinControl.FindNextControl TabOrderList ',dbgs(i),' ',DbgSName(TObject(List[i])));
    //end;
    If List.Count > 0 then begin
      J := List.IndexOf(CurrentControl);
      if J<0 then exit;
      //DebugLn('TWinControl.FindNextControl A ',DbgSName(CurrentControl),' ',dbgs(J),
      //  ' GoForward='+dbgs(GoForward)+' CheckTabStop='+dbgs(CheckTabStop)+' CheckParent='+dbgs(CheckParent));
      I := J;
      Repeat
        If GoForward then begin
          Inc(I);
          if I>=List.Count then
            I:=0;
        end else begin
          Dec(I);
          if I<0 then
            I:=List.Count-1;
        end;
        if I=J then exit;

        Next := TWinControl(List[I]);
        //DebugLn('TWinControl.FindNextControl B ',Next.Name,' ',dbgs(I),
        //  +' ChckTabStop='+dbgs(CheckTabStop)+' TabStop='+dbgs(Next.TabStop)
        //  +' ChckParent='+dbgs(CheckParent)+' Parent=Self='+dbgs(Next.Parent = Self)
        //  +' Enabled='+dbgs(Next.Enabled)
        //  +' TestTab='+dbgs(((Not CheckTabStop) or Next.TabStop))
        //  +' TestPar='+dbgs(((not CheckParent) or (Next.Parent = Self)))
        //  +' TestEnVi='+dbgs(Next.Enabled and Next.IsVisible)
        //  );
        If (((not CheckTabStop) or Next.TabStop)
        and ((not CheckParent) or (Next.Parent = Self)))
        and (Next.Enabled and Next.IsVisible) then
          Result := Next;
      until (Result <> nil);
      //DebugLn('TWinControl.FindNextControl END ',DbgSName(Result),' I=',dbgs(I));
    end;
  finally
    List.Free;
  end;
end;

procedure TWinControl.FixupTabList;
var
  Count, I, J: Integer;
  List: TList;
  Control: TWinControl;
begin
  if FWinControls <> nil then
  begin
    List := TList.Create;
    try
      Count := FWinControls.Count;
      List.Count := Count;
      for I := 0 to Count - 1 do
      begin
        Control := TWinControl(FWinControls[I]);
        J := Control.FTabOrder;
        if (J >= 0) and (J < Count) then List[J] := Control;
      end;
      for I := 0 to Count - 1 do
      begin
        Control := TWinControl(List[I]);
        if Control <> nil then Control.UpdateTabOrder(TTabOrder(I));
      end;
    finally
      List.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl GetTabOrderList
------------------------------------------------------------------------------}
Procedure TWinControl.GetTabOrderList(List: TList);
var
  I : Integer;
  lWinControl : TWinControl;
begin
  If FTabList <> nil then
    For I := 0 to FTabList.Count - 1 do begin
      lWinControl := TWinControl(FTabList[I]);
      If lWinControl.CanTab and lWinControl.TabStop then
        List.Add(lWinControl);
      lWinControl.GetTabOrderList(List);
    end;
end;

{------------------------------------------------------------------------------}
{  TWinControl IsControlMouseMsg                                               }
{------------------------------------------------------------------------------}
function TWinControl.IsControlMouseMsg(var TheMessage: TLMMouse) : Boolean;
var
  Control : TControl;
  P : TPoint;
  ClientBounds: TRect;
begin
  if FindOwnerControl(GetCapture) = Self
  then begin
    Control := nil;
    if (CaptureControl <> nil)
    and (CaptureControl.Parent = Self)
    then Control := CaptureControl;
  end
  else begin
    Control := ControlAtPos(SmallPointToPoint(TheMessage.Pos),False,True,False);
  end;

  Result := False;
  if Control <> nil then
  begin
    // map mouse coordinates to control
    P.X := TheMessage.XPos - Control.Left;
    P.Y := TheMessage.YPos - Control.Top;
    if (Control is TWinControl) and TWinControl(Control).HandleAllocated then
    begin
      // map coordinates to client area of control
      LCLIntf.GetClientBounds(TWinControl(Control).Handle,ClientBounds);
      dec(P.X,ClientBounds.Left);
      dec(P.Y,ClientBounds.Top);
      {$IFDEF VerboseMouseBugfix}
      DebugLn('TWinControl.IsControlMouseMsg ',Name,' -> ',Control.Name,
      ' MsgPos=',TheMessage.Pos.X,',',TheMessage.Pos.Y,
      ' Control=',Control.Left,',',Control.Top,
      ' ClientBounds=',ClientBounds.Left,',',ClientBounds.Top,
      ' Scrolled=',GetClientScrollOffset.X,',',GetClientScrollOffset.Y,
      ' P=',P.X,',',P.Y
      );
      {$ENDIF}
    end;
    Control.Perform(TheMessage.Msg, WParam(TheMessage.Keys),
                    LParam(Integer(PointToSmallPoint(P))));
    Result := True;
  end;
end;

procedure TWinControl.FontChanged(Sender: TObject);
begin
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    TWSWinControlClass(WidgetSetClass).SetFont(Self, Font);
    Exclude(FWinControlFlags,wcfFontChanged);
    //NotifyControls(CM_ ...);
  end else
    Include(FWinControlFlags,wcfFontChanged);
end;

procedure TWinControl.SetColor(Value: TColor);
begin
  if Value=Color then exit;
  inherited SetColor(Value);
  if FBrush <> nil then
    FBrush.Color := Color;
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    TWSWinControlClass(WidgetSetClass).SetColor(Self);
    Exclude(FWinControlFlags,wcfColorChanged);
    NotifyControls(CM_PARENTCOLORCHANGED);
  end else
    Include(FWinControlFlags,wcfColorChanged);
end;

procedure TWinControl.PaintHandler(var TheMessage: TLMPaint);

  function ControlMustBeClipped(AControl: TControl): boolean;
  begin
    with AControl do
      Result:=(Visible
               or ((csDesigning in ComponentState)
                   and not (csNoDesignVisible in ControlStyle)))
              and (csOpaque in ControlStyle);
  end;

var
  I, Clip, SaveIndex: Integer;
  DC: HDC;
  PS: TPaintStruct; //defined in LCLIntf.pp
  ControlsNeedsClipping: boolean;
  CurControl: TControl;
begin
  //DebugLn('[TWinControl.PaintHandler] ',Name,':',ClassName,'  DC=',DbgS(TheMessage.DC,8));
  if (csDestroying in ComponentState) or (not HandleAllocated) then exit;

  {$IFDEF VerboseDsgnPaintMsg}
  if csDesigning in ComponentState then
    DebugLn('TWinControl.PaintHandler A ',Name,':',ClassName);
  {$ENDIF}

  Assert(False, Format('Trace:> [TWinControl.PaintHandler] %s --> Msg.DC: 0x%x', [ClassName, TheMessage.DC]));
  DC := TheMessage.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  try
    // check if child controls need clipping
    //DebugLn('[TWinControl.PaintHandler] ',DbgSName(Self),' B');
    ControlsNeedsClipping:=false;
    if FControls<>nil then
      for I := 0 to FControls.Count - 1 do
        if ControlMustBeClipped(TControl(FControls[I])) then begin
          ControlsNeedsClipping:=true;
          break;
        end;
    // exclude child controls and send new paint message
    if not ControlsNeedsClipping then begin
      //DebugLn('[TWinControl.PaintHandler] ',DbgSName(Self),' no clipping ...');
      PaintWindow(DC)
    end else
    begin
      SaveIndex := SaveDC(DC);
      Clip := SimpleRegion;
      for I := 0 to FControls.Count - 1 do begin
        CurControl:=TControl(FControls[I]);
        if ControlMustBeClipped(CurControl) then
          with CurControl do begin
            //DebugLn('TWinControl.PaintHandler Exclude Child ',DbgSName(Self),' Control=',DbgSName(CurControl),'(',dbgs(CurControl.BoundsRect),')');
            Clip := ExcludeClipRect(DC, Left, Top, Left + Width, Top + Height);
            if Clip = NullRegion then Break;
          end;
      end;
      //DebugLn('[TWinControl.PaintHandler] ',DbgSName(Self),' with clipping ...');
      if Clip <> NullRegion then
        PaintWindow(DC);
      RestoreDC(DC, SaveIndex);
    end;
    // paint controls
    //DebugLn('[TWinControl.PaintHandler] ',DbgSName(Self),' PaintControls ...');
    PaintControls(DC, nil);
  finally
    if TheMessage.DC = 0 then EndPaint(Handle, PS);
  end;
  Assert(False, Format('Trace:< [TWinControl.PaintHandler] %s', [ClassName]));
//DebugLn('[TWinControl.PaintHandler] END  ',Name,':',ClassName,'  DC=',DbgS(Message.DC,8));
end;

procedure TWinControl.PaintControls(DC: HDC; First: TControl);
var
  I, Count, SaveIndex: Integer;
//  FrameBrush: HBRUSH;
  TempControl : TControl;
  {off $Define VerboseControlDCOrigin}
  {$IFDEF VerboseControlDCOrigin}
  P: TPoint;
  {$ENDIF}
begin
//DebugLn('[TWinControl.PaintControls] ',Name,':',ClassName,'  DC=',DbgS(DC,8));
  if (csDestroying in ComponentState)
  or ((DC=0) and (not HandleAllocated)) then
    exit;

  {$IFDEF VerboseDsgnPaintMsg}
  if csDesigning in ComponentState then
    DebugLn('TWinControl.PaintControls A ',Name,':',ClassName);
  {$ENDIF}

  // Controls that are not TWinControl, have no handle of their own, and so
  // they are repainted as part of the parent:
  if FControls <> nil then
  begin
    I := 0;
    if First <> nil then
    begin
      I := FControls.IndexOf(First);
      if I < 0 then I := 0;
    end;
    Count := FControls.Count;
    while I < Count do
    begin
      TempControl := TControl(FControls.Items[I]);
      //DebugLn('TWinControl.PaintControls B Self=',Self.Name,':',Self.ClassName,' Control=',TempControl.Name,':',TempControl.ClassName,' ',TempControl.Left,',',TempControl.Top,',',TempControl.Width,',',TempControl.Height);
      with TempControl do
        if (Visible
        or ((csDesigning in ComponentState)
            and not (csNoDesignVisible in ControlStyle)))
        and RectVisible(DC, Rect(Left, Top, Left + Width, Top + Height)) then
        begin
          if csPaintCopy in Self.ControlState then
            Include(FControlState, csPaintCopy);
          SaveIndex := SaveDC(DC);
          MoveWindowOrg(DC, Left, Top);
          {$IFDEF VerboseControlDCOrigin}
          DebugLn('TWinControl.PaintControls B Self=',DbgSName(Self),' Control=',DbgSName(TempControl),' ',dbgs(Left),',',dbgs(Top),',',dbgs(Width),',',dbgs(Height));
          {$ENDIF}
          IntersectClipRect(DC, 0, 0, Width, Height);
          {$IFDEF VerboseControlDCOrigin}
          DebugLn('TWinControl.PaintControls C');
          P:=Point(-1,-1);
          GetWindowOrgEx(DC,@P);
          debugln('  DCOrigin=',dbgs(P));
          {$ENDIF}
          Perform(LM_PAINT, WParam(DC), 0);
          {$IFDEF VerboseControlDCOrigin}
          DebugLn('TWinControl.PaintControls D TempControl=',DbgSName(TempControl));
          {$ENDIF}
          RestoreDC(DC, SaveIndex);
          Exclude(FControlState, csPaintCopy);
        end;
      Inc(I);
    end;
  end;
  // draw specials for the wincontrols:
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      with TWinControl(FWinControls.Items[I]) do
        if FCtl3D and (csFramed in ControlStyle)
        and (Visible
        or ((csDesigning in ComponentState)
            and not (csNoDesignVisible in ControlStyle)))
        then begin
         //TODO: CreateSolidBrush and FrameRect
         {FrameBrush := CreateSolidBrush(clBtnShadow);
          FrameRect(DC, Rect(Left - 1, Top - 1, Left + Width, Top + Height),
            FrameBrush);
          DeleteObject(FrameBrush);
          FrameBrush := CreateSolidBrush(clBtnHighlight);
          FrameRect(DC, Rect(Left, Top, Left + Width + 1, Top + Height + 1),
            FrameBrush);
          DeleteObject(FrameBrush);
          }
        end;
//DebugLn('[TWinControl.PaintControls] END ',Name,':',ClassName,'  DC=',DbgS(DC,8));
end;

procedure TWinControl.PaintWindow(DC: HDC);
var
  Message: TLMessage;
begin
//DebugLn('[TWinControl.PaintWindow] ',Name,':',Classname,'  DC=',DbgS(DC,8));
  if (csDestroying in ComponentState)
  or ((DC=0) and (not HandleAllocated)) then
    exit;

  {$IFDEF VerboseDsgnPaintMsg}
  if csDesigning in ComponentState then
    DebugLn('TWinControl.PaintWindow A ',Name,':',ClassName);
  {$ENDIF}

  Message.Msg := LM_PAINT;
  Message.WParam := WParam(DC);
  Message.LParam := 0;
  Message.Result := 0;
  DefaultHandler(Message);
end;

procedure TWinControl.CreateBrush;
begin
  if FBrush<>nil then exit;
  FBrush:=TBrush.Create;
  FBrush.Color:=Color;
  // ToDo: ParentColor
end;

{------------------------------------------------------------------------------
  procedure TWinControl.EraseBackground;
------------------------------------------------------------------------------}
procedure TWinControl.EraseBackground(DC: HDC);
var
  ARect: TRect;
begin
  if DC=0 then exit;
  ARect:=Rect(0,0,Width,Height);
  FillRect(DC,ARect,Brush.Handle)
end;

{------------------------------------------------------------------------------
  function TWinControl.IntfUTF8KeyPress(var UTF8Key: TUTF8Char;
    RepeatCount: integer): boolean;

  Called by the interface after the navigation and specials keys are handled
  (e.g. after KeyDown).
------------------------------------------------------------------------------}
function TWinControl.IntfUTF8KeyPress(var UTF8Key: TUTF8Char;
  RepeatCount: integer): boolean;
begin
  Result:=(RepeatCount>0) and DoUTF8KeyPress(UTF8Key)
end;

{------------------------------------------------------------------------------
  TWinControl ControlAtPos
  Params: const Pos : TPoint
          AllowDisabled: Boolean
  Results: TControl

  Searches a child (not grand child) control, which client area contains Pos.
  Pos is relative to the ClientOrigin.
------------------------------------------------------------------------------}
Function TWinControl.ControlAtPos(const Pos : TPoint;
  AllowDisabled : Boolean): TControl;
Begin
  Result := ControlAtPos(Pos,AllowDisabled,false,true);
end;

{------------------------------------------------------------------------------
  TWinControl ControlAtPos
  Params: const Pos : TPoint
          AllowDisabled, AllowWinControls: Boolean
  Results: TControl

  Searches a child (not grand child) control, which client area contains Pos.
  Pos is relative to the ClientOrigin.
------------------------------------------------------------------------------}
Function TWinControl.ControlAtPos(const Pos : TPoint;
  AllowDisabled, AllowWinControls: Boolean): TControl;
begin
  Result := ControlAtPos(Pos,AllowDisabled,AllowWinControls,true);
end;

{------------------------------------------------------------------------------
  TWinControl ControlAtPos
  Params: const Pos : TPoint
          AllowDisabled, AllowWinControls: Boolean
  Results: TControl

  Searches a child (not grand child) control, which contains Pos.
  Pos is relative to the ClientOrigin.
  If AllowDisabled is true it will also search in disabled controls.
  If AllowWinControls is true it will also search in the child wincontrols.
  If OnlyClientAreas is true then only the client areas are compared.
------------------------------------------------------------------------------}
function TWinControl.ControlAtPos(const Pos: TPoint; AllowDisabled,
  AllowWinControls, OnlyClientAreas: Boolean): TControl;
var
  I: Integer;
  P: TPoint;
  LControl: TControl;
  ClientBounds: TRect;

  function GetControlAtPos(AControl: TControl): Boolean;
  var
    ControlPos: TPoint;
    ControlClientBounds: TRect;
  begin
    with AControl do
    begin
      // MG: Delphi checks for PtInRect(ClientRect,P). But the client area is
      //     not always at 0,0, so I guess this is a bug in the VCL.

      ControlPos:=Point(P.X-Left,P.Y-Top);
      Result:=(ControlPos.X>=0) and (ControlPos.Y>=0)
              and (ControlPos.X<Width) and (ControlPos.Y<Height);
      if Result and OnlyClientAreas then begin
        ControlClientBounds:=GetChildsRect(false);
        Result:=PtInRect(ControlClientBounds,ControlPos);
      end;

      Result:= Result
        and (
             (
              (csDesigning in ComponentState)
               and not (csNoDesignVisible in ControlStyle)
               // Here was a VCL bug: VCL checks if control is Visible,
               // which should be ignored at designtime
             )
             or
             (
              (not (csDesigning in ComponentState))
              and
              (Visible)
              and
              (Enabled or AllowDisabled)
              and
              (Perform(CM_HITTEST, 0,
                       LParam(Integer(PointToSmallPoint(ControlPos)))) <> 0)
             )
            );
      {$IFDEF VerboseMouseBugfix}
      DebugLn('GetControlAtPos ',Name,':',ClassName,
      ' Pos=',Pos.X,',',Pos.Y,
      ' P=',P.X,',',P.Y,
      ' ClientBounds=',ClientBounds.Left,',',ClientBounds.Top,',',ClientBounds.Right,',',ClientBounds.Bottom,
      ' OnlyCl=',OnlyClientAreas,
      ' Result=',Result);
      {$ENDIF}
      if Result then
        LControl := AControl;
    end;
  end;

var
  ScrolledOffset: TPoint;
begin
  // check if Pos in visible client area
  ClientBounds:=GetClientRect;
  if not PtInRect(ClientBounds,Pos) then begin
    Result:=nil;
    exit;
  end;

  // map Pos to logical client area
  ScrolledOffset:=GetClientScrollOffset;
  P:=Point(Pos.X+ScrolledOffset.X,Pos.Y+ScrolledOffset.Y);

  LControl := nil;
  // check wincontrols
  if AllowWinControls and (FWinControls <> nil) then
    for I := FWinControls.Count - 1 downto 0 do
      if GetControlAtPos(TControl(FWinControls[I])) then
        Break;
  // check controls
  if (FControls <> nil) and (LControl = nil) then
    for I := FControls.Count - 1 downto 0 do
      if GetControlAtPos(TControl(FControls[I])) then
        Break;
  Result := LControl;
end;

{-------------------------------------------------------------------------------
  function TWinControl.GetControlIndex(AControl: TControl): integer;


-------------------------------------------------------------------------------}
function TWinControl.GetControlIndex(AControl: TControl): integer;
begin
  if FControls<>nil then
    Result:=FControls.IndexOf(AControl)
  else
    Result:=-1;
  if (Result<0) and (FWinControls<>nil) then begin
    Result:=FWinControls.IndexOf(AControl);
    if (Result>=0) and (FControls<>nil) then
      inc(Result,FControls.Count);
  end;
end;

{-------------------------------------------------------------------------------
  function TWinControl.GetControlIndex(AControl: TControl): integer;


-------------------------------------------------------------------------------}
procedure TWinControl.SetControlIndex(AControl: TControl; NewIndex: integer);
begin
  if AControl=nil then exit;
  AControl.SetZOrderPosition(NewIndex);
end;

function TWinControl.ControlByName(const ControlName: string): TControl;
var
  i: Integer;
begin
  if FControls<>nil then
    for i:=0 to FControls.Count-1 do begin
      Result:=TControl(FControls[i]);
      if CompareText(Result.Name,ControlName)=0 then exit;
    end;
  if FWinControls<>nil then
    for i:=0 to FWinControls.Count-1 do begin
      Result:=TControl(FWinControls[i]);
      if CompareText(Result.Name,ControlName)=0 then exit;
    end;
  Result:=nil;
end;

{------------------------------------------------------------------------------}
{  TWinControl DestroyHandle                                                   }
{------------------------------------------------------------------------------}
procedure TWinControl.DestroyHandle;
var
  i: integer;
  AWinControl: TWinControl;
begin
  if not HandleAllocated then begin
    DebugLn('Warning: TWinControl.DestroyHandle ',Name,':',ClassName,' Handle not Allocated');
    //RaiseGDBException('');
  end;

  // First destroy all children handles
  if FWinControls <> nil then begin
    for i:= 0 to FWinControls.Count - 1 do begin
      //DebugLn('  i=',i);
      //DebugLn('  ',TWinControl(FWinControls[i]).Name,':',TWinControl(FWinControls[i]).ClassName);
      AWinControl:=TWinControl(FWinControls[i]);
      if AWinControl.HandleAllocated then
        AWinControl.DestroyHandle;
    end;
  end;
  DestroyWnd;
end;

{------------------------------------------------------------------------------
  TWinControl WndPRoc
------------------------------------------------------------------------------}
Procedure TWinControl.WndProc(Var Message: TLMessage);
Var
  Form: TCustomForm;
Begin
//  Assert(False, Format('Trace:[TWinControl.WndPRoc] %s(%s) --> Message = %d', [ClassName, Name, Message.Msg]));
  case Message.Msg of

  LM_SETFOCUS:
    begin
      Assert(False, Format('Trace:[TWinControl.WndPRoc] %s --> LM_SETFOCUS', [ClassName]));
      Form := GetParentForm(Self);
      {$IFDEF VerboseFocus}
      DebugLn('TWinControl.WndProc LM_SetFocus ',Name,':',ClassName);
      {$ENDIF}
      if (Form <> nil) and not Form.SetFocusedControl(Self) then Exit;
      Message.Result:=0;
    end;

  LM_KILLFOCUS:
    begin
      Assert(False, Format('Trace:[TWinControl.WndPRoc] %s --> _KILLFOCUS', [ClassName]));
      if csFocusing in ControlState then begin
        {$IFDEF VerboseFocus}
        DebugLn('TWinControl.WndProc LM_KillFocus during focusing ',Name,':',ClassName);
        {$ENDIF}
        Exit;
      end;
      Message.Result:=0;
    end;

  LM_NCHITTEST:
    begin
      inherited WndPRoc(Message);
      if (Message.Result = HTTRANSPARENT)
      and (ControlAtPos(ScreenToClient(SmallPointToPoint(TLMNCHitTest(Message).Pos)), False) <> nil)
      then  Message.Result := HTCLIENT;
      Exit;
    end;

  LM_MOUSEFIRST..LM_MOUSELAST,
  LM_LBUTTONTRIPLECLK,
  LM_LBUTTONQUADCLK,
  LM_MBUTTONTRIPLECLK,
  LM_MBUTTONQUADCLK,
  LM_RBUTTONTRIPLECLK,
  LM_RBUTTONQUADCLK:
     begin
       {$IFDEF VerboseMouseBugfix}
       DebugLn('TWinControl.WndPRoc A ',Name,':',ClassName);
       {$ENDIF}
       if IsControlMouseMSG(TLMMouse(Message)) then
         Exit;
       {$IFDEF VerboseMouseBugfix}
       DebugLn('TWinControl.WndPRoc B ',Name,':',ClassName);
       {$ENDIF}
     end;

  LM_KEYFIRST..LM_KEYLAST:
    if Dragging then Exit;

  LM_CANCELMODE:
    if (FindOwnerControl(GetCapture) = Self)
    and (CaptureControl <> nil)
    and (CaptureControl.Parent = Self)
    then CaptureControl.Perform(LM_CANCELMODE,0,0);

  else
  end;

  inherited WndProc(Message);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DoAddDockClient(Client: TControl; const ARect: TRect);
  
  Default method for addind a dock client. Just become the new parent.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  Client.Parent := Self;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DockOver(Source: TDragDockObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
    
  Called to check whether this control allows docking and where.
 ------------------------------------------------------------------------------}
procedure TWinControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  PositionDockRect(Source);
  DoDockOver(Source,X,Y,State,Accept);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DoDockOver(Source: TDragDockObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
 ------------------------------------------------------------------------------}
procedure TWinControl.DoDockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnDockOver) then
    FOnDockOver(Self,Source,X,Y,State,Accept);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DoRemoveDockClient(Client: TControl);

  Called to remove client from dock list.
  This method exists for descendent overrides.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoRemoveDockClient(Client: TControl);
begin
  // empty (this method exists for descendent overrides)
end;

{------------------------------------------------------------------------------
  function TWinControl.DoUnDock(NewTarget: TWinControl; Client: TControl
    ): Boolean;
 ------------------------------------------------------------------------------}
function TWinControl.DoUnDock(NewTarget: TWinControl; Client: TControl
  ): Boolean;
begin
  DebugLn('TWinControl.DoUnDock ',Name,' NewTarget=',DbgSName(NewTarget),' Client=',DbgSName(Client));
  if Assigned(FOnUnDock) then begin
    Result := True;
    FOnUnDock(Self,Client,NewTarget,Result);
    if not Result then exit;
  end;
  // ToDo Dock
  //Result := (Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
    MousePos: TPoint; var CanDock: Boolean);
 ------------------------------------------------------------------------------}
procedure TWinControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(Handle,InfluenceRect);
  // VCL inflates the docking rectangle. Do we need this too? Why?
  //InflateRect(InfluenceRect,?,?);
  if Assigned(FOnGetSiteInfo) then
    FOnGetSiteInfo(Self,Client,InfluenceRect,MousePos,CanDock);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.ReloadDockedControl(const AControlName: string;
    var AControl: TControl);
 ------------------------------------------------------------------------------}
procedure TWinControl.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  AControl := Owner.FindComponent(AControlName) as TControl;
end;

{------------------------------------------------------------------------------
  function TWinControl.CreateDockManager: TDockManager;
 ------------------------------------------------------------------------------}
function TWinControl.CreateDockManager: TDockManager;
begin
  if (FDockManager = nil) and DockSite and UseDockManager then
    Result := DefaultDockTreeClass.Create(Self)
  else
    Result := FDockManager;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.SetUseDockManager(const AValue: Boolean);
 ------------------------------------------------------------------------------}
procedure TWinControl.SetUseDockManager(const AValue: Boolean);
begin
  if FUseDockManager=AValue then exit;
  FUseDockManager:=AValue;
  if FUseDockManager and ([csDesigning,csDestroying]*ComponentState=[]) then
    FDockManager := CreateDockManager;
end;

{------------------------------------------------------------------------------
  Procedure TWinControl.MainWndProc(Var Message : TLMessage);

  The message handler of this wincontrol.
  Only needed by controls, which needs features not yet supported by the LCL.
 ------------------------------------------------------------------------------}
Procedure TWinControl.MainWndProc(Var Msg: TLMessage);
Begin
  Assert(False, Format('Trace:[TWinControl.MainWndPRoc] %s(%s) --> Message = %d', [ClassName, Name, Msg.Msg]));
end;

{------------------------------------------------------------------------------
  TWinControl SetFocus
------------------------------------------------------------------------------}
procedure TWinControl.SetFocus;
var
  Form : TCustomForm;
begin
  {$IFDEF VerboseFocus}
  DebugLn('[TWinControl.SetFocus] ',Name,':',ClassName,' Visible=',dbgs(Visible),' HandleAllocated=',dbgs(HandleAllocated));
  {$ENDIF}
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.FocusControl(Self)
  else if Visible and HandleAllocated then
    LCLIntf.SetFocus(Handle);
end;

{------------------------------------------------------------------------------
  TWinControl SetParentCtl3D
------------------------------------------------------------------------------}
Procedure TWinControl.SetParentCtl3D(value : Boolean);
Begin
  if FParentCtl3D <> Value then
  Begin
    FParentCtl3D := Value;
    if FParent <> nil then
    Begin
      //Sendmessage to do something?
    End;
  end;
end;

procedure TWinControl.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TCustomAction then
    TCustomAction(Dest).HelpContext:=HelpContext;
end;

procedure TWinControl.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender,CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    if (not CheckDefaults) or (Self.HelpContext = 0) then
      Self.HelpContext := HelpContext;
end;

function TWinControl.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TWinControlActionLink;
end;

{------------------------------------------------------------------------------
  TWinControl KeyDown
------------------------------------------------------------------------------}
Procedure TWinControl.KeyDown(var Key: Word; shift : TShiftState);
Begin
  if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

{------------------------------------------------------------------------------
  TWinControl KeyDownBeforeInterface
------------------------------------------------------------------------------}
procedure TWinControl.KeyDownBeforeInterface(var Key: Word; Shift: TShiftState
  );
begin
  KeyDown(Key,Shift);
end;

{------------------------------------------------------------------------------
  TWinControl KeyDownAfterInterface
------------------------------------------------------------------------------}
procedure TWinControl.KeyDownAfterInterface(var Key: Word; Shift: TShiftState);
begin

end;

{------------------------------------------------------------------------------
  TWinControl KeyPress
------------------------------------------------------------------------------}
Procedure TWinControl.KeyPress(var Key: char);
begin
  if Assigned(FOnKeyPress) then FOnKeyPress(Self, Key);
end;

{------------------------------------------------------------------------------
  TWinControl KeyPress
------------------------------------------------------------------------------}
procedure TWinControl.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  if Assigned(FOnUTF8KeyPress) then FOnUTF8KeyPress(Self, UTF8Key);
end;

{------------------------------------------------------------------------------
  TWinControl KeyUp
------------------------------------------------------------------------------}
Procedure TWinControl.KeyUp(var Key: Word; Shift : TShiftState);
begin
  if Assigned(FOnKeyUp) then FOnKeyUp(Self, Key, Shift);
end;

procedure TWinControl.KeyUpBeforeInterface(var Key: Word; Shift: TShiftState);
begin
  //debugln('TWinControl.KeyUpBeforeInterface ',DbgSName(Self));
  KeyUp(Key,Shift);
end;

procedure TWinControl.KeyUpAfterInterface(var Key: Word; Shift: TShiftState);
begin
  //debugln('TWinControl.KeyUpAfterInterface ',DbgSName(Self));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WantsKey
  Params: CharCode - the key to inspect whether it is wanted
  Returns: true if key is wanted before the interface handles it.

  Checks if control wants the passed key to handle before the interface.
 ------------------------------------------------------------------------------}
function TWinControl.WantsKeyBeforeInterface(Key: word; Shift: TShiftState
  ): boolean;
var
  lWantKeys: dword;
{ values for lWantKeys
           0 - if not wanted
           1 - if wanted, but is special (arrow)
           2 - if wanted, but is special (tab)
           4 - if wanted, but is special (all)
           8 - if wanted, is normal key
}
begin
  // For Delphi compatibility we send a LM_GETDLGCODE message to the control
  // asking if it wants to handle the key.
  // We don't define a default handler for LM_GETDLGCODE,
  // so the default return is 0.
  // Note: Contrary to Delphi/win32api, we don't know what keys are special,
  // different widgetsets may have different sets of special keys;
  lWantKeys := Perform(LM_GETDLGCODE, 0, 0);
  if (lWantKeys and DLGC_WANTALLKEYS) <> 0 then
  begin
    lWantKeys := DLGC_WANTALLKEYS;
  end else begin
    case Key of
    VK_TAB: 
      lWantKeys := lWantKeys and DLGC_WANTTAB;
    VK_UP, VK_LEFT, VK_DOWN, VK_RIGHT:
      lWantKeys := lWantKeys and DLGC_WANTARROWS;
    end;
  end;  
  Result := (lWantKeys<>0);
end;

{------------------------------------------------------------------------------
  TWinControl DoKeyDownBeforeInterface
  
  returns true if handled
------------------------------------------------------------------------------}
function TWinControl.DoKeyDownBeforeInterface(Var Message: TLMKey): Boolean;
var
  F: TCustomForm;
  ShiftState: TShiftState;
  AParent: TWinControl;
begin
  //debugln('TWinControl.DoKeyDown ',Name,':',ClassName,' ');
  Result:=true;

  with Message do
  begin
    if CharCode = VK_UNKNOWN then Exit;
    ShiftState := KeyDataToShiftState(KeyData);
    
    // let application handle the key
    if Application<>nil then
      Application.NotifyKeyDownBeforeHandler(Self, CharCode, ShiftState);
    if CharCode = VK_UNKNOWN then Exit;

    // let each parent form with keypreview handle the key
    AParent:=Parent;
    while (AParent<>nil) do begin
      if (AParent is TCustomForm) then begin
        F := TCustomForm(AParent);
        if  (F.KeyPreview)
        and (F.DoKeyDownBeforeInterface(Message)) then Exit;
      end;
      AParent:=AParent.Parent;
    end;

    if CharCode = VK_UNKNOWN then Exit;
    ShiftState := KeyDataToShiftState(KeyData);

    // let drag object handle the key
    if Dragging and (DragObject<>nil) then
    begin
      DragObject.KeyDown(CharCode, ShiftState);
      if CharCode = VK_UNKNOWN then Exit;
    end;

    // let user handle the key
    if not (csNoStdEvents in ControlStyle) then 
    begin
      KeyDownBeforeInterface(CharCode, ShiftState);
      if CharCode = VK_UNKNOWN then Exit;
    end;

  end;

  Result := False;
end;

function TWinControl.ChildKey(var Message: TLMKey): boolean;
begin
  if Assigned(Parent) then
    Result := Parent.ChildKey(Message)
  else
    Result := false;
end;

function TWinControl.DialogChar(var Message: TLMKey): boolean;
var
  I: integer;
begin
  // broadcast to children
  Result := false;
  for I := 0 to ControlCount - 1 do
  begin
    Result := Controls[I].DialogChar(Message);
    if Result then exit;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl DoRemainingKeyDown

  Returns True if key handled
------------------------------------------------------------------------------}
function TWinControl.DoRemainingKeyDown(var Message: TLMKeyDown): Boolean;
var
  ShiftState: TShiftState;
  AParent: TWinControl;
begin
  Result:=true;
  
  ShiftState := KeyDataToShiftState(Message.KeyData);

  // check popup menu
  if Assigned(FPopupMenu) then
  begin
    if FPopupMenu.IsShortCut(Message) then
      exit;
  end;

  // let each parent form handle shortcuts
  AParent:=Parent;
  while (AParent<>nil) do begin
    if (AParent is TCustomForm) then begin
      if TCustomForm(AParent).IsShortcut(Message) then
        exit;
    end;
    AParent:=AParent.Parent;
  end;

  // let application handle shortcut
  if Assigned(Application) and Application.IsShortcut(Message) then
    exit;

  // let parent(s) handle key from child key
  if Assigned(Parent) then
    if Parent.ChildKey(Message) then
      exit;

  // handle LCL special keys
  ControlKeyDown(Message.CharCode,ShiftState);
  if Message.CharCode=VK_UNKNOWN then exit;

  //DebugLn('TWinControl.WMKeyDown ',Name,':',ClassName);
  if not (csNoStdEvents in ControlStyle) then
  begin
    KeyDownAfterInterface(Message.CharCode, ShiftState);
    if Message.CharCode=VK_UNKNOWN then exit;
    // Note: Message.CharCode can now be different or even 0
  end;

  // let application handle the remaining key
  if Application<>nil then
    Application.NotifyKeyDownHandler(Self, Message.CharCode, ShiftState);
  if Message.CharCode = VK_UNKNOWN then Exit;

  Result := False;
end;

{------------------------------------------------------------------------------
  TWinControl DoKeyPress
  
  Returns True if key handled
------------------------------------------------------------------------------}
Function TWinControl.DoKeyPress(Var Message : TLMKey): Boolean;
var
  F: TCustomForm;
  C: char;
  AParent: TWinControl;
begin
  Result := True;

  // let each parent form with keypreview handle the key
  AParent:=Parent;
  while (AParent<>nil) do begin
    if (AParent is TCustomForm) then begin
      F := TCustomForm(AParent);
      if  (F.KeyPreview)
      and (F.DoKeyPress(Message)) then Exit;
    end;
    AParent:=AParent.Parent;
  end;

  if not (csNoStdEvents in ControlStyle)
  then with Message do
  begin
    C := char(CharCode);
    KeyPress(C);
    CharCode := Ord(C);
    if Char(CharCode) = #0 then Exit;
  end;

  Result := False;
End;

{------------------------------------------------------------------------------
  TWinControl DoRemainingKeyPress
  
  Returns True if key handled
------------------------------------------------------------------------------}
function TWinControl.DoRemainingKeyPress(var Message : TLMKey): Boolean;
var
  ParentForm: TCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if ParentForm <> nil then
  begin
    Result := ParentForm.DialogChar(Message);
    if Result then exit;
  end;
end;

function TWinControl.DoRemainingKeyUp(var Message: TLMKeyDown): Boolean;
var
  ShiftState: TShiftState;
begin
  //debugln('TWinControl.DoRemainingKeyUp ',DbgSName(Self));
  Result:=true;

  ShiftState := KeyDataToShiftState(Message.KeyData);

  // handle LCL special keys
  ControlKeyUp(Message.CharCode,ShiftState);
  if Message.CharCode=VK_UNKNOWN then exit;

  if not (csNoStdEvents in ControlStyle) then
  begin
    KeyUpAfterInterface(Message.CharCode, ShiftState);
    if Message.CharCode=VK_UNKNOWN then exit;
    // Note: Message.CharCode can now be different or even 0
  end;
end;

{------------------------------------------------------------------------------
  TWinControl DoUTF8KeyPress

  Returns True if key handled
------------------------------------------------------------------------------}
function TWinControl.DoUTF8KeyPress(var UTF8Key: TUTF8Char): boolean;
var
  AParent: TWinControl;
  F: TCustomForm;
begin
  Result:=true;
  
  // let each parent form with keypreview handle the key
  AParent:=Parent;
  while (AParent<>nil) do begin
    if (AParent is TCustomForm) then begin
      F := TCustomForm(AParent);
      if  (F.KeyPreview)
      and (F.DoUTF8KeyPress(UTF8Key)) then Exit;
    end;
    AParent:=AParent.Parent;
  end;
  
  if not (csNoStdEvents in ControlStyle) then begin
    UTF8KeyPress(UTF8Key);
    if UTF8Key='' then exit;
  end;
  
  Result := False;
end;

{------------------------------------------------------------------------------
  TWinControl DoKeyUpBeforeInterface

  Returns True if key handled
------------------------------------------------------------------------------}
Function TWinControl.DoKeyUpBeforeInterface(Var Message : TLMKey): Boolean;
var
  F: TCustomForm;
  ShiftState: TShiftState;
  AParent: TWinControl;
begin
  Result := True;

  // let each parent form with keypreview handle the key
  AParent:=Parent;
  while (AParent<>nil) do begin
    if (AParent is TCustomForm) then begin
      F := TCustomForm(AParent);
      if  (F.KeyPreview)
      and (F.DoKeyUpBeforeInterface(Message)) then Exit;
    end;
    AParent:=AParent.Parent;
  end;

  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);

    if Dragging and (DragObject<>nil) then
      DragObject.KeyUp(CharCode, ShiftState);
    
    if not (csNoStdEvents in ControlStyle)
    then begin
      KeyUp(CharCode, ShiftState);
      if CharCode = 0 then Exit;
    end;

    // TODO
    //if (CharCode = VK_APPS) and not (ssAlt in ShiftState) then
    //  CheckMenuPopup(SmallPoint(0, 0));
  end;
  Result := False;
end;

{------------------------------------------------------------------------------
  TWinControl ControlKeyDown
------------------------------------------------------------------------------}
procedure TWinControl.ControlKeyDown(var Key: Word; Shift: TShiftState);
begin
  Application.ControlKeyDown(Self,Key,Shift);
end;

procedure TWinControl.ControlKeyUp(var Key: Word; Shift: TShiftState);
begin
  //debugln('TWinControl.ControlKeyUp ',DbgSName(Self));
  Application.ControlKeyUp(Self,Key,Shift);
end;

{------------------------------------------------------------------------------
  TWinControl CreateParams
------------------------------------------------------------------------------}
procedure TWinControl.CreateParams(var Params : TCreateParams);
begin
  FillChar(Params, SizeOf(Params),0);
  with Params do
  begin
    Caption := PChar(FCaption);
    Style := WS_CHILD or WS_CLIPSIBLINGS;
    if (Parent <> nil) then WndParent := Parent.Handle;
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl Invalidate                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.Invalidate;
Begin
  if HandleAllocated and ([csDestroying,csLoading]*ComponentState=[]) then
    TWSWinControlClass(WidgetSetClass).Invalidate(Self);
end;

{------------------------------------------------------------------------------}
{  TWinControl Repaint                                                         }
{------------------------------------------------------------------------------}
Procedure TWinControl.Repaint;
Begin
  if (not HandleAllocated) or (csDestroying in ComponentState) then exit;
  {$IFDEF VerboseDsgnPaintMsg}
  if csDesigning in ComponentState then
    DebugLn('TWinControl.Repaint A ',Name,':',ClassName);
  {$ENDIF}
  { TODO: check what to do with LM_PAINT message, neither gtk nor win32
    interface responded to it }
  //CNSendMessage(LM_PAINT, Self, nil);
  // Invalidate;
  // Update;
end;

{------------------------------------------------------------------------------}
{  TWinControl DoMouseWheel  "Event Handler"                                     }
{------------------------------------------------------------------------------}
function TWinControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;

  if Assigned(FOnMouseWheel)
  then FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);

  if not Result
  then begin
    if WheelDelta < 0
    then Result := DoMouseWheelDown(Shift, MousePos)
    else Result := DoMouseWheelUp(Shift, MousePos);
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl DoMouseWheelDown  "Event Handler"                                     }
{------------------------------------------------------------------------------}
function TWinControl.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

{------------------------------------------------------------------------------}
{  TWinControl DoMouseWheelUp  "Event Handler"                                     }
{------------------------------------------------------------------------------}
function TWinControl.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;

{------------------------------------------------------------------------------}
{  TWinControl Insert                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.Insert(AControl : TControl);
begin
  Insert(AControl,ControlCount);
End;

{------------------------------------------------------------------------------
  procedure TWinControl.Insert(AControl: TControl; Index: integer);
------------------------------------------------------------------------------}
procedure TWinControl.Insert(AControl: TControl; Index: integer);
begin
  if AControl = nil then exit;
  
  if AControl = Self then
    raise EInvalidOperation.Create(rsAControlCanNotHaveItselfAsParent);

  if AControl is TWinControl then
  begin
    if (FControls<>nil) then dec(Index,FControls.Count);
    if (FWinControls<>nil) and (Index<FWinControls.Count) then
      FWinControls.Insert(Index,AControl)
    else
      ListAdd(FWinControls, AControl);
    if TWinControl(AControl).TabStop or (csAcceptsControls in AControl.ControlStyle) then
      ListAdd(FTabList, AControl);

    if (csDesigning in ComponentState) and (not (csLoading in ComponentState))
    and AControl.CanTab then
      TWinControl(AControl).TabStop := true;
  end else begin
    if (FControls<>nil) and (Index<FControls.Count) then
      FControls.Insert(Index,AControl)
    else
      ListAdd(FControls, AControl);
  end;

  AControl.FParent := Self;
end;

{------------------------------------------------------------------------------
  TWinControl ReAlign

  Realign all childs
------------------------------------------------------------------------------}
procedure TWinControl.ReAlign;
begin
  if ([csLoading,csDestroying]*ComponentState<>[]) or (not HandleAllocated) then
  begin
    Include(FWinControlFlags,wcfReAlignNeeded);
    exit;
  end;
  //DebugLn('TWinControl.ReAlign ',Name,':',ClassName);
  AlignControl(nil);
  Exclude(FWinControlFlags,wcfReAlignNeeded);
end;

{------------------------------------------------------------------------------}
{  TWinControl Remove                                                          }
{------------------------------------------------------------------------------}
Procedure TWinControl.Remove(AControl : TControl);
begin
  if AControl <> nil then
  begin
    Assert(False, Format('trace:[TWinControl.Remove] %s(%S) --> Remove: %s(%s)', [ClassName, Name, AControl.ClassName, AControl.Name]));
    if AControl is TWinControl then
    begin
      ListRemove(FTabList, AControl);
      ListRemove(FWInControls, ACOntrol);
    end else
      Listremove(FControls, AControl);
    AControl.FParent := Nil;
  end;
End;

{------------------------------------------------------------------------------}
{  TWinControl RemoveFocus                                                      }
{------------------------------------------------------------------------------}
Procedure TWinControl.RemoveFocus(Removing : Boolean);
  //TODO: FINISH TWINCONTROL.REMOVEFOCUS
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.DefocusControl(Self, Removing);
end;

{------------------------------------------------------------------------------}
{  TWinControl UpdateControlState                                                      }
{------------------------------------------------------------------------------}
procedure TWinControl.UpdateControlState;
var AWinControl: TWinControl;
begin
  AWinControl:= Self;
  { If any of the parent is not visible, exit }
  while AWinControl.Parent <> nil do
  begin
    AWinControl:= AWinControl.Parent;
    if (not AWinControl.Showing) or (not AWinControl.HandleAllocated) then Exit;
  end;

  if ((AWinControl is TCustomForm) and (AWinControl.Parent=nil))
  or (AWinControl.FParentWindow <> 0) then
    UpdateShowing;
end;

{------------------------------------------------------------------------------}
{  TWinControl InsertControl                                                   }
{------------------------------------------------------------------------------}
Procedure TWinControl.InsertControl(AControl: TControl);
Begin
  InsertControl(AControl,ControlCount);
End;

procedure TWinControl.InsertControl(AControl: TControl; Index: integer);
begin
  AControl.ValidateContainer(Self);
  Perform(CM_CONTROLLISTCHANGE, WParam(AControl), LParam(True));
  Insert(AControl,Index);
  if not (csReadingState in AControl.ControlState) then
  begin
    AControl.Perform(CM_PARENTCOLORCHANGED, 0, 0);
    AControl.Perform(CM_PARENTFONTCHANGED, 0, 0);
    AControl.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
    AControl.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
    if AControl is TWinControl then
    begin
      AControl.Perform(CM_PARENTCTL3DCHANGED, 0, 0);
      UpdateControlState;
    end else
      if HandleAllocated then AControl.Invalidate;
    //DebugLn('TWinControl.InsertControl ',Name,':',ClassName);
  end;
  AControl.RequestAlign;
  Perform(CM_CONTROLCHANGE, WParam(AControl), LParam(True));
end;

{------------------------------------------------------------------------------
  TWinControl removeControl
------------------------------------------------------------------------------}
Procedure TWinControl.RemoveControl(AControl: TControl);
var
  AWinControl: TWinControl;
Begin
  Perform(CM_CONTROLCHANGE, WParam(AControl), LParam(False));
  if AControl is TWinControl then begin
    AWinControl:=TWinControl(AControl);
    AWinControl.RemoveFocus(True);
    if AWinControl.HandleAllocated then AWinControl.DestroyHandle;
  end else
    if HandleAllocated then
      AControl.InvalidateControl(AControl.Visible, False, True);
  Remove(AControl);
//  Perform(CM_CONTROLLISTCHANGE, WParam(AControl), LParam(False));
  Realign;
End;

{------------------------------------------------------------------------------
  TWinControl AlignControl
------------------------------------------------------------------------------}
procedure TWinControl.AlignControl(AControl: TControl);
var
  ARect: TRect;
  i: Integer;
  ChildControl: TControl;
begin
  //if csDesigning in ComponentState then begin
  //  DbgOut('TWinControl.AlignControl ',Name,':',ClassName);
  //  if AControl<>nil then DebugLn(' AControl=',AControl.Name,':',AControl.ClassName) else DebugLn(' AControl=nil');;
  //end;
  if csDestroying in ComponentState then exit;

  if FAlignLevel <> 0 then begin
    Include(FControlState, csAlignmentNeeded);
    exit;
  end;

  // check if all childs have finished loading
  for i:=0 to ControlCount-1 do begin
    ChildControl:=Controls[i];
    if csLoading in ChildControl.ComponentState then begin
      // child is loading
      // -> mark the child, that itself and its brothers needs realigning
      // (it will do this, when it has finished loading)
      Include(ChildControl.FControlFlags,cfRequestAlignNeeded);
      exit;
    end;
  end;

  DisableAlign;
  try
    ARect:= GetClientRect;
    AlignControls(AControl, ARect);
  finally
    Exclude(FControlState, csAlignmentNeeded);
    EnableAlign;
  end;
End;

{------------------------------------------------------------------------------
  Method: TWinControl.ContainsControl
  Params:  Control: the control to be checked
  Returns: Self is a (super)parent of Control

  Checks if Control is a child of Self
 ------------------------------------------------------------------------------}
function TWinControl.ContainsControl(Control: TControl): Boolean;
begin
  while (Control <> nil) and (Control <> Self) do Control := Control.Parent;
  Result := Control = Self;
end;

{------------------------------------------------------------------------------
  TWinControl GetBorderStyle
------------------------------------------------------------------------------}
function TWinControl.GetBorderStyle: TBorderStyle;
begin
  Result := TBorderStyle(FBorderStyle);
end;

{------------------------------------------------------------------------------
  TWinControl GetBrush
------------------------------------------------------------------------------}
function TWinControl.GetBrush: TBrush;
begin
  if FBrush=nil then CreateBrush;
  Result:=FBrush;
end;

{------------------------------------------------------------------------------}
{  TWinControl GetControl                                                      }
{------------------------------------------------------------------------------}
function TWinControl.GetControl(const Index: Integer): TControl;
var
  N: Integer;
begin
  if FControls <> nil then
    N := FControls.Count
  else
    N := 0;
  if Index < N then
    Result := TControl(FControls[Index])
  else
    Result := TControl(FWinControls[Index - N]);
end;

{------------------------------------------------------------------------------}
{  TWinControl GetControlCount                                                 }
{------------------------------------------------------------------------------}
function TWinControl.GetControlCount: Integer;
begin
  Result := 0;
  if FControls <> nil then Inc(Result, FControls.Count);
  if FWinControls <> nil then Inc(Result, FWinControls.Count);
end;

function TWinControl.GetDockClientCount: Integer;
begin
  if FDockClients<>nil then
    Result := FDockClients.Count
  else
    Result := 0;
end;

function TWinControl.GetDockClients(Index: Integer): TControl;
begin
  if FDockClients<>nil then
    Result := TControl(FDockClients[Index])
  else
    Result := nil;
end;

{------------------------------------------------------------------------------}
{  TWinControl GetHandle                                                       }
{------------------------------------------------------------------------------}
function TWinControl.GetHandle: HWND;
begin
  if not HandleAllocated then
    //Assert(False, Format('Trace:[TWinControl.GetHandle] %s(%s)', [ClassNAme, Name]))
    ;
  HandleNeeded;
  Result := FHandle;
end;

{------------------------------------------------------------------------------
  TWinControl SetHandle
  Params:  NewHandle
  Returns: Nothing
-------------------------------------------------------------------------------}
procedure TWincontrol.SetHandle(NewHandle: HWND);
begin
  //if (NewHandle=0) and (AnsiCompareText(ClassName,'TPAGE')=0) then
  //  RaiseGDBException('TWincontrol.SetHandle');
  FHandle:=NewHandle;
  InvalidatePreferredSize;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TWinControl.Create(TheOwner : TComponent);
begin
  // do not set borderstyle, as tcustomform needs to set it before calling
  // inherited, to have it set before handle is created via streaming
  // use property that bsNone is zero
  //FBorderStyle := bsNone;
  inherited Create(TheOwner);
  FCompStyle := csWinControl;
  FChildSizing:=TControlChildSizing.Create(Self);
  FChildSizing.OnChange:=@DoChildSizingChange;
  FBrush := nil; // Brush will be created on demand. Only few controls need it.
  FParentCtl3D:=true;
  FTabOrder := -1;
  FTabStop := False;
end;

{------------------------------------------------------------------------------}
{  TWinControl CreateParented                                                  }
{------------------------------------------------------------------------------}
constructor TWinControl.CreateParented(ParentWindow: hwnd);
begin
  FParentWindow := ParentWindow;
  inherited Create(nil);
end;

{------------------------------------------------------------------------------
  TWinControl CreateParentedControl
------------------------------------------------------------------------------}
class function TWinControl.CreateParentedControl(ParentWindow: hwnd): TWinControl;
begin
  // ToDo
  Result:=nil;
end;

{------------------------------------------------------------------------------
  TWinControl Hide
------------------------------------------------------------------------------}
procedure TWinControl.Hide;
begin
  Visible := False;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TWinControl.Destroy;
var
  n: Integer;
  Control: TControl;
begin
  //DebugLn('[TWinControl.Destroy] A  ',Name,':',ClassName);
  // prevent parent to try to focus a to be destroyed control
  if Parent <> nil then
    RemoveFocus(true);
  if HandleAllocated then
    DestroyHandle;
  //DebugLn('[TWinControl.Destroy] B  ',Name,':',ClassName);

  //for n:=0 to ComponentCount-1 do
  //  DebugLn('  n=',n,' ',Components[n].ClassName);

  n := ControlCount;

  while n > 0 do
  begin
    Control := Controls[n - 1];
    //DebugLn('[TWinControl.Destroy] C  ',Name,':',ClassName,' ',Control.Name,':',Control.ClassName);
    Remove(Control);
    // don't free the control just set parent to nil
    // controls are freed by the owner
    //Control.Free;
    Control.Parent := nil;
    n := ControlCount;
  end;

  FreeThenNil(FBrush);
  FreeThenNil(FChildSizing);
  FreeThenNil(FDockClients);
  FreeThenNil(FTabList);
  //DebugLn('[TWinControl.Destroy] D  ',Name,':',ClassName);
  inherited Destroy;
  //DebugLn('[TWinControl.Destroy] END  ',Name,':',ClassName);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.DoEnter
  Params: none
  Returns: Nothing

  Call user's callback for OnEnter.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoEnter;
begin
  if Assigned(FOnEnter) then FOnEnter(Self);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.DoExit
  Params: none
  Returns: Nothing

  Call user's callback for OnExit.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoExit;
begin
  if Assigned(FOnExit) then FOnExit(Self);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DoFlipChildren;
  
  Flip children horizontally. That means mirroring the left position.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoFlipChildren;
var
  i: Integer;
  CurControl: TControl;
  AWidth: Integer;
begin
  AWidth:=ClientWidth;
  for i:=0 to ControlCount-1 do begin
    CurControl:=Controls[i];
    CurControl.Left:=AWidth-CurControl.Left-CurControl.Width;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CMEnabledChanged
  Params: Message
  Returns: Nothing

  Called when enabled is changed. Takes action to enable control
 ------------------------------------------------------------------------------}
procedure TWinControl.CMEnabledChanged(var Message: TLMessage);
begin
  if not Enabled and (Parent <> nil)
  then RemoveFocus(False);

  if HandleAllocated and not (csDesigning in ComponentState) then begin
    //if (not Enabled) then debugln('TWinControl.CMEnabledChanged disable ',Name,':',CLassName);
    EnableWindow(Handle, Enabled);
  end;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CMShowHintChanged
  Params: Message
  Returns: Nothing

  Called when showhint is changed. Notifies children
 ------------------------------------------------------------------------------}
procedure TWinControl.CMShowHintChanged(var Message: TLMessage);
begin
  NotifyControls(CM_PARENTSHOWHINTCHANGED);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMSetFocus
  Params: Message
  Returns: Nothing

  SetFocus event handler
 ------------------------------------------------------------------------------}
Procedure TWinControl.WMSetFocus(var Message: TLMSetFocus);
Begin
  //DebugLn('TWinControl.WMSetFocus A ',Name,':',ClassName);
  Assert(False, Format('Trace: %s', [ClassName]));
  if [csLoading,csDestroying,csDesigning]*ComponentState=[] then begin
    DoEnter;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.LMKillFocus
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMKillFocus(var Message: TLMKillFocus);
begin
  //DebugLn('TWinControl.WMKillFocus A ',Name,':',ClassName);
  Assert(False, Format('Trace: %s', [ClassName]));
  if [csLoading,csDestroying,csDesigning]*ComponentState=[] then begin
    EditingDone;
    DoExit;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMPaint
  Params:   Msg: The paint message
  Returns:  nothing

  Paint event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMPaint(var Msg: TLMPaint);
var
  DC,MemDC: HDC;
{$ifdef BUFFERED_WMPAINT}
  MemBitmap, OldBitmap : HBITMAP;
  MemWidth: Integer;
  MemHeight: Integer;
{$ENDIF}
  PS : TPaintStruct;
  ClientBoundRect: TRect;
begin
  //DebugLn('[TWinControl.WMPaint] ',Name,':',ClassName,'  ',DbgS(Msg.DC,8));
  if ([csDestroying,csLoading]*ComponentState<>[]) or (not HandleAllocated) then
    exit;

  {$IFDEF VerboseDsgnPaintMsg}
  if csDesigning in ComponentState then
    DebugLn('TWinControl.WMPaint A ',Name,':',ClassName);
  {$ENDIF}

  Assert(False, Format('Trace:> [TWinControl.WMPaint] %s Msg.DC: 0x%x', [ClassName, Msg.DC]));
  if (Msg.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      begin
        DefaultHandler(Msg);
      end
    else
      PaintHandler(Msg);
  end
  else begin
    // NOTE: not every interface uses this part
    //DebugLn('TWinControl.WMPaint Painting doublebuffered ',Name,':',classname);
{$ifdef BUFFERED_WMPAINT}
    DC := GetDC(0);
    MemWidth:=Width;
    MemHeight:=Height;
    MemBitmap := CreateCompatibleBitmap(DC, MemWidth, MemHeight);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
{$ENDIF}
    try
      // Fetch a DC of the whole Handle (including client area)
      DC := BeginPaint(Handle, PS);
      if DC=0 then exit;
{$ifNdef BUFFERED_WMPAINT}
      MemDC := DC;
{$ENDIF}
      // erase background
      Include(FWinControlFlags,wcfEraseBackground);
      Perform(LM_ERASEBKGND, WParam(MemDC), 0);
      Exclude(FWinControlFlags,wcfEraseBackground);
      // create a paint message to paint the child controls.
      // WMPaint expects the DC origin to be equal to the client origin of its
      // parent
      // -> Move the DC Origin to the client origin
      if not GetClientBounds(Handle,ClientBoundRect) then exit;
      MoveWindowOrgEx(MemDC,ClientBoundRect.Left,ClientBoundRect.Top);
      // handle the paint message
      Msg.DC := MemDC;
      Perform(LM_PAINT, WParam(MemDC), 0);
      Msg.DC := 0;
      // restore the DC origin
      MoveWindowOrgEx(MemDC,-ClientBoundRect.Left,-ClientBoundRect.Top);
{$ifdef BUFFERED_WMPAINT}
      BitBlt(DC, 0,0, MemWidth, MemHeight, MemDC, 0, 0, SRCCOPY);
{$ENDIF}
      EndPaint(Handle, PS);
    finally
      Exclude(FWinControlFlags,wcfEraseBackground);
{$ifdef BUFFERED_WMPAINT}
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
{$ENDIF}
    end;
  end;
  Assert(False, Format('Trace:< [TWinControl.WMPaint] %s', [ClassName]));
//DebugLn('[TWinControl.WMPaint] END ',Name,':',ClassName);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMDestroy
  Params:   Msg: The destroy message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMDestroy(var Message: TLMDestroy);
begin
  Assert(False, Format('Trace: [TWinControl.LMDestroy] %s', [ClassName]));
  //DebugLn('TWinControl.WMDestroy ',Name,':',ClassName);
  // Our widget/window doesn't exist anymore
  Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMMove
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMMove(var Message: TLMMove);
var
  NewWidth, NewHeight: integer;
begin
  {$IFDEF VerboseSizeMsg}
  DebugLn('TWinControl.WMMove A ',Name,':',ClassName,' Message=',dbgs(Message.XPos),',',dbgs(Message.YPos),
    ' BoundsRealized='+dbgs(FBoundsRealized.Left)+','+dbgs(FBoundsRealized.Top),
      ','+dbgs(FBoundsRealized.Right-FBoundsRealized.Left),
      'x'+dbgs(FBoundsRealized.Bottom-FBoundsRealized.Top));
  {$ENDIF}
  NewWidth:=Width;
  NewHeight:=Height;
  if Message.MoveType=Move_SourceIsInterface then begin
    // interface widget has moved
    // -> update size and realized bounds
    NewWidth:=FBoundsRealized.Right-FBoundsRealized.Left;
    NewHeight:=FBoundsRealized.Bottom-FBoundsRealized.Top;
    if HandleAllocated then
      GetWindowSize(Handle,NewWidth,NewHeight);
    FBoundsRealized:=Bounds(Message.XPos,Message.YPos,NewWidth,NewHeight);
  end;
  SetBoundsKeepBase(Message.XPos,Message.YPos,NewWidth,NewHeight,Parent<>nil);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMSize
  Params:   Message: TLMSize
  Returns:  nothing

  Event handler for size messages. This is called, whenever width, height,
  clientwidth or clientheight have changed.
  If the source of the message is the interface, the new size is stored
  in FBoundsRealized to avoid sending a size message back to the interface.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMSize(var Message: TLMSize);
var
  NewLeft, NewTop: integer;
  NewBoundsRealized: TRect;
begin
  {$IFDEF VerboseSizeMsg}
  DebugLn('TWinControl.WMSize A ',Name,':',ClassName,' Message=',dbgs(Message.Width),',',dbgs(Message.Height),
    ' BoundsRealized=',dbgs(FBoundsRealized));
  {$ENDIF}
  NewLeft:=Left;
  NewTop:=Top;
  if (Message.SizeType and Size_SourceIsInterface)>0 then begin
    // interface widget has resized
    // -> update position and realized bounds
    NewLeft:=FBoundsRealized.Left;
    NewTop:=FBoundsRealized.Top;
    if HandleAllocated then
      GetWindowRelativePosition(Handle,NewLeft,NewTop);
    //DebugLn('TWinControl.WMSize B ',Name,':',ClassName,' ',NewLeft,',',NewTop);
    NewBoundsRealized:=Bounds(NewLeft,NewTop,Message.Width,Message.Height);
    If CompareRect(@NewBoundsRealized,@FBoundsRealized) then exit;
    FBoundsRealized:=NewBoundsRealized;
  end;

  SetBoundsKeepBase(NewLeft,NewTop,Message.Width,Message.Height,Parent<>nil);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CNKeyDown
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.CNKeyDown(var Message: TLMKeyDown);
begin
  //DebugLn('TWinControl.CNKeyDown ',Name,':',ClassName);
  if not DoKeyDownBeforeInterface(Message) then {inherited};  // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  procedure TWinControl.CNSysKeyDown(var Message: TLMKeyDown);
 ------------------------------------------------------------------------------}
procedure TWinControl.CNSysKeyDown(var Message: TLMKeyDown);
begin
  if not DoKeyDownBeforeInterface(Message) then {inherited};  // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  procedure TWinControl.CNSysKeyUp(var Message: TLMKeyUp);
 ------------------------------------------------------------------------------}
procedure TWinControl.CNSysKeyUp(var Message: TLMKeyUp);
begin
  if not DoKeyUpBeforeInterface(Message) then {inherited}; // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CNKeyUp
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.CNKeyUp(var Message: TLMKeyUp);
begin
  if not DoKeyUpBeforeInterface(Message) then {inherited}; // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CNChar
  Params:   Msg: The message
  Returns:  nothing

  event handler.
  CNChar is sent by the interface before it has handled the keypress itself.
 ------------------------------------------------------------------------------}
procedure TWinControl.CNChar(var Message: TLMKeyUp);
var
  c: TUTF8Char;
begin
  //debugln('TWinControl.CNChar A ',DbgSName(Self),' ',dbgs(Message.CharCode),' ',dbgs(IntfSendsUTF8KeyPress));
  if not DoKeyPress(Message) then {inherited}; // there is nothing to inherit
  //debugln('TWinControl.CNChar B ',DbgSName(Self),' ',dbgs(Message.CharCode),' ',dbgs(IntfSendsUTF8KeyPress));
  if not IntfSendsUTF8KeyPress then begin
    // current interface does not (yet) send UTF8 key press notifications
    // -> emulate
    if Message.CharCode < %11000000 then begin
      c:=chr(Message.CharCode);
      IntfUTF8KeyPress(c,1);
      if (length(c)<>1) or (c[1]<>chr(Message.CharCode)) then begin
        // character changed
        if length(c)=1 then
          Message.CharCode:=ord(c[1])
        else
          Message.CharCode:=0;
      end;
    end;
  end;
end;

procedure TWinControl.CNSysChar(var Message: TLMKeyUp);
begin
  if not DoKeyPress(Message) then {inherited}; // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMNofity
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWInControl.WMNotify(var Message: TLMNotify);
Begin
  if not DoControlMsg(Message.NMHdr^.hwndfrom,Message) then exit;
  //Inherited;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMShowWindow
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMShowWindow(var Message: TLMShowWindow);
begin
  Assert(False, Format('Trace: TODO: [TWinControl.LMShowWindow] %s', [ClassName]));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMEnter
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMEnter(var Message: TLMEnter);
begin
  Assert(False, Format('Trace: TODO: [TWinControl.LMEnter] %s', [ClassName]));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMEraseBkgnd
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  if (Message.DC<>0) and (wcfEraseBackground in FWinControlFlags) then
    EraseBackground(Message.DC);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMExit
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMExit(var Message: TLMExit);
begin
  Assert(False, Format('Trace: TODO: [TWinControl.LMExit] %s', [ClassName]));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMMouseWheel
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMMouseWheel(var Message: TLMMouseEvent);
Var
  MousePos : TPoint;
  Shift : TShiftState;
begin
  Assert(False, Format('Trace: [TWinControl.LMMouseWheel] %s', [ClassName]));

  MousePos.X := Message.X;
  MousePos.Y := Message.Y;

  { always the middle button }
  Shift := [ssMiddle];

  DoMouseWheel(Shift, Message.WheelDelta, MousePos);

  inherited;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMChar
  Params:   Msg: The message
  Returns:  nothing

  event handler.
  WMChar is sent by the interface after it has handled the keypress by itself.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMChar(var Message: TLMChar);
begin
  DoRemainingKeyPress(Message);
  Assert(False, Format('Trace:[TWinControl.WMChar] %s', [ClassName]));
end;

procedure TWinControl.WMSysChar(var Message: TLMChar);
begin
  DoRemainingKeyPress(Message);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMKeyDown
  Params:   Msg: The message
  Returns:  nothing

  Event handler for keys not handled by the interface
 ------------------------------------------------------------------------------}
Procedure TWinControl.WMKeyDown(Var Message: TLMKeyDown);
begin
  DoRemainingKeyDown(Message);
end;

procedure TWinControl.WMSysKeyDown(var Message: TLMKeyDown);
begin
  DoRemainingKeyDown(Message);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.WMSysKeyUp(var Message: TLMKeyUp);
 ------------------------------------------------------------------------------}
procedure TWinControl.WMSysKeyUp(var Message: TLMKeyUp);
begin
  //debugln('TWinControl.WMSysKeyUp ',DbgSName(Self));
  DoRemainingKeyUp(Message);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMKeyUp
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
Procedure TWinControl.WMKeyUp(Var Message: TLMKeyUp);
begin
  //debugln('TWinControl.WMKeyUp ',DbgSName(Self));
  DoRemainingKeyUp(Message);
end;

{------------------------------------------------------------------------------
  Function: TWinControl.HandleAllocated
  Params:   None
  Returns:  True is handle is allocated

  Checks if a handle is allocated. I.E. if the control is mapped
 ------------------------------------------------------------------------------}
function TWinControl.HandleAllocated : Boolean;
begin
  HandleAllocated := (FHandle <> 0);
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object) if not already done.
 ------------------------------------------------------------------------------}
procedure TWinControl.CreateHandle;
begin
  if (not HandleAllocated) then CreateWnd;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object and assigns the handle
 ------------------------------------------------------------------------------}
procedure TWinControl.CreateWnd;
var
  Params: TCreateParams;
  i: Integer;

{  procedure WriteClientRect(const Prefix: string);
  var r: TRect;
  begin
    LCLIntf.GetClientRect(Handle,r);
    if csDesigning in ComponentState then
      DebugLn('WriteClientRect ',Prefix,' ',Name,':',ClassName,' r=',r.Right,',',r.Bottom);
  end;}

begin
  //DebugLn('[TWinControl.CreateWnd] START ',Name,':',Classname);
  if (csDestroying in ComponentState) then exit;

  if wcfInitializing in FWinControlFlags
  then begin
    DebugLn('[WARNING] Recursive call to CreateWnd for ',DbgSName(Self), ' while initializing');
    Exit;
  end;

  if wcfCreatingHandle in FWinControlFlags
  then begin
    DebugLn('[WARNING] Recursive call to CreateWnd for ',DbgSName(Self), ' while creating handle');
    Exit;
  end;

  if wcfCreatingChildHandles in FWinControlFlags
  then begin
    DebugLn('[WARNING] Recursive call to CreateWnd for ',DbgSName(Self), ' while creating children');
    Exit;
  end;

  Include(FWinControlFlags,wcfCreatingHandle);
  try
    CreateParams(Params);
    with Params do begin
      if (WndParent = 0) and (Style and WS_CHILD <> 0) then
        RaiseGDBException('TWinControl.CreateWnd: no parent '+Name+':'+ClassName);
      Assert((parent <> nil) or (WndParent = 0), 'TODO: find parent if parent=nil and WndParent <> 0');
    end;

    FHandle := TWSWinControlClass(WidgetSetClass).CreateHandle(Self, Params);
    if not HandleAllocated then
      RaiseGDBException('Handle creation failed creating '+DbgSName(Self));
    Constraints.UpdateInterfaceConstraints;
    InvalidatePreferredSize;
    TWSWinControlClass(WidgetSetClass).ConstraintsChange(Self);
    FWinControlFlags := FWinControlFlags - [wcfColorChanged,wcfFontChanged];

    //WriteClientRect('A');
    if Parent <> nil then AddControl;
    //WriteClientRect('B');

    Include(FWinControlFlags, wcfInitializing);
    InitializeWnd;
    
  finally
    Exclude(FWinControlFlags, wcfInitializing);
    Exclude(FWinControlFlags, wcfCreatingHandle);
  end;

  Include(FWinControlFlags, wcfCreatingChildHandles);
  try
    //DebugLn('[TWinControl.CreateWnd] ',Name,':',ClassName,' ',Left,',',Top,',',Width,',',Height);
    //WriteClientRect('C');

    if FWinControls <> nil then begin
      for i := 0 to FWinControls.Count - 1 do
        with TWinControl(FWinControls.Items[i]) do
          if Visible then HandleNeeded;
    end;

    ChildHandlesCreated;
  finally
    Exclude(FWinControlFlags,wcfCreatingChildHandles);
  end;

  // size this control
  {$IFDEF EnablePreferredSize}
  AdjustSize;
  if FControls<>nil then
    for i:=0 to FControls.Count-1 do
      TControl(FControls[i]).DoAutoSize;
  {$ENDIF}
  // realign childs
  ReAlign;

  //DebugLn('[TWinControl.CreateWnd] END ',Name,':',Classname);
  //WriteClientRect('D');
end;

{------------------------------------------------------------------------------
  procedure TWinControl.CreateComponent(TheOwner : TComponent);
------------------------------------------------------------------------------}
(*
procedure TWinControl.CreateComponent(TheOwner : TComponent);

  procedure RaiseError(const Msg: string);
  begin
    RaiseGDBException('TWinControl.CreateComponent: '+Name+':'+ClassName+' '+Msg);
  end;

begin
  if HandleAllocated then
    RaiseError('Handle already created');
  CNSendMessage(LM_CREATE, Self, nil);
  Constraints.UpdateInterfaceConstraints;
  FFlags:=FFlags-[wcfColorChanged,wcfFontChanged];
  if not HandleAllocated then
    RaiseError('Handle creation failed');
end;
*)

{------------------------------------------------------------------------------
  TWinControl Destroy Component
------------------------------------------------------------------------------}
procedure TWinControl.DestroyComponent;
begin
  if not HandleAllocated then
    RaiseGDBException('TWinControl.DestroyComponent Handle already destroyed');
  TWSWinControlClass(WidgetSetClass).DestroyHandle(Self);
  InvalidatePreferredSize;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.InitializeWnd
  Params:  none
  Returns: Nothing

  Gets called after the window is created, but before the child controls are
  created. Place cached property code here.
 ------------------------------------------------------------------------------}
procedure TWinControl.InitializeWnd;
begin
  Assert(False, Format('Trace:[TWinControl.InitializeWnd]  %s', [ClassName]));
  // set all cached properties

  //DebugLn('[TWinControl.InitializeWnd] ',Name,':',ClassName,':', FCaption,' ',Left,',',Top,',',Width,',',Height);

  //First set the WinControl property.
  //The win32 interface depends on it to determine where to send call backs.
  SetProp(Handle,'WinControl',TWinControl(Self));

  DoSendBoundsToInterface;
  TWSWinControlClass(WidgetSetClass).ShowHide(Self);

  if [wcfColorChanged,wcfFontChanged]*FWinControlFlags<>[]
  then begin
    // replace by update style call
    TWSWinControlClass(WidgetSetClass).SetColor(Self);
    FWinControlFlags:=FWinControlFlags-[wcfColorChanged,wcfFontChanged];
  end;

  if not (csDesigning in ComponentState) then
    EnableWindow(Handle, Enabled);

  // Delay the setting of text until it is completely loaded
  if not (csLoading in ComponentState) 
  then TWSWinControlClass(WidgetSetClass).SetText(Self, FCaption);

  // send pending resize event
  Resize;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.ParentFormHandleInitialized;

  Called after all childs handles of the ParentForm are created.
 ------------------------------------------------------------------------------}
procedure TWinControl.ParentFormHandleInitialized;
var
  i: Integer;
begin
  inherited ParentFormHandleInitialized;
  // tell all wincontrols about the final end of the handle creation phase
  if FWinControls <> nil then begin
    for i := 0 to FWinControls.Count - 1 do
      TWinControl(FWinControls.Items[i]).ParentFormHandleInitialized;
  end;
  // tell all controls about the final end of the handle creation phase
  if FControls<>nil then begin
    for i:=0 to FControls.Count-1 do
      TControl(FControls[i]).ParentFormHandleInitialized;
  end;
  //debugln('TWinControl.ParentFormHandleInitialized A ',DbgSName(Self));
  if wcfAutoSizeNeeded in FWinControlFlags then AdjustSize;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.ChildHandlesCreated;
  
  Called after all childs handles are created.
 ------------------------------------------------------------------------------}
procedure TWinControl.ChildHandlesCreated;
begin
  Exclude(FWinControlFlags,wcfCreatingChildHandles);
end;

{------------------------------------------------------------------------------
  function TWinControl.ParentHandlesAllocated: boolean;
  
  Checks if all Handles of all Parents are created.
 ------------------------------------------------------------------------------}
function TWinControl.ParentHandlesAllocated: boolean;
var
  CurControl: TWinControl;
begin
  Result:=false;
  CurControl:=Self;
  while CurControl<>nil do begin
    if (not CurControl.HandleAllocated)
    or ([csDestroying,csDestroyingHandle]*ComponentState<>[]) then
      exit;
    CurControl:=CurControl.Parent;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.Loaded;
 ------------------------------------------------------------------------------}
procedure TWinControl.Loaded;
begin
  inherited Loaded;
  if HandleAllocated
  then begin              
    // Set cached caption
    TWSWinControlClass(WidgetSetClass).SetText(Self, FCaption);
  
    if [wcfColorChanged,wcfFontChanged]*FWinControlFlags<>[] then
    begin
      TWSWinControlClass(WidgetSetClass).SetColor(Self);
      FWinControlFlags:=FWinControlFlags-[wcfColorChanged,wcfFontChanged];
    end;
  end;
  
  FixupTabList;

  // autosize this control
  if wcfAutoSizeNeeded in FWinControlFlags then
    AdjustSize;
  RealizeBounds;

  // align the childs
  if wcfReAlignNeeded in FWinControlFlags then
    ReAlign;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.DestroyWnd
  Params:  None
  Returns: Nothing

  Destroys the interface object.
 ------------------------------------------------------------------------------}
procedure TWinControl.DestroyWnd;
var
  S: String;
begin
  if HandleAllocated 
  then begin    
    // make sure our text is saved
    if TWSWinControlClass(WidgetSetClass).GetText(Self, S)
    then FCaption := S;
    RemoveProp(Handle,'WinControl');
    DestroyComponent;
    Handle := 0;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.HandleNeeded
  Params:  None
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TWinControl.HandleNeeded;
begin
  if (not HandleAllocated) and (not (csDestroying in ComponentState)) then
  begin
    if Parent = Self
    then begin
      Assert(False, Format('Trace:[TWinControl.HandleNeeded] Somebody set Parent := Self in %s. DONT DO THAT !!', [Classname]));
    end
    else begin
      if (Parent <> nil) then
      begin
        Parent.HandleNeeded;
        // has parent triggered us to create our handle ?
        if HandleAllocated then
          exit;
      end;
    end;
    CreateHandle;
  end;
end;

function TWinControl.BrushCreated: Boolean;
begin
  Result:=FBrush<>nil;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.BeginUpdateBounds
  Params:  None
  Returns: Nothing

  increases the BoundsLockCount
 ------------------------------------------------------------------------------}
procedure TWinControl.BeginUpdateBounds;
begin
  inc(FBoundsLockCount);
end;

{------------------------------------------------------------------------------
  Method: TControl.EndUpdateBounds
  Params:  None
  Returns: Nothing

  decreases the BoundsLockCount
 ------------------------------------------------------------------------------}
procedure TWinControl.EndUpdateBounds;

  procedure RaiseTooManyEndUpdates;
  begin
    raise Exception.Create('TWinControl.EndUpdateBounds '+DbgSName(Self)
      +' too many calls.');
  end;

begin
  if FBoundsLockCount<=0 then RaiseTooManyEndUpdates;
  dec(FBoundsLockCount);
  if FBoundsLockCount=0 then begin
    SetBounds(Left,Top,Width,Height);
  end;
end;

procedure TWinControl.LockRealizeBounds;
begin
  inc(FRealizeBoundsLockCount);
end;

procedure TWinControl.UnlockRealizeBounds;
begin
  if FRealizeBoundsLockCount<=0 then
    RaiseGDBException('UnlockRealizeBounds');
  dec(FRealizeBoundsLockCount);
  if FRealizeBoundsLockCount=0 then
    RealizeBounds;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DockDrop(DockObject: TDragDockObject; X, Y: Integer);
 ------------------------------------------------------------------------------}
procedure TWinControl.DockDrop(DockObject: TDragDockObject; X, Y: Integer);
var
  DestRect: TRect;
  ParentForm: TCustomForm;
  MappedLeftTop: TPoint;
begin
  // get dock destination rectangle and map it to the client area
  DestRect := DockObject.DockRect;
  MappedLeftTop:=ScreenToClient(DestRect.TopLeft);
  OffsetRect(DestRect,
             DestRect.Left-MappedLeftTop.X,DestRect.Top-MappedLeftTop.Y);
  DebugLn('TWinControl.DockDrop A ',Name,' DockControl=',DbgSName(DockObject.Control),' DestRect=',dbgs(DestRect));
  DisableAlign;
  try
    if (not UseDockManager) or (DockManager=nil) then begin
      // Delphi ignores the DropAlign when no DockManager is available
      // Why that?
      DockObject.Control.Align:=DockObject.DropAlign;
    end;
    DockObject.Control.Dock(Self, DestRect);
    if UseDockManager and (DockManager <> nil) then
      DockManager.InsertControl(DockObject.Control,
        DockObject.DropAlign, DockObject.DropOnControl);
  finally
    EnableAlign;
  end;
  ParentForm := GetParentForm(Self);
  if ParentForm<>nil then ParentForm.BringToFront;

  if Assigned(FOnDockDrop) then
    FOnDockDrop(Self, DockObject, X, Y);
end;

{------------------------------------------------------------------------------
  Method: TControl.GetIsResizing
  Params:  None
  Returns: Nothing

  decreases the BoundsLockCount
 ------------------------------------------------------------------------------}
function TWinControl.GetIsResizing: boolean;
begin
  Result:=BoundsLockCount>0;
end;

{------------------------------------------------------------------------------
  function TWinControl.GetTabOrder: TTabOrder;
 ------------------------------------------------------------------------------}
function TWinControl.GetTabOrder: TTabOrder;
begin
  if FParent <> nil then
    Result := ListIndexOf(FParent.FTabList,Self)
  else
    Result := FTabOrder;
end;

{------------------------------------------------------------------------------
  function TWinControl.GetVisibleDockClientCount: Integer;
 ------------------------------------------------------------------------------}
function TWinControl.GetVisibleDockClientCount: Integer;
var
  i: integer;
begin
  Result := 0;
  for i:=FDockClients.Count-1 downto 0 do
    if TControl(FDockClients[I]).Visible then inc(Result);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.SetChildSizing(const AValue: TControlChildSizing);
 ------------------------------------------------------------------------------}
procedure TWinControl.SetChildSizing(const AValue: TControlChildSizing);
begin
  if (FChildSizing=AValue) or (FChildSizing.IsEqual(AValue)) then exit;
  FChildSizing.Assign(AValue);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.SetDockSite(const NewDockSite: Boolean);
 ------------------------------------------------------------------------------}
procedure TWinControl.SetDockSite(const NewDockSite: Boolean);
begin
  if FDockSite=NewDockSite then exit;
  FDockSite := NewDockSite;
  if not (csDesigning in ComponentState) then begin
    RegisterDockSite(Self,NewDockSite);
    if not NewDockSite then begin
      FreeAndNil(FDockClients);
      FDockClients := nil;
      FDockManager := nil;
    end
    else begin
      if FDockClients = nil then FDockClients := TList.Create;
      FDockManager := CreateDockManager;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.SetBounds
  Params:  aLeft, aTop, aWidth, aHeight
  Returns: Nothing

  Sets the bounds of the control.
 ------------------------------------------------------------------------------}
procedure TWinControl.SetBounds(aLeft, aTop, aWidth, aHeight : integer);

  procedure CheckDesignBounds;
  begin
    if FRealizeBoundsLockCount>0 then exit;
    // the user changed the bounds
    if aWidth<0 then
      raise Exception.Create(
        'TWinControl.SetBounds ('+DbgSName(Self)+'): Negative width '
          +dbgs(aWidth)+' not allowed.');
    if aHeight<0 then
      raise Exception.Create(
        'TWinControl.SetBounds ('+DbgSName(Self)+'): Negative height '
          +dbgs(aHeight)+' not allowed.');
  end;

var
  NewBounds, OldBounds: TRect;
begin
  {$IFDEF CHECK_POSITION}
  //if csDesigning in ComponentState then
  if AnsiCompareText(ClassName,'TScrollBar')=0 then
  DebugLn('[TWinControl.SetBounds] START ',Name,':',ClassName,
  ' Old=',Left,',',Top,',',Width,',',Height,
  ' -> New=',ALeft,',',ATop,',',AWidth,',',AHeight,
  ' Lock=',BoundsLockCount,
  ' Realized=',FBoundsRealized.Left,',',FBoundsRealized.Top,
  ',',FBoundsRealized.Right-FBoundsRealized.Left,',',FBoundsRealized.Bottom-FBoundsRealized.Top
  );
  {$ENDIF}
  if BoundsLockCount<>0 then exit;
  OldBounds:=BoundsRect;
  NewBounds:=Bounds(ALeft, ATop, AWidth, AHeight);

  if not CompareRect(@NewBounds,@OldBounds) then begin
    if [csDesigning,csDestroying,csLoading]*ComponentState=[csDesigning] then
      CheckDesignBounds;
    // LCL bounds are not up2date -> process new bounds
    LockRealizeBounds;
    try
      {$IFDEF CHECK_POSITION}
      //if csDesigning in ComponentState then
      if AnsiCompareText(ClassName,'TScrollBar')=0 then
      DebugLn('[TWinControl.SetBounds] Set LCL Bounds ',Name,':',ClassName,
      ' OldBounds=',Left,',',Top,',',Left+Width,',',Top+Height,
      ' -> New=',ALeft,',',ATop,',',ALeft+AWidth,',',ATop+AHeight);
      {$ENDIF}
      inherited SetBounds(ALeft, ATop, AWidth, AHeight);
      NewBounds:=Bounds(Left, Top, Width, Height);
    finally
      UnlockRealizeBounds;
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.CalculatePreferredSize(var PreferredWidth,
    PreferredHeight: integer);
    
  Calculates the default/preferred width and height for a TWinControl, which is
  used by the LCL autosizing algorithms as default size. Only positive values
  are valid. Negative or 0 are treated as undefined and the LCL uses other sizes
  instead.
  TWinControl overrides this:
  If there are childs, their total preferred size is calculated.
  If this value can not be computed (e.g. the childs depend too much on their
  parent clientrect), then the interface is asked for the preferred size.
  For example the preferred size of a TButton is the size, where the label fits
  exactly. This depends heavily on the current theme and widgetset.
  
  This value is independent of constraints and siblings, only the inner parts
  are relevant.
 ------------------------------------------------------------------------------}
procedure TWinControl.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer);
var
  ChildBounds: TRect;
  NewClientWidth: Integer;
  NewClientHeight: Integer;
  OldClientRect: TRect;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight);
  if HandleAllocated then
    TWSWinControlClass(WidgetSetClass).GetPreferredSize(Self,
                                               PreferredWidth, PreferredHeight);
  if ControlCount>0 then begin
    GetChildBounds(ChildBounds,true);
    NewClientWidth := ChildBounds.Right - ChildBounds.Left;
    NewClientHeight := ChildBounds.Bottom - ChildBounds.Top;
    OldClientRect := GetClientRect;
    {$IFDEF VerboseAutoSize}
    debugln('TWinControl.CalculatePreferredSize ',DbgSName(Self),
      ' HandleAllocated=',dbgs(HandleAllocated)+' ChildBounds='+dbgs(ChildBounds),
      ' Cur='+dbgs(Width)+'x'+dbgs(Height)+
      ' Client='+dbgs(OldClientRect.Right)+'x'+dbgs(OldClientRect.Bottom));
    {$ENDIF}
    PreferredWidth:=
                   Max(PreferredWidth,Width-OldClientRect.Right+NewClientWidth);
    PreferredHeight:=
               Max(PreferredHeight,Height-OldClientRect.Bottom+NewClientHeight);
  end;
  {$IFDEF VerboseAutoSize}
  debugln('TWinControl.CalculatePreferredSize ',DbgSName(Self),
    ' HandleAllocated=',dbgs(HandleAllocated),
    ' Preferred=',dbgs(PreferredWidth),'x',dbgs(PreferredHeight));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  procedure TWinControl.GetChildBounds(var ChildBounds: TRect;
    WithBorderSpace: boolean);
    
  Calculates the bounds of all visible childs in client coordinates.
 ------------------------------------------------------------------------------}
procedure TWinControl.GetChildBounds(var ChildBounds: TRect;
  WithBorderSpace: boolean);
var
  SpaceAround: TRect;
  I: Integer;
  AControl: TControl;
  ChildWidth,ChildHeight: integer;
begin
  ChildBounds := Rect(High(Integer),High(Integer),0,0);
  SpaceAround:=Rect(0,0,0,0);
  For I := 0 to ControlCount - 1 do begin
    AControl:=Controls[I];
    If AControl.Visible then begin
      AControl.GetPreferredSize(ChildWidth,ChildHeight,false);
      // TODO: aligned controls
      if WithBorderSpace then begin
        AControl.BorderSpacing.GetSpaceAround(SpaceAround);
        if SpaceAround.Left<ChildSizing.LeftRightSpacing then
          SpaceAround.Left:=ChildSizing.LeftRightSpacing;
        if SpaceAround.Right<ChildSizing.LeftRightSpacing then
          SpaceAround.Right:=ChildSizing.LeftRightSpacing;
        if SpaceAround.Top<ChildSizing.TopBottomSpacing then
          SpaceAround.Top:=ChildSizing.TopBottomSpacing;
        if SpaceAround.Bottom<ChildSizing.TopBottomSpacing then
          SpaceAround.Bottom:=ChildSizing.TopBottomSpacing;
      end;
      With ChildBounds do begin
        Left := Min(AControl.Left-SpaceAround.Left, Left);
        Top := Min(AControl.Top-SpaceAround.Top, Top);
        Right := Max(AControl.Left+ChildWidth+SpaceAround.Right,Right);
        Bottom := Max(AControl.Top+ChildHeight+SpaceAround.Bottom,Bottom);
      end;
    end;
  end;
  if ChildBounds.Left>ChildBounds.Right then begin
    ChildBounds.Left:=0;
    ChildBounds.Right:=0;
  end;
  if ChildBounds.Top>ChildBounds.Bottom then begin
    ChildBounds.Top:=0;
    ChildBounds.Bottom:=0;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.RealGetText
  Params:  None
  Returns: The text

  Gets the text/caption of a control
 ------------------------------------------------------------------------------}
function TWinControl.RealGetText: TCaption;
begin
  if not HandleAllocated 
  or (csLoading in ComponentState) 
  or (not TWSWinControlClass(WidgetSetClass).GetText(Self, Result))
  then Result := inherited RealGetText;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.GetTextLen
  Params:  None
  Returns: The length of the text

  Gets the length of the text/caption of a control
 ------------------------------------------------------------------------------}
function TWinControl.GetTextLen: Integer;
begin
  if not HandleAllocated 
  or (csLoading in ComponentState) 
  or not TWSWinControlClass(WidgetSetClass).GetTextLen(Self, Result)
  then Result := inherited GetTextLen;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.RealSetText
  Params:  Value: the text to be set
  Returns: Nothing

  Sets the text/caption of a control
 ------------------------------------------------------------------------------}
procedure TWinControl.RealSetText(const AValue: TCaption);
begin
  if HandleAllocated 
  and (not (csLoading in ComponentState))
  then TWSWinControlClass(WidgetSetClass).SetText(Self, AValue);
  inherited RealSetText(AValue);
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.GetDeviceContext
  Params:  WindowHandle: the windowhandle of this control
  Returns: a Devicecontext

  Get the devicecontext for this WinControl.
 ------------------------------------------------------------------------------}
function TWinControl.GetDeviceContext(var WindowHandle: HWnd): HDC;
begin
  Result := GetDC(Handle);
  //DebugLn('[TWinControl.GetDeviceContext] ',ClassName,' DC=',DbgS(Result,8),' Handle=',DbgS(FHandle));
  if Result = 0
  then raise EOutOfResources.CreateFmt(rsErrorCreatingDeviceContext, [Name,
    ClassName]);

  WindowHandle := Handle;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CMVisibleChanged
  Params:  Message : not used
  Returns: nothing

  Performs actions when visibility has changed
 ------------------------------------------------------------------------------}
procedure TWinControl.CMVisibleChanged(var TheMessage : TLMessage);
begin
  if not FVisible and (Parent <> nil)
  then RemoveFocus(False);

  if not (csDesigning in ComponentState)
  or (csNoDesignVisible in ControlStyle)
  then
    UpdateControlState;
end;

procedure TWinControl.ControlsAligned;
begin

end;

procedure TWinControl.DoSendBoundsToInterface;
var
  NewBounds: TRect;
begin
  NewBounds:=Bounds(Left, Top, Width, Height);
  //DebugLn('TWinControl.DoSendBoundsToInterface A ',Name,':',ClassName,' Old=',FBoundsRealized.Left,',',FBoundsRealized.Top,',',FBoundsRealized.Right,',',FBoundsRealized.Bottom,
  //' New=',NewBounds.Left,',',NewBounds.Top,',',NewBounds.Right,',',NewBounds.Bottom);
  FBoundsRealized:=NewBounds;
  TWSWinControlClass(WidgetSetClass).SetBounds(Self, Left, Top, Width, Height);
end;

procedure TWinControl.RealizeBounds;
var
  NewBounds: TRect;
begin
  NewBounds:=Bounds(Left, Top, Width, Height);
  if HandleAllocated
  and (not (csLoading in ComponentState))
  and (not CompareRect(@NewBounds,@FBoundsRealized)) then
  begin
    // the new bounds were not yet sent to the InterfaceObject -> send them
    {$IFDEF CHECK_POSITION}
    //if csDesigning in ComponentState then
    if AnsiCompareText(ClassName,'TScrollBar')=0 then
    DebugLn('[TWinControl.RealizeBounds] A ',Name,':',ClassName,
    ' OldRelBounds=',dbgs(FBoundsRealized),
    ' -> NewBounds=',dbgs(NewBounds));
    {$ENDIF}
    BeginUpdateBounds;
    try
      DoSendBoundsToInterface;
    finally
      EndUpdateBounds;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CMShowingChanged
  Params:  Message : not used
  Returns: nothing

  Shows or hides a control
 ------------------------------------------------------------------------------}
procedure TWinControl.CMShowingChanged(var Message: TLMessage);
begin
  // ToDo: do not send this while loading, send it after loading.
  if HandleAllocated and ([csDestroying]*ComponentState=[])then
    TWSWinControlClass(WidgetSetClass).ShowHide(Self);
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.ShowControl
  Params:  AControl: Control to show
  Returns: nothing

  Askes the parent to show ourself.
 ------------------------------------------------------------------------------}
procedure TWinControl.ShowControl(AControl: TControl);
begin
  if Parent <> nil then Parent.ShowControl(Self);
end;

{ $UNDEF CHECK_POSITION}

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}
{ =============================================================================

  $Log$
  Revision 1.318  2005/03/07 23:21:36  mattias
  fixed DefaultRowHeight=18 and various published properties  from Jesus

  Revision 1.317  2005/03/07 21:59:45  vincents
  changed hexstr(cardinal()) for pointers to dbgs() and other 64-bits fixes   from Peter Vreman

  Revision 1.316  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.315  2005/02/21 13:54:26  mattias
  added navigation key check for up/down already handled

  Revision 1.314  2005/02/19 22:48:23  mattias
  fixed navigation key handling for TButton

  Revision 1.313  2005/02/19 21:54:08  mattias
  moved LCL navigation key handling to key up, so that interface has the chance to handle keys

  Revision 1.312  2005/02/07 11:04:06  micha
  make cn_,wm_ keyup and syskeyup more symmetric with keydown

  Revision 1.311  2005/02/06 15:49:49  micha
  handle generic shortcuts after widgetset specific shortcuts

  Revision 1.310  2005/02/05 16:09:52  marc
  * first 64bit changes

  Revision 1.309  2005/02/03 15:10:23  micha
  implement shortcut handling, tcustomlabel accelerator focuscontrol functionality

  Revision 1.308  2005/01/26 15:45:08  mattias
  implemented adding files from directory in project inspector, fixed extrac proc checking overlapping blocks

  Revision 1.307  2005/01/25 09:58:16  mattias
  fixed fpc 1.0.10 compilation

  Revision 1.306  2005/01/24 11:49:42  mattias
  added checks for filenames with spaces

  Revision 1.305  2005/01/23 15:05:23  mattias
  fixed TabList access

  Revision 1.304  2005/01/21 19:18:35  mattias
  implemented option to reduce designer painting

  Revision 1.303  2005/01/21 11:52:01  micha
  cleanup focus; fix tabbing

  Revision 1.302  2005/01/21 10:34:56  mattias
  implemented streaming of anchorsides

  Revision 1.301  2005/01/18 18:46:59  mattias
  improved invert assignment tool by multilines   from Andrew Haines

  Revision 1.300  2005/01/17 17:49:27  mattias
  fixed constraints for forms under gtk

  Revision 1.299  2005/01/17 16:42:35  mattias
  improved TLabel autosizing

  Revision 1.298  2005/01/16 11:40:10  mattias
  fixed TGtkWidgetSet.ExtSelectClipRGN for DCOrigin

  Revision 1.297  2005/01/13 19:52:50  mattias
  added desgntime check for TControl.Width/Height for negative values

  Revision 1.296  2005/01/08 14:23:56  micha
  move taborder and tabstop to twincontrol

  Revision 1.295  2005/01/04 11:26:26  micha
  let canfocus imply that setfocus can be called

  Revision 1.294  2005/01/03 22:44:31  mattias
  implemented TControl.AnchorSide

  Revision 1.293  2005/01/01 18:56:47  mattias
  implemented TTIProgressBar

  Revision 1.292  2004/12/01 16:17:17  mattias
  updated fpdoc sceletons for lcl and gtk intf

  Revision 1.291  2004/11/29 01:12:36  mattias
  added SysKey messages to gtk intf and LCL

  Revision 1.290  2004/11/28 01:03:26  mattias
  do not send OnExit/OnEnter during destroy, design or loading

  Revision 1.289  2004/11/10 18:23:56  mattias
  impementing changing a TLabel.Font properties Size, Height, Name, Style - set only at Handle creation time

  Revision 1.288  2004/11/07 20:44:49  micha
  handle "re-entrancy" into handleneeded (via parent); fixes crash upon showing file diff dialog

  Revision 1.287  2004/11/05 22:08:53  mattias
  implemented auto sizing: child to parent sizing

  Revision 1.286  2004/11/03 14:18:36  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.285  2004/10/30 16:24:06  mattias
  disabled alClient RemainingClientRect adjust

  Revision 1.284  2004/10/28 17:56:11  mattias
  implemented Borderspacing

  Revision 1.283  2004/10/28 09:30:49  mattias
  implemented borderspacing TWinControl.ChildSizing.Left/Top

  Revision 1.282  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.281  2004/09/24 17:20:43  micha
  convert LM_SETGEOMETRY message to interface method

  Revision 1.280  2004/09/21 10:05:26  mattias
  fixed disable at designtime and bounding TCustomProgressBar position

  Revision 1.279  2004/09/18 12:43:15  micha
  convert LM_DESTROY message to interface methods

  Revision 1.278  2004/09/14 15:48:28  micha
  convert LM_INVALIDATE message to interface method

  Revision 1.277  2004/09/12 19:50:35  micha
  convert LM_SETSIZE message to new interface method

  Revision 1.276  2004/09/12 13:52:26  micha
  convert LM_SETFONT to interface method

  Revision 1.275  2004/09/10 16:28:50  mattias
  implemented very rudimentary TTabControl

  Revision 1.274  2004/09/08 20:47:16  micha
  convert LM_SHOWHIDE message to new intf method TWSWinControl.ShowHide

  Revision 1.273  2004/09/08 19:09:34  micha
  convert LM_SETCOLOR message to new intf method TWSWinControl.SetColor

  Revision 1.272  2004/09/04 22:24:16  mattias
  added default values for compiler skip options and improved many parts of synedit for UTF8

  Revision 1.271  2004/09/02 17:59:59  mattias
  removed double KeyPress method in synedit

  Revision 1.270  2004/09/02 17:42:38  mattias
  fixed changing CNCHar.CharCode when key changed

  Revision 1.269  2004/09/02 09:16:59  mattias
  improved double byte char fonts for gtk1, started synedit UTF8 support

  Revision 1.268  2004/08/30 16:37:58  mattias
  added OnUTF8KeyPresss

  Revision 1.267  2004/08/30 10:49:20  mattias
  fixed focus catch for combobox csDropDownList

  Revision 1.266  2004/08/26 19:09:34  mattias
  moved navigation key handling to TApplication and added options for custom navigation

  Revision 1.265  2004/08/25 22:22:39  mattias
  added unit info to View menu

  Revision 1.264  2004/08/18 22:56:11  mattias
  implemented basic manual docking

  Revision 1.263  2004/08/18 20:49:02  mattias
  simple forms can now be child controls

  Revision 1.262  2004/08/17 19:01:36  mattias
  gtk intf now ignores size notifications of unrealized widgets

  Revision 1.261  2004/08/13 19:48:53  mattias
  added default assembler style for compiler options

  Revision 1.260  2004/08/13 16:40:47  mazen
  + TCharater type used to allow UTF8 keyboard with gtk2

  Revision 1.259  2004/08/04 07:32:01  micha
  fix win32 keyhandling, send cn_char

  Revision 1.258  2004/08/03 09:01:54  mattias
  LCL now handles for non win32 CN_CHAR

  Revision 1.257  2004/07/17 15:08:36  mattias
  fixed tab for TPanel and TPage

  Revision 1.256  2004/07/16 21:49:00  mattias
  added RTTI controls

  Revision 1.255  2004/07/11 17:20:47  marc
  * Implemented most of TListColoum/Item in the Ws for gtk and win32

  Revision 1.254  2004/07/10 18:17:30  mattias
  added Delphi ToDo support, Application.WndProc, small bugfixes  from Colin

  Revision 1.253  2004/07/07 22:26:58  mattias
  fixed showing grabers for boundless components

  Revision 1.252  2004/07/04 11:09:43  mattias
  fixed IFDEF tool for blocks  from Colin

  Revision 1.251  2004/07/03 14:59:42  mattias
  fixed keydown geting all keys

  Revision 1.250  2004/07/03 11:11:08  mattias
  TGTKListStringList now keeps selection on Put and Move

  Revision 1.249  2004/07/02 12:23:24  micha
  fix capture return/escape key if no handler

  Revision 1.248  2004/07/01 20:42:11  micha
  implement better ExecuteXXAction design; break dependency on TButton class in TCustomForm

  Revision 1.247  2004/07/01 17:55:55  mattias
  LCL navigation keys are now handled after interface handles keys

  Revision 1.246  2004/07/01 10:08:31  mattias
  made key handling more flexible

  Revision 1.245  2004/06/30 11:07:20  micha
  implement return key clicks default button; escape key clicks cancel button

  Revision 1.244  2004/06/29 10:23:00  micha
  fix cnkeydown to check wm_getdlgcode result
  fix win32 intf to also send wm_keydown of cn_keydown wasn't processed

  Revision 1.243  2004/06/28 18:57:55  mattias
  fixed GetControlAtPos for non designing

  Revision 1.242  2004/06/28 18:54:27  mattias
  further fixed GetControlAtPos

  Revision 1.241  2004/06/28 18:47:30  mattias
  further fixed GetControlAtPos

  Revision 1.240  2004/06/28 17:41:26  mattias
  fixed GetControlAtPos

  Revision 1.239  2004/06/28 17:03:37  mattias
  clean up

  Revision 1.238  2004/06/20 21:21:49  micha
  fix GetVisible to return this control's visibility, instead introduce IsVisible to check for recursive visibility

  Revision 1.237  2004/06/20 20:25:47  micha
  fix tabbing to next control to skip invisible notebook pages

  Revision 1.236  2004/06/01 09:58:35  mattias
  implemented setting TCustomPage.PageIndex  from Andrew Haines

  Revision 1.235  2004/05/31 13:06:47  marc
  * Patch from Vincent, moved SetProp(wincontrol)

  Revision 1.234  2004/05/30 14:02:30  mattias
  implemented OnChange for TRadioButton, TCheckBox, TToggleBox and some more docking stuff

  Revision 1.233  2004/05/22 14:35:32  mattias
  fixed button return key

  Revision 1.232  2004/05/21 21:58:01  vincents
  Added if HandleAllocated to SetBorderStyle

  Revision 1.231  2004/05/21 10:02:59  micha
  consistent BorderStyle naming

  Revision 1.230  2004/05/21 09:03:55  micha
  implement new borderstyle
  - centralize to twincontrol (protected)
  - public expose at tcustomcontrol to let interface access it

  Revision 1.229  2004/05/19 18:41:54  micha
  trigger repaint on borderwidth change

  Revision 1.228  2004/05/16 22:43:23  mattias
  WMSize from interface are now ignored when BoundsRealized the same

  Revision 1.227  2004/05/11 11:42:27  mattias
  replaced writeln by debugln

  Revision 1.226  2004/05/11 10:53:59  mattias
  replaced writeln by debugln

  Revision 1.225  2004/05/11 09:49:46  mattias
  started sending CN_KEYUP

  Revision 1.224  2004/05/02 17:06:01  marc
  - Removed TWSWinControl.HasText

  Revision 1.223  2004/04/28 06:50:21  micha
  more detailed error message req. by ML

  Revision 1.222  2004/04/26 10:01:27  mattias
  fixed TSynEdit.RealGetText

  Revision 1.221  2004/04/23 11:18:28  mattias
  fixed unsetting csFocusing

  Revision 1.220  2004/04/20 23:39:01  marc
  * Fixed setting of TWincontrol.Text during load

  Revision 1.219  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.218  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.217  2004/04/10 17:54:52  micha
  - added: [win32] mousewheel default handler sends scrollbar messages
  - fixed: lmsetcursor; partial todo

  Revision 1.216  2004/04/09 23:52:01  mattias
  fixed hiding uninitialized controls

  Revision 1.215  2004/04/04 12:32:21  mattias
  TWinControl.CanTab now checks for CanFocus

  Revision 1.214  2004/03/30 20:38:14  mattias
  fixed interface constraints, fixed syncompletion colors

  Revision 1.213  2004/03/19 00:03:15  marc
  * Moved the implementation of (GTK)ButtonCreateHandle to the new
    (GTK)WSButton class

  Revision 1.212  2004/03/08 22:36:01  mattias
  added TWinControl.ParentFormInitializeWnd

  Revision 1.211  2004/03/07 09:37:20  mattias
  added workaround for AutoSize in TCustomLabel

  Revision 1.210  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.209  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.208  2004/02/23 23:15:13  mattias
  improved FindDragTarget

  Revision 1.207  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.206  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.204  2004/02/22 15:39:43  mattias
  fixed error handling on saving lpi file

  Revision 1.203  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.202  2004/02/21 15:37:33  mattias
  moved compiler options to project menu, added -CX for smartlinking

  Revision 1.201  2004/02/13 15:49:54  mattias
  started advanced LCL auto sizing

  Revision 1.200  2004/02/12 18:09:10  mattias
  removed win32 specific TToolBar code in new TToolBar, implemented TWinControl.FlipChildren

  Revision 1.199  2004/02/09 19:52:52  mattias
  implemented ByteOrder for TLazIntfImage and added call of to LM_SETFONT

  Revision 1.198  2004/02/04 23:30:18  mattias
  completed TControl actions

  Revision 1.197  2004/02/04 17:39:30  mattias
  quick fixed TToolBar destruction

  Revision 1.196  2004/02/02 16:59:28  mattias
  more Actions  TAction, TBasicAction, ...

  Revision 1.195  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.194  2004/02/02 11:07:43  mattias
  constraints and aligning now work together

  Revision 1.193  2004/02/02 00:41:06  mattias
  TScrollBar now automatically checks Align and Anchors for useful values

  Revision 1.192  2004/01/27 21:32:11  mattias
  improved changing style of controls

  Revision 1.191  2004/01/13 16:39:01  mattias
  changed consistency stops during var renaming to errors

  Revision 1.190  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.189  2004/01/10 02:01:37  mattias
  implemented Undo property for OI

  Revision 1.188  2004/01/09 13:49:43  mattias
  improved gtk intf key fetching and OI keyboard navigation

  Revision 1.187  2004/01/06 17:58:06  mattias
  fixed setting TRadioButton.Caption for gtk

  Revision 1.186  2004/01/03 23:14:59  mattias
  default font can now change height and fixed gtk crash

  Revision 1.185  2004/01/03 21:06:06  micha
  - fix win32/checklistbox
  - implement proper lcl to interface move/size notify via setwindowpos
  - fix treeview to use inherited canvas from customcontrol
  - implement double buffering in win32

  Revision 1.184  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.183  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.182  2003/12/23 16:50:45  micha
  fix defocus control when destroying it

  Revision 1.181  2003/12/14 19:18:04  micha
  hint fixes: parentfont, font itself, showing/hiding + more

  Revision 1.180  2003/12/06 19:20:46  mattias
  codecompletion: forward proc body position now block sensitive

  Revision 1.179  2003/11/23 00:28:51  mattias
  fixed closing IDE while debugging

  Revision 1.178  2003/10/31 11:49:30  micha
  fix win32 designer: grid, non-windowed components, selectiongrabbers

  Revision 1.177  2003/10/16 19:43:44  ajgenius
  disable Buffering in TWinControl.WM_PAINT

  Revision 1.176  2003/10/06 10:50:10  mattias
  added recursion to InvalidateClientRectCache

  Revision 1.175  2003/10/02 11:33:39  mattias
  completed compstyles and fixed calendar date parsing from Karl

  Revision 1.174  2003/09/26 06:59:59  mattias
  implemented GetBrush

  Revision 1.173  2003/09/25 09:01:54  mattias
  increased rpm version

  Revision 1.172  2003/09/23 08:00:46  mattias
  improved OnEnter for gtkcombo

  Revision 1.171  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.170  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.169  2003/09/17 15:26:41  mattias
  fixed removing TPage

  Revision 1.168  2003/09/13 16:43:01  mattias
  fixed PerformTab call

  Revision 1.167  2003/09/13 14:01:39  mattias
  fixed taborder dlg

  Revision 1.166  2003/08/31 17:30:49  mattias
  fixed TControl painting for win32

  Revision 1.165  2003/08/27 11:01:10  mattias
  started TDockTree

  Revision 1.164  2003/08/26 14:33:40  mattias
  implemented component tree for OI

  Revision 1.163  2003/08/25 16:18:15  mattias
  fixed background color of TPanel and clicks of TSpeedButton from Micha

  Revision 1.162  2003/08/23 21:17:09  mattias
  several fixes for the win32 intf, added pending OnResize events

  Revision 1.161  2003/08/23 11:30:50  mattias
  fixed SetComboHeight in win32 intf and finddeclaration of overloaded proc definition

  Revision 1.160  2003/08/21 13:04:10  mattias
  implemented insert marks for TTreeView

  Revision 1.159  2003/08/12 16:12:42  mattias
  reduced output

  Revision 1.158  2003/08/12 16:04:22  mattias
  added groupboxnested example

  Revision 1.157  2003/08/12 14:02:54  mattias
  fixed keypress/keyup, createcaret on synedit focus

  Revision 1.156  2003/07/30 13:03:44  mattias
  replaced label with memo

  Revision 1.155  2003/07/25 08:00:36  mattias
  fixed sending follow up move/size messages from gtk

  Revision 1.154  2003/07/24 06:54:32  mattias
  fixed anti circle mechnism for aligned controls

  Revision 1.153  2003/07/07 07:59:34  mattias
  made Size_SourceIsInterface a flag

  Revision 1.152  2003/07/06 20:40:34  mattias
  TWinControl.WmSize/Move now updates interface messages smarter

  Revision 1.151  2003/07/04 10:12:16  mattias
  added default message handler to win32 interface

  Revision 1.150  2003/07/02 15:56:15  mattias
  fixed win32 painting and started creating bitmaps from rawimages

  Revision 1.149  2003/06/30 16:39:40  mattias
  clean up

  Revision 1.148  2003/06/30 14:58:29  mattias
  implemented multi file add to package editor

  Revision 1.147  2003/06/30 07:05:30  mattias
  activated EraseBKGND hack

  Revision 1.146  2003/06/30 07:00:18  mattias
  activated EraseBckGrd messages in doublebuffer WMPaint section

  Revision 1.145  2003/06/28 12:10:02  mattias
  fixed LM_SETSIZE in InitializeWnd

  Revision 1.144  2003/06/27 23:42:38  mattias
  fixed TScrollBar resizing

  Revision 1.143  2003/06/27 22:07:39  mattias
  fixed targetos for clean all

  Revision 1.142  2002/08/17 23:41:34  mattias
  many clipping fixes

  Revision 1.141  2003/06/20 12:56:53  mattias
  reduced paint messages on destroy

  Revision 1.140  2003/06/19 22:38:21  mattias
  fixed update on changing package usage options

  Revision 1.139  2003/06/19 16:36:35  mattias
  started codeexplorer

  Revision 1.138  2003/06/19 09:26:58  mattias
  fixed changing unitname during update

  Revision 1.137  2003/06/18 11:21:06  mattias
  fixed taborder=0, implemented TabOrder Editor

  Revision 1.136  2003/06/12 18:55:44  mattias
  improved designer to recognize auto child moves

  Revision 1.135  2003/06/11 22:29:42  mattias
  fixed realizing bounds after loading form

  Revision 1.134  2003/06/10 17:23:35  mattias
  implemented tabstop

  Revision 1.133  2003/06/10 12:28:23  mattias
  fixed anchoring controls

  Revision 1.132  2003/06/10 00:46:16  mattias
  fixed aligning controls

  Revision 1.131  2003/06/01 21:09:09  mattias
  implemented datamodules

  Revision 1.130  2003/05/24 08:51:41  mattias
  implemented designer close query

  Revision 1.129  2003/04/20 20:32:40  mattias
  implemented removing, re-adding, updating project dependencies

  Revision 1.128  2003/04/11 10:31:57  mattias
  added Sender to Application OnKeyDownHandler

  Revision 1.127  2003/04/11 10:23:23  mattias
  added Application OnKeyDownHandler

  Revision 1.126  2003/04/10 09:22:42  mattias
  implemented changing dependency version

  Revision 1.125  2003/04/04 09:19:22  mattias
  activated TDataSource

  Revision 1.124  2003/03/28 19:39:54  mattias
  started typeinfo for double extended

  Revision 1.123  2003/03/25 10:45:41  mattias
  reduced focus handling and improved focus setting

  Revision 1.122  2003/03/18 13:04:25  mattias
  improved focus debugging output

  Revision 1.121  2003/03/17 08:51:09  mattias
  added IsWindowVisible

  Revision 1.120  2003/03/11 07:46:44  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.119  2003/03/09 17:44:12  mattias
  finshed Make Resourcestring dialog and implemented TToggleBox

  Revision 1.118  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.117  2003/02/09 18:07:42  mattias
  cleanup

  Revision 1.116  2003/01/24 13:53:53  mattias
  fixed TRadioGroup.Items editing in IDE

  Revision 1.115  2003/01/18 21:31:43  mattias
  fixed scrolling offset of TScrollingWinControl

  Revision 1.114  2003/01/01 13:04:26  mattias
  clean ups

  Revision 1.113  2003/01/01 13:01:01  mattias
  fixed setcolor for streamed components

  Revision 1.112  2002/12/29 18:13:38  mattias
  identifier completion: basically working, still hidden

  Revision 1.111  2002/12/28 17:43:43  mattias
  fixed FindControl and searching overloaded procs

  Revision 1.110  2002/12/28 12:42:38  mattias
  focus fixes, reduced lpi size

  Revision 1.109  2002/12/27 17:46:04  mattias
  fixed SetColor

  Revision 1.108  2002/12/27 17:12:38  mattias
  added more Delphi win32 compatibility functions

  Revision 1.107  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.106  2002/12/04 20:39:15  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.105  2002/12/03 09:28:31  mattias
  cleaned up

  Revision 1.104  2002/11/30 08:35:42  mattias
  TCustomForm.WMDestroy does not Free anymore

  Revision 1.103  2002/11/29 15:14:48  mattias
  replaced many invalidates by invalidaterect

  Revision 1.102  2002/11/27 15:40:36  mattias
  fixed resize request

  Revision 1.101  2002/11/21 18:49:53  mattias
  started OnMouseEnter and OnMouseLeave

  Revision 1.100  2002/11/10 21:49:28  lazarus
  MG: added smart hints in edit mode

  Revision 1.99  2002/11/09 18:13:33  lazarus
  MG: fixed gdkwindow checks

  Revision 1.98  2002/11/03 22:40:28  lazarus
  MG: fixed ControlAtPos

  Revision 1.97  2002/11/01 14:40:31  lazarus
  MG: fixed mouse coords on scrolling wincontrols

  Revision 1.96  2002/10/31 22:14:16  lazarus
  MG: fixed GetClipBox when clipping region invalid

  Revision 1.95  2002/10/31 21:29:47  lazarus
  MG: implemented TControlScrollBar.Size

  Revision 1.94  2002/10/26 15:15:49  lazarus
  MG: broke LCL<->interface circles

  Revision 1.93  2002/10/09 10:22:54  lazarus
  MG: fixed client origin coordinates

  Revision 1.92  2002/10/08 22:32:27  lazarus
  MG: fixed cool little bug (menu double attaching bug)

  Revision 1.91  2002/10/04 20:46:51  lazarus
  MG: improved TComboBox.SetItemIndex

  Revision 1.90  2002/10/04 14:24:15  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.89  2002/09/29 15:08:38  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>
    Patch includes:
      -fixes Problems with hiding modal forms
      -temporarily fixes TCustomForm.BorderStyle in bsNone
      -temporarily fixes problems with improper tabbing in TSynEdit

  Revision 1.88  2002/09/27 20:52:23  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.87  2002/09/16 15:42:17  lazarus
  MG: fixed calling DestroyHandle if not HandleAllocated

  Revision 1.86  2002/09/10 06:49:19  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.85  2002/09/05 12:11:43  lazarus
  MG: TNotebook is now streamable

  Revision 1.84  2002/09/03 08:07:20  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.83  2002/09/01 16:11:22  lazarus
  MG: double, triple and quad clicks now works

  Revision 1.82  2002/08/31 11:37:09  lazarus
  MG: fixed destroying combobox

  Revision 1.81  2002/08/30 12:32:21  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.80  2002/08/30 06:46:03  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.79  2002/08/25 14:32:11  lazarus
  MG: calendar now ignores double clicks

  Revision 1.78  2002/08/24 12:54:59  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.77  2002/08/24 06:51:22  lazarus
  MG: from Andrew: style list fixes, autosize for radio/checkbtns

  Revision 1.76  2002/08/17 15:45:33  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.75  2002/08/05 07:43:28  lazarus
  MG: fixed BadCursor bug and Circle Reference of FixedWidget of csPanel

  Revision 1.74  2002/06/21 15:41:56  lazarus
  MG: moved RectVisible, ExcludeClipRect and IntersectClipRect to interface dependent functions

  Revision 1.73  2002/06/19 19:46:09  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.72  2002/06/11 13:41:09  lazarus
  MG: fixed mouse coords and fixed mouse clicked thru bug

  Revision 1.71  2002/05/28 15:05:59  lazarus
  MG: reduced output

  Revision 1.70  2002/05/28 14:58:30  lazarus
  MG: added scrollbars for TListView

  Revision 1.69  2002/05/27 14:38:34  lazarus
  MG; fixed find declaration of overloaded procs and expression input types

  Revision 1.68  2002/05/24 07:16:32  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.67  2002/05/13 14:47:00  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.66  2002/05/12 04:56:20  lazarus
  MG: client rect bugs nearly completed

  Revision 1.65  2002/05/10 06:05:56  lazarus
  MG: changed license to LGPL

  Revision 1.64  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.63  2002/05/06 08:50:36  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.62  2002/04/28 14:10:31  lazarus
  MG: fixes for saving resource files

  Revision 1.61  2002/04/27 15:35:50  lazarus
  MG: fixed window shrinking

  Revision 1.60  2002/04/24 16:11:17  lazarus
  MG: started new client rectangle

  Revision 1.59  2002/04/22 13:07:45  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.58  2002/04/18 08:13:36  lazarus
  MG: added include comments

  Revision 1.56  2002/04/04 12:25:01  lazarus
  MG: changed except statements to more verbosity

  Revision 1.55  2002/03/31 23:20:38  lazarus
  MG: fixed initial size of TPage

  Revision 1.54  2002/03/29 14:32:49  lazarus
  MG: further internationalization

  Revision 1.53  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.52  2002/03/16 21:40:55  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.51  2002/03/14 23:25:52  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

  Revision 1.50  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.49  2002/01/21 14:17:47  lazarus
  MG: added find-block-start and renamed find-block-other-end

  Revision 1.48  2002/01/01 15:50:15  lazarus
  MG: fixed initial component aligning

  Revision 1.47  2001/12/31 22:43:00  lazarus
  Added a TViewColumn editor to be used in the object inspector as TViewColumn's property editor.
  Shane

  Revision 1.46  2001/12/28 15:12:02  lazarus
  MG: LM_SIZE and LM_MOVE messages are now send directly, not queued

  Revision 1.45  2001/12/20 14:41:20  lazarus
  Fixed setfocus for TComboBox and TMemo
  Shane

  Revision 1.44  2001/12/08 08:54:45  lazarus
  MG: added TControl.Refresh

  Revision 1.43  2001/12/07 20:12:15  lazarus
  Added a watch dialog.
  Shane

  Revision 1.42  2001/11/10 10:48:00  lazarus
  MG: fixed set formicon on invisible forms

  Revision 1.41  2001/11/09 19:14:23  lazarus
  HintWindow changes
  Shane

  Revision 1.40  2001/10/31 16:29:22  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.39  2001/10/10 17:55:04  lazarus
  MG: fixed caret lost, gtk cleanup, bracket lvls, bookmark saving

  Revision 1.38  2001/10/07 07:28:33  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.37  2001/10/03 17:34:27  lazarus
  MG: activated TCustomForm.OnCreate event

  Revision 1.35  2001/08/07 11:05:51  lazarus
  MG: small bugfixes

  Revision 1.34  2001/06/27 21:43:23  lazarus
  MG: added project bookmark support

  Revision 1.33  2001/06/15 10:31:06  lazarus
  MG: set longstrings as default

  Revision 1.32  2001/06/14 14:57:59  lazarus
  MG: small bugfixes and less notes

  Revision 1.31  2001/06/05 10:32:05  lazarus
  MG: small bugfixes for bitbtn, handles

  Revision 1.30  2001/05/16 10:00:00  lazarus
  MG: fixed wrong page index in editor closing

  Revision 1.29  2001/05/13 22:07:08  lazarus
  Implemented BringToFront / SendToBack.

  Revision 1.28  2001/04/02 14:45:26  lazarus
  MG: bugfixes for TBevel

  Revision 1.27  2001/03/27 14:27:43  lazarus
  Changes from Nagy Zsolt
  Shane

  Revision 1.26  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.24  2001/03/19 14:38:39  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.21  2001/03/12 12:17:02  lazarus
  MG: fixed random function results

  Revision 1.20  2001/02/28 13:17:33  lazarus
  Added some debug code for the top,left reporting problem.
  Shane

  Revision 1.19  2001/02/06 20:59:17  lazarus
  Trying to get the last control of the last form focused when a dialog closes.
  Still working on it.
  Shane

  Revision 1.18  2001/02/06 18:19:37  lazarus
  Shane

  Revision 1.17  2001/02/06 14:52:47  lazarus
  Changed TSpeedbutton in gtkobject so it erases itself when it's set to visible=false;
  Shane

  Revision 1.15  2001/02/04 04:18:12  lazarus
  Code cleanup and JITFOrms bug fix.
  Shane

  Revision 1.14  2001/02/01 19:34:50  lazarus
  TScrollbar created and a lot of code added.

  It's cose to working.
  Shane

  Revision 1.13  2001/01/30 18:15:02  lazarus
  Added code for TStatusBar
  I'm now capturing WMPainT and doing the drawing myself.
  Shane

  Revision 1.12  2001/01/28 21:06:07  lazarus
  Changes for TComboBox events KeyPress Focus.
  Shane

  Revision 1.11  2001/01/23 23:33:54  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.10  2001/01/18 13:27:31  lazarus
  Minor changees
  Shane

  Revision 1.9  2001/01/15 18:25:51  lazarus
  Fixed a stupid error I caused by using a variable as an index in main.pp and this variable sometimes caused an exception because the index was out of range.
  Shane

  Revision 1.8  2001/01/12 20:22:09  lazarus
  Shiftstate fixed so it reports ssCtrl and ssShift now.
  You can use Shift-Ctrl-Up and Down to jump to procedures in the code explorer.
  Shane

  Revision 1.7  2001/01/09 18:23:21  lazarus
  Worked on moving controls.  It's just not working with the X and Y coord's I'm getting.
  Shane

  Revision 1.6  2000/12/29 18:33:54  lazarus
  TStatusBar's create and destroy were not set to override TWinControls so they were never called.
  Shane

  Revision 1.5  2000/12/29 13:14:05  lazarus
  Using the lresources.pp and registering components.
  This is a major change but will create much more flexibility for the IDE.
  Shane

  Revision 1.4  2000/12/20 17:35:58  lazarus
  Added GetChildren
  Shane

  Revision 1.3  2000/09/10 23:08:30  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

  Revision 1.16  2000/07/09 20:18:56  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.15  2000/06/28 13:11:37  lazarus
  Fixed TNotebook so it gets page change events.  Shane

  Revision 1.14  2000/06/19 18:21:22  lazarus
  Spinedit was never getting created
  Shane

  Revision 1.13  2000/06/16 13:33:21  lazarus
  Created a new method for adding controls to the toolbar to be dropped onto the form!
  Shane

  Revision 1.12  2000/06/01 21:53:19  lazarus
  MWE:
    + Added check for HandleCreated in CMShowHintChanged

  Revision 1.11  2000/05/27 22:20:55  lazarus
  MWE & VRS:
    + Added new hint code

  Revision 1.10  2000/05/17 22:34:07  lazarus
  MWE:
    * Fixed Sizing & events

  Revision 1.9  2000/05/14 21:56:11  lazarus
  MWE:
    + added local messageloop
    + added PostMessage
    * fixed Peekmessage
    * fixed ClientToScreen
    * fixed Flat style of Speedutton (TODO: Draw)
    + Added TApplicatio.OnIdle

  Revision 1.8  2000/05/10 22:52:58  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.7  2000/05/09 12:52:03  lazarus
  *** empty log message ***

  Revision 1.6  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.5  2000/05/08 16:07:32  lazarus
  fixed screentoclient and clienttoscreen
  Shane

  Revision 1.4  2000/04/10 15:05:30  lazarus
  Modified the way the MOuseCapture works.
  Shane

  Revision 1.2  2000/04/07 16:59:55  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.1  2000/04/02 20:49:57  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.77  2000/03/30 18:07:55  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.76  2000/03/21 23:47:33  lazarus
  MWE:
    + Added TBitmap.MaskHandle & TGraphic.Draw & TBitmap.Draw

  Revision 1.75  2000/03/15 00:51:58  lazarus
  MWE:
    + Added LM_Paint on expose
    + Added forced creation of gdkwindow if needed
    ~ Modified DrawFrameControl
    + Added BF_ADJUST support on DrawEdge
    - Commented out LM_IMAGECHANGED in TgtkObject.IntSendMessage3
       (It did not compile)

  Revision 1.74  2000/03/14 19:49:05  lazarus
  Modified the painting process for TWincontrol.  Now it runs throug it's FCONTROLS list and paints all them
  Shane

  Revision 1.73  2000/03/09 23:48:10  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.72  2000/03/08 23:57:39  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.71  2000/03/03 22:58:27  lazarus
  MWE:
    Fixed focussing problem.
      LM-FOCUS was bound to the wrong signal
    Added GetKeyState api func.
      Now LCL knows if shift/trl/alt is pressed (might be handy for keyboard
      selections ;-)

  Revision 1.70  2000/03/01 00:41:03  lazarus
  MWE:
    Fixed updateshowing problem
    Added some debug code to display the name of messages
    Did a bit of cleanup in main.pp to get the code a bit more readable
      (my editor does funny things with tabs if the indent differs)

  Revision 1.69  2000/02/28 00:15:54  lazarus
  MWE:
    Fixed creation of visible componets at runtime. (when a new editor
      was created it didn't show up)
    Made the hiding/showing of controls more delphi compatible

  Revision 1.68  2000/02/26 23:31:50  lazarus
  MWE:
    Fixed notebook crash on insert
    Fixed loadfont problem for win32 (tleast now a fontname is required)

  Revision 1.67  2000/02/25 19:28:34  lazarus
  Played with TNotebook to see why it crashes when I add a tab and the tnotebook is showing.  Havn't figured it out
  Shane

  Revision 1.66  2000/02/22 23:26:13  lazarus
  MWE: Fixed cursor movement in editor
       Started on focus problem

  Revision 1.65  2000/02/22 22:19:50  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.64  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.63  2000/02/20 20:13:47  lazarus
  On my way to make alignments and stuff work :-)

  Revision 1.62  2000/02/19 18:11:59  lazarus
  More work on moving, resizing, forms' border style etc.

  Revision 1.61  2000/02/18 19:38:53  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.60  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.59  2000/01/10 00:07:13  lazarus
  MWE:
    Added more scrollbar support for TWinControl
    Most signals for TWinContorl are jet connected to the wrong widget
      (now scrolling window, should be fixed)
    Added some cvs entries

  Revision 1.58  2000/01/04 21:00:34  lazarus
  *** empty log message ***

  Revision 1.57  2000/01/03 00:19:21  lazarus
  MWE:
    Added keyup and buttonup events
    Added LM_MOUSEMOVE callback
    Started with scrollbars in editor

  Revision 1.56  2000/01/02 00:29:27  lazarus
  Stoppok:
    - safety check if fCompStyle <> csNone before call to CreateHandle

  Revision 1.55  1999/12/31 14:58:01  lazarus
  MWE:
    Set unkown VK_ codesto 0
    Added pfDevice support for bitmaps

  Revision 1.54  1999/12/23 21:48:13  lazarus
  *** empty log message ***

  Revision 1.52  1999/12/22 01:16:04  lazarus
  MWE:
    Changed/recoded keyevent callbacks
    We Can Edit!
    Commented out toolbar stuff

  Revision 1.51  1999/12/21 21:35:54  lazarus
  committed the latest toolbar code.  Currently it doesn't appear anywhere and I have to get it to add buttons correctly through (I think) setstyle.  I think I'll implement the LM_TOOLBARINSERTBUTTON call there.
  Shane

  Revision 1.50  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.49  1999/12/20 21:01:14  lazarus
  Added a few things for compatability with Delphi and TToolbar
  Shane

  Revision 1.48  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.47  1999/12/14 21:16:26  lazarus
  Added Autosize to TControl
  Shane

  Revision 1.46  1999/12/14 00:16:43  lazarus
  MWE:
    Renamed LM... message handlers to WM... to be compatible and to
      get more edit parts to compile
    Started to implement GetSystemMetrics
    Removed some Lazarus specific parts from mwEdit

  Revision 1.45  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick

  Revision 1.44  1999/12/08 21:42:37  lazarus
  Moved more messages over to wndproc.
  Shane

  Revision 1.43  1999/12/08 00:56:07  lazarus
  MWE:
    Fixed menus. Events aren't enabled yet (dumps --> invalid typecast ??)

  Revision 1.42  1999/12/07 01:19:26  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...


  Revision 1.41  1999/12/03 00:26:47  lazarus
  MWE:
    fixed control location
    added gdiobject reference counter

  Revision 1.40  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.39  1999/11/30 21:30:06  lazarus
  Minor Issues
  Shane

  Revision 1.38  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.37  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.36  1999/11/17 01:16:40  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.35  1999/11/05 17:48:17  lazarus
  Added a mwedit1 component to lazarus (MAIN.PP)
  It crashes on create.
  Shane

  Revision 1.34  1999/11/04 21:52:08  lazarus
  wndproc being used a little
  Shane

  Revision 1.33  1999/11/01 01:28:30  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.32  1999/10/30 17:03:15  lazarus
  MWE: Typo

  Revision 1.31  1999/10/30 16:42:12  lazarus
  MWE: Moved the Parent <> self check to the Parent property

  Revision 1.30  1999/10/30 12:30:02  peter
    * fixed some stupid crashes

  Revision 1.29  1999/10/28 23:48:57  lazarus
  MWE: Added new menu classes and started to use handleneeded

  Revision 1.28  1999/10/28 19:25:10  lazarus
  Added a ton of messaging stuff
  Shane

  Revision 1.27  1999/10/28 17:17:43  lazarus
  Removed references to FCOmponent.
  Shane

  Revision 1.26  1999/10/27 17:27:08  lazarus
  Added alot of changes and TODO: statements
  shane

  Revision 1.25  1999/10/25 21:07:49  lazarus
  Many changes for compatability made again..

  Shane

  Revision 1.24  1999/10/25 17:38:52  lazarus
  More stuff added for compatability.  Most stuff added was put in the windows.pp file.  CONST scroll bar messages and such.  2 functions were also added to that unit that needs to be completed.
  Shane

  Revision 1.23  1999/10/25 15:33:54  lazarus
  Added a few more procedures for compatability.
  Shane

  Revision 1.22  1999/10/22 18:56:36  lazarus
  Fixed a linking error in wincontrol.inc
  Shane

  Revision 1.21  1999/10/22 18:39:43  lazarus
  Added kEYUP- KeyPress - Keydown, etc.

  Shane

  Revision 1.20  1999/10/20 21:08:16  lazarus
  added OnDblClick, OnShowHint, OnParentShowHint, etc for compatability.

  Revision 1.18  1999/09/30 21:59:03  lazarus
  MWE: Fixed TNoteBook problems
       Modifications: A few
       - Removed some debug messages
       + Added some others
       * changed fixed widged of TPage. Code is still broken.
       + TWinControls are also added to the Controls collection
       + Added TControl.Controls[] property

  Revision 1.17  1999/09/26 13:30:15  lazarus

     Implemented OnEnter & OnExit events for TTrackbar. These properties
     and handler functions have been added to TWincontrol, two new
     callbacks have been added to gtkcallback.
      stoppok

  Revision 1.16  1999/09/17 23:12:58  lazarus
  *** empty log message ***

  Revision 1.15  1999/09/15 03:17:32  lazarus
  Changes to Editor.pp
  If the text was actually displayed, then it would work better.  :-)

  Revision 1.14  1999/09/15 02:14:44  lazarus
  *** empty log message ***

  Revision 1.13  1999/09/11 12:16:16  lazarus
  Fixed a bug in key press evaluation. Initial cut at Invalidate problem.

  Revision 1.12  1999/08/26 23:36:03  peter
    + paintbox
    + generic keydefinitions and gtk conversion
    * gtk state -> shiftstate conversion

  Revision 1.11  1999/08/24 21:26:53  lazarus
  *** empty log message ***

  Revision 1.9  1999/08/16 15:48:50  lazarus
  Changes by file:
       Control: TCOntrol-Function GetRect added
                         ClientRect property added
                TImageList - Added Count
                TWinControl- Function Focused added.
      Graphics: TCanvas - CopyRect added - nothing finished on it though
                          Draw added - nothing finiushed on it though
                clbtnhighlight and clbtnshadow added.  Actual color values not right.
               IMGLIST.PP and IMGLIST.INC files added.

   A few other minor changes for compatability added.

    Shane

  Revision 1.8  1999/08/12 18:36:58  lazarus
  Added a bunch of "stuff" for compatablility.  Not sure if it'll all compile yet, will look at that shortly.

  Revision 1.7  1999/08/11 20:41:35  lazarus

  Minor changes and additions made.  Lazarus may not compile due to these changes

  Revision 1.6  1999/08/07 17:59:25  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new SendMessage function.   MAH

  Revision 1.5  1999/07/31 14:27:04  peter
    * mouse fixes
    * wheel support

  Revision 1.4  1999/07/31 06:39:32  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
