{%MainUnit ../forms.pp}

{ TWindowMagnetManager

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TWindowMagnetOptions }

constructor TWindowMagnetOptions.Create;
begin
  inherited;
  FSnapMonitor:= false;
  FSnapForms:= false;
  FSnapFormTarget:= true;
  FDistance:= 10;
end;

procedure TWindowMagnetOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TWindowMagnetOptions then
    with TWindowMagnetOptions(Dest) do begin
      Distance:= Self.Distance;
      SnapToMonitor:= Self.SnapToMonitor;
      SnapToForms:= Self.SnapToForms;
      SnapFormTarget:= Self.SnapFormTarget;
    end;
end;

{ TWindowMagnetManager }

function PosToRect(wp: TWindowPos): TRect;
begin
  Result.Left:= wp.x;
  Result.Top:= wp.y;
  Result.Right:= wp.x + wp.cx;
  Result.Bottom:= wp.y + wp.cy;
end;

constructor TWindowMagnetManager.Create;
begin
  inherited Create;
  FEnabled:= true;
  FActiveForm:= nil;
  FPreviousSource:= Default(TWindowPos);
  FPreviousReturn:= Default(TWindowPos);
  FCompositorBorders:= Rect(0, 0, 0, 0);
end;

function TWindowMagnetManager.SnapForm(Form: TCustomForm; var WindowPos: TWindowPos): boolean;
var
  SrcPos: TWindowPos;
begin
  Result:= false;
  // FIXME: compiler bug? generates nonsensical comps if done in one statement
  if not FEnabled then Exit;
  if ((WindowPos.flags and SWP_SHOWWINDOW) > 0) then Exit;
  if ((WindowPos.flags and SWP_HIDEWINDOW) > 0) then Exit;
  if (Form.ComponentState * [csDestroying, csDesigning, csLoading] <> []) then Exit;
  if not (Form.SnapOptions.SnapToMonitor or Form.SnapOptions.SnapToForms) then Exit;

  SrcPos:= WindowPos;
  if Form <> FActiveForm then begin
    FActiveForm:= Form;
    FPreviousSource:= WindowPos;
    FPreviousReturn:= WindowPos;
    Exit;
  end;

  if not GetCompositorExtendedBorder(Form.Handle, FCompositorBorders) then
    FCompositorBorders:= Rect(0, 0, 0, 0);

  Result:= SnapToMonitor(Form.SnapOptions, WindowPos) or
           SnapToForms(Form.SnapOptions, WindowPos);

  FPreviousSource:= SrcPos;
  FPreviousReturn:= WindowPos;
end;

function TWindowMagnetManager.SnapToSides(var x, cx: integer; px, pcx, pxout, pcxout: integer; dist: integer; leftsides, rightsides: TIntegerDynArray): boolean;
var
  Moving, Resizing: Boolean;
  goal: Integer;
begin
  Result:= False;
  Moving:= x <> px;
  Resizing:= cx <> pcx;

  try
    if Resizing and Moving then begin
      // Resizing Left Edge
      for goal in leftsides do begin
        Result:= Abs(x - goal) < dist;
        if Result then begin
          Inc(cx, x - goal);
          x:= goal;
          Exit;
        end;
      end;
    end else
    if Resizing then begin
      // Resizing Right Edge
      for goal in rightsides do begin
        Result:= Abs(x + cx - goal) < dist;
        if Result then begin
          cx:= goal - x;
          Exit;
        end;
      end;
    end else
    if Moving then begin
      // Moving Left Edge
      for goal in leftsides do begin
        Result:= Abs(x - goal) < dist;
        if Result then begin
          x:= goal;
          Exit;
        end;
      end;
      // Moving Right Edge
      for goal in rightsides do begin
        Result:= Abs(x + cx - goal) < dist;
        if Result then begin
          x:= goal - cx;
          Exit;
        end;
      end;
    end else begin
      // No change on this axis
      x:= pxout;
      cx:= pcxout;
    end;
  finally
    Result:= Result and ((x<>pcxout) or (cx<>pcxout));
  end;
end;

function TWindowMagnetManager.SnapToMonitor(Opts: TWindowMagnetOptions; var WindowPos: TWindowPos): boolean;
var
  Rect, Area: TRect;
begin
  Result:= False;
  if not Opts.SnapToMonitor then
    Exit;
  Rect:= PosToRect(WindowPos);
  Area:= Screen.MonitorFromRect(Rect).WorkareaRect;
{$push}
{$BoolEval on} // do not short-circuit, SnapToSides has sideffects that must take place either way
  Result:= SnapToSides(WindowPos.x, WindowPos.cx, FPreviousSource.x, FPreviousSource.cx, FPreviousReturn.x, FPreviousReturn.cx,
                       Opts.Distance, [Area.Left - FCompositorBorders.Left], [Area.Right + FCompositorBorders.Right]) or
           SnapToSides(WindowPos.y, WindowPos.cy, FPreviousSource.y, FPreviousSource.cy, FPreviousReturn.y, FPreviousReturn.cy,
                       Opts.Distance, [Area.Top - FCompositorBorders.Top], [Area.Bottom + FCompositorBorders.Bottom]);
{$pop}
end;

function TWindowMagnetManager.SnapToForms(Opts: TWindowMagnetOptions; var WindowPos: TWindowPos): boolean;

  procedure Append(Item: integer; var List: TIntegerDynArray);
  begin
    Insert(Item, List, Length(List));
  end;

  procedure AddGoals(wtop, wbot, otop, obot, oleft, oright, mleft, mright,
                     btop, bbot, bleft, bright: integer;
                     var snapleft, snapright, snapup, snapdown: TIntegerDynArray);
  begin
    // is there any true overlap?
    if (wtop + btop < obot - bbot) and (wbot - bbot > otop + btop) then begin
      // add borders in order "apparent +- mine +- theirs"
      Append(oleft + bright + bleft, snapright);
      Append(oright - bleft - bright, snapleft);
      if (oleft + bleft = mright - bright) or
         (oright - bright = mleft + bleft) then begin
        // already magnetised on L-R axis, include top-top/bottom-bottom align goals
        Append(otop, snapup);
        Append(obot, snapdown);
      end;
    end;
  end;

var
  Rect, br, PrevRect: TRect;
  SnapTo: array[0..3] of TIntegerDynArray;    // CSS order: t,r,b,l
  fi: Integer;
  frm: TCustomForm;
begin
  Result:= False;
  if not Opts.SnapToForms then
    Exit;
  Rect:= PosToRect(WindowPos);
  PrevRect:= PosToRect(FPreviousReturn);

  SnapTo[0]:= nil;
  SnapTo[1]:= nil;
  SnapTo[2]:= nil;
  SnapTo[3]:= nil;

  for fi:= 0 to Screen.CustomFormCount - 1 do begin
    frm:= Screen.CustomForms[fi];
    if frm.Handle = WindowPos.hwnd then continue;
    if not frm.SnapOptions.SnapFormTarget then continue;
    if GetWindowRect(frm.Handle, br) = 0 then continue;
    // Left-Right snapping
    AddGoals(Rect.Top, Rect.Bottom, br.Top, br.Bottom, br.Left, br.Right, PrevRect.Left, PrevRect.Right,
             FCompositorBorders.Top, FCompositorBorders.Bottom, FCompositorBorders.Left, FCompositorBorders.Right,
             SnapTo[3], SnapTo[1], SnapTo[0], SnapTo[2]);
    // Up-Down snapping
    AddGoals(Rect.Left, Rect.Right, br.Left, br.Right, br.Top, br.Bottom, PrevRect.Top, PrevRect.Bottom,
             FCompositorBorders.Left, FCompositorBorders.Right, FCompositorBorders.Top, FCompositorBorders.Bottom,
             SnapTo[0], SnapTo[2], SnapTo[3], SnapTo[1]);
  end;

{$push}
{$BoolEval on} // do not short-circuit, SnapToSides has sideffects that must take place either way
  Result:= SnapToSides(WindowPos.x, WindowPos.cx, FPreviousSource.x, FPreviousSource.cx, FPreviousReturn.x, FPreviousReturn.cx, Opts.Distance, SnapTo[3], SnapTo[1]) or
           SnapToSides(WindowPos.y, WindowPos.cy, FPreviousSource.y, FPreviousSource.cy, FPreviousReturn.y, FPreviousReturn.cy, Opts.Distance, SnapTo[0], SnapTo[2]);
{$pop}
end;


{ TCustomForm }

function TCustomForm.GetScreenSnap: boolean;
begin
  Result:= FSnapOptions.SnapToMonitor;
end;

function TCustomForm.GetSnapBuffer: integer;
begin
  Result:= FSnapOptions.Distance;
end;

procedure TCustomForm.SetScreenSnap(aValue: boolean);
begin
  FSnapOptions.SnapToMonitor:= aValue;
end;

procedure TCustomForm.SetSnapBuffer(aValue: integer);
begin
  FSnapOptions.Distance:= aValue;
end;

procedure TCustomForm.SetSnapOptions(aValue: TWindowMagnetOptions);
begin
  if FSnapOptions=aValue then Exit;
  FSnapOptions.Assign(aValue);
end;

procedure TCustomForm.WMWindowPosChanging(var Message: TLMWindowPosChanging);
begin
  Screen.MagnetManager.SnapForm(Self, Message.WindowPos^);
end;


// included by forms.pp
