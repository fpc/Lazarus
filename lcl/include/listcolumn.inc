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

{------------------------------------------------------------------------------}
{   TListColumn                                                                }
{------------------------------------------------------------------------------}

procedure TListColumn.Assign(ASource: TPersistent);
var
  Col: TListColumn;
begin
  if ASource is TListColumn 
  then begin
    Col := TListColumn(ASource);
    FAlignment := Col.Alignment;
    FAutoSize := Col.AutoSize;
    FCaption := Col.Caption;
    FMaxWidth := Col.MaxWidth;
    FMinWidth := Col.MinWidth;
    FVisible := Col.Visible;
    FWidth := Col.Width;
    Changed(False);
  end
  else inherited Assign(ASource);
end;

constructor TListColumn.Create(ACollection: TCollection);
begin
  inherited;
  FAlignment := taLeftJustify;
  FCaption := '';
  FWidth := 50;
  FVisible := True;
  FMinWidth := 0;
  FMaxWidth := 0;
  FAutoSize := False;
  FTag := 0;         
  Changed(False);  
end;

destructor TListColumn.Destroy;
begin
  inherited;
  Changed(False);  
end;

function TListColumn.GetWidth: TWidth;
begin
// TODO: read actual width  
  Result := FWidth;
end;

procedure TListColumn.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment = AValue then Exit;
 
  FAlignment := AValue;
  Changed(False);  
end;

procedure TListColumn.SetCaption(const AValue: String);
begin
  if AValue = FCaption then Exit;
  FCaption := AValue;
  Changed(False);  
end;

procedure TListColumn.SetWidth(const AValue: TWidth);
begin
  if FWidth = AValue then Exit;

  FWidth := AValue;
  Changed(False);  
end;

procedure TListColumn.SetMaxWidth(const AValue: TWidth);
begin
  if FMaxWidth = AValue then Exit;

  FMaxWidth := AValue;
  Changed(False);  
end;

procedure TListColumn.SetMinWidth(const AValue: TWidth);
begin
  if FMinWidth = AValue then Exit;

  FMinWidth := AValue;
  Changed(False);  
end;

procedure TListColumn.SetAutoSize(const AValue: Boolean);
begin
  if FAutoSize = AValue then Exit;

  FAutoSize := AValue;
  Changed(False);  
end;

procedure TListColumn.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then Exit;

  FVisible := AValue;
  Changed(False);  
end;

{ =============================================================================

  $Log$
  Revision 1.4  2002/10/09 11:46:04  lazarus
  MG: fixed loading TListView from stream

  Revision 1.3  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.2  2002/03/23 15:49:22  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

  Revision 1.1  2002/03/12 23:55:37  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

}

