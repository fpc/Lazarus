{******************************************************************************
                                  TClipBoard
 ******************************************************************************}

destructor TClipboard.Destroy;
begin
  FData.Free;
  FData := nil;
  inherited Destroy;
end;

procedure TClipboard.Assign(Source: TPersistent);
begin
  AsText := '';

  if not Assigned(FData) then
     FData := TMemoryStream.Create;
  FData.Write(Source, SizeOf(Source));

  FFormat := CF_OBJECT;
  FUseText := False;
  FUseComponent := False;
end;

procedure TClipboard.Clear;
begin
  AsText := '';
  FData.Free;
  FData := nil;
end;

procedure TClipboard.Close;
begin
  Locked := False;
end;

{------------------------------------------------------------------------------
  Function: TClipBoard.GetAsText
  Params:
  Returns:  String


 ------------------------------------------------------------------------------}
function TClipboard.GetAsText: String;
begin
  if FUseText then
     Result := FText
  else
     Result := '';
end;

Function TClipboard.GetAsHandle(Format: Word): Integer;
begin
  // Not implemented yet!
  Result := 0;
end;

Function TClipboard.GetComponent(Owner, Parent: TComponent): TComponent;
begin
  Result := nil;
  if (not FUseComponent) or (not Assigned(FData)) then Exit;

  Open;
  try
     Result := TComponent(FData);
     Owner.InsertComponent(Result);
  finally
     Close;
  end;
end;

Function TClipboard.GetData: TPersistent;
begin
  Result := nil;
  if FUseText = False then
  begin
     if Assigned(FData) then
        if Assigned(FData.Memory) then
           Result := TPersistent(FData.Memory^)
  end;
end;

Function TClipboard.GetFormatCount: Integer;
begin
  // Not implemented yet!
  Result := 0;
end;

Function TClipboard.GetFormats(Value: Integer): Word;
begin
  // Not implemented yet!
  Result := 0;
end;

Function TClipboard.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
begin
  Result := 0;
  if FUseText then
  begin
     if (Length(AsText)+1) > BufSize then begin
        StrPLCopy(Buffer, AsText, BufSize-1);
        Result := BufSize-1;
     end else begin
        StrPLCopy(Buffer, AsText, Length(AsText));
        Result := Length(AsText);
     end;
  end else
     StrPLCopy(Buffer, '', BufSize-1);
end;

{------------------------------------------------------------------------------
  Function: TClipBoard.HasFormat
  Params:   Format
  Returns:  Boolean

  Used to determine what the clipboard currently holds.
 ------------------------------------------------------------------------------}
function TClipboard.HasFormat(Format: Word): Boolean;
begin
  Result := FFormat = Format;
end;

procedure TClipboard.Open;
begin
  Locked := True;
end;

procedure TClipboard.SetAsHandle(Format: Word; Value: Integer);
begin
  // Not implemented yet!
end;

{------------------------------------------------------------------------------
  Method: TClipBoard.SetAsText
  Params:   Value
  Returns:  None

  Set the clipboard value to a text string
 ------------------------------------------------------------------------------}
procedure TClipboard.SetAsText(Value: String);
begin
  if Locked then Exit;
  FUseText := True;
  FFormat := CF_TEXT;
  FText := Value;
  FUseComponent := False;
end;

procedure TClipboard.SetComponent(Component: TComponent);
begin
  Assign(Component);
  FUseComponent := True;
end;

{------------------------------------------------------------------------------
  Method:  TClipBoard.SetTextBuf
  Params:  Buffer - Text
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TClipboard.SetTextBuf(Buffer: PChar);
begin
  AsText := Buffer;
end;

{
  $Log$
  Revision 1.2  2001/08/07 11:05:51  lazarus
  MG: small bugfixes

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.7  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.6  2000/02/28 21:08:09  lazarus
  MWE:
    Converted to unix files :-)

  Revision 1.5  2000/02/28 21:05:27  lazarus
  MWE:
    Committed the changes for TClipbrd made by
    Hongli Lai <hongli@telekabel.nl>:
      Initial development. The "old" TClipboard doesn't get updated,
      so I started to write a new one.

  Revision 1.4  1999/10/25 17:38:51  lazarus
  More stuff added for compatability.  Most stuff added was put in the windows.pp file.  CONST scroll bar messages and such.  2 functions were also added to that unit that needs to be completed.
  Shane

  Revision 1.3  1999/10/25 15:33:54  lazarus
  Added a few more procedures for compatability.
  Shane

  Revision 1.2  1999/10/22 19:20:02  lazarus
  Added a clipboard function to create a default clipboard
  Shane

  Revision 1.1  1999/10/19 19:59:15  lazarus
  Added clipbrd.pp and clipbrd.inc.  Nothing done to them yet.
  Shane

  Revision 1.1  1999/08/12 16:22:19  lazarus

}
