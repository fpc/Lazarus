{%MainUnit ../dbctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TDBImage }

function TDBImage.GetDataField: string;
begin
  Result:=FDataLink.FieldName;
end;

function TDBImage.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBImage.GetField: TField;
begin
  Result:=FDataLink.Field;
end;
procedure TDBImage.Change;
begin
  //need to override this to make sure the datalink gets notified
  //its been modified, then when post etc, it will call
  //updatedata to update the field data with current value
  FDataLink.Modified;
end;

function TDBImage.GetReadOnly: Boolean;
begin
  Result:=FDataLink.ReadOnly;
end;

procedure TDBImage.SetAutoDisplay(const AValue: Boolean);
begin
  if FAutoDisplay=AValue then exit;
  FAutoDisplay:=AValue;
  if FAutoDisplay then LoadPicture;
end;

procedure TDBImage.SetDataField(const AValue: string);
begin
  FDataLink.FieldName:=AValue;
end;

procedure TDBImage.SetDataSource(const AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self,FDataLink,AValue);
end;

procedure TDBImage.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly:=AValue;
end;

procedure TDBImage.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TDBImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TDBImage.DataChange(Sender: TObject);
begin
  FUpdatingRecord := True;
  Picture.Graphic:=nil;
  FPictureLoaded:=False;
  if AutoDisplay then LoadPicture;
  FUpdatingRecord := False;
end;

procedure TDBImage.UpdateData(Sender: TObject);

var s        : Tstream;
    fe       : String;
    i        : Integer;

begin
  if not assigned(Picture.Graphic) or (Picture.Graphic.Empty) then
    begin
    FDataLink.Field.Clear;
    end
  else
    begin
    fe := Picture.Graphic.GetFileExtensions;
    s := FDataLink.DataSet.CreateBlobStream(FDataLink.Field,bmwrite);
    try
      i := pos(';',fe);
      if i > 0 then fe := copy(fe,1,i-1);
      if assigned(FOnDBImageWrite)  then
        OnDBImageWrite(self,s,fe) //Call extermal method to save type of image
      else
        begin
        if FWriteHeader then
          s.WriteAnsiString(fe);  //otherwise write file extension to stream
        end;
      Picture.Graphic.SaveToStream(s);
    finally
      s.Free;
    end;
    end;
end;

procedure TDBImage.PictureChanged(Sender: TObject);
begin
  Inherited;
  if not FUpdatingRecord then
    Change;
end;

procedure TDBImage.LoadPicture;

var s        : Tstream;
    GraphExt : string;
    gc       : TGraphicClass;
    AGraphic : TGraphic;
    CurPos   : Int64;
    
    function LoadImageFromStream: boolean;
    begin
      result := (s<>nil);
      if result then
        try
          curPos := s.Position;
          Picture.LoadFromStream(s);
        except
          s.Position := Curpos;
          result := false;
        end;
    end;

    procedure GraphExtToClass;
    begin
      gc := GetGraphicClassForFileExtension(GraphExt);
    end;

    procedure ReadImageHeader;
    begin
      CurPos := s.Position;
      try
        GraphExt := s.ReadAnsiString;
      except
        s.Position := CurPos;
        GraphExt := '';
      end;
      GraphExtToClass;
      if gc=nil then
        s.Position := CurPos;
    end;

begin
  if not FPictureLoaded then
  begin
    FUpdatingRecord := True;
    if not assigned(FDatalink.Field) then Picture.Assign(FDatalink.Field)
    else
    if FDatalink.field.IsBlob then
      begin
      if FDatalink.field is TBlobField then
        begin

        if FDatalink.Field.IsNull then
          begin
          Picture.Clear;
          exit;
          end;

        s := FDataLink.DataSet.CreateBlobStream(FDataLink.Field,bmRead);
        if (S=Nil) or (s.Size = 0) then
          begin
          if s<>nil then
            s.Free;
          Picture.Clear;
          exit;
          end;

        try
          AGraphic := nil;
          GraphExt := '';
          if assigned(FOnDBImageRead) then
          begin
            // External method to identify graphic type
            // returns file extension for graphic type (e.g. jpg)
            // If user implements OnDBImageRead, the control assumes that
            // the programmer either:
            //
            // -- Returns a valid identifier that matches a graphic class and
            //    the remainder of stream contains the image data. An instance of
            //    of graphic class will be used to load the image data.
            // or
            // -- Returns an invalid identifier that doesn't match a graphic class
            //    and the remainder of stream contains the image data. The control
            //    will try to load the image trying to identify the format
            //    by it's content
            //
            // In particular, returning an invalid identifier while the stream has
            // a image header will not work.
            OnDBImageRead(self,s,GraphExt);
            GraphExtToClass;
          end
          else
            ReadImageHeader;

          if gc<>nil then
          begin
            AGraphic := gc.Create;
            AGraphic.LoadFromStream(s);
            Picture.Assign(AGraphic);
            FPictureLoaded:=true;
          end
          else
          begin
            if LoadImageFromStream then
              FPictureLoaded:=true
            else
              Picture.Clear;
          end;

        finally
          AGraphic.Free;
          s.Free;
        end {try}

        end
      else
        Picture.Assign(FDataLink.FField);
        
      end;
    FUpdatingRecord := False;
    end;
end;

procedure TDBImage.CopyToClipboard;
begin
  if Assigned(Picture.Graphic) then
    DoCopyToClipboard;
end;

procedure TDBImage.CutToClipboard;
begin
  if Assigned(Picture.Graphic) then
  begin
    if FDataLink.Edit then
    begin
      DoCopyToClipboard;
      Picture.Clear;
    end;
  end;
end;

procedure TDBImage.PasteFromClipboard;
begin
  if Clipboard.HasPictureFormat then
    if FDataLink.Edit then
      // TODO: add some option to convert image to an user preferred format?
      Clipboard.AssignTo(Picture);
end;

class procedure TDBImage.WSRegisterClass;
const
  Done: Boolean = False;
begin
  if Done then
    Exit;
  inherited WSRegisterClass;
  RegisterPropertyToSkip(TDBImage, 'Picture', 'Removed in 0.9.29. DB control should not save/load their data from stream.', '');
  Done := True;
end;

procedure TDBImage.DoCopyToClipboard;
  procedure AddGraphicFormat(fmt: TClipboardFormat; aClass: TGraphicClass);
  var
    st: TMemoryStream;
    gp: TGraphic;
  begin
    if not (Picture.Graphic is aClass) then
    begin
      st := TMemoryStream.Create;
      gp := aClass.Create;
      try
        gp.assign(Picture.Graphic);
        gp.SaveToStream(st);
        st.Position := 0;
        Clipboard.Open;
        Clipboard.AddFormat(fmt, st);
        Clipboard.Close;
      finally
        st.free;
        gp.free;
      end;
    end;
  end;
begin
  Clipboard.Assign(Picture);
  {$IFDEF MSWINDOWS}
  AddGraphicFormat(CF_BITMAP, TBitmap);
  {$ELSE}
  // TODO: check: under linux most apps seems to understand png clipboard format
  //              under osx it will probably be tiff
  AddGraphicFormat(ClipboardRegisterFormat('image/png'), TPortableNetworkGraphic);
  {$ENDIF}
end;

constructor TDBImage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle+[csReplicatable];
  FAutoDisplay:=True;
  FQuickDraw:=true;
  FWriteHeader:=True;
  FDataLink:=TFieldDataLink.Create;
  FDataLink.Control:=Self;
  FDataLink.OnDataChange:=@DataChange;
  FDataLink.OnUpdateData:=@UpdateData;
  FUpdatingRecord := False;
end;

destructor TDBImage.Destroy;
begin
  FDataLink.Destroy;
  inherited Destroy;
end;

function TDBImage.ExecuteAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(AAction) or
            (FDataLink <> nil) and FDataLink.ExecuteAction(AAction);
end;

function TDBImage.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(AAction) or
            (FDataLink <> nil) and FDataLink.UpdateAction(AAction);
end;

// included by dbctrls.pp
