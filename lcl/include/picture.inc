// included by graphics.pp

{ TPicture and help classes TPictureFileFormatList}

type
  PPicFileFormat = ^TPicFileFormat;
  TPicFileFormat = record
    GraphicClass: TGraphicClass;
    Extension: string;
    Description: string;
  end;

  TPicFileFormatsList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Ext, Desc: String; AClass: TGraphicClass);
    function FindExt(Ext: string): TGraphicClass;
    function FindClassName(const AClassname: string): TGraphicClass;
    procedure Remove(AClass: TGraphicClass);
    procedure BuildFilterStrings(GraphicClass: TGraphicClass;
      var Descriptions, Filters: string);
  end;

constructor TPicFileFormatsList.Create;
begin
  inherited Create;
  Add('bmp', 'Bitmaps', TBitmap);
  Add('xpm', 'Pixmap', TPixmap);
  Add('ico', 'Icon', TIcon);
end;

destructor TPicFileFormatsList.Destroy;
var
  I: Integer;
  p: PPicFileFormat;
begin
  for I := 0 to Count-1 do begin
    p:=PPicFileFormat(Pointer(Items[I]));
    Dispose(p);
  end;
  inherited Destroy;
end;

procedure TPicFileFormatsList.Add(const Ext, Desc: String;
  AClass: TGraphicClass);
var
  NewRec: PPicFileFormat;
begin
  New(NewRec);
  with NewRec^ do begin
    Extension := AnsiLowerCase(Ext);
    GraphicClass := AClass;
    Description := Desc;
  end;
  inherited Add(NewRec);
end;

function TPicFileFormatsList.FindExt(Ext: string): TGraphicClass;
var
  I: Integer;
begin
  Ext := AnsiLowerCase(Ext);
  for I := Count-1 downto 0 do
    with PPicFileFormat(Items[I])^ do
      if Extension = Ext then
      begin
        Result := GraphicClass;
        Exit;
      end;
  Result := nil;
end;

function TPicFileFormatsList.FindClassName(
  const AClassName: string): TGraphicClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
  begin
    Result := PPicFileFormat(Items[I])^.GraphicClass;
    if AnsiLowerCase(Result.ClassName) = AnsiLowerCase(AClassname) then
      Exit;
  end;
  Result := nil;
end;

procedure TPicFileFormatsList.Remove(AClass: TGraphicClass);
var
  I: Integer;
  P: PPicFileFormat;
begin
  for I := Count-1 downto 0 do
  begin
    P := PPicFileFormat(Items[I]);
    if P^.GraphicClass.InheritsFrom(AClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

procedure TPicFileFormatsList.BuildFilterStrings(GraphicClass: TGraphicClass;
  var Descriptions, Filters: string);
var
  C, I: Integer;
  P: PPicFileFormat;
begin
  Descriptions := '';
  Filters := '';
  C := 0;
  for I := Count-1 downto 0 do begin
    P := PPicFileFormat(Items[I]);
    if P^.GraphicClass.InheritsFrom(GraphicClass) and (P^.Extension <> '') then
      with P^ do
      begin
        if C <> 0 then
        begin
          Descriptions := Descriptions + '|';
          Filters := Filters + ';';
        end;
        FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s',
             [Descriptions, Description, Extension]);
        FmtStr(Filters, '%s*.%s', [Filters, Extension]);
        Inc(C);
      end;
  end;
  if C > 1 then
    FmtStr(Descriptions, '%s (%s)|%1:s|%s',
     ['All files', Filters, Descriptions]);
end;

const
  PictureFileFormats: TPicFileFormatsList = nil;

function GetPicFileFormats: TPicFileFormatsList;
begin
  if PictureFileFormats = nil then 
    PictureFileFormats := TPicFileFormatsList.Create;
  Result := PictureFileFormats;
end;

{function GetPicClipboardFormats: TPicClipboardFormats;
begin
  if PicClipboardFormats = nil then
    PicClipboardFormats := TPicClipboardFormats.Create;
  Result := PicClipboardFormats;
end;}

//--TPicture--------------------------------------------------------------------


constructor TPicture.Create;
begin
  inherited Create;
  GetPicFileFormats;
  //GetClipboardFormats;
end;

destructor TPicture.Destroy;
begin
  FGraphic.Free;
  inherited Destroy;
end;

procedure TPicture.AssignTo(Dest: TPersistent);
begin
  if Graphic is Dest.ClassType then
    Dest.Assign(Graphic)
  else
    inherited AssignTo(Dest);
end;

procedure TPicture.ForceType(GraphicType: TGraphicClass);
begin
  if not (Graphic is GraphicType) then
  begin
    FGraphic.Free;
    FGraphic := nil;
    FGraphic := GraphicType.Create;
    FGraphic.OnChange := @Changed;
    FGraphic.OnProgress := @Progress;
    Changed(Self);
  end;
end;

function TPicture.GetBitmap: TBitmap;
begin
  ForceType(TBitmap);
  Result := TBitmap(Graphic);
end;

function TPicture.GetPixmap: TPixmap;
begin
  ForceType(TPixmap);
  Result := TPixmap(Graphic);
end;

function TPicture.GetIcon: TIcon;
begin
  ForceType(TIcon);
  Result := TIcon(Graphic);
end;

procedure TPicture.SetBitmap(Value: TBitmap);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetPixmap(Value: TPixmap);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetIcon(Value: TIcon);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetGraphic(Value: TGraphic);
var
  NewGraphic: TGraphic;
begin
  NewGraphic := nil;
  if Value <> nil then
  begin
    NewGraphic := TGraphicClass(Value.ClassType).Create;
    NewGraphic.Assign(Value);
    NewGraphic.OnChange := @Changed;
    NewGraphic.OnProgress := @Progress;
  end;
  try
    FGraphic.Free;
    FGraphic := NewGraphic;
    Changed(Self);
  except
    NewGraphic.Free;
    raise;
  end;
end;

{ Based on the extension of Filename, create the corresponding TGraphic class
  and call its LoadFromFile method. }

procedure TPicture.LoadFromFile(const Filename: string);
var
  Ext: string;
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
  ok: boolean;
begin
  Ext := ExtractFileExt(Filename);
  System.Delete(Ext, 1, 1); // delete '.'
  GraphicClass := GetPicFileFormats.FindExt(Ext);
  if GraphicClass = nil then
    raise EInvalidGraphic.CreateFmt('Unknown picture extension', [Ext]);

  NewGraphic := GraphicClass.Create;
  ok:=false;
  try
    NewGraphic.OnProgress := @Progress;
    NewGraphic.LoadFromFile(Filename);
    ok:=true;
  finally
    // this try..finally construction will in case of an exception
    // not alter the error backtrace output
    if not ok then NewGraphic.Free;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  FGraphic.OnChange := @Changed;
  Changed(Self);
end;

procedure TPicture.SaveToFile(const Filename: string);
begin
  if FGraphic <> nil then FGraphic.SaveToFile(Filename);
end;

procedure TPicture.LoadFromClipboardFormat(FormatID: TClipboardFormat);
{var
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;}
begin
{  GraphicClass := ClipboardFormats.FindFormat(FormatID);
  if GraphicClass = nil then
    InvalidGraphic(@SUnknownClipboardFormat);

  NewGraphic := GraphicClass.Create;
  try
    NewGraphic.OnProgress := Progress;
    NewGraphic.LoadFromClipboardFormat(AFormat, AData, APalette);
  except
    NewGraphic.Free;
    raise;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  FGraphic.OnChange := Changed;
  Changed(Self);}
end;

procedure TPicture.SaveToClipboardFormat(FormatID: TClipboardFormat);
begin

end;

function TPicture.SupportsClipboardFormat(FormatID: TClipboardFormat): Boolean;
begin
  Result:=false;
end;

procedure TPicture.Assign(Source: TPersistent);
begin
  if Source = nil then
    SetGraphic(nil)
  else if Source is TPicture then
    SetGraphic(TPicture(Source).Graphic)
  else if Source is TGraphic then
    SetGraphic(TGraphic(Source))
  else
    inherited Assign(Source);
end;

class procedure TPicture.RegisterFileFormat(const AnExtension,
  ADescription: string; AGraphicClass: TGraphicClass);
begin
  GetPicFileFormats.Add(AnExtension, ADescription, AGraphicClass);
end;

procedure TPicture.RegisterClipboardFormat(FormatID: TClipboardFormat;
  AGraphicClass: TGraphicClass);
begin

end;

class procedure TPicture.UnRegisterGraphicClass(AClass: TGraphicClass);
begin
  if GetPicFileFormats <> nil then GetPicFileFormats.Remove(AClass);
  //if ClipboardFormats <> nil then ClipboardFormats.Remove(AClass);
end;

procedure TPicture.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPicture.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TPicture.ReadData(Stream: TStream);
var
  GraphicClassName: Shortstring;
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
begin
  Stream.Read(GraphicClassName[0], 1);
  Stream.Read(GraphicClassName[1], length(GraphicClassName));
  GraphicClass := GetPicFileFormats.FindClassName(GraphicClassName);
  NewGraphic := nil;
  if GraphicClass <> nil then
  begin
    NewGraphic := GraphicClass.Create;
    try
      NewGraphic.ReadData(Stream);
    except
      NewGraphic.Free;
      raise;
    end;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  if NewGraphic <> nil then
  begin
    NewGraphic.OnChange := @Changed;
    NewGraphic.OnProgress := @Progress;
  end;
  Changed(Self);
end;

procedure TPicture.WriteData(Stream: TStream);
var
  GraphicClassName: ShortString;
begin
  with Stream do
  begin
    if Graphic <> nil then
      GraphicClassName := Graphic.ClassName
    else
      GraphicClassName := '';
    Write(GraphicClassName, Length(GraphicClassName) + 1);
    if Graphic <> nil then
      Graphic.WriteData(Stream);
  end;
end;

procedure TPicture.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  var
    Ancestor: TPicture;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TPicture then
      begin
        Ancestor := TPicture(Filer.Ancestor);
        Result := not ((Graphic = Ancestor.Graphic) or
          ((Graphic <> nil) and (Ancestor.Graphic <> nil) and
          Graphic.Equals(Ancestor.Graphic)));
      end;
    end
    else Result := Graphic <> nil;
  end;

begin
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData, DoWrite);
end;

function TPicture.GetWidth: Integer;
begin
  if FGraphic <> nil then
    Result := FGraphic.Width
  else
    Result := 0;
end;

function TPicture.GetHeight: Integer;
begin
  Result := 0;
  if FGraphic <> nil then
    Result := FGraphic.Height
  else
    Result := 0;
end;

// included by graphics.pp

