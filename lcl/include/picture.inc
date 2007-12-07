{%MainUnit ../graphics.pp}

{ TPicture and help classes TPictureFileFormatList

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

type
  { TPicFileFormatsList }

  PPicFileFormat = ^TPicFileFormat;
  TPicFileFormat = record
    GraphicClass: TGraphicClass;
    Extension: string;
    Description: string;
  end;

  TPicFileFormatsList = class(TList)
    // list of TPicFileFormat
  public
    constructor Create;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Add(const Ext, Desc: String; AClass: TGraphicClass);
    function GetFormats(Index: integer): PPicFileFormat;
    function FindExt(const Ext: string): TGraphicClass;
    function FindClassName(const AClassname: string): TGraphicClass;
    procedure Remove(AClass: TGraphicClass);
    procedure BuildFilterStrings(GraphicClass: TGraphicClass;
                                 var Descriptions, Filters: string);
    property Formats[Index: integer]: PPicFileFormat read GetFormats; default;
  end;

constructor TPicFileFormatsList.Create;
begin
  inherited Create;
  Add('bmp', rsBitmaps, TBitmap);
  Add('xpm', rsPixmap, TPixmap);
  Add('png', rsPortableNetworkGraphic, TPortableNetworkGraphic);
  Add('pbm', 'Portable BitMap', TPortableAnyMapGraphic);
  Add('pgm', 'Portable GrayMap', TPortableAnyMapGraphic);
  Add('ppm', 'Portable PixMap', TPortableAnyMapGraphic);
  Add('ico', rsIcon, TIcon);
  Add('jpg', 'Joint Picture Expert Group',TJpegImage);
end;

procedure TPicFileFormatsList.Clear;
var i: integer;
  P: PPicFileFormat;
begin
  for i:=0 to Count-1 do begin
    P:=GetFormats(i);
    Dispose(P);
  end;
  inherited Clear;
end;

procedure TPicFileFormatsList.Delete(Index: Integer);
var P: PPicFileFormat;
begin
  P:=GetFormats(Index);
  Dispose(P);
  inherited Delete(Index);
end;

procedure TPicFileFormatsList.Add(const Ext, Desc: String;
  AClass: TGraphicClass);
var
  NewFormat: PPicFileFormat;
begin
  New(NewFormat);
  with NewFormat^ do begin
    if (Ext<>'') and (Ext[1]='.') then
      Extension := AnsiLowerCase(copy(Ext,2,length(Ext)-1))
    else
      Extension := AnsiLowerCase(Ext);
    GraphicClass := AClass;
    Description := Desc;
  end;
  inherited Add(NewFormat);
end;

function TPicFileFormatsList.GetFormats(Index: integer): PPicFileFormat;
begin
  Result:=PPicFileFormat(Items[Index]);
end;

function TPicFileFormatsList.FindExt(const Ext: string): TGraphicClass;
var
  I: Integer;
begin
  if Ext<>'' then
    for I := Count-1 downto 0 do
      with PPicFileFormat(Items[I])^ do
        if (AnsiCompareText(Extension,Ext)=0)
        or ((Ext[1]='.')
            and (AnsiCompareText(Extension,copy(Ext,2,length(Ext)-1))=0)) then
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
  // search backwards so that new formats will be found first
  for I := Count-1 downto 0 do begin
    Result := GetFormats(I)^.GraphicClass;
    if AnsiCompareText(Result.ClassName,AClassname)=0 then
      Exit;
  end;
  Result := nil;
end;

procedure TPicFileFormatsList.Remove(AClass: TGraphicClass);
// remove all file formats which inherits from AClass
var
  I: Integer;
  P: PPicFileFormat;
begin
  for I := Count-1 downto 0 do begin
    P := GetFormats(I);
    if P^.GraphicClass.InheritsFrom(AClass) then
      Delete(I);
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
    P := GetFormats(I);
    if P^.GraphicClass.InheritsFrom(GraphicClass) and (P^.Extension <> '') then
      with P^ do begin
        if C <> 0 then begin
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
     ['Graphic', Filters, Descriptions]);
end;

//------------------------------------------------------------------------------

type
  PPicClipboardFormat = ^TPicClipboardFormat;
  TPicClipboardFormat = record
    GraphicClass: TGraphicClass;
    FormatID: TClipboardFormat;
  end;

  TPicClipboardFormats = class(TList)
    // list of TPicClipboardFormat
  private
    function GetFormats(Index: integer): PPicClipboardFormat;
  public
    constructor Create;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Add(AFormatID: TClipboardFormat; AClass: TGraphicClass);
    function FindFormat(FormatID: TClipboardFormat): TGraphicClass;
    procedure Remove(AClass: TGraphicClass);
    property Formats[Index: integer]: PPicClipboardFormat read GetFormats; default;
  end;

function TPicClipboardFormats.GetFormats(Index: integer): PPicClipboardFormat;
begin
  Result:=PPicClipboardFormat(Items[Index]);
end;

constructor TPicClipboardFormats.Create;
begin
  inherited Create;
  Add(PredefinedClipboardFormat(pcfBitmap), TBitmap);
  Add(PredefinedClipboardFormat(pcfDelphiBitmap), TBitmap);
  Add(PredefinedClipboardFormat(pcfPixmap), TPixmap);
  //Add(PredefinedClipboardFormat(pcfIcon), TIcon);
  Add(ClipboardRegisterFormat('image/png'), TPortableNetworkGraphic);
  Add(ClipboardRegisterFormat('image/jpeg'), TJPegImage);
end;

procedure TPicClipboardFormats.Clear;
var i: integer;
  P: PPicClipboardFormat;
begin
  for i:=0 to Count-1 do begin
    P:=GetFormats(i);
    Dispose(P);
  end;
  inherited Clear;
end;

procedure TPicClipboardFormats.Delete(Index: Integer);
var P: PPicClipboardFormat;
begin
  P:=GetFormats(Index);
  Dispose(P);
  inherited Delete(Index);
end;

procedure TPicClipboardFormats.Add(AFormatID: TClipboardFormat;
  AClass: TGraphicClass);
var NewFormat: PPicClipboardFormat;
begin
  if AFormatID=0 then exit;
  New(NewFormat);
  with NewFormat^ do begin
    GraphicClass:=AClass;
    FormatID:=AFormatID;
  end;
  inherited Add(NewFormat);
end;

function TPicClipboardFormats.FindFormat(
  FormatID: TClipboardFormat): TGraphicClass;
var
  I: Integer;
  P: PPicClipboardFormat;
begin
  for I := Count-1 downto 0 do begin
    P:=GetFormats(i);
    if P^.FormatID=FormatID then begin
      Result:=P^.GraphicClass;
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TPicClipboardFormats.Remove(AClass: TGraphicClass);
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    if GetFormats(i)^.GraphicClass.InheritsFrom(AClass) then
      Delete(i);
end;

//------------------------------------------------------------------------------

var
  PicClipboardFormats: TPicClipboardFormats=nil;
  PicFileFormats: TPicFileFormatsList=nil;

function GetPicFileFormats: TPicFileFormatsList;
begin
  if (PicFileFormats = nil) and (not GraphicsFinalized) then
    PicFileFormats := TPicFileFormatsList.Create;
  Result := PicFileFormats;
end;

function GetPicClipboardFormats: TPicClipboardFormats;
begin
  if (PicClipboardFormats = nil) and (not GraphicsFinalized) then
    PicClipboardFormats := TPicClipboardFormats.Create;
  Result := PicClipboardFormats;
end;

function GraphicFilter(GraphicClass: TGraphicClass): string;
var
  Filters: string;
begin
  Result := '';
  GetPicFileFormats.BuildFilterStrings(GraphicClass,Result,Filters);
end;

function GraphicExtension(GraphicClass: TGraphicClass): string;
var
  I: Integer;
  PicFormats: TPicFileFormatsList;
begin
  PicFormats := GetPicFileFormats;
  for I := PicFormats.Count-1 downto 0 do
    if PicFormats[I]^.GraphicClass.ClassName = GraphicClass.ClassName then begin
      Result := PicFormats[I]^.Extension;
      Exit;
    end;
  Result := '';
end;

function GraphicFileMask(GraphicClass: TGraphicClass): string;
var
  Descriptions: string;
begin
  Result := '';
  GetPicFileFormats.BuildFilterStrings(GraphicClass,Descriptions,Result);
end;

function GetGraphicClassForFileExtension(const FileExt: string): TGraphicClass;
begin
  Result:=GetPicFileFormats.FindExt(FileExt);
end;

function GetFPImageReaderForFileExtension(const FileExt: string
  ): TFPCustomImageReaderClass;
var
  GraphicClass: TGraphicClass;
begin
  GraphicClass:=GetGraphicClassForFileExtension(FileExt);
  if GraphicClass<>nil then
    Result:=GraphicClass.GetFPReaderForFileExt(FileExt)
  else
    Result:=nil;
end;

function GetFPImageWriterForFileExtension(const FileExt: string
  ): TFPCustomImageWriterClass;
var
  GraphicClass: TGraphicClass;
begin
  GraphicClass:=GetGraphicClassForFileExtension(FileExt);
  if GraphicClass<>nil then
    Result:=GraphicClass.GetFPWriterForFileExt(FileExt)
  else
    Result:=nil;
end;

//--TPicture--------------------------------------------------------------------


constructor TPicture.Create;
begin
  inherited Create;
  GetPicFileFormats;
  GetPicClipboardFormats;
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
  if not (FGraphic is GraphicType) then
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

function TPicture.GetPNG: TPortableNetworkGraphic;
begin
  ForceType(TPortableNetworkGraphic);
  Result := TPortableNetworkGraphic(Graphic);
end;

function TPicture.GetPNM: TPortableAnyMapGraphic;
begin
  ForceType(TPortableAnyMapGraphic);
  Result := TPortableAnyMapGraphic(Graphic);
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

function TPicture.GetJpeg: TJpegImage;
begin
  ForceType(TJpegImage);
  Result := TJpegImage(Graphic);
end;

procedure TPicture.SetBitmap(Value: TBitmap);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetPNG(const AValue: TPortableNetworkGraphic);
begin
  SetGraphic(AValue);
end;

procedure TPicture.SetPNM(const AValue: TPortableAnyMapGraphic);
begin
  SetGraphic(AValue);
end;

procedure TPicture.SetPixmap(Value: TPixmap);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetIcon(Value: TIcon);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetJpeg(Value: TJpegImage);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetGraphic(Value: TGraphic);
var
  NewGraphic: TGraphic;
  ok: boolean;
begin
  if (Value=FGraphic) then exit;
  NewGraphic := nil;
  ok:=false;
  try
    if Value <> nil then begin
      NewGraphic := TGraphicClass(Value.ClassType).Create;
      NewGraphic.Assign(Value);
      NewGraphic.OnChange := @Changed;
      NewGraphic.OnProgress := @Progress;
    end;
    FGraphic.Free;
    FGraphic := NewGraphic;
    Changed(Self);
    ok:=true;
  finally
    // this try..finally construction will in case of an exception
    // not alter the error backtrace output
    if not ok then
      NewGraphic.Free;
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
    raise EInvalidGraphic.CreateFmt(rsUnknownPictureExtension, [Ext]);

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
begin
  LoadFromClipboardFormatID(ctClipboard,FormatID);
end;

procedure TPicture.LoadFromClipboardFormatID(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat);
var
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
  ok: boolean;
begin
  GraphicClass := PicClipboardFormats.FindFormat(FormatID);
  if GraphicClass = nil then
    raise EInvalidGraphic.CreateFmt(rsUnsupportedClipboardFormat,
      [ClipboardFormatToMimeType(FormatID)]);

  NewGraphic := GraphicClass.Create;
  ok:=false;
  try
    NewGraphic.OnProgress := @Progress;
    NewGraphic.LoadFromClipboardFormatID(ClipboardType,FormatID);
    ok:=true;
  finally
    if not ok then NewGraphic.Free;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  FGraphic.OnChange := @Changed;
  Changed(Self);
end;

procedure TPicture.SaveToClipboardFormat(FormatID: TClipboardFormat);
begin
  if FGraphic <> nil then
    FGraphic.SaveToClipboardFormat(FormatID);
end;

class function TPicture.SupportsClipboardFormat(FormatID: TClipboardFormat): Boolean;
begin
  Result := GetPicClipboardFormats.FindFormat(FormatID) <> nil;
end;

procedure TPicture.Assign(Source: TPersistent);
begin
  if Source = nil then
    SetGraphic(nil)
  else if Source is TPicture then
    SetGraphic(TPicture(Source).Graphic)
  else if Source is TGraphic then
    SetGraphic(TGraphic(Source))
  else if Source is TFPCustomImage then
    Bitmap.Assign(Source)
  else
    inherited Assign(Source);
end;

class procedure TPicture.RegisterFileFormat(const AnExtension,
  ADescription: string; AGraphicClass: TGraphicClass);
begin
  GetPicFileFormats.Add(AnExtension, ADescription, AGraphicClass);
end;

class procedure TPicture.RegisterClipboardFormat(FormatID: TClipboardFormat;
  AGraphicClass: TGraphicClass);
begin
  GetPicClipboardFormats.Add(FormatID, AGraphicClass);
end;

class procedure TPicture.UnRegisterGraphicClass(AClass: TGraphicClass);
begin
  if PicFileFormats <> nil then PicFileFormats.Remove(AClass);
  if PicClipboardFormats <> nil then PicClipboardFormats.Remove(AClass);
end;

procedure TPicture.Clear;
begin
  SetGraphic(nil);
end;

procedure TPicture.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPicture.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string;
  var DoContinue: boolean);
begin
  DoContinue:=true;
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg, DoContinue);
end;

procedure TPicture.ReadData(Stream: TStream);
var
  GraphicClassName: Shortstring;
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
  ok: boolean;
begin
  Stream.Read(GraphicClassName[0], 1);
  Stream.Read(GraphicClassName[1], length(GraphicClassName));
  GraphicClass := GetPicFileFormats.FindClassName(GraphicClassName);
  NewGraphic := nil;
  if GraphicClass <> nil then begin
    NewGraphic := GraphicClass.Create;
    ok:=false;
    try
      NewGraphic.ReadData(Stream);
      ok:=true;
    finally
      if not ok then NewGraphic.Free;
    end;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  if NewGraphic <> nil then begin
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
    if Filer.Ancestor <> nil then begin
      Result := True;
      if Filer.Ancestor is TPicture then begin
        Ancestor := TPicture(Filer.Ancestor);
        Result := not ((Graphic = Ancestor.Graphic)
                       or ((Graphic <> nil) and (Ancestor.Graphic <> nil)
                           and Graphic.Equals(Ancestor.Graphic))
                      );
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
