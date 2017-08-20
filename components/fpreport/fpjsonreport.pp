unit fpjsonreport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpreport, fpjson, fpreportstreamer;

Type

  { TFPJSONReport }

  TFPJSONReport = class(TFPReport)
  private
    FDesignTimeJSON: TJSONObject;
    procedure ReadReportJSON(Reader: TReader);
    procedure WriteReportJSON(Writer: TWriter);
  Protected
    Procedure DefineProperties(Filer: TFiler); override;
  Public
    Constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(const aStream: TStream);
    procedure SaveToStream(const aStream: TStream);
    Procedure LoadFromJSON(aJSON : TJSONObject); virtual;
    Procedure SavetoJSON(aJSON : TJSONObject); virtual;
    Procedure LoadFromFile(const aFileName : String);
    Procedure SaveToFile(const aFileName : String);
    Property DesignTimeJSON : TJSONObject Read FDesignTimeJSON;
  end;

implementation

Resourcestring
  SErrInvalidJSONData = 'Invalid JSON Data';
  SErrFailedToLoad = 'Failed to load report: %s';

{ TFPJSONReport }

procedure TFPJSONReport.ReadReportJSON(Reader: TReader);

Var
  S : UnicodeString;
  D : TJSONData;

begin
  FDesignTimeJSON.Clear;
  S:=Reader.ReadUnicodeString;
  if (S<>'') then
    begin
    D:=GetJSON(UTF8Encode(S),True);
    if D is TJSONObject then
      begin
      FreeAndNil(FDesignTimeJSON);
      FDesignTimeJSON:=D as TJSONObject
      end
    else
      begin
      D.Free;
      FDesignTimeJSON:=TJSONObject.Create;
      Raise EReportError.CreateFmt(SErrFailedToLoad,[SErrInvalidJSONData]);
      end;
    end;
end;

procedure TFPJSONReport.WriteReportJSON(Writer: TWriter);

Var
  S : UnicodeString;

begin
  S:='';
  if (FDesignTimeJSON.Count>0) then
    S:=UTF8Decode(FDesignTimeJSON.AsJSON);
  Writer.WriteUnicodeString(S);
end;

procedure TFPJSONReport.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ReportJSON',@ReadReportJSON,@WriteReportJSON,Assigned(FDesignTimeJSON) and (FDesignTimeJSON.Count>0));
end;

constructor TFPJSONReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDesignTimeJSON:=TJSONObject.Create;
end;

destructor TFPJSONReport.Destroy;
begin
  FreeAndNil(FDesignTimeJSON);
  inherited Destroy;
end;

procedure TFPJSONReport.LoadFromJSON(aJSON: TJSONObject);

Var
  R : TFPReportJSONStreamer;

begin
  R:=TFPReportJSONStreamer.Create(Nil);
  try
    R.OwnsJSON:=False;
    R.JSON:=aJSON;
    ReadElement(R);
  finally
    R.Free;
  end;
end;

procedure TFPJSONReport.SavetoJSON(aJSON: TJSONObject);

Var
  R : TFPReportJSONStreamer;

begin
  R:=TFPReportJSONStreamer.Create(Nil);
  try
    R.OwnsJSON:=False;
    R.JSON:=aJSON;
    WriteElement(R);
  finally
    R.Free;
  end;
end;

procedure TFPJSONReport.LoadFromStream(const aStream : TStream);

Var
  D : TJSONData;

begin
  D:=GetJSON(aStream);
  try
    if not (D is TJSONObject) then
      Raise EReportError.CreateFmt(SErrFailedToLoad,[SErrInvalidJSONData]);
    LoadFromJSON(D as TJSONObject);
  finally
    D.Free;
  end;
end;

procedure TFPJSONReport.SaveToStream(const aStream: TStream);

Var
  O : TJSONObject;
  S : TJSONStringType;

begin
  O:=TJSONObject.Create;
  try
    SaveToJSON(O);
    S:=O.AsJSON;
    aStream.WriteBuffer(S[1],Length(S));
  finally
    O.Free;
  end;
end;

procedure TFPJSONReport.LoadFromFile(const aFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TFPJSONReport.SaveToFile(const aFileName: String);
Var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

end.

