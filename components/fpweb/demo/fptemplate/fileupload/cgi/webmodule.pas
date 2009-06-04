unit webmodule; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, HTTPDefs, websession, fpHTTP, fpWeb; 

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure DataModuleAfterResponse(Sender: TObject; AResponse: TResponse);
    procedure DataModuleCreate(Sender: TObject);
    procedure listfilesRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    UploadDir:String;
    FileDB:String;
    MaxSize:Integer;
    procedure DeleteTheFile(const FN:String);
    procedure HandleUploadedFiles;
    procedure listfilesReplaceTag(Sender: TObject; const TagString:String;
      TagParams: TStringList; Out ReplaceText: String);
  public
    { public declarations }
  end; 

var
  FPWebModule1: TFPWebModule1; 

implementation

{ TFPWebModule1 }

procedure TFPWebModule1.DataModuleAfterResponse(Sender: TObject;
  AResponse: TResponse);
begin
  //reset global variables for apache modules for the next incoming request

  //
end;

procedure TFPWebModule1.DataModuleCreate(Sender: TObject);
begin
  UploadDir := 'upfiles/';
  FileDB := 'filelist.txt';
  MaxSize := 2;//MB
end;

procedure TFPWebModule1.DeleteTheFile(const FN:String);
var
  FDB: TStringList;
  s:String;
begin
  FDB := TStringList.Create;
  if FileExists(FileDB) then
    FDB.LoadFromFile(FileDB);

  s := FDB.Values[FN];
  if s <> '' then
  begin
    FDB.Delete(FDB.IndexOfName(FN));
    FDB.SaveToFile(FileDB);
    FDB.Free;
  end else begin
    FDB.Free;
    Request.QueryFields.Add('_MSG=NOTFOUND');//NOTFOUND message will be displayed on the response page
    Exit;
  end;

  //delete the file
  s := UploadDir + FN;
  if FileExists(s) then
    DeleteFile(s);
end;

procedure TFPWebModule1.HandleUploadedFiles;
var
  i,j:Integer;
  all_ok:Boolean;
  FDB: TStringList;
  Uploader, FN:String;
begin
  if Request.Files.Count <= 0 then Exit;

  //process the uploaded files if there was any
  all_ok := true;
  for i := 0 to Request.Files.Count - 1 do
  begin//check sizes
    if Request.Files[i].Size > (MaxSize * 1024 * 1024) then
    begin//exceeds size limit
      all_ok := false;
      Request.QueryFields.Add('_MSG=TOOBIG');//TOOBIG message will be displayed on the response page
      break;
    end;
  end;

  if all_ok then //copy the file(s) to the upload directory (the temporary files will be deleted automatically after the request is handled)
  begin
    Uploader := Request.ContentFields.Values['UPLOADERPERSON'];
    if Uploader = '' then
      Uploader := '-';
    FDB := TStringList.Create;
    if FileExists(FileDB) then
      FDB.LoadFromFile(FileDB);
    for i := 0 to Request.Files.Count - 1 do
    begin
      FN := Request.Files[i].FileName;
      if (FN <> '')and(Request.Files[i].Size > 0) then
      begin
        CopyFile(Request.Files[i].LocalFileName, UploadDir + FN);//copy (or overwrite) the file to the upload dir
        if FDB.Values[FN] <> '' then
          FDB.Values[FN] := Uploader                              //overwrite the previous uploader
        else
          FDB.Add(FN + '=' + Uploader);                           //store the file and the uploader into the file database
      end;
    end;
    FDB.SaveToFile(FileDB);
    FDB.Free;
  end;
end;

procedure TFPWebModule1.listfilesRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  FN:String;
begin
  Template.FileName := 'uploadform.html';
  Template.AllowTagParams := true;
  Template.StartDelimiter := '{+';
  Template.EndDelimiter := '+}';
  Template.OnReplaceTag := @listfilesReplaceTag;

  FN := ARequest.QueryFields.Values['DELETE'];
  if FN <> '' then
    DeleteTheFile(FN)
  else
    HandleUploadedFiles;

  AResponse.Content := Template.GetContent;//Generate the response page using the template

  Handled := true;
end;

procedure TFPWebModule1.listfilesReplaceTag(Sender: TObject; const TagString:
  String; TagParams: TStringList; Out ReplaceText: String);
var
  SL:TStringList;
  i:Integer;
  FileName, Uploader, One_Row:String;
begin
  if AnsiCompareText(TagString, 'DATETIME') = 0 then
  begin
    ReplaceText := FormatDateTime(TagParams.Values['FORMAT'], Now);
  end else

  if AnsiCompareText(TagString, 'MAX_SIZE') = 0 then
  begin
    ReplaceText := IntToStr(MaxSize);
  end else

  if AnsiCompareText(TagString, 'UPLOAD_DIR') = 0 then
  begin
    ReplaceText := UploadDir;
  end else

  if AnsiCompareText(TagString, 'MESSAGES') = 0 then
  begin
    ReplaceText := TagParams.Values[Request.QueryFields.Values['_MSG']];
  end else

  if AnsiCompareText(TagString, 'FILELIST') = 0 then
  begin
    SL := TStringList.Create;
    if FileExists(FileDB) then
      SL.LoadFromFile(FileDB);
    if SL.Count > 0 then
    begin
      One_Row := TagParams.Values['ONE_ROW'];
      for i := 0 to SL.Count - 1 do
      begin
        FileName := SL.Names[i];
        Uploader := SL.Values[FileName];
        if (FileName <> '')and(Uploader <> '') then
          ReplaceText := ReplaceText + StringReplace(StringReplace(StringReplace(One_Row
                                       ,'~FILENAME', FileName, [])
                                       ,'~UPLOADER', Uploader, [])
                                       ,'~DFILENAME', HTTPEncode(FileName), []) + #13#10;
      end;
    end else begin
      ReplaceText := TagParams.Values['NOTHINGTOLIST'];
    end;
    SL.Free;
  end else

  {Message for tags not handled}
  begin
    ReplaceText := '[Template tag {+' + TagString + '+} is not implemented yet.]';
  end;
end;

initialization
  {$I webmodule.lrs}

  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.
