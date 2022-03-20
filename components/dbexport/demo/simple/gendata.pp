Unit gendata;

{$mode objfpc}{$H+}

Interface

Uses Classes, SysUtils, DB, dbf;

Const
  DefFirstNamesFile = 'firstnames.txt';
  DefLastNamesFile  = 'lastnames.txt';
  DefPersonCount     = 10;
  DefDayCount        = 3;

Type 

  { TDatagenerator }

  TDatagenerator = Class(TObject)
  private
    FTID : Integer;
    FDayCount: Integer;
    FFirstNamesFile: String;
    FLastNamesFile: String;
    FOnProgress: TNotifyEvent;
    FOutputFile: String;
    FPersonCount: Integer;
    FStartDate: TDateTime;
    FDataset : TDataset;
    procedure DoTrack(No,ID : Integer; Const FN,LN: String; Const D,Tin,Tout : TDateTime);
    procedure DoPerson(ID : Integer; FN,LN : String);
  Protected
    Function CreateDataset : TDataset; virtual; abstract;
    Procedure CloseDataset; virtual;
    Property Dataset : TDataset Read FDataset Write FDataset;  

  Public
    Constructor Create;
    Procedure GenerateData;
    Property FirstNamesFile : String Read FFirstNamesFile Write FFirstNamesFile;
    Property LastNamesFile : String Read FLastNamesFile Write FLastNamesFile;
    Property OutputFile : String Read FOutputFile Write FOutputFile;
    Property StartDate : TDateTime Read FStartDate Write FStartDate;
    Property OnProgress : TNotifyEvent Read FOnProgress Write FOnPRogress;
    Property PersonCount : Integer Read FPersonCount Write FPersonCount;
    Property DayCount : Integer Read FDayCount Write FDayCount;
  end;
  
  { TDBFGenerator }

  TDBFGenerator = Class(TDataGenerator)
  Protected
    Function CreateDataset : TDataset; override;
  End;

Implementation
  

{ TDatagenerator }

constructor TDatagenerator.Create;
begin
  FFirstNamesFile:=DefFirstNamesFile;
  FLastNamesFile:=DefLastNamesFile;
  FPersonCount:=DefPersonCount;
  FDayCount:=DefDayCount;
  FStartDate:=EncodeDate(2005,9,1);
end;

procedure TDatagenerator.DoTrack(No,ID : Integer; Const FN,LN: String; Const D,Tin,Tout : TDateTime);

begin
  With Dataset do
    begin
    Append;
    FieldByName('TrackID').AsInteger:=No;
    FieldByName('PersonID').AsInteger:=ID;
    FieldByName('FirstName').AsString:=FN;
    FieldByName('LastName').AsString:=LN;
    FieldByName('DayOfWeek').AsInteger:=DayOfWeek(D);
    FieldByName('Date').AsDateTime:=D;
    FieldByName('TimeIn').AsDateTime:=TIn;
    FieldByName('TimeOut').AsDateTime:=TOut;
    Post;
    end;
end;

procedure TDatagenerator.DoPerson(ID : Integer; FN,LN : String);

Var
  D,TIn,Tout : TDateTime;
  I : Integer;

begin
  For I:=1 to DayCount do
    begin
    D:=FStartDate+I;
    // Entry
    TIn:=EncodeTime(8,30+Random(10)-10,Random(60),0);
    //  Exit
    Tout:=EncodeTime(17,30+Random(10)-10,Random(60),0);
    DoTrack((ID-1)*DayCount+I,ID,FN,LN,D,TIn,Tout);
    end;
  If Assigned(FOnProgress) then
    FOnProgress(Self);
end;

procedure TDatagenerator.CloseDataset;
begin
  FDataset.Close;
  FreeAndNil(FDataset);
end;

procedure TDatagenerator.GenerateData;

Var
  FN,LN : TStrings;
  PFN,PLN : String;
  F : Text;
  I : Integer;
  
begin
  FTID:=0;
  FDataset:=CreateDataset;
  Try
    Randomize;
    FN:=TStringList.Create;
    Try
      FN.LoadFromFile(UTF8ToAnsi(FFirstNamesFile));
      LN:=TStringList.Create;
      Try
        LN.LoadFromFile(UTF8ToAnsi(FLastNamesFile));
        Assign(F,FOutputFile);
        Rewrite(F);
        Try
          For I:=1 to PersonCount do
            begin
            PFN:=FN[Random(FN.Count)];
            PLN:=LN[Random(LN.Count)];
            DoPerson(I,PFN,PLN);
          end;
        Finally
          Close(F);
        end;
      Finally
        LN.Free;
      end;
    Finally
      FN.Free;
    end;
  Finally
    CLoseDataset;
  end;
end;

{ TDBFGenerator }

function TDBFGenerator.CreateDataset: TDataset;

Var
  DS : TDBF;

begin
  DS:=TDBF.Create(Nil);
  with DS.FieldDefs do
    begin
    Clear;
    Add('TrackID',ftInteger,0);
    Add('PersonID',ftInteger,0);
    Add('FirstName',ftString,30);
    Add('LastName',ftString,30);
    Add('DayOfWeek',ftSmallint,0);
    Add('Date',ftDate,0);
    Add('TimeIn',ftDateTime,0);
    Add('TimeOut',ftDateTime,0);
    end;
  DS.TableName:=OutputFile;
  DS.CreateTable;
  DS.Exclusive := true;
  DS.Open;
  DS.AddIndex('LastName', 'LastName', []);
  Result:=DS;

end;

end.
 
