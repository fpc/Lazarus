unit frmsqldbrestdispatchini;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, extctrls,Graphics, Dialogs, ButtonPanel, sqldbrestini;


Const
  EnabledOptions : Array[Boolean,TDispatcherIniOption] of Boolean = (
    { Read }  ( true, true, true, true, false, false, true ),
    { Write } ( false, false, false, false, True, True, True  )
 );

type
  { TSQLDBRestIniOptionsForm }

  TSQLDBRestIniOptionsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CGOptions: TCheckGroup;
  private
    FForSave : Boolean;
  public
    Procedure DoInit(aForSave : Boolean);
    Function SelectedOptions :TDispatcherIniOptions;
  end;

Function GetDispatchLoadSaveOptions(aForSave : Boolean; Out Opts : TDispatcherIniOptions) : Boolean;

var
  SQLDBRestIniOptionsForm: TSQLDBRestIniOptionsForm;

implementation



{$R *.lfm}

Resourcestring
  SSkipReadConnections    =  'Do not Read connection definitions';
  SSkipExposeConnections  =  'Do not Expose connections defined in .ini file';
  SSkipReadSchemas        =  'Do not Read schema definitions';
  SDisableSchemas         =  'Do not enable schemas';
  SSkipWriteConnections   =  'Do not write connection definitions';
  SSkipWriteSchemas       =  'Do not write schema definitions';
  SSkipBasicAuth          =  'Do not read/write basic auth data.';


Function GetOptionString(O : TDispatcherIniOption) : String;

begin
  case o of
    dioSkipReadConnections   : Result:=SSkipReadConnections;
    dioSkipExposeConnections : Result:=SSkipExposeConnections;
    dioSkipReadSchemas       : Result:=SSkipReadSchemas;
    dioDisableSchemas        : Result:=SDisableSchemas;
    dioSkipWriteConnections  : Result:=SSkipWriteConnections;
    dioSkipWriteSchemas      : Result:=SSkipWriteSchemas;
    dioSkipBasicAuth         : Result:=SSkipBasicAuth;
  else
    Result:='';
  end
end;

Function GetDispatchLoadSaveOptions(aForSave : Boolean; Out Opts : TDispatcherIniOptions) : Boolean;

begin
  With TSQLDBRestIniOptionsForm.Create(Application) do
    try
      DoInit(aForSave);
      Result:=ShowModal=MROK;
      if Result then
        opts:=SelectedOptions;
    finally
      Free;
    end;
end;

{ TSQLDBRestIniOptionsForm }

procedure TSQLDBRestIniOptionsForm.DoInit(aForSave: Boolean);

Var
  T : TDispatcherIniOption;

begin
  CGOptions.Items.Clear;
  FForSave:=aForSave;
  For T in TDispatcherIniOption do
    if EnabledOptions[aForSave,T] then
      CGOptions.Items.Add(GetOptionString(T));
end;

function TSQLDBRestIniOptionsForm.SelectedOptions: TDispatcherIniOptions;
Var
  T : TDispatcherIniOption;
  I : integer;
begin
  I:=0;
  Result:=[];
  For T in TDispatcherIniOption do
    if EnabledOptions[FForSave,T] then
      begin
      if CGOptions.Checked[I] then
          Include(Result,T);
      end;
end;


end.

