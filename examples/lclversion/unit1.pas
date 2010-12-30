unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLVersion,
  StdCtrls;

type

  { TVersionForm }

  TVersionForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  VersionForm: TVersionForm;

implementation

{ TVersionForm }

procedure TVersionForm.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  // check if lcl_version is declared,
  // then we know if there is support for lcl version information
  {$if declared(lcl_version)}
  Memo1.Append('Example which uses the lcl version information');
  Memo1.Append(format('LCL Version: %s', [lcl_version]));
  Memo1.Append(format('LCL Major: %d', [lcl_major]));
  Memo1.Append(format('LCL Minor: %d', [lcl_minor]));
  Memo1.Append(format('LCL Release: %d', [lcl_release]));
  Memo1.Append(format('LCL Full Version: %d', [lcl_fullversion]));
  {$else}
  Memo1.Append('No lcl version information available');
  {$endif}
end;

procedure TVersionForm.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
  {$if (lcl_major=0) and (lcl_minor=9) and (lcl_release<26)}
  Memo1.Append('This program is compiled with lcl version before 0.9.26');
  // you cannot use features introduced in 0.9.26
  {$else}
  Memo1.Append('This program is compiled with lcl version 0.9.26 or later.');
  // you can use features available in 0.9.26
  {$endif}
  {$if (lcl_fullversion) > 93002}
  Memo1.Append('This program is compiled with lcl version after 0.9.30.2.');
  // you can use features available in 0.9.30.3 and later (e.g. 0.9.31)
  {$else}
  Memo1.Append('This program is compiled with lcl version 0.9.30.2 or before.');
  // you cannot use features introduced after 0.9.30.2
  {$endif}
end;

initialization
  {$I unit1.lrs}

end.

