unit LoadPictureFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type
  
  { TLoadBitmapForm }

  TLoadBitmapForm = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  LoadBitmapForm: TLoadBitmapForm;

implementation

{$R *.lfm}

{ TLoadBitmapForm }

procedure TLoadBitmapForm.FormCreate(Sender: TObject);
begin
  Image1.Picture.LoadFromFile('powered_by.png');
end;

end.

