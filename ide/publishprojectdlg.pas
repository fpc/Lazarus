unit PublishProjectDlg; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, LResources, Buttons, StdCtrls;

type
  TPublishProjectDialog = class(TForm)
    BrowseDestDirBitBtn: TBITBTN;
    Button1: TBUTTON;
    Button2: TBUTTON;
    DestDirComboBox: TCOMBOBOX;
    DestDirGroupBox: TGROUPBOX;
  public
  end;

var
  PublishProjectDialog: TPublishProjectDialog;

implementation

end.

