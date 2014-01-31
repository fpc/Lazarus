{
DateTimeControlsReg
- - - - - - - - - - - - - - - - - -
Author: Zoran Vučenović, January and February 2010
        Зоран Вученовић, јануар и фебруар 2010.

  This unit is part of DateTimeCtrls package for Lazarus.

-----------------------------------------------------------
LICENCE
- - - -
   Modified LGPL -- see the file COPYING.modifiedLGPL.

-----------------------------------------------------------
NO WARRANTY
- - - - - -
   There is no warranty whatsoever.

-----------------------------------------------------------
BEST REGARDS TO LAZARUS COMMUNITY!
- - - - - - - - - - - - - - - - - -
   I do hope the DateTimeCtrls package will be useful.
}
unit DateTimeControlsReg;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation

uses
  Classes, DateTimePicker, DBDateTimePicker, LResources;

procedure Register;
begin
  RegisterComponents('Common Controls', [TDateTimePicker, TDBDateTimePicker]);
end;

initialization
{$i datetimectrls.lrs}

end.

