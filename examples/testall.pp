program testall;

{$mode objfpc}

uses forms,
     testallform;

begin
   Application.Initialize;
   Application.CreateForm(TForm1, Form1);
   Application.Run;
end.
{
  $Log$
  Revision 1.1  2000/07/31 20:33:33  lazarus
  + added "testall" demo provided by <christer.t.johansson@se.abb.com>
  stoppok

} 
