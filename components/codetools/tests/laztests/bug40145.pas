unit bug40145;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type

  bad=record
    field1:string;
    field2:integer;
    case integer of
      1:(hh:procedure());
      2:(ff1:function(a:integer;b:boolean):boolean of object);       //<<--- Codetools error:  unit2.pas(17,9): Error bracket ) not found
                                                                     // sometimes  unit2.pas(17,22) Error: identifier expected, but ( found.
      3:(pp2:procedure(aa:integer;bb:boolean));
      4:(pp3:procedure);
      5:(ff2:function(a:integer;b:boolean):boolean; bbb:integer);
      6:(ff3:function(a:integer;b:boolean):boolean of object);
  end;

  tt=record
    i: packed record
         j: integer;
         k: record end;
         case y: integer of
           0: (a: integer deprecated);
           1,2,3: (b: array[char] of char; c: char);
           3: ( d: record
                     case byte of
                       10: (i: integer; );
                       11: (y: byte);
                   end; );
           4: (e: integer;
               case enum:(one, two, three) of
                  one:(F: Integer);
                  two:(Dd: Byte);
                  three:(Z:PChar)
               );
       end;
  end;

implementation


var
    MyRecord: record
              i: packed record
                   j: integer;
                   k: record end;
                   case integer of
                     0: (a: integer);
                     1,2,3: (b: array[char] of char; c: char);
                     3: ( d: record
                               case byte of
                                 10: (i: integer; );
                                 11: (y: byte);
                             end;)
                 end;
            end;
    MyPointer: ^integer;

function TestFunction(Index : Integer) : Integer;
var
  wr:bad;
  wt:tt;
begin
  Result := 2*Index;
  wr.pp2(2,true);
  wr.pp3;
  wr.bbb:=1;
  wr.ff1(1,true);
  wr.ff2(2,false);
  wr.{completion:field1,field2,hh,ff1,pp2,pp3,ff2,bbb,ff3,!a,!b,!bb,!bad,!tt,!i,!j}               //<< cursor after . and press Ctrl+Space.
end;

end.


