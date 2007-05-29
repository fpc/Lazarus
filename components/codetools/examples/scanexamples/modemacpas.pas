unit ModeMacPas; 

{$mode macpas}{$H+}

interface

uses
  Classes, SysUtils; 

{$DEFINE test3}
{$DEFINE bogus4}

{$ifc defined test1}
type aaa1 = integer;
{$elifc defined test2}
type aaa2 = integer;
{$elifc defined test3}
type aaa3 = integer;
  {$ifc defined bogus1}
  type bogus1 = integer;
  {$elifc defined bogus2}
  type bogus2 = integer;
  {$elifc defined bogus3}
  type bogus3 = integer;
  {$elsec}
  type bogusELSE = integer;
    {$error Neither bogus1 nor bogus2 nor bogus3 are defined.}
  {$endc}
{$elsec}
type aaaELSE = integer;
  {$error Neither test1 nor test2 nor test3 are defined.}
{$endc}

implementation

end.

