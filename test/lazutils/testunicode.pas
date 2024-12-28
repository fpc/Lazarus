program TestUnicode;

{$mode objfpc}{$H+}

uses
  sysutils, LazUTF8, LazUTF16;

procedure WriteStringHex(Str: utf8string);
var
  StrOut: utf8string;
  i: Integer;
begin
  StrOut := '';
  for i := 1 to Length(Str) do
    StrOut := StrOut + IntToHex(Byte(Str[i]), 2) + ' ';
  Write(StrOut);
end;

procedure AssertStringOperation(AMsg, AStr1, AStr2, AStrExpected2: utf8string);
begin
  Write(AMsg, ' ', AStr1, ' => ', AStr2);
  if UTF8CompareStr(AStr2, AStrExpected2) <> 0 then
  begin
    Write(' Expected ', AStrExpected2, ' !Error!');
    WriteLn();
    Write('Got      Len=', Length(AStr2), ' Str=');
    WriteStringHex(AStr2);
    WriteLn('');
    Write('Expected Len=', Length(AStrExpected2), ' Str=');
    WriteStringHex(AStrExpected2);
    WriteLn();
    Write('Orig     Len=', Length(AStr1), ' Str=');
    WriteStringHex(AStr1);
    WriteLn('');
  end;
  WriteLn();
end;

procedure AssertUTF8StringReplace(AMsg, ALocale, AStr1, OldPat, NewPat, AStrExpected: utf8string);
begin
  AssertStringOperation(AMsg,AStr1,UTF8StringReplace(AStr1,OldPat,NewPat,[rfIgnoreCase],ALocale),AStrExpected);
end;

procedure AssertUTF8UpperCase(AMsg, ALocale, AStr1, AStrExpected: utf8string);
begin
  AssertStringOperation(AMsg, AStr1, UTF8UpperCase(AStr1, ALocale), AStrExpected);
end;

procedure AssertUTF8LowerCase(AMsg, ALocale, AStr1, AStrExpected: utf8string);
begin
  AssertStringOperation(AMsg, AStr1, UTF8LowerCase(AStr1, ALocale), AStrExpected);
//  AssertStringOperation('2'+AMsg, AStr1, UTF8LowerCase2(AStr1, ALocale), AStrExpected);
//  AssertStringOperation('M'+AMsg, AStr1, UTF8LowerCaseMattias(AStr1), AStrExpected);
end;

function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
var
  TimeStamp: TTimeStamp;
begin
  {Call DateTimeToTimeStamp to convert DateTime to TimeStamp:}
  TimeStamp:= DateTimeToTimeStamp (aDateTime);
  {Multiply and add to complete the conversion:}
  Result:= TimeStamp.Time;
end;

procedure TestUTF8StringReplace;
begin
  AssertUTF8StringReplace('After Capital i', '', 'İa', 'a', 'b', 'İb');
  AssertUTF8StringReplace('Replace test 2', '', 'İa12b', '12', '3456', 'İa3456b');
  AssertUTF8StringReplace('Replace test 3', '', 'İa12bc', 'bc', 'ö', 'İa12ö');
  WriteLn;
end;

procedure TestUTF8UpperCase;
var
  lStartTime, lTimeDiff: TDateTime;
  Str: UTF8String;
  i: Integer;
begin
  // ASCII
  AssertUTF8UpperCase('ASCII UTF8UpperCase', '', 'abcdefghijklmnopqrstuwvxyz', 'ABCDEFGHIJKLMNOPQRSTUWVXYZ');
  // Latin
  AssertUTF8UpperCase('Portuguese UTF8UpperCase 1', '', 'Ç/ç Ã/ã Õ/õ Á/á É/é Í/í Ó/ó Ú/ú Ü/ü À/à Â/â Ê/ê Î/î Ô/ô Û/û', 'Ç/Ç Ã/Ã Õ/Õ Á/Á É/É Í/Í Ó/Ó Ú/Ú Ü/Ü À/À Â/Â Ê/Ê Î/Î Ô/Ô Û/Û');
  AssertUTF8UpperCase('French UTF8UpperCase 1', '', 'À/à Â/â æ Ç/ç É/é È/è Ê/ê Ë/ë Î/î Ï/ï Ô/ô œ Ù/ù Û/û Ü/ü Ÿ/ÿ', 'À/À Â/Â Æ Ç/Ç É/É È/È Ê/Ê Ë/Ë Î/Î Ï/Ï Ô/Ô Œ Ù/Ù Û/Û Ü/Ü Ÿ/Ÿ');
  AssertUTF8UpperCase('Polish UTF8UpperCase 1', '', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
  AssertUTF8UpperCase('Polish UTF8UpperCase 2', '', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
  AssertUTF8UpperCase('German UTF8UpperCase 1', '', 'Ä/ä,Ö/ö,Ü/ü,ß', 'Ä/Ä,Ö/Ö,Ü/Ü,SS');
  // Turkish
  AssertUTF8UpperCase('Turkish UTF8UpperCase 1', 'tr', 'abcçdefgğhııijklmnoöprsştuüvyz', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ');
  AssertUTF8UpperCase('Turkish UTF8UpperCase 2', 'tr', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ');
  // Cyrillic
  AssertUTF8UpperCase('Russian UTF8UpperCase 1', '', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ');
  AssertUTF8UpperCase('Russian UTF8UpperCase 2', '', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ');
  // Unicode table
  AssertUTF8UpperCase('Latin 00C0 UTF8UpperCase', '', 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ', 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ');
  AssertUTF8UpperCase('Latin 00D0 UTF8UpperCase', '', 'ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß', 'ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞSS');
  AssertUTF8UpperCase('Latin 00E0 UTF8UpperCase', '', 'àáâãäåæçèéêëìíîï', 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ');
  AssertUTF8UpperCase('Latin 00F0 UTF8UpperCase', '', 'ðñòóôõö÷øùúûüýþÿ', 'ÐÑÒÓÔÕÖ÷ØÙÚÛÜÝÞŸ');
  AssertUTF8UpperCase('Latin 0100 UTF8UpperCase', '', 'ĀāĂăĄąĆćĈĉĊċČčĎď', 'ĀĀĂĂĄĄĆĆĈĈĊĊČČĎĎ');
  AssertUTF8UpperCase('Latin 0110 UTF8UpperCase', '', 'ĐđĒēĔĕĖėĘęĚěĜĝĞğ', 'ĐĐĒĒĔĔĖĖĘĘĚĚĜĜĞĞ');
  AssertUTF8UpperCase('Latin 0120 UTF8UpperCase', '', 'ĠġĢģĤĥĦħĨĩĪīĬĭĮį', 'ĠĠĢĢĤĤĦĦĨĨĪĪĬĬĮĮ');
  AssertUTF8UpperCase('Latin 0130 UTF8UpperCase', '', 'İıĲĳĴĵĶķĸĹĺĻļĽľĿ', 'İIĲĲĴĴĶĶĸĹĹĻĻĽĽĿ');
  AssertUTF8UpperCase('Latin 0140 UTF8UpperCase', '', 'ŀŁłŃńŅņŇňŉŊŋŌōŎŏ', 'ĿŁŁŃŃŅŅŇŇŉŊŊŌŌŎŎ');
  AssertUTF8UpperCase('Latin 0150 UTF8UpperCase', '', 'ŐőŒœŔŕŖŗŘřŚśŜŝŞş', 'ŐŐŒŒŔŔŖŖŘŘŚŚŜŜŞŞ');
  AssertUTF8UpperCase('Latin 0160 UTF8UpperCase', '', 'ŠšŢţŤťŦŧŨũŪūŬŭŮů', 'ŠŠŢŢŤŤŦŦŨŨŪŪŬŬŮŮ');
  AssertUTF8UpperCase('Latin 0170 UTF8UpperCase', '', 'ŰűŲųŴŵŶŷŸŹźŻżŽžſ', 'ŰŰŲŲŴŴŶŶŸŹŹŻŻŽŽS');
  AssertUTF8UpperCase('Latin 0180 UTF8UpperCase', '', 'ƀƁƂƃƄƅƆƇƈƉƊƋƌƍƎƏ', 'ɃƁƂƂƄƄƆƇƇƉƊƋƋƍƎƏ');
  AssertUTF8UpperCase('Latin 0190 UTF8UpperCase', '', 'ƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞƟ', 'ƐƑƑƓƔǶƖƗƘƘȽƛƜƝȠƟ');
  AssertUTF8UpperCase('Latin 01A0 UTF8UpperCase', '', 'ƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯ', 'ƠƠƢƢƤƤƦƧƧƩƪƫƬƬƮƯ');
  AssertUTF8UpperCase('Latin 01B0 UTF8UpperCase', '', 'ưƱƲƳƴƵƶƷƸƹƺƻƼƽƾƿ', 'ƯƱƲƳƳƵƵƷƸƸƺƻƼƼƾǷ');
  AssertUTF8UpperCase('Latin 01C0 UTF8UpperCase', '', 'ǀǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏ', 'ǀǁǂǃǄǄǄǇǇǇǊǊǊǍǍǏ');
  AssertUTF8UpperCase('Latin 01D0 UTF8UpperCase', '', 'ǐǑǒǓǔǕǖǗǘǙǚǛǜǝǞǟ', 'ǏǑǑǓǓǕǕǗǗǙǙǛǛƎǞǞ');
  AssertUTF8UpperCase('Latin 01E0 UTF8UpperCase', '', 'ǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯ', 'ǠǠǢǢǤǤǦǦǨǨǪǪǬǬǮǮ');
  AssertUTF8UpperCase('Latin 01F0 UTF8UpperCase', '', 'ǰǱǲǳǴǵǶǷǸǹǺǻǼǽǾǿ', 'ǰǱǱǱǴǴǶǷǸǸǺǺǼǼǾǾ');
  AssertUTF8UpperCase('Latin 0200 UTF8UpperCase', '', 'ȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏ', 'ȀȀȂȂȄȄȆȆȈȈȊȊȌȌȎȎ');
  AssertUTF8UpperCase('Latin 0210 UTF8UpperCase', '', 'ȐȑȒȓȔȕȖȗȘșȚțȜȝȞȟ', 'ȐȐȒȒȔȔȖȖȘȘȚȚȜȜȞȞ');
  AssertUTF8UpperCase('Latin 0220 UTF8UpperCase', '', 'ȠȡȢȣȤȥȦȧȨȩȪȫȬȭȮȯ', 'ȠȡȢȢȤȤȦȦȨȨȪȪȬȬȮȮ');
  AssertUTF8UpperCase('Latin 0230 UTF8UpperCase', '', 'ȰȱȲȳȴȵȶȷȸȹȺȻȼȽȾȿ', 'ȰȰȲȲȴȵȶȷȸȹȺȻȻȽȾⱾ');
  AssertUTF8UpperCase('Latin 0240 UTF8UpperCase', '', 'ɀɁɂɃɄɅɆɇɈɉɊɋɌɍɎɏ', 'ⱿɁɁɃɄɅɆɆɈɈɊɊɌɌɎɎ');
  AssertUTF8UpperCase('Latin 0250 UTF8UpperCase', '', 'ɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟ', 'ⱯⱭⱰƁƆɕƉƊɘƏɚƐɜɝɞɟ');
  AssertUTF8UpperCase('Latin 0260 UTF8UpperCase', '', 'ɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯ', 'ƓɡɢƔɤꞍɦɧƗƖɪⱢɬɭɮƜ');
  AssertUTF8UpperCase('Latin 0270 UTF8UpperCase', '', 'ɰɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿ', 'ɰⱮƝɳɴƟɶɷɸɹɺɻɼⱤɾɿ');
  AssertUTF8UpperCase('Latin 0280 UTF8UpperCase', '', 'ʀʁʂʃʄʅʆʇʈʉʊʋʌʍʎʏ', 'ƦʁʂƩʄʅʆʇƮɄƱƲɅʍʎʏ');
  AssertUTF8UpperCase('Latin 0290 UTF8UpperCase', '', 'ʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟ', 'ʐʑƷʓʔʕʖʗʘʙʚʛʜʝʞʟ');
  AssertUTF8UpperCase('Latin 02A0 UTF8UpperCase', '', 'ʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯ', 'ʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯ');
  AssertUTF8UpperCase('Unicode 0380 UTF8UpperCase', '', '΄΅Ά·ΈΉΊΌΎΏ', '΄΅Ά·ΈΉΊΌΎΏ');
  AssertUTF8UpperCase('Unicode 0390 UTF8UpperCase', '', 'ΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟ', 'ΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟ');
  AssertUTF8UpperCase('Unicode 03A0 UTF8UpperCase', '', 'ΠΡΣΤΥΦΧΨΩΪΫάέήί', 'ΠΡΣΤΥΦΧΨΩΪΫΆΈΉΊ');
  AssertUTF8UpperCase('Unicode 03B0 UTF8UpperCase', '', 'ΰαβγδεζηθικλμνξο', 'ΰΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟ');
  AssertUTF8UpperCase('Unicode 03C0 UTF8UpperCase', '', 'πρςστυφχψωϊϋόύώϏ', 'ΠΡΣΣΤΥΦΧΨΩΪΫΌΎΏϏ');
  AssertUTF8UpperCase('Unicode 03D0 UTF8UpperCase', '', 'ϐϑϒϓϔϕϖϗϘϙϚϛϜϝϞϟ', 'ΒΘϒϓϔΦΠϏϘϘϚϚϜϜϞϞ');
  AssertUTF8UpperCase('Unicode 03E0 UTF8UpperCase', '', 'ϠϡϢϣϤϥϦϧϨϩϪϫϬϭϮϯ', 'ϠϠϢϢϤϤϦϦϨϨϪϪϬϬϮϮ');
  AssertUTF8UpperCase('Unicode 03F0 UTF8UpperCase', '', 'ϰϱϲϳϴϵ϶ϷϸϹϺϻϼϽϾϿ', 'ΚΡϹϳϴΕ϶ϷϷϹϺϺϼϽϾϿ');
  AssertUTF8UpperCase('Unicode 0400 UTF8UpperCase', '', 'ЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ', 'ЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ');
  AssertUTF8UpperCase('Unicode 0410 UTF8UpperCase', '', 'АБВГДЕЖЗИЙКЛМНОП', 'АБВГДЕЖЗИЙКЛМНОП');
  AssertUTF8UpperCase('Unicode 0420 UTF8UpperCase', '', 'РСТУФХЦЧШЩЪЫЬЭЮЯ', 'РСТУФХЦЧШЩЪЫЬЭЮЯ');
  AssertUTF8UpperCase('Unicode 0430 UTF8UpperCase', '', 'абвгдежзийклмноп', 'АБВГДЕЖЗИЙКЛМНОП');
  AssertUTF8UpperCase('Unicode 0440 UTF8UpperCase', '', 'рстуфхцчшщъыьэюя', 'РСТУФХЦЧШЩЪЫЬЭЮЯ');
  AssertUTF8UpperCase('Unicode 0450 UTF8UpperCase', '', 'ѐёђѓєѕіїјљњћќѝўџ', 'ЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ');

  // What shouldnt change
  AssertUTF8UpperCase('Chinese UTF8UpperCase 1', '', '名字叫嘉英，嘉陵江的嘉，英國的英', '名字叫嘉英，嘉陵江的嘉，英國的英');

  // Performance test
  lStartTime := Now;
  for i := 0 to 9999 do
  begin
    Str := UTF8UpperCase('ABCDEFGHIJKLMNOPQRSTUWVXYZ');
    Str := Str + UTF8UpperCase('aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
    Str := Str + UTF8UpperCase('AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
  end;
  lTimeDiff := Now - lStartTime;
  WriteLn('UpperCase Performance test took: ', DateTimeToMilliseconds(lTimeDiff), ' ms');
  WriteLn;
end;

procedure TestUTF8LowerCase;
var
  j, i: Integer;
  lStartTime, lTimeDiff: TDateTime;
  Str: UTF8String;
const
  TimerLoop = 5999999;
begin
  // ASCII
  AssertUTF8LowerCase('ASCII UTF8LowerCase', '', 'ABCDEFGHIJKLMNOPQRSTUWVXYZ', 'abcdefghijklmnopqrstuwvxyz');
  // Latin
  AssertUTF8LowerCase('Portuguese UTF8LowerCase 1', '', 'Ç/ç Ã/ã Õ/õ Á/á É/é Í/í Ó/ó Ú/ú Ü/ü À/à Â/â Ê/ê Î/î Ô/ô Û/û', 'ç/ç ã/ã õ/õ á/á é/é í/í ó/ó ú/ú ü/ü à/à â/â ê/ê î/î ô/ô û/û');
  AssertUTF8LowerCase('French UTF8LowerCase 1', '', 'À/à Â/â æ Ç/ç É/é È/è Ê/ê Ë/ë Î/î Ï/ï Ô/ô œ Ù/ù Û/û Ü/ü Ÿ/ÿ', 'à/à â/â æ ç/ç é/é è/è ê/ê ë/ë î/î ï/ï ô/ô œ ù/ù û/û ü/ü ÿ/ÿ');
  AssertUTF8LowerCase('Polish UTF8LowerCase 1', '', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
  AssertUTF8LowerCase('Polish UTF8LowerCase 2', '', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
  AssertUTF8LowerCase('German UTF8LowerCase 1', '', 'Ä/ä,Ö/ö,Ü/ü,ß', 'ä/ä,ö/ö,ü/ü,ß');
  // Unicode table
  AssertUTF8LowerCase('Latin 00C0 UTF8LowerCase', '', 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ', 'àáâãäåæçèéêëìíîï');
  AssertUTF8LowerCase('Latin 00D0 UTF8LowerCase', '', 'ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß', 'ðñòóôõö×øùúûüýþß');
  AssertUTF8LowerCase('Latin 00E0 UTF8LowerCase', '', 'àáâãäåæçèéêëìíîï', 'àáâãäåæçèéêëìíîï');
  AssertUTF8LowerCase('Latin 00F0 UTF8LowerCase', '', 'ðñòóôõö÷øùúûüýþÿ', 'ðñòóôõö÷øùúûüýþÿ');
  AssertUTF8LowerCase('Latin 0100 UTF8LowerCase', '', 'Āā Ăă Ąą Ćć Ĉĉ Ċċ Čč Ďď', 'āā ăă ąą ćć ĉĉ ċċ čč ďď');
  AssertUTF8LowerCase('Latin 0110 UTF8LowerCase', '', 'ĐđĒēĔĕĖėĘęĚěĜĝĞğ', 'đđēēĕĕėėęęěěĝĝğğ');
  AssertUTF8LowerCase('Latin 0120 UTF8LowerCase', '', 'ĠġĢģĤĥĦħĨĩĪīĬĭĮį', 'ġġģģĥĥħħĩĩīīĭĭįį');
  AssertUTF8LowerCase('Latin 0130 UTF8LowerCase', '', 'İıĲĳĴĵĶķĸĹĺĻļĽľĿ', 'iıĳĳĵĵķķĸĺĺļļľľŀ');
  AssertUTF8LowerCase('Latin 0140 UTF8LowerCase', '', 'ŀŁłŃńŅņŇňŉŊŋŌōŎŏ', 'ŀłłńńņņňňŉŋŋōōŏŏ');
  AssertUTF8LowerCase('Latin 0150 UTF8LowerCase', '', 'ŐőŒœŔŕŖŗŘřŚśŜŝŞş', 'őőœœŕŕŗŗřřśśŝŝşş');
  AssertUTF8LowerCase('Latin 0160 UTF8LowerCase', '', 'ŠšŢţŤťŦŧŨũŪūŬŭŮů', 'ššţţťťŧŧũũūūŭŭůů');
  AssertUTF8LowerCase('Latin 0170 UTF8LowerCase', '', 'ŰűŲųŴŵŶŷŸŹźŻżŽžſ', 'űűųųŵŵŷŷÿźźżżžžſ');
  AssertUTF8LowerCase('Latin 0180 UTF8LowerCase', '', 'ƀ Ɓ Ƃƃ Ƅƅ Ɔ Ƈƈ Ɖ Ɗ Ƌƌ ƍ Ǝ Ə', 'ƀ ɓ ƃƃ ƅƅ ɔ ƈƈ ɖ ɗ ƌƌ ƍ ǝ ə');
  AssertUTF8LowerCase('Latin 0190 UTF8LowerCase', '', 'ƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞƟ', 'ɛƒƒɠɣƕɩɨƙƙƚƛɯɲƞɵ');
  AssertUTF8LowerCase('Latin 01A0 UTF8LowerCase', '', 'ƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯ', 'ơơƣƣƥƥʀƨƨʃƪƫƭƭʈư');
  AssertUTF8LowerCase('Latin 01B0 UTF8LowerCase', '', 'ưƱƲƳƴƵƶƷƸƹƺƻƼƽƾƿ', 'ưʊʋƴƴƶƶʒƹƹƺƻƽƽƾƿ');
  AssertUTF8LowerCase('Latin 01C0 UTF8LowerCase', '', 'ǀǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏ', 'ǀǁǂǃǆǆǆǉǉǉǌǌǌǎǎǐ');
  AssertUTF8LowerCase('Latin 01D0 UTF8LowerCase', '', 'ǐǑǒǓǔǕǖǗǘǙǚǛǜǝǞǟ', 'ǐǒǒǔǔǖǖǘǘǚǚǜǜǝǟǟ');
  AssertUTF8LowerCase('Latin 01E0 UTF8LowerCase', '', 'ǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯ', 'ǡǡǣǣǥǥǧǧǩǩǫǫǭǭǯǯ');
  AssertUTF8LowerCase('Latin 01F0 UTF8LowerCase', '', 'ǰǱǲǳǴǵǶǷǸǹǺǻǼǽǾǿ', 'ǰǳǳǳǵǵƕƿǹǹǻǻǽǽǿǿ');
  AssertUTF8LowerCase('Latin 0200 UTF8LowerCase', '', 'ȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏ', 'ȁȁȃȃȅȅȇȇȉȉȋȋȍȍȏȏ');
  AssertUTF8LowerCase('Latin 0210 UTF8LowerCase', '', 'ȐȑȒȓȔȕȖȗȘșȚțȜȝȞȟ', 'ȑȑȓȓȕȕȗȗșșțțȝȝȟȟ');
  AssertUTF8LowerCase('Latin 0220 UTF8LowerCase', '', 'ȠȡȢȣȤȥȦȧȨȩȪȫȬȭȮȯ', 'ƞȡȣȣȥȥȧȧȩȩȫȫȭȭȯȯ');
  AssertUTF8LowerCase('Latin 0230 UTF8LowerCase', '', 'ȰȱȲȳȴȵȶȷȸȹȺȻȼȽȾȿ', 'ȱȱȳȳȴȵȶȷȸȹⱥȼȼƚⱦȿ');
  AssertUTF8LowerCase('Latin 0240 UTF8LowerCase', '', 'ɀɁɂɃɄɅɆɇɈɉɊɋɌɍɎɏ', 'ɀɂɂƀʉʌɇɇɉɉɋɋɍɍɏɏ');
  AssertUTF8LowerCase('Latin 0250 UTF8LowerCase', '', 'ɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟ', 'ɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟ');
  AssertUTF8LowerCase('Latin 0260 UTF8LowerCase', '', 'ɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯ', 'ɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯ');
  AssertUTF8LowerCase('Latin 0270 UTF8LowerCase', '', 'ɰɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿ', 'ɰɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿ');
  AssertUTF8LowerCase('Latin 0280 UTF8LowerCase', '', 'ʀʁʂʃʄʅʆʇʈʉʊʋʌʍʎʏ', 'ʀʁʂʃʄʅʆʇʈʉʊʋʌʍʎʏ');
  AssertUTF8LowerCase('Latin 0290 UTF8LowerCase', '', 'ʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟ', 'ʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟ');
  AssertUTF8LowerCase('Latin 02A0 UTF8LowerCase', '', 'ʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯ', 'ʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯ');
  AssertUTF8LowerCase('Unicode 0380 UTF8LowerCase', '', '΄΅Ά·ΈΉΊΌΎΏ', '΄΅ά·έήίόύώ');
  AssertUTF8LowerCase('Unicode 0390 UTF8LowerCase', '', 'ΐΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟ', 'ΐαβγδεζηθικλμνξο');
  AssertUTF8LowerCase('Unicode 03A0 UTF8LowerCase', '', 'ΠΡΣΤΥΦΧΨΩΪΫάέήί', 'πρστυφχψωϊϋάέήί');
  AssertUTF8LowerCase('Unicode 03B0 UTF8LowerCase', '', 'ΰαβγδεζηθικλμνξο', 'ΰαβγδεζηθικλμνξο');
  AssertUTF8LowerCase('Unicode 03C0 UTF8LowerCase', '', 'πρςστυφχψωϊϋόύώϏ', 'πρςστυφχψωϊϋόύώϗ');
  AssertUTF8LowerCase('Unicode 03D0 UTF8LowerCase', '', 'ϐϑϒϓϔϕϖϗϘϙϚϛϜϝϞϟ', 'ϐϑϒϓϔϕϖϗϙϙϛϛϝϝϟϟ');
  AssertUTF8LowerCase('Unicode 03E0 UTF8LowerCase', '', 'ϠϡϢϣϤϥϦϧϨϩϪϫϬϭϮϯ', 'ϡϡϣϣϥϥϧϧϩϩϫϫϭϭϯϯ');
  AssertUTF8LowerCase('Unicode 03F0 UTF8LowerCase', '', 'ϰϱϲϳϴϵ϶ϷϸϹϺϻϼϽϾϿ', 'ϰϱϲϳθϵ϶ϸϸϲϻϻϼͻͼͽ');
  AssertUTF8LowerCase('Unicode 0400 UTF8LowerCase', '', 'ЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ', 'ѐёђѓєѕіїјљњћќѝўџ');
  AssertUTF8LowerCase('Unicode 0410 UTF8LowerCase', '', 'АБВГДЕЖЗИЙКЛМНОП', 'абвгдежзийклмноп');
  AssertUTF8LowerCase('Unicode 0420 UTF8LowerCase', '', 'РСТУФХЦЧШЩЪЫЬЭЮЯ', 'рстуфхцчшщъыьэюя');
  AssertUTF8LowerCase('Unicode 0430 UTF8LowerCase', '', 'абвгдежзийклмноп', 'абвгдежзийклмноп');
  AssertUTF8LowerCase('Unicode 0440 UTF8LowerCase', '', 'рстуфхцчшщъыьэюя', 'рстуфхцчшщъыьэюя');
  AssertUTF8LowerCase('Unicode 0450 UTF8LowerCase', '', 'ѐёђѓєѕіїјљњћќѝўџ', 'ѐёђѓєѕіїјљњћќѝўџ');
  AssertUTF8LowerCase('Unicode 0460 UTF8LowerCase', '', 'ѠѡѢѣѤѥѦѧѨѩѪѫѬѭѮѯ', 'ѡѡѣѣѥѥѧѧѩѩѫѫѭѭѯѯ');
  AssertUTF8LowerCase('Unicode 0470 UTF8LowerCase', '', 'ѰѱѲѳѴѵѶѷѸѹѺѻѼѽѾѿ', 'ѱѱѳѳѵѵѷѷѹѹѻѻѽѽѿѿ');
  AssertUTF8LowerCase('Unicode 0480 UTF8LowerCase', '', 'Ҁҁ҂ ҃ ҄ ҅ ҆ ҇ ҈ ҉ҊҋҌҍҎҏ', 'ҁҁ҂ ҃ ҄ ҅ ҆ ҇ ҈ ҉ҋҋҍҍҏҏ');
  AssertUTF8LowerCase('Unicode 0490 UTF8LowerCase', '', 'ҐґҒғҔҕҖҗҘҙҚқҜҝҞҟ', 'ґґғғҕҕҗҗҙҙққҝҝҟҟ');
  AssertUTF8LowerCase('Unicode 04A0 UTF8LowerCase', '', 'ҠҡҢңҤҥҦҧҨҩҪҫҬҭҮү', 'ҡҡңңҥҥҧҧҩҩҫҫҭҭүү');
  AssertUTF8LowerCase('Unicode 04B0 UTF8LowerCase', '', 'ҰұҲҳҴҵҶҷҸҹҺһҼҽҾҿ', 'ұұҳҳҵҵҷҷҹҹһһҽҽҿҿ');
  AssertUTF8LowerCase('Unicode 04C0 UTF8LowerCase', '', 'ӀӁӂӃӄӅӆӇӈӉӊӋӌӍӎӏ', 'ӏӂӂӄӄӆӆӈӈӊӊӌӌӎӎӏ');
  AssertUTF8LowerCase('Unicode 04D0 UTF8LowerCase', '', 'ӐӑӒӓӔӕӖӗӘәӚӛӜӝӞӟ', 'ӑӑӓӓӕӕӗӗәәӛӛӝӝӟӟ');
  AssertUTF8LowerCase('Unicode 04E0 UTF8LowerCase', '', 'ӠӡӢӣӤӥӦӧӨөӪӫӬӭӮӯ', 'ӡӡӣӣӥӥӧӧөөӫӫӭӭӯӯ');
  AssertUTF8LowerCase('Unicode 04F0 UTF8LowerCase', '', 'ӰӱӲӳӴӵӶӷӸӹӺӻӼӽӾӿ', 'ӱӱӳӳӵӵӷӷӹӹӻӻӽӽӿӿ');
  AssertUTF8LowerCase('Unicode 0500 UTF8LowerCase', '', 'ԀԁԂԃԄԅԆԇԈԉԊԋԌԍԎԏ', 'ԁԁԃԃԅԅԇԇԉԉԋԋԍԍԏԏ');
  AssertUTF8LowerCase('Unicode 0510 UTF8LowerCase', '', 'ԐԑԒԓԔԕԖԗԘԙԚԛԜԝԞԟ', 'ԑԑԓԓԕԕԗԗԙԙԛԛԝԝԟԟ');
  AssertUTF8LowerCase('Unicode 0520 UTF8LowerCase', '', 'ԠԡԢԣԤԥԦԧ', 'ԡԡԣԣԥԥԧԧ');
  // Armenian Unicode Table
  AssertUTF8LowerCase('Unicode 0530 UTF8LowerCase', '', 'ԱԲԳԴԵԶԷԸԹԺԻԼԽԾԿ', 'աբգդեզէըթժիլխծկ');
  AssertUTF8LowerCase('Unicode 0540 UTF8LowerCase', '', 'ՀՁՂՃՄՅՆՇՈՉՊՋՌՍՎՏ', 'հձղճմյնշոչպջռսվտ');
  AssertUTF8LowerCase('Unicode 0550 UTF8LowerCase', '', 'ՐՑՒՓՔՕՖ', 'րցւփքօֆ');
  AssertUTF8LowerCase('Unicode 0560 UTF8LowerCase', '', 'աբգդեզէըթժիլխծկ', 'աբգդեզէըթժիլխծկ');
  AssertUTF8LowerCase('Unicode 0570 UTF8LowerCase', '', 'հձղճմյնշոչպջռսվտ', 'հձղճմյնշոչպջռսվտ');
  AssertUTF8LowerCase('Unicode 0580 UTF8LowerCase', '', 'րցւփքօֆ', 'րցւփքօֆ');
  // Higher Unicode Table
  AssertUTF8LowerCase('Unicode 1E00 UTF8LowerCase', '', 'ḀḁḂḃḄḅḆḇḈḉḊḋḌḍḎḏ', 'ḁḁḃḃḅḅḇḇḉḉḋḋḍḍḏḏ');
  AssertUTF8LowerCase('Unicode 1F00 UTF8LowerCase', '', 'ἀἁἂἃἄἅἆἇἈἉἊἋἌἍἎἏ', 'ἀἁἂἃἄἅἆἇἀἁἂἃἄἅἆἇ');
  AssertUTF8LowerCase('Unicode 1F10 UTF8LowerCase', '', 'ἐἑἒἓἔἕἘἙἚἛἜἝ', 'ἐἑἒἓἔἕἐἑἒἓἔἕ');
  AssertUTF8LowerCase('Unicode 1F20 UTF8LowerCase', '', 'ἠἡἢἣἤἥἦἧἨἩἪἫἬἭἮἯ', 'ἠἡἢἣἤἥἦἧἠἡἢἣἤἥἦἧ');
  AssertUTF8LowerCase('Unicode 1F30 UTF8LowerCase', '', 'ἰἱἲἳἴἵἶἷἸἹἺἻἼἽἾἿ', 'ἰἱἲἳἴἵἶἷἰἱἲἳἴἵἶἷ');
  AssertUTF8LowerCase('Unicode 1F40 UTF8LowerCase', '', 'ὀὁὂὃὄὅὈὉὊὋὌὍ', 'ὀὁὂὃὄὅὀὁὂὃὄὅ');
  AssertUTF8LowerCase('Unicode 1F50 UTF8LowerCase', '', 'ὐὑὒὓὔὕὖὗὙὛὝὟ', 'ὐὑὒὓὔὕὖὗὑὓὕὗ');
  AssertUTF8LowerCase('Unicode 1F60 UTF8LowerCase', '', 'ὠὡὢὣὤὥὦὧὨὩὪὫὬὭὮὯ', 'ὠὡὢὣὤὥὦὧὠὡὢὣὤὥὦὧ');
  AssertUTF8LowerCase('Unicode 1F70 UTF8LowerCase', '', 'ὰάὲέὴήὶίὸόὺύὼώ', 'ὰάὲέὴήὶίὸόὺύὼώ');
  AssertUTF8LowerCase('Unicode 1F80 UTF8LowerCase', '', 'ᾀᾁᾂᾃᾄᾅᾆᾇᾈᾉᾊᾋᾌᾍᾎᾏ', 'ᾀᾁᾂᾃᾄᾅᾆᾇᾀᾁᾂᾃᾄᾅᾆᾇ');
  AssertUTF8LowerCase('Unicode 1F90 UTF8LowerCase', '', 'ᾐᾑᾒᾓᾔᾕᾖᾗᾘᾙᾚᾛᾜᾝᾞᾟ', 'ᾐᾑᾒᾓᾔᾕᾖᾗᾐᾑᾒᾓᾔᾕᾖᾗ');
  AssertUTF8LowerCase('Unicode 1FA0 UTF8LowerCase', '', 'ᾠᾡᾢᾣᾤᾥᾦᾧᾨᾩᾪᾫᾬᾭᾮᾯ', 'ᾠᾡᾢᾣᾤᾥᾦᾧᾠᾡᾢᾣᾤᾥᾦᾧ');
  // Turkish
  AssertUTF8LowerCase('Turkish UTF8LowerCase 1', 'tr', 'abcçdefgğhııijklmnoöprsştuüvyz', 'abcçdefgğhııijklmnoöprsştuüvyz');
  AssertUTF8LowerCase('Turkish UTF8LowerCase 2', 'tr', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ', 'abcçdefgğhııijklmnoöprsştuüvyz');
  AssertUTF8LowerCase('Turkish UTF8LowerCase 3', 'tr', 'AhıIxXa', 'ahııxxa');
  // Cyrillic
  AssertUTF8LowerCase('Russian UTF8LowerCase 1', '', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит');
  AssertUTF8LowerCase('Russian UTF8LowerCase 2', '', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит');
  AssertUTF8LowerCase('Cyrillic UTF8UpperCase 1', '', 'Ѡѡ Ѣѣ Ѥѥ Ѧѧ Ѩѩ Ѫѫ Ѭѭ Ѯѯ Ѱѱ Ѳѳ Ѵѵ Ѷѷ Ѹѹ Ѻѻ Ѽѽ Ѿѿ Ҁҁ', 'ѡѡ ѣѣ ѥѥ ѧѧ ѩѩ ѫѫ ѭѭ ѯѯ ѱѱ ѳѳ ѵѵ ѷѷ ѹѹ ѻѻ ѽѽ ѿѿ ҁҁ');
  AssertUTF8LowerCase('Cyrillic UTF8UpperCase 2', '', 'Ҋҋ Ҍҍ Ҏҏ Ґґ Ғғ Ҕҕ Җҗ Ҙҙ Ққ Ҝҝ Ҟҟ Ҡҡ Ңң Ҥҥ Ҧҧ Ҩҩ Ҫҫ Ҭҭ Үү Ұұ Ҳҳ Ҵҵ Ҷҷ Ҹҹ Һһ Ҽҽ Ҿҿ', 'ҋҋ ҍҍ ҏҏ ґґ ғғ ҕҕ җҗ ҙҙ ққ ҝҝ ҟҟ ҡҡ ңң ҥҥ ҧҧ ҩҩ ҫҫ ҭҭ үү ұұ ҳҳ ҵҵ ҷҷ ҹҹ һһ ҽҽ ҿҿ');
  // What shouldnt change
  AssertUTF8LowerCase('Chinese UTF8LowerCase 1', '', '名字叫嘉英，嘉陵江的嘉，英國的英', '名字叫嘉英，嘉陵江的嘉，英國的英');
  // Georgian
  AssertUTF8LowerCase('Georgian UTF8LowerCase 1', '', 'Ⴀⴀ Ⴁⴁ Ⴂⴂ Ⴃⴃ Ⴄⴄ Ⴅⴅ Ⴆⴆ Ⴇⴇ Ⴈⴈ Ⴉⴉ Ⴊⴊ Ⴋⴋ Ⴌⴌ Ⴍⴍ Ⴎⴎ Ⴏⴏ Ⴐⴐ Ⴑⴑ', 'ⴀⴀ ⴁⴁ ⴂⴂ ⴃⴃ ⴄⴄ ⴅⴅ ⴆⴆ ⴇⴇ ⴈⴈ ⴉⴉ ⴊⴊ ⴋⴋ ⴌⴌ ⴍⴍ ⴎⴎ ⴏⴏ ⴐⴐ ⴑⴑ');
  AssertUTF8LowerCase('Georgian UTF8LowerCase 2', '', 'Ⴒⴒ Ⴓⴓ Ⴔⴔ Ⴕⴕ Ⴖⴖ Ⴗⴗ Ⴘⴘ Ⴙⴙ Ⴚⴚ Ⴛⴛ Ⴜⴜ Ⴝⴝ Ⴞⴞ Ⴟⴟ Ⴠⴠ Ⴡⴡ Ⴢⴢ Ⴣⴣ Ⴤⴤ Ⴥⴥ', 'ⴒⴒ ⴓⴓ ⴔⴔ ⴕⴕ ⴖⴖ ⴗⴗ ⴘⴘ ⴙⴙ ⴚⴚ ⴛⴛ ⴜⴜ ⴝⴝ ⴞⴞ ⴟⴟ ⴠⴠ ⴡⴡ ⴢⴢ ⴣⴣ ⴤⴤ ⴥⴥ');

  // repeat all tests with leading turkish i, to force offset
  // ASCII
  AssertUTF8LowerCase('Offset ASCII UTF8LowerCase', 'tr', 'IABCDEFGHIJKLMNOPQRSTUWVXYZ', 'ıabcdefghıjklmnopqrstuwvxyz');
  // Latin
  AssertUTF8LowerCase('Offset Portuguese UTF8LowerCase 1', 'tr', 'IÇ/ç Ã/ã Õ/õ Á/á É/é Í/í Ó/ó Ú/ú Ü/ü À/à Â/â Ê/ê Î/î Ô/ô Û/û', 'ıç/ç ã/ã õ/õ á/á é/é í/í ó/ó ú/ú ü/ü à/à â/â ê/ê î/î ô/ô û/û');
  AssertUTF8LowerCase('Offset French UTF8LowerCase 1', 'tr', 'IÀ/à Â/â æ Ç/ç É/é È/è Ê/ê Ë/ë Î/î Ï/ï Ô/ô œ Ù/ù Û/û Ü/ü Ÿ/ÿ', 'ıà/à â/â æ ç/ç é/é è/è ê/ê ë/ë î/î ï/ï ô/ô œ ù/ù û/û ü/ü ÿ/ÿ');
  AssertUTF8LowerCase('Offset Polish UTF8LowerCase 1', 'tr', 'Iaąbcćdeęfghijklłmnńoóprsśtuwyzźż', 'ıaąbcćdeęfghijklłmnńoóprsśtuwyzźż');
  AssertUTF8LowerCase('Offset Polish UTF8LowerCase 2', 'tr', 'IAĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ', 'ıaąbcćdeęfghıjklłmnńoóprsśtuwyzźż');
  AssertUTF8LowerCase('Offset German UTF8LowerCase 1', 'tr', 'IÄ/ä,Ö/ö,Ü/ü,ß', 'ıä/ä,ö/ö,ü/ü,ß');
  // Turkish
  AssertUTF8LowerCase('Offset Turkish UTF8LowerCase 1', 'tr', 'Iabcçdefgğhııijklmnoöprsştuüvyz', 'ıabcçdefgğhııijklmnoöprsştuüvyz');
  AssertUTF8LowerCase('Offset Turkish UTF8LowerCase 2', 'tr', 'IABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ', 'ıabcçdefgğhııijklmnoöprsştuüvyz');
  AssertUTF8LowerCase('Offset Turkish UTF8LowerCase 1', 'tr', 'IAhıIxXa', 'ıahııxxa');
  // Cyrillic
  AssertUTF8LowerCase('Offset Russian UTF8LowerCase 1', 'tr', 'IАБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ', 'ıабвеёжзклмнопрдйг суфхцчшщъыьэюяит');
  AssertUTF8LowerCase('Offset Russian UTF8LowerCase 2', 'tr', 'Iабвеёжзклмнопрдйг суфхцчшщъыьэюяит', 'ıабвеёжзклмнопрдйг суфхцчшщъыьэюяит');
  AssertUTF8LowerCase('Offset Cyrillic UTF8UpperCase 1', 'tr', 'IѠѡ Ѣѣ Ѥѥ Ѧѧ Ѩѩ Ѫѫ Ѭѭ Ѯѯ Ѱѱ Ѳѳ Ѵѵ Ѷѷ Ѹѹ Ѻѻ Ѽѽ Ѿѿ Ҁҁ', 'ıѡѡ ѣѣ ѥѥ ѧѧ ѩѩ ѫѫ ѭѭ ѯѯ ѱѱ ѳѳ ѵѵ ѷѷ ѹѹ ѻѻ ѽѽ ѿѿ ҁҁ');
  AssertUTF8LowerCase('Offset Cyrillic UTF8UpperCase 2', 'tr', 'IҊҋ Ҍҍ Ҏҏ Ґґ Ғғ Ҕҕ Җҗ Ҙҙ Ққ Ҝҝ Ҟҟ Ҡҡ Ңң Ҥҥ Ҧҧ Ҩҩ Ҫҫ Ҭҭ Үү Ұұ Ҳҳ Ҵҵ Ҷҷ Ҹҹ Һһ Ҽҽ Ҿҿ', 'ıҋҋ ҍҍ ҏҏ ґґ ғғ ҕҕ җҗ ҙҙ ққ ҝҝ ҟҟ ҡҡ ңң ҥҥ ҧҧ ҩҩ ҫҫ ҭҭ үү ұұ ҳҳ ҵҵ ҷҷ ҹҹ һһ ҽҽ ҿҿ');
  // What shouldnt change
  AssertUTF8LowerCase('Offset Chinese UTF8LowerCase 1', 'tr', 'I名字叫嘉英，嘉陵江的嘉，英國的英', 'ı名字叫嘉英，嘉陵江的嘉，英國的英');
  // Georgian
  AssertUTF8LowerCase('Offset Georgian UTF8LowerCase 1', 'tr', 'IႠⴀ Ⴁⴁ Ⴂⴂ Ⴃⴃ Ⴄⴄ Ⴅⴅ Ⴆⴆ Ⴇⴇ Ⴈⴈ Ⴉⴉ Ⴊⴊ Ⴋⴋ Ⴌⴌ Ⴍⴍ Ⴎⴎ Ⴏⴏ Ⴐⴐ Ⴑⴑ', 'ıⴀⴀ ⴁⴁ ⴂⴂ ⴃⴃ ⴄⴄ ⴅⴅ ⴆⴆ ⴇⴇ ⴈⴈ ⴉⴉ ⴊⴊ ⴋⴋ ⴌⴌ ⴍⴍ ⴎⴎ ⴏⴏ ⴐⴐ ⴑⴑ');
  // Georgian UTF8LowerCase 2 prevents any further output in console (in Manjaro Linux),
  //  you may want to comment it out.
  AssertUTF8LowerCase('Offset Georgian UTF8LowerCase 2', 'tr', 'IႲⴒ Ⴓⴓ Ⴔⴔ Ⴕⴕ Ⴖⴖ Ⴗⴗ Ⴘⴘ Ⴙⴙ Ⴚⴚ Ⴛⴛ Ⴜⴜ Ⴝⴝ Ⴞⴞ Ⴟⴟ Ⴠⴠ Ⴡⴡ Ⴢⴢ Ⴣⴣ Ⴤⴤ Ⴥⴥ', 'ıⴒⴒ ⴓⴓ ⴔⴔ ⴕⴕ ⴖⴖ ⴗⴗ ⴘⴘ ⴙⴙ ⴚⴚ ⴛⴛ ⴜⴜ ⴝⴝ ⴞⴞ ⴟⴟ ⴠⴠ ⴡⴡ ⴢⴢ ⴣⴣ ⴤⴤ ⴥⴥ');

  // Performance test
  Write('Mattias LowerCase- Performance test took:    ');
  for j := 0 to 9 do begin
    lStartTime := Now;
    for i := 0 to TimerLoop do
    begin
      if j = 0 then Str := UTF8LowerCaseViaTables('abcdefghijklmnopqrstuwvxyz');
      if j = 1 then Str := UTF8LowerCaseViaTables('ABCDEFGHIJKLMNOPQRSTUWVXYZ');
      if j = 2 then Str := UTF8LowerCaseViaTables('aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
      if j = 3 then Str := UTF8LowerCaseViaTables('AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
      if j = 4 then Str := UTF8LowerCaseViaTables('АБВЕЁЖЗКЛМНОПРДЙГ');
      if j = 5 then Str := UTF8LowerCaseViaTables('名字叫嘉英，嘉陵江的嘉，英國的英');
      if j = 6 then Str := UTF8LowerCaseViaTables('AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuWvVwXxYyZz');
      if j = 7 then Str := UTF8LowerCaseViaTables('AAaaBBbbCCccDDddEEeeFFffGGggHHhhIIiiJJjjKKkkLLllMMmm');
      if j = 8 then Str := UTF8LowerCaseViaTables('abcDefgHijkLmnoPqrsTuwvXyz');
      if j = 9 then Str := UTF8LowerCaseViaTables('ABCdEFGhIJKlMNOpQRStUWVxYZ');
    end;
    lTimeDiff := Now - lStartTime;
    Write(Format(' %7d ms ', [DateTimeToMilliseconds(lTimeDiff)]));
  end;
  WriteLn;
  Write('       LowerCase-- Performance test took:    ');
  for j := 0 to 9 do begin
    lStartTime := Now;
    for i := 0 to TimerLoop do
    begin
      if j = 0 then Str := UTF8LowerCase('abcdefghijklmnopqrstuwvxyz');
      if j = 1 then Str := UTF8LowerCase('ABCDEFGHIJKLMNOPQRSTUWVXYZ');
      if j = 2 then Str := UTF8LowerCase('aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
      if j = 3 then Str := UTF8LowerCase('AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
      if j = 4 then Str := UTF8LowerCase('АБВЕЁЖЗКЛМНОПРДЙГ');
      if j = 5 then Str := UTF8LowerCase('名字叫嘉英，嘉陵江的嘉，英國的英');
      if j = 6 then Str := UTF8LowerCase('AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuWvVwXxYyZz');
      if j = 7 then Str := UTF8LowerCase('AAaaBBbbCCccDDddEEeeFFffGGggHHhhIIiiJJjjKKkkLLllMMmm');
      if j = 8 then Str := UTF8LowerCase('abcDefgHijkLmnoPqrsTuwvXyz');
      if j = 9 then Str := UTF8LowerCase('ABCdEFGhIJKlMNOpQRStUWVxYZ');
    end;
    lTimeDiff := Now - lStartTime;
    Write(Format(' %7d ms ', [DateTimeToMilliseconds(lTimeDiff)]));
  end;
  WriteLn;
  Write('   Turk LowerCase-- Performance test took:    ');
  for j := 0 to 9 do begin
    lStartTime := Now;
    for i := 0 to TimerLoop do
    begin
      if j = 0 then Str := UTF8LowerCase('Iabcdefghijklmnopqrstuwvxyz', 'tr');
      if j = 1 then Str := UTF8LowerCase('IABCDEFGHIJKLMNOPQRSTUWVXYZ', 'tr');
      if j = 2 then Str := UTF8LowerCase('Iaąbcćdeęfghijklłmnńoóprsśtuwyzźż', 'tr');
      if j = 3 then Str := UTF8LowerCase('IAĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ', 'tr');
      if j = 4 then Str := UTF8LowerCase('IАБВЕЁЖЗКЛМНОПРДЙГ', 'tr');
      if j = 5 then Str := UTF8LowerCase('I名字叫嘉英，嘉陵江的嘉，英國的英', 'tr');
      if j = 6 then Str := UTF8LowerCase('IAaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuWvVwXxYyZz', 'tr');
      if j = 7 then Str := UTF8LowerCase('IAAaaBBbbCCccDDddEEeeFFffGGggHHhhIIiiJJjjKKkkLLllMMmm', 'tr');
      if j = 8 then Str := UTF8LowerCase('IabcDefgHijkLmnoPqrsTuwvXyz', 'tr');
      if j = 9 then Str := UTF8LowerCase('IABCdEFGhIJKlMNOpQRStUWVxYZ', 'tr');
    end;
    lTimeDiff := Now - lStartTime;
    Write(Format(' %7d ms ', [DateTimeToMilliseconds(lTimeDiff)]));
  end;
  WriteLn;
end;

begin
  WriteLn('===== StringReplace =====');
  TestUTF8StringReplace();
  WriteLn('======= UpperCase =======');
  TestUTF8UpperCase();
  WriteLn('======= LowerCase =======');
  TestUTF8LowerCase();
  WriteLn;
  WriteLn('Please press enter to continue');
  readln;
end.

