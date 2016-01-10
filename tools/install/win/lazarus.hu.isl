#include "compiler:Hungarian.isl"

[CustomMessages]


DelUserConf=Az előző telepítések összes felhasználói beállításának törlése
CleanUp=Tisztítás:

InstallQt=Qt felülethez szükséges DLL fájlok telepítése
InstallChm=CHM súgófájlok telepítése
AssociateGroup=Fájlkiterjesztések hozzárendelése

CheckSecondClick=Új másodlagos telepítés létrehozása
CheckSecondInfo=A másodlagos telepítés lehetővé teszi, hogy  kettő vagy több Lazarus változat legyen telepítve. Minden változat a saját beállításaival fog működni. Javasoljuk elolvasni a GY.I.K. több példányos telepítésről szóló részét, ezen beállítás használata előtt.

FolderHasSpaces=A kiválasztott könyvtár neve szóközt tartalmaz. Olyan könyvtárat kell választani, melynek nevében nincs szóköz.
FolderNotEmpty=A célkönyvtár nem üres. Biztosan folytatni szeretné a telepítést?
FolderNotEmpty2=A célkönyvtár nem üres.

FolderForSecondNoFile=A célkönyvtár nem üres és nem tartalmaz frissíthető másodlagos Lazarus telepítést.%0:sVálasszon üres könyvtárat vagy egy olyat, amelyben frissíthető másodlagos Lazarus telepítés található.
FolderForSecondBadFile=A célkönyvtár nem üres. A telepítő nem tudta kideríteni, hogy tartalmaz-e frissíthető másodlagos Lazarus telepítést.%0:sVálasszon üres könyvtárat vagy egy olyat, amelyben frissíthető másodlagos Lazarus telepítés található.
FolderForSecondUpgrading=A célkönyvtár nem üres.%0:sA könyvtárban egy másodlagos Lazarus telepítés található, amely a következő könyvtárban tárolja a beállításait:%0:s%1:s%0:s%0:sTelepítés folytatása?
FolderForSecondUpgradingPrimary=A célkönyvtár nem üres.%0:sA könyvtárban egy alapértelmezett (nem másodlagos) Lazarus telepítés található.%0:sHa folytatja másodlagos telepítéssé lesz alakítva.%0:s%0:s%0:sTelepítés folytatása?

FolderForSecondBadUninstall=A célkönyvtár nem üres. A telepítő nem tudta ellenőrizni, hogy a használata biztonságos-e.%0:sVálasszon üres könyvtárat vagy egy olyat, amelyben frissíthető másodlagos Lazarus telepítés található.

SecondConfCapt=Beállítások könyvtárának kiválasztása
SecondConfCapt2=Hol legyenek tárolva ezen Lazarus telepítés beállítási fájljai?
SecondConfBody=Válasszon egy üres könyvtárat ezen Lazarus telepítés számára, ahol az a beállításait tárolja majd. Folytatás a 'Tovább' gombbal.

FolderForConfig=Beállítások könyvtára

FolderForConfNotEmpty=A kiválasztott könyvtár nem üres.

AskUninstallTitle1=Előző telepítés
AskUninstallTitle2=Szeretné futtatni az eltávolítót?
BtnUninstall=Eltávolítás
ChkContinue=Folytatás eltávolítás nélkül

OldInDestFolder1=Egy másik %1:s telepítés található a célkönyvtárban. Ha előbb el szeretné távolítani használja a lenti gombot.
OldInDestFolder2=
OldInDestFolder3=
OldInDestFolder4=

OldInOtherFolder1=Egy másik %1:s telepítés található itt: %2:s. Használja az lenti gombot az eltávolításhoz. Ha meg szeretné tartani jelölje be a négyzetet folytatás előtt.
OldInOtherFolder2=Figyelmeztetés: Ütközés lehet a különböző telepítések között és esetleg nem működnek majd megfelelően.
OldInOtherFolder3=Megjegyzés: Nem választott az e telepítésnek megfelelő beállítási könyvtárat.
OldInOtherFolder4=Ha egynél több telepítést szeretne használni lépjen vissza és válassza a "Új másodlagos telepítés létrehozása" lehetőséget.

OldInBadFolder1=Figyelmeztetés: Egy másik %1:s telepítés található itt: %2:s, de az eltávolító nem található meg a következő helyen: %3:s. Győződjön meg róla, hogy az eltávolitó megfelelő-e.
OldInBadFolder2=Figyelmeztetés: Ütközés lehet a különböző telepítések között és esetleg nem működnek majd megfelelően.
OldInBadFolder3=Megjegyzés: Ha egynél több telepítést szeretne használni lépjen vissza és válassza a "Új másodlagos telepítés létrehozása" lehetőséget.
OldInBadFolder4=Használja az lenti gombot az eltávolításhoz. Ha meg szeretné tartani jelölje be a négyzetet folytatás előtt.

OldSecondInDestFolder1=Egy másik %1:s telepítés található a célkönyvtárban. Ha előbb el szeretné távolítani használja a lenti gombot.
OldSecondInDestFolder2=
OldSecondInDestFolder3=Ez egy másodlagos telepítés, melynek beállítása a következő könyvtárban vannak (és megmaradnak):
OldSecondInDestFolder4=%4:s

OldSecondInOtherFolder1=
OldSecondInOtherFolder2=
OldSecondInOtherFolder3=
OldSecondInOtherFolder4=

OldSecondInBadFolder1=
OldSecondInBadFolder2=
OldSecondInBadFolder3=
OldSecondInBadFolder4=

SecondTaskUpdate=Másodlagos Lazarus telepítés frissítése, melynek beállításai a következő könyvtárban lesznek:%0:s%1:s%2:s
SecondTaskCreate=Másodlagos Lazarus telepítés létrehozása, melynek beállításai a következő könyvtárban lesznek:%0:s%1:s%2:s

DuringInstall=Néhány információ a GY.I.K.-ból: http://wiki.lazarus.freepascal.org/Lazarus_Faq/hu%0:s%0:s    Mi a Lazarus?%0:sA Lazarus egy keresztplatformos IDE a Pascal nyelvhez. Jelmondata: írd meg egyszer, fordítsd le bárhol.%0:s%0:s    Hogyan csökkenthető a futtatható állomány mérete?%0:sA binárisok alapértelmezett esetben nagyon nagyok, mert hibakeresési információkat tartalmaznak. A kiadásra kész változatokhoz ez kikapcsolható a projek beállításainál.%0:s%0:s    Licencelés:%0:s- Az LCL használata az LGPL szerint engedélyezett összefűzési (linking) kivétellel. Ez lehetővé teszi a felhasználónak alkalmazások létrehozását tetszőleges licenc alatt, beleértve egyénit is.%0:s- Az IDE maga GPL licencű. Módosított IDE közreadása esetén be kell tartani a GPL előírásait.%0:s- Egyéb csomagok és komponensek különböző saját licencekkel rendelkeznek. Lásd a csomagok információit.

UninstVerbose=%1:s eltávolítása következő könyvtárból: %0:s. Folytatás?
