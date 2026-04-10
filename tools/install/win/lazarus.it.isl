#include "compiler:Languages\Italian.isl"

[CustomMessages]


DelUserConf=Elimina file configurazione utente installazioni precedenti
CleanUp=Pulizia:

InstallQtLocal=Installa DLL interfaccia Qt4
InstallQt=Installa globalmente DLL interfaccia Qt4
InstallQt5Local=Installa DLL interfaccia Qt5
InstallQt5Global=Installa globalmente DLL interfaccia Qt5
InstallChm=Installa file guida CHM
InstallOpenSSL=Installa librerie openssl (richiesto da OnlinePackageManager)
InstallOpenSSLGlobal=Installa globalmente librerie openssl
AssociateGroup=Associazione estensioni file

CheckSecondClick=Crea nuova installazione secondaria
CheckSecondInfo=Un'installazione secondaria consente di installare due o più versioni di Lazarus.%nOgni versione avrà la propria configurazione.%nPrima di usare questa opzione leggi le FAQ (domande frequenti) sulle installazioni multiple.

FolderHasSpaces=La cartella selezionata contiene spazi, seleziona una cartella senza spazi al suo interno.
FolderNotEmpty=La cartella destinazione non è vuota. Vuoi continuare con l'installazione?
FolderNotEmpty2=La cartella destinazione non è vuota.

FolderForSecondNoFile=La cartella destinazione non è vuota e non contiene un'installazione secondaria di Lazarus aggiornabile.%0:sScegli una cartella vuota o una cartella con un'installazione secondaria di Lazarus esistente per l'aggiornamento.
FolderForSecondBadFile=La cartella destinazione non è vuota.%0:sIl programma di installazione non è riuscito a rilevare se contiene un'installazione secondaria di Lazarus aggiornabile.%0:sScegli una cartella vuota o una cartella con un'installazione secondaria di Lazarus esistente per l'aggiornamento.
FolderForSecondUpgrading=La cartella destinazione non è vuota.%0:sContiene un'installazione secondaria di Lazarus che usa la seguente cartella per la configurazione:%0:s%1:s%0:s%0:sVuoi continuare con l'installazione?
FolderForSecondUpgradingPrimary=La cartella destinazione non è vuota.%0:sContiene un'installazione Lazarus predefinita (non secondaria).%0:sSe continui, verrà modificata in un'installazione secondaria.%0:s%0:s%0:sVuoi continua con installazione?

FolderForSecondBadUninstall=La cartella destinazione non è vuota.%0:sIl programma di installazione non è riuscito a verificare se è sicuro da usare.%0:sScegli una cartella vuota o una cartella con un'installazione secondaria di Lazarus esistente per l'aggiornamento.

SecondConfCapt=Selezione cartella configurazione
SecondConfCapt2=Dove vuoi che questa installazione di Lazarus memorizzi la configurazione?
SecondConfBody=Seleziona una nuova cartella vuota per questa la configurazione dell'installazione di Lazarus, quindi seleziona con 'Avanti'.

FolderForConfig=Cartella configurazione

FolderForConfNotEmpty=La cartella selezionata non è vuota.

AskUninstallTitle1=Installazione precedente
AskUninstallTitle2=Vuoi eseguire il programma di disinstallazione?
BtnUninstall=Disinstalla
ChkContinue=Continua senza disinstallare

OldInDestFolder1=Nella cartella di destinazione esiste un'altra installazione di %1:s. Se vuoi prima disinstallarla, usa il pulsante qui sotto.
OldInDestFolder2=
OldInDestFolder3=
OldInDestFolder4=

OldInOtherFolder1=È stata trovata un'altra installazione di %1:s in %2:s. Usa il pulsante qui sotto per disinstallarla ora. Se vuoi mantenerla, per continuare seleziona la casella di controllo.
OldInOtherFolder2=Attenzione: potrebbero esserci conflitti tra le diverse installazioni e potrebbero non funzionare correttamente.
OldInOtherFolder3=Nota: non hai scelto una cartella di configurazione dedicata per questa installazione.
OldInOtherFolder4=Se vuoi avere più di un'installazione, torna indietro e seleziona: "Crea nuova installazione secondaria".

OldInBadFolder1=Avviso: è stata trovata un'altra installazione di %1:s in %2:s, ma il programma di disinstallazione è stato trovato in %3:s. Assicurati che il programma di disinstallazione sia corretto.
OldInBadFolder2=Attenzione: potrebbero esserci conflitti tra le diverse installazioni e potrebbero non funzionare correttamente.
OldInBadFolder3=Nota: se vuoi avere più di un'installazione, torna indietro e seleziona: "Crea nuova installazione secondaria".
OldInBadFolder4=Usa il pulsante qui sotto per disinstallarla ora. Se vuoi mantenerla, per continuare seleziona la casella di controllo.

OldSecondInDestFolder1=Nella cartella destinazione esiste un'altra installazione di %1:s. Se vuoi prima disinstallarla, usa il pulsante qui sotto.
OldSecondInDestFolder2=
OldSecondInDestFolder3=Questa è un'installazione secondaria e la cartella per la configurazione è (e verrà mantenuta):
OldSecondInDestFolder4=%4:s

OldSecondInOtherFolder1=
OldSecondInOtherFolder2=
OldSecondInOtherFolder3=
OldSecondInOtherFolder4=

OldSecondInBadFolder1=
OldSecondInBadFolder2=
OldSecondInBadFolder3=
OldSecondInBadFolder4=

SecondTaskUpdate=Aggiornamento installazione secondaria con la configurazione nella cartella:%0:s%1:s%2:s
SecondTaskCreate=Creazione installazione secondaria con configurazione nella cartella:%0:s%1:s%2:s

DuringInstall=Dopo l'installazione:%0:s- Se stai aggiornando e hai dei pacchetti installati, ricostruisci l'IDE per rendere nuovamente disponibili i pacchetti.%0:s- A volte l'aggiornamento richiede prima di disinstallare la vecchia versione. Se ricevi errori durante la ricostruzione dell'IDE, potrebbe essere questo il caso.%0:s%0:s    Licenza:%0:s- LCL è concesso in licenza LGPL con eccezione di collegamento. Ciò ti consente di creare app con qualsiasi licenza desideri, inclusa quella proprietaria.%0:s- L'IDE è concesso in licenza GPL. Se distribuisci un IDE modificato devi seguire la GPL.%0:s- Altri pacchetti e componenti hanno varie licenze. Vedi il file readme di ogni pacchetto.%0:s%0:s    Debug:%0:s- Potrebbe esserti chiesto di scegliere informazioni di debug. Con le impostazioni di debug predefinite ("FpDebug"), puoi scegliere "DWARF-3", altrimenti "DWARF-2 (con set)". Il wiki fornisce maggiori dettagli in "DWARF" e "Debugger Setup".%0:s%0:s    Più componenti e pacchetti:%0:s- Pacchetti aggiuntivi sono disponibili tramite OPM (Online Package Manager) nel menu pacchetti.%0:s%0:s    Puoi trovare le nostre FAQ in http://wiki.lazarus.freepascal.org/Lazarus_Faq  %0:s

UninstVerbose=Disinstallazione di %1:s dalla cartella %0:s. Vuoi continue?
