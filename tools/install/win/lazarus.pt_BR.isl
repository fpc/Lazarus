#include "compiler:Languages\BrazilianPortuguese.isl"

[CustomMessages]


DelUserConf=Excluir todos os arquivos de configuração do usuário das instalações anteriores
CleanUp=Limpeza

InstallQtLocal=Instalar a DLL da interface Qt4
InstallQt=Instalar globamente a DLL da interface Qt4
InstallQt5Local=Instalar a DLL da interface Qt5
InstallQt5Global=Instalar globalmente a DLL da interface Qt5
InstallChm=Instalar os arquivos de ajuda CHM
InstallOpenSSL=Instalar as bibliotecas do openssl(requeridas pelo Gerenciador de pacotes online)
InstallOpenSSLGlobal=Instalar globalmente as bibliotecas do openssl
AssociateGroup=Associar extensões de arquivos

CheckSecondClick=Criar uma nova instalação secundária
CheckSecondInfo=Uma instalação secundária permite duas ou mais versões do Lazarus serem instaladas. Cada versão terá suas próprias configurações. Favor ler o FAQ sobre múltiplas instalações antes de usar esta opção.

FolderHasSpaces=A pasta selecionada contém espaços, favor selecionar outra pasta.
FolderNotEmpty=A pasta destino não está vazia. Continuar instalação?
FolderNotEmpty2=A pasta destino não está vazia.

FolderForSecondNoFile=A pasta destino não está vazia e não contém uma instalação secundária do Lazarus atualizável.%0:sFavor selecionar uma pasta vazia, ou uma pasta com uma instalação secundária do Lazarus existente para atualização.
FolderForSecondBadFile=A pasta destino não está vazia. O instalador não pode detectar se ela contém uma instalação secundária atualizável do Lazarus.%0:sFavor selecionar uma pasta vazia, ou uma pasta com uma instalação secundária do Lazarus existente para atualização.
FolderForSecondUpgrading=A pasta destino não está vazia.%0:sEla contém uma instalação secundária do Lazarus usando a seguinte pasta para configuração:%0:s%1:s%0:s%0:sContinuar instalação?
FolderForSecondUpgradingPrimary=A pasta destino não está vazia.%0:sEla contém uma instalação padrão [não-secundária] do Lazarus.%0:sSe você continuar, ela será alterada para uma instalação secundária.%0:s%0:s%0:sContinuar instalação?

FolderForSecondBadUninstall=A pasta destino não está vazia. O instalador não pode verificar se é seguro utilizá-la.%0:sFavor selecionar uma pasta vazia, ou uma pasta com uma instalação secundária do Lazarus existente para atualização.

SecondConfCapt=Selecione pasta para configuração
SecondConfCapt2=Onde você deseja que esta instalação do Lazarus armazene suas configurações?
SecondConfBody=Selecione uma nova pasta vazia para que esta instalação do Lazarus armazene suas configurações, então continue com 'Próximo'.

FolderForConfig=Pasta para configuração

FolderForConfNotEmpty=A pasta selecionada não está vazia.

AskUninstallTitle1=Instalação anterior
AskUninstallTitle2=Deseja executar o desinstalador?
BtnUninstall=Desinstalar
ChkContinue=Continuar sem desinstalar

OldInDestFolder1=Outra instalação do %1:s já existe na pasta destino. Se desejar desinstalar primeiro, clique o botão abaixo.
OldInDestFolder2=
OldInDestFolder3=
OldInDestFolder4=

OldInOtherFolder1=Outra instalação do %1:s encontrada em %2:s. Clique o botão abaixo para desinstalar agora. Se desejar mantê-la, marque o checkbox para continuar.
OldInOtherFolder2=Aviso: Pode haver conflitos entre as diferentes instalações e elas podem não funcionar apropriadamente.
OldInOtherFolder3=Nota: Você não selecionou uma pasta dedicada para as configurações desta instalação.
OldInOtherFolder4=Se você deseja usar mais de uma instalação, favor voltar e selecionar "Criar uma nova instalação secundária".

OldInBadFolder1=Aviso: Outra instalação do %1:s foi encontrada em %2:s, mas o desinstalador foi encontrado em %3:s. Favor verificar se o desinstalador está correto.
OldInBadFolder2=Aviso: Pode haver conflitos entre as diferentes instalações e elas podem não funcionar apropriadamente.
OldInBadFolder3=Nota: Se você deseja usar mais de uma instalação, favor voltar e selecionar "Criar uma nova instalação secundária".
OldInBadFolder4=Clique o botão abaixo para desinstalar agora. Se desejar mantê-la, marque o checkbox para continuar.

OldSecondInDestFolder1=Outra instalação do %1:s existe na pasta destino. Se desejar desinstalar primeiro, clique o botão abaixo.
OldSecondInDestFolder2=
OldSecondInDestFolder3=Esta é uma instalação secundária e a pasta para configuração é (e será mantida):
OldSecondInDestFolder4=%4:s

OldSecondInOtherFolder1=
OldSecondInOtherFolder2=
OldSecondInOtherFolder3=
OldSecondInOtherFolder4=

OldSecondInBadFolder1=
OldSecondInBadFolder2=
OldSecondInBadFolder3=
OldSecondInBadFolder4=

SecondTaskUpdate=Atualizando instalação secundária com a configuração na pasta:%0:s%1:s%2:s
SecondTaskCreate=Criando instalação secundária com a configuração na pasta:%0:s%1:s%2:s

DuringInstall=Algumas informações do nosso FAQ: http://wiki.lazarus.freepascal.org/Lazarus_Faq%0:s%0:s    O que é o Lazarus?%0:sLazarus é uma IDE cross-plataforma para o Pascal. Seu objetivo é escrever uma vez, compilar em qualquer lugar.%0:s%0:s    Como reduzir o tamanho do arquivo exe?%0:sOs binários padrão são grandes porque eles incluem as informações de depuração. Para versões finais você pode desligar isso nas configurações do Projeto.%0:s%0:s    Licenciamento:%0:s- A LCL é licenciada LGPL com exceção de vinculação. Isto permite à você criar apss com qualquer licença que desejar, incluindo proprietária.%0:s- A IDE é licenciada GPL. Se você distribuir uma IDE modificada, deverá seguir a GPL.%0:s- Outros pacotes e componentes têm várias licenças. Veja o arquivo readme de cada pacote.

UninstVerbose=Prestes à desinstalar o %1:s da pasta %0:s. Continuar?
