; ******************************************************
; *** Inno Setup version 4.2.2+ PT_BR messages       ***
; ***                                                ***
; *** Original Author:                               ***
; ***                                                ***
; ***       Sérgio Marcelo da Silva Gomes      	     ***
; *** (smace at smace.com.br) < www.smace.com.br >   ***
; ***                                                ***
; ******************************************************
;
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/is3rdparty.php
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; $jrsoftware: issrc/Files/Default.isl,v 1.58 2004/04/07 20:17:13 jr Exp $

[LangOptions]
LanguageName=Português (Brasil)
LanguageID=$0416
LanguageCodePage=0
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=
;DialogFontSize=8
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;TitleFontName=Arial
;TitleFontSize=29
;CopyrightFontName=Arial
;CopyrightFontSize=8

[Messages]

; *** Application titles
SetupAppTitle=Instalação
SetupWindowTitle=Instalação - %1
UninstallAppTitle=Desinstalação
UninstallAppFullTitle=%1 Desinstalação

; *** Misc. common
InformationTitle=Informação
ConfirmTitle=Confirme
ErrorTitle=Erro

; *** SetupLdr messages
SetupLdrStartupMessage=Este é um instalador para %1. Deseja continuar?
LdrCannotCreateTemp=Não foi possível criar um arquivo temporário. Instalação cancelada
LdrCannotExecTemp=Não foi possível executar o arquivo no diretório temporário. Instalação cancelada

; *** Startup error messages
LastErrorMessage=%1.%n%nErro %2: %3
SetupFileMissing=Está faltando o arquivo %1 no diretório da instalação. Por favor, corrija o problema ou obtenha uma nova cópia do programa.
SetupFileCorrupt=Os arquivos da instalação estão corrompidos. Por favor obtenha uma nova cópia do programa.
SetupFileCorruptOrWrongVer=Os arquivos da instalação estão corrompidos, ou são incompatíveis com esta versão do Instalador. Por favor, corrija o problema ou obtenha uma nova cópia do programa.
NotOnThisPlatform=Este programa não vai ser executado em %1.
OnlyOnThisPlatform=Este programa precisa ser executado em %1.
WinVersionTooLowError=Este programa requer %1 versão %2 ou superior.
WinVersionTooHighError=Este programa não pode ser instalado no %1 versão %2 ou superior.
AdminPrivilegesRequired=Você precisa estar logado como administrador quando for instalar este programa.
PowerUserPrivilegesRequired=Você precisa estar logado como administrador ou como um usuário membro do Grupo de Usuários Poderosos quando for instalar este programa
SetupAppRunningError=O instalador detectou que %1 está em execução.%n%nPor favor feche todas as instâncias dele agora, então, clique em OK para continuar, ou Cancelar para sair.
UninstallAppRunningError=O desinstalador detectou que %1 está em execução.%n%nPor favor feche todas as instâncias dele agora, então, clique em OK para continuar, ou Cancelar para sair.

; *** Misc. errors
ErrorCreatingDir=O instalador não conseguiu criar o diretório "%1"
ErrorTooManyFilesInDir=Não foi possível criar um arquivo no diretório "%1" por que ele contém arquivos demais.

; *** Setup common messages
ExitSetupTitle=Sair do Instalador
ExitSetupMessage=A instalação ainda não está completa. Se você sair agora, o programa não vai ser instalado.%n%nVocê pode executar o Instalador denovo uma outra hora para completar a instalação. Sair do Instalador?
AboutSetupMenuItem=&Sobre o Instalador...
AboutSetupTitle=Sobre o Instalador
AboutSetupMessage=%1 versão %2%n%3%n%n%1 Página Inicial:%n%4
AboutSetupNote=Versão Português-Brasil. Traduzido por Sérgio Marcelo < smace at smace.com.br >

; *** Buttons
ButtonBack=< &Voltar
ButtonNext=&Próximo >
ButtonInstall=&Instalar
ButtonOK=OK
ButtonCancel=Cancelar
ButtonYes=&Sim
ButtonYesToAll=Sim para &Todos
ButtonNo=&Não
ButtonNoToAll=Nã&o para Todos
ButtonFinish=&Finalizar
ButtonBrowse=&Procurar...
ButtonWizardBrowse=Procura&r...
ButtonNewFolder=&Criar Nova Pasta

; *** "Select Language" dialog messages
SelectLanguageTitle=Selecione o Idioma do Instalador
SelectLanguageLabel=Selecione o idioma a ser usado durante a instalação:

; *** Common wizard text
ClickNext=Clique em Próximo para continuar, ou Cancelar para sair do Instalador.
BeveledLabel=
BrowseDialogTitle=Procurar Pasta
BrowseDialogLabel=Selecione uma pasta na lista abaixo, então clique OK.
NewFolderName=Nova Pasta

; *** "Welcome" wizard page
WelcomeLabel1=Bem vindo ao Instalador do [name]
WelcomeLabel2=Será instalado no seu computador o programa:%n[name/ver].%n%nÉ recomendado que você feche todos os outros aplicativos antes de continuar.

; *** "Password" wizard page
WizardPassword=Senha
PasswordLabel1=Esta instalação é protegida por senha.
PasswordLabel3=Por favor forneça a sua senha, então clique em Próximo para continuar. Senhas são case-sensitive. (Faz diferença 'a' e 'A')
PasswordEditLabel=&Senha:
IncorrectPassword=A senha que você informou não é correta. Por favor tente denovo.

; *** "License Agreement" wizard page
WizardLicense=Termos da Licença
LicenseLabel=Por favor leia atentamente as seguintes informações antes de continuar.
LicenseLabel3=Por favor leia os seguintes Termos da Licença. Você precisa aceitar os termos para este acordo antes de continuar com a instalação.
LicenseAccepted=Eu aceito os termo&s
LicenseNotAccepted=Eu &não aceito os termos

; *** "Information" wizard pages
WizardInfoBefore=Informação
InfoBeforeLabel=Por favor leia as seguintes informações importantes antes de continuar.
InfoBeforeClickLabel=Quando você estiver pronto para continuar com a Instalação, clique em Próximo.
WizardInfoAfter=Informação
InfoAfterLabel=Por favor leia as seguintes informações importantes antes de continuar.
InfoAfterClickLabel=Quando você estiver pronto para continuar com a Instalação, clique em Próximo.

; *** "User Information" wizard page
WizardUserInfo=Informações do Usuário
UserInfoDesc=Por favor complete as suas informações.
UserInfoName=&Nome do Usuário:
UserInfoOrg=&Organização:
UserInfoSerial=&Número de Série:
UserInfoNameRequired=Você precisa colocar um nome.

; *** "Select Destination Location" wizard page
WizardSelectDir=Selecione um Local de Destino
SelectDirDesc=Aonde o [name] deveria ser instalado?
SelectDirLabel3=O [name] vai ser instalado no seguinte diretório:
SelectDirBrowseLabel=Para continuar, clique em Próximo. Se você preferir um diretório diferente, clique em Procurar.
DiskSpaceMBLabel=É requerido no mínimo [mb] MB de espaço livre em disco
ToUNCPathname= O Instalador não pode instalar em um diretório UNC. Se você está tentando instalar em uma rede, você vai precisar mapear uma unidade de rede.
InvalidPath=You precisa informar um caminho com a letra da unidade; por exemplo:%n%nC:\APP%n%nou um caminho UNC no formato:%n%n\\servidor\compartilhamento
InvalidDrive=A unidade ou compartilhamento UNC que você selecionou não existe ou não é acessível. Por favor selecione outro.
DiskSpaceWarningTitle=Espaço insuficiente no disco
DiskSpaceWarning=A instalação requer no mínimo %1 KB de espaço livre para prosseguir, mas o drive selecionado tem somente %2 KB disponível.%n%nDeseja continuar mesmo assim?
DirNameTooLong=O nome do diretório ou caminho é grande demais.
InvalidDirName=O nome do diretório não é válido.
BadDirName32=O nome da pasta não pode incluir nenhum dos seguintes caracteres:%n%n%1
DirExistsTitle=Pasta Existe
DirExists=O diretório:%n%n%1%n%njá existe. Você gostaria de instalar nesta pasta mesmo assim?
DirDoesntExistTitle=Pasta Não Existe
DirDoesntExist=A pasta:%n%n%1%n%nnão existe. Deseja criá-la?


; *** "Select Components" wizard page
WizardSelectComponents=Selecione os Componentes
SelectComponentsDesc=Quais componentes deveriam ser instalados?
SelectComponentsLabel2=Selecione os componentes que você deseja instalar; desmarque os componentes que você não deseja instalar.
FullInstallation=Instalação Completa

; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Instalação Compacta
CustomInstallation=Instalação Personalizada
NoUninstallWarningTitle=O Componente Existe
NoUninstallWarning=O Instalador detectou que os seguintes componentes já estão instalado no seu computador:%n%n%1%n%nDesmarcando estes componentes não irá desinstalá-los.%n%nVocê gostaria de continuar mesmo assim?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=A seleção atual requer no mínimo [mb] MB de espaço em disco.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Selecione as Tarefas Adicionais
SelectTasksDesc=Quais tarefas adicionais deveriam ser realizadas?
SelectTasksLabel2=Selecione as tarefas adicionais que você gostaria que fossem realizadas durante a instalação do [name], então clique em Próximo.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Selecione a Pasta do Menu Iniciar
SelectStartMenuFolderDesc=Aonde deverá ser colocado os atalhos do programa?
SelectStartMenuFolderLabel3=Os atalhos do programa serão criados na seguinte pasta do Menu Iniciar:
SelectStartMenuFolderBrowseLabel=Para continuar, clique em Próximo. Se você preferir escolher uma pasta diferente, clique em Procurar.
NoIconsCheck=&Não criar ícones
MustEnterGroupName=Você precisa especificar um nome para a pasta.
GroupNameTooLong=O nome da pasta ou caminho é longo demais.
InvalidGroupName=O nome da pasta não é válido.
BadGroupName=O nome da pasta não pode conter nenhum dos seguintes caracteres:%n%n%1
NoProgramGroupCheck2=&Não crie uma pasta no Menu Iniciar


; *** "Ready to Install" wizard page
WizardReady=Pronto para Instalar
ReadyLabel1=O instalador está pronto para começar a instalar o [name] no seu computador.
ReadyLabel2a=Clique em instalar para continuar com a instalação, ou clique em Voltar se você quiser revisar ou mudar alguma configuração.
ReadyLabel2b=Clique em Instalar para continuar com a instalação.
ReadyMemoUserInfo=Informações do Usuário:
ReadyMemoDir=Diretório de Destino
ReadyMemoType=Tipo da Instalação:
ReadyMemoComponents=Componentes Selecionados:
ReadyMemoGroup=Pasta do Menu Iniciar:
ReadyMemoTasks=Tarefas Adicionais:

; *** "Preparing to Install" wizard page
WizardPreparing=Preparando Instalação
PreparingDesc=Preparando instalação do [name] no seu computador.

PreviousInstallNotCompleted=A instalação/remoção do programa anterior não foi concluída. É necessário reiniciar o computador para completar a instalação.%n%nDepois de reiniciar o seu computador, execute o Instalador novamente para completar a instalação do [name].

CannotContinue=A instalação não pode continuar. Por favor clique em cancelar para sair.

; *** "Installing" wizard page
WizardInstalling=Instalando
InstallingLabel=Por favor aguarde enquanto é instalado o [name] no seu computador.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Finalizando o Assistente de Instalação do [name]
FinishedLabelNoIcons=A instalação do [name] foi concluída com sucesso.
FinishedLabel=A instalação do [name] foi concluída com sucesso. O aplicativo pode ser iniciado à partir dos ícones instalados.
ClickFinish=Clique em Finalizar para Sair da Instalação.
FinishedRestartLabel=Para completar a instalação do [name], é necessário reiniciar o seu computador. Deseja Reiniciar agora?
FinishedRestartMessage=Para completar a instalação do [name], é necessário reiniciar o seu computador.%n%nDeseja Reiniciar agora?
ShowReadmeCheck=Sim, Eu gostaria de ver o arquivo LEIAME
YesRadio=&Sim, Reiniciar o computador agora
NoRadio=&Não, Eu vou reiniciar mais tarde

; used for example as 'Run MyProg.exe'
RunEntryExec=Executar %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Exibir %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Insira o próximo disco
SelectDiskLabel2=Por favor insira o Disco %1 e clique OK.%n%NSe os arquivos no disco podem ser encontrados em uma pasta diferente da mostrada abaixo, insira o caminho correto ou clique em Procurar.
PathLabel=&Caminho:
FileNotInDir2=O arquivo "%1" não pode ser localizado em "%2". Por favor insira o disco correto ou selecione uma outra pasta.
SelectDirectoryLabel=Por favor especifique a localização do próximo disco.

; *** Installation phase messages
SetupAborted=A instalação não foi completada.%n%nPor favor corrija o problema e execute a Instalação denovo.
EntryAbortRetryIgnore=Clique em Repetir para tentar denovo, Ignorar para proceder mesmo assim, ou Abortar pra cancelar a instalação.

; *** Installation status messages
StatusCreateDirs=Criando diretórios...
StatusExtractFiles=Extraindo arquivos...
StatusCreateIcons=Criando atalhos...
StatusCreateIniEntries=Criando entradas INI...
StatusCreateRegistryEntries=Criando entradas de registro...
StatusRegisterFiles=Registrando arquivo...
StatusSavingUninstall=Salvando informações para desinstalação...
StatusRunProgram=Finalizando instalação...
StatusRollback=Revertendo mudanças...

; *** Misc. errors
ErrorInternal2=Erro Interno: %1
ErrorFunctionFailedNoCode=%1 falhou
ErrorFunctionFailed=%1 falhou; código %2
ErrorFunctionFailedWithMessage=%1 falhou; código %2.%n%3
ErrorExecutingProgram=Não foi possível executar o arquivo:%n%1

; *** Registry errors
ErrorRegOpenKey=Erro ao abrir a chave de registro:%n%1\%2
ErrorRegCreateKey=Erro ao criar a chave de registro:%n%1\%2
ErrorRegWriteKey=Erro ao gravar na chave de registro:%n%1\%2

; *** INI errors
ErrorIniEntry=Erro criando entrada INI no arquivo "%1".

; *** File copying errors
FileAbortRetryIgnore=Clique em Repetir para tentar denovo, Ignorar para pular este arquivo (não recomendado), ou Abortar para cancelar a instalação.
FileAbortRetryIgnore2=Clique em Repetir para tentar denovo, Ignorar para proceder mesmo assim (não recomendado), ou Abortar para cancelar a instalação.
SourceIsCorrupted=O arquivo fonte está corrompido
SourceDoesntExist=O arquivo fonte "%1" não existe
ExistingFileReadOnly=O arquivo existe está como somente-leitura.%n%nClique em repetir para remover a propriedade Somente Leitura e tentar denovo, Ignorar para pular este arquivo, ou Abortar para cancelar a instalação.
ErrorReadingExistingDest=Um erro ocorreu ao tentar ler o arquivo existente:
FileExists=O arquivo já existe.%n%nDeseja sobrescrevê-lo?
ExistingFileNewer=O arquivo existente é mais novo do que o Instalador está tentando instalar. É recomendado que você mantenha o arquivo existente. Deseja manter o arquivo existente?
ErrorChangingAttr=Um erro ocorreu enquanto tentava mudar as propriedades do arquivo existente:
ErrorCreatingTemp=Um erro ocorreu enquanto tentava criar um arquivo no diretório de destino:
ErrorReadingSource=Um erro ocorreu enquanto tentava ler o código fonte:
ErrorCopying=Um erro ocorreu enquanto tentva copiar o arquivo:
ErrorReplacingExistingFile=Um erro ocorreu enquanto tentava substituir o arquivo existente:
ErrorRestartReplace=Reinicio de Substituição falhou:
ErrorRenamingTemp=Um erro ocorreu enquanto tentava renomear um arquivo no diretório de destino:
ErrorRegisterServer=Não foi possível registar a DLL/OCX:%1
ErrorRegisterServerMissingExport=A exportação da DllRegisterServer não foi encontrada
ErrorRegisterTypeLib=Não foi possível registrar a biblioteca de tipo: %1

; *** Post-installation errors
ErrorOpeningReadme=Um erro ocorreu enquanto tentava abrir o arquivo LEIAME.
ErrorRestartingComputer=O instalador não conseguiu reiniciar o computador. Por favor, faça isso manualmente.



; *** Uninstaller messages
UninstallNotFound=O arquivo "%1" não existe. Não é possível desinstalar.
UninstallOpenError=O arquivo "%1" não pode ser aberto. Não é possível desinstalá-lo
UninstallUnsupportedVer=O arquivo de log "%1" do desinstalador está em formato não reconhecível por esta versão do desinstalador. Não é possível desinstalar
UninstallUnknownEntry=Uma entrada desconhecida (%1) foi encontrada no log de desinstalação
ConfirmUninstall=Você tem certeza que deseja remover complementamente %1 e todos os seus componentes?
OnlyAdminCanUninstall=Esta instalação pode somente ser desinstalada por um usuário com privilégios de administrador.
UninstallStatusLabel=Por favor aguarde enquanto %1 é removido do seu computador.
UninstalledAll=%1 foi removido com sucesso do seu computador.
UninstalledMost=Desinstalação do %1 completa.%n%nAlguns elementos não puderam ser removidos. Eles podem ser removidos manualmente.
UninstalledAndNeedsRestart=Para completar a desinstalação do %1, seu computador precisa ser reiniciado.%n%nGostaria de reiniciá-lo agora?
UninstallDataCorrupted=O arquivo "%1" está corrompido. Não é possível Desinstalar

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Remover Arquivo Compartilhado?

ConfirmDeleteSharedFile2=O sistema informa que os seguintes arquivos compatilhados não estão mais em uso por nenhum programa. Você gostaria que estes arquivos compartilhados sejam removidos?%n%nSe algum programa ainda estiver usando este arquivo e ele for removido, estes programas podem não mais funcionar corretamente. Se você não tiver certeza, escolha Não. Deixando o arquivo no seu sistema não lhe causará mal algum.


SharedFileNameLabel=Nome do Arquivo:
SharedFileLocationLabel=Localização:
WizardUninstalling=Status da Desinstalação
StatusUninstalling=Desinstalando %1...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versão %2
AdditionalIcons=Ícones adicionais:
CreateDesktopIcon=Criar um ícone na área de traba&lho
CreateQuickLaunchIcon=Criar um ícone na Iniciali&zação Rápida
ProgramOnTheWeb=%1 na Internet
UninstallProgram=Desinstalar %1
LaunchProgram=Executar %1
AssocFileExtension=&Associar %1 com a extensão do arquivo %2
AssocingFileExtension=Associar %1 com a extensão do arquivo %2...
