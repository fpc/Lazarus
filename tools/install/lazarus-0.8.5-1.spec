Name: lazarus
Version: 0.8.5
Release: 1
Copyright: GPL
Group: Development/Tools
Source: %{name}-%{version}.tgz
Summary: Lazarus Component Library and IDE
Packager: Mattias Gaertner (gaertner@informatik.uni-koeln.de)
URL: http://www.lazarus.freepascal.org/
BuildRoot: %{_tmppath}/lazarus-build
BuildRequires: fpc >= 1.0.7
Requires: fpcsrc >= 1.0.7

%define lazdir %{_datadir}/lazarus

%description
Lazarus is a free RAD tool for freepascal using the lazarus component library.

%prep
%setup -c

%build
  cd lazarus
  make

%install
  if [ %{buildroot} != "/" ]; then
  	rm -rf %{buildroot}
  fi
  mkdir -p %{buildroot}%{_datadir}/lazarus
  mkdir -p %{buildroot}%{_bindir}
  mkdir -p %{buildroot}%{_datadir}/pixmaps
  mkdir -p %{buildroot}%{_datadir}/gnome/apps/Development
  cp -a lazarus/* %{buildroot}%{_datadir}/lazarus/
  install -m 644 lazarus/images/ide_icon48x48.png %{buildroot}%{_datadir}/pixmaps/lazarus.png
  install -m 644 lazarus/gnome.ide.desktop %{buildroot}%{_datadir}/gnome/apps/Development/lazarus.desktop
  ln -sf %{lazdir}/lazarus %{buildroot}%{_bindir}/lazarus

%clean
  if [ %{buildroot} != "/" ]; then
  	rm -rf %{buildroot}
  fi

%files
%defattr(-,root,root)
%{_datadir}/lazarus
%{_bindir}/lazarus
%{_datadir}/pixmaps/lazarus.png
%{_datadir}/gnome/apps/Development/lazarus.desktop

%changelog


