Name: lazarus
Version: LAZVERSION
Release: LAZRELEASE
Copyright: LGPL2/GPL2
Group: Development/Tools
Source: LAZSOURCE
Summary: Lazarus Component Library and IDE
Packager: Mattias Gaertner (mattias@freepascal.org)
URL: http://www.lazarus.freepascal.org/
BuildRoot: %{_tmppath}/lazarus-build
BuildRequires: fpc >= 1.0.10
Requires: fpcsrc >= 1.0.10
Requires: gdk-pixbuf-devel >= 0.18.0

%define lazdir %{_datadir}/lazarus

%description
Lazarus is a free and opensource RAD tool for freepascal using the lazarus
component library - LCL, which is also included in this package.
The LCL is licensed under LGPL2, the IDE is licensed under GPL2.

%prep
%setup -c

%build
  cd lazarus
  make
  strip lazarus

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


