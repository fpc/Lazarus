Name: fpcsrc
Version: LAZVERSION
Release: LAZRELEASE
Copyright: LGPL2
Group: Development/Languages
Source: %{name}-%{version}-%{release}.tgz
Summary: FreePascal sources
Packager: Mattias Gaertner (mattias@freepascal.org)
URL: http://www.freepascal.org/
BuildRoot: %{_tmppath}/fpcsrc-build%{version}

%define fpcsrcdir %{_datadir}/fpcsrc
%define destdir %{buildroot}%{fpcsrcdir}/

# The normal redhat rpm scripts tests every installed file for requirements.
# We install only sources, so we don't need the requirements.
# But it seems, RPM ignores these macros:

# %define _use_internal_dependency_generator 0
%define __find_provides /tmp/do_nothing.sh
%define __find_requires /tmp/do_nothing.sh
# %define __find_requires %{nil}
# AutoReq: 0

# The normal redhat rpm scripts do not recognize properly, what files to strip
# Hook our own strip command
%define __strip /tmp/smart_strip.sh

%description
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked. Basic Delphi support is already implemented (classes,
exceptions, ansistrings, RTTI). This package contains the sources for the
commandline compiler and utils. Provided units are the runtime library (RTL),
free component library (FCL), gtk, ncurses, zlib, mysql, postgres, ibase
bindings and many more.

%prep

%setup -c

%build

# The normal redhat rpm scripts tests every installed file for requirements.
# We install only sources, so we don't need the requirements.
# I don't know, how to tell RPM, to not test for requirements, so simply rename
find . -name '*.pm' -exec mv {} {}.renamed_for_rpm \;
find . -name '*.pl' -exec mv {} {}.renamed_for_rpm \;

%install
if [ %{buildroot} != "/" ]; then
  rm -rf %{buildroot}
fi
mkdir -p %{destdir}
cp -a fpc/* %{destdir}/
# the palmos libcrt.a archive is making trouble
rm -f %{destdir}/rtl/palmos/libcrt.a

%clean
if [ %{buildroot} != "/" ]; then
  rm -rf %{buildroot}
fi

%files
%defattr(-,root,root)
%{fpcsrcdir}

%changelog


