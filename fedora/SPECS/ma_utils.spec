Summary:    Tools, utilties and libraries for environmental meteorology
Name:       ma_utils
Version:    0.13
Release:    500%{dist}
License:    GPL
Group:      Applications/Meteo
URL:        http://arpae.it/sim
Vendor:	    Enrico Minguzzi <eminguzzi@arpae.it>
Packager:   Daniele Branchini <dbranchini@arpae.it>
Source0:    %{name}-%{version}.tar.gz
Source:     https://github.com/arpa-simc/%{name}/archive/v%{version}-%{release}.tar.gz#/%{name}-%{version}-%{release}.tar.gz  
BuildRoot:  %{_tmppath}/%{name}-%{version}-%{release}-buildroot

%if 0%{?fedora} <= 24
# grib_api is used only on older fedoras
%define grib_sw grib_api
%else
%define grib_sw eccodes
BuildRequires: eccodes-simc
%endif

# expliciting eccodes for centos7
%if 0%{?el7}
%define grib_sw eccodes
BuildRequires: eccodes-simc
%endif

BuildRequires: libtool netcdf-devel %{grib_sw}-devel libdballe-devel libsim-devel %{!?rhel:libsmr} udunits2-devel cnf-devel ksh
Requires: libsim >= 6.0 libsim < 7.0 %{!?rhel:libsmr} ksh

%description

Tools, utilties and libraries for environmental meteorology
in use at ARPAE-SIMC

%prep
%setup -q -n %{name}-%{version}-%{release}
sh autogen.sh

%build

%configure %{?rhel:--enable-smnd-build}
make

%install
rm -rf %{buildroot}
make DESTDIR=%{buildroot} install

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root,-)
%doc
%dir %{_libexecdir}/%{name}
%{_libexecdir}/%{name}/*.exe
%{_libexecdir}/%{name}/*.r
%{_libexecdir}/%{name}/*sh

%if ! 0%{?rhel:1}
%{_libexecdir}/%{name}/wrom.gs
%{_libexecdir}/%{name}/plot_local_orog.gs
%{_libexecdir}/%{name}/crea_anag_stzqa_all.sql
%dir %{_datadir}/%{name}
%{_datadir}/%{name}/*
%endif

%package -n arkimet-postprocess-seriet
Summary: GRIB to seriet postprocessor for arkimet
BuildArch: noarch
Requires: arkimet, ma_utils

%description -n arkimet-postprocess-seriet
GRIB to seriet postprocessor for arkimet

%files -n arkimet-postprocess-seriet
%defattr(-,root,root,-)
%{_libdir}/arkimet/seriet

%changelog
* Wed Sep 12 2018 Daniele Branchini <dbranchini@arpae.it> - 0.13-500
- Github/Travis/Copr rebuild
