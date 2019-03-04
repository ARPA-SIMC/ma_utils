Summary:    Tools, utilties and libraries for environmental meteorology
Name:       ma_utils
Version:    0.14
Release:    2
License:    GPL
Group:      Applications/Meteo
URL:        http://arpae.it/sim
Vendor:	    Enrico Minguzzi <eminguzzi@arpae.it>
Packager:   Daniele Branchini <dbranchini@arpae.it>
Source:     https://github.com/arpa-simc/%{name}/archive/v%{version}-%{release}.tar.gz#/%{name}-%{version}-%{release}.tar.gz

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

BuildRequires: libtool
BuildRequires: gcc-gfortran
BuildRequires: netcdf-devel
BuildRequires: %{grib_sw}-devel
BuildRequires: libdballe-devel
BuildRequires: libsim-devel
BuildRequires: udunits2-devel
BuildRequires: ksh
BuildRequires: libemos
BuildRequires: jasper-devel
BuildRequires: libemos
BuildRequires: shapelib-devel
BuildRequires: proj-devel
BuildRequires: netcdf-fortran-devel

Requires: libsim >= 6.0, libsim < 7.0
Requires: ksh

%description

Tools, utilties and libraries for environmental meteorology
in use at ARPAE-SIMC

%prep
%setup -q -n %{name}-%{version}-%{release}
sh autogen.sh

%build

%configure
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
%attr(755, root, root) %{_libexecdir}/%{name}/*sh
%{_libexecdir}/%{name}/wrom.gs
%{_libexecdir}/%{name}/plot_local_orog.gs
%{_libexecdir}/%{name}/crea_anag_stzqa_all.sql
%dir %{_datadir}/%{name}
%{_datadir}/%{name}/*

%package -n arkimet-postprocess-seriet
Summary: GRIB to seriet postprocessor for arkimet
BuildArch: noarch
Requires: arkimet
Requires: ma_utils

%description -n arkimet-postprocess-seriet
GRIB to seriet postprocessor for arkimet

%files -n arkimet-postprocess-seriet
%defattr(-,root,root,-)
%{_libdir}/arkimet/seriet

%changelog
* Mon Mar 4 2019 Daniele Branchini <dbranchini@arpae.it> - 0.14-2
- rebuilt on libsim rebuilt on dballe 8

* Tue Jan 22 2019 Daniele Branchini <dbranchini@arpae.it> - 0.14-1
- fixed bugs in crea_progetto_point.ksh
- sh in libexec now executables

* Wed Sep 12 2018 Daniele Branchini <dbranchini@arpae.it> - 0.13-500
- Github/Travis/Copr rebuild
