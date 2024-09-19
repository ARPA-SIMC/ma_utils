Summary:    Tools, utilties and libraries for environmental meteorology
Name:       ma_utils
Version:    0.17
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

%if 0%{?rhel} >= 7
# expliciting eccodes for centos 7 and 8
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
BuildRequires: jasper-devel
BuildRequires: shapelib-devel
BuildRequires: proj-devel
BuildRequires: netcdf-fortran-devel

Requires: libsim >= 6.0
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
%{_libexecdir}/%{name}/g2ctl
%{_libexecdir}/%{name}/*.exe
%{_libexecdir}/%{name}/*.r
%attr(755, root, root) %{_libexecdir}/%{name}/*sh
%{_libexecdir}/%{name}/*.gs
%{_libexecdir}/%{name}/*.gsf
%{_libexecdir}/%{name}/*.pl
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
* Thu Sep 19 2024 Daniele Branchini <dbranchini@arpae.it> - 0.17-2
- Fixed grib_api check

* Thu Sep 19 2024 Daniele Branchini <dbranchini@arpae.it> - 0.17-1
- Added templates for ICON model
- Added program grib2_forc2ana.exe
- Other minor updates
- Removed libemos dependency

* Thu Apr 4 2024 Enrico Minguzzi <eminguzzi@arpae.it> - 0.16-1
- Bug fix in windrose
- Minor developments to math_grib and grads plots
- Avoid using libemos

* Thu May 18 2023 Emanuele Di Giacomo <edigiacomo@arpae.it> - 0.15-4
- Added missing scripts to spec file

* Mon Oct  3 2022 Daniele Branchini <dbranchini@arpae.it> - 0.15-3
- Updated the utilities to plot GRIB files (editions 1 and 2) with GRADS
- Partial removal of old hardcoded path
- Bug fix in configure.ac and grib_daily_stat.f90

* Mon Jan 31 2022 Daniele Branchini <dbranchini@arpae.it> - 0.15-2
- Removed libsim < 7 requirement

* Mon Jan 31 2022 Daniele Branchini <dbranchini@arpae.it> - 0.15-1
- Updated crea_progetto_point.ksh
- estra_grib_cosmo.sh now can also read data from files
- Bug fixes and updates in arkimet extractions
- Updated anagraphic files
- Removed reference from gribex grib_daily_stat.f90 (#8)
- Restored functionality of crea_anag_arkioss.sh, get_staz_var.sh, grt_var_staz.sh, wrom.sh

* Mon Mar 4 2019 Daniele Branchini <dbranchini@arpae.it> - 0.14-2
- rebuilt on libsim rebuilt on dballe 8

* Tue Jan 22 2019 Daniele Branchini <dbranchini@arpae.it> - 0.14-1
- fixed bugs in crea_progetto_point.ksh
- sh in libexec now executables

* Wed Sep 12 2018 Daniele Branchini <dbranchini@arpae.it> - 0.13-500
- Github/Travis/Copr rebuild
