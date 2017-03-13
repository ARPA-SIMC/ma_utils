Summary: Utilities area Meteorologia Ambientale SIMC
Name: ma_utils
Version: 0.13
Release: 362%{dist}
License: GPL
Group: Applications/Meteo
URL: http://www.arpa.emr.it/sim
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

BuildRequires: netcdf-devel grib_api-devel >= 1.10 libdballe-devel libsim-devel %{!?rhel:libsmr} udunits2-devel cnf-devel
Requires: libsim >= 6.0 libsim < 7.0 %{!?rhel:libsmr}

Vendor:	       Enrico Minguzzi <eminguzzi@arpae.it>
Packager:      Daniele Branchini <dbranchini@arpae.it>

Obsoletes: gacsv seriet-utils

%description

Utilities e programmi per procedure operative
area MetAmb SIMC

%prep
%setup -q

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
%{_libexecdir}/%{name}/*sh
%if ! 0%{?rhel:1}
%{_libexecdir}/%{name}/crea_anag_oracle.sql
%{_libexecdir}/%{name}/wrom.gs
%dir %{_datadir}/%{name}
%{_datadir}/%{name}/*
%endif

%package -n arkimet-postprocess-seriet
Summary: GRIB to seriet postprocessor for arkimet
BuildArch: noarch
Requires: arkimet, ma_utils
Obsoletes: arkimet-postprocess-gacsv

%description -n arkimet-postprocess-seriet
GRIB to seriet postprocessor for arkimet

%files -n arkimet-postprocess-seriet
%defattr(-,root,root,-)
%{_libdir}/arkimet/seriet

%changelog
* Wed Apr 9 2014 Daniele Branchini <dbranchini@arpa.emr.it> - 0.2-46
- Nuovi processi generatori cosmo I7 e cosmo I2 cineca

* Fri May 10 2013 Daniele Branchini <dbranchini@arpa.emr.it> - 0.2-46
- Rebuild on dballe 6 and to reflect upstream changes

* Tue May 07 2013 Emanuele Di Giacomo <edigiacomo@arpa.emr.it> - 0.2-43
- Rebuild to reflect upstream changes

* Thu May 02 2013 Emanuele Di Giacomo <edigiacomo@arpa.emr.it> - 0.1-41
- Rebuild to reflect upstream changes

* Tue Feb 12 2013 Emanuele Di Giacomo <edigiacomo@arpa.emr.it> - 0.1-26
- Changed program dir to pkglibexecdir
- arkimet-postprocess-seriet

* Mon Feb 4 2013 Daniele Branchini <dbranchini@arpa.emr.it> - 0.1-23
- Initial build.
