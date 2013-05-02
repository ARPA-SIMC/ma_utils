Summary: Utilities area Meteorologia Ambientale SIMC
Name: ma_utils
Version: 0.2
Release: 41%{dist}
License: GPL
Group: Applications/Meteo
URL: http://www.arpa.emr.it/sim
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

BuildRequires: netcdf-devel, grib_api-1.9.9
Requires: libsim >= 5.0, libsim < 6.0

Vendor:	       Enrico Minguzzi <eminguzzi@arpa.emr.it>
Packager:      Daniele Branchini <dbranchini@arpa.emr.it>

Obsoletes: gacsv seriet-utils

%description

Utilities e programmi per procedure operative
area MetAmb SIMC

%prep
%setup -q

%build

%configure
make

%install
rm -rf %{buildroot}
%makeinstall

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root,-)
%doc
%dir %{_libexecdir}/%{name}
%{_libexecdir}/%{name}/*.exe
%dir %{_datadir}/%{name}
%{_datadir}/%{name}/*

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
* Thu May 02 2013 Emanuele Di Giacomo <edigiacomo@arpa.emr.it> - 0.1-41
- Rebuild to reflect upstream changes

* Tue Feb 12 2013 Emanuele Di Giacomo <edigiacomo@arpa.emr.it> - 0.1-26
- Changed program dir to pkglibexecdir
- arkimet-postprocess-seriet

* Mon Feb 4 2013 Daniele Branchini <dbranchini@arpa.emr.it> - 0.1-23
- Initial build.
