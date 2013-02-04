Summary: Utilities area Meteorologia Ambientale SIMC
Name: ma_utils
Version: 0.1
Release: 23%{dist}
License: GPL
Group: Applications/Meteo
URL: http://www.arpa.emr.it/sim
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

BuildRequires: netcdf-devel, grib_api-1.9.9
Requires: libsim = 4.5
Vendor:	       Enrico Minguzzi <eminguzzi@arpa.emr.it>
Packager:      Daniele Branchini <dbranchini@arpa.emr.it>

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
%{_bindir}/*.exe
%dir %{_datadir}/%{name}
%{_datadir}/%{name}/*

%changelog
* Mon Feb 4 2013 Daniele Branchini <dbranchini@arpa.emr.it> - 0.1-23
- Initial build.
