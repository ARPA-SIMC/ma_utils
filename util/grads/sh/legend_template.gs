* This script cannot be executed: it contains an example on how to call
* cbar_line.gs scritp to plot the legend of a 2d graph
* co: line colours
* st: line styles
* mk: line marks
* lab: labels (text)

co.1=2; st.1=1; mk.1=0; lab.1=lev1
co.2=4; st.2=1; mk.2=0; lab.2=lev2
co.3=3; st.3=1; mk.3=0; lab.3=lev3
co.4=8; st.4=1; mk.4=0; lab.4=lev4
'cbar_line -x 9 -y 7.5 -c 'co.1' 'co.2' 'co.3' 'co.4' -l 'st.1' 'st.2' 'st.3' 'st.4' -m 'mk.1' 'mk.2' 'mk.3' 'mk.4' -t "'lab.1'" "'lab.2'" "'lab.3'" "'lab.4'"'
