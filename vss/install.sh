installFunc(){
   build=$1

   GPRINSTALL_FLAGS=\
" --prefix=$out"\
" --exec-subdir=$out/bin"\
" --lib-subdir=$out/lib"\
" --project-subdir=$out/share/gpr"\
" --link-lib-subdir=$out/lib"\
" -XVSS_LIBRARY_TYPE=$build"\
" -XXMLADA_BUILD=$build"\
" --build-name=$build"\
" --build-var=LIBRARY_TYPE"\
" --build-var=VSS_LIBRARY_TYPE"\
" --sources-subdir=$out/include/vss"

   gprinstall $GPRINSTALL_FLAGS/gnat -f -p -P gnat/vss_gnat.gpr
   gprinstall $GPRINSTALL_FLAGS/text -f -p -P gnat/vss_text.gpr
   gprinstall $GPRINSTALL_FLAGS/json -f -p -P gnat/vss_json.gpr
   gprinstall $GPRINSTALL_FLAGS/regexp -f -p -P gnat/vss_regexp.gpr
   gprinstall $GPRINSTALL_FLAGS/xml -f -p -P gnat/vss_xml.gpr
   gprinstall $GPRINSTALL_FLAGS/xml_templates -f -p -P gnat/vss_xml_templates.gpr
   gprinstall $GPRINSTALL_FLAGS/xml_xmlada -f -p -P gnat/vss_xml_xmlada.gpr
}

