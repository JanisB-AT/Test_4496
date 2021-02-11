***INCLUDE RADMASUTC.
* Several forms for massactivation

*----------------------------------------------------------------------*
* Form MA_VALUES_CHECK
*----------------------------------------------------------------------*
* Checks the input value of function DD_MASS_ACT.
* 1. Logging
*----------------------------------------------------------------------*
* --> DDMODE : Mode of checks
* --> PRID   : Id for log-writer
* <-- RC     : Returncode of input-checks
*----------------------------------------------------------------------*
form ma_values_check using ddmode  like ddmass-ddmode
                           prid    like syst-tabix
                           rc      like syst-subrc.

  clear rc.

  if 'OTACG' na ddmode.
* C is for convertor and G for Generation-Tools (only internally used)
    rc = 8.
*   DO513: Checkmode has to be choosen out of set {'O','T','A'}
    smi0> prid 'E' 'DO513'.
  endif.

endform.                    " MA_VALUES_CHECK

*----------------------------------------------------------------------*
* FORM MA_WRITE_HEADER
*----------------------------------------------------------------------*
* Writes introduction-messages
*----------------------------------------------------------------------*
* --> MEDIUM  : Kind of import-medium of DD-objects:
*               'D': Direct input via Select-options
*               'E': External table
*               'T': Transport-order
* --> INACTIVE: 'X': Inactive activation, ' ': Active activation
* --> DDMODE  : Mode for checks
* --> VERSION : 'M': Activate newest, 'A': Activate active version
* --> EXTTAB  : Name of external table
* --> TRKORR  : Name of transport-order
* --> DELALL  : Delete objects in all versions
* --> DELNOREF: Delete even if references exist
* --> ENQUEUE : Lock of mass-activation
* --> FRCACT  : Handling of dependent objects
* --> TEST_ON : 'X': Testmode without write-operations
* --> TRPROT  : 'X': Write tracelog
* --> PRID    : Id for log-writer
*----------------------------------------------------------------------*
form ma_write_header using medium   like ddmass-medium
                           inactive like ddmass-inactive
                           ddmode   like ddmass-ddmode
                           version  like ddmass-version
                           exttab   like ddmass-exttab
                           trkorr   like ddmass-trkorr
                           delall   like ddmass-delall
                           delnoref like ddmass-delnoref
                           enqueue  like ddmass-enqueue
                           frcact   like ddmass-frcact
                           test_on  like ddmass-test_on
                           trprot   like ddmass-follow
                           prid     like syst-tabix.

  data partab like ddpropval occurs 0 with header line.
  data par(30).

* Set medium
  if medium ca 'T'.
    par = 'Transportauftrag'(002).
    concatenate par trkorr into par separated by ' '.
  endif.
  if medium ca 'E'.
    if par <> ' '.
      concatenate par '/' into par.
    endif.
    concatenate par 'Externe Tabelle'(003) into par.
    concatenate par exttab into par separated by ' '.
  endif.
  if medium ca 'D'.
    if par <> ' '.
      concatenate par '/' into par.
    endif.
    concatenate par 'Direkte Objekteingabe'(004) into par.
  endif.
  add_par> partab 'Eingangsmedium'(100) par.

* Activation-parameters
  if inactive = 'X'.
    add_par> partab 'Aktivierungsmethode'(005) 'inaktiv'(006).
  elseif inactive = ' '.
    add_par> partab 'Aktivierungsmethode'(005) 'aktiv'(007).
  endif.
  par = ddmode.
  add_par> partab 'Prüfmodus'(008) par.
  par = version.
  add_par> partab 'Zu aktivierende Version'(009) par.
* Deletion-parameters
  if delall = 'X'.
    par = 'Alle Versionen werden gelöscht'(011).
  else.
    par = 'Nur aktive Versionen löschen'(012).
  endif.
  add_par> partab 'Zu löschende Versionen'(010) par.
* Enqueue massactivation
  if enqueue = 'S'.
    par = 'Shared'(028).
  elseif enqueue = 'E'.
    par = 'Exclusive'(029).
  endif.
  add_par> partab 'Sperre gegen Parallellauf'(030) par.
* Handling of dependent objects
  if frcact = 'X'.
    par = 'wurde erzwungen'(033).
    add_par> partab 'Abhängigenbehandlung'(032) par.
  endif.
* Testmodus
  if     test_on = 'X'. par = 'angeschaltet'(049).
  elseif test_on = ' '. par = 'ausgeschaltet'(050).
  endif.
  add_par> partab 'Testmodus'(051) par.
* Action-follow-log
  if trprot = 'X'. par = 'wird geschrieben'(052).
    add_par> partab 'Aktionsprotokoll'(053) par.
  endif.

* Write program-header
  prog_header> partab prid 'RADMASG0_C3'.

endform.                    " MA_OPEN_LOG

*----------------------------------------------------------------------*
* FORM MA_TAB_CHECK
*----------------------------------------------------------------------*
* Checks wether tables GENTAB and DELTAB contain objects to handle
*----------------------------------------------------------------------*
* --> GENTAB   : Contains objects to activate
* --> DELTAB   : Contains objects to delete
* <-- GEN_CNT  : Number of objects to activate
* <-- DEL_CNT  : Number of objects to delete
* <-- NTDEL_CNT: Number of Nametabs to delete
* --> PRID     : Id for log-writer
*----------------------------------------------------------------------*
form ma_tab_check tables gentab    type gentb
                         deltab    type deltb
                  using  gen_cnt   like syst-tabix
                         del_cnt   like syst-tabix
                         ntdel_cnt like syst-tabix
                         prid      like syst-tabix.

  data: domain(9) value 'DOMA DOMD',
        types(50) value
              'DTEL DTED TABL TABD SQLT SQLD TTYP TTYD VIEW VIED',
        technset(24)   value 'TABT VIET SQTT INDX XINX',
        f4_objects(35) value 'SHLP SHLD MCOB MCOD MACO MACD MCID',
        enqueue(9)     value 'ENQU ENQD',
        ntab(14)       value 'NTTT NTTB NTDT',
        switches(24)   value  sfw.

  loop at gentab.
    if domain   ns gentab-type and types      ns gentab-type and
       technset ns gentab-type and f4_objects ns gentab-type and
       enqueue  ns gentab-type and sqsc       ns gentab-type and
       stob     ns gentab-type and ntab       ns gentab-type and
       ddls     ns gentab-type and
       switches ns gentab-type and gentab-type <> enhd.
*    Objecttype & unkown -> Further actions will be prevented for object
     smi1> prid 'N' 'DO661' gentab-type.
     delete gentab.
    endif.
    "<KH> 20130130 STOB activation has to be reconstructed before
    "mass-activation can be prevented here
    "<KH> 20130114 STOB only activated via DDLS, for views we cannot
    "find out this from transport object only
*    if gentab-type = 'STOB'.
*      smi1> prid 'N' 'DO661' gentab-type.
*      delete gentab.
*    endif.
  endloop.
  loop at deltab.
    if domain   ns deltab-objtyp and types      ns deltab-objtyp and
       technset ns deltab-objtyp and f4_objects ns deltab-objtyp and
       enqueue  ns deltab-objtyp and sqsc       ns deltab-objtyp and
       stob     ns deltab-objtyp and ntab       ns deltab-objtyp and
       ddls     ns deltab-objtyp and
       switches ns deltab-objtyp and deltab-objtyp <> enhd.
*    Objecttype & unkown -> Further actions will be prevented for object
     smi1> prid 'N' 'DO661' deltab-objtyp.
     delete deltab.
    endif.
    "<KH> 20130130 STOB deletion has to be reconstructed before
    "mass-activation can be prevented here
    "<KH> 20130114 STOB only deleted via DDLS, for views we cannot
    "find out this from transport object only
*    if deltab-objtyp = 'STOB'.
*      smi1> prid 'N' 'DO661' deltab-objtyp.
*      delete deltab.
*    endif.
  endloop.
  describe table gentab lines gen_cnt.
  describe table deltab lines del_cnt.
  clear ntdel_cnt.
  loop at deltab where objtyp = nttb or objtyp = nttt or objtyp = ntdt.
    ntdel_cnt = ntdel_cnt + 1.
  endloop.
  del_cnt = del_cnt - ntdel_cnt.
  if gen_cnt = 0 and del_cnt = 0 and ntdel_cnt = 0.
    smi0> prid 'N' 'DO514'.
  endif.

endform.

*----------------------------------------------------------------------*
* FORM MA_SET_OPERATIONS
*----------------------------------------------------------------------*
* Sets control-structure for activation dependent of DD-type
*----------------------------------------------------------------------*
* --> INACTIVE: 'X': Inactive activation, ' ': Active activation
* --> DDMODE  : Mode for checks
* --> PRID    : Id for log-writer
* <-- ACTB    : Contains control-info for activation of DD-types
*               'N': Execute action if object is activated in newest
*                    version
*               'A': Execute action if object is activated as dependent
*                    object
*----------------------------------------------------------------------*
form ma_set_operations tables actb     type ddactb
                       using  inactive like ddmass-inactive
                              ddmode   like ddmass-ddmode
                              prid     like syst-tabix.

  data: acwa like ddmassac,
        ddmode_tabl like ddmass-ddmode.

  refresh actb.
  ddmode_tabl = ddmode.
  if ddmode = 'C' or ddmode = 'G'.
    ddmode = 'O'.
  endif.
* Domain
  acwa-type = acwa-deptype = doma. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if ddmode = 'O'. acwa-actmode1 = 2. acwa-actmode2 = 6. endif.
  if ddmode = 'T'. acwa-actmode1 = 5. acwa-actmode2 = 6. endif.
  if ddmode = 'A'. acwa-actmode1 = 5. acwa-actmode2 = 6. endif.
  acwa-wr_actflg = 'X'. acwa-maxflg = 'JB'. acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
*  Dependent domain-activation
  acwa-type = acwa-deptype = doma. acwa-actkind = 'A'.
  acwa-act = ddmode.
  if ddmode = 'O'. acwa-actmode1 = 2. acwa-actmode2 = 6. endif.
  if ddmode = 'T'. acwa-actmode1 = 5. acwa-actmode2 = 6. endif.
  if ddmode = 'A'. acwa-actmode1 = 5. acwa-actmode2 = 6. endif.
  acwa-wr_actflgd = 'X'. acwa-maxflg = 'JB'.
  append acwa to actb. clear acwa.
* Data-element
*  Direct data-element-activation
  acwa-type = acwa-deptype = dtel. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if ddmode = 'O'. acwa-actmode1 = 2. endif.
  if ddmode = 'T' or ddmode = 'A'. acwa-actmode1 = 5. endif.
  acwa-wr_actflg = 'X'. acwa-maxflg = 'JB'. acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
*  Dependent data-element-activation
  acwa-type = dtel. acwa-deptype = 'DTDP'. acwa-actkind = 'A'.
  acwa-act_chkn = 'X'. acwa-act_cmp = 'X'.
  acwa-act = ddmode. acwa-actmode1 = 4.
  "Weaker checks (than dependent checks) in case of transport
  if ddmode = 'T' or ddmode = 'A'. acwa-actmode1 = 41. endif.
  acwa-wr_actflgd = 'X'.
  append acwa to actb. clear acwa.
* Table
*  Direct table-activation
  acwa-type = acwa-deptype = tabl. acwa-actkind = 'N'.
  acwa-act = ddmode_tabl.
  if ddmode_tabl = 'O'. acwa-actmode1 = 2. acwa-actmode2  = 3.  endif.
  if ddmode_tabl = 'T'. acwa-actmode1 = 5. acwa-actmode2  = 6.  endif.
  if ddmode_tabl = 'A'. acwa-actmode1 = 5. acwa-actmode2  = 6.  endif.
  if ddmode_tabl = 'C'. acwa-actmode1 = 14. acwa-actmode2 = 14. endif.
  if ddmode_tabl = 'G'. acwa-actmode1 = 17. acwa-actmode2 = 6.  endif.
  acwa-wr_actflg = 'X'. acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
*  Activation of table dependent of domain or data-element or of
*  included table
  acwa-type = tabl. acwa-deptype = 'TBDP'. acwa-actkind = 'A'.
  acwa-act_chkn = 'X'. acwa-act_cmp = 'X'.
  acwa-act = ddmode. acwa-actmode1 = acwa-actmode2 = 4.
  acwa-actmode3 = 10.
  "Weaker checks (than dependent checks) in case of transport
  if ddmode = 'T' or ddmode = 'A'.
    acwa-actmode1 = acwa-actmode2 = 41.
    acwa-actmode3 = 11.
  endif.
  acwa-wr_actflgd = 'X'.
  append acwa to actb. clear acwa.
* Derived type (structure with Streams and Locators)
  acwa-type = 'DEST'. acwa-deptype = 'TBDP'. acwa-actkind = 'A'.
  acwa-act_chkn = 'X'. acwa-act_cmp = 'X'.
  acwa-act = ddmode. acwa-actmode1 = acwa-actmode2 = 4.
  acwa-actmode3 = 10.
  append acwa to actb. clear acwa.
* Tablepool/Tablecluster
  acwa-type = acwa-deptype = sqlt. acwa-actkind = 'N'.
  acwa-act = ddmode.
  acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
* Technical Settings
*  Technical Settings for transparent table or logical Pool/Cluster
  acwa-type = acwa-deptype = tabt. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if ddmode = 'O'. acwa-actmode1 = 2. endif.
  if ddmode = 'T' or ddmode = 'A'. acwa-actmode1 = 3. endif.
  acwa-wr_tr_ext = 'X'.
  "<KH> 20130128 "Passing timestamp in case of buffering change
  acwa-wr_actflgd = 'X'.
  append acwa to actb. clear acwa.
*  Technical Settings for tablepool or tablecluster
  acwa-type = acwa-deptype = sqtt. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if ddmode = 'O'. acwa-actmode1 = 2. endif.
  if ddmode = 'T' or ddmode = 'A'. acwa-actmode1 = 3. endif.
  acwa-wr_tr_ext  = 'X'.
  append acwa to actb. clear acwa.
*  Technical Settings for views
  acwa-type = acwa-deptype = viet. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if ddmode = 'O'. acwa-actmode1 = 2. endif.
  if ddmode = 'T' or ddmode = 'A'. acwa-actmode1 = 3. endif.
  acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
* Index
  acwa-type = acwa-deptype = indx. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if inactive     = ' '. acwa-actmode1 = 1.  "Checks are identical
  elseif inactive = 'X'. acwa-actmode1 = 2.  "in both modes
  endif.
  acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
* Customer Index
  acwa-type = acwa-deptype = xinx. acwa-actkind = 'N'.
  acwa-act_chkm = 'X'.
  acwa-act = ddmode.
  if inactive     = ' '. acwa-actmode1 = 1.  "Checks are identical
  elseif inactive = 'X'. acwa-actmode1 = 2.  "in both modes
  endif.
  acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
* View
*  Direct View-activation
  acwa-type = acwa-deptype = view. acwa-actkind = 'N'.
  acwa-act = ddmode.
  "In mode 0 special fields are set later (set_checks_fd)
  if ddmode = 'O'. acwa-actmode1 = 1.         "Checks are identical
  else.            acwa-actmode1 = 0.         "in both cases
  endif.
  acwa-act_chkm  = 'X'.
  acwa-wr_actflg = 'X'.  acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
*  Dependent view-activation
  acwa-type = view. acwa-deptype = 'VITB'. acwa-actkind = 'A'.
  acwa-act_chkn = 'X'. acwa-act_cmp = 'X'.
  acwa-act_chkm = 'X'.
  acwa-act = ddmode_tabl.
  "In mode 0 special fields are set later (set_checks_fd)
  if     ddmode_tabl = 'O'. acwa-actmode1 = 1.    "Checks are identical
  elseif ddmode_tabl = 'C'. acwa-actmode1 = 4.
  else.                     acwa-actmode1 = 0.    "in both cases
  endif.
  acwa-wr_actflgd = 'X'.
  append acwa to actb. clear acwa.
* Enqueue
* Direct enqueue-activation
  acwa-type = acwa-deptype = enqu. acwa-actkind = 'N'.
  acwa-act = ddmode.
  acwa-actmode1 = 0.
  if inactive = 'X'.
    acwa-wr_tbatg = 'X'.
  endif.
  acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
* Dependent enqueue-activation
  acwa-type = enqu. acwa-deptype = 'TBEN'. acwa-actkind = 'A'.
  acwa-act_chkn = 'X'. acwa-act_cmp = 'X'.
  acwa-act = ddmode.
  acwa-actmode1 = 0.
  if inactive = 'X'.
    acwa-wr_tbatg = 'X'.
  endif.
  append acwa to actb. clear acwa.
* Matchcode-Object
  acwa-type = acwa-deptype = mcob. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if inactive     = ' '.                      "Checks are identical
    acwa-actmode1 = ' '.                      "in both modes
  elseif inactive = 'X'.
    acwa-actmode1 = 'AU'. acwa-wr_tbatg = 'X'.
  endif.
  acwa-wr_actflg = 'X'. acwa-wr_tr_ext = 'X'.
  append acwa to actb.
  acwa-type = acwa-deptype = maco.
  append acwa to actb. clear acwa.
* Matchcode-Id
*  Direct Matchcode-Id activation
  acwa-type = acwa-deptype = mcid. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if inactive     = ' '.                       "Checks are identical
    acwa-actmode1 = ' '.                       "in both modes
  elseif inactive = 'X'.
    acwa-actmode1 = 'AT'. acwa-wr_tbatg = 'X'. acwa-wr_tr_ext = 'X'.
  endif.
  append acwa to actb. clear acwa.
*  Matchcode-Id as dependent object
  acwa-type = 'MCID'. acwa-deptype = 'MCMC'. acwa-actkind = 'A'.
  acwa-act = ddmode.
  if inactive     = ' '.                    "Checks are identical
    acwa-actmode1 = ' '.                    "in both modes
  elseif inactive = 'X'.
    acwa-actmode1 = 'AT'. acwa-wr_tbatg = 'X'.
  endif.
  acwa-act_chkn = 'X'. acwa-act_cmp = 'X'.
  append acwa to actb. clear acwa.
* Searchhelp
*  Direct Searchhelp-activation
  acwa-type = acwa-deptype = shlp. acwa-actkind = 'N'.
  acwa-act = ddmode.
  acwa-actmode1 = 0.
  acwa-wr_actflg = 'X'. acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
*  Dependent Searchhelp-activation
  acwa-type = shlp. acwa-deptype = 'SHTB'. acwa-actkind = 'A'.
  acwa-act_chkn = 'X'. acwa-act_cmp = 'X'.
  acwa-act = ddmode.
  acwa-actmode1 = 0.
  acwa-wr_actflgd = 'X'.
  append acwa to actb. clear acwa.
* Tabletype
*  Direct activation of tabletype
  acwa-type = acwa-deptype = ttyp. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if ddmode = 'O'. acwa-actmode1 = 2. endif.
  if ddmode_tabl = 'G'. acwa-actmode1 = 3. endif.
  if ddmode = 'T' or ddmode = 'A'. acwa-actmode1 = 5. endif.
  acwa-wr_actflg = 'X'. acwa-maxflg = 'JB'. acwa-wr_tr_ext = 'X'.
  append acwa to actb. clear acwa.
*  Activation of dependent tabletype
  acwa-type = ttyp. acwa-deptype = 'TTDP'. acwa-actkind = 'A'.
  acwa-act_chkn = 'X'. acwa-act_cmp = 'X'.
  acwa-act = ddmode. acwa-wr_actflgd = 'X'.
  acwa-actmode1 = 4.
  append acwa to actb. clear acwa.
* DB Procedure Proxy
  acwa-type    = sqsc.
  acwa-actkind = 'N'.
  acwa-act = ddmode.
  acwa-wr_tr_ext = 'X'.
  if inactive = 'X'.
    acwa-wr_tbatg = 'X'. "Thus we control the different handling
  endif.                 "in inactive case
  append acwa to actb. clear acwa.
* Structured object
*  Direct Structured Object activation -> Only one activation mode
  acwa-type = acwa-deptype = stob. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if ddmode = 'O'. acwa-actmode1 = 1.
  else.            acwa-actmode1 = 2.
  endif.
  acwa-act_chkm  = 'X'.
  acwa-wr_actflg = 'X'.
  acwa-wr_tr_ext = 'X'.
  append acwa to actb.
  "Dependent Structured Object
  acwa-actkind  = 'A'.
  acwa-act_chkn = 'X'. acwa-act_cmp = 'X'.
  acwa-act_chkm  = 'X'.
  clear acwa-wr_actflg.
  append acwa to actb. clear acwa.
* Ddl Source
  acwa-type = acwa-deptype = ddls. acwa-actkind = 'N'.
  acwa-act = ddmode.
  if ddmode = 'O'. acwa-actmode1 = 1.
  else.            acwa-actmode1 = 2.
  endif.
  acwa-wr_actflg = 'X'.
  acwa-wr_tr_ext = 'X'.
  append acwa to actb.
  "Dependent Ddl Source (from Ddl Extend)
  acwa-type = acwa-deptype = ddls. acwa-actkind = 'A'.
  acwa-act = ddmode.
  acwa-act_chkn = 'X'.
  if ddmode = 'O'. acwa-actmode1 = 1.
  else.            acwa-actmode1 = 2.
  endif.
  if ddmode_tabl = 'C'.   "Dependent of table conversion
    acwa-actmode1 = 3.
  endif.
  acwa-act_cmp    = 'X'.
  acwa-wr_actflgd = 'X'.
  append acwa to actb.

  sort actb by type actkind.

endform.

*----------------------------------------------------------------------*
* FORM MA_LEV_ACTIVATE
*----------------------------------------------------------------------*
* Activates all objects of one sort-level
*----------------------------------------------------------------------*
* <-> GENTAB   : Table with objects to activate
* --> DEPTAB   : Table with dependencies between DD-objects
* --> INVDEPTAB: Inverse table with dependencies between DD-objects
* <-- OKTAB    : Contains successfully activated objects
* <-- NTCHGTAB : Contains tables for which Nametab-timestamps have to
*                be set or Nametab has to be generated
* --> ACTB     : Contains control-information for every DD-type
* <-- CNVTAB   : Contains tables which have to be converted
* --> LEVEL    : Current level
* --> INACTIVE : 'X': Inactive Nametab, ' ': Active Nametab
* --> DEPACT   : Force activation of dependent objects
* --> AUTH_CHK : 'X': Authority-check, ' ': No execution of Auth.-check
* --> T_ON     : 'X': Timer on, ' ': Timer remains switched off
* --> COUNTER  : Counts the number of objects  activated per level
* --> ACT_S    : 'X': Activation of single table/structure with
*                dependent objects, ' ': Mass-activation
* --> EXCOMMIT : 'X': Commit work after every single object-activation,
*                ' ': No commit
* --> RC_IN    : Only objects with RC > RC_IN are activated
* --> TST      : TEST_ON: 'X': Testmode is switched on -> No activation
*                FOLLOW : 'X': Tracelog is written
* --> PRID     : Id for log-writer, physical existing channel for
*                reference-log
* --> PRID1    : Id for log-writer, actual internal channel for
*                activation-log
* --> PRIDF    : Id for log-writer, tracelog
*----------------------------------------------------------------------*
*form ma_lev_activate tables gentab    type gentb
*                            deptab    type deptb
*                            invdeptab type deptb
*                            actb      type ddactb
*                            ntchgtab  type dcntchgtb
*                            oktab     type oktb
*                            cnvtab    structure dctablres
*                     using  level     like dcgentb-level
*                            inactive  like ddmass-inactive
*                            auth_chk  like ddmass-authchk
*                            depact    like ddmass-frcact
*                            t_on      like ddmass-timer
*                            counter   like syst-tabix
*                            act_s     like ddmass-actsingle
*                            excommit  like ddmass-excommit
*                            rc_in     like dcgentb-rc
*                            tst       like ddmasstst
*                            prid      like syst-tabix
*                            prid1     like syst-tabix
*                            pridf     like syst-tabix.
*
*  data: genwa   like dcgentb,
*        acwa    like ddmassac,
*        line_nr like syst-tabix,
*        tbvi    type tbvitb, tbsh type tbshtb, tben type tbentb,
*                mcomcid type mcomcidtb, vitb type vitbtb,
*        action  like dcdeptb-action,
*        act_before like ddmassac-act,
*        wr_actflg_before like ddmassac-wr_actflg,
*        done    like ddrefstruc-bool.
*
*  clear counter. refresh oktab.
*  perform stdo_set_log(radbtout) using 6.
*  if tst-test_on = 'X'. smi1> prid 'I' 'DO624' level. endif.
*  loop at gentab into genwa where level =  level
*                            and   rc    >= rc_in.
*    line_nr = syst-tabix. clear acwa.
**---Get control-information about current type-------------------------*
*    read table actb into acwa with key type    = genwa-type
*                                       actkind = genwa-actkind
*                                       binary search.
**---Compute necessary operations on DD-object during activation--------*
*    if acwa-act_cmp = 'X'.
*      perform mt_act_cmp tables invdeptab using genwa action.
*      perform mt_act_chk tables ntchgtab
*                         using  genwa acwa depact action.
*      follow> tst-follow f genwa-type genwa-name 'Aktion:'(132)
*              action.
*      if action = ' '.
*        follow> tst-follow f genwa-type genwa-name
*          'Keine Aktion für abh. Objekt gefunden:'(130)
*          'Aktivierung nicht notwendig'(123).
*      endif.
*    endif.
**---If A-version: Check if N-version has already been activated--------*
*    if acwa-act_chkn = 'X'.
*      act_before = acwa-act.
*      perform mt_chkn tables gentab using genwa acwa level line_nr.
*      if act_before <> acwa-act.
*        follow> tst-follow f genwa-type genwa-name
*          'N-Version existiert'(122) 'Aktivierung nicht notwendig'(123).
*      endif.
*    endif.
**---Activate newest-version: R3TR object has already been activated ?--*
*    if acwa-act_chkm = 'X'.
*      act_before = acwa-act.
*      perform mt_chkm tables gentab using genwa acwa level line_nr.
*      if act_before <> acwa-act.
*        follow> tst-follow f genwa-type genwa-name
*        'R3TR-Objekt existiert'(133) 'Aktivierung nicht notwendig'(123).
*      endif.
*    endif.
**---Activate DD-object-or handle test-mode-----------------------------*
*    if acwa-act <> ' '.
*      wr_actflg_before = acwa-wr_actflg.
*      timer_on> t_on t-objact.
*      if tst-test_on = ' '.
*        perform mt_act tables gentab tbvi tbsh tben mcomcid vitb cnvtab deptab
*                       using  genwa acwa inactive auth_chk action t_on
*                              depact line_nr act_s excommit done
*                              prid prid1.
*        follow> tst-follow f genwa-type genwa-name
*                'Aktivierung'(047) ''.
*      elseif tst-test_on = 'X'.
*        perform mt_test_mode_handle tables gentab
*                                    using  genwa line_nr prid1.
*      endif.
*      timer_off> t_on t-objact prid1.
*      counter = counter + 1.
*      if wr_actflg_before <> acwa-wr_actflg.
*        follow> tst-follow f genwa-type genwa-name
*          'Keine Aktion zum Weitergeben vorhanden'(131) ''.
*      endif.
*    endif.
**---Sort tables delivered by single activators-------------------------*
*    sort tbvi    by tabname viewname.
*    sort tbsh    by name namedep.
*    sort tben    by name namedep.
*    sort mcomcid by mconame mcid.
*    sort vitb    by name namedep.
**---Store necessary actions to dependent objects-----------------------*
*    if acwa-wr_actflg <> ' '.
*      perform mt_wr_actflag
*        tables gentab deptab invdeptab tbvi tbsh tben mcomcid vitb
*        using  genwa line_nr depact tst-test_on tst-follow prid1 pridf.
*    endif.
**---Store necessary actions of dependent objects to dependent objects--*
*    if acwa-wr_actflgd <> ' '.
*      perform mt_wr_actflag_dep
*        tables gentab deptab invdeptab tbvi tbsh tben mcomcid vitb
*        using  genwa line_nr depact action tst-test_on tst-follow
*               prid1 pridf.
*    endif.
**---Collect objects completely activated-------------------------------*
*    if acwa-wr_tr_ext <> ' '.
*      perform mt_obj_collect tables oktab using genwa.
*      follow> tst-follow f genwa-type genwa-name
*             'vollständig ok'(127) ''.
*    endif.
*  endloop.
*
*endform.

*----------------------------------------------------------------------*
* FORM MT_CHKN
*----------------------------------------------------------------------*
* Look wether table has already been activated in newest version
*----------------------------------------------------------------------*
* --> GENTAB     : Table with objects to activate
* --> GENWA      : The currect object out of GENTAB
* --> ACWA       : Control-structure for activation
* --> LEVEL      : Current level
* --> ACTUAL_LINE: Line in GENTAB for actual object
*----------------------------------------------------------------------*
form mt_chkn tables gentab      type gentb
             using  genwa       like dcgentb
                    acwa        like ddmassac
                    level       like dcgentb-level
                    actual_line like syst-tabix.

  data: genwa_n like dcgentb.

* GENTAB sorted that concerning ACTKIND we have 'N' < 'A'
  read table gentab into genwa_n with key level = level
                                          type  = genwa-type
                                          name  = genwa-name
                                          indx  = genwa-indx
                                          binary search
                                 transporting rc actkind.
  if syst-subrc = 0.
    if genwa_n-rc <= 4 and genwa_n-actkind = 'N'.
      clear acwa-act.
      genwa-rc = genwa_n-rc.
      modify gentab from genwa index actual_line.
    endif.
  elseif genwa-type = 'TABL'.
    read table gentab transporting no fields with key type  = 'VIEW'
                                                      name  = genwa-name.
    if syst-subrc = 0.   "View will be activated
      data(pos) = syst-tabix.
      clear acwa-act.
      genwa-rc = 0.  "Result will come from View later
      modify gentab from genwa index pos.
    endif.
  endif.

endform.                    " MT_CHKN

*----------------------------------------------------------------------*
* FORM MT_CHKM
*----------------------------------------------------------------------*
* Check if for LIMU TABT, LIMU SQTT or LIMU INDX corresponding R3TR
* objects R3TR TABL or R3TR SQLT have already been activated
*----------------------------------------------------------------------*
* --> GENTAB     : Table with objects to activate
* --> GENWA      : The currect object out of GENTAB
* --> ACWA       : Control-structure for activation
* --> LEVEL      : Current level
* --> ACTUAL_LINE: Line in GENTAB for actual object
*----------------------------------------------------------------------*
form mt_chkm tables gentab  type gentb
             using  genwa   like dcgentb
                    acwa    like ddmassac
                    level   like dcgentb-level
                    line_nr like syst-tabix.

  data: l           like dcgentb-level,
        genwa_m     like dcgentb,
        master_type like dcddobj-type.

  check ( genwa-type = tabt or genwa-type = sqtt or genwa-type = viet or
          genwa-type = indx ).

  if genwa-type = tabt or genwa-type = indx. master_type = tabl. endif.
  if genwa-type = sqtt. master_type = sqlt. endif.
  if genwa-type = viet. master_type = view. endif.
*-Look if newest version of master has already been activated----------*
* GENTAB is sorted by LEVEL TYPE NAME ...
  while l < level.
    read table gentab into genwa_m
                      with key level = l
                               type  = master_type
                               name  = genwa-name binary search.
    if syst-subrc = 0.
      if genwa_m-rc <= 4 and genwa_m-actkind = 'N'.
        clear acwa-act.
        genwa-rc = genwa_m-rc.
        modify gentab from genwa index line_nr.
      endif.
      l = level.
    else.
      l = l + 1.
    endif.
  endwhile.

endform.                    " MT_CHKM

*----------------------------------------------------------------------*
* FORM MT_ACT_CMP
*----------------------------------------------------------------------*
* Computes necessary actions during activation
*----------------------------------------------------------------------*
* --> DEPTAB: Contains dependencies between DD-objects
* --> GENWA : Contains current object to activate
*----------------------------------------------------------------------*
*form mt_act_cmp tables invdeptab type invdeptb
*                using  genwa     like dcgentb
*                       action    like dcdeptb-action.
*
*  data: invdepwa like dcinvdeptb,
*        begin    like syst-tabix.
*
*  clear action.
*
*  read table invdeptab into invdepwa
*                       with key deptype = genwa-type
*                                depname = genwa-name
*                                depindx = genwa-indx binary search.
*  if syst-subrc = 0.
*    begin = syst-tabix.
*  else.
*    exit.     "Leave form
*  endif.
*
*  loop at invdeptab into invdepwa from begin.
*    if invdepwa-deptype = genwa-type and invdepwa-depname = genwa-name
*       and invdepwa-depindx = genwa-indx.
*      if invdepwa-depkind = 'A' or invdepwa-depkind = 'I'.
**-------Compute necessary operation------------------------------------*
*        if action = ' '. action = invdepwa-action. endif.
*        perform ut_set_action using action invdepwa-action.
*      endif.
*    else.
*      exit.
*    endif.
*  endloop.
*
*endform.                    " MT_ACT_CMP

*----------------------------------------------------------------------*
* FORM GET_MESSAGE
*----------------------------------------------------------------------*
* Gets last message send for object
*----------------------------------------------------------------------*
* <-- ARBGB      : Message-class
* <-- MESSNR     : Message number
* --> SEVERITY   : Severity of error to look for
* <-- PAR1...PAR4: Parameters for messages
*----------------------------------------------------------------------*
form get_message using arbgb messnr severity par1 par2 par3 par4.

  data: message like ddmess.

  clear: arbgb, messnr, par1, par2, par3, par4.
  perform stdo_get_message(radbtout) using severity arbgb messnr
                                          message.
  arbgb  = message-arbgb.
  messnr = message-msgnr.
  par1   = message-var1.
  par2   = message-var2.
  par3   = message-var3.
  par4   = message-var4.

endform.                    " GET_MESSAGE

*----------------------------------------------------------------------*
* FORM MT_ACT
*----------------------------------------------------------------------*
* Activates DD-Objects without commit
*----------------------------------------------------------------------*
* --> GENTAB  : Table with all DD-objects to activate
* --> DEPTAB  : Dependent objects of GENTAB objects of this level
*               for computing if activation of dependent objects is necessary
*               (There are objects as Ddls for which we cannot compute
*               changes and which therefore always have to be activated)
* <-- TBVI    : Contains all dependent views of the table to activate
* <-- TBSH    : Contains all dependent Searchhelps of the table to
*               activate
* <-- TBEN    : Contains all dependent enqueue-objects of the table to
*               activate
* <-- MCOMCID : Contains all dependent MC-Ids for a Matchcode-object
* <-- VITB    : Contains all dependent tables of the views to activate
* <-- CNVTAB  : Contains names of tables which have to be converted
* --> GENWA   : Entry of GENTAB with current object
* --> ACWA    : Control-parameters for activation
* --> INACTIVE: 'X': Inactive Nametab, ' ': Active Nametab
* --> AUTH_CHK: 'X': Execute authority-check, ' ': Execute without
*               authority-check
* --> ACTION  : Contains actions to be executed for a ceratin DD-object
* --> T_ON    : 'X': Switch timer on, ' ': No timer used
* --> DEPACT  : 'X': Force activation
* --> LINE_NR : Line of actual object in GENTAB
* --> ACT_S   : 'X': Activation of single table/structure with
*               dependent objects, ' ': Mass-activation
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* <-- DONE    : 'X': DROP/CREATE for DDFTX already executed, ' '
*               otherwise
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical channel)
* --> PRID1   : Id for log-writer, internal existing channel for
*               actual activation-log
*----------------------------------------------------------------------*
form mt_act tables gentab   type gentb
                   tbvi     type tbvitb
                   tbsh     type tbshtb
                   tben     type tbentb
                   mcomcid  type mcomcidtb
                   vitb     type vitbtb
                   cnvtab   structure dctablres
                   deptab   type deptb
            using  genwa    like dcgentb
                   acwa     like ddmassac
                   inactive like ddmass-inactive
                   auth_chk like ddmass-authchk
                   action   like dcdeptb-action
                   t_on     like ddmass-timer
                   depact   like ddmass-frcact
                   line_nr  like syst-tabix
                   act_s    like ddmass-actsingle
                   excommit like ddmass-excommit
                   done     like ddrefstruc-bool
                   prid     like syst-tabix
                   prid1    like syst-tabix.

* refresh tbvi.    clear tbvi.
  refresh tbsh.    clear tbsh.
  refresh mcomcid. clear mcomcid.
  if acwa-type = doma.
    perform mt_doma_act using genwa acwa t_on depact excommit
                              prid prid1.
  elseif acwa-type = dtel.
    perform mt_dtel_act using genwa acwa inactive action t_on depact
                              excommit done prid prid1.
  elseif acwa-type = tabl.
    perform mt_tabl_act tables tbvi tbsh tben cnvtab
                        using  genwa acwa inactive auth_chk action
                               depact t_on act_s excommit done
                               prid prid1.
  elseif acwa-type = tabt or acwa-type = sqtt or acwa-type = viet.
    perform mt_tabt_sqtt_act using genwa acwa inactive auth_chk
                                   t_on excommit prid prid1.
  elseif ( acwa-type = indx or acwa-type = xinx ).
    perform mt_indx_act using genwa acwa inactive auth_chk t_on
                              excommit prid prid1.
  elseif acwa-type = sqlt.
    perform mt_sqlt_act using genwa acwa inactive auth_chk action
                              t_on excommit prid prid1.
  elseif acwa-type = view.
    perform mt_view_act tables vitb
                        using  genwa acwa inactive auth_chk depact t_on
                               act_s excommit prid prid1.
  elseif acwa-type = enqu.
    perform mt_enqu_act using genwa acwa inactive t_on excommit
                              prid prid1.
  elseif acwa-type = mcob or acwa-type = maco.
    perform mt_mcob_act tables mcomcid
                        using genwa acwa t_on excommit prid prid1.
  elseif acwa-type = mcid.
    perform mt_mcid_act tables mcomcid
                        using genwa acwa t_on excommit prid prid1.
  elseif acwa-type = shlp.
    perform mt_shlp_act using genwa acwa t_on excommit prid prid1.
  elseif acwa-type = ttyp.
    perform mt_ttyp_act using genwa acwa inactive action t_on excommit
                              prid prid1.
  elseif acwa-type = sqsc.
    perform mt_sqsc_act using genwa acwa inactive t_on excommit
                              prid prid1.
  elseif acwa-type = stob.
    perform mt_stob_act using genwa acwa inactive t_on act_s excommit depact
                              prid prid1
                        changing cnvtab[].
  elseif acwa-type = ddls.
    perform mt_ddls_act tables   deptab
                        using    acwa inactive t_on act_s excommit prid prid1
                        changing genwa.
  endif.
  genwa-actexecuted = genwa-rc.
  modify gentab from genwa index line_nr.

endform.                    " MT_ACT

*----------------------------------------------------------------------*
* FORM MT_DOMA_ACT
*----------------------------------------------------------------------*
* Activates domain
*----------------------------------------------------------------------*
* --> GENWA   : Contains current domain to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> DEPACT  : 'X': Force activation
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_doma_act using genwa    like dcgentb
                       acwa     like ddmassac
                       t_on     like ddmass-timer
                       depact   like ddmass-frcact
                       excommit like ddmass-excommit
                       prid     like syst-tabix
                       prid1    like syst-tabix.

  data: doma      like dd01l-domname occurs 1,
        act_mode  like ddrefstruc-mode,
        doma_ctrl like dcdomaactp,
        act_res   like ddactres occurs 1 with header line.

*-Write reference-log--------------------------------------------------*
*  log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.
*-Set activation-parameters--------------------------------------------*
  append genwa-name to doma.
  act_mode = acwa-actmode1.
  if genwa-local = 'A'. doma_ctrl-get_state = 'A'. endif.
  clear doma_ctrl-do_commit.
  newob> prid1.
*-Activate domain------------------------------------------------------*
  call function 'DD_DOMA_ACTM'
       exporting
            act_mode         = act_mode
            act_ctrl_par     = doma_ctrl
            prid             = prid1
            off              = 0
            timer_on         = t_on
       tables
            doma_names       = doma
            do_act_results   = act_res
       exceptions
            illegal_value    = 1
            act_refused      = 2
            read_failure     = 3
            put_failure      = 4
            internal_failure = 5
            others           = 6.
  if syst-subrc <> 0.
    genwa-rc = 8.
  else.
    genwa-rc = 8.
    read table act_res index 1.
    if syst-subrc = 0.
      genwa-rc = act_res-act_result. genwa-actflag = act_res-depaction.
      if genwa-rc > 8. genwa-rc = 8. endif.
*-----In case of upgrade-mode set maximal ACTFLAG possible--------------*
      if ( acwa-act = 'A' or depact = 'X' ) and act_res-depaction = ' '.
        genwa-actflag = acwa-maxflg.
      endif.
      "<KH> 20040323
*-----For dependent domains (those with appends) set ACTFLAG/RESERVEDOM--*
      "Change of dependent domain is given to dependent objects. As domain parent
      "is always activated we can reduce action if dynpro timestamp is the
      "action for parent
      if genwa-rc <= 4 and genwa-local = 'A'.
        if genwa-action = 'JB' and genwa-actflag = 'D'.
          genwa-action = 'D'.
        endif.
        perform set_actflag using genwa ' ' prid.
      endif.
    endif.
  endif.
  if genwa-rc > 4 or
             ( genwa-rc <= 4 and genwa-actflag = ' ' and depact = ' ' ).
    clear acwa-wr_actflg.
    clear acwa-wr_actflgd.   "<KH> 20040323 Dependent objects exist
                   "because of domain appends
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

  if excommit = 'X'. commit work. endif.

endform.                    " MT_DOMA_ACT

*----------------------------------------------------------------------*
* FORM MT_DTEL_ACT
*----------------------------------------------------------------------*
* Activates data-element
*----------------------------------------------------------------------*
* --> GENWA   : Current data-element to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> DEPACT  : Force dependent activation
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* <--DONE     : 'X', if DDFTX has already been dropped and created,
*               ' ' otherwise
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_dtel_act using genwa    like dcgentb
                       acwa     like ddmassac
                       inactive like ddmass-inactive
                       action   like dcdeptb-action
                       t_on     like ddmass-timer
                       depact   like ddmass-frcact
                       excommit like ddmass-excommit
                       done     like ddrefstruc-bool
                       prid     like syst-tabix
                       prid1    like syst-tabix.

  data: act_mode like ddrefstruc-mode,
        rollname like dd04l-rollname occurs 1,
        act_res  like dddtstate occurs 1 with header line,
        mess(5),
        dtelact  like dcdtelactp,
        dtelctrl like dcdtelctrl.

*-Handle entry * for DTEL-name-----------------------------------------*
* perform mt_drop_create_ddftx using genwa acwa done prid.
  perform mt_delete_lang using genwa acwa prid.

  check genwa-name <> '*'.

* <KH> 20080206 If object will be deleted and could not be deletetd during
* first effort because of still existing references do not activate
* Second effort will delete these objects
  check genwa-delflag <> 'X'.

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.
*-Activate data-element------------------------------------------------*
  act_mode = acwa-actmode1.
  append genwa-name to rollname.
* Set parameter
  if genwa-local = 'A'. dtelact-get_state = 'A'. endif.
  if inactive = 'X'. dtelact-ntabstate = 'N'. endif.
  if action ca 'DB'. dtelact-settmst = 'D'. endif.
  if action ca 'AB'.
    concatenate dtelact-settmst 'A' into dtelact-settmst.
  endif.
  if genwa-cycleobj     = 'X'. dtelctrl-cycleobj = 'T'.
  elseif genwa-cycleobj = ' '. dtelctrl-cycleobj = 'F'.
  endif.
  if genwa-is_used     = 'X'. dtelctrl-is_used = 'T'.
  elseif genwa-is_used = ' '. dtelctrl-is_used = 'F'.
  endif.

  newob> prid1.
* Set Dynpro-Timestamp for data-element in case of language-import
  if genwa-pgmid = lang.
*   Set Dynpro-Timestamp for data-element in case of language-import
    dtelact-force_act = 'D'.
*   Special start-message in case of language-import
    dtelact-send_start = 'F'.   "No start-message via DTEL-Activator
    obj_header> prid1 'DT131' genwa-name '' '' ''.
  endif.

  call function 'DD_DTEL_ACTM'
       exporting
            act_mode         = act_mode
            act_ctrl_par     = dtelact
            prid             = prid1
            timer_on         = t_on
            off              = 0
            ext_chk_par      = dtelctrl
       tables
            rollnames        = rollname
            act_results      = act_res
       exceptions
            illegal_value    = 1
            read_failure     = 2
            put_failure      = 3
            internal_failure = 4
            others           = 5.
  if syst-subrc <> 0.
    genwa-rc = 8.
  else.
    genwa-rc = 8.
    read table act_res index 1.
    if syst-subrc = 0.
      genwa-rc = act_res-act_result. genwa-actflag = act_res-depaction.
      if genwa-rc > 8. genwa-rc = 8. endif.
*-----In case of upgrade-mode set maximal ACTFLAG possible-------------*
      if ( acwa-act = 'A' or depact = 'X' ) and act_res-depaction = ' '.
        genwa-actflag = acwa-maxflg.
      endif.
    endif.
  endif.
  if genwa-rc <= 4 and acwa-act = 'O'.
    if genwa-rc > 4. mess = 'DO303'.
    else.            mess = 'DO337'.
    endif.
    obj_tail> prid1 mess genwa-name.
  endif.
  if genwa-rc > 4 or
             ( genwa-rc <= 4 and genwa-actflag = ' ' and depact = ' ' ).
    clear acwa-wr_actflg.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

  if excommit = 'X'. commit work. endif.

endform.

*----------------------------------------------------------------------*
* FORM MT_TABL_ACT
*----------------------------------------------------------------------*
* Activates table
*----------------------------------------------------------------------*
* <-- TBVI    : Contains dependent views
* <-- TBSH    : Contains dependent Searchhelps
* <-- TBEN    : Contains dependent enqueue-objects
* <-- CNVTAB  : Contains names of tables which have to be converted
* --> GENWA   : Current table to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> INACTIVE: 'X': Inactive activation, ' ': Active activation
* --> AUTH_CHK: 'X': Authority-check, ' ': no Authority-check
* --> ACTION  : Necessary actions for table
* --> DEPACT  : 'X', if dependent objects have to be activated
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> ACT_S   : 'X': Activation of single table/structure with
*               dependent objects, ' ': Mass-activation
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* <--DONE     : 'X', if DDFTX has already been dropped and created,
*               ' ' otherwise
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_tabl_act tables tbvi     type tbvitb
                        tbsh     type tbshtb
                        tben     type tbentb
                        cnvtab   structure dctablres
                 using  genwa    like dcgentb
                        acwa     like ddmassac
                        inactive like ddmass-inactive
                        auth_chk like ddmass-authchk
                        action   like dcdeptb-action
                        depact   like ddmass-frcact
                        t_on     like ddmass-timer
                        act_s    like ddmass-actsingle
                        excommit like ddmass-excommit
                        done     like ddrefstruc-bool
                        prid     like syst-tabix
                        prid1    like syst-tabix.

  data: tabname        like dd02v-tabname,
        act_mode       like ddrefstruc-mode,
        get_state      like dctablget-tabl,
        ntab_put_state like ddxtt-modeflag,
        act_res        like syst-subrc,
        dd02v_hd       like dd02v,
        tabl_res       like dctablres occurs 1 with header line,
        mess           like messid,
        settmst        like dctablact-settmst,
        tbatg_wa       like tbatg,
        tbatg_tab      like tbatg occurs 5,
        ins_res        like syst-subrc,
        upgrmode       like dctablact-upgrmode,
        action_mdf     like tbatg-fct,
        modeflag_m     like ddxtt-modeflag.
* Variables for Cluster-restrictions
  data: clust_flag(1),
        system_type like sy-sysid,
        ddcl(4) value 'DDCL'.        "Memory-Id for Cluster-flag
  data: dd12v_tab like dd12v occurs 0 with header line,
        got_state like ddrefstruc-state.
  data: res_info  type dctbactres,
        depaction type depaction.

*-Handle entry * for TABL-name-----------------------------------------*
* perform mt_drop_create_ddftx using genwa acwa done prid.
  perform mt_delete_lang using genwa acwa prid.

  check genwa-name <> '*'.

* <KH> 20040528 If object will be deleted and could not be deletetd during
* first effort because of still existing references do not activate
* Second effort will delete these objects
  check genwa-delflag <> 'X'.

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.
*-Set parameter--------------------------------------------------------*
  tabname  = genwa-name.
  act_mode = acwa-actmode1.
  "<KH> 20111207: In case of prelevel activation change to actmode3
  if genwa-prelevel = 'Y'.
    act_mode = acwa-actmode2.
    if genwa-actkind = 'A'.
      act_mode = acwa-actmode3.
    endif.
  endif.
  if action ca 'IUV' and genwa-actkind = 'A'.
    act_mode = acwa-actmode3.
  endif.
  get_state = 'M'.
  if genwa-local = 'A'. get_state = 'A'. endif.
  ntab_put_state = 'A'.
  if inactive = 'X'.
    ntab_put_state = 'N'.
      if act_mode = 4 or act_mode = 10 or act_mode = 41 or act_mode = 11.
        clear auth_chk.
      endif.
  endif.
  if action ca 'DB'. settmst = 'D'. endif.
  if action ca 'AB'. concatenate settmst 'A' into settmst. endif.
* Set Dynpro-Timestamp in case of PGMID LANG
  if genwa-pgmid = lang. settmst = 'D'. endif.
  if depact = 'X'. upgrmode = 'A'. endif.
*-Prepare Cluster-restrictions-----------------------------------------*
  clust_flag = 'X'.
  call function 'TR_SYS_PARAMS'
       importing
             systemtype         = system_type
       exceptions
            no_systemname      = 1
            no_systemtype      = 2
            others             = 3.
  if system_type = 'SAP'.
*    if syst-uname = 'DDIC' and acwa-act = 'O' and
    if acwa-act = 'O' and not genwa-objfunc = 'X'.
      clear clust_flag.
    endif.
  endif.
  export clust_flag to memory id ddcl.
*-Write begin-message to log-------------------------------------------*
  if     genwa-local = 'N' and genwa-actkind = 'N'. mess = 'DT012'.
    if genwa-pgmid = lang. mess = 'DT129'. endif.
  elseif genwa-local = 'A' and genwa-actkind = 'N'. mess = 'DT082'.
  elseif genwa-local = 'A' and genwa-actkind = 'A'. mess = 'DT014'.
  else.                                             mess = 'DT012'.
  endif.
  newob> prid1.
  if act_s = 'X' and genwa-actkind = 'N'.
     obj_header> prid1 mess genwa-name syst-uname syst-datum syst-uzeit.
  else.
    obj_header> prid1 mess genwa-name '' '' ''.
  endif.
  if genwa-objfunc = 'M'.
*    DO596: Transportrequest: Activation including DROP/CREATE
     smi0> prid1 'N' 'DO596'.
     action_mdf = 'MDF'. modeflag_m = 'M'.
  endif.
*-Activate table-------------------------------------------------------*
  call function 'DD_TABL_ACTM'
       exporting
            mode             = act_mode
            getstate         = get_state
            ntab_putstate    = ntab_put_state
            prid             = prid1
            settmst          = settmst
            tabname          = tabname
            auth_chk         = auth_chk
            excommit         = ' '
            upgrmode         = upgrmode
            action           = action_mdf
            modeflag         = modeflag_m
            cycleobj         = genwa-cycleobj
            prelevel         = genwa-prelevel
       importing
            act_result       = act_res
            dd02v_wa         = dd02v_hd
            ctrl_tabl_res    = tabl_res
            act_res_info     = res_info
       tables
            ctrl_tbvi_tab    = tbvi
            ctrl_tbsh_tab    = tbsh
            ctrl_tben_tab    = tben
       exceptions
            dbchange_failure = 1
            ntab_gen_failure = 2
            put_failure      = 3
            read_failure     = 4
            access_failure   = 5
            others           = 6.
  if syst-subrc <> 0.
    genwa-rc = 8.
  else.
    genwa-rc = act_res. genwa-actflag = dd02v_hd-actflag.
    depaction = res_info-settmst.
    if genwa-pgmid = lang.
*   Force activation of dependent objects of those transported with
*   LANG TABL because DDFTX-entries have to be deleted
      perform ut_set_action using genwa-actflag 'UD'.
    else.
      perform ut_set_action using genwa-actflag depaction.
    endif.
    if genwa-rc <= 4 and inactive = 'X'.
      import tbatg_tab from memory id 'INDX'.
      if syst-subrc = 0.
        read table tbatg_tab into tbatg_wa index 1.
        if syst-subrc = 0.  "Index-Operations exist
          call function 'DD_TBIX_GET'
            EXPORTING
              GET_STATE           = 'M'
              INDEXNAME           = '*'
              TABL_NAME           = tabname
            IMPORTING
              GOT_STATE           = got_state
            TABLES
              DD12V_TAB_N         = dd12v_tab
            EXCEPTIONS
              ILLEGAL_VALUE       = 1
              OTHERS              = 2.
          if sy-subrc <> 0 or got_state = ' '.
*           DT562: DB-Aktionen für Indizes Tabelle &: Index-Info konnte nicht gelesen werden
            PERFORM STDO_SMI1(radbtout) USING PRID 'W' 'DT562' TABNAME.
          endif.
        endif.
        loop at tbatg_tab into tbatg_wa.
*         <KH> 20050512 We have to check wether index is extension index.
*         In this case no db-actions have to be done
          read table dd12v_tab with key sqltab    = tbatg_wa-tabname
                                        indexname = tbatg_wa-indname.
          if dd12v_tab-isextind = 'X'. continue. endif.
          tbatg_wa-execmode = 'I'.
          perform insert_tbatg using tbatg_wa ins_res prid1.
          if ins_res > 0.
            if genwa-rc = 0. genwa-rc = 4. endif.
          endif.
        endloop.
      endif.
      if genwa-objfunc = 'M'.
        update ddxtt set modeflag = 'M' where tabname = genwa-name.
      endif.
    endif.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.
  if not ( tabl_res is initial ).
    move-corresponding tabl_res to cnvtab. append cnvtab.
  endif.

*-Write end-message to log---------------------------------------------*
  if genwa-rc > 0 or ( genwa-rc = 0 and
     ( acwa-act = 'O' or acwa-act = 'G' or acwa-act = 'T' ) ).
    perform write_begin_end_message(saplsdta) using acwa-actmode1
      genwa-name '' '' genwa-rc '' prid1.
  endif.

  if excommit = 'X'. commit work. endif.

endform.

*----------------------------------------------------------------------*
* FORM MT_TABT_SQTT_ACT
*----------------------------------------------------------------------*
* Activates Technical Settings
*----------------------------------------------------------------------*
* --> GENWA   : Current table to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> INACTIVE: 'X': Inactive activation, ' ': Active activation
* --> AUTH_CHK: 'X': Authority-check, ' ': no Authority-check
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_tabt_sqtt_act using  genwa    like dcgentb
                             acwa     like ddmassac
                             inactive like ddmass-inactive
                             auth_chk like ddmass-authchk
                             t_on     like ddmass-timer
                             excommit like ddmass-excommit
                             prid     like syst-tabix
                             prid1    like syst-tabix.

  data: tabtname   like dd09l-tabname,
        act_mode   like ddrefstruc-mode,
        get_state  like ddrefstruc-state,
        ntabstate  like dd02l-as4local,
        cnv_order  like ddrefstruc-cnv_order,
        act_result like syst-subrc,
        do_commit  like ddrefstruc-state,
        settmst    type settmst.

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.
*-Set activation parameters--------------------------------------------*
  tabtname  = genwa-name.
  act_mode  = acwa-actmode1.
  get_state = 'M'. if genwa-local = 'A'. get_state = 'A'. endif.
  ntabstate = 'A'. cnv_order = 'T'.  "Only TBATG entry is written
  if inactive = 'X'.
    ntabstate = 'N'.
    cnv_order = 'A'. "No action concerning conversion. Distribution
  endif.             "will decide
  do_commit = excommit.
*-Activate Technical Settings------------------------------------------*
  newob> prid1.
  call function 'DD_TABT_ACTM'
       exporting
            act_mode      = act_mode
            tabname       = tabtname
            get_state     = get_state
            ntabstate     = ntabstate
            cnv_order     = cnv_order
            auth_chk      = auth_chk
            timer_on      = t_on
            prid          = prid1
            excommit      = do_commit
       importing
            act_result    = act_result
            settmst       = settmst
       exceptions
            illegal_value = 1
            op_failure    = 2
            others        = 3.
  if syst-subrc <> 0.
    genwa-rc = 8.
  else.
    genwa-rc      = act_result.
    genwa-settmstdep = settmst.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

endform.                    " MT_TABT_SQTT_ACT

*----------------------------------------------------------------------*
* Form MT_SQLT_ACT
*----------------------------------------------------------------------*
* Avtivates physical Pools or Cluster
*----------------------------------------------------------------------*
* --> GENWA   : Current table to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> INACTIVE: 'X': Inactive activation, ' ': Active activation
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_sqlt_act using genwa    like dcgentb
                       acwa     like ddmassac
                       inactive like ddmass-inactive
                       auth_chk like ddmass-authchk
                       action   like dcdeptb-action
                       t_on     like ddmass-timer
                       excommit like ddmass-excommit
                       prid     like syst-tabix
                       prid1    like syst-tabix.

  data: ntab_putstate type dcsqltctrl-ntabstate,
        settmst       type dcsqltctrl-settmst,
        get_state     type dcsqltctrl-getstate,
        write_endmess type dcsqltctrl-write_endm,
        tabname       type dd06l-sqltab,
        rc            type syst-subrc.

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.
*-Set activation parameters--------------------------------------------*
  tabname   = genwa-name.
  get_state = 'M'.
  if genwa-local = 'A'. get_state = 'A'. endif.
  ntab_putstate = 'A'.
  if inactive = 'X'. ntab_putstate = 'N'. endif.
  if action ca 'DB'. settmst = 'D'. endif.
  if action ca 'AB'. concatenate settmst 'A' into settmst. endif.
  newob> prid1.
*-Control endmessage---------------------------------------------------*
  if acwa-act = 'O'. write_endmess = 'X'. endif.
*-Activate tablepool/cluster-------------------------------------------*
  call function 'DD_SQLT_ACT'
    exporting
      tabname                     = tabname
      TIMER_ON                    = t_on
      AUTH_CHK                    = auth_chk
      EXCOMMIT                    = ' '
      SETTMST                     = settmst
      GET_STATE                   = get_state
      NTAB_PUTSTATE               = ntab_putstate
      write_endmess               = write_endmess
    IMPORTING
      RC                          = rc
    CHANGING
      PRID                        = prid1
    EXCEPTIONS
      NOT_CREATED                 = 1
      WRONG_INPUT_PARAMETER       = 2
      OTHERS                      = 3.
  if sy-subrc <> 0.
    genwa-rc = 8.
  else.
    genwa-rc = rc.
  endif.

  if genwa-rc >= 4. perform set_message using genwa. endif.

  if excommit = 'X'. commit work. endif.

endform.                    " MT_SQLT_ACT

*----------------------------------------------------------------------*
* Form MT_SQLT_ACT_OLD
*----------------------------------------------------------------------*
* Avtivates physical Pools or Cluster
*----------------------------------------------------------------------*
* --> GENWA   : Current table to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> INACTIVE: 'X': Inactive activation, ' ': Active activation
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_sqlt_act_old using genwa    like dcgentb
                           acwa     like ddmassac
                           inactive like ddmass-inactive
                           t_on     like ddmass-timer
                           excommit like ddmass-excommit
                           prid     like syst-tabix
                           prid1    like syst-tabix.

* DDERR_O defined as table to transport values to ACTDD06OO
  tables: dderr_o.
  data: sqltab   like dd06l-sqltab,
        control(2), numb(1) type n,
        protname like dderr-protname value 'ACT_',
        dderr_wa like dderr, dderr_o_wa like dderr_o,
        var1 like ddprt-par1, var2 like ddprt-par2,
        var3 like ddprt-par3, var4 like ddprt-par4,
        mass_flag(3), ddma(4) value 'DDMA',
        prid2  like syst-tabix.
* Variables for Cluster-restrictions
  data: clust_flag(1),
        system_type like sy-sysid,
        ddcl(4) value 'DDCL'.        "Memory-Id for Cluster-flag

*-Write reference-log--------------------------------------------------*
  log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.
*-Set activation parameters--------------------------------------------*
  sqltab  = genwa-name.
  control = 'G1'. numb = 1.
  if genwa-local = 'A'. control = 'G3'. endif.
  dderr_wa-severity = 'N'.
  dderr_wa-errnum   = '0'.
  dderr_wa-errlist  = 'FFFF'.
  concatenate protname genwa-name sy-uzeit into dderr_wa-protname.
  perform set_dderr_o(rdd06kon) using dderr_wa 'X' 'D' '' dderr_o.
  if inactive = 'X'. mass_flag = 'W'. endif.
  export mass_flag to memory id ddma.
*-Prepare Cluster-restrictions-----------------------------------------*
  clust_flag = 'X'.
  call function 'TR_SYS_PARAMS'
       importing
             systemtype         = system_type
       exceptions
            no_systemname      = 1
            no_systemtype      = 2
            others             = 3.
  if system_type = 'SAP'.
    if syst-uname = 'DDIC' and acwa-act = 'O' and
       not genwa-objfunc = 'X'.
      clear clust_flag.
    endif.
    if genwa-objfunc = 'Y'. clust_flag = 'Y'. endif.
  endif.
  export clust_flag to memory id ddcl.
*-Flush messages already written to the device-------------------------*
  sync> prid. newob> prid1.
*-Activate physical Pool/Cluster---------------------------------------*
* Log-messages are written to channel 0 -> Therefore they are written
* to DB-log with Name ACT_TMP (PRID1) and to channel PRID. Because
* log with name ACT_TMP are redirected to PRID the messages written
* to channel PRID have to be removed.
  perform actdd06oo(rdd06kon) using sqltab control numb dderr_wa.
  open> 'T' 'R' '' dderr_wa-protname prid2.
  redirect> prid1 prid2. close> prid2.
*-Removes the messages which are written to channel PRID---------------*
* <KH> 20009211 Line deleted: since change to parallel activation
*               PRID and PRID1 mean the same log and so we can not
*               remove log from PRID
* remove> prid.
*-Fill GENWA with important information--------------------------------*
  perform get_messagetab(rdd06kon) using dderr_wa var1 var2 var3 var4.
  if dderr_wa-severity = 'W'.
    genwa-rc = 4.
  elseif ( dderr_wa-severity = 'E' or dderr_wa-severity = 'A' ).
    genwa-rc = 8.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.
*-Delete temporary database log----------------------------------------*
  delete from ddprh where protname = dderr_wa-protname.
  if syst-subrc = 0.
    delete from ddprs where protname = dderr_wa-protname.
    if syst-subrc <> 0.
      smi2> prid1 'E' 'DO523' dderr_wa-protname 'FD'.
    endif.
  else.
    smi2> prid1 'E' 'DO523' dderr_wa-protname 'HD'.
  endif.

  if excommit = 'X'. commit work. endif.

endform.                    " MT_SQLT_ACT

*----------------------------------------------------------------------*
* Form MT_INDX_ACT
*----------------------------------------------------------------------*
* Activates secondary-indexes
*----------------------------------------------------------------------*
* --> GENWA   : Contains current index to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> INACTIVE: 'X': Inactive Nametab, ' ': Active Nametab
* --> AUTH_CHK: 'X': Authority-check, ' ': no Authority-check
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_indx_act using  genwa     like dcgentb
                        acwa      like ddmassac
                        inactive  like ddmass-inactive
                        auth_chk  like ddmass-authchk
                        t_on      like ddmass-timer
                        excommit  like ddmass-excommit
                        prid      like syst-tabix
                        prid1     like syst-tabix.

  data: act_mode   like ddrefstruc-mode,
        get_state  like dd12v-as4local,
        auth_check like ddrefstruc-bool,
        tabname    like dd12v-sqltab,
        indxname   like dd12v-indexname,
        rc         like syst-subrc,
        dd03p_tab  like dd03p occurs 0,   "Dummy tables
        dd12v_tab  like dd12v occurs 0,
        dd17v_tab  like dd17v occurs 0,
        id_indx(4) value 'INDX',
        dbact      like tbatg-fct,
        settmst    type settmst,
        tbatg_wa   like tbatg,
        tbatg_tab  like tbatg occurs 1.

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.
*-Set parameter--------------------------------------------------------*
  tabname   = genwa-name. indxname = genwa-indx. condense indxname.
  act_mode  = acwa-actmode1.
  get_state = 'M'.
  if genwa-local = 'A'. get_state = 'A'. endif.
  auth_check = auth_chk.
*-Activate secondary index---------------------------------------------*
  newob> prid1.
  call function 'DD_INDX_ACTIVATE'
       exporting
            act_control     = act_mode
            auth_chk        = auth_check
            get_status      = get_state
            indexname       = indxname
            prid            = prid1
            tabname         = tabname
       importing
            rc              = rc
            dbact           = dbact
            settmst         = settmst
       tables
            dd03p_tab       = dd03p_tab
            dd12v_tab       = dd12v_tab
            dd17v_tab       = dd17v_tab
       exceptions
            others          = 1.
  if syst-subrc = 8.
    genwa-rc = 8.
  else.
    genwa-rc = rc.
    genwa-settmstdep = settmst.
    if genwa-rc <= 4 and inactive = 'X'.
      import tbatg_tab from memory id 'INDX'.
      if syst-subrc = 0.
        read table tbatg_tab into tbatg_wa index 1.
        if syst-subrc = 0.
          tbatg_wa-execmode = 'I'.
          perform insert_tbatg using tbatg_wa rc prid1.
          if rc > 0.
            if genwa-rc = 0. genwa-rc = 4. endif.
          endif.
        endif.
      endif.
    endif.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

  if excommit = 'X'. commit work. endif.

endform.                    " MT_INDX_ACT

*----------------------------------------------------------------------*
* Form MT_VIEW_ACT
*----------------------------------------------------------------------*
* Activates views
*----------------------------------------------------------------------*
* <-> VITB    : Dependent tables to activate because of view-changes   *
* --> GENWA   : Contains current index to activate                     *
* --> ACWA    : Control-parameter such as activation-mode              *
* --> INACTIVE: 'X': Write inactiva Nametab, '': Write active Nametab  *
* --> AUTH_CHK: 'X': Authority-check, ' ': no Authority-check          *
* --> DEPACT  : 'X', if dependent objects have to be activated
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off         *
* --> ACT_S   : 'X': Activation of single table/structure with
*               dependent objects, ' ': Mass-activation
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for       *
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual         *
*               activation-log
*----------------------------------------------------------------------*
form mt_view_act tables vitb     type vitbtb
                 using  genwa    like dcgentb
                        acwa     like ddmassac
                        inactive like ddmass-inactive
                        auth_chk like ddmass-authchk
                        depact   like ddmass-frcact
                        t_on     like ddmass-timer
                        act_s    like ddmass-actsingle
                        excommit like ddmass-excommit
                        prid     like syst-tabix
                        prid1    like syst-tabix.

  data: get_state(1),
        act_mode like ddrefstruc-mode,
        rc like syst-subrc,
        dbact(3),
        dd26v_tab_a like dd26v occurs 0,   "Tables
        dd27p_tab_a like dd27p occurs 0,
        dd28v_tab_a like dd28v occurs 0,
        dd26v_tab_n like dd26v occurs 0,
        dd27p_tab_n like dd27p occurs 0,
        dd28v_tab_n like dd28v occurs 0,
        dd25v_wa_a  like dd25v,            "Structures
        dd25v_wa_n  like dd25v,
        dd25v_hd    like dd25v,
        mess(5),
        ctrl like dcviewactm.

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.

* <KH> 20080206 If object will be deleted and could not be deleted during
* first effort because of still existing references do not activate
* Second effort will delete these objects
  check genwa-delflag <> 'X'.

*-Set parameter--------------------------------------------------------*
  act_mode = acwa-actmode1.
  get_state = 'M'.
  if genwa-local = 'A' or genwa-actkind = 'A'. get_state = 'A'. endif.

*-Write begin-message to log-------------------------------------------*
  if     genwa-local = 'N' and genwa-actkind = 'N'. mess = 'MC738'.
  elseif genwa-local = 'A' and genwa-actkind = 'N'. mess = 'DO524'.
  elseif genwa-local = 'A' and genwa-actkind = 'A'. mess = 'DO525'.
  else.                                             mess = 'MC738'.
  endif.
  newob> prid1.
  if act_s = 'X' and genwa-actkind = 'N'.
    obj_header> prid1 mess genwa-name syst-uname syst-datum syst-uzeit.
  else.
    obj_header> prid1 mess genwa-name '' '' ''.
  endif.
  if genwa-objfunc = 'M'.
*   DO596: Transportrequest: Activation including DROP/CREATE
    smi0> prid1 'N' 'DO596'.
  endif.
*-Set control-ledge for view-activation--------------------------------*
  perform map_control using act_mode inactive ctrl.
  ctrl-checkonly  = ' '.
  ctrl-inactive   = inactive.   ctrl-getstate = get_state.
  ctrl-auth_check = auth_chk.
  if excommit = 'X'. ctrl-excommit = 'X'. endif.
* In active case adapt database
  if inactive = ' '. ctrl-dbadapt = 'X'. endif.
  ctrl-basfrombuf = ' '.
* In upgrade-mode smooth checks for objects dependent of views
  if depact = 'X'. ctrl-depabort = ' '. endif.
* Switch on dependent activation
  if genwa-actkind = 'A'. ctrl-depmode = 'X'. endif.
  ctrl-exdeptab = ' '.
* In upgrade-mode or during view-activation as dependent object changes
* can not be computed
  if depact = 'X' or genwa-actkind = 'A'. ctrl-nodelta = 'X'. endif.
* No database-changes in case of convertor-activation
  if act_mode = 4. ctrl-dbadapt = ' '. endif.
* <KH> 20130119 Enforce setting of timestamp
  if genwa-action ca 'DB'. ctrl-settmst = 'D'. endif.
  if genwa-action ca 'AB'.
    concatenate ctrl-settmst 'A' into ctrl-settmst.
  endif.
* For replacement check in view activation
  ctrl-prelevel = genwa-prelevel.
*-Activate view--------------------------------------------------------*
  call function 'DD_VIEW_INNER_ACT'
       exporting
            viewname     = genwa-name
            prid         = prid1
            mode_control = ctrl
            act_mode     = act_mode
       importing
            rc           = rc
            dbact        = dbact
            dd25v_hd     = dd25v_hd
       tables
            dep_tab      = vitb.
  genwa-rc = rc. genwa-actflag = dd25v_hd-actflag.
  if dd25v_hd-viewclass = 'A' and genwa-actflag = ' '.
    genwa-actflag = 'V'.
    perform set_actflag using genwa 'V' prid.
  endif.
  if dd25v_hd-viewclass = 'D' and genwa-actflag = ' '.
    perform mt_handle_derived_type using genwa prid.
  endif.

*-Handle OBJFUNC = 'M'-------------------------------------------------
  if genwa-rc <= 4 and genwa-objfunc = 'M'.
    perform mt_view_drop_create using genwa-name rc prid1.
  endif.

*-Write end-message to log---------------------------------------------*
  if genwa-rc > 0 or ( genwa-rc = 0 and acwa-act = 'O' ).
    if genwa-rc = 0. obj_tail> prid1 'MC740' genwa-name. endif.
    if genwa-rc = 4. obj_tail> prid1 'DO526' genwa-name. endif.
    if genwa-rc > 4. obj_tail> prid1 'MC742' genwa-name. endif.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

endform.                    " MT_VIEW_ACT

*----------------------------------------------------------------------*
* Form MAP_CONTROL
*----------------------------------------------------------------------*
* Initialises the view control list
*----------------------------------------------------------------------*
* --> ACT_MODE: Activation mode for views
* --> INACTIVE: '': Write active Nametab, 'X': Write inactive Nametab
* --> CTRL    : Control-parameter for activation actions
*----------------------------------------------------------------------*
form map_control using act_mode type dd_actmode inactive like ddmass-inactive
                       ctrl type dcviewactm.

  data: mode type dd_actmode.

  mode = act_mode.
  if act_mode = 0.
    mode = 1.
  endif.
  if inactive = 'X'.
    mode = 2.
  endif.
  perform map_mode2modecontrol(saplsdva) using mode '' '' '' ctrl.

endform.

*----------------------------------------------------------------------*
* Form MT_ENQU_ACT
*----------------------------------------------------------------------*
* Activates enqueue-objects
*----------------------------------------------------------------------*
* --> GENWA   : Contains current index to activate
* --> INACTIVE: '': Write active Nametab, 'X': Write inactive Nametab
* --> ACWA    : Control-parameter such as activation-mode
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_enqu_act using genwa    like dcgentb
                       acwa     like ddmassac
                       inactive like ddmass-inactive
                       t_on     like ddmass-timer
                       excommit like ddmass-excommit
                       prid     like syst-tabix
                       prid1    like syst-tabix.

  data: act_mode like ddrefstruc-mode,
        enquname like dd25v-viewname.
  data  enact like dcenactdet.
  data  rc like syst-subrc.
  data  tbatg_wa like tbatg.
  data  mess(5).

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.

*-Set parameter--------------------------------------------------------*
  enquname = genwa-name.
  act_mode = acwa-actmode1.
  if acwa-act = 'O'.
    enact-online_fl = 'X'.
  endif.
  enact-get_state = 'M'.
  if genwa-local = 'A'. enact-get_state = 'A'. endif.
  enact-write_sour = 'X'.
  enact-commitflag = ' '.
  if t_on = 'X'. enact-timer_on = 'X'. endif.
  if inactive = ' '. enact-write_fumo = 'X'. endif.
  enact-upgradek = 'P'. enact-is_depend = 'X'.
  enact-objheader = ' '.
*-Write begin-message to log-------------------------------------------*
  if     genwa-local = 'N' and genwa-actkind = 'N'. mess = 'DI806'.
  elseif genwa-local = 'A' and genwa-actkind = 'N'. mess = 'DO613'.
  elseif genwa-local = 'A' and genwa-actkind = 'A'. mess = 'DO614'.
  else.                                             mess = 'DI806'.
  endif.
  newob> prid1.
  obj_header> prid1 mess genwa-name '' '' ''.

*-Activate enqueue-object----------------------------------------------*
  call function 'DD_ENQU_ACT'
       exporting
            enquname        = enquname
            actmode         = act_mode
            actions         = enact
*           PROTNAME        = ' '
            prid            = prid1
       importing
*           RESULTS         =
            rc              = rc
       exceptions
            parameter_error = 1
            actok_failure   = 2
            others          = 3.
  if syst-subrc = 8.
    genwa-rc = 8.
  else.
    genwa-rc = rc.
  endif.

  if genwa-rc > 0 or acwa-act = 'O'.
    if genwa-rc = 0. obj_tail> prid1 'DI811' genwa-name. endif.
    if genwa-rc = 4. obj_tail> prid1 'DI812' genwa-name. endif.
    if genwa-rc > 4. obj_tail> prid1 'DI813' genwa-name. endif.
  endif.

  if genwa-rc <= 4 and acwa-wr_tbatg = 'X'.
* Initiate generation for enqueue-functions
    tbatg_wa-object  = enqu.
    tbatg_wa-tabname = enquname.
    tbatg_wa-fct     = 'CNV'.
    tbatg_wa-execmode = 'I'.
    perform insert_tbatg using tbatg_wa rc prid1.
    if rc > 4.
      if genwa-rc = 0. genwa-rc = 4. endif.
    endif.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

  if excommit = 'X'. commit work. endif.

endform.                    " MT_ENQU_ACT

*----------------------------------------------------------------------*
* Form MT_SQSC_ACT
*----------------------------------------------------------------------*
* Activates Database Prcedure Proxies
*----------------------------------------------------------------------*
* --> GENWA   : Contains current Database Function Pool to activate
* --> INACTIVE: '': Activate Database Function with Database adaptation
*                   and update of tables holding dependencies
*               'X': Send entry for conversion to TBATG
* --> ACWA    : Control-parameter such as activation-mode
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_sqsc_act using genwa    like dcgentb
                       acwa     like ddmassac
                       inactive like ddmass-inactive
                       t_on     like ddmass-timer
                       excommit like ddmass-excommit
                       prid     like syst-tabix
                       prid1    like syst-tabix.

  data: sqscname   type if_dbproc_proxy_basic_types=>ty_abap_name,
        err_ref    type ref to cx_dbproc_proxy,
        proxy_ref  type ref to if_dbproc_proxy_ma,
        class_name type progname,
        actmode    type i,
        rc         type sysubrc,
        tbatg_wa   type tbatg,
        mess(100).

*-Set parameter--------------------------------------------------------*
*-Write begin-message to log-------------------------------------------*
  mess = 'DI401'. "We only support direct handling of newest version,
                  "not as dependent object or active version if inactive
                  "is existing too
  newob> prid1.
  obj_header> prid1 mess genwa-name '' '' ''.
  if ( genwa-local = 'A' and genwa-actkind = 'N' ) or  "Acivation of active version
     ( genwa-local = 'A' and genwa-actkind = 'A' ).    "Activation as dep. object
    smi0> prid 'I' 'DI405'.
  endif.

*-Activate Database Function----------------------------------------------*
  sqscname = genwa-name.
  if inactive = 'X'.
    "In transport/upgrade mode we write sources only and TBATG entry for
    "creation of transfer tables. Runtime object is also generated in converter later.
    actmode    = if_dbproc_proxy_ma=>cf_act_transport.
    class_name = 'CL_DBPROC_PROXY_FACTORY_TR'.
  else.
    "All actions are executed immediately
    actmode    = if_dbproc_proxy_ma=>cf_act_online.
    class_name = 'CL_DBPROC_PROXY_FACTORY'.
  endif.

  try.
    call method (class_name)=>get_proxy_for_ma
                                     exporting
                                       if_proxy_name = sqscname
                                     receiving
                                       ref = proxy_ref.
    "proxy_ref = ( class_name )=>get_proxy_for_ma( if_proxy_name = sqscname ).
    genwa-rc = proxy_ref->activate( if_mode  = actmode
                                    if_prid  = prid
                                    if_force = ABAP_false ).
  catch cx_dbproc_proxy into err_ref.
    mess = err_ref->get_Text( ).
    str_sev> prid mess 'E'.
    genwa-rc = 8.
  endtry.

  if genwa-rc <= 4 and acwa-wr_tbatg = 'X'.  "Only set in inactive case
    tbatg_wa-object  = sqsc.
    tbatg_wa-tabname = sqscname.
    tbatg_wa-fct     = 'CNV'.
    tbatg_wa-execmode = 'I'.
    perform insert_tbatg using tbatg_wa rc prid1.
    if rc > 4.
      if genwa-rc = 0. genwa-rc = 4. endif.
    endif.
  endif.

  if genwa-rc > 0 or acwa-act = 'O'.
    if genwa-rc = 0. obj_tail> prid1 'DI402' genwa-name. endif.
    if genwa-rc = 4. obj_tail> prid1 'DI403' genwa-name. endif.
    if genwa-rc > 4. obj_tail> prid1 'DI404' genwa-name. endif.
  endif.

  if genwa-rc >= 4. perform set_message using genwa. endif.

  if excommit = 'X'. commit work. endif.

endform.                    " MT_SQSC_ACT

*----------------------------------------------------------------------*
* Form MT_STOB_ACT
*----------------------------------------------------------------------*
* Activates a Structured Object
*----------------------------------------------------------------------*
* --> GENWA   : Contains current index to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> INACTIVE: 'X': Write inactiva Nametab, '': Write active Nametab
* --> AUTH_CHK: 'X': Authority-check, ' ': no Authority-check
* --> DEPACT  : 'X', if dependent objects have to be activated
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> ACT_S   : 'X': Activation of single table/structure with
*               dependent objects, ' ': Mass-activation
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> FRCACT  : Force activation (newly compute Append/Parent info)
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
* <-- CNVTAB  : Table for structural changes
*----------------------------------------------------------------------*
form mt_stob_act using  genwa    like dcgentb
                        acwa     like ddmassac
                        inactive like ddmass-inactive
                        t_on     like ddmass-timer
                        act_s    like ddmass-actsingle
                        excommit like ddmass-excommit
                        frcact   type ddmass-frcact
                        prid     like syst-tabix
                        prid1    like syst-tabix
                 changing cnvtab  type dctablrestab.

  data: get_state    type objstate,
        act_mode     type dd_actmode,
        strucobjname type ddstrucobjname,
        access_ref   type ref to if_dd_sobject_int,
        err_ref      type ref to cx_dd_sobject,
        rc           type sysubrc,
        mess(8),
        ctrl         type dcsobjactctrl.

* <KH> 20080206 If object will be deleted and could not be deleted during
* first effort because of still existing references do not activate
* Second effort will delete these objects
* <KH> 20131112 If Ddl Source is deleted inactively the STOB sources remain
* active for further usage. Thus, the flag can also be set if STOB was
* still computed as dependent object and Ddl source is deleted
  check genwa-delflag <> 'X'.

*-Set parameter--------------------------------------------------------*
  ctrl-getstate = 'M'.
  ctrl-write = ABAP_true. "Write
  if genwa-local = 'A' or genwa-actkind = 'A'. ctrl-getstate = 'A'. endif.

*-Write begin-message to log-------------------------------------------*
  if     genwa-local = 'N' and genwa-actkind = 'N'. mess = 'STOBJ011'.
  elseif genwa-local = 'A' and genwa-actkind = 'N'. mess = 'STOBJ013'.
  elseif genwa-local = 'A' and genwa-actkind = 'A'. mess = 'STOBJ012'.
  else.                                             mess = 'STOBJ011'.
  endif.
  newob> prid1.
  if act_s = 'X' and genwa-actkind = 'N'.
    obj_header> prid1 mess genwa-name syst-uname syst-datum syst-uzeit.
  else.
    obj_header> prid1 mess genwa-name '' '' ''.
  endif.
*-Set control-ledge for Struc. Object activation------------------------*
  "We can only set this control information here which is independent
  "of object CLASS (Struc. Object or Struc. Object APPEND). All missing information
  "is set later during activation
  act_mode = acwa-actmode1.
  ctrl-compress = ABAP_true.
  if act_mode = 1.
    ctrl-checkmode        = 1.
    ctrl-tableappcheck    = ABAP_true.
    ctrl-tableappactmode  = 2.
    "We do not know here if we have Append or main Structures Object.
    "Control info is adapted again later during single activation
    ctrl-main_expand = ctrl-append_expand =  'E  '.
  elseif act_mode = 2.   "Transport/Put
    ctrl-checkmode       = 2.
    ctrl-tableappcheck   = ABAP_false.
    ctrl-tableappactmode = 5.
   "We do not know here if we have Append or main Structures Object.
   "Control info is adapted again later during single activation
    ctrl-main_expand = ctrl-append_expand = 'W  '.
  endif.
  "Appends and tables are newly generated and thus newest version has
  "to be activated
  ctrl-tableappgetstate = 'M'.
  ctrl-ntabstate = 'A'.
  ctrl-putstate  = 'A'.
  if genwa-local = 'A' or genwa-actkind = 'A'.
    ctrl-putstate  = 'R'.
  endif.
  if inactive = ABAP_true.
    ctrl-ntabstate = 'N'.
    ctrl-putstate  = 'I'.
    if genwa-local = 'A' or genwa-actkind = 'A'.
      ctrl-putstate  = 'S'.
    endif.
  endif.
  ctrl-verbose = 1.
  ctrl-frcact  = frcact.
*-Activate BO-structure------------------------------------------------*
  access_ref = cl_dd_sobject_factory=>create_internal( ).
  try.
    strucobjname = genwa-name.
    access_ref->activate_internal(
      exporting
        strucobjname    = strucobjname
        actctrl         = ctrl
        prid            = prid1
      importing
        action_tab      = cnvtab
        activation_flag = genwa-actflag
        rc              = rc ).
  catch cx_dd_sobject_get cx_dd_sobject_put into err_ref.
    rc = 8.
  endtry.
  genwa-rc = rc.
  if genwa-rc >= 4. perform set_message using genwa. endif.

*-Write end-message to log---------------------------------------------*
  if genwa-rc > 0 or ( genwa-rc = 0 and acwa-act = 'O' ).
    if genwa-rc = 0. obj_tail> prid1 'STOBJ004' genwa-name. endif.
    if genwa-rc = 4. obj_tail> prid1 'STOBJ005' genwa-name. endif.
    if genwa-rc > 4. obj_tail> prid1 'STOBJ006' genwa-name. endif.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

  if excommit = 'X'. commit work. endif.

endform.                    " MT_STOB_ACT

*----------------------------------------------------------------------*
* Form  MT_MCOB_ACT
*----------------------------------------------------------------------*
* Activates Matchcode-objects
*----------------------------------------------------------------------*
* --> GENWA   : Contains current index to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_mcob_act tables mcomcid  type mcomcidtb
                 using  genwa    like dcgentb
                        acwa     like ddmassac
                        t_on     like ddmass-timer
                        excommit like ddmass-excommit
                        prid     like syst-tabix
                        prid1    like syst-tabix.

  data: mconame like dd20v-mconame,
        act_control(2),
        rc like syst-subrc,
        objname like tbatg-tabname.

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.

*-Set parameter--------------------------------------------------------*
  mconame     = genwa-name.
  act_control = acwa-actmode1.
  if genwa-local = 'A'. endif.
  if t_on        = 'X'. endif.

*-Activate Matchcode-object--------------------------------------------*
  newob> prid1.
  call function 'MC_ACT_MCO'
       exporting
            act_control = act_control
            act_mconame = mconame
            prid        = prid1
       importing
            rc          = rc
       tables
            mctbat_tab  = mcomcid
       exceptions
            others      = 1.
  if syst-subrc = 8.
    genwa-rc = 8.
  else.
    genwa-rc = rc.
  endif.

*-Check wether TABTG-entries have to be done---------------------------*
  if genwa-rc < 8.
    read table mcomcid with key object = 'MCIA'.    "Check if dependent
    if syst-subrc = 0. genwa-actflag = 'U'. endif.  "MC-Ids exist
    if acwa-wr_tbatg = 'X'.
      perform insert_tbatg_mc tables mcomcid using rc prid1.
      if syst-subrc > 0.
        if genwa-rc = 0. genwa-rc = 4. endif.
      endif.
    endif.
  else.
    refresh mcomcid. clear mcomcid. free mcomcid.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

  if excommit = 'X'. commit work. endif.

endform.                    " MT_MCOB_ACT

*----------------------------------------------------------------------*
* Form MT_MCID_ACT
*----------------------------------------------------------------------*
* Activate Matchcode-Ids
*----------------------------------------------------------------------*
* --> GENWA   : Contains current index to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_mcid_act tables mcomcid  type mcomcidtb
                 using  genwa    like dcgentb
                        acwa     like ddmassac
                        t_on     like ddmass-timer
                        excommit like ddmass-excommit
                        prid     like syst-tabix
                        prid1    like syst-tabix.

  data: mconame like dd20v-mconame, mcidname like dd23v-mcid,
        act_control(2),
        rc like syst-subrc.

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.

*-Set parameter--------------------------------------------------------*
  mconame = genwa-name(4). mcidname = genwa-name+4(1).
  act_control    = acwa-actmode1.
  if genwa-local = 'A'. endif.
  if t_on        = 'X'. endif.

*-Activate Matchcode-Id------------------------------------------------*
  newob> prid1.
  call function 'MC_ACT_MCID'
       exporting
            act_mconame = mconame
            act_mcid    = mcidname
            act_control = act_control
            prid        = prid1
       importing
            rc          = rc
       tables
            mctbat_tab  = mcomcid
       exceptions
            others      = 1.
  if syst-subrc = 8.
    genwa-rc = 8.
  else.
    genwa-rc = rc.
  endif.

*-Check wether TABTG-entries have to be done---------------------------*
  if genwa-rc < 8.
    if acwa-wr_tbatg = 'X'.
      perform insert_tbatg_mc tables mcomcid using rc prid1.
      if syst-subrc > 0.
        if genwa-rc = 0. genwa-rc = 4. endif.
      endif.
    endif.
  else.
    refresh mcomcid. clear mcomcid. free mcomcid.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

  if excommit = 'X'. commit work. endif.

endform.                    " MT_MCID_ACT

*----------------------------------------------------------------------*
* Form  MT_SHLP_ACT
*----------------------------------------------------------------------*
* Activates Searchhelp
*----------------------------------------------------------------------*
* --> GENWA   : Contains current index to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_shlp_act using genwa    like dcgentb
                       acwa     like ddmassac
                       t_on     like ddmass-timer
                       excommit like ddmass-excommit
                       prid     like syst-tabix
                       prid1    like syst-tabix.

  data: actmode like ddrefstruc-mode,
        shlpname like dd30v-shlpname.
  data  shact like dcshactdet.
  data  rc like syst-subrc.
  data  mess(5).

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.

* <KH> 20080206 If object will be deleted and could not be deleted during
* first effort because of still existing references do not activate
* Second effort will delete these objects
  check genwa-delflag <> 'X'.

*-Set parameter--------------------------------------------------------*
  shlpname = genwa-name.
  actmode  = acwa-actmode1.
  if acwa-act = 'O'.
    shact-online_fl = 'X'.
    shact-upgradek  = 'N'.
  elseif acwa-act = 'T'.
    shact-upgradek = 'P'.
  elseif acwa-act = 'A'.
    shact-upgradek = 'A'.
  endif.
  shact-get_state = 'M'.
  if genwa-local = 'A' or genwa-actkind = 'A'.
    shact-get_state = 'A'.
  endif.
  if genwa-actkind = 'A'. shact-is_depend = 'X'. endif.
  shact-write_sour = 'X'.
  if excommit = 'X'. shact-commitflag = 'X'. endif.
  if t_on = 'X'. shact-timer_on = 'X'. endif.
  shact-objheader = ' '.

*-Write begin-message to log-------------------------------------------*
  if     genwa-local = 'N' and genwa-actkind = 'N'. mess = 'DH102'.
  elseif genwa-local = 'A' and genwa-actkind = 'N'. mess = 'DO547'.
  elseif genwa-local = 'A' and genwa-actkind = 'A'. mess = 'DO548'.
  else.                                             mess = 'DH102'.
  endif.
  newob> prid1.
  obj_header> prid1 mess genwa-name '' '' ''.

*-Activate Searchhelp--------------------------------------------------*
  call function 'DD_SHLP_ACT'
       exporting
            shlpname        = shlpname
            actmode         = actmode
            actions         = shact
            with_shlp       = ' '
            prid            = prid1
       importing
            rc              = rc
       exceptions
            parameter_error = 1
            actok_failure   = 2
            others          = 3.
  if syst-subrc = 8.
    genwa-rc = 8.
  else.
    genwa-rc = rc.
  endif.

  if genwa-rc > 0 or acwa-act = 'O'.
    if genwa-rc = 0. obj_tail> prid1 'DH107' genwa-name. endif.
    if genwa-rc = 4. obj_tail> prid1 'DH108' genwa-name. endif.
    if genwa-rc > 4. obj_tail> prid1 'DH109' genwa-name. endif.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

  if genwa-rc <= 4.
    if shact-is_depend = ' '.
      genwa-actflag = 'I'.
    else.
      perform set_actflag using genwa ' ' prid.
    endif.
  endif.

endform.                    " MT_SHLP_ACT

*----------------------------------------------------------------------*
* Form  MT_TTYP_ACT
*----------------------------------------------------------------------*
* Activates tabletypes
*----------------------------------------------------------------------*
* --> GENWA   : Contains current index to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> INACTIVE: 'X': Inactive activation, ' ': Active activation
* --> ACTION  : Contains actions which are necessary for dependent
*               tabletype
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_ttyp_act using genwa    like dcgentb
                       acwa     like ddmassac
                       inactive like ddmass-inactive
                       action   like dcdeptb-action
                       t_on     like ddmass-timer
                       excommit like ddmass-excommit
                       prid     like syst-tabix
                       prid1    like syst-tabix.

  data: actmode   like ddrefstruc-mode,
        ttypact   like dcttypactp,
        ttypctrl  like dcttypctrl,
        begin of ttyps occurs 0,
          name like dd40l-typename,
        end of ttyps,
        actres    like ddttactres occurs 0 with header line,
        tablres   like dctablres occurs 0 with header line,
        mess(5).

*-Write reference-log--------------------------------------------------*
* log_ref_cre> genwa-name genwa-type genwa-indx 'ACT' prid.

* <KH> 20080208 If object will be deleted and could not be deletetd during
* first effort because of still existing references do not activate
* Second effort will delete these objects
  check genwa-delflag <> 'X'.

*-Set parameter--------------------------------------------------------*
  ttyps-name = genwa-name. append ttyps.
  actmode   = acwa-actmode1.
  ttypact-get_state = 'M'.
  if genwa-local = 'A'. ttypact-get_state = 'A'. endif.
  ttypact-ntabstate = 'A'.
  if inactive = 'X'. ttypact-ntabstate = 'N'. endif.
  if action ca 'DB'. ttypact-settmst = 'D'. endif.
  if action ca 'AB'.
    concatenate ttypact-settmst 'A' into ttypact-settmst.
  endif.
  if genwa-cycleobj     = 'X'. ttypctrl-cycleobj = 'T'.
  elseif genwa-cycleobj = ' '.
    if acwa-act = 'O'.
      clear ttypctrl-cycleobj.  "Check is executed on base
    else.        "of locally computed dependencies in singel activator
      ttypctrl-cycleobj = 'F'.
    endif.
  endif.
  if genwa-is_used     = 'X'. ttypctrl-is_used = 'T'.
  elseif genwa-is_used = ' '. ttypctrl-is_used = 'F'.
  endif.
*-Write begin-message to log-------------------------------------------*
  if     genwa-local = 'N' and genwa-actkind = 'N'. mess = 'DO802'.
  elseif genwa-local = 'A' and genwa-actkind = 'N'. mess = 'DO812'.
  elseif genwa-local = 'A' and genwa-actkind = 'A'. mess = 'DO811'.
  else.                                             mess = 'DO802'.
  endif.
  ttypact-send_start = 'F'.
  ttypact-send_succ  = 'F'.
  ttypact-send_fail  = 'F'.
  newob> prid1.
  obj_header> prid1 mess genwa-name '' '' ''.
*-Activate tabletype---------------------------------------------------*
  call function 'DD_TTYP_SET_ACT'
       exporting
            act_mode         = actmode
            act_ctrl_par     = ttypact
            auth_chk         = 'X'
            prid             = prid1
            off              = 0
            len              = 0
            timer_on         = t_on
            trace            = 0
            ext_chk_par      = ttypctrl
       tables
            ttyp_names       = ttyps
            act_results      = actres
            ctrl_tabl_res    = tablres
       exceptions
            illegal_value    = 1
            act_refused      = 2
            read_failure     = 3
            put_failure      = 4
            internal_failure = 5
            others           = 6.
  if sy-subrc <> 0.
    genwa-rc = 8.
  else.
    genwa-rc = 8.
    read table actres index 1.
    if syst-subrc = 0.
      genwa-rc = actres-act_result. genwa-actflag = actres-depaction.
    endif.
    if genwa-rc > 8. genwa-rc = 8. endif.
*---In case of upgrade-mode set maximal ACTFLAG possible---------------*
    if acwa-act = 'A' and actres-depaction = ' '.
      genwa-actflag = acwa-maxflg.
    endif.
  endif.
  if genwa-rc > 4.      mess = 'DO804'.
  elseif genwa-rc = 4.  mess = 'DO813'.
  else.                 mess = 'DO803'.
  endif.
  if genwa-rc >= 4 or acwa-act = 'O'.
    obj_tail> prid1 mess genwa-name.
  endif.
  if genwa-rc >= 4. perform set_message using genwa. endif.

  if excommit = 'X'. commit work. endif.

endform.            "MT_TTYP_ACT

*----------------------------------------------------------------------*
* Form MT_DDLS_ACT
*----------------------------------------------------------------------*
* Activates a Ddls Source
*----------------------------------------------------------------------*
* --> DEPTAB  : Find out if there are dependent objects to activate
* --> GENWA   : Contains current index to activate
* --> ACWA    : Control-parameter such as activation-mode
* --> INACTIVE: 'X': Write inactiva Nametab, '': Write active Nametab
* --> AUTH_CHK: 'X': Authority-check, ' ': no Authority-check
* --> DEPACT  : 'X', if dependent objects have to be activated
* --> T_ON    : 'X': Timer on, ' ': Timer remains switched off
* --> ACT_S   : 'X': Activation of single table/structure with
*               dependent objects, ' ': Mass-activation
* --> EXCOMMIT: 'X': Commit work after every single object-activation,
*               ' ': No commit
* --> FRCACT  : Force activation (newly compute Append/Parent info)
* --> PRID    : Id for log-writer, physical existing channel for
*               reference-log (<- needs physical existing channel)
* --> PRID1   : Id for log-writer, internal channel for actual
*               activation-log
*----------------------------------------------------------------------*
form mt_ddls_act tables   deptab   type deptb
                 using    acwa     like ddmassac
                          inactive like ddmass-inactive
                          t_on     like ddmass-timer
                          act_s    like ddmass-actsingle
                          excommit like ddmass-excommit
                          prid     like syst-tabix
                          prid1    like syst-tabix
                 changing genwa    type dcgentb.

  data: get_state    type objstate,
        act_mode     type dd_actmode,
        ddlname      type ddlname,
        access_ref   type ref to if_dd_ddl_handler_internal,
        rc           type sysubrc,
        mess(8),
        ctrl         type dcddlsactctrl,
        ex           type ref to cx_dd_ddl_exception,
        err_info     type ddl2ddicerr,
        videptab     type dcobjdeptab.

* <KH> 20080206 If object will be deleted and could not be deleted during
* first effort because of still existing references do not activate
* Second effort will delete these objects
  check genwa-delflag <> 'X'.

*-Set parameter--------------------------------------------------------*
  ctrl-getstate = 'M'.
  if genwa-local = 'A' or genwa-actkind = 'A'. ctrl-getstate = 'A'. endif.

*-Write begin-message to log-------------------------------------------*
  if     genwa-local = 'N' and genwa-actkind = 'N'. mess = 'DDLS030'.
  elseif genwa-local = 'A' and genwa-actkind = 'N'. mess = 'DDLS031'.
  elseif genwa-local = 'A' and genwa-actkind = 'A'. mess = 'DDLS041'.
  else.                                             mess = 'DDLS030'.
   genwa-local   = 'N'.
   genwa-actkind = 'N'.
  endif.
  newob> prid1.
  if act_s = 'X' and genwa-actkind = 'N'.
    obj_header> prid1 mess genwa-name syst-uname syst-datum syst-uzeit.
  else.
    obj_header> prid1 mess genwa-name '' '' ''.
  endif.
  "Send message if object is contained in a cycle
  if genwa-prelevel = 'X' or genwa-prelevel = 'Y'.
    smi1> prid 'N' 'DDLS067' genwa-name.
    if genwa-prelevel = 'X'.
      "DDLS072: Activation of DDL source & is performed with associations (strong)
      smi1> prid 'N' 'DDLS072' genwa-name.
    elseif genwa-prelevel = 'Y'.
      "DDLS073: Activation of DDL source & is performed without associations (weak)
      smi1> prid 'N' 'DDLS073' genwa-name.
    endif.
  endif.
*-Set control-ledge for Struc. Object activation------------------------*
  "We can only set this control information here which is independent
  "of object CLASS (Struc. Object or Struc. Object APPEND). All missing information
  "is set later during activation
  act_mode = ctrl-actmode = acwa-actmode1.
  if act_mode = 1 or act_mode = 3.     "Online/Direct (1) or conversion (3)
    ctrl-checkmode      = 1.  "Ddls checkmode
    ctrl-view_checkmode = 9.  "Currently checkmode Online and transport are the same
    ctrl-entity_chkmode = 3.  "and transport are the same
    ctrl-extend_chkmode = 1.  "Additional checks during Extend handling
    if genwa-local = 'N'.     "In act. of active version do not check Ddl Dependency
      ctrl-check_ddldep = ABAP_true.   "as nothing has changed
      ctrl-write_ddldep = ABAP_true.   "For 'A' Dependencies should not be changed
    endif.
    ctrl-dbadapt = ''.        "<KH> 20150327 Always
    if act_mode = 3.          "No DB adaptation of views during dependent activation
      ctrl-dbadapt = ''.      "of conversion
    endif.
  elseif act_mode = 2. "Transport/Put
    ctrl-checkmode      = 2.  "Ddls checkmode
    ctrl-view_checkmode = 29. "<KH> 20140703 Checkmode Online and transport now differs
      "because we also call the checks in Ddl activation of transport or upgrade. And
      "in this situation we have to accept inconsistencies in dependent objects (views, tables)
      "as they may be in transport also. Normally check is not called during activation of other
      "object.
    ctrl-entity_chkmode = 4.  "Transport mode
    ctrl-extend_chkmode = 2.  "Additional checks during Extend handling, Transport mode
    if genwa-prelevel = 'Y'.
      ctrl-entity_chkmode = 3.  "Transport mode - second activation if circle occured
    endif.
    ctrl-ddl_parser_inact = ABAP_true.  "Ddl Parser takes inactive Nametabs first
    ctrl-write_ddldep     = ABAP_true.  "Write Ddl Dependency after import
    if genwa-local = 'N'.
      ctrl-check_ddldep   = ABAP_true.  "In act. of active version do not check Ddl Dependency
    endif.                              "as nothing has changed
  endif.
  if genwa-prelevel = 'X'. "First activation of cycle object with less restricive parser call,
    ctrl-parse_strictness = 1.   "e.g. non active target for association is missed
  endif.
  ctrl-ntabstate = 'A'.
  if inactive = ABAP_true.
    ctrl-ntabstate = 'N'.
  endif.
  ctrl-putstate  = 'A'. "We directly write to A because Ddl Source is not type-
                        "relevant
  ctrl-persist_ddlcont = ABAP_true. "Write N-version of generated Objects (View, Entity)
  if genwa-local = 'A' or genwa-actkind = 'A'.
    ctrl-putstate  = 'R'.
    clear ctrl-persist_ddlcont.     "If active Ddl Sources are activated we do not write
  endif.                            "because we only put with R. If we activate writing A-
    "version we save the generated objects, e.g. to check them in case of error
  ctrl-prelevel = genwa-prelevel. "For weaker checks

*-Activate Ddl Source -------------------------------------------------*
  access_ref = cl_dd_ddl_handler_factory=>create_internal( ).
  try.
    ddlname = genwa-name.
    access_ref->activate(
      exporting
        name            = ddlname
        actctrl         = ctrl
        prid            = prid1
      importing
        videptab        = videptab
        rc              = rc ).
  catch cx_dd_ddl_exception into ex.
    rc = 8.
    err_info = ex->get_error( ).
    genwa-arbgb    = err_info-arbgb.
    genwa-msgnr    = err_info-msgnr.
    genwa-severity = err_info-severity.
    genwa-par1     = err_info-var1.
    genwa-par2     = err_info-var2.
    genwa-par3     = err_info-var3.
    genwa-par4     = err_info-var4.
  endtry.
  genwa-rc = rc.
  if lines( videptab ) > 0.
    genwa-actflag = 'U'.
  endif.
  read table deptab transporting no fields
                    with key type    = genwa-type
                             name    = genwa-name
                             deptype = ddls.
  if syst-subrc = 0.  "At least one dependent Ddl Source -> trigger
    genwa-actflag = 'U'.   "dependent activation by actflag setting
  endif.
  if genwa-rc >= 4 and genwa-severity = ''. "Not set from err_info yet
    perform set_message using genwa.
  endif.

*-Write end-message to log---------------------------------------------*
  perform stdo_set_newob(radbtout) using prid1.
  if genwa-rc > 0 or ( genwa-rc = 0 and acwa-act = 'O' ).
    if genwa-rc = 0. obj_tail> prid1 'DDLS025' genwa-name. endif.
    if genwa-rc = 4. obj_tail> prid1 'DDLS026' genwa-name. endif.
    if genwa-rc > 4. obj_tail> prid1 'DDLS027' genwa-name. endif.
  endif.

  if excommit = 'X'. commit work. endif.

endform.                    " MT_DDLS_ACT

*----------------------------------------------------------------------*
* Form INSERT_TBATG_MC
*----------------------------------------------------------------------*
* Writes matchcode-actions to table TBATG
*----------------------------------------------------------------------*
* --> MCOMCID: Internal table with MCOB- or MCID-actions to insert
*              into table TBATG
* <-- RC     : 0: Insert into TBATG o.k, 8: Insert not successful
* --> PRID   : Id for log-writer
*----------------------------------------------------------------------*
form insert_tbatg_mc tables mcomcid type mcomcidtb
                     using  rc      like syst-subrc
                            prid    like syst-tabix.

  data: objname like tbatg-tabname,
        tbatg_wa like tbatg.

  loop at mcomcid where object <> 'MCIA'.
    if mcomcid-object = 'TABL'.
      objname = mcomcid-objname1.
    else.
      concatenate mcomcid-mconame mcomcid-mcid into objname.
    endif.
    tbatg_wa-tabname  = objname.
    tbatg_wa-indname  = mcomcid-objname2.
    tbatg_wa-object   = mcomcid-object.
    tbatg_wa-fct      = mcomcid-func.
    tbatg_wa-execmode = 'I'.
    perform insert_tbatg using tbatg_wa rc prid.
  endloop.

endform.                    " INSERT_TBATG_MC

*----------------------------------------------------------------------*
* Form INSERT_TBATG
*----------------------------------------------------------------------*
* Writes Index-and enqueue-actions into table TBATG
*----------------------------------------------------------------------*
* --> TBATG_WA: Contains necessary action for index just activated
* <-- RC      : 0: Insert o.k, 8: Insert not successful
* --> PRID    : Id for log-writer
*----------------------------------------------------------------------*
form insert_tbatg using tbatg_wa like tbatg
                        rc       like syst-subrc
                        prid     like syst-tabix.

  clear rc.

  call function 'DD_MASS_REQUESTS_INSERT'
       exporting
            objname           = tbatg_wa-tabname
            idname            = tbatg_wa-indname
            objtype           = tbatg_wa-object
            fct               = tbatg_wa-fct
            success_mess_type = ' '
            modeflag          = tbatg_wa-execmode
       importing
            subrc             = rc
       exceptions
            unexpected_error  = 1
            others            = 2.
  if syst-subrc <> 0. rc = 8. endif.
  if rc <= 4.
    if tbatg_wa-object = indx.
      smi3> prid 'N' 'DO521' tbatg_wa-fct tbatg_wa-tabname
                             tbatg_wa-indname.
    elseif tbatg_wa-object = enqu.
      smi2> prid 'N' 'DO553' tbatg_wa-fct tbatg_wa-tabname.
    elseif tbatg_wa-object = mcob or tbatg_wa-object = mcid.
      if tbatg_wa-tabname(5) <> ' '.   "MC-Id
        smi2> prid 'N' 'DO576' tbatg_wa-fct tbatg_wa-tabname.
      else.                            "MC-Object
        smi2> prid 'N' 'DO574' tbatg_wa-fct tbatg_wa-tabname.
      endif.
    elseif tbatg_wa-object = tabl.
      smi2> prid 'N' 'DO581' tbatg_wa-fct tbatg_wa-tabname.
    elseif tbatg_wa-object = view.
      smi2> prid 'N' 'DO583' tbatg_wa-fct tbatg_wa-tabname.
    endif.
  else.
    if tbatg_wa-object = indx.
      smi3> prid 'N' 'DO522' tbatg_wa-fct tbatg_wa-tabname
                             tbatg_wa-indname.
    elseif tbatg_wa-object = enqu.
      smi2> prid 'N' 'DO554' tbatg_wa-fct tbatg_wa-tabname.
    elseif tbatg_wa-object = mcob or tbatg_wa-object = mcid.
      if tbatg_wa-tabname(5) <> ' '.   "MC-Id
        smi2> prid 'N' 'DO577' tbatg_wa-fct tbatg_wa-tabname.
      else.                            "MC-Object
        smi2> prid 'N' 'DO575' tbatg_wa-fct tbatg_wa-tabname.
      endif.
    elseif tbatg_wa-object = tabl.
      smi2> prid 'N' 'DO582' tbatg_wa-fct tbatg_wa-tabname.
    elseif tbatg_wa-object = view.
      smi2> prid 'N' 'DO584' tbatg_wa-fct tbatg_wa-tabname.
    endif.
    rc = 8.
  endif.

endform.                    " INSERT_TBATG

*----------------------------------------------------------------------*
* Form MT_WR_ACTFLAG
*----------------------------------------------------------------------*
* Writes ACTFLAG computed out of activation-result to dependent objects
*----------------------------------------------------------------------*
* --> GENTAB   : Table with objects to activate
* --> DEPTAB   : Table with dependencies
* --> INVDEPTAB: Inverse-Table with dependencies
* --> TBVI     : Table containing relevant dependent views
* --> TBSH     : Table containing relevant dependent Searchhelps
* --> MCOMCID  : Table containing relevant dependent MC-Ids
* --> VITB     : Table containing relevant dependent Tables of views
* --> GENWA    : Current object to activate
* --> DEPACT   : 'X': Force-activation of dependent objects
* --> TEST_ON  : 'X': Testmode is switched on, pass actflag to dependent
*                objects
* --> FOLLOW   : 'X': Write trace-log, '': No trace-log
* --> PRID     : Id for log-writer
* --> PRIDF    : Id for log-writer, tracelog
*----------------------------------------------------------------------*
*form mt_wr_actflag tables gentab      type gentb
*                          deptab      type deptb
*                          invdeptab   type invdeptb
*                          tbvi        type tbvitb
*                          tbsh        type tbshtb
*                          tben        type tbentb
*                          mcomcid     type mcomcidtb
*                          vitb        type vitbtb
*                   using  genwa       like dcgentb
*                          actual_line like syst-tabix
*                          depact      like ddmass-frcact
*                          test_on     like ddmass-test_on
*                          follow      like ddmass-follow
*                          prid        like syst-tabix
*                          pridf       like syst-tabix.
*
*  data: depwa    like dcdeptb,
*        invdepwa like dcinvdeptb,
*        line_nr  like syst-tabix,
*        begin    like syst-tabix, realdep(1),
*        additional(30).
*
*  check genwa-rc <= 4.
*
**-In case of direct activation check if dependent objects exist--------*
*  if depact = ' '.
*    if genwa-actflag = ' '.
*       follow> follow f genwa-type genwa-name 'Keine Änderung'(128)
*               'Abhängige werden nicht behandelt'(129).
*       exit.
*     endif.
*   endif.
*
*
**-Move information about necessary actions to dependent objects--------*
*  read table deptab into depwa with key type = genwa-type
*                                        name = genwa-name
*                                        indx = genwa-indx binary search.
*  if syst-subrc = 0.
*    begin = syst-tabix.
*  else.
*    describe table deptab lines begin.
*    begin = begin + 1.
*  endif.
*
*  loop at deptab into depwa from begin.
*    if depwa-type = genwa-type and depwa-name = genwa-name and
*       depwa-indx = genwa-indx.
*      if depwa-depkind = 'A' or depwa-depkind = 'I'.
*        realdep = 'X'.
*        if depact = 'X'.
*          depwa-action = 'UJB'. modify deptab from depwa.
*          concatenate depwa-type depwa-name depwa-indx depwa-action
*                      into additional separated by ' '.
*          follow> follow f genwa-type genwa-name
*                  'Änderung weitergeben'(121) additional.
*          read table invdeptab into invdepwa
*                               with key deptype = depwa-deptype
*                                        depname = depwa-depname
*                                        depindx = depwa-depindx
*                                        type    = genwa-type
*                                        name    = genwa-name
*                                        indx    = genwa-indx
*                                        depkind = depwa-depkind
*                                        binary search.
*          line_nr = syst-tabix.
*          invdepwa-action = 'UJB'.
*          modify invdeptab from invdepwa index line_nr.
*          continue.
*        endif.
*
*        if genwa-actflag cn depwa-action.
*          if genwa-type = 'TABL' and depwa-deptype = 'VIEW'.
*            read table tbvi with key tabname  = genwa-name
*                                     viewname = depwa-depname
*                                     binary search.
*            if syst-subrc <> 0. continue. endif.
*          endif.
*          if genwa-type = 'TABL' and depwa-deptype = 'SHLP'.
*            read table tbsh with key name    = genwa-name
*                                     namedep = depwa-depname
*                                     binary search.
*            if syst-subrc <> 0. continue. endif.
*          endif.
*          if genwa-type = 'TABL' and depwa-deptype = 'ENQU'.
*            read table tben with key name    = genwa-name
*                                     namedep = depwa-depname
*                                     binary search.
*            if syst-subrc <> 0. continue. endif.
*          endif.
*          if genwa-type = 'MCOB' and depwa-deptype = 'MCID'.
*            read table mcomcid with key mconame = genwa-name
*                                        mcid    = depwa-depname
*                                        binary search.
*            if syst-subrc <> 0. continue. endif.
*          endif.
*          if genwa-type = 'VIEW' and ( depwa-deptype = 'TABL' or
*            depwa-deptype = 'SHLP' or depwa-deptype = 'TTYP' ).
*            read table vitb with key name    = genwa-name
*                                     namedep = depwa-depname
*                                     binary search.
*            if syst-subrc <> 0. continue. endif.
*          endif.
*          perform ut_set_action using depwa-action genwa-actflag.
*          modify deptab from depwa.
*          concatenate depwa-type depwa-name depwa-indx depwa-action
*                      into additional separated by ' '.
*          follow> follow f genwa-type genwa-name
*                  'Änderung weitergeben'(121) additional.
*        endif.
*
*        if genwa-actflag cn depwa-action and test_on = 'X'.
**       If Test-mode make sure dependencies are marked for later
**       activation
*          perform ut_set_action using depwa-action genwa-actflag.
*          modify deptab from depwa.
*        endif.
*
*        read table invdeptab into invdepwa
*                                       with key deptype = depwa-deptype
*                                                depname = depwa-depname
*                                                depindx = depwa-depindx
*                                                type    = genwa-type
*                                                name    = genwa-name
*                                                indx    = genwa-indx
*                                                depkind = depwa-depkind
*                                                binary search.
*        line_nr = syst-tabix.
*        if syst-subrc = 0 and genwa-actflag cn invdepwa-action.
*          perform ut_set_action using invdepwa-action genwa-actflag.
*          modify invdeptab from invdepwa index line_nr.
*        endif.
*      endif.
*    else.
*      follow> follow f genwa-type genwa-name
*              'Keine relevanten Abhängigen'(125) ''.
*      exit.
*    endif.
*  endloop.
*
**-Write ACTFLAG for activated DD-objects without dependent objects-----*
*  if realdep = ' ' and test_on = ' '.    "No test-mode
*    perform set_actflag using genwa ' ' prid.
*    clear genwa-actflag.
*    modify gentab from genwa index actual_line.
*    concatenate depwa-type depwa-name depwa-indx depwa-action
*                into additional separated by ' '.
*    follow> follow f genwa-type genwa-name
*            'Änderung weitergeben'(121) additional.
*  endif.
*
*endform.                    " MT_WR_ACTFLAG

*----------------------------------------------------------------------*
* Form MT_WR_ACTFLAG_DEP
*----------------------------------------------------------------------*
* Writes ACTFLAG for all dependent views and searchhelps of dependent
* tables which have to be activated. Furthermore dependent tabletypes
* are passed to dependent objects.
*----------------------------------------------------------------------*
* --> GENTAB   : Table with objects to activate
* --> DEPTAB   : Table with dependencies
* --> INVDEPTAB: Inverse-Table with dependencies
* --> TBVI     : Table containing relevant dependent views
* --> TBSH     : Table containing relevant dependent Searchhelps
* --> MCOMCID  : Table containing relevant dependent MC-Ids
* --> VITB     : Table containing relevant dependent Tables of views
* --> GENWA    : Current object to activate
* --> ACTION   : Contains what is to do for dependent object
* --> TEST_ON  : 'X': Testmode is switched on, pass actflag to dependent
*                objects
* --> FOLLOW   : 'X': Write tracelog
* --> PRID     : Id for log-writer
* --> PRIDF    : ID for log-writer, tracelog
*----------------------------------------------------------------------*
*form mt_wr_actflag_dep tables gentab      type gentb
*                              deptab      type deptb
*                              invdeptab   type invdeptb
*                              tbvi        type tbvitb
*                              tbsh        type tbshtb
*                              tben        type tbentb
*                              mcomcid     type mcomcidtb
*                              vitb        type vitbtb
*                       using  genwa       like dcgentb
*                              actual_line like syst-tabix
*                              depact      like ddmass-frcact
*                              action      like dcdeptb-action
*                              test_on     like ddmass-test_on
*                              follow      like ddmass-follow
*                              prid        like syst-tabix
*                              pridf       like syst-tabix.
*
*  data: depwa      like dcdeptb,
*        invdepwa   like dcinvdeptb,
*        line_nr    like syst-tabix,
*        action_tmp like dcdeptb-action,
*        begin      like syst-tabix,
*        additional(30).
*
*  check genwa-rc <= 4.
*
*  if not ( action co genwa-actflag ).
*    perform ut_set_action using action genwa-actflag.
*  endif.
*
*  read table deptab into depwa with key type = genwa-type
*                                        name = genwa-name
*                                        indx = genwa-indx
*                                        binary search.
*  if syst-subrc = 0.
*    begin = syst-tabix.
*  else.
*    exit.
*  endif.
*
**-Move information about necessary actions to dependent objects--------*
*  loop at deptab into depwa from begin.
*    if depwa-type = genwa-type and depwa-name = genwa-name and
*       depwa-indx = genwa-indx.
*      if depwa-depkind = 'A' or depwa-depkind = 'I'.
*        if action co 'DABN ' and
*                        genwa-type = 'TABL' and depwa-deptype = 'TABL'
*                                   and depact = ' '.
*          perform ut_set_action using depwa-action action.
*          concatenate depwa-type depwa-name depwa-indx depwa-action
*                      into additional separated by ' '.
*          follow> follow f genwa-type genwa-name
*                 'Änderung weitergeben (A)'(124) additional.
*          continue.
*        endif.
*
*        if depwa-action na 'UV'.
*          if genwa-type = 'TABL' and depwa-deptype = 'VIEW'.
*            read table tbvi with key tabname  = genwa-name
*                                     viewname = depwa-depname
*                                     binary search.
*            if syst-subrc <> 0. continue. endif.
*            perform ut_set_action using depwa-action 'U'.
*            action = 'U'.
*          elseif genwa-type = 'TABL' and depwa-deptype = 'SHLP'.
*            read table tbsh with key name    = genwa-name
*                                     namedep = depwa-depname
*                                     binary search.
*            if syst-subrc <> 0. continue. endif.
*            perform ut_set_action using depwa-action 'U'.
*            action = 'U'.
*          elseif genwa-type = 'TABL' and depwa-deptype = 'ENQU'.
*            read table tben with key name    = genwa-name
*                                     namedep = depwa-depname
*                                     binary search.
*            if syst-subrc <> 0. continue. endif.
*            perform ut_set_action using depwa-action 'U'.
*            action = 'U'.
*          elseif genwa-type = 'TABL' and depwa-deptype = 'TABL'.
**         <KH> 19981207 Change done for Appends during repositiory
**         switch:
**         Situation: Append A is active. Appending table T1 does not
**         contain Append-line and all further dependent objects, e.g T2
**         do not either because they are newly imported from SAP-
**         standard. So deptab does not contain dependency A->T2 and
**         activation of A does not mark T2 for activation. But
**         activation of T1 does not either because for tables we utilize
**         the recursive storage (A->T2) of includes which in case of
**         repository switch does not exist. What means that in case of
**         repository switch we have to mark dependent tables of
**         appending tables for activation. This is done by the next
**         three lines.
**         <KH> 19990120 Same case if Append is new and the appending
**         table or structure and its dependent tables/structures are
**         already active. So set flag in upgrade (DEPACT = 'X') and non-
**         upgrade-case (DEPACT = ' ').
*            perform ut_set_action using depwa-action 'U'.
*            action = 'U'.
*          elseif genwa-type = 'VIEW' and ( depwa-deptype = 'TABL' or
*                 depwa-deptype = 'SHLP' or depwa-deptype = 'TTYP' ).
*            read table vitb with key name    = genwa-name
*                                     namedep = depwa-depname
*                                     binary search.
*            if syst-subrc <> 0. continue. endif.
*            perform ut_set_action using depwa-action 'V'.
*            action = 'V'.
*          else.
*            perform ut_set_action using depwa-action action.
*          endif.
*          modify deptab from depwa.
*          concatenate depwa-type depwa-name depwa-indx depwa-action
*                      into additional separated by ' '.
*          follow> follow f genwa-type genwa-name
*                 'Änderung weitergeben (A)'(124) additional.
*        endif.
*
*        if depwa-action na 'UV' and test_on = 'X'.
**       If Test-mode make sure dependencies are marked for later
**       activation
*          perform ut_set_action using depwa-action 'U'.
*          action = 'U'.
*          modify deptab from depwa.
*        endif.
*
*        read table invdeptab into invdepwa
*                                   with key deptype = depwa-deptype
*                                            depname = depwa-depname
*                                            depindx = depwa-depindx
*                                            type    = genwa-type
*                                            name    = genwa-name
*                                            indx    = genwa-indx
*                                            depkind = depwa-depkind
*                                            binary search.
*        line_nr = syst-tabix.
*        if syst-subrc = 0.
*          perform ut_set_action using invdepwa-action action.
*          modify invdeptab from invdepwa index line_nr.
*        endif.
*      endif.
*    else.
*      follow> follow f genwa-type genwa-name
*              'Keine relevanten Abhängigen (A)'(126) ''.
*      exit.
*    endif.
*  endloop.
*
*endform.                    " MT_WR_ACTFLAG_DEP

*----------------------------------------------------------------------*
* Form UT_SET_ACTION
*----------------------------------------------------------------------*
* Sets non existing actflags in action field of DEPTAB
*----------------------------------------------------------------------*
* <-- RES_ACTION: Action summary for dependent object
* --> GENACTION : Action found out for object currently activated
*----------------------------------------------------------------------*
form ut_set_action using resaction like dcdeptb-action
                         value(genaction) like dcgentb-actflag.

  data: ch(1), j type i, len type i,
        ac(10).

  ch = genaction(1). j = 1. ac = resaction.
  len = strlen( genaction ).
  while j <= len.
    if ac ca 'B' and ( ch = 'A' or ch = 'D' ).
      j = j + 1.
      continue.
    endif.
    if ac na ch.
      concatenate ac ch into ac.
    endif.
    shift genaction. ch = genaction(1).
    j = j + 1.
  endwhile.
  if ac ca 'D' and ac ca 'A'.
    replace 'D' with ' ' into ac.
    replace 'A' with ' ' into ac.
    condense ac no-gaps.
    if ac na 'B'.
      concatenate ac 'B' into ac.
    endif.
  endif.
  if ac ca 'N' and ( ac ca 'J' or ac ca 'V' or ac ca 'U' ).
    replace 'N' with ' ' into ac.
    condense ac no-gaps.
  endif.
  if ac ca 'V' and ac ca 'U'.
    replace 'V' with ' ' into ac.
    condense ac no-gaps.
  endif.
  clear resaction. resaction = ac.

endform.                    " UT_SET_ACTION

*----------------------------------------------------------------------*
* Form MT_ACT_CHK
*----------------------------------------------------------------------*
* Computes wether activation is necessary
*----------------------------------------------------------------------*
* --> NTCHGTAB: Contains objects for which only Nametab-timestamp has
*               to be updated or Nametab has to be generated
* --> GENWA   : Information about current object
* --> ACWA    : Controls actions
* --> DEPACT  : 'X': Dependent objects have to be activated
* --> ACTION  : Necessary action
*----------------------------------------------------------------------*
form mt_act_chk tables ntchgtab  type dcntchgtb
                using  genwa     like dcgentb
                       acwa      like ddmassac
                       depact    like ddmass-frcact
                       action    like dcgentb-actflag.

  data ntchgwa type dcntchg.

*-Check if activation is forced----------------------------------------*
* if depact = 'X'.
*   if action = ' ' and ( genwa-local = 'A' and genwa-actkind = 'A' ).
*     clear acwa-act.      clear acwa-wr_actflg.
*     clear acwa-act_chkn. clear acwa-act_chkm.
*     perform ut_set_action using action 'DA'.    "Set timestamps
*   endif.
*   exit.
* endif.

  if genwa-local = 'A' and genwa-actkind = 'A'.
*---Look wether no action was found out -> EXIT------------------------*
    if action = ' '.
      clear acwa-act.        clear acwa-wr_actflg.
      clear acwa-wr_actflgd.
      clear acwa-act_chkn.   clear acwa-act_chkm. exit.
    endif.
*---If only timestamp has to be written, save tablename----------------*
    if action co 'DABN '.
      clear acwa-act.      clear acwa-wr_actflg.
      clear acwa-act_chkn. clear acwa-act_chkm.
      ntchgwa-type = genwa-type.
      ntchgwa-name = genwa-name.
      ntchgwa-chg  = action.
      append ntchgwa to ntchgtab.
    endif.
  endif.

endform.                    " MT_ACT_CHK

*----------------------------------------------------------------------*
* Form MA_RES_ANALYSE
*----------------------------------------------------------------------*
* Computes the total result for every DD-object which has been in
* GENTAB
*----------------------------------------------------------------------*
* --> GENTAB: Contains all objects to activate
* --> DEPTAB: Contains all dependencies
* <-- OKTAB : Contains all successfully activated objects with
*             dependent tables
* --> TEST_ON: 'X': Test-mode active -> Do not execute DB-changes
* --> PERSIST: 'X', Handle GENTAb as database table
* --> PRID   : ID for log-writing
*----------------------------------------------------------------------*
form ma_res_analyse tables gentab  type gentb
                           deptab  type deptb
                           oktab   type oktb
                    using  test_on like ddmass-test_on
                           persist
                           prid    like syst-tabix.

  data: dbdcgenwa type dbdcgentb,
        depwa type dcdeptb,
        genwa type dcgentb,
        ok_wa type okwa,
        type1 like dcgentb-type, type2 like dcgentb-type,
        name1 like dcgentb-name, name2 like dcgentb-name,
        cnt   like syst-tabix,
        rc    like syst-subrc,
        rc_not_found type sysubrc,
        modify(1),
        nr like syst-tabix.
  field-symbols: <genwa> like dcgentb.
  data: dcddobj_wa type dcddobj,
        objs_not_found type sorted table of dcddobj
                       with unique key type name indx.

  refresh oktab.
  sort gentab by type name indx actkind. "for binary search

  "We pre-handle view tables which went wrong as dependent objects
  "but view is activated successfully. In this case we do not deliver error
  loop at gentab assigning <genwa> where type    = 'TABL'
                                   and   local   = 'A'
                                   and   actkind = 'A'
                                   and   rc      > 4.
    read table gentab into genwa with key type    = 'VIEW'  "Direct activation
                                          name    = <genwa>-name
                                          actkind = 'N'
                                          binary search.
    if syst-subrc = 0 and genwa-rc <= 4.
      <genwa>-rc = genwa-rc.
    endif.
  endloop.

  loop at deptab where ( ( type = doma or type = dtel or type = tabl or
                           type = view or type = shlp or type = ttyp or
                           type = stob or type = ddls )
                 and   ( depkind = 'I' or depkind = 'A' ) )
                 or ( type = sf01 or type = sfsw or type = sfbs or
                   type = sfbf or type = sf02 or type = enhd ).
    cnt = syst-tabix.
    if deptab-type = tabl or deptab-type = shlp or deptab-type = view or
       deptab-type = ttyp or deptab-type = dtel or deptab-type = stob or
       deptab-type = doma or  "Fix value Appends
       deptab-type = ddls.    "Ddls has dependent objects (STOB, VIEW, DDLS)
*   Tables and Searchhelps can exist as dependent and direct objects in
*   DEPTAB. From release 4.0C on Data-elements and Tabletypes can also
*   exist as dependent objects.
*   For dependent objects there no further analysis is necessary.
      if persist = 'X'.
        clear rc_not_found.
        read table objs_not_found into dcddobj_wa
                                  with key type    = deptab-type
                                           name    = deptab-name
                                           indx    = '' binary search.
        if syst-subrc = 0. " not found in GENTAB
          rc_not_found = 8.
        else.
          select * from dbdcgentb into dbdcgenwa
                                  where type    = deptab-type
                                  and   name    = deptab-name
                                  and   indx    = ''
                                  and   actkind = 'N'.
            move-corresponding dbdcgenwa to genwa.
          endselect.
          if syst-subrc <> 0.
            rc_not_found = syst-subrc.
            dcddobj_wa-type = deptab-type.
            dcddobj_wa-name = deptab-name.
            dcddobj_wa-indx = ''.
            insert dcddobj_wa into table objs_not_found.
          endif.
        endif.
      else.
        read table gentab into genwa
                          with key type    = deptab-type
                                   name    = deptab-name
                                   indx    = ''
                                   actkind = 'N' binary search.
        rc_not_found = syst-subrc.
      endif.
      if rc_not_found <> 0. continue. endif.
*     Check if object is switched off. In this case continue.
      if genwa-switchedof = 'X'. continue. endif.
    endif.
    if type1 <> deptab-type or name1 <> deptab-name.
      type2 = deptab-type. name2 = deptab-name.
      depwa = deptab. clear rc.
      refresh objs_not_found.
      while depwa-type = type2 and depwa-name = name2.
        if persist = 'X'.
          select * from dbdcgentb into dbdcgenwa
                                  where type    = deptab-deptype
                                  and   name    = deptab-depname.
            move-corresponding dbdcgenwa to genwa.
            if genwa-rc > rc. rc = genwa-rc. endif.
          endselect.
        else.
          read table gentab into genwa
                            with key type    = deptab-deptype
                                     name    = deptab-depname
                                     binary search.
        endif.
        if syst-subrc = 0.
          if genwa-rc > rc. rc = genwa-rc. endif.
        endif.
        cnt = cnt + 1.
        read table deptab index cnt.
        if syst-subrc = 0.
          type2 = deptab-type. name2 = deptab-name.
        else.
          clear type2. clear name2.
        endif.
      endwhile.
      if persist = 'X'.
        select * from dbdcgentb into dbdcgenwa
                                where type    = depwa-type
                                and   name    = depwa-name
                                and   indx    = ''
                                and   actkind = 'N'.
          move-corresponding dbdcgenwa to genwa.
        endselect.
      else.
        read table gentab into genwa
                          with key type    = depwa-type
                                   name    = depwa-name
                                   indx    = ''
                                   actkind = 'N' binary search.
      endif.
      if syst-subrc = 0.
        nr = syst-tabix.
        if genwa-rc < rc.
          "<KH> 20030507 If at least one dependent object has errors
          "change returncode to 8. Object is not o.k.
          "<KH> 20030903 If at least one dependent object has errors
          "change returncode to 6. Object is not o.k. Total RC ist set to 8 later.
          "Objects are moved to oktab if this rc <= 4
          if rc = 8. genwa-rc = 6.  endif.
*          if rc = 8. genwa-rc = 8.  endif.
          "If Object has dependent object which was set to RC = 6 before because its
          "own dependent objects are also not correct we also set to RC = 6
          if rc = 6. genwa-rc = 6.  endif.
          if rc = 4. genwa-rc = rc. endif.
          modify = 'X'.
        endif.
        if genwa-rc <= 4.
          if test_on = ' ' and genwa-switchedof <> 'X'.  "No test-mode
            perform set_actflag using genwa ' ' prid.
          endif.
        endif.
*       <KH> 20011030 Delete object from DWINACTIV and set OK_GEN
*                     in transport even if object is still only
*                     partly active
*       "<KH> 20030903 Objects with rc=6 are not moved to oktab
        if genwa-rc <= 4 and genwa-switchdep = ''.
          clear genwa-actflag. modify = 'X'.
          move-corresponding genwa to ok_wa.
          append ok_wa to oktab.
        endif.
        if modify = 'X'.
          if persist = 'X'.
            move-corresponding genwa to dbdcgenwa.
            update dbdcgentb from dbdcgenwa.
            if ( genwa-type = sf01 or genwa-type = sfsw or
                 genwa-type = sfbs or genwa-type = sfbf or
                 genwa-type = sf02 or genwa-type = enhd ).
               append genwa to gentab.
            endif.
          else.
            modify gentab from genwa index nr.
          endif.
        endif.
      endif.
    endif.
    type1 = depwa-type. name1 = depwa-name.
  endloop.

  "If views and stobs (currently not possible) are contained in transport
  "which are generated via Ddl sources they are not activated but Ddl source
  "only. Thus we overtake returncode of Ddl source
  data(lin_nr) = lines( gentab ).
  loop at deptab into depwa where type = ddls
                            and   deptype = view or deptype = stob.
    read table gentab into genwa with key type  = ddls        "#EC CI_SORTED
                                          name  = depwa-name  "DDl was activated?
                                          binary search.
    if syst-subrc = 0.
      read table gentab into data(genwa1) with key type = depwa-deptype "#EC CI_SORTED
                                                   name = depwa-depname
                                                   binary search.
      if syst-subrc = 0.
        nr = syst-tabix.
        name1 = depwa-depname.
        type1 = depwa-deptype.
        while name1 = genwa1-name and type1 = genwa1-type.
          if genwa1-rc < genwa-rc.
            modify gentab from genwa1 index nr.
          endif.
          name1 = genwa1-name.
          type1 = genwa1-type.
          nr = nr + 1.
          if nr >= lin_nr.
            exit.
          endif.
          read table gentab into genwa1 index nr.
        endwhile.
      endif.
    endif.
  endloop.

endform.                    " MA_RES_ANALYSE

*----------------------------------------------------------------------*
* Form SET_ACTFLAG
*----------------------------------------------------------------------*
* Updates the field ACTFLAG in DD01L, DD02L or DD04L according to
* object type
*----------------------------------------------------------------------*
* --> GENTAB : Contains all objects to activate
* --> ACTFLAG: Value to set
* --> PRID   : Id for log-writer
*----------------------------------------------------------------------*
form set_actflag using genwa   like dcgentb
                       actflag like dcgentb-actflag
                       prid    like syst-tabix.

  data: rc     type syst-subrc,
        ddl_wa type ddldependency.

  if genwa-type = doma.
    update dd01l set:  actflag  = actflag reservedom  = actflag
                 where domname  = genwa-name
                 and   as4local = 'A'.
    if rc < syst-subrc. rc = syst-subrc. endif.
  elseif genwa-type = dtel.
    update dd04l set:  actflag  = actflag reservedte = actflag
                 where rollname = genwa-name
                 and   as4local = 'A'.
  elseif genwa-type = tabl.
    update dd02l set   actflag  = actflag
                 where tabname  = genwa-name
                 and   as4local = 'A'.
    rc = syst-subrc.
  elseif genwa-type = view.
    update dd25l set   actflag  = actflag
                 where viewname = genwa-name
                 and   as4local = 'A'.
    rc = syst-subrc.
  elseif genwa-type = shlp.
    update dd30l set   actflag  = actflag
                 where shlpname = genwa-name
                 and   as4local = 'A'.
    rc = syst-subrc.
  elseif genwa-type = ttyp.
    update dd40l set   actflag  = actflag
                 where typename = genwa-name
                 and   as4local = 'A'.
    rc = syst-subrc.
  elseif genwa-type = stob.
    "Try inactive version first because if it exists it is newer
    "and would have just been activated
    update dd02b set   actflag   = actflag
                 where strucobjn = genwa-name
                 and   as4local  = 'I'.
    if syst-subrc <> 0.
      update dd02b set   actflag   = actflag
                   where strucobjn = genwa-name
                   and   as4local  = 'A'.
    endif.
    rc = syst-subrc.
  elseif genwa-type = ddls. "Currently no ACTFLAG for Ddls
    select * from ddldependency into ddl_wa where ddlname = genwa-name.
      if ddl_wa-objecttype = 'STOB'.
        update dd02b set   actflag   = actflag
                     where strucobjn = ddl_wa-objectname
                     and   as4local  = 'I'.
        if syst-subrc <> 0.
          update dd02b set   actflag   = actflag
                       where strucobjn = genwa-name
                       and   as4local  = 'A'.
        endif.
        if rc > syst-subrc.
          rc = syst-subrc.
        endif.
      elseif ddl_wa-objecttype = 'VIEW'.
        update dd25l set   actflag  = actflag
                     where viewname = ddl_wa-objectname
                     and   as4local = 'A'.
        if rc > syst-subrc.
          rc = syst-subrc.
        endif.
      endif.
    endselect.
  endif.
  if rc <> 0.
    smi2> prid 'W' 'DO515' genwa-type genwa-name.
  endif.

endform.                    " SET_ACTFLAG

*----------------------------------------------------------------------*
* Form MA_TOTALRES_COMPUTE
*----------------------------------------------------------------------*
* Computes total result of activation and writes end log
*----------------------------------------------------------------------*
* --> GENTAB     : Contains all objects to activate
* --> DEPTAB     : Contains all dependent objects
* --> DELTAB     : Contains all objects which could not be deleted or
*                  which are deleted but still have references
* --> DELDEPTAB  : Contains exsiting references of objects to delete
* --> NTDELTAB   : Contains objects with Nametab to delete
* --> SWITCHES_DELTAB: The table with switches to delete
* <-- RC         : Total result
* --> SYSTEM_TYPE: Contains 'SAP' for an SAP-internal system
* --> DELALL     : Controls which objects have to be deleted:
*                  ' ': Active versions will be deleted.
*                  'X': All existing versions will be deleted.
* --> DELNOREF   : Controls wether in second run of deletion the still
*                  referenced objects shall be deleted:
*                  ' ': Delete objects even if they are still used.
*                  'X': Delete only objects which are no longer used.
* --> PERSIST    : 'X', get GENTAB information from database
* --> PRID       : Id for log-writer
*----------------------------------------------------------------------*
form ma_totalres_compute tables gentab      type gentb
                                deptab      type deptb
                                deltab      type deltb
                                deldeptab   type deldeptb
                                ntdeltab    type ntdeltb
                                switches_deltab type deltb
                         using  rc          like syst-subrc
                                system_type like syst-sysid
                                delall      like ddmass-delall
                                delnoref    like ddmass-delall
                                persist
                                prid        like syst-tabix.

  data type_exp(50).
  data len type i.
  field-symbols <f> type c.
  data: phase_txt(80), found.
  data  text(100).
  data: rc_del like syst-subrc, sev_del(1), del_err(1).
  field-symbols: <delwa> type dcdeltb.
  data: version(24), tabkind(16).
  data  ref_nr like syst-tabix.
  data  rc_deldep like syst-subrc.
  data  rc_ntdel like syst-subrc.
  data: dbdcgenwa type dbdcgentb,
        dbdcgentab type dbdcgentb occurs 0,
        genwa type dcgentb.

  clear rc.

  if persist = 'X'.
    select * from dbdcgentb into table dbdcgentab
             where rc >= 4
             and   prelevel <> 'X'.
    loop at dbdcgentab into dbdcgenwa.
      move-corresponding dbdcgenwa to genwa.
      append genwa to gentab.
    endloop.
    sort gentab by actkind descending rc descending type name indx.
  endif.

  sort switches_deltab by pgmid objtyp objname.

* Analyse GENTAB-results and compute returncode
* loop at gentab where actkind = 'N' and rc >= 4 and prelevel <> 'X'.
  loop at gentab where rc >= 4 and prelevel <> 'X' and switchedof = ' '.
    if found = ' '.
      phase_txt = text-014. phase_header> prid phase_txt.
      newob> prid. smi0> prid 'N' 'DO578'. newob> prid.
      found = 'X'.
    endif.
    perform ma_type_expand using gentab-type type_exp.
    len = strlen( type_exp ).
    assign type_exp(len) to <f>.
    if gentab-rc >= 8.
*    GENTAB-RC can even be 16 as returncode of Matchcode-Id-activation
*    with errors
     gentab-rc = 8.
      if gentab-indx = ' '.
        if gentab-type = tabt.
          smi2> prid 'E' 'DO536' <f> gentab-name.
          perform info_mess_write using gentab prid.
        elseif sfw cs gentab-type.
          perform ma_switches_write_result tables switches_deltab
            using gentab <f> prid.
        else.
          smi2> prid 'E' 'DO519' <f> gentab-name.
          perform info_mess_write using gentab prid.
        endif.
      else.
        smi3> prid 'E' 'DO516' <f> gentab-name gentab-indx.
        perform info_mess_write using gentab prid.
      endif.
    elseif gentab-rc = 6.
      if sfw cs gentab-type.
        perform ma_switches_write_result tables switches_deltab
          using gentab <f> prid.
      else.
        smi2> prid 'W' 'DO517' <f> gentab-name.
        perform info_mess_write using gentab prid.
      endif.
* PW: dont change gentab-rc, only set rc on end
*      gentab-rc = 8. modify gentab.
    elseif gentab-rc = 4.
      if gentab-indx = ' '.
        if gentab-type = tabt.
          smi2> prid 'W' 'DO537' <f> gentab-name.
          perform info_mess_write using gentab prid.
        else.
          if gentab-rc = 4 and
             gentab-arbgb = ' ' and gentab-msgnr = ' '.
            smi2> prid 'W' 'DO549' <f> gentab-name.
            perform info_mess_write using gentab prid.
          else.
            if sfw cs gentab-type.
              perform ma_switches_write_result tables switches_deltab
                using gentab <f> prid.
            else.
              smi2> prid 'W' 'DO520' <f> gentab-name.
              perform info_mess_write using gentab prid.
            endif.
          endif.
        endif.
      else.
        smi3> prid 'W' 'DO518' <f> gentab-name gentab-indx.
        perform info_mess_write using gentab prid.
      endif.
    endif.
    if gentab-rc > rc. rc = gentab-rc. endif.
  endloop.
* PW: because gentab-rc = 6 is NOT modified to 8 set now rc correctly
* (Because dependant must have 8, if causing object has 6, this should
*  never happen.)
  if rc = 6.
    rc = 8.
  endif.

  if persist = 'X'.
    free gentab.
  endif.

* Analyse DELTAB and if it is still necessary compute returncode
* Set version for messages
  version = text-035.   "active
  if delall = 'X'.
    version = text-036. "revised and active
  endif.
* Set maximal returncode
  if system_type = 'SAP'. rc_del = 8. sev_del = 'E'.
  else.
    if     delnoref = ' '. rc_del = 4. sev_del = 'W'.
    elseif delnoref = 'X'. rc_del = 8. sev_del = 'E'.
    endif.
  endif.
* Analyse DELTAB
* Do not look at objects witk GENDELOBJ = 'X' because those are
* generated to DELTAB during deletion.
  loop at deltab assigning <delwa> where gendelobj = ' '.
    if found = ' '.
*     Only write header line if there are still objects (deleted or
*     not deleted) existing outer references
      loop at deldeptab where deplocal <> 'D'.
        exit.
      endloop.
      if syst-subrc = 0.
        phase_txt = text-014. phase_header> prid phase_txt.
        newob> prid. smi0> prid 'N' 'DO578'. newob> prid.
        found = 'X'.
      endif.
    endif.
    perform ma_type_expand using <delwa>-objtyp type_exp.
    len = strlen( type_exp ).
    assign type_exp(len) to <f>.
    if <delwa>-errflag = 'X'.
      del_err = 'X'.
      if <delwa>-objtyp = indx.
        smi4> prid sev_del 'DO588' <f> <delwa>-objname <delwa>-indxname
              version.
      else.
        smi3> prid sev_del 'DO586' <f> <delwa>-objname version.
      endif.
      read table deldeptab with key reftyp  = <delwa>-objtyp
                                    refname = <delwa>-objname
                                    binary search.
      if syst-subrc = 0. ref_nr = syst-tabix. endif.
    elseif <delwa>-errflag = 'D'.
*     Look wether references with deplocal <> 'D' exist. References with
*     deplocal = 'D' are only those within the deleting set of objects
*     <KH> 20040319: No message in case of deplocal = 'A' because these are Append-
*                    dependencies
      loop at deldeptab where reftyp   = <delwa>-objtyp
                        and   refname  = <delwa>-objname
                        and   deplocal <> 'D'
                        and   kind     <> 'A'.  "Appends
        ref_nr = syst-tabix. exit.
      endloop.
      if syst-subrc = 0.  "Outer references still exist
        del_err = 'X'.
        if <delwa>-objtyp = indx.
          smi4> prid sev_del 'DO589' <f> <delwa>-objname
                <delwa>-indxname version.
        else.
          smi3> prid sev_del 'DO587' <f> <delwa>-objname version.
        endif.
      endif.              "Outer references still exist
    endif.
    if del_err = 'X'.
*     Write down which references exist
      loop at deldeptab from ref_nr.
        if deldeptab-reftyp   = <delwa>-objtyp  and
           deldeptab-refname  = <delwa>-objname and
           deldeptab-deplocal <> 'D'.
          perform ma_type_expand using deldeptab-deptyp type_exp.
          len = strlen( type_exp ).
          assign type_exp(len) to <f>.
          if     deldeptab-kind = 'U'.
            if <delwa>-objtyp = sqlt.
              smi2> prid 'N' 'DO593' deldeptab-depname <delwa>-objname.
            else.
              smi3> prid 'N' 'DO592' <delwa>-objname <f>
                    deldeptab-depname.
            endif.
          elseif deldeptab-kind = 'I'.
            smi2> prid 'N' 'DO590' <delwa>-objname deldeptab-depname.
          elseif deldeptab-kind = 'M'.
            smi3> prid 'N' 'DO592' <delwa>-objname <f>
                  deldeptab-depname.
          elseif deldeptab-kind = 'S'.
            smi3> prid 'N' 'DO592' <delwa>-objname <f>
                  deldeptab-depname.
          else.
            if deldeptab-kind = 'R'. tabkind = text-037. endif.
            if deldeptab-kind = 'F'. tabkind = text-038. endif.
            if deldeptab-kind = 'W'. tabkind = text-039. endif.
            if deldeptab-kind = 'T'. tabkind = text-040. endif.
            if deldeptab-kind = 'L'. tabkind = text-040. endif.
            if deldeptab-kind = 'A'. tabkind = text-041. endif.
            smi4> prid 'N' 'DO591' <delwa>-objname tabkind <f>
                  deldeptab-depname.
          endif.
        else.
          exit.  "No more dependencies found
        endif.
      endloop.
    endif.
  endloop.

* Compute returncode of activation of tables/structures dependent of
* Appends, CI- or SI-Includes
  perform ma_app_ci_dep_res_analyse tables gentab deptab deldeptab
                                    using  persist prid rc_deldep.

* Analyze objects with Nametabs to delete
  sort ntdeltab by type name.
  loop at ntdeltab.
    if found = ' '.
*     Only write header line if there were nametabactivities
      phase_txt = text-014. phase_header> prid phase_txt.
      newob> prid. smi0> prid 'N' 'DO929'. newob> prid.
      found = 'X'.
    endif.
    if     ntdeltab-type = tabl. tabkind = text-044.
    elseif ntdeltab-type = dtel. tabkind = text-016.
    elseif ntdeltab-type = ttyp. tabkind = text-034.
    endif.
    if ntdeltab-ntabaction = 'D' and ntdeltab-rc > 8.
      rc_ntdel = 8.
      if syst-tabix = 1.
        newob> prid. smi0> prid 'N' 'DO619'. newob> prid.
      endif.
*     DO618: &-Nametab & was not successfully deleted
      smi2> prid 'E' 'DO618' tabkind ntdeltab-name.
    endif.
    if ntdeltab-ntabaction = 'V'.
*     DO528: & & Sign of existence of buffered views reset
      smi2> prid 'N' 'DO528' tabkind ntdeltab-name.
    endif.
  endloop.

* Set total returncode
  if rc <= 8.  "No activation error
    if del_err = 'X' and rc < rc_del. rc = rc_del. endif.
  endif.
  if rc < rc_deldep. rc = rc_deldep. endif.
  if rc < rc_ntdel.  rc = rc_ntdel.  endif.

  if found = 'X'. phase_tail> prid phase_txt ' '. endif.

endform.                    " MA_TOTALRES_COMPUTE

*----------------------------------------------------------------------*
* Form MA_TYPE_EXPAND
*----------------------------------------------------------------------*
* Translates transport objects to logical names
*----------------------------------------------------------------------*
* --> TYPE_TR : Type of transport object
* <-- TYPE_EXP: Logical Type name
*----------------------------------------------------------------------*
form ma_type_expand using type_tr like dcgentb-type
                          type_exp.

  clear type_exp.

  if type_tr = doma.
    type_exp = 'Domäne'(015).
  elseif type_tr = dtel.
    type_exp = 'Datenelement'(016).
  elseif type_tr = tabl.
    type_exp = 'Tabelle'(017).
  elseif type_tr = tabt or type_tr = sqtt or type_tr = viet.
    type_exp = 'Technische Einstellungen'(018).
  elseif type_tr = indx.
    type_exp = 'Index'(019).
  elseif type_tr = xinx.
    type_exp = 'Index'(061).
  elseif type_tr = sqlt.
    type_exp = 'Tabellenpool/cluster'(020).
  elseif type_tr = view.
    type_exp = 'View'(021).
  elseif type_tr = mcob or type_tr = maco.
    type_exp = 'Matchcode-Objekt'(022).
  elseif type_tr = mcid.
    type_exp = 'Matchcode-Id'(023).
  elseif type_tr = shlp.
    type_exp = 'Suchhilfe'(024).
  elseif type_tr = enqu.
    type_exp = 'Sperrobjekt'(027).
  elseif type_tr = ttyp.
    type_exp = 'Tabellentyp'(034).
  elseif type_tr = sqsc.
    type_exp = 'DB Prozedur Proxy'(158).
  elseif type_tr = stob.
    type_exp = 'Strukt. Objekt'(159).
  elseif type_tr = ddls.
    type_exp = 'Ddl Source'(160).
  elseif type_tr = sfsw.
    type_exp = 'Schalter + Zuordnung von Objekten zum Schalter'(062).
  elseif type_tr = sfbf.
    type_exp = 'Business Function + Zuordnung'(063).
  elseif type_tr = sfbs.
    type_exp = 'Business Function Set + Zuordnung'(064).
  elseif type_tr = sf01.
    type_exp = 'Statustabellen Switchframework mandantenabhängig'(065).
  elseif type_tr = sf02.
    type_exp = 'Statustabellen Switchframework mandantenunabhängig'(066).
  elseif type_tr = enhd.
    type_exp = type_tr.
  else.
    type_exp = type_tr.
  endif.

endform.                    " MA_TYPE_EXPAND

*----------------------------------------------------------------------*
* Form  MA_WRITE_NTCHGTAB
*----------------------------------------------------------------------*
* Updates Nametab-timestamps for objects collected in table NTCHGTAB
*----------------------------------------------------------------------*
* --> GENTAB  : Table contains names which have to be activated
* --> NTCHGTAB: Objects for which Nametab-timestamps have to be updated
*               or Nametab has to be newly generated out of DD-sources
* --> INACTIVE: 'X': Nametab has to be written inactive, ' ': active
* --> TEST_ON : 'X': Test-mode active -> do not change Nametabs
* --> PRID    : Id for log-writer
*----------------------------------------------------------------------*
form ma_write_ntchgtab tables gentab   type gentb
                              ntchgtab type dcntchgtb
                       using  inactive like ddmass-inactive
                              test_on  like ddmass-test_on
                              prid     like syst-tabix.

  data  phase_txt(80).

  check test_on = ' '.

  read table ntchgtab index 1.
  if syst-subrc = 0. phase_txt = text-025. phase_header> prid phase_txt.
  else.              exit.    "--> EXIT form
  endif.

  delete adjacent duplicates from ntchgtab comparing all fields.

  loop at ntchgtab.
    if ( ntchgtab-type <> tabl and ntchgtab-type <> dtel and
         ntchgtab-type <> ttyp ).
      continue.
    endif.
    if ntchgtab-chg <> ' ' and ntchgtab-chg co 'DAB '.
      perform ntab_set_tmst tables gentab using ntchgtab inactive prid.
    elseif ntchgtab-chg ca 'N'.
      if ntchgtab-type = tabl.
        perform ntab_gen_tabl tables gentab
                              using  ntchgtab inactive prid.
      elseif ntchgtab-type = ttyp.
        perform ntab_gen_ttyp tables gentab
                              using  ntchgtab inactive prid.
      elseif ntchgtab-type = dtel.
        perform ntab_gen_dtel tables gentab
                              using  ntchgtab  inactive prid.
      endif.
    endif.
  endloop.
  if syst-subrc = 0. phase_tail> prid phase_txt ''. endif.

endform.                    " MA_WRITE_NTCHGTAB

*----------------------------------------------------------------------*
* Form  NTAB_SET_TMST
*----------------------------------------------------------------------*
* Handles update of Nametab-header
*----------------------------------------------------------------------*
* --> GENTAB  : Table contains names which have to be activated
* --> NTCHGWA : Object for which Nametab-timestamps has to be updated
* --> INACTIVE: 'X': Nametab has to be written inactive, ' ': active
* --> PRID    : Id for log-writer
*----------------------------------------------------------------------*
form ntab_set_tmst tables gentab   type gentb
                   using  ntchgwa  type dcntchg
                          inactive like ddmass-inactive
                          prid     like syst-tabix.

  data: x030l_wa   like x030l.
  data: x030l_wa_a like x030l, x031l_tab_a like x031l occurs 0.
  data  read_state like ddxtt-modeflag.
  data  rc like syst-subrc.
  data: upd_active_header(1), upd_inactive_header(1),
        ins_inactive_ntab(1).
  data  got_state like ddxtt-modeflag.
  data: modeflag       type ddxtt-modeflag,
        modeflag_dummy type ddxtt-modeflag.
  data  write_state like ddxtt-modeflag.
  field-symbols: <genwa> like dcgentb.

  perform ntab_header_get tables gentab
                          using  ntchgwa-type ntchgwa-name x030l_wa
                                 'M' read_state modeflag rc prid.
  if rc > 0. exit. endif.    "Nothing more to do --> EXIT
* At least one version of Nametab exists
  if inactive = ' '.         "Active Nametab has to be written
    if read_state = 'A'. x030l_wa_a = x030l_wa. clear x030l_wa. endif.
    if read_state = 'N'.
*     Newest Nametab is inactive one -> Read active Nametab-header
      upd_inactive_header = 'X'.
      perform ntab_header_get tables gentab
                              using  ntchgwa-type ntchgwa-name
                                     x030l_wa_a 'A' read_state
                                     modeflag_dummy rc prid.
    endif.
    if read_state = 'A' and rc = 0. upd_active_header = 'X'. endif.
  elseif inactive = 'X'.     "Inactive Nametab has to be written
    if read_state = 'N'. upd_inactive_header = 'X'. endif.
    if read_state = 'A'.
*     Newest Nametab is active one -> Read it and write as inactive
      call function 'DD_GET_NAMETAB'
           exporting
                status     = 'A'
                tabname    = ntchgwa-name
                get_all    = 'X'
           importing
                r_status   = got_state
                x030l_wa   = x030l_wa_a
           tables
                x031l_tab  = x031l_tab_a
           exceptions
                not_found  = 1
                no_fields  = 2
                others     = 3.
      if syst-subrc <> 0.
        rc = syst-subrc.
        smi2> prid 'W' 'DO529' ntchgwa-type ntchgwa-name.
        read table gentab assigning <genwa>
                          with key type = ntchgwa-type
                                   name = ntchgwa-name binary search.
        if syst-subrc = 0.
          <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
        else.
          smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
        endif.
      else.
        ins_inactive_ntab = 'X'.
      endif.
    endif.
  endif.
  if ntchgwa-chg ca 'DB'.
    clear x030l_wa-dystamp. clear x030l_wa_a-dystamp.
  endif.
  if ntchgwa-chg ca 'AB'.
    clear x030l_wa-abstamp. clear x030l_wa_a-abstamp.
  endif.
  perform ntab_write tables gentab x031l_tab_a
                     using x030l_wa x030l_wa_a ntchgwa-type ntchgwa-name
                           modeflag upd_active_header upd_inactive_header
                           ins_inactive_ntab rc prid.

endform.                    " NTAB_SET_TMST

*----------------------------------------------------------------------*
* Form  NTAB_GEN_TABL
*----------------------------------------------------------------------*
* Generates Nametab out of DD-Sources
*----------------------------------------------------------------------*
* --> GENTAB  : Table contains names which have to be activated
* --> NTCHGWA : Object for which Nametab-timestamps has to be updated
* --> INACTIVE: 'X': Nametab has to be written inactive, ' ': active
* --> PRID    : Id for log-writer
*----------------------------------------------------------------------*
form ntab_gen_tabl tables gentab   type gentb
                   using  ntchgwa  type dcntchg
                          inactive like ddmass-inactive
                          prid     like syst-tabix.

  data: x030l_wa like x030l, x031l_tab like x031l occurs 0.
  data  read_state like ddxtt-modeflag.
  data: rc like syst-subrc, rc_a like syst-subrc, rc_n like syst-subrc.
  data: write_active(1), write_inactive(1).
  data: get_state like dctablget, got_state like dctablget.
  data: dd02v_wa_n like dd02v, dd02v_wa_a like dd02v,
        dd09l_wa_n like dd09l, dd09l_wa_a like dd09l.
  data: dd03p_tab_n like dd03p occurs 0,
        dd03p_tab_a like dd03p occurs 0,
        dd05m_tab_n like dd05m occurs 0,
        dd05m_tab_a like dd05m occurs 0,
        dd08v_tab_n like dd08v occurs 0,
        dd08v_tab_a like dd08v occurs 0,
        dd12v_tab_n like dd12v occurs 0,
        dd12v_tab_a like dd12v occurs 0,
        dd17v_tab_n like dd17v occurs 0,
        dd17v_tab_a like dd17v occurs 0,
        dd35v_tab_n like dd35v occurs 0,
        dd35v_tab_a like dd35v occurs 0,
        dd36m_tab_n like dd36m occurs 0,
        dd36m_tab_a like dd36m occurs 0.
  data: uuid like sysuuid-x.
  data: abstamp like x030l-abstamp, dystamp like x030l-dystamp.
  field-symbols: <genwa> like dcgentb.

  if inactive = ' '.         "Active Nametab has to be written
    write_active = 'X'.
  elseif inactive = 'X'.     "Inactive Nametab has to be written
    write_inactive = 'X'.
  endif.
  perform ntab_chk_ex using ntchgwa-name x030l_wa 'A' read_state rc.
  abstamp = x030l_wa-abstamp. dystamp = x030l_wa-dystamp.
  uuid = x030l_wa-uuid.

* Generate Nametab from DD-sources
* Read active DD-sources
  get_state-tabl = 'A'.
  call function 'DD_TABL_GET'
       exporting
            get_state      = get_state
            tabl_name      = ntchgwa-name
            add_typeinfo   = 'D'
       importing
            dd02v_wa_a     = dd02v_wa_a
            dd02v_wa_n     = dd02v_wa_n
            dd09l_wa_a     = dd09l_wa_a
            dd09l_wa_n     = dd09l_wa_n
            got_state      = got_state
       tables
            dd03p_tab_a    = dd03p_tab_a
            dd03p_tab_n    = dd03p_tab_n
            dd05m_tab_a    = dd05m_tab_a
            dd05m_tab_n    = dd05m_tab_n
            dd08v_tab_a    = dd08v_tab_a
            dd08v_tab_n    = dd08v_tab_n
            dd12v_tab_a    = dd12v_tab_a
            dd12v_tab_n    = dd12v_tab_n
            dd17v_tab_a    = dd17v_tab_a
            dd17v_tab_n    = dd17v_tab_n
            dd35v_tab_a    = dd35v_tab_a
            dd35v_tab_n    = dd35v_tab_n
            dd36m_tab_a    = dd36m_tab_a
            dd36m_tab_n    = dd36m_tab_n
       exceptions
            access_failure = 1
            others         = 2.
  if syst-subrc <> 0 or got_state = ' '.
    smi1> prid 'W' 'DO538' ntchgwa-name.
    read table gentab assigning <genwa>
                             with key type = ntchgwa-type
                                      name = ntchgwa-name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
    endif.
    exit.
  endif.
* Prepare DD-sources for Nametab-generation
  perform fields_prepare(rattbutl)
                         tables dd03p_tab_a dd35v_tab_a dd08v_tab_a
                         using  dd02v_wa_a.
* Generate Nametab
* New expand is not necessary because the handling in question is
* only done for tables dependent of domains or data-elements
  call function 'DD_NTAB_GEN'
       exporting
            dd02v_wa       = dd02v_wa_a
            dd09l_wa       = dd09l_wa_a
       importing
            x030l_wa       = x030l_wa
       tables
            dd03p_tab      = dd03p_tab_a
            dd08v_tab      = dd08v_tab_a
            x031l_tab      = x031l_tab
       exceptions
            wrong_datatype = 1
            wrong_tabtype  = 2
            others         = 3.
  if syst-subrc <> 0.
    smi1> prid 'W' 'DO539' ntchgwa-name.
    read table gentab assigning <genwa>
                      with key type = ntchgwa-type
                               name = ntchgwa-name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
    endif.
    exit.
  endif.
* Write Timestamp
  x030l_wa-abstamp = abstamp. x030l_wa-dystamp = dystamp.
  if ntchgwa-chg ca 'DB'. clear x030l_wa-dystamp. endif.
  if ntchgwa-chg ca 'AB'. clear x030l_wa-abstamp. endif.
  concatenate syst-datum syst-uzeit into x030l_wa-crstamp.
* Set UUID
  x030l_wa-uuid = uuid.
  if x030l_wa-abstamp = ' ' or rc <> 0. "Old Nametab-header not read
    call function 'SYSTEM_UUID_CREATE'
         importing
             uuid    = uuid
         exceptions
              others  = 0.
    if syst-subrc = 0. x030l_wa-uuid = uuid. endif.
  endif.
* Write Nametab
  if write_active = 'X'.
    call function 'DD_PUT_NAMETAB'
         exporting
              status      = 'A'
              x030l_wa    = x030l_wa
         importing
              r_subrc     = rc
         tables
              x031l_tab   = x031l_tab
         exceptions
              write_error = 1
              others      = 2.
    if syst-subrc <> 0 or rc <> 0. rc_a = 8. endif.
  endif.
  if write_inactive = 'X'.
    call function 'DD_PUT_NAMETAB'
         exporting
              status      = 'N'
              x030l_wa    = x030l_wa
         importing
              r_subrc     = rc
         tables
              x031l_tab   = x031l_tab
         exceptions
              write_error = 1
              others      = 2.
    if syst-subrc <> 0 or rc <> 0. rc_n = 8. endif.
  endif.
  if rc_a > 0 or rc_n > 0.
    if rc_a > 0.
      smi1> prid 'W' 'DO540' ntchgwa-name.
    else.
      smi1> prid 'W' 'DO541' ntchgwa-name.
    endif.
    read table gentab assigning <genwa>
                      with key type = ntchgwa-type
                               name = ntchgwa-name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
    endif.
  else.
    smi1> prid 'I' 'DO542' ntchgwa-name.
  endif.

endform.                    " NTAB_GEN

*----------------------------------------------------------------------*
* Form  NTAB_GEN_TTYP
*----------------------------------------------------------------------*
* Generates Nametab for tabletype out of DD-Sources
*----------------------------------------------------------------------*
* --> GENTAB  : Table contains names which have to be activated
* --> NTCHGWA : Object for which Nametab has to be generated
* --> INACTIVE: 'X': Nametab has to be written inactive, ' ': active
* --> PRID    : Id for log-writer
*----------------------------------------------------------------------*
form ntab_gen_ttyp tables gentab   type gentb
                   using  ntchgwa  type dcntchg
                          inactive like ddmass-inactive
                          prid     like syst-tabix.

  data: x030l_wa like x030l, x031l_tab like x031l occurs 0,
        abstamp like x030l-abstamp, dystamp like x030l-dystamp,
        read_state like ddxtt-modeflag,
        rc like syst-subrc.
  data: get_state   like dcttypget, got_state like dcttypget,
        dd40v_wa_a  like dd40v,
        dd40v_wa_n  like dd40v,
        dd40v_tab_a like dd40v occurs 0 with header line,
        dd41v_tab_a like dd41v occurs 0,
        dd41v_tab_n like dd41v occurs 0,
        dd42v_tab_a like dd42v occurs 0,
        dd42v_tab_n like dd42v occurs 0,
        dd43v_tab_a like dd43v occurs 0,
        dd43v_tab_n like dd43v occurs 0.
  data: dd40v_tab_row like dd40v occurs 0,
        dd41v_tab_row like dd41v occurs 0 with header line,
        dd41v_wa_row  like dd41v,
        dd02v_tab_row like dd02v occurs 0,
        dd03p_tab_row like dd03p occurs 0.
  data  put_state like ddxtt-modeflag.
  data: uuid  like sysuuid-x,
        rc_nt like syst-subrc.
  field-symbols: <genwa> like dcgentb.

  data: classes_tab    like vseoclass  occurs 0 with header line,
        interfaces_tab like vseointerf occurs 0 with header line,
        range_ctyp_dd41v_tab like dd41v occurs 0 with header line.

  perform ntab_chk_ex using ntchgwa-name x030l_wa 'A' read_state rc_nt.
  abstamp = x030l_wa-abstamp. dystamp = x030l_wa-dystamp.
  uuid = x030l_wa-uuid.

*-Read DD-sources for tabletypes
  get_state-ttyp = 'A'.
  call function 'DD_TTYP_GET'
       exporting
            get_state     = get_state
            prid          = prid
            ttyp_name     = ntchgwa-name
       importing
            dd40v_wa_a    = dd40v_wa_a
            dd40v_wa_n    = dd40v_wa_n
            got_state     = got_state
       tables
            dd42v_tab_a   = dd42v_tab_a
            dd42v_tab_n   = dd42v_tab_n
            dd43v_tab_a   = dd43v_tab_a
            dd43v_tab_n   = dd43v_tab_n
       exceptions
            illegal_value = 1
            op_failure    = 2
            others        = 3.
  if syst-subrc <> 0 or got_state = ' '.
    smi1> prid 'W' 'DO579' ntchgwa-name.
    read table gentab assigning <genwa>
                             with key type = ntchgwa-type
                                      name = ntchgwa-name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
    endif.
    exit.
  endif.

* Read DD-sources of rowtype
  move-corresponding dd40v_wa_a to dd40v_tab_a. append dd40v_tab_a.
* uh, 28.12.1998: fehlende Parameter ergänzen
  " bisher:
  " PERFORM TTYP_ROWTYPE_DATA_GET(SAPLSDTTA) TABLES DD40V_TAB_A
  "  DD42V_TAB_A DD41V_TAB_ROW DD40V_TAB_ROW DD02V_TAB_ROW DD03P_TAB_ROW
  "  USING PRID RC.
  " neu:
  perform ttyp_rowtype_data_get(saplsdtta)
          tables dd40v_tab_a dd42v_tab_a
                 classes_tab interfaces_tab   " neu
                 range_ctyp_dd41v_tab         " neu
                 dd41v_tab_row dd40v_tab_row
                 dd02v_tab_row dd03p_tab_row
          using prid rc.

  if rc > 0.
    smi1> prid 'W' 'DO579' ntchgwa-name.
    read table gentab assigning <genwa>
                             with key type = ntchgwa-type
                                      name = ntchgwa-name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
    endif.
    exit.
  endif.

* Generate Nametab
  read table dd41v_tab_row index 1.
  move-corresponding dd41v_tab_row to dd41v_wa_row.
  call function 'DD_TABLTYPE_NTAB_GEN'
       exporting
            dd40v_wa        = dd40v_wa_a
            dd41v_wa        = dd41v_wa_row
       importing
            x030l_wa        = x030l_wa
       tables
            dd42v_tab       = dd42v_tab_a
            dd43v_tab       = dd43v_tab_a
            x031l_tab       = x031l_tab
       exceptions
            wrong_datatype  = 1
            wrong_fieldname = 2
            others          = 3.
  if syst-subrc <> 0.
    smi1> prid 'W' 'DO539' ntchgwa-name.
    read table gentab assigning <genwa>
                      with key type = ntchgwa-type
                               name = ntchgwa-name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
    endif.
    exit.
  endif.

* Write Timestamp
  x030l_wa-abstamp = abstamp. x030l_wa-dystamp = dystamp.
  if ntchgwa-chg ca 'DB'. clear x030l_wa-dystamp. endif.
  if ntchgwa-chg ca 'AB'. clear x030l_wa-abstamp. endif.
  concatenate syst-datum syst-uzeit into x030l_wa-crstamp.
* Set UUID
  x030l_wa-uuid = uuid.
  if x030l_wa-abstamp = ' ' or rc_nt <> 0. "Old Nametab-header not read
    call function 'SYSTEM_UUID_CREATE'
         importing
             uuid    = uuid
         exceptions
              others  = 0.
    if syst-subrc = 0. x030l_wa-uuid = uuid. endif.
  endif.
* Write Nametab
  if inactive = 'X'. put_state = 'N'. else. put_state = 'A'. endif.
  call function 'DD_PUT_NAMETAB'
       exporting
            status      = put_state
            x030l_wa    = x030l_wa
       importing
            r_subrc     = rc
       tables
            x031l_tab   = x031l_tab
       exceptions
            write_error = 1
            others      = 2.
  if rc > 0 or syst-subrc <> 0.
    if inactive = ' '.
      smi1> prid 'W' 'DO540' ntchgwa-name.
    else.
      smi1> prid 'W' 'DO541' ntchgwa-name.
    endif.
    read table gentab assigning <genwa>
                      with key type = ntchgwa-type
                               name = ntchgwa-name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
    endif.
  else.
    smi1> prid 'I' 'DO542' ntchgwa-name.
  endif.

endform.

*----------------------------------------------------------------------*
* Form  NTAB_GEN_DTEL
*----------------------------------------------------------------------*
* Generates Nametab for data-element out of DD-Sources
*----------------------------------------------------------------------*
* --> GENTAB  : Table contains names which have to be activated
* --> NTCHGWA : Object for which Nametab has to be generated
* --> INACTIVE: 'X': Nametab has to be written inactive, ' ': active
* --> PRID    : Id for log-writer
*----------------------------------------------------------------------*
form ntab_gen_dtel tables gentab   type gentb
                   using  ntchgwa  type dcntchg
                          inactive like ddmass-inactive
                          prid     like syst-tabix.

  data: x030l_wa like x030l,
        x031l_wa like x031l,
        x031l_tab like x031l occurs 0 with header line,
        abstamp like x030l-abstamp, dystamp like x030l-dystamp,
        read_state like ddxtt-modeflag,
        rc like syst-subrc.
  data: get_state   like dcdtelget, got_state like ddrefstruc-state,
        dd04l_wa_a  like dd04l,
        dd04l_wa_n  like dd04l,
        dd04t_tab_a like dd04t occurs 0,
        dd04t_tab_n like dd04t occurs 0,
        dd01l_wa    like dd01l,
        tpara_wa    like tpara.
  data  put_state like ddxtt-modeflag.
  data: uuid like sysuuid-x.
  field-symbols: <genwa> like dcgentb.

  perform ntab_chk_ex using ntchgwa-name x030l_wa 'A' read_state rc.
  abstamp = x030l_wa-abstamp. dystamp = x030l_wa-dystamp.
  uuid = x030l_wa-uuid.

*-Read DD-sources for data-elements
  get_state-dtget = 'A'. get_state-doma = 'X'. get_state-paramid = 'X'.
  call function 'DD_DTEL_GET'
       exporting
            get_state     = get_state
            roll_name     = ntchgwa-name
       importing
            got_state     = got_state
            dd04l_wa_a    = dd04l_wa_a
            dd04l_wa_n    = dd04l_wa_n
            dd01l_wa      = dd01l_wa
            tpara_wa      = tpara_wa
       tables
            dd04t_tab_a   = dd04t_tab_a
            dd04t_tab_n   = dd04t_tab_n
       exceptions
            illegal_value = 1
            others        = 2.
  if syst-subrc <> 0 or got_state = ' '.
    smi1> prid 'W' 'DO580' ntchgwa-name.
    read table gentab assigning <genwa>
                             with key type = ntchgwa-type
                                      name = ntchgwa-name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
    endif.
    exit.
  endif.

* Generate Nametab
  call function 'DD_TYPE_NTAB_GEN'
       exporting
            dd04l_wa       = dd04l_wa_a
            dd01l_wa       = dd01l_wa
       importing
            x030l_wa       = x030l_wa
            x031l_wa       = x031l_wa
       exceptions
            wrong_datatype = 1
            others         = 2.
  if syst-subrc <> 0.
    smi1> prid 'W' 'DO539' ntchgwa-name.
    read table gentab assigning <genwa>
                      with key type = ntchgwa-type
                               name = ntchgwa-name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
    endif.
    exit.
  endif.

* Write Timestamp
  x030l_wa-abstamp = abstamp. x030l_wa-dystamp = dystamp.
  if ntchgwa-chg ca 'DB'. clear x030l_wa-dystamp. endif.
  if ntchgwa-chg ca 'AB'. clear x030l_wa-abstamp. endif.
  concatenate syst-datum syst-uzeit into x030l_wa-crstamp.
* Set UUID
  x030l_wa-uuid = uuid.
  if x030l_wa-abstamp = ' ' or rc <> 0. "Old Nametab-header not read
    call function 'SYSTEM_UUID_CREATE'
         importing
             uuid    = uuid
         exceptions
              others  = 0.
    if syst-subrc = 0. x030l_wa-uuid = uuid. endif.
  endif.
* Write Nametab
  if inactive = 'X'. put_state = 'N'. else. put_state = 'A'. endif.
  move-corresponding x031l_wa to x031l_tab. append x031l_tab.
  call function 'DD_PUT_NAMETAB'
       exporting
            status      = put_state
            x030l_wa    = x030l_wa
       importing
            r_subrc     = rc
       tables
            x031l_tab   = x031l_tab
       exceptions
            write_error = 1
            others      = 2.
  if rc > 0 or syst-subrc <> 0.
    if inactive = ' '.
      smi1> prid 'W' 'DO540' ntchgwa-name.
    else.
      smi1> prid 'W' 'DO541' ntchgwa-name.
    endif.
    read table gentab assigning <genwa>
                      with key type = ntchgwa-type
                               name = ntchgwa-name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' ntchgwa-type ntchgwa-name.
    endif.
  else.
    smi1> prid 'I' 'DO542' ntchgwa-name.
  endif.

endform.

*----------------------------------------------------------------------*
* FORM NTAB_HEADER_GET
* Gets Nametab-header
*----------------------------------------------------------------------*
* <-> GENTAB    : Table with objects to activate
* --> TYPE      : Type of object to get Nametab for
* --> NAME      : Name of object to get Nametab for
* --> X030L_WA  : Nametab-header
* --> GET_STATE : Version of Nametab-header wanted
* <-- GOT_STATE : Version of Nametab-header
* <-- RC        : Returncode
* --> PRID      : Id for log-writer
*----------------------------------------------------------------------*
form ntab_header_get tables gentab     type gentb
                     using  type       like dcgentb-type
                            name       like dcgentb-name
                            x030l_wa   like x030l
                            get_state  like ddxtt-modeflag
                            got_state  like ddxtt-modeflag
                            modeflag   type ddxtt-modeflag
                            rc         like syst-subrc
                            prid       like syst-tabix.

  data x031l_tab like x031l occurs 0.
  field-symbols: <genwa> like dcgentb.

  clear got_state. clear rc. clear x030l_wa.
  clear modeflag.

  call function 'DD_GET_NAMETAB_HEADER'
       exporting
            status     = get_state
            tabname    = name
       importing
            r_modeflag = modeflag
            r_status   = got_state
            x030l_wa   = x030l_wa
       exceptions
            not_found  = 1
            others     = 2.
  if syst-subrc <> 0.
    rc = syst-subrc. clear got_state.
    smi2> prid 'W' 'DO527' type name.
    read table gentab assigning <genwa> with key
                                        type = type
                                        name = name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    endif.
  endif.

endform.                    " NTAB_HEADER_GET

*----------------------------------------------------------------------*
* FORM NTAB_CHK_EX
* Checks existence of Nametab-header
*----------------------------------------------------------------------*
* --> X030L_WA  : Nametab-header
* --> NAME      : Name of object
* --> GET_STATE : Version of Nametab-header wanted
* <-- GOT_STATE : Version of Nametab-header
* <-- RC        : Returncode
*----------------------------------------------------------------------*
form ntab_chk_ex using name      like dcgentb-name
                       x030l_wa  like x030l
                       get_state like ddxtt-modeflag
                       got_state like ddxtt-modeflag
                       rc        like syst-subrc.

  clear got_state. clear rc. clear x030l_wa.

  call function 'DD_GET_NAMETAB_HEADER'
       exporting
            status     = get_state
            tabname    = name
       importing
            r_status   = got_state
            x030l_wa   = x030l_wa
       exceptions
            not_found  = 1
            others     = 2.
  if syst-subrc <> 0. rc = syst-subrc. endif.

endform.                    " NTAB_CHK_EX

*----------------------------------------------------------------------*
* Form NTAB_WRITE
*----------------------------------------------------------------------*
* Writes Nametab-header in active or inactive version and Nametab-fields
* if necessary.
*----------------------------------------------------------------------*
* --> GENTAB             : Table with objectnames to activate
* --> X030L_WA           : Inactive Nametab-header
* --> X030L_WA_A         : Active Nametab-header
* --> X031L_TAB_A        : Active Nametab-fields
* --> TYPE               : Type of object to get Nametab for
* --> NAME               : Name of object to get Nametab for
* --> UPD_ACTIVE_HEADER  : ''/'X': Do not/Write active Nametab-header
* --> UPD_INACTIVE_HEADER: ''/'X': Do not/Write inactive Nametab-header
* --> INS_INACTIVE_NTAB  : ''/'X': Do not/Write inactive Nametab
* --> RC                 : Returncode
* --> PRID               : Id for log-writer
*----------------------------------------------------------------------*
form ntab_write tables gentab              type gentb
                       x031l_tab_a         structure x031l
                using  x030l_wa            structure x030l
                       x030l_wa_a          structure x030l
                       type                like dcgentb-type
                       name                like dcgentb-name
                       modeflag            type ddxtt-modeflag
                       upd_active_header   like ddrefstruc-flag
                       upd_inactive_header like ddrefstruc-flag
                       ins_inactive_ntab   like ddrefstruc-flag
                       rc                  like syst-subrc
                       prid                like syst-tabix.

  data  write_state like ddxtt-modeflag.
  data: rc_ah like syst-subrc, rc_nh like syst-subrc,
        rc_nn like syst-subrc.
  field-symbols: <genwa> like dcgentb.

  check ( upd_active_header = 'X' or upd_inactive_header = 'X' or
       ins_inactive_ntab = 'X' ).
  concatenate syst-datum syst-uzeit into x030l_wa-crstamp.
  concatenate syst-datum syst-uzeit into x030l_wa_a-crstamp.
  if upd_active_header = 'X' or upd_inactive_header = 'X'.
    if upd_active_header   = 'X'.
      write_state = 'A'. clear rc.
      call function 'DD_PUT_NAMETAB_HEADER'
           exporting
                status      = write_state
                x030l_wa    = x030l_wa_a
           importing
                r_subrc     = rc
           exceptions
                write_error = 1
                others      = 2.
      if syst-subrc <> 0 or rc <> 0. rc_ah = 8. endif.
    endif.
    if upd_inactive_header = 'X'.
      write_state = 'N'. clear rc.
      call function 'DD_PUT_NAMETAB_HEADER'
           exporting
                modeflag    = modeflag
                status      = write_state
                x030l_wa    = x030l_wa
           importing
                r_subrc     = rc
           exceptions
                write_error = 1
                others      = 2.
      if syst-subrc <> 0 or rc <> 0. rc_nh = 8. endif.
    endif.
  endif.
  if ins_inactive_ntab = 'X'.
    write_state = 'N'. clear rc.
    call function 'DD_PUT_NAMETAB'
         exporting
              status      = write_state
              x030l_wa    = x030l_wa_a
         importing
              r_subrc     = rc
         tables
              x031l_tab   = x031l_tab_a
         exceptions
              write_error = 1
              others      = 2.
    if syst-subrc <> 0 or rc <> 0. rc_nn = 8. endif.
  endif.

* Analyse write-statements
  if rc_ah > 0 or rc_nh > 0 or rc_nn > 0.
    rc = 8.
    if rc_nn > 0.
      smi2> prid 'w' 'DO531' type name.
    else.
      smi2> prid 'W' 'DO530' type name.
    endif.
    read table gentab assigning <genwa> with key type = type
                                             name = name binary search.
    if syst-subrc = 0.
      <genwa>-rc = 4. modify gentab from <genwa> index syst-tabix.
    else.
      smi2> prid 'W' 'DO528' type name.
    endif.
  elseif rc_ah = 0 and rc_nh = 0 and rc_nn = 0.
    smi2> prid 'I' 'DO532' type name.
  endif.

endform.                    " NTAB_WRITE

*----------------------------------------------------------------------*
* Form  MA_DEL_OBJ
*----------------------------------------------------------------------*
* Deletes objects in table DELTAB.
*----------------------------------------------------------------------*
* <-> DELTAB   : Tables with objects to delete
* --> GENTAB   : Tables with objects to activate
* <-> DELDEPTAB: Dependencies between objects to delete
* <-> DEPTAB   : Contains dependencies between objects
* <-- OKTAB    : Objects which are successfully deleted
* <-- NCONFTAB : Objects with name-conflits with objects to delete
* <-- BASETABS : Buffered views basetabs
* <-- DDLDEPTAB: DDLDEPENDENCY information for Ddl Sources to delete
* --> DDMODE   : Mode of activation (online, transport, rep-switch)
* --> INACTIVE : ' ': Write active, 'X': Write inactive Nametab
* --> DELALL   : Controls which objects have to be deleted:
*                ' ': Active versions will be deleted.
*                'X': All existing versions will be deleted.
* --> DELNOREF : Controls wether in second run of deletion the still
*                referenced objects shall be deleted:
*                ' ': Delete objects even if they are still used.
*                'X': Delete only objects which are no longer used.
* --> RUN      : '1': First execution of mass-deletion, '2': Second
* --> TEST_ON  : 'X': Testmode on -> no delete operations on DB
* --> PRID     : Id for log-writer
* <-- CNT_DEL  : Number of sucessfully deleted objects
*----------------------------------------------------------------------*
form ma_del_obj tables deltab    type deltb
                       gentab    type gentb
                       deldeptab type deldeptb
                       deptab    type deptb
                       oktab     type oktb
                       nconftab  type gentb
                       ntdeltab  type ntdeltb
                       basetabs  type gentb
                       ddldeptab type ty_t_ddldependency
                using  ddmode    like ddmass-ddmode
                       inactive  like ddmass-inactive
                       delall    like ddmass-delall
                       delnoref  like ddmass-delnoref
                       run       like ddrefstruc-bool
                       ntdel_cnt like syst-tabix
                       ntdel_err like syst-tabix
                       cnt_del   type sytabix
                       test_on   like ddmass-test_on
                       prid      like syst-tabix.

  types: begin of ty_s_ddlname,
           ddlname type ddlname,
         end of ty_s_ddlname.
* Different delete-modes:
* 'A': In inactive mode by using convertor
* 'I': In inactive mode by using distributor and dipgntab
* 'O': Direct deletion (DD-Sources, Nametab and Database-version)
  data  modus(1).
* Check references:
* 'X': Check references of objects to delete
* ' ': No check of references
  data  check(1).
* Contains in which cases object are deleted
* 'P': Delete only if object is no longer used by other objects
* 'A': Delete in every case
  data  del_option(1).
* Contains which object-versions are deleted
* 'A'   : Active version will be deleted
* '*'   : Active versions as well as T-, D-, S-versions are deleted
* Others: All existing versions are deleted
  data  del_state(1).
  data: protocol(1) value 'X',
        testdel(1) value ' '.
  data: genwa   like dcgentb,
        ok_wa   type okwa,
        depwa   like dcdeptb,
        nconfwa like dcgentb,
        ntdelwa like dcntdeltb.
  data  phase_txt(80).
  data  run_chr(1).
  data  ntab_objs(50) value
        'SQLT SQLD TABL TABD DTEL DTED TTYP TTYD VIEW VIED'.
* For deletion of MC-objects/Ids
  data: deltab_mc    type deltb,
        delwa_mc     type dcdeltb,
        deldeptab_mc type deldeptb,
        mc_del       type i.
  data: pos          like syst-tabix.
  data: delviews     type viewtb,
        delview_wa   type viewn_wa.
  data: ddlnames     type table of ty_s_ddlname.

  newob> prid.

  refresh oktab. clear oktab.

* Sets parameters for mass-deletion
  if ddmode = 'O' or ddmode = 'C' or ddmode = 'G'.
    modus = 'O'.
  elseif ddmode = 'T' or ddmode = 'A'.
    modus = 'I'.
  endif.

  check = 'X'.

  if     run = 1. del_option = 'P'.
  elseif run = 2. del_option = 'A'.
  endif.
  if delnoref = 'X'. del_option = 'P'. endif.

  del_state = 'A'.
  if delall = 'X'. del_state = '*'. endif.

* Prepare DELTAB for mass-deletion
  if run = 1.
*   Sort gentab for all following binary searches (only for run = 1)
    sort gentab by name indx type local actkind. "for binary search

*   Delete duplicates from deltab
    sort deltab by objtyp objname indxname.
    delete adjacent duplicates from deltab
                             comparing objtyp objname indxname.
*  For objects to delete and activate the activation-entry is stronger
    loop at deltab.
      if deltab-objtyp = view.
        delview_wa-name = deltab-objname.
        append delview_wa to delviews.
      endif.
      perform ma_del_handle_part_objects tables gentab oktab
        using deltab run.
      if not ( deltab-mainobjex is initial ).
        modify deltab.
      endif.
      read table gentab with key
                             name    = deltab-objname
                             indx    = deltab-indxname
                             type    = deltab-objtyp binary search.
      if syst-subrc <> 0.
*       Look if for INDX XINX or for XINX INDX with same name exist
*       If yes, ignore deletion
        data: indx_type like deltab-objtyp.
        if deltab-objtyp = xinx. indx_type = indx.
        elseif deltab-objtyp = indx. indx_type = xinx.
        else. continue.
        endif.
        read table gentab with key
                          name    = deltab-objname
                          indx    = deltab-indxname
                          type    = indx_type binary search.
      endif.
      pos = syst-tabix.
      if syst-subrc = 0.
        if gentab-pgmid = 'LANG'.
          ok_wa-name   = gentab-name.
          ok_wa-indx   = gentab-indx.
          ok_wa-pgmid  = gentab-pgmid.
          ok_wa-type   = gentab-type.
          ok_wa-tabix  = gentab-tabix.
          ok_wa-nodoku = gentab-nodoku.
          append ok_wa to oktab.
          delete gentab index pos.
          continue.
        endif.
*       Mark object to delete as correctly handled
        if deltab-gendelobj = ' '.    "Objects to delete which are
          ok_wa-name   = deltab-objname.    "directly given set to 'X'
          ok_wa-indx   = deltab-indxname.   "for MCs only in RADMASDL
          ok_wa-pgmid  = deltab-pgmid.
          ok_wa-type   = deltab-objtyp.
          ok_wa-tabix  = deltab-e071_index.
          ok_wa-nodoku = deltab-nodoku.
          append ok_wa to oktab.
        endif.
        delete deltab.
      endif.
    endloop.

*   Get tables of buffered views
    perform get_tabs_of_buffered_views using deltab[] delviews[]
      basetabs[].

*   Get Ddl dependencies for later check for STOB/VIEW activation
    refresh ddldeptab.
    if lines( deltab ) > 0.
      loop at deltab where objtyp = 'DDLS'.
        append deltab-objname to ddlnames.
      endloop.
      if lines( ddlnames ) > 0.
        select * from ddldependency into table ddldeptab
                                    for all entries in ddlnames
                                    where ddlname = ddlnames-ddlname.
      endif.
    endif.
  endif.  "if run = 1

*1. ERRFLAG in DELTAB has to be ' ' and if necessary delete names of
*   successfully deleted objects out of DELTAB
*2. Select MCOB/MCID to delete them with DEL_STATE = '*' (always N- and
*   A-version)
  loop at deltab.
    if deltab-errflag = 'D'.
      delete deltab.
      continue.
    else.
      clear deltab-errflag. modify deltab.
    endif.
    if deltab-objtyp = 'MCOB' or deltab-objtyp = 'MCOD' or
       deltab-objtyp = 'MACD' or deltab-objtyp = 'MCID' or
       deltab-objtyp = 'MCOF'.
      move-corresponding deltab to delwa_mc.
      append delwa_mc to deltab_mc. mc_del = mc_del + 1.
      delete deltab.
    endif.
  endloop.

* Exit if no entries left in deltab
  read table deltab index 1.
  if syst-subrc <> 0 and mc_del = 0. exit. endif. "-->

* In Testmode only write information about objects to delete
  if test_on = 'X'.
    if run = 1.
*      phase_txt = text-045.
*      phase_header> prid phase_txt.
      loop at deltab.
        smi3> prid 'I' 'DO623' deltab-objtyp deltab-objname
                               deltab-indxname.
      endloop.
*      phase_tail> prid phase_txt ' '.
      exit.   "--> Exit Form
    else.
      exit.   "--> Exit Form
    endif.
  endif.

* Delete Nametabs
  if run = 1.
    refresh ntdeltab. clear ntdeltab.
    clear ntdel_cnt. clear ntdel_err.
    perform ma_ntab_del_extract tables deltab ntdeltab using ntdel_cnt.
    perform ma_ntab_del tables ntdeltab using ntdel_err prid.
    loop at ntdeltab into ntdelwa where rc < 8.
      ok_wa-name   = ntdelwa-name.
      ok_wa-indx   = ntdelwa-indx.
      ok_wa-pgmid  = ntdelwa-pgmid.
      ok_wa-type   = ntdelwa-type.
      ok_wa-tabix  = ntdelwa-tabix.
      ok_wa-nodoku = ntdelwa-nodoku.
      append ok_wa to oktab.
    endloop.
  endif.

  read table deltab index 1.
  if syst-subrc <> 0.
    read table deltab_mc into delwa_mc index 1.
      if syst-subrc <> 0. exit. endif.
  endif.

* Get objects with possible name-conflicts. Later we need information
* about both, new and deleted object
  if run = 1.
    loop at deltab.
      read table gentab into genwa with key
                             name    = deltab-objname
                             indx    = deltab-indxname binary search.
      if syst-subrc = 0.   "Store in special table
        if ntab_objs cs deltab-objtyp and ntab_objs cs genwa-type.
          move-corresponding genwa to nconfwa.
          nconfwa-level = 1.
          nconfwa-actkind = 'N'.
          append nconfwa to nconftab.
          nconfwa-type = deltab-objtyp.
          nconfwa-name = deltab-objname.
          nconfwa-indx = deltab-indxname.
          nconfwa-delflag = 'D'.
          append nconfwa to nconftab.
        endif.
      endif.
    endloop.
  endif.

*  run_chr = run.
*  phase_txt = text-026.
*  replace '&' with run_chr into phase_txt.
*  phase_header> prid phase_txt.

* Mass-deletion
  perform dd_mass_delete(radmasdl) tables deltab deldeptab
    using modus check del_option del_state protocol prid testdel
          cnt_del.

* Mass deletion of MC-Objects and Ids (N- and A-version)
  read table deltab_mc into delwa_mc index 1.
  if syst-subrc = 0.
    perform dd_mass_delete(radmasdl) tables deltab_mc deldeptab_mc
      using modus check del_option '*' protocol prid testdel cnt_del.
    append lines of deltab_mc to deltab.
    append lines of deldeptab_mc to deldeptab.
  endif.

* Sign/Add tables/structures which used an append for activation
  if run = 1.
    sort deltab by objtyp objname indxname.
    loop at deldeptab where kind = 'A'.
*     <KH> 20041112 If appending object has to be deleted itself do not activate it
      read table deltab with key objtyp   = deldeptab-deptyp
                                 objname  = deldeptab-depname
                                 binary search.
      if syst-subrc = 0. continue. endif.

      read table gentab into genwa with key
                                        name    = deldeptab-depname
                                        indx    = ' '
                                        type    = deldeptab-deptyp
                                        local   = 'A'
                                        actkind = 'A' binary search.
      if syst-subrc = 0.
        concatenate 'U' genwa-actflag into genwa-actflag.
        modify gentab from genwa index syst-tabix.
      else.
        genwa-name    = deldeptab-depname.
        genwa-type    = deldeptab-deptyp.
        genwa-indx    = ' '.
        genwa-local   = 'A'.
        genwa-actkind = 'A'.
        genwa-actflag = 'U'.
        "Preserve the sort order, SY-TABIX set by the read ... binary search
        insert genwa into gentab index sy-tabix.
      endif.
      clear depwa.
      depwa-type    = deldeptab-reftyp.
      depwa-name    = deldeptab-refname.
      depwa-deptype = deldeptab-deptyp.
      depwa-depname = deldeptab-depname.
      depwa-local   = 'A'.
      depwa-depkind = 'A'.
      depwa-action  = 'U'.
      append depwa to deptab.
    endloop.
  endif.

* Move successfully deleted objects or objects which are deleted in
* spite of references
  loop at deltab.
    if deltab-errflag = 'D' or
       ( deltab-errflag = 'X' and del_option = 'A' ).
      if deltab-gendelobj = ' '.    "Objects to delete which are
        ok_wa-name   = deltab-objname.    "directly given
        ok_wa-indx   = deltab-indxname.
        ok_wa-pgmid  = deltab-pgmid.
        ok_wa-type   = deltab-objtyp.
        ok_wa-tabix  = deltab-e071_index.
        ok_wa-nodoku = deltab-nodoku.
        append ok_wa to oktab.
      endif.
      deltab-errflag = 'D'. modify deltab.
    endif.
  endloop.

  perform handle_tabs_of_buffered_views using deltab[] basetabs[] ntdeltab[]
    inactive prid.

*  phase_tail> prid phase_txt ' '.

endform.                    " MA_DEL_OBJ

*----------------------------------------------------------------------*
* Form  MA_DEL_HANDLE_PART_OBJECTS
*----------------------------------------------------------------------*
* Handles the case that a table is deleted but a part object as
* Technical Settings or index exists to activate. The part object will be
* set to o.k. and no activatiopn started.
*----------------------------------------------------------------------*
* --> DELWA    : Workarea with object to delete
* --> GENTAB   : Tables with objects to activate
* <-- OKTAB    : Objects which are fully activated and can therefore be
*                set to o.k in the input-medium
*----------------------------------------------------------------------*
form ma_del_handle_part_objects tables gentab    type gentb
                                       oktab     type oktb
                                using  delwa     type dcdeltb
                                       run       like ddrefstruc-bool.

  data: ok_wa type okwa.
  field-symbols: <genwa> type dcgentb.

  check run = 1.

* Analyses if main object tabl for part objects indx/xinx
* has to be activated as direct object
* MAINOBJEX will be used later during index-deletion (RADMASDL)
  if delwa-objtyp = indx or delwa-objtyp = xinx.
*   Get first object with same name from GENTAB (which is sorted by NAME ...).
*   Starting from that entry (SY-TABIX) a TABL entry with LOCAL = 'N' is searched.
    read table gentab transporting no fields
                      with key name = delwa-objname binary search.
    check sy-subrc = 0.
    loop at gentab assigning <genwa> from sy-tabix.
      if <genwa>-name <> delwa-objname.
*       Since GENTAB is sorted the loop can be left now
        exit.
      endif.
      if <genwa>-type = tabl and <genwa>-local = 'N'.
        delwa-mainobjex = 'T'.
        exit.
      endif.
    endloop.
  endif.

  check delwa-objtyp = tabl.  "Make sure table object is deleted

  read table gentab transporting no fields
                    with key name = delwa-objname binary search.
  check sy-subrc = 0.
  loop at gentab assigning <genwa> from sy-tabix.
    if <genwa>-name <> delwa-objname.
*     Since GENTAB is sorted the loop can be left now
      exit.
    endif.
    if ( <genwa>-type = indx or
         <genwa>-type = tabt or
         <genwa>-type = xinx ).
      ok_wa-name   = <genwa>-name.
      ok_wa-indx   = <genwa>-indx.
      ok_wa-pgmid  = <genwa>-pgmid.
      ok_wa-type   = <genwa>-type.
      ok_wa-tabix  = <genwa>-tabix.
      ok_wa-nodoku = <genwa>-nodoku.
      append ok_wa to oktab.
      delete gentab.
    endif.
  endloop.

endform.

*----------------------------------------------------------------------*
* Form  MA_DEL_OBJECTS_REVIEW
*----------------------------------------------------------------------*
* Handling of objects which have to be deleted and
* - currently could not
* - which are selected as dependent objects because their sources
*   could not yet been deleted, e.g. STOB
*----------------------------------------------------------------------*
* --> DELTAB   : Objects to delete
* --> DDLDEPTAB: DDLDEPENDENCY for Ddl sources to delete
* --> RUN      : Number of deletion run, first or second
* --> INACTIVE : 'X': inactive nametab is written, otherwise active
* <-> GENTAB   : Objects to activate including dependent objects
*----------------------------------------------------------------------*
form ma_del_objects_review tables deltab    type deltb
                                  gentab    type gentb
                                  ddldeptab type ty_t_ddldependency
                           using  run       like ddrefstruc-bool
                                  inactive  type ddinactive.

  data: genwa     like dcgentb,
        ddldepwa  type ddldependency,
        pos       type sytabix.

  check run = 1.

  sort gentab by name indx type local actkind.
*  <KH> 040528 Mark objects which could not be deleted in first step and
*  which are also added as dependent objects. These should not be
*  activated because its not necessary and: they can contain objects to
*  delete. For those DDTYPES-entry has already been deleted during
*  import what causes errors during activation of the using objects
*  (comptype = 'N' DT242 in RADTBCHK)
  loop at deltab where errflag = 'X'.
    read table gentab into genwa with key
                               name    = deltab-objname
                               indx    = deltab-indxname
                               type    = deltab-objtyp binary search.
    if syst-subrc = 0.
      genwa-delflag = 'X'.
      modify gentab from genwa index syst-tabix.
    endif.
  endloop.

* <KH> 20131111: Structured Object metadata is used as runtime object.
* Therefore - in inactive mode - it is  - deleted as late as possible during
* move ntabs phase. Thus, we may have computed dependent STOBs here
  check inactive = ABAP_true.
  sort deltab by objtyp objname.
  sort ddldeptab by objecttype objectname.
  loop at gentab into genwa where ( type    = 'STOB' or type = 'VIEW' )
                            and   local   = 'A'
                            and   actkind = 'A'.
    pos = syst-tabix.
    read table deltab with key objtyp  = genwa-type
                               objname = genwa-name
                               binary search.
    if syst-subrc = 0.
      genwa-delflag = 'X'.
      modify gentab from genwa index pos.
      continue.  "Next object
    else.
      read table ddldeptab into ddldepwa with key
                                         objecttype = genwa-type
                                         objectname = genwa-name
                                         binary search.
      if syst-subrc = 0.  "Object is generated from Ddl source
        read table deltab with key objtyp  = 'DDLS'
                                   objname = ddldepwa-ddlname.
        if syst-subrc = 0.
          genwa-delflag = 'X'.
          modify gentab from genwa index pos.
        endif.
      endif.
    endif.
  endloop.

endform.

*----------------------------------------------------------------------*
* Form  MA_DEL_OBJECTS_REVIEW_FROM_DB
*----------------------------------------------------------------------*
* Collects GENWA-entries for objects which are fully activated
*----------------------------------------------------------------------*
* <-- DELTAB : Objects to delete
* --> DDLDEPTAB: DDLDEPENDENCY for Ddl sources to delete
* --> RUN    : First or second run of deletion
* --> INACTIVE : 'X': inactive nametab is written, otherwise active
* --> PERSIST: 'X', execute form with database tables
*----------------------------------------------------------------------*
form ma_del_objects_review_from_db tables deltab    type deltb
                                          ddldeptab type ty_t_ddldependency
                                   using  run       like ddrefstruc-bool
                                          persist
                                          inactive  type ddinactive.

  data: dbdcgenwa  type dbdcgentb,
        dbdcgentab type dbdcgentb occurs 0,
        ddldepwa   type ddldependency,
        delwa      type dcdeltb,
        deltab_err type deltb.

  check run = 1.
  check persist = 'X'.

  loop at deltab where errflag = 'X'.
    move-corresponding deltab to delwa.
    append delwa to deltab_err.
  endloop.

  read table deltab_err into delwa index 1.
  if syst-subrc <> 0.
    exit.
  endif.

*  <KH> 040528 Mark objects which could not be deleted in first step and
*  which are also added as dependent objects. These should not be
*  activated because its not necessary and: they can contain objects to
*  delete. For those DDTYPES-entry has already been deleted during
*  import what causes errors during activation of the using objects
*  (comptype = 'N' DT242 in RADTBCHK)
  select * from dbdcgentb into table dbdcgentab
      for all entries in deltab_err
            where name    = deltab_err-objname
            and   indx    = deltab_err-indxname
            and   type    = deltab_err-objtyp
            and   local   = 'A'
            and   actkind = 'A'.
  if sy-dbcnt > 0.
    "sort dbdcgentab by name indx type local actkind.
    "delete adjacent duplicates from dbdcgentab.

    loop at dbdcgentab into dbdcgenwa.
      dbdcgenwa-delflag = 'X'.
      modify dbdcgentab from dbdcgenwa.
    endloop.

    update dbdcgentb from table dbdcgentab.

  endif.

* <KH> 20131111: Structured Object metadata is used as runtime object.
* Therefore - in inactive mode - it is  - deleted as late as possible during
* move ntabs phase. Thus, we may have computed dependent STOBs here
  check inactive = ABAP_true.
  sort deltab by objtyp objname.
  sort ddldeptab by objecttype objectname.
  select * from dbdcgentb into dbdcgenwa
                            where ( type  = 'STOB' or type = 'VIEW' )
                            and   local   = 'A'
                            and   actkind = 'A'.
    read table deltab with key objtyp  = dbdcgenwa-type
                               objname = dbdcgenwa-name
                               binary search.
    if syst-subrc = 0.
      dbdcgenwa-delflag = 'X'.
      update dbdcgentb from dbdcgenwa.
      continue.  "Next object
    else.
      read table ddldeptab into ddldepwa with key
                                         objecttype = dbdcgenwa-type
                                         objectname = dbdcgenwa-name
                                         binary search.
      if syst-subrc = 0.  "Object is generated from Ddl source
        read table deltab with key objtyp  = 'DDLS'
                                   objname = ddldepwa-ddlname.
        if syst-subrc = 0.
          dbdcgenwa-delflag = 'X'.
          update dbdcgentb from dbdcgenwa.
        endif.
      endif.
    endif.
  endselect.

endform.

*----------------------------------------------------------------------*
* Form  MT_OBJ_COLLECT
*----------------------------------------------------------------------*
* Collects GENWA-entries for objects which are fully activated
*----------------------------------------------------------------------*
* <-- OBJOK: Objects which are fully activated and can therefore be
*            set to o.k in the input-medium
* --> GENWA: Contains information about current object
*----------------------------------------------------------------------*
form mt_obj_collect tables oktab type oktb
                    using  genwa like dcgentb.

  data ok_wa type okwa.

  if genwa-rc <= 4 and genwa-actflag = ' ' and genwa-switchdep = '' and
     genwa-prelevel <> 'X'.
    move-corresponding genwa to ok_wa.
    append ok_wa to oktab.
  endif.

endform.                    " MT_OBJ_COLLECT

*----------------------------------------------------------------------*
* Form  MA_UP_CHECK
*----------------------------------------------------------------------*
* Checks wether program is running during upgrade and kind of upgrade
* and if force-activation is necessary.
* Exporting variable contains wether activation of all dependent
* objects is recommended.
*----------------------------------------------------------------------*
* --> FRC_ACT: Force activation
* <-- DEP_ACT: 'X': Activation of dependent objects is recommended.
*              ' ': Activator computes for which tables activation is
*                   necessary.
*----------------------------------------------------------------------*
form ma_up_check using frc_act like ddmass-frcact
                       dep_act like ddmass-frcact.

  data: uvers_wa like uvers,
        uvers_tab like uvers occurs 0.

  clear dep_act.
  call function 'UPG_GET_UPGRADE_INFO'
       exporting
            iv_component         = '*'
            iv_newrelease        = ' '
            iv_upgtype           = 'A'
            iv_readmode          = 'ACT'
            iv_comp_select       = 'A'
            iv_buffered          = ' '
       tables
            tt_upginfo           = uvers_tab
       exceptions
            readmode_unknown     = 1
            component_not_active = 2
            ambigious_entries    = 3
            no_upgrade_active    = 4
            others               = 5.
  if syst-subrc = 0.               "Repository-Switch
    read table uvers_tab into uvers_wa index 1.
    if syst-subrc = 0. dep_act = 'X'. exit. endif.
  endif.
* call function 'SUBST_GET_ACTIVE_UVERS_ENTRY'
*      importing
*           ew_uvers           = uvers_wa
*      exceptions
*           no_svers_entry     = 1
*           ambigious_entries  = 2
*           no_exchange_active = 3
*           others             = 4.
* if syst-subrc = 0.
*   if uvers_wa-puttype = 'A'.   "Repository-Switch
*     dep_act = 'X'. exit.
*   endif.
* endif.

  if frc_act = 'X'. dep_act = 'X'. endif.

endform.                    " MA_UP_CHECK

*&---------------------------------------------------------------------*
*&      Form  MA_CHG_TROBJ
*&---------------------------------------------------------------------*
*       Makes necessary changes to transport-objects in GENTAB
*----------------------------------------------------------------------*
*      -->GENTAB: Table with objects to activate
*      -->DELTAB: Table with objects to delete
*----------------------------------------------------------------------*
form ma_chg_trobj tables gentab type gentb
                         deltab type deltb
                  using  medium like ddmass-medium.

  data: genwa like dcgentb,
        delwa like dcdeltb.
  data  mod(1).

  check medium = 'T'.

  loop at gentab into genwa.
    clear mod.
    if     genwa-type = domd. genwa-type = doma. mod = 'X'.
    elseif genwa-type = dted. genwa-type = dtel. mod = 'X'.
    elseif genwa-type = tabd. genwa-type = tabl. mod = 'X'..
    elseif genwa-type = sqld. genwa-type = sqlt. mod = 'X'.
    elseif genwa-type = vied. genwa-type = view. mod = 'X'.
    elseif genwa-type = enqd. genwa-type = enqu. mod = 'X'.
    elseif genwa-type = mcod. genwa-type = mcob. mod = 'X'.
    elseif genwa-type = macd. genwa-type = maco. mod = 'X'.
    elseif genwa-type = ttyd. genwa-type = ttyp. mod = 'X'.
    endif.
    if mod = 'X'. modify gentab from genwa. endif.
  endloop.

  loop at deltab into delwa.
    clear mod.
    if     delwa-objtyp = domd. delwa-objtyp = doma. mod = 'X'.
    elseif delwa-objtyp = dted. delwa-objtyp = dtel. mod = 'X'.
    elseif delwa-objtyp = tabd. delwa-objtyp = tabl. mod = 'X'..
    elseif delwa-objtyp = sqld. delwa-objtyp = sqlt. mod = 'X'.
    elseif delwa-objtyp = vied. delwa-objtyp = view. mod = 'X'.
    elseif delwa-objtyp = enqd. delwa-objtyp = enqu. mod = 'X'.
    elseif delwa-objtyp = mcod. delwa-objtyp = mcob. mod = 'X'.
    elseif delwa-objtyp = macd. delwa-objtyp = maco. mod = 'X'.
    elseif delwa-objtyp = ttyd. delwa-objtyp = ttyp. mod = 'X'.
    endif.
    if mod = 'X'. modify deltab from delwa. endif.
  endloop.

endform.                    " MA_CHG_TROBJ

*&---------------------------------------------------------------------*
*&      Form  SET_MESSAGE
*&---------------------------------------------------------------------*
*       Stes message-information to GENWA
*----------------------------------------------------------------------*
*      -->GENWA: Workarea with information about currently activated
*                object
*----------------------------------------------------------------------*
form set_message using genwa structure dcgentb.

* Get message of special severity
  data: arbgb like messid-arbgb,
        msgnr like messid-msgnr,
        severity like sprot-severity,
        par1(30), par2(30), par3(30), par4(30),
        message like ddmess.

  severity = 'E'.
  perform get_message using arbgb msgnr severity par1 par2 par3 par4.
  genwa-arbgb    = arbgb.
  genwa-msgnr    = msgnr.
  genwa-severity = severity.
  genwa-par1     = par1.
  genwa-par2     = par2.
  genwa-par3     = par3.
  genwa-par4     = par4.
  if genwa-arbgb = ' '.
    severity = 'W'.
    perform get_message using arbgb msgnr severity par1 par2 par3 par4.
    genwa-arbgb    = arbgb.
    genwa-msgnr    = msgnr.
    genwa-severity = severity.
    genwa-par1     = par1.
    genwa-par2     = par2.
    genwa-par3     = par3.
    genwa-par4     = par4.
  endif.

endform.                    " SET_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  INFO_MESS_WRITE
*&---------------------------------------------------------------------*
*       Gets message-text out of T100
*----------------------------------------------------------------------*
*      -->GENWA: Workarea with information about currently activated
*                object
*      -->PRID : Id for log-writer
*----------------------------------------------------------------------*
form info_mess_write using genwa structure dcgentb
                           prid  like syst-tabix.

  data: t100_wa like t100,
        text_tmp(1000), text(150), par(3).

  select * from t100 into t100_wa where sprsl = syst-langu
                                  and   arbgb = genwa-arbgb
                                  and   msgnr = genwa-msgnr.
  endselect.
  if syst-subrc = 0.
    text_tmp = t100_wa-text.
    translate text_tmp using ' ~'.
    replace '&' with genwa-par1 into text_tmp.
    replace '&' with genwa-par2 into text_tmp.
    replace '&' with genwa-par3 into text_tmp.
    replace '&' with genwa-par4 into text_tmp.
    condense text_tmp no-gaps. text = text_tmp.
    concatenate '(' genwa-severity '-' into par.
    translate text using '~ '.
*   DO546: (&)
    str3> prid par text ')'.
  endif.

endform.                    " GET_MESSAGE_TEXT

*&---------------------------------------------------------------------*
*&      Form  MA_OBJ_CONST_COMP
*&---------------------------------------------------------------------*
*       Computes length of DD-object-names
*----------------------------------------------------------------------*
form ma_obj_const_comp using dddomalen like ddmass-dddomalen
                             dddtellen like ddmass-dddtellen
                             ddtabllen like ddmass-ddtabllen.

  if dddomalen <> 0 and dddtellen <> 0 and ddtabllen <> 0.
    exit.
  endif.
  describe field dd01l-domname  length dddomalen in character mode.
  describe field dd04l-rollname length dddtellen in character mode.
  describe field dd02l-tabname  length ddtabllen in character mode.

endform.                    " MA_OBJ_CONST_COMP

*&---------------------------------------------------------------------*
*&      Form  MA_ENQU_CHECK
*&---------------------------------------------------------------------*
*       Checks wether mass-activation can be executed or other relavnt
*       massactivator is already running
*----------------------------------------------------------------------*
*      -->ENQUEUE: 'E': Exclusive or 'S': Shared enqueue
*      -->RC     : Returncode
*                  8: Relevant mass-activation already running
*                  0: Massactivation can be executed
*      -->PRID   : Id for log-writer
*----------------------------------------------------------------------*
form ma_enqu_check using enqueue like ddmass-enqueue
                         rc      like syst-subrc
                         prid    like syst-tabix.

  data: mode like dd26e-enqmode,
        objtype like rsdeo-objtype,
        objname like rsdeo-objname.

  clear rc.
  mode = enqueue. objtype = 'MASS'. objname = 'ACT'.
  call function 'ENQUEUE_ESDICT'
       exporting
            mode_rsdeo     = mode
            objtype        = objtype
            objname        = objname
*           X_OBJTYPE      = ' '
*           X_OBJNAME      = ' '
*           _SCOPE         = '2'
*           _WAIT          = ' '
*           _COLLECT       = ' '
       exceptions
            foreign_lock   = 1
            system_failure = 2
            others         = 3.
  if syst-subrc <> 0.
    rc = 8.
    if sy-subrc = 1.
*     DO544: Massactivation already running. No second call possible
      smi1> prid 'E' 'DO544' syst-msgv1.
    else.
      clear syst-msgv1.
*     AD025: System error in lock management
      smi0> prid 'E' 'AD025'.
    endif.
  endif.

endform.                    " MA_ENQU_CHECK

*&---------------------------------------------------------------------*
*&      Form  MA_DEQU
*&---------------------------------------------------------------------*
*       Deletes mass-activation-lock (lock against parallel run of
*       massactivator)
*----------------------------------------------------------------------*
*      -->ENQUEUE: 'E': Exclusive or 'S': Shared enqueue
*----------------------------------------------------------------------*
form ma_dequeue using enqueue like ddmass-enqueue.

  data: mode like dd26e-enqmode,
        objtype like rsdeo-objtype,
        objname like rsdeo-objname.

  mode = enqueue. objtype = 'MASS'. objname = 'ACT'.
  call function 'DEQUEUE_ESDICT'
       exporting
            mode_rsdeo = mode
            objtype    = objtype
            objname    = objname.
*           X_OBJTYPE  = ' '
*           X_OBJNAME  = ' '
*           _SCOPE     = '3'
*           _SYNCHRON  = ' '
*           _COLLECT   = ' '.

endform.                    " MA_ENQU_CHECK

*&---------------------------------------------------------------------*
*&      Form  MA_STATISTIC_WRITE
*&---------------------------------------------------------------------*
*       Write statistic about activated and deleted objects
*----------------------------------------------------------------------*
*      -->GENTAB    : Internal table with objects to activate
*      -->DELTAB    : Internal table with objects to delete
*      -->GEN_CNT   : Activates objects
*      -->DEL_CNT   : Deleted objects
*      -->DEL_CNT_REAL: Really deleted objects
*      -->PRID      : Id for log-writer
*      -->MV_CNT_OK : Objects successfully moved to TBATG
*      -->MV_CNT_ERR: Objects unsuccessfully moved to TBATG
*      -->NTDEL_CNT : Number of objects with Nametabs to delete
*      -->NTDEL_ERR : Number of Nametabs not deleted
*      -->PERSIST   : 'X', GENTAB information lies on database
*      -->PRID      : ID for logwriter
*----------------------------------------------------------------------*
form ma_statistic_write tables gentab     type gentb
                               deltab     type deltb
                        using  gen_cnt    like syst-tabix
                               del_cnt    like syst-tabix
                               del_cnt_real type sytabix
                               mv_cnt_ok  like syst-tabix
                               mv_cnt_err like syst-tabix
                               ntdel_cnt  like syst-tabix
                               ntdel_err  like syst-tabix
                               persist
                               prid       like syst-tabix.

  data: phase_txt(80),
        cnt_chr(10),
        cnt_ok like syst-tabix, cnt_warning like syst-tabix,
        cnt_err like syst-tabix, cnt_dep_err like syst-tabix,
        mv_cnt_chr(10), mv_cnt_ok_chr(10), mv_cnt_err_chr(10),
        ntdel_cnt_chr(10), ntdel_cnt_ok_chr(10), ntdel_err_chr(10),
        dbdcgenwa type dbdcgentb.
  field-symbols: <genwa> like dcgentb, <delwa> like dcdeltb.

  phase_txt = text-031. phase_header> prid phase_txt.

  cnt_chr = gen_cnt.
* DO601: Number of activated objects: &
  smi1> prid 'N' 'DO601' cnt_chr.

  if persist = 'X'.
    select rc from dbdcgentb into dbdcgenwa-rc where rc = 4.
      cnt_warning = cnt_warning + 1.
    endselect.
    select rc from dbdcgentb into dbdcgenwa-rc where rc = 6.
      cnt_dep_err = cnt_dep_err + 1.
    endselect.
    select rc from dbdcgentb into dbdcgenwa-rc where rc = 8.
      cnt_err = cnt_err + 1.
    endselect.
  else.
    loop at gentab assigning <genwa>.
      if <genwa>-rc >= 4. "most rcs are < 4
        if     <genwa>-rc = 4. add 1 to cnt_warning.
        elseif <genwa>-rc = 6. add 1 to cnt_dep_err.
        elseif <genwa>-rc = 8.
          if <genwa>-prelevel = 'X'.
            read table gentab into data(genwa_l)
                       with key type     = <genwa>-type
                                name     = <genwa>-name
                                indx     = <genwa>-indx
                                prelevel = 'Y' binary search.
            if genwa_l-rc > 4.
              add 1 to cnt_err.
            endif.
          elseif <genwa>-prelevel = ''.
            add 1 to cnt_err.
          endif.
        endif.
      endif.
  endloop.
  endif.
  cnt_chr = cnt_err.
* DO605: Number of objects with errors: &
  smi1> prid 'N' 'DO605' cnt_chr.
  cnt_chr = cnt_dep_err.
* DO606: Number of objects with errors durning dependent activation: &
  smi1> prid 'N' 'DO606' cnt_chr.
  cnt_chr = cnt_warning.
* DO604: Number of objects with warnings: &
  smi1> prid 'N' 'DO604' cnt_chr.
  cnt_chr = gen_cnt - cnt_err - cnt_dep_err - cnt_warning.
* DO603: Number of objects successfully activated: &
  smi1> prid 'N' 'DO603' cnt_chr.

  cnt_chr = del_cnt.
* DO602: Number of objects to delete: &
  smi1> prid 'N' 'DO602' cnt_chr.

  cnt_chr = del_cnt - del_cnt_real.
* DO608: Number of objects not deleted: &
  smi1> prid 'N' 'DO608' cnt_chr.
  cnt_chr = del_cnt_real.
* DO607: Number of objects successfully deleted: &
  smi1> prid 'N' 'DO607' cnt_chr.

  mv_cnt_chr = mv_cnt_ok + mv_cnt_err.
* DO609: Tables/Views with DROP/CREATE: &
  smi1> prid 'N' 'DO609' mv_cnt_chr.
* DO610: Successfully moved tables/views
  mv_cnt_ok_chr = mv_cnt_ok.
  smi1> prid 'N' 'DO610' mv_cnt_ok_chr.
* DO611: Unsuccessfully moved tables/views
  mv_cnt_err_chr = mv_cnt_err.
  smi1> prid 'N' 'DO611' mv_cnt_err_chr.

* DO620: Number of Nametabs to delete: &
  ntdel_cnt_chr = ntdel_cnt.
  smi1> prid 'N' 'DO620' ntdel_cnt_chr.
  ntdel_cnt_ok_chr = ntdel_cnt - ntdel_err.
* DO622: Number of Nametabs successfully deleted: &
  smi1> prid 'N' 'DO622' ntdel_cnt_ok_chr.
  ntdel_err_chr = ntdel_err.
* DO621: Number of Nametabs not successfully deleted: &
  smi1> prid 'N' 'DO621' ntdel_err_chr.

  phase_tail> prid phase_txt ''.

endform.                    " MA_STATISTIC_WRITE

*&---------------------------------------------------------------------*
*&      Form  MA_OBJ_MV_TBATG
*&---------------------------------------------------------------------*
*       Gets tables and views with drop/delete-instruction out of
*       GENTAB and writes them with function MDF in TBATG
*----------------------------------------------------------------------*
*      -->GENTAB  : Table with DD-object-names to activate
*      -->INACTIVE: 'X': Inactive handling, ' ': Active handling
*      -->PRID    : Id for log-writer
*      <--CNT_OK  : Counter for successfully moved objects
*      <--CNT_ERR : Counter for unsuccessfully moves objects
*----------------------------------------------------------------------*
form ma_obj_mv_tbatg tables gentab type gentb
                     using  inactive like ddmass-inactive
                            medium   like ddmass-medium
                            prid     like syst-tabix
                            cnt_ok   like syst-tabix
                            cnt_err  like syst-tabix.

  data: genwa    type dcgentb,
        tbatg_wa like tbatg,
        rc       like syst-subrc,
        objok    type oktb,
        ok_wa    type okwa,
        nr       like syst-tabix.

  check medium = 'T'.

  newob> prid.

  clear cnt_err.
  sort gentab by type name indx objfunc.
  loop at gentab into genwa
                 where ( type    = 'TABL' or type = 'VIEW' ) and
                         objfunc = 'M'.
    nr = syst-tabix.
    read table gentab with key type    = genwa-type
                               name    = genwa-name
                               indx    = genwa-indx
                               objfunc = ' ' binary search.
    if syst-subrc = 0.
      delete gentab index nr.
*     DO585: &&: DROP/CREATE ignored because of activation-entry in
*            transport-order
      smi2> prid 'N' 'DO585' genwa-type genwa-name.
      continue.
    endif.   "No entry in TBATG
*   if inactive = 'X'.
*     tbatg_wa-fct      = 'MDF'.
*     tbatg_wa-execmode = 'I'.
*     tbatg_wa-object   = genwa-type.
*     tbatg_wa-tabname  = genwa-name.
*     perform insert_tbatg using tbatg_wa rc prid.
*     if rc <= 4.
*       clear genwa-objfunc. modify gentab from genwa.
*     else.
*       cnt_err = cnt_err + 1.
*     endif.
*   else.
*   Handling in form MT_TABL_ACT and MT_VIEW_ACT
*   endif.
  endloop.

endform.                    " MA_OBJ_MV_TBATG

*&---------------------------------------------------------------------*
*&      Form  MA_SYSTEMTYPE_GET
*&---------------------------------------------------------------------*
*       Computes if system has type SAP or customer
*----------------------------------------------------------------------*
*      <->SYSTEM_TYPE: Contains 'SAP' for SAP-internal system
*----------------------------------------------------------------------*
form ma_systemtype_get using system_type like syst-sysid.

  data: system_name like syst-sysid.

  if system_type = ' '.
    call function 'TR_SYS_PARAMS'
         importing
              systemname    = system_name
              systemtype    = system_type
         exceptions
              no_systemname = 01
              no_systemtype = 02.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  MT_VIEW_DROP_CREATE
*&---------------------------------------------------------------------*
*       Drops and creates a view
*----------------------------------------------------------------------*
*      -->VIEWNAME: Name of view to drop and create
*      -->RC      : Returncode of DB-operation for view
*      <--PRID    : Id fpr log-writer
*----------------------------------------------------------------------*
form mt_view_drop_create using viewname like dd25l-viewname
                               rc       like syst-subrc
                               prid     like syst-tabix.

  clear rc.
  call function 'DB_DROP_VIEW'
       exporting
            prid                  = prid
            viewname              = viewname
       importing
            subrc                 = rc
       exceptions
            program_not_generated = 1
            program_not_written   = 2
            view_not_dropped      = 3
            others                = 4.
  if sy-subrc <> 0 or rc > 4.
    rc = 8.
*   MC532: Error during deletion of view &
    smi1> prid 'E' 'MC532' viewname.
  else.
    call function 'DD_CREATE_VIEW'
         exporting
              prid             = prid
              viewname         = viewname
         exceptions
              basetab_error    = 1
              db_error         = 2
              dd_error         = 3
              view_exists      = 4
              view_not_created = 5
              no_dbview        = 6
              others           = 7.
    if sy-subrc <> 0.
      rc = 8.
*     MC533: Error during creation of view &
      smi1> prid 'E' 'MC533' viewname.
    else.
*     DO595: View & was deleted and newely created on DB
      smi1> prid 'N' 'DO595' viewname.
    endif.
  endif.

endform.                    " MT_VIEW_DROP_CREATE

*----------------------------------------------------------------------*
* Form MA_APP_CI_DEP_RES_ANALYSE
*----------------------------------------------------------------------*
* Computes the total result for the dependent objects of Appends, CI-
* and SI-Includes
*----------------------------------------------------------------------*
* --> GENTAB   : Contains all objects to activate
* --> DEPTAB   : Contains all dependencies
* --> DELDEPTAB: Contains all references for DD-objects with deletion
*                order
* --> PERSIST  : 'X', get GENTAB information from database
* --> PRID     : Id for log-writer
* <-- RC       : Returncode of activation of all objects dependent of
*                of an Append, CI- or SI-Include
*----------------------------------------------------------------------*
form ma_app_ci_dep_res_analyse tables gentab    type gentb
                                      deptab    type deptb
                                      deldeptab type deldeptb
                               using  persist
                                      prid like syst-tabix
                                      rc   like syst-subrc.

  data: depwa like dcdeptb,
        genwa type dcgentb,
        dbdcgenwa type dbdcgentb,
        type1 like dcgentb-type, type2 like dcgentb-type,
        name1 like dcgentb-name, name2 like dcgentb-name,
        depkind like dcdeptb-depkind,
        cnt   like syst-tabix,
        modify(1),
        nr like syst-tabix.
  field-symbols: <deldepwa> type dcdeldeptb.

  clear rc.

  sort deldeptab by depname deptyp kind.
  sort gentab by type name indx.
  "<KH> 20040323 type = doma added because of domain appends
  loop at deptab where ( type = tabl or type = doma )
                 and   ( depkind = 'A' or depkind = 'I' ).
    cnt = syst-tabix.
*   Look wether table is dependent of an Append or CI- or SI-Include
    read table deldeptab assigning <deldepwa>
                         with key depname = deptab-name
                                  deptyp  = deptab-type
                                  kind    = 'A' binary search.
    if syst-subrc <> 0. continue. endif.
    if persist = 'X'.
      select * from dbdcgentb into dbdcgenwa
               where type = deptab-type
               and   name = deptab-name.
        move-corresponding dbdcgenwa to genwa.
      endselect.
    else.
      read table gentab into genwa
                        with key type = deptab-type
                                 name = deptab-name
                                 binary search.
    endif.
    if syst-subrc = 0.
      if genwa-rc > rc. rc = genwa-rc. endif.
    endif.
    if type1 <> deptab-type or name1 <> deptab-name.
      type2 = deptab-type. name2 = deptab-name.
      depkind = deptab-depkind.
      depwa = deptab.
      while depwa-type = type2 and depwa-name = name2.
        if depkind = 'A' or depkind = 'I'.
          if persist = 'X'.
            select * from dbdcgentb into dbdcgenwa
                     where type    = deptab-deptype
                     and   name    = deptab-depname.
               move-corresponding dbdcgenwa to genwa.
            endselect.
          else.
            read table gentab into genwa
                              with key type    = deptab-deptype
                                       name    = deptab-depname
                                       binary search.
          endif.
          if syst-subrc = 0.
            if genwa-rc > rc. rc = genwa-rc. endif.
          endif.
        endif.
        cnt = cnt + 1.
        read table deptab index cnt.
        if syst-subrc = 0.
          type2 = deptab-type. name2 = deptab-name.
          depkind = deptab-depkind.
        else.
          clear type2. clear name2.
        endif.
      endwhile.
    endif.
    type1 = depwa-type. name1 = depwa-name.
  endloop.

endform.                    " MA_APP_CI_DEP_RES_ANALYSE

*&---------------------------------------------------------------------*
*&      Form  MA_NAME_CONFLICTS_ANALYZE
*&---------------------------------------------------------------------*
*       Handles objets which are possibly not activated because
*       name-conflicts with deleted objects
*       Activate the nameconflict objects and afterwards all objects
*       which are directly in GENTAB and which could not be activated
*       successfully before. Afterwards there are objects which are still
*       dependent of the deleted objects. These have to be found and activated
*       as dependent objects.
*----------------------------------------------------------------------*
*      --> GENTAB   : Table with objects to activate
*      --> DEPTAB   : Table with dependencies between DD-objects
*      --> INVDEPTAB: Inverse deptab
*      <-- OKTAB    : Contains successfully activated objects
*      --> ACTB     : Contains control-information for every DD-type
*      <-- CNVTAB   : Contains tables which have to be converted
*      --> NCONFTAB:  Tables to activate with name-conflicts to deleted
*                     objects
*      --> CT       : Activation information such as activation level
*                     or medium
*      --> LEVEL    : Maximallevel
*      --> PERSIST  : 'X' Select GENTAB-entries from database
*----------------------------------------------------------------------*
form ma_name_conflicts_analyze tables gentab    type gentb
                                      deptab    type deptb
                                      actb      type ddactb
                                      oktab     type oktb
                                      cnvtab    structure dctablres
                                      nconftab  type gentb
                               using  invdeptab type invdeptb
                                      ct        type ctrl
                                      level     like dcgentb-level
                                      persist.

  data: genwa    like dcgentb,
        depwa    like dcdeptb,
        dbdcgenwa type dbdcgentb,
        minlev   like dcgentb-level, l like dcgentb-level,
        phase_txt(80),
        logname1 like ddmass-logname,
        cnt      like syst-tabix,   "Count objects activated per level
        act_s    like ddmass-actsingle,
        pridf    like syst-tabix,
        tst      like ddmasstst,
        depchg   type deptb,
        lev      like dcgentb-level,
        t        like t,
        t_on     like ddmass-timer,
        oldobjs  type gentb,
        deptab_loc     type deptb,
        invdeptab_loc  type invdeptb,
        invdepwa       type dcinvdep,
        rc_persist     type sysubrc,   "not used
        phase_begin    type ABAP_BOOL.
  field-symbols: <depwa> type dcdeptb.

  check ct-test_on = ' '.

  read table nconftab into genwa index 1.
  if syst-subrc <> 0. exit. endif.

  sort nconftab by name delflag.
  loop at nconftab.
    if nconftab-delflag = ' '.
      if persist = 'X'.
        refresh gentab.
        select * from dbdcgentb into dbdcgenwa
                 where type    = nconftab-type
                 and   name    = nconftab-name
                 and   indx    = nconftab-indx
                 and   actkind = 'N'.
          move-corresponding dbdcgenwa to genwa.
        endselect.
      else.
        read table gentab into genwa with key type = nconftab-type
                                              name = nconftab-name
                                              indx = nconftab-indx
                                              actkind = 'N' binary search.
      endif.
      if syst-subrc = 0.
        if genwa-rc <= 4.
        "Sucessfully activated (object to delete could be deleted in 1. run)
          delete nconftab where name = genwa-name.
        else. "Object directly activated with error
          if minlev = 0. minlev = genwa-level. endif.
          if minlev > genwa-level. minlev = genwa-level. endif.
        endif.
      else. "Not found (should not occur)
        delete nconftab.
      endif.
    elseif nconftab-delflag = 'D'.
      move-corresponding nconftab to genwa.
      genwa-level = 1.
      append genwa to oldobjs.
      delete nconftab.
    endif.
  endloop.

* Activate objects which could not be activate before what may be caused by
* the nameconfict. Start with the nameconflict-objects
  if minlev > 0.
    phase_txt = text-042. phase_header> ct-prid phase_txt.
    phase_begin = ABAP_true.
    l = minlev.
    if persist = 'X'.
      refresh gentab.
      select * from dbdcgentb into dbdcgenwa where rc >= 8.
        move-corresponding dbdcgenwa to genwa.
        append genwa to gentab.
      endselect.
    endif.
*   Sort so that N-version of objects is activated before A-version
    sort gentab by level type name indx actkind descending.
    while l <= level.
      sync> ct-prid. log_open> 'I' 'N' '' 'single_style' 'ACT_TMP' ''
                            logname1 ct-prid1.
      timer_on> ct-t_on t-levact.
      "Only objects with error are activated
      ct-rc_in = 8. ct-level = l. ct-depact = 'X'.
      perform ma_lev_activate_c3(SAPLSDMP) tables gentab deptab cnvtab oktab depchg
        using invdeptab ct ct-t_on.
      timer_off> ct-t_on t-levact ct-prid1.
*-----Mark Objects in transportorder or delete them from external table--*
      perform ma_set_ready(radmasg0_c3) using oktab[] ct-medium  ' ' ct-prid1.
      l = l + 1.
      if ct-counter > 0. redirect> ct-prid ct-prid1. endif.
      close> ct-prid1.
    endwhile.
  endif.

  if persist = 'X'.
    free gentab.
  endif.

  if phase_begin = ABAP_false.  "Phase header not already written
    phase_txt = text-042. phase_header> ct-prid phase_txt.
    phase_begin = ABAP_true.
  endif.

* Compute objects which are dependent of new object but which could not been
* found before because object was not active (Objects with DELFLAG = 'D' are
* already deleted)
  perform ma_get_act_sequence tables   nconftab deptab_loc
                              using    lev '' '' '' t t_on rc_persist ct-prid
                              changing invdeptab_loc.

* Compute objects which are still dependent of deleted objects
  perform ma_get_act_sequence tables   oldobjs deptab_loc
                              using    lev '' '' '' t t_on rc_persist ct-prid
                              changing invdeptab_loc.

* Append additional objects to activate
  append lines of oldobjs to nconftab.
  delete nconftab where delflag = 'D'.

* Change object type of dependencies
  sort nconftab by name.
  loop at deptab_loc into depwa. "Only entries with DELFLAG = 'A'
    read table nconftab with key name = depwa-name
                binary search.
    if syst-subrc = 0.
      depwa-type = nconftab-type.
      modify deptab_loc from depwa.
    endif.
  endloop.

* Check if deleted objects still have dependencies
  read table deptab_loc into depwa index 1.
  if syst-subrc = 0.
    "Overtake to DEPTAB with preserving the sequence
    perform add_to_deptab using deptab_loc deptab[].
    l = 1. refresh oktab. clear oktab.
*   Sort so that N-version of objects is activated before A-version
    sort nconftab by level type name indx actkind descending.
    "Build inverse DEPTAB for later direct access
    refresh invdeptab_loc.
    loop at deptab assigning <depwa>.
      move-corresponding <depwa> to invdepwa.
      invdepwa-tabix = syst-tabix.
      insert invdepwa into table invdeptab_loc.
    endloop.
    while l <= lev.
      sync> ct-prid. log_open> 'I' 'N' '' 'single_style' 'ACT_TMP' ''
                            logname1 ct-prid1.
      timer_on> ct-t_on t-levact.
      ct-rc_in = 0. ct-level = l. ct-depact = 'X'.
      perform ma_lev_activate_c3(SAPLSDMP) tables nconftab deptab cnvtab oktab depchg
        using invdeptab_loc ct ct-t_on.
      timer_off> ct-t_on t-levact ct-prid1.
*---  Mark Objects in transportorder or delete them from external table--*
      perform ma_set_ready(radmasg0_c3) using oktab[] ct-medium  ' ' ct-prid1.
      l = l + 1.
      if ct-counter > 0. redirect> ct-prid ct-prid1. endif.
      close> ct-prid1.
    endwhile.

    sort gentab by type name indx local actkind.
    loop at nconftab where local = 'N'.
      if persist = 'X'.
        update dbdcgentb set rc = nconftab-rc
          where type    = nconftab-type
          and   name    = nconftab-name
          and   indx    = nconftab-indx
          and   local   = nconftab-local
          and   actkind = nconftab-actkind.
      else.
        read table gentab into genwa with key
                      type    = nconftab-type
                      name    = nconftab-name
                      indx    = nconftab-indx
                      local   = nconftab-local
                      actkind = nconftab-actkind binary search.
        if syst-subrc = 0.   "Store in special table
          genwa-rc = nconftab-rc.
          modify gentab from genwa index syst-tabix.
        endif.
      endif.
    endloop.
  endif.

  if phase_begin = ABAP_true.
    phase_tail> ct-prid phase_txt ' '.
  endif.

endform.                    " MA_NAME_CONFLICTS_ANALYZE

*&---------------------------------------------------------------------*
*&      Form  MA_UPDTMST_INDX_TABT
*&---------------------------------------------------------------------*
*      If table buffering information is changed or index changed,
*      created or deleted we need to update the dependent objects
*      timestamps. Reason is that these information is used in table
*      buffer and invalidation is necessary because new table buffer
*      works with table types with secondary and these have to be
*      invalidated in the described cases. We need to invalidate the
*      table dependent objects too because otherwise typeload information
*      of table would be newer than table information.
*----------------------------------------------------------------------*
*      --> GENTAB   : Table with objects to activate
*      --> DELTAB   : Table with objects to delete
*      --> CT       : Control structure with additional information
*      --> PERSIST  : 'X' Select GENTAB-entries from database
*----------------------------------------------------------------------*
form ma_updtmst_indx_tabt using  gentab    type gentb
                                 deltab    type deltb
                                 ct        type ctrl
                                 persist.

  types: begin of ty_s_objwa,
           type type objtype,
           name type objname,
        end of ty_s_objwa.
  data: genwa     type dcgentb,
        delwa     type dcdeltb,
        depwa     type dcdeptb,
        gentab_l  type gentb,
        tabltab   type ddobjtb,
        sqlttab   type ddobjtb,
        dteltab   type ddobjtb,
        domatab   type ddobjtb,
        mcobtab   type ddobjtb,
        viewtab   type ddobjtb,
        shlptab   type ddobjtb,
        ttyptab   type ddobjtb,
        mcidtab   type ddobjtb,
        typetab   type ddobjtb,
        stobtab   type ddobjtb,
        ddlstab   type ddobjtb,
        deptab_l  type deptb,
        tabname   type tabname,
        objwa     type ty_s_objwa,
        objtab    type sorted table of ty_s_objwa
                       with unique key type name,
        dbdcgenwa type dbdcgentb,
        rc_tmst   type sysubrc,
        ntabstate type modeflag value 'A',
        got_state type modeflag,
        x030l_wa  type x030l,
        ex        type ABAP_BOOL,
        phase_txt(60),
        logreal_l type ddmass-logname,
        prid_l    type sytabix.

  if persist = 'X'.
    refresh gentab.
    select * from dbdcgentb into dbdcgenwa
             where ( type    = tabt or type = indx )
             and   settmstdep <> ''.
      move-corresponding dbdcgenwa to genwa.
    endselect.
  endif.

  "Get start set
  sort gentab by type name local.
  loop at gentab into genwa where ( type = tabt or type = indx )
                            and   settmstdep <> ''.
    check ( 'ADB' cs genwa-settmstdep ).
    if persist = 'X'.
      select * from dbdcgentb into dbdcgenwa
                              where type = tabl
                              and   name = genwa-name.
                              "and   local = 'N'.
      endselect.
      if syst-dbcnt = 0.
        continue.
      endif.
    else.
      read table gentab transporting no fields
                        with key type  = tabl
                                 name  = genwa-name
                                 "local = 'N'
                        binary search.
      if syst-subrc = 0.  "Table in act set, timestamp update already done
        continue.
      endif.
    endif.
    genwa-type = tabl.
    append genwa to gentab_l.
    append genwa-name to tabltab.
  endloop.

  loop at deltab into delwa where objtyp = indx.
    tabname = delwa-objname.
    clear: x030l_wa, got_state.
    call function 'DD_GET_NAMETAB_HEADER'
      exporting
        STATUS           = 'M'
        tabname          = tabname
      IMPORTING
        r_status         = got_state
        X030L_WA         = x030l_wa
      EXCEPTIONS
        NOT_FOUND        = 1
        OTHERS           = 2.
    if x030l_wa is initial or got_state = ''.
      continue. "Table may be deleted too
    endif.
    if x030l_wa-bufstate = ''.
      continue.
    endif.
    genwa-type  = tabl.
    genwa-name  = delwa-objname.
    genwa-local = 'N'.
    append genwa to gentab_l.
    append genwa-name to tabltab.
  endloop.

  check lines( gentab_l ) > 0.
  log_open> 'I' 'N' 1 'single_style' 'UTMST&DATE&&TIME&' ' ' logreal_l prid_l.

  perform ma_fill_tables
         tables gentab_l deptab_l
                tabltab sqlttab dteltab domatab mcobtab viewtab shlptab
                ttyptab mcidtab stobtab ddlstab
         using  persist -1.

  if ct-inactive = ABAP_true.
    ntabstate = 'N'.
  endif.
  sort deptab_l by type name indx deptype depname depindx.
  delete adjacent duplicates from deptab_l
    comparing type name indx deptype depname depindx.
  loop at deptab_l into depwa where local = 'A'   "Only dependencies which are active
                              and   ( depkind = 'A' or depkind = 'I' or depkind = 'Y' ) or
                                    ( depkind = 'S' or depkind = 'X' ).
    check depwa-type = tabl or depwa-type = dtel or depwa-type = ttyp or
         depwa-type = view.
    read table objtab transporting no fields
                      with key type = depwa-type
                               name = depwa-name
                           binary search.
    if syst-subrc <> 0.  "Not already handled
      tabname = depwa-name.
      perform update_timestamp_with_check(saplsdta) using tabname '' ntabstate
        ABAP_false rc_tmst prid_l.
      if rc_tmst <> 2.
        ex = ABAP_true.
      endif.
      move-corresponding depwa to objwa.
      insert objwa into table objtab.
    endif.
    check depwa-deptype = tabl or depwa-deptype = dtel or depwa-deptype = ttyp or
          depwa-deptype = view.
    read table objtab transporting no fields
                      with key type = depwa-deptype
                               name = depwa-depname
                           binary search.
    if syst-subrc <> 0.  "Not already handled
      tabname = depwa-depname.
      perform update_timestamp_with_check(saplsdta) using tabname '' ntabstate
        ABAP_false rc_tmst prid_l.
      if rc_tmst <> 2.
        ex = ABAP_true.
      endif.
      objwa-type = depwa-deptype.
      objwa-name = depwa-depname.
      insert objwa into table objtab.
    endif.
  endloop.

  if ex = ABAP_true.
    phase_txt = text-085.
    phase_header> ct-prid phase_txt.
    level_add> ct-prid.
    redirect> ct-prid prid_l.
    level_sub> ct-prid.
    phase_tail> ct-prid phase_txt space.
  endif.
  close> prid_l.

endform.

*&---------------------------------------------------------------------*
*&      Form  MA_NTAB_DEL_EXTRACT
*&---------------------------------------------------------------------*
*       Extracts objects for which nametab has to be deleted
*----------------------------------------------------------------------*
*      -->DELTAB  : Objects to delete
*      -->NTDELTAB: Types and Names of Nametabs to delete
*      <--NTDELCNT: Counter for objects with Nametab to delete
*----------------------------------------------------------------------*
form ma_ntab_del_extract tables deltab   type deltb
                                ntdeltab type ntdeltb
                         using  ntdelcnt like syst-tabix.

  data delwa like dcdeltb.

  clear ntdelcnt.

  loop at deltab into delwa
                 where objtyp = nttb or objtyp = nttt or objtyp = ntdt.
    ntdeltab-name = delwa-objname.
    if     delwa-objtyp = nttb. ntdeltab-type = tabl.
    elseif delwa-objtyp = nttt. ntdeltab-type = ttyp.
    elseif delwa-objtyp = ntdt. ntdeltab-type = dtel.
    endif.
    ntdeltab-pgmid      = delwa-pgmid.
    ntdeltab-nodoku     = delwa-nodoku.
    ntdeltab-tabix      = delwa-e071_index.
    ntdeltab-ntabaction = 'D'. "Action delete
    append ntdeltab.
    delete table deltab from delwa.
    ntdelcnt = ntdelcnt + 1.
  endloop.

endform.                    " MA_NTAB_DEL_EXTRACT

*&---------------------------------------------------------------------*
*&      Form  MA_NTAB_DEL
*&---------------------------------------------------------------------*
*       Deletes Nametabs of tables/structures, tabletypes and data-
*       elements
*----------------------------------------------------------------------*
*      -->NTDELTAB : Tables to delete
*      <--NTDEL_ERR: Error-counter
*      -->PRID     : Id of log-writer
*----------------------------------------------------------------------*
form ma_ntab_del tables ntdeltab  type ntdeltb
                 using  ntdel_err like syst-tabix
                        prid      like syst-subrc.

  data: rc like syst-subrc, rc_total like syst-subrc,
        phase_txt(80), par(16).

  read table ntdeltab index 1.
  if syst-subrc = 0.
    phase_txt = text-043. phase_header> prid phase_txt.
    newob> prid.
  else.
    exit.
  endif.

  clear ntdel_err.
  sort ntdeltab by ntabaction type name.
  loop at ntdeltab where ntabaction = 'D'.
    if     ntdeltab-type = tabl. par = text-044.
    elseif ntdeltab-type = dtel. par = text-016.
    elseif ntdeltab-type = ttyp. par = text-034.
    endif.
    call function 'DD_NAMETAB_DELETE'
         exporting
              tabname = ntdeltab-name
         importing
              subrc   = rc.
    if rc = 0.
*     DO617: &-Nametab & was successfully deleted
      smi2> prid 'I' 'DO617' par ntdeltab-name.
    else.
      rc_total = 8. ntdel_err = ntdel_err + 1.
      ntdeltab-rc = 8. modify ntdeltab.
*     DO618: &-Nametab & was not successfully deleted
      smi2> prid 'E' 'DO618' par ntdeltab-name.
    endif.
  endloop.
  if rc_total = 0. commit work. else. rollback work. endif.

  phase_tail> prid phase_txt ' '.

endform.                    " MA_NTAB_DEL

*&---------------------------------------------------------------------*
*&      Form  MT_TEST_MODE_HANDLE
*&---------------------------------------------------------------------*
*       Handles objects in test-mode
*----------------------------------------------------------------------*
*      -->GENTAB : Table with objects to activate
*      -->GENWA  : Current object to activate
*      -->LINE_NR: Position of actual object within GENTAB
*      -->PRID1  : Local Id for log-writer. Container for log per level
*----------------------------------------------------------------------*
form mt_test_mode_handle tables gentab  type gentb
                         using  genwa   structure dcgentb
                                line_nr like syst-tabix
                                prid1   like syst-tabix.

  smi3> prid1 'I' 'DO625' genwa-type genwa-name genwa-indx.
  genwa-actflag = 'U'.
  modify gentab from genwa index line_nr.

endform.                    " MT_TEST_MODE_HANDLE

*----------------------------------------------------------------------*
* Writes introduction-messages for trance-log
*----------------------------------------------------------------------*
* --> LOGNAME : Name of activation-log
* --> MEDIUM  : Kind of import-medium of DD-objects:
*               'D': Direct input via Select-options
*               'E': External table
*               'T': Transport-order
* --> INACTIVE: 'X': Inactive activation, ' ': Active activation
* --> DDMODE  : Mode for checks
* --> VERSION : 'M': Activate newest, 'A': Activate active version
* --> EXTTAB  : Name of external table
* --> TRKORR  : Name of transport-order
* --> DELALL  : Delete objects in all versions
* --> DELNOREF: Delete even if references exist
* --> ENQUEUE : Lock of mass-activation
* --> FRCACT  : Handling of dependent objects
* --> PRID    : Id for log-writer
*----------------------------------------------------------------------*
form ma_write_header_tracelog using logname  like ddmass-logname
                                    medium   like ddmass-medium
                                    inactive like ddmass-inactive
                                    ddmode   like ddmass-ddmode
                                    version  like ddmass-version
                                    exttab   like ddmass-exttab
                                    trkorr   like ddmass-trkorr
                                    delall   like ddmass-delall
                                    delnoref like ddmass-delnoref
                                    enqueue  like ddmass-enqueue
                                    frcact   like ddmass-frcact
                                    prid     like syst-tabix.

  data partab like ddpropval occurs 0 with header line.
  data par(30).

* Reference to activation log
  add_par> partab 'Trace zum Protokoll'(048) logname.
* Set medium
  if medium ca 'T'.
    par = 'Transportauftrag'(002).
    concatenate par trkorr into par separated by ' '.
  endif.
  if medium ca 'E'.
    if par <> ' '.
      concatenate par '/' into par.
    endif.
    concatenate par 'Externe Tabelle'(003) into par.
    concatenate par exttab into par separated by ' '.
  endif.
  if medium ca 'D'.
    if par <> ' '.
      concatenate par '/' into par.
    endif.
    concatenate par 'Direkte Objekteingabe'(004) into par.
  endif.
  add_par> partab 'Eingangsmedium'(100) par.

* Activation-parameters
  if inactive = 'X'.
    add_par> partab 'Aktivierungsmethode'(005) 'inaktiv'(006).
  elseif inactive = ' '.
    add_par> partab 'Aktivierungsmethode'(005) 'aktiv'(007).
  endif.
  par = ddmode.
  add_par> partab 'Prüfmodus'(008) par.
  par = version.
  add_par> partab 'Zu aktivierende Version'(009) par.
* Deletion-parameters
  if delall = 'X'.
    par = 'Alle Versionen werden gelöscht'(011).
  else.
    par = 'Nur aktive Versionen löschen'(012).
  endif.
  add_par> partab 'Zu löschende Versionen'(010) par.
* Enqueue massactivation
  if enqueue = 'S'.
    par = 'Shared'(028).
  elseif enqueue = 'E'.
    par = 'Exclusive'(029).
  endif.
  add_par> partab 'Sperre gegen Parallellauf'(030) par.
* Handling of dependent objects
  if frcact = 'X'.
    par = 'wurde erzwungen'(033).
    add_par> partab 'Abhängigenbehandlung'(032) par.
  endif.

* Write program-header
  prog_header> partab prid 'RADMASG0_C3'.

endform.                    " MA_OPEN_LOG

*&---------------------------------------------------------------------*
*&      Form  mt_drop_create_ddftx
*&---------------------------------------------------------------------*
*       Drop/Create DDFTX
*----------------------------------------------------------------------*
*      -->GENWA : DTEL/TABL * to handle and additional information
*      -->ACWA  : Activation-information
*      <--DONE  : 'X', if DDFTX has already been dropped and created,
*                 ' ' otherwise
*      -->PRID  : Id for log-writer
*----------------------------------------------------------------------*
form mt_drop_create_ddftx using genwa like dcgentb
                                acwa  like ddmassac
                                done  like ddrefstruc-bool
                                prid  like syst-tabix.

  data: rc        like syst-subrc,
        x030l_wa  like x030l,
        x031l_tab like x031l occurs 0 with header line,
        ddftx     like dd02l-tabname value 'DDFTX'.

  check genwa-pgmid = lang. check genwa-name = '*'.
  if genwa-type <> dtel and genwa-type <> tabl. exit. endif.

  genwa-rc = 8.
  clear acwa-wr_actflg. clear acwa-maxflg.
  newob> prid.
  if genwa-type = dtel.
*   DO901: Import of all dataelement-texts
    smi0> prid 'N' 'DO901'.
  elseif genwa-type = tabl.
*   DO906: Import of all table-texts
    smi0> prid 'N' 'DO906'.
  endif.

  if done = 'X'.
*   Nothing to do -> DDFTX already dropped and created
    clear genwa-rc.
*   DO907: DROP/CREATE of runtimeobject with screen-painter-texts
*          already executed
    smi0> prid 'N' 'DO907'.
*   str> prid 'DDFTX-Handling will be executed later'.
    exit.
  endif.

* To get rid of locks on DDFTX execute commit
  commit work.

* DO905: DROP/CREATE of runtimeobject with screen-painter-texts
  smi0> prid 'N' 'DO905'.

*----------------------------------------------------------------------*
* call function 'DD_GET_NAMETAB'
*   exporting
*     STATUS           = 'A'
*     tabname          = ddftx
*     GET_ALL          = 'X'
*   IMPORTING
*     F_STATUS         =
*     R_MODEFLAG       =
*     R_STATUS         =
*     X030L_WA         = x030l_wa
*   tables
*     x031l_tab        = x031l_tab
*   EXCEPTIONS
*     NOT_FOUND        = 1
*     NO_FIELDS        = 2
*     OTHERS           = 3.
* if sy-subrc <> 0.
*   exit.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
* else.
*   call function 'DD_PUT_NAMETAB'
*     exporting
*       MODEFLAG           = 'M'
*       status             = 'N'
*       x030l_wa           = x030l_Wa
*       TRTYPE             = ' '
*       NO_INACT_DEL       = ' '
*     IMPORTING
*       R_SUBRC            = rc
*     tables
*       x031l_tab          = x031l_tab
*     EXCEPTIONS
*       WRITE_ERROR        = 1
*       OTHERS             = 2.
*   if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*   endif.
*   exit.
* endif.
*----------------------------------------------------------------------*

  if syst-dbsys = 'MSSQL'.
    perform ddftx_handle_mssql using genwa done prid.
    exit.
  endif.

  call function 'DB_DROP_TABLE'
    exporting
      tabname                     = ddftx
      db_check_flag               = 'X'
    importing
      subrc                       = rc
    exceptions
      program_not_generated       = 1
      program_not_written         = 2
      table_not_dropped           = 3
      others                      = 4.
  if sy-subrc <> 0.
*   DO902: Table & could not be deleted
    smi1> prid 'E' 'DO902' ddftx.
  else.
*   Read active Nametab of table TABNAME
    call function 'DD_GET_NAMETAB'
      exporting
*       STATUS           = 'A'
        tabname          = ddftx
*       GET_ALL          = ' '
      importing
        x030l_wa         = x030l_wa
      tables
        x031l_tab        = x031l_tab
      exceptions
        not_found        = 1
        no_fields        = 2
        others           = 3.
    if sy-subrc <> 0.
*     DO903: Table & could not be created
      smi1> prid 'E' 'DO903' ddftx.
    else.
      clear rc.
*     Create table DDFTX from active Nametab
      call function 'DD_CREATE_TABLE'
        exporting
          keyfields                  = 'X'
          nullable                   = 'X'
          tabname                    = ddftx
          tab_ind_control            = 'A'
        importing
          subrc                      = rc
        tables
          x031l_tab                  = x031l_tab
        exceptions
          index_already_exists       = 1
          mapping_error              = 2
          table_already_exists       = 3
          table_not_created          = 4
          table_not_found            = 5
          op_failure                 = 6
          others                     = 7.
      if sy-subrc <> 0.
*       DO903: Table & could not be created
        smi1> prid 'E' 'DO903' ddftx.
        rollback work.
      else.
*      DO904: Table & deleted and newly created
       smi1> prid 'N' 'DO904' ddftx.
       genwa-rc = 0.
       done = 'X'.
*      Update of dynpro-timestamps
       perform dynpro_update.
       commit work.
      endif.
    endif.
  endif.

endform.                   " mt_drop_create_ddftx

*&---------------------------------------------------------------------*
*&      Form  dynpro_update
*&---------------------------------------------------------------------*
*       Update of dynpro-timestamp for all tables
*----------------------------------------------------------------------*
form dynpro_update.

  data: crstamp like ddxtt-crstamp,
        dystamp like ddxtt-dystamp.
  data: ddxtt_tab like ddxtt occurs 0 with header line,
        ddxtt_sav like ddxtt occurs 0 with header line.
  data: j type i.

  crstamp = dystamp = syst-datum.
  crstamp+8(6) = dystamp+8(6) = sy-uzeit.

* Active Nametab
  EXEC SQL.
    UPDATE DDNTT SET CRTIMESTMP = :CRSTAMP,
                     DYTIMESTMP = :DYSTAMP
           WHERE TABNAME <> ' '
           AND   TABTYPE <> 'E'
           AND   TABTYPE <> 'L'
  ENDEXEC.

* Inactive Nametab
  select * from ddxtt into table ddxtt_tab                             "#EC CI_NOFIELD
                      where tabname <> ' '.
  loop at ddxtt_tab.
    if ( ddxtt_tab-tabtype = 'E' and ddxtt_tab-tabform = 'J' ) or
       ( ddxtt_tab-tabtype = 'L' and ddxtt_tab-tabform = 'J' ).
      continue.
    endif.
    clear ddxtt_tab-crstamp. clear ddxtt_tab-dystamp.
    move-corresponding ddxtt_tab to ddxtt_sav.
    append ddxtt_sav. j = j + 1.
    if j > 10000.
      update ddxtt from table ddxtt_sav.
      commit work.
      refresh ddxtt_sav. clear ddxtt_sav. clear j.
    endif.
  endloop.
  read table ddxtt_sav index 1.
  if syst-subrc = 0.
    update ddxtt from table ddxtt_sav.
    commit work.
  endif.

endform.                    " dynpro_update

*&---------------------------------------------------------------------*
*&      Form  ddftx_handle_mssql
*&---------------------------------------------------------------------*
*       Handles DDFTX during generic language-transport for database
*       MSSQL
*       Problem: During generic language-transport of data-elements or
*       tables the newly imported texts have to be deleted. From the
*       performance point of view the better solution is to drop and
*       newly create table DDFTX. Unfortunately the recreation leads
*       to errors for MSSQL if a backup is also running. Backup and
*       index-creation can not be executed together.
*       So for MSSQL the proceeding is changed to the following steps:
*       - Copy DDFTX to DDFXT_n (n=1,2,...) where n is the first number
*         for which the table does not exist on the database
*       - Create table DDFTX_n on the database (via activation-tool)
*       - In case of error a backup is possibly running: send error-
*         message into log with the request to drop/create the table
*         later
*       - If DDFTX_n can be created then drop DDFTX on the database
*         and rename DDFTX_n to DDFTX on the database
*       - Delete copy DDFTX_n in dictionary and nametab
*----------------------------------------------------------------------*
form ddftx_handle_mssql using genwa like dcgentb
                              done  like ddrefstruc-bool
                              prid  like syst-tabix.

  data: tabname   like dd02l-tabname,
        ddftx     like dd02l-tabname value 'DDFTX',
        suffix(3) value '&', suffix_n(3), n(1) value '1',
        rc        like syst-subrc,
        got_state like ddrefstruc-state,
        indexname like dd12l-indexname,
        rc_total  like syst-subrc,
        get_state like dctablget.

* Get the name for the DDFTX-copy
  do.
    suffix_n = suffix. clear rc.
    replace '&' with n into suffix_n.
    concatenate ddftx suffix_n into tabname.
    condense tabname no-gaps.
    call function 'DB_EXISTS_TABLE'
      exporting
        tabname       = tabname
      IMPORTING
        SUBRC         = rc.
    if rc = 4.       "Table DDFTX_n does not already exist on DB
      exit.
    else.
      n = n + 1. clear tabname.
    endif.
  enddo.

* Copy DDFTX to DDFTX_N
  get_state = 'A      '.
  call function 'DD_TABL_COPY'
    EXPORTING
      DST_NAME              = tabname
      FORCE                 = 'X'
      GET_STATE             = get_state
      PRID                  = prid
      SRC_NAME              = ddftx
*     WITHTEXT              = ' '
*     LANGU                 = SY-LANGU
*     SOURCE_SYSTEM         = 'NONE'
    EXCEPTIONS
      ILLEGAL_VALUE         = 1
      OP_FAILURE            = 2
      COPYING_REFUSED       = 3
      OTHERS                = 4.
  if sy-subrc <> 0.
*   DO629: Table DDFTX could not be dropped and deleted
    smi0> prid 'N' 'DO629'.
*   DO909: Copy of table DDFTX could not be created
    smi0> prid 'E' 'DO909'.
    exit.
  else.
    update dd02l set authclass = '00' where tabname = tabname.
*   DO911: Copy & of table DDFTX created
    smi1> prid 'N' 'DO911' tabname.
  endif.

* Activate table
  clear rc.
  CALL FUNCTION 'DD_TABL_ACTM'
    EXPORTING
      MODE                   = 17
*     GETSTATE               = 'M'
*     NTAB_PUTSTATE          = 'A'
      PRID                   = prid
*     SETTMST                = '  '
      TABNAME                = tabname
      AUTH_CHK               = ' '
*     EXCOMMIT               = 'X'
    IMPORTING
      ACT_RESULT             = rc
    EXCEPTIONS
      DBCHANGE_FAILURE       = 1
      NTAB_GEN_FAILURE       = 2
      PUT_FAILURE            = 3
      READ_FAILURE           = 4
      ACCESS_FAILURE         = 5
      OTHERS                 = 6.
  if sy-subrc <> 0 or rc > 4.
*   DO629: Table DDFTX could not be dropped and deleted
    smi0> prid 'N' 'DO629'.
*   DO910: Copy of table DDFTX could not be activated
    smi0> prid 'E' 'DO910'.
    exit.
  else.
*   DO912: Copy & of table DDFTX successfully activated
    smi1> prid 'N' 'DO912' tabname.
  endif.

* Drop original table DDFTX
  clear rc.
  call function 'DB_DROP_TABLE'
    exporting
*     DBSYS                       = SY-DBSYS
*     NO_EXEC                     = ' '
*     PRID                        = 0
*     PROGNAME                    = ' '
      tabname                     = ddftx
*     DB_CHECK_FLAG               = ' '
    IMPORTING
*     GENPROG                     =
      SUBRC                       = rc
    EXCEPTIONS
      PROGRAM_NOT_GENERATED       = 1
      PROGRAM_NOT_WRITTEN         = 2
      TABLE_NOT_DROPPED           = 3
      OTHERS                      = 4.
  if sy-subrc <> 0.
*   DO629: Table DDFTX could not be dropped and deleted
    smi0> prid 'N' 'DO629'.
*   DO914: Table DDFTX could not be deleted on the database
    smi0> prid 'E' 'DO914'.
    exit.
  else.
*   DO913: Table DDFTX deleted
    smi0> prid 'N' 'DO913'.
  endif.

* Handle indexes
  do 2 times.
    clear rc.
    if syst-index = 1. indexname = '0'. endif.
    if syst-index = 2. indexname = '1'. endif.
    call function 'DD_DROP_INDEX'
      exporting
        PRID                        = prid
        tabname                     = tabname
        indexname                   = indexname
      IMPORTING
        SUBRC                       = rc
      EXCEPTIONS
        INDEX_NOT_DROPPED           = 1
        PROGRAM_NOT_GENERATED       = 2
        PROGRAM_NOT_WRITTEN         = 3
        OTHERS                      = 4.
    if sy-subrc <> 0 or rc > 4.
      rc_total = 8.
*     DO629: Table DDFTX could not be dropped and deleted
      smi0> prid 'N' 'DO629'.
*     DO919: Index & table & could not be deleted
      smi2> prid 'E' 'DO919' tabname indexname.
    else.
*     DO920: Index & of table & deleted
      smi2> prid 'N' 'DO920' tabname indexname.
    endif.
  enddo.

  if rc_total = 0.    "Indexes of DDFTX_N could be deleted
* Create indexes of copy DDFTX_N with 'wrong' name of destination tab.
    call function 'DB_INDX_SET_CRE'
      EXPORTING
        TABNAME               = tabname
        DBTABNAME             = ddftx
        INDEXNAME             = '*'
        DROP_CREATE           = ' '
        CHECK_EXISTENCE       = 'X'
        PRID                  = prid
      EXCEPTIONS
        INDEX_NOT_FOUND       = 1
        TABLE_NOT_FOUND       = 2
        UNKNOWN_ERROR         = 3
        OP_FAILURE            = 4
        OTHERS                = 5.
    if sy-subrc <> 0.
*     DO629: Table DDFTX could not be dropped and deleted
      smi0> prid 'N' 'DO629'.
*     DO922: Indexes of table & could not be created with names of &
      smi2> prid 'E' 'DO922' tabname ddftx.
    else.
*     DO921: Indexes of table & could be created with names of &
      smi2> prid 'N' 'DO921' tabname ddftx.
    endif.
  endif.

* Rename DDFTX_N to DDFTX
  call function 'DB_RENAME_TABLE'
    exporting
*     DBSYS                          = SY-DBSYS
*     NO_EXEC                        = ' '
      PRID                           = prid
*     TOLERANT                       = ' '
*     PROGNAME                       = ' '
      tabname_new                    = ddftx
      tabname_old                    = tabname
*     LINK_ALLOWED                   = ' '
*   IMPORTING
*     GENPROG                        =
    EXCEPTIONS
      NEW_TABLE_EXISTS               = 1
      OLD_TABLE_DOES_NOT_EXIST       = 2
      PROGRAM_NOT_GENERATED          = 3
      PROGRAM_NOT_WRITTEN            = 4
      OTHERS                         = 5.
  if sy-subrc <> 0.
*   DO629: Table DDFTX could not be dropped and deleted
    smi0> prid 'N' 'DO629'.
*   DO916: Table & could not be renamed to DDFTX on the database
    smi1> prid 'E' 'DO916' tabname.
    exit.
  else.
*   DO915: Table & was renamed to DDFTX on the database
    smi1> prid 'N' 'DO915' tabname.
  endif.

* Delete table DDFTX_N in dictionary and nametab (DB-version already
* used for rename-action)
  call function 'DD_TABL_DEL'
    exporting
      tabname         = tabname
      del_state       = 'M'
      prid            = prid.
  call function 'DD_TBHD_GET'
    EXPORTING
      GET_STATE           = 'A'
      TABL_NAME           = tabname
    IMPORTING
      GOT_STATE           = got_state
    EXCEPTIONS
      ILLEGAL_VALUE       = 1
      OP_FAILURE          = 2
      OTHERS              = 3.
  if got_state <> ' '.
*   DO918: Copy & of DDFTX: DD-sources and runtimeobject not deleted
    smi1> prid 'N' 'DO918' tabname.
  else.
*   DO917: Copy & of DDFTX: DD-sources and runtimeobject deleted
    smi1> prid 'N' 'DO917' tabname.
*   DO904: Table & deleted and newly created
    smi1> prid 'N' 'DO904' ddftx.
    genwa-rc = 0.
    done = 'X'.
*   Update of dynpro-timestamp for all tables
    perform dynpro_update.
    commit work.
  endif.

endform.                    " ddftx_handle_mssql

*&---------------------------------------------------------------------*
*&      Form  ma_objfunc_cluster
*&---------------------------------------------------------------------*
*       Handles OBJFUNC for SQLT if not already done
*----------------------------------------------------------------------*
*      -->GENTAB: Objects to activate
*----------------------------------------------------------------------*
form ma_objfunc_cluster tables gentab structure dcgentb.

  data genwa like dcgentb.

  loop at gentab into genwa where ( type = sqlt or type = tabl )
                            and   ddcall = ' '.
    if syst-uname = 'DDIC' and genwa-objfunc = ' '.
      genwa-objfunc = 'X'.
      modify gentab from genwa.
    endif.
  endloop.

endform.                    " ma_objfunc_cluster

*&---------------------------------------------------------------------*
*&      Form  mt_delete_lang
*&---------------------------------------------------------------------*
*       From release 5.0 on the language-handling is changed concerning
*       DDFTX. Drop and Create of that table has often ended in
*       problems especially when being combined with a necessary
*       update of all dynpro-timestamps of tables and structures.
*       Both action have often caused lock-problems or the drop/create
*       could not be executed because of parallel database action such
*       as backups in MSSQL to give an example.
*       So we change the procedure to:
*       Delete texts of actual language from DDFTX
*       The dynpro-loads of the languages being concerned are deleted
*       by R3trans so dynpro-timestamp-updates are no longer necessary.
*----------------------------------------------------------------------*
*      -->GENWA : DTEL/TABL * to handle and additional information
*      -->ACWA  : Activation-information
*      -->PRID  : Id for log-writer
*----------------------------------------------------------------------*
form mt_delete_lang using genwa like dcgentb
                          acwa  like ddmassac
                          prid  like syst-tabix.

  data: rc        like syst-subrc,
        x030l_wa  like x030l,
        x031l_tab like x031l occurs 0 with header line,
        ddftx     like dd02l-tabname value 'DDFTX'.

  check genwa-pgmid = lang. check genwa-name = '*'.
  if genwa-type <> dtel and genwa-type <> tabl. exit. endif.

  genwa-rc = 8.
  clear acwa-wr_actflg. clear acwa-maxflg.
  newob> prid.
  if genwa-type = dtel.
*   DO901: Import of all dataelement-texts
    smi0> prid 'N' 'DO901'.
  elseif genwa-type = tabl.
*   DO906: Import of all table-texts
    smi0> prid 'N' 'DO906'.
  endif.

* To get rid of locks on DDFTX execute commit
  commit work.

* DO926: Delete of entries of runtimeobject with screen-painter-
*        texts in language &
  smi1> prid 'N' 'DO926' genwa-lang.

* Deletion of entries in current language
  delete from ddftx where ddlanguage = genwa-lang.                     "#EC CI_NOFIRST
  if sy-subrc <> 0.
*   <KH> 20020201 Send Info instead of error. In case of first import
*   or if we have an exotic language with wich has not already been
*   worked it is possible that there are no texts.
*   The case there are texts we could not delete should not occur
*   or database has severe problems.
*   DO927: Screen-Painter-Texts in language & could not be deleted
    smi1> prid 'N' 'DO927' genwa-lang.
    genwa-rc = 0.
    commit work.
*   rollback work.
  else.
*   DO928: Screen-Painter-Texts in language & successfully deleted
    smi1> prid 'N' 'DO928' genwa-lang.
    genwa-rc = 0.
    commit work.
  endif.

endform.                    " mt_delete_lang

*&---------------------------------------------------------------------*
*&      Form  ma_overtake_switch_dep_objects
*&---------------------------------------------------------------------*
*       Overtakes switch-dependent ddic objects into gentab
*       The switches are contained in GENTAB and DELTAB dependent wether
*       they have to be activated or deleted. All switches have to be given to
*       method activate_sfw in table SWITCHES_GENTAB because this method does
*       compute itself wether a switch has to be activated or deleted. The
*       method also computes the dependent Ddic-objects to activate which
*       are then overtaken to GENTAB. The switches are deleted from GENTAB
*       and DELTAB but they remain in SWITCHES_GENTAB and SWITCHES_DEPTAB.
*       Before analysing the results the switches are overtaken again in
*       table GENTAB. SWITCHES_DELTAB is then used to write a correct end
*       message.
*----------------------------------------------------------------------*
*      <->GENTAB: Objects to activate, including the switches, exported
*                 GENTAB does no longer contain switch-objects
*      <->DELTAB: Objects to delete, including switched to delete, exported
*                 DELTAB does not contain switch-objects
*      <--SWITCHES_GENTAB: Contains dependent ddic-objects for all switches
*                          which were activated with RC 0 or 4 and switches themselves
*      <--SWITCHES_DEPTAB: Contains dependencies switch - ddic-object
*      <--SWITCHES_DELTAB: Contains switches to be deleted
*      <--RC  : Returncode, >4 critical error, stop whole activation
*      <->PRID: Id for log-writer
*----------------------------------------------------------------------*
form ma_overtake_switch_dep_objects tables   gentab          structure dcgentb
                                             deltab          structure dcdeltb
                                             switches_gentab structure dcgentb
                                             switches_deptab structure dcdeptb
                                             switches_deltab structure dcdeltb
                                    using    rc              like syst-subrc
                                             prid            like syst-tabix
                                    changing cnt_del         type i.

  data: logname like ddmass-logname,
        logreal like ddmass-logname,
        phase_txt(89),
        cnt_act type i,
        cnt_overtake type i.

  refresh switches_gentab. clear switches_gentab.
  refresh switches_deptab. clear switches_deptab.
  refresh switches_deltab. clear switches_deltab.

* Overtake the switches to a special gentab and delete them from original
  loop at gentab where type = sf01 or
                       type = sfsw or
                       type = sfbs or
                       type = sfbf or
                       type = sf02 or
                       type = enhd.
    move-corresponding gentab to switches_gentab.
    switches_gentab-actkind = 'N'.  "For later binary search
    append switches_gentab.
    delete gentab.
  endloop.
  cnt_act = lines( switches_gentab ).

* Overtake the switches to delete to a special gentab and delete them from original
  clear cnt_del.
  loop at deltab where objtyp = sf01 or
                       objtyp = sfsw or
                       objtyp = sfbs or
                       objtyp = sfbf or
                       objtyp = sf02 or
                       objtyp = enhd.
    switches_gentab-pgmid = deltab-pgmid.
    switches_gentab-type = deltab-objtyp.
    switches_gentab-name = deltab-objname.
    switches_gentab-local = 'N'.
    switches_gentab-ddcall = 'I'.
    switches_gentab-actkind = 'N'.  "For later binary search
    switches_gentab-tabix = deltab-e071_index.
    append switches_gentab.
    move-corresponding deltab to switches_deltab.
    append switches_deltab.
    delete deltab.
  endloop.
  cnt_del = lines( switches_deltab ).

  if cnt_act = 0 and cnt_del = 0.
    exit.
  endif.

  phase_txt = 'Übernahme der Switch-abhängigen Objekte'(059).
  phase_header> prid phase_txt.
  smi2> prid 'N' 'DO654' cnt_act cnt_del.


* Get dependent ddic-objects:
* If switch could be activated GENTAB-RC will be 0 or 4, the dependent
* objects will be delivered. If switch could no be activated GENTAB-RC
* is 8 and dependent objects are returned.
  clear rc.
  perform activate_sfw(SFW_DDIC_IMPORT) tables switches_deptab switches_gentab
    using prid rc.
  if rc > 4.
    smi0> prid 'E' 'DO671'.
    exit.         "Critical error in SWITCH activation, stop further actions
  endif.

  sort switches_deptab by type name indx.
  delete adjacent duplicates from switches_deptab.
* switches_gentab now contains the dictionary objects. Overtake them into
* gentab
  sort gentab by type name indx.
  sort deltab by objtyp objname indxname.
  loop at switches_gentab.
    if switches_gentab-type <> sf01 and switches_gentab-type <> sfsw and
       switches_gentab-type <> sfbs and switches_gentab-type <> sfbf and
       switches_gentab-type <> sf02 and switches_gentab-type <> enhd.
      read table gentab with key type = switches_gentab-type
                                 name = switches_gentab-name
                                 indx = switches_gentab-indx.
      if syst-subrc <> 0. "Object not in GENTAB
        read table deltab with key objtyp   = switches_gentab-type
                                   objname  = switches_gentab-name
                                   indxname = switches_gentab-indx
                                   binary search.
        if syst-subrc <> 0. "Object not in DELTAB also
          move-corresponding switches_gentab to gentab.
          gentab-switchdep = 'X'.
          append gentab.
          cnt_overtake = cnt_overtake + 1.
        endif.
      endif.
    endif.
  endloop.
  smi1> prid 'N' 'DO658' cnt_overtake.

  phase_tail> prid phase_txt space.

endform.

*&---------------------------------------------------------------------*
*&      Form  ma_overtake_switch_objects
*&---------------------------------------------------------------------*
*       Overtakes switch-objects to gentab for result analyzation
*       Objects from former deltab are also in GENTAB now. With help
*       of internal table SWITCHES_DELTAB they are analysed later.
*----------------------------------------------------------------------*
*      -->SWITCHES_GENTAB: Objects dependent of switches, switches
*      -->SWITCHES_DELTAB: Objects dependent of deleted switches
*      <->GENTAB :   Objects to activate, including the switches
*      -->PERSIST:   'X', write GENTAB to database
*      -->NOTFILL:   Dependent objects and GENTAB objects are already
*                    selected and stored to database
*      <--OKTAB  :   Switch Objects are not handled in level activation,
*                    only the dependent Dictionary Objects are activated
*                    in level activation. Objects without dependent
*                    objects are set to o.k. after each level.
*                    Switch Objects without dependencies to Dictionary
*                    Objects are now collected in OKTAB
*      <--SFW_TFILL: Lines of swicth framework objects
*----------------------------------------------------------------------*
form ma_overtake_switch_objects tables   gentab structure dcgentb
                                         switches_gentab structure dcgentb
                                         switches_deltab type deltb
                                         deptab structure dcdeptb
                                         switches_deptab structure dcdeptb
                                         oktab type oktb
                                using    persist notfill
                                changing sfw_tfill type sytabix.

  data: dbdcgenwa  type dbdcgentb,
        dbdcgentab type dbdcgentb occurs 0,
        ok_wa      type okwa.

  clear sfw_tfill.
  loop at switches_gentab where type = sf01 or
                                type = sfsw or
                                type = sfbs or
                                type = sfbf or
                                type = sf02 or
                                type = enhd.
    move-corresponding switches_gentab to gentab.
    append gentab.
    read table switches_deltab transporting no fields
                               with key objtyp  = switches_gentab-type
                                        objname = switches_gentab-name.
    if syst-subrc <> 0.
      sfw_tfill =  sfw_tfill + 1.
    endif.
    if persist = 'X'.
      move-corresponding switches_gentab to dbdcgenwa.
      append dbdcgenwa to dbdcgentab.
    endif.
    read table switches_deptab transporting no fields
                               with key type = switches_gentab-type
                                        name = switches_gentab-name.
    if syst-subrc <> 0. "No dependent objects
      move-corresponding switches_gentab to ok_wa.
      append ok_wa to oktab.
    endif.
  endloop.
  if persist = 'X'.
    if notfill = 'X'.
      delete dbdcgentb from table dbdcgentab.
      insert dbdcgentb from table dbdcgentab.
    else.
      insert dbdcgentb from table dbdcgentab
      ACCEPTING DUPLICATE KEYS.
    endif.
  endif.

  free switches_gentab.

  loop at switches_deptab where type = sf01 or
                                type = sfsw or
                                type = sfbs or
                                type = sfbf or
                                type = sf02 or
                                type = enhd.
    move-corresponding switches_deptab to deptab.
    append deptab.
  endloop.

  free switches_deptab.

endform.

*&---------------------------------------------------------------------*
*&      Form  ma_switches_write_result
*&---------------------------------------------------------------------*
*       Writes results for switches
*----------------------------------------------------------------------*
*      -->SWITCHES_DELTAB: Table with switches to delete
*      -->RC: Returncode of activation for current object
*      -->DDTYPE: Text for current type
*      -->PRID: Id for log-writer
*----------------------------------------------------------------------*
form ma_switches_write_result tables switches_deltab type deltb
                              using genwa like dcgentb
                                    ddtype
                                    prid like syst-tabix.

  read table switches_deltab with key pgmid    = genwa-pgmid
                                        objtyp   = genwa-type
                                        objname  = genwa-name
                                    binary search.
  if genwa-rc = 8.
    if syst-subrc = 0.
      smi2> prid 'E' 'DO655' ddtype genwa-name.
      perform info_mess_write using genwa prid.
    else.
      smi2> prid 'E' 'DO519' ddtype genwa-name.
      perform info_mess_write using genwa prid.
    endif.
  elseif genwa-rc = 6.
    if syst-subrc = 0.
      smi2> prid 'E' 'DO655' ddtype genwa-name.
      perform info_mess_write using genwa prid.
    else.
      smi2> prid 'W' 'DO517' ddtype genwa-name.
      perform info_mess_write using genwa prid.
    endif.
  elseif genwa-rc = 4.
    if syst-subrc = 0.
      smi2> prid 'E' 'DO657' ddtype genwa-name.
      perform info_mess_write using genwa prid.
    else.
      smi2> prid 'W' 'DO520' ddtype genwa-name.
      perform info_mess_write using genwa prid.
    endif.
  endif.

endform.


*&---------------------------------------------------------------------*
*&      Form  ma_put_switches
*&---------------------------------------------------------------------*
*       During mass-activation the switch-dependent ddic objects are
*       computed. In case of a mass-activation restart this analysis has
*       to be done again. This is only possible if the switches are written
*       in a temporary version. The below method is called if activation
*       is successful and it calls a switch method which transfers the
*       temporary switch versions in their final state.
*----------------------------------------------------------------------*
*      -->SWITCHES_GENTAB: Objects dependent of switches, switches
*      -->PERSIST        : 'X', GENTAB information is stored on database
*      -->PRID: Id for log writer
*----------------------------------------------------------------------*
form ma_put_switches tables switches_gentab type gentb
                                            using persist prid like syst-tabix.

  data: gentab_loc type gentb,
        genwa like dcgentb.

  loop at switches_gentab into genwa
                          where type = sf01 or
                                type = sfsw or
                                type = sfbs or
                                type = sfbf or
                                type = sf02 or
                                type = enhd.
    if genwa-rc <= 4.
      append genwa to gentab_loc.
    endif.
  endloop.

* Call method to set switches to fully active.
  cl_sfw_ddic_import=>finalize_import( p_dcgentb = gentab_loc[] ).

  if persist = 'X'.
    free switches_gentab.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  ma_handle_derived_type
*&---------------------------------------------------------------------*
*       Views can have LOB structures as dependent objects. If such object
*       exist the view is marked with an ACTFLAG and this ACTFLAG is
*       then transferred to the dependent LOB structure entries in DEPTAB.
*       With this mechanism the LOB structures are then activated as
*       dependent objects later.
*----------------------------------------------------------------------*
*      <->GENWA: Information about object currently activated
*      -->PRID : Id for log writer
*----------------------------------------------------------------------*
form mt_handle_derived_type using genwa type dcgentb prid type sytabix.

  select * from dd02l where sqltab = genwa-name and tabclass = 'INTTAB'.
  endselect.
  if syst-subrc = 0.
    genwa-actflag = 'V'.
    perform set_actflag using genwa 'V' prid.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  GET_TABLES_OF_BUFFERED_VIEWS
*&---------------------------------------------------------------------*
*       Get basetables of buffered views which are deleted
*       and which are not contained in any other buffered view.
*----------------------------------------------------------------------*
*    -->DELTAB   : Objects to delete
*    -->DELVIEWS : Names of views to delete
*    <--BASETABS : Basistables of buffered views to delete
*----------------------------------------------------------------------*
form get_tabs_of_buffered_views using deltab    type deltb
                                      delviews  type viewtb
                                      basetabs  type gentb.

  data: view_wa type viewn_wa,
        basetabs_wa type dcgentb,
        tabnames type table of ddtabname,
        name(30),
        delwa type dcdeltb.

  read table delviews into view_wa index 1.
  check syst-tabix > 0.

  call function 'DD_RANGE_SELECT'
      exporting
           pointer       = 'NAME'
           formid        = 'MA_TABLES_OF_BUFFERED_VIEWS'
           repid         = 'RADMASDSC'
      tables
           result_tab    = tabnames
           symbol_tab    = delviews
      exceptions
           illegal_value = 1
           others        = 2.
  if syst-subrc <> 0.
*  Message
  endif.
  read table tabnames into name index 1.
  if syst-subrc <> 0.
    exit.
  endif.

  loop at tabnames into name.
    read table deltab into delwa with key objtyp  = tabl
                                          objname = name
                                 binary search.
    if syst-subrc <> 0. "Only tables which are not deleted
      basetabs_wa-name    = name.  "are stored
      basetabs_wa-type    = tabl.
      basetabs_wa-local   = 'A'.
      basetabs_wa-actkind = 'A'.
      basetabs_wa-action  = 'V'. "Header update for buffered views
      append basetabs_wa to basetabs.
    endif.
  endloop.

endform.                    " GET_TABS_OF_BUFFERED_VIEWS

*&---------------------------------------------------------------------*
*&      Form  HANDLE_TABLES_OF_BUFFERED_VIEWS
*&---------------------------------------------------------------------*
*       Handles basetables of deleted buffered views
*----------------------------------------------------------------------*
*    -->DELTAB    : Objects to delete
*    -->BASETABLES: Basetables of deleted views
*    <--NTTAB     : Objects with nametab operations
*    --> INACTIVE : 'X': Nametab has to be written inactive, ' ': active
*    --> PRID     : Id for log-writer
*----------------------------------------------------------------------*
form handle_tabs_of_buffered_views using deltab     type deltb
                                         basetables type gentb
                                         nttab      type ntdeltb
                                         inactive   type ddinactive
                                         prid       type sytabix.

  data: basetables_wa type dcgentb,
        view_tab_name type view_tab_t,
        view_tab_names type table of view_tab_t,
        delwa type dcdeltb,
        level type ddlevelnr value 0,
        ntwa type dcntdeltb.

  read table basetables into basetables_wa index 1.
  if syst-subrc <> 0.
    exit.
  endif.

  call function 'DD_RANGE_SELECT'
      exporting
           pointer       = 'NAME'
           formid        = 'MA_BUFFERED_VIEWS_FOR_TABLES'
           repid         = 'RADMASDSC'
      tables
           result_tab    = view_tab_names
           symbol_tab    = basetables
      exceptions
           illegal_value = 1
           others        = 2.
  if syst-subrc <> 0.
*  Message
  endif.

* Handle all basetables which are no longer contained in buffered view. If view
* does still exist during first deletion step a second analysis is done in second
* call of deletion
  newob> prid.
  sort view_tab_names by tabname.
  loop at basetables into basetables_wa.
    read table view_tab_names into view_tab_name
                               with key tabname = basetables_wa-name.
    if syst-subrc <> 0.
      perform ntab_set_header(saplsdmp) tables basetables
                                        using  basetables_wa inactive level prid.
      ntwa-type       = tabl.
      ntwa-name       = basetables_wa-name.
      ntwa-ntabaction = 'V'.
      append ntwa to nttab.
      delete basetables where type = tabl
                        and   name = basetables_wa-name.
    endif.
  endloop.
  newob> prid.

endform.
