*&---------------------------------------------------------------------*
*& Include          ZMPLC_PROCESS_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITLE_9001'.

  PERFORM hdr_pic.
  PERFORM ttl_pic.
  CLEAR: gv_disp_flag, gv_mod_flag, gv_save.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_1001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
  DATA: lv_ucomm TYPE sy-ucomm.
  SET PF-STATUS 'ZSTATUS'.

  IF gv_screenmode = 'PI_CRT'.
    SET TITLEBAR 'ZTTL_PI_CRT'.
  ELSEIF gv_screenmode = 'PI_MOD'.
    SET TITLEBAR 'ZTTL_PI_MOD'.
  ELSEIF gv_screenmode = 'PI_DISP'.
    SET TITLEBAR 'ZTTL_PI_DISP'.
  ENDIF.
*  SET TITLEBAR 'ZTITLE'.
  IF sy-ucomm = 'PI_CRT'.
    gs_1001-ernam = sy-uname.
  ENDIF.
  IF sy-ucomm = 'PI_DISP' OR sy-ucomm = 'PI_MOD'.
    CLEAR  zlc_process-pi_dt.
  ENDIF.
  IF sy-ucomm = 'PI_DISP' OR sy-ucomm = 'SAVE'.
    gv_save = 'X'.
  ELSE.
    CLEAR gv_save.
  ENDIF.

  PERFORM get_cortype.
  PERFORM get_paytype.
  PERFORM get_shipmode.

*  CLEAR lv_ucomm.
  IF sy-ucomm = 'PI_DISP' OR sy-ucomm = 'PI_CRT' OR sy-ucomm = 'PI_MOD'.
    lv_ucomm = sy-ucomm.
  ENDIF.

  IF gv_disp_flag = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'GRP'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-group2 = 'G1'.
      IF sy-ucomm = 'PI_MOD'." OR sy-ucomm = 'SRCH'.
        screen-input = '0'.
      ELSE.
*        sy-ucomm = 'SRCH'.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  LOOP AT SCREEN.
    IF screen-group1 = 'G2'.
      IF sy-ucomm = 'PI_DISP' OR sy-ucomm = 'SRCH'.
        screen-invisible = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_1002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1002 OUTPUT.
  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITLE_1002'.

*--Added On 28-01-2021 BY EY_SANTOSH
  LOOP AT SCREEN.
    IF screen-name = 'LBL_PLANT' OR screen-name = 'GS_1002-PLANT_DESC'
    OR screen-name = 'LBL_SHIPMODE' OR screen-name = 'GS_1002-SHIP_MODE'
    OR screen-name = 'LBL_PROFORMA' OR screen-name = 'INP_PI'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*--EOA

*******************************
  IF s_pi-low IS NOT INITIAL.
    gv_pi_1002 = s_pi-low.
  ENDIF.

  IF sy-ucomm = 'BANK_DISP' OR sy-ucomm = 'SAVE'.
    gv_save = 'X'.
  ELSE.
    CLEAR gv_save.
  ENDIF.

  PERFORM get_lctype.
  PERFORM get_retirement.

**Display
  IF gv_disp_flag = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

**disable fields in Create mode,if bank vendor already created.
  IF gv_crt_flag EQ 'X' AND s_pi[] IS NOT INITIAL.
    SELECT SINGLE lc_bank_vendor
    FROM zlc_process
    INTO @DATA(lc_bank_vend)
    WHERE pi_no IN @s_pi[].
    IF lc_bank_vend "gs_1002-lc_bank_vendor
      IS NOT INITIAL.
      LOOP AT SCREEN.
        IF screen-name = 'BANK_VEN'
        OR screen-name = 'GS_1002-LC_BANK'
        OR screen-name = 'GS_1002-LC_BANK_ADD'
        OR screen-name = 'GS_1002-LC_ACC_NO'
        OR screen-name = 'GS_1002-LC_TYPE'
        OR screen-name = 'GS_1002-LC_MARGIN'
        OR screen-name = 'GS_1002-LC_RETIRE'
        OR screen-name = 'GS_1002-BANK_REMARKS'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
      MESSAGE i025(zabap)  .
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_1003 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1003 OUTPUT.

  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITLE_1003'.

  IF sy-ucomm = 'INS_DISP' OR sy-ucomm = 'SAVE'.
    gv_save = 'X'.
  ELSE.
    CLEAR gv_save.
  ENDIF.

  PERFORM get_risktype.

  IF gv_disp_flag = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_1004 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1004 OUTPUT.


  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITLE_1004'.

  ok_1004 = sy-ucomm.
********************************************
**Display & save
  IF sy-ucomm = 'LC_DISP' OR sy-ucomm = 'SAVE'.
    gv_save = 'X'.
  ELSE.
    CLEAR gv_save.
  ENDIF.

********************************************
**Create button
  IF gv_mod_flag NE abap_true
  AND gv_disp_flag NE abap_true.
    IF s_pi IS NOT INITIAL AND inp_lc IS NOT INITIAL.
      LOOP AT SCREEN.
        IF screen-name = 'INP_LC'.
          screen-input = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

********************************************
**Update button
  IF gv_mod_flag = abap_true.
    LOOP AT SCREEN.
      IF screen-group2 = 'G2'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'INP_LC'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'EXECUTE'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

********************************************
**Display button
  IF gv_disp_flag = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'INP_LC'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'EXECUTE'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

********************************************
***Lot number/ LC val fields
*  "Added on 3-01-2021 BY EY_SANTOSH
*  LOOP AT SCREEN.
*    IF screen-name = 'GS_1004-LOT_NO'
*    OR screen-name = 'LBL_LOT_NUMBER'.
*      screen-active = 0.
*    ENDIF.
*    IF screen-name = 'GS_1004-LC_VAL'.
*      screen-input = 0.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.
*
****BOC 03-02-2021
*  IF sy-ucomm EQ 'LC_DISP' OR sy-ucomm = 'LC_MOD'.
*    LOOP AT SCREEN.
*      IF screen-name = 'INP_LC'
*      OR screen-name = 'GS_1004-LC_DATE'
*      OR screen-name = 'GS_1004-LC_EXP_DATE'.
*        screen-required = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDLOOP.
*  ELSE.
*    LOOP AT SCREEN.
*      IF screen-name = 'INP_LC'
*      OR screen-name = 'GS_1004-LC_DATE'
*      OR screen-name = 'GS_1004-LC_EXP_DATE'.
*        screen-required = 1.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDLOOP.
*  ENDIF.
***EOC 03-02-2021
  SORT gt_1004 BY pi_no ebeln ebelp.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_1005 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1005 OUTPUT.
  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITLE_1005'.

  IF sy-ucomm = 'LC_TRACK' OR sy-ucomm = 'SAVE'.
    gv_save = 'X'.
  ELSE.
    CLEAR gv_save.
  ENDIF.

  PERFORM get_decision USING:
        'GS_1005-PSI' 'ZDECISION',
        'GS_1005-FREIGHT' 'ZDECISION',
        'GS_1005-CPY_DOC' 'ZCOPY',
        'GS_1005-RLI_DOC' 'ZFLAG',
        'GS_1005-FA_DCSN' 'ZFLAG',
        'GS_1005-SHIP_DOC' 'ZFLAG',
        'GS_1005-DOC_HAND' 'ZFLAG',
        'GS_1005-APPL_DP' 'ZFLAG',
        'GS_1005-RCPT_DA' 'ZFLAG',
        'GS_1005-DUTY_PYMT' 'ZFLAG',
        'GS_1005-TRN_DCSN' 'ZFLAG'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
  SET PF-STATUS 'ZPF_ALV'.
  SET TITLEBAR 'ZTITLE_9002'.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_1004'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_1004_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_1004 LINES tc_1004-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_1004'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_1004_get_lines OUTPUT.
  g_tc_1004_lines = sy-loopc.
ENDMODULE.
