*&---------------------------------------------------------------------*
*& Include          ZMPLC_PROCESS_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      CLEAR: gs_1001, gs_1002, gs_1003, gs_1004, gs_1005, s_ebeln-low,
      inp_pi, inp_lc, bank_ven, ins_ven, sup_bank,s_pi-low, zlc_process-pi_dt.
      REFRESH: it_podet ,it_podet1, s_pi[], s_ebeln[].
      LEAVE TO SCREEN 9001.
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SRCH'.
      PERFORM search_pi.
    WHEN 'SRCH2'.
      PERFORM search_1001.
    WHEN 'SAVE'.
      PERFORM create_pi.
    WHEN ''.
      if it_podet1[] is NOT INITIAL.
      REFRESH it_podet.
      ENDIF.
      IF s_ebeln IS NOT INITIAL.
        PERFORM search_pi.
      ELSEIF inp_pi IS NOT INITIAL.
        PERFORM search_1001.
      ENDIF.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  gv_screenmode = sy-ucomm.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'PI_CRT'.
      CLEAR gv_disp_flag.
      CALL SCREEN '1001'.
    WHEN 'PI_MOD'.
      CLEAR gv_disp_flag.
      CALL SCREEN '1001'.
    WHEN 'PI_DISP'.
      gv_disp_flag = abap_true.
      CALL SCREEN '1001'.
    WHEN 'PI_CANC'.
      CALL SCREEN '9003'.
    WHEN 'PI_LIST'.
      CALL SCREEN '9005'.
    WHEN 'BANK_CRT'.
      CLEAR:  gs_1002,inp_pi,bank_ven.
      CLEAR s_pi-low.
      REFRESH s_pi[].
      REFRESH: it_podet,gt_1002.
      CLEAR : gv_mod_flag,gv_disp_flag.
      gv_crt_flag = abap_true.
      gv_screen = '1002'.
      CALL SCREEN '1002'.
    WHEN 'BANK_MOD'.
      CLEAR:  gs_1002,inp_pi,bank_ven.
      CLEAR s_pi-low.
      REFRESH s_pi[].
      REFRESH: it_podet,gt_1002.
      CLEAR : gv_crt_flag,gv_disp_flag.
      gv_mod_flag = abap_true.
      gv_screen = '1002'.
      CALL SCREEN '1002'.
    WHEN 'BANK_DISP'.
      CLEAR:  gs_1002,inp_pi,bank_ven.
      CLEAR s_pi-low.
      REFRESH s_pi[].
      REFRESH: it_podet,gt_1002.
      CLEAR : gv_crt_flag,gv_mod_flag.
      gv_disp_flag = abap_true.
      gv_screen = '1002'.
      CALL SCREEN '1002'.
    WHEN 'INS_CRT'.
      CLEAR gv_disp_flag.
      CALL SCREEN '1003'.
    WHEN 'INS_MOD'.
      CLEAR gv_disp_flag.
      CALL SCREEN '1003'.
    WHEN 'INS_DISP'.
      gv_disp_flag = abap_true.
      CALL SCREEN '1003'.
    WHEN 'LC_CRT'.
      CLEAR gv_disp_flag.
      CLEAR gv_mod_flag.
      CLEAR:  gs_1004,gw_1004,
      inp_pi, inp_lc, bank_ven, ins_ven, sup_bank.
      CLEAR s_pi-low.
      REFRESH s_pi[].
      REFRESH: it_podet,gt_1004.
      gv_crt_flag = abap_true.
      gv_screen = '1004'.
      CALL SCREEN '1004'.
    WHEN 'LC_MOD'.
      CLEAR gv_disp_flag.
      CLEAR:  gs_1004,gw_1004,
      inp_pi, inp_lc, bank_ven, ins_ven, sup_bank.
      CLEAR s_pi-low.
      REFRESH s_pi[].
      REFRESH: it_podet,gt_1004.
      gv_mod_flag = abap_true.
      gv_screen = '1004'.
      CALL SCREEN '1004'.
    WHEN 'LC_DISP'.
      CLEAR gv_mod_flag.
      CLEAR:  gs_1004,gw_1004,
      inp_pi, inp_lc, bank_ven, ins_ven, sup_bank.
      CLEAR s_pi-low.
      REFRESH s_pi[].
      REFRESH: it_podet,gt_1004.
      gv_disp_flag = abap_true.
      gv_screen = '1004'.
      CALL SCREEN '1004'.
    WHEN 'LC_CANC'.
      CALL SCREEN '9004'.
    WHEN 'LC_LIST'.
      CALL SCREEN '9006'.
    WHEN 'LC_TRACK'.
      CALL SCREEN '1005'.
    WHEN 'LC_REP'.
      CALL SCREEN '9002'.
    WHEN 'LC_CLOSE'.
      CALL SCREEN '9008'.
    WHEN 'LC_TRACK_REP'.
      CALL SCREEN '9007'.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1002 INPUT.
  ok_code = sy-ucomm.
  CASE sy-ucomm.
    WHEN 'CLEAR'.
      PERFORM clear_1002.
      CLEAR: ok_code,sy-ucomm.
    WHEN 'BACK'.
      PERFORM clear_1002.
      LEAVE TO SCREEN 9001.
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
**SAVE
    WHEN 'SAVE_1002' OR 'SAVE'.
      PERFORM validate_piven.
      PERFORM save_1002.
      PERFORM clear_1002.
      PERFORM clear_var_1002.
      CLEAR : gs_1002.
      CLEAR sy-ucomm.

**Create
    WHEN 'BANK_CRT'.
      PERFORM clear_var_1002.
      IF s_pi[] IS NOT INITIAL.
        PERFORM validate_pi_1002.
        PERFORM validate_pibank_1002.
      ENDIF.
      IF s_pi[] IS NOT INITIAL.
        PERFORM srch_1002.
      ENDIF.
      IF gs_1002 IS INITIAL.
        MESSAGE i021(zabap).
      ENDIF.
**Update/Display
    WHEN ''
    OR 'BANK_DISP'
    OR 'BANK_MOD'
    OR 'EXEC'.
      CLEAR : gs_1002.
      IF s_pi IS NOT INITIAL.
        PERFORM validate_piven.
        PERFORM validate_pi_1002.
*        PERFORM validate_pibank_1002.
      ENDIF.
      PERFORM srch_1002.
      IF gs_1002 IS INITIAL.
        MESSAGE i021(zabap).
      ENDIF.
    WHEN '%00191010001824563'.
      CLEAR : gs_1002.
      PERFORM clear_var_1002.
    WHEN 'BANK_LTR'.
      PERFORM bank_letter.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1003 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR: gs_1001, gs_1002, gs_1003, gs_1004, gs_1005,
      inp_ebeln, inp_pi, inp_lc, bank_ven, ins_ven, sup_bank, s_pi-low,
      ins_add, ins_bank_add, ins_comp, ins_exch_rate, ins_exp_date, ins_note_date,
      ins_note_no, ins_note_val, ins_rcpt_no, ins_remarks, ins_ven.
      REFRESH: it_podet, it_insdet, gt_1003, it_insdet1, s_pi[].
      LEAVE TO SCREEN 9001.

    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SRCH' OR ''.
      PERFORM validate_piven.
      PERFORM srch_1003.
    WHEN 'SAVE'.
      PERFORM validate_piven.
      PERFORM save_1003.
    WHEN 'INS_LTR'.
      PERFORM ins_letter.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1004 INPUT.
  CASE ok_code.
    WHEN 'CLEAR'.
      PERFORM clear_1004.
      CLEAR: sy-ucomm,ok_code.
**Back
    WHEN 'BACK'.
      PERFORM clear_1004.
      LEAVE TO SCREEN 9001.
**Cancel
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
**Exit
    WHEN 'EXIT'.
      LEAVE PROGRAM.
**save
    WHEN 'SAVE_1004' OR 'SAVE'.
      PERFORM clear_1004_var.
      PERFORM save_1004.
      CLEAR : sy-ucomm,ok_code.
***Create
    WHEN  'LC_CRT' .
      CLEAR gs_1004.
      IF s_pi[] IS NOT INITIAL.
        PERFORM validate_piven.
        PERFORM validate_pi_1004.
      ELSE.
        MESSAGE i020(zabap) DISPLAY LIKE 'E'.
      ENDIF.

      IF s_pi[] IS NOT INITIAL.
        PERFORM srch_1004.
      ELSE.
        MESSAGE i020(zabap) DISPLAY LIKE 'E'.
      ENDIF.

**Enter/Execute/Display/Update
    WHEN ''  "Pressing enter
    OR 'LC_DISP'
    OR 'LC_MOD'
    OR 'EXEC'.
      CLEAR gs_1004.
**Create
      IF gv_mod_flag NE 'X' AND gv_disp_flag NE 'X'.
        IF inp_lc IS NOT INITIAL AND s_pi IS INITIAL .
          CLEAR inp_lc.
          MESSAGE i020(zabap).
        ENDIF.
      ENDIF.

**Validate the entered PIs
      IF s_pi[] IS NOT INITIAL .
        PERFORM validate_pi_1004.
      ENDIF.

**Display information
      IF s_pi[] IS NOT INITIAL .
        PERFORM srch_1004.
        IF gs_1004 IS INITIAL .
          MESSAGE i021(zabap).
          CLEAR inp_lc.
        ENDIF.
      ENDIF.

**display/update
      IF gv_mod_flag EQ 'X' OR gv_disp_flag EQ 'X' .
        IF inp_lc IS NOT INITIAL.
          PERFORM validate_lc_1004.
          PERFORM srch_1004.
          IF gs_1004 IS INITIAL .
            MESSAGE i021(zabap).
            CLEAR inp_lc.
          ENDIF.
        ELSE.
          CLEAR inp_lc.
          MESSAGE i020(zabap) DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 1004.
        ENDIF.
      ENDIF.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1005 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR: gs_1001, gs_1002, gs_1003, gs_1004, gs_1005, inp_ebeln, inp_pi, inp_lc, bank_ven, ins_ven, sup_bank, zlc_process.
      REFRESH: it_podet, it_lctrack, it_lctrack1.
      LEAVE TO SCREEN 9001.

    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM save_1005.
    WHEN ''.
      PERFORM srch_1005.
    WHEN 'INS_PO'.
      PERFORM ins_po_letter.
    WHEN 'END_COP'.
*      PERFORM end_copy_doc.
      PERFORM end_copy_doc_n USING 1.
    WHEN 'END_SHP'.
*      PERFORM end_shp_doc.
      PERFORM end_copy_doc_n USING 2.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form SRCH_1005
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM srch_1005 .
**  SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1005 WHERE lc_no = @inp_lc.
*  SELECT * FROM zlc_process INTO CORRESPONDING FIELDS OF TABLE @gt_1005 WHERE lc_no = @inp_lc.
*  IF sy-subrc NE 0.
*    MESSAGE w008(zelc) WITH inp_lc.
*    CLEAR: gs_1005.
*    CALL SCREEN 1005.
*  ELSE.
*    LOOP AT gt_1005 INTO gs_1005.
*      zlc_process-eta = gs_1005-eta.
*      zlc_process-freight_date = gs_1005-freight_date.
*      zlc_process-cpy_doc_date = gs_1005-cpy_doc_date.
*      zlc_process-doc_hand_date = gs_1005-doc_hand_date.
*      zlc_process-appl_dp_date = gs_1005-appl_dp_date.
*      zlc_process-duty_pymt_date = gs_1005-duty_pymt_date.
*      gs_1005-etd = gs_1005-ship_date.
*
*      wa_lctrack-comm_inv_no = gs_1005-comm_inv_no.
*      wa_lctrack-comm_inv_date = gs_1005-comm_inv_date.
*      wa_lctrack-comm_inv_val = gs_1005-comm_inv_val.
*      wa_lctrack-assemble_value = gs_1005-assemble_value.
*      wa_lctrack-total_duty = gs_1005-total_duty.
*      wa_lctrack-bl_awb_no = gs_1005-bl_awb_no.
*      wa_lctrack-bl_awb_date = gs_1005-bl_awb_date.
*      wa_lctrack-boe_no = gs_1005-boe_no.
*      wa_lctrack-boe_date = gs_1005-boe_date.
*      wa_lctrack-blklist = gs_1005-blklist.
*      APPEND wa_lctrack TO it_lctrack.
*      CLEAR wa_lctrack.
*    ENDLOOP.
*  ENDIF.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 9001.
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM lc_report.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9003 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 9001.
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM canc_pi.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9004 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 9001.
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM canc_lc.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9005 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 9001.
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM list_pi.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9006  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9006 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 9001.
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM list_lc.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9007  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9007 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 9001.
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM track_rep.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9008  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9008 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 9001.
    WHEN 'CANC'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM close_lc.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PODET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE podet INPUT.

  READ TABLE it_podet1 INTO DATA(wa_podet1) WITH KEY pi  =  wa_podet-pi
                                                    ebeln = wa_podet-ebeln
                                                    ebelp = wa_podet-ebelp.
  IF sy-subrc = 0.
    MODIFY it_podet1 FROM wa_podet INDEX sy-tabix TRANSPORTING lstel vstel inco lddat ship_mode hs_code.
  ELSE.
    APPEND wa_podet TO it_podet1.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INSDET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE insdet INPUT.
  READ TABLE it_insdet1 INTO DATA(wa_insdet1) WITH KEY pi = wa_insdet-pi
                                                       ebeln = wa_insdet-ebeln
                                                       ebelp = wa_insdet-ebelp.
  IF sy-subrc = 0.
    MODIFY it_insdet1 FROM wa_insdet INDEX sy-tabix TRANSPORTING risk_type amend_no policy_date.
  ELSE.
    APPEND wa_insdet TO it_insdet1.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LCTRACK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lctrack INPUT.
  READ TABLE it_lctrack1 INTO DATA(wa_lctrack1) WITH KEY pi = wa_lctrack-pi
                                                       ebeln = wa_lctrack-ebeln
                                                       ebelp = wa_lctrack-ebelp.
  IF sy-subrc = 0.
    MODIFY it_lctrack1 FROM wa_lctrack INDEX sy-tabix TRANSPORTING comm_inv_no
comm_inv_date
comm_inv_val
bl_awb_no
bl_awb_date
boe_no
boe_date
assemble_value
total_duty.
  ELSE.
    APPEND wa_lctrack TO it_lctrack1.
  ENDIF.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_1004'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc_1004_modify INPUT.
  MODIFY gt_1004
    FROM gw_1004
    INDEX tc_1004-current_line.
ENDMODULE.

*&SPWIZARD: INPUT MODUL FOR TC 'TC_1004'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc_1004_mark INPUT.
  DATA: g_tc_1004_wa2 LIKE LINE OF gt_1004.
  IF tc_1004-line_sel_mode = 1
  AND gw_1004-mark = 'X'.
    LOOP AT gt_1004 INTO g_tc_1004_wa2
      WHERE mark = 'X'.
      g_tc_1004_wa2-mark = ''.
      MODIFY gt_1004
        FROM g_tc_1004_wa2
        TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  MODIFY gt_1004
    FROM gw_1004
    INDEX tc_1004-current_line
    TRANSPORTING mark.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'TC_1004'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc_1004_user_command INPUT.
  ok_1004 = sy-ucomm.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc_1004 USING 'TC_1004'
                                'GT_1004'
                                'MARK'
                       CHANGING ok_code.
  sy-ucomm = ok_1004.
  sy-ucomm = ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_LC_BANK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_lc_bank INPUT.
  READ TABLE gt_vendor INTO gs_vendor WITH KEY partner = bank_ven.
  IF sy-subrc = 0.
    gs_1002-lc_bank = gs_vendor-partner.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_LC_BANK_ADD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_lc_bank_add INPUT.

  IF bank_ven IS NOT INITIAL AND
     gs_bank-lifnr = bank_ven.
    CONCATENATE gs_bank-stras  gs_bank-regio gs_bank-ort01 gs_bank-pstlz INTO gs_1002-lc_bank_add SEPARATED BY ','.
  ENDIF.
ENDMODULE.
