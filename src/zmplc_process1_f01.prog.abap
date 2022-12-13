*&---------------------------------------------------------------------*
*& Include          ZMPLC_PROCESS_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SEARCH_PI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM search_pi .

*  SELECT * FROM zlc_process INTO CORRESPONDING FIELDS OF TABLE @gt_1001 WHERE ebeln IN @s_ebeln...
  SELECT * FROM zlc_process INTO CORRESPONDING FIELDS OF TABLE @gt_1001 WHERE pi_no EQ @inp_pi.

  IF sy-subrc NE 0.
    IF gv_screenmode NE 'PI_CRT' AND sy-tcode NE 'ZPI'.
      MESSAGE w001(zelc) WITH s_ebeln.
      CALL SCREEN 1001.
    ENDIF.
  ELSE.
*    IF gv_screenmode EQ 'PI_CRT'.    "Commented to allow Multiple PO creation for 1 PO
*      CLEAR gs_1001.
*      REFRESH it_podet.
*      CLEAR: zlc_process-pi_dt, zlc_process-lddat.
*      MESSAGE w007(zelc) WITH gs_1001-pi_no.
*      CALL SCREEN 1001.
*    ENDIF.
  ENDIF.

  IF gv_screenmode EQ 'PI_DISP' OR gv_screenmode = 'PI_MOD'.
    CLEAR: gs_1001-pi_dt.

    READ TABLE gt_1001 INTO DATA(wa_1001) INDEX 1.
    IF sy-subrc = 0.
      IF gs_1001-pi_no IS NOT INITIAL.
        inp_pi = gs_1001-pi_no.
      ENDIF.
      sup_bank = gs_1001-sup_bank_vendor.
      IF zlc_process-pi_dt IS INITIAL.
        zlc_process-pi_dt = wa_1001-pi_dt.
      ENDIF.
      IF zlc_process-lddat IS INITIAL.
        zlc_process-lddat = wa_1001-lddat.
      ENDIF.
      IF gs_1005-pi_amt IS NOT INITIAL.
        gs_1005-pi_amt = wa_1001-pi_amt.
      ENDIF.
      IF gs_1005-pi_qty IS NOT INITIAL.
        gs_1005-pi_qty = wa_1001-pi_qty.
      ENDIF.
    ENDIF.
  ENDIF.
  PERFORM fill_po.

  IF gv_disp_flag = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'GRP'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF sy-ucomm NE 'BACK'.
    IF gs_1001-pi_canc_ind = 'X'.
      CLEAR gs_1001.
      REFRESH it_podet.
      CLEAR: zlc_process-pi_dt, zlc_process-lddat.
      MESSAGE w018(zelc) WITH inp_pi.
    ENDIF.
  ENDIF.

  CALL SCREEN 1001.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HDR_PIC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM hdr_pic .
  DATA: w_lines TYPE i.
  TYPES pict_line(256) TYPE c.

  DATA :
    container TYPE REF TO cl_gui_custom_container,
    editor    TYPE REF TO cl_gui_textedit,
    picture   TYPE REF TO cl_gui_picture,
    pict_tab  TYPE TABLE OF pict_line,
    url(255)  TYPE c.
  DATA: graphic_url(255).
  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.
  DATA: graphic_size TYPE i.
  DATA: l_graphic_xstr TYPE xstring.
  .
  CALL METHOD cl_gui_cfw=>flush.
  CREATE OBJECT:
  container EXPORTING container_name = 'BACKGROUND',
  picture EXPORTING parent = container.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = 'ZLC_LOGO'
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr
*  EXCEPTIONS
*     NOT_FOUND      = 1
*     INTERNAL_ERROR = 2
*     others   = 3
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
  CALL METHOD picture->load_picture_from_url
    EXPORTING
      url = url.
  CALL METHOD picture->set_display_mode
    EXPORTING
      display_mode = picture->display_mode_fit_center.
ENDFORM.

FORM ttl_pic .
  DATA: w_lines TYPE i.
  TYPES pict_line(256) TYPE c.

  DATA :
    container TYPE REF TO cl_gui_custom_container,
    editor    TYPE REF TO cl_gui_textedit,
    picture   TYPE REF TO cl_gui_picture,
    pict_tab  TYPE TABLE OF pict_line,
    url(255)  TYPE c.
  DATA: graphic_url(255).
  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.
  DATA: graphic_size TYPE i.
  DATA: l_graphic_xstr TYPE xstring.
  .
  CALL METHOD cl_gui_cfw=>flush.
  CREATE OBJECT:
  container EXPORTING container_name = 'LOGO',
  picture EXPORTING parent = container.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = 'ZRAD_RBCL'
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr
*  EXCEPTIONS
*     NOT_FOUND      = 1
*     INTERNAL_ERROR = 2
*     others   = 3
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
  CALL METHOD picture->load_picture_from_url
    EXPORTING
      url = url.
  CALL METHOD picture->set_display_mode
    EXPORTING
      display_mode = picture->display_mode_fit_center.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form GET_LCTYPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_lctype .

  REFRESH: gt_dom_val1, gt_dom_val2, gt_lctype.

  CLEAR: gs_lctype.

  IF sy-ucomm = 'BANK_CRT' OR sy-ucomm = 'BANK_MOD' OR sy-tcode = 'ZBANK'.
    gv_id = 'GS_1002-LC_TYPE'.
  ELSEIF sy-ucomm = 'LC_CRT' OR sy-ucomm = 'LC_MOD' OR sy-tcode = 'ZLC_PROC'.
    gv_id = 'GS_1004-LC_TYPE'.
  ENDIF.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZLC_TYPE'
*     TEXT            = 'X'
*     FILL_DD07L_TAB  = ' '
    TABLES
      values_tab      = gt_dom_val1
      values_dd07l    = gt_dom_val2
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT gt_dom_val1 INTO DATA(gs_doma_val).

    gs_lctype-key = gs_doma_val-domvalue_l.
    gs_lctype-text = gs_doma_val-ddtext.

    APPEND gs_lctype TO gt_lctype.
    CLEAR gs_lctype.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gv_id
      values          = gt_lctype
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_1002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_1002 .
  FIELD-SYMBOLS : <lfs_1002_t> TYPE any.

**ensire to update All POs against the same PI
*  IF gs_1002 IS NOT INITIAL.
  LOOP AT gt_1002 ASSIGNING FIELD-SYMBOL(<lfs_1002>).
    gs_1002-ebeln = <lfs_1002>-ebeln.
    gs_1002-ebelp = <lfs_1002>-ebelp.
    gs_1002-pi_no = <lfs_1002>-pi_no.
    gs_1002-bukrs = <lfs_1002>-bukrs.
    gs_1002-werks = <lfs_1002>-werks.
*    data(ls_1002_t) = <lfs_1002>.
*      MOVE-CORRESPONDING gs_1002 TO <lfs_1002>.
    <lfs_1002>-lc_bank_vendor = bank_ven.
    <lfs_1002>-lc_bank = bank_name.
    <lfs_1002>-lc_bank_add = bank_add.
    <lfs_1002>-lc_acc_no = lc_acc_no.
    <lfs_1002>-lc_type = lc_type.
    <lfs_1002>-lc_margin = lc_margin.
    <lfs_1002>-lc_retire = lc_retire.
    <lfs_1002>-bank_remarks = bank_remarks.
  ENDLOOP.
*  ENDIF.

**Save
  IF s_pi-low IS INITIAL.
    MESSAGE w012(zelc).
    CALL SCREEN 1002.
  ELSE.
    gs_1002-pi_no = s_pi-low.
    MODIFY zlc_process FROM TABLE gt_1002.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s003(zelc) WITH gs_1002-pi_no .
    ELSE.
      MESSAGE e009(zelc).
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SRCH_1002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM srch_1002 .
  IF s_pi IS NOT INITIAL .
    IF gv_disp_flag NE 'X' .

      SELECT * FROM zlc_process
      INTO CORRESPONDING FIELDS OF TABLE gt_1002
      WHERE pi_no IN s_pi.

      IF gv_screenmode = 'BANK_MOD'.

*      IF bank_ven IS INITIAL AND bank_name IS INITIAL AND bank_add IS INITIAL AND
        IF lc_acc_no IS INITIAL AND lc_type IS INITIAL AND lc_margin IS INITIAL AND
           lc_retire IS INITIAL AND bank_remarks IS INITIAL.

          READ TABLE gt_1002 INTO gs_1002 INDEX 1.
          IF sy-subrc = 0.
            bank_ven = gs_1002-lc_bank_vendor.
            bank_name = gs_1002-lc_bank.
            bank_add = gs_1002-lc_bank_add.
            lc_acc_no = gs_1002-lc_acc_no.
            lc_type = gs_1002-lc_type.
            lc_margin = gs_1002-lc_margin.
            lc_retire = gs_1002-lc_retire.
            bank_remarks = gs_1002-bank_remarks.
          ENDIF.
        ENDIF.

      ENDIF.


    ELSEIF gv_disp_flag = 'X' .

      SELECT * FROM zlc_process
      INTO CORRESPONDING FIELDS OF TABLE @gt_1002
      WHERE pi_no IN @s_pi.

      READ TABLE gt_1002 INTO gs_1002 INDEX 1.
      IF sy-subrc = 0.
        bank_ven = gs_1002-lc_bank_vendor.
        bank_name = gs_1002-lc_bank.
        bank_add = gs_1002-lc_bank_add.
        lc_acc_no = gs_1002-lc_acc_no.
        lc_type = gs_1002-lc_type.
        lc_margin = gs_1002-lc_margin.
        lc_retire = gs_1002-lc_retire.
        bank_remarks = gs_1002-bank_remarks.
      ENDIF.
    ENDIF.
  ENDIF.

**********************************************
*  IF gs_1002 IS INITIAL .
*    MESSAGE i000(zelc) WITH s_pi-low.
*    CLEAR s_pi-low.
*  ELSE.
*    IF gs_1002-pi_no IS INITIAL.
*      MESSAGE i021(zelc) WITH s_pi-low.
*      CLEAR s_pi-low.
*    ELSEIF gs_1002-pi_canc_ind = 'X'.
*      MESSAGE i018(zelc) WITH s_pi-low.
*      CLEAR gs_1002.
*      CLEAR s_pi-low.
*    ENDIF.
*    IF gs_1002-lc_bank_vendor IS NOT INITIAL.
*      bank_ven = gs_1002-lc_bank_vendor.
*    ENDIF.
*  ENDIF.

***for use later on in saving.
*  SELECT * FROM zlc_process
*  INTO CORRESPONDING FIELDS OF TABLE @gt_1002
*  WHERE pi_no = @s_pi-low.
*  IF sy-subrc NE 0.
*    REFRESH gt_1002.
*  ENDIF.

  IF gv_disp_flag = abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CALL SCREEN 1002.
***  IF sy-subrc = 0.
***    MESSAGE s003(zelc).
***  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_1004
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_1004 .
  DATA: lv_lcno TYPE zlc_no.

  SELECT SINGLE lc_no FROM zlc_process INTO lv_lcno WHERE lc_no eq inp_lc.
  IF sy-subrc = 0.
    IF lv_lcno IS NOT INITIAL.
      IF lv_lcno NE inp_lc.
        MESSAGE w017(zelc).
        CALL SCREEN 1004.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA: lv_blklist TYPE string.
*  IF inp_pi IS INITIAL.
*  IF s_pi-low IS INITIAL.
*    MESSAGE w012(zelc).
*    CALL SCREEN 1004.
*  ELSEIF inp_lc IS INITIAL.
*    MESSAGE w014(zelc).
*    CALL SCREEN 1004.
*  ELSEIF gs_1004-lc_date IS INITIAL.
*    MESSAGE w015(zelc).
*    CALL SCREEN 1004.
*  ELSEIF gs_1004-lc_exp_date IS INITIAL.
*    MESSAGE w015(zelc).
*    CALL SCREEN 1004.
*  ELSE.
*    SELECT SINGLE bsart FROM ekko INTO @DATA(lv_bsart) WHERE ebeln = @gs_1004-ebeln.
*    IF lv_bsart = 'ZIMP'.
*      SELECT * FROM zblk_po INTO TABLE @DATA(it_blk) WHERE ebeln = @gs_1004-ebeln.
*      LOOP AT it_blk INTO DATA(wa_blk).
*        IF lv_blklist IS INITIAL.
*          lv_blklist = wa_blk-blkno.
*        ELSE.
*          CONCATENATE lv_blklist wa_blk-blkno INTO lv_blklist
*          SEPARATED BY ','.
*        ENDIF.
*      ENDLOOP.
*      gs_1004-blklist = lv_blklist.
*      CLEAR lv_blklist.
*
*      IF gs_1004-blklist IS INITIAL.
*        gs_1004-lc_no = ''.
*        gs_1004-lc_canc_ind = 'H'.
*      ELSE.
*        gs_1004-lc_no = inp_lc.
*      ENDIF.
*    ENDIF.
*    gs_1004-pi_no = inp_pi.
*    gs_1004-lc_no = inp_lc.
*
*    IF gs_1004-lc_exp_date IS NOT INITIAL AND gs_1004-lc_date IS NOT INITIAL.
*      gs_1004-lc_term = gs_1004-lc_exp_date - gs_1004-lc_date.
*    ENDIF.
*
*    IF gs_1004-lc_exp_date IS NOT INITIAL AND gs_1004-ship_date IS NOT INITIAL.
*      gs_1004-doc_sub_days = gs_1004-lc_exp_date - gs_1004-ship_date.
*    ENDIF.

**************SAVE
**ensure to update All POs against the same PI

  IF gs_1004 IS NOT INITIAL.
    LOOP AT gt_1004 ASSIGNING FIELD-SYMBOL(<lfs_1004>).
*      gs_1004-ebeln = <lfs_1004>-ebeln.
*      gs_1004-ebelp = <lfs_1004>-ebelp.
*      gs_1004-pi_no = <lfs_1004>-pi_no.
*      gs_1004-bukrs = <lfs_1004>-bukrs.
*      gs_1004-werks = <lfs_1004>-werks.
*      gs_1004-lc_no = inp_lc.
*      MOVE-CORRESPONDING gs_1004 TO <lfs_1004>.
      <lfs_1004>-lc_no = inp_lc.
      <lfs_1004>-lc_date = gs_1004-lc_date.
      <lfs_1004>-lc_exp_date = gs_1004-lc_exp_date.
      <lfs_1004>-lc_conf_date = gs_1004-lc_conf_date.
      <lfs_1004>-ship_date =  gs_1004-ship_date.
      <lfs_1004>-lc_term =  gs_1004-lc_term.
*      <lfs_1004>-lc_val = gs_1004-lc_val.
      <lfs_1004>-doc_sub_days = gs_1004-doc_sub_days.
      <lfs_1004>-lcaf_no =  gs_1004-lcaf_no.
      <lfs_1004>-irc_no =  gs_1004-irc_no.
      <lfs_1004>-bin_no =    gs_1004-bin_no.
      <lfs_1004>-tin_no =  gs_1004-tin_no.
      <lfs_1004>-tol_lmt =  gs_1004-tol_lmt.
      <lfs_1004>-proj_dpt =  gs_1004-proj_dpt.
      <lfs_1004>-name1 = gs_1004-name1.
      <lfs_1004>-pi_pymt =  gs_1004-pi_pymt.
      <lfs_1004>-waers = gs_1004-waers.
*      <lfs_1004>-pi_dt = gs_1004-pi_dt .
*      gs_1004-pi_amt =  <lfs_1004>-pi_amt.
      <lfs_1004>-lc_bank = gs_1004-lc_bank.
      <lfs_1004>-lc_acc_no =  gs_1004-lc_acc_no.
      <lfs_1004>-lc_type =  gs_1004-lc_type.
      <lfs_1004>-conf_bank =   gs_1004-conf_bank.
      <lfs_1004>-adv_bank = gs_1004-adv_bank.
      <lfs_1004>-sup_bank =   gs_1004-sup_bank .
      <lfs_1004>-sup_bank_add =  gs_1004-sup_bank_add.
      <lfs_1004>-sup_bank_acc =  gs_1004-sup_bank_acc.
      <lfs_1004>-swift_code =   gs_1004-swift_code.
      <lfs_1004>-ins_comp = gs_1004-ins_comp.
      <lfs_1004>-exch_rate = gs_1004-exch_rate.
      <lfs_1004>-ins_note_no =   gs_1004-ins_note_no.
      <lfs_1004>-ins_note_date =  gs_1004-ins_note_date.
      <lfs_1004>-ins_exp_date =   gs_1004-ins_exp_date.
      <lfs_1004>-lc_remarks =  gs_1004-lc_remarks.
    ENDLOOP.
  ENDIF.

  gs_1004-pi_no = s_pi-low.
  MODIFY zlc_process FROM TABLE gt_1004.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s005(zelc).
  ELSE.
    MESSAGE e009(zelc).
  ENDIF.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SRCH_1004
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM srch_1004 .
**LC type
  PERFORM get_lctype.
**Other header fields
  IF gv_disp_flag = 'X'.
    SELECT SINGLE * FROM zlc_process
    INTO CORRESPONDING FIELDS OF gs_1004
    WHERE lc_no =  inp_lc.
  ELSEIF  gv_mod_flag = 'X'.
    SELECT SINGLE * FROM zlc_process
    INTO CORRESPONDING FIELDS OF @gs_1004
    WHERE lc_no = @inp_lc.
    IF sy-subrc = 0.
      IF gv_date_1004 IS NOT INITIAL.
        gs_1004-lc_date = gv_date_1004.
      ENDIF.
      IF gv_expdate_1004 IS NOT INITIAL.
        gs_1004-lc_exp_date = gv_expdate_1004.
      ENDIF.
      IF gv_confdate_1004 IS NOT INITIAL.
        gs_1004-lc_conf_date = gv_confdate_1004.
      ENDIF.
      IF  gv_shipdate_1004 IS NOT INITIAL.
        gs_1004-ship_date =  gv_shipdate_1004.
      ENDIF.
      IF gv_term_1004 IS NOT INITIAL.
        gs_1004-lc_term = gv_term_1004.
      ENDIF.
      IF gv_sdays_1004 IS NOT INITIAL.
        gs_1004-doc_sub_days = gv_sdays_1004.
      ENDIF.
      IF gv_lcaf_1004 IS NOT INITIAL.
        gs_1004-lcaf_no = gv_lcaf_1004.
      ENDIF.
      IF gv_cbank_1004 IS NOT INITIAL.
        gs_1004-conf_bank = gv_cbank_1004.
      ENDIF.
      IF gv_abank_1004 IS NOT INITIAL.
        gs_1004-adv_bank = gv_abank_1004.
      ENDIF.
      IF gv_rem_1004 IS NOT INITIAL.
        gs_1004-lc_remarks = gv_rem_1004.
      ENDIF.
    ENDIF.

  ELSE. " Create mode -- some fields are editable - so shld not be overwritten
    IF s_pi IS NOT INITIAL .
      SELECT SINGLE * FROM zlc_process
      INTO CORRESPONDING FIELDS OF gs_1004
      WHERE pi_no IN s_pi.

      IF gs_1004-lc_no IS NOT INITIAL.
        inp_lc = gs_1004-lc_no .
      ENDIF.
      IF gv_date_1004 IS NOT INITIAL.
        gs_1004-lc_date = gv_date_1004.
      ENDIF.
      IF gv_expdate_1004 IS NOT INITIAL.
        gs_1004-lc_exp_date = gv_expdate_1004.
      ENDIF.
      IF gv_confdate_1004 IS NOT INITIAL.
        gs_1004-lc_conf_date = gv_confdate_1004.
      ENDIF.
      IF gv_shipdate_1004 IS NOT INITIAL.
        gs_1004-ship_date = gv_shipdate_1004.
      ENDIF.
      IF gv_term_1004 IS NOT INITIAL.
        gs_1004-lc_term = gv_term_1004.
      ENDIF.
      IF gv_sdays_1004 IS NOT INITIAL.
        gs_1004-doc_sub_days = gv_sdays_1004.
      ENDIF.
      IF gv_lcaf_1004 IS NOT INITIAL.
        gs_1004-lcaf_no = gv_lcaf_1004.
      ENDIF.
      IF gv_cbank_1004 IS NOT INITIAL.
        gs_1004-conf_bank = gv_cbank_1004.
      ENDIF.
      IF gv_abank_1004 IS NOT INITIAL.
        gs_1004-adv_bank = gv_abank_1004.
      ENDIF.
      IF gv_rem_1004 IS NOT INITIAL.
        gs_1004-lc_remarks = gv_rem_1004.
      ENDIF.
    ELSE.
*      MESSAGE e020(zabap) DISPLAY LIKE 'I'.
*      LEAVE to SCREEN 1004.
    ENDIF.
  ENDIF.

*  gs_1004-lc_val = gs_1004-pi_amt. "LC amount

  IF gv_disp_flag NE '' AND gv_mod_flag NE ''.
    IF gs_1004 IS INITIAL.
      MESSAGE i000(zelc) WITH inp_pi.
    ELSEIF gs_1004-pi_canc_ind = 'X'.
      CLEAR gs_1004.
      CLEAR inp_pi.
      MESSAGE i018(zelc) WITH gs_1004-pi_no.
    ELSE.
      IF gv_disp_flag = abap_true.
        LOOP AT SCREEN.
          IF screen-group1 = 'G1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF gv_mod_flag = abap_true.
        LOOP AT SCREEN.
          IF screen-group2 = 'G2'.
            screen-input = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF gs_1004-lc_exp_date IS NOT INITIAL AND gs_1004-lc_date IS NOT INITIAL.
        gs_1004-lc_term = gs_1004-lc_exp_date - gs_1004-lc_date.
      ENDIF.

      IF gs_1004-lc_exp_date IS NOT INITIAL AND gs_1004-ship_date IS NOT INITIAL.
        gs_1004-doc_sub_days = gs_1004-lc_exp_date - gs_1004-ship_date.
      ENDIF.
    ENDIF.
  ENDIF.

********************************************
**IRC/TIN/BIN
  IF s_pi-low IS NOT INITIAL.
    SELECT bukrs,
      CASE            "IRC
        WHEN bukrs = '1000' THEN '260326120299120'
        WHEN bukrs = '2000' THEN '260326120244920'
        WHEN bukrs = '3000' THEN '260326120240420'
        WHEN bukrs = '5000' THEN '260326110314219'
        WHEN bukrs = '9000' THEN '260326110342819'
      END AS irc_no,
      CASE            "BIN
        WHEN bukrs = '1000' THEN '000002071-0102'
        WHEN bukrs = '2000' THEN '000080701-0102'
        WHEN bukrs = '3000' THEN '000082993-0102'
        WHEN bukrs = '5000' THEN '000092768-0102'
        WHEN bukrs = '9000' THEN '000278899-0102'
      END AS bin_no,
      CASE           "TIN
        WHEN bukrs = '1000' THEN '166127442711'
        WHEN bukrs = '2000' THEN '726776610117'
        WHEN bukrs = '3000' THEN '558135255290'
        WHEN bukrs = '5000' THEN '534893619898'
        WHEN bukrs = '9000' THEN '568601315851'
      END AS tin_no
      FROM zlc_process
      INTO TABLE @DATA(lt_lc)
      WHERE pi_no = @s_pi-low.
  ELSE.
    SELECT bukrs,
    CASE            "IRC
      WHEN bukrs = '1000' THEN '260326120299120'
      WHEN bukrs = '2000' THEN '260326120244920'
      WHEN bukrs = '3000' THEN '260326120240420'
      WHEN bukrs = '5000' THEN '260326110314219'
      WHEN bukrs = '9000' THEN '260326110342819'
    END AS irc_no,
    CASE            "BIN
      WHEN bukrs = '1000' THEN '000002071-0102'
      WHEN bukrs = '2000' THEN '000080701-0102'
      WHEN bukrs = '3000' THEN '000082993-0102'
      WHEN bukrs = '5000' THEN '000092768-0102'
      WHEN bukrs = '9000' THEN '000278899-0102'
    END AS bin_no,
    CASE           "TIN
      WHEN bukrs = '1000' THEN '166127442711'
      WHEN bukrs = '2000' THEN '726776610117'
      WHEN bukrs = '3000' THEN '558135255290'
      WHEN bukrs = '5000' THEN '534893619898'
      WHEN bukrs = '9000' THEN '568601315851'
    END AS tin_no
    FROM zlc_process
    INTO TABLE @lt_lc
    WHERE lc_no = @inp_lc.
  ENDIF.

  IF line_exists( lt_lc[ 1 ] ).
    gs_1004-irc_no = VALUE #( lt_lc[ 1 ]-irc_no OPTIONAL ).
    gs_1004-bin_no = VALUE #( lt_lc[ 1 ]-bin_no OPTIONAL ).
    gs_1004-tin_no = VALUE #( lt_lc[ 1 ]-tin_no OPTIONAL ).
  ENDIF.

*******************************************************************
**Added for table control - 03/02/2021
  IF gv_disp_flag = 'X'
  OR gv_mod_flag = 'X'.
    SELECT * FROM zlc_process INTO CORRESPONDING FIELDS OF TABLE gt_1004
     WHERE lc_no EQ inp_lc.
  ELSE.
    SELECT * FROM zlc_process INTO CORRESPONDING FIELDS OF TABLE gt_1004
    WHERE pi_no IN s_pi.
  ENDIF.

****Other header fields
**  IF gv_disp_flag = 'X'
**  OR gv_mod_flag = 'X'.
**    READ TABLE gt_1004 INTO gs_1004 INDEX 1.
**    IF sy-subrc NE 0 .
**      CLEAR gs_1004.
**    ENDIF.
**  ENDIF.

*  IF gt_1004 IS INITIAL.
*    IF gv_screenmode NE 'PI_CRT'.
*      MESSAGE w000(zelc) WITH inp_ebeln.
*      CALL SCREEN 1004.
*    ENDIF.
*  ELSE.
*    IF gv_screenmode EQ 'PI_CRT'.
*      CLEAR gs_1004.
*      REFRESH it_podet.
*      CLEAR: zlc_process-pi_dt, zlc_process-lddat.
*      MESSAGE w007(zelc) WITH gs_1004-pi_no.
*      CALL SCREEN 1004.
*    ENDIF.
*  ENDIF.

*******************************************************************
**PI details
  LOOP AT gt_1004 INTO DATA(ls_1004).
    s_ebeln-sign = 'I'.
    s_ebeln-option = 'EQ'.
    s_ebeln-low = ls_1004-ebeln.
    APPEND s_ebeln.
    CLEAR: s_ebeln,ls_1004.
  ENDLOOP.

  CLEAR ls_1004.
  READ TABLE gt_1004 INTO ls_1004 INDEX 1.
  IF sy-subrc = 0.
    sup_bank = gs_1004-sup_bank_vendor.
    zlc_process-pi_dt = gs_1004-pi_dt.
    zlc_process-lddat = gs_1004-lddat.
  ENDIF.

*******************************************************************
**PI Details frm PI screen - item num etc
*  IF gv_disp_flag = 'X'
*  OR gv_mod_flag = 'X'.
  PERFORM fill_po_1004.

**blocklist
  IF gt_1004 IS NOT INITIAL.
    SELECT bsart FROM ekko
    INTO TABLE @DATA(lt_bsart)
    FOR ALL ENTRIES IN @gt_1004
    WHERE ebeln = @gt_1004-ebeln.

    SELECT * FROM zblk_po
    INTO TABLE @DATA(it_blk)
    FOR ALL ENTRIES IN @gt_1004
    WHERE ebeln = @gt_1004-ebeln.
  ENDIF.
  DATA lv_amt TYPE zpi_amt.
  DATA: lv_pi    TYPE zpi_no,
        lv_po    TYPE ebeln,
        lv_pi_dt TYPE zpi_dt,
        lv_tol   TYPE ztol_lmt.
  SORT gt_1004 BY pi_no ebeln.

  LOOP AT gt_1004 ASSIGNING FIELD-SYMBOL(<lfs_1004>).
    READ TABLE it_podet INTO DATA(ls_podet)
    WITH KEY pi    = <lfs_1004>-pi_no
             ebeln = <lfs_1004>-ebeln
             ebelp = <lfs_1004>-ebelp.
    IF sy-subrc = 0 .
      <lfs_1004>-ebelp = ls_podet-ebelp.
      <lfs_1004>-meins = ls_podet-meins.
      <lfs_1004>-kbetr = ls_podet-kbetr.
      <lfs_1004>-matnr = |{ ls_podet-matnr ALPHA = OUT }|.
      <lfs_1004>-txz01 = ls_podet-txz01.
    ENDIF.

    IF <lfs_1004>-pi_no NE lv_pi OR <lfs_1004>-ebeln <> lv_po.
      lv_amt = lv_amt + <lfs_1004>-pi_amt.
      <lfs_1004>-lc_val = lv_amt.
    ENDIF.
    IF <lfs_1004>-pi_dt > lv_pi_dt .
      lv_pi_dt =  <lfs_1004>-pi_dt.
    ENDIF.
    lv_pi = <lfs_1004>-pi_no.
    lv_po = <lfs_1004>-ebeln.
*    AT END OF pi_no.
    lv_tol = <lfs_1004>-tol_lmt.


*    ENDAT.
*    <lfs_1004>-pi_amt  = lv_amt.
*    <lfs_1004>-ship_date  = lv_ship_dt.
*    CLEAR lv_amt.

    READ TABLE it_blk INTO DATA(wa_blk)
    WITH KEY ebeln = <lfs_1004>-ebeln.
    IF sy-subrc = 0 .
      <lfs_1004>-blklist = wa_blk-blkno.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_1004 ASSIGNING FIELD-SYMBOL(<fs_1004>).
    <fs_1004>-lc_val = lv_amt.
  ENDLOOP.
  gs_1004-pi_amt = lv_amt.
  gs_1004-lc_val = lv_amt.
  gs_1004-pi_dt = lv_pi_dt.

  CLEAR: lv_amt, lv_pi_dt.
  CLEAR: lv_po, lv_pi.

  IF gv_screenmode NE 'LC_CRT'.
    IF tol_lmt IS INITIAL.
      tol_lmt = lv_tol.
    ELSE.
    ENDIF.
  ENDIF.
**  LOOP AT it_blk INTO DATA(wa_blk).
**    IF lv_blklist IS INITIAL.
**      lv_blklist = wa_blk-blkno.
**    ELSE.
**      CONCATENATE lv_blklist wa_blk-blkno INTO lv_blklist
**      SEPARATED BY ','.
**    ENDIF.
**  ENDLOOP.
**  gs_1004-blklist = lv_blklist.
**  CLEAR lv_blklist.
***      ENDIF.

*  IF gs_1004-lc_canc_ind = 'X'.
*    MESSAGE 'LC No has been cancelled' TYPE 'E' DISPLAY LIKE 'I'.
*    CLEAR gs_1004.
*  ENDIF.
*  ENDIF."Display / Update

*******************************************************************
  CALL SCREEN 1004.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_pi .

  CLEAR ok_code.


*  SELECT pi_no FROM zlc_process INTO TABLE @DATA(lt_pi) WHERE ebeln IN @s_ebeln. "Commented to allow Multiple PI for 1 PO
*  LOOP AT lt_pi INTO DATA(lv_pi).
*    IF lv_pi-pi_no NE inp_pi.
*      MESSAGE e013(zelc) WITH lv_pi-pi_no.
*    ELSE.
**      MESSAGE e007(zelc) WITH lv_pi.
*    ENDIF.
*  ENDLOOP.
*  PERFORM fill_po.


  IF gv_screenmode = 'PI_MOD'.
    LOOP AT gt_1001 ASSIGNING FIELD-SYMBOL(<gs_1001>).
*      MOVE-CORRESPONDING gs_1001 TO <gs_1001>.
      <gs_1001>-ernam = gs_1001-ernam.
      <gs_1001>-proj_dpt = gs_1001-proj_dpt.
      <gs_1001>-crsp_type = gs_1001-crsp_type.
      <gs_1001>-name1 = gs_1001-name1.
      <gs_1001>-pi_pymt = gs_1001-pi_pymt.
      <gs_1001>-waers = gs_1001-waers.
      <gs_1001>-pi_dt = gs_1001-pi_dt.
      <gs_1001>-pi_qty = gs_1001-pi_qty.
      <gs_1001>-pi_amt = gs_1001-pi_amt.
      <gs_1001>-offc_add = gs_1001-offc_add.
      <gs_1001>-plant_add = gs_1001-plant_add.
      <gs_1001>-sup_bank = gs_1001-sup_bank.
      <gs_1001>-sup_bank_add = gs_1001-sup_bank_add.
      <gs_1001>-sup_bank_acc = gs_1001-sup_bank_acc.
      <gs_1001>-swift_code = gs_1001-swift_code.
      <gs_1001>-prd_det = gs_1001-prd_det.
      <gs_1001>-pi_remarks = gs_1001-pi_remarks.
    ENDLOOP.

    LOOP AT gt_1001 INTO gs_1001.
      READ TABLE it_podet1 INTO wa_podet1 WITH KEY pi = gs_1001-pi_no
                                                ebeln = gs_1001-ebeln
                                                ebelp = gs_1001-ebelp.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING wa_podet1 TO gs_1001.
        MODIFY gt_1001 FROM gs_1001.
        CLEAR wa_podet1.
      ELSE.
        READ TABLE it_podet INTO wa_podet WITH KEY pi = gs_1001-pi_no
                                                ebeln = gs_1001-ebeln
                                                ebelp = gs_1001-ebelp.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING wa_podet TO gs_1001.
          MODIFY gt_1001 FROM gs_1001.
          CLEAR wa_podet.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF gv_screenmode = 'PI_CRT'.
    IF it_podet1 IS NOT INITIAL.
      LOOP AT it_podet1 INTO wa_podet1.
        MOVE-CORRESPONDING wa_podet1 TO gs_1001.
        APPEND gs_1001 TO gt_1001.
        CLEAR wa_podet1.
      ENDLOOP.
    ELSE.
      LOOP AT it_podet INTO wa_podet.
        MOVE-CORRESPONDING wa_podet TO gs_1001.
        APPEND gs_1001 TO gt_1001.
        CLEAR wa_podet.
      ENDLOOP.
    ENDIF.
  ENDIF.
  IF s_ebeln IS INITIAL.
    MESSAGE e010(zelc).
  ENDIF.

  IF inp_pi IS INITIAL.
    MESSAGE e012(zelc).
  ENDIF.

  LOOP AT gt_1001 ASSIGNING FIELD-SYMBOL(<fs_1001>).
    <fs_1001>-pi_dt = zlc_process-pi_dt.
*    <fs_1001>-lddat = zlc_process-lddat.

*    IF ( inp_pi = <fs_1001>-pi_no ) OR <fs_1001>-pi_no IS INITIAL.
    gv_save = 'X'.
    <fs_1001>-pi_no = inp_pi.
*    ELSE.
*      MESSAGE e011(zelc).
*    ENDIF.
  ENDLOOP.

*  REFRESH it_podet.
  LOOP AT it_podet INTO wa_podet.
    READ TABLE it_podet1 INTO wa_podet1 WITH KEY pi = wa_podet-pi
                                              ebeln = wa_podet-ebeln
                                              ebelp = wa_podet-ebelp.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_podet1 TO wa_podet.
      MODIFY it_podet FROM wa_podet.
    ENDIF.
*    APPEND wa_podet TO it_podet.
  ENDLOOP.

  MODIFY zlc_process FROM TABLE gt_1001.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE s002(zelc).
  ELSE.
    ROLLBACK WORK.
    MESSAGE e009(zelc).
  ENDIF.

  CLEAR ok_code.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_po .
  SELECT ebeln, bukrs, bsart, knumv,waers, lifnr, zterm, inco1, inco2 FROM ekko INTO TABLE @DATA(lt_ekko)  WHERE ebeln IN @s_ebeln.

  IF lt_ekko IS NOT INITIAL.

    SELECT ebeln, ebelp, matnr, werks, txz01, menge, meins, packno, netpr
      FROM ekpo INTO TABLE @DATA(lt_ekpo)
      FOR ALL ENTRIES IN @lt_ekko
      WHERE ebeln = @lt_ekko-ebeln.
    IF lt_ekpo IS NOT INITIAL.
      SELECT a~packno, a~sub_packno, a~extrow, b~packno AS item, b~extrow AS line, b~menge, b~meins, b~ktext1, b~netwr
        FROM esll AS a INNER JOIN esll AS b ON a~sub_packno = b~packno
        INTO TABLE @DATA(lt_ser)
        FOR ALL ENTRIES IN @lt_ekpo WHERE a~packno = @lt_ekpo-packno.
    ENDIF.

    SELECT knumv, kposn, kbetr,kschl
      FROM prcd_elements INTO TABLE @DATA(lt_price)
      FOR ALL ENTRIES IN @lt_ekko
      WHERE knumv = @lt_ekko-knumv.

  ENDIF.

  SELECT name1 FROM lfa1 INTO TABLE @DATA(lt_lfa1) FOR ALL ENTRIES IN @lt_ekko WHERE lifnr = @lt_ekko-lifnr.
  READ TABLE lt_lfa1 INTO DATA(ls_lfa1) INDEX 1.
  IF sy-subrc = 0.
    gs_1001-name1 = ls_lfa1-name1.
  ENDIF.

  gs_1001-pi_no = inp_pi.

  READ TABLE lt_ekko INTO DATA(ls_ekko) INDEX 1.
  IF sy-subrc = 0.
    gs_1001-bukrs = ls_ekko-bukrs.
*  gs_1001-pi_pymt = ls_ekko-zterm.
    gs_1001-lifnr = ls_ekko-lifnr.

    SELECT SINGLE bukrs, adrnr FROM t001 INTO @DATA(ls_t001) WHERE bukrs = @ls_ekko-bukrs.
    IF sy-subrc = 0.
      SELECT SINGLE addrnumber, name1, city1, post_code1, street FROM adrc INTO @DATA(ls_adrc) WHERE addrnumber = @ls_t001-adrnr.
      IF sy-subrc = 0.
        CONCATENATE ls_adrc-name1 ls_adrc-street ls_adrc-city1 ls_adrc-post_code1 INTO gs_1001-offc_add SEPARATED BY ','.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT gt_1001 INTO DATA(ls_1001).
    READ TABLE it_podet1 INTO wa_podet1 WITH KEY pi = ls_1001-pi_no
                                                ebeln = ls_1001-ebeln
                                                ebelp = ls_1001-ebelp.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_podet1 TO ls_1001.
      MODIFY gt_1001 FROM ls_1001.
    ELSE.
      READ TABLE it_podet INTO wa_podet WITH KEY pi = ls_1001-pi_no
                                                ebeln = ls_1001-ebeln
                                                ebelp = ls_1001-ebelp.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING wa_podet TO ls_1001.
        MODIFY gt_1001 FROM ls_1001.
        CLEAR: wa_podet,ls_1001.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_ser IS INITIAL.
    LOOP AT lt_ekpo INTO DATA(ls_ekpo).
      READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = ls_ekpo-ebeln.
      IF sy-subrc = 0.

**        READ TABLE gt_1001 INTO DATA(ls_1001) WITH KEY ebeln = ls_ekko-ebeln.
**        IF sy-subrc = 0 AND gv_screenmode <> 'PI_MOD'.
**          wa_podet-lstel = ls_1001-lstel.
**          wa_podet-vstel = ls_1001-vstel.
**          wa_podet-inco = ls_1001-inco.
**          wa_podet-lddat = ls_1001-lddat.
**          wa_podet-ship_mode = ls_1001-ship_mode.
**          wa_podet-hs_code = ls_1001-hs_code.
**        ELSE.
**          READ TABLE it_podet1 INTO wa_podet1 WITH KEY ebeln = ls_ekko-ebeln.
**          IF sy-subrc = 0.
**            wa_podet-lstel = wa_podet1-lstel.
**            wa_podet-vstel = wa_podet1-vstel.
**            wa_podet-inco = wa_podet1-inco.
**            wa_podet-lddat = wa_podet1-lddat.
**            wa_podet-ship_mode = wa_podet1-ship_mode.
**            wa_podet-hs_code = wa_podet1-hs_code.
**          ENDIF.
**        ENDIF.

        IF gv_screenmode = 'PI_DISP' OR gv_screenmode = 'PI_MOD'.
          READ TABLE gt_1001 INTO ls_1001 WITH KEY ebeln = ls_ekpo-ebeln
                                                   ebelp = ls_ekpo-ebelp.
          IF sy-subrc = 0.
            wa_podet-land1 = ls_1001-land1.
            wa_podet-lstel = ls_1001-lstel.
            wa_podet-vstel = ls_1001-vstel.
            wa_podet-inco = ls_1001-inco.
            wa_podet-lddat = ls_1001-lddat.
            wa_podet-ship_mode = ls_1001-ship_mode.
            wa_podet-hs_code = ls_1001-hs_code.
          ENDIF.
        ENDIF.

        IF gt_1001 IS INITIAL.
          READ TABLE it_podet1 INTO wa_podet1 WITH KEY ebeln = ls_ekpo-ebeln
                                                       ebelp = ls_ekpo-ebelp.
          IF sy-subrc = 0.
            wa_podet-land1 = wa_podet1-land1.
            wa_podet-lstel = wa_podet1-lstel.
            wa_podet-vstel = wa_podet1-vstel.
            wa_podet-inco = wa_podet1-inco.
            wa_podet-lddat = wa_podet1-lddat.
            wa_podet-ship_mode = wa_podet1-ship_mode.
            wa_podet-hs_code = wa_podet1-hs_code.
          ENDIF.
        ENDIF.



*        IF gv_screenmode = 'PI_MOD'.
*          IF it_podet1 IS INITIAL.
*            READ TABLE gt_1001 INTO ls_1001 WITH KEY ebeln = ls_ekko-ebeln.
*            IF sy-subrc = 0.
*              wa_podet-land1 = ls_1001-land1.
*              wa_podet-lstel = ls_1001-lstel.
*              wa_podet-vstel = ls_1001-vstel.
*              wa_podet-inco = ls_1001-inco.
*              wa_podet-lddat = ls_1001-lddat.
*              wa_podet-ship_mode = ls_1001-ship_mode.
*              wa_podet-hs_code = ls_1001-hs_code.
*            ENDIF.
*          ELSE.
*            READ TABLE it_podet1 INTO wa_podet1 WITH KEY ebeln = ls_ekko-ebeln.
*            IF sy-subrc = 0.
*              wa_podet-land1 = wa_podet1-land1.
*              wa_podet-lstel = wa_podet1-lstel.
*              wa_podet-vstel = wa_podet1-vstel.
*              wa_podet-inco = wa_podet1-inco.
*              wa_podet-lddat = wa_podet1-lddat.
*              wa_podet-ship_mode = wa_podet1-ship_mode.
*              wa_podet-hs_code = wa_podet1-hs_code.
*            ENDIF.
*          ENDIF.
*        ENDIF.

        wa_podet-pi = inp_pi.
        wa_podet-ebeln = ls_ekpo-ebeln.
        wa_podet-ebelp = ls_ekpo-ebelp.
        wa_podet-matnr = ls_ekpo-matnr.
        wa_podet-txz01 = ls_ekpo-txz01.
        wa_podet-menge = ls_ekpo-menge.
        wa_podet-meins = ls_ekpo-meins.

        IF lt_ser IS NOT INITIAL.

        ENDIF.

        SELECT SINGLE werks,name1 FROM t001w INTO @DATA(ls_t001w1) WHERE werks = @ls_ekpo-werks.
        IF sy-subrc = 0.
          wa_podet-plant_desc = ls_t001w1-name1.
        ENDIF.

        IF gs_1001-plant_add IS INITIAL.
          SELECT SINGLE werks, name1, stras, pstlz, ort01 FROM t001w INTO @DATA(ls_t001w) WHERE werks = @ls_ekpo-werks.
          IF sy-subrc = 0.
            gs_1001-werks = ls_ekpo-werks.
            gs_1001-waers = ls_ekko-waers.
            gs_1001-plant_desc = ls_t001w-name1.
            CONCATENATE ls_t001w-name1 ls_t001w-stras ls_t001w-ort01 ls_t001w-pstlz INTO gs_1001-plant_add SEPARATED BY ','.
          ENDIF.
        ENDIF.

*        READ TABLE lt_price INTO DATA(ls_price) WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'PB00'.
*        IF sy-subrc = 0.
        wa_podet-kbetr = ls_ekpo-netpr.
*        ENDIF.

*        READ TABLE lt_price INTO DATA(ls_price) WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZLOD'.
*        IF sy-subrc = 0.
*          wa_podet-loading_cost = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZFIR'.
*        IF sy-subrc = 0.
*          wa_podet-trans_cost = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZFRF'.
*        IF sy-subrc = 0.
*          wa_podet-sea_freight = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZFRA'.
*        IF sy-subrc = 0.
*          wa_podet-air_freight = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZFRT'.
*        IF sy-subrc = 0.
*          wa_podet-truck_freight = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZSEA'.
*        IF sy-subrc = 0.
*          wa_podet-calc_sea = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZAI1'.
*        IF sy-subrc = 0.
*          wa_podet-calc_air = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZAI2'.
*        IF sy-subrc = 0.
*          wa_podet-calc_air2 = ls_price-kbetr.
*        ENDIF.

        READ TABLE lt_price INTO DATA(ls_price) WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZCDU'.
        IF sy-subrc = 0.
          wa_podet-custom_duty = ls_price-kbetr.
        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZRDU'.
*        IF sy-subrc = 0.
*          wa_podet-regl_duty = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZSDU'.
*        IF sy-subrc = 0.
*          wa_podet-suppl_duty = ls_price-kbetr.
*        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZAIT'.
        IF sy-subrc = 0.
          wa_podet-ait = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZATX'.
        IF sy-subrc = 0.
          wa_podet-atx = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZVI1'.
        IF sy-subrc = 0.
          wa_podet-vi1 = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZOTS'.
        IF sy-subrc = 0.
          wa_podet-ots = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZBNK'.
        IF sy-subrc = 0.
          wa_podet-bnk = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZVLC'.
        IF sy-subrc = 0.
          wa_podet-vlc = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZBNC'.
        IF sy-subrc = 0.
          wa_podet-bnc = ls_price-kbetr.
        ENDIF.


        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZIPM'.
        IF sy-subrc = 0.
          wa_podet-ipm = ls_price-kbetr.
        ENDIF.


*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZLRC'.
*        IF sy-subrc = 0.
*          wa_podet-lrc = ls_price-kbetr.
*        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZVTI'.
        IF sy-subrc = 0.
          wa_podet-vti = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZTIC'.
        IF sy-subrc = 0.
          wa_podet-tic = ls_price-kbetr.
        ENDIF.


        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZCFC'.
        IF sy-subrc = 0.
          wa_podet-cfc = ls_price-kbetr.
        ENDIF.


*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZCRC'.
*        IF sy-subrc = 0.
*          wa_podet-crc = ls_price-kbetr.
*        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZVCF'.
        IF sy-subrc = 0.
          wa_podet-vcf = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZOCC'.
        IF sy-subrc = 0.
          wa_podet-occ = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZPWC'.
        IF sy-subrc = 0.
          wa_podet-pwc = ls_price-kbetr.
        ENDIF.


        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZFFC'.
        IF sy-subrc = 0.
          wa_podet-ffc = ls_price-kbetr.
        ENDIF.

*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZITC'.
*        IF sy-subrc = 0.
*          wa_podet-itc = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZLOD'.
*        IF sy-subrc = 0.
*          wa_podet-lod = ls_price-kbetr.
*        ENDIF.



        wa_podet-total_duty =
          wa_podet-custom_duty +
*          wa_podet-regl_duty +
*          wa_podet-suppl_duty +
           wa_podet-ait +
           wa_podet-atx +
           wa_podet-vi1 +
           wa_podet-bnk +
           wa_podet-bnc +
           wa_podet-vlc +
*           wa_podet-lrc +
           wa_podet-tic +
           wa_podet-ipm +
*           wa_podet-cnf +
           wa_podet-cfc +
           wa_podet-pwc +
*           wa_podet-crc +
           wa_podet-occ +
           wa_podet-vcf +
           wa_podet-ffc +
*           wa_podet-itc +
*           wa_podet-lod +
           wa_podet-ots.

        gs_1001-total_duty = wa_podet-total_duty + gs_1001-total_duty.
*        IF it_podet1 IS NOT INITIAL OR gt_1001 IS NOT INITIAL.
        APPEND wa_podet TO it_podet.
        CLEAR wa_podet.
*        ENDIF.

      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT lt_ser INTO DATA(ls_ser).
      wa_podet-ebeln = ls_ser-item.
      wa_podet-ebelp = ls_ser-line.
*      wa_podet-matnr = ls_ekpo-matnr.
      wa_podet-txz01 = ls_ser-ktext1.
      wa_podet-menge = ls_ser-menge.
      wa_podet-meins = ls_ser-meins.
      wa_podet-kbetr = ls_ser-netwr.
      APPEND wa_podet TO it_podet.
      CLEAR wa_podet.
    ENDLOOP.
  ENDIF.

  SORT it_podet BY  ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM it_podet COMPARING  ebeln ebelp.
*  CALL SCREEN 1001.

  CLEAR: it_podet1, wa_podet1.
* ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SRCH_1005
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM srch_1005 .
  REFRESH it_lctrack.
*  SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1005 WHERE lc_no = @inp_lc.
  SELECT * FROM zlc_process INTO CORRESPONDING FIELDS OF TABLE @gt_1005 WHERE lc_no = @inp_lc.
  IF sy-subrc NE 0.
    MESSAGE w008(zelc) WITH inp_lc.
    CLEAR: gs_1005.
    CALL SCREEN 1005.
  ELSE.
    IF zlc_process-eta IS INITIAL AND gs_1005-vessel_nm IS INITIAL AND gs_1005-voyage_no IS INITIAL AND gs_1005-ship_date IS INITIAL
     AND gs_1005-freight IS INITIAL AND gs_1005-freight_vendor IS INITIAL AND zlc_process-freight_date IS INITIAL AND gs_1005-cpy_doc IS INITIAL
     AND zlc_process-freight_date IS INITIAL AND gs_1005-cpy_doc IS INITIAL AND zlc_process-cpy_doc_date IS INITIAL AND gs_1005-doc_hand_vendor IS INITIAL
    AND zlc_process-doc_hand_date IS INITIAL AND gs_1005-doc_hand IS INITIAL AND zlc_process-appl_dp_date IS INITIAL AND gs_1005-appl_dp IS  INITIAL
      AND gs_1005-appl_dp_vendor IS INITIAL AND zlc_process-duty_pymt_date IS INITIAL AND gs_1005-duty_pymt IS INITIAL AND zlc_process-eta IS INITIAL.
    ELSE.
      LOOP AT gt_1005 ASSIGNING FIELD-SYMBOL(<gs_1005>).
        <gs_1005>-eta = zlc_process-eta.
        <gs_1005>-vessel_nm = gs_1005-vessel_nm.
        <gs_1005>-voyage_no = gs_1005-voyage_no.
        <gs_1005>-etd       =  gs_1005-ship_date.
        <gs_1005>-freight    = gs_1005-freight.
        <gs_1005>-freight_vendor    = gs_1005-freight_vendor.
        <gs_1005>-freight_date = zlc_process-freight_date.
        <gs_1005>-cpy_doc = gs_1005-cpy_doc.
        <gs_1005>-cpy_doc_date = zlc_process-cpy_doc_date.
        <gs_1005>-doc_hand_date = zlc_process-doc_hand_date.
        <gs_1005>-doc_hand_vendor = gs_1005-doc_hand_vendor.
        <gs_1005>-doc_hand = gs_1005-doc_hand.
        <gs_1005>-appl_dp_date = zlc_process-appl_dp_date.
        <gs_1005>-appl_dp = gs_1005-appl_dp.
        <gs_1005>-appl_dp_vendor = gs_1005-appl_dp_vendor.
        <gs_1005>-duty_pymt_date = zlc_process-duty_pymt_date.
        <gs_1005>-duty_pymt = gs_1005-duty_pymt.
        <gs_1005>-eta = zlc_process-eta.
      ENDLOOP.
    ENDIF.

    LOOP AT gt_1005 INTO DATA(ls_1005).
      READ TABLE it_lctrack1 INTO wa_lctrack1 WITH KEY pi = ls_1005-pi_no
                                                    ebeln = ls_1005-ebeln
                                                    ebelp = ls_1005-ebelp.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING wa_lctrack1 TO ls_1005.
        MODIFY gt_1005 FROM ls_1005.
        CLEAR ls_1005.
      ENDIF.
    ENDLOOP.

    READ TABLE gt_1005 INTO gs_1005 INDEX 1.
    IF sy-subrc = 0..
      zlc_process-eta = gs_1005-eta.
      zlc_process-freight_date = gs_1005-freight_date.
      zlc_process-cpy_doc_date = gs_1005-cpy_doc_date.
      zlc_process-doc_hand_date = gs_1005-doc_hand_date.
      zlc_process-appl_dp_date = gs_1005-appl_dp_date.
      zlc_process-duty_pymt_date = gs_1005-duty_pymt_date.
      gs_1005-etd = gs_1005-ship_date.
    ENDIF.
    LOOP AT gt_1005 INTO ls_1005.
      wa_lctrack-pi          = ls_1005-pi_no.
      wa_lctrack-ebeln          = ls_1005-ebeln.
      wa_lctrack-ebelp          = ls_1005-ebelp.
      wa_lctrack-comm_inv_no = ls_1005-comm_inv_no.
      wa_lctrack-comm_inv_date = ls_1005-comm_inv_date.
      wa_lctrack-comm_inv_val = ls_1005-comm_inv_val.
      wa_lctrack-assemble_value = ls_1005-assemble_value.
      wa_lctrack-total_duty = ls_1005-total_duty.
      wa_lctrack-bl_awb_no = ls_1005-bl_awb_no.
      wa_lctrack-bl_awb_date = ls_1005-bl_awb_date.
      wa_lctrack-boe_no = ls_1005-boe_no.
      wa_lctrack-boe_date = ls_1005-boe_date.
      wa_lctrack-blklist = ls_1005-blklist.
      APPEND wa_lctrack TO it_lctrack.
      CLEAR: wa_lctrack,ls_1005.
    ENDLOOP.
  ENDIF.
  CALL SCREEN 1005.
  CLEAR: it_lctrack1, wa_lctrack1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_1005
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_1005 .

  IF inp_lc IS INITIAL.
    MESSAGE w014(zelc).
    CALL SCREEN 1005.
  ELSE.
    LOOP AT gt_1005 ASSIGNING FIELD-SYMBOL(<gs_1005>).
      <gs_1005>-eta = zlc_process-eta.
      <gs_1005>-vessel_nm = gs_1005-vessel_nm.
      <gs_1005>-voyage_no = gs_1005-voyage_no.
      <gs_1005>-freight    = gs_1005-freight.
      <gs_1005>-freight_vendor    = gs_1005-freight_vendor.
      <gs_1005>-freight_date = zlc_process-freight_date.
      <gs_1005>-cpy_doc = gs_1005-cpy_doc.
      <gs_1005>-cpy_doc_date = zlc_process-cpy_doc_date.
      <gs_1005>-doc_hand_date = zlc_process-doc_hand_date.
      <gs_1005>-doc_hand_vendor = gs_1005-doc_hand_vendor.
      <gs_1005>-doc_hand = gs_1005-doc_hand.
      <gs_1005>-appl_dp_date = zlc_process-appl_dp_date.
      <gs_1005>-appl_dp = gs_1005-appl_dp.
      <gs_1005>-appl_dp_vendor = gs_1005-appl_dp_vendor.
      <gs_1005>-duty_pymt_date = zlc_process-duty_pymt_date.
      <gs_1005>-duty_pymt = gs_1005-duty_pymt.
      <gs_1005>-eta = zlc_process-eta.
*      MODIFY gt_1005 FROM gs_1005.
*      CLEAR gs_1005.
    ENDLOOP.
    LOOP AT gt_1005 INTO DATA(ls_1005).
      READ TABLE it_lctrack1 INTO DATA(wa_lctrack1) WITH KEY pi = ls_1005-pi_no
                                                         ebeln = ls_1005-ebeln
                                                         ebelp = ls_1005-ebelp.
      IF sy-subrc = 0.
        ls_1005-comm_inv_no = wa_lctrack1-comm_inv_no.
        ls_1005-comm_inv_date = wa_lctrack1-comm_inv_date.
        ls_1005-comm_inv_val = wa_lctrack1-comm_inv_val.
        ls_1005-assemble_value = wa_lctrack1-assemble_value.
        ls_1005-total_duty = wa_lctrack1-total_duty.
        ls_1005-bl_awb_no = wa_lctrack1-bl_awb_no.
        ls_1005-bl_awb_date = wa_lctrack1-bl_awb_date.
        ls_1005-boe_no = wa_lctrack1-boe_no.
        ls_1005-boe_date = wa_lctrack1-boe_date.


        MODIFY gt_1005 FROM ls_1005.
        CLEAR ls_1005.
      ENDIF.
    ENDLOOP.

    LOOP AT it_lctrack INTO wa_lctrack.
      READ TABLE it_lctrack1 INTO wa_lctrack1 WITH KEY pi = wa_lctrack-pi
                                                 ebeln = wa_lctrack-ebeln
                                                 ebelp = wa_lctrack-ebelp.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING wa_lctrack1 TO wa_lctrack.
        MODIFY it_lctrack FROM wa_lctrack.
      ENDIF.
    ENDLOOP.

    MODIFY zlc_process FROM TABLE @gt_1005.
    IF sy-subrc = 0.
      MESSAGE s006(zelc).
    ELSE.
      MESSAGE e009(zelc).
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_1003
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_1003 .

*  REFRESH gt_1003.
*  CLEAR gs_1003.
  IF s_pi-low IS INITIAL.
    MESSAGE w012(zelc).
    CALL SCREEN 1003.
  ELSE.
*    REFRESH gt_1003.
    IF gv_screenmode = 'INS_MOD' OR gv_screenmode = 'INS_CRT'.
      LOOP AT gt_1003 INTO gs_1003.
*        MOVE-CORRESPONDING wa_insdet TO gs_1003.
        gs_1003-ins_vendor = ins_ven.
        gs_1003-ins_comp = ins_comp.
        gs_1003-ins_add = ins_add.
        gs_1003-exch_rate = ins_exch_rate.
        gs_1003-ins_note_no = ins_note_no.
        gs_1003-ins_exp_date = ins_exp_date.
        gs_1003-ins_note_date = ins_note_date.
        gs_1003-ins_note_val = ins_note_val.
        gs_1003-rcpt_no = ins_rcpt_no.
        gs_1003-ins_remarks = ins_remarks.
        MODIFY gt_1003 FROM gs_1003.
        CLEAR: gs_1003.
      ENDLOOP.
    ENDIF.
    LOOP AT gt_1003 INTO gs_1003.
      READ TABLE it_insdet1 INTO wa_insdet1 WITH KEY pi = gs_1003-pi_no
                                                    ebeln = gs_1003-ebeln
                                                    ebelp = gs_1003-ebelp.
      IF sy-subrc = 0.
*    LOOP AT it_insdet1 INTO wa_insdet1.
*      gs_1003-ins_vendor = ins_ven.
*      gs_1003-ins_comp = ins_comp.
*      gs_1003-ins_add = ins_add.
*      gs_1003-exch_rate = ins_exch_rate.
*      gs_1003-ins_note_no = ins_note_no.
*      gs_1003-ins_exp_date = ins_exp_date.
*      gs_1003-ins_note_date = ins_note_date.
*      gs_1003-ins_note_val = ins_note_val.
*      gs_1003-rcpt_no = ins_rcpt_no.
*      gs_1003-ins_remarks = ins_remarks.
*        APPEND gs_1003 TO gt_1003.
        MOVE-CORRESPONDING wa_insdet1 TO gs_1003.
        MODIFY gt_1003 FROM gs_1003.
      ENDIF.
*      CLEAR: gs_1003.
    ENDLOOP.

    LOOP AT it_insdet INTO wa_insdet.
      READ TABLE it_insdet1 INTO wa_insdet1 WITH KEY pi = wa_insdet-pi
                                                 ebeln = wa_insdet-ebeln
                                                 ebelp = wa_insdet-ebelp.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING wa_insdet1 TO wa_insdet.
        MODIFY it_insdet FROM wa_insdet.
      ENDIF.
    ENDLOOP.
*    MODIFY zlc_process FROM @gs_1003.
    IF gt_1003 IS NOT INITIAL.

      CALL FUNCTION 'ENQUEUE_E_TABLE'
        EXPORTING
          mode_rstable   = 'E'
          tabname        = 'ZLC_PROCESS'
          _scope         = '2'
          _wait          = 'X'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      MODIFY zlc_process FROM TABLE gt_1003.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
        MESSAGE s004(zelc).
      ELSE.
        ROLLBACK WORK.
        MESSAGE e009(zelc).
      ENDIF.

      CALL FUNCTION 'DEQUEUE_E_TABLE'
        EXPORTING
          tabname = 'ZLC_PROCESS'
          _scope  = '3'.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SRCH_1003
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM srch_1003 .

  CLEAR it_insdet.
*  IF gs_1003 IS INITIAL.
*  SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1003 WHERE pi_no in @s_pi. "@inp_pi.
  SELECT * FROM zlc_process INTO CORRESPONDING FIELDS OF TABLE @gt_1003 WHERE pi_no IN @s_pi. "@inp_pi.

  IF gt_1003 IS NOT INITIAL.
    SELECT * FROM ekpo INTO TABLE @DATA(it_ekpo_1003) FOR ALL ENTRIES IN @gt_1003 WHERE ebeln = @gt_1003-ebeln
                                                                                   AND  ebelp = @gt_1003-ebelp.

  ENDIF.



  READ TABLE gt_1003 INTO gs_1003 INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE i000(zelc) WITH s_pi-low.
  ELSEIF gs_1003-pi_canc_ind = 'X'.
    MESSAGE i018(zelc) WITH s_pi-low.
*    CLEAR gs_1003.
    CLEAR s_pi.
  ELSE.

    IF gv_screenmode = 'INS_CRT'.
      LOOP AT gt_1003 ASSIGNING FIELD-SYMBOL(<fs_1003>).
        IF <fs_1003>-ins_vendor IS NOT INITIAL.
          MESSAGE 'Insurance Information already created for PI' TYPE 'I'.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF gv_screenmode = 'INS_MOD' OR gv_screenmode = 'INS_DISP'.
      IF ins_exch_rate IS INITIAL AND ins_note_no IS INITIAL AND ins_exp_date IS INITIAL
        AND ins_note_date IS INITIAL AND ins_note_val IS INITIAL AND ins_rcpt_no IS INITIAL AND ins_remarks IS INITIAL.
        ins_ven = gs_1003-ins_vendor.
        ins_comp = gs_1003-ins_comp.
        ins_add = gs_1003-ins_add.
        ins_exch_rate = gs_1003-exch_rate.
        ins_note_no = gs_1003-ins_note_no.
        ins_exp_date = gs_1003-ins_exp_date.
        ins_note_date = gs_1003-ins_note_date.
        ins_note_val = gs_1003-ins_note_val.
        ins_rcpt_no = gs_1003-rcpt_no.
        ins_remarks = gs_1003-ins_remarks.
      ENDIF.
    ENDIF.
  ENDIF.
*  ELSE.
*    ins_ven = gs_1003-ins_vendor.
*  ENDIF.
  LOOP AT gt_1003 INTO gs_1003.
    READ TABLE it_insdet1 INTO wa_insdet1 WITH KEY pi = gs_1003-pi_no
                                                  ebeln = gs_1003-ebeln
                                                  ebelp = gs_1003-ebelp.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_insdet1 TO gs_1003.
      MODIFY gt_1003 FROM gs_1003.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_1003 INTO gs_1003.
    wa_insdet-pi = gs_1003-pi_no.
    wa_insdet-ebeln = gs_1003-ebeln.
    wa_insdet-ebelp = gs_1003-ebelp.
    wa_insdet-ship_mode = gs_1003-ship_mode.
    IF gs_1003-ins_vendor IS NOT INITIAL.
      ins_ven = gs_1003-ins_vendor.
    ENDIF.

    READ TABLE it_ekpo_1003 INTO DATA(wa_ekpo) WITH KEY ebeln = gs_1003-ebeln
                                                        ebelp = gs_1003-ebelp.
    IF sy-subrc = 0..
      wa_insdet-matnr = wa_ekpo-matnr.
      wa_insdet-maktx = wa_ekpo-txz01.
    ENDIF.
*    IF gv_screenmode = 'INS_MOD'.
*      IF it_insdet1 IS INITIAL.
*        wa_insdet-risk_type = gs_1003-risk_type.
*        wa_insdet-amend_no = gs_1003-amend_no.
*        wa_insdet-policy_date = gs_1003-policy_date.
*      ELSE.
*        READ TABLE it_insdet1 INTO wa_insdet1 WITH KEY ebeln = gs_1003-ebeln.
*        IF sy-subrc = 0.
*          wa_insdet-risk_type = wa_insdet1-risk_type.
*          wa_insdet-amend_no = wa_insdet1-amend_no.
*          wa_insdet-policy_date = wa_insdet1-policy_date.
*        ENDIF.
*      ENDIF.
*    ENDIF.


*    IF gv_screenmode = 'INS_DISP'..
    wa_insdet-risk_type = gs_1003-risk_type.
    wa_insdet-amend_no = gs_1003-amend_no.
    wa_insdet-policy_date = gs_1003-policy_date.
*    ENDIF.
*    IF gv_screenmode = 'INS_CRT'.
*      READ TABLE it_insdet1 INTO wa_insdet1 WITH KEY ebeln = gs_1003-ebeln.
*      IF sy-subrc = 0.
*        wa_insdet-risk_type = wa_insdet1-risk_type.
*        wa_insdet-amend_no = wa_insdet1-amend_no.
*        wa_insdet-policy_date = wa_insdet1-policy_date.
*      ENDIF.
*    ENDIF.

    APPEND wa_insdet TO it_insdet.
    CLEAR wa_insdet.
  ENDLOOP.

  SORT it_insdet BY pi ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM it_insdet COMPARING pi ebeln ebelp.
  IF gv_disp_flag = abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CALL SCREEN 1003.
  CLEAR: it_insdet1, wa_insdet1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form LC_REPORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM lc_report .

  DATA: lv_maktx TYPE maktx.
  SELECT ebeln, pi_no, pi_dt, pi_amt, lifnr, waers, inco, lc_val, lc_no, lc_date, ship_date, lc_exp_date, lstel, vstel
    FROM zlc_process
    INTO CORRESPONDING FIELDS OF TABLE @gt_lc
     WHERE pi_no      IN @s_pino.
*      AND lc_no       IN @s_lcno
*      AND lc_date     IN @s_lcdt
*      AND lc_exp_date IN @s_lcexdt
*      AND ebeln       IN @s_pono
*      AND lifnr       IN @s_vendor.

  IF gt_lc IS NOT INITIAL.
    SELECT ebeln, matnr, meins, menge FROM ekpo INTO TABLE @DATA(lt_ekpo)
      FOR ALL ENTRIES IN @gt_lc WHERE ebeln = @gt_lc-ebeln.
  ENDIF.

  LOOP AT gt_lc INTO DATA(gs_lc).
    gs_final-pi_no  = gs_lc-pi_no.
    gs_final-pi_dt  = gs_lc-pi_dt.
    gs_final-pi_amt = gs_lc-pi_amt.
    gs_final-lifnr  = gs_lc-lifnr.
    READ TABLE lt_ekpo INTO DATA(ls_ekpo) WITH KEY ebeln = gs_lc-ebeln.
    IF sy-subrc = 0.
      gs_final-matnr       = ls_ekpo-matnr.
      gs_final-meins       = ls_ekpo-meins.
      gs_final-menge       = ls_ekpo-menge.
      SELECT SINGLE maktx INTO lv_maktx FROM makt WHERE matnr = ls_ekpo-matnr.
      IF sy-subrc = 0.
        gs_final-maktx       = lv_maktx.
      ENDIF.
    ENDIF.
    gs_final-waers       = gs_lc-waers.
    gs_final-inco        = gs_lc-inco.
    gs_final-lc_val      = gs_lc-lc_val.
    gs_final-lc_no       = gs_lc-lc_no.
    gs_final-lc_date     = gs_lc-lc_date.
    gs_final-ship_date   = gs_lc-ship_date.
    gs_final-lc_exp_date = gs_lc-lc_exp_date.
    gs_final-lstel       = gs_lc-lstel.
    gs_final-vstel       = gs_lc-vstel.

    APPEND gs_final TO gt_final.
    CLEAR: gs_final, gs_lc , ls_ekpo, lv_maktx.

  ENDLOOP.
*--------Preparing Field Catalogue------------->
  wa_fcat-fieldname  = 'PI_NO'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_m  = 'Proforma Invoice No'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'PI_DT'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_m  = 'Proforma Inv Date'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'PI_AMT'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_m  = 'Proforma Inv Amount'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'LIFNR'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_m  = 'Vendor/Supplier'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'MATNR'.
  wa_fcat-outputlen  = '15' .
  wa_fcat-seltext_m  = 'Material Code'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'MAKTX'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_m  = 'Material Description'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'MENGE'.
  wa_fcat-outputlen  = '15' .
  wa_fcat-seltext_m  = 'Qty'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'MEINS'.
  wa_fcat-outputlen  = '15' .
  wa_fcat-seltext_m  = 'Unit'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'LC_VAL'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_m  = 'LC Value'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'WAERS'.
  wa_fcat-outputlen  = '10' .
  wa_fcat-seltext_m  = 'Currency'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'INCO'.
  wa_fcat-outputlen  = '15' .
  wa_fcat-seltext_m  = 'Incoterms'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'LC_NO'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_m  = 'LC No'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'LC_DATE'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_m  = 'LC Date'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'SHIP_DATE'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_m  = 'Latest Shipment Date'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'LC_EXP_DATE'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_m  = 'LC Expiry Date'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'LSTEL'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_s = wa_fcat-seltext_m  = wa_fcat-seltext_l = 'Loading Port'.
  APPEND wa_fcat TO it_fcat.

  wa_fcat-fieldname  = 'VSTEL'.
  wa_fcat-outputlen  = '20' .
  wa_fcat-seltext_s = wa_fcat-seltext_m  = wa_fcat-seltext_l = 'Destination Port'.
  APPEND wa_fcat TO it_fcat.

  wa_layout-zebra             = 'X'.
  wa_layout-colwidth_optimize  = 'X'.

*---------------Calling ALV FM to display--------->
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK      = ' '
*     I_BYPASSING_BUFFER     = ' '
*     I_BUFFER_ACTIVE        = ' '
      i_callback_program     = sy-repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
      i_callback_top_of_page = 'TOP_OF_PAGE'
      is_layout              = wa_layout
      it_fieldcat            = it_fcat
    TABLES
      t_outtab               = gt_final
*   EXCEPTIONS
*     PROGRAM_ERROR          = 1
*     OTHERS                 = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  REFRESH: gt_final,it_fcat.
ENDFORM.
FORM top_of_page.

  PERFORM comment_build  USING lf_list_top_of_page[].
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      i_logo             = 'ZEPGL_LOGO'
      it_list_commentary = lf_list_top_of_page.
*      i_end_of_list_grid = space.
*       i_alv_form         = 'X'.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*& Form COMMENT_BUILD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LF_LIST_TOP_OF_PAGE[]
*&---------------------------------------------------------------------*
FORM comment_build  USING u_lf_list_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader.
  DATA: str  TYPE string,
        str1 TYPE string,
        str2 TYPE string.
  REFRESH u_lf_list_top_of_page.

  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info  = 'LETTER OF CREDIT REPORT'.
  APPEND ls_line TO u_lf_list_top_of_page.

  CLEAR: str, str1, str2.
  CONCATENATE str1 sy-datum+6(2) ':' sy-datum+4(2) ':' sy-datum(4) INTO str1.
  CONCATENATE str 'DATE:' str1 INTO str SEPARATED BY space.

  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info  = str.
  APPEND ls_line TO u_lf_list_top_of_page.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEARCH_1001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM search_1001.

  SELECT * FROM zlc_process INTO CORRESPONDING FIELDS OF TABLE gt_1001 WHERE pi_no = inp_pi.
  IF sy-subrc NE 0.
    IF gv_screenmode NE 'PI_CRT'.
      MESSAGE w000(zelc) WITH inp_ebeln.
      CALL SCREEN 1001.
    ENDIF.
  ELSE.
*    IF gv_screenmode EQ 'PI_CRT'.
*      CLEAR gs_1001.
*      REFRESH it_podet.
*      CLEAR: zlc_process-pi_dt, zlc_process-lddat.
*      MESSAGE w007(zelc) WITH gs_1001-pi_no.
*      CALL SCREEN 1001.
*    ENDIF.
  ENDIF.


  IF sy-ucomm NE 'BACK'.
    IF gs_1001-pi_canc_ind = 'X'.
      MESSAGE w018(zelc) WITH inp_pi.
      CLEAR gs_1001.
      REFRESH it_podet.
    ENDIF.
  ENDIF.

  LOOP AT gt_1001 INTO gs_1001.
    s_ebeln-sign = 'I'.
    s_ebeln-option = 'EQ'.
    s_ebeln-low = gs_1001-ebeln.
    APPEND s_ebeln.
    CLEAR: s_ebeln.
  ENDLOOP.

  READ TABLE gt_1001 INTO gs_1001 INDEX 1.
  IF sy-subrc = 0.
    sup_bank = gs_1001-sup_bank_vendor.
    zlc_process-pi_dt = gs_1001-pi_dt.
    zlc_process-lddat = gs_1001-lddat.
  ENDIF.

  PERFORM fill_po.

*  IF inp_pi IS INITIAL.
*    MESSAGE i000(zelc) WITH inp_pi.
*  ENDIF.
  IF gv_disp_flag = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'GRP'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CALL SCREEN 1001.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INS_LETTER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ins_letter .

*  BEGIN OF ADDITION RITAM 23.02.2021
  DATA: pi_tab  TYPE ztt_pi,
        pi_line TYPE zty_pi.

  LOOP AT s_pi.
    pi_line-pi = s_pi-low.
    APPEND pi_line TO pi_tab.
    CLEAR: pi_line.
  ENDLOOP.

  CALL FUNCTION 'Z_INS_COVER_NOTE'
    EXPORTING
      pi_tab = pi_tab.
*  END OF ADDITION RITAM 23.02.2021



  """""""""""""""""""""""""""""""""""""""""
*BEGIN OF COMMENT RITAM 23.02.2021
*
*  CLEAR: gs_ssfcompop, gs_control, gv_devtype, gv_job_output, form_name, fm_name.
*
*  DATA: gt_po_details    TYPE ztt_bank_letter,
*        gs_po_details    TYPE zstr_bank_letter,
*        lv_sea_freight   TYPE p LENGTH 6 DECIMALS 2,
*        lv_air_freight   TYPE p LENGTH 6 DECIMALS 2,
*        lv_truck_freight TYPE p LENGTH 6 DECIMALS 2,
*        lv_name1         TYPE ad_name1,
*        lv_city2         TYPE ad_city2,
*        lv_street        TYPE ad_street,
*        lv_post          TYPE ad_pstcd1,
*        lv_address       TYPE char100.
*
*
*
*  SELECT a~ebeln, a~ebelp, a~txz01, a~matnr, a~menge, a~meins, a~packno, a~netwr, b~maktx
*    FROM ekpo AS a LEFT OUTER JOIN makt AS b ON a~matnr = b~matnr
*    INTO TABLE @DATA(gt_po)
*    WHERE a~ebeln = @gs_1003-ebeln.
**    AND b~spras = @sy-langu.
*
*  IF gt_po IS NOT INITIAL.
*
*    SELECT a~packno, a~sub_packno, a~extrow, b~packno AS item, b~extrow AS line, b~menge, b~meins, b~ktext1, b~netwr
*      FROM esll AS a INNER JOIN esll AS b ON a~sub_packno = b~packno
*      INTO TABLE @DATA(lt_ser)
*      FOR ALL ENTRIES IN @gt_po WHERE a~packno = @gt_po-packno.
*
*    SELECT ebeln, bukrs, bsart, knumv, lifnr, zterm, inco1, inco2 FROM ekko INTO TABLE @DATA(gt_ekko) FOR ALL ENTRIES IN @gt_po
*                                                                                            WHERE ebeln = @gt_po-ebeln.
*    SELECT knumv, kposn, kbetr,kschl
*  FROM prcd_elements INTO TABLE @DATA(lt_price) FOR ALL ENTRIES IN @gt_ekko WHERE knumv = @gt_ekko-knumv.
*
**BOC by Arka on 03.07.2020
*    SELECT SINGLE adrnr FROM t001 INTO @DATA(lv_adrnr) WHERE bukrs = @gs_1003-bukrs.
*
*    SELECT SINGLE name1 street city2 post_code1 FROM adrc INTO ( lv_name1,lv_street,lv_city2,lv_post ) WHERE addrnumber = lv_adrnr.
*
*    CONCATENATE lv_street ',' lv_city2 '-' lv_post INTO lv_address SEPARATED BY space.
**EOC by Arka on 03.07.2020
*    IF lt_ser IS INITIAL.
*      LOOP AT gt_po INTO DATA(gs_po).
**BOC by Arka on 04.02.2020 to add the freight charges
*        READ TABLE gt_ekko INTO DATA(gs_ekko) WITH KEY ebeln = gs_po-ebeln.
*        READ TABLE lt_price INTO DATA(ls_price) WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRF'.
*        IF sy-subrc = 0.
*          lv_sea_freight = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRA'.
*        IF sy-subrc = 0.
*          lv_air_freight = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRT'.
*        IF sy-subrc = 0.
*          lv_truck_freight = ls_price-kbetr.
*        ENDIF.
**EOC by Arka on 04.02.2020 to add the freight charges
*        gs_po_details-matnr = gs_po-matnr.
*        IF gs_po-maktx IS INITIAL.
*          gs_po_details-maktx = gs_po-txz01.
*        ELSE.
*          gs_po_details-maktx = gs_po-maktx.
*        ENDIF.
*
*        gs_po_details-meins = gs_po-meins.
*        gs_po_details-menge = gs_po-menge.
*        gs_po_details-value = gs_po-netwr + lv_sea_freight +  lv_air_freight +  lv_truck_freight. "inserted by Arka on 04.02.2020
*        APPEND gs_po_details TO gt_po_details.
*        CLEAR gs_po_details.
*      ENDLOOP.
*    ELSE.
*      LOOP AT lt_ser INTO DATA(ls_ser).
*        gs_po_details-maktx = ls_ser-ktext1.
*        gs_po_details-menge = ls_ser-menge.
*        gs_po_details-meins = ls_ser-meins.
*        gs_po_details-value = ls_ser-netwr + lv_sea_freight +  lv_air_freight +  lv_truck_freight. "inserted by Arka on 04.02.2020
*        APPEND gs_po_details TO gt_po_details.
*        CLEAR gs_po_details.
*      ENDLOOP.
*    ENDIF.
*
*  ENDIF.
*
*  form_name = 'ZLC_INSURANCE_MARINE_COVER'.
*
*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
*      formname           = form_name
**     VARIANT            = ' '
**     DIRECT_CALL        = ' '
*    IMPORTING
*      fm_name            = fm_name
*    EXCEPTIONS
*      no_form            = 1
*      no_function_module = 2
*      OTHERS             = 3.
*
*  IF sy-subrc = 0.
*
*    CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
*      EXPORTING
*        i_language             = sy-langu
*      IMPORTING
*        e_devtype              = gv_devtype
*      EXCEPTIONS
*        no_language            = 1
*        language_not_installed = 2
*        no_devtype_found       = 3
*        system_error           = 4
*        OTHERS                 = 5.
*
*    gs_ssfcompop-tdprinter = gv_devtype.
**Suppress print dialog
*    gs_control-no_dialog = 'X'.
*    gs_control-getotf = 'X'.
*
**    CALL FUNCTION '/1BCDWB/SF00000058'
**      EXPORTING
***       ARCHIVE_INDEX              =
***       ARCHIVE_INDEX_TAB          =
***       ARCHIVE_PARAMETERS         =
**       CONTROL_PARAMETERS         = gs_control
***       MAIL_APPL_OBJ              =
***       MAIL_RECIPIENT             =
***       MAIL_SENDER                =
**        OUTPUT_OPTIONS             = gs_ssfcompop
***       USER_SETTINGS              = 'X'
**        GS_DETAILS                 = gs_1003
**     IMPORTING
***       DOCUMENT_OUTPUT_INFO       =
**       JOB_OUTPUT_INFO            = gv_job_output
***       JOB_OUTPUT_OPTIONS         =
**      TABLES
**        GT_PO_DETAILS              =
***     EXCEPTIONS
***       FORMATTING_ERROR           = 1
***       INTERNAL_ERROR             = 2
***       SEND_ERROR                 = 3
***       USER_CANCELED              = 4
***       OTHERS                     = 5
**              .
**    IF SY-SUBRC <> 0.
*** Implement suitable error handling here
**    ENDIF.
**
**
**
***
*    CALL FUNCTION fm_name "'/1BCDWB/SF00000058'
*      EXPORTING
**       ARCHIVE_INDEX      =
**       ARCHIVE_INDEX_TAB  =
**       ARCHIVE_PARAMETERS =
*        control_parameters = gs_control
**       MAIL_APPL_OBJ      =
**       MAIL_RECIPIENT     =
**       MAIL_SENDER        =
*        output_options     = gs_ssfcompop
**       USER_SETTINGS      = 'X'
*        gs_details         = gs_1003
*        lv_name1           = lv_name1
*        lv_address         = lv_address
*      IMPORTING
**       DOCUMENT_OUTPUT_INFO       =
*        job_output_info    = gv_job_output
**       JOB_OUTPUT_OPTIONS =
*      TABLES
*        gt_po_details      = gt_po_details
*      EXCEPTIONS
*        formatting_error   = 1
*        internal_error     = 2
*        send_error         = 3
*        user_canceled      = 4
*        OTHERS             = 5.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*
*
*
*
*
*
*    CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
*      EXPORTING
*        i_otf                    = gv_job_output-otfdata
*      EXCEPTIONS
*        convert_otf_to_pdf_error = 1
*        cntl_error               = 2
*        OTHERS                   = 3.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*  ENDIF.
*
*END OF COMMENT RITAM 23.02.2021
  """""""""""""""""""""""""""""""""""

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DECISION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM get_decision  USING    VALUE(p_key)
                            VALUE(p_domain).

  DATA: lv_domain TYPE dd07l-domname.

  CLEAR: gs_list, gv_id.
  REFRESH: gt_dom_val1, gt_dom_val2, gt_list.

  gv_id = p_key.
  lv_domain = p_domain.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = lv_domain
*     TEXT            = 'X'
*     FILL_DD07L_TAB  = ' '
    TABLES
      values_tab      = gt_dom_val1
      values_dd07l    = gt_dom_val2
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT gt_dom_val1 INTO DATA(gs_doma_val).
    gs_list-key = gs_doma_val-domvalue_l.
    APPEND gs_list TO gt_list.
    CLEAR gs_list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gv_id
      values          = gt_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BANK_LETTER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM bank_letter .

  DATA: pi_line TYPE zty_pi,
        pi_tab  TYPE ztt_pi.

  LOOP AT s_pi.
    pi_line-pi = s_pi-low.
    APPEND pi_line TO pi_tab.
    CLEAR: pi_line.
  ENDLOOP.

  CALL FUNCTION 'Z_BANK_LETTER'
    EXPORTING
      pi_tab = pi_tab.


*  BEGIN OF COMMENT RITAM 26.02.2021
*
*  DATA: gt_po_details    TYPE ztt_bank_letter,
*        gs_po_details    TYPE zstr_bank_letter,
*        ls_seqno         TYPE znumber,
*        lv_seqno         TYPE znumb,
*        lv_year          TYPE gjahr,
*        lv_dmbtr         TYPE dmbtr,
*        lv_name1         TYPE ad_name1,
*        lv_city2         TYPE ad_city2,
*        lv_street        TYPE ad_street,
*        lv_post          TYPE ad_pstcd1,
*        lv_address       TYPE char100,
*        lv_sea_freight   TYPE p LENGTH 6 DECIMALS 2,
*        lv_air_freight   TYPE p LENGTH 6 DECIMALS 2,
*        lv_truck_freight TYPE p LENGTH 6 DECIMALS 2.
*
*  CLEAR: gs_ssfcompop, gs_control, gv_devtype, gv_job_output, form_name, fm_name.
*  IF gs_1002-crsp_type = 'TT'.
*    form_name = 'ZLC_BANK_TT_LETTER_RAD'.
*  ELSEIF gs_1002-crsp_type = 'LC'.
*    form_name = 'ZLC_BANK_LC_LETTER_RAD'.
*  ELSEIF gs_1002-crsp_type = 'LCAF'.
*    form_name = 'ZLC_BANK_LCA_LETTER_RAD'.
*  ELSE.
*    form_name = 'ZELC_BANK_LETTER'.
*  ENDIF.
**BOC by Arka on 03.07.2020
*  SELECT SINGLE adrnr FROM t001 INTO @DATA(lv_adrnr) WHERE bukrs = @gs_1002-bukrs.
*
*  SELECT SINGLE name1 street city2 post_code1 FROM adrc INTO (lv_name1,lv_street,lv_city2,lv_post) WHERE addrnumber = lv_adrnr.
*
*  CONCATENATE lv_street ',' lv_city2 '-' lv_post INTO lv_address SEPARATED BY space.
**EOC by Arka on 03.07.2020
*
*  SELECT SINGLE * FROM znumber INTO ls_seqno WHERE ztcode = sy-tcode.
*  IF sy-subrc = 0.
*    lv_seqno = ls_seqno-znum + 1.
*  ELSE.
*    lv_seqno = '1'.
*    IF gs_1002-crsp_type = 'TT'.
*      ls_seqno-ztcode = 'ZBL_TT'.
*    ELSEIF gs_1002-crsp_type = 'LC'.
*      ls_seqno-ztcode = 'ZBL_LC'.
*    ELSEIF gs_1002-crsp_type = 'LCAF'.
*      ls_seqno-ztcode = 'ZBL_LCAF'.
*    ENDIF.
**    ls_seqno-znum = LV_SEQNO.
*    ls_seqno-zyear = sy-datum+0(4).
*
*  ENDIF.
*  lv_year = sy-datum+0(4).
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = lv_seqno
*    IMPORTING
*      output = lv_seqno.
*
*  ls_seqno-znum = lv_seqno.
*
*  MODIFY znumber FROM ls_seqno.
*
*  SELECT a~ebeln, a~ebelp, a~txz01, a~matnr, a~menge, a~meins, a~netwr, a~packno, b~maktx
*    FROM ekpo AS a LEFT OUTER JOIN makt AS b ON a~matnr = b~matnr
*    INTO TABLE @DATA(gt_po)
*    WHERE a~ebeln = @gs_1002-ebeln.
**    AND b~spras = @sy-langu.
*
*  IF gt_po IS NOT INITIAL.
*
*    SELECT a~packno, a~sub_packno, a~extrow, b~packno AS item, b~extrow AS line, b~menge, b~meins, b~ktext1, b~netwr
*      FROM esll AS a INNER JOIN esll AS b ON a~sub_packno = b~packno
*      INTO TABLE @DATA(lt_ser)
*      FOR ALL ENTRIES IN @gt_po WHERE a~packno = @gt_po-packno.
*
*    SELECT ebeln, bukrs, bsart, knumv, lifnr, zterm, inco1, inco2 FROM ekko INTO TABLE @DATA(gt_ekko) FOR ALL ENTRIES IN @gt_po
*                                                                                            WHERE ebeln = @gt_po-ebeln.
*    SELECT knumv, kposn, kbetr,kschl
*  FROM prcd_elements INTO TABLE @DATA(lt_price) FOR ALL ENTRIES IN @gt_ekko WHERE knumv = @gt_ekko-knumv.
*
*
*    IF lt_ser IS INITIAL.
*      LOOP AT gt_po INTO DATA(gs_po).
**BOC by Arka on 04.02.2020 to add the freight charges
*        READ TABLE gt_ekko INTO DATA(gs_ekko) WITH KEY ebeln = gs_po-ebeln.
*        READ TABLE lt_price INTO DATA(ls_price) WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRF'.
*        IF sy-subrc = 0.
*          lv_sea_freight = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRA'.
*        IF sy-subrc = 0.
*          lv_air_freight = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRT'.
*        IF sy-subrc = 0.
*          lv_truck_freight = ls_price-kbetr.
*        ENDIF.
**BOC by Arka on 04.02.2020 to add the freight charges
*        gs_po_details-matnr = gs_po-matnr.
*        IF gs_po-maktx IS INITIAL.
*          gs_po_details-maktx = gs_po-txz01.
*        ELSE.
*          gs_po_details-maktx = gs_po-maktx.
*        ENDIF.
*        gs_po_details-meins = gs_po-meins.
*        gs_po_details-menge = gs_po-menge.  "gs_1002-PI_QTY.
**        gs_po_details-value = gs_po-netwr."gs_1002-PI_AMT.
*        gs_po_details-value = gs_po-netwr +  lv_sea_freight +  lv_air_freight +  lv_truck_freight. "inserted by Arka on 04.02.2020
*        CLEAR: lv_sea_freight, lv_air_freight, lv_truck_freight.
*        lv_dmbtr = lv_dmbtr + gs_po-netwr.
*        APPEND gs_po_details TO gt_po_details.
*        CLEAR gs_po_details.
*      ENDLOOP.
*    ELSE.
*      LOOP AT lt_ser INTO DATA(ls_ser).
*        gs_po_details-maktx = ls_ser-ktext1.
*        gs_po_details-menge = ls_ser-menge.
*        gs_po_details-meins = ls_ser-meins.
*        gs_po_details-value = ls_ser-netwr.
*        lv_dmbtr = lv_dmbtr + gs_po-netwr.
*        APPEND gs_po_details TO gt_po_details.
*        CLEAR gs_po_details.
*      ENDLOOP.
*    ENDIF.
*
*  ENDIF.
*  IF gs_1002-crsp_type = 'TT'.
*    gs_1002-pi_amt = lv_dmbtr.
*  ENDIF.
*
*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
*      formname           = form_name
**     VARIANT            = ' '
**     DIRECT_CALL        = ' '
*    IMPORTING
*      fm_name            = fm_name
*    EXCEPTIONS
*      no_form            = 1
*      no_function_module = 2
*      OTHERS             = 3.
*
*  IF sy-subrc = 0.
*
*    CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
*      EXPORTING
*        i_language             = sy-langu
*      IMPORTING
*        e_devtype              = gv_devtype
*      EXCEPTIONS
*        no_language            = 1
*        language_not_installed = 2
*        no_devtype_found       = 3
*        system_error           = 4
*        OTHERS                 = 5.
*
*    gs_ssfcompop-tdprinter = gv_devtype.
**Suppress print dialog
*    gs_control-no_dialog = 'X'.
*    gs_control-getotf = 'X'.
*
*    CALL FUNCTION fm_name "'/1BCDWB/SF00000001'
*      EXPORTING
**       ARCHIVE_INDEX      =
**       ARCHIVE_INDEX_TAB  =
**       ARCHIVE_PARAMETERS =
*        control_parameters = gs_control
**       MAIL_APPL_OBJ      =
**       MAIL_RECIPIENT     =
**       MAIL_SENDER        =
*        output_options     = gs_ssfcompop
**       USER_SETTINGS      = 'X'
*        gs_details         = gs_1002
*        lv_seqno           = lv_seqno
*        lv_year            = lv_year
*        lv_name1           = lv_name1
*        lv_address         = lv_address
*      IMPORTING
**       DOCUMENT_OUTPUT_INFO       =
*        job_output_info    = gv_job_output
**       JOB_OUTPUT_OPTIONS =
*      TABLES
*        gt_po_details      = gt_po_details
*      EXCEPTIONS
*        formatting_error   = 1
*        internal_error     = 2
*        send_error         = 3
*        user_canceled      = 4
*        OTHERS             = 5.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
*      EXPORTING
*        i_otf                    = gv_job_output-otfdata
*      EXCEPTIONS
*        convert_otf_to_pdf_error = 1
*        cntl_error               = 2
*        OTHERS                   = 3.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*  ENDIF.
*
*  END OF COMMENT 26.02.2021

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_RISKTYPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_risktype .

  CLEAR: gv_id, gs_list.
  REFRESH: gt_dom_val1, gt_dom_val2, gt_list, gt_lctype.

  gv_id = 'GS_1003-RISK_TYPE'.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZRISK_TYPE'
*     TEXT            = 'X'
*     FILL_DD07L_TAB  = ' '
    TABLES
      values_tab      = gt_dom_val1
      values_dd07l    = gt_dom_val2
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT gt_dom_val1 INTO DATA(gs_doma_val).

    gs_lctype-key = gs_doma_val-domvalue_l.
    gs_lctype-text = gs_doma_val-ddtext.

    APPEND gs_lctype TO gt_lctype.
    CLEAR gs_lctype.

  ENDLOOP.

**  gs_list-key = 'Risk 1'.
**  APPEND gs_list TO gt_list.
**  CLEAR gs_list.
**
**  gs_list-key = 'Risk 2'.
**  APPEND gs_list TO gt_list.
**  CLEAR gs_list.
**
**  gs_list-key = 'Risk 3'.
**  APPEND gs_list TO gt_list.
**  CLEAR gs_list.
**
**  gs_list-key = 'Risk 4'.
**  APPEND gs_list TO gt_list.
**  CLEAR gs_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gv_id
      values          = gt_lctype
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CORTYPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_cortype .

  CLEAR: gv_id, gs_list, gs_lctype.
  REFRESH: gt_dom_val1, gt_dom_val2, gt_list, gt_lctype.

  gv_id = 'GS_1001-CRSP_TYPE'.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZCRSP_TYPE'
*     TEXT            = 'X'
*     FILL_DD07L_TAB  = ' '
    TABLES
      values_tab      = gt_dom_val1
      values_dd07l    = gt_dom_val2
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT gt_dom_val1 INTO DATA(gs_doma_val).

    gs_lctype-key = gs_doma_val-domvalue_l.
    gs_lctype-text = gs_doma_val-ddtext.

    APPEND gs_lctype TO gt_lctype.
    CLEAR gs_lctype.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gv_id
      values          = gt_lctype
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*{   INSERT         RSDK900796                                        1
*&---------------------------------------------------------------------*
*& Form GET_PAYTYPE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_paytype .
  REFRESH gt_dropbox.
  gv_id = 'GS_1001-PI_PYMT'.
  gs_dropbox-key = 'AT SIGHT'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.
  gs_dropbox-key = '30 DAYS'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.
  gs_dropbox-key = '60 DAYS'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.
  gs_dropbox-key = '90 DAYS'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.
  gs_dropbox-key = '120 DAYS'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.
  gs_dropbox-key = '180 DAYS'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.
  gs_dropbox-key = '360 DAYS'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.
  gs_dropbox-key = 'CUSTOMISED'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.
  gs_dropbox-key = 'ADVANCE'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gv_id
      values          = gt_dropbox
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

*}   INSERT

*{   INSERT         RSDK900796                                        2
*&---------------------------------------------------------------------*
*& Form GET_SHIPMODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_shipmode .
  REFRESH gt_dropbox.
  gv_id = 'WA_PODET-SHIP_MODE'.
  gs_dropbox-key = 'AIR'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.
  gs_dropbox-key = 'SEA'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.
  gs_dropbox-key = 'ROAD'.
  APPEND gs_dropbox TO gt_dropbox.
  CLEAR gs_dropbox.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gv_id
      values          = gt_dropbox
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.
*}   INSERT

*{   INSERT         RSDK900796                                        3
*&---------------------------------------------------------------------*
*& Form CANC_PI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM canc_pi .
  SELECT * FROM zlc_process INTO CORRESPONDING FIELDS OF TABLE gt_1001 WHERE pi_no eq inp_pi.
  LOOP AT gt_1001 INTO gs_1001.
    gs_1001-pi_canc_ind = 'X'.
    MODIFY gt_1001 FROM gs_1001.
  ENDLOOP.
  IF gt_1001 IS NOT INITIAL.
    MODIFY zlc_process FROM TABLE gt_1001.
    COMMIT WORK.
    MESSAGE s018(zelc) WITH inp_pi.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CANC_LC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM canc_lc .
  SELECT  * FROM zlc_process INTO CORRESPONDING FIELDS OF TABLE gt_1004 WHERE lc_no = inp_lc.
  LOOP AT gt_1004 INTO gs_1004.
    gs_1004-lc_canc_ind = 'X'.
    MODIFY gt_1004 FROM gs_1004.
    CLEAR gs_1004.
  ENDLOOP.
  MODIFY zlc_process FROM TABLE gt_1004.
  COMMIT WORK.
  MESSAGE s020(zelc) WITH inp_lc.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form LIST_PI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM list_pi .
  EXPORT pi_low TO MEMORY ID 'list'.
  EXPORT pi_high TO MEMORY ID 'list1'.
  EXPORT date_low TO MEMORY ID 'list2'.
  EXPORT date_high TO MEMORY ID 'list3'.
  EXPORT plant_low TO MEMORY ID 'list4'.
  EXPORT plant_high TO MEMORY ID 'list5'.


  CALL TRANSACTION 'ZPI_LIST'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form LIST_LC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM list_lc .
  EXPORT lc_low TO MEMORY ID 'ABC'.
  EXPORT lc_high TO MEMORY ID 'ABC1'.
  EXPORT plant_low TO MEMORY ID 'ABC2'.
  EXPORT plant_high TO MEMORY ID 'ABC3'.
  EXPORT date_low TO MEMORY ID 'ABC4'.
  EXPORT date_high TO MEMORY ID 'ABC5'.

  CALL  TRANSACTION 'ZLC_LIST'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INS_PO_LETTER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ins_po_letter .
*******************  MKUMARI
******************    Insurance Policy Letter
  TYPES : BEGIN OF lty_product,
            productname TYPE string,
            quantity    TYPE string,
            unit        TYPE string,
          END  OF lty_product.

  DATA : lt_product           TYPE STANDARD TABLE OF lty_product,
         ls_product           TYPE lty_product,
         ls_productname(1000) TYPE c,
         ls_quantity(1000)    TYPE c,
         lv_name1             TYPE ad_name1,
         lv_city2             TYPE ad_city2,
         lv_street            TYPE ad_street,
         lv_post              TYPE ad_pstcd1,
         lv_address           TYPE char100,
         lv_index             TYPE sy-index,
         f_day                TYPE char2,
         f_month              TYPE char10,
         f_year               TYPE char4.


  CLEAR:               gs_ssfcompop, gs_control, gv_devtype, gv_job_output, form_name, fm_name.

*  SELECT ebeln, txz01,menge,meins FROM ekpo INTO TABLE @DATA(lt_zlcprocess) WHERE ebeln = @gs_1005-ebeln.
*  IF sy-subrc = 0.
*
*    LOOP AT lt_zlcprocess INTO DATA(ls_zlcprocess).
*      ls_product-productname = ls_zlcprocess-txz01.
*      ls_productname         = ls_productname && |,| &&  ls_product-productname.
*      ls_product-quantity    = ls_zlcprocess-menge && |  | && ls_zlcprocess-meins.
*      ls_quantity            = ls_quantity && |,|  &&   ls_product-quantity .
*      CLEAR ls_product.
*    ENDLOOP.
*  ENDIF.

  ls_productname  = ls_productname+1(999) .
  ls_quantity  = ls_quantity+1(999) .

  CLEAR: gs_ssfcompop, gs_control, gv_devtype, gv_job_output, form_name, fm_name.

  DATA: gt_po_details TYPE TABLE OF zstr_ins_pol_item,
        gs_po_details TYPE zstr_ins_pol_item,
        wa_header     TYPE zstr_ins_pol_header,
        wa_zins       TYPE zins_policy.

*  data : count type i.

  READ TABLE gt_1005 INTO gs_1005 INDEX 1.
  IF sy-subrc = 0.
    wa_header-bukrs = gs_1005-bukrs.

    SELECT SINGLE name1,
                  street,
                  city1,
                  post_code1,
                  landx50
      INTO @DATA(wa_adrc5)
      FROM t001 AS t
      LEFT OUTER JOIN adrc AS ad ON t~adrnr = ad~addrnumber
      LEFT OUTER JOIN t005t AS t5 ON ad~country = t5~land1 AND
                                     t5~spras = @sy-langu
      WHERE t~bukrs = @gs_1005-bukrs.


    IF sy-subrc = 0.
      wa_header-comp_name = wa_adrc5-name1.
      wa_header-comp_add = |{ wa_adrc5-street }, { wa_adrc5-city1 }-{ wa_adrc5-post_code1 }, { wa_adrc5-landx50 }|.
    ENDIF.

*  -------------------reference no ----------------------------

    SELECT MAX( inc )
      FROM zins_policy
      INTO @DATA(lv_inc).

    IF sy-subrc <> 0.
      lv_inc = '0001'.
    ELSE.
      lv_inc = lv_inc + 1.
    ENDIF.

    wa_zins-mandt = sy-mandt.
    wa_zins-inc = lv_inc.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = 'ZINS_POLICY'
        _scope         = '2'
        _wait          = 'X'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    MODIFY zins_policy FROM wa_zins.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = 'ZINS_POLICY'
        _scope       = '3'.

* ---------- End of reference -------------

    SELECT SINGLE sort2
      INTO @DATA(lv_sort2)
        FROM lfa1 AS lf
        LEFT OUTER JOIN adrc AS ad ON lf~adrnr = ad~addrnumber
        WHERE lifnr = @gs_1005-ins_vendor.

    CASE wa_header-bukrs.
      WHEN '1000'.
        wa_header-bin = '000002071-0102'.
        wa_header-irc = '260326120299120'.
        wa_header-ref = |RPL/CMR/{ lv_sort2 }/MPOL/{ sy-datum(4) }/{ lv_inc }|.
      WHEN '2000'.
        wa_header-bin = '000080701-0102'.
        wa_header-irc = '260326120244920'.
        wa_header-ref = |RNL/CMR/{ lv_sort2 }/MPOL/{ sy-datum(4) }/{ lv_inc }|.
      WHEN '3000'.
        wa_header-bin = '000082993-0102'.
        wa_header-irc = '260326120240420'.
        wa_header-ref = |PCL/CMR/{ lv_sort2 }/MPOL/{ sy-datum(4) }/{ lv_inc }|.
      WHEN '5000'.
        wa_header-bin = '000092768-0102'.
        wa_header-irc = '260326110314219'.
        wa_header-ref = |RBCL/CMR/{ lv_sort2 }/MPOL/{ sy-datum(4) }/{ lv_inc }|.
      WHEN '9000'.
        wa_header-bin = '000278899-0102'.
        wa_header-irc = '260326110342819'.
        wa_header-ref = |REXIME/CMR/{ lv_sort2 }/MPOL/{ sy-datum(4) }/{ lv_inc }|.
    ENDCASE.

    SELECT SINGLE str_suppl3
      INTO @DATA(lc_branch)
      FROM lfa1 AS lf
      LEFT OUTER JOIN adrc AS ad ON lf~adrnr = ad~addrnumber
      WHERE lifnr = @gs_1005-lc_bank_vendor.

    IF sy-subrc = 0.
      wa_header-zlc_branch = lc_branch.
    ENDIF.
    .
    CALL FUNCTION 'J_1UF_DATE'
      EXPORTING
        f_date  = sy-datum
      IMPORTING
        f_day   = f_day
        f_month = f_month
        f_year  = f_year.
    wa_header-print_date = |{ f_day } { f_month } { f_year }|.
    CLEAR: f_day, f_month, f_year.
  ENDIF.

  SELECT ebeln, ebelp, txz01, matnr, menge, meins, packno, netwr
    FROM ekpo
    INTO TABLE @DATA(it_ekpo)
    FOR ALL ENTRIES IN @gt_1005
    WHERE ebeln = @gt_1005-ebeln AND
          ebelp = @gt_1005-ebelp.


*  SELECT a~ebeln, a~ebelp, a~txz01, a~matnr, a~menge, a~meins, a~packno, a~netwr, b~maktx
*   FROM ekpo AS a LEFT OUTER JOIN makt AS b ON a~matnr = b~matnr
*   INTO TABLE @DATA(gt_po)
*   WHERE b~spras = @sy-langu
*  AND  b~matnr = a~matnr.


  SELECT matnr, maktx
    FROM makt
    INTO TABLE @DATA(it_makt)
    FOR ALL ENTRIES IN @it_ekpo
    WHERE matnr = @it_ekpo-matnr
    AND spras = @sy-langu.

  IF it_ekpo IS NOT INITIAL.
    SELECT a~packno, a~sub_packno, a~extrow, b~packno AS item, b~extrow AS line, b~menge, b~meins, b~ktext1, b~netwr
      FROM esll AS a INNER JOIN esll AS b ON a~sub_packno = b~packno
      INTO TABLE @DATA(lt_ser)
      FOR ALL ENTRIES IN @it_ekpo WHERE a~packno = @it_ekpo-packno.

    IF lt_ser IS INITIAL.

      CLEAR lv_index.
      LOOP AT gt_1005 INTO gs_1005.
        ADD 1 TO lv_index.
        gs_po_details-sl_no = lv_index.
        READ TABLE  it_ekpo INTO DATA(gs_po) WITH KEY ebeln = gs_1005-ebeln
                                                      ebelp = gs_1005-ebelp.
        IF sy-subrc = 0.
          READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr = gs_po-matnr.


          " gs_po_details-matnr = gs_po-matnr.
          IF wa_makt-maktx IS INITIAL.
            gs_po_details-maktx = gs_po-txz01.
          ELSE.
            gs_po_details-maktx = wa_makt-maktx.
          ENDIF.

          gs_po_details-meins = gs_po-meins.
          gs_po_details-menge = gs_po-menge.
        ENDIF.
        gs_po_details-hs_code = gs_1005-hs_code.
        gs_po_details-lstel = gs_1005-lstel.
        gs_po_details-vstel = gs_1005-vstel.
        " gs_po_details-value = gs_po-netwr.
        APPEND gs_po_details TO gt_po_details.
        CLEAR gs_po_details.

      ENDLOOP.
    ELSE.
      LOOP AT lt_ser INTO DATA(ls_ser).
        ADD 1 TO lv_index.
        gs_po_details-sl_no = lv_index.
        gs_po_details-maktx = ls_ser-ktext1.
        gs_po_details-menge = ls_ser-menge.
        gs_po_details-meins = ls_ser-meins.
        "     gs_po_details-value = ls_ser-netwr.
*        lv_dmbtr = lv_dmbtr + gs_po-netwr.
        APPEND gs_po_details TO gt_po_details.
        CLEAR gs_po_details.
      ENDLOOP.
    ENDIF.

  ENDIF.

*  LOOP AT gt_po_details ASSIGNING FIELD-SYMBOL(<fs_gt_po_details>).
*
*    count = count+1.
*    <fs_gt_po_details>-sl_no = count.

  "form_name = 'ZLC_INSURANCE_MARINE_POLICY'.      "COMMENTED BY ZINIA ON 01.02.2021
  form_name = 'ZINSURANCE_POLICY_LETTER'.       "INSERTED BY ZINIA ON 01.02.2021

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = form_name
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc = 0.

    CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
      EXPORTING
        i_language             = sy-langu
      IMPORTING
        e_devtype              = gv_devtype
      EXCEPTIONS
        no_language            = 1
        language_not_installed = 2
        no_devtype_found       = 3
        system_error           = 4
        OTHERS                 = 5.

    gs_ssfcompop-tdprinter = gv_devtype.
*Suppress print dialog
    gs_control-no_dialog = 'X'.
    gs_control-getotf = 'X'.



    CALL FUNCTION fm_name "'/1BCDWB/SF00000060'
      EXPORTING
*       ARCHIVE_INDEX      =
*       ARCHIVE_INDEX_TAB  =
*       ARCHIVE_PARAMETERS =
        control_parameters = gs_control
*       MAIL_APPL_OBJ      =
*       MAIL_RECIPIENT     =
*       MAIL_SENDER        =
        output_options     = gs_ssfcompop
        user_settings      = 'X'
        gs_1005            = gs_1005
        wa_header          = wa_header
      IMPORTING
*       DOCUMENT_OUTPUT_INFO       =
        job_output_info    = gv_job_output
*       JOB_OUTPUT_OPTIONS =
      TABLES
        gt_po_details      = gt_po_details
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


*    CALL FUNCTION FM_NAME"'/1BCDWB/SF00000019'
*      EXPORTING
**       ARCHIVE_INDEX      =
**       ARCHIVE_INDEX_TAB  =
**       ARCHIVE_PARAMETERS =
*        control_parameters = gs_control
**       MAIL_APPL_OBJ      =
**       MAIL_RECIPIENT     =
**       MAIL_SENDER        =
*        output_options     = gs_ssfcompop
**       USER_SETTINGS      = 'X'
*        zlc_process        = gs_1005
*        ls_productname     = ls_productname
*        ls_quantity        = ls_quantity
*      IMPORTING
**       document_output_info =
*        job_output_info    = gv_job_output
**       JOB_OUTPUT_OPTIONS =
*      EXCEPTIONS
*        formatting_error   = 1
*        internal_error     = 2
*        send_error         = 3
*        user_canceled      = 4
*        OTHERS             = 5.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.

    CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
      EXPORTING
        i_otf                    = gv_job_output-otfdata
      EXCEPTIONS
        convert_otf_to_pdf_error = 1
        cntl_error               = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form END_SHP_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM end_shp_doc .
  DATA: gt_po_details    TYPE ztt_bank_letter,
        gs_po_details    TYPE zstr_bank_letter,
        ls_seqno         TYPE znumber,
        lv_seqno         TYPE znumb,
        lv_year          TYPE gjahr,
        lv_name1         TYPE ad_name1,
        lv_city2         TYPE ad_city2,
        lv_street        TYPE ad_street,
        lv_post          TYPE ad_pstcd1,
        lv_address       TYPE char100,
        lv_sea_freight   TYPE p LENGTH 6 DECIMALS 2,
        lv_air_freight   TYPE p LENGTH 6 DECIMALS 2,
        lv_truck_freight TYPE p LENGTH 6 DECIMALS 2.


  CLEAR: gs_ssfcompop, gs_control, gv_devtype, gv_job_output, form_name, fm_name.

  SELECT SINGLE * FROM znumber INTO ls_seqno WHERE ztcode = sy-tcode.
  IF sy-subrc = 0.
    lv_seqno = ls_seqno-znum + 1.
  ELSE.
    lv_seqno = '1'.

    ls_seqno-ztcode = 'ZLC_SHIP'.

*    ls_seqno-znum = LV_SEQNO.
    ls_seqno-zyear = sy-datum+0(4).

  ENDIF.
  lv_year = sy-datum+0(4).
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_seqno
    IMPORTING
      output = lv_seqno.

  ls_seqno-znum = lv_seqno.

  MODIFY znumber FROM ls_seqno.

  SELECT a~ebeln, a~ebelp, a~txz01, a~matnr, a~menge, a~meins, a~netwr, a~packno, b~maktx
      FROM ekpo AS a LEFT OUTER JOIN makt AS b ON a~matnr = b~matnr
      INTO TABLE @DATA(gt_po)
      WHERE a~ebeln = @gs_1005-ebeln.
*    AND b~spras = @sy-langu.

  IF gt_po IS NOT INITIAL.

    SELECT a~packno, a~sub_packno, a~extrow, b~packno AS item, b~extrow AS line, b~menge, b~meins, b~ktext1, b~netwr
      FROM esll AS a INNER JOIN esll AS b ON a~sub_packno = b~packno
      INTO TABLE @DATA(lt_ser)
      FOR ALL ENTRIES IN @gt_po WHERE a~packno = @gt_po-packno.

    SELECT ebeln, bukrs, bsart, knumv, lifnr, zterm, inco1, inco2 FROM ekko INTO TABLE @DATA(gt_ekko) FOR ALL ENTRIES IN @gt_po
                                                                                        WHERE ebeln = @gt_po-ebeln.
    SELECT knumv, kposn, kbetr,kschl
  FROM prcd_elements INTO TABLE @DATA(lt_price) FOR ALL ENTRIES IN @gt_ekko WHERE knumv = @gt_ekko-knumv.

*BOC by Arka on 03.07.2020
    SELECT SINGLE adrnr FROM t001 INTO @DATA(lv_adrnr) WHERE bukrs = @gs_1005-bukrs.

    SELECT SINGLE name1 street city2 post_code1 FROM adrc INTO ( lv_name1,lv_street,lv_city2,lv_post ) WHERE addrnumber = lv_adrnr.

    CONCATENATE lv_street ',' lv_city2 '-' lv_post INTO lv_address SEPARATED BY space.
*EOC by Arka on 03.07.2020

    IF lt_ser IS INITIAL.
      LOOP AT gt_po INTO DATA(gs_po).
*BOC by Arka on 04.02.2020 to add the freight charges
        READ TABLE gt_ekko INTO DATA(gs_ekko) WITH KEY ebeln = gs_po-ebeln.
        READ TABLE lt_price INTO DATA(ls_price) WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRF'.
        IF sy-subrc = 0.
          lv_sea_freight = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRA'.
        IF sy-subrc = 0.
          lv_air_freight = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRT'.
        IF sy-subrc = 0.
          lv_truck_freight = ls_price-kbetr.
        ENDIF.
*EOC by Arka on 04.02.2020 to add the freight charges
        gs_po_details-matnr = gs_po-matnr.
        IF gs_po-maktx IS INITIAL.
          gs_po_details-maktx = gs_po-txz01.
        ELSE.
          gs_po_details-maktx = gs_po-maktx.
        ENDIF.

        gs_po_details-meins = gs_po-meins.
        gs_po_details-menge = gs_po-menge.
        gs_po_details-value = gs_po-netwr + lv_sea_freight +  lv_air_freight +  lv_truck_freight. "inserted by Arka on 04.02.2020
        APPEND gs_po_details TO gt_po_details.
        CLEAR gs_po_details.
      ENDLOOP.
    ELSE.
      LOOP AT lt_ser INTO DATA(ls_ser).
        gs_po_details-maktx = ls_ser-ktext1.
        gs_po_details-menge = ls_ser-menge.
        gs_po_details-meins = ls_ser-meins.
        gs_po_details-value = ls_ser-netwr + lv_sea_freight +  lv_air_freight +  lv_truck_freight. "inserted by Arka on 04.02.2020
*        lv_dmbtr = lv_dmbtr + gs_po-netwr.
        APPEND gs_po_details TO gt_po_details.
        CLEAR gs_po_details.
      ENDLOOP.
    ENDIF.

  ENDIF.

  form_name = 'ZLC_BANK_ENDORSE_ORIGINAL'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = form_name
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc EQ 0.

    CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
      EXPORTING
        i_language             = sy-langu
*       I_APPLICATION          = 'SAPDEFAULT'
      IMPORTING
        e_devtype              = gv_devtype
      EXCEPTIONS
        no_language            = 1
        language_not_installed = 2
        no_devtype_found       = 3
        system_error           = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    gs_ssfcompop-tdprinter = gv_devtype.
*Suppress print dialog
    gs_control-no_dialog = 'X'.
    gs_control-getotf = 'X'.

    CALL FUNCTION fm_name "'/1BCDWB/SF00000062'
      EXPORTING
        control_parameters = gs_control
        output_options     = gs_ssfcompop
*       USER_SETTINGS      = 'X'
        gs_details         = gs_1005
        lv_seqno           = lv_seqno
        lv_year            = lv_year
        lv_name1           = lv_name1
        lv_address         = lv_address
      IMPORTING
*       DOCUMENT_OUTPUT_INFO       =
        job_output_info    = gv_job_output
*       JOB_OUTPUT_OPTIONS =
      TABLES
        gt_po_details      = gt_po_details
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
      EXPORTING
        i_otf                    = gv_job_output-otfdata
      EXCEPTIONS
        convert_otf_to_pdf_error = 1
        cntl_error               = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ENDIF.










ENDFORM.
*&---------------------------------------------------------------------*
*& Form END_COPY_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM end_copy_doc .

  DATA: gt_po_details    TYPE ztt_bank_letter,
        gs_po_details    TYPE zstr_bank_letter,
        ls_seqno         TYPE znumber,
        lv_seqno         TYPE znumb,
        lv_year          TYPE gjahr,
        lv_name1         TYPE ad_name1,
        lv_city2         TYPE ad_city2,
        lv_street        TYPE ad_street,
        lv_post          TYPE ad_pstcd1,
        lv_address       TYPE char100,
        lv_sea_freight   TYPE p LENGTH 6 DECIMALS 2,
        lv_air_freight   TYPE p LENGTH 6 DECIMALS 2,
        lv_truck_freight TYPE p LENGTH 6 DECIMALS 2.

  CLEAR: gs_ssfcompop, gs_control, gv_devtype, gv_job_output, form_name, fm_name.

  SELECT SINGLE * FROM znumber INTO ls_seqno WHERE ztcode = sy-tcode.
  IF sy-subrc = 0.
    lv_seqno = ls_seqno-znum + 1.
  ELSE.
    lv_seqno = '1'.

    ls_seqno-ztcode = 'ZLC_COPY'.

*    ls_seqno-znum = LV_SEQNO.
    ls_seqno-zyear = sy-datum+0(4).

  ENDIF.
  lv_year = sy-datum+0(4).
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_seqno
    IMPORTING
      output = lv_seqno.

  ls_seqno-znum = lv_seqno.

  MODIFY znumber FROM ls_seqno.

  SELECT a~ebeln, a~ebelp, a~txz01, a~matnr, a~menge, a~meins, a~netwr, a~packno, b~maktx
      FROM ekpo AS a LEFT OUTER JOIN makt AS b ON a~matnr = b~matnr
      INTO TABLE @DATA(gt_po)
      WHERE a~ebeln = @gs_1005-ebeln.
*    AND b~spras = @sy-langu.

  IF gt_po IS NOT INITIAL.

    SELECT a~packno, a~sub_packno, a~extrow, b~packno AS item, b~extrow AS line, b~menge, b~meins, b~ktext1, b~netwr
      FROM esll AS a INNER JOIN esll AS b ON a~sub_packno = b~packno
      INTO TABLE @DATA(lt_ser)
      FOR ALL ENTRIES IN @gt_po WHERE a~packno = @gt_po-packno.

    SELECT ebeln, bukrs, bsart, knumv, lifnr, zterm, inco1, inco2 FROM ekko INTO TABLE @DATA(gt_ekko) FOR ALL ENTRIES IN @gt_po
                                                                                          WHERE ebeln = @gt_po-ebeln.
    SELECT knumv, kposn, kbetr,kschl
  FROM prcd_elements INTO TABLE @DATA(lt_price) FOR ALL ENTRIES IN @gt_ekko WHERE knumv = @gt_ekko-knumv.

*BOC by Arka on 03.07.2020
    SELECT SINGLE adrnr FROM t001 INTO @DATA(lv_adrnr) WHERE bukrs = @gs_1005-bukrs.

    SELECT SINGLE name1 street city2 post_code1 FROM adrc INTO (lv_name1,lv_street,lv_city2,lv_post) WHERE addrnumber = lv_adrnr.
    CONCATENATE lv_street ',' lv_city2 '-' lv_post INTO lv_address SEPARATED BY space.
*EOC by Arka on 03.07.2020

    IF lt_ser IS INITIAL.
      LOOP AT gt_po INTO DATA(gs_po).
*BOC by Arka on 04.02.2020 to add the freight charges
        READ TABLE gt_ekko INTO DATA(gs_ekko) WITH KEY ebeln = gs_po-ebeln.
        READ TABLE lt_price INTO DATA(ls_price) WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRF'.
        IF sy-subrc = 0.
          lv_sea_freight = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRA'.
        IF sy-subrc = 0.
          lv_air_freight = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = gs_ekko-knumv kposn = gs_po-ebelp kschl = 'ZFRT'.
        IF sy-subrc = 0.
          lv_truck_freight = ls_price-kbetr.
        ENDIF.
*EOC by Arka on 04.02.2020 to add the freight charges
        gs_po_details-matnr = gs_po-matnr.
        IF gs_po-maktx IS INITIAL.
          gs_po_details-maktx = gs_po-txz01.
        ELSE.
          gs_po_details-maktx = gs_po-maktx.
        ENDIF.

        gs_po_details-meins = gs_po-meins.
        gs_po_details-menge = gs_po-menge.
        gs_po_details-value = gs_po-netwr + lv_sea_freight +  lv_air_freight +  lv_truck_freight. "inserted by Arka on 04.02.2020
        APPEND gs_po_details TO gt_po_details.
        CLEAR gs_po_details.
      ENDLOOP.
    ELSE.
      LOOP AT lt_ser INTO DATA(ls_ser).
        gs_po_details-maktx = ls_ser-ktext1.
        gs_po_details-menge = ls_ser-menge.
        gs_po_details-meins = ls_ser-meins.
        gs_po_details-value = ls_ser-netwr + lv_sea_freight +  lv_air_freight +  lv_truck_freight. "inserted by Arka on 04.02.2020
*        lv_dmbtr = lv_dmbtr + gs_po-netwr.
        APPEND gs_po_details TO gt_po_details.
        CLEAR gs_po_details.
      ENDLOOP.
    ENDIF.

  ENDIF.

  form_name = 'ZLC_BANK_ENDORSE_COPY'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = form_name
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc EQ 0.

    CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
      EXPORTING
        i_language             = sy-langu
*       I_APPLICATION          = 'SAPDEFAULT'
      IMPORTING
        e_devtype              = gv_devtype
      EXCEPTIONS
        no_language            = 1
        language_not_installed = 2
        no_devtype_found       = 3
        system_error           = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    gs_ssfcompop-tdprinter = gv_devtype.
*Suppress print dialog
    gs_control-no_dialog = 'X'.
    gs_control-getotf = 'X'.

    CALL FUNCTION fm_name "'/1BCDWB/SF00000055'
      EXPORTING
*       ARCHIVE_INDEX      =
*       ARCHIVE_INDEX_TAB  =
*       ARCHIVE_PARAMETERS =
        control_parameters = gs_control
*       MAIL_APPL_OBJ      =
*       MAIL_RECIPIENT     =
*       MAIL_SENDER        =
        output_options     = gs_ssfcompop
*       USER_SETTINGS      = 'X'
        gs_details         = gs_1005
        lv_seqno           = lv_seqno
        lv_year            = lv_year
        lv_name1           = lv_name1
        lv_address         = lv_address
      IMPORTING
*       DOCUMENT_OUTPUT_INFO       =
        job_output_info    = gv_job_output
*       JOB_OUTPUT_OPTIONS =
      TABLES
        gt_po_details      = gt_po_details
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
      EXPORTING
        i_otf                    = gv_job_output-otfdata
      EXCEPTIONS
        convert_otf_to_pdf_error = 1
        cntl_error               = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form TRACK_REP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM track_rep .
  EXPORT pi_low TO MEMORY ID 'list'.
  EXPORT pi_high TO MEMORY ID 'list1'.
  EXPORT date_low TO MEMORY ID 'list2'.
  EXPORT date_high TO MEMORY ID 'list3'.
  EXPORT lc_low TO MEMORY ID 'list4'.
  EXPORT lc_high TO MEMORY ID 'list5'.

  CALL TRANSACTION 'ZLC_TRACK_REP'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLOSE_LC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM close_lc .
  EXPORT lc_low TO MEMORY ID 'list1'.
  EXPORT lc_high TO MEMORY ID 'list2'.

  CALL TRANSACTION 'ZLC_CLOSE'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_retirement
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_retirement .
  REFRESH: gt_dom_val1, gt_dom_val2, gt_lcretire.

  CLEAR: gs_lcretire,gv_id.

  IF sy-ucomm = 'BANK_CRT' OR sy-ucomm = 'BANK_MOD' OR sy-tcode = 'ZBANK'.
    gv_id = 'GS_1002-LC_RETIRE'.
  ELSEIF sy-ucomm = 'LC_CRT' OR sy-ucomm = 'LC_MOD' OR sy-tcode = 'ZLC_PROC'.
    gv_id = 'GS_1004-LC_RETIRE'.
  ENDIF.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZLC_RETIRE'
*     TEXT            = 'X'
*     FILL_DD07L_TAB  = ' '
    TABLES
      values_tab      = gt_dom_val1
      values_dd07l    = gt_dom_val2
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT gt_dom_val1 INTO DATA(gs_doma_val).

    gs_lcretire-key = gs_doma_val-domvalue_l.
    gs_lcretire-text = gs_doma_val-ddtext.

    APPEND gs_lcretire TO gt_lcretire.
    CLEAR gs_lcretire.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gv_id
      values          = gt_lcretire
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC_1004                                          *
*&---------------------------------------------------------------------*
FORM user_ok_tc_1004 USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok     TYPE sy-ucomm,
        l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row_1004 USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row_1004 USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc_1004 USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines_1004 USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines_1004 USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC_1004

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW_1004                                    *
*&---------------------------------------------------------------------*
FORM fcode_insert_row_1004
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW_1004

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW_1004                                    *
*&---------------------------------------------------------------------*
FORM fcode_delete_row_1004
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW_1004

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC_1004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc_1004 USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
      EXPORTING
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      IMPORTING
        entry_new      = l_tc_new_top_line
      EXCEPTIONS
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        OTHERS         = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC_1004

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES_1004
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines_1004 USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines_1004

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES_1004
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines_1004 USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines_1004
*&---------------------------------------------------------------------*
*& Form validate_pi
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_pi_1004.
**This is to validate the all PI's entered belong to the Same co.code
  IF s_pi[] IS NOT INITIAL.
    SELECT pi_no,bukrs,lc_no
     FROM zlc_process
      INTO TABLE @DATA(lt_pi)
     WHERE pi_no IN @s_pi.
    IF sy-subrc = 0 .
      SORT lt_pi BY pi_no bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_pi COMPARING pi_no bukrs.
      DESCRIBE TABLE lt_pi LINES DATA(lv_line_pi).
      DESCRIBE TABLE s_pi LINES DATA(lv_pi).
*      IF lv_line_pi > 1 .
*        REFRESH:gt_1004,s_pi[].
*        CLEAR :gs_1004,gw_1004, s_pi-low,inp_lc.
*        SET CURSOR FIELD 'S_PI-LOW'.
*        MESSAGE i019(zabap) DISPLAY LIKE 'E'.
*        LEAVE TO SCREEN 1004.
      IF lv_line_pi NE lv_pi.
        REFRESH:gt_1004.
        CLEAR :gs_1004,gw_1004, s_pi-low,inp_lc.
        SET CURSOR FIELD 'S_PI'.
        MESSAGE i023(zabap) DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 1004.
      ENDIF.
    ELSE.
      PERFORM clear_1004.
      MESSAGE i022(zabap) DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.

FORM validate_lc_1004.
**This is to validate the all PI's entered belong to the Same co.code and have same LC
  IF inp_lc IS NOT INITIAL.
    SELECT pi_no,bukrs,lc_no
     FROM zlc_process
      INTO TABLE @DATA(lt_pi)
     WHERE lc_no EQ @inp_lc.
    IF sy-subrc = 0 .
      SORT lt_pi BY bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_pi COMPARING bukrs.
      DESCRIBE TABLE lt_pi LINES DATA(lv_line_pi).
      IF lv_line_pi > 1.
        REFRESH:gt_1004,s_pi[].
        CLEAR :gs_1004,gw_1004, s_pi-low,inp_lc.
        SET CURSOR FIELD 'S_PI-LOW'.
        MESSAGE i019(zabap) DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 1004.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fill_po_1004
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_po_1004 .
  SELECT ebeln, bukrs, bsart, knumv,waers, lifnr, zterm, inco1, inco2
  FROM ekko INTO TABLE @DATA(lt_ekko)
  WHERE ebeln IN @s_ebeln.
  IF lt_ekko IS NOT INITIAL.
    SELECT ebeln, ebelp, matnr, werks, txz01, menge, meins, packno, netpr
      FROM ekpo INTO TABLE @DATA(lt_ekpo)
      FOR ALL ENTRIES IN @lt_ekko
      WHERE ebeln = @lt_ekko-ebeln.
    IF lt_ekpo IS NOT INITIAL.
      SELECT a~packno, a~sub_packno, a~extrow, b~packno AS item, b~extrow AS line, b~menge, b~meins, b~ktext1, b~netwr
       FROM esll AS a INNER JOIN esll AS b ON a~sub_packno = b~packno
       INTO TABLE @DATA(lt_ser)
       FOR ALL ENTRIES IN @lt_ekpo WHERE a~packno = @lt_ekpo-packno.
    ENDIF.

    SELECT knumv, kposn, kbetr,kschl
    FROM prcd_elements INTO TABLE @DATA(lt_price)
    FOR ALL ENTRIES IN @lt_ekko
    WHERE knumv = @lt_ekko-knumv.
  ENDIF.

  SELECT name1 FROM lfa1 INTO TABLE @DATA(lt_lfa1) FOR ALL ENTRIES IN @lt_ekko WHERE lifnr = @lt_ekko-lifnr.
  READ TABLE lt_lfa1 INTO DATA(ls_lfa1) INDEX 1.
  IF sy-subrc = 0.
    gs_1001-name1 = ls_lfa1-name1.
  ENDIF.

  gs_1001-pi_no = inp_pi.

  READ TABLE lt_ekko INTO DATA(ls_ekko) INDEX 1.
  IF sy-subrc = 0.
    gs_1001-bukrs = ls_ekko-bukrs.
*  gs_1001-pi_pymt = ls_ekko-zterm.
    gs_1001-lifnr = ls_ekko-lifnr.

    SELECT SINGLE bukrs, adrnr FROM t001 INTO @DATA(ls_t001) WHERE bukrs = @ls_ekko-bukrs.
    IF sy-subrc = 0.
      SELECT SINGLE addrnumber, name1, city1, post_code1, street
      FROM adrc INTO @DATA(ls_adrc)
      WHERE addrnumber = @ls_t001-adrnr.
      IF sy-subrc = 0.
        CONCATENATE ls_adrc-name1 ls_adrc-street ls_adrc-city1 ls_adrc-post_code1
        INTO gs_1001-offc_add SEPARATED BY ','.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT gt_1004 INTO DATA(ls_1004).
    READ TABLE it_podet1 INTO wa_podet1 WITH KEY pi = ls_1004-pi_no
                                                   ebeln = ls_1004-ebeln
                                                   ebelp = ls_1004-ebelp.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_podet1 TO ls_1004.
      MODIFY gt_1004 FROM ls_1004.
    ENDIF.
  ENDLOOP.

*  IF lt_ser IS INITIAL.
  LOOP AT gt_1004 INTO ls_1004.
**        READ TABLE gt_1001 INTO DATA(ls_1004) WITH KEY ebeln = ls_ekko-ebeln.
**        IF sy-subrc = 0 AND gv_screenmode <> 'PI_MOD'.
**          wa_podet-lstel = ls_1004-lstel.
**          wa_podet-vstel = ls_1004-vstel.
**          wa_podet-inco = ls_1004-inco.
**          wa_podet-lddat = ls_1004-lddat.
**          wa_podet-ship_mode = ls_1004-ship_mode.
**          wa_podet-hs_code = ls_1004-hs_code.
**        ELSE.
**          READ TABLE it_podet1 INTO wa_podet1 WITH KEY ebeln = ls_ekko-ebeln.
**          IF sy-subrc = 0.
**            wa_podet-lstel = wa_podet1-lstel.
**            wa_podet-vstel = wa_podet1-vstel.
**            wa_podet-inco = wa_podet1-inco.
**            wa_podet-lddat = wa_podet1-lddat.
**            wa_podet-ship_mode = wa_podet1-ship_mode.
**            wa_podet-hs_code = wa_podet1-hs_code.
**          ENDIF.
**        ENDIF.

*    IF gv_screenmode = 'LC_CRT'.
*      READ TABLE it_podet1 INTO wa_podet1 WITH KEY ebeln = ls_1004-ebeln
*                                                    ebelp = ls_1004-ebelp.
*      IF sy-subrc = 0.
*        wa_podet-land1 = wa_podet1-land1.
*        wa_podet-lstel = wa_podet1-lstel.
*        wa_podet-vstel = wa_podet1-vstel.
*        wa_podet-inco = wa_podet1-inco.
*        wa_podet-lddat = wa_podet1-lddat.
*        wa_podet-ship_mode = wa_podet1-ship_mode.
*        wa_podet-hs_code = wa_podet1-hs_code.
*      ENDIF.
*    ENDIF.

    IF gv_screenmode = 'LC_DISP' OR gv_screenmode = 'LC_MOD'.
      wa_podet-land1 = ls_1004-land1.
      wa_podet-lstel = ls_1004-lstel.
      wa_podet-vstel = ls_1004-vstel.
      wa_podet-inco = ls_1004-inco.
      wa_podet-lddat = ls_1004-lddat.
      wa_podet-ship_mode = ls_1004-ship_mode.
      wa_podet-hs_code = ls_1004-hs_code.
    ENDIF.

*    IF gv_screenmode = 'LC_MOD'.
*      IF it_podet1 IS INITIAL.
*          wa_podet-land1 = ls_1004-land1.
*          wa_podet-lstel = ls_1004-lstel.
*          wa_podet-vstel = ls_1004-vstel.
*          wa_podet-inco = ls_1004-inco.
*          wa_podet-lddat = ls_1004-lddat.
*          wa_podet-ship_mode = ls_1004-ship_mode.
*          wa_podet-hs_code = ls_1004-hs_code.
*      ELSE.
*        READ TABLE it_podet1 INTO wa_podet1 WITH KEY pi    = ls_1004-pi_no
*                                                     ebeln = ls_1004-ebeln
*                                                     ebelp = ls_1004-ebelp.
*        IF sy-subrc = 0.
*          wa_podet-land1 = wa_podet1-land1.
*          wa_podet-lstel = wa_podet1-lstel.
*          wa_podet-vstel = wa_podet1-vstel.
*          wa_podet-inco = wa_podet1-inco.
*          wa_podet-lddat = wa_podet1-lddat.
*          wa_podet-ship_mode = wa_podet1-ship_mode.
*          wa_podet-hs_code = wa_podet1-hs_code.
*        ENDIF.
*      ENDIF.
*    ENDIF.
    READ TABLE lt_ekpo INTO DATA(ls_ekpo) WITH KEY ebeln = ls_1004-ebeln
                                                  ebelp = ls_1004-ebelp.
    IF sy-subrc = 0.

      wa_podet-pi = ls_1004-pi_no.
      wa_podet-ebeln = ls_ekpo-ebeln.
      wa_podet-ebelp = ls_ekpo-ebelp.
      wa_podet-matnr = ls_ekpo-matnr.
      wa_podet-txz01 = ls_ekpo-txz01.
      wa_podet-menge = ls_ekpo-menge.
      wa_podet-meins = ls_ekpo-meins.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_podet-matnr
        IMPORTING
          output = wa_podet-matnr.

      IF gv_screenmode = 'LC_CRT'.
        SELECT SINGLE blk_no FROM zmm_blocklist_po INTO @DATA(lv_blk) WHERE matnr = @wa_podet-matnr
                                                                      AND ebeln = @wa_podet-ebeln.
        IF lv_blk IS INITIAL.
          MESSAGE 'No Blocklist is assigned' TYPE 'W' DISPLAY LIKE 'S'.
        ENDIF.
      ENDIF.

      READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = ls_ekpo-ebeln.
      IF sy-subrc = 0.

        IF lt_ser IS NOT INITIAL.

        ENDIF.

        SELECT SINGLE werks,name1 FROM t001w INTO @DATA(ls_t001w1) WHERE werks = @ls_ekpo-werks.
        IF sy-subrc = 0.
          wa_podet-plant_desc = ls_t001w1-name1.
        ENDIF.

        IF gs_1004-plant_add IS INITIAL.
          SELECT SINGLE werks, name1, stras, pstlz, ort01 FROM t001w INTO @DATA(ls_t001w) WHERE werks = @ls_ekpo-werks.
          IF sy-subrc = 0.
            gs_1004-werks = ls_ekpo-werks.
            gs_1004-waers = ls_ekko-waers.
            gs_1004-plant_desc = ls_t001w-name1.
            CONCATENATE ls_t001w-name1 ls_t001w-stras ls_t001w-ort01 ls_t001w-pstlz INTO gs_1001-plant_add SEPARATED BY ','.
          ENDIF.
        ENDIF.

*        READ TABLE lt_price INTO DATA(ls_price) WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'PB00'.
*        IF sy-subrc = 0.
          wa_podet-kbetr = ls_ekpo-netpr.
*        ENDIF.

*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZLOD'.
*        IF sy-subrc = 0.
*          wa_podet-loading_cost = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZFIR'.
*        IF sy-subrc = 0.
*          wa_podet-trans_cost = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZFRF'.
*        IF sy-subrc = 0.
*          wa_podet-sea_freight = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZFRA'.
*        IF sy-subrc = 0.
*          wa_podet-air_freight = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZFRT'.
*        IF sy-subrc = 0.
*          wa_podet-truck_freight = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZSEA'.
*        IF sy-subrc = 0.
*          wa_podet-calc_sea = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZAI1'.
*        IF sy-subrc = 0.
*          wa_podet-calc_air = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZAI2'.
*        IF sy-subrc = 0.
*          wa_podet-calc_air2 = ls_price-kbetr.
*        ENDIF.

        READ TABLE lt_price INTO DATA(ls_price) WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZCDU'.
        IF sy-subrc = 0.
          wa_podet-custom_duty = ls_price-kbetr.
        ENDIF.

*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZRDU'.
*        IF sy-subrc = 0.
*          wa_podet-regl_duty = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZSDU'.
*        IF sy-subrc = 0.
*          wa_podet-suppl_duty = ls_price-kbetr.
*        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZAIT'.
        IF sy-subrc = 0.
          wa_podet-ait = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZATX'.
        IF sy-subrc = 0.
          wa_podet-atx = ls_price-kbetr.
        ENDIF.

*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZDCF'.
*        IF sy-subrc = 0.
*          wa_podet-dcf = ls_price-kbetr.
*        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZBNK'.
        IF sy-subrc = 0.
          wa_podet-bnk = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZBNC'.
        IF sy-subrc = 0.
          wa_podet-bnc = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZVLC'.
        IF sy-subrc = 0.
          wa_podet-vlc = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZVI1'.
        IF sy-subrc = 0.
          wa_podet-vi1 = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZTIC'.
        IF sy-subrc = 0.
          wa_podet-tic = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZIPM'.
        IF sy-subrc = 0.
          wa_podet-ipm = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZVCF'.
        IF sy-subrc = 0.
          wa_podet-vcf = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZCFC'.
        IF sy-subrc = 0.
          wa_podet-cfc = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZPWC'.
        IF sy-subrc = 0.
          wa_podet-pwc = ls_price-kbetr.
        ENDIF.

*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZCRC'.
*        IF sy-subrc = 0.
*          wa_podet-crc = ls_price-kbetr.
*        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZOCC'.
        IF sy-subrc = 0.
          wa_podet-occ = ls_price-kbetr.
        ENDIF.
*
        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZVTI'.
        IF sy-subrc = 0.
          wa_podet-vti = ls_price-kbetr.
        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZFFC'.
        IF sy-subrc = 0.
          wa_podet-ffc = ls_price-kbetr.
        ENDIF.

*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZITC'.
*        IF sy-subrc = 0.
*          wa_podet-itc = ls_price-kbetr.
*        ENDIF.
*
*        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZLOD'.
*        IF sy-subrc = 0.
*          wa_podet-lod = ls_price-kbetr.
*        ENDIF.

        READ TABLE lt_price INTO ls_price WITH KEY knumv = ls_ekko-knumv kposn = ls_ekpo-ebelp kschl = 'ZOTS'.
        IF sy-subrc = 0.
          wa_podet-ots = ls_price-kbetr.
        ENDIF.

        wa_podet-total_duty =
        wa_podet-custom_duty +
*         wa_podet-regl_duty +
*         wa_podet-suppl_duty +
           wa_podet-ait +
           wa_podet-atx +
           wa_podet-vi1 +
           wa_podet-bnk +
           wa_podet-bnc +
           wa_podet-vlc +
           wa_podet-vti +
           wa_podet-tic +
           wa_podet-ipm +
           wa_podet-vcf +
           wa_podet-cfc +
           wa_podet-pwc +
*           wa_podet-crc +
           wa_podet-occ +
*           wa_podet-srf +
           wa_podet-ffc +
*           wa_podet-itc +
*           wa_podet-lod +
           wa_podet-ots.
        gs_1001-total_duty = wa_podet-total_duty + gs_1001-total_duty.
        APPEND wa_podet TO it_podet.
        CLEAR wa_podet.
      ENDIF.
    ENDIF.
  ENDLOOP.
*  ELSE.
*    LOOP AT lt_ser INTO DATA(ls_ser).
*      wa_podet-ebeln = ls_ser-item.
*      wa_podet-ebelp = ls_ser-line.
**      wa_podet-matnr = ls_ekpo-matnr.
*      wa_podet-txz01 = ls_ser-ktext1.
*      wa_podet-menge = ls_ser-menge.
*      wa_podet-meins = ls_ser-meins.
*      wa_podet-kbetr = ls_ser-netwr.
*      APPEND wa_podet TO it_podet.
*      CLEAR wa_podet.
*    ENDLOOP.
*  ENDIF.

  SORT it_podet BY pi ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM it_podet COMPARING pi ebeln ebelp.
*  CALL SCREEN 1001.

  CLEAR: it_podet1, wa_podet1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_pi_1002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_pi_1002 .
**This is to validate the all PI's entered belong to the Same co.code
  IF s_pi[] IS NOT INITIAL.
    SELECT pi_no,ebeln,bukrs,lc_no
     FROM zlc_process
      INTO TABLE @DATA(lt_pi)
     WHERE pi_no IN @s_pi.
    IF sy-subrc = 0 .
      SORT lt_pi BY pi_no bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_pi COMPARING pi_no bukrs .
      DESCRIBE TABLE lt_pi LINES DATA(lv_line_pi).
      DESCRIBE TABLE s_pi LINES DATA(lv_pi).
*      IF lv_line_pi > 1.
*        PERFORM clear_1002.
*        SET CURSOR FIELD 'S_PI-LOW'.
*        MESSAGE i019(zabap) DISPLAY LIKE 'E'.
*        LEAVE TO SCREEN 1002.
*      ELSE
      IF lv_line_pi NE lv_pi.
        PERFORM clear_1002.
        SET CURSOR FIELD 'S_PI'.
        MESSAGE i023(zabap) DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 1002.
      ENDIF.
    ELSE.
      MESSAGE i022(zabap) DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1002
    WHERE pi_no = @s_pi-low.
    IF sy-subrc NE 0.
      MESSAGE i000(zelc) WITH s_pi-low.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_BANK_VEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_bank_ven INPUT.

  IF bank_ven IS NOT INITIAL.
    IF gv_disp_flag NE 'X'"non create
    AND gv_mod_flag NE 'X'.
      gs_1002-lc_bank_vendor = bank_ven.
    ELSE.                 "create
*      IF gs_1002-lc_bank_vendor IS NOT INITIAL
*     AND bank_ven NE gs_1002-lc_bank_vendor.
*        bank_ven = gs_1002-lc_bank_vendor.
*        MESSAGE e024(zabap) DISPLAY LIKE 'I'.
*        LEAVE TO SCREEN 1002.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALIDATE_LC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validate_lc INPUT.
**This is to validate the all PI's entered belong to the Same co.code and have same LC
  IF inp_lc IS NOT INITIAL.
    SELECT pi_no,bukrs,lc_no
     FROM zlc_process
      INTO TABLE @DATA(lt_pi)
     WHERE lc_no EQ @inp_lc.
    IF sy-subrc = 0 .
      SORT lt_pi BY bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_pi COMPARING bukrs.
      DESCRIBE TABLE lt_pi LINES DATA(lv_line_pi).
      IF lv_line_pi > 1.
        REFRESH:gt_1004,s_pi[].
        CLEAR :gs_1004,gw_1004, s_pi-low,inp_lc.
        SET CURSOR FIELD 'S_PI-LOW'.
        MESSAGE i019(zabap) DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 1004.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form clear_1004
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear_1004 .
  CLEAR: gs_1004,gw_1004,inp_lc.
  CLEAR s_pi-low.
  REFRESH s_pi[].
  REFRESH: it_podet,gt_1004.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form clear_1002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear_1002 .
  CLEAR:  gs_1002,inp_pi, bank_ven, bank_name, bank_add, s_pi-low,
            lc_acc_no,
            lc_type,
            lc_margin,
            lc_retire,
            bank_remarks,
            tol_lmt.
  REFRESH: it_podet,gt_1002, s_pi[].
  CLEAR: gv_margin_1002.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_LCC_ACC_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_lcc_acc_1002 INPUT.
  IF gs_1002-lc_acc_no IS NOT INITIAL.
    gv_lcacc_1002 = gs_1002-lc_acc_no.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_LCC_TYPE_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_lcc_type_1002 INPUT.
  IF gs_1002-lc_type IS NOT INITIAL.
    gv_type_1002 = gs_1002-lc_type.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_LCC_TYPE_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_lcc_margin_1002 INPUT.
  IF gs_1002-lc_margin IS NOT INITIAL.
    gv_margin_1002 = gs_1002-lc_margin.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_LCC_TYPE_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_lcc_retire_1002 INPUT.
  IF gs_1002-lc_retire IS NOT INITIAL.
    gv_retire_1002 = gs_1002-lc_retire.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_LCC_TYPE_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_lcc_rem_1002 INPUT.
  IF gs_1002-bank_remarks IS NOT INITIAL.
    gv_rem_1002 = gs_1002-bank_remarks.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FETCH_PI_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fetch_pi_1002 INPUT.
  IF s_pi[] IS NOT INITIAL.
    SELECT SINGLE * FROM zlc_process
    INTO CORRESPONDING FIELDS OF gs_1002
    WHERE pi_no IN s_pi.
  ENDIF.
ENDMODULE.
MODULE tol_lmt_1004.
  IF tol_lmt IS NOT INITIAL AND
     tol_lmt NE gs_1004-tol_lmt.
    gs_1004-tol_lmt = tol_lmt.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LC_DATE_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_date_1004 INPUT.
  IF gs_1004-lc_date IS NOT INITIAL.
    gv_date_1004 = gs_1004-lc_date.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LC_DATE_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_expdate_1004 INPUT.
  IF gs_1004-lc_exp_date IS NOT INITIAL.
    gv_expdate_1004 = gs_1004-lc_exp_date.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LC_DATE_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_confdate_1004 INPUT.
  IF gs_1004-lc_conf_date IS NOT INITIAL.
    gv_confdate_1004 = gs_1004-lc_conf_date.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LC_DATE_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_shipdate_1004 INPUT.
  IF gs_1004-ship_date IS NOT INITIAL.
    gv_shipdate_1004 = gs_1004-ship_date.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LC_DATE_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_term_1004 INPUT.
  IF gs_1004-lc_term IS NOT INITIAL.
    gv_term_1004 = gs_1004-lc_term.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LC_DATE_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_cbank_1004 INPUT.
  IF gs_1004-conf_bank IS NOT INITIAL.
    gv_cbank_1004 = gs_1004-conf_bank.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LC_DATE_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_abank_1004 INPUT.
  IF gs_1004-adv_bank IS NOT INITIAL.
    gv_abank_1004 = gs_1004-adv_bank.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LC_DATE_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_rem_1004 INPUT.
  IF gs_1004-lc_remarks IS NOT INITIAL.
    gv_rem_1004 = gs_1004-lc_remarks.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FETCH_PI_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fetch_pi_1004 INPUT.

  IF s_pi IS NOT INITIAL .
    SELECT SINGLE * FROM zlc_process
    INTO CORRESPONDING FIELDS OF gs_1004
    WHERE pi_no IN s_pi.
  ELSE.
    SELECT SINGLE * FROM zlc_process
   INTO CORRESPONDING FIELDS OF gs_1004
   WHERE lc_no =  inp_lc.
  ENDIF.
  gs_1004-lc_val = gs_1004-pi_amt.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SUB_DAYS_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE sub_days_1004 INPUT.
  IF gs_1004-doc_sub_days IS NOT INITIAL.
    gv_sdays_1004 = gs_1004-doc_sub_days.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SUB_DAYS_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lcaf_1004 INPUT.
  IF gs_1004-lcaf_no IS NOT INITIAL.
    gv_lcaf_1004 = gs_1004-lcaf_no.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form clear_1004_var
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear_1004_var .
  CLEAR:gv_rem_1004,
  gv_date_1004,gv_expdate_1004,
  gv_confdate_1004,gv_shipdate_1004,
  gv_term_1004,gv_sdays_1004,
  gv_lcaf_1004,gv_cbank_1004,
  gv_abank_1004.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_pibank_1002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_pibank_1002 .
**blank
  IF s_pi[] IS NOT INITIAL.
    SELECT COUNT(*)
    FROM zlc_process
     INTO @DATA(lv_bven)
    WHERE pi_no IN @s_pi
    AND lc_bank_vendor = ''.
    DESCRIBE TABLE s_pi LINES DATA(lv_spi).
    IF lv_spi > 1 AND lv_bven > 0.
      MESSAGE i027(zabap) DISPLAY LIKE 'E'.
      LEAVE TO SCREEN 1002.
    ENDIF.
  ENDIF.

**differenct bank vendors
  IF s_pi[] IS NOT INITIAL.
    SELECT pi_no,ebeln,bukrs,lc_no,lc_bank_vendor
     FROM zlc_process
      INTO TABLE @DATA(lt_bankven)
     WHERE pi_no IN @s_pi.
    IF sy-subrc = 0 .
      SORT s_pi[] BY low.
      DELETE ADJACENT DUPLICATES FROM s_pi[] COMPARING low.

      SORT lt_bankven BY pi_no lc_bank_vendor.
      DELETE ADJACENT DUPLICATES FROM lt_bankven COMPARING lc_bank_vendor.
      DELETE lt_bankven WHERE lc_bank_vendor IS INITIAL.

      DESCRIBE TABLE lt_bankven LINES DATA(lv_line_pi).
      DESCRIBE TABLE s_pi LINES DATA(lv_pi).
      IF lv_line_pi > 1.
        SET CURSOR FIELD 'S_PI'.
*          refresh s_pi[].
        MESSAGE i026(zabap) DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 1002.
      ENDIF.
    ELSE.
      MESSAGE i022(zabap) DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form clear_var_1002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear_var_1002 .
  CLEAR:gv_lcacc_1002,gv_margin_1002,gv_type_1002,
        gv_margin_1002,gv_retire_1002,gv_rem_1002.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form end_copy_doc_n
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM end_copy_doc_n USING lv_i TYPE i.

  DATA:
    gw_header   TYPE zendors_ltr_header,
    lt_proc     TYPE TABLE OF zlc_process,
    lt_proc1    TYPE TABLE OF zlc_process,
    lw_endi     TYPE zendrs_ltr,
    lv_frmname  TYPE tdsfname VALUE 'ZENDORS_LTR',
    lv_null     TYPE char1    VALUE IS INITIAL,
    lv_fnam     TYPE rs38l_fnam,
    lv_ind      TYPE syst_index,
    lv_vstel    TYPE char100,
    lv_adrnr    TYPE adrnr,
    lv_sort2    TYPE ad_sort2,
    lv_ctype(4) TYPE c,
    lv_badrc    TYPE adrnr,
    lv_fcltx    TYPE fcltx.

  gw_header-bukrs = gs_1005-bukrs.
  SELECT SINGLE adrnr
    FROM t001
    INTO lv_badrc
    WHERE bukrs = gw_header-bukrs.
  IF sy-subrc EQ 0.
    SELECT SINGLE name1
      INTO gw_header-cname
      FROM adrc
      WHERE addrnumber = lv_badrc.
    IF sy-subrc NE 0.
      CLEAR gw_header-cname.
    ENDIF.
  ENDIF.
  gw_header-lc_no = gs_1005-lc_no.
  gw_header-lcaf_no = gs_1005-lcaf_no.

  SELECT *
    FROM zlc_process
    INTO TABLE lt_proc
    WHERE lc_no = gs_1005-lc_no.
  IF sy-subrc EQ 0.
    CLEAR lv_ind.
    LOOP AT lt_proc INTO DATA(wa_proc) WHERE comm_inv_no IS NOT INITIAL.
      lv_ind = lv_ind + 1.
      IF lv_ind > 1.
        CONTINUE.
      ENDIF.
      gw_header-comm_inv_date = wa_proc-comm_inv_date.
      gw_header-comm_inv_no = wa_proc-comm_inv_no.
      gw_header-comm_inv_value = wa_proc-comm_inv_val.
    ENDLOOP.

    CLEAR: lt_proc1,
           wa_proc.
    lt_proc1 = lt_proc.
    DELETE lt_proc1 WHERE NOT lc_bank_vendor NE lv_null.
    READ TABLE lt_proc1 INTO wa_proc INDEX 1.
    IF sy-subrc EQ 0.
      gw_header-bank_name = wa_proc-lc_bank.
      gw_header-lc_date = wa_proc-lc_date.
      SELECT SINGLE adrnr
        FROM lfa1
        INTO @lv_adrnr
        WHERE lifnr = @wa_proc-lc_bank_vendor.
      IF sy-subrc EQ 0.
        SELECT SINGLE str_suppl3, sort2
          FROM adrc
          INTO (@gw_header-bank_add, @lv_sort2)
          WHERE addrnumber = @lv_adrnr.
        IF sy-subrc NE 0.
          CLEAR gw_header-bank_add.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  gw_header-bank_add = wa_proc-lc_bank_add.

  CLEAR: lt_proc1,
         wa_proc.
  lt_proc1 = lt_proc.
  DELETE lt_proc1 WHERE NOT bl_awb_no NE lv_null.
  READ TABLE lt_proc1 INTO wa_proc INDEX 1.
  IF sy-subrc EQ 0.
    gw_header-tr_no = wa_proc-bl_awb_no.
  ENDIF.

  CLEAR: lt_proc1,
         wa_proc.
  lt_proc1 = lt_proc.
  DELETE lt_proc1 WHERE NOT bl_awb_date NE lv_null.
  READ TABLE lt_proc1 INTO wa_proc INDEX 1.
  IF sy-subrc EQ 0.
    gw_header-trans_doc_date = wa_proc-bl_awb_date.
  ENDIF.

  CLEAR: lv_vstel,
         lv_ind.

  LOOP AT lt_proc INTO wa_proc.
    lv_ind = lv_ind + 1.
    IF lv_ind = 1.
      IF wa_proc-vstel IS NOT INITIAL.
        MOVE wa_proc-vstel TO lv_vstel.
        CONDENSE lv_vstel.
      ENDIF.
    ELSE.
      IF wa_proc-vstel IS NOT INITIAL.
        CONCATENATE lv_vstel wa_proc-vstel INTO lv_vstel SEPARATED BY ','.
        CONDENSE lv_vstel.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lv_vstel IS NOT INITIAL.
    CONDENSE lv_vstel.
    MOVE lv_vstel TO gw_header-port_dest.
  ENDIF.

  CLEAR: lt_proc1,
         wa_proc.
  lt_proc1 = lt_proc.
  DELETE lt_proc1 WHERE NOT crsp_type NE lv_null.
  READ TABLE lt_proc1 INTO wa_proc INDEX 1.
  IF sy-subrc EQ 0.
    IF lv_i = 1.
      CASE wa_proc-crsp_type.
        WHEN 'TT'.
          gw_header-subject = 'Endorsement of copy documents against FTT'.
          gw_header-line1 = 'We are pleased to inform you that the above mentioned consignment has already arrived at port of destination indicated above.'.
          gw_header-line2 = 'In this context, we would like to request you to kindly endorse the copy documents for customs clearing of goods of which payment has already been made.'.
          gw_header-name1 = 'Abu Shahriar Zahedee'.
          gw_header-desig1 = 'Executive Director'.
          CONDENSE: gw_header-line1,
                    gw_header-line2.
          CLEAR: gw_header-line3.
          CLEAR lv_ctype.
          lv_ctype = 'TTDM'.
        WHEN 'LC'  OR
             'CAD' OR
             'LCAF'.
          gw_header-subject = 'Endorsement of copy documents against LC/CAD (LCAF)'.
          gw_header-line1 = 'We are please to inform you that the above mentioned consignment has already arrived at Port of Destination.'.
*          CONCATENATE TEXT-t00 TEXT-t01 INTO gw_header-line2 SEPARATED BY space.
          CONCATENATE
          'In this context, we would like to request you to kindly endorse the copy documents for customs clearing of goods following the debit'
          'instruction. We hereby confirm you that we will accept the original shipping documents irrespective of discrepancy (if any) .'
          INTO gw_header-line2 SEPARATED BY space.
          CONDENSE gw_header-line2.
          gw_header-line3 = 'Please note that in case of payment against CAD (LCAF), it should be made only after submission of Bill of Entry at your bank counter.'.
          CONDENSE: gw_header-line1,
                    gw_header-line2,
                    gw_header-line3.
          gw_header-name1 = 'M A Latif Shahrear Zahedee'.
          gw_header-name2 = 'Masrur Ahmed'.
          gw_header-desig1 = 'Director, Supply Chain Management'.
          gw_header-desig2 = 'Managing Director'.
          CLEAR lv_ctype.
          lv_ctype = 'CEDM'.
        WHEN  'DS'.
        WHEN OTHERS.
      ENDCASE.
    ELSEIF lv_i = 2.
      CASE wa_proc-crsp_type.
        WHEN 'LC'  OR
             'TT'  OR
             'CAD' OR
             'LCAF' .
          gw_header-subject = 'Endorsement of original shipping documents against LC / CAD (LCAF) / FTT'.
          gw_header-line1 = 'We are please to inform you that the above mentioned consignment has already arrived at Port of Destination.'.
          gw_header-line2 = 'In this context, we would like to request you to kindly endorse the original shipping documents of the consignment for customs clearing of goods following the debit instruction.'.
          gw_header-line3 = 'Please note that in case of payment against CAD (LCAF), it should be made only after submission of Bill of Entry at your bank counter.'.
          CONDENSE: gw_header-line1,
                    gw_header-line2,
                    gw_header-line3.
          gw_header-name1 = 'M A Latif Shahrear Zahedee'.
          gw_header-name2 = 'Masrur Ahmed'.
          gw_header-desig1 = 'Director, Supply Chain Management'.
          gw_header-desig2 = 'Managing Director'.
          CLEAR lv_ctype.
          lv_ctype = 'OEDM'.
        WHEN 'DS'.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDIF.

  CLEAR: lt_proc1,
         wa_proc.
  lt_proc1 = lt_proc.
  DELETE lt_proc1 WHERE NOT name1 NE lv_null.
  READ TABLE lt_proc1 INTO wa_proc INDEX 1.
  IF sy-subrc EQ 0.
    gw_header-benif = wa_proc-name1.
  ENDIF.

  CLEAR: lt_proc1,
         wa_proc.
  lt_proc1 = lt_proc.
  DELETE lt_proc1 WHERE NOT lc_retire NE lv_null.
  READ TABLE lt_proc1 INTO wa_proc INDEX 1.
  IF sy-subrc EQ 0.
    gw_header-retire = wa_proc-lc_retire.
  ENDIF.

  SELECT SINGLE ltx
    INTO lv_fcltx
    FROM t247
    WHERE mnr = sy-datum+4(2) AND
          spras = sy-langu.
  IF sy-subrc EQ 0.
    CONCATENATE sy-datum+6(2) lv_fcltx sy-datum+0(4) INTO gw_header-print_date SEPARATED BY space.
  ENDIF.

  SELECT MAX( inc )
    FROM zendrs_ltr
    INTO @DATA(lv_incr)
    WHERE flag = @lv_i.
  IF sy-subrc <> 0.
    lv_incr = '0001'.
  ELSE.
    lv_incr = lv_incr + 1.
  ENDIF.

  CLEAR lw_endi.
  lw_endi-mandt = sy-mandt.
  lw_endi-flag = lv_i.
  lw_endi-inc = lv_incr.

  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      mode_rstable   = 'E'
      tabname        = 'ZENRS_LTR'
      _scope         = '2'
      _wait          = 'X'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc NE 0.
  ENDIF.

  MODIFY zendrs_ltr FROM lw_endi.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      mode_rstable = 'E'
      tabname      = 'ZENRS_LTR'
      _scope       = '3'.

  CASE gw_header-bukrs.
    WHEN '1000'.
      CONCATENATE 'RPL/CMR/' lv_sort2 '/' lv_ctype '/' sy-datum+0(4) '/' lv_incr INTO gw_header-ref.
      CONDENSE gw_header-ref.
      gw_header-name1 = 'M A Latif Shahrear Zahedee'.
      gw_header-desig1 = 'Director, SCM'.
      gw_header-name2 = 'Masrur Ahmed'.
      gw_header-desig2 = 'Managing Director'.
    WHEN '2000'.
      CONCATENATE 'RNL/CMR/' lv_sort2 '/' lv_ctype '/' sy-datum+0(4) '/' lv_incr INTO gw_header-ref.
      CONDENSE gw_header-ref.
      gw_header-name1 = 'M A Latif Shahrear Zahedee'.
      gw_header-desig1 = 'Director, SCM'.
      gw_header-name2 = 'Sina Ibn Jamali'.
      gw_header-desig2 = ' Managing Director'.
    WHEN '3000'.
      CONCATENATE 'PCL/CMR/' lv_sort2 '/' lv_ctype '/' sy-datum+0(4) '/' lv_incr INTO gw_header-ref.
      CONDENSE gw_header-ref.
      gw_header-name1 = 'M A Latif Shahrear Zahedee'.
      gw_header-desig1 = 'Director, SCM'.
      gw_header-name2 = 'Sina Ibn Jamali'.
      gw_header-desig2 = ' Managing Director'.
    WHEN '5000'.
      CONCATENATE 'RBCL/CMR/' lv_sort2 '/' lv_ctype '/' sy-datum+0(4) '/' lv_incr INTO gw_header-ref.
      CONDENSE gw_header-ref.
      gw_header-name1 = 'M A Latif Shahrear Zahedee'.
      gw_header-desig1 = 'Director, SCM'.
      gw_header-name2 = 'Masrur Ahmed'.
      gw_header-desig2 = 'Managing Director'.
    WHEN '9000'.
      CONCATENATE 'REXIME/CMR/' lv_sort2 '/' lv_ctype '/' sy-datum+0(4) '/' lv_incr INTO gw_header-ref.
      CONDENSE gw_header-ref.
      gw_header-name1 = 'Abu Shahriar Zahedee'.
      gw_header-desig1 = 'Executive Director'.
      CLEAR: gw_header-name2,
             gw_header-desig2.
    WHEN OTHERS.
  ENDCASE.

  CONDENSE: gw_header-bukrs,
            gw_header-bank_name,
            gw_header-bank_add,
            gw_header-lc_no,
            gw_header-lc_date,
            gw_header-lcaf_no,
            gw_header-hbl_no,
            gw_header-tr_no,
            gw_header-trans_doc_date,
            gw_header-comm_inv_no,
            gw_header-comm_inv_date,
            gw_header-port_dest,
            gw_header-benif,
            gw_header-retire,
            gw_header-waers,
            gw_header-print_date,
            gw_header-ref,
            gw_header-subject,
            gw_header-line1,
            gw_header-line2,
            gw_header-line3,
            gw_header-cname,
            gw_header-name1,
            gw_header-name2,
            gw_header-desig1,
            gw_header-desig2.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lv_frmname
    IMPORTING
      fm_name            = lv_fnam
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc = 0.
    CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
      EXPORTING
        i_language             = sy-langu
      IMPORTING
        e_devtype              = gv_devtype
      EXCEPTIONS
        no_language            = 1
        language_not_installed = 2
        no_devtype_found       = 3
        system_error           = 4
        OTHERS                 = 5.

    gs_ssfcompop-tdprinter = gv_devtype.
    gs_control-no_dialog = 'X'.
    gs_control-getotf = 'X'.

    CALL FUNCTION lv_fnam
      EXPORTING
        control_parameters = gs_control
        output_options     = gs_ssfcompop
        user_settings      = 'X'
        wa_header          = gw_header
      IMPORTING
        job_output_info    = gv_job_output
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
      EXPORTING
        i_otf                    = gv_job_output-otfdata
      EXCEPTIONS
        convert_otf_to_pdf_error = 1
        cntl_error               = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_piven
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM validate_piven .
  SELECT ekko~lifnr FROM ekko
   INNER JOIN zlc_process
   ON ekko~ebeln = zlc_process~ebeln
   WHERE zlc_process~pi_no IN @s_pi
    INTO TABLE @DATA(it_lifnr).
  IF it_lifnr IS NOT INITIAL.
    SORT it_lifnr BY lifnr.
    DELETE ADJACENT DUPLICATES FROM it_lifnr COMPARING lifnr.
    DESCRIBE TABLE it_lifnr LINES DATA(lv_lifnr).
    IF lv_lifnr > 1.
      MESSAGE 'Vendor is not same please check' TYPE 'E'.
      LEAVE TO SCREEN 1003.
    ENDIF.
  ENDIF.
ENDFORM.
