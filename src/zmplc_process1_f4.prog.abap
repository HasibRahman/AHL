*&---------------------------------------------------------------------*
*& Include          ZMPLC_PROCESS_F4
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PI_NO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pi_no INPUT.
  SELECT ebeln, pi_no, lc_no FROM zlc_process INTO TABLE @DATA(gt_pi_no).
  SORT gt_pi_no BY ebeln.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'PI_NO'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1002'
      dynprofield  = 'INP_PI'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_pi_no
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO inp_pi.
    IF gv_disp_flag = 'X'
    OR gv_mod_flag = 'X'.
      SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1002
        WHERE pi_no = @s_pi-low.
      IF sy-subrc NE 0.
        MESSAGE i000(zelc) WITH s_pi-low.
        CLEAR gs_1002.
        CLEAR s_pi-low.
      ELSEIF gs_1002-pi_canc_ind = 'X'.
        CLEAR s_pi-low.
        MESSAGE i018(zelc) WITH s_pi-low.
        CLEAR gs_1002.
*      call screen 1002.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PI_NO_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ebeln INPUT.
  SELECT ebeln, pi_no FROM zlc_process INTO TABLE @DATA(gt_ebeln).
  SORT gt_ebeln BY ebeln.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'EBELN'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1001'
      dynprofield  = 'INP_EBELN'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_ebeln
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO inp_ebeln.
    CLEAR gs_1001.
    SELECT SINGLE * FROM zlc_process INTO gs_1001 WHERE ebeln = inp_ebeln.
    PERFORM fill_po.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PI_NO_1003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pi_no_1003 INPUT.
  SELECT ebeln, pi_no FROM zlc_process INTO TABLE @DATA(gt_pino).
  SORT gt_pino BY ebeln.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'PI_NO'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1003'
      dynprofield  = 'INP_PI'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_pino
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO inp_pi.

    SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1003 WHERE pi_no = @inp_pi.
    IF sy-subrc NE 0.
      MESSAGE i000(zelc) WITH inp_pi.
    ELSEIF gs_1003-pi_canc_ind = 'X'.
      CLEAR inp_pi.
      MESSAGE i018(zelc) WITH gs_1003-pi_no.
      CLEAR gs_1003.
    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PI_NO_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pi_no_1001 INPUT.
  SELECT ebeln, pi_no FROM zlc_process INTO TABLE @DATA(gt_pi_1001).
  SORT gt_pi_1001 BY ebeln.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'PI_NO'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1001'
      dynprofield  = 'INP_PI'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_pi_1001
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO inp_pi.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  LC_NO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_no INPUT.

  SELECT ebeln, pi_no, lc_no FROM zlc_process INTO TABLE @DATA(gt_lcno).
  SORT gt_lcno BY ebeln.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'LC_NO'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1005'
      dynprofield  = 'INP_LC'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_lcno
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO inp_lc.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BANK_VENDOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE bank_vendor INPUT.

  REFRESH gt_vendor.
  SELECT lifnr name1 FROM lfa1 INTO TABLE gt_vendor.

*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
**     DDIC_STRUCTURE         = ' '
*      retfield     = 'PARTNER'
**     PVALKEY      = ' '
*      dynpprog     = 'ZLC_PROCESS'
*      dynpnr       = '1002'
*      dynprofield  = 'BANK_VEN'
**     STEPL        = 0
*      window_title = 'input'
**     VALUE        = ' '
*      value_org    = 'S'
*    TABLES
*      value_tab    = gt_vendor
**     FIELD_TAB    =
*      return_tab   = it_ret.
*  IF sy-subrc = 0.
*
*    READ TABLE it_ret INDEX 1.
*
*    MOVE it_ret-fieldval TO bank_ven.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = bank_ven
    IMPORTING
      output = bank_ven.

  READ TABLE gt_vendor INTO gs_vendor WITH KEY partner = bank_ven.
  IF sy-subrc = 0.


*    IF bank_ven IS NOT INITIAL.
*    IF gv_disp_flag NE 'X'"non create
*    AND gv_mod_flag NE 'X'.
*        gs_1002-lc_bank_vendor = bank_ven.
    bank_name = gs_vendor-name_org1.

    SELECT SINGLE * FROM lfa1 INTO gs_bank WHERE lifnr = bank_ven.
    IF sy-subrc = 0.
      CONCATENATE gs_bank-stras  gs_bank-regio gs_bank-ort01 gs_bank-pstlz INTO bank_add SEPARATED BY ','.
    ENDIF.
*    ELSE.                 "create
*      IF gs_1002-lc_bank_vendor IS NOT INITIAL
*     AND bank_ven NE gs_1002-lc_bank_vendor.
*        bank_ven = gs_1002-lc_bank_vendor.
*        MESSAGE e024(zabap) DISPLAY LIKE 'I'.
*        LEAVE TO SCREEN 1002.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*      MODIFY zlc_process FROM gs_1002.
*    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INS_COMP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ins_comp INPUT.
  REFRESH gt_vendor.
  CLEAR gs_bank.
  SELECT lifnr name1 FROM lfa1 INTO TABLE gt_vendor.

*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
**     DDIC_STRUCTURE         = ' '
*      retfield     = 'PARTNER'
**     PVALKEY      = ' '
*      dynpprog     = 'ZLC_PROCESS'
*      dynpnr       = '1003'
*      dynprofield  = 'INS_VEN'
**     STEPL        = 0
*      window_title = 'input'
**     VALUE        = ' '
*      value_org    = 'S'
*    TABLES
*      value_tab    = gt_vendor
**     FIELD_TAB    =
*      return_tab   = it_ret.

*  IF sy-subrc = 0.

*    READ TABLE it_ret INDEX 1.

*  MOVE it_ret-fieldval TO ins_ven.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ins_ven
    IMPORTING
      output = ins_ven.
*    SELECT SINGLE ins_vendor FROM zlc_process INTO @DATA(lv_vendor) WHERE pi_no IN @s_pi.
*    IF lv_vendor IS NOT INITIAL.
*      MESSAGE 'Insurance Information already Created for this PI' TYPE 'E' DISPLAY LIKE 'S'.
*    ENDIF.
  READ TABLE gt_vendor INTO gs_vendor WITH KEY partner = ins_ven.
  IF sy-subrc = 0.
    gs_1003-ins_vendor = ins_ven.
    ins_comp = gs_vendor-name_org1.
    SELECT SINGLE * FROM lfa1 INTO gs_bank WHERE lifnr = ins_ven.
    IF sy-subrc = 0.
      CONCATENATE gs_bank-stras  gs_bank-regio gs_bank-ort01 gs_bank-pstlz INTO ins_add SEPARATED BY ','.
    ENDIF.
*    MODIFY zlc_process FROM gs_1003.
  ENDIF.

*  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DATE_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE date_f4_1 INPUT.

  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = gs_1001-pi_dt
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = gs_1001-pi_dt
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  DATE_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE date_f4_2 INPUT.

  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = wa_podet-lddat
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = wa_podet-lddat
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SUPP_BANK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE supp_bank INPUT.
  REFRESH gt_vendor.
  SELECT lifnr name1 FROM lfa1 INTO TABLE gt_vendor.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'PARTNER'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1001'
      dynprofield  = 'SUP_BANK'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_vendor
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.

    READ TABLE it_ret INDEX 1.

    MOVE it_ret-fieldval TO sup_bank.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = sup_bank
      IMPORTING
        output = sup_bank.

    READ TABLE gt_vendor INTO gs_vendor WITH KEY partner = sup_bank.
    IF sy-subrc = 0.
      gs_1001-sup_bank_vendor = sup_bank.
      gs_1001-sup_bank = gs_vendor-name_org1.
      MODIFY zlc_process FROM gs_1001.
    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DATE_F4_3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE date_f4_3 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = gs_1004-lc_date
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = gs_1004-lc_date
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.

MODULE date_f4_4 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = gs_1004-lc_exp_date
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = gs_1004-lc_exp_date
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.

MODULE date_f4_5 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = gs_1004-lc_conf_date
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = gs_1004-lc_conf_date
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.

MODULE date_f4_6 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = gw_1004-lddat
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = gw_1004-lddat
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.

MODULE date_f4_7 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = ins_note_date
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = ins_note_date
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.

MODULE date_f4_8 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = ins_exp_date
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = ins_exp_date
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  V1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v1 INPUT.

  SELECT lifnr name1 FROM lfa1 INTO TABLE gt_vendor.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'V1'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1005'
      dynprofield  = 'GS_1005-PSI_VENDOR'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_vendor
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.

    READ TABLE it_ret INDEX 1.

    MOVE it_ret-fieldval TO gs_1005-psi_vendor.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  V2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v2 INPUT.

  SELECT lifnr name1 FROM lfa1 INTO TABLE gt_vendor.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'V2'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1005'
      dynprofield  = 'GS_1005-FREIGHT_VENDOR'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_vendor
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.

    READ TABLE it_ret INDEX 1.

    MOVE it_ret-fieldval TO gs_1005-freight_vendor.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  V3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v3 INPUT.

  SELECT lifnr name1 FROM lfa1 INTO TABLE gt_vendor.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'V3'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1005'
      dynprofield  = 'GS_1005-DOC_HAND_VENDOR'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_vendor
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.

    READ TABLE it_ret INDEX 1.

    MOVE it_ret-fieldval TO gs_1005-doc_hand_vendor.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  V4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v4 INPUT.

  SELECT lifnr name1 FROM lfa1 INTO TABLE gt_vendor.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'V4'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1005'
      dynprofield  = 'GS_1005-APPL_DP_VENDOR'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_vendor
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.

    READ TABLE it_ret INDEX 1.

    MOVE it_ret-fieldval TO gs_1005-appl_dp_vendor.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  V5  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v5 INPUT.

  SELECT lifnr name1 FROM lfa1 INTO TABLE gt_vendor.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'V5'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1005'
      dynprofield  = 'GS_1005-TRN_VENDOR'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_vendor
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.

    READ TABLE it_ret INDEX 1.

    MOVE it_ret-fieldval TO gs_1005-trn_vendor.

  ENDIF.
ENDMODULE.

*{   INSERT         RSDK900796                                        1
*&---------------------------------------------------------------------*
*&      Module  V6  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v6 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = wa_lctrack-comm_inv_date
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = wa_lctrack-comm_inv_date
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                        2
*&---------------------------------------------------------------------*
*&      Module  V7  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v7 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = gs_1005-etd
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = gs_1005-etd
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                        3
*&---------------------------------------------------------------------*
*&      Module  V8  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v8 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = wa_lctrack-bl_awb_date
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = wa_lctrack-bl_awb_date
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                        4
*&---------------------------------------------------------------------*
*&      Module  PI_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pi_date INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = pi_date
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = pi_date
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                        5
*&---------------------------------------------------------------------*
*&      Module  LC_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_date INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = lc_date
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = lc_date
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                        6
*&---------------------------------------------------------------------*
*&      Module  WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE werks INPUT.
  SELECT pi_no,werks FROM zlc_process INTO TABLE @DATA(gt_werks).
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'WERKS'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1003'
      dynprofield  = 'PLANT_LOW'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_werks
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO plant_low.

    SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1003 WHERE pi_no = @inp_pi.
    IF sy-subrc NE 0.
      MESSAGE i000(zelc) WITH inp_pi.
    ENDIF.

  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                        7
*&---------------------------------------------------------------------*
*&      Module  PI_NO_LOW  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pi_no_low INPUT.
  DATA: lv_dynfld TYPE help_info-dynprofld.
  SELECT ebeln, pi_no, lc_no FROM zlc_process INTO TABLE @gt_pi_no.
  SORT gt_pi_no BY ebeln.
  IF sy-dynnr = '9002'.
    lv_dynfld = 'S_PINO-LOW'.
  ELSE.
    lv_dynfld = 'PI_LOW'.
  ENDIF.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'PI_NO'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = sy-dynnr "'1002'
      dynprofield  = lv_dynfld "'PI_LOW'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_pi_no
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO pi_low.

    SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1002
      WHERE pi_no = @pi_low.
    IF sy-subrc NE 0.
      MESSAGE i000(zelc) WITH pi_low.
    ENDIF.

  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                        8
*&---------------------------------------------------------------------*
*&      Module  PI_NO_HIGH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pi_no_high INPUT.
  SELECT ebeln, pi_no, lc_no FROM zlc_process INTO TABLE @gt_pi_no.
  SORT gt_pi_no BY ebeln.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'PI_NO'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1002'
      dynprofield  = 'PI_HIGH'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_pi_no
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO pi_high.

    SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1002
      WHERE pi_no = @pi_high.
    IF sy-subrc NE 0.
      MESSAGE i000(zelc) WITH pi_low.
    ENDIF.

  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                        9
*&---------------------------------------------------------------------*
*&      Module  PI_DATE_LOW  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pi_date_low INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = date_low
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = date_low
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                       10
*&---------------------------------------------------------------------*
*&      Module  PI_DATE_HIGH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pi_date_high INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = date_high
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = date_high
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                       11
*&---------------------------------------------------------------------*
*&      Module  LC_NO_LOW  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_no_low INPUT.
  SELECT ebeln, pi_no, lc_no FROM zlc_process INTO TABLE @DATA(gt_lc_no).
  SORT gt_pi_no."gt_lc_no BY ebeln.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield     = 'LC_NO'
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1002'
      dynprofield  = 'LC_LOW'
      window_title = 'input'
      value_org    = 'S'
    TABLES
      value_tab    = gt_lc_no
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO lc_low.

    SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1002
      WHERE lc_no = @lc_low.
    IF sy-subrc NE 0.
      MESSAGE i000(zelc) WITH lc_low.
    ENDIF.

  ENDIF.
ENDMODULE.
*}   INSERT

*{   INSERT         RSDK900796                                       12
*&---------------------------------------------------------------------*
*&      Module  LC_NO_HIGH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lc_no_high INPUT.
  SELECT ebeln, pi_no, lc_no FROM zlc_process INTO TABLE @gt_lc_no.
  SORT gt_lc_no BY ebeln.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'LC_NO'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1002'
      dynprofield  = 'LC_HIGH'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_lc_no
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO lc_high.

    SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1002
      WHERE lc_no = @lc_high.
    IF sy-subrc NE 0.
      MESSAGE i000(zelc) WITH lc_high.
    ENDIF.

  ENDIF.
ENDMODULE.
*}   INSERT
*&---------------------------------------------------------------------*
*&      Module  PLANT_DESC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PI_NO4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pi_no4 INPUT.
  REFRESH: gt_pi_no.
  SELECT ebeln pi_no lc_no FROM zlc_process INTO TABLE gt_pi_no.
  SORT gt_pi_no BY ebeln.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield     = 'PI_NO'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1004'
      dynprofield  = 'INP_PI'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_pi_no
*     FIELD_TAB    =
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO inp_pi.
    SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1004
      WHERE pi_no = @inp_pi.
    IF sy-subrc NE 0.
      MESSAGE i000(zelc) WITH inp_pi.
      CLEAR gs_1004.
      CLEAR inp_pi.
    ELSEIF gs_1004-pi_canc_ind = 'X'.
      CLEAR inp_pi.
      MESSAGE i018(zelc) WITH gs_1004-pi_no.
      CLEAR gs_1004.
*      call screen 1002.
    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DATE_F4_9  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE date_f4_9 INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_LC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_lc INPUT.

  REFRESH: gt_lc_no.
  SELECT ebeln,pi_no,lc_no FROM zlc_process INTO TABLE @gt_lc_no.
  SORT gt_lc_no BY ebeln.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield     = 'LC_NO'
*     PVALKEY      = ' '
      dynpprog     = 'ZLC_PROCESS'
      dynpnr       = '1004'
      dynprofield  = 'INP_LC'
*     STEPL        = 0
      window_title = 'input'
*     VALUE        = ' '
      value_org    = 'S'
    TABLES
      value_tab    = gt_lc_no
      return_tab   = it_ret.
  IF sy-subrc = 0.
    READ TABLE it_ret INDEX 1.
    MOVE it_ret-fieldval TO inp_lc.
    SELECT SINGLE * FROM zlc_process INTO CORRESPONDING FIELDS OF @gs_1004
      WHERE lc_no = @inp_lc.
    IF sy-subrc NE 0.
      MESSAGE i019(zelc) WITH inp_lc.
      CLEAR gs_1004.
      CLEAR inp_lc.
    ELSEIF gs_1004-pi_canc_ind = 'X'.
      CLEAR inp_lc.
      MESSAGE i020(zelc) WITH gs_1004-lc_no.
      CLEAR gs_1004.
*      call screen 1002.
    ENDIF.
*
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  V9  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE v9 INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month         = wa_lctrack-boe_date
      display                      = ' '
*     FACTORY_CALENDAR_ID          = ' '
*     GREGORIAN_CALENDAR_FLAG      = ' '
*     HOLIDAY_CALENDAR_ID          = ' '
*     PROGNAME_FOR_FIRST_MONTH     = ' '
*     DATE_POSITION                = ' '
    IMPORTING
      select_date                  = wa_lctrack-boe_date
*     SELECT_WEEK                  =
*     SELECT_WEEK_BEGIN            =
*     SELECT_WEEK_END              =
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      date_after_range             = 2
      date_before_range            = 3
      date_invalid                 = 4
      factory_calendar_not_found   = 5
      holiday_calendar_not_found   = 6
      parameter_conflict           = 7
      OTHERS                       = 8.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.
