*&---------------------------------------------------------------------*
*& Include ZMPLC_PROCESS_TOP                        - Module Pool      ZMPLC_PROCESS
*&---------------------------------------------------------------------*
PROGRAM zmplc_process.
TABLES: zlc_process.

DATA: ins_comp      TYPE bu_nameor1,
      ins_add       TYPE zbank_add,
      ins_exch_rate TYPE wkurs,
      ins_note_no   TYPE zins_note_no,
      ins_exp_date  TYPE zins_exp_dt,
      ins_note_date TYPE zins_note_dt,
      ins_note_val  TYPE zins_note_val,
      ins_rcpt_no   TYPE zrcpt_no,
      ins_remarks   TYPE zins_rem.

TYPES: BEGIN OF ty_podet,
         mark        TYPE c,
         pi          TYPE zpi_no,
         ebeln       TYPE ebeln,
         manuf_name  TYPE mara-mfrnr,
         fob_freight TYPE c LENGTH 30,
         land1       TYPE zcountry,
         lstel       TYPE zport_load,
         vstel       TYPE zport_dest,
         inco        TYPE dzterm,
         lddat       TYPE lddat,
         ship_mode   TYPE zship_mode,
         hs_code     TYPE zhs_code,
         plant_desc  TYPE name1,
         ebelp       TYPE ebelp,
         matnr       TYPE matnr,
         txz01       TYPE txz01,
         menge       TYPE menge_d,
         meins       TYPE meins_d,
         kbetr       TYPE p LENGTH 6 DECIMALS 2, "vfprc_element_amount,
*         trans_cost    TYPE p LENGTH 6 DECIMALS 2,
*         loading_cost  TYPE p LENGTH 6 DECIMALS 2,
*         cnf_charge    TYPE p LENGTH 6 DECIMALS 2,
*         duty          TYPE p LENGTH 6 DECIMALS 2,
*         ins_charge    TYPE p LENGTH 6 DECIMALS 2,
*         bank_charge   TYPE p LENGTH 6 DECIMALS 2,
*         sea_freight   TYPE p LENGTH 6 DECIMALS 2,
*         air_freight   TYPE p LENGTH 6 DECIMALS 2,
*         truck_freight TYPE p LENGTH 6 DECIMALS 2,
*         calc_sea      TYPE p LENGTH 6 DECIMALS 2,
*         calc_air      TYPE p LENGTH 6 DECIMALS 2,
*         calc_air2     TYPE p LENGTH 6 DECIMALS 2,
         custom_duty TYPE p LENGTH 6 DECIMALS 2,
*         regl_duty     TYPE p LENGTH 6 DECIMALS 2,
*         suppl_duty    TYPE p LENGTH 6 DECIMALS 2,
         ait         TYPE p LENGTH 6 DECIMALS 2,
         atx         TYPE p LENGTH 6 DECIMALS 2,
         vi1         TYPE p LENGTH 6 DECIMALS 2,
         ots         TYPE p LENGTH 6 DECIMALS 2,
*         dcf           TYPE p LENGTH 6 DECIMALS 2,
         bnk         TYPE p LENGTH 6 DECIMALS 2,
         vlc         TYPE p LENGTH 6 DECIMALS 2,
         bnc         TYPE p LENGTH 6 DECIMALS 2,
         ipm         TYPE p LENGTH 6 DECIMALS 2,
         vti         TYPE p LENGTH 6 DECIMALS 2,
         tic         TYPE p LENGTH 6 DECIMALS 2,
         cfc         TYPE p LENGTH 6 DECIMALS 2,
         vcf         TYPE p LENGTH 6 DECIMALS 2,
         occ         TYPE p LENGTH 6 DECIMALS 2,
         pwc         TYPE p LENGTH 6 DECIMALS 2,
         ffc         TYPE p LENGTH 6 DECIMALS 2,
*         lcc         TYPE p LENGTH 6 DECIMALS 2,
*         lrc         TYPE p LENGTH 6 DECIMALS 2,
**
*
*         cnf         TYPE p LENGTH 6 DECIMALS 2,
*
*
*         crc         TYPE p LENGTH 6 DECIMALS 2,
*
*         srf         TYPE p LENGTH 6 DECIMALS 2,
*
*         itc         TYPE p LENGTH 6 DECIMALS 2,
*         lod         TYPE p LENGTH 6 DECIMALS 2,

         total_duty  TYPE p LENGTH 6 DECIMALS 2,
       END OF ty_podet,

       BEGIN OF ty_insdet,
         mark        TYPE c,
         pi          TYPE zpi_no,
         ebeln       TYPE ebeln,
         ebelp       TYPE ebelp,
         matnr       TYPE matnr,
         maktx       TYPE txz01,
         ship_mode   TYPE zship_mode,
         risk_type   TYPE zrisk_type,
         amend_no    TYPE zamend,
         policy_date TYPE zpol_dt,
       END OF ty_insdet,


       BEGIN OF ty_lctrack,
         mark           TYPE c,
         pi             TYPE zpi_no,
         ebeln          TYPE ebeln,
         ebelp          TYPE ebelp,
         comm_inv_no    TYPE  zcom_no,
         comm_inv_date  TYPE  zcomm_date,
         comm_inv_val   TYPE  zcomm_val,
         bl_awb_no      TYPE  zbl_no,
         bl_awb_date    TYPE  zbl_date,
         boe_no         TYPE  zboe_no,
         boe_date       TYPE  zboe_date,
         assemble_value TYPE  zassemble,
         total_duty     TYPE  ztot_duty,
         blklist        TYPE  z_blocklistno,
       END OF ty_lctrack.


TYPES: BEGIN OF ty_vendor,
         partner   TYPE bu_partner,
         name_org1 TYPE bu_nameor1,
       END OF ty_vendor.
*Added on 31-01-2021 BY EY_SANTOSH
TYPES:BEGIN OF ty_1004.
INCLUDE TYPE: zlc_process.
TYPES:mark TYPE char1,
*      ebelp TYPE ebelp,
      meins TYPE meins_d,
      kbetr TYPE kbetr,
      matnr TYPE matnr,
      txz01 TYPE txz01.
TYPES:END OF ty_1004.


DATA: it_podet    TYPE STANDARD TABLE OF ty_podet,
      it_podet1   TYPE STANDARD TABLE OF ty_podet,
      it_insdet   TYPE STANDARD TABLE OF ty_insdet,
      it_insdet1  TYPE STANDARD TABLE OF ty_insdet,
      it_lctrack  TYPE STANDARD TABLE OF ty_lctrack,
      it_lctrack1 TYPE STANDARD TABLE OF ty_lctrack,
      wa_lctrack  TYPE ty_lctrack,
      wa_insdet   TYPE ty_insdet,
      wa_podet    TYPE ty_podet,

      gt_vendor   TYPE STANDARD TABLE OF ty_vendor,
      gs_bank     TYPE lfa1,
      gs_vendor   TYPE ty_vendor.

DATA: inp_ebeln    TYPE ebeln,
      inp_pi       TYPE zpi_no,
      inp_lc       TYPE zlc_no,
      lc_low       TYPE zlc_no,
      lc_high      TYPE zlc_no,
      ins_ven      TYPE bu_partner,
      bank_ven     TYPE bu_partner,
      bank_name    TYPE zlc_bank,
      bank_add     TYPE zbank_add,
      tol_lmt      TYPE ztol_lmt,
      lc_acc_no    TYPE zlc_acc,
      lc_type      TYPE zlc_type,
      lc_margin    TYPE zlc_margin,
      lc_retire    TYPE zlc_retire,
      bank_remarks TYPE zbank_rem,
      gv_crt_flag,
      gv_mod_flag,
      gv_disp_flag,
      gv_screen,
      gv_save,
      gv_ans,
      sup_bank     TYPE bu_partner,
      inp_plant    TYPE werks_d,
      pi_date      TYPE dats,
      lc_date      TYPE dats,
      pi_low       TYPE zpi_no,
      pi_high      TYPE zpi_no,
      plant_high   TYPE werks_d,
      plant_low    TYPE werks_d,
      date_low     TYPE dats,
      date_high    TYPE dats,
      lc_bank_add  TYPE ad_smtpadr,
      ins_bank_add TYPE ad_smtpadr.

DATA : gv_rem_1002    TYPE zlc_process-bank_remarks,
       gv_retire_1002 TYPE zlc_process-lc_retire,
       gv_margin_1002 TYPE zlc_process-lc_margin,
       gv_lcacc_1002  TYPE zlc_process-lc_acc_no,
       gv_type_1002   TYPE zlc_process-lc_type,
       gv_pi_1002     TYPE zlc_process-pi_no.

DATA :gv_rem_1004      TYPE zlc_process-bank_remarks,
      gv_date_1004     TYPE zlc_process-lc_date,
      gv_expdate_1004  TYPE zlc_process-lc_exp_date,
      gv_confdate_1004 TYPE zlc_process-lc_conf_date,
      gv_shipdate_1004 TYPE zlc_process-ship_date,
      gv_term_1004     TYPE zlc_process-lc_term,
      gv_sdays_1004    TYPE zlc_process-doc_sub_days,
      gv_lcaf_1004     TYPE zlc_process-lcaf_no,
      gv_cbank_1004    TYPE zlc_process-conf_bank,
      gv_abank_1004    TYPE zlc_process-adv_bank.

DATA: gt_1001  TYPE TABLE OF zlc_process,
      gt_1002  TYPE TABLE OF zlc_process,
      gt_1003  TYPE TABLE OF zlc_process,
      gt_1005  TYPE TABLE OF zlc_process,
      gs_1001  TYPE zlc_process,
      gs_1001a TYPE zlc_process,
      gs_1002  TYPE zlc_process,
      gs_1003  TYPE zlc_process,
      gs_1004  TYPE zlc_process,
      gs_1005  TYPE zlc_process,
      "Added on 31-01-2021 BY EY_SANTOSH
      gt_1004  TYPE STANDARD TABLE OF ty_1004,
      gw_1004  TYPE ty_1004.
"EOA
DATA: gs_ssfcompop  TYPE ssfcompop,
      gs_control    TYPE ssfctrlop,
      gv_devtype    TYPE rspoptype,
      gv_job_output TYPE ssfcrescl,
      fm_name       TYPE rs38l_fnam,
      form_name     TYPE tdsfname.

DATA: gt_dropbox TYPE vrm_values,
      gs_dropbox LIKE LINE OF gt_dropbox.

DATA: ok_code LIKE sy-ucomm,
      it_ret  LIKE ddshretval OCCURS 0 WITH HEADER LINE.

DATA: gt_dom_val1 TYPE TABLE OF dd07v,
      gt_dom_val2 TYPE TABLE OF dd07l.

DATA: line_length      TYPE i VALUE 254,
      editor_container TYPE REF TO cl_gui_custom_container,
      text_editor      TYPE REF TO cl_gui_textedit,
      text             TYPE string.

DATA: gt_lctype   TYPE vrm_values,
      gs_lctype   LIKE LINE OF gt_lctype,
      gt_lcretire TYPE vrm_values,
      gs_lcretire LIKE LINE OF gt_lcretire,
      gt_list     TYPE vrm_values,
      gs_list     LIKE LINE OF gt_lctype,
      gs_list1    LIKE LINE OF gt_lcretire,
      gv_id       TYPE vrm_id.

*-------------Data Declaration of LC report---------------------
TYPES: BEGIN OF ty_final,
         ebeln       TYPE ebeln,
         pi_no       TYPE zpi_no,
         pi_dt       TYPE zpi_dt,
         pi_amt      TYPE zpi_amt,
         lifnr       TYPE lifnr,
         matnr       TYPE matnr,
         maktx       TYPE maktx,
         menge       TYPE menge_d,
         meins       TYPE meins_d,
         waers       TYPE waers,
         inco        TYPE dzterm,
         lc_val      TYPE zlc_val,
         lc_no       TYPE zlc_no,
         lc_date     TYPE zlc_date,
         ship_date   TYPE zship_date,
         lc_exp_date TYPE zlc_exp_date,
         lstel       TYPE char20,
         vstel       TYPE char20,
         lc_status   TYPE char20,
       END OF ty_final.

DATA:gt_final TYPE TABLE OF ty_final,
     gs_final TYPE          ty_final,
     gt_lc    TYPE STANDARD TABLE OF zlc_process.

DATA: it_fcat             TYPE slis_t_fieldcat_alv,
      wa_fcat             TYPE slis_fieldcat_alv,
      wa_layout           TYPE slis_layout_alv,
      lf_list_top_of_page TYPE slis_t_listheader.

DATA: gv_screenmode LIKE sy-ucomm.

" Selection Screen for multiple PO

*********************** Selection screen for LC report **********************************
SELECTION-SCREEN: BEGIN OF SCREEN 9102 AS SUBSCREEN.
  SELECT-OPTIONS: s_pino FOR zlc_process-pi_no,
                  s_lcno FOR zlc_process-lc_no,
                  s_lcdt FOR zlc_process-lc_date,
                  s_lcexdt FOR zlc_process-lc_exp_date,
                  s_pono FOR zlc_process-ebeln,
                  s_vendor FOR zlc_process-lifnr.
SELECTION-SCREEN: END OF SCREEN 9102.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

SELECTION-SCREEN BEGIN OF SCREEN 9100 AS SUBSCREEN.
  SELECT-OPTIONS: s_ebeln FOR zlc_process-ebeln NO INTERVALS VISIBLE LENGTH 20.
SELECTION-SCREEN END OF SCREEN 9100.


SELECTION-SCREEN BEGIN OF SCREEN 9101 AS SUBSCREEN.
  SELECT-OPTIONS: s_pi FOR zlc_process-pi_no MATCHCODE OBJECT zpi MODIF ID 001
  NO INTERVALS VISIBLE LENGTH 15.
SELECTION-SCREEN END OF SCREEN 9101.


*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_1004' ITSELF
CONTROLS: tc_1004 TYPE TABLEVIEW USING SCREEN 1004.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_1004'
DATA:     g_tc_1004_lines  LIKE sy-loopc.

DATA:     ok_1004 LIKE sy-ucomm.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
**sel screen disbale -for 1004 display and update only
AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = '9101'
**Display LC screen
  AND ( ( gv_screenmode = 'LC_DISP' )
**Update LC screen
  OR    ( gv_screenmode = 'LC_MOD' )
 ).
    LOOP AT SCREEN.
      IF screen-group1 EQ '001'.
        screen-input = '0'.
*        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
