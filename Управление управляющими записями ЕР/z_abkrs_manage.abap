*&---------------------------------------------------------------------*
*& Report  Z_ABKRS_MANAGE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
"
REPORT z_abkrs_manage.
INCLUDE <color>.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.

PARAMETERS: p_log AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b01.



TYPES:
BEGIN OF ts_abkrs,
abkrs LIKE p0001-abkrs,
END OF ts_abkrs,

tt_abkrs TYPE TABLE OF ts_abkrs,


BEGIN OF ts_pv0000,


abkrs TYPE abkrs,
pabrj TYPE pabrj,
pabrp TYPE pabrp,
state TYPE vwsta,
srtfd TYPE vwsrt,
mycolor TYPE lvc_t_scol,
END OF ts_pv0000,
tt_pv000 TYPE TABLE OF ts_pv0000 WITH NON-UNIQUE DEFAULT KEY.

DATA:

ls_pv000 TYPE ts_pv0000,
lt_pv000 TYPE tt_pv000,
ls_abkrs TYPE ts_abkrs,
lt_abkrs TYPE tt_abkrs,
lt_color TYPE lvc_t_scol,
ls_color TYPE lvc_s_scol.

FIELD-SYMBOLS: <fs_pv0000> TYPE LINE OF tt_pv000.



TABLES: pernr.
INFOTYPES: 0001.


CLASS lo_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
on_double_click FOR EVENT double_click
OF if_salv_events_actions_table
IMPORTING row column.
ENDCLASS.

CLASS lo_handler IMPLEMENTATION.
  METHOD on_double_click.

    DATA: ls_bdcdata TYPE bdcdata,
          lt_bdcdata TYPE TABLE OF bdcdata,

        l_row TYPE i.


    CLEAR: ls_bdcdata, lt_bdcdata.
    l_row = row.
    READ TABLE lt_pv000 INTO ls_pv000 INDEX l_row.

    ls_bdcdata-fnam = 'ABK'.
    ls_bdcdata-fval = ls_pv000-abkrs.

    APPEND ls_bdcdata TO lt_bdcdata.


    SET PARAMETER ID 'ABK' FIELD ls_pv000-abkrs.
    " call SCREEN '1000' SUBMIT SAPMP52X WITH ABKRS = ls_abkrs-abkrs AND RETURN.
    CALL TRANSACTION 'PA03'.
    " CALL TRANSACTION 'PA03'. "USING lt_bdcdata.
  ENDMETHOD.
ENDCLASS.

LOAD-OF-PROGRAM.
INITIALIZATION.


START-OF-SELECTION.
  CLEAR lt_abkrs.
GET pernr.

  PROVIDE abkrs FROM p0001 BETWEEN pn-begda AND pn-endda. "RP_PROVIDE_FROM_LAST

  ls_abkrs-abkrs = p0001-abkrs.
  COLLECT ls_abkrs INTO lt_abkrs.

  IF p_log IS NOT INITIAL.
    WRITE: / pernr-pernr, '...', p0001-abkrs, '...', p0001-begda, '-', p0001-endda.
  ENDIF.

  ENDPROVIDE.

END-OF-SELECTION.

  CHECK lt_abkrs IS NOT INITIAL.
  DELETE ADJACENT DUPLICATES FROM lt_abkrs.

  SELECT *
INTO CORRESPONDING FIELDS OF TABLE lt_pv000
FROM t569v
FOR ALL ENTRIES IN lt_abkrs WHERE abkrs = lt_abkrs-abkrs.

  LOOP AT lt_pv000 ASSIGNING <fs_pv0000> WHERE state = '1'.


    CLEAR lt_color.
    CLEAR ls_color.

    ls_color-color-col = col_negative.
    ls_color-color-int = 0.
    ls_color-color-inv = 0.
    APPEND ls_color TO lt_color.
    <fs_pv0000>-mycolor = lt_color.





  ENDLOOP.



  PERFORM z_show USING lt_pv000.

FORM z_show USING fu_data
TYPE tt_pv000.
  DATA:

lo_salv TYPE REF TO cl_salv_table,
lo_functions TYPE REF TO cl_salv_functions_list,
lo_evt TYPE REF TO cl_salv_events_table,
lo_columns TYPE REF TO cl_salv_columns_table.

  IF lines( fu_data ) = 0.
    EXIT.
  ENDIF.
  TRY.
      CALL METHOD cl_salv_table=>factory
IMPORTING
r_salv_table = lo_salv
CHANGING
t_table = fu_data.
    CATCH cx_salv_msg .
      RETURN.



  ENDTRY.


  PERFORM z_set_fc4alv USING lo_salv.


  lo_columns = lo_salv->get_columns( ).

  lo_functions = lo_salv->get_functions( ).
  lo_evt = lo_salv->get_event( ).
  SET HANDLER lo_handler=>on_double_click FOR lo_evt.
  lo_functions->set_default( ).
  lo_functions->set_export_localfile( abap_true ).

  lo_columns->set_color_column( value = 'MYCOLOR' ).
  lo_salv->display( ).


ENDFORM. " z_show_kpi


FORM z_set_fc4alv USING
fu_o_salv TYPE REF TO cl_salv_table.
  DATA:
lt_fc TYPE salv_t_column_ref,
lo_column TYPE REF TO cl_salv_column,
lo_columns TYPE REF TO cl_salv_columns_table.
  FIELD-SYMBOLS:
<fs_fc> TYPE salv_s_column_ref.

  lo_columns = fu_o_salv->get_columns( ).
  lt_fc = lo_columns->get( ).
  LOOP AT lt_fc ASSIGNING <fs_fc>.
    CASE <fs_fc>-columnname.
      WHEN 'ABKRS'.
        <fs_fc>-r_column->set_long_text( 'ABKRS' ).
        <fs_fc>-r_column->set_medium_text( 'ABKRS' ).
        <fs_fc>-r_column->set_output_length( 12 ).

      WHEN 'PABRJ'.
        <fs_fc>-r_column->set_long_text( 'PABRJ' ).
        <fs_fc>-r_column->set_medium_text( 'PABRJ' ).
        <fs_fc>-r_column->set_output_length( 12 ).
      WHEN 'PABRP'.
        <fs_fc>-r_column->set_long_text( 'PABRP' ).
        <fs_fc>-r_column->set_medium_text( 'PABRP' ).
        <fs_fc>-r_column->set_output_length( 12 ).
      WHEN 'STATE'.
        <fs_fc>-r_column->set_long_text( 'STATE' ).
        <fs_fc>-r_column->set_medium_text( 'STATE' ).
        <fs_fc>-r_column->set_output_length( 12 ).


      WHEN OTHERS.
        <fs_fc>-r_column->set_technical( abap_true ).
    ENDCASE.
  ENDLOOP.


ENDFORM. " z_set_fc4alv