"! <p class="shorttext synchronized" lang="en">Converts the internal table into CSV</p>
CLASS zcl_itab_to_csv DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS convert
      IMPORTING
        delimiter      TYPE cl_rsda_csv_converter=>char DEFAULT cl_rsda_csv_converter=>c_default_delimiter
        separator      TYPE cl_rsda_csv_converter=>char DEFAULT cl_rsda_csv_converter=>c_default_separator
        escape         TYPE cl_rsda_csv_converter=>char OPTIONAL
        line_separator TYPE cl_rsda_csv_converter=>char DEFAULT cl_abap_char_utilities=>newline
        VALUE(tbl)     TYPE ANY TABLE
      RETURNING
        VALUE(ret)     TYPE xstring.
  PROTECTED SECTION.

ENDCLASS.



CLASS zcl_itab_to_csv IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD convert.
    DATA:
      lv_csv_string    TYPE string,
      ls_csv_itab_line TYPE LINE OF truxs_t_text_data.

    DATA(csv) = cl_rsda_csv_converter=>create(
                  i_delimiter      = delimiter
                  i_separator      = separator
                  i_escape         = escape
                  i_line_separator = line_separator ).

    LOOP AT tbl ASSIGNING FIELD-SYMBOL(<tbl>).
      csv->structure_to_csv(
        EXPORTING i_s_data = <tbl>
        IMPORTING e_data   = ls_csv_itab_line ).

      lv_csv_string = lv_csv_string && ls_csv_itab_line && cl_abap_char_utilities=>cr_lf.
    ENDLOOP.

    TRY.
        ret = cl_bcs_convert=>string_to_xstring( lv_csv_string ).
      CATCH cx_bcs INTO DATA(lx_bcs_error).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
