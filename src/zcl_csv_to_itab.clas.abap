"! <p class="shorttext synchronized" lang="en">Converts CSV ITAB to STRUCTURED itab</p>
CLASS zcl_csv_to_itab DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS convert
      IMPORTING
        delimiter      TYPE cl_rsda_csv_converter=>char DEFAULT cl_rsda_csv_converter=>c_default_delimiter
        separator      TYPE cl_rsda_csv_converter=>char DEFAULT cl_rsda_csv_converter=>c_default_separator
        escape         TYPE cl_rsda_csv_converter=>char OPTIONAL
        i_csv_tbl      TYPE ANY TABLE
      EXPORTING
        e_strc_tbl     TYPE table.
  PROTECTED SECTION.

ENDCLASS.



CLASS zcl_csv_to_itab IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD convert.
    DATA:
      lv_csv_string    TYPE string,
      ls_csv_itab_line TYPE LINE OF truxs_t_text_data.
    FIELD-SYMBOLS: <e_strc_tbl> TYPE any.

    DATA(csv) = cl_rsda_csv_converter=>create(
                  i_delimiter      = delimiter
                  i_separator      = separator
                  i_escape         = escape ).

    LOOP AT i_csv_tbl ASSIGNING FIELD-SYMBOL(<i_tbl>).
      "remove special Characters from the line
      IF <i_tbl> CS cl_abap_char_utilities=>newline.
        <i_tbl> = replace( val = <i_tbl> sub = cl_abap_char_utilities=>newline with = space occ = 0 ).
      ENDIF.

      APPEND INITIAL LINE TO e_strc_tbl ASSIGNING <e_strc_tbl>.

      IF sy-subrc = 0 AND <e_strc_tbl> IS ASSIGNED.
        " Parse the CSV line
        csv->csv_to_structure( EXPORTING i_data   = <i_tbl>
                               IMPORTING e_s_data = <e_strc_tbl> ).
      ENDIF.
      UNASSIGN <e_strc_tbl>.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
