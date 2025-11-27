"! <p class="shorttext synchronized" lang="en">Converts the internal table into excel_xml</p>
CLASS zcl_itab_to_xls DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS convert
      IMPORTING
        value(tbl)     TYPE ANY TABLE
        alv_disvariant TYPE disvariant OPTIONAL
      RETURNING
        VALUE(ret)      TYPE xstring.
  PROTECTED SECTION.

ENDCLASS.

CLASS zcl_itab_to_xls IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD convert.
    DATA :
      mt_data TYPE REF TO data,
      salv    TYPE REF TO cl_salv_table.

    FIELD-SYMBOLS:
      <tab> TYPE STANDARD TABLE.

    GET REFERENCE OF tbl INTO mt_data.

    ASSIGN mt_data->* TO <tab>.
    TRY .
        cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = salv
        CHANGING
          t_table      = <tab> ).
      CATCH cx_salv_msg.

    ENDTRY.

    IF alv_disvariant-variant IS NOT INITIAL.
      DATA:
        lr_layout TYPE REF TO cl_salv_layout.

      lr_layout = salv->get_layout( ).

      lr_layout->set_key( VALUE #(
               report = COND #( WHEN alv_disvariant-report IS INITIAL THEN sy-cprog
                                ELSE alv_disvariant-report )
               handle = alv_disvariant-handle ) ).

      lr_layout->set_initial_layout( alv_disvariant-variant ).

    ENDIF.

    ret = salv->to_xml( xml_type = if_salv_bs_xml=>C_TYPE_XLSX ). "c_type_excel_xml ).

  ENDMETHOD.
ENDCLASS.
