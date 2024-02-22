"! <p class="shorttext synchronized" lang="en">Converts the internal table into excel_xml</p>
class ZCL_ITAB_TO_XLS definition
  public
  create public .

public section.

  methods CONSTRUCTOR .
  methods CONVERT
    importing
      !TBL type ANY TABLE
      !ALV_DISVARIANT type DISVARIANT optional
    returning
      value(RET) type XSTRING .
  PROTECTED SECTION.

ENDCLASS.



CLASS ZCL_ITAB_TO_XLS IMPLEMENTATION.


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

    ret = salv->to_xml( xml_type = if_salv_bs_xml=>c_type_excel_xml ).

  ENDMETHOD.
ENDCLASS.
