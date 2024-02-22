"! <p class="shorttext synchronized" lang="en">Converts the internal table into HTML format</p>
CLASS zcl_itab_to_html DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:

      constructor
        IMPORTING
          table_id    TYPE string OPTIONAL
          table_class TYPE string OPTIONAL
          table_style TYPE string DEFAULT 'width: 80%; border: #999 1px solid; border-collapse: collapse;'
          tr_class    TYPE string OPTIONAL
          tr_style    TYPE string OPTIONAL
          th_class    TYPE string OPTIONAL
          th_style    TYPE string DEFAULT 'font-weight: bold; border: #999 1px solid; background: #eee;'
          td_class    TYPE string OPTIONAL
          td_style    TYPE string DEFAULT 'border: #999 1px solid;',

      convert
        IMPORTING tbl        TYPE ANY TABLE
        RETURNING VALUE(ret) TYPE string.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF t_table_description,
        name        TYPE string,
        title       TYPE string,
        description TYPE string,
      END OF t_table_description,
      t_table_description_t TYPE STANDARD TABLE OF t_table_description WITH KEY name.


    DATA:
      cellpadding TYPE i VALUE 0,
      cellspacing TYPE i VALUE 0,

      table_class TYPE string VALUE '',
      table_id    TYPE string VALUE '',
      table_style TYPE string VALUE '',

      tr_class    TYPE string VALUE '',
      tr_style    TYPE string VALUE '',

      th_class    TYPE string VALUE '',
      th_style    TYPE string VALUE '',

      td_class    TYPE string VALUE '',
      td_style    TYPE string VALUE ''.

    METHODS:
      get_description
        IMPORTING tbl TYPE ANY TABLE
        RETURNING VALUE(ret) TYPE t_table_description_t,

     title
       IMPORTING desc TYPE t_table_description_t
       RETURNING VALUE(ret) TYPE string,

    table_params
      RETURNING VALUE(ret) TYPE string,
    tr_params
      RETURNING VALUE(ret) TYPE string,
    th_params
      RETURNING VALUE(ret) TYPE string,
    td_params
      RETURNING VALUE(ret) TYPE string,

    value_to_string
      IMPORTING field TYPE string val TYPE any
      RETURNING VALUE(ret) TYPE string,

    footer
      IMPORTING desc TYPE t_table_description_t
      RETURNING VALUE(ret) TYPE string.
ENDCLASS.



CLASS ZCL_ITAB_TO_HTML IMPLEMENTATION.


  METHOD constructor.
    IF table_id IS SUPPLIED.
      me->table_id = table_id.
    ENDIF.
    IF table_class IS SUPPLIED.
      me->table_class = table_class.
    ENDIF.
    "IF table_style IS SUPPLIED.
    me->table_style = table_style.
    "ENDIF.

    IF tr_class IS SUPPLIED.
      me->tr_class = tr_class.
    ENDIF.
    IF tr_style IS SUPPLIED.
      me->tr_style = tr_style.
    ENDIF.

    IF th_class IS SUPPLIED.
      me->th_class = th_class.
    ENDIF.
    "IF th_style IS SUPPLIED.
    me->th_style = th_style.
    "ENDIF.

    IF td_class IS SUPPLIED.
      me->td_class = td_class.
    ENDIF.
    "IF td_style IS SUPPLIED.
    me->td_style = td_style.
    "ENDIF.
  ENDMETHOD.


  METHOD convert.
    DATA(descr) = get_description( tbl ).
    DATA(tr) = tr_params( ).
    DATA(td) = td_params( ).

    DATA row TYPE string VALUE ''.
    LOOP AT tbl ASSIGNING FIELD-SYMBOL(<row>).
      CLEAR row.
      LOOP AT descr ASSIGNING FIELD-SYMBOL(<struct>).
        ASSIGN COMPONENT <struct>-name OF STRUCTURE <row> TO FIELD-SYMBOL(<value>).
        row = |{ row }  <td{ td }>{ value_to_string( field = <struct>-name val = <value> ) }</td>|.
      ENDLOOP.

      ret = |{ ret } <tr{ tr }>{ row } </tr>|.
    ENDLOOP.


    ret = |{ title( descr ) }{ ret }{ footer( descr ) }|.
  ENDMETHOD.


  METHOD footer.
    ret = |</table>|.
  ENDMETHOD.


  METHOD get_description.
    DATA(description) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data( tbl ) ).
    DATA(components) = CAST cl_abap_structdescr( description->get_table_line_type( ) )->get_components( ).

    LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
      DATA(elemdescr) = CAST cl_abap_elemdescr( <component>-type )."->get_ddic_field( ).

      elemdescr->get_ddic_field(
        RECEIVING
          p_flddescr = DATA(elem)
        EXCEPTIONS
          not_found    = 1
          no_ddic_type = 2
          OTHERS       = 3 ).
      IF sy-subrc <> 0.
        APPEND VALUE #( name = <component>-name title = <component>-name description = <component>-name ) TO ret.
      ELSE.
        APPEND VALUE #( name = <component>-name title = elem-reptext description = elem-fieldtext ) TO ret.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD table_params.
    IF table_id <> ''.
      ret = |{ ret } id="{ table_id }"|.
    ENDIF.

    IF table_class <> ''.
      ret = |{ ret } class="{ table_class }"|.
    ENDIF.

    IF table_style <> ''.
      ret = |{ ret } style="{ table_style }"|.
    ENDIF.
  ENDMETHOD.


  METHOD td_params.
    IF td_class <> ''.
      ret = |{ ret } class="{ td_class }"|.
    ENDIF.

    IF td_style <> ''.
      ret = |{ ret } style="{ td_style }"|.
    ENDIF.
  ENDMETHOD.


  METHOD th_params.
    IF th_class <> ''.
      ret = |{ ret } class="{ th_class }"|.
    ENDIF.

    IF th_style <> ''.
      ret = |{ ret } style="{ th_style }"|.
    ENDIF.
  ENDMETHOD.


  METHOD title.
    DATA(th) = th_params( ).

    LOOP AT desc ASSIGNING FIELD-SYMBOL(<item>).
      ret = |{ ret }  <th{ th }>{ <item>-title }</th>|.
    ENDLOOP.

    ret = |<table{ table_params( ) }> <tr{ tr_params( ) }>{ ret } </tr>|.
  ENDMETHOD.


  METHOD tr_params.
    IF tr_class <> ''.
      ret = |{ ret } class="{ tr_class }"|.
    ENDIF.

    IF tr_style <> ''.
      ret = |{ ret } style="{ tr_style }"|.
    ENDIF.
  ENDMETHOD.


  METHOD value_to_string.
    ret = CONV string( val ).
  ENDMETHOD.
ENDCLASS.
